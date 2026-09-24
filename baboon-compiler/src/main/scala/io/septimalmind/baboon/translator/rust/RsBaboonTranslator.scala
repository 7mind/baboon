package io.septimalmind.baboon.translator.rust

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.{escapeRustKeyword, escapeRustModuleName, escapeRustTypeName, toSnakeCaseRaw}
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, DomainProductTranslator, EvolutionMetadataPlan, McpServerGeneratorHook, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import io.septimalmind.baboon.translator.BaboonRuntimeResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class RsBaboonTranslator[F[+_, +_]: Error2](
  trans: RsTypeTranslator,
  convTransFac: RsConversionTranslator.Factory[F],
  defnTranslator: Subcontext[RsDefnTranslator[F]],
  target: RsTarget,
  rsFiles: RsFileTools,
  mcpHook: McpServerGeneratorHook[F],
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime()
      fixture    <- sharedFixture()
      testHelper <- sharedTestHelper()
      // Generate MCP sources early so that the per-service and runtime files
      // are included in the module-declaration pass (generateModFiles /
      // generateLibRs). Without this, `baboon_mcp_server.rs` and the
      // per-service `*_mcp_server.rs` files are absent from `lib.rs` and the
      // enclosing `mod.rs`, causing `cargo build` to ignore them.
      mcpSrcs <- if (target.language.generateMcpServer) mcpHook.generateMcpServer(family) else F.pure(Sources(Map.empty))

      // Convert MCP OutputFile entries to RsDefnTranslator.Output so they
      // flow through generateModFiles/generateLibRs. They are verbatim
      // (doNotModify = true) — the content is already rendered text.
      mcpOutputs = mcpSrcs.files.toList.map {
        case (path, of) =>
          RsDefnTranslator.Output(
            path,
            TextTree.verbatim(of.content),
            RsValue.RsCrateId(izumi.fundamentals.collections.nonempty.NEList("crate")),
            of.product,
            doNotModify = true,
          )
      }

      // Detect type/namespace conflicts: `foo.rs` conflicts with directory `foo/`
      // In Rust you can't have both — merge the type content into `foo/mod.rs`
      allPaths = translated.filterNot(_.doNotModify).map(_.path).toSet
      dirPaths = allPaths.flatMap {
        p =>
          val parts = p.split('/').toList
          (1 until parts.size).map(i => parts.take(i).mkString("/"))
      }
      conflicting = translated.filter {
        o =>
          !o.doNotModify && dirPaths.contains(o.path.stripSuffix(".rs"))
      }
      conflictPaths = conflicting.map(_.path).toSet
      normal        = translated.filterNot(o => conflictPaths.contains(o.path))

      allModFiles <- generateModFiles(translated ++ mcpOutputs.filter(!_.isModFile), conflicting)
      modFiles     = allModFiles.filterNot(_.path == "mod.rs")
      allEmitted   = normal ++ runtime ++ fixture ++ testHelper ++ modFiles ++ mcpOutputs
      libFile      = generateLibRs(allEmitted)
      rootMod      = generateRootMod(allEmitted)
      cargoToml    = generateCargoToml(family)
      rendered = (allEmitted ++ libFile ++ rootMod ++ cargoToml).map {
        o =>
          val content = renderTree(o)
          (o.path, OutputFile(content, o.product))
      }
      unique <- F.fromEither(rendered.toUniqueMap(c => BaboonIssue.of(TranslationIssue.NonUniqueOutputFiles(c))))
    } yield {
      Sources(unique)
    }
  }

  private def translateFamily(family: BaboonFamily): Out[List[RsDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(lineage: BaboonLineage): Out[List[RsDefnTranslator.Output]] = {
    for {
      perVersion <- F.flatSequenceAccumErrors {
        lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
      }
      facade = generateDomainFacade(lineage)
    } yield {
      perVersion ++ facade
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: DomainMember.User => F[NEList[BaboonIssue], List[RsDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[RsDefnTranslator.Output]] = {
    DomainProductTranslator.translate(domain, target.output.products, p, translate)
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[RsDefnTranslator.Output]] = {
    val evo = lineage.evolution
    defnTranslator.provide(domain).provide(evo).produce().use {
      defnTranslator =>
        for {
          defnSources     <- translateProduct(domain, CompilerProduct.Definition, defnTranslator.translate)
          fixturesSources <- translateProduct(domain, CompilerProduct.Fixture, defnTranslator.translateFixtures)
          testsSources    <- translateProduct(domain, CompilerProduct.Test, defnTranslator.translateTests)
          serviceRt       <- defnTranslator.translateServiceRt()

          conversionSources <- {
            if (target.output.products.contains(CompilerProduct.Conversion)) {
              val evosToCurrent = evo.diffs.keySet.filter(_.to == domain.version)
              generateConversions(domain, lineage, evosToCurrent)
            } else {
              F.pure(List.empty)
            }
          }
        } yield {
          defnSources ++ serviceRt ++ conversionSources ++ fixturesSources ++ testsSources
        }
    }
  }

  private def generateDomainFacade(lineage: BaboonLineage): List[RsDefnTranslator.Output] = {
    if (!target.output.products.contains(CompilerProduct.Runtime)) return List.empty
    if (!target.language.generateDomainFacade) return List.empty
    // The rust facade enumerates per-type `BaboonGeneratedDyn` wrappers and references the
    // `BaboonBinEncode`/`BaboonBinDecode` traits per type. Both are only implemented when
    // UEBA + JSON codec generation is on by default — without that, individual types may
    // lack the trait impls and the facade fails to compile (see conv-test-rs, where neither
    // byDefault flag is set).
    if (!(target.language.generateUebaCodecs && target.language.generateUebaCodecsByDefault)) return List.empty
    if (!(target.language.generateJsonCodecs && target.language.generateJsonCodecsByDefault)) return List.empty

    val evo            = lineage.evolution
    val latestVersion  = evo.latest
    val latestDomain   = lineage.versions(latestVersion)
    val latestBasename = rsFiles.basename(latestDomain, evo)

    // Pascal-case domain name: "my.ok" → "MyOk"
    val pascalDomainId = latestDomain.id.path.map(_.capitalize).mkString
    val structName     = s"Domain${pascalDomainId}Facade"
    val domainIdStr    = latestDomain.id.path.mkString(".")

    // Collect non-ADT-branch User types per version.
    // Returns list of (rustFullPath, dynBaseName, typeIdentifier, versionStr).
    // `dynBaseName` is a stable per-type symbol that uniquely identifies the type within
    // the domain across all `Owner.Ns` paths — it is the type name prefixed by the
    // PascalCase namespace path. Two types named `Clash` in different namespaces (e.g.
    // top-level vs `ns clash`) get distinct names (`Clash` vs `ClashClash`) so the
    // generated `${dynBaseName}V${ver}Dyn` symbol does not collide.
    def collectTypes(domain: Domain): List[(String, String, String, String)] = {
      val versionStr  = domain.version.v.toString
      val isLatest    = domain.version == evo.latest
      val versionTail = if (isLatest) Nil else List("v" + versionStr.replace('.', '_'))
      val domainParts = domain.id.path.map(_.toLowerCase) ++ versionTail
      domain.defs.meta.nodes.toList.flatMap {
        case (_, defn: DomainMember.User) =>
          defn.id.owner match {
            case Owner.Adt(_) => None // skip ADT branches
            case owner =>
              defn.defn match {
                case _: Typedef.Dto | _: Typedef.Enum | _: Typedef.Adt =>
                  val typeName   = escapeRustTypeName(defn.id.name.name.capitalize)
                  val moduleName = escapeRustModuleName(toSnakeCaseRaw(defn.id.name.name))
                  val nsParts    = owner.asPseudoPkg.map(s => escapeRustModuleName(s.toLowerCase)).toList
                  val fullPath   = (List("crate") ++ domainParts ++ nsParts ++ List(moduleName) :+ typeName).mkString("::")
                  val nsPrefix   = owner.asPseudoPkg.map(_.capitalize).mkString
                  val dynBase    = s"$nsPrefix$typeName"
                  val typeId     = defn.id.toString
                  Some((fullPath, dynBase, typeId, versionStr))
                case _ => None
              }
          }
        case _ => None
      }
    }

    // Ordered versions (ascending by semver)
    val orderedVersions = lineage.versions.toSeq.sortBy(_._1).map { case (_, domain) => domain }

    val header: TextTree[RsValue] =
      q"""use crate::baboon_codecs_facade::{
         |    AbstractBaboonJsonCodecsImpl, AbstractBaboonUebaCodecsImpl,
         |    BaboonAnyBinCodec, BaboonAnyJsonCodec, BaboonAnyMeta,
         |    BaboonCodecsFacade, BaboonDomainVersion, BaboonGeneratedDyn,
         |};
         |use crate::baboon_runtime::{BaboonBinDecode, BaboonBinEncode, BaboonCodecContext};
         |use std::io::{Read, Write};
         |use std::sync::Arc;""".stripMargin

    def factoryNames(domain: Domain): (String, String, String) = {
      val verSuffix = domain.version.v.toString.replace('.', '_')
      (
        s"make_${domainIdStr.replace('.', '_')}_v${verSuffix}_json_codecs",
        s"make_${domainIdStr.replace('.', '_')}_v${verSuffix}_bin_codecs",
        s"Domain${pascalDomainId}V${verSuffix}Meta",
      )
    }

    // Per-version: impl BaboonGeneratedDyn directly on the user type, plus per-type codec
    // structs, codec factory functions, meta. Implementing the trait on the user type
    // (rather than on a private wrapper) lets external callers pass `&my_inner` directly
    // to `facade.encode_to_bin(&ctx, ...)` — closing MFACADE-PR-D's deferred round-trip
    // exposure note. Older versions live in their own `v<X_Y_Z>` module so each rust
    // type carries its own version metadata via its impl; no symbol clash.
    val perVersion: List[TextTree[RsValue]] = orderedVersions.toList.map {
      domain =>
        val versionStr                = domain.version.v.toString
        val verSuffix                 = versionStr.replace('.', '_')
        val types                     = collectTypes(domain)
        val (jsonFn, binFn, metaName) = factoryNames(domain)

        val metadata    = EvolutionMetadataPlan(lineage.evolution, domain.version)
        val fwdByTypeId = metadata.forwardReadable.map { case EvolutionMetadataPlan.ForwardReadable(tid, readers) => (tid.toString, readers) }.toMap
        def fwdPairs(typeId: String): String = {
          fwdByTypeId
            .get(typeId).toList.flatten.map {
              case EvolutionMetadataPlan.ReaderVersion(v, tier) => s"""("$v".to_string(), "$tier".to_string())"""
            }.mkString(", ")
        }
        val minReadersByTypeId = metadata.forwardReadable.map {
          entry => (entry.typeId.toString, lineage.evolution.minReaders(domain.version, entry.typeId))
        }.toMap
        def minReaderPairs(typeId: String): String = {
          minReadersByTypeId
            .get(typeId).toList.flatMap(_.toList.sortBy(_._1.weight)).map {
              case (tier, v) => s"""("${tier.wireName}".to_string(), "${v.v.toString}".to_string())"""
            }.mkString(", ")
        }
        val sameInByTypeId = metadata.sameIn.map {
          case EvolutionMetadataPlan.SameIn(tid, versions) => (tid.toString, versions)
        }.toMap

        val perType: List[TextTree[RsValue]] = types.map {
          case (fullPath, dynBase, typeId, _) =>
            val binCodec  = s"${dynBase}V${verSuffix}BinCodec"
            val jsonCodec = s"${dynBase}V${verSuffix}JsonCodec"
            // the real sameIn run: the envelope's byte-identical bound is its head, so a degenerate
            // `[own version]` here would make Rust-written envelopes of unchanged types unreadable by
            // older readers in every language
            val sameInVec   = sameInByTypeId.getOrElse(typeId, List(versionStr)).map(v => s""""$v".to_string()""").mkString(", ")
            val firstSameIn = sameInByTypeId.getOrElse(typeId, List(versionStr)).head
            val minReaderCases = minReadersByTypeId
              .get(typeId).toList.flatMap(_.toList.sortBy(_._1.weight)).map {
                case (tier, v) => q""""${tier.wireName}" => Some(std::borrow::Cow::Borrowed("${v.v.toString}")),"""
              }.joinN()
            q"""impl BaboonGeneratedDyn for $fullPath {
               |    fn baboon_domain_version_dyn(&self) -> &str { "$versionStr" }
               |    fn baboon_domain_identifier_dyn(&self) -> &str { "$domainIdStr" }
               |    fn baboon_type_identifier_dyn(&self) -> &str { "$typeId" }
               |    fn baboon_same_in_versions_dyn(&self) -> Vec<String> { vec![$sameInVec] }
               |    fn baboon_first_same_in_version_dyn(&self) -> Option<std::borrow::Cow<'_, str>> { Some(std::borrow::Cow::Borrowed("$firstSameIn")) }
               |    fn baboon_forward_readable_dyn(&self) -> Vec<(String, String)> { vec![${fwdPairs(typeId)}] }
               |    fn baboon_min_reader_versions_dyn(&self) -> Vec<(String, String)> { vec![${minReaderPairs(typeId)}] }
               |    fn baboon_min_reader_version_dyn(&self, tier: &str) -> Option<std::borrow::Cow<'_, str>> {
               |        match tier {
               |            ${minReaderCases.shift(12).trim}
               |            _ => None,
               |        }
               |    }
               |    fn as_any(&self) -> &dyn std::any::Any { self }
               |    fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self }
               |}
               |struct $binCodec;
               |impl BaboonAnyBinCodec for $binCodec {
               |    fn type_identifier(&self) -> &str { "$typeId" }
               |    fn encode_dyn(&self, ctx: &BaboonCodecContext, writer: &mut dyn Write, value: &dyn BaboonGeneratedDyn) -> Result<(), crate::any_opaque::BaboonCodecError> {
               |        let v = value.as_any().downcast_ref::<$fullPath>().ok_or_else(|| crate::any_opaque::BaboonCodecError::encoder_failure("$binCodec.encode: wrong type"))?;
               |        v.encode_ueba(ctx, writer).map_err(|e| crate::any_opaque::BaboonCodecError::encoder_failure(format!("{}", e)))
               |    }
               |    fn decode_dyn(&self, ctx: &BaboonCodecContext, reader: &mut dyn Read) -> Result<Box<dyn BaboonGeneratedDyn>, crate::any_opaque::BaboonCodecError> {
               |        let v = <$fullPath as BaboonBinDecode>::decode_ueba(ctx, reader).map_err(|e| crate::any_opaque::BaboonCodecError::decoder_failure(format!("{}", e)))?;
               |        Ok(Box::new(v))
               |    }
               |}
               |struct $jsonCodec;
               |impl BaboonAnyJsonCodec for $jsonCodec {
               |    fn type_identifier(&self) -> &str { "$typeId" }
               |    fn encode_json_dyn(&self, ctx: &BaboonCodecContext, value: &dyn BaboonGeneratedDyn) -> Result<serde_json::Value, crate::any_opaque::BaboonCodecError> {
               |        let v = value.as_any().downcast_ref::<$fullPath>().ok_or_else(|| crate::any_opaque::BaboonCodecError::encoder_failure("$jsonCodec.encode: wrong type"))?;
               |        v.encode_json(ctx)
               |    }
               |    fn decode_json_dyn(&self, ctx: &BaboonCodecContext, wire: &serde_json::Value) -> Result<Box<dyn BaboonGeneratedDyn>, crate::any_opaque::BaboonCodecError> {
               |        let v = <$fullPath>::decode_json(ctx, wire)?;
               |        Ok(Box::new(v))
               |    }
               |}""".stripMargin
        }

        // Meta struct: the real per-type sameIn / forward-readable tables for this version
        val sameInArms = types.map {
          case (_, _, typeId, _) =>
            val vs = sameInByTypeId.getOrElse(typeId, List(versionStr)).map(v => s""""$v".to_string()""").mkString(", ")
            q""""$typeId" => vec![$vs],"""
        }
        val forwardArms = types.map {
          case (_, _, typeId, _) => q""""$typeId" => vec![${fwdPairs(typeId)}],"""
        }
        val meta: TextTree[RsValue] =
          q"""struct $metaName;
             |impl BaboonAnyMeta for $metaName {
             |    fn same_in_versions(&self, type_id: &str) -> Vec<String> {
             |        match type_id {
             |            ${sameInArms.joinN().shift(12).trim}
             |            _ => Vec::new(),
             |        }
             |    }
             |    fn forward_readable_versions(&self, type_id: &str) -> Vec<(String, String)> {
             |        match type_id {
             |            ${forwardArms.joinN().shift(12).trim}
             |            _ => Vec::new(),
             |        }
             |    }
             |}""".stripMargin

        // Codec factories
        val jsonRegistrations = types.map {
          case (_, dynBase, typeId, _) => q"""t.register("$typeId", || Arc::new(${dynBase}V${verSuffix}JsonCodec) as Arc<dyn BaboonAnyJsonCodec>);"""
        }
        val binRegistrations = types.map {
          case (_, dynBase, typeId, _) => q"""t.register("$typeId", || Arc::new(${dynBase}V${verSuffix}BinCodec) as Arc<dyn BaboonAnyBinCodec>);"""
        }
        val factories: TextTree[RsValue] =
          q"""fn $jsonFn() -> Arc<AbstractBaboonJsonCodecsImpl> {
             |    let mut t = AbstractBaboonJsonCodecsImpl::new();
             |    ${jsonRegistrations.joinN().shift(4).trim}
             |    Arc::new(t)
             |}
             |fn $binFn() -> Arc<AbstractBaboonUebaCodecsImpl> {
             |    let mut t = AbstractBaboonUebaCodecsImpl::new();
             |    ${binRegistrations.joinN().shift(4).trim}
             |    Arc::new(t)
             |}""".stripMargin

        (perType ++ List(meta, factories)).joinN()
    }

    // Facade struct
    val registrations: List[TextTree[RsValue]] = orderedVersions.toList.map {
      domain =>
        val (jsonFn, binFn, metaName) = factoryNames(domain)
        q"""facade.register_with_meta(
           |    BaboonDomainVersion::new("$domainIdStr", "${domain.version.v.toString}"),
           |    $jsonFn,
           |    $binFn,
           |    || Arc::new($metaName) as Arc<dyn BaboonAnyMeta>,
           |);""".stripMargin
    }
    val facadeStruct: TextTree[RsValue] =
      q"""pub struct $structName {
         |    pub facade: BaboonCodecsFacade,
         |}
         |
         |impl $structName {
         |    pub fn new() -> Self {
         |        let facade = BaboonCodecsFacade::new();
         |        ${registrations.joinN().shift(8).trim}
         |        $structName { facade }
         |    }
         |}
         |
         |impl Default for $structName {
         |    fn default() -> Self { Self::new() }
         |}""".stripMargin

    val tree: TextTree[RsValue] = List(header, perVersion.joinN(), facadeStruct).joinNN()
    val crate                   = trans.toRsCrate(latestDomain.id, latestVersion, evo)

    List(
      RsDefnTranslator.Output(
        s"$latestBasename/domain_${pascalDomainId.toLowerCase}_facade.rs",
        tree,
        crate,
        CompilerProduct.Runtime,
        doNotModify = true,
      )
    )
  }

  private def generateModFiles(
    outputs: List[RsDefnTranslator.Output],
    conflicting: List[RsDefnTranslator.Output],
  ): Out[List[RsDefnTranslator.Output]] = {
    // Per-domain facades are emitted with `doNotModify = true` (verbatim text rendering)
    // but live inside the domain directory tree, so their module declaration must appear
    // in the enclosing `mod.rs`. Don't filter them out of the path scan.
    val allPaths      = outputs.map(_.path)
    val adtPaths      = outputs.filterNot(_.doNotModify).filter(_.isAdt).map(_.path.stripSuffix(".rs")).toSet
    val conflictByDir = conflicting.groupBy(_.path.stripSuffix(".rs"))
    val mods          = generateModFilesForPaths(allPaths, conflictByDir, adtPaths, CompilerProduct.Definition)
    F.pure(mods)
  }

  private def generateModFilesForPaths(
    paths: List[String],
    conflictContent: Map[String, List[RsDefnTranslator.Output]],
    adtPaths: Set[String],
    product: CompilerProduct,
  ): List[RsDefnTranslator.Output] = {
    if (paths.isEmpty) return Nil

    val allDirs = scala.collection.mutable.Set.empty[String]

    val filesByDir = paths.groupBy {
      path =>
        val parts = path.split('/').toList
        if (parts.size > 1) parts.init.mkString("/") else ""
    }

    paths.foreach {
      path =>
        val parts = path.split('/').toList
        for (i <- 1 until parts.size) {
          allDirs += parts.take(i).mkString("/")
        }
        if (parts.size == 1) allDirs += ""
    }

    val childrenByDir = allDirs.iterator
      .filter(_.nonEmpty).toList.groupBy {
        dir =>
          val separator = dir.lastIndexOf('/')
          if (separator < 0) "" else dir.substring(0, separator)
      }.view.mapValues(_.map(_.split('/').last).toSet).toMap

    allDirs.toList.sorted.map {
      dir =>
        val prefix = if (dir.isEmpty) "" else dir + "/"

        // Direct child directories of this directory
        val childDirs = childrenByDir.getOrElse(dir, Set.empty[String])

        // File modules in this directory, EXCLUDING those that clash with child directories
        val fileModNames = filesByDir
          .getOrElse(dir, Nil).map {
            file =>
              file.split('/').last.stripSuffix(".rs")
          }.sorted.distinct.filterNot(childDirs.contains)

        val allModNames = (fileModNames ++ childDirs.toList).sorted.distinct
        val modDecls = allModNames.flatMap {
          name =>
            val escaped  = escapeRustModuleName(name)
            val fullPath = if (prefix.isEmpty) name else s"$dir/$name"
            if (childDirs.contains(name)) {
              // Directory module: declare but don't re-export (avoids name clashes with versioned modules)
              List(q"pub mod $escaped;")
            } else if (adtPaths.contains(fullPath)) {
              // ADT module: declare but don't glob-re-export (branch structs would conflict with other ADTs/types)
              List(q"pub mod $escaped;")
            } else {
              val reexport = target.language.reexportMode match {
                case "none"      => false
                case "selective" =>
                  // Only re-export if not a wiring, client, conversion, or test module
                  // Note: _fixture must be re-exported because generated tests and cross-fixture references use super::
                  val isInfraModule = name.endsWith("_wiring") || name.endsWith("_client") || name.startsWith("from_") ||
                    name.endsWith("_tests")
                  !isInfraModule
                case _ => true // "all" (default)
              }
              if (reexport) List(q"pub mod $escaped;", q"pub use $escaped::*;")
              else List(q"pub mod $escaped;")
            }
        }

        // Merge content from conflicting types that were absorbed into this directory
        val mergedContent = conflictContent.getOrElse(dir, Nil).map(_.tree)

        val modTree = if (mergedContent.nonEmpty) {
          (modDecls ++ mergedContent).joinNN()
        } else {
          modDecls.joinN()
        }

        val modPath = if (dir.isEmpty) "mod.rs" else s"$dir/mod.rs"

        RsDefnTranslator.Output(
          modPath,
          modTree,
          RsValue.RsCrateId(NEList("crate")),
          product,
          isModFile = true,
        )
    }
  }

  private def sharedRuntime(): Out[List[RsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          RsDefnTranslator.Output(
            "baboon_runtime.rs",
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/rust/baboon_runtime.rs")),
            RsValue.RsCrateId(NEList("crate")),
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          RsDefnTranslator.Output(
            "any_opaque.rs",
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/rust/any_opaque.rs")),
            RsValue.RsCrateId(NEList("crate")),
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          RsDefnTranslator.Output(
            "baboon_type_meta.rs",
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/rust/baboon_type_meta.rs")),
            RsValue.RsCrateId(NEList("crate")),
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          RsDefnTranslator.Output(
            "baboon_codecs_facade.rs",
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/rust/baboon_codecs_facade.rs")),
            RsValue.RsCrateId(NEList("crate")),
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          RsDefnTranslator.Output(
            "baboon_service_wiring.rs",
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/rust/baboon_service_wiring.rs")),
            RsValue.RsCrateId(NEList("crate")),
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          RsDefnTranslator.Output(
            "baboon_identifier_repr.rs",
            // `verbatim` (not `text`): the file contains backslash characters
            // (escape-handling, hex literals) that would crash Scala's
            // StringContext.processEscapes if routed through `text`.
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/rust/baboon_identifier_repr.rs")),
            RsValue.RsCrateId(NEList("crate")),
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
        )
      )
    } else {
      F.pure(List.empty)
    }
  }

  private def sharedFixture(): Out[List[RsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      F.pure(
        List(
          RsDefnTranslator.Output(
            "baboon_fixture.rs",
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/rust/baboon_fixture.rs")),
            RsValue.RsCrateId(NEList("crate")),
            CompilerProduct.FixtureRuntime,
            doNotModify = true,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def sharedTestHelper(): Out[List[RsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Test)) {
      F.pure(
        List(
          RsDefnTranslator.Output(
            "cross_language_fixture_path.rs",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/rust/cross_language_fixture_path.rs")),
            RsValue.RsCrateId(NEList("crate")),
            CompilerProduct.Test,
            doNotModify = true,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def renderTree(o: RsDefnTranslator.Output): String = {
    if (o.doNotModify) {
      o.tree.mapRender {
        case t: RsValue.RsType     => t.name
        case t: RsValue.RsTypeName => t.name
      }
    } else {
      // Compute this output's own module path from its file path
      val selfModulePath = {
        val stripped = o.path.stripSuffix(".rs").replace("/", "::")
        if (o.isModFile) {
          val dir = stripped.stripSuffix("::mod")
          if (dir.isEmpty) NEList("crate") else NEList.unsafeFrom(("crate" +: dir.split("::").toList).toList)
        } else {
          NEList.unsafeFrom(("crate" +: stripped.split("::").toList).toList)
        }
      }

      // Collect used types for use statements, filtering out self-imports
      val usedTypes = o.tree.values.collect { case t: RsValue.RsType => t }.distinct
        .filterNot(_.predef)
        .filterNot(_.fq)
        .filterNot(t => t.crate.parts == selfModulePath)
        .sortBy(_.toString)

      val imports = usedTypes.map {
        t =>
          q"use ${t.crate.parts.mkString("::")}::${t.name};"
      }.joinN()

      val full = Seq(imports, o.tree).joinNN()

      full.mapRender {
        case t: RsValue.RsType if t.fq =>
          (t.crate.parts :+ t.name).mkString("::")
        case t: RsValue.RsType     => t.name
        case t: RsValue.RsTypeName => t.name
      }
    }
  }

  private def topLevelModuleNames(allOutputs: List[RsDefnTranslator.Output]): List[String] = {
    allOutputs
      .filterNot(_.isModFile)
      .filterNot(_.path == "lib.rs")
      .filterNot(_.path == "mod.rs")
      .map {
        o =>
          val first = o.path.split('/').head
          first.stripSuffix(".rs")
      }
      .distinct
      .sorted
  }

  private val crateAllows = {
    val base = List(
      q"#![allow(unused_imports)]",
      q"#![allow(non_camel_case_types)]",
      q"#![allow(non_snake_case)]",
      q"#![allow(dead_code)]",
      q"#![allow(unused_variables)]",
      q"#![allow(clippy::too_many_arguments)]",
      q"#![allow(clippy::large_enum_variant)]",
    )
    // Under `--rs-async-services=true` the generated service traits declare
    // their methods as bare `async fn` (AFIT). rustc warns (`async_fn_in_trait`)
    // that auto-trait bounds like `Send` cannot be specified on the returned
    // future; this is intentional here (the service futures are deliberately
    // `?Send`, matching the wiring future shape — see RsServiceWiringTranslator).
    // Allow it crate-wide so async output builds clean under `-D warnings`.
    // Sync output omits this line and stays byte-identical to baseline.
    if (target.language.asyncServices) base :+ q"#![allow(async_fn_in_trait)]"
    else base
  }

  private def modDeclsFor(names: List[String]): List[TextTree[RsValue]] = {
    names.map {
      name =>
        val escaped = escapeRustModuleName(name)
        q"pub mod $escaped;"
    }
  }

  private def generateLibRs(allOutputs: List[RsDefnTranslator.Output]): List[RsDefnTranslator.Output] = {
    val tree = (crateAllows ++ modDeclsFor(topLevelModuleNames(allOutputs))).joinN()

    List(
      RsDefnTranslator.Output(
        "lib.rs",
        tree,
        RsValue.RsCrateId(NEList("crate")),
        CompilerProduct.Definition,
        doNotModify = true,
      )
    )
  }

  private def generateRootMod(allOutputs: List[RsDefnTranslator.Output]): List[RsDefnTranslator.Output] = {
    // Aggregator emitted alongside lib.rs. Consumers that re-include the generated tree
    // from a hand-written lib.rs use this via `#[path = "generated/mod.rs"] mod baboon_generated;`
    // — they get auto-routed top-level modules without hand-curating each `#[path]`.
    val tree = (crateAllows ++ modDeclsFor(topLevelModuleNames(allOutputs))).joinN()

    List(
      RsDefnTranslator.Output(
        "mod.rs",
        tree,
        RsValue.RsCrateId(NEList("crate")),
        CompilerProduct.Definition,
        doNotModify = true,
      )
    )
  }

  private def generateCargoToml(family: BaboonFamily): List[RsDefnTranslator.Output] = {
    // Only generate Cargo.toml as a custom metadata file - written to the output dir only when CustomMeta is requested
    if (!target.output.products.contains(CompilerProduct.Runtime)) return Nil

    import io.septimalmind.baboon.typer.model.TypeId

    val allTypes = family.domains.toMap.values.flatMap(_.versions.toMap.values).flatMap(_.defs.meta.nodes.keys).toSet

    val hasTimestamps = allTypes.exists {
      case TypeId.Builtins.tsu | TypeId.Builtins.tso => true
      case _                                         => false
    }
    val hasUuids      = allTypes.contains(TypeId.Builtins.uid)
    val hasDecimals   = allTypes.contains(TypeId.Builtins.f128)

    val deps = scala.collection.mutable.ListBuffer.empty[String]
    deps += """serde = { version = "1", features = ["derive"] }"""
    // `preserve_order` is load-bearing, not a nicety: the explicit JSON encoders build a
    // `serde_json::Map` per object, and without the feature that Map is a BTreeMap, so every
    // generated document would come out with its keys alphabetised instead of in field order.
    deps += """serde_json = { version = "1", features = ["preserve_order"], optional = true }"""
    deps += """rust_decimal = { version = "1", features = ["serde-with-str"], optional = true }"""
    deps += """chrono = { version = "0.4", features = ["serde"], optional = true }"""
    deps += """uuid = { version = "1", features = ["v4", "serde"], optional = true }"""

    val defaultFeatures = scala.collection.mutable.ListBuffer.empty[String]
    if (hasDecimals) defaultFeatures += """"decimal""""
    // Unconditional: `encode_json`/`decode_json` are emitted for every generated type — they are
    // also the structural round-trip the conversions copy identical types through — and they are
    // written in terms of `baboon_runtime::json_tools`, which this feature gates.
    defaultFeatures += """"json-helpers""""
    if (hasTimestamps) defaultFeatures += """"timestamps""""
    if (hasUuids) defaultFeatures += """"uuids""""

    val content =
      s"""[package]
         |name = "baboon-generated"
         |version = "0.1.0"
         |edition = "${target.language.edition}"
         |rust-version = "1.75"
         |
         |[dependencies]
         |${deps.mkString("\n")}
         |
         |[features]
         |default = [${defaultFeatures.mkString(", ")}]
         |decimal = ["dep:rust_decimal"]
         |json-helpers = ["dep:serde_json"]
         |timestamps = ["dep:chrono"]
         |uuids = ["dep:uuid"]
         |""".stripMargin

    List(
      RsDefnTranslator.Output(
        "Cargo.toml",
        TextTree.text(content),
        RsValue.RsCrateId(NEList("crate")),
        CompilerProduct.Definition,
        doNotModify = true,
      )
    )
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
  ): Out[List[RsDefnTranslator.Output]] = {
    val crate = trans.toRsCrate(domain.id, domain.version, lineage.evolution)

    for {
      convs <-
        F.flatSequenceAccumErrors {
          lineage.evolution.rules
            .filter(kv => toCurrent.contains(kv._1))
            .map {
              case (srcVer, rules) =>
                convTransFac(
                  crate  = crate,
                  srcDom = lineage.versions(srcVer.from),
                  domain = domain,
                  rules  = rules,
                  evo    = lineage.evolution,
                ).makeConvs
            }
        }
    } yield {
      val basename = rsFiles.basename(domain, lineage.evolution)
      convs.toList.map {
        conv =>
          RsDefnTranslator.Output(
            s"$basename/${conv.fname}",
            conv.conv,
            crate,
            CompilerProduct.Conversion,
          )
      }
    }
  }
}
