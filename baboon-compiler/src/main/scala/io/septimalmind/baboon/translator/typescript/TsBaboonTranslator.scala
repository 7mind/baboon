package io.septimalmind.baboon.translator.typescript

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.typescript.TsTypes.{tsBaboonAnyOpaqueModule, tsBaboonIdReprModule, tsBaboonRuntimeShared, tsCrossLangFixtureModule, tsFixtureShared}
import io.septimalmind.baboon.translator.typescript.TsValue.{TsModuleId, TsType}
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import io.septimalmind.baboon.translator.BaboonRuntimeResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class TsBaboonTranslator[F[+_, +_]: Error2](
  convTransFac: TsConversionTranslator.Factory[F],
  defnTranslator: Subcontext[TsDefnTranslator[F]],
  target: TsTarget,
  tsFileTools: TsFileTools,
  enquiries: BaboonEnquiries,
  typeTranslator: TsTypeTranslator,
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime
      fixture    <- sharedFixture
      testHelper <- sharedTestHelper
      barrels     = generateBarrels(translated ++ runtime, family)
      rendered = (
        translated ++
          runtime ++
          fixture ++
          testHelper ++
          barrels
      ).map {
        o =>
          val content = renderTree(o)
          (o.path, OutputFile(content, o.product))
      }
      unique <- F.fromEither(rendered.toUniqueMap(c => BaboonIssue.of(TranslationIssue.NonUniqueOutputFiles(c))))
    } yield {
      Sources(unique)
    }
  }

  private def translateFamily(family: BaboonFamily): Out[List[TsDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(lineage: BaboonLineage): Out[List[TsDefnTranslator.Output]] = {
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
    translate: DomainMember.User => F[NEList[BaboonIssue], List[TsDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[TsDefnTranslator.Output]] = {
    val evo = lineage.evolution
    defnTranslator.provide(domain).provide(evo).produce().use {
      defnTranslator =>
        for {
          defnSources     <- translateProduct(domain, CompilerProduct.Definition, defnTranslator.translate)
          fixturesSources <- translateProduct(domain, CompilerProduct.Fixture, defnTranslator.translateFixtures)
          testsSources    <- translateProduct(domain, CompilerProduct.Test, defnTranslator.translateTests)
          serviceRt       <- defnTranslator.translateServiceRt()
          dispatcher      <- defnTranslator.translateDispatcher()

          conversionSources <- {
            if (target.output.products.contains(CompilerProduct.Conversion)) {
              val evosToCurrent = evo.diffs.keySet.filter(_.to == domain.version)
              generateConversions(domain, lineage, evosToCurrent)
            } else {
              F.pure(List.empty)
            }
          }
        } yield {
          defnSources ++ serviceRt ++ dispatcher ++ conversionSources ++ fixturesSources ++ testsSources
        }
    }
  }

  private def generateDomainFacade(lineage: BaboonLineage): List[TsDefnTranslator.Output] = {
    if (!target.output.products.contains(CompilerProduct.Runtime)) return List.empty

    val evo            = lineage.evolution
    val latestVersion  = evo.latest
    val latestDomain   = lineage.versions(latestVersion)
    val latestBasename = tsFileTools.basename(latestDomain, evo)

    // Pascal-case domain name: "my.ok" → "MyOk"
    val pascalDomainId = latestDomain.id.path.map(_.capitalize).mkString
    val className      = s"Domain${pascalDomainId}Facade"
    val domainIdStr    = latestDomain.id.path.mkString(".")

    // Per-type codec activation predicate, mirroring `isActive` in
    // TsJsonCodecGenerator / TsUEBACodecGenerator. Foreign-bound types emit
    // no codec class at all, and the global generate-X flags plus the
    // per-type `derived[…]` opt-in further constrain emission. If either
    // codec is inactive for a type, the facade must NOT import or register
    // that flavour for it.
    def jsonCodecActive(domain: Domain, id: TypeId): Boolean = {
      !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Typescript) &&
      target.language.generateJsonCodecs && (
        target.language.generateJsonCodecsByDefault ||
        domain.derivationRequests.getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id)
      )
    }
    def uebaCodecActive(domain: Domain, defn: DomainMember.User): Boolean = {
      val id = defn.id
      !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Typescript) &&
      target.language.generateUebaCodecs && (
        target.language.generateUebaCodecsByDefault ||
        domain.derivationRequests.getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id)
      )
      // Foreign-bearing types are no longer suppressed: a Custom foreign now emits a `<F>_UEBACodec`
      // value codec (throwing by default, host-overridable via lazyInstance), so containing types
      // route the foreign field through it — mirroring the JSON side and the C# backend.
    }

    // Collect non-ADT-branch User types per version with codec-activation flags.
    // Returns list of (tsImportPath, typeAlias, typeName, typeIdentifier, hasJsonCodec, hasUebaCodec).
    // typeAlias differs from typeName when either (a) the version is older
    // (to disambiguate cross-version imports) or (b) the type lives in a
    // non-toplevel namespace and might collide with a same-named type under
    // a different namespace (e.g. service method `In`/`Out`/`Err` synthetic
    // types). Path segments derived from the source are lowercased to match
    // the on-disk layout produced by TsDefnTranslator.getOutputPath. Types
    // where neither codec is active (foreign-bound types) are dropped — they
    // emit no codec class so there is nothing for the facade to register.
    def collectTypes(domain: Domain): List[(String, String, String, String, Boolean, Boolean)] = {
      val versionStr    = domain.version.v.toString
      val isLatest      = domain.version == evo.latest
      val verSuffix     = versionStr.replace('.', '_')
      // Relative path prefix from facade file's dir (= latestBasename) to type's dir
      val relPrefix = if (isLatest) "." else s"./v$verSuffix"
      domain.defs.meta.nodes.toList.flatMap {
        case (_, defn: DomainMember.User) =>
          defn.id.owner match {
            case Owner.Adt(_) => None // skip ADT branches
            case owner =>
              defn.defn match {
                case _: Typedef.Dto | _: Typedef.Enum | _: Typedef.Adt =>
                  val hasJson = jsonCodecActive(domain, defn.id)
                  val hasUeba = uebaCodecActive(domain, defn)
                  if (!hasJson && !hasUeba) None
                  else {
                    // `typeName` is the TS-class symbol exported by the source
                    // file (always Pascal-case). `fileBase` is the on-disk file
                    // basename, which preserves the source-declared case — for
                    // service-method synthetic types (`in`/`out`/`err` from the
                    // parser keywords) this is lowercase, while top-level type
                    // declarations are typically Pascal-case already. The import
                    // path MUST use `fileBase` so it resolves on case-sensitive
                    // filesystems; the import binding uses `typeName`.
                    val rawName  = defn.id.name.name
                    val typeName = rawName.capitalize
                    val fileBase = rawName
                    // Path segments must match the on-disk layout, which kebab-cases
                    // the service segment (`pet-store`) via the shared rule.
                    val nsSegs: List[String] = owner match {
                      case Owner.Toplevel => Nil
                      case Owner.Ns(p)    => typeTranslator.renderNsOwnerPath(p, domain)
                      case Owner.Adt(_)   => Nil // unreachable, filtered above
                    }
                    val nsPathSegment = if (nsSegs.isEmpty) "" else nsSegs.mkString("", "/", "/")
                    // Unique local alias to avoid collisions: an `In` under
                    // `adminservice/shutdown` and an `In` under `reportservice/fetch`
                    // would otherwise both import as bare `In`. Kebab segments are
                    // flattened to a valid TS identifier prefix (`pet-store` -> `Petstore`).
                    val nsAliasPrefix = nsSegs.map(_.replace("-", "").capitalize).mkString
                    val baseAlias     = s"$nsAliasPrefix$typeName"
                    val typeAlias     = if (isLatest) baseAlias else s"${baseAlias}V$verSuffix"
                    val importPath    = s"$relPrefix/$nsPathSegment$fileBase"
                    val typeId        = defn.id.toString
                    Some((importPath, typeAlias, typeName, typeId, hasJson, hasUeba))
                  }
                case _ => None
              }
          }
        case _ => None
      }
    }

    // Ordered versions (ascending)
    val orderedVersions = lineage.versions.toSeq.sortBy(_._1).map { case (_, domain) => domain }

    val sfx = target.language.importSuffix
    val sb  = new StringBuilder

    // Relative path back to the generated/ root (where BaboonSharedRuntime lives). The facade
    // file lives at `<latestBasename>/<className>.ts`, so depth = segments in `latestBasename`.
    val rootRel = "../" * latestBasename.split('/').length
    // Imports
    sb.append(s"import { AbstractBaboonJsonCodecs, AbstractBaboonUebaCodecs, BaboonBinCodec, BaboonJsonCodec, BaboonDomainVersion, BaboonMeta, Lazy } from '${rootRel}BaboonSharedRuntime$sfx';\n")
    sb.append(s"import type { AbstractBaboonConversions } from '${rootRel}BaboonSharedRuntime$sfx';\n")
    sb.append(s"import { BaboonCodecsFacade } from '${rootRel}BaboonCodecsFacade$sfx';\n")

    // Imports: emit a single line per type with only the codec flavours
    // actually generated for that type. A type may have only JSON, only
    // UEBA, or both; the type itself (class/enum/adt) is always imported
    // since identity registration goes through the codec class.
    for (domain <- orderedVersions) {
      val types = collectTypes(domain)

      for ((importPath, typeAlias, typeName, _, hasJson, hasUeba) <- types) {
        val pieces = List(
          Some((typeName, typeAlias)),
          if (hasJson) Some((s"${typeName}_JsonCodec", s"${typeAlias}_JsonCodec")) else None,
          if (hasUeba) Some((s"${typeName}_UEBACodec", s"${typeAlias}_UEBACodec")) else None,
        ).flatten
        val rendered = pieces.map {
          case (src, alias) => if (src == alias) src else s"$src as $alias"
        }.mkString(", ")
        sb.append(s"import { $rendered } from '$importPath$sfx';\n")
      }
    }

    sb.append("\n")

    // Per-version codec/meta/conversions classes. The JSON and UEBA codec
    // classes each iterate the type list filtered by their respective
    // activation flag, so a domain that emits only UEBA codecs produces an
    // empty (but well-formed) JSON codec class — keeps the facade contract
    // uniform without referencing non-existent symbols.
    for (domain <- orderedVersions) {
      val versionStr   = domain.version.v.toString
      val verSuffix    = versionStr.replace('.', '_')
      val types        = collectTypes(domain)
      val verClassName = s"Domain${pascalDomainId}V${verSuffix}"

      // JSON codecs class. We reference `${typeAlias}_JsonCodec.BaboonTypeIdentifier`
      // (rather than `${typeAlias}.BaboonTypeIdentifier`) because TypeScript `enum`s
      // cannot carry static properties — only the generated codec classes do.
      // The codec class carries the same identifier string for every type kind.
      sb.append(s"class ${verClassName}JsonCodecs extends AbstractBaboonJsonCodecs {\n")
      sb.append( "    constructor() {\n")
      sb.append( "        super();\n")
      for ((_, typeAlias, _, _, hasJson, _) <- types if hasJson) {
        sb.append(s"        this.register(${typeAlias}_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => ${typeAlias}_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));\n")
      }
      sb.append( "    }\n")
      sb.append( "}\n\n")

      // UEBA codecs class — same reasoning as above.
      sb.append(s"class ${verClassName}UebaCodecs extends AbstractBaboonUebaCodecs {\n")
      sb.append( "    constructor() {\n")
      sb.append( "        super();\n")
      for ((_, typeAlias, _, _, _, hasUeba) <- types if hasUeba) {
        sb.append(s"        this.register(${typeAlias}_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => ${typeAlias}_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));\n")
      }
      sb.append( "    }\n")
      sb.append( "}\n\n")

      // Conversions class
      sb.append(s"class ${verClassName}Conversions implements AbstractBaboonConversions {\n")
      sb.append(s"    public versionsFrom(): string[] { return []; }\n")
      sb.append(s"""    public versionTo(): string { return '$versionStr'; }\n""")
      sb.append( "}\n\n")

      // Meta class
      sb.append(s"class ${verClassName}Meta implements BaboonMeta {\n")
      sb.append(s"""    public sameInVersions(_typeId: string): string[] { return ['$versionStr']; }\n""")
      sb.append( "}\n\n")
    }

    // Facade class
    sb.append(s"export class $className extends BaboonCodecsFacade {\n")
    sb.append( "    constructor() {\n")
    sb.append( "        super();\n")
    for (domain <- orderedVersions) {
      val versionStr   = domain.version.v.toString
      val verSuffix    = versionStr.replace('.', '_')
      val verClassName = s"Domain${pascalDomainId}V${verSuffix}"
      sb.append( "        this.register(\n")
      sb.append(s"""            new BaboonDomainVersion('$domainIdStr', '$versionStr'),\n""")
      sb.append(s"            () => new ${verClassName}JsonCodecs(),\n")
      sb.append(s"            () => new ${verClassName}UebaCodecs(),\n")
      sb.append(s"            () => new ${verClassName}Conversions(),\n")
      sb.append(s"            () => new ${verClassName}Meta(),\n")
      sb.append( "        );\n")
    }
    sb.append( "    }\n")
    sb.append( "}\n")

    val content    = sb.toString()
    val outputPath = s"$latestBasename/$className.ts"
    val moduleParts = tsFileTools.definitionsBasePkg ++ outputPath.stripSuffix(".ts").split('/').toList
    val moduleId   = TsValue.TsModuleId(moduleParts)

    List(
      TsDefnTranslator.Output(
        outputPath,
        TextTree.verbatim(content),
        moduleId,
        CompilerProduct.Runtime,
        doNotModify = true,
      )
    )
  }

  private def sharedRuntime: Out[List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          TsDefnTranslator.Output(
            "BaboonSharedRuntime.ts",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonSharedRuntime.ts")),
            tsBaboonRuntimeShared,
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          TsDefnTranslator.Output(
            "BaboonAnyOpaque.ts",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonAnyOpaque.ts")),
            tsBaboonRuntimeShared,
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          TsDefnTranslator.Output(
            "BaboonCodecsFacade.ts",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonCodecsFacade.ts")),
            tsBaboonRuntimeShared,
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          TsDefnTranslator.Output(
            "baboon-identifier-repr.ts",
            // `verbatim` (not `text`): the file contains backslash characters
            // (escape-handling) that would crash Scala's
            // StringContext.processEscapes if routed through `text`.
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/baboon-identifier-repr.ts")),
            tsBaboonIdReprModule,
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
        )
      )
    } else {
      F.pure(List.empty)
    }
  }

  private def sharedFixture: Out[List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      F.pure(
        List(
          TsDefnTranslator.Output(
            "BaboonSharedFixture.ts",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonSharedFixture.ts")),
            tsBaboonRuntimeShared,
            CompilerProduct.Runtime,
            doNotModify = true,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def sharedTestHelper: Out[List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Test)) {
      F.pure(
        List(
          TsDefnTranslator.Output(
            "CrossLanguageFixturePath.ts",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/CrossLanguageFixturePath.ts")),
            tsCrossLangFixtureModule,
            CompilerProduct.Test,
            doNotModify = true,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def renderTree(o: TsDefnTranslator.Output): String = {
    val sfx = target.language.importSuffix

    def relativeImportPath(srcModulePath: List[String], tgtModulePath: List[String]): String = {
      val srcDir          = srcModulePath.dropRight(1)
      val tgtDir          = tgtModulePath.dropRight(1)
      val tgtName         = tgtModulePath.last
      val commonPrefixLen = srcDir.zip(tgtDir).takeWhile { case (a, b) => a == b }.size
      val levelsUp        = srcDir.size - commonPrefixLen
      val remainingPath   = (tgtDir.drop(commonPrefixLen) :+ tgtName).mkString("/")
      if (levelsUp == 0) s"./$remainingPath$sfx" else s"${"../" * levelsUp}$remainingPath$sfx"
    }

    def baboonTypeImport(moduleId: TsModuleId, types: String): TextTree[TsValue] = {
      // The cross-language fixture helper is emitted with CompilerProduct.Test,
      // so the file lands under testsOutput (testBasePkg), NOT output
      // (definitionsBasePkg). Imports for it must therefore be resolved against
      // testBasePkg.
      val targetBasePkg =
        if (moduleId == tsCrossLangFixtureModule) tsFileTools.testBasePkg
        else tsFileTools.definitionsBasePkg
      if (o.module.path.startsWith(targetBasePkg)) {
        val afterBase    = o.module.path.drop(targetBasePkg.size)
        val pathToModule = (0 until afterBase.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToModule${moduleId.path.mkString("")}$sfx'"
      } else {
        val pathToCommonParent = (0 until o.module.path.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToCommonParent${targetBasePkg.mkString("/")}/${moduleId.path.mkString("")}$sfx'"
      }
    }

    def definitionImport(moduleId: TsModuleId, types: String): TextTree[TsValue] = {
      if (o.module.path.startsWith(tsFileTools.definitionsBasePkg)) {
        val importPath = relativeImportPath(o.module.path, moduleId.path)
        q"import {$types} from '$importPath'"
      } else {
        val pathToCommonParent = (0 until o.module.path.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToCommonParent${moduleId.path.mkString("/")}$sfx'"
      }
    }

    def fixtureImport(moduleId: TsModuleId, types: String): TextTree[TsValue] = {
      val importPath = relativeImportPath(o.module.path, moduleId.path)
      q"import {$types} from '$importPath'"
    }

    val usedTypes = o.tree.values.collect { case t: TsValue.TsType => t }
      .filterNot(_.moduleId.path.isEmpty)
      .filterNot(_.moduleId == o.module)
      .filterNot(_.predef)
      .distinct

    val aliasMap = buildAliasMap(usedTypes)

    val typesByModule = usedTypes.groupBy(_.moduleId).toList.sortBy { case (moduleId, types) => moduleId.path.size + types.size }.reverse

    val importsByModule =
      typesByModule.flatMap {
        case (moduleId, types) =>
          // Partition into type-only and value-bearing
          val (typeOnlyTypes, valueTypes) = types.partition(_.typeOnly)

          def typeString(ts: Seq[TsType], perNameType: Boolean): String = ts.map {
            case t if aliasMap.contains(t)          => s"${if (perNameType) "type " else ""}${t.name} as ${aliasMap(t)}"
            case TsType(_, name, Some(alias), _, _) => s"${if (perNameType) "type " else ""}$name as $alias"
            case t: TsValue.TsType                  => s"${if (perNameType) "type " else ""}${t.name}"
          }.mkString(", ")

          def mkImportFor(tsList: Seq[TsType], importType: Boolean): Option[TextTree[TsValue]] = {
            if (tsList.isEmpty) None
            else {
              val typesString = typeString(tsList, perNameType = importType)
              val result = if (moduleId.path.startsWith(tsFileTools.definitionsBasePkg)) {
                definitionImport(moduleId, typesString)
              } else if (moduleId.path.startsWith(tsFileTools.fixturesBasePkg) && tsFileTools.fixturesBasePkg.nonEmpty) {
                fixtureImport(moduleId, typesString)
              } else if (moduleId == tsBaboonRuntimeShared || moduleId == tsFixtureShared || moduleId == tsBaboonAnyOpaqueModule || moduleId == tsBaboonIdReprModule || moduleId == tsCrossLangFixtureModule) {
                baboonTypeImport(moduleId, typesString)
              } else {
                q"import {$typesString} from '${moduleId.path.mkString("/")}'"
              }
              Some(result)
            }
          }

          List(
            mkImportFor(typeOnlyTypes, importType = true),
            mkImportFor(valueTypes, importType    = false),
          ).flatten
      }

    val allImports = importsByModule.joinN()

    val full = Seq(allImports, o.tree).joinNN()

    full.mapRender {
      case t: TsValue.TsType if aliasMap.contains(t) => aliasMap(t)
      case TsValue.TsType(_, _, Some(alias), _, _)   => alias
      case t: TsValue.TsType                         => t.name
    }
  }

  private def buildAliasMap(usedTypes: Seq[TsType]): Map[TsType, String] = {
    val conflicting = usedTypes.groupBy(_.name).filter(_._2.size > 1)
    conflicting.flatMap {
      case (name, group) =>
        val paths     = group.map(t => t -> t.moduleId.path)
        val commonLen = paths.map(_._2).reduce((a, b) => a.zip(b).takeWhile { case (x, y) => x == y }.map(_._1)).size
        paths.map {
          case (t, path) =>
            // An explicit alias on the ref (e.g. the bare-service-symbol refs, which carry a
            // service-name-prefixed alias) wins — auto-aliasing only fills in for refs that don't
            // declare one. Path segments are kebab-cased (per-service dirs), so a module-derived
            // prefix must be sanitized to a valid TS identifier (`report-service` -> `report_service`).
            val alias = t.alias.getOrElse {
              val distinguishing = path.drop(commonLen).dropRight(1)
              val raw =
                if (distinguishing.nonEmpty) distinguishing.mkString("_")
                else path.dropRight(1).lastOption.getOrElse("m")
              s"${raw.replaceAll("[^A-Za-z0-9_$]", "_")}_$name"
            }
            t -> alias
        }
    }
  }

  /** Extract type names that a file defines (types whose module matches the file's own module).
    * Returns a map from name to typeOnly flag (true = type-only, false = value-bearing).
    */
  private def exportedNames(output: TsDefnTranslator.Output): Map[String, Boolean] = {
    output.tree.values.collect {
      case t: TsValue.TsType if t.moduleId == output.module && !t.predef => t.name -> t.typeOnly
    }.toMap
  }

  /** Whether a file emits at least one top-level export. `exportedNames` only
    * sees exports embedded as `TsType` values; declarations whose symbol is
    * interpolated as a plain string (service interfaces, client/wiring classes
    * and functions) are invisible to it. A textual scan catches those so a
    * non-empty wiring/client/service file is barrel-able while a genuinely empty
    * one (e.g. wiring suppressed by flags) is not (avoids TS2306). */
  private def hasExports(output: TsDefnTranslator.Output): Boolean = {
    if (exportedNames(output).nonEmpty) true
    else {
      val text = output.tree.mapRender {
        case TsValue.TsType(_, name, Some(alias), _, _) => alias
        case t: TsValue.TsType                          => t.name
      }
      text.linesIterator.exists(_.trim.startsWith("export "))
    }
  }

  /** Render a named re-export line, splitting type-only and value-bearing names into separate statements. */
  private def namedReexports(
    nameToTypeOnly: Map[String, Boolean],
    names: Iterable[String],
    renderName: String => String,
    from: String,
  ): List[TextTree[TsValue]] = {
    val sorted       = names.toList.sorted
    val typeOnly     = sorted.filter(n => nameToTypeOnly.getOrElse(n, false))
    val valueBearing = sorted.filterNot(n => nameToTypeOnly.getOrElse(n, false))
    List(
      if (typeOnly.nonEmpty) Some(TextTree.text[TsValue](s"export type { ${typeOnly.map(renderName).mkString(", ")} } from '$from';")) else None,
      if (valueBearing.nonEmpty) Some(TextTree.text[TsValue](s"export { ${valueBearing.map(renderName).mkString(", ")} } from '$from';")) else None,
    ).flatten
  }

  /** Layout of one service's per-service directory, derived from the model so
    * the barrels stay aligned with `TsTypeTranslator.serviceDirSegments` /
    * `getOutputPath`. `serviceDirPath` and `modelsDirPath` are relative paths
    * (no definitionsBasePkg prefix), matching `TsDefnTranslator.Output.path`.
    */
  private final case class ServiceBarrelInfo(
    modelsDirPath: String,
    serviceDirPath: String,
    pascalName: String,
    kebabName: String,
  )

  private def collectServiceInfos(family: BaboonFamily): List[ServiceBarrelInfo] = {
    family.domains.toMap.values.toList.flatMap {
      lineage =>
        val evo = lineage.evolution
        lineage.versions.toMap.values.toList.flatMap {
          domain =>
            val basename = tsFileTools.basename(domain, evo)
            domain.defs.meta.nodes.valuesIterator.collect {
              case DomainMember.User(_, svc: Typedef.Service, _, _) =>
                val segs       = typeTranslator.serviceDirSegments(svc)
                val serviceDir = (basename +: segs).mkString("/")
                ServiceBarrelInfo(
                  modelsDirPath  = (basename +: segs.dropRight(1)).mkString("/"),
                  serviceDirPath = serviceDir,
                  pascalName     = svc.id.name.name.capitalize,
                  kebabName      = segs.last,
                )
            }.toList
        }
    }
  }

  private def barrelOutput(path: String, tree: TextTree[TsValue]): TsDefnTranslator.Output = {
    val module = TsModuleId(tsFileTools.definitionsBasePkg ++ path.stripSuffix(".ts").split('/').toList)
    TsDefnTranslator.Output(path, tree, module, CompilerProduct.Definition, doNotModify = true, isBarrel = true)
  }

  /** Service-specific barrels: the per-service `index.ts`, the `methods.ts`
    * namespace aggregator, and the extra `export * as <Service>` lines that the
    * models-level `index.ts` must carry. Method-level `<method>/index.ts`
    * barrels are produced by the generic per-directory logic (a method dir
    * contains only `in`/`out`/`err`, so `export *` of each is correct).
    */
  private def generateServiceBarrels(
    outputs: List[TsDefnTranslator.Output],
    family: BaboonFamily,
  ): (List[TsDefnTranslator.Output], Map[String, List[TextTree[TsValue]]]) = {
    val sfx = target.language.importSuffix
    // Paths whose module actually exports symbols (`hasExports` also sees
    // string-interpolated declarations like the service interface and the
    // wiring/client classes). An empty file (e.g. wiring suppressed by flags)
    // must NOT be `export *`-ed — that yields TS2306 under strict tsc.
    val pathsWithExports = outputs.iterator.filter(o => o.path.endsWith(".ts") && hasExports(o)).map(_.path).toSet
    val infos            = collectServiceInfos(family)

    val serviceBarrels = infos.flatMap {
      info =>
        // Method directory names = immediate child dirs of the service dir that
        // hold emitted files (so absent message types never produce dead exports).
        val prefix = s"${info.serviceDirPath}/"
        val methodDirs = pathsWithExports
          .filter(_.startsWith(prefix))
          .flatMap { p =>
            val rest = p.drop(prefix.length)
            val slash = rest.indexOf('/')
            if (slash >= 0) Some(rest.substring(0, slash)) else None
          }
          .toList
          .distinct
          .sorted

        val hasClient  = pathsWithExports.contains(s"${info.serviceDirPath}/client.ts")
        val hasWiring  = pathsWithExports.contains(s"${info.serviceDirPath}/wiring.ts")
        val hasService = pathsWithExports.contains(s"${info.serviceDirPath}/service.ts")

        val methodsBarrel: Option[TsDefnTranslator.Output] =
          if (methodDirs.isEmpty) None
          else {
            // Directory re-export: target the method dir's `index` explicitly. A bare `./<method>`
            // specifier does not resolve under strict ESM / NodeNext (no directory-index fallback).
            val lines = methodDirs.map(m => TextTree.text[TsValue](s"export * as $m from './$m/index$sfx';"))
            Some(barrelOutput(s"${info.serviceDirPath}/methods.ts", lines.reduce((a, b) => q"$a\n$b")))
          }

        val indexLines: List[TextTree[TsValue]] =
          (if (methodDirs.nonEmpty) List(TextTree.text[TsValue](s"export * as methods from './methods$sfx';")) else Nil) ++
            (if (hasService) List(TextTree.text[TsValue](s"export * from './service$sfx';")) else Nil) ++
            (if (hasClient) List(TextTree.text[TsValue](s"export * from './client$sfx';")) else Nil) ++
            (if (hasWiring) List(TextTree.text[TsValue](s"export * from './wiring$sfx';")) else Nil)

        val indexBarrel: Option[TsDefnTranslator.Output] =
          if (indexLines.isEmpty) None
          else Some(barrelOutput(s"${info.serviceDirPath}/index.ts", indexLines.reduce((a, b) => q"$a\n$b")))

        methodsBarrel.toList ++ indexBarrel.toList
    }

    // Extra `export * as <Service> from './<kebab>/index'` lines per models-level dir. The service
    // dir is a directory, so the specifier must target its `index` explicitly (strict ESM has no
    // directory-index fallback).
    val modelsExtras: Map[String, List[TextTree[TsValue]]] =
      infos
        .groupBy(_.modelsDirPath)
        .view
        .mapValues(_.sortBy(_.kebabName).map(i => TextTree.text[TsValue](s"export * as ${i.pascalName} from './${i.kebabName}/index$sfx';")))
        .toMap

    (serviceBarrels, modelsExtras)
  }

  private def generateBarrels(outputs: List[TsDefnTranslator.Output], family: BaboonFamily): List[TsDefnTranslator.Output] = {
    val sfx = target.language.importSuffix
    val definitionOutputs = outputs
      .filter(o => o.product == CompilerProduct.Definition || o.product == CompilerProduct.Runtime)
      .filterNot(_.isBarrel)
      .filter(_.path.endsWith(".ts"))

    val (serviceBarrels, modelsExtras) = generateServiceBarrels(outputs, family)
    // Service directories own a bespoke `index.ts` (methods namespace + service +
    // client/wiring), so the generic per-directory logic must skip them.
    val serviceDirs = serviceBarrels.iterator
      .filter(_.path.endsWith("/index.ts"))
      .map(_.path.stripSuffix("/index.ts"))
      .toSet

    // Group files by their direct parent directory
    val byDir = definitionOutputs.groupBy {
      o =>
        val idx = o.path.lastIndexOf('/')
        if (idx >= 0) o.path.substring(0, idx) else ""
    }

    // Generate per-directory barrels with collision detection.
    // Skip root directory (runtime-only files, not useful in barrel).
    val perDirBarrels = byDir.toList.filter(_._1.nonEmpty).filterNot(kv => serviceDirs.contains(kv._1)).sortBy(_._1).flatMap {
      case (dir, files) =>
        val sortedFiles = files.sortBy(_.path)

        // Collect exported names per file and detect collisions across files in this directory.
        // Files with NO exports (e.g. baboon-ref foreigns that map to a builtin and emit an
        // empty .ts) must be skipped from the barrel — `export * from './X'` against an empty
        // file produces TS2306 "File ... is not a module" on strict tsc/deno.
        val fileExports = sortedFiles.map(f => (f, exportedNames(f))).filter(_._2.nonEmpty)
        val nameCount   = fileExports.flatMap(_._2.keys).groupBy(identity).view.mapValues(_.size).toMap
        val colliding   = nameCount.filter(_._2 > 1).keySet

        val reexports = if (colliding.isEmpty) {
          // No collisions — simple export * for all files with exports
          fileExports.map {
            case (f, _) =>
              val fname = f.path.drop(dir.length + 1).stripSuffix(".ts")
              TextTree.text[TsValue](s"export * from './$fname$sfx';")
          }
        } else {
          // Has collisions — files with colliding names get individual qualified re-exports,
          // files without collisions get export *
          fileExports.flatMap {
            case (f, nameMap) =>
              val names        = nameMap.keySet
              val fname        = f.path.drop(dir.length + 1).stripSuffix(".ts")
              val myCollisions = names.intersect(colliding)
              if (myCollisions.isEmpty) {
                List(TextTree.text[TsValue](s"export * from './$fname$sfx';"))
              } else {
                val safeNames = names -- colliding
                val qualifier = fname.replace('.', '_').replace('-', '_').replace('/', '_')
                val safeExports = if (safeNames.nonEmpty) {
                  namedReexports(nameMap, safeNames, identity, s"./$fname$sfx")
                } else Nil
                val qualifiedExports = namedReexports(nameMap, myCollisions, n => s"$n as ${qualifier}_$n", s"./$fname$sfx")
                safeExports ++ qualifiedExports
              }
          }
        }

        // A models-level dir that contains services also re-exports each
        // service as a namespace (`export * as PetStore from './pet-store'`).
        val allReexports = reexports ++ modelsExtras.getOrElse(dir, Nil)

        if (allReexports.nonEmpty) {
          Some(barrelOutput(s"$dir/index.ts", allReexports.reduce((a, b) => q"$a\n$b")))
        } else None
    }

    // Models dirs that emit no per-directory barrel of their own (no top-level
    // model files, services only) still need an `index.ts` carrying the service
    // namespace re-exports.
    val handledDirs = byDir.keySet
    val orphanModelBarrels = modelsExtras.toList.collect {
      case (dir, lines) if dir.nonEmpty && !handledDirs.contains(dir) =>
        barrelOutput(s"$dir/index.ts", lines.reduce((a, b) => q"$a\n$b"))
    }

    perDirBarrels ++ orphanModelBarrels ++ serviceBarrels
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
  ): Out[List[TsDefnTranslator.Output]] = {
    for {
      convs <-
        F.flatSequenceAccumErrors {
          lineage.evolution.rules
            .filter(kv => toCurrent.contains(kv._1))
            .map {
              case (srcVer, rules) =>
                convTransFac(
                  srcDom = lineage.versions(srcVer.from),
                  domain = domain,
                  rules  = rules,
                  evo    = lineage.evolution,
                ).makeConvs
            }
        }
    } yield {
      val basename = tsFileTools.basename(domain, lineage.evolution)
      convs.toList.map {
        conv =>
          val outputPath  = s"$basename/${conv.fname}"
          val moduleParts = tsFileTools.definitionsBasePkg ++ outputPath.stripSuffix(".ts").split('/').toList
          val convModule  = TsModuleId(moduleParts)
          TsDefnTranslator.Output(
            outputPath,
            conv.conv,
            convModule,
            CompilerProduct.Conversion,
          )
      }
    }
  }
}
