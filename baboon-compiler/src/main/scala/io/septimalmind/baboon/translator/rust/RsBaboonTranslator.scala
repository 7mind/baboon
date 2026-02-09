package io.septimalmind.baboon.translator.rust

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.escapeRustKeyword
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class RsBaboonTranslator[F[+_, +_]: Error2](
  trans: RsTypeTranslator,
  convTransFac: RsConversionTranslator.Factory[F],
  defnTranslator: Subcontext[RsDefnTranslator[F]],
  target: RsTarget,
  rsFiles: RsFileTools,
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime()
      fixture    <- sharedFixture()

      // Detect type/namespace conflicts: `foo.rs` conflicts with directory `foo/`
      // In Rust you can't have both — merge the type content into `foo/mod.rs`
      allPaths = translated.filterNot(_.doNotModify).map(_.path).toSet
      dirPaths = allPaths.flatMap { p =>
        val parts = p.split('/').toList
        (1 until parts.size).map(i => parts.take(i).mkString("/"))
      }
      conflicting = translated.filter { o =>
        !o.doNotModify && dirPaths.contains(o.path.stripSuffix(".rs"))
      }
      conflictPaths = conflicting.map(_.path).toSet
      normal = translated.filterNot(o => conflictPaths.contains(o.path))

      allModFiles <- generateModFiles(translated, conflicting)
      modFiles = allModFiles.filterNot(_.path == "mod.rs")
      libFile = generateLibRs(normal ++ runtime ++ fixture ++ modFiles)
      rendered = (normal ++ runtime ++ fixture ++ modFiles ++ libFile).map {
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
    F.flatSequenceAccumErrors {
      lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: DomainMember.User => F[NEList[BaboonIssue], List[RsDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[RsDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[RsDefnTranslator.Output]] = {
    val evo = lineage.evolution
    defnTranslator.provide(domain).provide(evo).produce().use {
      defnTranslator =>
        for {
          defnSources     <- translateProduct(domain, CompilerProduct.Definition, defnTranslator.translate)
          fixturesSources <- translateProduct(domain, CompilerProduct.Fixture, defnTranslator.translateFixtures)
          testsSources    <- translateProduct(domain, CompilerProduct.Test, defnTranslator.translateTests)

          conversionSources <- {
            if (target.output.products.contains(CompilerProduct.Conversion)) {
              val evosToCurrent = evo.diffs.keySet.filter(_.to == domain.version)
              generateConversions(domain, lineage, evosToCurrent)
            } else {
              F.pure(List.empty)
            }
          }
        } yield {
          defnSources ++ conversionSources ++ fixturesSources ++ testsSources
        }
    }
  }

  private def generateModFiles(
    outputs: List[RsDefnTranslator.Output],
    conflicting: List[RsDefnTranslator.Output],
  ): Out[List[RsDefnTranslator.Output]] = {
    val allPaths      = outputs.filterNot(_.doNotModify).map(_.path)
    val conflictByDir = conflicting.groupBy(_.path.stripSuffix(".rs"))
    val mods          = generateModFilesForPaths(allPaths, conflictByDir, CompilerProduct.Definition)
    F.pure(mods)
  }

  private def generateModFilesForPaths(
    paths: List[String],
    conflictContent: Map[String, List[RsDefnTranslator.Output]],
    product: CompilerProduct,
  ): List[RsDefnTranslator.Output] = {
    if (paths.isEmpty) return Nil

    val allDirs = scala.collection.mutable.Set.empty[String]

    val filesByDir = paths.groupBy { path =>
      val parts = path.split('/').toList
      if (parts.size > 1) parts.init.mkString("/") else ""
    }

    paths.foreach { path =>
      val parts = path.split('/').toList
      for (i <- 1 until parts.size) {
        allDirs += parts.take(i).mkString("/")
      }
      if (parts.size == 1) allDirs += ""
    }

    allDirs.toList.sorted.map { dir =>
      val prefix = if (dir.isEmpty) "" else dir + "/"

      // Direct child directories of this directory
      val childDirs = allDirs.filter { d =>
        d.startsWith(prefix) && d != dir && !d.drop(prefix.length).contains('/')
      }.map(_.drop(prefix.length)).toSet

      // File modules in this directory, EXCLUDING those that clash with child directories
      val fileModNames = filesByDir.getOrElse(dir, Nil).map { file =>
        file.split('/').last.stripSuffix(".rs")
      }.sorted.distinct.filterNot(childDirs.contains)

      val allModNames = (fileModNames ++ childDirs.toList).sorted.distinct
      val modDecls = allModNames.flatMap { name =>
        val escaped = escapeRustKeyword(name)
        if (childDirs.contains(name)) {
          // Directory module: declare but don't re-export (avoids name clashes with versioned modules)
          List(q"pub mod $escaped;")
        } else {
          // File module: declare and re-export so types/functions are accessible from parent
          List(q"pub mod $escaped;", q"pub use $escaped::*;")
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
            TextTree.text(IzResources.readAsString("baboon-runtime/rust/baboon_runtime.rs").get),
            RsValue.RsCrateId(NEList("crate")),
            CompilerProduct.Runtime,
            doNotModify = true,
          )
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
            TextTree.text(IzResources.readAsString("baboon-runtime/rust/baboon_fixture.rs").get),
            RsValue.RsCrateId(NEList("crate")),
            CompilerProduct.FixtureRuntime,
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

      val imports = usedTypes.map { t =>
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

  private def generateLibRs(allOutputs: List[RsDefnTranslator.Output]): List[RsDefnTranslator.Output] = {
    val topLevelModules = allOutputs
      .filterNot(_.isModFile)
      .filterNot(_.path == "lib.rs")
      .map { o =>
        val first = o.path.split('/').head
        first.stripSuffix(".rs")
      }
      .distinct
      .sorted

    val allows = List(
      q"#![allow(dead_code)]",
      q"#![allow(unused_imports)]",
      q"#![allow(unused_variables)]",
      q"#![allow(non_camel_case_types)]",
      q"#![allow(non_snake_case)]",
    )

    val modDecls = topLevelModules.map { name =>
      val escaped = escapeRustKeyword(name)
      q"pub mod $escaped;"
    }

    val tree = (allows ++ modDecls).joinN()

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
