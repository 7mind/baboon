package io.septimalmind.baboon.translator.typescript

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class TsBaboonTranslator[F[+_, +_]: Error2](
  trans: TsTypeTranslator,
  convTransFac: TsConversionTranslator.Factory[F],
  defnTranslator: Subcontext[TsDefnTranslator[F]],
  target: TsTarget,
  tsFiles: TsFileTools,
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime()
      fixture    <- sharedFixture()

      allOutputs = translated ++ runtime ++ fixture

      rendered = allOutputs.map {
        o =>
          val content = renderTree(o, allOutputs)
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
    F.flatSequenceAccumErrors {
      lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
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

  private def sharedRuntime(): Out[List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          TsDefnTranslator.Output(
            "baboon_runtime.ts",
            TextTree.text(IzResources.readAsString("baboon-runtime/typescript/baboon_runtime.ts").get),
            TsValue.TsModuleId(NEList("baboon_runtime")),
            CompilerProduct.Runtime,
            doNotModify = true,
          )
        )
      )
    } else {
      F.pure(List.empty)
    }
  }

  private def sharedFixture(): Out[List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      F.pure(
        List(
          TsDefnTranslator.Output(
            "baboon_fixture.ts",
            TextTree.text(IzResources.readAsString("baboon-runtime/typescript/baboon_fixture.ts").get),
            TsValue.TsModuleId(NEList("baboon_fixture")),
            CompilerProduct.FixtureRuntime,
            doNotModify = true,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def generateBarrelFiles(outputs: List[TsDefnTranslator.Output]): List[TsDefnTranslator.Output] = {
    val allPaths = outputs.filter(o => !o.doNotModify && o.product == CompilerProduct.Definition).map(_.path)
    if (allPaths.isEmpty) return Nil

    val allDirs = scala.collection.mutable.Set.empty[String]

    val filesByDir = allPaths.groupBy { path =>
      val parts = path.split('/').toList
      if (parts.size > 1) parts.init.mkString("/") else ""
    }

    allPaths.foreach { path =>
      val parts = path.split('/').toList
      for (i <- 1 until parts.size) {
        allDirs += parts.take(i).mkString("/")
      }
      if (parts.size == 1) allDirs += ""
    }

    allDirs.toList.sorted.map { dir =>
      val prefix = if (dir.isEmpty) "" else dir + "/"

      val childDirs = allDirs.filter { d =>
        d.startsWith(prefix) && d != dir && !d.drop(prefix.length).contains('/')
      }.map(_.drop(prefix.length)).toSet

      val fileModNames = filesByDir.getOrElse(dir, Nil).map { file =>
        file.split('/').last.stripSuffix(".ts")
      }.sorted.distinct

      val fileExports = fileModNames.map { name =>
        q"""export * from "./$name";"""
      }

      val versionedDirPattern = "v\\d+_\\d+(_\\d+)?".r
      val fileModNameSet = fileModNames.toSet
      val dirExports = childDirs.toList.sorted
        .filterNot(fileModNameSet.contains)
        .filterNot(name => versionedDirPattern.matches(name))
        .map { name =>
          q"""export * from "./$name/index";"""
        }

      val barrelTree = (fileExports ++ dirExports).joinN()
      val barrelPath = if (dir.isEmpty) "index.ts" else s"$dir/index.ts"

      TsDefnTranslator.Output(
        barrelPath,
        barrelTree,
        TsValue.TsModuleId(NEList("index")),
        CompilerProduct.Definition,
        isBarrel = true,
      )
    }
  }

  private def renderTree(o: TsDefnTranslator.Output, allOutputs: List[TsDefnTranslator.Output]): String = {
    if (o.doNotModify || o.isBarrel) {
      o.tree.mapRender {
        case t: TsValue.TsType     => t.name
        case t: TsValue.TsTypeName => t.name
      }
    } else {
      val selfDir = {
        val parts = o.path.split('/').toList
        if (parts.size > 1) parts.init.mkString("/") else ""
      }
      val selfBase = o.path.stripSuffix(".ts")

      val usedTypes = o.tree.values.collect { case t: TsValue.TsType => t }.distinct
        .filterNot(_.predef)

      val runtimeImports = usedTypes.filter(t => t.module == TsTypes.runtimeModule).map(_.name).sorted.distinct
      val fixtureImports = usedTypes.filter(t => t.module == TsTypes.fixtureModule).map(_.name).sorted.distinct

      val userTypes = usedTypes.filterNot(t =>
        t.module == TsTypes.runtimeModule ||
        t.module == TsTypes.fixtureModule ||
        t.module == TsTypes.predefModule
      )

      val userImportsByModule = userTypes.groupBy(_.module).toList.sortBy(_._1.parts.mkString("/"))

      // Build alias map: when the same name appears from multiple modules, alias the non-primary ones
      val nameToModules = userTypes.groupBy(_.name).filter(_._2.map(_.module).distinct.size > 1)
      val aliasMap = scala.collection.mutable.Map.empty[(TsValue.TsModuleId, String), String]

      nameToModules.foreach { case (name, types) =>
        val modules = types.map(_.module).distinct.sortBy(_.parts.mkString("/"))
        // The last module (typically the latest/current version) keeps the original name
        modules.init.foreach { mod =>
          val suffix = mod.parts.last.replace('-', '_')
          aliasMap((mod, name)) = s"${name}__${suffix}"
        }
      }

      val runtimeImportLine = if (runtimeImports.nonEmpty) {
        val relPath = computeRelativePath(selfDir, "baboon_runtime")
        List(q"""import { ${runtimeImports.mkString(", ")} } from "$relPath";""")
      } else Nil

      val fixtureImportLine = if (fixtureImports.nonEmpty) {
        val relPath = computeRelativePath(selfDir, "baboon_fixture")
        List(q"""import { ${fixtureImports.mkString(", ")} } from "$relPath";""")
      } else Nil

      val userImportLines = userImportsByModule.flatMap { case (mod, types) =>
        val targetFile = findModuleFile(mod, allOutputs)
        targetFile.flatMap { tf =>
          val targetBase = tf.stripSuffix(".ts")
          if (targetBase == selfBase) None
          else {
            val relPath = computeRelativePath(selfDir, targetBase)
            val importParts = types.map(_.name).sorted.distinct.map { name =>
              aliasMap.get((mod, name)) match {
                case Some(alias) => s"$name as $alias"
                case None        => name
              }
            }
            Some(q"""import { ${importParts.mkString(", ")} } from "$relPath";""")
          }
        }
      }

      val allImports = (runtimeImportLine ++ fixtureImportLine ++ userImportLines).joinN()
      val full = Seq(allImports, o.tree).joinNN()

      full.mapRender {
        case t: TsValue.TsType =>
          aliasMap.getOrElse((t.module, t.name), t.name)
        case t: TsValue.TsTypeName => t.name
      }
    }
  }

  private def findModuleFile(mod: TsValue.TsModuleId, allOutputs: List[TsDefnTranslator.Output]): Option[String] = {
    val modPath = mod.parts.mkString("/") + ".ts"
    allOutputs.find(_.path == modPath).map(_.path).orElse {
      val barrelPath = mod.parts.mkString("/") + "/index.ts"
      allOutputs.find(_.path == barrelPath).map(_ => mod.parts.mkString("/") + "/index")
    }
  }

  private def computeRelativePath(fromDir: String, toPath: String): String = {
    val fromParts = if (fromDir.isEmpty) Array.empty[String] else fromDir.split('/')
    val toParts   = toPath.split('/')

    var common = 0
    while (common < fromParts.length && common < toParts.length && fromParts(common) == toParts(common)) {
      common += 1
    }

    val ups = fromParts.length - common
    val downs = toParts.drop(common)

    val prefix = if (ups == 0) "./" else "../" * ups
    prefix + downs.mkString("/")
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
  ): Out[List[TsDefnTranslator.Output]] = {
    val module = trans.toTsModule(domain.id, domain.version, lineage.evolution)

    for {
      convs <-
        F.flatSequenceAccumErrors {
          lineage.evolution.rules
            .filter(kv => toCurrent.contains(kv._1))
            .map {
              case (srcVer, rules) =>
                convTransFac(
                  module = module,
                  srcDom = lineage.versions(srcVer.from),
                  domain = domain,
                  rules  = rules,
                  evo    = lineage.evolution,
                ).makeConvs
            }
        }
    } yield {
      val basename = tsFiles.basename(domain, lineage.evolution)
      convs.toList.map {
        conv =>
          TsDefnTranslator.Output(
            s"$basename/${conv.fname}",
            conv.conv,
            module,
            CompilerProduct.Conversion,
          )
      }
    }
  }
}
