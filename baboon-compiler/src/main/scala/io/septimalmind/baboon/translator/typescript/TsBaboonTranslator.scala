package io.septimalmind.baboon.translator.typescript

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.typescript.TsTypes.{tsBaboonRuntimeShared, tsFixtureShared}
import io.septimalmind.baboon.translator.typescript.TsValue.{TsModuleId, TsType}
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
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
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime
      fixture    <- sharedFixture
      barrels     = generateBarrels(translated ++ runtime)
      rendered = (
        translated ++
          runtime ++
          fixture ++
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

  private def sharedRuntime: Out[List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          TsDefnTranslator.Output(
            "BaboonSharedRuntime.ts",
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonSharedRuntime.ts")),
            tsBaboonRuntimeShared,
            CompilerProduct.Runtime,
            doNotModify = true,
          )
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
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonSharedFixture.ts")),
            tsBaboonRuntimeShared,
            CompilerProduct.Runtime,
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
      if (o.module.path.startsWith(tsFileTools.definitionsBasePkg)) {
        val afterBase    = o.module.path.drop(tsFileTools.definitionsBasePkg.size)
        val pathToModule = (0 until afterBase.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToModule${moduleId.path.mkString("")}$sfx'"
      } else {
        val pathToCommonParent = (0 until o.module.path.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToCommonParent${tsFileTools.definitionsBasePkg.mkString("/")}/${moduleId.path.mkString("")}$sfx'"
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
      typesByModule.map {
        case (moduleId, types) =>
          val typesString = types.map {
            case t if aliasMap.contains(t)       => s"${t.name} as ${aliasMap(t)}"
            case TsType(_, name, Some(alias), _) => s"$name as $alias"
            case t: TsValue.TsType               => t.name
          }.mkString(", ")
          if (moduleId.path.startsWith(tsFileTools.definitionsBasePkg)) {
            definitionImport(moduleId, typesString)
          } else if (moduleId.path.startsWith(tsFileTools.fixturesBasePkg) && tsFileTools.fixturesBasePkg.nonEmpty) {
            fixtureImport(moduleId, typesString)
          } else if (moduleId == tsBaboonRuntimeShared || moduleId == tsFixtureShared) {
            baboonTypeImport(moduleId, typesString)
          } else {
            q"import {$typesString} from '${moduleId.path.mkString("/")}'"
          }
      }

    val allImports = importsByModule.joinN()

    val full = Seq(allImports, o.tree).joinNN()

    full.mapRender {
      case t: TsValue.TsType if aliasMap.contains(t) => aliasMap(t)
      case TsValue.TsType(_, _, Some(alias), _)      => alias
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
            val distinguishing = path.drop(commonLen).dropRight(1)
            val prefix =
              if (distinguishing.nonEmpty) distinguishing.mkString("_")
              else path.dropRight(1).lastOption.getOrElse("m")
            t -> s"${prefix}_$name"
        }
    }
  }

  /** Extract type names that a file defines (types whose module matches the file's own module). */
  private def exportedNames(output: TsDefnTranslator.Output): Set[String] = {
    output.tree.values.collect { case t: TsValue.TsType if t.moduleId == output.module && !t.predef => t.name }.toSet
  }

  private def generateBarrels(outputs: List[TsDefnTranslator.Output]): List[TsDefnTranslator.Output] = {
    val sfx = target.language.importSuffix
    val definitionOutputs = outputs
      .filter(o => o.product == CompilerProduct.Definition || o.product == CompilerProduct.Runtime)
      .filterNot(_.isBarrel)
      .filter(_.path.endsWith(".ts"))

    // Group files by their direct parent directory
    val byDir = definitionOutputs.groupBy {
      o =>
        val idx = o.path.lastIndexOf('/')
        if (idx >= 0) o.path.substring(0, idx) else ""
    }

    // Generate per-directory barrels with collision detection.
    // Skip root directory (runtime-only files, not useful in barrel).
    val perDirBarrels = byDir.toList.filter(_._1.nonEmpty).sortBy(_._1).flatMap {
      case (dir, files) =>
        val sortedFiles = files.sortBy(_.path)

        // Collect exported names per file and detect collisions across files in this directory
        val fileExports = sortedFiles.map(f => (f, exportedNames(f)))
        val nameCount   = fileExports.flatMap(_._2).groupBy(identity).view.mapValues(_.size).toMap
        val colliding   = nameCount.filter(_._2 > 1).keySet

        val reexports = if (colliding.isEmpty) {
          // No collisions — simple export * for all files
          sortedFiles.map {
            f =>
              val fname = f.path.drop(dir.length + 1).stripSuffix(".ts")
              TextTree.text[TsValue](s"export * from './$fname$sfx';")
          }
        } else {
          // Has collisions — files with colliding names get individual qualified re-exports,
          // files without collisions get export *
          fileExports.flatMap {
            case (f, names) =>
              val fname        = f.path.drop(dir.length + 1).stripSuffix(".ts")
              val myCollisions = names.intersect(colliding)
              if (myCollisions.isEmpty) {
                List(TextTree.text[TsValue](s"export * from './$fname$sfx';"))
              } else {
                val safeNames = names -- colliding
                val qualifier = fname.replace('.', '_').replace('-', '_').replace('/', '_')
                val safeExport = if (safeNames.nonEmpty) {
                  List(TextTree.text[TsValue](s"export { ${safeNames.toList.sorted.mkString(", ")} } from './$fname$sfx';"))
                } else Nil
                val qualifiedExports = myCollisions.toList.sorted.map {
                  name =>
                    TextTree.text[TsValue](s"export { $name as ${qualifier}_$name } from './$fname$sfx';")
                }
                safeExport ++ qualifiedExports
              }
          }
        }

        if (reexports.nonEmpty) {
          val barrelPath   = s"$dir/index.ts"
          val barrelModule = TsModuleId(tsFileTools.definitionsBasePkg ++ barrelPath.stripSuffix(".ts").split('/').toList)
          val tree         = reexports.reduce((a, b) => q"$a\n$b")
          Some(
            TsDefnTranslator.Output(
              barrelPath,
              tree,
              barrelModule,
              CompilerProduct.Definition,
              doNotModify = true,
              isBarrel    = true,
            )
          )
        } else None
    }

    perDirBarrels
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
