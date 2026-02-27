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
  trans: TsTypeTranslator,
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
      rendered = (
        translated ++
          runtime ++
          fixture
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
    def baboonTypeImport(moduleId: TsModuleId, types: String): TextTree[TsValue] = {
      if (o.module.path.startsWith(tsFileTools.definitionsBasePkg)) {
        val baboonPkg    = o.module.path.dropWhile(part => tsFileTools.definitionsBasePkg.contains(part))
        val pathToModule = (0 until baboonPkg.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToModule${moduleId.path.mkString("")}'"
      } else {
        val pathToCommonParent = (0 until o.module.path.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToCommonParent${tsFileTools.definitionsBasePkg.mkString("/")}/${moduleId.path.mkString("")}'"
      }
    }

    def definitionImport(moduleId: TsModuleId, types: String): TextTree[TsValue] = {
      if (o.module.path.startsWith(tsFileTools.definitionsBasePkg)) {
        val baboonPkg = moduleId.path.dropWhile(part => o.module.path.contains(part))
        q"import {$types} from './${baboonPkg.mkString("/")}'"
      } else {
        val pathToCommonParent = (0 until o.module.path.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToCommonParent${moduleId.path.mkString("/")}'"
      }
    }

    def fixtureImport(moduleId: TsModuleId, types: String): TextTree[TsValue] = {
      if (o.module.path.startsWith(tsFileTools.fixturesBasePkg)) {
        val baboonPkg = moduleId.path.dropWhile(part => o.module.path.contains(part))
        q"import {$types} from './${baboonPkg.mkString("/")}'"
      } else {
        val baboonPkg    = moduleId.path.dropWhile(part => o.module.path.contains(part))
        val diff         = baboonPkg.size - 1
        val parentImport = (0 until diff).map(_ => "../").mkString("")
        q"import {$types} from '$parentImport${baboonPkg.mkString("/")}'"
      }
    }

    val usedTypes = o.tree.values.collect { case t: TsValue.TsType => t }
      .filterNot(_.moduleId.path.isEmpty)
      .filterNot(_.moduleId == o.module)
      .filterNot(_.predef)
      .distinct

    val typesByModule = usedTypes.groupBy(_.moduleId).toList.sortBy { case (moduleId, types) => moduleId.path.size + types.size }.reverse

    val importsByModule =
      typesByModule.map {
        case (moduleId, types) =>
          val typesString = types.map {
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
      case TsValue.TsType(_, _, Some(alias), _) => alias
      case t: TsValue.TsType                    => t.name
    }
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
  ): Out[List[TsDefnTranslator.Output]] = {
    val module = trans.toTsModule(domain.id, domain.version, lineage.evolution, tsFileTools.definitionsBasePkg)

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
