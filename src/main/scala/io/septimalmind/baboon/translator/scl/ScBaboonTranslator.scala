package io.septimalmind.baboon.translator.scl

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSTypes.{csCollectionsGenericPkg, csCollectionsImmutablePkg, csLinqPkg, csSystemPkg}
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.translator.csharp.{CSDefnTranslator, CSTypes, CSValue}
import io.septimalmind.baboon.translator.scl.ScValue.ScPackageId
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources, scl}
import io.septimalmind.baboon.typer.model.{BaboonFamily, BaboonLineage, Domain, DomainMember}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class ScBaboonTranslator[F[+_, +_]: Error2](
  translator: Subcontext[ScDefnTranslator[F]],
  target: ScTarget,
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue.TranslationIssue], T]

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue.TranslationIssue], Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime()
      fixture    <- sharedFixture()
      rendered = (translated ++ runtime ++ fixture).map {
        o =>
          val content = renderTree(o)
          (o.path, OutputFile(content, o.product))
      }
      unique <- F.fromEither(rendered.toUniqueMap(c => NEList(BaboonIssue.NonUniqueOutputFiles(c))))
    } yield {
      Sources(unique)
    }
  }

  private def translateFamily(
    family: BaboonFamily
  ): Out[List[ScDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[ScDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: (DomainMember.User) => F[NEList[BaboonIssue.TranslationIssue], List[ScDefnTranslator.Output]],
  ): F[NEList[BaboonIssue.TranslationIssue], List[ScDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Definition)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[ScDefnTranslator.Output]] = {
    val evo = lineage.evolution
    translator.provide(domain).provide(evo).produce().use {
      defnTranslator =>
        // TODO:
        for {
          defnSources <- translateProduct(domain, CompilerProduct.Definition, defnTranslator.translate)
        } yield {
          defnSources
        }
    }
  }

  private def sharedFixture(): Out[List[ScDefnTranslator.Output]] = {
    F.pure(List.empty)

  }

  private def renderTree(o: ScDefnTranslator.Output): String = {
//    val alwaysAvailable: Set[ScPackageId] = Set.empty
//    val forcedUses: Set[ScPackageId]      = Set.empty

    val usedTypes = o.tree.values.collect { case t: ScValue.ScType => t }.distinct
      .sortBy(_.toString) // TODO: dirty

//    val available = Set(o.pkg)
//    val requiredPackages = Set.empty
//    val allPackages      =  //(requiredPackages ++ usedPackages ++ forcedUses).diff(available ++ alwaysAvailable)

    val imports = usedTypes.toSeq.map {
      p => q"import ${p.pkg.parts.mkString(".")}.${p.name}"
    }.join("\n")

//    println(s"${o.path}: ${usedTypes.size} / ${o.doNotModify}")
//    if (o.path.contains("Clash.scala")) {
//      println(imports)
//    }

    val full = if (o.doNotModify) {
      o.tree
    } else {
      Seq(
        Seq(imports),
        Seq(o.tree),
      ).flatten.join("\n\n")
    }

    full.mapRender {
      case t: ScValue.ScTypeName =>
        t.name

      case t: ScValue.ScType if !t.fq =>
        if (o.pkg == t.pkg || !t.pkg.parts.startsWith(o.pkg.parts)) {
          t.name
        } else {
          (t.pkg.parts :+ t.name).mkString(".")
        }

      case t: ScValue.ScType =>
        (t.pkg.parts :+ t.name).mkString(".")
    }
  }

  private def sharedRuntime(): Out[List[scl.ScDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      val sharedOutput = ScDefnTranslator.Output(
        s"BaboonRuntimeShared.scala",
        TextTree.text(IzResources.readAsString("baboon-runtime/scala/BaboonRuntimeShared.scala").get),
        ScTypes.baboonRuntimePkg,
        CompilerProduct.Runtime,
        doNotModify = true,
      )
      F.pure(List(sharedOutput))
    } else {
      F.pure(List.empty)
    }
  }
}
