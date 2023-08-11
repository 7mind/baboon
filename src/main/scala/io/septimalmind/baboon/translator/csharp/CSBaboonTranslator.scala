package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{
  AbstractBaboonTranslator,
  Sources,
  TextTree
}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NonEmptyList
import TextTree.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId

class CSBaboonTranslator() extends AbstractBaboonTranslator {

  type Out[T] = Either[NonEmptyList[BaboonIssue.TranslationIssue], T]

  private val defnTranslator = new CSDefnTranslator.CSDefnTranslatorImpl()

  override def translate(family: BaboonFamily): Out[Sources] = {
    for {
      translated <- doTranslate(family)
      rendered = translated.map { case (p, v) => (p, renderTree(v)) }
      unique <- rendered.toUniqueMap(
        c => NonEmptyList(BaboonIssue.NonUniqueOutputFiles(c))
      )
    } yield {
      Sources(unique)
    }
  }

  private def renderTree(v: TextTree[CSValue]): String = {
    val requiredPackages = Set(
      CSPackageId(NonEmptyList("System")),
      CSPackageId(NonEmptyList("System", "Collections", "Generic")),
      CSPackageId(NonEmptyList("System", "Linq")),
    )

    val usedPackages = v.values.collect {
      case t: CSValue.CSType => t.pkg
    }.toSet

    val allPackages = (requiredPackages ++ usedPackages).diff(Set())

    val imports = allPackages.toSeq
      .map { p =>
        q"using ${p.parts.mkString(".")};"
      }
      .join("\n")

    val full =
      q"""#nullable enable
         |
         |$imports
         |
         |$v""".stripMargin

    full.mapRender {
      case t: CSValue.CSType =>
        t.name
    }
  }

  private def doTranslate(
    family: BaboonFamily
  ): Out[List[(String, TextTree[CSValue])]] = {
    // TODO: fix .toSeq.toList
    family.domains.toSeq.toList.map {
      case (_, lineage) =>
        translateLineage(lineage)
    }.biFlatAggregate
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[(String, TextTree[CSValue])]] = {
    lineage.versions.toSeq.toList.map {
      case (version, domain) =>
        val evo = if (lineage.evolution.latest == version) {
          Some(lineage.evolution)
        } else {
          None
        }
        translateDomain(domain, evo)
    }.biFlatAggregate
  }

  private def translateDomain(
    domain: Domain,
    evo: Option[BaboonEvolution]
  ): Out[List[(String, TextTree[CSValue])]] = {
    for {
      defnSources <- domain.defs.meta.nodes.toList.map {
        case (_, defn: DomainMember.User) =>
          defnTranslator.translate(defn, domain)
        case _ => Right(List.empty)
      }.biFlatAggregate
      conversionSources <- evo match {
        case Some(value) =>
          generateConversions(domain, value)
        case None =>
          Right(List.empty)
      }
    } yield {
      defnSources.map(o => (o.path, o.tree)) ++ conversionSources
    }
  }

  private def generateConversions(
    domain: Domain,
    value: BaboonEvolution
  ): Out[List[(String, TextTree[CSValue])]] = {
    // TODO
    Right(List.empty)
  }
}
