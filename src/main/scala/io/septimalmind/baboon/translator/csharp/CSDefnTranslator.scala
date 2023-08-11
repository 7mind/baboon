package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.TextTree
import io.septimalmind.baboon.translator.TextTree.*
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.typer.model.{Domain, DomainMember, Typedef}
import izumi.fundamentals.collections.nonempty.NonEmptyList

trait CSDefnTranslator {
  def translate(defn: DomainMember.User, domain: Domain): Either[NonEmptyList[
    BaboonIssue.TranslationIssue
  ], List[CSDefnTranslator.Output]]
}

object CSDefnTranslator {

  case class Output(path: String, tree: TextTree[CSValue])

  class CSDefnTranslatorImpl() extends CSDefnTranslator {
    type Out[T] = Either[NonEmptyList[BaboonIssue.TranslationIssue], T]

    override def translate(
      defn: DomainMember.User,
      domain: Domain
    ): Either[NonEmptyList[BaboonIssue.TranslationIssue], List[Output]] = {

      val verString = "v" + domain.version.version
        .split('.')
        .mkString("_")

      val pkg =
        defn.id.pkg.path.map(_.capitalize) :+ verString

      val name = CSType(CSPackageId(pkg), defn.id.name.name.capitalize)

      val defnRepr = defn.defn match {
        case d: Typedef.Dto =>
          q""

        case e: Typedef.Enum =>
          val branches =
            e.members.map(m => q"""${m.name.capitalize}""").toSeq.join(",\n")

          q"""enum $name {
             |${branches.shift(4)}
             |}""".stripMargin

        case a: Typedef.Adt =>
          q""
      }

      val fbase =
        (defn.id.pkg.path.map(_.capitalize) ++ Seq(domain.version.version))
          .mkString("-")

      val fname = s"${defn.id.name.name.capitalize}.cs"

      val ns = pkg.mkString(".")
      val content =
        q"""namespace ${ns} {
           |${defnRepr.shift(4)}
           |}""".stripMargin

      Right(List(Output(s"$fbase/$fname", content)))
    }
  }

}
