package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.TextTree
import io.septimalmind.baboon.translator.TextTree.*
import io.septimalmind.baboon.typer.model.*
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
      val trans = new CSTypeTranslator(domain)
      val name = trans.toCsVal(defn.id)

      val defnRepr = defn.defn match {
        case d: Typedef.Dto =>
          val outs = d.fields.map { f =>
            val tpe = trans.asCsType(f.tpe)
            val fname = s"_${f.name.name}"
            val mname = s"${f.name.name.capitalize}"
            (q"""private readonly $tpe ${fname};""", q"""public $tpe ${mname}
                 |{
                 |    return ${fname};
                 |}""".stripMargin, (fname, tpe))
          }
          val fields = outs.map(_._1)
          val methods = outs.map(_._2)

          val cargs = outs
            .map(_._3)
            .map {
              case (fname, ftpe) =>
                q"${ftpe} $fname"
            }
            .join(", ")

          val inits = outs
            .map(_._3)
            .map {
              case (fname, ftpe) =>
                q"this.$fname = $fname;"
            }
            .join("\n")

          val constructor =
            q"""public ${name.name}($cargs) {
               |${inits.shift(4)}
               |}""".stripMargin

          q"""public class $name {
             |${fields.join("\n").shift(4)}
             |
             |${constructor.shift(4)}
             |
             |${methods.join("\n").shift(4)}
             |}""".stripMargin

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

      val ns = name.pkg.parts.mkString(".")
      val content =
        q"""namespace ${ns} {
           |${defnRepr.shift(4)}
           |}""".stripMargin

      Right(List(Output(s"$fbase/$fname", content)))
    }

  }

}
