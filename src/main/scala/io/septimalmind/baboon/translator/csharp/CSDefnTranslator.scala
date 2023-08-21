package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.TextTree
import io.septimalmind.baboon.translator.TextTree.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NonEmptyList

trait CSDefnTranslator {
  def translate(defn: DomainMember.User, domain: Domain): Either[NonEmptyList[
    BaboonIssue.TranslationIssue
  ], List[CSDefnTranslator.Output]]

  def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue]

  def basename(dom: Domain): String
}

object CSDefnTranslator {

  case class Output(path: String, tree: TextTree[CSValue], pkg: CSPackageId)

  class CSDefnTranslatorImpl() extends CSDefnTranslator {
    type Out[T] = Either[NonEmptyList[BaboonIssue.TranslationIssue], T]

    override def translate(
      defn: DomainMember.User,
      domain: Domain
    ): Either[NonEmptyList[BaboonIssue.TranslationIssue], List[Output]] = {
      val trans = new CSTypeTranslator()
      val name = trans.toCsVal(defn.id, domain.version)

      val defnRepr = defn.defn match {
        case d: Typedef.Dto =>
          val outs = d.fields.map { f =>
            val tpe = trans.asCsRef(f.tpe, domain.version)
            val fname = s"_${f.name.name}"
            val mname = s"${f.name.name.capitalize}"
            (q"""private readonly $tpe ${fname};""", q"""public $tpe ${mname}()
                 |{
                 |    return this.${fname};
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
              case (fname, _) =>
                q"this.$fname = $fname;"
            }
            .join("\n")

          val constructor =
            q"""public ${name.name}($cargs) {
               |${inits.shift(4)}
               |}""".stripMargin

          val parent = d.id.owner match {
            case Owner.Toplevel =>
              q""
            case Owner.Adt(id) =>
              val parentId = trans.asCsType(id, domain.version)
              q": $parentId"
          }

          q"""public class $name$parent {
             |${fields.join("\n").shift(4)}
             |
             |${constructor.shift(4)}
             |
             |${methods.join("\n").shift(4)}
             |}""".stripMargin

//          d.id.owner match {
//            case Owner.Toplevel =>
//              clz
//            case Owner.Adt(id) =>
//              val adtns = id.name.name.toLowerCase
//              q"""namespace $adtns {
//                 |${clz.shift(4)}
//                 |}""".stripMargin
//          }

        case e: Typedef.Enum =>
          val branches =
            e.members.map(m => q"""${m.name.capitalize}""").toSeq.join(",\n")

          q"""public enum $name {
             |${branches.shift(4)}
             |}""".stripMargin

        case _: Typedef.Adt =>
          q"""public interface $name {
             |}""".stripMargin
      }

      assert(defn.id.pkg == domain.id)
      val fbase =
        basename(domain)

      val fname = s"${defn.id.name.name.capitalize}.cs"

      val ns = name.pkg.parts

      val content = inNs(ns.toSeq, defnRepr)

      val outname = defn.defn.id.owner match {
        case Owner.Toplevel =>
          s"$fbase/$fname"
        case Owner.Adt(id) =>
          s"$fbase/${id.name.name.toLowerCase}-$fname"
      }
      Right(
        List(Output(outname, content, trans.toCsPkg(domain.id, domain.version)))
      )
    }

    def basename(dom: Domain): String = {
      (dom.id.path.map(_.capitalize) ++ Seq(dom.version.version))
        .mkString("-")
    }

    private def inNs(name: String,
                     tree: TextTree[CSValue]): TextTree[CSValue] = {
      q"""namespace ${name} {
         |${tree.shift(4)}
         |}""".stripMargin
    }

    def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue] = {
      nss.foldRight(tree) {
        case (ns, acc) =>
          inNs(ns, acc)
      }
    }
  }

}
