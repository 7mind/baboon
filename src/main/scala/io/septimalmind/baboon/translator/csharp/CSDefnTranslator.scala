package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.TextTree
import io.septimalmind.baboon.translator.TextTree.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.ComparatorType
import izumi.fundamentals.collections.nonempty.NonEmptyList

trait CSDefnTranslator {
  def translate(defn: DomainMember.User,
                domain: Domain,
                evo: BaboonEvolution,
  ): Either[NonEmptyList[BaboonIssue.TranslationIssue], List[
    CSDefnTranslator.Output
  ]]

  def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue]

  def basename(dom: Domain): String
}

object CSDefnTranslator {

  case class Output(path: String, tree: TextTree[CSValue], pkg: CSPackageId)

  class CSDefnTranslatorImpl(options: CompilerOptions)
      extends CSDefnTranslator {
    type Out[T] = Either[NonEmptyList[BaboonIssue.TranslationIssue], T]
    private val trans = new CSTypeTranslator()

    override def translate(defn: DomainMember.User,
                           domain: Domain,
                           evo: BaboonEvolution,
    ): Either[NonEmptyList[BaboonIssue.TranslationIssue], List[Output]] = {
      val name = trans.toCsVal(defn.id, domain.version)

      val defnReprBase = makeRepr(defn, domain, name)
      val isLatestVersion = domain.version == evo.latest
      val defnRepr = if (isLatestVersion) {
        defnReprBase
      } else {
        q"""[Obsolete("Version ${domain.version.version} is obsolete, you should migrate to ${evo.latest.version}", ${options.obsoleteErrors.toString})]
           |$defnReprBase""".stripMargin
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

    private def makeRepr(defn: DomainMember.User,
                         domain: Domain,
                         name: CSValue.CSType): TextTree[CSValue] = {
      defn.defn match {
        case d: Typedef.Dto =>
          val outs = d.fields.map { f =>
            val tpe = trans.asCsRef(f.tpe, domain.version)
            val fname = s"_${f.name.name}"
            val mname = s"${f.name.name.capitalize}"
            val fieldDef = q"""private readonly $tpe ${fname};"""
            val methodDef =
              q"""public $tpe ${mname}
                 |{
                 |    get { return this.${fname}; }
                 |}""".stripMargin
            (fieldDef, methodDef, (fname, tpe), f)
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
               |    ${inits.shift(4).trim}
               |}""".stripMargin

          val parent = d.id.owner match {
            case Owner.Toplevel =>
              q""
            case Owner.Adt(id) =>
              val parentId = trans.asCsType(id, domain.version)
              q": $parentId"
          }

          val hcGroups = outs
            .map(_._3._1)
            .map(name => q"$name")
            .grouped(8)
            .map(group => q"""HashCode.Combine(${group.join(", ")})""")
            .toList

          def mkComparator(ref: TextTree[CSValue],
                           oref: TextTree[CSValue],
                           tpe: TypeRef): TextTree[CSValue] = {
            TypeId.comparator(tpe) match {
              case ComparatorType.Direct =>
                q"$ref == $oref"
              case ComparatorType.ObjectEquals =>
                q"((Object)$ref).Equals($oref)"
              case ComparatorType.OptionEquals =>
                q"Equals($ref, $oref)"
              case ComparatorType.SeqEquals =>
                q"$ref.SequenceEqual($oref)"
              case ComparatorType.SetEquals =>
                q"$ref.SetEquals($oref)"
              case ComparatorType.MapEquals(valtpe) =>
                val vref = q"$oref[key]"
                val ovref = q"$ref[key]"

                val cmp = mkComparator(vref, ovref, valtpe)

                q"($ref.Count == $oref.Count && !$ref.Keys.Any(key => !$oref.Keys.Contains(key)) && !$ref.Keys.Any(key => $cmp))"
            }
          }

          val comparators = outs.map(o => (o._4, o._3._1)).map {
            case (f, name) =>
              val ref = q"$name"
              val oref = q"other.$ref"

              mkComparator(ref, oref, f.tpe)
          }

          val hc = if (hcGroups.isEmpty) {
            q"0"
          } else {
            hcGroups.join(" ^\n")
          }
          val cmp = if (comparators.isEmpty) {
            q"true"
          } else {
            comparators.join(" &&\n")
          }
          val eq = Seq(q"""public override int GetHashCode()
               |{
               |    return ${hc.shift(8).trim};
               |}""".stripMargin, q"""public bool Equals($name other) {
               |    return ${cmp.shift(8).trim};
               |}""".stripMargin, q"""public override bool Equals(object? obj) {
               |     if (ReferenceEquals(null, obj)) return false;
               |     if (ReferenceEquals(this, obj)) return true;
               |     if (obj.GetType() != this.GetType()) return false;
               |     return Equals(($name)obj);
               |}""".stripMargin)

          q"""[Serializable]
             |public sealed class $name$parent {
             |    ${fields.join("\n").shift(4).trim}
             |
             |    ${constructor.shift(4).trim}
             |
             |    ${methods.join("\n").shift(4).trim}
             |
             |    ${eq.join("\n\n").shift(4).trim}
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

          q"""[Serializable]
             |public enum $name {
             |    ${branches.shift(4).trim}
             |}""".stripMargin

        case _: Typedef.Adt =>
          q"""public interface $name {
             |}""".stripMargin
      }
    }

    def basename(dom: Domain): String = {
      (dom.id.path.map(_.capitalize) ++ Seq(dom.version.version))
        .mkString("-")
    }

    private def inNs(name: String,
                     tree: TextTree[CSValue]): TextTree[CSValue] = {
      q"""namespace ${name} {
         |    ${tree.shift(4).trim}
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
