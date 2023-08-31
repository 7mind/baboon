package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import izumi.fundamentals.platform.strings.TextTree.*
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.iBaboonGenerated
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.ComparatorType
import izumi.fundamentals.collections.nonempty.NonEmptyList
import izumi.fundamentals.platform.strings.TextTree

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

  class CSDefnTranslatorImpl(options: CompilerOptions, trans: CSTypeTranslator)
      extends CSDefnTranslator {
    type Out[T] = Either[NonEmptyList[BaboonIssue.TranslationIssue], T]

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
            val mname = s"${f.name.name.capitalize}"
            (mname, tpe, f)
          }

          val cargs = outs
            .map {
              case (fname, tpe, _) =>
                q"${tpe} $fname"
            }
            .join(",\n")

          val parent = d.id.owner match {
            case Owner.Toplevel =>
              None
            case Owner.Adt(id) =>
              val parentId = trans.asCsType(id, domain.version)
              Some(parentId)
          }

          val allParents = parent.toSeq ++ Seq(q"$iBaboonGenerated")
          val parents = if (allParents.isEmpty) {
            q""
          } else {
            q" : ${allParents.join(", ")} "
          }

          val hcGroups = outs
            .map(_._1)
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

          val comparators = outs.map {
            case (name, _, f) =>
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
               |}""".stripMargin, q"""public bool Equals($name? other) {
               |    if (other == null) {
               |        return false;
               |    }
               |    return ${cmp.shift(8).trim};
               |}""".stripMargin)

          q"""[Serializable]
             |public sealed record $name(
             |    ${cargs.shift(4).trim}
             |)$parents {
             |    ${eq.join("\n\n").shift(4).trim}
             |};""".stripMargin

        case e: Typedef.Enum =>
          val branches =
            e.members.map(m => q"""${m.name.capitalize}""").toSeq.join(",\n")

          q"""[Serializable]
             |public enum $name {
             |    ${branches.shift(4).trim}
             |}""".stripMargin

        case _: Typedef.Adt =>
          q"""public interface $name : $iBaboonGenerated {
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
