package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.{
  baboonTypeCodecs,
  iBaboonGenerated,
  iBaboonGeneratedLatest
}
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.{
  iBaboonGenerated,
  iBaboonGeneratedLatest
}
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.ComparatorType
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSDefnTranslator {
  def translate(defn: DomainMember.User,
                domain: Domain,
                evo: BaboonEvolution,
  ): Either[NEList[BaboonIssue.TranslationIssue], List[
    CSDefnTranslator.OutputExt
  ]]
}

object CSDefnTranslator {

  case class Output(path: String,
                    tree: TextTree[CSValue],
                    pkg: CSPackageId,
                    isTest: Boolean)

  case class OutputExt(output: Output, codecReg: TextTree[CSValue])

  val obsolete: CSType =
    CSType(CSBaboonTranslator.systemPkg, "Obsolete", fq = false)

  val serializable: CSType =
    CSType(CSBaboonTranslator.systemPkg, "Serializable", fq = false)

  class CSDefnTranslatorImpl(options: CompilerOptions,
                             trans: CSTypeTranslator,
                             tools: CSDefnTools,
                             codecs: Set[CSCodecTranslator],
                             codecsTests: CSCodecTestsTranslator)
      extends CSDefnTranslator {
    type Out[T] = Either[NEList[BaboonIssue.TranslationIssue], T]

    override def translate(defn: DomainMember.User,
                           domain: Domain,
                           evo: BaboonEvolution,
    ): Either[NEList[BaboonIssue.TranslationIssue], List[OutputExt]] = {
      val isLatestVersion = domain.version == evo.latest

      def obsoletePrevious(tree: TextTree[CSValue]) = {
        val hackyIsEmpty = tree.mapRender(_ => "?").isEmpty
        if (isLatestVersion || hackyIsEmpty) {
          tree
        } else {
          q"""[$obsolete("Version ${domain.version.version} is obsolete, you should migrate to ${evo.latest.version}", ${options.obsoleteErrors.toString})]
             |$tree""".stripMargin
        }
      }

      def getOutputPath(tests: Boolean): String = {
        val fbase = tools.basename(domain, evo, options)
        val fsufix = if (tests) "_Tests" else ""
        val fname = s"${defn.id.name.name.capitalize}$fsufix.cs"
        defn.defn.id.owner match {
          case Owner.Toplevel =>
            s"$fbase/$fname"
          case Owner.Adt(id) =>
            s"$fbase/${id.name.name.toLowerCase}-$fname"
        }
      }

      val csTypeRef = trans.toCsTypeRefDeref(defn.id, domain, evo, options)
      val srcRef = trans.toCsTypeRefNoDeref(defn.id, domain, evo, options)

      val defnReprBase = makeRepr(defn, domain, csTypeRef, isLatestVersion, evo)

      val codecTrees =
        codecs.toList
          .map(t => t.translate(defn, csTypeRef, srcRef, domain, evo))
          .map(obsoletePrevious)

      val defnRepr = obsoletePrevious(defnReprBase)

      assert(defn.id.pkg == domain.id)

      val ns = srcRef.pkg.parts

      val allDefs = (defnRepr +: codecTrees).join("\n\n")
      val content = tools.inNs(ns.toSeq, allDefs)

      val reg = (List(q""""${defn.id.toString}"""") ++ codecs.toList
        .sortBy(_.getClass.getName)
        .map(codec => q"${codec.codecName(srcRef).copy(fq = true)}.Instance"))
        .join(", ")

      val codecTestTrees =
        codecsTests.translate(defn, csTypeRef, srcRef, domain, evo)
      val codecTestWithNS = codecTestTrees.map(tools.inNs(ns.toSeq, _))
      val codecTestOut = codecTestWithNS.map(
        codecTestWithNS =>
          OutputExt(
            Output(
              getOutputPath(tests = true),
              codecTestWithNS,
              trans.toCsPkg(domain.id, domain.version, evo, options),
              isTest = true
            ),
            q""
        )
      )

      Right(
        List(
          Some(
            OutputExt(
              Output(
                getOutputPath(tests = false),
                content,
                trans.toCsPkg(domain.id, domain.version, evo, options),
                isTest = false,
              ),
              q"Register(new $baboonTypeCodecs($reg));"
            )
          ),
          codecTestOut
        ).flatten
      )
    }

    private def makeRepr(defn: DomainMember.User,
                         domain: Domain,
                         name: CSValue.CSType,
                         isLatestVersion: Boolean,
                         evo: BaboonEvolution): TextTree[CSValue] = {
      val genMarker =
        if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val meta = tools.makeMeta(defn, domain.version) ++ codecs.map(
        _.codecMeta(defn, name).member
      )
      defn.defn match {
        case dto: Typedef.Dto =>
          val outs = dto.fields.map { f =>
            val tpe = trans.asCsRef(f.tpe, domain, evo, options)
            val mname = s"${f.name.name.capitalize}"
            (mname, tpe, f)
          }

          val constructorArgs =
            outs.map { case (fname, tpe, _) => q"$tpe $fname" }.join(",\n")

          val parent = dto.id.owner match {
            case Owner.Toplevel =>
              None
            case Owner.Adt(id) =>
              val parentId = trans.asCsType(id, domain, evo, options)
              Some(parentId)
          }

          val allParents = parent.toSeq ++ Seq(q"$genMarker")
          val parents = if (allParents.isEmpty) {
            q""
          } else {
            q" : ${allParents.join(", ")} "
          }

          val comparators = outs.map {
            case (name, _, f) =>
              val ref = q"$name"
              val oref = q"other.$ref"
              val comparator = TypeId.comparator(f.tpe)
              (ref, oref, comparator)
          }

          val renderedHcParts = comparators.map {
            case (ref, _, cmp) =>
              renderHashcode(ref, cmp, 0)
          }

          val hcGroups = renderedHcParts
            .grouped(8)
            .map(group => q"""HashCode.Combine(
                 |    ${group.join(",\n").shift(4).trim}
                 |)""".stripMargin)
            .toList

          val hc = if (hcGroups.isEmpty) {
            q"0"
          } else {
            hcGroups.join(" ^\n")
          }

          val renderedCmps = comparators.map {
            case (ref, oref, cmp) =>
              renderComparator(ref, oref, cmp)
          }

          val cmp = if (renderedCmps.isEmpty) {
            q"true"
          } else {
            renderedCmps.join(" &&\n")
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

          val members = eq ++ meta
          q"""[$serializable]
             |public sealed record $name(
             |    ${constructorArgs.shift(4).trim}
             |)$parents {
             |    ${members.join("\n\n").shift(4).trim}
             |};""".stripMargin

        case e: Typedef.Enum =>
          val branches =
            e.members
              .map { m =>
                val base = q"""${m.name.capitalize}"""
                m.const match {
                  case Some(value) =>
                    q"""$base = ${value.toString}"""
                  case None => base
                }
              }
              .toSeq
              .join(",\n")

          q"""[$serializable]
             |public enum $name {
             |    ${branches.shift(4).trim}
             |}""".stripMargin

        case _: Typedef.Adt =>
          q"""public interface $name : $genMarker {}""".stripMargin
        case _: Typedef.Foreign =>
          q""
      }
    }

    private def renderHashcode(ref: TextTree[CSValue],
                               cmp: ComparatorType,
                               depth: Int): TextTree[CSValue] = {
      val itemRef = q"item${depth.toString}"
      cmp match {
        case _: ComparatorType.Basic =>
          if (depth == 0) {
            ref
          } else {
            q"HashCode.Combine($ref)"
          }

        case c: ComparatorType.Complex =>
          c match {
            case ComparatorType.OptionEquals(subComparator) =>
              q"($ref == null ? 0 : ${renderHashcode(ref, subComparator, depth + 1)})"
            case ComparatorType.SeqEquals(subComparator) =>
              q"($ref.Aggregate(0x1EAFDEAD, (current, $itemRef) => current ^ ${renderHashcode(itemRef, subComparator, depth + 1)}))"
            case ComparatorType.SetEquals(subComparator) =>
              q"($ref.Select($itemRef => ${renderHashcode(itemRef, subComparator, depth + 1)}).OrderBy(c => c).Aggregate(0x1EAFDEAD, (current, $itemRef) => current ^ $itemRef))"
            case ComparatorType.MapEquals(keyComparator, valComparator) =>
              q"($ref.Select($itemRef => HashCode.Combine(${renderHashcode(
                q"$itemRef.Key",
                keyComparator,
                depth + 1
              )}, ${renderHashcode(q"$itemRef.Value", valComparator, depth + 1)})).OrderBy(c => c).Aggregate(0x1EAFDEAD, (current, $itemRef) => current ^ $itemRef))"
          }
      }
    }

    private def renderComparator(ref: TextTree[CSValue],
                                 oref: TextTree[CSValue],
                                 cmp: ComparatorType): TextTree[CSValue] = {
      cmp match {
        case ComparatorType.Direct =>
          q"$ref == $oref"
        case ComparatorType.ObjectEquals =>
          q"((Object)$ref).Equals($oref)"
        case ComparatorType.OptionEquals(subComparator) =>
          subComparator match {
            case _: ComparatorType.Basic =>
              q"Equals($ref, $oref)"
            case c: ComparatorType.Complex =>
              q"(Equals($ref, $oref) || ($ref != null && $oref != null && ${renderComparator(ref, oref, c)}))"
          }

        case ComparatorType.SeqEquals(subComparator) =>
          subComparator match {
            case _: ComparatorType.Basic =>
              q"$ref.SequenceEqual($oref)"
            case c: ComparatorType.Complex =>
              q"($ref.SequenceEqual($oref) || ($ref.Count == $oref.Count && ($ref.Zip($oref).All(p => ${renderComparator(q"p.First", q"p.Second", c)}))))"
          }

        case ComparatorType.SetEquals(_) =>
          q"$ref.SetEquals($oref)"

        case ComparatorType.MapEquals(_, valComp) =>
          val vref = q"$oref[key]"
          val ovref = q"$ref[key]"

          val cmp = renderComparator(vref, ovref, valComp)

          q"($ref.Count == $oref.Count && $ref.Keys.All(key => $oref.ContainsKey(key)) && $ref.Keys.All(key => $cmp))"
      }
    }
  }
}
