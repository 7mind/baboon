package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.{baboonCodecImpls, iBaboonGenerated, iBaboonGeneratedLatest}
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.{Builtins, ComparatorType}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSDefnTranslator {
  def translate(
                 defn: DomainMember.User,
                 domain: Domain,
                 evo: BaboonEvolution,
               ): Either[NEList[BaboonIssue.TranslationIssue], List[CSDefnTranslator.OutputExt]]
}

object CSDefnTranslator {

  case class Output(path: String, tree: TextTree[CSValue], pkg: CSPackageId)

  case class OutputExt(output: Output, codecReg: TextTree[CSValue])

  val obsolete: CSType =
    CSType(CSBaboonTranslator.systemPkg, "Obsolete", fq = false)

  val serializable: CSType =
    CSType(CSBaboonTranslator.systemPkg, "Serializable", fq = false)

  class CSDefnTranslatorImpl(
                              options: CompilerOptions,
                              trans: CSTypeTranslator,
                              tools: CSDefnTools,
                              codecs: Set[CSCodecTranslator],
                              codecsTests: CSCodecTestsTranslator
                            ) extends CSDefnTranslator {
    type Out[T] = Either[NEList[BaboonIssue.TranslationIssue], T]

    override def translate(
                            defn: DomainMember.User,
                            domain: Domain,
                            evo: BaboonEvolution,
                          ): Either[NEList[BaboonIssue.TranslationIssue], List[OutputExt]] = {
      val isLatestVersion = domain.version == evo.latest
      val fbase = tools.basename(domain)
      val fname = s"${defn.id.name.name.capitalize}.cs"

      def obsoletePrevious(tree: TextTree[CSValue]) = {
        val hackyIsEmpty = tree.mapRender(_ => "?").isEmpty
        if (isLatestVersion || hackyIsEmpty) {
          tree
        } else {
          q"""[$obsolete("Version ${domain.version.version} is obsolete, you should migrate to ${evo.latest.version}", ${options.obsoleteErrors.toString})]
             |$tree""".stripMargin
        }
      }

      def getOutputPath(subPath: String): String = {
        defn.defn.id.owner match {
          case Owner.Toplevel =>
            s"$fbase/$subPath/$fname"
          case Owner.Adt(id) =>
            s"$fbase/$subPath/${id.name.name.toLowerCase}-$fname"
        }
      }

      val csTypeRef = trans.toCsTypeRefDeref(defn.id, domain)
      val srcRef = trans.toCsTypeRefNoDeref(defn.id, domain)

      val defnReprBase = makeRepr(defn, domain, csTypeRef, isLatestVersion)

      val codecTrees =
        codecs.toList
          .map(t => t.translate(defn, csTypeRef, srcRef, domain))
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

      val codecTestTrees = codecsTests.translate(defn, csTypeRef, srcRef, domain)
      val codecTestWithNS = codecTestTrees.map(tools.inNs(ns.toSeq, _))
      val codecTestOut = codecTestWithNS.map(codecTestWithNS => OutputExt(
        Output(getOutputPath("test"), codecTestWithNS, trans.toCsPkg(domain.id, domain.version)),
        q""
      ))


      Right(
        List(
          Some(OutputExt(
            Output(getOutputPath("main"), content, trans.toCsPkg(domain.id, domain.version)),
            q"Register(new $baboonCodecImpls($reg));"
          )),
          codecTestOut
        ).flatten
      )
    }

    private def makeRepr(defn: DomainMember.User,
                         domain: Domain,
                         name: CSValue.CSType,
                         isLatestVersion: Boolean,
                        ): TextTree[CSValue] = {
      val genMarker =
        if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val meta = tools.makeMeta(defn, domain.version) ++ codecs.map(
        _.codecMeta(defn, name).member
      )
      defn.defn match {
        case dto: Typedef.Dto =>
          val outs = dto.fields.map { f =>
            val tpe = trans.asCsRef(f.tpe, domain)
            val mname = s"${f.name.name.capitalize}"
            (mname, tpe, f)
          }

          val parent = dto.id.owner match {
            case Owner.Toplevel =>
              None
            case Owner.Adt(id) =>
              val parentId = trans.asCsType(id, domain)
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
            .map(group =>
              q"""HashCode.Combine(
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
          val eq = Seq(
            q"""public override int GetHashCode()
               |{
               |    return ${hc.shift(8).trim};
               |}""".stripMargin,
            q"""public bool Equals($name? other) {
               |    if (other == null) {
               |        return false;
               |    }
               |    return ${cmp.shift(8).trim};
               |}""".stripMargin
          )

          val fields = Seq(outs.map { case (name, tpe, _) =>
            q"public $tpe $name { get; init; }"
          }.join("\n"))

          val constructor = Seq(renderConstructor(dto, outs, domain))

          val members = fields ++ constructor ++ eq ++ meta
          q"""[$serializable]
             |public sealed record $name $parents
             |{
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

    private def renderConstructor(
                                   dto: Typedef.Dto,
                                   fields: List[(String, TextTree[CSValue], Field)],
                                   domain: Domain
                                 ): TextTree[CSValue] = {
      def renderFieldAssignments(name: String, field: Field): TextTree[CSValue] = {
        def isCollection(tpe: TypeRef): Boolean = {
          tpe match {
            case TypeRef.Constructor(Builtins.lst, _) => true
            case TypeRef.Constructor(Builtins.map, _) => true
            case TypeRef.Constructor(Builtins.set, _) => true
            case _ => false
          }
        }

        def eligibleForDateTruncation(tpe: TypeRef): Boolean = {
          tpe match {
            case TypeRef.Scalar(Builtins.tsu) | TypeRef.Scalar(Builtins.tso) => true
            case TypeRef.Constructor(Builtins.lst, args) => eligibleForDateTruncation(args.head)
            case TypeRef.Constructor(Builtins.set, args) => eligibleForDateTruncation(args.head)
            case TypeRef.Constructor(Builtins.opt, args) => eligibleForDateTruncation(args.head)
            case TypeRef.Constructor(Builtins.map, args) => eligibleForDateTruncation(args(0)) || eligibleForDateTruncation(args(1))
            case _ => false
          }
        }

        def render(tpe: TypeRef, name: String): TextTree[CSValue] = {
          tpe match {
            case TypeRef.Scalar(Builtins.tsu) | TypeRef.Scalar(Builtins.tso) => q"BaboonDateTimeFormats.TruncateToMilliseconds($name)"
            case TypeRef.Constructor(Builtins.lst, args) => q"$name.Select(value => ${render(args.head, s"value")}).ToImmutableList()"
            case TypeRef.Constructor(Builtins.set, args) => q"$name.Select(value => ${render(args.head, s"value")}).ToImmutableHashSet()"
            case TypeRef.Constructor(Builtins.opt, args) =>
              val nextName = if (isCollection(args.head)) s"$name" else s"$name.Value"
              q"$name == null ? $name : ${render(args.head, s"$nextName")}"
            case TypeRef.Constructor(Builtins.map, args) =>
              val key = args(0)
              val value = args(1)
              val keyType = trans.asCsRef(key, domain)
              val valueType = trans.asCsRef(value, domain)
              q"$name.Select(kv => new KeyValuePair<$keyType, $valueType>(${render(key, "kv.Key")}, ${render(value, "kv.Value")})).ToImmutableDictionary(kv => kv.Key, kv => kv.Value)".stripMargin
            case _ => q"${field.name.name}"
          }
        }

        val rendered = if (eligibleForDateTruncation(field.tpe)) {
          render(field.tpe, field.name.name)
        } else q"${field.name.name}"

        q"$name = $rendered;"
      }

      val parameters = fields.map { case (_, tpe, field) =>
        q"$tpe ${field.name.name}"
      }.join(",\n")

      val assignments = fields.map { case (name, fieldType, field) =>
        renderFieldAssignments(name, field)
      }.join("\n")


      q"""public ${dto.id.name.name} (
         |  ${parameters.shift(4).trim}
         |)
         |{
         |  ${assignments.shift(4).trim}
         |}
         |""".stripMargin
    }

    private def renderHashcode(
                                ref: TextTree[CSValue],
                                cmp: ComparatorType,
                                depth: Int
                              ): TextTree[CSValue] = {
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
              q"($ref.Select($itemRef => HashCode.Combine(${
                renderHashcode(
                  q"$itemRef.Key",
                  keyComparator,
                  depth + 1
                )
              }, ${renderHashcode(q"$itemRef.Value", valComparator, depth + 1)})).OrderBy(c => c).Aggregate(0x1EAFDEAD, (current, $itemRef) => current ^ $itemRef))"
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
