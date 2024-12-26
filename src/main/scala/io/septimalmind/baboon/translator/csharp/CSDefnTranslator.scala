package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.ComparatorType
import io.septimalmind.baboon.{CompilerOptions, CompilerProduct}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSDefnTranslator {
  def translate(defn: DomainMember.User): Either[NEList[BaboonIssue.TranslationIssue], List[CSDefnTranslator.OutputExt]]
  def translateFixtures(defn: DomainMember.User): Either[NEList[BaboonIssue.TranslationIssue], List[CSDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): Either[NEList[BaboonIssue.TranslationIssue], List[CSDefnTranslator.Output]]
}

object CSDefnTranslator {
  case class Output(path: String, tree: TextTree[CSValue], pkg: CSPackageId, product: CompilerProduct, doNotModify: Boolean = false)
  case class OutputExt(output: Output, codecReg: TextTree[CSValue])

  private val obsolete: CSType     = CSType(CSTypes.csSystemPkg, "Obsolete", fq = false)
  private val serializable: CSType = CSType(CSTypes.csSystemPkg, "Serializable", fq = false)

  class CSDefnTranslatorImpl(
    options: CompilerOptions,
    trans: CSTypeTranslator,
    csTrees: CSTreeTools,
    csDomTrees: CSDomainTreeTools,
    csFiles: CSFileTools,
    codecs: Set[CSCodecTranslator],
    codecsTests: CSCodecTestsTranslator,
    codecsFixture: CSCodecFixtureTranslator,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends CSDefnTranslator {
    type Out[T] = Either[NEList[BaboonIssue.TranslationIssue], T]

    override def translate(defn: DomainMember.User): Either[NEList[BaboonIssue.TranslationIssue], List[OutputExt]] = {
      defn.id.owner match {
        case Owner.Adt(_) if options.csOptions.useCompactAdtForm => Right(List.empty)
        case _                                                   => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): Either[NEList[BaboonIssue.TranslationIssue], List[OutputExt]] = {
      val (content, reg) = makeFullRepr(defn, inNs = true)

      // Generic codec variant have poor performance on empty JIT and il2cpp (for some reason ._.)
      // val registrations = reg.map { case (srcRef, reg) => q"Register(new $baboonTypeCodecs<${srcRef.fullyQualified}>($reg));" }.join("\n")
      val registrations = reg.map { case (_, reg) => q"Register(new $baboonTypeCodecs($reg));" }.join("\n")

      Right(
        List(
          OutputExt(
            Output(
              getOutputPath(defn),
              content,
              trans.toCsPkg(domain.id, domain.version, evo),
              CompilerProduct.Definition,
            ),
            registrations,
          )
        )
      )
    }

    override def translateFixtures(defn: DomainMember.User): Either[NEList[BaboonIssue.TranslationIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) if options.csOptions.useCompactAdtForm => Right(List.empty)
        case _                                                   => doTranslateFixtures(defn)
      }
    }

    private def doTranslateFixtures(defn: DomainMember.User): Either[NEList[BaboonIssue.TranslationIssue], List[Output]] = {
      val fixtureTreeOut = makeFixtureRepr(defn).map {
        fixtureTreeWithNs =>
          Output(
            getOutputPath(defn, suffix = Some(".Fixture")),
            fixtureTreeWithNs,
            trans.toCsPkg(domain.id, domain.version, evo),
            CompilerProduct.Fixture,
          )
      }

      Right(fixtureTreeOut.toList)
    }

    override def translateTests(defn: DomainMember.User): Either[NEList[BaboonIssue.TranslationIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) if options.csOptions.useCompactAdtForm => Right(List.empty)
        case _                                                   => doTranslateTest(defn)
      }
    }

    private def doTranslateTest(defn: DomainMember.User): Either[NEList[BaboonIssue.TranslationIssue], List[Output]] = {
      val codecTestOut = makeTestRepr(defn).map {
        codecTestWithNS =>
          Output(
            getOutputPath(defn, suffix = Some(".Tests")),
            codecTestWithNS,
            trans.toCsPkg(domain.id, domain.version, evo),
            CompilerProduct.Test,
          )
      }

      Right(codecTestOut.toList)
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = csFiles.basename(domain, evo)
      val fname = s"${defn.id.name.name.capitalize}${suffix.getOrElse("")}.cs"
      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString(".")}.$fname"
        case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}.$fname"
      }
    }

    private def makeFullRepr(
      defn: DomainMember.User,
      inNs: Boolean,
    ): (TextTree[CSValue], List[(CSType, TextTree[CSValue])]) = {
      val isLatestVersion = domain.version == evo.latest

      def obsoletePrevious(tree: TextTree[CSValue]): TextTree[CSValue] = {
        val hackyIsEmpty = tree.mapRender(_ => "?").isEmpty
        if (isLatestVersion || hackyIsEmpty) {
          tree
        } else {
          q"""[$obsolete("Version ${domain.version.version} is obsolete, you should migrate to ${evo.latest.version}", ${options.generic.obsoleteErrors.toString})]
             |$tree""".stripMargin
        }
      }

      val csTypeRef = trans.toCsTypeRefDeref(defn.id, domain, evo)
      val srcRef    = trans.toCsTypeRefNoDeref(defn.id, domain, evo)

      val (defnReprBase, extraRegs) =
        makeRepr(defn, csTypeRef, isLatestVersion)

      val codecTrees = codecs.toList
        .flatMap(t => t.translate(defn, csTypeRef, srcRef).toList)
        .map(obsoletePrevious)

      val defnRepr = obsoletePrevious(defnReprBase)

      assert(defn.id.pkg == domain.id)

      val ns = srcRef.pkg.parts

      val allDefs = (defnRepr +: codecTrees).join("\n\n")
      val content = if (inNs) csTrees.inNs(ns.toSeq, allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef =>
          List.empty[(CSType, TextTree[CSValue])]
        case _ =>
          // wrap Lazy<Child> -> Lazy<Parent> as C# Lazy do not support type variance
          // generic types are possible, but have bad JIT and il2cpp performance for generic constructors
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .map(codec => q"new Lazy<$iBaboonCodecData>(() => ${codec.codecName(srcRef).copy(fq = true)}.Instance)")
          // Generic codec variant have poor performance on empty JIT and il2cpp (for some reason ._.)
          // val codecsReg = codecs.toList
          //   .sortBy(_.getClass.getName)
          //   .map(codec => q"new Lazy<$iBaboonCodecData>(() => ${codec.codecName(srcRef).copy(fq = true)}.LazyInstance)")
          val reg =
            (List(q"""\"${defn.id.toString}\"""") ++ codecsReg).join(", ")
          List(csTypeRef -> reg)
      }

      val allRegs = reg ++ extraRegs

      (content, allRegs)
    }

    private def makeRepr(defn: DomainMember.User, name: CSValue.CSType, isLatestVersion: Boolean): (TextTree[CSValue], List[(CSType, TextTree[CSValue])]) = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = csDomTrees.makeMeta(defn, isCodec = false)
      val codecMeta = codecs.map(_.codecMeta(defn, name).member)
      val meta      = mainMeta ++ codecMeta

      defn.defn match {
        case contract: Typedef.Contract =>
          val methods = renderContractFields(contract.fields).join("\n")
          val refs    = contract.contracts.map(t => q"${trans.asCsType(t, domain, evo)}") ++ List(q"$genMarker")
          val parents = makeParents(refs)

          (
            q"""public interface ${name.asName}$parents  {
               |    ${methods.shift(4).trim}
               |}""".stripMargin,
            List.empty,
          )

        case dto: Typedef.Dto =>
          val outs = dto.fields.map {
            f =>
              val tpe   = trans.asCsRef(f.tpe, domain, evo)
              val mname = s"${f.name.name.capitalize}"
              (mname, tpe, f)
          }

          val constructorArgs = outs.map { case (fname, tpe, _) => q"$tpe $fname" }.join(",\n")

          val contractParents = dto.contracts.toSeq.map(c => q"${trans.asCsType(c, domain, evo)}")

          val adtParents = dto.id.owner match {
            case Owner.Adt(id) => Seq(q"${trans.asCsType(id, domain, evo)}", q"$iBaboonAdtMemberMeta")
            case _             => Seq.empty
          }

          val allParents = adtParents ++ contractParents ++ Seq(q"$genMarker")
          val parents    = makeParents(allParents.toList)

          val comparators = outs.map {
            case (name, _, f) =>
              val ref        = q"$name"
              val oref       = q"other.$ref"
              val comparator = TypeId.comparator(f.tpe)
              (ref, oref, comparator)
          }

          val renderedHcParts = comparators.map {
            case (ref, _, cmp) => renderHashcode(ref, cmp, 0)
          }

          val hcGroups = renderedHcParts
            .grouped(8).map {
              group =>
                q"""HashCode.Combine(
                   |    ${group.join(",\n").shift(4).trim}
                   |)""".stripMargin
            }.toList

          val hc = if (hcGroups.isEmpty) q"0" else hcGroups.join(" ^\n")

          val renderedCmps = comparators.map {
            case (ref, oref, cmp) => renderComparator(ref, oref, cmp)
          }

          val cmp =
            if (renderedCmps.isEmpty) q"true" else renderedCmps.join(" &&\n")

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
               |}""".stripMargin,
          )

          val members = eq ++ meta
          (
            q"""[$serializable]
               |public sealed record ${name.asName}(
               |    ${constructorArgs.shift(4).trim}
               |)$parents {
               |    ${members.join("\n\n").shift(4).trim}
               |};""".stripMargin,
            List.empty,
          )

        case e: Typedef.Enum =>
          val branches =
            e.members.map {
              m =>
                val base = q"""${m.name.capitalize}"""
                m.const match {
                  case Some(value) => q"""$base = ${value.toString}"""
                  case None        => base
                }
            }.toSeq
              .join(",\n")

          (
            q"""[$serializable]
               |public enum ${name.asName} {
               |    ${branches.shift(4).trim}
               |}""".stripMargin,
            List.empty,
          )

        case adt: Typedef.Adt =>
          val allParents = Seq(q"$genMarker")
          val parents    = makeParents(allParents.toList)

          if (options.csOptions.useCompactAdtForm) {
            val memberTrees = adt.members.map {
              mid =>
                domain.defs.meta.nodes.get(mid) match {
                  case Some(mdefn: DomainMember.User) =>
                    makeFullRepr(mdefn, inNs = false)
                  case m =>
                    throw new RuntimeException(
                      s"BUG: missing/wrong adt member: $mid => $m"
                    )
                }
            }

            val branches = memberTrees
              .map(_._1)
              .toSeq
              .join("\n\n")

            val regs    = memberTrees.map(_._2)
            val members = meta

            (
              q"""public abstract record ${name.asName}$parents {
                 |    ${branches.shift(4).trim}
                 |
                 |    ${members.join("\n\n").shift(4).trim}
                 |}""".stripMargin,
              regs.toList.flatten,
            )

          } else {
            (
              q"""public interface ${name.asName}$parents {
                 |}""".stripMargin,
              List.empty,
            )
          }

        case _: Typedef.Foreign => (q"", List.empty)
      }
    }

    private def makeFixtureRepr(defn: DomainMember.User): Option[TextTree[CSValue]] = {
      val srcRef = trans.toCsTypeRefNoDeref(defn.id, domain, evo)
      val ns     = srcRef.pkg.parts

      val fixtureTree       = codecsFixture.translate(defn)
      val fixtureTreeWithNs = fixtureTree.map(t => csTrees.inNs(ns.toSeq, t))

      fixtureTreeWithNs
    }

    private def makeTestRepr(defn: DomainMember.User): Option[TextTree[CSValue]] = {
      val csTypeRef = trans.toCsTypeRefDeref(defn.id, domain, evo)
      val srcRef    = trans.toCsTypeRefNoDeref(defn.id, domain, evo)
      val ns        = srcRef.pkg.parts

      val testTree       = codecsTests.translate(defn, csTypeRef, srcRef)
      val testTreeWithNs = testTree.map(t => csTrees.inNs(ns.toSeq, t))

      testTreeWithNs
    }

    private def renderContractFields(
      fields: List[Field]
    ): List[TextTree[CSValue]] = {
      fields.map {
        f =>
          val tpe   = trans.asCsRef(f.tpe, domain, evo)
          val mname = s"${f.name.name.capitalize}"
          q"public $tpe $mname { get; }"
      }
    }

    private def makeParents(
      refs: List[TextTree[CSValue]]
    ): TextTree[CSValue] = {
      if (refs.isEmpty) q"" else q" : ${refs.join(", ")} "
    }

    private def renderHashcode(ref: TextTree[CSValue], cmp: ComparatorType, depth: Int): TextTree[CSValue] = {
      val itemRef = q"item${depth.toString}"
      cmp match {
        case _: ComparatorType.Basic =>
          if (depth == 0) ref else q"HashCode.Combine($ref)"
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
                  depth + 1,
                )}, ${renderHashcode(q"$itemRef.Value", valComparator, depth + 1)})).OrderBy(c => c).Aggregate(0x1EAFDEAD, (current, $itemRef) => current ^ $itemRef))"
          }
      }
    }

    private def renderComparator(ref: TextTree[CSValue], oref: TextTree[CSValue], cmp: ComparatorType): TextTree[CSValue] = {
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
          val vref  = q"$oref[key]"
          val ovref = q"$ref[key]"

          val cmp = renderComparator(vref, ovref, valComp)

          q"($ref.Count == $oref.Count && $ref.Keys.All(key => $oref.ContainsKey(key)) && $ref.Keys.All(key => $cmp))"
      }
    }
  }
}
