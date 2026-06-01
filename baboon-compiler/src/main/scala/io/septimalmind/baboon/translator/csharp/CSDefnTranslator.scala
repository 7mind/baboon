package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType, CSTypeOrigin}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.ComparatorType
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle, TypeInfo}
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[CSDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[CSDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[CSDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[CSDefnTranslator.Output]]
}

object CSDefnTranslator {
  case class CodecReg(
    typeId: TypeId,
    tpe: CSType,
    tpeKeepForeigns: CSType,
    tpeId: TextTree[CSValue],
    trees: Map[String, TextTree[CSValue]],
  )
  case class DefnRepr(
    defn: TextTree[CSValue],
    codecs: List[CodecReg],
  )

  sealed trait OutputOrigin

  object OutputOrigin {
    case class TypeInDomain(id: TypeId, pkg: Pkg, version: Version) extends OutputOrigin
    case object Runtime extends OutputOrigin
  }

  case class Output(
    path: String,
    tree: Option[TextTree[CSValue]],
    pkg: CSPackageId,
    product: CompilerProduct,
    origin: OutputOrigin,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[CSValue]])] = List.empty,
  )
//  case class OutputExt(output: Output, codecReg: TextTree[CSValue])

  private val obsolete: CSType     = CSType(CSTypes.csSystemPkg, "Obsolete", fq = false, CSTypeOrigin.Other)
  private val serializable: CSType = CSType(CSTypes.csSystemPkg, "Serializable", fq = false, CSTypeOrigin.Other)

  class CSDefnTranslatorImpl[F[+_, +_]: Applicative2 /* This impl has no errors right now */ ](
    target: CSTarget,
    trans: CSTypeTranslator,
    csTrees: CSTreeTools,
    csDomTrees: CSDomainTreeTools,
    csFiles: CSFileTools,
    codecs: Set[CSCodecTranslator],
    codecsTests: CSCodecTestsTranslator,
    codecsFixture: CSCodecFixtureTranslator,
    wiringTranslator: CSServiceWiringTranslator,
    domain: Domain,
    evo: BaboonEvolution,
    lineage: BaboonLineage,
    types: TypeInfo,
    csTypeInfo: CSTypeInfo,
    enquiries: BaboonEnquiries,
  ) extends CSDefnTranslator[F] {
    type Out[T] = F[NEList[BaboonIssue], T]

    override def translate(defn: DomainMember.User): Out[List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): Out[List[Output]] = {
      val repr = makeFullRepr(defn, inNs = true)

      val regsPerCodec = codecs.toList.map {
        c =>
          val regs = repr.codecs.flatMap {
            reg =>
              reg.trees.get(c.id).map(expr => q"${reg.tpeId}, $expr")
          }
          (c.id, regs)
      }

      val reprOut = if (csTypeInfo.eliminated(defn.id, domain.version, lineage)) {
        None
      } else {
        Some(repr.defn)
      }

      val mainOutput = Output(
        getOutputPath(defn),
        reprOut,
        trans.toCsPkg(domain.id, domain.version, evo),
        CompilerProduct.Definition,
        codecReg = regsPerCodec,
        origin   = OutputOrigin.TypeInDomain(defn.id, domain.id, domain.version),
      )

      val wiringOutput = wiringTranslator
        .translate(defn).map {
          wiringTree =>
            val srcRef = trans.asCsTypeKeepForeigns(defn.id, domain, evo)
            val ns     = srcRef.pkg.parts
            Output(
              getOutputPath(defn, suffix = Some(".Wiring")),
              Some(csTrees.inNs(ns.toSeq, wiringTree)),
              trans.toCsPkg(domain.id, domain.version, evo),
              CompilerProduct.Definition,
              origin = OutputOrigin.TypeInDomain(defn.id, domain.id, domain.version),
            )
        }.toList

      val clientOutput = wiringTranslator
        .translateClient(defn).map {
          clientTree =>
            val srcRef = trans.asCsTypeKeepForeigns(defn.id, domain, evo)
            val ns     = srcRef.pkg.parts
            Output(
              getOutputPath(defn, suffix = Some("_Client")),
              Some(csTrees.inNs(ns.toSeq, clientTree)),
              trans.toCsPkg(domain.id, domain.version, evo),
              CompilerProduct.Definition,
              origin = OutputOrigin.TypeInDomain(defn.id, domain.id, domain.version),
            )
        }.toList

      F.pure(mainOutput :: wiringOutput ::: clientOutput)
    }

    override def translateFixtures(defn: DomainMember.User): Out[List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateFixtures(defn)
      }
    }

    private def doTranslateFixtures(defn: DomainMember.User): Out[List[Output]] = {
      val fixtureTreeOut = makeFixtureRepr(defn).map {
        fixtureTreeWithNs =>
          Output(
            getOutputPath(defn, suffix = Some(".Fixture")),
            Some(fixtureTreeWithNs),
            trans.toCsPkg(domain.id, domain.version, evo),
            CompilerProduct.Fixture,
            origin = OutputOrigin.TypeInDomain(defn.id, domain.id, domain.version),
          )
      }

      F.pure(fixtureTreeOut.toList)
    }

    override def translateTests(defn: DomainMember.User): Out[List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTest(defn)
      }
    }

    private def doTranslateTest(defn: DomainMember.User): Out[List[Output]] = {
      val codecTestOut = makeTestRepr(defn).map {
        codecTestWithNS =>
          Output(
            getOutputPath(defn, suffix = Some(".Tests")),
            Some(codecTestWithNS),
            trans.toCsPkg(domain.id, domain.version, evo),
            CompilerProduct.Test,
            origin = OutputOrigin.TypeInDomain(defn.id, domain.id, domain.version),
          )
      }

      F.pure(codecTestOut.toList)
    }

    override def translateServiceRt(): Out[List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map {
        tree =>
          val pkg     = trans.toCsPkg(domain.id, domain.version, evo)
          val wrapped = csTrees.inNs(pkg.parts.toSeq, tree)
          val fbase   = csFiles.basename(domain, evo)
          Output(
            s"$fbase/BaboonServiceRt.cs",
            Some(wrapped),
            pkg,
            CompilerProduct.Definition,
            origin = OutputOrigin.Runtime,
          )
      }.toList
      F.pure(result)
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
    ): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      def obsoletePrevious(tree: TextTree[CSValue]): TextTree[CSValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""[$obsolete("Version ${domain.version.v.toString} is obsolete, you should migrate to ${evo.latest.v.toString}", ${target.language.obsoleteErrors.toString})]
             |$tree""".stripMargin
        }
      }

      val csTypeRef = trans.asCsType(defn.id, domain, evo)
      val srcRef    = trans.asCsTypeKeepForeigns(defn.id, domain, evo)

      val repr =
        makeRepr(defn, csTypeRef, isLatestVersion)

      val codecTrees = codecs.toList
        .flatMap(t => t.translate(defn, csTypeRef, srcRef).toList)
        .map(obsoletePrevious)

      // D01 fix: apply obsoletePrevious BEFORE prependDocs so the emitted order
      // is /// <summary>doc</summary> then [Obsolete] then the declaration,
      // matching C# XML doc convention (doc-then-annotation-then-symbol).
      val defnRepr = prependDocs(defn.docs, obsoletePrevious(repr.defn))

      assert(defn.id.pkg == domain.id)

      val allDefs = (defnRepr +: codecTrees).join("\n\n")
      val content = if (inNs) wrapInContainer(defn, srcRef, allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef =>
          List.empty[CodecReg]
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap {
              codec =>
                if (codec.isActive(d.id)) {
                  List(
                    codec.id -> q"new Lazy<$iBaboonCodecData>(() => ${codec.codecName(srcRef, CSTypeOrigin(d.id, domain)).copy(fq = true)}.Instance)"
                  )
                } else {
                  List.empty
                }
            }
          List(CodecReg(defn.id, csTypeRef, srcRef, q"""\"${defn.id.toString}\"""", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      DefnRepr(content, allRegs)
    }

    /** Prepend a C# XML doc comment block before a tree when `docs` is
      * non-empty. Returns the tree unchanged when `docs` is empty.
      */
    private def prependDocs(docs: Docs, tree: TextTree[CSValue]): TextTree[CSValue] = {
      val block = csTrees.renderDocs(docs, "")
      if (block.isEmpty) tree else q"${block}$tree"
    }

    private def makeRepr(defn: DomainMember.User, name: CSValue.CSType, isLatestVersion: Boolean): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = csDomTrees.makeDataMeta(defn)
      val codecMeta = codecs.flatMap(_.codecMeta(defn, name)).map(_.member)
      val meta      = mainMeta ++ codecMeta

      defn.defn match {
        case contract: Typedef.Contract =>
          val methods = renderContractFields(contract.fields).join("\n")
          val refs    = contract.contracts.map(t => q"${trans.asCsType(t, domain, evo)}") ++ List(q"$genMarker")
          val parents = makeParents(refs)

          DefnRepr(
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

          val constructorArgs = outs.map {
            case (fname, tpe, f) =>
              val fieldEx = q"$tpe $fname"
              prependDocs(f.docs, fieldEx)
          }.join(",\n")

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
              val comparator = types.comparator(f.tpe)
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

          // Identifier toString + parseRepr (PR-57a / spec: docs/spec/identifier-repr.md).
          // Emitted only when `dto.isIdentifier == true`. Wire format:
          // <SimpleName>:<version>#field:value:field:value:{<NestedName>:<version>#…}
          val identifierToStringOverride: TextTree[CSValue] =
            if (dto.isIdentifier) renderIdentifierToString(dto, name) else q""

          val identifierCodecObject: TextTree[CSValue] =
            if (dto.isIdentifier) renderIdentifierCodecClass(dto, name) else q""

          val members = (eq ++ meta) ++ (if (dto.isIdentifier) Seq(identifierToStringOverride) else Seq.empty)
          val mainTree =
            q"""[$serializable]
               |public sealed record ${name.asName}(
               |    ${constructorArgs.shift(4).trim}
               |)$parents {
               |    ${members.join("\n\n").shift(4).trim}
               |}""".stripMargin

          val combined =
            if (dto.isIdentifier) q"""$mainTree
                                     |
                                     |$identifierCodecObject""".stripMargin
            else mainTree

          DefnRepr(combined, List.empty)

        case e: Typedef.Enum =>
          val branches =
            e.members.map {
              m =>
                val base = q"""${EnumWireStyle.wireName(m.name)}"""
                m.const match {
                  case Some(value) => q"""$base = ${value.toString}"""
                  case None        => base
                }
            }.toSeq
              .join(",\n")

          DefnRepr(
            q"""[$serializable]
               |public enum ${name.asName} {
               |    ${branches.shift(4).trim}
               |}""".stripMargin,
            List.empty,
          )

        case adt: Typedef.Adt =>
          val allParents = Seq(q"$genMarker") ++ adt.contracts.map(t => q"${trans.asCsType(t, domain, evo)}")
          val parents    = makeParents(allParents.toList)

          val allFields = enquiries.unfold(domain, adt.contracts)

          val abstractFields = allFields.map {
            f =>
              val tpe   = trans.asCsRef(f.tpe, domain, evo)
              val mname = s"${f.name.name.capitalize}" // todo: dedup
              q"public abstract $tpe $mname { get; init; }"
          }.join("\n")

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
            .map(_.defn)
            .toSeq
            .join("\n\n")

          val regs    = memberTrees.map(_.codecs)
          val members = meta

          DefnRepr(
            q"""public abstract record ${name.asName}$parents {
               |    private ${name.asName}() {}
               |
               |    ${abstractFields.shift(4).trim}
               |
               |    ${branches.shift(4).trim}
               |
               |    ${members.join("\n\n").shift(4).trim}
               |}""".stripMargin,
            regs.toList.flatten,
          )

        case f: Typedef.Foreign => DefnRepr(makeForeignKeyCodecRepr(f, name), List.empty)

        case service: Typedef.Service =>
          val resolved    = ServiceResultResolver.resolve(domain, "cs", target.language.serviceResult, target.language.pragmas)
          val resolvedCtx = ServiceContextResolver.resolve(domain, "cs", target.language.serviceContext, target.language.pragmas)
          val ctxParam = resolvedCtx match {
            case ResolvedServiceContext.NoContext               => ""
            case ResolvedServiceContext.AbstractContext(tn, pn) => s"$tn $pn, "
            case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$tn $pn, "
          }
          val methods = service.methods.map {
            m =>
              val out = m.out.map(r => trans.asCsRef(r, domain, evo))
              val err = m.err.map(r => trans.asCsRef(r, domain, evo))
              val csFqName: CSValue => String = {
                case t: CSValue.CSType     => (t.pkg.parts :+ t.name).mkString(".")
                case t: CSValue.CSTypeName => t.name
              }
              val outStr   = out.map(_.mapRender(csFqName)).getOrElse("")
              val errStr   = err.map(_.mapRender(csFqName))
              val syncRet  = resolved.renderReturnType(outStr, errStr, "void")
              val retStr =
                if (target.language.asyncServices) {
                  if (syncRet == "void") "System.Threading.Tasks.Task"
                  else s"System.Threading.Tasks.Task<$syncRet>"
                } else syncRet
              val methodEx = q"""public $retStr ${m.name.name}($ctxParam${trans.asCsRef(m.sig, domain, evo)} arg);"""
              prependDocs(m.docs, methodEx)
          }.join("\n")

          val genericParam = resolvedCtx match {
            case ResolvedServiceContext.AbstractContext(tn, _) => s"<$tn>"
            case _                                             => ""
          }
          DefnRepr(
            q"""public interface ${name.asName}$genericParam  {
               |    ${methods.shift(4).trim}
               |}""".stripMargin,
            List.empty,
          )
      }
    }

    /** PR-I.1c (M24 Phase 3.1): emit a `<Foreign>_KeyCodec` extension hook for
      * every Custom-mapped C# foreign declaration. The host application
      * registers an implementation at boot which the JSON codec then uses to
      * encode/decode map keys. For BaboonRef-mapped foreigns we emit nothing —
      * the existing recursion into the aliased type covers the codec needs.
      *
      * Stringy foreigns (`string` / `System.String`) get a default identity
      * impl so the common case works out of the box. Non-stringy foreigns get
      * a stub default that throws BaboonCodecException.DecoderFailure with an
      * FQN-bearing diagnostic referring to the Host class (PR-I.1b-D01).
      *
      * C# allows multiple top-level types per file, so the interface and the
      * host class are emitted side-by-side in a single tree, sharing the
      * existing namespace wrapper from `makeFullRepr`.
      */
    private def makeForeignKeyCodecRepr(f: Typedef.Foreign, name: CSValue.CSType): TextTree[CSValue] = {
      f.bindings.get(BaboonLang.Cs) match {
        case None                                                               => q""
        case Some(Typedef.ForeignEntry(_, _: Typedef.ForeignMapping.BaboonRef)) => q""
        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) =>
          val srcRef    = trans.asCsTypeKeepForeigns(f.id, domain, evo)
          val codecName = s"${srcRef.name}_KeyCodec"
          val hostName  = s"${srcRef.name}_KeyCodecHost"
          val codecFqn  = s"${srcRef.pkg.parts.mkString(".")}.$hostName"
          // Stringy allowlist (PR-I-D06 pattern guidance: only the language's allowlist; no dead alternatives).
          val isStringy = decl == "string" || decl == "System.String"
          val defaultImpl = if (isStringy) {
            q"""private class DefaultImpl : $codecName {
               |    public $csString EncodeKey($name value) => value;
               |    public $name DecodeKey($csString s) => s;
               |}""".stripMargin
          } else {
            q"""private class DefaultImpl : $codecName {
               |    public $csString EncodeKey($name value) => throw new $baboonCodecException.DecoderFailure(\"$codecFqn is not registered; call $codecFqn.Register(impl) at app boot.\");
               |    public $name DecodeKey($csString s) => throw new $baboonCodecException.DecoderFailure(\"$codecFqn is not registered; call $codecFqn.Register(impl) at app boot.\");
               |}""".stripMargin
          }
          q"""public interface $codecName {
             |    $csString EncodeKey($name value);
             |    $name DecodeKey($csString s);
             |}
             |
             |public static class $hostName {
             |    private static volatile $codecName _instance = new DefaultImpl();
             |    public static void Register($codecName impl) { _instance = impl; }
             |    public static $codecName Instance => _instance;
             |
             |    ${defaultImpl.shift(4).trim}
             |}""".stripMargin
      }
    }

    /** Wrap a per-type tree in its enclosing C# container. Inline
      * service-method I/O types nest in `static partial class <Service> {
      * static partial class <Method> { … } }` (the interface-companion layout);
      * everything else lives in a flat namespace. Shared by the definition,
      * fixture, and test emitters so all three agree on the type's location. */
    private def wrapInContainer(defn: DomainMember.User, srcRef: CSValue.CSType, tree: TextTree[CSValue]): TextTree[CSValue] = {
      trans.serviceMethodContainers(defn.id, domain, evo) match {
        case Some((nsPrefix, classes)) =>
          val nested = classes.foldRight(tree) {
            (cls, acc) =>
              q"""public static partial class $cls {
                 |    ${acc.shift(4).trim}
                 |}""".stripMargin
          }
          csTrees.inNs(nsPrefix, nested)
        case None =>
          csTrees.inNs(srcRef.pkg.parts.toSeq, tree)
      }
    }

    /** Wrap a fixture/test tree. For inline service-method types this is the
      * `serviceMethodFixtureNs` namespace (NOT the static-class companion): the
      * fixture/test classes are siblings referencing the type by FQN, so the
      * separate test assembly compiles (no cross-assembly `partial`). Other
      * types keep the fixture beside the type's own namespace. */
    private def wrapFixtureNs(defn: DomainMember.User, srcRef: CSValue.CSType, tree: TextTree[CSValue]): TextTree[CSValue] = {
      trans.serviceMethodFixtureNs(defn.id, domain, evo) match {
        case Some(ns) => csTrees.inNs(ns, tree)
        case None     => csTrees.inNs(srcRef.pkg.parts.toSeq, tree)
      }
    }

    private def makeFixtureRepr(defn: DomainMember.User): Option[TextTree[CSValue]] = {
      val srcRef = trans.asCsTypeKeepForeigns(defn.id, domain, evo)

      val fixtureTree       = codecsFixture.translate(defn)
      val fixtureTreeWithNs = fixtureTree.map(t => wrapFixtureNs(defn, srcRef, t))

      fixtureTreeWithNs
    }

    private def makeTestRepr(defn: DomainMember.User): Option[TextTree[CSValue]] = {
      val csTypeRef = trans.asCsType(defn.id, domain, evo)
      val srcRef    = trans.asCsTypeKeepForeigns(defn.id, domain, evo)

      val testTree       = codecsTests.translate(defn, csTypeRef, srcRef)
      val testTreeWithNs = testTree.map(t => wrapFixtureNs(defn, srcRef, t))

      testTreeWithNs
    }

    private def renderContractFields(
      fields: List[Field]
    ): List[TextTree[CSValue]] = {
      fields.map {
        f =>
          val tpe      = trans.asCsRef(f.tpe, domain, evo)
          val mname    = s"${f.name.name.capitalize}"
          val fieldEx  = q"public $tpe $mname { get; }"
          prependDocs(f.docs, fieldEx)
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
              q"$BaboonTools.SeqHashcode($ref, $itemRef => ${renderHashcode(itemRef, subComparator, depth + 1)})"
            case ComparatorType.SetEquals(subComparator) =>
              q"$BaboonTools.SetHashcode($ref, $itemRef => ${renderHashcode(itemRef, subComparator, depth + 1)})"
            case ComparatorType.MapEquals(keyComparator, valComparator) =>
              val hk = renderHashcode(
                q"$itemRef",
                keyComparator,
                depth + 1,
              )

              val hv = renderHashcode(q"$itemRef", valComparator, depth + 1)
              q"$BaboonTools.MapHashcode($ref, $itemRef => $hk, $itemRef => $hv)"
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
              q"$BaboonTools.OptionEquals($ref, $oref, (left, right) => ${renderComparator(q"left", q"right", c)})"
          }

        case ComparatorType.SeqEquals(subComparator) =>
          subComparator match {
            case _: ComparatorType.Basic =>
              q"$ref.SequenceEqual($oref)"
            case c: ComparatorType.Complex =>
              q"$BaboonTools.SeqEquals($ref, $oref, (left, right) => ${renderComparator(q"left", q"right", c)})"
          }

        case ComparatorType.SetEquals(_) =>
          q"$ref.SetEquals($oref)"

        case ComparatorType.MapEquals(_, valComp) =>
          val cmp = renderComparator(q"left", q"right", valComp)
          q"$BaboonTools.MapEquals($ref, $oref, (left, right) => $cmp)"
      }
    }

    // ----- Identifier toString + parseRepr emission (PR-57a) -----
    // Spec contract: docs/spec/identifier-repr.md. Mirrors ScDefnTranslator
    // section but uses C# stdlib + BaboonIdentifierRepr runtime helper.

    private sealed trait IdentifierFieldKind
    private object IdentifierFieldKind {
      case object Bit extends IdentifierFieldKind
      case object SignedInt extends IdentifierFieldKind /* i08/i16/i32/i64 */
      case object UnsignedSmallInt extends IdentifierFieldKind /* u08/u16/u32 */
      case object UnsignedLong extends IdentifierFieldKind /* u64 */
      case object Str extends IdentifierFieldKind
      case object Uid extends IdentifierFieldKind
      case object Tsu extends IdentifierFieldKind
      case object Tso extends IdentifierFieldKind
      case object Bytes extends IdentifierFieldKind
      final case class NestedId(id: TypeId.User) extends IdentifierFieldKind
    }

    private def identifierFieldKind(tpe: TypeRef): IdentifierFieldKind = {
      tpe match {
        case TypeRef.Scalar(b: TypeId.BuiltinScalar) =>
          import TypeId.Builtins.*
          b match {
            case `bit`                         => IdentifierFieldKind.Bit
            case `i08` | `i16` | `i32` | `i64` => IdentifierFieldKind.SignedInt
            case `u08` | `u16` | `u32`         => IdentifierFieldKind.UnsignedSmallInt
            case `u64`                         => IdentifierFieldKind.UnsignedLong
            case `str`                         => IdentifierFieldKind.Str
            case `uid`                         => IdentifierFieldKind.Uid
            case `tsu`                         => IdentifierFieldKind.Tsu
            case `tso`                         => IdentifierFieldKind.Tso
            case `bytes`                       => IdentifierFieldKind.Bytes
            case other =>
              throw new IllegalStateException(s"Identifier field has unsupported scalar $other; validator should have rejected this.")
          }
        case TypeRef.Scalar(uid: TypeId.User) =>
          IdentifierFieldKind.NestedId(uid)
        case other =>
          throw new IllegalStateException(s"Identifier field has unsupported TypeRef $other; validator should have rejected this.")
      }
    }

    private def renderFieldValueExpr(csFieldName: String, kind: IdentifierFieldKind): TextTree[CSValue] = {
      kind match {
        case IdentifierFieldKind.Bit              => q"$baboonIdRepr.BitToString(this.$csFieldName)"
        case IdentifierFieldKind.SignedInt        => q"this.$csFieldName.ToString($csInvariantCulture.InvariantCulture)"
        case IdentifierFieldKind.UnsignedSmallInt =>
          // u08/u16/u32 already map to C# unsigned types; ToString(InvariantCulture) is
          // unsigned-correct unlike Java/Scala where the same value is signed.
          q"this.$csFieldName.ToString($csInvariantCulture.InvariantCulture)"
        case IdentifierFieldKind.UnsignedLong => q"$baboonIdRepr.U64ToString(this.$csFieldName)"
        case IdentifierFieldKind.Str          => q"$baboonIdRepr.EscapeStr(this.$csFieldName)"
        case IdentifierFieldKind.Uid          =>
          // Guid.ToString() default form is 32 lowercase hex digits with hyphens (RFC 4122).
          // Spec §3 mandates this exact form.
          q"this.$csFieldName.ToString()"
        case IdentifierFieldKind.Tsu         => q"$baboonIdRepr.TsuToString(this.$csFieldName.DateTimeOffset)"
        case IdentifierFieldKind.Tso         => q"$baboonIdRepr.TsoToString(this.$csFieldName.DateTimeOffset)"
        case IdentifierFieldKind.Bytes       => q"$baboonIdRepr.BytesToHex(this.$csFieldName)"
        case IdentifierFieldKind.NestedId(_) => q""""{" + this.$csFieldName.ToString() + "}""""
      }
    }

    private def renderIdentifierToString(dto: Typedef.Dto, name: CSType): TextTree[CSValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString
      val header     = s"$simpleName:$versionStr#"

      val fieldExprs: List[TextTree[CSValue]] = dto.fields.map {
        f =>
          val srcFieldName = f.name.name
          val csFieldName  = srcFieldName.capitalize
          val kind         = identifierFieldKind(f.tpe)
          val valueExpr    = renderFieldValueExpr(csFieldName, kind)
          // The repr field name is the source name per spec §2.1.
          q""""$srcFieldName:" + ($valueExpr)"""
      }

      val joinedFields =
        if (fieldExprs.isEmpty) q""""""""
        else fieldExprs.toSeq.join(""" + ":" + """)

      q"""public override string ToString()
         |{
         |    return "$header" + $joinedFields;
         |}""".stripMargin
    }

    private def signedNarrowCast(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.i08) => "(sbyte)"
        case TypeRef.Scalar(TypeId.Builtins.i16) => "(short)"
        case TypeRef.Scalar(TypeId.Builtins.i32) => "(int)"
        case TypeRef.Scalar(TypeId.Builtins.i64) => ""
        case other                               => throw new IllegalStateException(s"signedNarrowCast on non-signed-int: $other")
      }
    }

    private def signedRangeCheck(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.i08) => "v >= -128L && v <= 127L"
        case TypeRef.Scalar(TypeId.Builtins.i16) => "v >= -32768L && v <= 32767L"
        case TypeRef.Scalar(TypeId.Builtins.i32) => "v >= -2147483648L && v <= 2147483647L"
        case TypeRef.Scalar(TypeId.Builtins.i64) => "true"
        case other                               => throw new IllegalStateException(s"signedRangeCheck on non-signed-int: $other")
      }
    }

    private def signedTypeName(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.i08) => "i08"
        case TypeRef.Scalar(TypeId.Builtins.i16) => "i16"
        case TypeRef.Scalar(TypeId.Builtins.i32) => "i32"
        case TypeRef.Scalar(TypeId.Builtins.i64) => "i64"
        case other                               => throw new IllegalStateException(s"signedTypeName on non-signed-int: $other")
      }
    }

    private def unsignedSmallNarrowCast(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => "(byte)"
        case TypeRef.Scalar(TypeId.Builtins.u16) => "(ushort)"
        case TypeRef.Scalar(TypeId.Builtins.u32) => "(uint)"
        case other                               => throw new IllegalStateException(s"unsignedSmallNarrowCast on non-u08/u16/u32: $other")
      }
    }

    private def unsignedSmallRangeCheck(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => "v <= 255UL"
        case TypeRef.Scalar(TypeId.Builtins.u16) => "v <= 65535UL"
        case TypeRef.Scalar(TypeId.Builtins.u32) => "v <= 4294967295UL"
        case other                               => throw new IllegalStateException(s"unsignedSmallRangeCheck on non-u08/u16/u32: $other")
      }
    }

    private def unsignedSmallTypeName(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => "u08"
        case TypeRef.Scalar(TypeId.Builtins.u16) => "u16"
        case TypeRef.Scalar(TypeId.Builtins.u32) => "u32"
        case other                               => throw new IllegalStateException(s"unsignedSmallTypeName on non-u08/u16/u32: $other")
      }
    }

    private def renderIdentifierCodecClass(dto: Typedef.Dto, name: CSType): TextTree[CSValue] = {
      val simpleName     = name.name
      val versionStr     = domain.version.toString
      val codecClassName = s"${name.name}Codec"

      // Per-field decoder. Operates on `cursor`. Accumulates decoded values
      // into local vars `<srcFieldName>_v` and finally constructs the type.
      val fieldDecoders: List[TextTree[CSValue]] = dto.fields.zipWithIndex.map {
        case (f, idx) =>
          val srcFieldName = f.name.name
          val rawVar       = s"${srcFieldName}_raw"
          val valVar       = s"${srcFieldName}_v"
          val isLast       = idx == dto.fields.length - 1
          val kind         = identifierFieldKind(f.tpe)
          val tpe          = trans.asCsRef(f.tpe, domain, evo)

          val parseHead =
            q"""{
               |    var __r = $baboonIdRepr.ParseFieldName(cursor, "$srcFieldName");
               |    if (__r is $either<string, $unit>.Left __l) return $either.Left<string, $name>(__l.Value);
               |}""".stripMargin

          val parseValue: TextTree[CSValue] = kind match {
            case IdentifierFieldKind.Bit =>
              q"""var $rawVar = cursor.ReadUntilStructural();
                 |bool $valVar;
                 |{
                 |    var __r = $baboonIdRepr.ParseBit($rawVar);
                 |    if (__r is $either<string, bool>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |    $valVar = (($either<string, bool>.Right)__r).Value;
                 |}""".stripMargin
            case IdentifierFieldKind.SignedInt =>
              val rangeCheck = signedRangeCheck(f.tpe)
              val typeName   = signedTypeName(f.tpe)
              val cast       = signedNarrowCast(f.tpe)
              // i64 has the full long range — no narrow check needed (and emitting
              // `if (!(true))` triggers C# CS0162 "unreachable code" on the rejection branch).
              val rangeBlock =
                if (rangeCheck == "true") q""
                else
                  q"""    if (!($rangeCheck))
                     |    {
                     |        return $either.Left<string, $name>("$typeName out of range for field $srcFieldName: " + $rawVar);
                     |    }
                     |""".stripMargin
              q"""var $rawVar = cursor.ReadUntilStructural();
                 |$tpe $valVar;
                 |{
                 |    // Spec §5.4: signed integers must not carry a leading '+'.
                 |    if ($rawVar.Length > 0 && $rawVar[0] == '+')
                 |    {
                 |        return $either.Left<string, $name>("signed integer must not have leading '+' for field $srcFieldName: " + $rawVar);
                 |    }
                 |    if (!long.TryParse($rawVar, $csNumberStyles.AllowLeadingSign, $csInvariantCulture.InvariantCulture, out var v))
                 |    {
                 |        return $either.Left<string, $name>("could not parse signed integer for field $srcFieldName: " + $rawVar);
                 |    }
                 |$rangeBlock    $valVar = $cast v;
                 |}""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              val rangeCheck = unsignedSmallRangeCheck(f.tpe)
              val typeName   = unsignedSmallTypeName(f.tpe)
              val cast       = unsignedSmallNarrowCast(f.tpe)
              q"""var $rawVar = cursor.ReadUntilStructural();
                 |$tpe $valVar;
                 |{
                 |    if (!ulong.TryParse($rawVar, $csNumberStyles.None, $csInvariantCulture.InvariantCulture, out var v))
                 |    {
                 |        return $either.Left<string, $name>("could not parse unsigned integer for field $srcFieldName: " + $rawVar);
                 |    }
                 |    if (!($rangeCheck))
                 |    {
                 |        return $either.Left<string, $name>("$typeName out of range for field $srcFieldName: " + $rawVar);
                 |    }
                 |    $valVar = $cast v;
                 |}""".stripMargin
            case IdentifierFieldKind.UnsignedLong =>
              q"""var $rawVar = cursor.ReadUntilStructural();
                 |ulong $valVar;
                 |{
                 |    if (!ulong.TryParse($rawVar, $csNumberStyles.None, $csInvariantCulture.InvariantCulture, out var v))
                 |    {
                 |        return $either.Left<string, $name>("could not parse u64 for field $srcFieldName: " + $rawVar);
                 |    }
                 |    $valVar = v;
                 |}""".stripMargin
            case IdentifierFieldKind.Str =>
              q"""string $valVar;
                 |{
                 |    var __r = cursor.ReadStrField();
                 |    if (__r is $either<string, string>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |    $valVar = (($either<string, string>.Right)__r).Value;
                 |}""".stripMargin
            case IdentifierFieldKind.Uid =>
              // Spec §5.4 mandates lowercase hex form. Validate before delegating to
              // Guid.Parse (which accepts mixed/uppercase).
              q"""var $rawVar = cursor.ReadUntilStructural();
                 |$csGuid $valVar;
                 |{
                 |    if (!$baboonIdRepr.UidLowerRegex.IsMatch($rawVar))
                 |    {
                 |        return $either.Left<string, $name>("uid not in canonical lowercase form for field $srcFieldName: " + $rawVar);
                 |    }
                 |    if (!$csGuid.TryParseExact($rawVar, "D", out var v))
                 |    {
                 |        return $either.Left<string, $name>("could not parse uid for field $srcFieldName: " + $rawVar);
                 |    }
                 |    $valVar = v;
                 |}""".stripMargin
            case IdentifierFieldKind.Tsu =>
              // tsu fixed-width lexeme: 24 chars `yyyy-MM-ddTHH:mm:ss.SSSZ` per spec §3 / §5.4.
              q"""string $rawVar;
                 |{
                 |    var __rf = cursor.ReadFixed(24);
                 |    if (__rf is $either<string, string>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |    $rawVar = (($either<string, string>.Right)__rf).Value;
                 |}
                 |$rpDateTime $valVar;
                 |{
                 |    var __r = $baboonIdRepr.ParseTsuRepr($rawVar);
                 |    if (__r is $either<string, $csDateTime>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |    var __dt = (($either<string, $csDateTime>.Right)__r).Value;
                 |    $valVar = new $rpDateTime(__dt);
                 |}""".stripMargin
            case IdentifierFieldKind.Tso =>
              // tso fixed-width lexeme: 29 chars `yyyy-MM-ddTHH:mm:ss.SSS±HH:MM` per spec §3 / §5.4.
              q"""string $rawVar;
                 |{
                 |    var __rf = cursor.ReadFixed(29);
                 |    if (__rf is $either<string, string>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |    $rawVar = (($either<string, string>.Right)__rf).Value;
                 |}
                 |$rpDateTime $valVar;
                 |{
                 |    var __r = $baboonIdRepr.ParseTsoRepr($rawVar);
                 |    if (__r is $either<string, $csDateTimeOffset>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |    var __dto = (($either<string, $csDateTimeOffset>.Right)__r).Value;
                 |    $valVar = new $rpDateTime(__dto);
                 |}""".stripMargin
            case IdentifierFieldKind.Bytes =>
              q"""var $rawVar = cursor.ReadUntilStructural();
                 |$csByteString $valVar;
                 |{
                 |    var __r = $baboonIdRepr.ParseBytesHex($rawVar);
                 |    if (__r is $either<string, $csByteString>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |    $valVar = (($either<string, $csByteString>.Right)__r).Value;
                 |}""".stripMargin
            case IdentifierFieldKind.NestedId(uid) =>
              val nestedTpe   = trans.asCsTypeKeepForeigns(uid, domain, evo)
              val nestedCodec = CSType(nestedTpe.pkg, s"${nestedTpe.name}Codec", fq = false, CSTypeOrigin.Other)
              q"""{
                 |    var __r = cursor.Expect('{');
                 |    if (__r is $either<string, $unit>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |}
                 |$nestedTpe $valVar;
                 |{
                 |    var __r = $nestedCodec.ParseRepr(cursor);
                 |    if (__r is $either<string, $nestedTpe>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |    $valVar = (($either<string, $nestedTpe>.Right)__r).Value;
                 |}
                 |{
                 |    var __r = cursor.Expect('}');
                 |    if (__r is $either<string, $unit>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |}""".stripMargin
          }

          val sep =
            if (isLast) q""
            else
              q"""{
                 |    var __r = cursor.Expect(':');
                 |    if (__r is $either<string, $unit>.Left __l) return $either.Left<string, $name>(__l.Value);
                 |}""".stripMargin

          q"""$parseHead
             |$parseValue
             |$sep""".stripMargin.trim
      }

      val constructorArgs = dto.fields.map {
        f =>
          val srcFieldName = f.name.name
          val csFieldName  = srcFieldName.capitalize
          q"$csFieldName: ${srcFieldName}_v"
      }.toSeq

      val ctor =
        if (constructorArgs.nonEmpty) q"new $name(${constructorArgs.join(", ")})"
        else q"new $name()"

      val body = (fieldDecoders :+ q"return $either.Right<string, $name>($ctor);").joinNN()

      q"""public static class $codecClassName {
         |    /// <summary>Parse the canonical identifier repr per docs/spec/identifier-repr.md.
         |    /// Schema-directed parser: walks declared field order and dispatches per
         |    /// field type. Returns Left on any malformed input.</summary>
         |    public static $either<string, $name> ParseRepr(string s)
         |    {
         |        var cursor = new $baboonIdReprCursor(s);
         |        var inner  = ParseRepr(cursor);
         |        if (inner is $either<string, $name>.Left l) return l;
         |        if (!cursor.AtEnd) return $either.Left<string, $name>("unexpected trailing input at " + cursor.Position);
         |        return inner;
         |    }
         |
         |    public static $either<string, $name> ParseRepr($baboonIdReprCursor cursor)
         |    {
         |        {
         |            var __r = $baboonIdRepr.ParseHeader(cursor, "$simpleName", "$versionStr");
         |            if (__r is $either<string, $unit>.Left __l) return $either.Left<string, $name>(__l.Value);
         |        }
         |        ${body.shift(8).trim}
         |    }
         |}""".stripMargin
    }
  }
}
