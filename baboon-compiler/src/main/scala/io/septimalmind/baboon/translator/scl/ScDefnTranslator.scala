package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.scl.ScValue.ScType
import io.septimalmind.baboon.typer.EnumWireStyle
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait ScDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[ScDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[ScDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[ScDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[ScDefnTranslator.Output]]
}

object ScDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: ScType,
    tpeKeepForeigns: ScType,
    tpeId: TextTree[ScValue],
    trees: Map[String, TextTree[ScValue]],
  )

  final case class DefnRepr(
    defn: TextTree[ScValue],
    codecs: List[CodecReg],
  )

  /** Discriminator for identifier-repr field rendering — see PR-56 spec
    * (`docs/spec/identifier-repr.md` §3). All cases are populated from the
    * validator-restricted set of allowed field types.
    */
  sealed trait IdentifierFieldKind
  object IdentifierFieldKind {
    case object Bit extends IdentifierFieldKind
    case object SignedInt extends IdentifierFieldKind /* i08/i16/i32/i64 */
    case object UnsignedSmallInt extends IdentifierFieldKind /* u08/u16/u32 */
    case object UnsignedLong extends IdentifierFieldKind /* u64 */
    case object Str extends IdentifierFieldKind
    case object Uid extends IdentifierFieldKind
    case object Tsu extends IdentifierFieldKind
    case object Tso extends IdentifierFieldKind
    case object Bytes extends IdentifierFieldKind
    final case class NestedId(id: io.septimalmind.baboon.typer.model.TypeId.User) extends IdentifierFieldKind
  }

  final case class Output(
    path: String,
    tree: TextTree[ScValue],
    pkg: ScValue.ScPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[ScValue]])] = Nil,
  )

  class ScDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: ScTarget,
    domain: Domain,
    evo: BaboonEvolution,
    scFiles: ScFileTools,
    scTrees: ScTreeTools,
    trans: ScTypeTranslator,
    codecs: Set[ScCodecTranslator],
    codecTests: ScCodecTestsTranslator,
    codecsFixture: ScCodecFixtureTranslator,
    wiringTranslator: ScServiceWiringTranslator,
    scDomainTreeTools: ScDomainTreeTools,
  ) extends ScDefnTranslator[F] {
    import ScTypes.*

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr   = makeFullRepr(defn, inNs = true)
      val srcRef = trans.toScTypeRefKeepForeigns(defn.id, domain, evo)

      val registrations = codecs.toList.map(codec => codec.id -> repr.codecs.flatMap(reg => reg.trees.get(codec.id).map(expr => q"${reg.tpeId}, $expr")))

      val mainOutput = Output(
        getOutputPath(defn),
        repr.defn,
        srcRef.pkg,
        CompilerProduct.Definition,
        codecReg = registrations,
      )

      val wiringOutput = wiringTranslator
        .translate(defn).map {
          wiringTree =>
            val pkg     = srcRef.pkg
            val wrapped = scTrees.inNs(pkg.parts.toSeq, wiringTree)
            Output(
              getOutputPath(defn, suffix = Some("_Wiring")),
              wrapped,
              pkg,
              CompilerProduct.Definition,
            )
        }.toList

      F.pure(mainOutput :: wiringOutput)
    }

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateFixtures(defn)
      }
    }

    private def doTranslateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val srcRef = trans.toScTypeRefKeepForeigns(defn.id, domain, evo)
      val fixtureTreeOut = makeFixtureRepr(defn).map {
        fixtureTreeWithNs =>
          Output(
            getOutputPath(defn, suffix = Some("_Fixture")),
            fixtureTreeWithNs,
            srcRef.pkg,
            CompilerProduct.Fixture,
          )
      }

      F.pure(fixtureTreeOut.toList)
    }
    private def makeFixtureRepr(defn: DomainMember.User): Option[TextTree[ScValue]] = {
      val srcRef = trans.toScTypeRefKeepForeigns(defn.id, domain, evo)
      val ns     = srcRef.pkg.parts

      val fixtureTree       = codecsFixture.translate(defn)
      val fixtureTreeWithNs = fixtureTree.map(t => scTrees.inNs(ns.toSeq, t))

      fixtureTreeWithNs
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTest(defn)
      }
    }
    private def doTranslateTest(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val srcRef = trans.toScTypeRefKeepForeigns(defn.id, domain, evo)
      val codecTestOut = makeTestRepr(defn).map {
        codecTestWithNS =>
          Output(
            getOutputPath(defn, suffix = Some("_Tests")),
            codecTestWithNS,
            srcRef.pkg,
            CompilerProduct.Test,
          )
      }

      F.pure(codecTestOut.toList)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map {
        tree =>
          val pkg     = trans.toScPkg(domain.id, domain.version, evo)
          val wrapped = scTrees.inNs(pkg.parts.toSeq, tree)
          val fbase   = scFiles.basename(domain, evo)
          Output(
            s"$fbase/BaboonServiceRt.scala",
            wrapped,
            pkg,
            CompilerProduct.Definition,
          )
      }.toList
      F.pure(result)
    }

    private def makeTestRepr(defn: DomainMember.User): Option[TextTree[ScValue]] = {
      val csTypeRef = trans.asScType(defn.id, domain, evo)
      val srcRef    = trans.toScTypeRefKeepForeigns(defn.id, domain, evo)
      val ns        = srcRef.pkg.parts

      val testTree       = codecTests.translate(defn, csTypeRef, srcRef)
      val testTreeWithNs = testTree.map(t => scTrees.inNs(ns.toSeq, t))

      testTreeWithNs
    }

    /** Conditionally prepend `@deprecated(...)` to a tree on non-latest versions.
      *
      * Scala annotation grammar binds an annotation to the immediately
      * following declaration only, so when a single TextTree carries multiple
      * top-level decls (e.g. `trait X { ... }` plus its companion
      * `object X { ... }`), prepending one annotation marks ONLY the trait —
      * the companion is left un-annotated. Callers that emit multiple decls
      * must call this helper per decl, not on the joined result. See PR-I-D04.
      */
    private def obsoletePrevious(tree: TextTree[ScValue]): TextTree[ScValue] = {
      if (domain.version == evo.latest || tree.isEmpty) {
        tree
      } else {
        q"""@${ScTypes.deprecated}("Version ${domain.version.v.toString} is deprecated, you should migrate to ${evo.latest.v.toString}")
           |$tree""".stripMargin
      }
    }

    private def makeFullRepr(
      defn: DomainMember.User,
      inNs: Boolean,
    ): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      val scTypeRef = trans.asScType(defn.id, domain, evo)
      val srcRef    = trans.toScTypeRefKeepForeigns(defn.id, domain, evo)

      val repr = makeRepr(defn, scTypeRef, isLatestVersion)

      val codecTrees =
        codecs.toList
          .flatMap(t => t.translate(defn, scTypeRef, srcRef).toList)
          .map(obsoletePrevious)

      // PR-I-D04: Foreign emits two top-level decls (trait + companion object)
      // and applies obsoletePrevious per-decl inside makeRepr. Skip the outer
      // annotation so we don't double-annotate the trait.
      // D01 fix: apply obsoletePrevious BEFORE prependDocs so the emitted order
      // is /** doc */ then @deprecated then the declaration, matching Scaladoc
      // convention (doc-then-annotation-then-symbol).
      val defnRepr = defn.defn match {
        // Spec §3.1: docs attach to the user-facing trait (not the companion,
        // which is implementation scaffolding). obsoletePrevious is applied
        // per-decl inside makeRepr, so repr.defn already has @deprecated on
        // each part; prependDocs goes last to place the doc block above it.
        case _: Typedef.Foreign => prependDocs(defn.docs, repr.defn)
        case _                  => prependDocs(defn.docs, obsoletePrevious(repr.defn))
      }

      assert(defn.id.pkg == domain.id)

      val ns = srcRef.pkg.parts

      val allDefs = (defnRepr +: codecTrees).joinNN()
      val content = if (inNs) scTrees.inNs(ns.toSeq, allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => Nil
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap(
              codec =>
                if (codec.isActive(d.id)) List(codec.id -> q"$baboonLazy(${codec.codecName(srcRef)})")
                else Nil
            )
          List(CodecReg(defn.id, scTypeRef, srcRef, q"\"${defn.id.toString}\"", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      DefnRepr(content, allRegs)
    }

    /** Prepend a Javadoc-style doc comment block before a tree when `docs` is
      * non-empty. Returns the tree unchanged when `docs` is empty.
      */
    private def prependDocs(docs: Docs, tree: TextTree[ScValue]): TextTree[ScValue] = {
      val block = scTrees.renderDocs(docs, "")
      if (block.isEmpty) tree else q"${block}$tree"
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: ScValue.ScType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = scDomainTreeTools.makeDataMeta(defn)
      val codecMeta = codecs.flatMap(_.codecMeta(defn, name).map(_.member)).map {
        m => if (isLatestVersion) m else q"""@scala.annotation.nowarn("cat=deprecation") $m"""
      }

      defn.defn match {
        case dto: Typedef.Dto =>
          val params = dto.fields.map {
            f =>
              val t       = trans.asScRef(f.tpe, domain, evo)
              val fieldEx = q"${f.name.name}: $t"
              prependDocs(f.docs, fieldEx)
          }
          val paramsList      = if (params.nonEmpty) params.join(",\n") else q""
          val contractParents = dto.contracts.map(c => trans.toScTypeRefKeepForeigns(c, domain, evo))
          val adtParents = dto.id.owner match {
            case Owner.Adt(id) => Seq(trans.toScTypeRefKeepForeigns(id, domain, evo), iBaboonAdtMemberMeta)
            case _             => Seq.empty
          }
          val parents          = adtParents ++ contractParents :+ genMarker
          val extendsClauseDto = if (parents.nonEmpty) q" extends ${parents.map(t => q"$t").join(" with ")}" else q""

          val objectMetaFields = mainMeta.map(_.valueField) ++ codecMeta
          val classMetaFields  = mainMeta.map(mt => q"override ${mt.refValueField}")

          // Identifier toString + parseRepr (PR-56 / spec: docs/spec/identifier-repr.md).
          // Emitted only when `dto.isIdentifier == true`. Wire format:
          // <SimpleName>:<version>#field:value:field:value:{<NestedName>:<version>#…}
          val identifierToStringOverride: TextTree[ScValue] =
            if (dto.isIdentifier) renderIdentifierToString(dto, name) else q""

          val identifierCodecObject: TextTree[ScValue] =
            if (dto.isIdentifier) renderIdentifierCodecObject(dto, name) else q""

          val classBody = (classMetaFields :+ identifierToStringOverride).filter(_.nonEmpty).joinN()

          val mainTree =
            q"""final case class ${name.name}(
               |  ${paramsList.shift(2).trim}
               |)$extendsClauseDto {
               |  ${classBody.shift(2).trim}
               |}
               |
               |object ${name.name} {
               |  ${objectMetaFields.joinN().shift(2).trim}
               |}""".stripMargin

          val combined =
            if (dto.isIdentifier) q"""$mainTree
                                     |
                                     |$identifierCodecObject""".stripMargin
            else mainTree

          DefnRepr(combined, Nil)

        case e: Typedef.Enum =>
          val traitTree = q"sealed trait ${name.name}"

          val cases = e.members.map {
            m =>
              val obj = EnumWireStyle.wireName(m.name)
              q"case object $obj extends ${name.name}"
          }.toList

          val parseCases = e.members.map {
            m =>
              val obj = EnumWireStyle.wireName(m.name)
              q"case \"$obj\" => Some($obj)"
          }.toList

          val names = e.members.map {
            m =>
              val obj = EnumWireStyle.wireName(m.name)
              q"$obj"
          }.toList

          val companion =
            q"""object ${name.name} extends $baboonEnum[${name.name}] {
               |  ${cases.joinN().shift(2).trim}
               |  
               |  def parse(s: $scString): $scOption[${name.name}] = {
               |    s match {
               |      ${parseCases.joinN().shift(6).trim}
               |      case _ => None
               |    }
               |  }
               |  
               |  def all: $scList[${name.name}] = $scList(
               |    ${names.join(",\n").shift(4).trim}
               |  ) 
               |}""".stripMargin
          DefnRepr(Seq(traitTree, companion).joinNN(), Nil)

        case adt: Typedef.Adt =>
          val parents          = adt.contracts.map(c => trans.toScTypeRefKeepForeigns(c, domain, evo)) :+ genMarker
          val extendsClauseAdt = if (parents.nonEmpty) q" extends ${parents.map(t => q"$t").join(" with ")}" else q""
          val sealedTrait      = q"""sealed trait ${name.name}$extendsClauseAdt""".stripMargin
          val memberTrees = adt.members.map {
            mid =>
              domain.defs.meta.nodes(mid) match {
                case mdefn: DomainMember.User => makeFullRepr(mdefn, inNs = false)
                case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
              }
          }

          val objectMetaFields = mainMeta.map(_.valueField) ++ codecMeta
          val classMetaFields  = mainMeta.map(mt => q"override ${mt.refValueField}")

          val adtTree =
            q"""$sealedTrait {
               |  ${classMetaFields.joinN().shift(2).trim}
               |}
               |
               |object ${name.name} {
               |  ${memberTrees.map(_.defn).toList.joinNN().shift(2).trim}
               |  ${objectMetaFields.joinN().shift(2).trim}
               |}""".stripMargin

          DefnRepr(adtTree, Nil)

        case contract: Typedef.Contract =>
          val methods = contract.fields.map {
            f =>
              val t        = trans.asScRef(f.tpe, domain, evo)
              val methodEx = q"def ${f.name.name}: $t"
              prependDocs(f.docs, methodEx)
          }
          val parents       = contract.contracts.map(c => trans.toScTypeRefKeepForeigns(c, domain, evo)) :+ genMarker
          val extendsClause = if (parents.nonEmpty) q" extends ${parents.map(t => q"$t").join(" with ")}" else q""
          val body          = if (methods.nonEmpty) methods.joinN() else q""
          DefnRepr(
            q"""trait ${name.name}$extendsClause {
               |    ${body.shift(4).trim}
               |}""".stripMargin,
            Nil,
          )

        case service: Typedef.Service =>
          val resolved    = ServiceResultResolver.resolve(domain, "scala", target.language.serviceResult, target.language.pragmas)
          val resolvedCtx = ServiceContextResolver.resolve(domain, "scala", target.language.serviceContext, target.language.pragmas)
          val ctxParam = resolvedCtx match {
            case ResolvedServiceContext.NoContext               => ""
            case ResolvedServiceContext.AbstractContext(tn, pn) => s"$pn: $tn, "
            case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$pn: $tn, "
          }
          val methods = service.methods.map {
            m =>
              val in              = trans.asScRef(m.sig, domain, evo)
              val out             = m.out.map(trans.asScRef(_, domain, evo))
              val err             = m.err.map(trans.asScRef(_, domain, evo))
              val servicePkgParts = name.pkg.parts.toList
              val scFqName: ScValue => String = {
                case t: ScValue.ScType if t.predef => t.name
                case t: ScValue.ScType =>
                  val typeParts = (t.pkg.parts :+ t.name).toList
                  if (typeParts.startsWith(servicePkgParts)) typeParts.drop(servicePkgParts.size).mkString(".")
                  else typeParts.mkString(".")
              }
              val outStr   = out.map(_.mapRender(scFqName)).getOrElse("")
              val errStr   = err.map(_.mapRender(scFqName))
              val retStr   = resolved.renderReturnType(outStr, errStr, "Unit")
              val methodEx = q"def ${m.name.name}(${ctxParam}arg: $in): $retStr"
              prependDocs(m.docs, methodEx)
          }
          val typeParams = Seq(
            resolved.traitTypeParam,
            resolvedCtx match {
              case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
              case _                                             => None
            },
          ).flatten
          val traitTypeParam = if (typeParams.nonEmpty) typeParams.mkString("[", ", ", "]") else ""
          val body           = if (methods.nonEmpty) methods.joinN() else q""
          DefnRepr(
            q"""trait ${name.name}$traitTypeParam {
               |    ${body.shift(4).trim}
               |}""".stripMargin,
            Nil,
          )

        case f: Typedef.Foreign =>
          // PR-I-D04: Apply obsoletePrevious per-tree (trait + companion) so
          // both decls receive the @deprecated annotation on non-latest
          // versions. Joining first and annotating once would only mark the
          // trait — see Scala annotation grammar.
          val parts = makeForeignKeyCodecRepr(f, name).map(obsoletePrevious)
          DefnRepr(parts.joinNN(), Nil)

      }
    }

    /** PR-I.1a (M24 Phase 3.1): emit a `<Foreign>_KeyCodec` extension hook for
      * every Custom-mapped Scala foreign declaration. The host application
      * registers an implementation at boot which the JSON codec then uses to
      * encode/decode map keys. For BaboonRef-mapped foreigns we emit nothing —
      * the existing recursion into the aliased type covers the codec needs.
      *
      * Stringy foreigns (`java.lang.String`) get a default identity impl so the
      * common case works out of the box. Non-stringy foreigns get a stub
      * default that throws BaboonCodecException.DecoderFailure with a clear
      * "host has not registered" message, giving the user a precise diagnostic
      * if they forget to register.
      *
      * Returns the trait and its companion object as SEPARATE TextTrees so
      * that callers can apply per-decl annotations (e.g. `@deprecated` on
      * non-latest versions). Joining first and annotating once would only mark
      * the trait, leaving the companion object un-annotated — see PR-I-D04
      * and Scala's annotation grammar.
      */
    private def makeForeignKeyCodecRepr(f: Typedef.Foreign, name: ScValue.ScType): List[TextTree[ScValue]] = {
      f.bindings.get(BaboonLang.Scala) match {
        case None                                                               => Nil
        case Some(Typedef.ForeignEntry(_, _: Typedef.ForeignMapping.BaboonRef)) => Nil
        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) =>
          val srcRef    = trans.toScTypeRefKeepForeigns(f.id, domain, evo)
          val traitName = s"${srcRef.name}_KeyCodec"
          val traitFqn  = s"${srcRef.pkg.parts.mkString(".")}.$traitName"
          val isStringy = decl == "java.lang.String"
          val defaultBlock = if (isStringy) {
            q"""private object DefaultImpl extends $traitName {
               |  def encodeKey(value: $name): $scString = value
               |  def decodeKey(s: $scString): $name = s
               |}""".stripMargin
          } else {
            q"""private object DefaultImpl extends $traitName {
               |  private def fail: Nothing = throw $baboonCodecException.DecoderFailure(
               |    \"$traitFqn is not registered; call $traitFqn.register(impl) at app boot.\"
               |  )
               |  def encodeKey(value: $name): $scString = fail
               |  def decodeKey(s: $scString): $name = fail
               |}""".stripMargin
          }
          val traitTree =
            q"""trait $traitName {
               |  def encodeKey(value: $name): $scString
               |  def decodeKey(s: $scString): $name
               |}""".stripMargin
          val companionTree =
            q"""object $traitName {
               |  @volatile private var _instance: $traitName = DefaultImpl
               |  def register(impl: $traitName): $scUnit = { _instance = impl }
               |  def instance: $traitName = _instance
               |
               |  ${defaultBlock.shift(2).trim}
               |}""".stripMargin
          List(traitTree, companionTree)
      }
    }

    /** Resolve a TypeRef to its underlying scalar/user kind for identifier-repr
      * dispatch. Aliases are already collapsed in BaboonTranslator (the field's
      * TypeRef is the resolved scalar), so we only need to discriminate
      * Scalar/User from collection/any (which the validator rejects).
      */
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

    /** Per-field render expression. The variable holding the field value is
      * named `value` because we wrap the body inside a per-field block.
      */
    private def renderFieldValueExpr(fieldName: String, kind: IdentifierFieldKind): TextTree[ScValue] = {
      kind match {
        case IdentifierFieldKind.Bit              => q"$baboonIdRepr.bitToString(this.$fieldName)"
        case IdentifierFieldKind.SignedInt        => q"this.$fieldName.toString"
        case IdentifierFieldKind.UnsignedSmallInt =>
          // u08/u16/u32 require width-aware masking. The single 32-bit mask used
          // here would silently produce wrong values for u08/u16. All call sites
          // MUST special-case UnsignedSmallInt and dispatch to renderUnsignedSmallInt.
          throw new IllegalStateException("UnsignedSmallInt requires width-aware emission via renderUnsignedSmallInt")
        case IdentifierFieldKind.UnsignedLong => q"$baboonIdRepr.u64ToString(this.$fieldName)"
        case IdentifierFieldKind.Str          => q"$baboonIdRepr.escapeStr(this.$fieldName)"
        case IdentifierFieldKind.Uid          => q"this.$fieldName.toString"
        case IdentifierFieldKind.Tsu          => q"$baboonIdRepr.tsuToString(this.$fieldName)"
        case IdentifierFieldKind.Tso          => q"$baboonIdRepr.tsoToString(this.$fieldName)"
        case IdentifierFieldKind.Bytes        => q"$baboonIdRepr.bytesToHex(this.$fieldName)"
        case IdentifierFieldKind.NestedId(_)  => q"\"{\" + this.$fieldName.toString + \"}\""
      }
    }

    /** Per-field rendering for u08/u16 needs separate masks. Collapse here for clarity. */
    private def renderUnsignedSmallInt(fieldName: String, b: TypeId.BuiltinScalar): TextTree[ScValue] = {
      import TypeId.Builtins.*
      b match {
        case `u08` => q"((this.$fieldName.toInt) & 0xFF).toString"
        case `u16` => q"((this.$fieldName.toInt) & 0xFFFF).toString"
        case `u32` => q"(this.$fieldName.toLong & 0xFFFFFFFFL).toString"
        case _     => throw new IllegalStateException(s"renderUnsignedSmallInt called with $b")
      }
    }

    private def renderIdentifierToString(dto: Typedef.Dto, name: ScValue.ScType): TextTree[ScValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString
      val header     = s"$simpleName:$versionStr#"

      val fieldExprs: List[TextTree[ScValue]] = dto.fields.map {
        f =>
          val fieldName = f.name.name
          val kind      = identifierFieldKind(f.tpe)
          val valueExpr = kind match {
            case IdentifierFieldKind.UnsignedSmallInt =>
              f.tpe match {
                case TypeRef.Scalar(b: TypeId.BuiltinScalar) => renderUnsignedSmallInt(fieldName, b)
                case _                                       => throw new IllegalStateException(s"unexpected non-scalar for unsigned small int: ${f.tpe}")
              }
            case _ => renderFieldValueExpr(fieldName, kind)
          }
          q"\"$fieldName:\" + ($valueExpr)"
      }

      val joinedFields = if (fieldExprs.isEmpty) q"\"\"" else fieldExprs.toSeq.join(" + \":\" + ")

      q"""override def toString: String = {
         |  \"$header\" + $joinedFields
         |}""".stripMargin
    }

    private def renderIdentifierCodecObject(dto: Typedef.Dto, name: ScValue.ScType): TextTree[ScValue] = {
      val simpleName      = name.name
      val versionStr      = domain.version.toString
      val codecObjectName = s"${name.name}Codec"

      // Per-field decoder. Operates on `cursor` and accumulates the decoded value
      // into a `val ${fieldName}_v: <ScalaType>`.
      val fieldDecoders: List[TextTree[ScValue]] = dto.fields.zipWithIndex.map {
        case (f, idx) =>
          val fieldName = f.name.name
          val isLast    = idx == dto.fields.length - 1
          val kind      = identifierFieldKind(f.tpe)
          val parseHead = q"""$baboonIdRepr.parseFieldName(cursor, \"$fieldName\") match {
                             |  case Left(e)  => return Left(e)
                             |  case Right(_) => ()
                             |}""".stripMargin

          val parseValue: TextTree[ScValue] = kind match {
            case IdentifierFieldKind.Bit =>
              q"""val ${fieldName}_raw = cursor.readUntilStructural()
                 |val ${fieldName}_v: $scBoolean = $baboonIdRepr.parseBit(${fieldName}_raw) match {
                 |  case Right(v) => v
                 |  case Left(e)  => return Left(e)
                 |}""".stripMargin
            case IdentifierFieldKind.SignedInt =>
              val tpeRef     = trans.asScRef(f.tpe, domain, evo)
              val rangeCheck = signedRangeCheck(f.tpe)
              val typeName   = signedTypeName(f.tpe)
              q"""val ${fieldName}_raw = cursor.readUntilStructural()
                 |val ${fieldName}_v: $tpeRef = {
                 |  // Spec §5.4: signed integers must not carry a leading '+'.
                 |  if (${fieldName}_raw.startsWith(\"+\")) {
                 |    return Left(s\"signed integer must not have leading '+' for field $fieldName: \" + ${fieldName}_raw)
                 |  }
                 |  scala.util.Try(${fieldName}_raw.toLong).toOption match {
                 |    case Some(v) =>
                 |      if (!($rangeCheck)) {
                 |        return Left(s\"$typeName out of range for field $fieldName: \" + ${fieldName}_raw)
                 |      } else {
                 |        v.${signedNarrow(f.tpe)}
                 |      }
                 |    case None    => return Left(s\"could not parse signed integer for field $fieldName: \" + ${fieldName}_raw)
                 |  }
                 |}""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              val tpeRef     = trans.asScRef(f.tpe, domain, evo)
              val rangeCheck = unsignedSmallRangeCheck(f.tpe)
              val typeName   = unsignedSmallTypeName(f.tpe)
              q"""val ${fieldName}_raw = cursor.readUntilStructural()
                 |val ${fieldName}_v: $tpeRef = scala.util.Try(java.lang.Long.parseUnsignedLong(${fieldName}_raw)).toOption match {
                 |  case Some(v) =>
                 |    if (!($rangeCheck)) {
                 |      return Left(s\"$typeName out of range for field $fieldName: \" + ${fieldName}_raw)
                 |    } else {
                 |      v.${unsignedSmallNarrow(f.tpe)}
                 |    }
                 |  case None    => return Left(s\"could not parse unsigned integer for field $fieldName: \" + ${fieldName}_raw)
                 |}""".stripMargin
            case IdentifierFieldKind.UnsignedLong =>
              q"""val ${fieldName}_raw = cursor.readUntilStructural()
                 |val ${fieldName}_v: $scLong = scala.util.Try(java.lang.Long.parseUnsignedLong(${fieldName}_raw)).toOption match {
                 |  case Some(v) => v
                 |  case None    => return Left(s\"could not parse u64 for field $fieldName: \" + ${fieldName}_raw)
                 |}""".stripMargin
            case IdentifierFieldKind.Str =>
              q"""val ${fieldName}_v: $scString = cursor.readStrField() match {
                 |  case Right(v) => v
                 |  case Left(e)  => return Left(e)
                 |}""".stripMargin
            case IdentifierFieldKind.Uid =>
              // Spec §5.4 mandates lowercase hex form. Validate before delegating to
              // UUID.fromString (which accepts mixed/uppercase) to keep the parser
              // consistent with bytes strict-lowercase enforcement.
              q"""val ${fieldName}_raw = cursor.readUntilStructural()
                 |val ${fieldName}_v: $scUid =
                 |  if (!${fieldName}_raw.matches(\"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\")) {
                 |    return Left(s\"uid not in canonical lowercase form for field $fieldName: \" + ${fieldName}_raw)
                 |  } else {
                 |    scala.util.Try($scUid.fromString(${fieldName}_raw)).toOption match {
                 |      case Some(v) => v
                 |      case None    => return Left(s\"could not parse uid for field $fieldName: \" + ${fieldName}_raw)
                 |    }
                 |  }""".stripMargin
            case IdentifierFieldKind.Tsu =>
              // tsu fixed-width lexeme: 24 chars `yyyy-MM-ddTHH:mm:ss.SSSZ` per spec §3 / §5.4.
              q"""val ${fieldName}_raw = cursor.readFixed(24) match {
                 |  case Right(v) => v
                 |  case Left(e)  => return Left(e)
                 |}
                 |val ${fieldName}_v: $scTime = $baboonIdRepr.parseTsuRepr(${fieldName}_raw) match {
                 |  case Right(v) => v
                 |  case Left(e)  => return Left(e)
                 |}""".stripMargin
            case IdentifierFieldKind.Tso =>
              // tso fixed-width lexeme: 29 chars `yyyy-MM-ddTHH:mm:ss.SSS±HH:MM` per spec §3 / §5.4.
              q"""val ${fieldName}_raw = cursor.readFixed(29) match {
                 |  case Right(v) => v
                 |  case Left(e)  => return Left(e)
                 |}
                 |val ${fieldName}_v: $scTime = $baboonIdRepr.parseTsoRepr(${fieldName}_raw) match {
                 |  case Right(v) => v
                 |  case Left(e)  => return Left(e)
                 |}""".stripMargin
            case IdentifierFieldKind.Bytes =>
              q"""val ${fieldName}_raw = cursor.readUntilStructural()
                 |val ${fieldName}_v: $scByteString = $baboonIdRepr.parseBytesHex(${fieldName}_raw) match {
                 |  case Right(v) => v
                 |  case Left(e)  => return Left(e)
                 |}""".stripMargin
            case IdentifierFieldKind.NestedId(uid) =>
              val nestedTpe   = trans.toScTypeRefKeepForeigns(uid, domain, evo)
              val nestedCodec = ScValue.ScType(nestedTpe.pkg, s"${nestedTpe.name}Codec", nestedTpe.inObject)
              q"""cursor.expect('{') match {
                 |  case Left(e)  => return Left(e)
                 |  case Right(_) => ()
                 |}
                 |val ${fieldName}_v: $nestedTpe = $nestedCodec.parseRepr(cursor) match {
                 |  case Right(v) => v
                 |  case Left(e)  => return Left(e)
                 |}
                 |cursor.expect('}') match {
                 |  case Left(e)  => return Left(e)
                 |  case Right(_) => ()
                 |}""".stripMargin
          }

          val sep =
            if (isLast) q""
            else
              q"""cursor.expect(':') match {
                 |  case Left(e)  => return Left(e)
                 |  case Right(_) => ()
                 |}""".stripMargin

          q"""$parseHead
             |$parseValue
             |$sep""".stripMargin.trim
      }

      val constructorArgs = dto.fields.map(f => q"${f.name.name} = ${f.name.name}_v").toSeq
      val ctor =
        if (constructorArgs.nonEmpty) q"new ${name.name}(${constructorArgs.join(", ")})"
        else q"new ${name.name}()"

      val body = (fieldDecoders :+ q"Right($ctor)").joinNN()

      q"""object $codecObjectName {
         |  /** Parse the canonical identifier repr per docs/spec/identifier-repr.md.
         |    * The parser is schema-directed: it walks the declared field order
         |    * and dispatches per field type. Returns Left(message) on any
         |    * malformed input.
         |    *
         |    * Public entry: takes a String. Internal entry (taking a Cursor) is
         |    * used by parent identifiers to recurse into nested-id values.
         |    */
         |  def parseRepr(s: $scString): $scEither[$scString, ${name.name}] = {
         |    val cursor = new $baboonIdReprCursor(s)
         |    parseRepr(cursor) match {
         |      case Left(e)  => Left(e)
         |      case Right(v) =>
         |        if (cursor.atEnd) Right(v)
         |        else Left(s\"unexpected trailing input at \" + cursor.position)
         |    }
         |  }
         |
         |  def parseRepr(cursor: $baboonIdReprCursor): $scEither[$scString, ${name.name}] = {
         |    $baboonIdRepr.parseHeader(cursor, \"$simpleName\", \"$versionStr\") match {
         |      case Left(e)  => return Left(e)
         |      case Right(_) => ()
         |    }
         |    ${body.shift(4).trim}
         |  }
         |}""".stripMargin
    }

    /** For signed-int parsing we read via toLong and narrow to the field type. */
    private def signedNarrow(tpe: TypeRef): TextTree[ScValue] = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.i08) => q"toByte"
        case TypeRef.Scalar(TypeId.Builtins.i16) => q"toShort"
        case TypeRef.Scalar(TypeId.Builtins.i32) => q"toInt"
        case TypeRef.Scalar(TypeId.Builtins.i64) => q"toLong"
        case other                               => throw new IllegalStateException(s"signedNarrow on non-signed-int: $other")
      }
    }

    /** Range predicate as a Scala expression operating on `v: Long`. Used to fail
      * fast on out-of-range input before narrowing (which would otherwise truncate
      * silently). i64 is unconstrained since it is the parsed type itself.
      */
    private def signedRangeCheck(tpe: TypeRef): TextTree[ScValue] = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.i08) => q"v >= -128L && v <= 127L"
        case TypeRef.Scalar(TypeId.Builtins.i16) => q"v >= -32768L && v <= 32767L"
        case TypeRef.Scalar(TypeId.Builtins.i32) => q"v >= -2147483648L && v <= 2147483647L"
        case TypeRef.Scalar(TypeId.Builtins.i64) => q"true"
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

    private def unsignedSmallNarrow(tpe: TypeRef): TextTree[ScValue] = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => q"toByte"
        case TypeRef.Scalar(TypeId.Builtins.u16) => q"toShort"
        case TypeRef.Scalar(TypeId.Builtins.u32) => q"toInt"
        case other                               => throw new IllegalStateException(s"unsignedSmallNarrow on non-u08/u16/u32: $other")
      }
    }

    /** Range predicate operating on `v: Long` produced by `parseUnsignedLong`. For
      * valid u08/u16/u32 inputs the value lands in 0..2^N-1 as a positive Long.
      * Out-of-range inputs (including 2^64-1 which parseUnsignedLong returns as -1)
      * are caught by the comparison.
      */
    private def unsignedSmallRangeCheck(tpe: TypeRef): TextTree[ScValue] = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => q"v >= 0L && v <= 255L"
        case TypeRef.Scalar(TypeId.Builtins.u16) => q"v >= 0L && v <= 65535L"
        case TypeRef.Scalar(TypeId.Builtins.u32) => q"v >= 0L && v <= 4294967295L"
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

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = scFiles.basename(domain, evo)
      val fname = s"${defn.id.name.name}${suffix.getOrElse("")}.scala"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("_")}.$fname"
        case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}.$fname"
      }
    }
  }
}
