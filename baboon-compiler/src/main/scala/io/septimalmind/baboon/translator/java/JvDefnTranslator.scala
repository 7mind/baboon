package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.java.JvValue.JvType
import io.septimalmind.baboon.typer.EnumWireStyle
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait JvDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[JvDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[JvDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[JvDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[JvDefnTranslator.Output]]
}

object JvDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: JvType,
    tpeKeepForeigns: JvType,
    tpeId: TextTree[JvValue],
    trees: Map[String, TextTree[JvValue]],
  )

  final case class CodecDef(
    className: String,
    owner: Owner,
    tree: TextTree[JvValue],
  )

  final case class DefnRepr(
    defn: TextTree[JvValue],
    codecDefs: List[CodecDef],
    codecs: List[CodecReg],
  )

  final case class Output(
    path: String,
    tree: TextTree[JvValue],
    pkg: JvValue.JvPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[JvValue]])] = Nil,
  )

  class JvDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: JvTarget,
    domain: Domain,
    evo: BaboonEvolution,
    jvFiles: JvFileTools,
    jvTrees: JvTreeTools,
    trans: JvTypeTranslator,
    codecs: Set[JvCodecTranslator],
    codecTests: JvCodecTestsTranslator,
    codecsFixture: JvCodecFixtureTranslator,
    wiringTranslator: JvServiceWiringTranslator,
    jvDomainTreeTools: JvDomainTreeTools,
  ) extends JvDefnTranslator[F] {
    import JvTypes.*

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr = makeFullRepr(defn, inPkg = true)

      val registrations = codecs.toList.map(codec => codec.id -> repr.codecs.flatMap(reg => reg.trees.get(codec.id).map(expr => q"${reg.tpeId}, $expr")))

      val pkg = effectivePkg(defn.defn.id.owner)

      val mainOutput = Output(
        getOutputPath(defn),
        repr.defn,
        pkg,
        CompilerProduct.Definition,
        codecReg = registrations,
      )

      val codecOutputs = repr.codecDefs.map {
        codecDef =>
          val fbase    = jvFiles.basename(domain, evo)
          val codecPkg = effectivePkg(codecDef.owner)
          val subdir   = ownerSubdir(codecDef.owner)
          val fname    = s"${codecDef.className}.java"
          val wrapped  = jvTrees.inPkg(codecPkg.parts.toSeq, codecDef.tree)
          Output(
            s"$fbase$subdir/$fname",
            wrapped,
            codecPkg,
            CompilerProduct.Definition,
          )
      }

      val wiringOutput = wiringTranslator
        .translate(defn).map {
          wiringTree =>
            val wrapped = jvTrees.inPkg(pkg.parts.toSeq, wiringTree)
            Output(
              getOutputPath(defn, suffix = Some("Wiring")),
              wrapped,
              pkg,
              CompilerProduct.Definition,
            )
        }.toList

      F.pure(mainOutput :: (codecOutputs ++ wiringOutput))
    }

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateFixtures(defn)
      }
    }

    private def doTranslateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val fixtureTreeOut = makeFixtureRepr(defn).map {
        fixtureTreeWithPkg =>
          Output(
            getOutputPath(defn, suffix = Some("_Fixture")),
            fixtureTreeWithPkg,
            effectivePkg(defn.defn.id.owner),
            CompilerProduct.Fixture,
          )
      }

      F.pure(fixtureTreeOut.toList)
    }

    private def makeFixtureRepr(defn: DomainMember.User): Option[TextTree[JvValue]] = {
      val srcRef = trans.toJvTypeRefKeepForeigns(defn.id, domain, evo)
      val ns     = srcRef.pkg.parts

      val fixtureTree        = codecsFixture.translate(defn)
      val fixtureTreeWithPkg = fixtureTree.map(t => jvTrees.inPkg(ns.toSeq, t))

      fixtureTreeWithPkg
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTest(defn)
      }
    }

    private def doTranslateTest(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val codecTestOut = makeTestRepr(defn).map {
        codecTestWithPkg =>
          Output(
            getOutputPath(defn, suffix = Some("_tests")),
            codecTestWithPkg,
            effectivePkg(defn.defn.id.owner),
            CompilerProduct.Test,
          )
      }

      F.pure(codecTestOut.toList)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map {
        tree =>
          val pkg     = trans.toJvPkg(domain.id, domain.version, evo)
          val wrapped = jvTrees.inPkg(pkg.parts.toSeq, tree)
          val fbase   = jvFiles.basename(domain, evo)
          Output(
            s"$fbase/BaboonServiceRt.java",
            wrapped,
            pkg,
            CompilerProduct.Definition,
          )
      }.toList
      F.pure(result)
    }

    private def makeTestRepr(defn: DomainMember.User): Option[TextTree[JvValue]] = {
      val jvTypeRef = trans.asJvType(defn.id, domain, evo)
      val srcRef    = trans.toJvTypeRefKeepForeigns(defn.id, domain, evo)
      val ns        = srcRef.pkg.parts

      val testTree        = codecTests.translate(defn, jvTypeRef, srcRef)
      val testTreeWithPkg = testTree.map(t => jvTrees.inPkg(ns.toSeq, t))

      testTreeWithPkg
    }

    private def makeFullRepr(
      defn: DomainMember.User,
      inPkg: Boolean,
    ): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      def deprecatePrevious(tree: TextTree[JvValue]): TextTree[JvValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""@Deprecated
             |$tree""".stripMargin
        }
      }

      val jvTypeRef = trans.asJvType(defn.id, domain, evo)
      val srcRef    = trans.toJvTypeRefKeepForeigns(defn.id, domain, evo)

      val repr = makeRepr(defn, jvTypeRef, isLatestVersion)

      val ownCodecDefs = codecs.toList.flatMap {
        c =>
          c.translate(defn, jvTypeRef, srcRef).map {
            tree =>
              CodecDef(c.codecName(srcRef, defn.defn.id.owner).name, defn.defn.id.owner, deprecatePrevious(tree))
          }
      }

      val defnRepr = deprecatePrevious(repr.defn)

      assert(defn.id.pkg == domain.id)

      val ns = srcRef.pkg.parts

      val content = if (inPkg) jvTrees.inPkg(ns.toSeq, defnRepr) else defnRepr

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => Nil
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap(
              codec =>
                if (codec.isActive(d.id)) List(codec.id -> q"$baboonLazy.of(() -> ${codec.codecName(srcRef, defn.defn.id.owner)}.INSTANCE)")
                else Nil
            )
          List(CodecReg(defn.id, jvTypeRef, srcRef, q"\"${defn.id.toString}\"", codecsReg.toMap))
      }

      val allRegs      = reg ++ repr.codecs
      val allCodecDefs = ownCodecDefs ++ repr.codecDefs

      DefnRepr(content, allCodecDefs, allRegs)
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: JvValue.JvType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = jvDomainTreeTools.makeDataMeta(defn)
      val codecMeta = codecs.flatMap(_.codecMeta(defn, name).map(_.member))

      defn.defn match {
        case dto: Typedef.Dto =>
          renderDto(dto, name, genMarker, mainMeta, codecMeta)

        case e: Typedef.Enum =>
          renderEnum(e, name, mainMeta, codecMeta)

        case adt: Typedef.Adt =>
          renderAdt(defn, adt, name, genMarker, mainMeta, codecMeta)

        case contract: Typedef.Contract =>
          renderContract(contract, name, genMarker)

        case service: Typedef.Service =>
          renderService(service, name)

        case _: Typedef.Foreign => DefnRepr(q"", Nil, Nil)
      }
    }

    private def renderDto(
      dto: Typedef.Dto,
      name: JvValue.JvType,
      genMarker: JvType,
      mainMeta: List[JvDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[JvValue]],
    ): DefnRepr = {
      val params = dto.fields.map {
        f =>
          val t = trans.asJvRef(f.tpe, domain, evo)
          q"$t ${f.name.name}"
      }
      val paramsList = if (params.nonEmpty) params.join(",\n") else q""

      val contractParents = dto.contracts.map(c => trans.toJvTypeRefKeepForeigns(c, domain, evo))
      val (adtMarker, adtParent) = dto.id.owner match {
        case Owner.Adt(adtId) =>
          val adtType = trans.toJvTypeRefKeepForeigns(adtId, domain, evo)
          (Seq(iBaboonAdtMemberMeta), Seq(adtType))
        case _ => (Seq.empty, Seq.empty)
      }

      val interfaceParents = (adtParent ++ adtMarker ++ contractParents :+ genMarker).distinct
      val implementsList   = interfaceParents.map(t => q"$t").join(", ")

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta
      val hasFields        = params.nonEmpty

      val paramsBlock = if (hasFields) {
        q"""(
           |  ${paramsList.shift(2).trim}
           |)""".stripMargin
      } else q"()"

      // Identifier toString override (PR-57a / spec: docs/spec/identifier-repr.md).
      // Emitted only when `dto.isIdentifier == true`. For records with fields we
      // override Java's default record toString. For empty-field records we keep
      // the manual hashCode/equals overrides but replace the toString with the
      // identifier-repr form.
      val identifierToStringOverride: TextTree[JvValue] =
        if (dto.isIdentifier) renderIdentifierToString(dto, name) else q""

      val emptyRecordMethods = if (!hasFields) {
        // For empty-field id records, emit the spec form `<Name>:<version>#`
        // instead of the original `<Name>()` form.
        val toStringMethod =
          if (dto.isIdentifier) identifierToStringOverride
          else
            q"""@Override
               |public String toString() {
               |  return "${name.asName}()";
               |}""".stripMargin

        q"""
           |@Override
           |public boolean equals(Object other) {
           |  return other instanceof ${name.asName};
           |}
           |
           |@Override
           |public int hashCode() {
           |  return ${name.asName.hashCode.toString};
           |}
           |
           |${toStringMethod}
           |""".stripMargin
      } else q""

      // For non-empty id records, the toString override is appended to the body.
      val nonEmptyExtra: TextTree[JvValue] =
        if (dto.isIdentifier && hasFields) identifierToStringOverride else q""

      val defnTree =
        q"""public record ${name.asName}$paramsBlock implements $implementsList {
           |  ${staticMetaFields.joinN().shift(2).trim}
           |  ${emptyRecordMethods.shift(2).trim}
           |  ${nonEmptyExtra.shift(2).trim}
           |}""".stripMargin

      // Identifier <TypeName>Codec class (PR-57a Q-FU-4): static parseRepr lives on a
      // sibling class, NOT on the record itself, so it does not appear in IDE
      // autocomplete on the record type as the obvious "parse my id" entry point.
      val identifierCodecDef: List[CodecDef] =
        if (dto.isIdentifier) {
          val codecClassName = s"${name.name}Codec"
          val tree           = renderIdentifierCodecClass(dto, name, codecClassName)
          List(CodecDef(codecClassName, dto.id.owner, tree))
        } else Nil

      DefnRepr(defnTree, identifierCodecDef, Nil)
    }

    private def renderEnum(
      e: Typedef.Enum,
      name: JvValue.JvType,
      mainMeta: List[JvDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[JvValue]],
    ): DefnRepr = {
      val cases = e.members.map {
        m =>
          val obj = EnumWireStyle.wireName(m.name)
          q"$obj"
      }.toList

      val parseCases = e.members.map {
        m =>
          val obj = EnumWireStyle.wireName(m.name)
          q"case \"$obj\" -> $obj;"
      }.toList

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      DefnRepr(
        q"""public enum ${name.asName} implements $iBaboonGenerated {
           |  ${cases.join(",\n").shift(2).trim};
           |
           |  ${staticMetaFields.joinN().shift(2).trim}
           |
           |  public static ${name.asName} parse(String s) {
           |    return switch (s) {
           |      ${parseCases.joinN().shift(6).trim}
           |      default -> throw new IllegalArgumentException("Unknown enum value: " + s);
           |    };
           |  }
           |
           |  public static $jvList<${name.asName}> all() {
           |    return $jvList.of(${cases.join(", ")});
           |  }
           |}""".stripMargin,
        Nil,
        Nil,
      )
    }

    private def renderAdt(
      defn: DomainMember.User,
      adt: Typedef.Adt,
      name: JvValue.JvType,
      genMarker: JvType,
      mainMeta: List[JvDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[JvValue]],
    ): DefnRepr = {
      val contractParents = adt.contracts.map(c => trans.toJvTypeRefKeepForeigns(c, domain, evo))
      val parents         = (contractParents :+ genMarker).distinct
      val parentsList     = parents.map(t => q"$t").join(", ")

      val memberTrees = adt.members.map {
        mid =>
          domain.defs.meta.nodes(mid) match {
            case mdefn: DomainMember.User => makeFullRepr(mdefn, inPkg = false)
            case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
          }
      }

      val memberNames = adt.members.map {
        mid =>
          val memberName = mid.name.name.capitalize
          q"${name.asName}.$memberName"
      }.toList

      val permitsClause = q" permits ${memberNames.join(", ")}"

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      DefnRepr(
        q"""public sealed interface ${name.asName} extends $parentsList$permitsClause {
           |  ${staticMetaFields.joinN().shift(2).trim}
           |
           |  ${memberTrees.map(_.defn).toList.joinNN().shift(2).trim}
           |}""".stripMargin,
        memberTrees.toList.flatMap(_.codecDefs),
        memberTrees.toList.flatMap(_.codecs),
      )
    }

    private def renderContract(
      contract: Typedef.Contract,
      name: JvValue.JvType,
      genMarker: JvType,
    ): DefnRepr = {
      val methods = contract.fields.map {
        f =>
          val t = trans.asJvRef(f.tpe, domain, evo)
          q"$t ${f.name.name}();"
      }
      val contractParents = contract.contracts.map(c => trans.toJvTypeRefKeepForeigns(c, domain, evo))
      val adtParent = contract.id.owner match {
        case Owner.Adt(adtId) => Seq(trans.toJvTypeRefKeepForeigns(adtId, domain, evo))
        case _                => Seq.empty
      }
      val parents     = (adtParent ++ contractParents :+ genMarker).distinct
      val parentsList = parents.map(t => q"$t").join(", ")
      val body        = if (methods.nonEmpty) methods.joinN() else q""
      val sealedModifier = contract.id.owner match {
        case Owner.Adt(_) => "non-sealed"
        case _            => ""
      }
      DefnRepr(
        q"""public $sealedModifier interface ${name.asName} extends $parentsList {
           |  ${body.shift(2).trim}
           |}""".stripMargin,
        Nil,
        Nil,
      )
    }

    private def renderService(
      service: Typedef.Service,
      name: JvValue.JvType,
    ): DefnRepr = {
      val resolved    = ServiceResultResolver.resolve(domain, "java", target.language.serviceResult, target.language.pragmas)
      val resolvedCtx = ServiceContextResolver.resolve(domain, "java", target.language.serviceContext, target.language.pragmas)
      val ctxParam = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => ""
        case ResolvedServiceContext.AbstractContext(tn, pn) => s"$tn $pn, "
        case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$tn $pn, "
      }
      val methods = service.methods.map {
        m =>
          val in  = trans.asJvRef(m.sig, domain, evo)
          val out = m.out.map(trans.asJvRef(_, domain, evo))
          val err = m.err.map(trans.asJvRef(_, domain, evo))
          val jvFqName: JvValue => String = {
            case t: JvValue.JvType     => if (t.predef) t.name else (t.pkg.parts :+ t.name).mkString(".")
            case t: JvValue.JvTypeName => t.name
          }
          val outStr = out.map(_.mapRender(jvFqName)).getOrElse("void")
          val errStr = err.map(_.mapRender(jvFqName))
          val retStr = resolved.renderReturnType(outStr, errStr, "void")
          q"$retStr ${m.name.name}($ctxParam$in arg);"
      }
      val typeParams = Seq(
        resolved.traitTypeParam,
        resolvedCtx match {
          case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
          case _                                             => None
        },
      ).flatten
      val traitTypeParam = if (typeParams.nonEmpty) typeParams.mkString("<", ", ", ">") else ""
      val body           = if (methods.nonEmpty) methods.joinN() else q""
      DefnRepr(
        q"""public interface ${name.asName}$traitTypeParam {
           |  ${body.shift(2).trim}
           |}""".stripMargin,
        Nil,
        Nil,
      )
    }

    private def collectContractFieldNames(contracts: List[TypeId.User]): Set[String] = {
      contracts.flatMap {
        contractId =>
          domain.defs.meta.nodes.get(contractId) match {
            case Some(DomainMember.User(_, ct: Typedef.Contract, _, _)) =>
              ct.fields.map(_.name.name) ++ collectContractFieldNames(ct.contracts)
            case _ => Seq.empty
          }
      }.toSet
    }

    private def ownerSubdir(owner: Owner): String = owner match {
      case Owner.Toplevel => ""
      case Owner.Ns(path) => "/" + path.map(_.name.toLowerCase).mkString("/")
      case Owner.Adt(id)  => ownerSubdir(id.owner)
    }

    private def effectivePkg(owner: Owner): JvValue.JvPackageId = {
      trans.effectiveJvPkg(owner, domain, evo)
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase    = jvFiles.basename(domain, evo)
      val subdir   = ownerSubdir(defn.defn.id.owner)
      val javaName = defn.id.name.name.capitalize
      val fname    = s"$javaName${suffix.getOrElse("")}.java"
      s"$fbase$subdir/$fname"
    }

    // ----- Identifier toString + parseRepr emission (PR-57a) -----
    // Spec contract: docs/spec/identifier-repr.md. Mirrors ScDefnTranslator
    // section but uses Java stdlib + BaboonIdentifierRepr runtime helper.

    private sealed trait IdentifierFieldKind
    private object IdentifierFieldKind {
      case object Bit              extends IdentifierFieldKind
      case object SignedInt        extends IdentifierFieldKind /* i08/i16/i32/i64 */
      case object UnsignedSmallInt extends IdentifierFieldKind /* u08/u16/u32 */
      case object UnsignedLong     extends IdentifierFieldKind /* u64 */
      case object Str              extends IdentifierFieldKind
      case object Uid              extends IdentifierFieldKind
      case object Tsu              extends IdentifierFieldKind
      case object Tso              extends IdentifierFieldKind
      case object Bytes            extends IdentifierFieldKind
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

    private def renderFieldValueExpr(jvFieldName: String, kind: IdentifierFieldKind, f: Field): TextTree[JvValue] = {
      kind match {
        case IdentifierFieldKind.Bit          => q"$baboonIdRepr.bitToString(this.$jvFieldName())"
        case IdentifierFieldKind.SignedInt    =>
          // Java's primitive toString is locale-independent for integers.
          q"$jvBoxedLong.toString(this.$jvFieldName())"
        case IdentifierFieldKind.UnsignedSmallInt =>
          // u08/u16/u32 are stored in next-wider signed Java types (short/int/long)
          // and constrained to non-negative range by validator + JSON decoder.
          // Signed `toString` therefore produces the correct unsigned decimal.
          f.tpe match {
            case TypeRef.Scalar(TypeId.Builtins.u08) => q"$jvBoxedShort.toString(this.$jvFieldName())"
            case TypeRef.Scalar(TypeId.Builtins.u16) => q"$jvBoxedInteger.toString(this.$jvFieldName())"
            case TypeRef.Scalar(TypeId.Builtins.u32) => q"$jvBoxedLong.toString(this.$jvFieldName())"
            case other => throw new IllegalStateException(s"unexpected u-small kind: $other")
          }
        case IdentifierFieldKind.UnsignedLong => q"$baboonIdRepr.u64ToString(this.$jvFieldName())"
        case IdentifierFieldKind.Str          => q"$baboonIdRepr.escapeStr(this.$jvFieldName())"
        case IdentifierFieldKind.Uid          =>
          // UUID.toString() canonical form is 32 lowercase hex digits with hyphens (RFC 4122).
          q"this.$jvFieldName().toString()"
        case IdentifierFieldKind.Tsu          => q"$baboonIdRepr.tsuToString(this.$jvFieldName())"
        case IdentifierFieldKind.Tso          => q"$baboonIdRepr.tsoToString(this.$jvFieldName())"
        case IdentifierFieldKind.Bytes        => q"$baboonIdRepr.bytesToHex(this.$jvFieldName())"
        case IdentifierFieldKind.NestedId(_)  => q""""{" + this.$jvFieldName().toString() + "}""""
      }
    }

    private def renderIdentifierToString(dto: Typedef.Dto, name: JvType): TextTree[JvValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString
      val header     = s"$simpleName:$versionStr#"

      val fieldExprs: List[TextTree[JvValue]] = dto.fields.map {
        f =>
          val srcFieldName = f.name.name
          val kind         = identifierFieldKind(f.tpe)
          val valueExpr    = renderFieldValueExpr(srcFieldName, kind, f)
          // The repr field name is the source name per spec §2.1.
          q""""$srcFieldName:" + ($valueExpr)"""
      }

      val joinedFields = if (fieldExprs.isEmpty) q""""""""
      else fieldExprs.toSeq.join(""" + ":" + """)

      q"""@Override
         |public String toString() {
         |  return "$header" + ${joinedFields};
         |}""".stripMargin
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

    private def signedNarrow(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.i08) => "(byte)"
        case TypeRef.Scalar(TypeId.Builtins.i16) => "(short)"
        case TypeRef.Scalar(TypeId.Builtins.i32) => "(int)"
        case TypeRef.Scalar(TypeId.Builtins.i64) => ""
        case other                               => throw new IllegalStateException(s"signedNarrow on non-signed-int: $other")
      }
    }

    private def unsignedSmallNarrow(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => "(short)"
        case TypeRef.Scalar(TypeId.Builtins.u16) => "(int)"
        case TypeRef.Scalar(TypeId.Builtins.u32) => ""
        case other                               => throw new IllegalStateException(s"unsignedSmallNarrow on non-u08/u16/u32: $other")
      }
    }

    private def unsignedSmallRangeCheck(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => "v >= 0L && v <= 255L"
        case TypeRef.Scalar(TypeId.Builtins.u16) => "v >= 0L && v <= 65535L"
        case TypeRef.Scalar(TypeId.Builtins.u32) => "v >= 0L && v <= 4294967295L"
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

    private def renderIdentifierCodecClass(dto: Typedef.Dto, name: JvType, codecClassName: String): TextTree[JvValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString

      val fieldDecoders: List[TextTree[JvValue]] = dto.fields.zipWithIndex.map {
        case (f, idx) =>
          val srcFieldName = f.name.name
          val rawVar       = s"${srcFieldName}_raw"
          val valVar       = s"${srcFieldName}_v"
          val isLast       = idx == dto.fields.length - 1
          val kind         = identifierFieldKind(f.tpe)
          val tpe          = trans.asJvRef(f.tpe, domain, evo)

          val parseHead =
            q"""{
               |  $baboonEither<String, Void> __r = $baboonIdRepr.parseFieldName(cursor, "$srcFieldName");
               |  if (__r instanceof $baboonEither.Left<String, Void> __l) return $baboonEither.left(__l.value());
               |}""".stripMargin

          val parseValue: TextTree[JvValue] = kind match {
            case IdentifierFieldKind.Bit =>
              q"""String $rawVar = cursor.readUntilStructural();
                 |boolean $valVar;
                 |{
                 |  $baboonEither<String, Boolean> __r = $baboonIdRepr.parseBit($rawVar);
                 |  if (__r instanceof $baboonEither.Left<String, Boolean> __l) return $baboonEither.left(__l.value());
                 |  $valVar = (($baboonEither.Right<String, Boolean>) __r).value();
                 |}""".stripMargin
            case IdentifierFieldKind.SignedInt =>
              val rangeCheck = signedRangeCheck(f.tpe)
              val typeName   = signedTypeName(f.tpe)
              val cast       = signedNarrow(f.tpe)
              q"""String $rawVar = cursor.readUntilStructural();
                 |$tpe $valVar;
                 |try {
                 |  long v = Long.parseLong($rawVar);
                 |  if (!($rangeCheck)) {
                 |    return $baboonEither.left("$typeName out of range for field $srcFieldName: " + $rawVar);
                 |  }
                 |  $valVar = $cast v;
                 |} catch (NumberFormatException __nfe) {
                 |  return $baboonEither.left("could not parse signed integer for field $srcFieldName: " + $rawVar);
                 |}""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              val rangeCheck = unsignedSmallRangeCheck(f.tpe)
              val typeName   = unsignedSmallTypeName(f.tpe)
              val cast       = unsignedSmallNarrow(f.tpe)
              // Reject leading sign (spec §5.4 D06): canonical toString never emits
              // `+` or `-` for unsigned. Long.parseUnsignedLong silently accepts `+`
              // and rejects `-`; we tighten both for cross-backend consistency.
              q"""String $rawVar = cursor.readUntilStructural();
                 |$tpe $valVar;
                 |if (!$rawVar.isEmpty() && ($rawVar.charAt(0) == '+' || $rawVar.charAt(0) == '-')) {
                 |  return $baboonEither.left("unsigned value has leading sign for field $srcFieldName: " + $rawVar);
                 |}
                 |try {
                 |  long v = Long.parseUnsignedLong($rawVar);
                 |  if (!($rangeCheck)) {
                 |    return $baboonEither.left("$typeName out of range for field $srcFieldName: " + $rawVar);
                 |  }
                 |  $valVar = $cast v;
                 |} catch (NumberFormatException __nfe) {
                 |  return $baboonEither.left("could not parse unsigned integer for field $srcFieldName: " + $rawVar);
                 |}""".stripMargin
            case IdentifierFieldKind.UnsignedLong =>
              // Reject leading sign (spec §5.4 D06) — same rationale as UnsignedSmallInt.
              q"""String $rawVar = cursor.readUntilStructural();
                 |long $valVar;
                 |if (!$rawVar.isEmpty() && ($rawVar.charAt(0) == '+' || $rawVar.charAt(0) == '-')) {
                 |  return $baboonEither.left("unsigned value has leading sign for field $srcFieldName: " + $rawVar);
                 |}
                 |try {
                 |  $valVar = Long.parseUnsignedLong($rawVar);
                 |} catch (NumberFormatException __nfe) {
                 |  return $baboonEither.left("could not parse u64 for field $srcFieldName: " + $rawVar);
                 |}""".stripMargin
            case IdentifierFieldKind.Str =>
              q"""String $valVar;
                 |{
                 |  $baboonEither<String, String> __r = cursor.readStrField();
                 |  if (__r instanceof $baboonEither.Left<String, String> __l) return $baboonEither.left(__l.value());
                 |  $valVar = (($baboonEither.Right<String, String>) __r).value();
                 |}""".stripMargin
            case IdentifierFieldKind.Uid =>
              // Spec §5.4 mandates lowercase hex form. Validate before delegating to
              // UUID.fromString (which accepts mixed/uppercase).
              q"""String $rawVar = cursor.readUntilStructural();
                 |$jvUid $valVar;
                 |if (!$rawVar.matches("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}")) {
                 |  return $baboonEither.left("uid not in canonical lowercase form for field $srcFieldName: " + $rawVar);
                 |}
                 |try {
                 |  $valVar = $jvUid.fromString($rawVar);
                 |} catch (IllegalArgumentException __iae) {
                 |  return $baboonEither.left("could not parse uid for field $srcFieldName: " + $rawVar);
                 |}""".stripMargin
            case IdentifierFieldKind.Tsu =>
              // tsu fixed-width lexeme: 24 chars `yyyy-MM-ddTHH:mm:ss.SSSZ` per spec §3 / §5.4.
              q"""String $rawVar;
                 |{
                 |  $baboonEither<String, String> __rf = cursor.readFixed(24);
                 |  if (__rf instanceof $baboonEither.Left<String, String> __l) return $baboonEither.left(__l.value());
                 |  $rawVar = (($baboonEither.Right<String, String>) __rf).value();
                 |}
                 |$jvOffsetDateTime $valVar;
                 |{
                 |  $baboonEither<String, $jvOffsetDateTime> __r = $baboonIdRepr.parseTsuRepr($rawVar);
                 |  if (__r instanceof $baboonEither.Left<String, $jvOffsetDateTime> __l) return $baboonEither.left(__l.value());
                 |  $valVar = (($baboonEither.Right<String, $jvOffsetDateTime>) __r).value();
                 |}""".stripMargin
            case IdentifierFieldKind.Tso =>
              // tso fixed-width lexeme: 29 chars `yyyy-MM-ddTHH:mm:ss.SSS±HH:MM` per spec §3 / §5.4.
              q"""String $rawVar;
                 |{
                 |  $baboonEither<String, String> __rf = cursor.readFixed(29);
                 |  if (__rf instanceof $baboonEither.Left<String, String> __l) return $baboonEither.left(__l.value());
                 |  $rawVar = (($baboonEither.Right<String, String>) __rf).value();
                 |}
                 |$jvOffsetDateTime $valVar;
                 |{
                 |  $baboonEither<String, $jvOffsetDateTime> __r = $baboonIdRepr.parseTsoRepr($rawVar);
                 |  if (__r instanceof $baboonEither.Left<String, $jvOffsetDateTime> __l) return $baboonEither.left(__l.value());
                 |  $valVar = (($baboonEither.Right<String, $jvOffsetDateTime>) __r).value();
                 |}""".stripMargin
            case IdentifierFieldKind.Bytes =>
              q"""String $rawVar = cursor.readUntilStructural();
                 |$jvByteString $valVar;
                 |{
                 |  $baboonEither<String, $jvByteString> __r = $baboonIdRepr.parseBytesHex($rawVar);
                 |  if (__r instanceof $baboonEither.Left<String, $jvByteString> __l) return $baboonEither.left(__l.value());
                 |  $valVar = (($baboonEither.Right<String, $jvByteString>) __r).value();
                 |}""".stripMargin
            case IdentifierFieldKind.NestedId(uid) =>
              val nestedTpe   = trans.toJvTypeRefKeepForeigns(uid, domain, evo)
              val nestedCodec = JvType(nestedTpe.pkg, s"${nestedTpe.name}Codec")
              q"""{
                 |  $baboonEither<String, Void> __r = cursor.expect('{');
                 |  if (__r instanceof $baboonEither.Left<String, Void> __l) return $baboonEither.left(__l.value());
                 |}
                 |$nestedTpe $valVar;
                 |{
                 |  $baboonEither<String, $nestedTpe> __r = $nestedCodec.parseRepr(cursor);
                 |  if (__r instanceof $baboonEither.Left<String, $nestedTpe> __l) return $baboonEither.left(__l.value());
                 |  $valVar = (($baboonEither.Right<String, $nestedTpe>) __r).value();
                 |}
                 |{
                 |  $baboonEither<String, Void> __r = cursor.expect('}');
                 |  if (__r instanceof $baboonEither.Left<String, Void> __l) return $baboonEither.left(__l.value());
                 |}""".stripMargin
          }

          val sep =
            if (isLast) q""
            else
              q"""{
                 |  $baboonEither<String, Void> __r = cursor.expect(':');
                 |  if (__r instanceof $baboonEither.Left<String, Void> __l) return $baboonEither.left(__l.value());
                 |}""".stripMargin

          q"""$parseHead
             |$parseValue
             |$sep""".stripMargin.trim
      }

      val constructorArgs = dto.fields.map(f => q"${f.name.name}_v").toSeq
      val ctor =
        if (constructorArgs.nonEmpty) q"new $name(${constructorArgs.join(", ")})"
        else q"new $name()"

      val body = (fieldDecoders :+ q"return $baboonEither.right($ctor);").joinNN()

      // Note: each emitted block re-declares `__r` in its own block scope so there's no
      // shadow; Java's record-pattern variable `__l` is scoped to the `if` statement.
      // We emit the enclosing class as a top-level public class (separate file via codecDefs).
      val classBody =
        q"""public final class $codecClassName {
           |  private $codecClassName() {}
           |
           |  /** Parse the canonical identifier repr per docs/spec/identifier-repr.md.
           |   *  Schema-directed parser: walks declared field order and dispatches per
           |   *  field type. Returns Left on any malformed input. */
           |  public static $baboonEither<String, $name> parseRepr(String s) {
           |    $baboonIdReprCursor cursor = new $baboonIdReprCursor(s);
           |    $baboonEither<String, $name> inner = parseRepr(cursor);
           |    if (inner instanceof $baboonEither.Left<String, $name> __l) return __l;
           |    if (!cursor.atEnd()) return $baboonEither.left("unexpected trailing input at " + cursor.position());
           |    return inner;
           |  }
           |
           |  public static $baboonEither<String, $name> parseRepr($baboonIdReprCursor cursor) {
           |    {
           |      $baboonEither<String, Void> __r = $baboonIdRepr.parseHeader(cursor, "$simpleName", "$versionStr");
           |      if (__r instanceof $baboonEither.Left<String, Void> __l) return $baboonEither.left(__l.value());
           |    }
           |    ${body.shift(4).trim}
           |  }
           |}""".stripMargin
      classBody
    }
  }
}
