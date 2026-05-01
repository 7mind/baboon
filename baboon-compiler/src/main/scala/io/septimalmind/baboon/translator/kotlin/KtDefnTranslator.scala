package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.KtTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.kotlin.KtValue.KtType
import io.septimalmind.baboon.typer.EnumWireStyle
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait KtDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[KtDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[KtDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[KtDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[KtDefnTranslator.Output]]
}

object KtDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: KtType,
    tpeKeepForeigns: KtType,
    tpeId: TextTree[KtValue],
    trees: Map[String, TextTree[KtValue]],
  )

  final case class DefnRepr(
    defn: TextTree[KtValue],
    codecs: List[CodecReg],
  )

  final case class Output(
    path: String,
    tree: TextTree[KtValue],
    pkg: KtValue.KtPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[KtValue]])] = Nil,
  )

  class KtDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: KtTarget,
    domain: Domain,
    evo: BaboonEvolution,
    ktFiles: KtFileTools,
    ktTrees: KtTreeTools,
    trans: KtTypeTranslator,
    ktTypes: KtTypes,
    codecs: Set[KtCodecTranslator],
    codecTests: KtCodecTestsTranslator,
    codecsFixture: KtCodecFixtureTranslator,
    wiringTranslator: KtServiceWiringTranslator,
    ktDomainTreeTools: KtDomainTreeTools,
  ) extends KtDefnTranslator[F] {
    import KtTypes.*

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr = makeFullRepr(defn, inPkg = true)

      val registrations = codecs.toList.map(codec => codec.id -> repr.codecs.flatMap(reg => reg.trees.get(codec.id).map(expr => q"${reg.tpeId}, $expr")))

      val srcRef = trans.toKtTypeRefKeepForeigns(defn.id, domain, evo)

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
            val wrapped = ktTrees.inPkg(pkg.parts.toSeq, wiringTree)
            Output(
              getOutputPath(defn, suffix = Some("Wiring")),
              wrapped,
              pkg,
              CompilerProduct.Definition,
            )
        }.toList

      val clientOutput = wiringTranslator
        .translateClient(defn).map {
          clientTree =>
            val pkg     = srcRef.pkg
            val wrapped = ktTrees.inPkg(pkg.parts.toSeq, clientTree)
            Output(
              getOutputPath(defn, suffix = Some("Client")),
              wrapped,
              pkg,
              CompilerProduct.Definition,
            )
        }.toList

      F.pure(mainOutput :: wiringOutput ::: clientOutput)
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
            getOutputPath(defn, suffix = Some("Fixture")),
            fixtureTreeWithPkg,
            trans.toKtPkg(domain.id, domain.version, evo),
            CompilerProduct.Fixture,
          )
      }

      F.pure(fixtureTreeOut.toList)
    }

    private def makeFixtureRepr(defn: DomainMember.User): Option[TextTree[KtValue]] = {
      val srcRef = trans.toKtTypeRefKeepForeigns(defn.id, domain, evo)
      val ns     = srcRef.pkg.parts

      val fixtureTree        = codecsFixture.translate(defn)
      val fixtureTreeWithPkg = fixtureTree.map(t => ktTrees.inPkg(ns.toSeq, t))

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
            getOutputPath(defn, suffix = Some("Tests")),
            codecTestWithPkg,
            trans.toKtPkg(domain.id, domain.version, evo),
            CompilerProduct.Test,
          )
      }

      F.pure(codecTestOut.toList)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map {
        tree =>
          val pkg     = trans.toKtPkg(domain.id, domain.version, evo)
          val wrapped = ktTrees.inPkg(pkg.parts.toSeq, tree)
          val fbase   = ktFiles.basename(domain, evo)
          Output(
            s"$fbase/BaboonServiceRt.kt",
            wrapped,
            pkg,
            CompilerProduct.Definition,
          )
      }.toList
      F.pure(result)
    }

    private def makeTestRepr(defn: DomainMember.User): Option[TextTree[KtValue]] = {
      val ktTypeRef = trans.asKtType(defn.id, domain, evo)
      val srcRef    = trans.toKtTypeRefKeepForeigns(defn.id, domain, evo)
      val ns        = srcRef.pkg.parts

      val testTree        = codecTests.translate(defn, ktTypeRef, srcRef)
      val testTreeWithPkg = testTree.map(t => ktTrees.inPkg(ns.toSeq, t))

      testTreeWithPkg
    }

    private def makeFullRepr(
      defn: DomainMember.User,
      inPkg: Boolean,
    ): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      def deprecatePrevious(tree: TextTree[KtValue]): TextTree[KtValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""@Suppress("DEPRECATION") @Deprecated("Version ${domain.version.v.toString} is deprecated, you should migrate to ${evo.latest.v.toString}")
             |$tree""".stripMargin
        }
      }

      val ktTypeRef = trans.asKtType(defn.id, domain, evo)
      val srcRef    = trans.toKtTypeRefKeepForeigns(defn.id, domain, evo)

      val repr = makeRepr(defn, ktTypeRef, isLatestVersion)

      val codecTrees =
        codecs.toList
          .flatMap(t => t.translate(defn, ktTypeRef, srcRef).toList)
          .map(deprecatePrevious)

      val defnRepr = deprecatePrevious(repr.defn)

      assert(defn.id.pkg == domain.id)

      val ns = srcRef.pkg.parts

      val allDefs = (defnRepr +: codecTrees).joinNN()
      val content = if (inPkg) ktTrees.inPkg(ns.toSeq, allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => Nil
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap(
              codec =>
                if (codec.isActive(d.id)) List(codec.id -> q"$baboonLazy { ${codec.codecName(srcRef).copy(fq = true)} }")
                else Nil
            )
          List(CodecReg(defn.id, ktTypeRef, srcRef, q"\"${defn.id.toString}\"", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      DefnRepr(content, allRegs)
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: KtValue.KtType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = ktDomainTreeTools.makeDataMeta(defn)
      val codecMeta = codecs.flatMap(_.codecMeta(defn, name).map(_.member)).map {
        m => if (isLatestVersion) m else q"""@Suppress("DEPRECATION") $m"""
      }

      defn.defn match {
        case dto: Typedef.Dto =>
          val contractFieldNames = collectContractFieldNames(dto.contracts)
          val params = dto.fields.map {
            f =>
              val t      = trans.asKtNullableRef(f.tpe, domain, evo)
              val prefix = if (contractFieldNames.contains(f.name.name)) "override val" else "val"
              q"$prefix ${f.name.name}: $t"
          }
          val paramsList      = if (params.nonEmpty) params.join(",\n") else q""
          val contractParents = dto.contracts.map(c => trans.toKtTypeRefKeepForeigns(c, domain, evo))
          val (adtParent, adtMarker) = dto.id.owner match {
            case Owner.Adt(id) => (Some(trans.toKtTypeRefKeepForeigns(id, domain, evo)), Seq(iBaboonAdtMemberMeta))
            case _             => (None, Seq.empty)
          }
          val interfaceParents = (adtMarker ++ contractParents :+ genMarker).distinct

          val parentsList = adtParent match {
            case Some(adtType) =>
              if (interfaceParents.nonEmpty) q" : $adtType(), ${interfaceParents.map(t => q"$t").join(", ")}"
              else q" : $adtType()"
            case None =>
              if (interfaceParents.nonEmpty) q" : ${interfaceParents.map(t => q"$t").join(", ")}"
              else q""
          }

          val objectMetaFields = mainMeta.map(_.valueField) ++ codecMeta
          val classMetaFields  = mainMeta.map(mt => q"override ${mt.refValueField}")

          val hasFields    = params.nonEmpty
          val classKeyword = if (hasFields) "data class" else "class"

          val paramsBlock = if (hasFields) {
            q"""(
               |  ${paramsList.shift(2).trim}
               |)""".stripMargin
          } else q"" // no-arg class

          // Identifier toString override (PR-57b / spec: docs/spec/identifier-repr.md).
          // Emitted only when `dto.isIdentifier == true`.
          val identifierToStringOverride: TextTree[KtValue] =
            if (dto.isIdentifier) renderIdentifierToString(dto, name) else q""

          val emptyClassMethods = if (!hasFields) {
            // For empty-field id, replace the default toString with the spec form
            // `<Name>:<version>#`. equals/hashCode keep their structural identity.
            val toStringMethod =
              if (dto.isIdentifier) identifierToStringOverride
              else q"""override fun toString(): String = "${name.asName}()""""
            q"""
               |override fun equals(other: kotlin.Any?): Boolean = other is ${name.asName}
               |override fun hashCode(): Int = ${name.asName.hashCode.toString}
               |$toStringMethod
               |""".stripMargin
          } else q""

          // For non-empty id data classes, the toString override is appended to the body.
          val nonEmptyExtra: TextTree[KtValue] =
            if (dto.isIdentifier && hasFields) identifierToStringOverride else q""

          val mainTree =
            q"""$classKeyword ${name.asName}$paramsBlock$parentsList {
               |  ${classMetaFields.joinN().shift(2).trim}
               |  ${emptyClassMethods.shift(2).trim}
               |  ${nonEmptyExtra.shift(2).trim}
               |
               |  companion object {
               |    ${objectMetaFields.joinN().shift(4).trim}
               |  }
               |}""".stripMargin

          // Identifier <TypeName>Codec object (PR-57b Q-FU-4): parseRepr lives on a
          // sibling top-level object, NOT on the data class companion, so it does not
          // appear in IDE autocomplete on the type as the obvious "parse my id" entry.
          val combined =
            if (dto.isIdentifier) {
              val codecObject = renderIdentifierCodecObject(dto, name)
              q"""$mainTree
                 |
                 |$codecObject""".stripMargin
            } else mainTree

          DefnRepr(combined, Nil)

        case e: Typedef.Enum =>
          val cases = e.members.map {
            m =>
              val obj = EnumWireStyle.wireName(m.name)
              q"$obj"
          }.toList

          val parseCases = e.members.map {
            m =>
              val obj = EnumWireStyle.wireName(m.name)
              q"\"$obj\" to $obj"
          }.toList

          DefnRepr(
            q"""enum class ${name.asName} {
               |  ${cases.join(",\n").shift(2).trim};
               |
               |  companion object : $baboonEnum<${name.asName}> {
               |    private val byName = mapOf(
               |      ${parseCases.join(",\n").shift(6).trim}
               |    )
               |    override fun parse(s: String): ${name.asName}? = byName[s]
               |    override fun all(): List<${name.asName}> = entries.toList()
               |  }
               |}""".stripMargin,
            Nil,
          )

        case adt: Typedef.Adt =>
          val contractParents = adt.contracts.map(c => trans.toKtTypeRefKeepForeigns(c, domain, evo))
          val parents         = (contractParents :+ genMarker).distinct
          val parentsList     = if (parents.nonEmpty) q" : ${parents.map(t => q"$t").join(", ")}" else q""

          val memberTrees = adt.members.map {
            mid =>
              domain.defs.meta.nodes(mid) match {
                case mdefn: DomainMember.User => makeFullRepr(mdefn, inPkg = false)
                case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
              }
          }

          val objectMetaFields = mainMeta.map(_.valueField) ++ codecMeta
          val classMetaFields  = mainMeta.map(mt => q"override ${mt.refValueField}")

          DefnRepr(
            q"""sealed class ${name.asName}$parentsList {
               |  ${classMetaFields.joinN().shift(2).trim}
               |
               |  ${memberTrees.map(_.defn).toList.joinNN().shift(2).trim}
               |
               |  companion object {
               |    ${objectMetaFields.joinN().shift(4).trim}
               |  }
               |}""".stripMargin,
            Nil,
          )

        case contract: Typedef.Contract =>
          val methods = contract.fields.map {
            f =>
              val t = trans.asKtNullableRef(f.tpe, domain, evo)
              q"val ${f.name.name}: $t"
          }
          val contractParents = contract.contracts.map(c => trans.toKtTypeRefKeepForeigns(c, domain, evo))
          val parents         = (contractParents :+ genMarker).distinct
          val parentsList     = if (parents.nonEmpty) q" : ${parents.map(t => q"$t").join(", ")}" else q""
          val body            = if (methods.nonEmpty) methods.joinN() else q""
          DefnRepr(
            q"""interface ${name.asName}$parentsList {
               |  ${body.shift(2).trim}
               |}""".stripMargin,
            Nil,
          )

        case service: Typedef.Service =>
          val resolved    = ServiceResultResolver.resolve(domain, "kotlin", target.language.serviceResult, target.language.pragmas)
          val resolvedCtx = ServiceContextResolver.resolve(domain, "kotlin", target.language.serviceContext, target.language.pragmas)
          val ctxParam = resolvedCtx match {
            case ResolvedServiceContext.NoContext               => ""
            case ResolvedServiceContext.AbstractContext(tn, pn) => s"$pn: $tn, "
            case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$pn: $tn, "
          }
          val methods = service.methods.map {
            m =>
              val in  = trans.asKtRef(m.sig, domain, evo)
              val out = m.out.map(trans.asKtRef(_, domain, evo))
              val err = m.err.map(trans.asKtRef(_, domain, evo))
              val ktFqName: KtValue => String = {
                case t: KtValue.KtType     => if (t.predef) t.name else (t.pkg.parts :+ t.name).mkString(".")
                case t: KtValue.KtTypeName => t.name
              }
              val outStr = out.map(_.mapRender(ktFqName)).getOrElse("")
              val errStr = err.map(_.mapRender(ktFqName))
              val retStr = resolved.renderReturnType(outStr, errStr, "Unit")
              q"fun ${m.name.name}(${ctxParam}arg: $in): $retStr"
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
          val resultImportHint = resolved.resultType
            .filter(_ != "Unit").map {
              rt =>
                val ktType = KtValue.KtType(baboonRuntimePkg, rt)
                q"private typealias _${service.id.name.name}SvcResult<L, R> = $ktType<L, R>\n\n"
            }.getOrElse(q"")
          DefnRepr(
            q"""${resultImportHint}interface ${name.asName}$traitTypeParam {
               |  ${body.shift(2).trim}
               |}""".stripMargin,
            Nil,
          )

        case _: Typedef.Foreign => DefnRepr(q"", Nil)
      }
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

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = ktFiles.basename(domain, evo)
      val fname = s"${defn.id.name.name}${suffix.getOrElse("")}.kt"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}/$fname"
      }
    }

    // ----- Identifier toString + parseRepr emission (PR-57b) -----
    // Spec contract: docs/spec/identifier-repr.md. Mirrors ScDefnTranslator /
    // JvDefnTranslator but uses Kotlin idiom (top-level objects, ULong unsigned
    // arithmetic, Either with Left/Right subclasses).

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

    private def renderFieldValueExpr(ktFieldName: String, kind: IdentifierFieldKind): TextTree[KtValue] = {
      kind match {
        case IdentifierFieldKind.Bit              => q"$baboonIdRepr.bitToString(this.$ktFieldName)"
        // Kotlin's signed and unsigned-small primitive toString already produces
        // canonical decimal (no locale, unsigned-small types render as unsigned).
        case IdentifierFieldKind.SignedInt        => q"this.$ktFieldName.toString()"
        case IdentifierFieldKind.UnsignedSmallInt => q"this.$ktFieldName.toString()"
        case IdentifierFieldKind.UnsignedLong     => q"$baboonIdRepr.u64ToString(this.$ktFieldName)"
        case IdentifierFieldKind.Str              => q"$baboonIdRepr.escapeStr(this.$ktFieldName)"
        // UUID.toString() (java.util.UUID) and kotlin.uuid.Uuid.toString() both
        // emit the canonical lowercase 36-char hyphenated form per RFC 4122.
        case IdentifierFieldKind.Uid              => q"this.$ktFieldName.toString()"
        case IdentifierFieldKind.Tsu              => q"$baboonIdRepr.tsuToString(this.$ktFieldName)"
        case IdentifierFieldKind.Tso              => q"$baboonIdRepr.tsoToString(this.$ktFieldName)"
        case IdentifierFieldKind.Bytes            => q"$baboonIdRepr.bytesToHex(this.$ktFieldName)"
        case IdentifierFieldKind.NestedId(_)      => q""""{" + this.$ktFieldName.toString() + "}""""
      }
    }

    private def renderIdentifierToString(dto: Typedef.Dto, name: KtValue.KtType): TextTree[KtValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString
      val header     = s"$simpleName:$versionStr#"

      val fieldExprs: List[TextTree[KtValue]] = dto.fields.map {
        f =>
          val srcFieldName = f.name.name
          val kind         = identifierFieldKind(f.tpe)
          val valueExpr    = renderFieldValueExpr(srcFieldName, kind)
          q""""$srcFieldName:" + ($valueExpr)"""
      }

      val joinedFields =
        if (fieldExprs.isEmpty) q""""""""
        else fieldExprs.toSeq.join(""" + ":" + """)

      q"""override fun toString(): String {
         |  return "$header" + $joinedFields
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
        case TypeRef.Scalar(TypeId.Builtins.i08) => "toByte()"
        case TypeRef.Scalar(TypeId.Builtins.i16) => "toShort()"
        case TypeRef.Scalar(TypeId.Builtins.i32) => "toInt()"
        case TypeRef.Scalar(TypeId.Builtins.i64) => "toLong()"
        case other                               => throw new IllegalStateException(s"signedNarrow on non-signed-int: $other")
      }
    }

    private def unsignedSmallNarrow(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => "toUByte()"
        case TypeRef.Scalar(TypeId.Builtins.u16) => "toUShort()"
        case TypeRef.Scalar(TypeId.Builtins.u32) => "toUInt()"
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

    private def renderIdentifierCodecObject(dto: Typedef.Dto, name: KtValue.KtType): TextTree[KtValue] = {
      val simpleName     = name.name
      val versionStr     = domain.version.toString
      val codecObjectName = s"${name.name}Codec"

      val fieldDecoders: List[TextTree[KtValue]] = dto.fields.zipWithIndex.map {
        case (f, idx) =>
          val srcFieldName = f.name.name
          val rawVar       = s"${srcFieldName}_raw"
          val valVar       = s"${srcFieldName}_v"
          val isLast       = idx == dto.fields.length - 1
          val kind         = identifierFieldKind(f.tpe)
          val tpe          = trans.asKtRef(f.tpe, domain, evo)

          // All locals beyond `valVar` (which the constructor consumes) are scoped
          // per-field with unique suffix names so multiple fields can declare them
          // in the same enclosing function body without name collisions.
          val rN     = s"${srcFieldName}_r"
          val rNn    = s"${srcFieldName}_rn"
          val rNb    = s"${srcFieldName}_rb"
          val rNs    = s"${srcFieldName}_rs"
          val rNh    = s"${srcFieldName}_rh"
          val rNt    = s"${srcFieldName}_rt"
          val rNrf   = s"${srcFieldName}_rrf"
          val vL     = s"${srcFieldName}_vL"
          val vU     = s"${srcFieldName}_vU"

          val parseHead =
            q"""val $rN = $baboonIdRepr.parseFieldName(cursor, "$srcFieldName")
               |if ($rN is $baboonEither.Left) return $baboonEither.Left($rN.value)""".stripMargin

          val parseValue: TextTree[KtValue] = kind match {
            case IdentifierFieldKind.Bit =>
              q"""val $rawVar = cursor.readUntilStructural()
                 |val $rNb = $baboonIdRepr.parseBit($rawVar)
                 |if ($rNb is $baboonEither.Left) return $baboonEither.Left($rNb.value)
                 |val $valVar: $tpe = ($rNb as $baboonEither.Right).value""".stripMargin
            case IdentifierFieldKind.SignedInt =>
              val rangeCheck = signedRangeCheck(f.tpe)
              val typeName   = signedTypeName(f.tpe)
              val narrow     = signedNarrow(f.tpe)
              // i64 covers the full Long range — signedRangeCheck returns "true" — so
              // no range-check block is needed (mirrors CSDefnTranslator PR-57a-D01 fix).
              val rangeBlock =
                if (rangeCheck == "true") q""
                else {
                  val rc = rangeCheck.replace("v ", s"$vL ")
                  q"""if (!($rc)) {
                     |  return $baboonEither.Left("$typeName out of range for field $srcFieldName: " + $rawVar)
                     |}
                     |""".stripMargin
                }
              q"""val $rawVar = cursor.readUntilStructural()
                 |// Spec §5.4: signed integers must not carry a leading '+'.
                 |if ($rawVar.isNotEmpty() && $rawVar[0] == '+') {
                 |  return $baboonEither.Left("signed integer must not have leading '+' for field $srcFieldName: " + $rawVar)
                 |}
                 |val $vL = $rawVar.toLongOrNull()
                 |  ?: return $baboonEither.Left("could not parse signed integer for field $srcFieldName: " + $rawVar)
                 |${rangeBlock}val $valVar: $tpe = $vL.$narrow""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              // Range check operates on the parsed Long ($vL = vU.toLong()). Substitute
              // the placeholder `v` with the field-scoped name to avoid collisions when
              // multiple u-small fields appear in the same id.
              val rangeCheck = unsignedSmallRangeCheck(f.tpe).replace("v ", s"$vL ")
              val typeName   = unsignedSmallTypeName(f.tpe)
              val narrow     = unsignedSmallNarrow(f.tpe)
              // Reject leading sign (spec §5.4 D06) before delegating to toULongOrNull
              // (which silently accepts `+`).
              q"""val $rawVar = cursor.readUntilStructural()
                 |if ($rawVar.isNotEmpty() && ($rawVar[0] == '+' || $rawVar[0] == '-')) {
                 |  return $baboonEither.Left("unsigned value has leading sign for field $srcFieldName: " + $rawVar)
                 |}
                 |val $vU = $rawVar.toULongOrNull()
                 |  ?: return $baboonEither.Left("could not parse unsigned integer for field $srcFieldName: " + $rawVar)
                 |val $vL = $vU.toLong()
                 |if (!($rangeCheck)) {
                 |  return $baboonEither.Left("$typeName out of range for field $srcFieldName: " + $rawVar)
                 |}
                 |val $valVar: $tpe = $vU.$narrow""".stripMargin
            case IdentifierFieldKind.UnsignedLong =>
              q"""val $rawVar = cursor.readUntilStructural()
                 |if ($rawVar.isNotEmpty() && ($rawVar[0] == '+' || $rawVar[0] == '-')) {
                 |  return $baboonEither.Left("unsigned value has leading sign for field $srcFieldName: " + $rawVar)
                 |}
                 |val $valVar: $tpe = $rawVar.toULongOrNull()
                 |  ?: return $baboonEither.Left("could not parse u64 for field $srcFieldName: " + $rawVar)""".stripMargin
            case IdentifierFieldKind.Str =>
              q"""val $rNs = cursor.readStrField()
                 |if ($rNs is $baboonEither.Left) return $baboonEither.Left($rNs.value)
                 |val $valVar: $tpe = ($rNs as $baboonEither.Right).value""".stripMargin
            case IdentifierFieldKind.Uid =>
              val uidType   = ktTypes.ktUid
              val uidParse  =
                if (target.language.multiplatform) q"$uidType.parse($rawVar)"
                else q"$uidType.fromString($rawVar)"
              q"""val $rawVar = cursor.readUntilStructural()
                 |if (!$baboonIdRepr.isCanonicalUid($rawVar)) {
                 |  return $baboonEither.Left("uid not in canonical lowercase form for field $srcFieldName: " + $rawVar)
                 |}
                 |val $valVar: $tpe = try {
                 |  $uidParse
                 |} catch (e: IllegalArgumentException) {
                 |  return $baboonEither.Left("could not parse uid for field $srcFieldName: " + $rawVar)
                 |}""".stripMargin
            case IdentifierFieldKind.Tsu =>
              q"""val $rNrf = cursor.readFixed(24)
                 |if ($rNrf is $baboonEither.Left) return $baboonEither.Left($rNrf.value)
                 |val $rawVar = ($rNrf as $baboonEither.Right).value
                 |val $rNt = $baboonIdRepr.parseTsuRepr($rawVar)
                 |if ($rNt is $baboonEither.Left) return $baboonEither.Left($rNt.value)
                 |val $valVar: $tpe = ($rNt as $baboonEither.Right).value""".stripMargin
            case IdentifierFieldKind.Tso =>
              q"""val $rNrf = cursor.readFixed(29)
                 |if ($rNrf is $baboonEither.Left) return $baboonEither.Left($rNrf.value)
                 |val $rawVar = ($rNrf as $baboonEither.Right).value
                 |val $rNt = $baboonIdRepr.parseTsoRepr($rawVar)
                 |if ($rNt is $baboonEither.Left) return $baboonEither.Left($rNt.value)
                 |val $valVar: $tpe = ($rNt as $baboonEither.Right).value""".stripMargin
            case IdentifierFieldKind.Bytes =>
              q"""val $rawVar = cursor.readUntilStructural()
                 |val $rNh = $baboonIdRepr.parseBytesHex($rawVar)
                 |if ($rNh is $baboonEither.Left) return $baboonEither.Left($rNh.value)
                 |val $valVar: $tpe = ($rNh as $baboonEither.Right).value""".stripMargin
            case IdentifierFieldKind.NestedId(uid) =>
              val nestedTpe   = trans.toKtTypeRefKeepForeigns(uid, domain, evo)
              val nestedCodec = KtType(nestedTpe.pkg, s"${nestedTpe.name}Codec")
              val rOpen       = s"${srcFieldName}_ro"
              val rClose      = s"${srcFieldName}_rc"
              q"""val $rOpen = cursor.expect('{')
                 |if ($rOpen is $baboonEither.Left) return $baboonEither.Left($rOpen.value)
                 |val $rNn = $nestedCodec.parseRepr(cursor)
                 |if ($rNn is $baboonEither.Left) return $baboonEither.Left($rNn.value)
                 |val $valVar: $nestedTpe = ($rNn as $baboonEither.Right).value
                 |val $rClose = cursor.expect('}')
                 |if ($rClose is $baboonEither.Left) return $baboonEither.Left($rClose.value)""".stripMargin
          }

          val sep =
            if (isLast) q""
            else {
              val rSep = s"${srcFieldName}_rsep"
              q"""val $rSep = cursor.expect(':')
                 |if ($rSep is $baboonEither.Left) return $baboonEither.Left($rSep.value)""".stripMargin
            }

          q"""$parseHead
             |$parseValue
             |$sep""".stripMargin.trim
      }

      val ctorArgs = dto.fields.map(f => q"${f.name.name}_v").toSeq
      val ctor =
        if (ctorArgs.nonEmpty) q"$name(${ctorArgs.join(", ")})"
        else q"$name()"

      val body = (fieldDecoders :+ q"return $baboonEither.Right($ctor)").joinNN()

      q"""object $codecObjectName {
         |  /** Parse the canonical identifier repr per docs/spec/identifier-repr.md.
         |   *  Schema-directed parser: walks declared field order and dispatches per
         |   *  field type. Returns Left on any malformed input. */
         |  fun parseRepr(s: String): $baboonEither<String, $name> {
         |    val cursor = $baboonIdReprCursor(s)
         |    val inner = parseRepr(cursor)
         |    if (inner is $baboonEither.Left) return inner
         |    if (!cursor.atEnd()) return $baboonEither.Left("unexpected trailing input at " + cursor.position())
         |    return inner
         |  }
         |
         |  fun parseRepr(cursor: $baboonIdReprCursor): $baboonEither<String, $name> {
         |    run {
         |      val r = $baboonIdRepr.parseHeader(cursor, "$simpleName", "$versionStr")
         |      if (r is $baboonEither.Left) return $baboonEither.Left(r.value)
         |    }
         |    ${body.shift(4).trim}
         |  }
         |}""".stripMargin
    }

  }
}
