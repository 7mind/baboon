package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.dart.DtValue.DtType
import io.septimalmind.baboon.typer.EnumWireStyle
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait DtDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[DtDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[DtDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[DtDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[DtDefnTranslator.Output]]
}

object DtDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: DtType,
    tpeKeepForeigns: DtType,
    tpeId: TextTree[DtValue],
    trees: Map[String, TextTree[DtValue]],
  )

  final case class DefnRepr(
    defn: TextTree[DtValue],
    codecs: List[CodecReg],
  )

  final case class Output(
    path: String,
    tree: TextTree[DtValue],
    module: DtValue.DtPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[DtValue]])] = Nil,
  )

  class DtDefnTranslatorImpl[F[+_, +_]: Applicative2](
    domain: Domain,
    evo: BaboonEvolution,
    dtFiles: DtFileTools,
    dtTrees: DtTreeTools,
    trans: DtTypeTranslator,
    codecs: Set[DtCodecTranslator],
    codecTests: DtCodecTestsTranslator,
    codecsFixture: DtCodecFixtureTranslator,
    wiringTranslator: DtServiceWiringTranslator,
    dtDomainTreeTools: DtDomainTreeTools,
  ) extends DtDefnTranslator[F] {
    import DtTypes.*

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr = makeFullRepr(defn, inLib = true)

      val registrations = codecs.toList.map(codec => codec.id -> repr.codecs.flatMap(reg => reg.trees.get(codec.id).map(expr => q"${reg.tpeId}, $expr")))

      val mainOutput = Output(
        getOutputPath(defn),
        repr.defn,
        getOutputModule(defn),
        CompilerProduct.Definition,
        codecReg = registrations,
      )

      F.pure(List(mainOutput))
    }

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateFixtures(defn)
      }
    }

    private def doTranslateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val fixtureTree = codecsFixture.translate(defn)
      val result = fixtureTree.map {
        tree =>
          Output(
            getOutputPath(defn, suffix = Some("_fixture")),
            tree,
            getOutputModule(defn),
            CompilerProduct.Fixture,
          )
      }.toList
      F.pure(result)
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTest(defn)
      }
    }

    private def doTranslateTest(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val dtTypeRef   = trans.asDtType(defn.id, domain, evo)
      val srcRef      = trans.toDtTypeRefKeepForeigns(defn.id, domain, evo)
      val testPath    = getOutputPath(defn, suffix = Some("_test"))
      val typePath    = getOutputPath(defn)
      val fixturePath = getOutputPath(defn, suffix = Some("_fixture"))
      val testTree    = codecTests.translate(defn, dtTypeRef, srcRef, testPath, typePath, fixturePath)
      val result = testTree.map {
        tree =>
          Output(
            testPath,
            tree,
            getOutputModule(defn),
            CompilerProduct.Test,
            doNotModify = true,
          )
      }.toList
      F.pure(result)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map {
        tree =>
          val pkg   = trans.toDtPkg(domain.id, domain.version, evo)
          val fbase = dtFiles.basename(domain, evo)
          Output(
            s"$fbase/baboon_service_rt.dart",
            tree,
            pkg,
            CompilerProduct.Definition,
          )
      }.toList
      F.pure(result)
    }

    private def makeFullRepr(
      defn: DomainMember.User,
      inLib: Boolean,
    ): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      def deprecatePrevious(tree: TextTree[DtValue]): TextTree[DtValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""@Deprecated('Version ${domain.version.v.toString} is deprecated, you should migrate to ${evo.latest.v.toString}')
             |$tree""".stripMargin
        }
      }

      val dtTypeRef = trans.asDtType(defn.id, domain, evo)
      val srcRef    = trans.toDtTypeRefKeepForeigns(defn.id, domain, evo)

      val repr = makeRepr(defn, dtTypeRef, isLatestVersion)

      val codecTrees =
        codecs.toList
          .flatMap(t => t.translate(defn, dtTypeRef, srcRef).toList)
          .map(deprecatePrevious)

      val defnRepr = deprecatePrevious(repr.defn)

      assert(defn.id.pkg == domain.id)

      val allDefs = (defnRepr +: codecTrees).joinNN()
      val content = if (inLib) dtTrees.inLib(allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => Nil
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap(
              codec =>
                // PR 13.2 fix (PR-26-D02): Dart per-domain codec aggregator was registering the
                // codec class type literal (e.g. `InnerPayload_JsonCodec`) instead of the instance.
                // The runtime then `as BaboonCodecData`-cast the `Type` and threw. Append `.instance`
                // so the registration produces the singleton object the runtime expects.
                if (codec.isActive(d.id)) List(codec.id -> q"() => ${codec.codecName(srcRef).copy(fq = true)}.instance")
                else Nil
            )
          List(CodecReg(defn.id, dtTypeRef, srcRef, q"'${defn.id.toString}'", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      DefnRepr(content, allRegs)
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: DtType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = dtDomainTreeTools.makeDataMeta(defn)
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

        case _: Typedef.Service =>
          renderService(defn, name)

        case f: Typedef.Foreign => makeForeignKeyCodecRepr(f, name)
      }
    }

    /** PR-I.1d (M24 Phase 3.1) — emit a `<Foreign>_KeyCodec` extension hook for
      * every Custom-mapped Dart foreign declaration. The host application
      * registers an implementation at boot which the JSON codec then uses to
      * encode/decode map keys. For BaboonRef-mapped foreigns we emit nothing —
      * the existing recursion into the aliased type covers the codec needs.
      *
      * Stringy foreigns (`dart.core.String` / `String`) get a default identity
      * impl so the common case works out of the box. Non-stringy foreigns get
      * a stub default that throws BaboonDecoderFailure with an FQN-bearing
      * diagnostic referring to the Host class (PR-I.1b-D01 lesson).
      */
    private def makeForeignKeyCodecRepr(f: Typedef.Foreign, name: DtType): DefnRepr = {
      f.bindings.get(BaboonLang.Dart) match {
        case None                                                                  => DefnRepr(q"", Nil)
        case Some(Typedef.ForeignEntry(_, _: Typedef.ForeignMapping.BaboonRef))    => DefnRepr(q"", Nil)
        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) =>
          val srcRef    = trans.toDtTypeRefKeepForeigns(f.id, domain, evo)
          val codecName = s"${srcRef.name}_KeyCodec"
          val hostName  = s"${srcRef.name}_KeyCodecHost"
          val hostFqn   = s"${srcRef.pkg.parts.mkString(".")}.$hostName"
          // Stringy allowlist (PR-I-D06 pattern guidance: only the language's allowlist; no dead alternatives).
          // Both the FQN form (`dart.core.String`) and the bare identifier (`String`) are accepted because
          // Dart code idiomatically references the type either way.
          val isStringy = decl == "dart.core.String" || decl == "String"
          val defaultImplName = s"_${codecName}DefaultImpl"
          val defaultImpl = if (isStringy) {
            q"""class $defaultImplName implements $codecName {
               |  @override $dtString encodeKey($name value) => value;
               |  @override $name decodeKey($dtString s) => s;
               |}""".stripMargin
          } else {
            q"""class $defaultImplName implements $codecName {
               |  @override $dtString encodeKey($name value) => throw $baboonDecoderFailure('$hostFqn is not registered; call $hostFqn.register(impl) at app boot.', null);
               |  @override $name decodeKey($dtString s) => throw $baboonDecoderFailure('$hostFqn is not registered; call $hostFqn.register(impl) at app boot.', null);
               |}""".stripMargin
          }
          val tree =
            q"""abstract class $codecName {
               |  $dtString encodeKey($name value);
               |  $name decodeKey($dtString s);
               |}
               |
               |class $hostName {
               |  static $codecName _instance = $defaultImplName();
               |  static void register($codecName impl) { _instance = impl; }
               |  static $codecName get instance => _instance;
               |}
               |
               |${defaultImpl}""".stripMargin
          DefnRepr(tree, Nil)
      }
    }

    private def renderDto(
      dto: Typedef.Dto,
      name: DtType,
      genMarker: DtType,
      mainMeta: List[DtDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[DtValue]],
    ): DefnRepr = {
      val contractFieldNames = collectContractFieldNames(dto.contracts)
      val hasFields          = dto.fields.nonEmpty

      val fieldDeclarations = dto.fields.map {
        f =>
          val t              = trans.asDtRef(f.tpe, domain, evo)
          val overridePrefix = if (contractFieldNames.contains(f.name.name)) "@override " else ""
          f.tpe match {
            case TypeRef.Constructor(TypeId.Builtins.opt, _) =>
              q"${overridePrefix}final $t ${f.name.name};"
            case _ =>
              q"${overridePrefix}final $t ${f.name.name};"
          }
      }

      val constructorParams = dto.fields.map {
        f =>
          f.tpe match {
            case TypeRef.Constructor(TypeId.Builtins.opt, _) =>
              q"this.${f.name.name}"
            case _ =>
              q"required this.${f.name.name}"
          }
      }

      val contractParents = dto.contracts.map(c => trans.toDtTypeRefKeepForeigns(c, domain, evo))
      val (adtMarker, adtParent) = dto.id.owner match {
        case Owner.Adt(adtId) =>
          val adtType = trans.toDtTypeRefKeepForeigns(adtId, domain, evo)
          (Seq(iBaboonAdtMemberMeta), Seq(adtType))
        case _ => (Seq.empty, Seq.empty)
      }

      val interfaceParents = (adtParent ++ adtMarker ++ contractParents :+ genMarker).distinct
      val implementsList   = interfaceParents.map(t => q"$t").join(", ")

      val extendsClause = dto.id.owner match {
        case Owner.Adt(_) =>
          val adtType = adtParent.head
          val ifaces  = (adtMarker ++ contractParents :+ genMarker).distinct
          if (ifaces.nonEmpty) q" extends $adtType implements ${ifaces.map(t => q"$t").join(", ")}"
          else q" extends $adtType"
        case _ =>
          q" implements $implementsList"
      }

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      val constructorBlock = if (hasFields) {
        q"""const ${name.asName}({
           |  ${constructorParams.join(",\n").shift(2).trim}
           |});""".stripMargin
      } else {
        q"const ${name.asName}();"
      }

      val fieldsBlock = if (hasFields) {
        fieldDeclarations.joinN()
      } else q""

      val equalsBody = if (hasFields) {
        val fieldComparisons = dto.fields.map(f => q"baboonDeepEquals(${f.name.name}, other.${f.name.name})")
        q"""@override
           |bool operator ==(Object other) =>
           |  identical(this, other) ||
           |  other is ${name.asName} &&
           |  ${fieldComparisons.join(" &&\n  ").shift(2).trim};""".stripMargin
      } else {
        q"""@override
           |bool operator ==(Object other) => other is ${name.asName};""".stripMargin
      }

      val hashCodeBody = if (hasFields) {
        val hashParts = dto.fields.map(f => q"baboonDeepHashCode(${f.name.name})")
        q"""@override
           |int get hashCode => Object.hashAll([${hashParts.join(", ")}]);""".stripMargin
      } else {
        q"""@override
           |int get hashCode => ${name.asName.hashCode.toString};""".stripMargin
      }

      val toStringBody = if (dto.isIdentifier) {
        // Identifier toString — spec docs/spec/identifier-repr.md, PR-57d.
        renderIdentifierToString(dto, name)
      } else if (hasFields) {
        val fieldStrings = dto.fields.map(f => q"${f.name.name}: $$${f.name.name}")
        q"""@override
           |String toString() => '${name.asName}(${fieldStrings.join(", ")})';""".stripMargin
      } else {
        q"""@override
           |String toString() => '${name.asName}()';""".stripMargin
      }

      val identifierCodec: TextTree[DtValue] =
        if (dto.isIdentifier) renderIdentifierCodecClass(dto, name) else q""

      val classDefn =
        q"""class ${name.asName}$extendsClause {
           |  ${fieldsBlock.shift(2).trim}
           |
           |  ${constructorBlock.shift(2).trim}
           |
           |  ${staticMetaFields.joinN().shift(2).trim}
           |
           |  ${equalsBody.shift(2).trim}
           |
           |  ${hashCodeBody.shift(2).trim}
           |
           |  ${toStringBody.shift(2).trim}
           |}""".stripMargin

      val combined =
        if (dto.isIdentifier)
          q"""$classDefn
             |
             |$identifierCodec""".stripMargin
        else classDefn

      DefnRepr(combined, Nil)
    }

    private def renderEnum(
      e: Typedef.Enum,
      name: DtType,
      mainMeta: List[DtDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[DtValue]],
    ): DefnRepr = {
      val cases = e.members.map {
        m =>
          q"${EnumWireStyle.wireName(m.name)}"
      }.toList

      val parseCases = e.members.map {
        m =>
          val pascal = EnumWireStyle.wireName(m.name)
          q"'$pascal' => $pascal,"
      }.toList

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      DefnRepr(
        q"""enum ${name.asName} implements $iBaboonGenerated {
           |  ${cases.join(",\n").shift(2).trim};
           |
           |  ${staticMetaFields.joinN().shift(2).trim}
           |
           |  static ${name.asName}? parse(String s) {
           |    return switch (s) {
           |      ${parseCases.joinN().shift(6).trim}
           |      _ => null,
           |    };
           |  }
           |
           |  static List<${name.asName}> get all => values;
           |}""".stripMargin,
        Nil,
      )
    }

    private def renderAdt(
      defn: DomainMember.User,
      adt: Typedef.Adt,
      name: DtType,
      genMarker: DtType,
      mainMeta: List[DtDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[DtValue]],
    ): DefnRepr = {
      val contractParents  = adt.contracts.map(c => trans.toDtTypeRefKeepForeigns(c, domain, evo))
      val parents          = (contractParents :+ genMarker).distinct
      val implementsClause = if (parents.nonEmpty) q" implements ${parents.map(t => q"$t").join(", ")}" else q""

      val memberTrees = adt.members.map {
        mid =>
          domain.defs.meta.nodes(mid) match {
            case mdefn: DomainMember.User => makeFullRepr(mdefn, inLib = false)
            case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
          }
      }

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      DefnRepr(
        q"""sealed class ${name.asName}$implementsClause {
           |  const ${name.asName}();
           |
           |  ${staticMetaFields.joinN().shift(2).trim}
           |}
           |
           |${memberTrees.map(_.defn).toList.joinNN()}""".stripMargin,
        memberTrees.toList.flatMap(_.codecs),
      )
    }

    private def renderContract(
      contract: Typedef.Contract,
      name: DtType,
      genMarker: DtType,
    ): DefnRepr = {
      val methods = contract.fields.map {
        f =>
          val t = trans.asDtRef(f.tpe, domain, evo)
          q"$t get ${f.name.name};"
      }
      val contractParents  = contract.contracts.map(c => trans.toDtTypeRefKeepForeigns(c, domain, evo))
      val parents          = (contractParents :+ genMarker).distinct
      val implementsClause = if (parents.nonEmpty) q" implements ${parents.map(t => q"$t").join(", ")}" else q""
      val body             = if (methods.nonEmpty) methods.joinN() else q""

      DefnRepr(
        q"""abstract interface class ${name.asName}$implementsClause {
           |  ${body.shift(2).trim}
           |}""".stripMargin,
        Nil,
      )
    }

    private def renderService(
      defn: DomainMember.User,
      name: DtType,
    ): DefnRepr = {
      val service = defn.defn.asInstanceOf[Typedef.Service]
      val methods = service.methods.map {
        m =>
          val in     = trans.asDtRef(m.sig, domain, evo)
          val out    = m.out.map(trans.asDtRef(_, domain, evo))
          val retStr = out.map(o => q"$o").getOrElse(q"void")
          q"$retStr ${m.name.name}($in arg);"
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""

      DefnRepr(
        q"""abstract class ${name.asName} {
           |  ${body.shift(2).trim}
           |}""".stripMargin,
        Nil,
      )
    }

    // ----- Identifier toString + parseRepr emission (PR-57d) -----
    // Spec: docs/spec/identifier-repr.md.
    private sealed trait IdentifierFieldKind
    private object IdentifierFieldKind {
      case object Bit              extends IdentifierFieldKind
      case object SignedInt        extends IdentifierFieldKind /* i08/i16/i32 */
      case object SignedLong       extends IdentifierFieldKind /* i64 */
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
            case `bit`                   => IdentifierFieldKind.Bit
            case `i08` | `i16` | `i32`   => IdentifierFieldKind.SignedInt
            case `i64`                   => IdentifierFieldKind.SignedLong
            case `u08` | `u16` | `u32`   => IdentifierFieldKind.UnsignedSmallInt
            case `u64`                   => IdentifierFieldKind.UnsignedLong
            case `str`                   => IdentifierFieldKind.Str
            case `uid`                   => IdentifierFieldKind.Uid
            case `tsu`                   => IdentifierFieldKind.Tsu
            case `tso`                   => IdentifierFieldKind.Tso
            case `bytes`                 => IdentifierFieldKind.Bytes
            case other =>
              throw new IllegalStateException(s"Identifier field has unsupported scalar $other; validator should have rejected this.")
          }
        case TypeRef.Scalar(uid: TypeId.User) =>
          IdentifierFieldKind.NestedId(uid)
        case other =>
          throw new IllegalStateException(s"Identifier field has unsupported TypeRef $other; validator should have rejected this.")
      }
    }

    private def signedTypeName(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => "i08"
      case TypeRef.Scalar(TypeId.Builtins.i16) => "i16"
      case TypeRef.Scalar(TypeId.Builtins.i32) => "i32"
      case other                               => throw new IllegalStateException(s"signedTypeName on non-signed-int: $other")
    }

    private def signedRangeCheck(tpe: TypeRef, varName: String): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => s"$varName >= -128 && $varName <= 127"
      case TypeRef.Scalar(TypeId.Builtins.i16) => s"$varName >= -32768 && $varName <= 32767"
      case TypeRef.Scalar(TypeId.Builtins.i32) => s"$varName >= -2147483648 && $varName <= 2147483647"
      case other                               => throw new IllegalStateException(s"signedRangeCheck on non-signed-int: $other")
    }

    private def unsignedSmallTypeName(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => "u08"
      case TypeRef.Scalar(TypeId.Builtins.u16) => "u16"
      case TypeRef.Scalar(TypeId.Builtins.u32) => "u32"
      case other                               => throw new IllegalStateException(s"unsignedSmallTypeName on non-u08/u16/u32: $other")
    }

    private def unsignedSmallRangeCheck(tpe: TypeRef, varName: String): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => s"$varName >= 0 && $varName <= 255"
      case TypeRef.Scalar(TypeId.Builtins.u16) => s"$varName >= 0 && $varName <= 65535"
      case TypeRef.Scalar(TypeId.Builtins.u32) => s"$varName >= 0 && $varName <= 4294967295"
      case other                               => throw new IllegalStateException(s"unsignedSmallRangeCheck on non-u08/u16/u32: $other")
    }

    private def renderIdentifierFieldValueExpr(dtFieldName: String, kind: IdentifierFieldKind): TextTree[DtValue] = {
      kind match {
        case IdentifierFieldKind.Bit              => q"$baboonIdRepr.bitToString($dtFieldName)"
        case IdentifierFieldKind.SignedInt        => q"$dtFieldName.toString()"
        case IdentifierFieldKind.SignedLong       => q"$dtFieldName.toString()"
        case IdentifierFieldKind.UnsignedSmallInt => q"$dtFieldName.toString()"
        case IdentifierFieldKind.UnsignedLong     => q"$baboonIdRepr.u64ToString($dtFieldName)"
        case IdentifierFieldKind.Str              => q"$baboonIdRepr.escapeStr($dtFieldName)"
        case IdentifierFieldKind.Uid              => q"$dtFieldName"
        case IdentifierFieldKind.Tsu              => q"$baboonIdRepr.tsuToString($dtFieldName)"
        case IdentifierFieldKind.Tso              => q"$baboonIdRepr.tsoToString($dtFieldName)"
        case IdentifierFieldKind.Bytes            => q"$baboonIdRepr.bytesToHex($dtFieldName)"
        case IdentifierFieldKind.NestedId(_)      => q"""'{$$$dtFieldName}'"""
      }
    }

    private def renderIdentifierToString(dto: Typedef.Dto, name: DtType): TextTree[DtValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString
      val header     = s"$simpleName:$versionStr#"

      val fieldExprs: List[TextTree[DtValue]] = dto.fields.map {
        f =>
          val srcFieldName = f.name.name
          val kind         = identifierFieldKind(f.tpe)
          val valueExpr    = renderIdentifierFieldValueExpr(srcFieldName, kind)
          q""""$srcFieldName:" + ($valueExpr)"""
      }

      val joinedFields =
        if (fieldExprs.isEmpty) q""""""""
        else fieldExprs.toSeq.join(""" + ":" + """)

      q"""@override
         |String toString() {
         |  return "$header" + $joinedFields;
         |}""".stripMargin
    }

    private def renderIdentifierCodecClass(dto: Typedef.Dto, name: DtType): TextTree[DtValue] = {
      val simpleName    = name.name
      val versionStr    = domain.version.toString
      val codecClassName = s"${name.name}Codec"

      val fieldDecoders: List[TextTree[DtValue]] = dto.fields.zipWithIndex.map {
        case (f, idx) =>
          val srcFieldName = f.name.name
          val rawVar       = s"${srcFieldName}_raw"
          val valVar       = s"${srcFieldName}_v"
          val resVar       = s"${srcFieldName}_r"
          val isLast       = idx == dto.fields.length - 1
          val kind         = identifierFieldKind(f.tpe)
          val tpe          = trans.asDtRef(f.tpe, domain, evo)

          val parseHead =
            q"""final ${srcFieldName}_fnr = $baboonIdRepr.parseFieldName(cursor, "$srcFieldName");
               |if (${srcFieldName}_fnr is $baboonLeft<String, void>) return $baboonLeft(${srcFieldName}_fnr.value);""".stripMargin

          val parseValue: TextTree[DtValue] = kind match {
            case IdentifierFieldKind.Bit =>
              q"""final $rawVar = cursor.readUntilStructural();
                 |final $resVar = $baboonIdRepr.parseBit($rawVar);
                 |if ($resVar is $baboonLeft<String, bool>) return $baboonLeft($resVar.value);
                 |final bool $valVar = ($resVar as $baboonRight<String, bool>).value;""".stripMargin
            case IdentifierFieldKind.SignedInt =>
              val typeName   = signedTypeName(f.tpe)
              val parsedVar  = s"${srcFieldName}_n"
              val rangeCheck = signedRangeCheck(f.tpe, parsedVar)
              q"""final $rawVar = cursor.readUntilStructural();
                 |// Spec §5.4: signed integers must not carry a leading '+'.
                 |if ($rawVar.isNotEmpty && $rawVar[0] == '+') {
                 |  return $baboonLeft("signed integer must not have leading '+' for field $srcFieldName: " + $rawVar);
                 |}
                 |final $parsedVar = int.tryParse($rawVar);
                 |if ($parsedVar == null) {
                 |  return $baboonLeft("could not parse signed integer for field $srcFieldName: " + $rawVar);
                 |}
                 |if (!($rangeCheck)) {
                 |  return $baboonLeft("$typeName out of range for field $srcFieldName: " + $rawVar);
                 |}
                 |final $tpe $valVar = $parsedVar;""".stripMargin
            case IdentifierFieldKind.SignedLong =>
              // i64 in Dart = `int` (signed 64-bit on native). Range check is
              // always true so no dead block emitted (PR-57a-D01 carryover).
              q"""final $rawVar = cursor.readUntilStructural();
                 |// Spec §5.4: signed integers must not carry a leading '+'.
                 |if ($rawVar.isNotEmpty && $rawVar[0] == '+') {
                 |  return $baboonLeft("signed integer must not have leading '+' for field $srcFieldName: " + $rawVar);
                 |}
                 |final $valVar = int.tryParse($rawVar);
                 |if ($valVar == null) {
                 |  return $baboonLeft("could not parse i64 for field $srcFieldName: " + $rawVar);
                 |}""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              val typeName   = unsignedSmallTypeName(f.tpe)
              val parsedVar  = s"${srcFieldName}_n"
              val rangeCheck = unsignedSmallRangeCheck(f.tpe, parsedVar)
              q"""final $rawVar = cursor.readUntilStructural();
                 |if ($rawVar.isNotEmpty && ($rawVar[0] == '+' || $rawVar[0] == '-')) {
                 |  return $baboonLeft("unsigned value has leading sign for field $srcFieldName: " + $rawVar);
                 |}
                 |final $parsedVar = int.tryParse($rawVar);
                 |if ($parsedVar == null) {
                 |  return $baboonLeft("could not parse unsigned integer for field $srcFieldName: " + $rawVar);
                 |}
                 |if (!($rangeCheck)) {
                 |  return $baboonLeft("$typeName out of range for field $srcFieldName: " + $rawVar);
                 |}
                 |final $tpe $valVar = $parsedVar;""".stripMargin
            case IdentifierFieldKind.UnsignedLong =>
              // Dart `int` is signed-64 native; values >= 2^63 wrap to negative.
              // We parse via BigInt for the full unsigned range, then convert
              // back via toSigned(64) so the runtime int matches u64-as-int.
              val parsedVar = s"${srcFieldName}_big"
              q"""final $rawVar = cursor.readUntilStructural();
                 |if ($rawVar.isNotEmpty && ($rawVar[0] == '+' || $rawVar[0] == '-')) {
                 |  return $baboonLeft("unsigned value has leading sign for field $srcFieldName: " + $rawVar);
                 |}
                 |final $parsedVar = BigInt.tryParse($rawVar);
                 |if ($parsedVar == null || $parsedVar.isNegative || $parsedVar > BigInt.parse('18446744073709551615')) {
                 |  return $baboonLeft("could not parse u64 for field $srcFieldName: " + $rawVar);
                 |}
                 |final int $valVar = $parsedVar.toSigned(64).toInt();""".stripMargin
            case IdentifierFieldKind.Str =>
              q"""final $resVar = cursor.readStrField();
                 |if ($resVar is $baboonLeft<String, String>) return $baboonLeft($resVar.value);
                 |final String $valVar = ($resVar as $baboonRight<String, String>).value;""".stripMargin
            case IdentifierFieldKind.Uid =>
              q"""final $rawVar = cursor.readUntilStructural();
                 |if (!$baboonIdRepr.isCanonicalUid($rawVar)) {
                 |  return $baboonLeft("uid not in canonical lowercase form for field $srcFieldName: " + $rawVar);
                 |}
                 |final String $valVar = $rawVar;""".stripMargin
            case IdentifierFieldKind.Tsu =>
              q"""final ${srcFieldName}_rrf = cursor.readFixed(24);
                 |if (${srcFieldName}_rrf is $baboonLeft<String, String>) return $baboonLeft(${srcFieldName}_rrf.value);
                 |final $rawVar = (${srcFieldName}_rrf as $baboonRight<String, String>).value;
                 |final $resVar = $baboonIdRepr.parseTsuRepr($rawVar);
                 |if ($resVar is $baboonLeft<String, DateTime>) return $baboonLeft($resVar.value);
                 |final $tpe $valVar = ($resVar as $baboonRight<String, DateTime>).value;""".stripMargin
            case IdentifierFieldKind.Tso =>
              q"""final ${srcFieldName}_rrf = cursor.readFixed(29);
                 |if (${srcFieldName}_rrf is $baboonLeft<String, String>) return $baboonLeft(${srcFieldName}_rrf.value);
                 |final $rawVar = (${srcFieldName}_rrf as $baboonRight<String, String>).value;
                 |final $resVar = $baboonIdRepr.parseTsoRepr($rawVar);
                 |if ($resVar is $baboonLeft<String, $baboonDateTimeOffset>) return $baboonLeft($resVar.value);
                 |final $tpe $valVar = ($resVar as $baboonRight<String, $baboonDateTimeOffset>).value;""".stripMargin
            case IdentifierFieldKind.Bytes =>
              q"""final $rawVar = cursor.readUntilStructural();
                 |final $resVar = $baboonIdRepr.parseBytesHex($rawVar);
                 |if ($resVar is $baboonLeft<String, $dtUint8List>) return $baboonLeft($resVar.value);
                 |final $tpe $valVar = ($resVar as $baboonRight<String, $dtUint8List>).value;""".stripMargin
            case IdentifierFieldKind.NestedId(uid) =>
              val nestedTpe = trans.toDtTypeRefKeepForeigns(uid, domain, evo)
              // The codec lives in the SAME file as the nested type (not a sibling
              // file). importAs forces the resolver to use the type's file name.
              val nestedCodec = DtType(nestedTpe.pkg, s"${nestedTpe.name}Codec",
                                        importAs = Some(trans.toSnakeCase(nestedTpe.name)))
              q"""final ${srcFieldName}_ro = cursor.expect("{");
                 |if (${srcFieldName}_ro is $baboonLeft<String, void>) return $baboonLeft(${srcFieldName}_ro.value);
                 |final $resVar = $nestedCodec.parseReprCursor(cursor);
                 |if ($resVar is $baboonLeft<String, $nestedTpe>) return $baboonLeft($resVar.value);
                 |final $nestedTpe $valVar = ($resVar as $baboonRight<String, $nestedTpe>).value;
                 |final ${srcFieldName}_rc = cursor.expect("}");
                 |if (${srcFieldName}_rc is $baboonLeft<String, void>) return $baboonLeft(${srcFieldName}_rc.value);""".stripMargin
          }

          val sep =
            if (isLast) q""
            else
              q"""final ${srcFieldName}_rsep = cursor.expect(":");
                 |if (${srcFieldName}_rsep is $baboonLeft<String, void>) return $baboonLeft(${srcFieldName}_rsep.value);""".stripMargin

          q"""$parseHead
             |$parseValue
             |$sep""".stripMargin.trim
      }

      val ctorArgs = dto.fields.map {
        f =>
          q"${f.name.name}: ${f.name.name}_v"
      }

      val ctor =
        if (ctorArgs.nonEmpty)
          q"""${name.asName}(
             |  ${ctorArgs.join(",\n").shift(2).trim}
             |)""".stripMargin
        else q"${name.asName}()"

      val body = (fieldDecoders :+ q"return $baboonRight($ctor);").joinNN()

      q"""class $codecClassName {
         |  /// Parse the canonical identifier repr per docs/spec/identifier-repr.md.
         |  /// Schema-directed parser: walks declared field order and dispatches per
         |  /// field type. Returns [BaboonLeft] on any malformed input.
         |  static $baboonEither<String, ${name.asName}> parseRepr(String s) {
         |    final cursor = $baboonIdReprCursor(s);
         |    final inner = parseReprCursor(cursor);
         |    if (inner is $baboonLeft<String, ${name.asName}>) return inner;
         |    if (!cursor.atEnd) {
         |      return $baboonLeft("unexpected trailing input at " + cursor.position.toString());
         |    }
         |    return inner;
         |  }
         |
         |  static $baboonEither<String, ${name.asName}> parseReprCursor($baboonIdReprCursor cursor) {
         |    final h = $baboonIdRepr.parseHeader(cursor, "$simpleName", "$versionStr");
         |    if (h is $baboonLeft<String, void>) return $baboonLeft(h.value);
         |    ${body.shift(4).trim}
         |  }
         |}""".stripMargin
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
      val fbase = dtFiles.basename(domain, evo)
      val fname = s"${trans.toSnakeCase(defn.id.name.name)}${suffix.getOrElse("")}.dart"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${trans.toSnakeCase(id.name.name)}/$fname"
      }
    }

    private def getOutputModule(defn: DomainMember.User): DtValue.DtPackageId = {
      val basePkg = trans.toDtPkg(domain.id, domain.version, evo)
      defn.defn.id.owner match {
        case Owner.Toplevel => basePkg
        case Owner.Ns(path) => DtValue.DtPackageId(basePkg.parts ++ path.map(_.name.toLowerCase))
        case Owner.Adt(id)  => DtValue.DtPackageId(basePkg.parts :+ trans.toSnakeCase(id.name.name))
      }
    }
  }
}
