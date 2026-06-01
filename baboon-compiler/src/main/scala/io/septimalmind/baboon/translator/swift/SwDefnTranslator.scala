package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.ServiceResultResolver
import io.septimalmind.baboon.translator.swift.SwValue.SwType
import io.septimalmind.baboon.typer.EnumWireStyle
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait SwDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[SwDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[SwDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[SwDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[SwDefnTranslator.Output]]
}

object SwDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: SwType,
    tpeKeepForeigns: SwType,
    tpeId: TextTree[SwValue],
    trees: Map[String, TextTree[SwValue]],
  )

  final case class DefnRepr(
    defn: TextTree[SwValue],
    codecs: List[CodecReg],
  )

  final case class Output(
    path: String,
    tree: TextTree[SwValue],
    module: SwValue.SwPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[SwValue]])] = Nil,
    imports: Set[String]                              = Set.empty,
  )

  class SwDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: io.septimalmind.baboon.CompilerTarget.SwTarget,
    domain: Domain,
    evo: BaboonEvolution,
    swFiles: SwFileTools,
    swTrees: SwTreeTools,
    trans: SwTypeTranslator,
    codecs: Set[SwCodecTranslator],
    codecTests: SwCodecTestsTranslator,
    codecsFixture: SwCodecFixtureTranslator,
    wiringTranslator: SwServiceWiringTranslator,
    swDomainTreeTools: SwDomainTreeTools,
  ) extends SwDefnTranslator[F] {
    import SwTypes.*

    /** Prepend `///` doc lines before a tree when `docs` is non-empty.
      * Returns the tree unchanged when `docs` is empty.
      */
    private def prependDocs(docs: Docs, tree: TextTree[SwValue]): TextTree[SwValue] = {
      val block = swTrees.renderDocs(docs, "")
      if (block.isEmpty) tree else q"${block}$tree"
    }

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
        imports  = Set("BaboonRuntime"),
      )

      val wiringOutput = wiringTranslator.translate(defn).map {
        wiringTree =>
          Output(
            getOutputPath(defn, suffix = Some("_wiring")),
            wiringTree,
            getOutputModule(defn),
            CompilerProduct.Definition,
            imports = Set("BaboonRuntime"),
          )
      }.toList

      val clientOutput = wiringTranslator.translateClient(defn).map {
        clientTree =>
          Output(
            getOutputPath(defn, suffix = Some("_client")),
            clientTree,
            getOutputModule(defn),
            CompilerProduct.Definition,
            imports = Set("BaboonRuntime"),
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
      val fixtureTree = codecsFixture.translate(defn)
      val result = fixtureTree.map {
        tree =>
          Output(
            getOutputPath(defn, suffix = Some("_fixture")),
            tree,
            getOutputModule(defn),
            CompilerProduct.Fixture,
            imports = Set("BaboonRuntime"),
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
      val swTypeRef   = trans.asSwType(defn.id, domain, evo)
      val srcRef      = trans.toSwTypeRefKeepForeigns(defn.id, domain, evo)
      val testPath    = getOutputPath(defn, suffix = Some("_test"))
      val typePath    = getOutputPath(defn)
      val fixturePath = getOutputPath(defn, suffix = Some("_fixture"))
      val testTree    = codecTests.translate(defn, swTypeRef, srcRef, testPath, typePath, fixturePath)
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
          val pkg   = trans.toSwPkg(domain.id, domain.version, evo)
          val fbase = swFiles.basename(domain, evo)
          Output(
            s"$fbase/baboon_service_rt.swift",
            tree,
            pkg,
            CompilerProduct.Definition,
            imports = Set("BaboonRuntime"),
          )
      }.toList
      F.pure(result)
    }

    private def makeFullRepr(
      defn: DomainMember.User,
      inLib: Boolean,
    ): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      def deprecatePrevious(tree: TextTree[SwValue]): TextTree[SwValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""@available(*, deprecated, message: "Version ${domain.version.v.toString} is deprecated, migrate to ${evo.latest.v.toString}")
             |$tree""".stripMargin
        }
      }

      val swTypeRef = trans.asSwType(defn.id, domain, evo)
      val srcRef    = trans.toSwTypeRefKeepForeigns(defn.id, domain, evo)
      val reprName = defn.defn match {
        case _: Typedef.Foreign => srcRef
        case _                  => swTypeRef
      }

      val repr = makeRepr(defn, reprName, isLatestVersion)

      val codecTrees =
        codecs.toList
          .flatMap(t => t.translate(defn, swTypeRef, srcRef).toList)
          .map(deprecatePrevious)

      val defnRepr = deprecatePrevious(repr.defn)

      assert(defn.id.pkg == domain.id)

      val allDefs = (defnRepr +: codecTrees).joinNN()
      val content = if (inLib) swTrees.inLib(allDefs, defn.defn.id.owner) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => Nil
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap(
              codec =>
                if (codec.isActive(d.id))
                  List(codec.id -> q"{ () -> Any in ${codec.codecName(srcRef)}.instance }")
                else Nil
            )
          List(CodecReg(defn.id, swTypeRef, srcRef, q""""${defn.id.toString}"""", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      DefnRepr(content, allRegs)
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: SwType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = swDomainTreeTools.makeDataMeta(defn)
      val codecMeta = codecs.flatMap(_.codecMeta(defn, name).map(_.member))

      defn.defn match {
        case dto: Typedef.Dto =>
          val repr = renderDto(dto, name, genMarker, mainMeta, codecMeta)
          repr.copy(defn = prependDocs(defn.docs, repr.defn))

        case e: Typedef.Enum =>
          val repr = renderEnum(e, name, mainMeta, codecMeta)
          repr.copy(defn = prependDocs(defn.docs, repr.defn))

        case adt: Typedef.Adt =>
          renderAdt(defn, adt, name, genMarker, mainMeta, codecMeta)

        case contract: Typedef.Contract =>
          val repr = renderContract(contract, name, genMarker)
          repr.copy(defn = prependDocs(defn.docs, repr.defn))

        case _: Typedef.Service =>
          renderService(defn, name)

        case _: Typedef.Foreign =>
          renderForeign(defn, name)
      }
    }

    private def renderForeign(
      defn: DomainMember.User,
      name: SwType,
    ): DefnRepr = {
      val target    = trans.asSwRef(TypeRef.Scalar(defn.id), domain, evo)
      val typealias = q"public typealias ${name.asDeclName} = $target"
      val keyCodecBlock = defn.defn match {
        case f: Typedef.Foreign => makeForeignKeyCodecRepr(f, name)
        case _                  => q""
      }
      val combined =
        if (keyCodecBlock.isEmpty) typealias
        else
          q"""$typealias
             |
             |$keyCodecBlock""".stripMargin
      DefnRepr(combined, Nil)
    }

    /** PR-I.2 (M24 Phase 3.2) — emit a `<Foreign>_KeyCodec` extension hook for
      * every Custom-mapped Swift foreign declaration. The host application
      * registers an implementation at boot which the JSON codec then uses to
      * encode/decode map keys. For BaboonRef-mapped foreigns we emit nothing —
      * the existing recursion into the aliased type covers the codec needs.
      *
      * Stringy foreigns (`Swift.String` / `String`) get a default identity impl
      * so the common case works out of the box. Non-stringy foreigns get a
      * stub default that throws `BaboonCodecException.decoderFailure` with an
      * FQN-bearing diagnostic referring to the Host enum (PR-I.1b-D01 lesson).
      */
    private def makeForeignKeyCodecRepr(f: Typedef.Foreign, name: SwType): TextTree[SwValue] = {
      f.bindings.get(BaboonLang.Swift) match {
        case None                                                               => q""
        case Some(Typedef.ForeignEntry(_, _: Typedef.ForeignMapping.BaboonRef)) => q""
        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) =>
          val srcRef    = trans.toSwTypeRefKeepForeigns(f.id, domain, evo)
          val codecName = s"${srcRef.name}_KeyCodec"
          val hostName  = s"${srcRef.name}_KeyCodecHost"
          val hostFqn   = s"${srcRef.pkg.parts.mkString(".")}.$hostName"
          // Stringy allowlist (PR-I-D06 pattern guidance: only the language's allowlist; no dead alternatives).
          // Both the FQN form (`Swift.String`) and the bare identifier (`String`) are accepted because
          // Swift code idiomatically references the type either way.
          val isStringy = decl == "Swift.String" || decl == "String"
          val defaultImpl = if (isStringy) {
            q"""private struct DefaultImpl: $codecName {
               |  func encodeKey(_ value: ${name.asDeclName}) -> $swString { return value }
               |  func decodeKey(_ s: $swString) throws -> ${name.asDeclName} { return s }
               |}""".stripMargin
          } else {
            q"""private struct DefaultImpl: $codecName {
               |  func encodeKey(_ value: ${name.asDeclName}) -> $swString {
               |    fatalError(\"$hostFqn is not registered; call $hostFqn.register(impl) at app boot.\")
               |  }
               |  func decodeKey(_ s: $swString) throws -> ${name.asDeclName} {
               |    throw $baboonCodecException.decoderFailure(\"$hostFqn is not registered; call $hostFqn.register(impl) at app boot.\", nil)
               |  }
               |}""".stripMargin
          }
          q"""public protocol $codecName {
             |  func encodeKey(_ value: ${name.asDeclName}) -> $swString
             |  func decodeKey(_ s: $swString) throws -> ${name.asDeclName}
             |}
             |
             |public enum $hostName {
             |  nonisolated(unsafe) private static var _instance: $codecName = DefaultImpl()
             |  public static func register(_ impl: $codecName) { _instance = impl }
             |  public static var instance: $codecName { _instance }
             |
             |  ${defaultImpl.shift(2).trim}
             |}""".stripMargin
      }
    }

    private def renderDto(
      dto: Typedef.Dto,
      name: SwType,
      genMarker: SwType,
      mainMeta: List[SwDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[SwValue]],
    ): DefnRepr = {
      val hasFields = dto.fields.nonEmpty

      val fieldDeclarations = dto.fields.map {
        f =>
          val t       = trans.asSwRef(f.tpe, domain, evo)
          val escaped = trans.escapeSwiftKeyword(f.name.name)
          prependDocs(f.docs, q"public let $escaped: $t")
      }

      val contractParents = dto.contracts.map(c => trans.toSwTypeRefKeepForeigns(c, domain, evo))
      val adtMemberProto = dto.id.owner match {
        case Owner.Adt(_) =>
          Seq(iBaboonAdtMemberMeta)
        case _ => Seq.empty
      }

      // MFACADE-PR-E: append `BaboonMetaProvider` so user code can pass a generated DTO
      // directly through `BaboonCodecsFacade.encodeToBin(_, _)` (and round-trip via the
      // facade). Instance forwarders below satisfy the protocol's instance reqs by
      // delegating to the static metadata the codegen already emits.
      val allParents        = (adtMemberProto ++ contractParents :+ genMarker :+ iBaboonMetaProvider).distinct
      val conformanceClause = allParents.map(t => q"$t").join(", ")

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta
      // BaboonMetaProvider's instance reqs — emitted as forwarders to the static metadata.
      // ADT branches additionally satisfy `BaboonAdtMember` via baboonAdtTypeIdentifier;
      // include it when present.
      val providerFieldNames = Set(
        "baboonDomainVersion",
        "baboonDomainIdentifier",
        "baboonTypeIdentifier",
        "baboonSameInVersions",
        "baboonAdtTypeIdentifier",
      )
      val instanceForwarders = mainMeta.filter(m => providerFieldNames.contains(m.name)).map(_.instanceForwarder)

      val fieldsBlock = if (hasFields) {
        fieldDeclarations.joinN()
      } else q""

      val initDecl = if (hasFields) {
        val initParams = dto.fields.map {
          f =>
            val t       = trans.asSwRef(f.tpe, domain, evo)
            val escaped = trans.escapeSwiftKeyword(f.name.name)
            q"$escaped: $t"
        }
        val initBody = dto.fields.map {
          f =>
            val escaped = trans.escapeSwiftKeyword(f.name.name)
            q"self.$escaped = $escaped"
        }
        q"""public init(
           |    ${initParams.join(",\n").shift(4).trim}
           |) {
           |    ${initBody.joinN().shift(4).trim}
           |}""".stripMargin
      } else {
        q"public init() {}"
      }

      val structDef =
        q"""public struct ${name.asDeclName}: Equatable, Hashable, $conformanceClause {
           |  ${fieldsBlock.shift(4).trim}
           |
           |  ${initDecl.shift(4).trim}
           |
           |  ${staticMetaFields.joinN().shift(4).trim}
           |
           |  ${instanceForwarders.joinN().shift(4).trim}
           |}""".stripMargin

      // Identifier toString + parseRepr emission (PR-57c / spec:
      // docs/spec/identifier-repr.md). Emitted only when `dto.isIdentifier`.
      // The toString surface is `extension X: CustomStringConvertible`; the
      // parser lives on a sibling `XCodec` enum (Q-FU-4: keeps it discoverable
      // by code-path but not as the obvious `MyId.parse(s)` autocomplete entry).
      val identifierExt: TextTree[SwValue] =
        if (dto.isIdentifier) renderIdentifierDescription(dto, name) else q""
      val identifierCodec: TextTree[SwValue] =
        if (dto.isIdentifier) renderIdentifierCodecEnum(dto, name) else q""

      val combined =
        if (dto.isIdentifier)
          q"""$structDef
             |
             |$identifierExt
             |
             |$identifierCodec""".stripMargin
        else structDef

      DefnRepr(
        combined,
        Nil,
      )
    }

    // ----- Identifier toString + parseRepr emission (PR-57c) -----
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

    private def signedTypeName(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => "i08"
      case TypeRef.Scalar(TypeId.Builtins.i16) => "i16"
      case TypeRef.Scalar(TypeId.Builtins.i32) => "i32"
      case TypeRef.Scalar(TypeId.Builtins.i64) => "i64"
      case other                               => throw new IllegalStateException(s"signedTypeName on non-signed-int: $other")
    }

    private def signedRangeCheck(tpe: TypeRef, varName: String): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => s"$varName >= -128 && $varName <= 127"
      case TypeRef.Scalar(TypeId.Builtins.i16) => s"$varName >= -32768 && $varName <= 32767"
      case TypeRef.Scalar(TypeId.Builtins.i32) => s"$varName >= -2147483648 && $varName <= 2147483647"
      case TypeRef.Scalar(TypeId.Builtins.i64) => "true"
      case other                               => throw new IllegalStateException(s"signedRangeCheck on non-signed-int: $other")
    }

    private def signedNarrow(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => "Int8"
      case TypeRef.Scalar(TypeId.Builtins.i16) => "Int16"
      case TypeRef.Scalar(TypeId.Builtins.i32) => "Int32"
      case TypeRef.Scalar(TypeId.Builtins.i64) => "Int64"
      case other                               => throw new IllegalStateException(s"signedNarrow on non-signed-int: $other")
    }

    private def unsignedSmallTypeName(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => "u08"
      case TypeRef.Scalar(TypeId.Builtins.u16) => "u16"
      case TypeRef.Scalar(TypeId.Builtins.u32) => "u32"
      case other                               => throw new IllegalStateException(s"unsignedSmallTypeName on non-u08/u16/u32: $other")
    }

    private def unsignedSmallRangeCheck(tpe: TypeRef, varName: String): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => s"$varName <= 255"
      case TypeRef.Scalar(TypeId.Builtins.u16) => s"$varName <= 65535"
      case TypeRef.Scalar(TypeId.Builtins.u32) => s"$varName <= 4294967295"
      case other                               => throw new IllegalStateException(s"unsignedSmallRangeCheck on non-u08/u16/u32: $other")
    }

    private def unsignedSmallNarrow(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => "UInt8"
      case TypeRef.Scalar(TypeId.Builtins.u16) => "UInt16"
      case TypeRef.Scalar(TypeId.Builtins.u32) => "UInt32"
      case other                               => throw new IllegalStateException(s"unsignedSmallNarrow on non-u08/u16/u32: $other")
    }

    private def renderFieldValueExpr(swFieldName: String, kind: IdentifierFieldKind): TextTree[SwValue] = {
      kind match {
        case IdentifierFieldKind.Bit              => q"$baboonIdRepr.bitToString(self.$swFieldName)"
        case IdentifierFieldKind.SignedInt        => q"String(self.$swFieldName)"
        case IdentifierFieldKind.UnsignedSmallInt => q"String(self.$swFieldName)"
        case IdentifierFieldKind.UnsignedLong     => q"$baboonIdRepr.u64ToString(self.$swFieldName)"
        case IdentifierFieldKind.Str              => q"$baboonIdRepr.escapeStr(self.$swFieldName)"
        // UUID.uuidString in Foundation is uppercase; canonical lowercase per spec.
        case IdentifierFieldKind.Uid         => q"self.$swFieldName.uuidString.lowercased()"
        case IdentifierFieldKind.Tsu         => q"$baboonIdRepr.tsuToString(self.$swFieldName)"
        case IdentifierFieldKind.Tso         => q"$baboonIdRepr.tsoToString(self.$swFieldName)"
        case IdentifierFieldKind.Bytes       => q"$baboonIdRepr.bytesToHex(self.$swFieldName)"
        case IdentifierFieldKind.NestedId(_) => q""""{" + self.$swFieldName.description + "}""""
      }
    }

    private def renderIdentifierDescription(dto: Typedef.Dto, name: SwType): TextTree[SwValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString
      val header     = s"$simpleName:$versionStr#"

      val fieldExprs: List[TextTree[SwValue]] = dto.fields.map {
        f =>
          val srcFieldName = f.name.name
          val swFieldName  = trans.escapeSwiftKeyword(srcFieldName)
          val kind         = identifierFieldKind(f.tpe)
          val valueExpr    = renderFieldValueExpr(swFieldName, kind)
          q""""$srcFieldName:" + ($valueExpr)"""
      }

      val joinedFields =
        if (fieldExprs.isEmpty) q""""""""
        else fieldExprs.toSeq.join(""" + ":" + """)

      q"""extension ${name.asDeclName}: CustomStringConvertible {
         |  public var description: String {
         |    return "$header" + $joinedFields
         |  }
         |}""".stripMargin
    }

    private def renderIdentifierCodecEnum(dto: Typedef.Dto, name: SwType): TextTree[SwValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString
      val codecName  = s"${name.name}Codec"

      val fieldDecoders: List[TextTree[SwValue]] = dto.fields.zipWithIndex.map {
        case (f, idx) =>
          val srcFieldName = f.name.name
          val swFieldName  = trans.escapeSwiftKeyword(srcFieldName)
          val rawVar       = s"${swFieldName}_raw"
          val valVar       = s"${swFieldName}_v"
          val resVar       = s"${swFieldName}_r"
          val isLast       = idx == dto.fields.length - 1
          val kind         = identifierFieldKind(f.tpe)
          val tpe          = trans.asSwRef(f.tpe, domain, evo)

          val parseHead =
            q"""let ${swFieldName}_fnr = $baboonIdRepr.parseFieldName(cursor, "$srcFieldName")
               |if case .left(let l) = ${swFieldName}_fnr { return .left(l) }""".stripMargin

          val parseValue: TextTree[SwValue] = kind match {
            case IdentifierFieldKind.Bit =>
              q"""let $rawVar = cursor.readUntilStructural()
                 |let $resVar = $baboonIdRepr.parseBit($rawVar)
                 |if case .left(let l) = $resVar { return .left(l) }
                 |guard case .right(let $valVar) = $resVar else { return .left("internal: bit decode") }""".stripMargin
            case IdentifierFieldKind.SignedInt =>
              val typeName   = signedTypeName(f.tpe)
              val rangeCheck = signedRangeCheck(f.tpe, "v_long")
              val narrow     = signedNarrow(f.tpe)
              val rangeBlock =
                if (rangeCheck == "true") q""
                else
                  q"""if !($rangeCheck) {
                     |  return .left("$typeName out of range for field $srcFieldName: " + $rawVar)
                     |}""".stripMargin
              q"""let $rawVar = cursor.readUntilStructural()
                 |// Spec §5.4: signed integers must not carry a leading '+'.
                 |if !$rawVar.isEmpty && $rawVar.first == "+" {
                 |  return .left("signed integer must not have leading '+' for field $srcFieldName: " + $rawVar)
                 |}
                 |guard let v_long = Int64($rawVar) else {
                 |  return .left("could not parse signed integer for field $srcFieldName: " + $rawVar)
                 |}
                 |$rangeBlock
                 |let $valVar: $tpe = $narrow(v_long)""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              val typeName   = unsignedSmallTypeName(f.tpe)
              val rangeCheck = unsignedSmallRangeCheck(f.tpe, "v_ulong")
              val narrow     = unsignedSmallNarrow(f.tpe)
              q"""let $rawVar = cursor.readUntilStructural()
                 |if !$rawVar.isEmpty && ($rawVar.first == "+" || $rawVar.first == "-") {
                 |  return .left("unsigned value has leading sign for field $srcFieldName: " + $rawVar)
                 |}
                 |guard let v_ulong = UInt64($rawVar) else {
                 |  return .left("could not parse unsigned integer for field $srcFieldName: " + $rawVar)
                 |}
                 |if !($rangeCheck) {
                 |  return .left("$typeName out of range for field $srcFieldName: " + $rawVar)
                 |}
                 |let $valVar: $tpe = $narrow(v_ulong)""".stripMargin
            case IdentifierFieldKind.UnsignedLong =>
              q"""let $rawVar = cursor.readUntilStructural()
                 |if !$rawVar.isEmpty && ($rawVar.first == "+" || $rawVar.first == "-") {
                 |  return .left("unsigned value has leading sign for field $srcFieldName: " + $rawVar)
                 |}
                 |guard let $valVar = UInt64($rawVar) else {
                 |  return .left("could not parse u64 for field $srcFieldName: " + $rawVar)
                 |}""".stripMargin
            case IdentifierFieldKind.Str =>
              q"""let $resVar = cursor.readStrField()
                 |if case .left(let l) = $resVar { return .left(l) }
                 |guard case .right(let $valVar) = $resVar else { return .left("internal: str decode") }""".stripMargin
            case IdentifierFieldKind.Uid =>
              q"""let $rawVar = cursor.readUntilStructural()
                 |if !$baboonIdRepr.isCanonicalUid($rawVar) {
                 |  return .left("uid not in canonical lowercase form for field $srcFieldName: " + $rawVar)
                 |}
                 |guard let $valVar = UUID(uuidString: $rawVar) else {
                 |  return .left("could not parse uid for field $srcFieldName: " + $rawVar)
                 |}""".stripMargin
            case IdentifierFieldKind.Tsu =>
              q"""let ${swFieldName}_rrf = cursor.readFixed(24)
                 |if case .left(let l) = ${swFieldName}_rrf { return .left(l) }
                 |guard case .right(let $rawVar) = ${swFieldName}_rrf else { return .left("internal: tsu fixed") }
                 |let $resVar = $baboonIdRepr.parseTsuRepr($rawVar)
                 |if case .left(let l) = $resVar { return .left(l) }
                 |guard case .right(let $valVar) = $resVar else { return .left("internal: tsu decode") }""".stripMargin
            case IdentifierFieldKind.Tso =>
              q"""let ${swFieldName}_rrf = cursor.readFixed(29)
                 |if case .left(let l) = ${swFieldName}_rrf { return .left(l) }
                 |guard case .right(let $rawVar) = ${swFieldName}_rrf else { return .left("internal: tso fixed") }
                 |let $resVar = $baboonIdRepr.parseTsoRepr($rawVar)
                 |if case .left(let l) = $resVar { return .left(l) }
                 |guard case .right(let $valVar) = $resVar else { return .left("internal: tso decode") }""".stripMargin
            case IdentifierFieldKind.Bytes =>
              q"""let $rawVar = cursor.readUntilStructural()
                 |let $resVar = $baboonIdRepr.parseBytesHex($rawVar)
                 |if case .left(let l) = $resVar { return .left(l) }
                 |guard case .right(let $valVar) = $resVar else { return .left("internal: bytes decode") }""".stripMargin
            case IdentifierFieldKind.NestedId(uid) =>
              val nestedTpe   = trans.toSwTypeRefKeepForeigns(uid, domain, evo)
              val nestedCodec = SwType(nestedTpe.pkg, s"${nestedTpe.name}Codec")
              q"""let ${swFieldName}_ro = cursor.expect("{")
                 |if case .left(let l) = ${swFieldName}_ro { return .left(l) }
                 |let $resVar = $nestedCodec.parseReprCursor(cursor)
                 |if case .left(let l) = $resVar { return .left(l) }
                 |guard case .right(let $valVar) = $resVar else { return .left("internal: nested decode") }
                 |let ${swFieldName}_rc = cursor.expect("}")
                 |if case .left(let l) = ${swFieldName}_rc { return .left(l) }""".stripMargin
          }

          val sep =
            if (isLast) q""
            else
              q"""let ${swFieldName}_rsep = cursor.expect(":")
                 |if case .left(let l) = ${swFieldName}_rsep { return .left(l) }""".stripMargin

          q"""$parseHead
             |$parseValue
             |$sep""".stripMargin.trim
      }

      val ctorArgs = dto.fields.map {
        f =>
          val swFieldName = trans.escapeSwiftKeyword(f.name.name)
          q"$swFieldName: ${swFieldName}_v"
      }

      val ctor =
        if (ctorArgs.nonEmpty)
          q"""${name.asDeclName}(
             |  ${ctorArgs.join(",\n").shift(2).trim}
             |)""".stripMargin
        else q"${name.asDeclName}()"

      val body = (fieldDecoders :+ q"return .right($ctor)").joinNN()

      // Codec emitted as an enum with static methods (case-less; pure namespace).
      // `parseRepr(_:)` is the public entry point requiring fully-consumed input;
      // `parseReprCursor(_:)` is the partial-parse used by nested-id dispatch.
      q"""public enum $codecName {
         |  /// Parse the canonical identifier repr per docs/spec/identifier-repr.md.
         |  /// Schema-directed parser: walks declared field order and dispatches per
         |  /// field type. Returns `.left` on any malformed input.
         |  public static func parseRepr(_ s: String) -> $baboonEither<String, ${name.asDeclName}> {
         |    let cursor = $baboonIdReprCursor(s)
         |    let inner = parseReprCursor(cursor)
         |    if case .left = inner { return inner }
         |    if !cursor.atEnd() {
         |      return .left("unexpected trailing input at " + String(cursor.position()))
         |    }
         |    return inner
         |  }
         |
         |  public static func parseReprCursor(_ cursor: $baboonIdReprCursor) -> $baboonEither<String, ${name.asDeclName}> {
         |    let h = $baboonIdRepr.parseHeader(cursor, "$simpleName", "$versionStr")
         |    if case .left(let l) = h { return .left(l) }
         |    ${body.shift(4).trim}
         |  }
         |}""".stripMargin
    }

    private def renderEnum(
      e: Typedef.Enum,
      name: SwType,
      mainMeta: List[SwDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[SwValue]],
    ): DefnRepr = {
      val cases = e.members.map {
        m =>
          val pascal = EnumWireStyle.wireName(m.name)
          q"""case $pascal = "$pascal""""
      }.toList

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta
      // MFACADE-PR-E: instance forwarders to satisfy BaboonMetaProvider.
      val providerFieldNames = Set(
        "baboonDomainVersion",
        "baboonDomainIdentifier",
        "baboonTypeIdentifier",
        "baboonSameInVersions",
      )
      val instanceForwarders = mainMeta.filter(m => providerFieldNames.contains(m.name)).map(_.instanceForwarder)

      DefnRepr(
        q"""public enum ${name.asDeclName}: String, CaseIterable, $iBaboonGenerated, $iBaboonMetaProvider {
           |  ${cases.joinN().shift(4).trim}
           |
           |  ${staticMetaFields.joinN().shift(4).trim}
           |
           |  ${instanceForwarders.joinN().shift(4).trim}
           |
           |  public static func parse(_ s: String) -> ${name.asDeclName}? {
           |    return ${name.asDeclName}(rawValue: s)
           |  }
           |
           |  public static var all: [${name.asDeclName}] { return Array(Self.allCases) }
           |}""".stripMargin,
        Nil,
      )
    }

    private def renderAdt(
      defn: DomainMember.User,
      adt: Typedef.Adt,
      name: SwType,
      genMarker: SwType,
      mainMeta: List[SwDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[SwValue]],
    ): DefnRepr = {
      // MFACADE-PR-E: include BaboonMetaProvider so ADT-typed values can flow through the
      // facade's `encodeToBin(_, _)`. Default extension forwards to static metadata.
      val parents           = Seq(genMarker, iBaboonMetaProvider)
      val conformanceClause = parents.map(t => q"$t").join(", ")

      val dataMembers = adt.members.toList.filter {
        mid =>
          domain.defs.meta.nodes(mid) match {
            case DomainMember.User(_, _: Typedef.NonDataTypedef, _, _) => false
            case _                                                     => true
          }
      }

      val memberTrees = adt.members.map {
        mid =>
          domain.defs.meta.nodes(mid) match {
            case mdefn: DomainMember.User => makeFullRepr(mdefn, inLib = false)
            case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
          }
      }

      val memberDtos = memberTrees.map(_.defn).toList.joinNN()

      val enumCases = dataMembers.map {
        mid =>
          val memberName = mid.name.name
          val caseName   = memberName.head.toLower.toString + memberName.tail
          val memberRef  = trans.toSwTypeRefKeepForeigns(mid, domain, evo)
          q"case $caseName(${memberRef.asDeclName})"
      }.toList

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta
      // MFACADE-PR-E: instance forwarders to satisfy BaboonMetaProvider on the ADT enum.
      val providerFieldNames = Set(
        "baboonDomainVersion",
        "baboonDomainIdentifier",
        "baboonTypeIdentifier",
        "baboonSameInVersions",
      )
      val instanceForwarders = mainMeta.filter(m => providerFieldNames.contains(m.name)).map(_.instanceForwarder)

      val adtEnum =
        q"""public indirect enum ${name.asDeclName}: Equatable, Hashable, $conformanceClause {
           |  ${memberDtos.shift(4).trim}
           |
           |  ${enumCases.joinN().shift(4).trim}
           |
           |  ${staticMetaFields.joinN().shift(4).trim}
           |
           |  ${instanceForwarders.joinN().shift(4).trim}
           |}""".stripMargin

      DefnRepr(
        prependDocs(defn.docs, adtEnum),
        memberTrees.toList.flatMap(_.codecs),
      )
    }

    private def renderContract(
      contract: Typedef.Contract,
      name: SwType,
      genMarker: SwType,
    ): DefnRepr = {
      val methods = contract.fields.map {
        f =>
          val t       = trans.asSwRef(f.tpe, domain, evo)
          val escaped = trans.escapeSwiftKeyword(f.name.name)
          prependDocs(f.docs, q"var $escaped: $t { get }")
      }
      val contractParents   = contract.contracts.map(c => trans.toSwTypeRefKeepForeigns(c, domain, evo))
      val parents           = (contractParents :+ genMarker).distinct
      val conformanceClause = parents.map(t => q"$t").join(", ")
      val body              = if (methods.nonEmpty) methods.joinN() else q""

      DefnRepr(
        q"""public protocol ${name.asDeclName}: $conformanceClause {
           |  ${body.shift(4).trim}
           |}""".stripMargin,
        Nil,
      )
    }

    private def renderService(
      defn: DomainMember.User,
      name: SwType,
    ): DefnRepr = {
      val service = defn.defn.asInstanceOf[Typedef.Service]
      // Async axis: when enabled, service protocol methods become
      // `func ... async throws -> T`, matching the async invoke dispatchers /
      // client in SwServiceWiringTranslator. When disabled the signature is
      // emitted exactly as before (no `async`, no `throws`).
      val effectsKw = if (target.language.asyncServices) " async throws" else ""

      val resolved = ServiceResultResolver.resolve(domain, "swift", target.language.serviceResult, target.language.pragmas)

      // Render a Swift type ref to its keyword-escaped, fully-qualified string,
      // matching SwBaboonTranslator.renderTree (each dotted path segment escaped
      // independently). Used to build the result-container return type in errors
      // mode; the container syntax itself is plain text emitted via SwTypeName so
      // the no-codec backends never see it as a real symbol.
      val swFqName: SwValue => String = {
        case t: SwType if t.fq =>
          val module = t.pkg.parts.head
          val escaped = t.name.split('.').map(trans.escapeSwiftKeyword).mkString(".")
          s"$module.$escaped"
        case t: SwType        => t.name.split('.').map(trans.escapeSwiftKeyword).mkString(".")
        case n: SwValue.SwTypeName => trans.escapeSwiftKeyword(n.name)
      }

      val methods = service.methods.map {
        m =>
          val in   = trans.asSwRef(m.sig, domain, evo)
          val out  = m.out.map(trans.asSwRef(_, domain, evo))
          val err  = m.err.map(trans.asSwRef(_, domain, evo))

          // Bare-`out` cases (noErrors mode, or an err-free method) keep the
          // original `out` SwType subtree verbatim so the render codec still
          // collects its module for imports and the output is byte-identical to
          // the prior emission. Only an err-carrying method in errors mode
          // returns the result CONTAINER (e.g. BaboonEither<Err, Out>); the
          // wiring's rt.leftMap relies on the impl returning that container.
          val emitContainer = !resolved.noErrors && m.err.isDefined && out.isDefined
          val retStr: TextTree[SwValue] =
            if (emitContainer) {
              val outStr     = out.map(_.mapRender(swFqName)).getOrElse("")
              val errStr     = err.map(_.mapRender(swFqName))
              val retTypeStr = resolved.renderReturnType(outStr, errStr, "Void")
              q" -> ${SwValue.SwTypeName(retTypeStr)}"
            } else {
              out.map(o => q" -> $o").getOrElse(q"")
            }
          val methodEx = q"func ${m.name.name}(arg: $in)$effectsKw$retStr"
          prependDocs(m.docs, methodEx)
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""

      val serviceTree =
        q"""public protocol ${name.asDeclName} {
           |  ${body.shift(4).trim}
           |}""".stripMargin

      DefnRepr(prependDocs(defn.docs, serviceTree), Nil)
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase    = swFiles.basename(domain, evo)
      val typeRef  = trans.toSwTypeRefKeepForeigns(defn.id, domain, evo)
      val flatName = typeRef.name.replace('.', '_')
      val fname    = s"${trans.toSnakeCase(flatName)}${suffix.getOrElse("")}.swift"
      s"$fbase/$fname"
    }

    private def getOutputModule(defn: DomainMember.User): SwValue.SwPackageId = {
      trans.effectiveSwPkg(defn.defn.id.owner, domain, evo)
    }
  }
}
