package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.CompilerTarget.SwTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.swift.SwCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.swift.SwDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class SwJsonCodecGenerator(
  trans: SwTypeTranslator,
  target: SwTarget,
  domain: Domain,
  evo: BaboonEvolution,
  swDomainTreeTools: SwDomainTreeTools,
) extends SwCodecTranslator {

  override def translate(defn: DomainMember.User, swRef: SwValue.SwType, srcRef: SwValue.SwType): Option[TextTree[SwValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto      => Some(genDtoBodies(swRef, d))
        case _: Typedef.Enum     => Some(genEnumBodies(swRef))
        case a: Typedef.Adt      => Some(genAdtBodies(swRef, a))
        case _: Typedef.Foreign  => Some(genForeignBodies(swRef))
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          genCodec(defn, swRef, srcRef, enc, dec)
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: SwValue.SwType,
    srcRef: SwValue.SwType,
    enc: TextTree[SwValue],
    dec: TextTree[SwValue],
  ): TextTree[SwValue] = {
    val isEncoderEnabled = domain.version == evo.latest
    val encReturnType    = "Any"
    val localName        = name.asDeclName
    val encodeMethod =
      if (isEncoderEnabled) {
        List(
          q"""public override func encode(_ ctx: $baboonCodecContext, _ value: $localName) -> $encReturnType {
             |    ${enc.shift(4).trim}
             |}
             |""".stripMargin.trim
        )
      } else Nil
    val decodeMethod =
      List(
        q"""public override func decode(_ ctx: $baboonCodecContext, _ wire: Any) throws -> $localName {
           |    ${dec.shift(4).trim}
           |}""".stripMargin.trim
      )

    val anyHelpers: List[TextTree[SwValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil
    val baseMethods                         = encodeMethod ++ decodeMethod ++ anyHelpers
    val cName                               = codecName(srcRef)
    val meta        = renderMeta(defn, swDomainTreeTools.makeCodecMeta(defn))

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecBase<$localName>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecBase<$localName>"
        case _ if defn.isAdt                                => q"$baboonJsonCodecBaseGeneratedAdt<$localName>"
        case _                                              => q"$baboonJsonCodecBaseGenerated<$localName>"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecNoEncoder<$localName>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecNoEncoder<$localName>"
        case _ if defn.isAdt                                => q"$baboonJsonCodecNoEncoderGeneratedAdt<$localName>"
        case _                                              => q"$baboonJsonCodecNoEncoderGenerated<$localName>"
      }
    }

    q"""public class ${cName.asDeclName}: $cParent {
       |    public static let instance = ${cName.asDeclName}()
       |    private override init() { super.init() }
       |
       |    ${baseMethods.joinNN().shift(4).trim}
       |
       |    ${meta.joinN().shift(4).trim}
       |}
       |""".stripMargin
  }

  private def genForeignBodies(name: SwValue.SwType): (TextTree[SwValue], TextTree[SwValue]) = {
    (
      q"""fatalError("${name.name} is a foreign type")""",
      q"""fatalError("${name.name} is a foreign type")""",
    )
  }

  private def wrapAdtBranchEncoder(
    branchName: String,
    tree: TextTree[SwValue],
  ): TextTree[SwValue] = {
    q"""["$branchName": $tree as Any]"""
  }

  private def genAdtBodies(name: SwValue.SwType, adt: Typedef.Adt): (TextTree[SwValue], TextTree[SwValue]) = {
    val branches = adt.dataMembers(domain).map {
      m =>
        val branchName = m.name.name
        val caseName   = branchName.head.toLower.toString + branchName.tail
        val fqBranch   = trans.toSwTypeRefKeepForeigns(m, domain, evo)

        val routedBranchEncoder = q"${codecName(fqBranch)}.instance.encode(ctx, branchVal)"
        val branchEncoder = if (target.language.wrappedAdtBranchCodecs) {
          routedBranchEncoder
        } else {
          wrapAdtBranchEncoder(branchName, routedBranchEncoder)
        }

        (
          q"""case .$caseName(let branchVal):
             |    return $branchEncoder""".stripMargin,
          q"""case "$branchName":
             |    return .$caseName(try ${codecName(fqBranch)}.instance.decode(ctx, entryValue))""".stripMargin,
        )
    }

    (
      q"""switch value {
         |${branches.map(_._1).joinN().shift(0).trim}
         |}""".stripMargin,
      q"""guard let jsonObj = wire as? [String: Any] else {
         |    throw BaboonCodecError.invalidInput("Expected dictionary for ${name.name}")
         |}
         |guard let entry = jsonObj.first else {
         |    throw BaboonCodecError.invalidInput("Cannot decode to ${name.name}: empty json object")
         |}
         |let entryValue = entry.value
         |switch entry.key {
         |${branches.map(_._2).joinN().shift(0).trim}
         |default:
         |    throw BaboonCodecError.invalidInput("Cannot decode to ${name.name}: unknown key \\(entry.key)")
         |}""".stripMargin,
    )
  }

  private def genEnumBodies(name: SwValue.SwType): (TextTree[SwValue], TextTree[SwValue]) = {
    (
      q"""return value.rawValue""",
      q"""guard let str = wire as? String else {
         |    throw BaboonCodecError.invalidInput("Expected string for ${name.name}")
         |}
         |guard let parsed = ${name.asDeclName}.parse(str.trimmingCharacters(in: .whitespaces)) else {
         |    throw BaboonCodecError.invalidInput("Cannot decode to ${name.name}: no matching value for \\(str)")
         |}
         |return parsed""".stripMargin,
    )
  }

  private def genDtoBodies(name: SwValue.SwType, d: Typedef.Dto): (TextTree[SwValue], TextTree[SwValue]) = {
    val encFields = d.fields.map {
      f =>
        val escaped  = trans.escapeSwiftKeyword(f.name.name)
        val fieldRef = q"value.$escaped"
        val enc      = mkEncoder(f.tpe, fieldRef)
        q""""${f.name.name}": $enc as Any,"""
    }

    val decFields = d.fields.map {
      f =>
        val escaped = trans.escapeSwiftKeyword(f.name.name)
        q"$escaped: try ${mkDecoder(f.name.name, f.tpe, q"jsonObj")}"
    }

    val encodedMap = if (encFields.nonEmpty) {
      q"""[
         |    ${encFields.joinN().shift(4).trim}
         |]""".stripMargin
    } else {
      q"[String: Any]()"
    }

    val mainEnc = q"return $encodedMap"

    val encBody = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val innerMap = encodedMap
        q"""return ${wrapAdtBranchEncoder(d.id.name.name, innerMap)}"""
      case _ => mainEnc
    }

    val decBody = if (d.fields.nonEmpty) {
      q"""guard let jsonObj = wire as? [String: Any] else {
         |    throw BaboonCodecError.invalidInput("Expected dictionary for ${name.name}")
         |}
         |return ${name.asDeclName}(
         |    ${decFields.join(",\n").shift(4).trim}
         |)""".stripMargin
    } else {
      q"""return ${name.asDeclName}()"""
    }

    (encBody, decBody)
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[SwValue], depth: Int = 0): TextTree[SwValue] = {
    def encodeKey(tpe: TypeRef, ref: TextTree[SwValue]): TextTree[SwValue] = {
      tpe.id match {
        case TypeId.Builtins.tsu   => q"$baboonTimeFormats.formatUtc($ref)"
        case TypeId.Builtins.tso   => q"$baboonTimeFormats.formatOffset($ref)"
        case TypeId.Builtins.uid   => q"$ref.uuidString"
        case TypeId.Builtins.f128  => q"$ref.stringValue"
        case TypeId.Builtins.bytes => q"$baboonByteStringTools.toHexString($ref)"
        case _: TypeId.Builtin     => q"String(describing: $ref)"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum    => q"$ref.rawValue"
                case _: Typedef.Foreign => q"String(describing: $ref)"
                case o                  => throw new RuntimeException(s"BUG: Unexpected key usertype: $o")
              }
            case o => throw new RuntimeException(s"BUG: Type/usertype mismatch: $o")
          }
        case o => throw new RuntimeException(s"BUG: Unexpected key type: $o")
      }
    }

    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.bit                                             => q"$ref"
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 => q"Int($ref)"
          case TypeId.Builtins.i64                                             => q"String($ref)"
          case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 => q"Int($ref)"
          case TypeId.Builtins.u64                                             => q"String($ref)"
          case TypeId.Builtins.f32                                             => q"Double($ref)"
          case TypeId.Builtins.f64                                             => q"$ref"
          case TypeId.Builtins.f128                                            => q"$ref.stringValue"
          case TypeId.Builtins.str                                             => q"$ref"
          case TypeId.Builtins.uid                                             => q"$ref.uuidString"
          case TypeId.Builtins.bytes                                           => q"$baboonByteStringTools.toHexString($ref)"
          case TypeId.Builtins.tsu                                             => q"$baboonTimeFormats.formatUtc($ref)"
          case TypeId.Builtins.tso                                             => q"$baboonTimeFormats.formatOffset($ref)"
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toSwTypeRefKeepForeigns(u, domain, evo))
            q"$targetTpe.instance.encode(ctx, $ref)"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""$ref == nil ? NSNull() : ${mkEncoder(c.args.head, q"$ref!", depth + 1)}"""
          case TypeId.Builtins.map =>
            val varName  = s"e$depth"
            val keyEnc   = encodeKey(c.args.head, q"$varName.key")
            val valueEnc = mkEncoder(c.args.last, q"$varName.value", depth + 1)
            q"""Dictionary(uniqueKeysWithValues: $ref.map { $varName in ($keyEnc, $valueEnc as Any) })"""
          case TypeId.Builtins.lst =>
            val varName = s"e$depth"
            q"""$ref.map { $varName in ${mkEncoder(c.args.head, q"$varName", depth + 1)} as Any }"""
          case TypeId.Builtins.set =>
            val varName = s"e$depth"
            q"""Array($ref).map { $varName in ${mkEncoder(c.args.head, q"$varName", depth + 1)} as Any }"""
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref)
    }
  }

  private def mkDecoder(fieldName: String, tpe: TypeRef, jsonObjRef: TextTree[SwValue]): TextTree[SwValue] = {
    def decodeElement(tpe: TypeRef, ref: TextTree[SwValue], depth: Int): TextTree[SwValue] = {
      val varName = s"e$depth"
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case TypeId.Builtins.bit   => q"$ref as! Bool"
            case TypeId.Builtins.i08   => q"Int8(truncatingIfNeeded: ($ref as! NSNumber).intValue)"
            case TypeId.Builtins.i16   => q"Int16(truncatingIfNeeded: ($ref as! NSNumber).intValue)"
            case TypeId.Builtins.i32   => q"Int32(truncatingIfNeeded: ($ref as! NSNumber).intValue)"
            case TypeId.Builtins.i64   => q"($ref is String ? Int64($ref as! String)! : Int64(truncatingIfNeeded: ($ref as! NSNumber).int64Value))"
            case TypeId.Builtins.u08   => q"UInt8(truncatingIfNeeded: ($ref as! NSNumber).intValue)"
            case TypeId.Builtins.u16   => q"UInt16(truncatingIfNeeded: ($ref as! NSNumber).intValue)"
            case TypeId.Builtins.u32   => q"UInt32(truncatingIfNeeded: ($ref as! NSNumber).intValue)"
            case TypeId.Builtins.u64   => q"($ref is String ? UInt64($ref as! String)! : UInt64(truncatingIfNeeded: ($ref as! NSNumber).uint64Value))"
            case TypeId.Builtins.f32   => q"Float(($ref as! NSNumber).doubleValue)"
            case TypeId.Builtins.f64   => q"($ref as! NSNumber).doubleValue"
            case TypeId.Builtins.f128  => q"$baboonDecimal($ref is String ? $ref as! String : String(describing: $ref))"
            case TypeId.Builtins.str   => q"$ref as! String"
            case TypeId.Builtins.uid   => q"UUID(uuidString: $ref as! String)!"
            case TypeId.Builtins.bytes => q"$baboonByteStringTools.fromHexString($ref as! String)"
            case TypeId.Builtins.tsu   => q"$baboonTimeFormats.parseUtc($ref as! String)"
            case TypeId.Builtins.tso   => q"$baboonTimeFormats.parseOffset($ref as! String)"
            case u: TypeId.User =>
              val targetTpe = codecName(trans.toSwTypeRefKeepForeigns(u, domain, evo))
              q"try $targetTpe.instance.decode(ctx, $ref)"
            case o =>
              throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
        case c: TypeRef.Constructor =>
          c.id match {
            case TypeId.Builtins.opt =>
              q"""$ref is NSNull || $ref == nil ? nil : ${decodeElement(c.args.head, ref, depth + 1)}"""
            case TypeId.Builtins.lst =>
              val elemDec = decodeElement(c.args.head, q"$varName", depth + 1)
              q"""try ($ref as! [Any]).map { $varName in $elemDec }"""
            case TypeId.Builtins.set =>
              val elemDec = decodeElement(c.args.head, q"$varName", depth + 1)
              q"""Set(try ($ref as! [Any]).map { $varName in $elemDec })"""
            case TypeId.Builtins.map =>
              val keyDec   = decodeKey(c.args.head, q"$varName.key")
              val valueDec = decodeElement(c.args.last, q"$varName.value", depth + 1)
              q"""Dictionary(uniqueKeysWithValues: try ($ref as! [String: Any]).map { $varName in ($keyDec, $valueDec) })"""
            case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
        case a: TypeRef.Any => mkAnyDecoder(a, ref)
      }
    }

    def decodeKey(tpe: TypeRef, ref: TextTree[SwValue]): TextTree[SwValue] = {
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case TypeId.Builtins.bit   => q"$ref == \"true\""
            case TypeId.Builtins.i08   => q"Int8($ref)!"
            case TypeId.Builtins.i16   => q"Int16($ref)!"
            case TypeId.Builtins.i32   => q"Int32($ref)!"
            case TypeId.Builtins.i64   => q"Int64($ref)!"
            case TypeId.Builtins.u08   => q"UInt8($ref)!"
            case TypeId.Builtins.u16   => q"UInt16($ref)!"
            case TypeId.Builtins.u32   => q"UInt32($ref)!"
            case TypeId.Builtins.u64   => q"UInt64($ref)!"
            case TypeId.Builtins.f32   => q"Float($ref)!"
            case TypeId.Builtins.f64   => q"Double($ref)!"
            case TypeId.Builtins.f128  => q"$baboonDecimal($ref)"
            case TypeId.Builtins.str   => q"$ref"
            case TypeId.Builtins.uid   => q"UUID(uuidString: $ref)!"
            case TypeId.Builtins.bytes => q"$baboonByteStringTools.fromHexString($ref)"
            case TypeId.Builtins.tsu   => q"$baboonTimeFormats.parseUtc($ref)"
            case TypeId.Builtins.tso   => q"$baboonTimeFormats.parseOffset($ref)"
            case u: TypeId.User =>
              domain.defs.meta.nodes(u) match {
                case ud: DomainMember.User =>
                  ud.defn match {
                    case _: Typedef.Enum =>
                      val targetTpe = trans.toSwTypeRefKeepForeigns(u, domain, evo)
                      q"$targetTpe.parse($ref)!"
                    case _: Typedef.Foreign =>
                      val targetTpe = codecName(trans.toSwTypeRefKeepForeigns(u, domain, evo))
                      q"try $targetTpe.instance.decode(ctx, $ref)"
                    case o => throw new RuntimeException(s"BUG: Unexpected key usertype: $o")
                  }
                case o => throw new RuntimeException(s"BUG: Type/usertype mismatch: $o")
              }
            case o => throw new RuntimeException(s"BUG: Unexpected key type: $o")
          }
        case _ => throw new RuntimeException(s"Collection cannot be key: $tpe")
      }
    }

    tpe match {
      case TypeRef.Constructor(id, args) if id.name.name == "opt" =>
        q"""{ let v = $jsonObjRef["$fieldName"]; return v is NSNull || v == nil ? nil : ${decodeElement(args.head, q"v!", 0)} }()"""
      case _ =>
        q"""${decodeElement(tpe, q"""$jsonObjRef["$fieldName"]!""", 0)}"""
    }
  }

  // Deep walk (mirrors Scala/C#/Rust/Kotlin/Java/TS/Dart `hasAnyField`): a codec class needs the
  // any-field helpers if any direct or nested-via-Constructor-arg field has type `any`.
  private def hasAnyField(defn: DomainMember.User): Boolean = {
    def hasAny(tpe: TypeRef): Boolean = tpe match {
      case _: TypeRef.Any         => true
      case _: TypeRef.Scalar      => false
      case c: TypeRef.Constructor => c.args.exists(hasAny)
    }
    defn.defn match {
      case d: Typedef.Dto => d.fields.exists(f => hasAny(f.tpe))
      case _              => false
    }
  }

  // Encode delegates to the per-codec-class `encodeAnyField` helper. This site wires the expected
  // kind byte and the field's static (codec-gen-time) fallbacks for cross-format meta resolution.
  // See `anyStaticFallbacks` for the per-variant table.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[SwValue]): TextTree[SwValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"encodeAnyField(ctx, $expectedHex, $staticDom, $staticVer, $staticTid, $ref)"
  }

  // Decode delegates to the per-codec-class `decodeAnyField` helper. JSON decode never cross-
  // converts (always returns `AnyOpaque.json` from JSON wire); user calls `facade.decodeAny(opaque)`
  // for typed resolution. No `ctx` / no static fallbacks needed at the decode site. The `try`
  // prefix is mandatory inside `.map`-style closure bodies (Swift does not propagate throws
  // implicitly through closures); at the top-level field site the redundant outer `try` from
  // `decFields` (`q"$escaped: try …"`) merely composes — Swift accepts redundant `try try`. This
  // matches the existing convention for `Typedef.User`'s `q"try $targetTpe.instance.decode(...)"`
  // emission in `decodeElement`.
  private def mkAnyDecoder(a: TypeRef.Any, ref: TextTree[SwValue]): TextTree[SwValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"try decodeAnyField($expectedHex, $ref)"
  }

  // Static fallbacks for the cross-format facade helper (`uebaToJson`). The wire `meta` may omit
  // components that are pinned by the field's static declaration; the codec emits whatever is
  // statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade.buildSyntheticTypeMeta` for the merge semantics. Per spec table:
  //   A=(nil,nil,nil), B=(currentDomain,nil,nil), C=(currentDomain,currentVersion,nil),
  //   D1=(nil,nil,underlyingFqid), D2=(currentDomain,nil,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // Duplicated across Scala/C#/Rust/Kotlin/Java/TS/Dart UEBA + JSON sites — extraction deferred
  // (textual emission diverges by language flavor; see PR 4.2 ledger entry's DRY analysis).
  // 12th instance.
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[SwValue], TextTree[SwValue], TextTree[SwValue]) = {
    val none                     = q"nil"
    def some(s: String)          = q""""$s""""
    val currentDomain: String    = domain.id.toString
    val currentDomainVer: String = domain.version.v.toString
    val typeidStatic = a.underlying match {
      case Some(u) => some(u.id.toString)
      case None    => none
    }
    val (domainStatic, versionStatic) = a.variant match {
      case AnyVariant.Global  => (none, none)
      case AnyVariant.ThisDom => (some(currentDomain), none)
      case AnyVariant.Current => (some(currentDomain), some(currentDomainVer))
    }
    (domainStatic, versionStatic, typeidStatic)
  }

  // Per-codec-class helpers consolidating the any-field JSON envelope encode/decode (kind check,
  // cross-format conversion via facade, envelope `$ak/$ad/$av/$at/$c` build & disassemble).
  // Emitted at most once per codec class that has any any-bearing fields. Mirrors PR 6.3's Java
  // helper shape and PR 8.3's Dart shape. Kind-check on encode runs before any envelope construction.
  //
  // Cast-via-`as?`: `ctx.facade` returns `BaboonCodecsFacadeBase?` (PR 9.1 import-cycle break).
  // The concrete facade carries `uebaToJson`; the cast is safe because
  // `BaboonCodecContext.withFacade` is the only construction path and accepts the same hierarchy.
  // Mirrors the symmetric cast used by `SwUEBACodecGenerator.anyFieldHelpers` (PR 9.2).
  //
  // The encoder helper is non-throwing: `encode(...)` on `BaboonJsonCodecBase` is non-throwing
  // (see `baboon_runtime.swift` line 100), so kind-mismatch / null-facade / facade-returned-failure
  // paths use `preconditionFailure`. Mirrors the symmetric Swift type-erasure pattern in
  // `AnyBaboonJsonEncoder.encodeAnyValue` and the UEBA helper. The `decodeAnyField` helper IS
  // `throws` since the base `decode(...)` is `throws` — uses typed
  // `BaboonCodecException.decoderFailure`.
  //
  // PR-08-D06 lesson: `AnyMetaCodec.writeJson` returns mutable `[String: Any]` already (Swift
  // dictionaries are value types), so adding the `$c` envelope key needs no cast (cf. Java where
  // an `(ObjectNode)` cast is required since the static type is `JsonNode`).
  private def anyFieldHelpers: TextTree[SwValue] = {
    q"""private func encodeAnyField(
       |    _ ctx: $baboonCodecContext,
       |    _ expectedKind: UInt8,
       |    _ staticDomain: String?,
       |    _ staticVersion: String?,
       |    _ staticTypeid: String?,
       |    _ value: $baboonAnyOpaque
       |) -> Any {
       |    if value.meta.kind != expectedKind {
       |        preconditionFailure(
       |            "any: meta-kind 0x\\(String(format: \"%02x\", value.meta.kind & 0xFF)) " +
       |            "does not match field-declared 0x\\(String(format: \"%02x\", expectedKind & 0xFF))"
       |        )
       |    }
       |    let anyInner: Any?
       |    switch value {
       |    case .json(_, let jsonValue):
       |        anyInner = jsonValue
       |    case .ueba(let uebaMeta, let uebaBytes):
       |        guard let anyFacadeBase = ctx.facade else {
       |            preconditionFailure(
       |                "Cannot encode AnyOpaque.ueba into JSON without a facade reference. " +
       |                "Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), " +
       |                "or supply AnyOpaque.json directly."
       |            )
       |        }
       |        // Downcast to the concrete facade — the marker base is empty by design (PR 9.1
       |        // import-cycle break). Construction goes through BaboonCodecContext.withFacade
       |        // which only accepts BaboonCodecsFacadeBase, but real callers pass BaboonCodecsFacade.
       |        guard let anyFacade = anyFacadeBase as? $baboonCodecsFacade else {
       |            preconditionFailure(
       |                "BaboonCodecContext.facade is not a BaboonCodecsFacade: " +
       |                "\\(type(of: anyFacadeBase))"
       |            )
       |        }
       |        let anyConvResult = anyFacade.uebaToJson(
       |            uebaMeta,
       |            uebaBytes,
       |            staticDomain: staticDomain,
       |            staticVersion: staticVersion,
       |            staticTypeid: staticTypeid
       |        )
       |        switch anyConvResult {
       |        case .failure(let err):
       |            preconditionFailure("any: uebaToJson failed: \\(err)")
       |        case .success(let json):
       |            anyInner = json
       |        }
       |    }
       |    var anyEnvelope = $baboonAnyMetaCodec.writeJson(value.meta)
       |    // Swift `[String: Any]` treats `dict[k] = nil` as "remove key" — use NSNull() so the
       |    // content envelope key is preserved when the inner JSON is null (PR-08-D06 analog:
       |    // keep wire-shape consistent with Java/Dart/TS regardless of payload-null state).
       |    anyEnvelope[$baboonAnyMetaCodec.ANY_CONTENT_KEY] = anyInner ?? NSNull()
       |    return anyEnvelope
       |}
       |
       |private func decodeAnyField(_ expectedKind: UInt8, _ wire: Any) throws -> $baboonAnyOpaque {
       |    guard let anyEnvelope = wire as? [String: Any] else {
       |        throw $baboonCodecException.decoderFailure(
       |            "any: JSON envelope must be an object",
       |            nil
       |        )
       |    }
       |    let anyMetaResult = $baboonAnyMetaCodec.readJson(anyEnvelope)
       |    let anyMeta: $baboonAnyMeta
       |    switch anyMetaResult {
       |    case .failure(let err):
       |        throw err
       |    case .success(let m):
       |        anyMeta = m
       |    }
       |    if anyMeta.kind != expectedKind {
       |        throw $baboonCodecException.decoderFailure(
       |            "any: wire kind 0x\\(String(format: \"%02x\", anyMeta.kind & 0xFF)) " +
       |            "does not match field-declared 0x\\(String(format: \"%02x\", expectedKind & 0xFF))",
       |            nil
       |        )
       |    }
       |    guard anyEnvelope.keys.contains($baboonAnyMetaCodec.ANY_CONTENT_KEY) else {
       |        throw $baboonCodecException.decoderFailure(
       |            "any: JSON envelope missing '\\($baboonAnyMetaCodec.ANY_CONTENT_KEY)' content key",
       |            nil
       |        )
       |    }
       |    // `keys.contains(...)` confirmed presence above; the value can still be NSNull() for an
       |    // explicit JSON null payload — pass through to the AnyOpaque.json variant. Swift
       |    // dictionary lookup returns `Any?` (None means absent — already excluded), so the
       |    // force-unwrap is safe inside the contains-guard.
       |    let anyContent = anyEnvelope[$baboonAnyMetaCodec.ANY_CONTENT_KEY]!
       |    return .json(meta: anyMeta, json: anyContent)
       |}""".stripMargin
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[SwValue]] = {
    defn.defn match {
      case _: Typedef.Enum | _: Typedef.Foreign => meta.map(_.valueField)
      case _                                    => meta.map(_.refValueField)
    }
  }

  def codecName(name: SwValue.SwType): SwValue.SwType = {
    val baseFileName = name.importAs.getOrElse(trans.toSnakeCase(name.name))
    val localName    = name.localName.getOrElse(name.name)
    SwValue.SwType(name.pkg, s"${name.name}_JsonCodec", name.fq, importAs = Some(baseFileName), localName = Some(s"${localName}_JsonCodec"))
  }

  override def codecMeta(defn: DomainMember.User, name: SwValue.SwType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"public static let codecJson: $baboonJsonCodec<${name.asDeclName}> = ${codecName(name).asDeclName}.instance"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Json"
}
