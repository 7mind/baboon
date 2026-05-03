package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.CompilerTarget.SwTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.swift.SwCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.swift.SwDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.translator.swift.SwValue.SwType
import io.septimalmind.baboon.typer.EnumWireStyle
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class SwUEBACodecGenerator(
  trans: SwTypeTranslator,
  target: SwTarget,
  domain: Domain,
  evo: BaboonEvolution,
  swDomainTreeTools: SwDomainTreeTools,
) extends SwCodecTranslator {

  override def translate(
    defn: DomainMember.User,
    swRef: SwType,
    srcRef: SwType,
  ): Option[TextTree[SwValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto      => Some(genDtoBodies(swRef, d))
        case e: Typedef.Enum     => Some(genEnumBodies(swRef, e))
        case a: Typedef.Adt      => Some(genAdtBodies(swRef, a))
        case _: Typedef.Foreign  => Some(genForeignBodies(swRef))
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          val branchDecoder = defn.defn match {
            case d: Typedef.Dto => genBranchDecoder(swRef, d)
            case _              => None
          }

          genCodec(
            defn,
            swRef,
            srcRef,
            enc,
            dec,
            branchDecoder,
          )
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: SwType,
    srcRef: SwType,
    enc: TextTree[SwValue],
    dec: TextTree[SwValue],
    branchDecoder: Option[TextTree[SwValue]],
  ): TextTree[SwValue] = {
    val isEncoderEnabled = domain.version == evo.latest
    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = d.fields.filter(f => domain.refMeta(f.tpe).len.isVariable)
        val comment = varlens.map(f => q"// ${f.toString}").joinN()
        q"""$comment
           |return ${varlens.size.toString}""".stripMargin

      case _: Typedef.Enum    => q"""return 0"""
      case _: Typedef.Adt     => q"""return 0"""
      case _: Typedef.Foreign => q"""fatalError("${name.name} is a foreign type")"""

      case d: Typedef.Contract =>
        throw new IllegalArgumentException(s"BUG: contract codec should not be rendered: $d")
      case d: Typedef.Service =>
        throw new IllegalArgumentException(s"BUG: service codec should not be rendered: $d")
    }

    val indexGetter =
      q"""public var indexElementsCount: Int {
         |    ${indexBody.shift(4).trim}
         |}""".stripMargin

    val localName = name.asDeclName
    val encoderMethods = if (isEncoderEnabled) {
      List(
        q"""public override func encode(_ ctx: $baboonCodecContext, _ writer: $baboonBinWriter, _ value: $localName) {
           |    ${enc.shift(4).trim}
           |}
           |""".stripMargin
      )
    } else Nil

    val decoderMethods = List(
      q"""public override func decode(_ ctx: $baboonCodecContext, _ reader: $baboonBinReader) throws -> $localName {
         |    ${dec.shift(4).trim}
         |}""".stripMargin
    )

    val anyHelpers: List[TextTree[SwValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil

    val baseMethods = encoderMethods ++ decoderMethods
      ++ branchDecoder.map {
        body =>
          q"""func decodeBranch(_ ctx: $baboonCodecContext, _ reader: $baboonBinReader) throws -> $localName {
             |    ${body.shift(4).trim}
             |}""".stripMargin
      }.toList ++ List(indexGetter) ++ anyHelpers

    val cName = codecName(srcRef)

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => baboonBinCodecBase
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => baboonBinCodecBase
        case _ if defn.isAdt                                => baboonBinCodecBaseGeneratedAdt
        case _                                              => baboonBinCodecBaseGenerated
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => baboonBinCodecNoEncoder
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => baboonBinCodecNoEncoder
        case _ if defn.isAdt                                => baboonBinCodecNoEncoderGeneratedAdt
        case _                                              => baboonBinCodecNoEncoderGenerated
      }
    }

    val meta = renderMeta(defn, swDomainTreeTools.makeCodecMeta(defn))

    q"""public class ${cName.asDeclName}: $cParent<$localName>, $baboonBinCodecIndexed {
       |    public static let instance = ${cName.asDeclName}()
       |    private override init() { super.init() }
       |
       |    ${baseMethods.joinNN().shift(4).trim}
       |
       |    ${meta.joinN().shift(4).trim}
       |}
       |""".stripMargin
  }

  private def genForeignBodies(name: SwType): (TextTree[SwValue], TextTree[SwValue]) = {
    (
      q"""fatalError("${name.name} is a foreign type")""",
      q"""fatalError("${name.name} is a foreign type")""",
    )
  }

  private def genAdtBodies(name: SwType, adt: Typedef.Adt): (TextTree[SwValue], TextTree[SwValue]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList.map {
      case (m, idx) =>
        val branchName = m.name.name
        val caseName   = branchName.head.toLower.toString + branchName.tail

        val adtRef = trans.toSwTypeRefKeepForeigns(m, domain, evo)
        val cName  = codecName(adtRef)

        val encBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.instance.encode(ctx, writer, branchVal)"""
        } else {
          q"""writer.writeU8(${idx.toString})
             |$cName.instance.encode(ctx, writer, branchVal)""".stripMargin
        }

        val decBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""return .$caseName(try ($cName.instance as! $cName).decodeBranch(ctx, reader))"""
        } else {
          q"""return .$caseName(try $cName.instance.decode(ctx, reader))"""
        }

        (
          q"""case .$caseName(let branchVal):
             |    ${encBody.shift(4).trim}""".stripMargin,
          q"""case ${idx.toString}:
             |    ${decBody.shift(4).trim}""".stripMargin,
        )
    }

    (
      q"""switch value {
         |${branches.map(_._1).joinN().shift(0).trim}
         |}""".stripMargin,
      q"""let asByte = reader.readU8()
         |
         |switch asByte {
         |${branches.map(_._2).joinN().shift(0).trim}
         |default:
         |    throw BaboonCodecError.invalidInput("Cannot decode to ${name.name}: no matching value for ordinal \\(asByte)")
         |}""".stripMargin,
    )
  }

  private def genEnumBodies(name: SwType, e: Typedef.Enum): (TextTree[SwValue], TextTree[SwValue]) = {
    val encBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val pascal = EnumWireStyle.wireName(m.name)
        q"case .$pascal: writer.writeU8(${idx.toString})"
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val pascal = EnumWireStyle.wireName(m.name)
        q"case ${idx.toString}: return ${name.asDeclName}.$pascal"
    }

    (
      q"""switch value {
         |${encBranches.joinN().shift(0).trim}
         |}""".stripMargin,
      q"""let asByte = reader.readU8()
         |
         |switch asByte {
         |${decBranches.joinN().shift(0).trim}
         |default:
         |    throw BaboonCodecError.invalidInput("Cannot decode to ${name.name}: no matching value for ordinal \\(asByte)")
         |}""".stripMargin,
    )
  }

  private def genBranchDecoder(
    name: SwType,
    d: Typedef.Dto,
  ): Option[TextTree[SwValue]] = {
    d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(name, fields.map(_._2)))
      case _ =>
        None
    }
  }

  private def genDtoBodies(name: SwType, dto: Typedef.Dto): (TextTree[SwValue], TextTree[SwValue]) = {
    val fields = fieldsOf(dto)

    val noIndex = Seq(
      q"writer.writeU8(header)",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).joinN()

    val fenc =
      q"""var header: UInt8 = 0
         |
         |if ctx.useIndices {
         |    header = header | 1
         |    writer.writeU8(header)
         |    let buffer = $baboonBinWriter()
         |    ${fields.map(_._3).joinN().shift(4).trim}
         |    writer.writeAll(buffer.toData())
         |} else {
         |    ${noIndex.shift(4).trim}
         |}""".stripMargin

    val fdec = dtoDec(name, fields.map(_._2))

    def adtBranchIndex(id: TypeId.User) = {
      domain.defs.meta
        .nodes(id)
        .asInstanceOf[DomainMember.User]
        .defn
        .asInstanceOf[Typedef.Adt]
        .dataMembers(domain)
        .zipWithIndex
        .find(_._1 == dto.id)
        .get
        ._2
    }

    val enc = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)

        q"""writer.writeU8(${idx.toString})
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)
        q"""let marker = reader.readU8()
           |assert(marker == ${idx.toString})
           |return try decodeBranch(ctx, reader)""".stripMargin
      case _ => fdec
    }
    (enc, dec)
  }

  private def dtoDec(name: SwType, fields: List[TextTree[SwValue]]): TextTree[SwValue] = {
    val fieldAssignments = if (fields.isEmpty) {
      q""
    } else {
      q"""${fields.join(",\n").shift(4).trim}"""
    }

    q"""let index = try readIndex(ctx, reader)
       |if ctx.useIndices { assert(index.count == indexElementsCount) }
       |return ${name.asDeclName}(
       |    $fieldAssignments
       |)""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[SwValue], TextTree[SwValue], TextTree[SwValue])] = {
    dto.fields.map {
      field =>
        val escaped             = trans.escapeSwiftKeyword(field.name.name)
        val fieldRef            = q"value.$escaped"
        val enc                 = mkEncoder(field.tpe, fieldRef, q"writer")
        val bufferEnc           = mkEncoder(field.tpe, fieldRef, q"buffer")
        val (decoder, mayThrow) = mkDecoder(field.tpe)
        val decodeTree          = if (mayThrow) q"$escaped: try $decoder" else q"$escaped: $decoder"

        val w = domain.refMeta(field.tpe).len match {
          case BinReprLen.Fixed(bytes) =>
            q"""do {
               |    // ${field.toString}
               |    let before = buffer.position
               |    ${bufferEnc.shift(4).trim}
               |    let after = buffer.position
               |    let length = after - before
               |    assert(length == ${bytes.toString})
               |}""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = v match {
              case BinReprLen.Unknown() =>
                q"""assert(after >= before, "Got after=\\(after), before=\\(before)")"""

              case BinReprLen.Alternatives(variants) =>
                q"""assert([${variants.mkString(", ")}].contains(length), "Got length=\\(length)")"""

              case BinReprLen.Range(min, max) =>
                (
                  Seq(q"""assert(length >= ${min.toString}, "Got length=\\(length)")""") ++
                  max.toSeq.map(m => q"""assert(length <= ${m.toString}, "Got length=\\(length)")""")
                ).joinN()
            }

            q"""do {
               |    // ${field.toString}
               |    let before = buffer.position
               |    writer.writeI32(Int32(before))
               |    ${bufferEnc.shift(4).trim}
               |    let after = buffer.position
               |    let length = after - before
               |    writer.writeI32(Int32(length))
               |    ${sanityChecks.shift(4).trim}
               |}""".stripMargin
        }

        (enc, decodeTree, w)
    }
  }

  // Returns (expression, mayThrow). `mayThrow` is true when the expression contains a top-level
  // throwing call that needs `try` at the parent site (e.g. inside a closure body or ternary).
  private def mkDecoder(tpe: TypeRef): (TextTree[SwValue], Boolean) = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit   => (q"reader.readBool()", false)
              case TypeId.Builtins.i08   => (q"reader.readI8()", false)
              case TypeId.Builtins.i16   => (q"reader.readI16()", false)
              case TypeId.Builtins.i32   => (q"reader.readI32()", false)
              case TypeId.Builtins.i64   => (q"reader.readI64()", false)
              case TypeId.Builtins.u08   => (q"reader.readU8()", false)
              case TypeId.Builtins.u16   => (q"reader.readU16()", false)
              case TypeId.Builtins.u32   => (q"reader.readU32()", false)
              case TypeId.Builtins.u64   => (q"reader.readU64()", false)
              case TypeId.Builtins.f32   => (q"reader.readF32()", false)
              case TypeId.Builtins.f64   => (q"reader.readF64()", false)
              case TypeId.Builtins.f128  => (q"reader.readDecimal()", false)
              case TypeId.Builtins.str   => (q"reader.readString()", true)
              case TypeId.Builtins.bytes => (q"reader.readBytes()", true)
              case TypeId.Builtins.uid   => (q"reader.readUuid()", true)
              case TypeId.Builtins.tsu   => (q"reader.readTsu()", false)
              case TypeId.Builtins.tso   => (q"reader.readTso()", false)
              case o                     => throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toSwTypeRefKeepForeigns(u, domain, evo))
            (q"$targetTpe.instance.decode(ctx, reader)", true)
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            val (innerDecoder, innerThrows) = mkDecoder(c.args.head)
            // Do NOT add inner `try` — if innerThrows is true, the parent adds `try` at the call
            // site which covers throwing calls inside the ternary branch (Swift allows outermost
            // `try` to cover nested throwing calls: `try (cond ? f() : nil)` is valid).
            (q"""(reader.readBool() ? $innerDecoder : nil)""", innerThrows)
          case TypeId.Builtins.map =>
            val (keyDecoder, keyThrows)     = mkDecoder(c.args.head)
            val (valueDecoder, valueThrows) = mkDecoder(c.args.last)
            val keyExpr                     = if (keyThrows) q"try $keyDecoder" else keyDecoder
            val valueExpr                   = if (valueThrows) q"try $valueDecoder" else valueDecoder
            (q"""Dictionary(uniqueKeysWithValues: (0..<Int(reader.readI32())).map { _ in ($keyExpr, $valueExpr) })""", keyThrows || valueThrows)
          case TypeId.Builtins.lst =>
            val (elemDecoder, elemThrows) = mkDecoder(c.args.head)
            val elemExpr                  = if (elemThrows) q"try $elemDecoder" else elemDecoder
            (q"""(0..<Int(reader.readI32())).map { _ in $elemExpr }""", elemThrows)
          case TypeId.Builtins.set =>
            val (elemDecoder, elemThrows) = mkDecoder(c.args.head)
            val elemExpr                  = if (elemThrows) q"try $elemDecoder" else elemDecoder
            (q"""Set((0..<Int(reader.readI32())).map { _ in $elemExpr })""", elemThrows)
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => (mkAnyDecoder(a), true)
    }
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[SwValue], wref: TextTree[SwValue]): TextTree[SwValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit   => q"$wref.writeBool($ref)"
              case TypeId.Builtins.i08   => q"$wref.writeI8($ref)"
              case TypeId.Builtins.i16   => q"$wref.writeI16($ref)"
              case TypeId.Builtins.i32   => q"$wref.writeI32($ref)"
              case TypeId.Builtins.i64   => q"$wref.writeI64($ref)"
              case TypeId.Builtins.u08   => q"$wref.writeU8($ref)"
              case TypeId.Builtins.u16   => q"$wref.writeU16($ref)"
              case TypeId.Builtins.u32   => q"$wref.writeU32($ref)"
              case TypeId.Builtins.u64   => q"$wref.writeU64($ref)"
              case TypeId.Builtins.f32   => q"$wref.writeF32($ref)"
              case TypeId.Builtins.f64   => q"$wref.writeF64($ref)"
              case TypeId.Builtins.f128  => q"$wref.writeDecimal($ref)"
              case TypeId.Builtins.str   => q"$wref.writeString($ref)"
              case TypeId.Builtins.bytes => q"$wref.writeBytes($ref)"
              case TypeId.Builtins.uid   => q"$wref.writeUuid($ref)"
              case TypeId.Builtins.tsu   => q"$wref.writeTsu($ref)"
              case TypeId.Builtins.tso   => q"$wref.writeTso($ref)"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toSwTypeRefKeepForeigns(u, domain, evo))
            q"""$targetTpe.instance.encode(ctx, $wref, $ref)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""if let unwrapped = $ref {
               |    $wref.writeBool(true)
               |    ${mkEncoder(c.args.head, q"unwrapped", wref).shift(4).trim}
               |} else {
               |    $wref.writeBool(false)
               |}""".stripMargin

          case TypeId.Builtins.map =>
            q"""$wref.writeI32(Int32($ref.count))
               |for (key, value) in $ref {
               |    ${mkEncoder(c.args.head, q"key", wref).shift(4).trim}
               |    ${mkEncoder(c.args.last, q"value", wref).shift(4).trim}
               |}""".stripMargin

          case TypeId.Builtins.lst =>
            q"""$wref.writeI32(Int32($ref.count))
               |for item in $ref {
               |    ${mkEncoder(c.args.head, q"item", wref).shift(4).trim}
               |}""".stripMargin

          case TypeId.Builtins.set =>
            q"""$wref.writeI32(Int32($ref.count))
               |for item in $ref {
               |    ${mkEncoder(c.args.head, q"item", wref).shift(4).trim}
               |}""".stripMargin

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref, wref)
    }
  }

  // Deep walk (mirrors Scala/C#/Rust/Kotlin/Java/TS/Dart hasAnyField): a codec class needs the
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
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[SwValue], wref: TextTree[SwValue]): TextTree[SwValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"encodeAnyField(ctx, $wref, $expectedHex, $staticDom, $staticVer, $staticTid, $ref)"
  }

  // Decode delegates to the per-codec-class `decodeAnyField` helper, returning an `AnyOpaque`
  // (the helper's return type is `AnyOpaque` since Swift enums don't have separate per-case
  // surface types — the `.ueba` discriminator is in the value itself).
  private def mkAnyDecoder(a: TypeRef.Any): TextTree[SwValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"decodeAnyField(reader, $expectedHex)"
  }

  // Static fallbacks for the cross-format facade helpers (`jsonToUebaBytes`/`uebaToJson`). The
  // wire `meta` may omit components that are pinned by the field's static declaration; the codec
  // emits whatever is statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade.buildSyntheticTypeMeta` for the merge semantics. Per spec table:
  //   A=(nil,nil,nil), B=(currentDomain,nil,nil), C=(currentDomain,currentVersion,nil),
  //   D1=(nil,nil,underlyingFqid), D2=(currentDomain,nil,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // Duplicated across Scala/C#/Rust/Kotlin/Java/TS/Dart — extraction deferred (textual emission
  // diverges by language flavor; see PR 4.2 ledger entry's DRY analysis). 11th instance.
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

  // Per-codec-class helpers consolidating the any-field framing, kind-check, and
  // buffer-then-write / read-then-skip paths — emitted at most once per codec class that has any
  // any-bearing field. Mirrors `JvUEBACodecGenerator.anyFieldHelpers` /
  // `DtUEBACodecGenerator.anyFieldHelpers`. Wire layout (locked, see
  // docs/drafts/20260424-1738-any-opaque-fields.md §"Wire format"):
  //   length:i32 | meta-length:i32 | meta-kind:u8 | meta-strings | blob
  //
  // Swift `encode` on `BaboonBinCodecBase` is non-throwing (see `baboon_runtime.swift`); the
  // helper uses `preconditionFailure` for unrecoverable conditions (kind mismatch, missing
  // facade, facade-returned failure). This mirrors the existing Swift type-erasure pattern in
  // `AnyBaboonBinEncoder.encodeAnyValue`. The `decodeAnyField` helper IS `throws` since the
  // base `decode(...)` is `throws`.
  //
  // PR-12-D01 lesson applied: explicit non-negative sanity check on BOTH `anyTotalLength` AND
  // `anyMetaLength` before any size arithmetic. Swift `Int32` is signed; a malicious wire
  // `0xFFFF_FFFF` decodes to `-1`, and `Data.subdata(in: 0..<-1)` would trap.
  //
  // Cast-via-`as`: `ctx.facade` returns `BaboonCodecsFacadeBase?` (PR 9.1 import-cycle break).
  // The concrete facade carries `jsonToUebaBytes`; the cast is safe because
  // `BaboonCodecContext.withFacade` is the only construction path and accepts the same hierarchy.
  private def anyFieldHelpers: TextTree[SwValue] = {
    q"""private func encodeAnyField(
       |    _ ctx: $baboonCodecContext,
       |    _ writer: $baboonBinWriter,
       |    _ expectedKind: UInt8,
       |    _ staticDomain: String?,
       |    _ staticVersion: String?,
       |    _ staticTypeid: String?,
       |    _ value: $baboonAnyOpaque
       |) {
       |    if value.meta.kind != expectedKind {
       |        preconditionFailure(
       |            "any: meta-kind 0x\\(String(format: \"%02x\", value.meta.kind & 0xFF)) " +
       |            "does not match field-declared 0x\\(String(format: \"%02x\", expectedKind & 0xFF))"
       |        )
       |    }
       |    let anyBlob: Data
       |    switch value {
       |    case .ueba(_, let bytes):
       |        anyBlob = bytes
       |    case .json(let jsonMeta, let jsonValue):
       |        guard let anyFacadeBase = ctx.facade else {
       |            preconditionFailure(
       |                "Cannot encode AnyOpaque.json into UEBA without a facade reference. " +
       |                "Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), " +
       |                "or supply AnyOpaque.ueba directly."
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
       |        let anyConvResult = anyFacade.jsonToUebaBytes(
       |            jsonMeta,
       |            jsonValue,
       |            staticDomain: staticDomain,
       |            staticVersion: staticVersion,
       |            staticTypeid: staticTypeid
       |        )
       |        switch anyConvResult {
       |        case .failure(let err):
       |            preconditionFailure("any: jsonToUebaBytes failed: \\(err)")
       |        case .success(let bytes):
       |            anyBlob = bytes
       |        }
       |    }
       |    // Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
       |    let anyMetaBuf = $baboonBinWriter()
       |    $baboonAnyMetaCodec.writeBin(value.meta, anyMetaBuf)
       |    let anyMetaBytes = anyMetaBuf.toData()
       |    let anyTotalLength = Int32(4 + anyMetaBytes.count + anyBlob.count)
       |    writer.writeI32(anyTotalLength)
       |    writer.writeI32(Int32(anyMetaBytes.count))
       |    writer.writeAll(anyMetaBytes)
       |    writer.writeAll(anyBlob)
       |}
       |
       |private func decodeAnyField(_ wire: $baboonBinReader, _ expectedKind: UInt8) throws -> $baboonAnyOpaque {
       |    let anyTotalLength = wire.readI32()
       |    if anyTotalLength < 0 {
       |        throw $baboonCodecException.decoderFailure(
       |            "any: negative total-length \\(anyTotalLength)",
       |            nil
       |        )
       |    }
       |    let anyMetaLength = wire.readI32()
       |    if anyMetaLength < 0 {
       |        throw $baboonCodecException.decoderFailure(
       |            "any: negative meta-length \\(anyMetaLength)",
       |            nil
       |        )
       |    }
       |    if anyTotalLength < 4 + anyMetaLength {
       |        throw $baboonCodecException.decoderFailure(
       |            "any: total-length \\(anyTotalLength) smaller than 4 + meta-length \\(anyMetaLength)",
       |            nil
       |        )
       |    }
       |    let (anyMeta, anyBytesRead) = try $baboonAnyMetaCodec.readBinWithLength(wire)
       |    if anyBytesRead > Int(anyMetaLength) {
       |        throw $baboonCodecException.decoderFailure(
       |            "any: meta bytes-read \\(anyBytesRead) exceeded meta-length window \\(anyMetaLength)",
       |            nil
       |        )
       |    }
       |    if anyBytesRead < Int(anyMetaLength) {
       |        // Forward-compat: skip future meta-extension bytes within the meta-length window.
       |        wire.skipBytes(Int(anyMetaLength) - anyBytesRead)
       |    }
       |    if anyMeta.kind != expectedKind {
       |        throw $baboonCodecException.decoderFailure(
       |            "any: wire kind 0x\\(String(format: \"%02x\", anyMeta.kind & 0xFF)) " +
       |            "does not match field-declared 0x\\(String(format: \"%02x\", expectedKind & 0xFF))",
       |            nil
       |        )
       |    }
       |    let anyBlobLen = Int(anyTotalLength) - 4 - Int(anyMetaLength)
       |    let anyBlob = wire.readNBytes(anyBlobLen)
       |    return .ueba(meta: anyMeta, bytes: anyBlob)
       |}""".stripMargin
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[SwValue]] = {
    defn.defn match {
      case _: Typedef.Enum | _: Typedef.Foreign => meta.map(_.valueField)
      case _                                    => meta.map(_.refValueField)
    }
  }

  def codecName(name: SwType): SwType = {
    val baseFileName = name.importAs.getOrElse(trans.toSnakeCase(name.name))
    val localName    = name.localName.getOrElse(name.name)
    SwType(name.pkg, s"${name.name}_UebaCodec", name.fq, importAs = Some(baseFileName), localName = Some(s"${localName}_UebaCodec"))
  }

  override def codecMeta(defn: DomainMember.User, name: SwType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"public static let codecUeba: $baboonBinCodec<${name.asDeclName}> = ${codecName(name).asDeclName}.instance"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
