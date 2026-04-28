package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.typescript.TsTypes.*
import io.septimalmind.baboon.translator.typescript.TsValue.TsType
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class TsUEBACodecGenerator(
  typeTranslator: TsTypeTranslator,
  target: TsTarget,
  domain: Domain,
  evo: BaboonEvolution,
  enquiries: BaboonEnquiries,
  tsFileTools: TsFileTools,
  tsDomainTreeTools: TsDomainTreeTools,
) extends TsCodecTranslator {
  override def translate(defn: DomainMember.User, tsRef: TsValue.TsType, srcRef: TsValue.TsType): Option[TextTree[TsValue]] = {
    if (isActive(defn.id) && !enquiries.hasForeignType(defn, domain)) {
      (defn.defn match {
        case d: Typedef.Dto      => Some(genDtoCodec(srcRef, d))
        case e: Typedef.Enum     => Some(genEnumCodec(srcRef, e))
        case a: Typedef.Adt      => Some(genAdtCodec(srcRef, a))
        case _: Typedef.Foreign  => None
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          val branchDecoder = defn.defn match {
            case d: Typedef.Dto => genBranchDecoder(tsRef, d)
            case _              => None
          }
          genCodec(defn, tsRef, srcRef, enc, dec, branchDecoder)
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: TsType,
    srcRef: TsType,
    enc: TextTree[TsValue],
    dec: TextTree[TsValue],
    branchDecoder: Option[TextTree[TsValue]],
  ): TextTree[TsValue] = {
    val cName = codecName(name)

    val encodeMethod =
      List(
        q"""public encode(ctx: $tsBaboonCodecContext, value: $name, writer: $tsBaboonBinWriter): void {
           |    if (this !== $cName.lazyInstance.value) {
           |        $cName.lazyInstance.value.encode(ctx, value, writer); return;
           |    }
           |
           |    ${enc.shift(4).trim}
           |}
           |""".stripMargin.trim
      )

    val decodeMethod =
      List(
        q"""public decode(ctx: $tsBaboonCodecContext, reader: $tsBaboonBinReader): $name {
           |    if (this !== $cName .lazyInstance.value) {
           |        return $cName.lazyInstance.value.decode(ctx, reader)
           |    }
           |
           |    ${dec.shift(4).trim}
           |}""".stripMargin.trim
      )

    val anyHelpers: List[TextTree[TsValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil

    val baseMethods = encodeMethod ++ decodeMethod ++
      branchDecoder.map(tree => q"""public decodeBranch(ctx: $tsBaboonCodecContext, reader: $tsBaboonBinReader) {
                                   |    ${tree.shift(4).trim}
                                   |}""".stripMargin) ++ anyHelpers

    val meta = tsDomainTreeTools.makeCodecMeta(defn, codecName(srcRef))

    q"""export class $cName {
       |    ${baseMethods.joinNN().shift(4).trim}
       |
       |    ${meta.joinN().shift(4).trim}
       |
       |    protected static lazyInstance = new $tsBaboonLazy(() => new $cName())
       |    public static get instance(): $cName {
       |        return $cName.lazyInstance.value
       |    }
       |}""".stripMargin
  }

  private def adtBranchIndex(adtId: TypeId.User, dtoId: TypeId): Int = {
    domain.defs.meta
      .nodes(adtId)
      .asInstanceOf[DomainMember.User]
      .defn
      .asInstanceOf[Typedef.Adt]
      .dataMembers(domain)
      .zipWithIndex
      .find(_._1 == dtoId)
      .get
      ._2
  }

  private def genBranchDecoder(
    name: TsType,
    d: Typedef.Dto,
  ): Option[TextTree[TsValue]] = {
    d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(d, name, fields.map(_._2)))
      case _ =>
        None
    }
  }

  private def dtoDec(dto: Typedef.Dto, name: TsType, fields: List[TextTree[TsValue]]): TextTree[TsValue] = {
    val indexCount = dto.fields.count(f => domain.refMeta(f.tpe).len.isVariable)
    val ctorFields = dto.fields.map {
      f =>
        q"${f.name.name},"
    }
    q"""const header = $tsBinTools.readByte(reader);
       |const useIndices = header === 0x01;
       |if (useIndices) {
       |    for (let i = 0; i < ${indexCount.toString}; i++) {
       |        $tsBinTools.readI32(reader);
       |        $tsBinTools.readI32(reader);
       |    }
       |}
       |${fields.joinN().trim}
       |return new $name(
       |    ${ctorFields.joinN().shift(4).trim}
       |);""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[TsValue], TextTree[TsValue], TextTree[TsValue])] = {
    dto.fields.map {
      field =>
        val fieldName  = field.name.name
        val isVariable = domain.refMeta(field.tpe).len.isVariable
        val enc        = mkEncoder(field.tpe, q"value.$fieldName", "writer")
        val fakeEnc    = mkEncoder(field.tpe, q"value.$fieldName", "buffer")
        val dec        = q"const $fieldName = ${mkDecoder(field.tpe)};"
        val w = if (isVariable) {
          q"""{
             |    const before = buffer.position();
             |    $tsBinTools.writeI32(writer, before);
             |    $fakeEnc
             |    const after = buffer.position();
             |    $tsBinTools.writeI32(writer, after - before);
             |}""".stripMargin
        } else {
          fakeEnc
        }

        (enc, dec, w)
    }
  }

  private def genDtoCodec(name: TsValue.TsType, dto: Typedef.Dto): (TextTree[TsValue], TextTree[TsValue]) = {
    val fields = fieldsOf(dto)
    val noIndex = Seq(
      q"$tsBinTools.writeByte(writer, 0x00)",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).joinN()

    val fenc =
      q"""if (ctx === $tsBaboonCodecContext.Indexed) {
         |    $tsBinTools.writeByte(writer, 0x01);
         |    const buffer = new $tsBaboonBinWriter();
         |    ${fields.map(_._3).joinN().shift(4).trim}
         |    writer.writeAll(buffer.toBytes());
         |} else {
         |    ${noIndex.shift(4).trim}
         |}""".stripMargin

    val fdec = dtoDec(dto, name, fields.map(_._2))

    val enc = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id, dto.id)

        q"""$tsBinTools.writeByte(writer, ${idx.toString})
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id, dto.id)
        q"""const marker = $tsBinTools.readByte(reader)
           |if (marker !== ${idx.toString}) { throw new Error("Expected ADT branch marker ${idx.toString}, got " + marker); }
           |return this.decodeBranch(ctx, reader)""".stripMargin
      case _ => fdec
    }
    (enc, dec)
  }

  private def genEnumCodec(name: TsValue.TsType, e: Typedef.Enum): (TextTree[TsValue], TextTree[TsValue]) = {
    val lowercaseValues = target.language.enumLowercaseValues
    val encBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val value = if (lowercaseValues) m.name.toLowerCase else EnumWireStyle.wireName(m.name)
        q"""case "$value": $tsBinTools.writeByte(writer, ${idx.toString}); break;"""
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val value = if (lowercaseValues) m.name.toLowerCase else EnumWireStyle.wireName(m.name)
        q"""case ${idx.toString}: return "$value" as ${name.name};"""
    }

    (
      q"""switch (value) {
         |    ${encBranches.joinN().shift(8).trim}
         |    default: throw new Error("Unknown enum variant: " + value);
         |}""".stripMargin,
      q"""const tag = $tsBinTools.readByte(reader);
         |switch (tag) {
         |    ${decBranches.joinN().shift(8).trim}
         |    default: throw new Error("Unknown enum variant tag: " + tag);
         |}""".stripMargin,
    )
  }

  private def genAdtCodec(name: TsValue.TsType, adt: Typedef.Adt): (TextTree[TsValue], TextTree[TsValue]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList

    val encBranches = branches.map {
      case (mid, idx) =>
        val branchName = mid.name.name
        val branchType = typeTranslator.asTsType(mid, domain, evo, tsFileTools.definitionsBasePkg)
        val codecType  = codecName(branchType)
        if (target.language.wrappedAdtBranchCodecs) {
          q"""if (value instanceof $branchName) {
             |    $codecType.instance.encode(ctx, value, writer);
             |    return;
             |}""".stripMargin
        } else {
          q"""if (value instanceof $branchName) {
             |    $tsBinTools.writeByte(writer, ${idx.toString});
             |    $codecType.instance.encode(ctx, value, writer);
             |    return;
             |}""".stripMargin
        }
    }

    val decBranches = branches.map {
      case (mid, idx) =>
        val branchType = typeTranslator.asTsType(mid, domain, evo, tsFileTools.definitionsBasePkg)
        val codecType  = codecName(branchType)
        if (target.language.wrappedAdtBranchCodecs) {
          q"""case ${idx.toString}: return $codecType.instance.decodeBranch(ctx, reader)"""
        } else {
          q"""case ${idx.toString}: return $codecType.instance.decode(ctx, reader)"""
        }
    }

    q"""export function encode_${name.name}_ueba(value: ${name.name}, ctx: $tsBaboonCodecContext, writer: $tsBaboonBinWriter): void {
       |    ${encBranches.joinN().shift(4).trim}
       |}
       |
       |export function decode_${name.name}_ueba(ctx: $tsBaboonCodecContext, reader: $tsBaboonBinReader): ${name.name} {
       |    const tag = $tsBinTools.readByte(reader);
       |    switch (tag) {
       |        ${decBranches.joinN().shift(8).trim}
       |        default: throw new Error("Unknown ADT branch tag: " + tag);
       |    }
       |}""".stripMargin
    (
      q"""${encBranches.joinN().shift(4).trim}
         |throw new Error("Unhandled ADT branch: " + (value as {constructor?: {name?: string}}).constructor?.name);""".stripMargin,
      q"""const tag = $tsBinTools.readByte(reader);
         |switch (tag) {
         |    ${decBranches.joinN().shift(8).trim}
         |    default: throw new Error("Unknown ADT branch tag: " + tag);
         |}""".stripMargin,
    )
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[TsValue], writer: String): TextTree[TsValue] = {
    val w = q"$writer"
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.bit   => q"$tsBinTools.writeBool($w, $ref);"
          case TypeId.Builtins.i08   => q"$tsBinTools.writeI8($w, $ref);"
          case TypeId.Builtins.i16   => q"$tsBinTools.writeI16($w, $ref);"
          case TypeId.Builtins.i32   => q"$tsBinTools.writeI32($w, $ref);"
          case TypeId.Builtins.i64   => q"$tsBinTools.writeI64($w, $ref);"
          case TypeId.Builtins.u08   => q"$tsBinTools.writeU8($w, $ref);"
          case TypeId.Builtins.u16   => q"$tsBinTools.writeU16($w, $ref);"
          case TypeId.Builtins.u32   => q"$tsBinTools.writeU32($w, $ref);"
          case TypeId.Builtins.u64   => q"$tsBinTools.writeU64($w, $ref);"
          case TypeId.Builtins.f32   => q"$tsBinTools.writeF32($w, $ref);"
          case TypeId.Builtins.f64   => q"$tsBinTools.writeF64($w, $ref);"
          case TypeId.Builtins.f128  => q"$tsBinTools.writeDecimal($w, $ref);"
          case TypeId.Builtins.str   => q"$tsBinTools.writeString($w, $ref);"
          case TypeId.Builtins.bytes => q"$tsBinTools.writeBytes($w, $ref);"
          case TypeId.Builtins.uid   => q"$tsBinTools.writeUuid($w, $ref);"
          case TypeId.Builtins.tsu =>
            target.language.timestampsUtcMode match {
              case "string" => q"$tsBinTools.writeTimestampUtc($w, $tsBaboonDateTimeUtc.fromISO($ref));"
              case "date"   => q"$tsBinTools.writeTimestampUtc($w, $tsBaboonDateTimeUtc.fromDate($ref));"
              case _        => q"$tsBinTools.writeTimestampUtc($w, $ref);"
            }
          case TypeId.Builtins.tso =>
            target.language.timestampsOffsetMode match {
              case "string" => q"$tsBinTools.writeTimestampOffset($w, $tsBaboonDateTimeOffset.fromISO($ref));"
              case "date"   => q"$tsBinTools.writeTimestampOffset($w, $tsBaboonDateTimeOffset.fromISO($ref.toISOString()));"
              case _        => q"$tsBinTools.writeTimestampOffset($w, $ref);"
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Typescript) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, writer)
                  case _ =>
                    val tsType = typeTranslator.asTsTypeDerefForeign(u, domain, evo, tsFileTools.definitionsBasePkg)
                    val codec  = codecName(tsType)
                    q"$codec.instance.encode(ctx, $ref, $w);"
                }
              case _ =>
                val tsType = typeTranslator.asTsTypeDerefForeign(u, domain, evo, tsFileTools.definitionsBasePkg)
                val codec  = codecName(tsType)
                q"$codec.instance.encode(ctx, $ref, $w);"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case TypeRef.Constructor(cid, args) =>
        cid match {
          case TypeId.Builtins.opt =>
            q"""if ($ref === undefined) {
               |    $tsBinTools.writeByte($w, 0);
               |} else {
               |    $tsBinTools.writeByte($w, 1);
               |    ${mkEncoder(args.head, ref, writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.lst =>
            q"""$tsBinTools.writeI32($w, Array.from($ref).length);
               |for (const item of $ref) {
               |    ${mkEncoder(args.head, q"item", writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.set =>
            q"""$tsBinTools.writeI32($w, $ref.size);
               |for (const item of $ref) {
               |    ${mkEncoder(args.head, q"item", writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.map =>
            val isRecord = typeTranslator.isStringKeyMap(tpe)
            if (isRecord) {
              // Wrap in a block to avoid duplicate `const` declarations when
              // multiple Record-typed map fields exist in the same DTO
              q"""{
                 |    const entries = Object.entries($ref);
                 |    $tsBinTools.writeI32($w, entries.length);
                 |    for (const [k, v] of entries) {
                 |        ${mkEncoder(args.head, q"k", writer).shift(8).trim}
                 |        ${mkEncoder(args.last, q"v", writer).shift(8).trim}
                 |    }
                 |}""".stripMargin
            } else {
              q"""$tsBinTools.writeI32($w, $ref.size);
                 |for (const [k, v] of $ref) {
                 |    ${mkEncoder(args.head, q"k", writer).shift(4).trim}
                 |    ${mkEncoder(args.last, q"v", writer).shift(4).trim}
                 |}""".stripMargin
            }
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref, writer)
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[TsValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.bit   => q"$tsBinTools.readBool(reader)"
          case TypeId.Builtins.i08   => q"$tsBinTools.readI8(reader)"
          case TypeId.Builtins.i16   => q"$tsBinTools.readI16(reader)"
          case TypeId.Builtins.i32   => q"$tsBinTools.readI32(reader)"
          case TypeId.Builtins.i64   => q"$tsBinTools.readI64(reader)"
          case TypeId.Builtins.u08   => q"$tsBinTools.readU8(reader)"
          case TypeId.Builtins.u16   => q"$tsBinTools.readU16(reader)"
          case TypeId.Builtins.u32   => q"$tsBinTools.readU32(reader)"
          case TypeId.Builtins.u64   => q"$tsBinTools.readU64(reader)"
          case TypeId.Builtins.f32   => q"$tsBinTools.readF32(reader)"
          case TypeId.Builtins.f64   => q"$tsBinTools.readF64(reader)"
          case TypeId.Builtins.f128  => q"$tsBinTools.readDecimal(reader)"
          case TypeId.Builtins.str   => q"$tsBinTools.readString(reader)"
          case TypeId.Builtins.bytes => q"$tsBinTools.readBytes(reader)"
          case TypeId.Builtins.uid   => q"$tsBinTools.readUuid(reader)"
          case TypeId.Builtins.tsu =>
            target.language.timestampsUtcMode match {
              case "string" => q"$tsBinTools.readTimestampUtc(reader).toISOString()"
              case "date"   => q"$tsBinTools.readTimestampUtc(reader).date"
              case _        => q"$tsBinTools.readTimestampUtc(reader)"
            }
          case TypeId.Builtins.tso =>
            target.language.timestampsOffsetMode match {
              case "string" => q"$tsBinTools.readTimestampOffset(reader).toISOString()"
              case "date"   => q"$tsBinTools.readTimestampOffset(reader).date"
              case _        => q"$tsBinTools.readTimestampOffset(reader)"
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Typescript) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ =>
                    val tsType = typeTranslator.asTsTypeKeepForeigns(u, domain, evo, tsFileTools.definitionsBasePkg)
                    val codec  = codecName(tsType)
                    q"$codec.instance.decode(ctx, reader)"
                }
              case _ =>
                val tsType = typeTranslator.asTsTypeKeepForeigns(u, domain, evo, tsFileTools.definitionsBasePkg)
                val codec  = codecName(tsType)
                q"$codec.instance.decode(ctx, reader)"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case TypeRef.Constructor(cid, args) =>
        cid match {
          case TypeId.Builtins.opt =>
            q"($tsBinTools.readByte(reader) === 0 ? undefined : ${mkDecoder(args.head)})"
          case TypeId.Builtins.lst =>
            q"Array.from({ length: $tsBinTools.readI32(reader) }, () => ${mkDecoder(args.head)})"
          case TypeId.Builtins.set =>
            q"new Set(Array.from({ length: $tsBinTools.readI32(reader) }, () => ${mkDecoder(args.head)}))"
          case TypeId.Builtins.map =>
            val isRecord = typeTranslator.isStringKeyMap(tpe)
            if (isRecord) {
              q"Object.fromEntries(Array.from({ length: $tsBinTools.readI32(reader) }, () => [${mkDecoder(args.head)}, ${mkDecoder(args.last)}] as const))"
            } else {
              q"new Map(Array.from({ length: $tsBinTools.readI32(reader) }, () => [${mkDecoder(args.head)}, ${mkDecoder(args.last)}] as const))"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a)
    }
  }

  // Deep walk (mirrors Scala/C#/Rust/Kotlin/Java hasAnyField): a codec class needs the any-field
  // helpers if any direct or nested-via-Constructor-arg field has type `any`.
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

  // Encode delegates to the per-codec-class `encodeAnyField` helper. Wires the expected kind byte
  // and the field's static (codec-gen-time) fallbacks for cross-format meta resolution.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[TsValue], writer: String): TextTree[TsValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"this.encodeAnyField(ctx, $writer, $expectedHex, $staticDom, $staticVer, $staticTid, $ref);"
  }

  // Decode delegates to the per-codec-class `decodeAnyField` helper.
  private def mkAnyDecoder(a: TypeRef.Any): TextTree[TsValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"this.decodeAnyField(reader, $expectedHex)"
  }

  // Static fallbacks for the cross-format facade helper (`jsonToUebaBytes`). The wire `meta` may
  // omit components that are pinned by the field's static declaration; the codec emits whatever is
  // statically known so the facade can fill the gaps. Per spec table:
  //   A=(undef,undef,undef), B=(currentDomain,undef,undef), C=(currentDomain,currentVersion,undef),
  //   D1=(undef,undef,underlyingFqid), D2=(currentDomain,undef,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // Duplicated across Scala/C#/Rust/Kotlin/Java/TS — extraction deferred (textual emission diverges
  // by language flavor; see PR 4.2 ledger entry's DRY analysis).
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[TsValue], TextTree[TsValue], TextTree[TsValue]) = {
    val none                     = q"undefined"
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
  // any-bearing field. Mirrors `JvUEBACodecGenerator.anyFieldHelpers`. Wire layout (locked, see
  // docs/drafts/20260424-1738-any-opaque-fields.md §"Wire format"):
  //   length:i32 | meta-length:i32 | meta-kind:u8 | meta-strings | blob
  //
  // PR-12-D01 lesson applied: explicit negative-i32 sanity guards on on-wire lengths run before
  // any size arithmetic — `4 + Int32.min` overflows silently otherwise.
  private def anyFieldHelpers: TextTree[TsValue] = {
    q"""private encodeAnyField(
       |    ctx: $tsBaboonCodecContext,
       |    writer: $tsBaboonBinWriter,
       |    expectedKind: number,
       |    staticDomain: string | undefined,
       |    staticVersion: string | undefined,
       |    staticTypeid: string | undefined,
       |    value: $tsBaboonAnyOpaque,
       |): void {
       |    if (value.meta.kind !== expectedKind) {
       |        throw new $tsBaboonEncoderFailure(
       |            `any: meta-kind 0x$${(value.meta.kind & 0xFF).toString(16).padStart(2, "0")} does not match field-declared 0x$${(expectedKind & 0xFF).toString(16).padStart(2, "0")}`
       |        );
       |    }
       |    let anyBlob: Uint8Array;
       |    if (value.tag === "Ueba") {
       |        anyBlob = value.bytes;
       |    } else {
       |        const anyFacade = ctx.facade;
       |        if (anyFacade === undefined) {
       |            throw new $tsBaboonEncoderFailure(
       |                "Cannot encode AnyOpaqueJson into UEBA without a facade reference. Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), or supply anyOpaqueUeba directly."
       |            );
       |        }
       |        const anyConvResult = anyFacade.jsonToUebaBytes(value.meta, value.json, staticDomain, staticVersion, staticTypeid);
       |        if (anyConvResult.tag === "Left") {
       |            throw anyConvResult.value;
       |        }
       |        anyBlob = anyConvResult.value;
       |    }
       |    // Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
       |    const anyMetaBuf = new $tsBaboonBinWriter();
       |    $tsBaboonAnyMetaCodec.writeBin(value.meta, anyMetaBuf);
       |    const anyMetaBytes = anyMetaBuf.toBytes();
       |    const anyTotalLength = 4 + anyMetaBytes.length + anyBlob.length;
       |    $tsBinTools.writeI32(writer, anyTotalLength);
       |    $tsBinTools.writeI32(writer, anyMetaBytes.length);
       |    writer.writeBytes(anyMetaBytes);
       |    writer.writeBytes(anyBlob);
       |}
       |
       |private decodeAnyField(reader: $tsBaboonBinReader, expectedKind: number): $tsBaboonAnyOpaque {
       |    const anyTotalLength = $tsBinTools.readI32(reader);
       |    if (anyTotalLength < 0) {
       |        throw new $tsBaboonDecoderFailure(`any: negative total-length $${anyTotalLength}`);
       |    }
       |    const anyMetaLength = $tsBinTools.readI32(reader);
       |    if (anyMetaLength < 0) {
       |        throw new $tsBaboonDecoderFailure(`any: negative meta-length $${anyMetaLength}`);
       |    }
       |    if (anyTotalLength < 4 + anyMetaLength) {
       |        throw new $tsBaboonDecoderFailure(`any: total-length $${anyTotalLength} smaller than 4 + meta-length $${anyMetaLength}`);
       |    }
       |    const anyReadResult = $tsBaboonAnyMetaCodec.readBinWithLength(reader);
       |    const anyMeta = anyReadResult.meta;
       |    const anyBytesRead = anyReadResult.bytesRead;
       |    if (anyBytesRead > anyMetaLength) {
       |        throw new $tsBaboonDecoderFailure(`any: meta bytes-read $${anyBytesRead} exceeded meta-length window $${anyMetaLength}`);
       |    }
       |    if (anyBytesRead < anyMetaLength) {
       |        // Forward-compat: skip future meta-extension bytes within the meta-length window.
       |        reader.readBytes(anyMetaLength - anyBytesRead);
       |    }
       |    if (anyMeta.kind !== expectedKind) {
       |        throw new $tsBaboonDecoderFailure(
       |            `any: wire kind 0x$${(anyMeta.kind & 0xFF).toString(16).padStart(2, "0")} does not match field-declared 0x$${(expectedKind & 0xFF).toString(16).padStart(2, "0")}`
       |        );
       |    }
       |    const anyBlobLen = anyTotalLength - 4 - anyMetaLength;
       |    const anyBlob = reader.readBytes(anyBlobLen);
       |    return $tsBaboonAnyOpaqueUebaCtor(anyMeta, anyBlob);
       |}""".stripMargin
  }

  def codecName(name: TsValue.TsType): TsValue.TsType = {
    TsValue.TsType(name.moduleId, s"${name.name}_UEBACodec")
  }

  override def codecMeta(definition: DomainMember.User, name: TsValue.TsType): Option[TextTree[TsValue]] = {
    if (isActive(definition.id) && !enquiries.hasForeignType(definition, domain)) {
      definition.defn match {
        case _: Typedef.Adt =>
          Some(q"""binCodec(): ${codecName(name)} {
                  |    return ${codecName(name)}.instance
                  |}""".stripMargin)
        case _ =>
          Some(q"""public static binCodec(): ${codecName(name)} {
                  |    return ${codecName(name)}.instance
                  |}""".stripMargin)
      }
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Typescript) &&
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
