package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.typescript.TsTypes.*
import io.septimalmind.baboon.translator.typescript.TsValue.TsType
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
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
        q"""public encode(ctx: $tsBaboonCodecContext, value: $name, writer: $tsBaboonBinWriter): unknown {
           |    if (this !== $cName.lazyInstance.value) {
           |      return $cName.lazyInstance.value.encode(ctx, value, writer)
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

    val baseMethods = encodeMethod ++ decodeMethod ++
      branchDecoder.map(tree => q"""public decodeBranch(ctx: $tsBaboonCodecContext, reader: $tsBaboonBinReader) {
                                   |    ${tree.shift(4).trim}
                                   |}""".stripMargin)

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
    val encBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"""case "${m.name.capitalize}": $tsBinTools.writeByte(writer, ${idx.toString}); break;"""
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"""case ${idx.toString}: return "${m.name.capitalize}" as ${name.name};"""
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
             |    $codecType.instance.encode(ctx, value, writer)
             |}""".stripMargin
        } else {
          q"""if (value instanceof $branchName) {
             |    $tsBinTools.writeByte(writer, ${idx.toString});
             |    $codecType.instance.encode(ctx, value, writer);
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
      q"${encBranches.joinN().shift(4).trim}",
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
          case TypeId.Builtins.tsu   => q"$tsBinTools.writeTimestampUtc($w, $ref);"
          case TypeId.Builtins.tso   => q"$tsBinTools.writeTimestampOffset($w, $ref);"
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
            q"""$tsBinTools.writeI32($w, $ref.size);
               |for (const [k, v] of $ref) {
               |    ${mkEncoder(args.head, q"k", writer).shift(4).trim}
               |    ${mkEncoder(args.last, q"v", writer).shift(4).trim}
               |}""".stripMargin
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
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
          case TypeId.Builtins.tsu   => q"$tsBinTools.readTimestampUtc(reader)"
          case TypeId.Builtins.tso   => q"$tsBinTools.readTimestampOffset(reader)"
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
            q"new Map(Array.from({ length: $tsBinTools.readI32(reader) }, () => [${mkDecoder(args.head)}, ${mkDecoder(args.last)}] as const))"
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
    }
  }

  def codecName(name: TsValue.TsType): TsValue.TsType = {
    TsValue.TsType(name.moduleId, s"${name.name}_UEBACodec")
  }

  override def codecMeta(definition: DomainMember.User, name: TsValue.TsType): Option[TextTree[TsValue]] = {
    if (isActive(definition.id)) {
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
