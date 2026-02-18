package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class TsUEBACodecGenerator(
  trans: TsTypeTranslator,
  target: TsTarget,
  domain: Domain,
  evo: BaboonEvolution,
  enquiries: BaboonEnquiries,
) extends TsCodecTranslator {

  import TsTypes.{baboonBinReader, baboonBinWriter, baboonCodecContext, binTools}

  private def codecFnRef(tsType: TsValue.TsType, prefix: String, suffix: String): TsValue.TsType = {
    TsValue.TsType(tsType.module, s"$prefix${tsType.name}$suffix")
  }

  override def translate(defn: DomainMember.User, tsRef: TsValue.TsType, srcRef: TsValue.TsType): Option[TextTree[TsValue]] = {
    if (isActive(defn.id) && !enquiries.hasForeignType(defn, domain, BaboonLang.Typescript)) {
      defn.defn match {
        case d: Typedef.Dto      => Some(genDtoCodec(srcRef, d))
        case e: Typedef.Enum     => Some(genEnumCodec(srcRef, e))
        case a: Typedef.Adt      => Some(genAdtCodec(srcRef, a))
        case _: Typedef.Foreign  => None
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }
    } else None
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

  private def genDtoCodec(name: TsValue.TsType, dto: Typedef.Dto): TextTree[TsValue] = {
    val indexCount = dto.fields.count(f => domain.refMeta(f.tpe).len.isVariable)

    val compactEncFields = dto.fields.map {
      f =>
        mkEncoder(f.tpe, q"value.${f.name.name}")
    }

    val indexedEncFields = dto.fields.map {
      f =>
        val fld        = f.name.name
        val enc        = mkEncoder(f.tpe, q"value.$fld", "buffer")
        val isVariable = domain.refMeta(f.tpe).len.isVariable

        if (isVariable) {
          q"""{
             |    const before = buffer.position();
             |    $binTools.writeI32(writer, before);
             |    $enc
             |    const after = buffer.position();
             |    $binTools.writeI32(writer, after - before);
             |}""".stripMargin
        } else {
          enc
        }
    }

    val decFields = dto.fields.map {
      f =>
        val fld = f.name.name
        q"const $fld = ${mkDecoder(f.tpe)};"
    }

    val ctorFields = dto.fields.map {
      f =>
        q"${f.name.name},"
    }

    val encPrefix = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id, dto.id)
        Some(q"$binTools.writeByte(writer, ${idx.toString});")
      case _ => None
    }

    val encPrefixTree = encPrefix
      .map(p => q"""$p
                   |""".stripMargin).getOrElse(q"")

    val branchDecodeFn = dto.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        Some(
          q"""export function decode_${name.asName}_ueba_branch(ctx: $baboonCodecContext, reader: $baboonBinReader): ${name.asName} {
             |    const header = $binTools.readByte(reader);
             |    const useIndices = header === 0x01;
             |    if (useIndices) {
             |        for (let i = 0; i < ${indexCount.toString}; i++) {
             |            $binTools.readI32(reader);
             |            $binTools.readI32(reader);
             |        }
             |    }
             |    ${decFields.joinN().shift(4).trim}
             |    return {
             |        ${ctorFields.joinN().shift(8).trim}
             |    };
             |}""".stripMargin
        )
      case _ => None
    }

    val decBody = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id, dto.id)
        q"""const marker = $binTools.readByte(reader);
           |if (marker !== ${idx.toString}) { throw new Error("Expected ADT branch marker ${idx.toString}, got " + marker); }
           |return decode_${name.asName}_ueba_branch(ctx, reader);""".stripMargin
      case _ =>
        q"""const header = $binTools.readByte(reader);
           |const useIndices = header === 0x01;
           |if (useIndices) {
           |    for (let i = 0; i < ${indexCount.toString}; i++) {
           |        $binTools.readI32(reader);
           |        $binTools.readI32(reader);
           |    }
           |}
           |${decFields.joinN().shift(0).trim}
           |return {
           |    ${ctorFields.joinN().shift(4).trim}
           |};""".stripMargin
    }

    val mainCodec = q"""export function encode_${name.asName}_ueba(value: ${name.asName}, ctx: $baboonCodecContext, writer: $baboonBinWriter): void {
                       |    ${encPrefixTree}if (ctx === $baboonCodecContext.Indexed) {
                       |        $binTools.writeByte(writer, 0x01);
                       |        const buffer = new $baboonBinWriter();
                       |        ${indexedEncFields.joinN().shift(8).trim}
                       |        writer.writeAll(buffer.toBytes());
                       |    } else {
                       |        $binTools.writeByte(writer, 0x00);
                       |        ${compactEncFields.joinN().shift(8).trim}
                       |    }
                       |}
                       |
                       |export function decode_${name.asName}_ueba(ctx: $baboonCodecContext, reader: $baboonBinReader): ${name.asName} {
                       |    ${decBody.shift(4).trim}
                       |}""".stripMargin

    branchDecodeFn match {
      case Some(branchFn) =>
        q"""$branchFn
           |
           |$mainCodec""".stripMargin
      case None => mainCodec
    }
  }

  private def genEnumCodec(name: TsValue.TsType, e: Typedef.Enum): TextTree[TsValue] = {
    val encBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"""case "${m.name.capitalize}": $binTools.writeByte(writer, ${idx.toString}); break;"""
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"""case ${idx.toString}: return "${m.name.capitalize}" as ${name.asName};"""
    }

    q"""export function encode_${name.asName}_ueba(value: ${name.asName}, ctx: $baboonCodecContext, writer: $baboonBinWriter): void {
       |    switch (value) {
       |        ${encBranches.joinN().shift(8).trim}
       |        default: throw new Error("Unknown enum variant: " + value);
       |    }
       |}
       |
       |export function decode_${name.asName}_ueba(ctx: $baboonCodecContext, reader: $baboonBinReader): ${name.asName} {
       |    const tag = $binTools.readByte(reader);
       |    switch (tag) {
       |        ${decBranches.joinN().shift(8).trim}
       |        default: throw new Error("Unknown enum variant tag: " + tag);
       |    }
       |}""".stripMargin
  }

  private def genAdtCodec(name: TsValue.TsType, adt: Typedef.Adt): TextTree[TsValue] = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList

    val encBranches = branches.map {
      case (mid, idx) =>
        val branchName = mid.name.name
        val branchType = trans.asTsType(mid, domain, evo)
        if (target.language.wrappedAdtBranchCodecs) {
          q"""case "$branchName": encode_${branchType.asName}_ueba(value.value, ctx, writer); break;"""
        } else {
          q"""case "$branchName": $binTools.writeByte(writer, ${idx.toString}); encode_${branchType.asName}_ueba(value.value, ctx, writer); break;"""
        }
    }

    val decBranches = branches.map {
      case (mid, idx) =>
        val branchName = mid.name.name
        val branchType = trans.asTsType(mid, domain, evo)
        if (target.language.wrappedAdtBranchCodecs) {
          q"""case ${idx.toString}: return ${name.asName}_$branchName(decode_${branchType.asName}_ueba_branch(ctx, reader));"""
        } else {
          q"""case ${idx.toString}: return ${name.asName}_$branchName(decode_${branchType.asName}_ueba(ctx, reader));"""
        }
    }

    q"""export function encode_${name.asName}_ueba(value: ${name.asName}, ctx: $baboonCodecContext, writer: $baboonBinWriter): void {
       |    switch (value._tag) {
       |        ${encBranches.joinN().shift(8).trim}
       |        default: throw new Error("Unknown ADT branch: " + (value as { _tag: string })._tag);
       |    }
       |}
       |
       |export function decode_${name.asName}_ueba(ctx: $baboonCodecContext, reader: $baboonBinReader): ${name.asName} {
       |    const tag = $binTools.readByte(reader);
       |    switch (tag) {
       |        ${decBranches.joinN().shift(8).trim}
       |        default: throw new Error("Unknown ADT branch tag: " + tag);
       |    }
       |}""".stripMargin
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[TsValue], writer: String = "writer"): TextTree[TsValue] = {
    val w = q"$writer"
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.bit   => q"$binTools.writeBool($w, $ref);"
          case TypeId.Builtins.i08   => q"$binTools.writeI8($w, $ref);"
          case TypeId.Builtins.i16   => q"$binTools.writeI16($w, $ref);"
          case TypeId.Builtins.i32   => q"$binTools.writeI32($w, $ref);"
          case TypeId.Builtins.i64   => q"$binTools.writeI64($w, $ref);"
          case TypeId.Builtins.u08   => q"$binTools.writeU8($w, $ref);"
          case TypeId.Builtins.u16   => q"$binTools.writeU16($w, $ref);"
          case TypeId.Builtins.u32   => q"$binTools.writeU32($w, $ref);"
          case TypeId.Builtins.u64   => q"$binTools.writeU64($w, $ref);"
          case TypeId.Builtins.f32   => q"$binTools.writeF32($w, $ref);"
          case TypeId.Builtins.f64   => q"$binTools.writeF64($w, $ref);"
          case TypeId.Builtins.f128  => q"$binTools.writeDecimal($w, $ref);"
          case TypeId.Builtins.str   => q"$binTools.writeString($w, $ref);"
          case TypeId.Builtins.bytes => q"$binTools.writeBytes($w, $ref);"
          case TypeId.Builtins.uid   => q"$binTools.writeUuid($w, $ref);"
          case TypeId.Builtins.tsu   => q"$binTools.writeTimestampUtc($w, $ref);"
          case TypeId.Builtins.tso   => q"$binTools.writeTimestampOffset($w, $ref);"
          case u: TypeId.User =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Typescript) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, writer)
                  case _ =>
                    val tsType = trans.toTsTypeRefKeepForeigns(u, domain, evo)
                    val fn     = codecFnRef(tsType, "encode_", "_ueba")
                    q"$fn($ref, ctx, $w);"
                }
              case _ =>
                val tsType = trans.toTsTypeRefKeepForeigns(u, domain, evo)
                val fn     = codecFnRef(tsType, "encode_", "_ueba")
                q"$fn($ref, ctx, $w);"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case TypeRef.Constructor(cid, args) =>
        cid match {
          case TypeId.Builtins.opt =>
            q"""if ($ref === undefined) {
               |    $binTools.writeByte($w, 0);
               |} else {
               |    $binTools.writeByte($w, 1);
               |    ${mkEncoder(args.head, ref, writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.lst =>
            q"""$binTools.writeI32($w, Array.from($ref).length);
               |for (const item of $ref) {
               |    ${mkEncoder(args.head, q"item", writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.set =>
            q"""$binTools.writeI32($w, $ref.size);
               |for (const item of $ref) {
               |    ${mkEncoder(args.head, q"item", writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.map =>
            q"""$binTools.writeI32($w, $ref.size);
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
          case TypeId.Builtins.bit   => q"$binTools.readBool(reader)"
          case TypeId.Builtins.i08   => q"$binTools.readI8(reader)"
          case TypeId.Builtins.i16   => q"$binTools.readI16(reader)"
          case TypeId.Builtins.i32   => q"$binTools.readI32(reader)"
          case TypeId.Builtins.i64   => q"$binTools.readI64(reader)"
          case TypeId.Builtins.u08   => q"$binTools.readU8(reader)"
          case TypeId.Builtins.u16   => q"$binTools.readU16(reader)"
          case TypeId.Builtins.u32   => q"$binTools.readU32(reader)"
          case TypeId.Builtins.u64   => q"$binTools.readU64(reader)"
          case TypeId.Builtins.f32   => q"$binTools.readF32(reader)"
          case TypeId.Builtins.f64   => q"$binTools.readF64(reader)"
          case TypeId.Builtins.f128  => q"$binTools.readDecimal(reader)"
          case TypeId.Builtins.str   => q"$binTools.readString(reader)"
          case TypeId.Builtins.bytes => q"$binTools.readBytes(reader)"
          case TypeId.Builtins.uid   => q"$binTools.readUuid(reader)"
          case TypeId.Builtins.tsu   => q"$binTools.readTimestampUtc(reader)"
          case TypeId.Builtins.tso   => q"$binTools.readTimestampOffset(reader)"
          case u: TypeId.User =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Typescript) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ =>
                    val tsType = trans.toTsTypeRefKeepForeigns(u, domain, evo)
                    val fn     = codecFnRef(tsType, "decode_", "_ueba")
                    q"$fn(ctx, reader)"
                }
              case _ =>
                val tsType = trans.toTsTypeRefKeepForeigns(u, domain, evo)
                val fn     = codecFnRef(tsType, "decode_", "_ueba")
                q"$fn(ctx, reader)"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case TypeRef.Constructor(cid, args) =>
        cid match {
          case TypeId.Builtins.opt =>
            q"($binTools.readByte(reader) === 0 ? undefined : ${mkDecoder(args.head)})"
          case TypeId.Builtins.lst =>
            q"Array.from({ length: $binTools.readI32(reader) }, () => ${mkDecoder(args.head)})"
          case TypeId.Builtins.set =>
            q"new Set(Array.from({ length: $binTools.readI32(reader) }, () => ${mkDecoder(args.head)}))"
          case TypeId.Builtins.map =>
            q"new Map(Array.from({ length: $binTools.readI32(reader) }, () => [${mkDecoder(args.head)}, ${mkDecoder(args.last)}] as const))"
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
    }
  }

  def codecName(name: TsValue.TsType): TsValue.TsType = {
    TsValue.TsType(name.module, s"${name.name}_UEBACodec", name.fq)
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Typescript) &&
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
