package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class TsJsonCodecGenerator(
  trans: TsTypeTranslator,
  target: TsTarget,
  domain: Domain,
  evo: BaboonEvolution,
  enquiries: BaboonEnquiries,
) extends TsCodecTranslator {

  import TsTypes.{binTools, baboonDecimal, baboonDateTimeUtc, baboonDateTimeOffset}

  private def codecFnRef(tsType: TsValue.TsType, prefix: String, suffix: String): TsValue.TsType = {
    TsValue.TsType(tsType.module, s"${prefix}${tsType.name}${suffix}")
  }

  override def translate(defn: DomainMember.User, tsRef: TsValue.TsType, srcRef: TsValue.TsType): Option[TextTree[TsValue]] = {
    if (isActive(defn.id) && !enquiries.hasForeignType(defn, domain)) {
      defn.defn match {
        case d: Typedef.Dto     => Some(genDtoCodec(d, srcRef))
        case e: Typedef.Enum    => Some(genEnumCodec(e, srcRef))
        case a: Typedef.Adt     => Some(genAdtCodec(a, srcRef))
        case _: Typedef.Foreign  => None
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }
    } else None
  }

  private def genDtoCodec(dto: Typedef.Dto, name: TsValue.TsType): TextTree[TsValue] = {
    val encodeFields = dto.fields.map { f =>
      val fld = f.name.name
      q""""$fld": ${mkJsonEncoder(f.tpe, q"value.$fld")},"""
    }

    val decodeFields = dto.fields.map { f =>
      val fld = f.name.name
      f.tpe match {
        case TypeRef.Constructor(TypeId.Builtins.opt, _) =>
          q"""$fld: obj["$fld"] === undefined || obj["$fld"] === null ? undefined : ${mkJsonDecoder(f.tpe, q"""obj["$fld"]""")},"""
        case _ =>
          q"""$fld: ${mkJsonDecoder(f.tpe, q"""obj["$fld"]""")},"""
      }
    }

    q"""export function encode_${name.asName}_json(value: ${name.asName}): unknown {
       |    return {
       |        ${encodeFields.joinN().shift(8).trim}
       |    };
       |}
       |
       |export function decode_${name.asName}_json(json: unknown): ${name.asName} {
       |    const obj = json as Record<string, unknown>;
       |    return {
       |        ${decodeFields.joinN().shift(8).trim}
       |    };
       |}""".stripMargin
  }

  private def genEnumCodec(e: Typedef.Enum, name: TsValue.TsType): TextTree[TsValue] = {
    q"""export function encode_${name.asName}_json(value: ${name.asName}): unknown {
       |    return value;
       |}
       |
       |export function decode_${name.asName}_json(json: unknown): ${name.asName} {
       |    return ${name.asName}_parse(json as string);
       |}""".stripMargin
  }

  private def genAdtCodec(adt: Typedef.Adt, name: TsValue.TsType): TextTree[TsValue] = {
    val dataMembers = adt.dataMembers(domain)

    val encCases = dataMembers.map { mid =>
      val branchName = mid.name.name
      val branchType = trans.toTsTypeRefKeepForeigns(mid, domain, evo)
      q"""case "$branchName": return { "$branchName": encode_${branchType.asName}_json(value.value) };"""
    }

    val decCases = dataMembers.map { mid =>
      val branchName = mid.name.name
      val branchType = trans.toTsTypeRefKeepForeigns(mid, domain, evo)
      q"""case "$branchName": return ${name.asName}_$branchName(decode_${branchType.asName}_json(obj[key]));"""
    }

    q"""export function encode_${name.asName}_json(value: ${name.asName}): unknown {
       |    switch (value._tag) {
       |        ${encCases.toList.joinN().shift(8).trim}
       |    }
       |}
       |
       |export function decode_${name.asName}_json(json: unknown): ${name.asName} {
       |    const obj = json as Record<string, unknown>;
       |    const key = Object.keys(obj)[0];
       |    switch (key) {
       |        ${decCases.toList.joinN().shift(8).trim}
       |        default: throw new Error("Unknown ADT branch: " + key);
       |    }
       |}""".stripMargin
  }

  private def mkJsonEncoder(tpe: TypeRef, ref: TextTree[TsValue]): TextTree[TsValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.i64 | TypeId.Builtins.u64 =>
            q"$ref.toString()"
          case TypeId.Builtins.f128 =>
            q"$ref.toString()"
          case TypeId.Builtins.bytes =>
            q"${binTools}.hexEncode($ref)"
          case TypeId.Builtins.tsu =>
            q"$ref.toISOString()"
          case TypeId.Builtins.tso =>
            q"$ref.toISOString()"
          case u: TypeId.User =>
            val tsType = trans.toTsTypeRefKeepForeigns(u, domain, evo)
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, _: Typedef.Enum | _: Typedef.Dto | _: Typedef.Adt, _, _)) =>
                val fn = codecFnRef(tsType, "encode_", "_json")
                q"${fn}($ref)"
              case _ => ref
            }
          case _ => ref
        }
      case TypeRef.Constructor(cid, args) =>
        cid match {
          case TypeId.Builtins.opt =>
            q"$ref === undefined ? null : ${mkJsonEncoder(args.head, ref)}"
          case TypeId.Builtins.lst =>
            q"Array.from($ref).map(item => ${mkJsonEncoder(args.head, q"item")})"
          case TypeId.Builtins.set =>
            q"Array.from($ref).map(item => ${mkJsonEncoder(args.head, q"item")})"
          case TypeId.Builtins.map =>
            val keyType = args.head
            keyType match {
              case TypeRef.Scalar(TypeId.Builtins.str) =>
                q"Object.fromEntries(Array.from($ref.entries()).map(([k, v]) => [k, ${mkJsonEncoder(args.last, q"v")}]))"
              case _ =>
                q"Array.from($ref.entries()).map(([k, v]) => [${mkJsonEncoder(args.head, q"k")}, ${mkJsonEncoder(args.last, q"v")}])"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
    }
  }

  private def mkJsonDecoder(tpe: TypeRef, ref: TextTree[TsValue]): TextTree[TsValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.bit   => q"$ref as boolean"
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 |
               TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 |
               TypeId.Builtins.f32 | TypeId.Builtins.f64 =>
            q"$ref as number"
          case TypeId.Builtins.i64 | TypeId.Builtins.u64 =>
            q"BigInt($ref as string)"
          case TypeId.Builtins.f128 =>
            q"${baboonDecimal}.fromString($ref as string)"
          case TypeId.Builtins.str | TypeId.Builtins.uid =>
            q"$ref as string"
          case TypeId.Builtins.bytes =>
            q"${binTools}.hexDecode($ref as string)"
          case TypeId.Builtins.tsu =>
            q"${baboonDateTimeUtc}.fromISO($ref as string)"
          case TypeId.Builtins.tso =>
            q"${baboonDateTimeOffset}.fromISO($ref as string)"
          case u: TypeId.User =>
            val tsType = trans.toTsTypeRefKeepForeigns(u, domain, evo)
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, _: Typedef.Enum | _: Typedef.Dto | _: Typedef.Adt, _, _)) =>
                val fn = codecFnRef(tsType, "decode_", "_json")
                q"${fn}($ref)"
              case Some(DomainMember.User(_, _: Typedef.Foreign, _, _)) =>
                val mappedType = trans.asTsType(u, domain, evo)
                q"$ref as ${mappedType}"
              case _ => ref
            }
          case o => throw new RuntimeException(s"BUG: Unexpected scalar type: $o")
        }
      case TypeRef.Constructor(cid, args) =>
        cid match {
          case TypeId.Builtins.opt =>
            q"$ref === undefined || $ref === null ? undefined : ${mkJsonDecoder(args.head, ref)}"
          case TypeId.Builtins.lst =>
            q"($ref as unknown[]).map(item => ${mkJsonDecoder(args.head, q"item")})"
          case TypeId.Builtins.set =>
            q"new Set(($ref as unknown[]).map(item => ${mkJsonDecoder(args.head, q"item")}))"
          case TypeId.Builtins.map =>
            val keyType = args.head
            keyType match {
              case TypeRef.Scalar(TypeId.Builtins.str) =>
                q"new Map(Object.entries($ref as Record<string, unknown>).map(([k, v]) => [k, ${mkJsonDecoder(args.last, q"v")}]))"
              case _ =>
                q"new Map(($ref as unknown[][]).map(([k, v]) => [${mkJsonDecoder(args.head, q"k")}, ${mkJsonDecoder(args.last, q"v")}] as const))"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
    }
  }

  def codecName(name: TsValue.TsType): TsValue.TsType = {
    TsValue.TsType(name.module, s"${name.name}_JsonCodec", name.fq)
  }

  override def isActive(id: TypeId): Boolean = {
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Json"
}
