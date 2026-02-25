package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.typescript.TsTypes.{tsBaboonCodecContext, tsBaboonDateTimeOffset, tsBaboonDateTimeUtc, tsBaboonDecimal, tsBaboonLazy, tsBinTools}
import io.septimalmind.baboon.translator.typescript.TsValue.TsType
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
  tsFileTools: TsFileTools,
  tsDomainTreeTools: TsDomainTreeTools,
) extends TsCodecTranslator {
  override def translate(defn: DomainMember.User, tsRef: TsValue.TsType, srcRef: TsValue.TsType): Option[TextTree[TsValue]] = {
    if (isActive(defn.id) && !enquiries.hasForeignType(defn, domain)) {
      (defn.defn match {
        case d: Typedef.Dto      => Some(genDtoCodec(d, srcRef))
        case _: Typedef.Enum     => Some(genEnumCodec(srcRef))
        case a: Typedef.Adt      => Some(genAdtCodec(a, srcRef))
        case _: Typedef.Foreign  => None
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) => genCodec(defn, tsRef, srcRef, enc, dec)
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: TsType,
    srcRef: TsType,
    enc: TextTree[TsValue],
    dec: TextTree[TsValue],
  ): TextTree[TsValue] = {
    val cName = codecName(name)

    val encodeMethod =
      List(
        q"""public encode(ctx: $tsBaboonCodecContext, value: $name): unknown {
           |    if (this !== $cName.lazyInstance.value) {
           |      return $cName.lazyInstance.value.encode(ctx, value)
           |    }
           |
           |    ${enc.shift(4).trim}
           |}
           |""".stripMargin.trim
      )

    val decodeMethod =
      List(
        q"""public decode(ctx: $tsBaboonCodecContext, json: unknown): $name {
           |    if (this !== $cName .lazyInstance.value) {
           |        return $cName.lazyInstance.value.decode(ctx, json)
           |    }
           |
           |    ${dec.shift(4).trim}
           |}""".stripMargin.trim
      )

    val baseMethods = encodeMethod ++ decodeMethod

    val meta = tsDomainTreeTools.makeCodecMeta(defn, codecName(srcRef))

    q"""export class $cName {
       |    ${baseMethods.joinN().shift(4).trim}
       |
       |    ${meta.joinN().shift(4).trim}
       |
       |    protected static lazyInstance = new $tsBaboonLazy(() => new $cName())
       |    public static get instance(): $cName {
       |        return $cName.lazyInstance.value
       |    }
       |}""".stripMargin
  }

  private def genDtoCodec(dto: Typedef.Dto, name: TsValue.TsType): (TextTree[TsValue], TextTree[TsValue]) = {
    val encodeFields = dto.fields.map {
      f =>
        val fld = f.name.name
        q""""$fld": ${mkJsonEncoder(f.tpe, q"value.$fld")},"""
    }

    val decodeFields = dto.fields.map {
      f =>
        val fld = f.name.name
        f.tpe match {
          case TypeRef.Constructor(TypeId.Builtins.opt, _) =>
            q"""obj["$fld"] === undefined || obj["$fld"] === null ? undefined : ${mkJsonDecoder(f.tpe, q"""obj["$fld"]""")},"""
          case _ =>
            q"""${mkJsonDecoder(f.tpe, q"""obj["$fld"]""")},"""
        }
    }

    val mainEnc = q"""{
                     |    ${encodeFields.joinN().shift(4).trim}
                     |}""".stripMargin

    val fullEnc = dto.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        q"""{ "${dto.id.name.name}": $mainEnc }"""
      case _ => mainEnc
    }

    (
      q"return $fullEnc",
      q"""const obj = json as Record<string, unknown>;
         |return new $name (
         |    ${decodeFields.joinN().shift(4).trim}
         |)""".stripMargin,
    )
  }

  private def genEnumCodec(name: TsValue.TsType): (TextTree[TsValue], TextTree[TsValue]) = {
    (
      q"return value",
      q"return ${name.name}_parse(json as string)",
    )
  }

  private def genAdtCodec(adt: Typedef.Adt, name: TsValue.TsType): (TextTree[TsValue], TextTree[TsValue]) = {
    val dataMembers = adt.dataMembers(domain)

    val encCases = dataMembers.map {
      mid =>
        val branchName  = mid.name.name
        val branchType  = trans.asTsTypeDerefForeign(mid, domain, evo, tsFileTools.definitionsBasePkg)
        val branchCodec = codecName(branchType)
        if (target.language.wrappedAdtBranchCodecs) {
          q"""if (value instanceof $branchName) {
             |    return $branchCodec.instance.encode($tsBaboonCodecContext.Default, value)
             |}""".stripMargin
        } else {
          q"""if (value instanceof $branchName) {
             |    return { "$branchName": $branchCodec.instance.encode($tsBaboonCodecContext.Default, value) }
             |}""".stripMargin
        }
    }

    val decCases = dataMembers.map {
      mid =>
        val branchName  = mid.name.name
        val branchType  = trans.asTsTypeKeepForeigns(mid, domain, evo, tsFileTools.definitionsBasePkg)
        val branchCodec = codecName(branchType)
        q"""case "$branchName": return $branchCodec.instance.decode($tsBaboonCodecContext.Default, obj[key])"""
    }

    (
      q"${encCases.toList.joinN().shift(4).trim}",
      q"""const obj = json as Record<string, unknown>;
         |const key = Object.keys(obj)[0];
         |switch (key) {
         |    ${decCases.toList.joinN().shift(8).trim}
         |    default: throw new Error("Unknown ADT branch: " + key);
         |}""".stripMargin,
    )
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
            q"$tsBinTools.hexEncode($ref)"
          case TypeId.Builtins.tsu =>
            q"$ref.toISOString()"
          case TypeId.Builtins.tso =>
            q"$ref.toISOString()"
          case u: TypeId.User =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Typescript) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkJsonEncoder(aliasedRef, ref)
                  case _ => ref
                }
              case Some(DomainMember.User(_, _: Typedef.Enum | _: Typedef.Dto | _: Typedef.Adt, _, _)) =>
                val tsType = trans.asTsTypeDerefForeign(u, domain, evo, tsFileTools.definitionsBasePkg)
                val codec  = codecName(tsType)
                q"$codec.instance.encode($tsBaboonCodecContext.Default, $ref)"
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
          case TypeId.Builtins.bit => q"$ref as boolean"
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 | TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 | TypeId.Builtins.f32 |
              TypeId.Builtins.f64 =>
            q"$ref as number"
          case TypeId.Builtins.i64 | TypeId.Builtins.u64 =>
            q"BigInt($ref as string)"
          case TypeId.Builtins.f128 =>
            q"$tsBaboonDecimal.fromString($ref as string)"
          case TypeId.Builtins.str | TypeId.Builtins.uid =>
            q"$ref as string"
          case TypeId.Builtins.bytes =>
            q"$tsBinTools.hexDecode($ref as string)"
          case TypeId.Builtins.tsu =>
            q"$tsBaboonDateTimeUtc.fromISO($ref as string)"
          case TypeId.Builtins.tso =>
            q"$tsBaboonDateTimeOffset.fromISO($ref as string)"
          case u: TypeId.User =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Typescript) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkJsonDecoder(aliasedRef, ref)
                  case _ =>
                    val mappedType = trans.asTsType(u, domain, evo)
                    q"$ref as $mappedType"
                }
              case Some(DomainMember.User(_, _: Typedef.Enum | _: Typedef.Dto | _: Typedef.Adt, _, _)) =>
                val tsType = trans.asTsTypeDerefForeign(u, domain, evo, tsFileTools.definitionsBasePkg)
                val codec  = codecName(tsType)
                q"$codec.instance.decode($tsBaboonCodecContext.Default, $ref)"
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
                q"new Map(Array.isArray($ref) ? ($ref as unknown[][]).map(([k, v]) => [${mkJsonDecoder(args.head, q"k")}, ${mkJsonDecoder(args.last, q"v")}] as const) : [])"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
    }
  }

  def codecName(name: TsValue.TsType): TsValue.TsType = {
    TsValue.TsType(name.moduleId, s"${name.name}_JsonCodec")
  }

  override def codecMeta(definition: DomainMember.User, name: TsValue.TsType): Option[TextTree[TsValue]] = {
    if (isActive(definition.id)) {
      definition.defn match {
        case _: Typedef.Adt =>
          Some(q"""jsonCodec(): ${codecName(name)} {
                  |    return ${codecName(name)}.instance
                  |}""".stripMargin)
        case _ =>
          Some(q"""public static jsonCodec(): ${codecName(name)} {
                  |    return ${codecName(name)}.instance
                  |}""".stripMargin)
      }
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Typescript) &&
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Json"
}
