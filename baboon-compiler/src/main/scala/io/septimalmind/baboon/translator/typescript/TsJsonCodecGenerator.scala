package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.typescript.TsTypes.{
  tsBaboonAnyMetaCodec,
  tsBaboonAnyOpaque,
  tsBaboonAnyOpaqueJsonCtor,
  tsBaboonCodecContext,
  tsBaboonDateTimeOffset,
  tsBaboonDateTimeUtc,
  tsBaboonDecimal,
  tsBaboonDecoderFailure,
  tsBaboonEncoderFailure,
  tsBaboonLazy,
  tsBinTools,
}
import io.septimalmind.baboon.translator.typescript.TsValue.TsType
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
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

    val anyHelpers: List[TextTree[TsValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil

    val baseMethods = encodeMethod ++ decodeMethod ++ anyHelpers

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
      q"""${encCases.toList.joinN().shift(4).trim}
         |throw new Error("Unhandled ADT branch: " + (value as {constructor?: {name?: string}}).constructor?.name);""".stripMargin,
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
            target.language.timestampsUtcMode match {
              case "string" => ref
              case "date"   => q"$ref.toISOString()"
              case _        => q"$ref.toISOString()"
            }
          case TypeId.Builtins.tso =>
            target.language.timestampsOffsetMode match {
              case "string" => ref
              case "date"   => q"$ref.toISOString()"
              case _        => q"$ref.toISOString()"
            }
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
            val keyType  = args.head
            val isRecord = trans.isStringKeyMap(tpe)
            keyType match {
              case TypeRef.Scalar(TypeId.Builtins.str) if isRecord =>
                q"Object.fromEntries(Object.entries($ref).map(([k, v]) => [k, ${mkJsonEncoder(args.last, q"v")}]))"
              case TypeRef.Scalar(TypeId.Builtins.str) =>
                q"Object.fromEntries(Array.from($ref.entries()).map(([k, v]) => [k, ${mkJsonEncoder(args.last, q"v")}]))"
              // M19/PR-60: user-type map keys (wrapper/id) emit a string-keyed object via the
              // canonical primitive-string key form (NOT the wrapper's value-position object form).
              // PR-G (M24.2.2): direct-builtin non-string keys also emit string-keyed-object form
              // (was: tuple-array). String() coerces numeric/bool/uid/tsu/tso to a stringy key for
              // cross-language consistency with Scala/Rust/Java/Kotlin/C#/Dart/Swift/Python.
              case _ =>
                q"Object.fromEntries(Array.from($ref.entries()).map(([k, v]) => [${mkJsonKeyEncoder(keyType, q"k")}, ${mkJsonEncoder(args.last, q"v")}]))"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref)
    }
  }

  // M19/PR-60: encoder for user-type map keys. Produces a JS string expression suitable
  // as a JSON object key. Single-primitive wrappers recurse on inner field; ids use toString().
  private def mkJsonKeyEncoder(tpe: TypeRef, ref: TextTree[TsValue]): TextTree[TsValue] = tpe match {
    case TypeRef.Scalar(u: TypeId.User) =>
      domain.defs.meta.nodes.get(u) match {
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.isIdentifier =>
          q"$ref.toString()"
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.fields.size == 1 && d.contracts.isEmpty =>
          val inner = d.fields.head
          mkJsonKeyEncoder(inner.tpe, q"$ref.${inner.name.name}")
        case Some(DomainMember.User(_, _: Typedef.Enum, _, _)) =>
          // PR-G (M24.2.2): enum keys — generated TS enums are string enums; the value itself
          // is a string suitable as a JSON object key.
          ref
        case _ =>
          // PR-60-D04: validator (PR-59) should reject any other user-type as a map key. If we
          // land here, the validator missed a case — fail loudly rather than silently emit
          // value-position output that may not be a string.
          q"""(() => { throw new Error("BUG: Unexpected key usertype (validator should have rejected): " + JSON.stringify($ref)) })()"""
      }
    case _ =>
      // Builtin scalar key — use String coercion of the value-position encoded form.
      q"String(${mkJsonEncoder(tpe, ref)})"
  }

  // M19/PR-60: decoder for user-type map keys. Single-primitive wrappers recurse
  // and construct via `new Wrapper({field: parsed})`; ids use codecObj.parseRepr.
  private def mkJsonKeyDecoder(tpe: TypeRef, ref: TextTree[TsValue]): TextTree[TsValue] = tpe match {
    case TypeRef.Scalar(u: TypeId.User) =>
      domain.defs.meta.nodes.get(u) match {
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.isIdentifier =>
          val tsType   = trans.asTsTypeDerefForeign(u, domain, evo, tsFileTools.definitionsBasePkg)
          val codecObj = tsType.name.head.toLower.toString + tsType.name.tail + "Codec"
          val codecRef = TsValue.TsType(tsType.moduleId, codecObj)
          // PR-F (M24): throw BaboonDecoderFailure on Left for cross-language malformed-key
          // consistency (replaces unchecked `as unknown as { tag: "Right" }` cast).
          q"""((): $tsType => { const __e = $codecRef.parseRepr($ref); if (__e.tag === "Right") return __e.value; throw new $tsBaboonDecoderFailure("malformed key: " + $ref); })()"""
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.fields.size == 1 && d.contracts.isEmpty =>
          val inner    = d.fields.head
          val tsType   = trans.asTsTypeDerefForeign(u, domain, evo, tsFileTools.definitionsBasePkg)
          val innerDec = mkJsonKeyDecoder(inner.tpe, ref)
          q"new $tsType($innerDec)"
        case Some(DomainMember.User(_, _: Typedef.Enum, _, _)) =>
          // PR-G (M24.2.2): enum keys — parse via the generated `<EnumName>_parse` helper,
          // matching the value-position enum decoder shape.
          val tsType = trans.asTsTypeDerefForeign(u, domain, evo, tsFileTools.definitionsBasePkg)
          val parser = TsValue.TsType(tsType.moduleId, s"${tsType.name}_parse")
          q"$parser($ref)"
        case _ =>
          // Validator (PR-59) should reject any other user-type as a map key. If we land here, the
          // validator missed a case — fail loudly with a defensive throw rather than emit silently
          // wrong code (PR-60-D04).
          q"""(() => { throw new Error("BUG: Unexpected key usertype (validator should have rejected): " + JSON.stringify($ref)) })()"""
      }
    case _ =>
      // PR-60-D01: builtin scalar key — `ref` is a string at runtime (from `Object.entries`/keys).
      // The value-position `mkJsonDecoder` emits TS type-system casts (`$ref as number`) that do NOT
      // parse the string at runtime. Emit the explicit JS-runtime parse for each primitive.
      parsePrimitiveKey(tpe, ref)
  }

  // PR-60-D01: parse a JS string into the typed primitive value for a map-key position. Used by
  // `mkJsonKeyDecoder` only — the value-position decoder consumes already-typed JSON values from
  // the parsed wire form, while keys arrive as strings (`Object.keys`/`Object.entries` outputs).
  private def parsePrimitiveKey(tpe: TypeRef, ref: TextTree[TsValue]): TextTree[TsValue] = tpe match {
    case TypeRef.Scalar(id) =>
      id match {
        case TypeId.Builtins.bit =>
          q"""((__r: string) => { if (__r === "true") return true; if (__r === "false") return false; throw new $tsBaboonDecoderFailure("malformed key: " + __r); })($ref)"""
        case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 | TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 =>
          q"""((__r: string) => { const __n = parseInt(__r, 10); if (Number.isNaN(__n) || String(__n) !== __r) throw new $tsBaboonDecoderFailure("malformed key: " + __r); return __n; })($ref)"""
        case TypeId.Builtins.i64 | TypeId.Builtins.u64 =>
          q"""((__r: string) => { try { return BigInt(__r); } catch (_e) { throw new $tsBaboonDecoderFailure("malformed key: " + __r); } })($ref)"""
        case TypeId.Builtins.f32 | TypeId.Builtins.f64 =>
          q"""((__r: string) => { const __n = parseFloat(__r); if (Number.isNaN(__n)) throw new $tsBaboonDecoderFailure("malformed key: " + __r); return __n; })($ref)"""
        case TypeId.Builtins.f128 =>
          q"$tsBaboonDecimal.fromString($ref)"
        case TypeId.Builtins.str | TypeId.Builtins.uid =>
          ref
        case TypeId.Builtins.bytes =>
          q"$tsBinTools.hexDecode($ref)"
        case TypeId.Builtins.tsu =>
          target.language.timestampsUtcMode match {
            case "string" => ref
            case "date"   => q"""((__r: string) => { const __d = new Date(__r); if (Number.isNaN(__d.getTime())) throw new $tsBaboonDecoderFailure("malformed key: " + __r); return __d; })($ref)"""
            case _        => q"$tsBaboonDateTimeUtc.fromISO($ref)"
          }
        case TypeId.Builtins.tso =>
          target.language.timestampsOffsetMode match {
            case "string" => ref
            case "date"   => q"""((__r: string) => { const __d = new Date(__r); if (Number.isNaN(__d.getTime())) throw new $tsBaboonDecoderFailure("malformed key: " + __r); return __d; })($ref)"""
            case _        => q"$tsBaboonDateTimeOffset.fromISO($ref)"
          }
        case o => throw new RuntimeException(s"BUG: Unexpected primitive key type: $o")
      }
    case o => throw new RuntimeException(s"BUG: Non-scalar in primitive-key position: $o")
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
            target.language.timestampsUtcMode match {
              case "string" => q"$ref as string"
              case "date"   => q"new Date($ref as string)"
              case _        => q"$tsBaboonDateTimeUtc.fromISO($ref as string)"
            }
          case TypeId.Builtins.tso =>
            target.language.timestampsOffsetMode match {
              case "string" => q"$ref as string"
              case "date"   => q"new Date($ref as string)"
              case _        => q"$tsBaboonDateTimeOffset.fromISO($ref as string)"
            }
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
            val keyType  = args.head
            val isRecord = trans.isStringKeyMap(tpe)
            keyType match {
              case TypeRef.Scalar(TypeId.Builtins.str) if isRecord =>
                q"Object.fromEntries(Object.entries($ref as Record<string, unknown>).map(([k, v]) => [k, ${mkJsonDecoder(args.last, q"v")}]))"
              case TypeRef.Scalar(TypeId.Builtins.str) =>
                q"new Map(Object.entries($ref as Record<string, unknown>).map(([k, v]) => [k, ${mkJsonDecoder(args.last, q"v")}]))"
              // M19/PR-60: user-type map keys decode from the string-keyed object emitted by
              // `mkJsonKeyEncoder` — pair-up entries and reconstruct typed keys via parseRepr/peel.
              // PR-G (M24.2.2): direct-builtin non-string keys decode from the same string-keyed
              // object form (was: tuple-array). `mkJsonKeyDecoder` dispatches to `parsePrimitiveKey`
              // for builtin scalars (parseInt/BigInt/parseFloat/Date/etc).
              case _ =>
                q"new Map(Object.entries($ref as Record<string, unknown>).map(([k, v]) => [${mkJsonKeyDecoder(keyType, q"k")}, ${mkJsonDecoder(args.last, q"v")}] as const))"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a, ref)
    }
  }

  // Deep walk (mirrors Scala/C#/Rust/Kotlin/Java/TS-UEBA `hasAnyField`): a codec class needs the
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

  // Encode delegates to the per-codec-class `encodeAnyField` helper. Wires the expected kind byte
  // and the field's static (codec-gen-time) fallbacks for cross-format meta resolution.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[TsValue]): TextTree[TsValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"this.encodeAnyField(ctx, $expectedHex, $staticDom, $staticVer, $staticTid, $ref)"
  }

  // Decode delegates to the per-codec-class `decodeAnyField` helper. JSON decode never cross-
  // converts (always returns `AnyOpaqueJson` from JSON wire); user calls `facade.decodeAny(opaque)`
  // for typed resolution. No `ctx` / no static fallbacks needed at the decode site.
  private def mkAnyDecoder(a: TypeRef.Any, ref: TextTree[TsValue]): TextTree[TsValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"this.decodeAnyField($expectedHex, $ref)"
  }

  // Static fallbacks for the cross-format facade helper (`uebaToJson`). The wire `meta` may omit
  // components that are pinned by the field's static declaration; the codec emits whatever is
  // statically known so the facade can fill the gaps. Per spec table:
  //   A=(undef,undef,undef), B=(currentDomain,undef,undef), C=(currentDomain,currentVersion,undef),
  //   D1=(undef,undef,underlyingFqid), D2=(currentDomain,undef,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // Duplicated across Scala/C#/Rust/Kotlin/Java/TS — extraction deferred (textual emission diverges
  // by language flavor; see PR 4.2 ledger entry's DRY analysis). 9th instance.
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

  // Per-codec-class helpers consolidating the any-field JSON envelope encode/decode (kind check,
  // cross-format conversion via facade, envelope `$ak/$ad/$av/$at/$c` build & disassemble).
  // Emitted at most once per codec class that has any any-bearing field. Mirrors PR 6.3's Java
  // helper shape (closest precedent: JsonNode-style API). PR-08-D06 lesson: kind-check on encode
  // runs before any envelope construction.
  private def anyFieldHelpers: TextTree[TsValue] = {
    q"""private encodeAnyField(
       |    ctx: $tsBaboonCodecContext,
       |    expectedKind: number,
       |    staticDomain: string | undefined,
       |    staticVersion: string | undefined,
       |    staticTypeid: string | undefined,
       |    value: $tsBaboonAnyOpaque,
       |): unknown {
       |    if (value.meta.kind !== expectedKind) {
       |        throw new $tsBaboonEncoderFailure(
       |            `any: meta-kind 0x$${(value.meta.kind & 0xFF).toString(16).padStart(2, "0")} does not match field-declared 0x$${(expectedKind & 0xFF).toString(16).padStart(2, "0")}`
       |        );
       |    }
       |    let anyInner: unknown;
       |    if (value.tag === "Json") {
       |        anyInner = value.json;
       |    } else {
       |        const anyFacade = ctx.facade;
       |        if (anyFacade === undefined) {
       |            throw new $tsBaboonEncoderFailure(
       |                "Cannot encode AnyOpaqueUeba into JSON without a facade reference. Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), or supply anyOpaqueJson directly."
       |            );
       |        }
       |        const anyConvResult = anyFacade.uebaToJson(value.meta, value.bytes, staticDomain, staticVersion, staticTypeid);
       |        if (anyConvResult.tag === "Left") {
       |            throw anyConvResult.value;
       |        }
       |        anyInner = anyConvResult.value;
       |    }
       |    const anyEnvelope = $tsBaboonAnyMetaCodec.writeJson(value.meta);
       |    anyEnvelope[$tsBaboonAnyMetaCodec.ANY_CONTENT_KEY] = anyInner;
       |    return anyEnvelope;
       |}
       |
       |private decodeAnyField(expectedKind: number, wire: unknown): $tsBaboonAnyOpaque {
       |    if (wire === null || typeof wire !== "object" || Array.isArray(wire)) {
       |        throw new $tsBaboonDecoderFailure("any: JSON envelope must be an object");
       |    }
       |    const anyMetaResult = $tsBaboonAnyMetaCodec.readJson(wire);
       |    if (anyMetaResult.tag === "Left") {
       |        throw anyMetaResult.value;
       |    }
       |    const anyMeta = anyMetaResult.value;
       |    if (anyMeta.kind !== expectedKind) {
       |        throw new $tsBaboonDecoderFailure(
       |            `any: wire kind 0x$${(anyMeta.kind & 0xFF).toString(16).padStart(2, "0")} does not match field-declared 0x$${(expectedKind & 0xFF).toString(16).padStart(2, "0")}`
       |        );
       |    }
       |    const anyObj = wire as Record<string, unknown>;
       |    const anyContent = anyObj[$tsBaboonAnyMetaCodec.ANY_CONTENT_KEY];
       |    if (anyContent === undefined) {
       |        throw new $tsBaboonDecoderFailure(`any: JSON envelope missing '$${$tsBaboonAnyMetaCodec.ANY_CONTENT_KEY}' content key`);
       |    }
       |    return $tsBaboonAnyOpaqueJsonCtor(anyMeta, anyContent);
       |}""".stripMargin
  }

  def codecName(name: TsValue.TsType): TsValue.TsType = {
    TsValue.TsType(name.moduleId, s"${name.name}_JsonCodec")
  }

  override def codecMeta(definition: DomainMember.User, name: TsValue.TsType): Option[TextTree[TsValue]] = {
    if (isActive(definition.id) && !enquiries.hasForeignType(definition, domain)) {
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
