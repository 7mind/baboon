package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.translator.typescript.TsTypes.{tsBaboonCodecContext, tsBaboonDateTimeOffset, tsBaboonDateTimeUtc, tsBaboonDecimal, tsBaboonDecoderFailure, tsBaboonEncoderFailure, tsBaboonLazy, tsBinTools, tsDecodeAnyJsonField, tsEncodeAnyJsonField}
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
  private val scalarOps = new TsScalarCodecOps(target)
  override def translate(defn: DomainMember.User, tsRef: TsValue.TsType, srcRef: TsValue.TsType): Option[TextTree[TsValue]] = {
    // PR-I.1d (M24 Phase 3.1): the prior `&& !enquiries.hasForeignType(defn, domain)` short-circuit
    // suppressed JsonCodec emission for any type containing a Custom-bound foreign — including
    // single-primitive wrappers like `ItemKey { v: FStr }` and `Holder { m: map[ItemKey, str] }`.
    // With the new `<F>_KeyCodecHost` hook (emitted from `TsDefnTranslator`'s foreign emit site),
    // foreign keys flow through the registered impl, so the codec can be generated unconditionally.
    // The UEBA gate in `TsUEBACodecGenerator` is left intact pending a UEBA-side hook (out of scope).
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto     => Some(genDtoCodec(d, srcRef))
        case _: Typedef.Enum    => Some(genEnumCodec(srcRef))
        case a: Typedef.Adt     => Some(genAdtCodec(a, srcRef))
        case f: Typedef.Foreign =>
          // Mirror C# (CSJsonCodecGenerator): a Custom-bound foreign emits a value codec class whose
          // encode/decode throw by default; the host application overrides it via `lazyInstance`.
          // BaboonRef-bound foreigns (and unbound) emit nothing — value sites recurse through the alias.
          f.bindings.get(BaboonLang.Typescript) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) => Some(genForeignBodies(srcRef))
            case _                                                                  => None
          }
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
    // Codec class is named after the keep-foreigns source ref (the baboon type name), not the mapped
    // value type `name` — for foreigns `name` is the mapped host type (e.g. `string` or an import
    // expression) which cannot form a class identifier. For non-foreign types the two coincide.
    val cName = codecName(srcRef)

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
       |    public static lazyInstance = new $tsBaboonLazy(() => new $cName())
       |    public static get instance(): $cName {
       |        return $cName.lazyInstance.value
       |    }
       |}""".stripMargin
  }

  // Throwing default bodies for a Custom-bound foreign value codec (mirrors C# `genForeignBodies`).
  // The host application supplies a real codec by reassigning `<F>_JsonCodec.lazyInstance`; the
  // `if (this !== lazyInstance.value)` guard in `genCodec` then routes encode/decode to it.
  private def genForeignBodies(srcRef: TsValue.TsType): (TextTree[TsValue], TextTree[TsValue]) = {
    val fqn = s"${(srcRef.moduleId.path :+ codecName(srcRef).name).mkString(".")}"
    val msg = s"$fqn is a foreign type with no built-in codec; provide one via $fqn.lazyInstance = new Lazy(() => yourCodec)."
    (
      q"""throw new $tsBaboonEncoderFailure("$msg");""",
      q"""throw new $tsBaboonDecoderFailure("$msg");""",
    )
  }

  private def genDtoCodec(dto: Typedef.Dto, name: TsValue.TsType): (TextTree[TsValue], TextTree[TsValue]) = {
    val encodeFields = dto.fields.map {
      f =>
        val fld        = f.name.name
        val escapedFld = trans.escapeTsKeyword(fld)
        // Wire key is always the original model name; the getter may be renamed when fld is a TS keyword.
        q""""$fld": ${mkJsonEncoder(f.tpe, q"value.$escapedFld")},"""
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
          q"""if (value instanceof $branchType) {
             |    return $branchCodec.instance.encode($tsBaboonCodecContext.Default, value)
             |}""".stripMargin
        } else {
          q"""if (value instanceof $branchType) {
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
          case b: TypeId.BuiltinScalar => scalarOps.encodeJson(b, ref)
          case u: TypeId.User =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Typescript) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkJsonEncoder(aliasedRef, ref)
                  case _ =>
                    // Custom foreign value: route through the emitted `<F>_JsonCodec` (throws unless the
                    // host registered an impl via lazyInstance). Naming uses keep-foreigns to match the
                    // codec class emitted in the foreign's own module.
                    val codec = codecName(trans.asTsTypeKeepForeigns(u, domain, evo, tsFileTools.definitionsBasePkg))
                    q"$codec.instance.encode($tsBaboonCodecContext.Default, $ref)"
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
          case TypeId.Builtins.lst | TypeId.Builtins.set =>
            val item    = q"item"
            val encoded = mkJsonEncoder(args.head, item)
            if (encoded == item) q"Array.from($ref)" else q"Array.from($ref).map(item => $encoded)"
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
        case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
          f.bindings.get(BaboonLang.Typescript) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
              mkJsonKeyEncoder(aliasedRef, ref)
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
              // PR-I.1d (M24 Phase 3.1): Custom-foreign map keys route through
              // the emitted `<Foreign>_KeyCodecHost.instance` extension hook.
              val srcRef  = trans.asTsTypeKeepForeigns(u, domain, evo, tsFileTools.definitionsBasePkg)
              val hostTpe = TsValue.TsType(srcRef.moduleId, s"${srcRef.name}_KeyCodecHost")
              q"$hostTpe.instance.encodeKey($ref)"
            case None =>
              throw new RuntimeException(s"BUG: Foreign type $u has no TypeScript binding")
          }
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
        case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
          f.bindings.get(BaboonLang.Typescript) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
              mkJsonKeyDecoder(aliasedRef, ref)
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
              // PR-I.1d (M24 Phase 3.1): Custom-foreign map keys route through
              // the emitted `<Foreign>_KeyCodecHost.instance` extension hook.
              // `catch (e: unknown)` (PR-I-D01 pattern guidance for TS): TS catches
              // are `unknown`-typed by tsconfig `useUnknownInCatchVariables` default;
              // we still discriminate Error-vs-everything-else only at the throw site.
              val srcRef  = trans.asTsTypeKeepForeigns(u, domain, evo, tsFileTools.definitionsBasePkg)
              val mapped  = trans.asTsType(u, domain, evo)
              val hostTpe = TsValue.TsType(srcRef.moduleId, s"${srcRef.name}_KeyCodecHost")
              q"""((): $mapped => { try { return $hostTpe.instance.decodeKey($ref); } catch (e) { throw new $tsBaboonDecoderFailure("malformed key: " + $ref, { cause: e }); } })()"""
            case None =>
              throw new RuntimeException(s"BUG: Foreign type $u has no TypeScript binding")
          }
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
            case "date" =>
              q"""((__r: string) => { const __d = new Date(__r); if (Number.isNaN(__d.getTime())) throw new $tsBaboonDecoderFailure("malformed key: " + __r); return __d; })($ref)"""
            case _ => q"$tsBaboonDateTimeUtc.fromISO($ref)"
          }
        case TypeId.Builtins.tso =>
          target.language.timestampsOffsetMode match {
            case "string" => ref
            case "date" =>
              q"""((__r: string) => { const __d = new Date(__r); if (Number.isNaN(__d.getTime())) throw new $tsBaboonDecoderFailure("malformed key: " + __r); return __d; })($ref)"""
            case _ => q"$tsBaboonDateTimeOffset.fromISO($ref)"
          }
        case o => throw new RuntimeException(s"BUG: Unexpected primitive key type: $o")
      }
    case o => throw new RuntimeException(s"BUG: Non-scalar in primitive-key position: $o")
  }

  private def mkJsonDecoder(tpe: TypeRef, ref: TextTree[TsValue]): TextTree[TsValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case b: TypeId.BuiltinScalar => scalarOps.decodeJson(b, ref)
          case u: TypeId.User =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Typescript) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkJsonDecoder(aliasedRef, ref)
                  case _ =>
                    // Custom foreign value: route through the emitted `<F>_JsonCodec` (throws unless the
                    // host registered an impl via lazyInstance), replacing the prior no-op `as` cast.
                    val codec = codecName(trans.asTsTypeKeepForeigns(u, domain, evo, tsFileTools.definitionsBasePkg))
                    q"$codec.instance.decode($tsBaboonCodecContext.Default, $ref)"
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

  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[TsValue]): TextTree[TsValue] = {
    val plan = TsAnyFieldPlan.forField(a, domain)
    q"$tsEncodeAnyJsonField(ctx, ${plan.kindHex}, ${plan.staticDomain}, ${plan.staticVersion}, ${plan.staticTypeId}, $ref)"
  }

  private def mkAnyDecoder(a: TypeRef.Any, ref: TextTree[TsValue]): TextTree[TsValue] = {
    val plan = TsAnyFieldPlan.forField(a, domain)
    q"$tsDecodeAnyJsonField(${plan.kindHex}, $ref)"
  }

  def codecName(name: TsValue.TsType): TsValue.TsType = {
    TsValue.TsType(name.moduleId, s"${name.name}_JsonCodec")
  }

  override def codecMeta(definition: DomainMember.User, name: TsValue.TsType): Option[TextTree[TsValue]] = {
    // PR-I.1d (M24 Phase 3.1): see PR-I.1d note in `translate()` above — codecMeta now mirrors the
    // codec emission gate so the metadata accessor is co-emitted with the codec class.
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

  override def isActive(id: TypeId): Boolean = TsCodecActivation.isActive(target, domain, id, TsCodecActivation.Json)

  override def id: String = "Json"
}
