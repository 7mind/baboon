package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.PyType
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

final class PyJsonCodecGenerator(
  typeTranslator: PyTypeTranslator,
  treeTools: PyDomainTreeTools,
  pyFileTools: PyFileTools,
  evolution: BaboonEvolution,
  pyTarget: PyTarget,
  domain: Domain,
) extends PyCodecTranslator {
  override def translate(defn: DomainMember.User, pyRef: PyType, srcRef: PyType): Option[TextTree[PyValue]] = {
    (defn.defn match {
      case d: Typedef.Dto  => Some(genDtoBodies(pyRef, d))
      case _: Typedef.Adt  => Some(genAdtBodies(pyRef))
      case _: Typedef.Enum => Some(genEnumBodies(pyRef))
      case f: Typedef.Foreign =>
        f.bindings.get(BaboonLang.Py) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
          case _                                                                  => Some(genForeignTypesBodies(pyRef))
        }
      case _: Typedef.Service  => None
      case _: Typedef.Contract => None
    }).map {
      case (enc, dec) => genCodec(defn, pyRef, srcRef, enc, dec)
    }
  }

  private def genCodec(
    defn: DomainMember.User,
    name: PyType,
    srcRef: PyType,
    enc: TextTree[PyValue],
    dec: TextTree[PyValue],
  ): TextTree[PyValue] = {
    val isEncoderEnabled = pyTarget.language.enableDeprecatedEncoders || domain.version == evolution.latest
    val encodeMethod = if (isEncoderEnabled) {
      List(
        q"""def encode(self, context: $baboonCodecContext, value: $name) -> $pyStr:
           |    ${enc.shift(4).trim}
           |""".stripMargin.trim
      )
    } else Nil
    val decodeMethod =
      List(q"""def decode(self, context: $baboonCodecContext, wire: $pyStr) -> $name:
              |    ${dec.shift(4).trim}
              |""".stripMargin)
    val anyHelpers: List[TextTree[PyValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil
    val baseMethods = encodeMethod ++ decodeMethod ++ anyHelpers
    val cName       = q"${srcRef.name}_JsonCodec"
    val cType       = q"'${codecType(defn.id)}'"

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecBase[$name, $cType]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecBase[$name, $cType]"
        case _ if defn.ownedByAdt                           => q"$baboonJsonCodecBaseGeneratedAdt[$name, $cType]"
        case _                                              => q"$baboonJsonCodecBaseGenerated[$name, $cType]"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecNoEncoder[$name, $cType]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecNoEncoder[$name, $cType]"
        case _ if defn.ownedByAdt                           => q"$baboonJsonCodecNoEncoderGeneratedAdt[$name, $cType]"
        case _                                              => q"$baboonJsonCodecNoEncoderGenerated[$name, $cType]"
      }
    }

    q"""class $cName($cParent):
       |    ${baseMethods.joinN().shift(4).trim}
       |
       |    ${treeTools.makeCodecMeta(defn).joinN().shift(4).trim}
       |
       |    def target_type(self) -> $pyType:
       |        return $name
       |
       |    _lazy_instance: $baboonLazy['$cName'] = $baboonLazy(lambda: $cName())
       |""".stripMargin
  }

  private def genForeignTypesBodies(name: PyType): (TextTree[PyValue], TextTree[PyValue]) = {
    (
      q"""raise ValueError(f"$name is a foreign type")""",
      q"""raise ValueError(f"$name is a foreign type")""",
    )
  }

  private def genEnumBodies(name: PyType): (TextTree[PyValue], TextTree[PyValue]) = {
    val encode = q"""return $pyJsonDumps(value.value)""".stripMargin
    val decode = q"""return $name($pyJsonLoads(wire))""".stripMargin
    (encode, decode)
  }

  private def genAdtBodies(name: PyType): (TextTree[PyValue], TextTree[PyValue]) = {
    val encode = q"""return value.model_dump_json()""".stripMargin
    val decode = q"""return $name.model_validate_json(wire)""".stripMargin
    (encode, decode)
  }

  private def genDtoBodies(name: PyType, dto: Typedef.Dto): (TextTree[PyValue], TextTree[PyValue]) = {
    // For DTOs without `any`-bearing fields AND without `map[user-key, V]` fields, fall through to
    // pydantic's transparent `model_dump_json` / `model_validate_json` round-trip — pydantic knows
    // every field type. For DTOs WITH `any`-bearing fields, pydantic cannot round-trip an
    // `AnyOpaque` instance:
    //   - `model_dump_json` doesn't know how to serialize the runtime ABC.
    //   - `model_validate_json` cannot reconstruct it from a raw envelope dict.
    // PR-60-D02: For DTOs containing `map[user-key, V]` fields (where the key is a user DTO/id/
    // foreign), pydantic's `model_dump_json` is unreliable across versions for keys (does not
    // deterministically use `__str__`) and `model_validate_json` cannot coerce a string key back
    // into a Pydantic model (no field validator emitted).
    // In both cases we split the DTO around the affected fields: pydantic handles the rest via
    // `model_dump(mode='json', exclude=excluded_fields)` / `model_validate(obj_with_patches)`,
    // and we patch in the explicit-walker output manually. The resulting dict is then serialised
    // with `json.dumps`. Direct counterpart of the per-field walks emitted by Java (PR 6.3) /
    // TS (PR 7.3) / Dart (PR 8.3) / Swift (PR 9.3) JSON generators.
    if (!dtoNeedsExplicitWalker(dto)) {
      val encode = q"""return value.model_dump_json()""".stripMargin
      val decode = q"""return $name.model_validate_json(wire)""".stripMargin
      (encode, decode)
    } else {
      val walkedFieldNames = dto.fields.filter(f => fieldNeedsExplicitWalk(f.tpe)).map(_.name.name)
      val excludeSet       = walkedFieldNames.map(n => q"'$n'").join(", ")
      val encodePatches = dto.fields.collect {
        case f if fieldNeedsExplicitWalk(f.tpe) =>
          q"obj['${f.name.name}'] = ${mkJsonAnyEncoder(f.tpe, q"value.${f.name.name}")}"
      }
      val decodePatches = dto.fields.collect {
        case f if fieldNeedsExplicitWalk(f.tpe) =>
          q"obj['${f.name.name}'] = ${mkJsonAnyDecoder(f.tpe, q"obj['${f.name.name}']")}"
      }
      val encode =
        q"""obj = value.model_dump(mode='json', exclude={${excludeSet}})
           |${encodePatches.joinN()}
           |return $pyJsonDumps(obj)""".stripMargin
      val decode =
        q"""obj = $pyJsonLoads(wire)
           |${decodePatches.joinN()}
           |return $name.model_validate(obj)""".stripMargin
      (encode, decode)
    }
  }

  // Top-level field test: a codec class needs the any-field helpers if any direct or nested-via-
  // Constructor-arg field has type `any`. Mirrors `PyUEBACodecGenerator.hasAnyField`; this 14th
  // instance (7 UEBA + 7 JSON generators) is duplicated per-language per-codec — extraction
  // deferred per ledger.
  private def hasAnyField(defn: DomainMember.User): Boolean = {
    defn.defn match {
      case d: Typedef.Dto => dtoHasAnyField(d)
      case _              => false
    }
  }

  private def dtoHasAnyField(dto: Typedef.Dto): Boolean = dto.fields.exists(f => fieldHasAny(f.tpe))

  private def fieldHasAny(tpe: TypeRef): Boolean = tpe match {
    case _: TypeRef.Any         => true
    case _: TypeRef.Scalar      => false
    case c: TypeRef.Constructor => c.args.exists(fieldHasAny)
  }

  // PR-60-D02: detect whether the DTO contains any `map[user-key, V]` fields where the key is a
  // user type (DTO/id/foreign). Pydantic's transparent path mis-handles such keys; the explicit
  // walker uses `mkJsonKeyEncoder`/`mkJsonKeyDecoder` to round-trip them as strings.
  private def dtoHasUserKeyMapField(dto: Typedef.Dto): Boolean = dto.fields.exists(f => fieldHasUserKeyMap(f.tpe))

  private def fieldHasUserKeyMap(tpe: TypeRef): Boolean = tpe match {
    case _: TypeRef.Any    => false
    case _: TypeRef.Scalar => false
    case c: TypeRef.Constructor =>
      val isMapWithUserKey = c.id == TypeId.Builtins.map && (c.args.head match {
        case TypeRef.Scalar(_: TypeId.User) => true
        case _                              => false
      })
      isMapWithUserKey || c.args.exists(fieldHasUserKeyMap)
  }

  // PR-60-D02: route a DTO through the explicit walker path when EITHER any-bearing OR user-key-
  // map-bearing fields are present. The walker handles both cases; non-walked subtrees are passed
  // through `pydantic_core.to_jsonable_python` (encode) / left untouched for Pydantic
  // `model_validate` to coerce (decode).
  private def dtoNeedsExplicitWalker(dto: Typedef.Dto): Boolean =
    dtoHasAnyField(dto) || dtoHasUserKeyMapField(dto)

  private def fieldNeedsExplicitWalk(tpe: TypeRef): Boolean =
    fieldHasAny(tpe) || fieldHasUserKeyMap(tpe)

  // Builds an expression that produces a JSON-friendly Python value (dict/list/scalar) for a
  // walked field's value. Recurses only into subtrees that need explicit walking (any-bearing
  // OR user-key-map). Subtrees that need NO explicit walking are passed through
  // `pydantic_core.to_jsonable_python(ref)` — equivalent to what `model_dump(mode='json')` would
  // have produced if we'd let it process the field naturally.
  private def mkJsonAnyEncoder(tpe: TypeRef, ref: TextTree[PyValue]): TextTree[PyValue] = tpe match {
    case a: TypeRef.Any => mkAnyEncoderCall(a, ref)
    case c: TypeRef.Constructor =>
      c.id match {
        case TypeId.Builtins.opt =>
          q"None if $ref is None else ${mkJsonAnyEncoder(c.args.head, ref)}"
        case TypeId.Builtins.lst | TypeId.Builtins.set =>
          q"[${mkJsonAnyEncoder(c.args.head, q"v")} for v in $ref]"
        case TypeId.Builtins.map =>
          // M19/PR-60: map-key handling. For user-DTO/`id`/foreign keys, the validator (PR-59)
          // gates eligibility; we emit explicit primitive-string projection via `mkJsonKeyEncoder`
          // so cross-language compat sees a flat string-keyed JSON object. For builtin keys,
          // `mkJsonKeyEncoder` is identity (Pydantic model_dump_json handles primitive keys via
          // mode='json'). The value side recurses only if needed.
          val keyExpr = mkJsonKeyEncoder(c.args.head, q"k")
          q"{$keyExpr: ${mkJsonAnyEncoder(c.args.last, q"v")} for k, v in $ref.items()}"
        case o => throw new RuntimeException(s"BUG: unexpected walked-field constructor: $o")
      }
    case _: TypeRef.Scalar =>
      // PR-60-D02: a scalar may legitimately appear inside a walked subtree (e.g. the value side
      // of `map[user-key, V]` where V is itself a non-any-bearing scalar). Defer JSON conversion
      // to Pydantic's `to_jsonable_python` — equivalent to what `model_dump(mode='json')` would
      // have produced for that scalar.
      q"$pyToJsonablePython($ref)"
  }

  // M19/PR-60: produces a JSON-serializable key (Python str/int/etc.) from a typed key. For
  // single-primitive wrappers, peel via attribute access and recurse. For `id` types, use
  // `str(k)` (PR-57d emits __str__ as the canonical repr). For builtins, return the value
  // (Pydantic model_dump_json handles primitive keys via mode='json').
  private def mkJsonKeyEncoder(tpe: TypeRef, ref: TextTree[PyValue]): TextTree[PyValue] = tpe match {
    case TypeRef.Scalar(u: TypeId.User) =>
      domain.defs.meta.nodes.get(u) match {
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.isIdentifier =>
          q"str($ref)"
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.fields.size == 1 && d.contracts.isEmpty =>
          val inner = d.fields.head
          mkJsonKeyEncoder(inner.tpe, q"$ref.${inner.name.name}")
        // PR-60-D03: foreign-typed keys — defensive str() coercion. The user-supplied codec
        // may produce a non-string value; str() ensures `json.dumps` accepts it as an object key.
        case Some(DomainMember.User(_, _: Typedef.Foreign, _, _)) =>
          q"str($ref)"
        case _ =>
          // PR-60-D04: validator (PR-59) should reject any other user-type as a map key. If we
          // land here, the validator missed a case — fail loudly rather than silently emit
          // a non-string dict key that json.dumps would reject. `.throw()` on an empty
          // generator is the canonical Python idiom for raising in expression position.
          q"""(_ for _ in ()).throw(Exception(f"BUG: Unexpected key usertype (validator should have rejected): {repr($ref)}"))"""
      }
    case _ => ref
  }

  // Builds an expression that consumes a JSON-friendly Python value (dict/list/scalar) and
  // returns a typed-or-coercible value. Recurses only into subtrees that need explicit walking;
  // the subsequent `model_validate(obj)` call coerces the remaining (non-walked) leaves into
  // typed objects.
  private def mkJsonAnyDecoder(tpe: TypeRef, ref: TextTree[PyValue]): TextTree[PyValue] = tpe match {
    case a: TypeRef.Any => mkAnyDecoderCall(a, ref)
    case c: TypeRef.Constructor =>
      c.id match {
        case TypeId.Builtins.opt =>
          q"None if $ref is None else ${mkJsonAnyDecoder(c.args.head, ref)}"
        case TypeId.Builtins.lst | TypeId.Builtins.set =>
          q"[${mkJsonAnyDecoder(c.args.head, q"v")} for v in $ref]"
        case TypeId.Builtins.map =>
          // PR-60-D02: rebuild typed keys via `mkJsonKeyDecoder` BEFORE handing the dict to
          // Pydantic — `model_validate` cannot coerce a JSON string back into a Pydantic model,
          // so the key must already be the typed form. For builtin keys (where `mkJsonKeyDecoder`
          // is identity), Pydantic coerces normally.
          val keyExpr = mkJsonKeyDecoder(c.args.head, q"k")
          q"{$keyExpr: ${mkJsonAnyDecoder(c.args.last, q"v")} for k, v in $ref.items()}"
        case o => throw new RuntimeException(s"BUG: unexpected walked-field constructor: $o")
      }
    case _: TypeRef.Scalar =>
      // PR-60-D02: scalar leaves inside walked subtrees pass through unchanged — the subsequent
      // `model_validate(obj)` call handles primitive/user-type coercion from JSON.
      ref
  }

  // PR-60-D02: rebuild a typed key from a JSON-string key. Mirrors `mkJsonKeyEncoder`. For
  // single-primitive wrappers, recurse and reconstruct via the wrapper's constructor; for `id`
  // types, parse via `<IdName>Codec.parse_repr(s).value` (validator gates well-formedness — see
  // PR-60-D07 for the deferred malformed-key error-semantics work). For foreigns and builtins,
  // rely on Pydantic's coercion via `model_validate` at the call site.
  private def mkJsonKeyDecoder(tpe: TypeRef, ref: TextTree[PyValue]): TextTree[PyValue] = tpe match {
    case TypeRef.Scalar(u: TypeId.User) =>
      domain.defs.meta.nodes.get(u) match {
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.isIdentifier =>
          val tpeRef     = typeTranslator.asPyTypeKeepForeigns(u, domain, evolution, pyFileTools.definitionsBasePkg)
          val codecClass = PyType(tpeRef.moduleId, s"${tpeRef.name}Codec")
          // PR-F (M24): throw BaboonCodecException.DecoderFailure on Left for cross-language
          // malformed-key consistency. Use a single-call lambda to bind the parse result
          // exactly once; Python expressions cannot raise directly, so we route the throw
          // through the generator-throw idiom. (Walrus inside a conditional expression
          // doesn't help here because the COND is evaluated before the true-branch's
          // walrus bind.)
          q"""(lambda __r: __r.value if isinstance(__r, $baboonRightType) else (_ for _ in ()).throw($baboonCodecException.DecoderFailure(f"malformed key: {$ref}")))($codecClass.parse_repr($ref))"""
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.fields.size == 1 && d.contracts.isEmpty =>
          val inner    = d.fields.head
          val tpeRef   = typeTranslator.asPyTypeKeepForeigns(u, domain, evolution, pyFileTools.definitionsBasePkg)
          val innerDec = mkJsonKeyDecoder(inner.tpe, ref)
          q"$tpeRef(${inner.name.name}=$innerDec)"
        case Some(DomainMember.User(_, _: Typedef.Foreign, _, _)) =>
          // Foreign-keyed wrapper: defer to Pydantic — `model_validate` will use the user-supplied
          // codec to coerce the JSON-string into the foreign type.
          ref
        case _ =>
          // PR-60-D04: validator (PR-59) should reject any other user-type as a map key.
          q"""(_ for _ in ()).throw(Exception(f"BUG: Unexpected key usertype (validator should have rejected): {repr($ref)}"))"""
      }
    case _ => ref
  }

  private def mkAnyEncoderCall(a: TypeRef.Any, ref: TextTree[PyValue]): TextTree[PyValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined) & 0xFF
    val expectedHex                       = "0x%02x".format(expectedKind)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"self._encode_any_field(context, $expectedHex, $staticDom, $staticVer, $staticTid, $ref)"
  }

  private def mkAnyDecoderCall(a: TypeRef.Any, ref: TextTree[PyValue]): TextTree[PyValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined) & 0xFF
    val expectedHex  = "0x%02x".format(expectedKind)
    q"self._decode_any_field($expectedHex, $ref)"
  }

  // Static fallbacks for the cross-format facade helpers (`json_to_ueba_bytes` / `ueba_to_json`).
  // Per spec table:
  //   A=(None,None,None), B=(currentDomain,None,None), C=(currentDomain,currentVersion,None),
  //   D1=(None,None,underlyingFqid), D2=(currentDomain,None,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // 14th instance of this duplication across UEBA + JSON generators — extraction deferred per
  // ledger (textual emission diverges by language flavor; PR 10.2 noted the same).
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[PyValue], TextTree[PyValue], TextTree[PyValue]) = {
    val none                     = q"None"
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
  // Emitted at most once per codec class that has any any-bearing field. Mirrors the PR 6.3 (Java)
  // / 7.3 (TS) / 8.3 (Dart) / 9.3 (Swift) JSON helper shape.
  //
  // Cast-vs-base-method: Python `BaboonCodecContext.facade` is `Optional[BaboonCodecsFacade]`
  // directly (PR 10.1 design — Python doesn't have the runtime-cycle issue C++/Swift had), so the
  // helper accesses it without a cast. Mirrors PR 10.2 UEBA decision.
  //
  // The encoder helper returns a Python `dict` (the envelope) that the caller patches into the
  // per-DTO `model_dump`-derived object before `json.dumps`. UEBA cross-conversion via
  // `ctx.facade.ueba_to_json(...)` returns a JSON-encoded string (Python's JSON codec encodes to
  // `str`); the helper re-parses it via `json.loads` to embed it as a structured value under
  // the `$c` content key — symmetrical with the Dart/Swift helpers that work in JSON-tree space.
  private def anyFieldHelpers: TextTree[PyValue] = {
    q"""def _encode_any_field(self, context: $baboonCodecContext, expected_kind: $pyInt, static_domain, static_version, static_typeid, value: $baboonAnyOpaque):
       |    if value.meta.kind != expected_kind:
       |        raise $baboonCodecException.EncoderFailure(
       |            f"any: meta-kind 0x{value.meta.kind & 0xFF:02x} does not match field-declared 0x{expected_kind & 0xFF:02x}"
       |        )
       |    if isinstance(value, $baboonAnyOpaqueJson):
       |        any_inner = value.json
       |    elif isinstance(value, $baboonAnyOpaqueUeba):
       |        if context.facade is None:
       |            raise $baboonCodecException.EncoderFailure(
       |                "Cannot encode AnyOpaqueUeba into JSON without a facade reference. Pass BaboonCodecContext.with_facade(use_indices, facade) into encode(), or supply AnyOpaqueJson directly."
       |            )
       |        any_conv_result = context.facade.ueba_to_json(value.meta, value.bytes, static_domain=static_domain, static_version=static_version, static_typeid=static_typeid)
       |        if isinstance(any_conv_result, $baboonLeftType):
       |            raise any_conv_result.value
       |        any_inner = $pyJsonLoads(any_conv_result.value)
       |    else:
       |        raise $baboonCodecException.EncoderFailure(
       |            f"unexpected AnyOpaque subclass: {type(value).__name__}"
       |        )
       |    any_envelope = $baboonAnyMetaCodec.write_json(value.meta)
       |    any_envelope[$baboonAnyMetaCodec.ANY_CONTENT_KEY] = any_inner
       |    return any_envelope
       |
       |def _decode_any_field(self, expected_kind: $pyInt, wire) -> $baboonAnyOpaque:
       |    if not isinstance(wire, dict):
       |        raise $baboonCodecException.DecoderFailure(
       |            f"any: JSON envelope must be an object, got {type(wire).__name__}"
       |        )
       |    any_meta_result = $baboonAnyMetaCodec.read_json(wire)
       |    if isinstance(any_meta_result, $baboonLeftType):
       |        raise any_meta_result.value
       |    any_meta = any_meta_result.value
       |    if any_meta.kind != expected_kind:
       |        raise $baboonCodecException.DecoderFailure(
       |            f"any: wire kind 0x{any_meta.kind & 0xFF:02x} does not match field-declared 0x{expected_kind & 0xFF:02x}"
       |        )
       |    if $baboonAnyMetaCodec.ANY_CONTENT_KEY not in wire:
       |        raise $baboonCodecException.DecoderFailure(
       |            f"any: JSON envelope missing '{$baboonAnyMetaCodec.ANY_CONTENT_KEY}' content key"
       |        )
       |    any_content = wire[$baboonAnyMetaCodec.ANY_CONTENT_KEY]
       |    return $baboonAnyOpaqueJson(any_meta, any_content)
       |""".stripMargin
  }

  override def codecType(tid: TypeId.User): PyType = {
    val typeName = s"${tid.name.name.capitalize}_JsonCodec"
    val moduleId = typeTranslator.toPyModule(tid, domain.version, evolution, pyFileTools.definitionsBasePkg)
    PyType(moduleId, typeName)
  }

  override def codecMeta(tid: TypeId.User): PyCodecTranslator.CodecMeta = {
    val meta = q"""@$pyStaticMethod
                  |def codec_json():
                  |    return ${codecType(tid)}.instance()""".stripMargin
    PyCodecTranslator.CodecMeta(meta)
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Py) &&
    pyTarget.language.generateJsonCodecs && (pyTarget.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "json"
}
