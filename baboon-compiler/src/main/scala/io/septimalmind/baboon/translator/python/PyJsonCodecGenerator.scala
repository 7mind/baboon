package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.python.PyKeywords.escapePyKeyword
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
    val anyHelpers: List[TextTree[PyValue]]       = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil
    val customKeyHelpers: List[TextTree[PyValue]] = if (hasCustomForeignMapKey(defn)) List(tryDecodeKeyHelper) else Nil
    val baseMethods                               = encodeMethod ++ decodeMethod ++ anyHelpers ++ customKeyHelpers
    val cName                                     = q"${srcRef.name}_JsonCodec"
    val cType                                     = q"'${codecType(defn.id)}'"

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
      // The exclude set for model_dump uses Python attribute names (not aliases).
      // Keyword fields have attribute name `fieldName_`, others use `fieldName`.
      val walkedFieldAttrNames = dto.fields.filter(f => fieldNeedsExplicitWalk(f.tpe))
        .map(f => if (PyKeywords.isKeyword(f.name.name)) s"${f.name.name}_" else f.name.name)
      val excludeSet           = walkedFieldAttrNames.map(n => q"'$n'").join(", ")
      val encodePatches = dto.fields.collect {
        case f if fieldNeedsExplicitWalk(f.tpe) =>
          // Wire key is the ORIGINAL model name; attribute access uses the escaped Python name.
          q"obj['${f.name.name}'] = ${mkJsonAnyEncoder(f.tpe, q"value.${escapePyKeyword(f.name.name)}")}"
      }
      val decodePatches = dto.fields.collect {
        case f if fieldNeedsExplicitWalk(f.tpe) =>
          // Wire key is the ORIGINAL model name.
          q"obj['${f.name.name}'] = ${mkJsonAnyDecoder(f.tpe, q"obj['${f.name.name}']")}"
      }
      val encode =
        q"""obj = value.model_dump(mode='json', exclude={$excludeSet})
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

  // PR-I.2-D01: detect whether the DTO contains any `map[custom-foreign-key, V]` fields. When
  // present, the codec class needs the `_try_decode_key` helper that wraps the host call in
  // try/except and re-raises as `BaboonCodecException.DecoderFailure("malformed key: ...")` —
  // Python expressions cannot contain try/except, so the wrap is factored into a class method
  // callable from the dict-comprehension key expression via `self._try_decode_key(lambda: ..., k)`.
  private def hasCustomForeignMapKey(defn: DomainMember.User): Boolean = defn.defn match {
    case d: Typedef.Dto => dtoHasCustomForeignMapKey(d)
    case _              => false
  }

  private def dtoHasCustomForeignMapKey(dto: Typedef.Dto): Boolean =
    dto.fields.exists(f => fieldHasCustomForeignMapKey(f.tpe))

  // Returns true iff `tpe` (used as a map key) transitively reaches a Custom-foreign via
  // `mkJsonKeyDecoder`'s recursion paths: (a) direct Custom-foreign scalar, or (b) single-field
  // wrapper DTO whose inner type reaches a Custom-foreign. Mirrors `mkJsonKeyDecoder`'s match arms
  // so `_try_decode_key` is emitted exactly when the generated comprehension expression uses it.
  private def keyTypeNeedsCustomForeignWrap(tpe: TypeRef): Boolean = tpe match {
    case TypeRef.Scalar(u: TypeId.User) =>
      domain.defs.meta.nodes.get(u) match {
        case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
          f.bindings.get(BaboonLang.Py) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
              keyTypeNeedsCustomForeignWrap(aliasedRef)
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) => true
            case _                                                                  => false
          }
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.fields.size == 1 && d.contracts.isEmpty =>
          keyTypeNeedsCustomForeignWrap(d.fields.head.tpe)
        case _ => false
      }
    case _ => false
  }

  private def fieldHasCustomForeignMapKey(tpe: TypeRef): Boolean = tpe match {
    case _: TypeRef.Any    => false
    case _: TypeRef.Scalar => false
    case c: TypeRef.Constructor =>
      val isCustomForeignKeyMap = c.id == TypeId.Builtins.map && (c.args.head match {
        case TypeRef.Scalar(_: TypeId.User) => keyTypeNeedsCustomForeignWrap(c.args.head)
        case _                              => false
      })
      isCustomForeignKeyMap || c.args.exists(fieldHasCustomForeignMapKey)
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

  // PR-29.10-D01: detect whether the DTO contains any field whose type is an ADT user type.
  // When a DTO field is declared as an ADT base type (e.g. `okEnvelope: IntStrEnvelope`),
  // pydantic's `model_dump_json()` uses the declared base-type schema for the `model_serializer`
  // wrap handler — which has no fields — producing `{"VariantName": {}}` instead of
  // `{"VariantName": {"field": value}}`. The explicit walker bypasses pydantic's path and
  // dispatches directly to the ADT's JSON codec.
  private def dtoHasAdtField(dto: Typedef.Dto): Boolean = dto.fields.exists(f => fieldHasAdt(f.tpe))

  private def fieldHasAdt(tpe: TypeRef): Boolean = tpe match {
    case TypeRef.Scalar(u: TypeId.User) =>
      domain.defs.meta.nodes.get(u) match {
        case Some(DomainMember.User(_, _: Typedef.Adt, _, _)) => true
        case _                                                => false
      }
    case _: TypeRef.Scalar      => false
    case _: TypeRef.Any         => false
    case c: TypeRef.Constructor => c.args.exists(fieldHasAdt)
  }

  // PR-60-D02: route a DTO through the explicit walker path when EITHER any-bearing OR user-key-
  // map-bearing fields are present. The walker handles both cases; non-walked subtrees are passed
  // through `pydantic_core.to_jsonable_python` (encode) / left untouched for Pydantic
  // `model_validate` to coerce (decode).
  // PR-29.10-D01: also route when ADT-bearing fields are present (pydantic's inherited
  // model_serializer loses subclass fields when serialized as a declared base-type field).
  private def dtoNeedsExplicitWalker(dto: Typedef.Dto): Boolean =
    dtoHasAnyField(dto) || dtoHasUserKeyMapField(dto) || dtoHasAdtField(dto)

  private def fieldNeedsExplicitWalk(tpe: TypeRef): Boolean =
    fieldHasAny(tpe) || fieldHasUserKeyMap(tpe) || fieldHasAdt(tpe)

  // Builds an expression that produces a JSON-friendly Python value (dict/list/scalar) for a
  // walked field's value. Recurses only into subtrees that need explicit walking (any-bearing,
  // user-key-map, OR ADT-bearing). Subtrees that need NO explicit walking are passed through
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
    case TypeRef.Scalar(u: TypeId.User) =>
      // PR-29.10-D01: ADT fields must bypass pydantic's model_dump_json path and go through
      // the explicit JSON codec. When pydantic serializes a field declared as an ADT base type
      // (e.g. `okEnvelope: IntStrEnvelope`), the inherited model_serializer's `serializer(self)`
      // handler uses the base-type schema (no fields), losing the subclass field values.
      // We detect the ADT case here and emit a direct codec call: the result is a JSON string
      // which we parse back to a dict for embedding in the parent object.
      domain.defs.meta.nodes.get(u) match {
        case Some(DomainMember.User(_, _: Typedef.Adt, _, _)) =>
          val c = codecType(u)
          q"$pyJsonLoads($c.instance().encode(context, $ref))"
        case _ =>
          // PR-60-D02: a scalar may legitimately appear inside a walked subtree (e.g. the value
          // side of `map[user-key, V]` where V is itself a non-any-bearing scalar). Defer JSON
          // conversion to Pydantic's `to_jsonable_python` — equivalent to `model_dump(mode='json')`
          // for that scalar.
          q"$pyToJsonablePython($ref)"
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
        // D6 (T30): enum map keys stringify via the member's `.value` (the wire-name string),
        // mirroring the value-side enum codec (`json.dumps(value.value)`) and Scala's
        // `$ref.toString` arm (ScJsonCodecGenerator.scala:317-321). A `map[Color, V]` thus
        // round-trips as a string-keyed JSON object instead of hitting the `case _` BUG-throw.
        case Some(DomainMember.User(_, _: Typedef.Enum, _, _)) =>
          q"$ref.value"
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.isIdentifier =>
          q"str($ref)"
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.fields.size == 1 && d.contracts.isEmpty =>
          val inner = d.fields.head
          mkJsonKeyEncoder(inner.tpe, q"$ref.${escapePyKeyword(inner.name.name)}")
        // PR-I.2 (M24 Phase 3.2): Custom-foreign map keys route through the emitted
        // `<Foreign>_KeyCodecHost.instance()` extension hook (replaces PR-60-D03 silent
        // str() coercion). Explicit Custom match (PR-I-D05 pattern guidance).
        case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
          f.bindings.get(BaboonLang.Py) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
              mkJsonKeyEncoder(aliasedRef, ref)
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
              val srcRef = typeTranslator.asPyTypeKeepForeigns(u, domain, evolution, pyFileTools.definitionsBasePkg)
              val host   = PyType(srcRef.moduleId, s"${srcRef.name}_KeyCodecHost")
              q"$host.instance().encode_key($ref)"
            case None =>
              throw new RuntimeException(s"BUG: Foreign type $u has no Python binding")
          }
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
    case TypeRef.Scalar(u: TypeId.User) =>
      // PR-29.10-D01: ADT fields must bypass pydantic's model_validate path and go through
      // the explicit JSON codec. The field value in the parsed object is a JSON-decoded dict
      // (e.g. `{"Ok": {"value": 42}}`); we re-encode it to a JSON string and pass it to the
      // ADT codec's decode method, which uses the polymorphic model_validator to dispatch to
      // the correct variant. Non-ADT user scalars pass through unchanged for Pydantic coercion.
      domain.defs.meta.nodes.get(u) match {
        case Some(DomainMember.User(_, _: Typedef.Adt, _, _)) =>
          val c = codecType(u)
          q"$c.instance().decode(context, $pyJsonDumps($ref))"
        case _ =>
          // PR-60-D02: scalar leaves inside walked subtrees pass through unchanged — the
          // subsequent `model_validate(obj)` call handles primitive/user-type coercion from JSON.
          ref
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
        // D6 (T30): reconstruct an enum map key from its wire-name string via the enum's
        // value-lookup constructor (`EnumType(s)`), mirroring the value-side decoder
        // (`EnumType(json.loads(wire))`) and Scala's enum key-decoder arm
        // (ScJsonCodecGenerator.scala:476-481). A malformed key raises ValueError from the
        // enum constructor — consistent with the fail-fast contract for the other key arms.
        case Some(DomainMember.User(_, _: Typedef.Enum, _, _)) =>
          val tpeRef = typeTranslator.asPyTypeKeepForeigns(u, domain, evolution, pyFileTools.definitionsBasePkg)
          q"$tpeRef($ref)"
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
          // Use the keyword-escaped attribute name as the constructor kwarg.
          val attrName = if (PyKeywords.isKeyword(inner.name.name)) s"${inner.name.name}_" else inner.name.name
          q"$tpeRef($attrName=$innerDec)"
        case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
          // PR-I.2 (M24 Phase 3.2): Custom-foreign map keys route through the emitted
          // `<Foreign>_KeyCodecHost.instance()` extension hook (replaces PR-60-D03 silent
          // Pydantic-coerce fallback). `except Exception as e` (NOT bare `except` /
          // `except BaseException`) keeps fail-fast on KeyboardInterrupt / SystemExit
          // (PR-I-D01 pattern guidance). Explicit Custom match (PR-I-D05).
          f.bindings.get(BaboonLang.Py) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
              mkJsonKeyDecoder(aliasedRef, ref)
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
              val srcRef = typeTranslator.asPyTypeKeepForeigns(u, domain, evolution, pyFileTools.definitionsBasePkg)
              val host   = PyType(srcRef.moduleId, s"${srcRef.name}_KeyCodecHost")
              // PR-I.2-D01: wrap the host call via `_try_decode_key` to re-raise any exception
              // (including user-raised DecoderFailure with a custom message) as
              // `DecoderFailure("malformed key: <repr>")`, matching the cross-backend invariant.
              // A dict-comprehension key expression cannot contain try/except directly, so the
              // wrap is factored into the `_try_decode_key` class method (emitted when
              // `hasCustomForeignMapKey` is true). `except Exception` (not `BaseException`)
              // lets KeyboardInterrupt / SystemExit propagate unimpeded (PR-I-D01 guidance).
              q"self._try_decode_key(lambda: $host.instance().decode_key($ref), $ref)"
            case None =>
              throw new RuntimeException(s"BUG: Foreign type $u has no Python binding")
          }
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

  // PR-I.2-D01: helper emitted into codec classes that have a Custom-foreign map key. Wraps the
  // host `decode_key` call in try/except and re-raises as `DecoderFailure("malformed key: ...")`,
  // matching the cross-backend invariant (Swift/Java/Kotlin/Dart/TS/C# all do the same wrap).
  // A class-level method is necessary because Python expressions (including dict comprehensions)
  // cannot contain try/except; the comprehension key expression uses
  // `self._try_decode_key(lambda: host.instance().decode_key(k), k)` instead of a bare call.
  // `except Exception` (not `BaseException`) lets KeyboardInterrupt / SystemExit propagate
  // unimpeded, per PR-I-D01 pattern guidance.
  private def tryDecodeKeyHelper: TextTree[PyValue] = {
    q"""def _try_decode_key(self, fn, raw_repr):
       |    try:
       |        return fn()
       |    except Exception as __e:
       |        raise $baboonCodecException.DecoderFailure(f"malformed key: {repr(raw_repr)}") from __e
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
