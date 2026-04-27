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

class PyUEBACodecGenerator(
  typeTranslator: PyTypeTranslator,
  treeTools: PyDomainTreeTools,
  evolution: BaboonEvolution,
  pyFileTools: PyFileTools,
  pyTarget: PyTarget,
  domain: Domain,
) extends PyCodecTranslator {
  override def translate(
    defn: DomainMember.User,
    pyRef: PyValue.PyType,
    srcRef: PyValue.PyType,
  ): Option[TextTree[PyValue]] = {
    (defn.defn match {
      case d: Typedef.Dto  => Some(genDtoBodies(pyRef, d))
      case e: Typedef.Enum => Some(genEnumBodies(e))
      case a: Typedef.Adt  => Some(genAdtBodies(pyRef, a))
      case f: Typedef.Foreign =>
        f.bindings.get(BaboonLang.Py) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
          case _                                                                  => Some(genForeignTypesBodies(pyRef))
        }
      case _: Typedef.Service  => None
      case _: Typedef.Contract => None
    }).map {
      case (enc, dec) =>
        val branchDecoder = defn.defn match {
          case d: Typedef.Dto => genBranchDecoder(pyRef, d)
          case _              => None
        }
        genCodec(defn, pyRef, srcRef, enc, dec, branchDecoder)
    }
  }

  private def genCodec(
    defn: DomainMember.User,
    name: PyType,
    srcRef: PyType,
    enc: TextTree[PyValue],
    dec: TextTree[PyValue],
    branchDecoder: Option[TextTree[PyValue]],
  ): TextTree[PyValue] = {
    val isEncoderEnabled = pyTarget.language.enableDeprecatedEncoders || domain.version == evolution.latest
    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = d.fields.filter(f => domain.refMeta(f.tpe).len.isVariable)
        val comment = varlens.map(f => q"# ${f.toString}").joinN()
        q"""$comment
           |return ${varlens.size.toString}""".stripMargin

      case _: Typedef.Enum    => q"return 0"
      case _: Typedef.Adt     => q"return 0"
      case _: Typedef.Foreign => q"""raise ValueError("$name is a foreign type")"""

      case d: Typedef.Contract => throw new IllegalArgumentException(s"BUG: contract codec should not be rendered: $d")
      case d: Typedef.Service  => throw new IllegalArgumentException(s"BUG: service codec should not be rendered: $d")
    }

    val indexMethods = List(
      q"""def index_elements_count(self, ctx: $baboonCodecContext) -> $pyInt:
         |    ${indexBody.shift(4).trim}
         |""".stripMargin
    )

    val encoderMethod = if (isEncoderEnabled) {
      List(
        q"""def encode(self, ctx: $baboonCodecContext, wire: $baboonLEDataOutputStream, value: $name):
           |    ${enc.shift(4).trim}
           |""".stripMargin
      )
    } else Nil

    val decoderMethod = List(
      q"""def decode(self, ctx: $baboonCodecContext, wire: $baboonLEDataInputStream) -> $name: 
         |    ${dec.shift(4).trim}
         |""".stripMargin
    )

    val anyHelpers: List[TextTree[PyValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil

    val baseMethods = encoderMethod ++ decoderMethod ++
      branchDecoder.map {
        body =>
          q"""def decode_branch(self, ctx: $baboonCodecContext, wire: $baboonLEDataInputStream) -> $name: 
             |    ${body.shift(4).trim}
             |""".stripMargin
      } ++ indexMethods ++ anyHelpers

    val cName = q"${srcRef.name}_UEBACodec"
    val cType = q"'${codecType(defn.id)}'"

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecBase[$name, $cType]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecBase[$name, $cType]"
        case _ if defn.ownedByAdt                           => q"$baboonBinCodecGeneratedAdt[$name, $cType]"
        case _                                              => q"$baboonBinCodecGenerated[$name, $cType]"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecNoEncoder[$name, $cType]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecNoEncoder[$name, $cType]"
        case _ if defn.ownedByAdt                           => q"$baboonBinCodecNoEncoderGeneratedAdt[$name, $cType]"
        case _                                              => q"$baboonBinCodecNoEncoderGenerated[$name, $cType]"
      }
    }

    val parents = List(cParent, q"$baboonBinCodecIndexed")

    q"""class $cName(${parents.join(", ")}):
       |    ${baseMethods.joinNN().shift(4).trim}
       |
       |    ${treeTools.makeCodecMeta(defn).joinN().shift(4).trim}
       |
       |    def target_type(self) -> $pyType:
       |        return $name
       |
       |    _lazy_instance: $baboonLazy['$cName'] = $baboonLazy(lambda: $cName())
       |""".stripMargin
  }

  private def genBranchDecoder(
    name: PyType,
    dto: Typedef.Dto,
  ): Option[TextTree[PyValue]] = {
    dto.id.owner match {
      case Owner.Adt(_) if pyTarget.language.wrappedAdtBranchCodecs =>
        val fieldsCodecs = fieldsOf(dto).map { case (enc, dec, _) => (enc, dec) }
        Some(genDtoDecoder(name, fieldsCodecs, dto))
      case _ => None
    }
  }

  private def genEnumBodies(enum: Typedef.Enum): (TextTree[PyValue], TextTree[PyValue]) = {
    val branches = enum.members.zipWithIndex.toList.map {
      case (m, i) =>
        (
          q"""if value.value == "${m.name}":
             |    wire.write_byte(${i.toString})
             |    return
             |""".stripMargin,
          q"""if as_byte == ${i.toString}:
             |    return ${enum.id.name.name.capitalize}.${m.name}
             |""".stripMargin,
        )
    }

    (
      q"""${branches.map(_._1).joinN()}
         |
         |raise ValueError(f"Cannot encode {value} to ${enum.id.name.name.capitalize}: no matching value")""".stripMargin,
      q"""as_byte = wire.read_byte()
         |
         |${branches.map(_._2).joinN()}
         |
         |raise ValueError(f"Cannot decode {wire} to ${enum.id.name.name.capitalize}: no matching value")""".stripMargin,
    )
  }

  private def genAdtBodies(name: PyType, adt: Typedef.Adt): (TextTree[PyValue], TextTree[PyValue]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.map {
      case (member, i) =>
        val cName = codecType(member)
        val encoder = {
          if (pyTarget.language.wrappedAdtBranchCodecs) {
            q"""if isinstance(value, ${member.name.name.capitalize}):
               |    $cName.instance().encode(ctx, wire, value)
               |    return
               |""".stripMargin
          } else {
            q"""if isinstance(value, ${member.name.name.capitalize}):
               |    wire.write_byte(${i.toString})
               |    $cName.instance().encode(ctx, wire, value)
               |    return
               |""".stripMargin
          }
        }
        val decoder = {
          if (pyTarget.language.wrappedAdtBranchCodecs) {
            q"""if as_byte == ${i.toString}:
               |    return $cName.instance().decode_branch(ctx, wire)
               |""".stripMargin
          } else {
            q"""if as_byte == ${i.toString}:
               |    return $cName.instance().decode(ctx, wire)
               |""".stripMargin
          }
        }
        (encoder, decoder)
    }

    (
      q"""${branches.map(_._1).joinN()}
         |
         |raise ValueError(f"Cannot encode {value} to $name: no matching value")
         |""".stripMargin,
      q"""as_byte = wire.read_byte()
         |
         |${branches.map(_._2).joinN()}
         |
         |raise ValueError(f"Cannot decode {wire} to $name: no matching value")
         |""".stripMargin,
    )
  }

  private def genDtoDecoder(name: PyValue.PyType, fields: List[(TextTree[PyValue], TextTree[PyValue])], dto: Typedef.Dto): TextTree[PyValue] = {
    val fieldsDecoders = dto.fields.zip(fields.map(_._2)).map { case (field, decoder) => q"${field.name.name}=$decoder" }
    q"""index = self.read_index(ctx, wire)
       |
       |if ctx.use_indices:
       |    assert len(index) == self.index_elements_count(ctx)
       |
       |return ${name.name}(
       |    ${fieldsDecoders.join(",\n").shift(4).trim}
       |)
       |""".stripMargin
  }

  private def genDtoBodies(name: PyType, dto: Typedef.Dto): (TextTree[PyValue], TextTree[PyValue]) = {
    def adtBranchIndex(id: TypeId.User) = {
      domain.defs.meta
        .nodes(id).asInstanceOf[DomainMember.User]
        .defn.asInstanceOf[Typedef.Adt]
        .dataMembers(domain)
        .zipWithIndex.find(_._1 == dto.id).get._2
    }

    val fields = fieldsOf(dto)

    val noIndex = Seq(
      q"wire.write_byte(header)",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).join("\n")

    val fieldsEncoders =
      q"""header = 0b0000000
         |
         |if ctx.use_indices:
         |    header = (header | 0b0000001) & 0xFF
         |    wire.write_byte(header)
         |    write_memory_stream = $pyBytesIO()
         |    fake_writer = $baboonLEDataOutputStream(write_memory_stream)
         |    ${fields.map(_._3).join("\n").shift(4).trim}
         |    write_memory_stream.flush()
         |    wire.write(write_memory_stream.getvalue())
         |else:
         |    ${noIndex.shift(4).trim}
         |""".stripMargin

    val fieldsDecoders = genDtoDecoder(name, fields.map { case (a, b, _) => (a, b) }, dto)

    val enc = dto.id.owner match {
      case Owner.Adt(id) if pyTarget.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)
        q"""wire.write_byte(${idx.toString})
           |$fieldsEncoders""".stripMargin
      case _ => fieldsEncoders
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if pyTarget.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)
        q"""marker = wire.read_byte()
           |assert marker == ${idx.toString}
           |return self.decode_branch(ctx, wire)""".stripMargin
      case _ => fieldsDecoders
    }

    (enc, dec)
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[PyValue], TextTree[PyValue], TextTree[PyValue])] = {
    dto.fields.map {
      f =>
        val fieldRef = q"value.${f.name.name}"
        val encoder  = mkEncoder(f.tpe, fieldRef, q"wire")
        val fakeEnc  = mkEncoder(f.tpe, fieldRef, q"fake_writer")
        val dec      = mkDecoder(f.tpe)

        val w = domain.refMeta(f.tpe).len match {
          case BinReprLen.Fixed(bytes) =>
            q"""# ${f.toString}
               |before = write_memory_stream.tell()
               |${fakeEnc.trim}
               |after = write_memory_stream.tell()
               |length = after - before
               |assert length == ${bytes.toString}
               |""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = v match {
              case BinReprLen.Unknown() =>
                q"assert after >= before, f\"Got after={after}, before={before}\""

              case BinReprLen.Alternatives(variants) =>
                q"assert length in {${variants.mkString(", ")}}, f\"Got length={length}\""

              case BinReprLen.Range(min, max) =>
                List(
                  Some(q"assert length >= ${min.toString}, f\"Got length={length}\" "),
                  max.map(m => q"assert length <= ${m.toString}, $$\"Got length={length}\""),
                ).flatten.joinN()
            }

            q"""# ${f.toString}
               |before = write_memory_stream.tell()
               |wire.write_i32(before)
               |${fakeEnc.trim}
               |after = write_memory_stream.tell()
               |length = after - before
               |wire.write_i32(length)
               |${sanityChecks.trim}
               |""".stripMargin
        }
        (encoder, dec, w)
    }
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[PyValue], writerRef: TextTree[PyValue]): TextTree[PyValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit => q"$writerRef.write_bool($ref)"
              case TypeId.Builtins.i08 => q"$writerRef.write_byte($ref)"
              case TypeId.Builtins.i16 => q"$writerRef.write_i16($ref)"
              case TypeId.Builtins.i32 => q"$writerRef.write_i32($ref)"
              case TypeId.Builtins.i64 => q"$writerRef.write_i64($ref)"
              case TypeId.Builtins.u08 => q"$writerRef.write_ubyte($ref)"
              case TypeId.Builtins.u16 => q"$writerRef.write_u16($ref)"
              case TypeId.Builtins.u32 => q"$writerRef.write_u32($ref)"
              case TypeId.Builtins.u64 => q"$writerRef.write_u64($ref)"
              case TypeId.Builtins.f32 => q"$writerRef.write_f32($ref)"
              case TypeId.Builtins.f64 => q"$writerRef.write_f64($ref)"

              case TypeId.Builtins.f128 => q"$writerRef.write_f128($ref)"
              case TypeId.Builtins.str  => q"$writerRef.write_str($ref)"

              case TypeId.Builtins.uid => q"$writerRef.write_uuid($ref)"
              case TypeId.Builtins.tsu => q"$writerRef.write_datetime($ref)"
              case TypeId.Builtins.tso => q"$writerRef.write_datetime($ref)"

              case TypeId.Builtins.bytes => q"$writerRef.write_bytes($ref)"

              case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Py) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, writerRef)
                  case _ =>
                    val target = codecType(u)
                    q"$target.instance().encode(ctx, $writerRef, $ref)"
                }
              case _ =>
                val target = codecType(u)
                q"$target.instance().encode(ctx, $writerRef, $ref)"
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"$writerRef.write_optional($ref, lambda v: ${mkEncoder(c.args.head, q"v", writerRef)})"
          case TypeId.Builtins.map =>
            val keyEncoder   = mkEncoder(c.args.head, q"v", writerRef)
            val valueEncoder = mkEncoder(c.args.last, q"v", writerRef)
            q"$writerRef.write_dict($ref, lambda v: $keyEncoder, lambda v: $valueEncoder)"
          case TypeId.Builtins.lst =>
            q"$writerRef.write_seq($ref, lambda v: ${mkEncoder(c.args.head, q"v", writerRef)})"
          case TypeId.Builtins.set =>
            q"$writerRef.write_seq($ref, lambda v: ${mkEncoder(c.args.head, q"v", writerRef)})"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref, writerRef)
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[PyValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit => q"wire.read_bool()"
              case TypeId.Builtins.i08 => q"wire.read_byte()"
              case TypeId.Builtins.i16 => q"wire.read_i16()"
              case TypeId.Builtins.i32 => q"wire.read_i32()"
              case TypeId.Builtins.i64 => q"wire.read_i64()"
              case TypeId.Builtins.u08 => q"wire.read_ubyte()"
              case TypeId.Builtins.u16 => q"wire.read_u16()"
              case TypeId.Builtins.u32 => q"wire.read_u32()"
              case TypeId.Builtins.u64 => q"wire.read_u64()"
              case TypeId.Builtins.f32 => q"wire.read_f32()"
              case TypeId.Builtins.f64 => q"wire.read_f64()"

              case TypeId.Builtins.f128 => q"wire.read_f128()"
              case TypeId.Builtins.str  => q"wire.read_string()"

              case TypeId.Builtins.uid => q"wire.read_uuid()"
              case TypeId.Builtins.tsu => q"wire.read_datetime()"
              case TypeId.Builtins.tso => q"wire.read_datetime()"

              case TypeId.Builtins.bytes => q"wire.read_bytes()"

              case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Py) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ => q"${codecType(u)}.instance().decode(ctx, wire)"
                }
              case _ => q"${codecType(u)}.instance().decode(ctx, wire)"
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"None if wire.read_byte() == 0 else ${mkDecoder(c.args.head)}"
          case TypeId.Builtins.map =>
            val keyDecoder   = mkDecoder(c.args.head)
            val valueDecoder = mkDecoder(c.args.last)
            q"{$keyDecoder: $valueDecoder for _ in range(wire.read_i32())}"
          case TypeId.Builtins.lst =>
            q"[${mkDecoder(c.args.head)} for _ in range(wire.read_i32())]"
          case TypeId.Builtins.set =>
            q"{${mkDecoder(c.args.head)} for _ in range(wire.read_i32())}"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a)
    }
  }

  // Deep walk (mirrors Scala/C#/Rust/Kotlin/Java/TS/Dart/Swift `hasAnyField`): a codec class
  // needs the any-field helpers if any direct or nested-via-Constructor-arg field has type `any`.
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

  // Encode delegates to the per-codec-class `_encode_any_field` helper. This site wires the
  // expected kind byte and the field's static (codec-gen-time) fallbacks for cross-format meta
  // resolution. See `anyStaticFallbacks` for the per-variant table.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[PyValue], wref: TextTree[PyValue]): TextTree[PyValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined) & 0xFF
    val expectedHex                       = "0x%02x".format(expectedKind)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"self._encode_any_field(ctx, $wref, $expectedHex, $staticDom, $staticVer, $staticTid, $ref)"
  }

  // Decode delegates to the per-codec-class `_decode_any_field` helper, returning an
  // `AnyOpaqueUeba`. Field-position type is `AnyOpaque`; the helper's narrow return type
  // widens implicitly via duck-typing.
  private def mkAnyDecoder(a: TypeRef.Any): TextTree[PyValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined) & 0xFF
    val expectedHex  = "0x%02x".format(expectedKind)
    q"self._decode_any_field(wire, $expectedHex)"
  }

  // Static fallbacks for the cross-format facade helpers (`json_to_ueba_bytes`/`ueba_to_json`).
  // The wire `meta` may omit components that are pinned by the field's static declaration; the
  // codec emits whatever is statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade._build_synthetic_type_meta` for the merge semantics. Per spec table:
  //   A=(None,None,None), B=(currentDomain,None,None), C=(currentDomain,currentVersion,None),
  //   D1=(None,None,underlyingFqid), D2=(currentDomain,None,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // 13th instance of this duplication across UEBA + JSON generators — extraction deferred per
  // ledger (textual emission diverges by language flavor).
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

  // Per-codec-class helpers consolidating the any-field framing, kind-check, and
  // buffer-then-write / read-then-skip paths — emitted at most once per codec class that has
  // any any-bearing field. Mirrors `JvUEBACodecGenerator.anyFieldHelpers` /
  // `DtUEBACodecGenerator.anyFieldHelpers`. Wire layout (locked, see
  // docs/drafts/20260424-1738-any-opaque-fields.md §"Wire format"):
  //   length:i32 | meta-length:i32 | meta-kind:u8 | meta-strings | blob
  //
  // PR-12-D01 lesson applied: explicit negative-i32 sanity guards on BOTH on-wire lengths
  // before any size arithmetic — Python `int` is unbounded but the wire format uses signed i32
  // and the slice arithmetic must operate on validated non-negative values.
  //
  // Cast vs base-method: `ctx.facade` is `Optional[BaboonCodecsFacade]` directly on the context
  // (PR 10.1 design — Python doesn't have the runtime-cycle issue C++/Swift had), so the helper
  // accesses it without a cast.
  private def anyFieldHelpers: TextTree[PyValue] = {
    q"""def _encode_any_field(self, ctx: $baboonCodecContext, writer: $baboonLEDataOutputStream, expected_kind: $pyInt, static_domain, static_version, static_typeid, value: $baboonAnyOpaque) -> None:
       |    if value.meta.kind != expected_kind:
       |        raise $baboonCodecException.EncoderFailure(
       |            f"any: meta-kind 0x{value.meta.kind & 0xFF:02x} does not match field-declared 0x{expected_kind & 0xFF:02x}"
       |        )
       |    if isinstance(value, $baboonAnyOpaqueUeba):
       |        any_blob = value.bytes
       |    elif isinstance(value, $baboonAnyOpaqueJson):
       |        if ctx.facade is None:
       |            raise $baboonCodecException.EncoderFailure(
       |                "Cannot encode AnyOpaqueJson into UEBA without a facade reference. Pass BaboonCodecContext.with_facade(use_indices, facade) into encode(), or supply AnyOpaqueUeba directly."
       |            )
       |        any_conv_result = ctx.facade.json_to_ueba_bytes(value.meta, value.json, static_domain=static_domain, static_version=static_version, static_typeid=static_typeid)
       |        if isinstance(any_conv_result, $baboonLeftType):
       |            raise any_conv_result.value
       |        any_blob = any_conv_result.value
       |    else:
       |        raise $baboonCodecException.EncoderFailure(
       |            f"unexpected AnyOpaque subclass: {type(value).__name__}"
       |        )
       |    # Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
       |    any_meta_buf = $pyBytesIO()
       |    any_meta_writer = $baboonLEDataOutputStream(any_meta_buf)
       |    $baboonAnyMetaCodec.write_bin(any_meta_writer, value.meta)
       |    any_meta_bytes = any_meta_buf.getvalue()
       |    any_total_length = 4 + len(any_meta_bytes) + len(any_blob)
       |    writer.write_i32(any_total_length)
       |    writer.write_i32(len(any_meta_bytes))
       |    writer.write(any_meta_bytes)
       |    writer.write(any_blob)
       |
       |def _decode_any_field(self, wire: $baboonLEDataInputStream, expected_kind: $pyInt) -> $baboonAnyOpaque:
       |    any_total_length = wire.read_i32()
       |    if any_total_length < 0:
       |        raise $baboonCodecException.DecoderFailure(
       |            f"any: negative total-length {any_total_length}"
       |        )
       |    any_meta_length = wire.read_i32()
       |    if any_meta_length < 0:
       |        raise $baboonCodecException.DecoderFailure(
       |            f"any: negative meta-length {any_meta_length}"
       |        )
       |    if any_total_length < 4 + any_meta_length:
       |        raise $baboonCodecException.DecoderFailure(
       |            f"any: total-length {any_total_length} smaller than 4 + meta-length {any_meta_length}"
       |        )
       |    any_meta, any_bytes_read = $baboonAnyMetaCodec.read_bin_with_length(wire)
       |    if any_bytes_read > any_meta_length:
       |        raise $baboonCodecException.DecoderFailure(
       |            f"any: meta bytes-read {any_bytes_read} exceeded meta-length window {any_meta_length}"
       |        )
       |    if any_bytes_read < any_meta_length:
       |        # Forward-compat: skip future meta-extension bytes within the meta-length window.
       |        any_skip = any_meta_length - any_bytes_read
       |        any_skip_buf = wire.stream.read(any_skip)
       |        if len(any_skip_buf) != any_skip:
       |            raise $baboonCodecException.DecoderFailure(
       |                f"any: short read while skipping meta-extension bytes, expected {any_skip} got {len(any_skip_buf)}"
       |            )
       |    if any_meta.kind != expected_kind:
       |        raise $baboonCodecException.DecoderFailure(
       |            f"any: wire kind 0x{any_meta.kind & 0xFF:02x} does not match field-declared 0x{expected_kind & 0xFF:02x}"
       |        )
       |    any_blob_len = any_total_length - 4 - any_meta_length
       |    any_blob = wire.stream.read(any_blob_len)
       |    if len(any_blob) != any_blob_len:
       |        raise $baboonCodecException.DecoderFailure(
       |            f"any: short read on blob, expected {any_blob_len} got {len(any_blob)}"
       |        )
       |    return $baboonAnyOpaqueUeba(any_meta, any_blob)
       |""".stripMargin
  }

  private def genForeignTypesBodies(name: PyType): (TextTree[PyValue], TextTree[PyValue]) = {
    (
      q"""raise ValueError(f"${name.name} is a foreign type")""",
      q"""raise ValueError(f"${name.name} is a foreign type")""",
    )
  }

  override def codecType(tid: TypeId.User): PyType = {
    val typeName = s"${tid.name.name.capitalize}_UEBACodec"
    val moduleId = typeTranslator
      .toPyModule(tid, domain.version, evolution, pyFileTools.definitionsBasePkg)
    PyType(moduleId, typeName)
  }

  override def codecMeta(tid: TypeId.User): PyCodecTranslator.CodecMeta = {
    val meta = q"""@$pyStaticMethod
                  |def codec_ueba():
                  |    return ${codecType(tid)}.instance()""".stripMargin
    PyCodecTranslator.CodecMeta(meta)
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Py) &&
    pyTarget.language.generateUebaCodecs && (pyTarget.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "ueba"
}
