package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.CompilerTarget.DtTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.dart.DtCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.dart.DtDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.translator.dart.DtValue.DtType
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class DtUEBACodecGenerator(
  trans: DtTypeTranslator,
  target: DtTarget,
  domain: Domain,
  evo: BaboonEvolution,
  dtDomainTreeTools: DtDomainTreeTools,
) extends DtCodecTranslator {

  override def translate(
    defn: DomainMember.User,
    dtRef: DtType,
    srcRef: DtType,
  ): Option[TextTree[DtValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto  => Some(genDtoBodies(dtRef, d))
        case e: Typedef.Enum => Some(genEnumBodies(dtRef, e))
        case a: Typedef.Adt  => Some(genAdtBodies(dtRef, a))
        case f: Typedef.Foreign =>
          f.bindings.get(BaboonLang.Dart) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
            case _                                                                  => Some(genForeignBodies(dtRef))
          }
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          val branchDecoder = defn.defn match {
            case d: Typedef.Dto => genBranchDecoder(dtRef, d)
            case _              => None
          }

          genCodec(
            defn,
            dtRef,
            srcRef,
            enc,
            dec,
            branchDecoder,
          )
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: DtType,
    srcRef: DtType,
    enc: TextTree[DtValue],
    dec: TextTree[DtValue],
    branchDecoder: Option[TextTree[DtValue]],
  ): TextTree[DtValue] = {
    val isEncoderEnabled = domain.version == evo.latest
    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = d.fields.filter(f => domain.refMeta(f.tpe).len.isVariable)
        val comment = varlens.map(f => q"// ${f.toString}").joinN()
        q"""$comment
           |return ${varlens.size.toString};""".stripMargin

      case _: Typedef.Enum    => q"""return 0;"""
      case _: Typedef.Adt     => q"""return 0;"""
      case _: Typedef.Foreign => q"""throw ArgumentError('${name.name} is a foreign type');"""

      case d: Typedef.Contract =>
        throw new IllegalArgumentException(s"BUG: contract codec should not be rendered: $d")
      case d: Typedef.Service =>
        throw new IllegalArgumentException(s"BUG: service codec should not be rendered: $d")
    }

    val indexGetter =
      q"""@override
         |int get indexElementsCount {
         |  ${indexBody.shift(2).trim}
         |}""".stripMargin

    val encoderMethods = if (isEncoderEnabled) {
      List(
        q"""@override
           |void encode($baboonCodecContext ctx, $baboonBinWriter writer, $name value) {
           |  ${enc.shift(2).trim}
           |}
           |""".stripMargin
      )
    } else Nil

    val decoderMethods = List(
      q"""@override
         |$name decode($baboonCodecContext ctx, $baboonBinReader reader) {
         |  ${dec.shift(2).trim}
         |}""".stripMargin
    )

    val anyHelpers: List[TextTree[DtValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil

    val baseMethods = encoderMethods ++ decoderMethods
      ++ branchDecoder.map {
        body =>
          q"""$name decodeBranch($baboonCodecContext ctx, $baboonBinReader reader) {
             |  ${body.shift(2).trim}
             |}""".stripMargin
      }.toList ++ List(indexGetter) ++ anyHelpers

    val cName = codecName(srcRef)

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => baboonBinCodecBase
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => baboonBinCodecBase
        case _ if defn.isAdt                                => baboonBinCodecBaseGeneratedAdt
        case _                                              => baboonBinCodecBaseGenerated
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => baboonBinCodecNoEncoder
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => baboonBinCodecNoEncoder
        case _ if defn.isAdt                                => baboonBinCodecNoEncoderGeneratedAdt
        case _                                              => baboonBinCodecNoEncoderGenerated
      }
    }

    val meta = renderMeta(defn, dtDomainTreeTools.makeCodecMeta(defn))

    q"""class ${cName.asName} extends $cParent<$name> with $baboonBinCodecIndexed {
       |  const ${cName.asName}._();
       |  static const instance = ${cName.asName}._();
       |
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${meta.joinN().shift(2).trim}
       |}
     """.stripMargin
  }

  private def genForeignBodies(name: DtType): (TextTree[DtValue], TextTree[DtValue]) = {
    (
      q"""throw ArgumentError('${name.name} is a foreign type');""",
      q"""throw ArgumentError('${name.name} is a foreign type');""",
    )
  }

  private def genAdtBodies(name: DtType, adt: Typedef.Adt): (TextTree[DtValue], TextTree[DtValue]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList.map {
      case (m, idx) =>
        val branchName = m.name.name

        val adtRef = trans.toDtTypeRefKeepForeigns(m, domain, evo)
        val cName  = codecName(adtRef)

        val castedName = branchName.substring(0, 1).toLowerCase + branchName.substring(1)

        val encBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.instance.encode(ctx, writer, $castedName);"""
        } else {
          q"""writer.writeU8(${idx.toString});
             |$cName.instance.encode(ctx, writer, $castedName);
           """.stripMargin
        }

        val decBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""return $cName.instance.decodeBranch(ctx, reader);"""
        } else {
          q"""return $cName.instance.decode(ctx, reader);"""
        }

        (
          q"""if (value is $adtRef) {
             |  final $castedName = value;
             |  ${encBody.shift(2).trim}
             |}""".stripMargin,
          q"""case ${idx.toString}:
             |  ${decBody.shift(2).trim}""".stripMargin,
        )
    }

    val encElse = q"""throw ArgumentError('Cannot encode to ${name.name}: unexpected type $${value.runtimeType}');"""

    val encBranches = branches.map(_._1)
    val encChain = encBranches match {
      case Nil => encElse
      case _ =>
        val elseIfBranches = encBranches.tail.map {
          b =>
            q" else $b"
        }
        val chain = (Seq(encBranches.head) ++ elseIfBranches).join("")
        q"""$chain else {
           |  $encElse
           |}
           |""".stripMargin
    }

    (
      encChain,
      q"""final asByte = reader.readU8();
         |
         |switch (asByte) {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |  default:
         |    throw ArgumentError('Cannot decode to ${name.name}: no matching value for ordinal $$asByte');
         |}
         |""".stripMargin,
    )
  }

  private def genEnumBodies(name: DtType, e: Typedef.Enum): (TextTree[DtValue], TextTree[DtValue]) = {
    val encBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"case $name.${m.name}: writer.writeU8(${idx.toString}); break;"
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"case ${idx.toString}: return $name.${m.name};"
    }

    (
      q"""switch (value) {
         |  ${encBranches.joinN().shift(2)}
         |}
         """.stripMargin,
      q"""final asByte = reader.readU8();
         |
         |switch (asByte) {
         |  ${decBranches.joinN().shift(2)}
         |  default:
         |    throw ArgumentError('Cannot decode to ${name.name}: no matching value for ordinal $$asByte');
         |}
         |""".stripMargin,
    )
  }

  private def genBranchDecoder(
    name: DtType,
    d: Typedef.Dto,
  ): Option[TextTree[DtValue]] = {
    d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(name, fields.map(_._2)))
      case _ =>
        None
    }
  }

  private def genDtoBodies(name: DtType, dto: Typedef.Dto): (TextTree[DtValue], TextTree[DtValue]) = {
    val fields = fieldsOf(dto)

    val noIndex = Seq(
      q"writer.writeU8(header);",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).joinN()

    val fenc =
      q"""var header = 0;
         |
         |if (ctx.useIndices) {
         |  header = header | 1;
         |  writer.writeU8(header);
         |  final buffer = $baboonBinWriter();
         |  ${fields.map(_._3).joinN().shift(2).trim}
         |  writer.writeAll(buffer.toBytes());
         |} else {
         |  ${noIndex.shift(2).trim}
         |}
         |""".stripMargin

    val fdec = dtoDec(name, fields.map(_._2))

    def adtBranchIndex(id: TypeId.User) = {
      domain.defs.meta
        .nodes(id)
        .asInstanceOf[DomainMember.User]
        .defn
        .asInstanceOf[Typedef.Adt]
        .dataMembers(domain)
        .zipWithIndex
        .find(_._1 == dto.id)
        .get
        ._2
    }

    val enc = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)

        q"""writer.writeU8(${idx.toString});
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)
        q"""final marker = reader.readU8();
           |assert(marker == ${idx.toString});
           |return decodeBranch(ctx, reader);""".stripMargin
      case _ => fdec
    }
    (enc, dec)
  }

  private def dtoDec(name: DtType, fields: List[TextTree[DtValue]]): TextTree[DtValue] = {
    val fieldAssignments = if (fields.isEmpty) {
      q""
    } else {
      q"""${fields.join(",\n").shift(2).trim},"""
    }

    q"""final index = readIndex(ctx, reader);
       |if (ctx.useIndices) assert(index.length == indexElementsCount);
       |return $name(
       |  $fieldAssignments
       |);
       |""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[DtValue], TextTree[DtValue], TextTree[DtValue])] = {
    dto.fields.map {
      field =>
        val fieldRef   = q"value.${field.name.name}"
        val enc        = mkEncoder(field.tpe, fieldRef, q"writer", 0)
        val bufferEnc  = mkEncoder(field.tpe, fieldRef, q"buffer", 0)
        val decoder    = mkDecoder(field.tpe)
        val decodeTree = q"${field.name.name}: $decoder"

        val w = domain.refMeta(field.tpe).len match {
          case BinReprLen.Fixed(bytes) =>
            q"""{
               |  // ${field.toString}
               |  final before = buffer.position;
               |  ${bufferEnc.shift(2).trim}
               |  final after = buffer.position;
               |  final length = after - before;
               |  assert(length == ${bytes.toString});
               |}""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = v match {
              case BinReprLen.Unknown() =>
                q"""assert(after >= before, 'Got after=$$after, before=$$before');"""

              case BinReprLen.Alternatives(variants) =>
                q"""assert({${variants.mkString(", ")}}.contains(length), 'Got length=$$length');"""

              case BinReprLen.Range(min, max) =>
                (
                  Seq(q"""assert(length >= ${min.toString}, 'Got length=$$length');""") ++
                  max.toSeq.map(m => q"""assert(length <= ${m.toString}, 'Got length=$$length');""")
                ).joinN()
            }

            q"""{
               |  // ${field.toString}
               |  final before = buffer.position;
               |  writer.writeI32(before);
               |  ${bufferEnc.shift(2).trim}
               |  final after = buffer.position;
               |  final length = after - before;
               |  writer.writeI32(length);
               |  ${sanityChecks.shift(2).trim}
               |}""".stripMargin
        }

        (enc, decodeTree, w)
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[DtValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit => q"reader.readBool()"
              case TypeId.Builtins.i08 => q"reader.readI8()"
              case TypeId.Builtins.i16 => q"reader.readI16()"
              case TypeId.Builtins.i32 => q"reader.readI32()"
              case TypeId.Builtins.i64 => q"reader.readI64()"
              case TypeId.Builtins.u08 => q"reader.readU8()"
              case TypeId.Builtins.u16 => q"reader.readU16()"
              case TypeId.Builtins.u32 => q"reader.readU32()"
              case TypeId.Builtins.u64 => q"reader.readU64()"
              case TypeId.Builtins.f32 => q"reader.readF32()"
              case TypeId.Builtins.f64 => q"reader.readF64()"

              case TypeId.Builtins.f128  => q"reader.readDecimal()"
              case TypeId.Builtins.str   => q"reader.readString()"
              case TypeId.Builtins.bytes => q"reader.readBytes()"

              case TypeId.Builtins.uid => q"reader.readUuid()"
              case TypeId.Builtins.tsu => q"reader.readTsu()"
              case TypeId.Builtins.tso => q"reader.readTso()"

              case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Dart) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ =>
                    val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                    q"""$targetTpe.instance.decode(ctx, reader)"""
                }
              case _ =>
                val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                q"""$targetTpe.instance.decode(ctx, reader)"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            val innerDecoder = mkDecoder(c.args.head)
            q"""(reader.readBool() ? $innerDecoder : null)"""
          case TypeId.Builtins.map =>
            val keyDecoder   = mkDecoder(c.args.head)
            val valueDecoder = mkDecoder(c.args.last)
            q"""Map.fromEntries(List.generate(reader.readI32(), (_) => MapEntry($keyDecoder, $valueDecoder)))"""
          case TypeId.Builtins.lst =>
            q"""List.generate(reader.readI32(), (_) => ${mkDecoder(c.args.head)})"""
          case TypeId.Builtins.set =>
            q"""Set.of(List.generate(reader.readI32(), (_) => ${mkDecoder(c.args.head)}))"""
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a)
    }
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[DtValue], wref: TextTree[DtValue], depth: Int): TextTree[DtValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit   => q"$wref.writeBool($ref);"
              case TypeId.Builtins.i08   => q"$wref.writeI8($ref);"
              case TypeId.Builtins.i16   => q"$wref.writeI16($ref);"
              case TypeId.Builtins.i32   => q"$wref.writeI32($ref);"
              case TypeId.Builtins.i64   => q"$wref.writeI64($ref);"
              case TypeId.Builtins.u08   => q"$wref.writeU8($ref);"
              case TypeId.Builtins.u16   => q"$wref.writeU16($ref);"
              case TypeId.Builtins.u32   => q"$wref.writeU32($ref);"
              case TypeId.Builtins.u64   => q"$wref.writeU64($ref);"
              case TypeId.Builtins.f32   => q"$wref.writeF32($ref);"
              case TypeId.Builtins.f64   => q"$wref.writeF64($ref);"
              case TypeId.Builtins.f128  => q"$wref.writeDecimal($ref);"
              case TypeId.Builtins.str   => q"$wref.writeString($ref);"
              case TypeId.Builtins.bytes => q"$wref.writeBytes($ref);"
              case TypeId.Builtins.uid   => q"$wref.writeUuid($ref);"
              case TypeId.Builtins.tsu   => q"$wref.writeTsu($ref);"
              case TypeId.Builtins.tso   => q"$wref.writeTso($ref);"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Dart) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, wref, depth)
                  case _ =>
                    val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                    q"""$targetTpe.instance.encode(ctx, $wref, $ref);"""
                }
              case _ =>
                val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                q"""$targetTpe.instance.encode(ctx, $wref, $ref);"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            val nonNullRef = if (depth > 0) ref else q"$ref!"
            q"""if ($ref == null) {
               |  $wref.writeBool(false);
               |} else {
               |  $wref.writeBool(true);
               |  ${mkEncoder(c.args.head, nonNullRef, wref, depth + 1).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.map =>
            q"""$wref.writeI32($ref.length);
               |for (final entry in $ref.entries) {
               |  ${mkEncoder(c.args.head, q"entry.key", wref, depth + 1).shift(2).trim}
               |  ${mkEncoder(c.args.last, q"entry.value", wref, depth + 1).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.lst =>
            q"""$wref.writeI32($ref.length);
               |for (final item in $ref) {
               |  ${mkEncoder(c.args.head, q"item", wref, depth + 1).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.set =>
            q"""$wref.writeI32($ref.length);
               |for (final item in $ref) {
               |  ${mkEncoder(c.args.head, q"item", wref, depth + 1).shift(2).trim}
               |}""".stripMargin

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref, wref)
    }
  }

  // Deep walk (mirrors Scala/C#/Rust/Kotlin/Java/TS hasAnyField): a codec class needs the any-field
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

  // Encode delegates to the per-codec-class `encodeAnyField` helper. This site wires the expected
  // kind byte and the field's static (codec-gen-time) fallbacks for cross-format meta resolution.
  // See `anyStaticFallbacks` for the per-variant table.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[DtValue], wref: TextTree[DtValue]): TextTree[DtValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"encodeAnyField(ctx, $wref, $expectedHex, $staticDom, $staticVer, $staticTid, $ref);"
  }

  // Decode delegates to the per-codec-class `decodeAnyField` helper, returning an `AnyOpaqueUeba`
  // (the helper's narrow return type — `mkDecoder`'s field type position is `AnyOpaque`, the
  // sealed supertype, so the upcast is implicit).
  private def mkAnyDecoder(a: TypeRef.Any): TextTree[DtValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"decodeAnyField(reader, $expectedHex)"
  }

  // Static fallbacks for the cross-format facade helpers (`jsonToUebaBytes`/`uebaToJson`). The
  // wire `meta` may omit components that are pinned by the field's static declaration; the codec
  // emits whatever is statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade._buildSyntheticTypeMeta` for the merge semantics. Per spec table:
  //   A=(null,null,null), B=(currentDomain,null,null), C=(currentDomain,currentVersion,null),
  //   D1=(null,null,underlyingFqid), D2=(currentDomain,null,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // Duplicated across Scala/C#/Rust/Kotlin/Java/TS/Dart — extraction deferred (textual emission
  // diverges by language flavor; see PR 4.2 ledger entry's DRY analysis).
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[DtValue], TextTree[DtValue], TextTree[DtValue]) = {
    val none                     = q"null"
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
  // any-bearing field. Mirrors `JvUEBACodecGenerator.anyFieldHelpers` and
  // `TsUEBACodecGenerator.anyFieldHelpers`. Wire layout (locked, see
  // docs/drafts/20260424-1738-any-opaque-fields.md §"Wire format"):
  //   length:i32 | meta-length:i32 | meta-kind:u8 | meta-strings | blob
  //
  // PR-12-D01 lesson applied (Dart): Dart `int` is 64-bit so the failure mode differs from JVM
  // wraparound, but explicit negative-i32 sanity guards on the on-wire lengths keep the error
  // message specific (rather than letting the sublist call throw a generic RangeError).
  //
  // Cast-via-`as`: `ctx.facade` returns the `BaboonCodecsFacadeBase` opaque base (chosen by PR 8.1
  // to break the `baboon_runtime.dart` <-> `baboon_codecs_facade.dart` import cycle). The concrete
  // facade class carries `jsonToUebaBytes`; the cast is safe because `BaboonCodecContext.withFacade`
  // is the only construction path and accepts the same hierarchy.
  private def anyFieldHelpers: TextTree[DtValue] = {
    q"""void encodeAnyField(
       |    $baboonCodecContext ctx,
       |    $baboonBinWriter writer,
       |    int expectedKind,
       |    String? staticDomain,
       |    String? staticVersion,
       |    String? staticTypeid,
       |    $baboonAnyOpaque value,
       |) {
       |  if (value.meta.kind != expectedKind) {
       |    throw $baboonEncoderFailure(
       |      'any: meta-kind 0x' + (value.meta.kind & 0xFF).toRadixString(16).padLeft(2, '0') +
       |      ' does not match field-declared 0x' + (expectedKind & 0xFF).toRadixString(16).padLeft(2, '0'),
       |    );
       |  }
       |  $dtUint8List anyBlob;
       |  switch (value) {
       |    case $baboonAnyOpaqueUeba(:final bytes):
       |      anyBlob = bytes;
       |    case $baboonAnyOpaqueJson(:final meta, :final json):
       |      final anyFacadeBase = ctx.facade;
       |      if (anyFacadeBase == null) {
       |        throw $baboonEncoderFailure(
       |          'Cannot encode AnyOpaqueJson into UEBA without a facade reference. '
       |          'Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), '
       |          'or supply AnyOpaqueUeba directly.',
       |        );
       |      }
       |      // Downcast to the concrete facade — the marker base is empty by design (PR 8.1
       |      // import-cycle break). Construction goes through BaboonCodecContext.withFacade
       |      // which only accepts BaboonCodecsFacadeBase, but real callers pass BaboonCodecsFacade.
       |      final anyFacade = anyFacadeBase as $baboonCodecsFacade;
       |      final anyConvResult = anyFacade.jsonToUebaBytes(
       |        meta,
       |        json,
       |        staticDomain: staticDomain,
       |        staticVersion: staticVersion,
       |        staticTypeid: staticTypeid,
       |      );
       |      switch (anyConvResult) {
       |        case $baboonLeft(:final value):
       |          throw value;
       |        case $baboonRight(:final value):
       |          anyBlob = value;
       |      }
       |  }
       |  // Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
       |  final anyMetaBuf = $baboonBinWriter();
       |  $baboonAnyMetaCodec.writeBin(value.meta, anyMetaBuf);
       |  final anyMetaBytes = anyMetaBuf.toBytes();
       |  final anyTotalLength = 4 + anyMetaBytes.length + anyBlob.length;
       |  writer.writeI32(anyTotalLength);
       |  writer.writeI32(anyMetaBytes.length);
       |  writer.writeAll(anyMetaBytes);
       |  writer.writeAll(anyBlob);
       |}
       |
       |$baboonAnyOpaque decodeAnyField($baboonBinReader wire, int expectedKind) {
       |  final anyTotalLength = wire.readI32();
       |  if (anyTotalLength < 0) {
       |    throw $baboonDecoderFailure(
       |      'any: negative total-length $$anyTotalLength',
       |    );
       |  }
       |  final anyMetaLength = wire.readI32();
       |  if (anyMetaLength < 0) {
       |    throw $baboonDecoderFailure(
       |      'any: negative meta-length $$anyMetaLength',
       |    );
       |  }
       |  if (anyTotalLength < 4 + anyMetaLength) {
       |    throw $baboonDecoderFailure(
       |      'any: total-length $$anyTotalLength smaller than 4 + meta-length $$anyMetaLength',
       |    );
       |  }
       |  final anyReadResult = $baboonAnyMetaCodec.readBinWithLength(wire);
       |  final anyMeta = anyReadResult.$$1;
       |  final anyBytesRead = anyReadResult.$$2;
       |  if (anyBytesRead > anyMetaLength) {
       |    throw $baboonDecoderFailure(
       |      'any: meta bytes-read $$anyBytesRead exceeded meta-length window $$anyMetaLength',
       |    );
       |  }
       |  if (anyBytesRead < anyMetaLength) {
       |    // Forward-compat: skip future meta-extension bytes within the meta-length window.
       |    wire.skipBytes(anyMetaLength - anyBytesRead);
       |  }
       |  if (anyMeta.kind != expectedKind) {
       |    throw $baboonDecoderFailure(
       |      'any: wire kind 0x' + (anyMeta.kind & 0xFF).toRadixString(16).padLeft(2, '0') +
       |      ' does not match field-declared 0x' + (expectedKind & 0xFF).toRadixString(16).padLeft(2, '0'),
       |    );
       |  }
       |  final anyBlobLen = anyTotalLength - 4 - anyMetaLength;
       |  final anyBlob = wire.readNBytes(anyBlobLen);
       |  return $baboonAnyOpaqueUeba(anyMeta, anyBlob);
       |}""".stripMargin
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[DtValue]] = {
    defn.defn match {
      case _: Typedef.Enum => meta.map(_.valueField)
      case f: Typedef.Foreign =>
        f.bindings.get(BaboonLang.Dart) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => meta.map(_.refValueField)
          case _                                                                  => meta.map(_.valueField)
        }
      case _ => meta.map(_.refValueField)
    }
  }

  def codecName(name: DtType): DtType = {
    val baseFileName = name.importAs.getOrElse(trans.toSnakeCase(name.name))
    DtType(name.pkg, s"${name.name}_UebaCodec", name.fq, importAs = Some(baseFileName))
  }

  override def codecMeta(defn: DomainMember.User, name: DtType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"static final $baboonBinCodec<$name> codecUeba = ${codecName(name).asName}.instance;"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Dart) &&
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
