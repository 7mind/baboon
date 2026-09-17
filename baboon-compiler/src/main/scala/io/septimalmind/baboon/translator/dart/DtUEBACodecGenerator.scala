package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.{UebaLayoutPlan, UebaLengthCheckRenderer}
import io.septimalmind.baboon.CompilerTarget.DtTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.dart.DtCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.dart.DtDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.translator.dart.DtValue.DtType
import io.septimalmind.baboon.typer.EnumWireStyle
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
  private val layout = new UebaLayoutPlan(domain)

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
        // PR-26.7 round-2 (M26): drop dead `<F>_UebaCodec` emission for ALL Foreign typedefs
        // when an inline UEBA wire mapping is available:
        //   - BaboonRef-mapped: aliasedRef recurses through `mkEncoder`/`mkDecoder`.
        //   - stringy Custom (`dart.core.String` / bare `String`): inlined as `writeString`/
        //     `readString`.
        //   - non-stringy Custom with `runtimeMapping` (e.g. `ObscureInt` with `rt = i32`):
        //     recurses through `mkEncoder`/`mkDecoder` on the underlying TypeRef.
        // Non-stringy Custom WITHOUT `runtimeMapping` (e.g. `ForeignStruct`) retains the
        // throwing-stub codec class — no underlying primitive is declared, and the value-
        // position call site references this class. Closes PR-I.1d-N03 / PR-26.7-D01.
        case f: Typedef.Foreign =>
          DtForeignWirePlan.ueba(f) match {
            case _: DtForeignWirePlan.Inline => None
            case DtForeignWirePlan.Codec     => Some(genForeignBodies(dtRef))
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
        val varlens = layout.indexedFields(d)
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

        val castedName = trans.escapeDartKeyword(branchName.substring(0, 1).toLowerCase + branchName.substring(1))

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
        val pascal = EnumWireStyle.wireName(m.name)
        q"case $name.$pascal: writer.writeU8(${idx.toString}); break;"
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val pascal = EnumWireStyle.wireName(m.name)
        q"case ${idx.toString}: return $name.$pascal;"
    }

    (
      q"""switch (value) {
         |  ${encBranches.joinN().shift(2).trim}
         |}
         """.stripMargin,
      q"""final asByte = reader.readU8();
         |
         |switch (asByte) {
         |  ${decBranches.joinN().shift(2).trim}
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
         |  writer.writeBuffer(buffer);
         |} else {
         |  ${noIndex.shift(2).trim}
         |}
         |""".stripMargin

    val fdec = dtoDec(name, fields.map(_._2))

    val enc = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, dto.id)

        q"""writer.writeU8(${idx.toString});
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, dto.id)
        q"""final marker = reader.readU8();
           |if (marker != ${idx.toString}) throw FormatException("Unexpected UEBA ADT branch marker: " + marker.toString());
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
       |if (ctx.useIndices && index.length != indexElementsCount) throw FormatException("Unexpected UEBA index count: " + index.length.toString());
       |return $name(
       |  $fieldAssignments
       |);
       |""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[DtValue], TextTree[DtValue], TextTree[DtValue])] = {
    layout.fields(dto).map {
      case UebaLayoutPlan.FieldLayout(field, length) =>
        val dartName   = trans.escapeDartKeyword(field.name.name)
        val fieldRef   = q"value.$dartName"
        val enc        = mkEncoder(field.tpe, fieldRef, q"writer", 0)
        val bufferEnc  = mkEncoder(field.tpe, fieldRef, q"buffer", 0)
        val decoder    = mkDecoder(field.tpe)
        val decodeTree = q"$dartName: $decoder"

        val w = length match {
          case BinReprLen.Fixed(bytes) =>
            q"""{
               |  // ${field.toString}
               |  final before = buffer.position;
               |  ${bufferEnc.shift(2).trim}
               |  final after = buffer.position;
               |  final length = after - before;
               |  ${lengthChecks(BinReprLen.Fixed(bytes)).shift(2).trim}
               |}""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = lengthChecks(v)

            q"""{
               |  // ${field.toString}
               |  final before = buffer.position;
               |  if (before > ${UebaLayoutPlan.MaxIndexValue.toString}) throw FormatException("UEBA index offset exceeds i32");
               |  writer.writeI32(before);
               |  ${bufferEnc.shift(2).trim}
               |  final after = buffer.position;
               |  final length = after - before;
               |  ${sanityChecks.shift(2).trim}
               |  writer.writeI32(length);
               |}""".stripMargin
        }

        (enc, decodeTree, w)
    }
  }

  private def lengthChecks(length: BinReprLen): TextTree[DtValue] =
    UebaLengthCheckRenderer.render[DtValue](
      length,
      equalTo = bytes => q"length == ${bytes.toString}",
      oneOf   = bytes => q"{${bytes.mkString(", ")}}.contains(length)",
      enforce = condition => q"""if (!($condition)) throw FormatException("Invalid UEBA field length: " + length.toString());""",
    )

  private def mkDecoder(tpe: TypeRef): TextTree[DtValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar => DtScalarCodecOps.decodeUeba(s, q"reader")
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                DtForeignWirePlan.ueba(f) match {
                  case DtForeignWirePlan.Inline(ref) => mkDecoder(ref)
                  case DtForeignWirePlan.Codec =>
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
          case s: TypeId.BuiltinScalar => DtScalarCodecOps.encodeUeba(s, wref, ref)
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                DtForeignWirePlan.ueba(f) match {
                  case DtForeignWirePlan.Inline(wireRef) => mkEncoder(wireRef, ref, wref, depth)
                  case DtForeignWirePlan.Codec =>
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
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[DtValue], wref: TextTree[DtValue]): TextTree[DtValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = DtAnyFieldPlan.fallbacks(a, domain)
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

  // Public generated helpers remain forwarding methods for source compatibility.
  private def anyFieldHelpers: TextTree[DtValue] = {
    q"""void encodeAnyField(
       |    $baboonCodecContext ctx,
       |    $baboonBinWriter writer,
       |    int expectedKind,
       |    String? staticDomain,
       |    String? staticVersion,
       |    String? staticTypeid,
       |    $baboonAnyOpaque value,
       |) => $baboonEncodeAnyUebaField(ctx, writer, expectedKind, staticDomain, staticVersion, staticTypeid, value);
       |
       |$baboonAnyOpaque decodeAnyField($baboonBinReader wire, int expectedKind) =>
       |    $baboonDecodeAnyUebaField(wire, expectedKind);""".stripMargin
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[DtValue]] = {
    val (asValue, asRef): (MetaField => TextTree[DtValue], MetaField => TextTree[DtValue]) = (
      m => if (m.isCodecData) m.valueGetter else m.valueField,
      m => if (m.isCodecData) m.refValueGetter else m.refValueField,
    )
    defn.defn match {
      case _: Typedef.Enum => meta.map(asValue)
      case f: Typedef.Foreign =>
        f.bindings.get(BaboonLang.Dart) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => meta.map(asRef)
          case _                                                                  => meta.map(asValue)
        }
      case _ => meta.map(asRef)
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
    // PR-26.7 (M26): Foreign typedefs with an inline UEBA wire mapping no longer get a
    // `<F>_UebaCodec` class. Suppress them from `isActive` so the per-domain
    // `BaboonCodecsUeba` aggregator (DtBaboonTranslator codec-registration loop) doesn't
    // reference a dropped class. Suppression covers:
    //   - stringy Custom (`dart.core.String` / bare `String`) — round-1
    //   - non-stringy Custom with `runtimeMapping` (e.g. `ObscureInt` with `rt = i32`) —
    //     round-2 (closes PR-26.7-D01).
    // BaboonRef-aliased foreigns are also suppressed via `isBaboonRefForeign`. Non-stringy
    // Custom WITHOUT `runtimeMapping` still register a throwing-stub class.
    val isInlineForeign = domain.defs.meta.nodes.get(id).exists {
      case DomainMember.User(_, f: Typedef.Foreign, _, _) => DtForeignWirePlan.ueba(f).isInstanceOf[DtForeignWirePlan.Inline]
      case _                                              => false
    }
    !isInlineForeign &&
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
