package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.scl.ScCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.scl.ScDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class ScUEBACodecGenerator(
  trans: ScTypeTranslator,
  target: ScTarget,
  domain: Domain,
  evo: BaboonEvolution,
  scDomainTreeTools: ScDomainTreeTools,
) extends ScCodecTranslator {

  override def translate(
    defn: DomainMember.User,
    csRef: ScValue.ScType,
    srcRef: ScValue.ScType,
  ): Option[TextTree[ScValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto  => Some(genDtoBodies(csRef, d))
        case e: Typedef.Enum => Some(genEnumBodies(csRef, e))
        case a: Typedef.Adt  => Some(genAdtBodies(csRef, a))
        case f: Typedef.Foreign =>
          f.bindings.get(BaboonLang.Scala) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
            case _                                                                  => Some(genForeignBodies(csRef))
          }
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          // plumbing reference leaks
          val insulatedEnc =
            q"""if (this ne LazyInstance.value) {
               |  LazyInstance.value.encode(ctx, writer, value)
               |  return
               |}
               |
               |$enc
               |""".stripMargin.trim

          val insulatedDec =
            q"""if (this ne LazyInstance.value) {
               |  return LazyInstance.value.decode(ctx, wire)
               |}
               |
               |$dec
               |""".stripMargin.trim

          val branchDecoder = defn.defn match {
            case d: Typedef.Dto => genBranchDecoder(csRef, d)
            case _              => None
          }

          genCodec(
            defn,
            csRef,
            srcRef,
            insulatedEnc,
            insulatedDec,
            branchDecoder,
          )
      }
    } else None
  }

  // Per-codec-object helpers consolidating the any-field framing, kind-check, and
  // buffer-then-write / read-then-skip paths — emitted at most once per codec object.
  // See spec §172 (factor any-field encode/decode into helpers).
  private def anyFieldHelpers: TextTree[ScValue] = {
    q"""private def encodeAnyField(
       |  ctx: $baboonCodecContext,
       |  writer: $binaryOutput,
       |  expectedKind: $scByte,
       |  staticDomain: $scOption[$scString],
       |  staticVersion: $scOption[$scString],
       |  staticTypeid: $scOption[$scString],
       |  value: $baboonAnyOpaque,
       |): $scUnit = {
       |  if (value.meta.kind != expectedKind) {
       |    throw $baboonCodecException.EncoderFailure(s"any: meta-kind mismatch on encode: expected 0x$${(expectedKind & 0xFF).toHexString}, got 0x$${(value.meta.kind & 0xFF).toHexString}")
       |  }
       |  val anyBytes: $scArray[$scByte] = value match {
       |    case anyUeba: $baboonAnyOpaqueUeba =>
       |      anyUeba.bytes
       |    case anyJson: $baboonAnyOpaqueJson =>
       |      val f = ctx.facade.getOrElse(
       |        throw $baboonCodecException.EncoderFailure(
       |          "Cannot encode AnyOpaqueJson into UEBA without a facade reference. Pass BaboonCodecContext.WithFacade(useIndices, facade) into encode(), or supply AnyOpaqueUeba directly."
       |        )
       |      )
       |      f.jsonToUebaBytes(anyJson.meta, anyJson.json, staticDomain, staticVersion, staticTypeid) match {
       |        case Right(b) => b
       |        case Left(e)  => throw e
       |      }
       |  }
       |  val anyMetaBuf    = new $byteArrayOutputStream()
       |  val anyMetaWriter = new $binaryOutput(anyMetaBuf)
       |  try {
       |    $baboonAnyMetaCodec.writeBin(value.meta, anyMetaWriter)
       |  } finally {
       |    anyMetaWriter.close()
       |  }
       |  val anyMetaBytes = anyMetaBuf.toByteArray
       |  val anyLength    = 4 + anyMetaBytes.length + anyBytes.length
       |  writer.writeInt(anyLength)
       |  writer.writeInt(anyMetaBytes.length)
       |  writer.write(anyMetaBytes)
       |  writer.write(anyBytes)
       |}
       |
       |private def decodeAnyField(wire: $binaryInput, expectedKind: $scByte): $baboonAnyOpaqueUeba = {
       |  val anyLength            = wire.readInt()
       |  val anyMetaLen           = wire.readInt()
       |  val (anyMeta, anyMetaBytesRead) = $baboonAnyMetaCodec.readBinWithLength(wire)
       |  if (anyMetaBytesRead > anyMetaLen) {
       |    throw $baboonCodecException.DecoderFailure(s"any: meta consumed $$anyMetaBytesRead bytes but meta-length=$$anyMetaLen")
       |  }
       |  if (anyMetaBytesRead < anyMetaLen) {
       |    // Skip future meta extension bytes within the meta-length window (forward-compat).
       |    wire.skipBytes(anyMetaLen - anyMetaBytesRead)
       |  }
       |  if (anyMeta.kind != expectedKind) {
       |    throw new $genericException(s"any: meta-kind mismatch: expected 0x$${(expectedKind & 0xFF).toHexString}, got 0x$${(anyMeta.kind & 0xFF).toHexString}")
       |  }
       |  val anyBlobLen = anyLength - 4 - anyMetaLen
       |  if (anyBlobLen < 0) {
       |    throw new $genericException(s"any: negative blob length $$anyBlobLen (length=$$anyLength, metaLen=$$anyMetaLen)")
       |  }
       |  val anyBlob = new $scArray[$scByte](anyBlobLen)
       |  wire.readFully(anyBlob)
       |  $baboonAnyOpaqueUeba(anyMeta, anyBlob)
       |}""".stripMargin
  }

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

  private def genCodec(
    defn: DomainMember.User,
    name: ScValue.ScType,
    srcRef: ScValue.ScType,
    enc: TextTree[ScValue],
    dec: TextTree[ScValue],
    branchDecoder: Option[TextTree[ScValue]],
  ): TextTree[ScValue] = {
    val isEncoderEnabled = target.language.enableDeprecatedEncoders || domain.version == evo.latest
    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = d.fields.filter(f => domain.refMeta(f.tpe).len.isVariable)
        val comment = varlens.map(f => q"// ${f.toString}").joinN()
        q"""$comment
           |${varlens.size.toString}""".stripMargin

      case _: Typedef.Enum    => q"""0"""
      case _: Typedef.Adt     => q"""0"""
      case _: Typedef.Foreign => q"""throw new $javaIllegalArgumentException("${name.name} is a foreign type")"""

      case d: Typedef.Contract =>
        throw new IllegalArgumentException(s"BUG: contract codec should not be rendered: $d")
      case d: Typedef.Service =>
        throw new IllegalArgumentException(s"BUG: service codec should not be rendered: $d")
    }

    val indexMethods = List(
      q"""def indexElementsCount(ctx: $baboonCodecContext): $scShort = {
         |  ${indexBody.shift(2).trim}
         |}""".stripMargin
    )

    val encoderMethods = if (isEncoderEnabled) {
      List(
        q"""def encode(ctx: $baboonCodecContext, writer: $binaryOutput, value: $name): $scUnit = {
           |  ${enc.shift(2).trim}
           |}
           |""".stripMargin
      )
    } else Nil

    val decoderMethods = List(
      q"""def decode(ctx: $baboonCodecContext, wire: $binaryInput): $scEither[$javaThrowable, $name] = {
         |  ${dec.shift(2).trim}
         |}""".stripMargin
    )

    val baseMethods = encoderMethods ++ decoderMethods
      ++ branchDecoder.map {
        body =>
          q"""def decodeBranch(ctx: $baboonCodecContext, wire: $binaryInput): $scEither[$javaThrowable, $name] = {
             |  ${body.shift(2).trim}
             |}""".stripMargin
      }.toList ++ indexMethods

    val codecIface = q"$baboonBinCodec[$name]"
    val cName      = codecName(srcRef)
    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecBase[$name, $codecIface]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecBase[$name, $codecIface]"
        case _ if defn.ownedByAdt                           => q"$baboonBinCodecBaseGeneratedAdt[$name, $codecIface]"
        case _                                              => q"$baboonBinCodecBaseGenerated[$name, $codecIface]"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecNoEncoder[$name, $codecIface]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecNoEncoder[$name, $codecIface]"
        case _ if defn.ownedByAdt                           => q"$baboonBinCodecNoEncoderGeneratedAdt[$name, $codecIface]"
        case _                                              => q"$baboonBinCodecNoEncoderGenerated[$name, $codecIface]"
      }
    }
    val parents = List(cParent, q"$baboonBinCodecIndexed")
    val meta    = renderMeta(defn, scDomainTreeTools.makeCodecMeta(defn))

    val anyHelpers: List[TextTree[ScValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil
    val tail                                = (anyHelpers ++ meta).joinNN()

    q"""object ${cName.name} extends ${parents.join(" with ")} {
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${tail.shift(2).trim}
       |
       |  override protected def LazyInstance: $baboonLazy[$codecIface] = $baboonLazy($cName)
       |  override def instance: $codecIface = LazyInstance.value
       |}
     """.stripMargin
  }

  private def genForeignBodies(name: ScValue.ScType): (TextTree[ScValue], TextTree[ScValue]) = {
    (
      q"""throw new $javaIllegalArgumentException("${name.name} is a foreign type")""",
      q"""throw new $javaIllegalArgumentException("${name.name} is a foreign type")""",
    )
  }

  private def genAdtBodies(name: ScValue.ScType, adt: Typedef.Adt): (TextTree[ScValue.ScType], TextTree[ScValue.ScType]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList.map {
      case (m, idx) =>
        val branchNs   = q"${adt.id.name.name}"
        val branchName = m.name.name
        val fqBranch   = q"$branchNs.$branchName"
        val cName      = q"${fqBranch}_UEBACodec"

        val castedName = branchName.toLowerCase

        val encBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.instance.encode(ctx, writer, $castedName)"""
        } else {
          q"""writer.writeByte(${idx.toString})
             |$cName.instance.encode(ctx, writer, $castedName)
           """.stripMargin
        }

        val decBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.instance.asInstanceOf[$cName.type].decodeBranch(ctx, wire)"""
        } else {
          q"""$cName.instance.decode(ctx, wire)"""
        }

        (
          q"case $castedName: $fqBranch => ${encBody.shift(2).trim}",
          q"case ${idx.toString} => $decBody",
        )
    }

    (
      q"""value match {
         |  ${branches.map(_._1).joinN().shift(2).trim}
         |}
         |""".stripMargin,
      q"""val asByte = wire.readByte();
         |
         |asByte match {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |
         |  case _ => Left(new $genericException(s"Cannot decode {wire} to ${name.name}: no matching value"))
         |}
         |""".stripMargin,
    )
  }

  private def genEnumBodies(name: ScValue.ScType, e: Typedef.Enum): (TextTree[ScValue.ScType], TextTree[ScValue.ScType]) = {
    val branches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val obj = EnumWireStyle.wireName(m.name)
        (
          q"case $name.$obj => writer.writeByte(${idx.toString})",
          q"case ${idx.toString} => Right($name.$obj)",
        )
    }

    (
      q"""value match {
         |  ${branches.map(_._1).joinN().shift(2).trim}
         |}
         """.stripMargin,
      q"""val asByte = wire.readByte()
         |
         |asByte match {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |  case _ => Left(new $genericException(s"Cannot decode {wire} to ${name.name}: no matching value"))
         |}
         |""".stripMargin,
    )
  }

  private def genBranchDecoder(
    name: ScValue.ScType,
    d: Typedef.Dto,
  ): Option[TextTree[ScValue]] = {
    d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(name, fields.map(_._2)))
      case _ =>
        None
    }
  }

  private def genDtoBodies(name: ScValue.ScType, dto: Typedef.Dto): (TextTree[ScValue], TextTree[ScValue]) = {
    val fields = fieldsOf(dto)

    val noIndex = Seq(
      q"writer.writeByte(header.toInt)",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).joinN()

    val fenc =
      q"""var header: $scByte = 0b0000000;
         |
         |if (ctx.useIndices) {
         |  header = (header | 0b0000001).toByte
         |  writer.writeByte(header.toInt)
         |  val writeMemoryStream = new $byteArrayOutputStream()
         |  try  {
         |    val fakeWriter = new $binaryOutput(writeMemoryStream)
         |    try {
         |      ${fields.map(_._3).joinN().shift(6).trim}
         |    } finally {
         |      fakeWriter.close()
         |    }
         |    writeMemoryStream.flush()
         |    writer.write(writeMemoryStream.toByteArray)
         |  } finally {
         |      writeMemoryStream.close()
         |  }
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

        q"""writer.writeByte(${idx.toString})
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)
        q"""val marker = wire.readByte()
           |assert(marker == ${idx.toString})
           |decodeBranch(ctx, wire)""".stripMargin
      case _ => fdec
    }
    (enc, dec)
  }

  private def dtoDec(name: ScValue.ScType, fields: List[TextTree[ScValue]]): TextTree[ScValue] = {
    q"""for {
       |  index  <- this.readIndex(ctx, wire)
       |  _      <- $scTry(if (ctx.useIndices) assert(index.size == indexElementsCount(ctx)) else ()).toEither
       |  result <- $scTry {
       |    $name(
       |      ${fields.join(",\n").shift(6).trim}
       |    )
       |  }.toEither
       |} yield result
       |""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[ScValue], TextTree[ScValue], TextTree[ScValue])] = {
    dto.fields.map {
      field =>
        val fieldRef   = q"value.${field.name.name}"
        val enc        = mkEncoder(field.tpe, fieldRef, q"writer")
        val fakeEnc    = mkEncoder(field.tpe, fieldRef, q"fakeWriter")
        val decoder    = mkDecoder(field.tpe)
        val decodeTree = q"${field.name.name} = $decoder"

        val w = domain.refMeta(field.tpe).len match {
          case BinReprLen.Fixed(bytes) =>
            q"""{
               |  // ${field.toString}
               |  val before = writeMemoryStream.size()
               |  ${fakeEnc.shift(2).trim}
               |  val after = writeMemoryStream.size()
               |  val length = after - before
               |  assert(length == ${bytes.toString})
               |}""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = v match {
              case BinReprLen.Unknown() =>
                q"assert(after >= before, s\"Got after={after}, before={before}\")"

              case BinReprLen.Alternatives(variants) =>
                q"assert($scSet(${variants.mkString(", ")}).contains(length), s\"Got length={length}\")"

              case BinReprLen.Range(min, max) =>
                (
                  Seq(q"assert(length >= ${min.toString}, s\"Got length={length}\")") ++
                  max.toSeq.map(m => q"assert(length <= ${m.toString}, $$\"Got length={length}\")")
                ).joinN()
            }

            q"""{
               |  // ${field.toString}
               |  val before = writeMemoryStream.size()
               |  writer.writeInt(before)
               |  ${fakeEnc.shift(2).trim}
               |  val after = writeMemoryStream.size()
               |  val length = after - before
               |  writer.writeInt(length)
               |  ${sanityChecks.shift(2).trim}
               |}""".stripMargin
        }

        (enc, decodeTree, w)
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[ScValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit => q"wire.readBoolean()"
              case TypeId.Builtins.i08 => q"wire.readByte()"
              case TypeId.Builtins.i16 => q"wire.readShort()"
              case TypeId.Builtins.i32 => q"wire.readInt()"
              case TypeId.Builtins.i64 => q"wire.readLong()"
              case TypeId.Builtins.u08 => q"wire.readByte()"
              case TypeId.Builtins.u16 => q"wire.readShort()"
              case TypeId.Builtins.u32 => q"wire.readInt()"
              case TypeId.Builtins.u64 => q"wire.readLong()"
              case TypeId.Builtins.f32 => q"wire.readFloat()"
              case TypeId.Builtins.f64 => q"wire.readDouble()"

              case TypeId.Builtins.f128  => q"$baboonBinTools.readBigDecimal(wire)"
              case TypeId.Builtins.str   => q"$baboonBinTools.readString(wire)"
              case TypeId.Builtins.bytes => q"$baboonBinTools.readByteString(wire)"

              case TypeId.Builtins.uid => q"$baboonBinTools.readUid(wire)"
              case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
                q"$baboonBinTools.readTimestamp(wire)"

              case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Scala) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ =>
                    val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
                    q"""$targetTpe.instance.decode(ctx, wire).toTry.get"""
                }
              case _ =>
                val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
                q"""$targetTpe.instance.decode(ctx, wire).toTry.get"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""(if (wire.read() == 0) $scOption.empty else $scOption(${mkDecoder(c.args.head)}))""".stripMargin
          case TypeId.Builtins.map =>
            val keyDecoder   = mkDecoder(c.args.head)
            val valueDecoder = mkDecoder(c.args.last)
            q"(0 until wire.readInt()).map(_ => ($keyDecoder -> $valueDecoder)).toMap"
          case TypeId.Builtins.lst =>
            q"(0 until wire.readInt()).map(_ => ${mkDecoder(c.args.head)}).toList"
          case TypeId.Builtins.set =>
            q"(0 until wire.readInt()).map(_ => ${mkDecoder(c.args.head)}).toSet"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a)
    }

  }

  // Wire layout (locked, see docs/drafts/20260424-1738-any-opaque-fields.md §"Wire format"):
  //   length:i32 | meta-length:i32 | meta-kind:u8 | meta-strings | blob
  // length covers everything after itself; meta-length covers (kind + strings); blob runs the rest.
  // The actual framing/kind-check/skip-extension logic lives in the per-codec-object
  // `decodeAnyField` helper emitted by `anyFieldHelpers`; this just wires the expected kind in.
  private def mkAnyDecoder(a: TypeRef.Any): TextTree[ScValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"""decodeAnyField(wire, $expectedHex.toByte)"""
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[ScValue], wref: TextTree[ScValue]): TextTree[ScValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit   => q"$wref.writeBoolean($ref)"
              case TypeId.Builtins.i08   => q"$wref.writeByte($ref.toInt)"
              case TypeId.Builtins.i16   => q"$wref.writeShort($ref.toInt)"
              case TypeId.Builtins.i32   => q"$wref.writeInt($ref)"
              case TypeId.Builtins.i64   => q"$wref.writeLong($ref)"
              case TypeId.Builtins.u08   => q"$wref.writeByte($ref.toInt)"
              case TypeId.Builtins.u16   => q"$wref.writeShort($ref.toInt)"
              case TypeId.Builtins.u32   => q"$wref.writeInt($ref)"
              case TypeId.Builtins.u64   => q"$wref.writeLong($ref)"
              case TypeId.Builtins.f32   => q"$wref.writeFloat($ref)"
              case TypeId.Builtins.f64   => q"$wref.writeDouble($ref)"
              case TypeId.Builtins.f128  => q"$baboonBinTools.writeBigDecimal($wref, $ref)"
              case TypeId.Builtins.str   => q"$baboonBinTools.writeString($wref, $ref)"
              case TypeId.Builtins.bytes => q"$baboonBinTools.writeByteString($wref, $ref)"
              case TypeId.Builtins.uid   => q"$baboonBinTools.writeUid($wref, $ref)"
              case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
                q"$baboonBinTools.writeTimestamp($wref, $ref)"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Scala) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, wref)
                  case _ =>
                    val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
                    q"""$targetTpe.instance.encode(ctx, $wref, $ref)"""
                }
              case _ =>
                val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
                q"""$targetTpe.instance.encode(ctx, $wref, $ref)"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""if ($ref.isEmpty) {
               |  $wref.writeByte(0);
               |}
               |else {
               |  $wref.writeByte(1);
               |  ${mkEncoder(c.args.head, q"$ref.get", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.map =>
            q"""$wref.writeInt($ref.size)
               |$ref.foreach {
               |  case (k, v) =>
               |  ${mkEncoder(c.args.head, q"k", wref).shift(2).trim}
               |  ${mkEncoder(c.args.last, q"v", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.lst =>
            q"""$wref.writeInt($ref.size)
               |$ref.foreach {
               |  i =>
               |    ${mkEncoder(c.args.head, q"i", wref).shift(4).trim}
               |}""".stripMargin

          case TypeId.Builtins.set =>
            q"""$wref.writeInt($ref.size)
               |$ref.foreach {
               |  i =>
               |    ${mkEncoder(c.args.head, q"i", wref).shift(4).trim}
               |}""".stripMargin

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref, wref)
    }
  }

  // Encode delegates to the per-codec-object `encodeAnyField` helper emitted by
  // `anyFieldHelpers`. This site wires the expected kind byte and the field's static
  // (codec-gen-time) fallbacks for cross-format meta resolution; see `anyStaticFallbacks`.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[ScValue], wref: TextTree[ScValue]): TextTree[ScValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"""encodeAnyField(ctx, $wref, $expectedHex.toByte, $staticDom, $staticVer, $staticTid, $ref)"""
  }

  // Static fallbacks for the cross-format facade helpers (`jsonToUebaBytes` / `uebaToJson`).
  // The wire `meta` may omit components that are pinned by the field's static declaration; the
  // codec emits whatever is statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade.buildSyntheticTypeMeta` for the merge semantics.
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[ScValue], TextTree[ScValue], TextTree[ScValue]) = {
    val none                     = q"_root_.scala.None"
    def some(s: String)          = q"""_root_.scala.Some("$s")"""
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

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[ScValue]] = {
    defn.defn match {
      case _: Typedef.Enum => meta.map(_.valueField)
      case f: Typedef.Foreign =>
        f.bindings.get(BaboonLang.Scala) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => Nil
          case _                                                                  => meta.map(_.valueField)
        }
      case _ => meta.map(_.refValueField)
    }
  }

  def codecName(name: ScValue.ScType): ScValue.ScType = {
    ScValue.ScType(name.pkg, s"${name.name}_UEBACodec", name.inObject)
  }

  override def codecMeta(defn: DomainMember.User, name: ScValue.ScType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"def codecUeba: $baboonBinCodec[$name] = ${codecName(name)}.instance"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Scala) &&
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
