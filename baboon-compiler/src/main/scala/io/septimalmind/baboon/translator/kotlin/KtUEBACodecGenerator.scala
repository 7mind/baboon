package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.CompilerTarget.KtTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.kotlin.KtCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.kotlin.KtDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.kotlin.KtTypes.*
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class KtUEBACodecGenerator(
  trans: KtTypeTranslator,
  target: KtTarget,
  domain: Domain,
  evo: BaboonEvolution,
  ktDomainTreeTools: KtDomainTreeTools,
  ktTypes: KtTypes,
) extends KtCodecTranslator {
  import ktTypes.*

  // In KMP mode, the buffer IS the BaboonBinaryWriter; in JVM mode, it's the ByteArrayOutputStream
  private val bufferSizeExpr: String = if (ktTypes.multiplatform) "fakeWriter.size()" else "writeMemoryStream.size()"

  override def translate(
    defn: DomainMember.User,
    ktRef: KtValue.KtType,
    srcRef: KtValue.KtType,
  ): Option[TextTree[KtValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto  => Some(genDtoBodies(ktRef, d))
        case e: Typedef.Enum => Some(genEnumBodies(ktRef, e))
        case a: Typedef.Adt  => Some(genAdtBodies(ktRef, a))
        case f: Typedef.Foreign =>
          f.bindings.get(BaboonLang.Kotlin) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
            case _                                                                  => Some(genForeignBodies(ktRef))
          }
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          val insulatedEnc =
            q"""if (this !== LazyInstance.value) {
               |  LazyInstance.value.encode(ctx, writer, instance)
               |  return
               |}
               |
               |$enc
               |""".stripMargin.trim

          val insulatedDec =
            q"""if (this !== LazyInstance.value) {
               |  return LazyInstance.value.decode(ctx, wire)
               |}
               |
               |$dec
               |""".stripMargin.trim

          val branchDecoder = defn.defn match {
            case d: Typedef.Dto => genBranchDecoder(ktRef, d)
            case _              => None
          }

          genCodec(
            defn,
            ktRef,
            srcRef,
            insulatedEnc,
            insulatedDec,
            branchDecoder,
          )
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: KtValue.KtType,
    srcRef: KtValue.KtType,
    enc: TextTree[KtValue],
    dec: TextTree[KtValue],
    branchDecoder: Option[TextTree[KtValue]],
  ): TextTree[KtValue] = {
    val isEncoderEnabled = target.language.enableDeprecatedEncoders || domain.version == evo.latest
    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = d.fields.filter(f => domain.refMeta(f.tpe).len.isVariable)
        val comment = varlens.map(f => q"// ${f.toString}").joinN()
        q"""$comment
           |return ${varlens.size.toString}.toShort()""".stripMargin

      case _: Typedef.Enum    => q"""return 0.toShort()"""
      case _: Typedef.Adt     => q"""return 0.toShort()"""
      case _: Typedef.Foreign => q"""throw $javaIllegalArgumentException("${name.name} is a foreign type")"""

      case d: Typedef.Contract =>
        throw new IllegalArgumentException(s"BUG: contract codec should not be rendered: $d")
      case d: Typedef.Service =>
        throw new IllegalArgumentException(s"BUG: service codec should not be rendered: $d")
    }

    val indexMethods = List(
      q"""override fun indexElementsCount(ctx: $baboonCodecContext): $ktShort {
         |  ${indexBody.shift(2).trim}
         |}""".stripMargin
    )

    val encoderMethods = if (isEncoderEnabled) {
      List(
        q"""override fun encode(ctx: $baboonCodecContext, writer: $binaryOutput, instance: $name) {
           |  ${enc.shift(2).trim}
           |}
           |""".stripMargin
      )
    } else Nil

    val decoderMethods = List(
      q"""override fun decode(ctx: $baboonCodecContext, wire: $binaryInput): $name {
         |  ${dec.shift(2).trim}
         |}""".stripMargin
    )

    val anyHelpers: List[TextTree[KtValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil

    val baseMethods = encoderMethods ++ decoderMethods
      ++ branchDecoder.map {
        body =>
          q"""fun decodeBranch(ctx: $baboonCodecContext, wire: $binaryInput): $name {
             |  ${body.shift(2).trim}
             |}""".stripMargin
      }.toList ++ indexMethods ++ anyHelpers

    val codecIface = q"$baboonBinCodec<$name>"
    val cName      = codecName(srcRef)
    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecBase<$name, $codecIface>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecBase<$name, $codecIface>"
        case _ if defn.isAdt                                => q"$baboonBinCodecBaseGeneratedAdt<$name, $codecIface>"
        case _                                              => q"$baboonBinCodecBaseGenerated<$name, $codecIface>"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecNoEncoder<$name, $codecIface>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecNoEncoder<$name, $codecIface>"
        case _ if defn.isAdt                                => q"$baboonBinCodecNoEncoderGeneratedAdt<$name, $codecIface>"
        case _                                              => q"$baboonBinCodecNoEncoderGenerated<$name, $codecIface>"
      }
    }
    val parents = List(q"$cParent", q"$baboonBinCodecIndexed")
    val meta    = renderMeta(defn, ktDomainTreeTools.makeCodecMeta(defn))

    q"""object ${cName.asName} : ${parents.join(", ")} {
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${meta.joinN().shift(2).trim}
       |
       |  override val LazyInstance: $baboonLazy<$codecIface> = $baboonLazy { ${cName.asName} }
       |  override val instance: $codecIface get() = LazyInstance.value
       |}
     """.stripMargin
  }

  private def genForeignBodies(name: KtValue.KtType): (TextTree[KtValue], TextTree[KtValue]) = {
    (
      q"""throw $javaIllegalArgumentException("${name.name} is a foreign type")""",
      q"""throw $javaIllegalArgumentException("${name.name} is a foreign type")""",
    )
  }

  private def genAdtBodies(name: KtValue.KtType, adt: Typedef.Adt): (TextTree[KtValue], TextTree[KtValue]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList.map {
      case (m, idx) =>
        val branchNs   = q"${adt.id.name.name}"
        val branchName = m.name.name
        val fqBranch   = q"$branchNs.$branchName"

        val adtRef = trans.toKtTypeRefKeepForeigns(m, domain, evo)
        val cName  = codecName(adtRef)

        val castedName = branchName.toLowerCase

        val encBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.instance.encode(ctx, writer, $castedName)"""
        } else {
          q"""writer.writeByte(${idx.toString})
             |$cName.instance.encode(ctx, writer, $castedName)
           """.stripMargin
        }

        val decBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.decodeBranch(ctx, wire)"""
        } else {
          q"""$cName.instance.decode(ctx, wire)"""
        }

        (
          q"""is $fqBranch -> {
             |  val $castedName = instance
             |  ${encBody.shift(2).trim}
             |}""".stripMargin,
          q"""${idx.toString} -> $decBody
             |""".stripMargin,
        )
    }

    (
      q"""when (instance) {
         |  ${branches.map(_._1).joinN().shift(2).trim}
         |}
         |""".stripMargin,
      q"""val asByte = wire.readByte().toInt()
         |
         |return when (asByte) {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |
         |  else -> throw $genericException("Cannot decode to ${name.name}: no matching value")
         |}
         |""".stripMargin,
    )
  }

  private def genEnumBodies(name: KtValue.KtType, e: Typedef.Enum): (TextTree[KtValue], TextTree[KtValue]) = {
    val branches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val obj = EnumWireStyle.wireName(m.name)
        (
          q"$name.$obj -> writer.writeByte(${idx.toString})",
          q"${idx.toString} -> $name.$obj",
        )
    }

    (
      q"""when (instance) {
         |  ${branches.map(_._1).joinN().shift(2).trim}
         |}
         """.stripMargin,
      q"""val asByte = wire.readByte().toInt()
         |
         |return when (asByte) {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |  else -> throw $genericException("Cannot decode to ${name.name}: no matching value")
         |}
         |""".stripMargin,
    )
  }

  private def genBranchDecoder(
    name: KtValue.KtType,
    d: Typedef.Dto,
  ): Option[TextTree[KtValue]] = {
    d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(name, fields.map(_._2)))
      case _ =>
        None
    }
  }

  private def genDtoBodies(name: KtValue.KtType, dto: Typedef.Dto): (TextTree[KtValue], TextTree[KtValue]) = {
    val fields = fieldsOf(dto)

    val noIndex = Seq(
      q"writer.writeByte(header.toInt())",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).joinN()

    val indexedBody = if (ktTypes.multiplatform) {
      q"""header = (header.toInt() or 1).toByte()
         |writer.writeByte(header.toInt())
         |val fakeWriter = $binaryOutput()
         |@Suppress("UNUSED_VARIABLE") val _init = Unit
         |${fields.map(_._3).joinN().trim}
         |writer.write(fakeWriter.toByteArray())""".stripMargin
    } else {
      q"""header = (header.toInt() or 1).toByte()
         |writer.writeByte(header.toInt())
         |val writeMemoryStream = $byteArrayOutputStream()
         |try {
         |  val fakeWriter = $binaryOutput(writeMemoryStream)
         |  try {
         |    ${fields.map(_._3).joinN().shift(4).trim}
         |  } finally {
         |    fakeWriter.close()
         |  }
         |  writeMemoryStream.flush()
         |  writer.write(writeMemoryStream.toByteArray())
         |} finally {
         |  writeMemoryStream.close()
         |}""".stripMargin
    }

    val fenc =
      q"""var header: $ktByte = 0
         |
         |if (ctx.useIndices) {
         |  ${indexedBody.shift(2).trim}
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
        q"""val marker = wire.readByte().toInt()
           |assert(marker == ${idx.toString})
           |return decodeBranch(ctx, wire)""".stripMargin
      case _ => fdec
    }
    (enc, dec)
  }

  private def dtoDec(name: KtValue.KtType, fields: List[TextTree[KtValue]]): TextTree[KtValue] = {
    q"""val index = this.readIndex(ctx, wire)
       |if (ctx.useIndices) assert(index.size == indexElementsCount(ctx).toInt())
       |return $name(
       |  ${fields.join(",\n").shift(2).trim}
       |)
       |""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[KtValue], TextTree[KtValue], TextTree[KtValue])] = {
    dto.fields.map {
      field =>
        val fieldRef   = q"instance.${field.name.name}"
        val enc        = mkEncoder(field.tpe, fieldRef, q"writer")
        val fakeEnc    = mkEncoder(field.tpe, fieldRef, q"fakeWriter")
        val decoder    = mkDecoder(field.tpe)
        val decodeTree = q"${field.name.name} = $decoder"

        val w = domain.refMeta(field.tpe).len match {
          case BinReprLen.Fixed(bytes) =>
            q"""{
               |  // ${field.toString}
               |  val before = $bufferSizeExpr
               |  ${fakeEnc.shift(2).trim}
               |  val after = $bufferSizeExpr
               |  val length = after - before
               |  assert(length == ${bytes.toString})
               |}""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = v match {
              case BinReprLen.Unknown() =>
                q"assert(after >= before) { \"Got after=$$after, before=$$before\" }"

              case BinReprLen.Alternatives(variants) =>
                q"assert(setOf(${variants.mkString(", ")}).contains(length)) { \"Got length=$$length\" }"

              case BinReprLen.Range(min, max) =>
                (
                  Seq(q"assert(length >= ${min.toString}) { \"Got length=$$length\" }") ++
                  max.toSeq.map(m => q"assert(length <= ${m.toString}) { \"Got length=$$length\" }")
                ).joinN()
            }

            q"""{
               |  // ${field.toString}
               |  val before = $bufferSizeExpr
               |  writer.writeInt(before)
               |  ${fakeEnc.shift(2).trim}
               |  val after = $bufferSizeExpr
               |  val length = after - before
               |  writer.writeInt(length)
               |  ${sanityChecks.shift(2).trim}
               |}""".stripMargin
        }

        (enc, decodeTree, w)
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[KtValue] = {
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
              case TypeId.Builtins.u08 => q"wire.readByte().toUByte()"
              case TypeId.Builtins.u16 => q"wire.readShort().toUShort()"
              case TypeId.Builtins.u32 => q"wire.readInt().toUInt()"
              case TypeId.Builtins.u64 => q"wire.readLong().toULong()"
              case TypeId.Builtins.f32 => q"wire.readFloat()"
              case TypeId.Builtins.f64 => q"wire.readDouble()"

              case TypeId.Builtins.f128  => if (ktTypes.multiplatform) q"$baboonBinTools.readBaboonDecimal(wire)" else q"$baboonBinTools.readBigDecimal(wire)"
              case TypeId.Builtins.str   => q"$baboonBinTools.readString(wire)"
              case TypeId.Builtins.bytes => q"$baboonBinTools.readByteString(wire)"

              case TypeId.Builtins.uid => q"$baboonBinTools.readUid(wire)"
              case TypeId.Builtins.tsu =>
                if (ktTypes.multiplatform) q"$baboonBinTools.readTimestamp(wire)" else q"$baboonBinTools.readTimestamp(wire)"
              case TypeId.Builtins.tso =>
                if (ktTypes.multiplatform) q"$baboonBinTools.readTimestampOffset(wire)" else q"$baboonBinTools.readTimestamp(wire)"

              case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Kotlin) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ =>
                    val targetTpe = codecName(trans.toKtTypeRefKeepForeigns(u, domain, evo))
                    q"""$targetTpe.instance.decode(ctx, wire)"""
                }
              case _ =>
                val targetTpe = codecName(trans.toKtTypeRefKeepForeigns(u, domain, evo))
                q"""$targetTpe.instance.decode(ctx, wire)"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            val readByteExpr = if (ktTypes.multiplatform) "wire.readByte().toInt()" else "wire.read()"
            q"""(if ($readByteExpr == 0) null else ${mkDecoder(c.args.head)})"""
          case TypeId.Builtins.map =>
            val keyDecoder   = mkDecoder(c.args.head)
            val valueDecoder = mkDecoder(c.args.last)
            q"(0 until wire.readInt()).associate { ($keyDecoder) to ($valueDecoder) }"
          case TypeId.Builtins.lst =>
            q"(0 until wire.readInt()).map { ${mkDecoder(c.args.head)} }"
          case TypeId.Builtins.set =>
            q"(0 until wire.readInt()).map { ${mkDecoder(c.args.head)} }.toSet()"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a)
    }
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[KtValue], wref: TextTree[KtValue]): TextTree[KtValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit => q"$wref.writeBoolean($ref)"
              case TypeId.Builtins.i08 => q"$wref.writeByte($ref.toInt())"
              case TypeId.Builtins.i16 => q"$wref.writeShort($ref.toInt())"
              case TypeId.Builtins.i32 => q"$wref.writeInt($ref)"
              case TypeId.Builtins.i64 => q"$wref.writeLong($ref)"
              case TypeId.Builtins.u08 => q"$wref.writeByte($ref.toInt())"
              case TypeId.Builtins.u16 => q"$wref.writeShort($ref.toInt())"
              case TypeId.Builtins.u32 => q"$wref.writeInt($ref.toInt())"
              case TypeId.Builtins.u64 => q"$wref.writeLong($ref.toLong())"
              case TypeId.Builtins.f32 => q"$wref.writeFloat($ref)"
              case TypeId.Builtins.f64 => q"$wref.writeDouble($ref)"
              case TypeId.Builtins.f128 =>
                if (ktTypes.multiplatform) q"$baboonBinTools.writeBaboonDecimal($wref, $ref)" else q"$baboonBinTools.writeBigDecimal($wref, $ref)"
              case TypeId.Builtins.str   => q"$baboonBinTools.writeString($wref, $ref)"
              case TypeId.Builtins.bytes => q"$baboonBinTools.writeByteString($wref, $ref)"
              case TypeId.Builtins.uid   => q"$baboonBinTools.writeUid($wref, $ref)"
              case TypeId.Builtins.tsu =>
                if (ktTypes.multiplatform) q"$baboonBinTools.writeTimestamp($wref, $ref)" else q"$baboonBinTools.writeTimestamp($wref, $ref)"
              case TypeId.Builtins.tso =>
                if (ktTypes.multiplatform) q"$baboonBinTools.writeTimestampOffset($wref, $ref)" else q"$baboonBinTools.writeTimestamp($wref, $ref)"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Kotlin) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, wref)
                  case _ =>
                    val targetTpe = codecName(trans.toKtTypeRefKeepForeigns(u, domain, evo))
                    q"""$targetTpe.instance.encode(ctx, $wref, $ref)"""
                }
              case _ =>
                val targetTpe = codecName(trans.toKtTypeRefKeepForeigns(u, domain, evo))
                q"""$targetTpe.instance.encode(ctx, $wref, $ref)"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""if ($ref == null) {
               |  $wref.writeByte(0)
               |} else {
               |  $wref.writeByte(1)
               |  ${mkEncoder(c.args.head, q"$ref", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.map =>
            q"""$wref.writeInt($ref.size)
               |$ref.forEach { (k, v) ->
               |  ${mkEncoder(c.args.head, q"k", wref).shift(2).trim}
               |  ${mkEncoder(c.args.last, q"v", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.lst =>
            q"""$wref.writeInt($ref.size)
               |$ref.forEach { i ->
               |  ${mkEncoder(c.args.head, q"i", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.set =>
            q"""$wref.writeInt($ref.size)
               |$ref.forEach { i ->
               |  ${mkEncoder(c.args.head, q"i", wref).shift(2).trim}
               |}""".stripMargin

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref, wref)
    }
  }

  // Deep walk (mirrors Scala/C#/Rust hasAnyField): a codec object needs the any-field helpers if
  // any direct or nested-via-Constructor-arg field has type `any`.
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

  // Encode delegates to the per-codec-object `encodeAnyField` helper. This site wires the expected
  // kind byte and the field's static (codec-gen-time) fallbacks for cross-format meta resolution.
  // See `anyStaticFallbacks` for the per-variant table.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[KtValue], wref: TextTree[KtValue]): TextTree[KtValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"encodeAnyField(ctx, $wref, ${expectedHex}.toByte(), $staticDom, $staticVer, $staticTid, $ref)"
  }

  // Decode delegates to the per-codec-object `decodeAnyField` helper, returning an
  // `AnyOpaqueUeba` (the helper's narrow return type — `mkDecoder`'s field type position is
  // `AnyOpaque`, the supertype, so this widens implicitly).
  private def mkAnyDecoder(a: TypeRef.Any): TextTree[KtValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"decodeAnyField(wire, ${expectedHex}.toByte())"
  }

  // Static fallbacks for the cross-format facade helpers (`jsonToUebaBytes`/`uebaToJson`). The
  // wire `meta` may omit components that are pinned by the field's static declaration; the codec
  // emits whatever is statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade.buildSyntheticTypeMeta` for the merge semantics. Per spec table:
  //   A=(null,null,null), B=(currentDomain,null,null), C=(currentDomain,currentVersion,null),
  //   D1=(null,null,underlyingFqid), D2=(currentDomain,null,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // Duplicated across Scala/C#/Rust/Kotlin — extraction deferred (see PR 4.2 ledger entry's DRY
  // analysis): textual emission diverges by language flavor.
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[KtValue], TextTree[KtValue], TextTree[KtValue]) = {
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

  // Per-codec-object helpers consolidating the any-field framing, kind-check, and
  // buffer-then-write / read-then-skip paths — emitted at most once per codec object that has any
  // any-bearing field. Mirrors `CSUEBACodecGenerator.anyFieldHelpers` and
  // `RsUEBACodecGenerator.anyFieldHelpers`. Wire layout (locked, see
  // docs/drafts/20260424-1738-any-opaque-fields.md §"Wire format"):
  //   length:i32 | meta-length:i32 | meta-kind:u8 | meta-strings | blob
  //
  // Multiplatform fork: JVM uses `ByteArrayOutputStream` + `LEDataOutputStream` to buffer the meta;
  // KMP uses `BaboonBinaryWriter()` directly (no nested stream). Both expose `toByteArray()`.
  // Negative-i32 sanity guards on the on-wire lengths run before any usize arithmetic — Kotlin
  // `Int` is 32-bit signed and `4 + Int.MIN_VALUE` overflows silently otherwise (PR-12-D01 lesson
  // applied to Kotlin).
  private def anyFieldHelpers: TextTree[KtValue] = {
    val metaWriteBlock: TextTree[KtValue] = if (ktTypes.multiplatform) {
      q"""val anyMetaWriter = $binaryOutput()
         |$baboonAnyMetaCodec.writeBin(value.meta, anyMetaWriter)
         |val anyMetaBytes = anyMetaWriter.toByteArray()""".stripMargin
    } else {
      q"""val anyMetaBuf = $byteArrayOutputStream()
         |val anyMetaWriter = $binaryOutput(anyMetaBuf)
         |$baboonAnyMetaCodec.writeBin(value.meta, anyMetaWriter)
         |anyMetaWriter.flush()
         |val anyMetaBytes = anyMetaBuf.toByteArray()""".stripMargin
    }

    q"""private fun encodeAnyField(
       |    ctx: $baboonCodecContext,
       |    writer: $binaryOutput,
       |    expectedKind: Byte,
       |    staticDomain: String?,
       |    staticVersion: String?,
       |    staticTypeid: String?,
       |    value: $baboonAnyOpaque,
       |) {
       |    if (value.meta.kind != expectedKind) {
       |        throw $baboonCodecException.EncoderFailure(
       |            "any: meta-kind 0x" + (value.meta.kind.toInt() and 0xFF).toString(16).padStart(2, '0') +
       |            " does not match field-declared 0x" + (expectedKind.toInt() and 0xFF).toString(16).padStart(2, '0')
       |        )
       |    }
       |    val anyBlob: ByteArray = when (value) {
       |        is $baboonAnyOpaqueUeba -> value.bytes
       |        is $baboonAnyOpaqueJson -> {
       |            val anyFacade = ctx.facade ?: throw $baboonCodecException.EncoderFailure(
       |                "Cannot encode AnyOpaqueJson into UEBA without a facade reference. Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), or supply AnyOpaqueUeba directly."
       |            )
       |            val anyConvResult = anyFacade.jsonToUebaBytes(value.meta, value.json, staticDomain, staticVersion, staticTypeid)
       |            when (anyConvResult) {
       |                is $baboonEither.Left -> throw anyConvResult.value
       |                is $baboonEither.Right -> anyConvResult.value
       |            }
       |        }
       |    }
       |    // Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
       |    ${metaWriteBlock.shift(4).trim}
       |    val anyTotalLength: Int = 4 + anyMetaBytes.size + anyBlob.size
       |    writer.writeInt(anyTotalLength)
       |    writer.writeInt(anyMetaBytes.size)
       |    writer.write(anyMetaBytes)
       |    writer.write(anyBlob)
       |}
       |
       |private fun decodeAnyField(wire: $binaryInput, expectedKind: Byte): $baboonAnyOpaqueUeba {
       |    val anyTotalLength = wire.readInt()
       |    if (anyTotalLength < 0) {
       |        throw $baboonCodecException.DecoderFailure(
       |            "any: negative total-length " + anyTotalLength
       |        )
       |    }
       |    val anyMetaLength = wire.readInt()
       |    if (anyMetaLength < 0) {
       |        throw $baboonCodecException.DecoderFailure(
       |            "any: negative meta-length " + anyMetaLength
       |        )
       |    }
       |    if (anyTotalLength < 4 + anyMetaLength) {
       |        throw $baboonCodecException.DecoderFailure(
       |            "any: total-length " + anyTotalLength + " smaller than 4 + meta-length " + anyMetaLength
       |        )
       |    }
       |    val anyReadResult = $baboonAnyMetaCodec.readBinWithLength(wire)
       |    val anyMeta = anyReadResult.first
       |    val anyBytesRead = anyReadResult.second
       |    if (anyBytesRead > anyMetaLength) {
       |        throw $baboonCodecException.DecoderFailure(
       |            "any: meta bytes-read " + anyBytesRead + " exceeded meta-length window " + anyMetaLength
       |        )
       |    }
       |    if (anyBytesRead < anyMetaLength) {
       |        // Forward-compat: skip future meta-extension bytes within the meta-length window.
       |        val anySkip = ByteArray(anyMetaLength - anyBytesRead)
       |        wire.readFully(anySkip)
       |    }
       |    if (anyMeta.kind != expectedKind) {
       |        throw $baboonCodecException.DecoderFailure(
       |            "any: wire kind 0x" + (anyMeta.kind.toInt() and 0xFF).toString(16).padStart(2, '0') +
       |            " does not match field-declared 0x" + (expectedKind.toInt() and 0xFF).toString(16).padStart(2, '0')
       |        )
       |    }
       |    val anyBlobLen = anyTotalLength - 4 - anyMetaLength
       |    val anyBlob = ByteArray(anyBlobLen)
       |    wire.readFully(anyBlob)
       |    return $baboonAnyOpaqueUeba(anyMeta, anyBlob)
       |}""".stripMargin
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[KtValue]] = {
    defn.defn match {
      case _: Typedef.Enum => meta.map(_.valueField)
      case f: Typedef.Foreign =>
        f.bindings.get(BaboonLang.Kotlin) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => meta.map(_.refValueField)
          case _                                                                  => meta.map(_.valueField)
        }
      case _ => meta.map(_.refValueField)
    }
  }

  def codecName(name: KtValue.KtType): KtValue.KtType = {
    KtValue.KtType(name.pkg, s"${name.name}_UEBACodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User, name: KtValue.KtType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"val codecUeba: $baboonBinCodec<$name> by lazy { ${codecName(name)}.instance }"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Kotlin) &&
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
