package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.CompilerTarget.KtTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.kotlin.KtCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.kotlin.KtDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.kotlin.KtTypes.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class KtUEBACodecGenerator(
  trans: KtTypeTranslator,
  target: KtTarget,
  domain: Domain,
  evo: BaboonEvolution,
  ktDomainTreeTools: KtDomainTreeTools,
) extends KtCodecTranslator {

  override def translate(
    defn: DomainMember.User,
    ktRef: KtValue.KtType,
    srcRef: KtValue.KtType,
  ): Option[TextTree[KtValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto      => Some(genDtoBodies(ktRef, d))
        case e: Typedef.Enum     => Some(genEnumBodies(ktRef, e))
        case a: Typedef.Adt      => Some(genAdtBodies(ktRef, a))
        case f: Typedef.Foreign =>
          f.bindings.get(BaboonLang.Kotlin) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
            case _ => Some(genForeignBodies(ktRef))
          }
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          val insulatedEnc =
            q"""if (this !== LazyInstance.value) {
               |  LazyInstance.value.encode(ctx, writer, value)
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
        q"""override fun encode(ctx: $baboonCodecContext, writer: $binaryOutput, value: $name) {
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

    val baseMethods = encoderMethods ++ decoderMethods
      ++ branchDecoder.map {
        body =>
          q"""fun decodeBranch(ctx: $baboonCodecContext, wire: $binaryInput): $name {
             |  ${body.shift(2).trim}
             |}""".stripMargin
      }.toList ++ indexMethods

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
          q"""($cName as $cName).decodeBranch(ctx, wire)"""
        } else {
          q"""$cName.instance.decode(ctx, wire)"""
        }

        (
          q"""is $fqBranch -> {
             |  val $castedName = value as $fqBranch
             |  ${encBody.shift(2).trim}
             |}""".stripMargin,
          q"""${idx.toString} -> $decBody
             |""".stripMargin,
        )
    }

    (
      q"""when (value) {
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
        (
          q"$name.${m.name} -> writer.writeByte(${idx.toString})",
          q"${idx.toString} -> $name.${m.name}",
        )
    }

    (
      q"""when (value) {
         |  ${branches.map(_._1).joinN().shift(2)}
         |}
         """.stripMargin,
      q"""val asByte = wire.readByte().toInt()
         |
         |return when (asByte) {
         |  ${branches.map(_._2).joinN().shift(2)}
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

    val fenc =
      q"""var header: $ktByte = 0
         |
         |if (ctx.useIndices) {
         |  header = (header.toInt() or 1).toByte()
         |  writer.writeByte(header.toInt())
         |  val writeMemoryStream = $byteArrayOutputStream()
         |  try {
         |    val fakeWriter = $binaryOutput(writeMemoryStream)
         |    try {
         |      ${fields.map(_._3).joinN().shift(6).trim}
         |    } finally {
         |      fakeWriter.close()
         |    }
         |    writeMemoryStream.flush()
         |    writer.write(writeMemoryStream.toByteArray())
         |  } finally {
         |    writeMemoryStream.close()
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
            q"""(if (wire.read() == 0) null else ${mkDecoder(c.args.head)})"""
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
    }
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[KtValue], wref: TextTree[KtValue]): TextTree[KtValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit   => q"$wref.writeBoolean($ref)"
              case TypeId.Builtins.i08   => q"$wref.writeByte($ref.toInt())"
              case TypeId.Builtins.i16   => q"$wref.writeShort($ref.toInt())"
              case TypeId.Builtins.i32   => q"$wref.writeInt($ref)"
              case TypeId.Builtins.i64   => q"$wref.writeLong($ref)"
              case TypeId.Builtins.u08   => q"$wref.writeByte($ref.toInt())"
              case TypeId.Builtins.u16   => q"$wref.writeShort($ref.toInt())"
              case TypeId.Builtins.u32   => q"$wref.writeInt($ref.toInt())"
              case TypeId.Builtins.u64   => q"$wref.writeLong($ref.toLong())"
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
               |  ${mkEncoder(c.args.head, q"$ref!!", wref).shift(2).trim}
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
    }
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
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
