package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.scl.ScCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.scl.ScDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.model.*
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
        case d: Typedef.Dto      => Some(genDtoBodies(csRef, d))
        case e: Typedef.Enum     => Some(genEnumBodies(csRef, e))
        case a: Typedef.Adt      => Some(genAdtBodies(csRef, a))
        case _: Typedef.Foreign  => Some(genForeignBodies(csRef))
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
        case _ if defn.isAdt                                => q"$baboonBinCodecBaseGeneratedAdt[$name, $codecIface]"
        case _                                              => q"$baboonBinCodecBaseGenerated[$name, $codecIface]"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecNoEncoder[$name, $codecIface]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecNoEncoder[$name, $codecIface]"
        case _ if defn.isAdt                                => q"$baboonBinCodecNoEncoderGeneratedAdt[$name, $codecIface]"
        case _                                              => q"$baboonBinCodecNoEncoderGenerated[$name, $codecIface]"
      }
    }
    val parents = List(cParent, q"$baboonBinCodecIndexed")
    val meta    = renderMeta(defn, scDomainTreeTools.makeCodecMeta(defn))

    q"""object ${cName.asName} extends ${parents.join(" with ")} {
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${meta.joinN().shift(2).trim}
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

        val adtRef = trans.toScTypeRefKeepForeigns(m, domain, evo)
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
          q"""$cName.instance.asInstanceOf[$cName.type].decodeBranch(ctx, wire)"""
        } else {
          q"""$cName.instance.decode(ctx, wire)"""
        }

        (
          q"""case $castedName: $fqBranch => 
             |  ${encBody.shift(2).trim}
             |""".stripMargin,
          q"""case ${idx.toString} => $decBody
             |""".stripMargin,
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
        (
          q"case $name.${m.name} => writer.writeByte(${idx.toString})",
          q"case ${idx.toString} => Right($name.${m.name})",
        )
    }

    (
      q"""value match {
         |  ${branches.map(_._1).joinN().shift(2)}
         |}
         """.stripMargin,
      q"""val asByte = wire.readByte()
         |
         |asByte match {
         |  ${branches.map(_._2).joinN().shift(2)}
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
            val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
            q"""$targetTpe.instance.decode(ctx, wire).toTry.get"""
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
    }

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
            val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
            q"""$targetTpe.instance.encode(ctx, $wref, $ref)"""
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
    }
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[ScValue]] = {
    defn.defn match {
      case _: Typedef.Enum | _: Typedef.Foreign => meta.map(_.valueField)
      case _                                    => meta.map(_.refValueField)
    }
  }

  def codecName(name: ScValue.ScType): ScValue.ScType = {
    ScValue.ScType(name.pkg, s"${name.name}_UEBACodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User, name: ScValue.ScType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"def codecUeba: $baboonBinCodec[$name] = ${codecName(name)}.instance"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
