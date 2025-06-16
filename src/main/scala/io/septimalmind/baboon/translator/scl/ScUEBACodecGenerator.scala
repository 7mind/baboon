package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.translator.scl
import io.septimalmind.baboon.translator.scl.ScCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.model.{BaboonEvolution, BinReprLen, Domain, DomainMember, Owner, TypeId, TypeRef, Typedef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class ScUEBACodecGenerator(
  trans: ScTypeTranslator,
  csDomTrees: ScTreeTools,
  target: ScTarget,
  domain: Domain,
  evo: BaboonEvolution,
  csTypeInfo: ScTypeInfo,
) extends ScCodecTranslator {

  override def translate(
    defn: DomainMember.User,
    csRef: ScValue.ScType,
    srcRef: ScValue.ScType,
  ): Option[TextTree[ScValue]] = {
    val isLatestVersion = domain.version == evo.latest

    (defn.defn match {
      case d: Typedef.Dto      => Some(genDtoBodies(csRef, d))
      case e: Typedef.Enum     => Some(genEnumBodies(csRef, e))
      case a: Typedef.Adt      => Some(genAdtBodies(csRef, a))
      case _: Typedef.Foreign  => Some(genForeignBodies(csRef))
      case _: Typedef.Contract => None
      case _: Typedef.Service  => None
    }).map {
      case (enc, dec) =>
        if (!isLatestVersion) {
          (q"""throw new RuntimeException("Type ${defn.id.toString}@${domain.version.toString} is deprecated, encoder was not generated")""", dec)
        } else {
          (enc, dec)
        }
    }.map {
      case (enc, dec) =>
        // plumbing reference leaks
        val insulatedEnc =
          q"""if (this ne LazyInstance.value)
             |{
             |  LazyInstance.value.encode(ctx, writer, value)
             |  return
             |}
             |
             |$enc
             |""".stripMargin.trim

        val insulatedDec =
          q"""if (this ne LazyInstance.value)
             |{
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
          !defn.defn.isInstanceOf[Typedef.Foreign],
          branchDecoder,
        )
    }
  }

  private def genCodec(
    defn: DomainMember.User,
    name: ScValue.ScType,
    srcRef: ScValue.ScType,
    enc: TextTree[ScValue],
    dec: TextTree[ScValue],
    addExtensions: Boolean,
    branchDecoder: Option[TextTree[ScValue]],
  ): TextTree[ScValue] = {
    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = d.fields.filter(f => domain.refMeta(f.tpe).len.isVariable)
        val comment = varlens.map(f => q"// ${f.toString}").join("\n")
        q"""$comment
           |${varlens.size.toString}""".stripMargin

      case _: Typedef.Enum    => q"""0"""
      case _: Typedef.Adt     => q"""0"""
      case _: Typedef.Foreign => q"""throw new IllegalArgumentException("${name.name} is a foreign type")"""

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

    val baseMethods = List(
      q"""def encode(ctx: $baboonCodecContext, writer: $binaryOutput, value: $name): $scUnit = {
         |  ${enc.shift(2).trim}
         |}
         |
         |def decode(ctx: $baboonCodecContext, wire: $binaryInput): $name = {
         |  ${dec.shift(2).trim}
         |}""".stripMargin
    ) ++ branchDecoder.map {
      body =>
        q"""private $name DecodeBranch(ctx: $baboonCodecContext, wire: $binaryInput) = {
           |    ${body.shift(2).trim}
           |}""".stripMargin
    }.toList ++ indexMethods

    val codecIface = q"$baboonBinCodec[$name]"
    val (parents, methods) = defn.defn match {
      case _: Typedef.Enum =>
        (List(q"$baboonBinCodec[$name]", q"$baboonBinCodecIndexed"), baseMethods)
      case _ =>
        val extensions = List(
//          q"""def encode(ctx: $baboonCodecContext, writer: $binaryOutput, value: $iBaboonGenerated) = {
//             |  if (!value.isInstanceOf[$name])
//             |      throw new Exception("Expected to have ${name.name} type")
//             |
//             |  encode(ctx, writer, dvalue)
//             |}""".stripMargin
//          q"""$iBaboonGenerated $iBaboonStreamCodec<$iBaboonGenerated, $binaryOutput, $binaryInput>.decode($baboonCodecContext ctx, $binaryInput wire)
//             |{
//             |    return Decode(ctx, wire);
//             |}""".stripMargin,
        )

        val adtParents = defn.id.owner match {
          case Owner.Adt(_) => List(q"$iBaboonAdtMemberMeta")
          case _            => List.empty
        }
        val extParents = /*List(q"$baboonBinCodec[$iBaboonGenerated]") ++*/ adtParents

        val mm = if (addExtensions) {
          baseMethods ++ extensions
        } else {
          baseMethods
        }

        val baseParents = List(codecIface, q"$baboonBinCodecIndexed")

        val pp = if (addExtensions) {
          baseParents ++ extParents
        } else {
          baseParents
        }

        (pp, mm)
    }

    val cName = codecName(srcRef)

    val meta = q""
    /**
      * csDomTrees
      * .makeMeta(defn, isCodec = true)
      * .join("\n")
      * .shift(2)
      * .trim
      */

    q"""object ${cName.asName} extends ${parents.join(" with ")} {
       |  ${methods.join("\n\n").shift(2).trim}
       |
       |  ${meta.shift(2).trim}
       |
       |  private val LazyInstance: $baboonLazy[$codecIface] = $baboonLazy($cName)
       |
       |  def instance: $codecIface = LazyInstance.value
       |}
     """.stripMargin
  }

  private def genForeignBodies(name: ScValue.ScType) = {
    (
      q"""throw new IllegalArgumentException("${name.name} is a foreign type")""",
      q"""throw new IllegalArgumentException("${name.name} is a foreign type")""",
    )
  }

  private def genAdtBodies(name: ScValue.ScType, a: Typedef.Adt) = {
    val branches = a.dataMembers(domain).zipWithIndex.toList.map {
      case (m, idx) =>
        val branchNs   = q"${csTypeInfo.adtNsName(a.id)}"
        val branchName = m.name.name
        val fqBranch   = q"$branchNs.$branchName"

        val adtRef = trans.toScTypeRefKeepForeigns(m, domain, evo)
        val cName  = codecName(adtRef)

        val castedName = branchName.toLowerCase

        val encBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.instance.encode(ctx, writer, $castedName)"""
        } else {
          q"""writer.writeByte(${idx.toString});
             |$cName.instance.encode(ctx, writer, $castedName)
           """.stripMargin
        }

        val decBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""return (($cName)$cName.instance).decodeBranch(ctx, wire)"""
        } else {
          q"""return $cName.instance.decode(ctx, wire)"""
        }

        (
          q"""if (value.isInstanceOf[$fqBranch])
             |{   
             |  val $castedName = value.asInstanceOf[$fqBranch]
             |  ${encBody.shift(2).trim}
             |  return;
             |}""".stripMargin,
          q"""if (asByte == ${idx.toString})
             |{
             |  ${decBody.shift(2).trim}
             |}""".stripMargin,
        )
    }

    (
      q"""${branches.map(_._1).join("\n")}
         |
         |throw new $genericException(s"Cannot encode {value} to ${name.name}: no matching value")""".stripMargin,
      q"""val asByte = wire.readByte();
         |
         |${branches.map(_._2).join("\n")}
         |
         |throw new $genericException(s"Cannot decode {wire} to ${name.name}: no matching value")""".stripMargin,
    )
  }

  private def genEnumBodies(name: ScValue.ScType, e: Typedef.Enum) = {
    val branches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        (
          q"""if (value == $name.${m.name})
             |{
             |   writer.writeByte(${idx.toString})
             |   return
             |}""".stripMargin,
          q"""if (asByte == ${idx.toString})
             |{
             |   return $name.${m.name}
             |}""".stripMargin,
        )
    }

    (
      q"""${branches.map(_._1).join("\n")}
         |
         |throw new $genericException(s"Cannot encode {value} to ${name.name}: no matching value")""".stripMargin,
      q"""val asByte = wire.readByte()
         |
         |${branches.map(_._2).join("\n")}
         |
         |throw new $genericException(s"Cannot decode {wire} to ${name.name}: no matching value")""".stripMargin,
    )
  }

  private def genBranchDecoder(
    name: ScValue.ScType,
    d: Typedef.Dto,
  ): Option[TextTree[ScValue]] = {

    d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(name, fields.map { case (a, b, _) => (a, b) }))
      case _ =>
        None
    }
  }

  private def genDtoBodies(name: ScValue.ScType, d: Typedef.Dto) = {
    val fields = fieldsOf(d)

    val noIndex = Seq(
      q"writer.writeByte(header)",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).join("\n")

    val fenc =
      q"""var header: $scByte = 0b0000000;
         |
         |if (true)
         |{
         |  header = (header | 0b0000001).toByte
         |  writer.writeByte(header)
         |  val writeMemoryStream = new $byteArrayOutputStream()
         |  try  {
         |    val fakeWriter = new $binaryOutput(writeMemoryStream)
         |    try {
         |      ${fields.map(_._3).join("\n").shift(6).trim}
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

    val fdec =
      dtoDec(name, fields.map { case (a, b, _) => (a, b) })

    def adtBranchIndex(id: TypeId.User) = {
      domain.defs.meta
        .nodes(id)
        .asInstanceOf[DomainMember.User]
        .defn
        .asInstanceOf[Typedef.Adt]
        .dataMembers(domain)
        .zipWithIndex
        .find(_._1 == d.id)
        .get
        ._2
    }

    val enc = d.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)

        q"""writer.writeByte(${idx.toString})
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = d.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)

        q"""val marker = wire.readByte()
           |assert(marker == ${idx.toString})
           |return DecodeBranch(ctx, wire)""".stripMargin
      case _ => fdec
    }

    (enc, dec)
  }

  private def dtoDec(name: ScValue.ScType, fields: List[(TextTree[ScValue], TextTree[ScValue])]) = {
    q"""val index = this.readIndex(ctx, wire)
       |
       |if (true)
       |{
       |  assert(index.count == indexElementsCount(ctx))
       |}
       |
       |$name(
       |${fields.map(_._2).join(",\n").shift(2)}
       |);
       |""".stripMargin
  }

  private def fieldsOf(d: Typedef.Dto) = {
    d.fields.map {
      f =>
        val fieldRef = q"value.${f.name.name}"
        val enc      = mkEncoder(f.tpe, fieldRef, q"writer")
        val fakeEnc  = mkEncoder(f.tpe, fieldRef, q"fakeWriter")
        val dec      = mkDecoder(f.tpe)

        val w = domain.refMeta(f.tpe).len match {
          case BinReprLen.Fixed(bytes) =>
            q"""{
               |  // ${f.toString}
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
               |  // ${f.toString}
               |  val before = writeMemoryStream.size()
               |  writer.writeInt(before)
               |  ${fakeEnc.shift(2).trim}
               |  val after = writeMemoryStream.size()
               |  val length = after - before
               |  writer.writeInt(length)
               |  ${sanityChecks.shift(2).trim};
               |}""".stripMargin
        }

        (enc, dec, w)
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

              case TypeId.Builtins.f128 => q"$baboonBinTools.readBigDecimal(wire)"
              case TypeId.Builtins.str  => q"$baboonBinTools.readString(wire)"

              case TypeId.Builtins.uid                       => q"new $scUid(wire.readLong(), wire.readLong())"
              case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.decodeFromBin(wire)"

              case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
            q"""$targetTpe.instance.decode(ctx, wire)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""( if (wire.read() == 0) $scOption.empty else $scOption(${mkDecoder(c.args.head)}) )""".stripMargin
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
              case TypeId.Builtins.bit                       => q"$wref.writeBoolean($ref)"
              case TypeId.Builtins.i08                       => q"$wref.writeByte($ref)"
              case TypeId.Builtins.i16                       => q"$wref.writeShort($ref)"
              case TypeId.Builtins.i32                       => q"$wref.writeInt($ref)"
              case TypeId.Builtins.i64                       => q"$wref.writeLong($ref)"
              case TypeId.Builtins.u08                       => q"$wref.writeByte($ref)"
              case TypeId.Builtins.u16                       => q"$wref.writeShort($ref)"
              case TypeId.Builtins.u32                       => q"$wref.writeInt($ref)"
              case TypeId.Builtins.u64                       => q"$wref.writeLong($ref)"
              case TypeId.Builtins.f32                       => q"$wref.writeFloat($ref)"
              case TypeId.Builtins.f64                       => q"$wref.writeDouble($ref)"
              case TypeId.Builtins.f128                      => q"???" // q"$wref.Write($ref)"
              case TypeId.Builtins.str                       => q"???" // q"$wref.Write($ref)"
              case TypeId.Builtins.uid                       => q"???" // q"$wref.Write($ref.ToByteArray())"
              case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.encodeToBin($ref, $wref)"
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
            q"""if ($ref == null)
               |{
               |  $wref.writeByte(0);
               |}
               |else
               |{
               |  $wref.writeByte(1);
               |  ${mkEncoder(c.args.head, q"$ref.get", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.map =>
            q"""$wref.writeInt($ref.size)
               |$ref.foreach {
               |  case (k, v) =>
               |  ${mkEncoder(c.args.head, q"k", wref).shift(2).trim}
               |  ${mkEncoder(c.args.last, q"v", wref).shift(2).trim}
               |
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

  def codecName(name: ScValue.ScType): ScValue.ScType = {
    ScValue.ScType(name.pkg, s"${name.name}_UEBACodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User, name: ScValue.ScType): scl.ScCodecTranslator.CodecMeta = {
    CodecMeta(
      q"""public $baboonBinCodec[$name] Codec_UEBA()
         |{
         |  return ${codecName(name)}.instance
         |}""".stripMargin
    )
  }
}
