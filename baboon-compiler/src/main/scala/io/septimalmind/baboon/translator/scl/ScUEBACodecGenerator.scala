package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.scl.ScCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.scl.ScDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.translator.{AnyFieldPlan, UebaLayoutPlan, UebaLengthCheckRenderer}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class ScUEBACodecGenerator(
  domainTypes: ScDomainTypes,
  target: ScTarget,
  domain: Domain,
  evo: BaboonEvolution,
  scDomainTreeTools: ScDomainTreeTools,
) extends ScCodecTranslator {
  private val layout = new UebaLayoutPlan(domain)

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
        val varlens = layout.indexedFields(d)
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

    val tail = meta.joinNN()

    q"""object ${cName.name} extends ${parents.join(" with ")} {
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${tail.shift(2).trim}
       |
       |  private lazy val cachedLazyInstance: $baboonLazy[$codecIface] = $baboonLazy($cName)
       |  override protected def LazyInstance: $baboonLazy[$codecIface] = cachedLazyInstance
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

        val castedName = escapeScKeyword(branchName.toLowerCase)

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

    val enc = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, dto.id)

        q"""writer.writeByte(${idx.toString})
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, dto.id)
        q"""val marker = wire.readByte() & 0xFF
           |if (marker != ${idx.toString}) Left(new $javaIllegalArgumentException("Unexpected UEBA ADT branch marker: " + marker))
           |else decodeBranch(ctx, wire)""".stripMargin
      case _ => fdec
    }
    (enc, dec)
  }

  private def dtoDec(name: ScValue.ScType, fields: List[TextTree[ScValue]]): TextTree[ScValue] = {
    q"""for {
       |  index  <- this.readIndexCount(ctx, wire)
       |  _      <- $scTry(if (ctx.useIndices) require(index == indexElementsCount(ctx), "Unexpected UEBA index count: " + index) else ()).toEither
       |  result <- $scTry {
       |    $name(
       |      ${fields.join(",\n").shift(6).trim}
       |    )
       |  }.toEither
       |} yield result
       |""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[ScValue], TextTree[ScValue], TextTree[ScValue])] = {
    layout.fields(dto).map {
      case UebaLayoutPlan.FieldLayout(field, length) =>
        val escapedName = escapeScKeyword(field.name.name)
        val fieldRef    = q"value.$escapedName"
        val enc         = mkEncoder(field.tpe, fieldRef, q"writer")
        val fakeEnc     = mkEncoder(field.tpe, fieldRef, q"fakeWriter")
        val decoder     = mkDecoder(field.tpe)
        val decodeTree  = q"$escapedName = $decoder"

        val w = length match {
          case BinReprLen.Fixed(bytes) =>
            q"""{
               |  // ${field.toString}
               |  val before = writeMemoryStream.size()
               |  ${fakeEnc.shift(2).trim}
               |  val after = writeMemoryStream.size()
               |  val length = after - before
               |  ${lengthChecks(BinReprLen.Fixed(bytes)).shift(2).trim}
               |}""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = lengthChecks(v)

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

  private def lengthChecks(length: BinReprLen): TextTree[ScValue] =
    UebaLengthCheckRenderer.render[ScValue](
      length,
      equalTo = bytes => q"length == ${bytes.toString}",
      oneOf   = bytes => q"$scSet(${bytes.mkString(", ")}).contains(length)",
      enforce = condition => q"""require($condition, "Invalid UEBA field length: " + length)""",
    )

  private def mkDecoder(tpe: TypeRef): TextTree[ScValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar => ScScalarCodecEmitter.uebaDecode(s, q"wire")
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Scala) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ =>
                    val targetTpe = codecName(domainTypes.toScTypeRefKeepForeigns(u))
                    q"""$targetTpe.instance.decode(ctx, wire).toTry.get"""
                }
              case _ =>
                val targetTpe = codecName(domainTypes.toScTypeRefKeepForeigns(u))
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

  // The runtime helper owns envelope framing and metadata-window skipping.
  private def mkAnyDecoder(a: TypeRef.Any): TextTree[ScValue] = {
    val expectedKind = AnyFieldPlan.forField(a, domain).kind
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"""$baboonAnyBinCodec.decode(wire, $expectedHex.toByte)"""
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[ScValue], wref: TextTree[ScValue]): TextTree[ScValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar => ScScalarCodecEmitter.uebaEncode(s, wref, ref)
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Scala) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, wref)
                  case _ =>
                    val targetTpe = codecName(domainTypes.toScTypeRefKeepForeigns(u))
                    q"""$targetTpe.instance.encode(ctx, $wref, $ref)"""
                }
              case _ =>
                val targetTpe = codecName(domainTypes.toScTypeRefKeepForeigns(u))
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

  // The runtime helper owns envelope framing and cross-format conversion.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[ScValue], wref: TextTree[ScValue]): TextTree[ScValue] = {
    val expectedKind                      = AnyFieldPlan.forField(a, domain).kind
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"""$baboonAnyBinCodec.encode(ctx, $wref, $expectedHex.toByte, $staticDom, $staticVer, $staticTid, $ref)"""
  }

  // Static fallbacks for the cross-format facade helpers (`jsonToUebaBytes` / `uebaToJson`).
  // The wire `meta` may omit components that are pinned by the field's static declaration; the
  // codec emits whatever is statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade.buildSyntheticTypeMeta` for the merge semantics.
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[ScValue], TextTree[ScValue], TextTree[ScValue]) = {
    val plan                                             = AnyFieldPlan.forField(a, domain)
    def render(value: Option[String]): TextTree[ScValue] = value.fold[TextTree[ScValue]](q"_root_.scala.None")(s => q"""_root_.scala.Some("$s")""")
    (render(plan.staticDomain), render(plan.staticVersion), render(plan.staticTypeId))
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
