package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.java.JvCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.java.JvDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.translator.{AnyFieldPlan, UebaLayoutPlan, UebaLengthCheckRenderer}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class JvUEBACodecGenerator(
  trans: JvTypeTranslator,
  target: JvTarget,
  domain: Domain,
  evo: BaboonEvolution,
  jvDomainTreeTools: JvDomainTreeTools,
) extends JvCodecTranslator {
  private val layout = new UebaLayoutPlan(domain)

  override def translate(
    defn: DomainMember.User,
    jvRef: JvValue.JvType,
    srcRef: JvValue.JvType,
  ): Option[TextTree[JvValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto  => Some(genDtoBodies(jvRef, d))
        case e: Typedef.Enum => Some(genEnumBodies(jvRef, e))
        case a: Typedef.Adt  => Some(genAdtBodies(jvRef, a))
        case f: Typedef.Foreign =>
          f.bindings.get(BaboonLang.Java) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
            case _                                                                  => Some(genForeignBodies(jvRef))
          }
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          val insulatedEnc =
            q"""if (this != LazyInstance.get()) {
               |  LazyInstance.get().encode(ctx, output, value);
               |  return;
               |}
               |
               |$enc
               |""".stripMargin.trim

          val insulatedDec =
            q"""if (this != LazyInstance.get()) {
               |  return LazyInstance.get().decode(ctx, input);
               |}
               |
               |$dec
               |""".stripMargin.trim

          val branchDecoder = defn.defn match {
            case d: Typedef.Dto => genBranchDecoder(jvRef, d)
            case _              => None
          }

          genCodec(
            defn,
            jvRef,
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
    name: JvValue.JvType,
    srcRef: JvValue.JvType,
    enc: TextTree[JvValue],
    dec: TextTree[JvValue],
    branchDecoder: Option[TextTree[JvValue]],
  ): TextTree[JvValue] = {
    val isEncoderEnabled = target.language.enableDeprecatedEncoders || domain.version == evo.latest
    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = layout.indexedFields(d)
        val comment = varlens.map(f => q"// ${f.toString}").joinN()
        q"""$comment
           |return (short) ${varlens.size.toString};""".stripMargin

      case _: Typedef.Enum    => q"""return (short) 0;"""
      case _: Typedef.Adt     => q"""return (short) 0;"""
      case _: Typedef.Foreign => q"""throw new $javaIllegalArgumentException("${name.name} is a foreign type");"""

      case d: Typedef.Contract =>
        throw new IllegalArgumentException(s"BUG: contract codec should not be rendered: $d")
      case d: Typedef.Service =>
        throw new IllegalArgumentException(s"BUG: service codec should not be rendered: $d")
    }

    val indexMethods = List(
      q"""@Override
         |public short indexElementsCount($baboonCodecContext ctx) {
         |  ${indexBody.shift(2).trim}
         |}""".stripMargin
    )

    val encoderMethods = if (isEncoderEnabled) {
      List(
        q"""@Override
           |public void encode($baboonCodecContext ctx, $binaryOutput output, $name value) throws Exception {
           |  ${enc.shift(2).trim}
           |}
           |""".stripMargin
      )
    } else Nil

    val decoderMethods = List(
      q"""@Override
         |public $name decode($baboonCodecContext ctx, $binaryInput input) throws Exception {
         |  ${dec.shift(2).trim}
         |}""".stripMargin
    )

    val baseMethods = encoderMethods ++ decoderMethods
      ++ branchDecoder.map {
        body =>
          q"""public $name decodeBranch($baboonCodecContext ctx, $binaryInput input) throws Exception {
             |  ${body.shift(2).trim}
             |}""".stripMargin
      }.toList ++ indexMethods

    val codecIface = q"$baboonBinCodec<$name>"
    val cName      = codecName(srcRef, defn.defn.id.owner)
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
    val meta = renderMeta(defn, jvDomainTreeTools.makeCodecMeta(defn))

    q"""public class ${cName.asName} extends $cParent implements $baboonBinCodecIndexed {
       |  private ${cName.asName}() {}
       |
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${meta.joinN().shift(2).trim}
       |
       |  public static final ${cName.asName} INSTANCE = new ${cName.asName}();
       |  public static final $baboonLazy<$codecIface> LazyInstance = new $baboonLazy<>(() -> INSTANCE);
       |}
     """.stripMargin
  }

  private def genForeignBodies(name: JvValue.JvType): (TextTree[JvValue], TextTree[JvValue]) = {
    (
      q"""throw new $javaIllegalArgumentException("${name.name} is a foreign type");""",
      q"""throw new $javaIllegalArgumentException("${name.name} is a foreign type");""",
    )
  }

  private def genAdtBodies(name: JvValue.JvType, adt: Typedef.Adt): (TextTree[JvValue], TextTree[JvValue]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList.map {
      case (m, idx) =>
        val branchName = m.name.name

        val adtRef = trans.toJvTypeRefKeepForeigns(m, domain, evo)
        val cName  = codecName(adtRef, m.owner)

        val castedName = JvTypeTranslator.escapeJvKeyword(branchName.toLowerCase)

        val encBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.INSTANCE.encode(ctx, output, $castedName);"""
        } else {
          q"""output.writeByte(${idx.toString});
             |$cName.INSTANCE.encode(ctx, output, $castedName);
           """.stripMargin
        }

        val decBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""yield $cName.INSTANCE.decodeBranch(ctx, input);"""
        } else {
          q"""yield $cName.INSTANCE.decode(ctx, input);"""
        }

        (
          q"""if (value instanceof $adtRef $castedName) {
             |  ${encBody.shift(2).trim}
             |}""".stripMargin,
          q"""case ${idx.toString} -> {
             |  ${decBody.shift(2).trim}
             |}""".stripMargin,
        )
    }

    val encElse = q"""throw new $genericException("Cannot encode to ${name.name}: unexpected type " + value.getClass().getName());"""

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
      q"""int asByte = input.readByte() & 0xFF;
         |
         |return switch (asByte) {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |
         |  default -> throw new $genericException("Cannot decode to ${name.name}: no matching value for ordinal " + asByte);
         |};
         |""".stripMargin,
    )
  }

  private def genEnumBodies(name: JvValue.JvType, e: Typedef.Enum): (TextTree[JvValue], TextTree[JvValue]) = {
    val branches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val obj = EnumWireStyle.wireName(m.name)
        (
          q"case $obj -> output.writeByte(${idx.toString});",
          q"case ${idx.toString} -> $name.$obj;",
        )
    }

    (
      q"""switch (value) {
         |  ${branches.map(_._1).joinN().shift(2).trim}
         |}
         """.stripMargin,
      q"""int asByte = input.readByte() & 0xFF;
         |
         |return switch (asByte) {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |  default -> throw new $genericException("Cannot decode to ${name.name}: no matching value for ordinal " + asByte);
         |};
         |""".stripMargin,
    )
  }

  private def genBranchDecoder(
    name: JvValue.JvType,
    d: Typedef.Dto,
  ): Option[TextTree[JvValue]] = {
    d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(name, fields.map(_._2)))
      case _ =>
        None
    }
  }

  private def genDtoBodies(name: JvValue.JvType, dto: Typedef.Dto): (TextTree[JvValue], TextTree[JvValue]) = {
    val fields = fieldsOf(dto)

    val noIndex = Seq(
      q"output.writeByte(header);",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).joinN()

    val fenc =
      q"""byte header = 0;
         |
         |if (ctx.useIndices()) {
         |  header = (byte) (header | 1);
         |  output.writeByte(header);
         |  var writeMemoryStream = new $byteArrayOutputStream();
         |  try {
         |    var fakeWriter = new $binaryOutput(writeMemoryStream);
         |    try {
         |      ${fields.map(_._3).joinN().shift(6).trim}
         |    } finally {
         |      fakeWriter.close();
         |    }
         |    writeMemoryStream.flush();
         |    output.write(writeMemoryStream.toByteArray());
         |  } finally {
         |    writeMemoryStream.close();
         |  }
         |} else {
         |  ${noIndex.shift(2).trim}
         |}
         |""".stripMargin

    val fdec = dtoDec(name, fields.map(_._2))

    val enc = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, dto.id)

        q"""output.writeByte(${idx.toString});
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, dto.id)
        q"""int marker = input.readByte() & 0xFF;
           |if (marker != ${idx.toString}) throw new $javaIllegalArgumentException("Unexpected UEBA ADT branch marker: " + marker);
           |return decodeBranch(ctx, input);""".stripMargin
      case _ => fdec
    }
    (enc, dec)
  }

  private def dtoDec(name: JvValue.JvType, fields: List[TextTree[JvValue]]): TextTree[JvValue] = {
    q"""var indexCount = $baboonBinCodec.consumeIndex(ctx, input, (int) indexElementsCount(ctx));
       |if (ctx.useIndices() && indexCount != (int) indexElementsCount(ctx)) throw new $javaIllegalArgumentException("Unexpected UEBA index count: " + indexCount);
       |return new $name(
       |  ${fields.join(",\n").shift(2).trim}
       |);
       |""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[JvValue], TextTree[JvValue], TextTree[JvValue])] = {
    layout.fields(dto).map {
      case UebaLayoutPlan.FieldLayout(field, length) =>
        val javaName   = JvTypeTranslator.escapeJvKeyword(field.name.name)
        val fieldRef   = q"value.$javaName()"
        val enc        = mkEncoder(field.tpe, fieldRef, q"output")
        val fakeEnc    = mkEncoder(field.tpe, fieldRef, q"fakeWriter")
        val decoder    = mkDecoder(field.tpe)
        val decodeTree = q"/* ${field.name.name} */ $decoder"

        val w = length match {
          case BinReprLen.Fixed(bytes) =>
            q"""{
               |  // ${field.toString}
               |  int before = writeMemoryStream.size();
               |  ${fakeEnc.shift(2).trim}
               |  int after = writeMemoryStream.size();
               |  int length = after - before;
               |  ${lengthChecks(BinReprLen.Fixed(bytes)).shift(2).trim}
               |}""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = lengthChecks(v)

            q"""{
               |  // ${field.toString}
               |  int before = writeMemoryStream.size();
               |  output.writeInt(before);
               |  ${fakeEnc.shift(2).trim}
               |  int after = writeMemoryStream.size();
               |  int length = after - before;
               |  output.writeInt(length);
               |  ${sanityChecks.shift(2).trim}
               |}""".stripMargin
        }

        (enc, decodeTree, w)
    }
  }

  private def lengthChecks(length: BinReprLen): TextTree[JvValue] =
    UebaLengthCheckRenderer.render[JvValue](
      length,
      equalTo = bytes => q"length == ${bytes.toString}",
      oneOf   = bytes => q"java.util.Set.of(${bytes.mkString(", ")}).contains(length)",
      enforce = condition => q"""if (!($condition)) throw new $javaIllegalArgumentException("Invalid UEBA field length: " + length);""",
    )

  private def mkDecoder(tpe: TypeRef): TextTree[JvValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            JvScalarCodecEmitter.uebaDecode(s, q"input")
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Java) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ =>
                    val targetTpe = codecName(trans.toJvTypeRefKeepForeigns(u, domain, evo), u.owner)
                    q"""$targetTpe.INSTANCE.decode(ctx, input)"""
                }
              case _ =>
                val targetTpe = codecName(trans.toJvTypeRefKeepForeigns(u, domain, evo), u.owner)
                q"""$targetTpe.INSTANCE.decode(ctx, input)"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            val innerDecoder = mkDecoder(c.args.head)
            q"""(input.readByte() == 0 ? $jvOptional.empty() : $jvOptional.of($innerDecoder))"""
          case TypeId.Builtins.map =>
            val keyDecoder   = mkDecoder(c.args.head)
            val valueDecoder = mkDecoder(c.args.last)
            q"""$baboonBinTools.readMap(input.readInt(), () -> $keyDecoder, () -> $valueDecoder)"""
          case TypeId.Builtins.lst =>
            q"""$baboonBinTools.readList(input.readInt(), () -> ${mkDecoder(c.args.head)})"""
          case TypeId.Builtins.set =>
            q"""$baboonBinTools.readSet(input.readInt(), () -> ${mkDecoder(c.args.head)})"""
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a)
    }
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[JvValue], wref: TextTree[JvValue]): TextTree[JvValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            JvScalarCodecEmitter.uebaEncode(s, wref, ref)
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Java) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, wref)
                  case _ =>
                    val targetTpe = codecName(trans.toJvTypeRefKeepForeigns(u, domain, evo), u.owner)
                    q"""$targetTpe.INSTANCE.encode(ctx, $wref, $ref);"""
                }
              case _ =>
                val targetTpe = codecName(trans.toJvTypeRefKeepForeigns(u, domain, evo), u.owner)
                q"""$targetTpe.INSTANCE.encode(ctx, $wref, $ref);"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""if ($ref.isEmpty()) {
               |  $wref.writeByte(0);
               |} else {
               |  $wref.writeByte(1);
               |  ${mkEncoder(c.args.head, q"$ref.get()", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.map =>
            q"""$wref.writeInt($ref.size());
               |for (var entry : $ref.entrySet()) {
               |  ${mkEncoder(c.args.head, q"entry.getKey()", wref).shift(2).trim}
               |  ${mkEncoder(c.args.last, q"entry.getValue()", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.lst =>
            q"""$wref.writeInt($ref.size());
               |for (var item : $ref) {
               |  ${mkEncoder(c.args.head, q"item", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.set =>
            q"""$wref.writeInt($ref.size());
               |for (var item : $ref) {
               |  ${mkEncoder(c.args.head, q"item", wref).shift(2).trim}
               |}""".stripMargin

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref, wref)
    }
  }

  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[JvValue], wref: TextTree[JvValue]): TextTree[JvValue] = {
    val plan                                               = AnyFieldPlan.forField(a, domain)
    val expectedHex                                        = "0x%02x".format(plan.kind & 0xFF)
    def fallback(value: Option[String]): TextTree[JvValue] = value.fold(q"(String) null")(v => q"""(String) "$v"""")
    q"$baboonAnyBinCodec.encode(ctx, $wref, (byte) $expectedHex, ${fallback(plan.staticDomain)}, ${fallback(plan.staticVersion)}, ${fallback(plan.staticTypeId)}, $ref);"
  }

  private def mkAnyDecoder(a: TypeRef.Any): TextTree[JvValue] = {
    val expectedHex = "0x%02x".format(AnyFieldPlan.forField(a, domain).kind & 0xFF)
    q"$baboonAnyBinCodec.decode(input, (byte) $expectedHex)"
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[JvValue]] = {
    defn.defn match {
      case _: Typedef.Enum => meta.map(_.valueField)
      case f: Typedef.Foreign =>
        f.bindings.get(BaboonLang.Java) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => meta.map(_.refValueField)
          case _                                                                  => meta.map(_.valueField)
        }
      case _ => meta.map(_.refValueField)
    }
  }

  def codecName(name: JvValue.JvType, owner: Owner): JvValue.JvType = {
    val domainPkg   = trans.toJvPkg(domain.id, domain.version, evo)
    val ownerPrefix = name.pkg.parts.toSeq.drop(domainPkg.parts.toSeq.length)
    val prefixStr   = if (ownerPrefix.nonEmpty) ownerPrefix.mkString("_") + "_" else ""
    val realPkg     = trans.effectiveJvPkg(owner, domain, evo)
    JvValue.JvType(realPkg, s"$prefixStr${name.name}_UEBACodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User, name: JvValue.JvType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"public static final $baboonBinCodec<$name> codecUeba = ${codecName(name, defn.defn.id.owner)}.INSTANCE;"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Java) &&
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
