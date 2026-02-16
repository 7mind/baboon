package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.scl.ScCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.scl.ScDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class ScJsonCodecGenerator(
  trans: ScTypeTranslator,
  target: ScTarget,
  domain: Domain,
  evo: BaboonEvolution,
  scDomainTreeTools: ScDomainTreeTools,
) extends ScCodecTranslator {

  override def translate(defn: DomainMember.User, csRef: ScValue.ScType, srcRef: ScValue.ScType): Option[TextTree[ScValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto      => Some(genDtoBodies(csRef, d))
        case _: Typedef.Enum     => Some(genEnumBodies(csRef))
        case a: Typedef.Adt      => Some(genAdtBodies(csRef, a))
        case _: Typedef.Foreign  => Some(genForeignBodies(csRef))
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          val insulatedEnc =
            q"""if (this ne LazyInstance.value) {
               |  return LazyInstance.value.encode(ctx, value)
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

          genCodec(
            defn,
            csRef,
            srcRef,
            insulatedEnc,
            insulatedDec,
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
  ): TextTree[ScValue] = {
    val isEncoderEnabled = target.language.enableDeprecatedEncoders || domain.version == evo.latest
    val iName            = q"$baboonJsonCodec[$name]"
    val encodeMethod =
      if (isEncoderEnabled) {
        List(
          q"""def encode(ctx: $baboonCodecContext, value: $name): $circeJson = {
             |  ${enc.shift(2).trim}
             |}
             |""".stripMargin.trim
        )
      } else Nil
    val decodeMethod =
      List(
        q"""def decode(ctx: $baboonCodecContext, wire: $circeJson): $scEither[$javaThrowable, $name] = {
           |  ${dec.shift(2).trim}
           |}""".stripMargin.trim
      )

    val baseMethods = encodeMethod ++ decodeMethod
    val cName       = codecName(srcRef)
    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecBase[$name, $iName]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecBase[$name, $iName]"
        case _ if defn.isAdt                                => q"$baboonJsonCodecBaseGeneratedAdt[$name, $iName]"
        case _                                              => q"$baboonJsonCodecBaseGenerated[$name, $iName]"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecNoEncoder[$name, $iName]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecNoEncoder[$name, $iName]"
        case _ if defn.isAdt                                => q"$baboonJsonCodecNoEncoderGeneratedAdt[$name, $iName]"
        case _                                              => q"$baboonJsonCodecNoEncoderGenerated[$name, $iName]"
      }
    }

    val meta = renderMeta(defn, scDomainTreeTools.makeCodecMeta(defn))
    q"""object ${cName.asName} extends $cParent {
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${meta.joinN().shift(2).trim}
       |
       |  override protected def LazyInstance: $baboonLazy[$iName] = $baboonLazy($cName)
       |  override def instance: $iName = LazyInstance.value
       |}
       |""".stripMargin
  }

  private def genForeignBodies(name: ScValue.ScType): (TextTree[ScValue], TextTree[ScValue]) = {
    (
      q"""throw new $javaIllegalArgumentException(s"${name.name} is a foreign type")""",
      q"""throw new $javaIllegalArgumentException(s"${name.name} is a foreign type")""",
    )
  }

  private def wrapAdtBranchEncoder(
    branchName: String,
    tree: TextTree[ScValue],
  ): TextTree[ScValue] = {
    q"""$circeJson.obj("$branchName" -> $tree)"""
  }

  private def genAdtBodies(name: ScValue.ScType, adt: Typedef.Adt): (TextTree[ScValue], TextTree[ScValue]) = {
    val branches = adt.dataMembers(domain).map {
      m =>
        val branchNs            = q"${adt.id.name.name}"
        val branchName          = m.name.name
        val fqBranch            = q"$branchNs.$branchName"
        val branchNameRef       = q"${branchName.toLowerCase}"
        val routedBranchEncoder = q"${fqBranch}_JsonCodec.instance.encode(ctx, $branchNameRef)"

        val branchEncoder = if (target.language.wrappedAdtBranchCodecs) {
          routedBranchEncoder
        } else {
          wrapAdtBranchEncoder(branchName, routedBranchEncoder)
        }

        (
          q"""case $branchNameRef: $fqBranch => $branchEncoder
             |""".stripMargin,
          q"""case "$branchName" =>
             |  ${fqBranch}_JsonCodec.instance.decode(ctx, json)
             |""".stripMargin,
        )

    }

    (
      q"""value match {
         |  ${branches.map(_._1).joinN().shift(2).trim}
         |
         |  case _ => throw new $javaIllegalArgumentException(s"Cannot encode $$value: unexpected subclass")
         |}
         |""".stripMargin,
      q"""wire.asObject match {
         |  case Some(jsonObject) =>
         |    jsonObject.toList.headOption match {
         |      case Some((key, json)) =>
         |        key match {
         |          ${branches.map(_._2).joinN().shift(10).trim}
         |
         |          case _ => Left(new $javaIllegalArgumentException(s"Cannot decode $$wire to ${name.name}: no matching value"))
         |        }
         |      case _ => Left(new $javaIllegalArgumentException(s"Cannot decode $$wire to ${name.name}: empty json object"))
         |    }
         |  case _ => Left(new $javaIllegalArgumentException(s"Cannot decode $$wire to ${name.name}: object expected"))
         |}
         |""".stripMargin,
    )
  }

  private def genEnumBodies(name: ScValue.ScType): (TextTree[ScValue.ScType], TextTree[ScValue.ScType]) = {
    (
      q"""$circeJson.fromString(value.toString)""",
      q"""wire.asString match {
         |  case Some(str) =>
         |    $name.parse(str.trim) match {
         |      case Some(result) => Right(result)
         |      case None => Left(new $javaIllegalArgumentException(s"Cannot decode $$wire to ${name.name}: no matching value"))
         |    }
         |  case None => Left(new $javaIllegalArgumentException(s"Cannot decode $$wire to ${name.name}: string expected"))
         |}
         |""".stripMargin,
    )
  }

  private def genDtoBodies(name: ScValue.ScType, d: Typedef.Dto): (TextTree[ScValue], TextTree[ScValue]) = {
    val fields = d.fields.map {
      f =>
        val fieldRef = q"value.${f.name.name}"
        val enc      = mkEncoder(f.tpe, fieldRef)
        val dec      = decoder(f.name.name, f.tpe, q"jsonObject")
        (
          q""""${f.name.name}" -> $enc""",
          q"${f.name.name} <- $dec",
        )
    }

    val mainEnc = q"""$circeJson.obj(
                     |  ${fields.map(_._1).join(",\n").shift(2).trim}
                     |)""".stripMargin

    val encBody = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs => wrapAdtBranchEncoder(d.id.name.name, mainEnc)
      case _                                                      => mainEnc
    }

    val decoderForExpr =
      if (d.fields.nonEmpty) {
        q"""for {
           |  ${fields.map(_._2).joinN().shift(2).trim}
           |} yield $name(${d.fields.map(_.name.name).mkString(",")})
           |""".stripMargin
      } else q"Right($name())"

    val decBody =
      q"""wire.asObject match {
         |  case Some(jsonObject) => ${decoderForExpr.shift(4).trim}
         |  case _ => Left(new $javaIllegalArgumentException("Cannot decode $$wire to ${name.name}: object expected"))
         |}
         |""".stripMargin

    (encBody, decBody)
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[ScValue]): TextTree[ScValue] = {
    def encodeKey(tpe: TypeRef, ref: TextTree[ScValue]): TextTree[ScValue] = {
      tpe.id match {
        case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.format($ref)"
        case _: TypeId.Builtin                         => q"$ref.toString"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum | _: Typedef.Foreign =>
                  val targetTpe = trans.toScTypeRefKeepForeigns(uid, domain, evo)
                  q"""${targetTpe}_JsonCodec.instance.encode(ctx, $ref)"""
                case o =>
                  throw new RuntimeException(s"BUG: Unexpected key usertype: $o")
              }
            case o =>
              throw new RuntimeException(s"BUG: Type/usertype mismatch: $o")
          }
        case o =>
          throw new RuntimeException(s"BUG: Unexpected key type: $o")
      }
    }

    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.uid => q"$circeJson.fromString($ref.toString())"
          case TypeId.Builtins.tsu => q"$circeJson.fromString($baboonTimeFormats.formatTsu($ref))"
          case TypeId.Builtins.tso => q"$circeJson.fromString($baboonTimeFormats.formatTso($ref))"
          case TypeId.Builtins.bit => q"$circeJson.fromBoolean($ref)"
          case TypeId.Builtins.i08 => q"$circeJson.fromInt($ref.toInt)"
          case TypeId.Builtins.i16 => q"$circeJson.fromInt($ref.toInt)"
          case TypeId.Builtins.i32 => q"$circeJson.fromInt($ref)"

          case TypeId.Builtins.i64 => q"$circeJson.fromLong($ref)"
          case TypeId.Builtins.u08 => q"$circeJson.fromInt(java.lang.Byte.toUnsignedInt($ref))"
          case TypeId.Builtins.u16 => q"$circeJson.fromInt(java.lang.Short.toUnsignedInt($ref))"
          case TypeId.Builtins.u32 => q"$circeJson.fromLong(java.lang.Integer.toUnsignedLong($ref))"
          case TypeId.Builtins.u64 => q"$circeJson.fromBigInt($baboonBinTools.toUnsignedBigInt($ref))"

          case TypeId.Builtins.f32  => q"$circeJson.fromFloat($ref).get"
          case TypeId.Builtins.f64  => q"$circeJson.fromDouble($ref).get"
          case TypeId.Builtins.f128 => q"$circeJson.fromBigDecimal($ref)"

          case TypeId.Builtins.str   => q"$circeJson.fromString($ref)"
          case TypeId.Builtins.bytes => q"$circeJson.fromString($ref.toHexString)"
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
            q"$targetTpe.instance.encode(ctx, $ref)"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"$ref.map(v => ${mkEncoder(c.args.head, q"v")}).getOrElse($circeJson.Null)"

          case TypeId.Builtins.map =>
            val keyEnc   = encodeKey(c.args.head, q"e._1")
            val valueEnc = mkEncoder(c.args.last, q"e._2")
            q"$circeJson.obj($ref.map(e => ($keyEnc, $valueEnc)).toList: _*)"
          case TypeId.Builtins.lst =>
            q"$circeJson.fromValues($ref.map(e => ${mkEncoder(c.args.head, q"e")}))"
          case TypeId.Builtins.set =>
            q"$circeJson.fromValues($ref.map(e => ${mkEncoder(c.args.head, q"e")}))"
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def decoder(fieldName: String, tpe: TypeRef, jsonObjectRef: TextTree[ScValue]): TextTree[ScValue] = {
    def getKeyDecoder(typeRef: TypeRef): TextTree[ScValue] = {
      typeRef match {
        case TypeRef.Scalar(id) =>
          id match {
            case s: TypeId.BuiltinScalar =>
              s match {
                case TypeId.Builtins.bit => q"$baboonDecodeKeyBoolean"
                case TypeId.Builtins.i08 => q"$circeDecodeKeyByte"
                case TypeId.Builtins.i16 => q"$circeDecodeKeyShort"
                case TypeId.Builtins.i32 => q"$circeDecodeKeyInt"
                case TypeId.Builtins.i64 => q"$circeDecodeKeyLong"
                case TypeId.Builtins.u08 => q"$circeDecodeKeyByte"
                case TypeId.Builtins.u16 => q"$circeDecodeKeyShort"
                case TypeId.Builtins.u32 => q"$circeDecodeKeyInt"
                case TypeId.Builtins.u64 => q"$baboonDecodeLong"

                case TypeId.Builtins.f32   => q"$baboonDecodeKeyFloat"
                case TypeId.Builtins.f64   => q"$circeDecodeKeyDouble"
                case TypeId.Builtins.f128  => q"$baboonDecodeKeyBigDecimal"
                case TypeId.Builtins.str   => q"$circeDecodeKeyString"
                case TypeId.Builtins.bytes => q"$baboonDecodeKeyByteString"
                case TypeId.Builtins.uid   => q"$circeDecodeKeyUUID"
                case TypeId.Builtins.tsu   => q"$baboonDecodeKeyTsu"
                case TypeId.Builtins.tso   => q"$baboonDecodeKeyTso"
                case other                 => throw new RuntimeException(s"BUG: Unexpected type: $other")
              }
            case uid: TypeId.User =>
              domain.defs.meta.nodes(uid) match {
                case u: DomainMember.User =>
                  u.defn match {
                    case _: Typedef.Enum | _: Typedef.Foreign =>
                      val targetTpe = trans.toScTypeRefKeepForeigns(uid, domain, evo)
                      q"$circeKeyDecoder.instance(s => ${targetTpe}_JsonCodec.instance.decode(ctx, $circeJson.fromString(s)).toOption)"
                    case o => throw new RuntimeException(s"BUG: Unexpected key usertype: $o")
                  }
                case o =>
                  throw new RuntimeException(s"BUG: Type/usertype mismatch: $o")
              }
          }
        case _ => throw new Exception(s"collection cannot be key: $tpe")
      }
    }

    def getDecoder(tpe: TypeRef): TextTree[ScValue] = {
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case s: TypeId.BuiltinScalar =>
              s match {
                case TypeId.Builtins.bit => q"$circeDecodeBoolean"
                case TypeId.Builtins.i08 => q"$baboonDecodeByte"
                case TypeId.Builtins.i16 => q"$baboonDecodeShort"
                case TypeId.Builtins.i32 => q"$baboonDecodeInt"
                case TypeId.Builtins.i64 => q"$baboonDecodeLong"
                case TypeId.Builtins.u08 => q"$baboonDecodeByte"
                case TypeId.Builtins.u16 => q"$baboonDecodeShort"
                case TypeId.Builtins.u32 => q"$baboonDecodeInt"
                case TypeId.Builtins.u64 => q"$baboonDecodeLong"

                case TypeId.Builtins.f32   => q"$circeDecodeFloat"
                case TypeId.Builtins.f64   => q"$circeDecodeDouble"
                case TypeId.Builtins.f128  => q"$baboonDecodeBigDecimalLenient"
                case TypeId.Builtins.str   => q"$circeDecodeString"
                case TypeId.Builtins.bytes => q"$baboonDecodeByteString"
                case TypeId.Builtins.uid   => q"$circeDecodeUuid"
                case TypeId.Builtins.tsu   => q"$baboonDecodeTsu"
                case TypeId.Builtins.tso   => q"$baboonDecodeTso"
                case other                 => throw new RuntimeException(s"BUG: Unexpected type: $other")
              }
            case u: TypeId.User =>
              val targetTpe = trans.toScTypeRefKeepForeigns(u, domain, evo)
              q"${targetTpe}_JsonCodec.circeDecoder"
          }
        case TypeRef.Constructor(id, args) =>
          id match {
            case TypeId.Builtins.opt => q"$circeDecodeOption(${getDecoder(args.head)})"
            case TypeId.Builtins.map =>
              val keyDec   = getKeyDecoder(args.head)
              val valueDec = getDecoder(args(1))
              q"$circeDecodeMap($keyDec, $valueDec)"
            case TypeId.Builtins.lst => q"$circeDecodeList(${getDecoder(args.head)})"
            case TypeId.Builtins.set => q"$circeDecodeSet(${getDecoder(args.head)})"
            case o                   => throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
      }
    }

    val field   = q"getField($jsonObjectRef, \"$fieldName\")"
    val decoder = getDecoder(tpe)
    q"$field.flatMap(v => $decoder(v.hcursor))"
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[ScValue]] = {
    defn.defn match {
      case _: Typedef.Enum | _: Typedef.Foreign => meta.map(_.valueField)
      case _                                    => meta.map(_.refValueField)
    }
  }

  def codecName(name: ScValue.ScType): ScValue.ScType = {
    ScValue.ScType(name.pkg, s"${name.name}_JsonCodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User, name: ScValue.ScType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"def codecJson: $baboonJsonCodec[$name] = ${codecName(name)}.instance"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Json"
}
