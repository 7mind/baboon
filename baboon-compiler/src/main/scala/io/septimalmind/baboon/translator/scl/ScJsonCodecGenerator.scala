package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.DerivationDecl
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
  scTypeInfo: ScTypeInfo,
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
          q"""def encode(ctx: $baboonCodecContext, value: $name): io.circe.Json = {
             |  ${enc.shift(2).trim}
             |}
             |""".stripMargin.trim
        )
      } else Nil
    val decodeMethod =
      List(
        q"""def decode(ctx: $baboonCodecContext, wire: io.circe.Json): Either[String, $name] = {
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

  private def genForeignBodies(
    name: ScValue.ScType
  ): (TextTree[Nothing], TextTree[Nothing]) = {
    (
      q"""throw new IllegalArgumentException(s"${name.name} is a foreign type")""",
      q"""throw new IllegalArgumentException(s"${name.name} is a foreign type")""",
    )
  }

  private def wrapAdtBranchEncoder(
    branchName: String,
    tree: TextTree[ScValue],
  ): TextTree[ScValue] = {
    q"""io.circe.Json.obj("$branchName" -> $tree)"""
  }

  private def genAdtBodies(name: ScValue.ScType, a: Typedef.Adt): (TextTree[ScValue], TextTree[Nothing]) = {
    val branches = a.dataMembers(domain).map {
      m =>
        val branchNs            = q"${scTypeInfo.adtNsName(a.id)}"
        val branchName          = m.name.name
        val fqBranch            = q"$branchNs.$branchName"
        val branchNameRef       = q"${branchName.toLowerCase}"
        val routedBranchEncoder = q"${fqBranch}_JsonCodec.instance.encode(ctx, $branchNameRef)"

        val branchEncoder = if (target.language.wrappedAdtBranchCodecs) {
          routedBranchEncoder
        } else {
          wrapAdtBranchEncoder(branchName, routedBranchEncoder)
        }

        val branchValue = if (target.language.wrappedAdtBranchCodecs) q"wire" else q"head._2"

        (
          q"""case $branchNameRef: $fqBranch => 
             |  $branchEncoder
             |""".stripMargin,
          q"""case "$branchName" =>
             |  ${fqBranch}_JsonCodec.instance.decode(ctx, $branchValue)
             |""".stripMargin,
        )

    }

    (
      q"""value match {
         |  ${branches.map(_._1).joinN().shift(2).trim}
         |  
         |  case _ => throw new IllegalArgumentException(s"Cannot encode $$value: unexpected subclass")
         |}
         |
         |
         |""".stripMargin,
      q"""val asObject = wire.asObject
         |if (asObject.isEmpty) {
         |  throw new IllegalArgumentException(s"Cannot decode $$wire to ${name.name}: object expected")
         |}
         |val head = asObject.get.toList.head
         |
         |head._1 match {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |
         |  case _ =>  throw new IllegalArgumentException(s"Cannot decode $$wire to ${name.name}: no matching value")
         |}
         |""".stripMargin,
    )
  }

  private def genEnumBodies(
    name: ScValue.ScType
  ): (TextTree[ScValue.ScType], TextTree[ScValue.ScType]) = {
    (
      q"""io.circe.Json.fromString(value.toString)""",
      q"""wire.asString match {
         |  case Some(str) =>
         |    $name.parse(str.trim) match {
         |      case Some(result) => Right(result)
         |      case None => Left(s"Cannot decode $$wire to ${name.name}: no matching value")
         |    }
         |  case None => Left(s"Cannot decode $$wire to ${name.name}: string expected")
         |}
         |""".stripMargin,
    )
  }

  private def genDtoBodies(name: ScValue.ScType, d: Typedef.Dto): (TextTree[ScValue], TextTree[ScValue]) = {
    val fields = d.fields.map {
      f =>
        val fieldRef = q"value.${f.name.name}"
        val enc      = mkEncoder(f.tpe, fieldRef)
        val dec      = mkDecoder(f.tpe, q"""asObject("${f.name.name}")""")
        (
          q""""${f.name.name}" -> $enc""",
          q"${f.name.name} = $dec",
        )
    }

    val mainEnc = q"""io.circe.Json.obj(
                     |  ${fields.map(_._1).join(",\n").shift(2).trim}
                     |)""".stripMargin

    val encBody = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs => wrapAdtBranchEncoder(d.id.name.name, mainEnc)
      case _                                                      => mainEnc
    }

    val fullDec = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs => q"wire.asObject.get.toList.head._2.asObject.get"
      case _                                                      => q"wire.asObject.get"
    }

    val decBody =
      q"""val asObject = $fullDec
         |
         |Right($name(
         |  ${fields.map(_._2).join(",\n").shift(2).trim}
         |))
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
          case TypeId.Builtins.uid =>
            q"""io.circe.Json.fromString($ref.toString())"""
          case TypeId.Builtins.tsu =>
            q"""io.circe.Json.fromString($baboonTimeFormats.formatTsu($ref))"""
          case TypeId.Builtins.tso =>
            q"""io.circe.Json.fromString($baboonTimeFormats.formatTso($ref))"""
          case TypeId.Builtins.bit => q"""io.circe.Json.fromBoolean($ref)"""
          case TypeId.Builtins.i08 => q"""io.circe.Json.fromInt($ref.toInt)"""
          case TypeId.Builtins.i16 => q"""io.circe.Json.fromInt($ref.toInt)"""
          case TypeId.Builtins.i32 => q"""io.circe.Json.fromInt($ref)"""

          case TypeId.Builtins.i64 => q"""io.circe.Json.fromLong($ref)"""
          case TypeId.Builtins.u08 => q"""io.circe.Json.fromInt(java.lang.Byte.toUnsignedInt($ref))"""
          case TypeId.Builtins.u16 => q"""io.circe.Json.fromInt(java.lang.Short.toUnsignedInt($ref))"""
          case TypeId.Builtins.u32 => q"""io.circe.Json.fromLong(java.lang.Integer.toUnsignedLong($ref))"""
          case TypeId.Builtins.u64 => q"""io.circe.Json.fromBigInt($baboonBinTools.toUnsignedBigInt($ref))"""

          case TypeId.Builtins.f32  => q"""io.circe.Json.fromFloat($ref).get"""
          case TypeId.Builtins.f64  => q"""io.circe.Json.fromDouble($ref).get"""
          case TypeId.Builtins.f128 => q"""io.circe.Json.fromBigDecimal($ref)"""

          case TypeId.Builtins.str =>
            q"""io.circe.Json.fromString($ref)"""
          case TypeId.Builtins.bytes =>
            q"""io.circe.Json.fromString($ref.toHexString)"""
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
            q"""$targetTpe.instance.encode(ctx, $ref)"""
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""$ref.map(v => ${mkEncoder(c.args.head, q"v")}).getOrElse(io.circe.Json.Null)"""

          case TypeId.Builtins.map =>
            val keyEnc   = encodeKey(c.args.head, q"e._1")
            val valueEnc = mkEncoder(c.args.last, q"e._2")
            q"""io.circe.Json.obj( $ref.map(e => ($keyEnc, $valueEnc)).toList: _* )"""
          case TypeId.Builtins.lst =>
            q"""io.circe.Json.fromValues($ref.map(e => ${mkEncoder(c.args.head, q"e")}))"""
          case TypeId.Builtins.set =>
            q"""io.circe.Json.fromValues($ref.map(e => ${mkEncoder(c.args.head, q"e")}))"""
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def mkDecoder(tpe: TypeRef, ref: TextTree[ScValue]): TextTree[ScValue] = {
    def mkReader(bs: TypeId.BuiltinScalar): TextTree[ScValue] = {
      val fref = q"$ref"
      bs match {
        case TypeId.Builtins.bit => q"""$fref.flatMap(_.asBoolean).get"""
        case TypeId.Builtins.i08 => q"""$fref.flatMap(_.asNumber).flatMap(_.toLong).map(_.toByte).get"""
        case TypeId.Builtins.i16 => q"""$fref.flatMap(_.asNumber).flatMap(_.toLong).map(_.toShort).get"""
        case TypeId.Builtins.i32 => q"""$fref.flatMap(_.asNumber).flatMap(_.toLong).map(_.toInt).get"""
        case TypeId.Builtins.i64 => q"""$fref.flatMap(_.asNumber).flatMap(_.toLong).get"""
        case TypeId.Builtins.u08 => q"""$fref.flatMap(_.asNumber).flatMap(_.toLong).map(_.toByte).get"""
        case TypeId.Builtins.u16 => q"""$fref.flatMap(_.asNumber).flatMap(_.toLong).map(_.toShort).get"""
        case TypeId.Builtins.u32 => q"""$fref.flatMap(_.asNumber).flatMap(_.toLong).map(_.toInt).get"""
        case TypeId.Builtins.u64 => q"""$fref.flatMap(_.asNumber).flatMap(_.toBigInt).map(_.longValue).get"""

        case TypeId.Builtins.f32   => q"""$fref.flatMap(_.asNumber).map(_.toFloat).get"""
        case TypeId.Builtins.f64   => q"""$fref.flatMap(_.asNumber).map(_.toDouble).get"""
        case TypeId.Builtins.f128  => q"""$fref.flatMap(_.asNumber).flatMap(_.toBigDecimal).get"""
        case TypeId.Builtins.str   => q"""$fref.flatMap(_.asString).get"""
        case TypeId.Builtins.bytes => q"""$fref.flatMap(_.asString).map($scByteString.parseHex).get"""
        case TypeId.Builtins.uid   => q"""$fref.flatMap(_.asString).map(java.util.UUID.fromString).get"""
        case TypeId.Builtins.tsu   => q"""$fref.flatMap(_.asString).flatMap($baboonTimeFormats.parseTsu).get"""
        case TypeId.Builtins.tso   => q"""$fref.flatMap(_.asString).flatMap($baboonTimeFormats.parseTso).get"""
        case other                 => throw new RuntimeException(s"BUG: Unexpected type: $other")
      }
    }

    def decodeKey(tpe: TypeRef, ref: TextTree[ScValue]): TextTree[ScValue] = {
      tpe.id match {
        case TypeId.Builtins.bit   => q"""$ref.toBoolean"""
        case TypeId.Builtins.i08   => q"""$ref.toByte"""
        case TypeId.Builtins.i16   => q"""$ref.toShort"""
        case TypeId.Builtins.i32   => q"""$ref.toInt"""
        case TypeId.Builtins.i64   => q"""$ref.toLong"""
        case TypeId.Builtins.u08   => q"""$ref.toByte"""
        case TypeId.Builtins.u16   => q"""$ref.toShort"""
        case TypeId.Builtins.u32   => q"""$ref.toInt"""
        case TypeId.Builtins.u64   => q"""$ref.toLong"""
        case TypeId.Builtins.f32   => q"""$ref.toFloat"""
        case TypeId.Builtins.f64   => q"""$ref.toDouble"""
        case TypeId.Builtins.f128  => q"""$ref.toBigDecimal"""
        case TypeId.Builtins.str   => ref
        case TypeId.Builtins.bytes => q"""ref.toHexString"""
        case TypeId.Builtins.uid   => q"""java.util.UUID.fromString($ref.toString)"""
        case TypeId.Builtins.tsu   => q"""$baboonTimeFormats.parseTsu($ref).get"""
        case TypeId.Builtins.tso   => q"""$baboonTimeFormats.parseTso($ref).get"""

        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum | _: Typedef.Foreign =>
                  val targetTpe =
                    trans.toScTypeRefKeepForeigns(uid, domain, evo)
                  q"""${targetTpe}_JsonCodec.instance.decode(ctx, io.circe.Json.fromString($ref)).right.get"""
                case o =>
                  throw new RuntimeException(
                    s"BUG: Unexpected key usertype: $o"
                  )
              }
            case o =>
              throw new RuntimeException(s"BUG: Type/usertype mismatch: $o")
          }
        case o =>
          throw new RuntimeException(s"BUG: Unexpected key type: $o")
      }
    }

    tpe match {
      case TypeRef.Scalar(bs: TypeId.BuiltinScalar) =>
        mkReader(bs)

      case TypeRef.Scalar(u: TypeId.User) =>
        val targetTpe = trans.toScTypeRefKeepForeigns(u, domain, evo)
        q"""$ref.flatMap(v => ${targetTpe}_JsonCodec.instance.decode(ctx, v).toOption).get"""

      case TypeRef.Constructor(id, args) =>
        id match {
          case TypeId.Builtins.opt =>
            // Handle JSON null specially: if e is null, return None; otherwise decode the value
            // Use flatMap with Option() to ensure type compatibility: Option[T] from both branches
            val innerDecoder = mkDecoder(args.head, q"Option(e)")
            q"""$ref.flatMap(e => if (e.isNull) None else Option($innerDecoder))"""

          case TypeId.Builtins.map =>
            val keyDec   = decodeKey(args.head, q"kv._1")
            val valueDec = mkDecoder(args.last, q"Option(kv._2)")

            q"""$ref.flatMap(_.asObject).map(_.toList.map(kv => ($keyDec, $valueDec)).toMap).get"""
          case TypeId.Builtins.lst =>
            q"""$ref.flatMap(_.asArray).map(_.toList).map(e => e.map(e1 => ${mkDecoder(args.head, q"Option(e1)")})).get"""

          case TypeId.Builtins.set =>
            q"""$ref.flatMap(_.asArray).map(_.toSet).map(e => e.map(e1 => ${mkDecoder(args.head, q"Option(e1)")})).get"""

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
    ScValue.ScType(name.pkg, s"${name.name}_JsonCodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User, name: ScValue.ScType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"def codecJson: $baboonJsonCodec[$name] = ${codecName(name)}.instance"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(DerivationDecl("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Json"
}
