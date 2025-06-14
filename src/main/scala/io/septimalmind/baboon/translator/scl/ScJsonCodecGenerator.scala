package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.translator.scl.ScCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class ScJsonCodecGenerator(
  trans: ScTypeTranslator,
  csDomTrees: ScTreeTools,
  target: ScTarget,
  domain: Domain,
  evo: BaboonEvolution,
  csTypeInfo: ScTypeInfo,
) extends ScCodecTranslator {

  override def translate(defn: DomainMember.User, csRef: ScValue.ScType, srcRef: ScValue.ScType): Option[TextTree[ScValue]] = {
    val isLatestVersion = domain.version == evo.latest

    (defn.defn match {
      case d: Typedef.Dto =>
        Some(genDtoBodies(csRef, d))
      case _: Typedef.Enum =>
        Some(genEnumBodies(csRef))
      case a: Typedef.Adt =>
        Some(genAdtBodies(csRef, a))
      case _: Typedef.Foreign =>
        Some(genForeignBodies(csRef))
      case _: Typedef.Contract =>
        None
      case _: Typedef.Service =>
        None
    }).map {
      case (enc, dec) =>
        if (!isLatestVersion && !target.language.enableDeprecatedEncoders) {
          (q"""throw new Exception("Type ${defn.id.toString}@${domain.version.toString} is deprecated, encoder was not generated");""", dec)
        } else {
          (enc, dec)
        }
    }.map {
      case (enc, dec) =>
        // plumbing reference leaks
        val insulatedEnc =
          q"""if (this != LazyInstance.Value)
             |{
             |    return LazyInstance.Value.Encode(ctx, value);
             |
             |}
             |
             |${enc.shift(4).trim}
             |""".stripMargin.trim

        val insulatedDec =
          q"""if (this != LazyInstance.Value)
             |{
             |    return LazyInstance.Value.Decode(ctx, wire);
             |}
             |
             |${dec.shift(4).trim}
             |""".stripMargin.trim

        genCodec(
          defn,
          csRef,
          srcRef,
          insulatedEnc,
          insulatedDec,
          !defn.defn.isInstanceOf[Typedef.Foreign],
        )
    }
  }

  private def genCodec(defn: DomainMember.User, name: ScValue.ScType, srcRef: ScValue.ScType, enc: TextTree[ScValue], dec: TextTree[ScValue], addExtensions: Boolean)
    : TextTree[ScValue] = {
    val iName = q"$iBaboonJsonCodec<$name>"
    val baseMethods = List(
      q"""def encode(ctx: BaboonCodecContext, value: $name): io.circe.Json =
         |  ${enc.shift(4).trim}
         |
         |def decode(ctx: BaboonCodecContext, wire: io.circe.Json): Either[String, $name] =
         |  ${dec.shift(4).trim}""".stripMargin
    )

    val (parents, methods) = defn.defn match {
      case _: Typedef.Enum =>
        (List(q"$iBaboonJsonCodec<$name>"), baseMethods)
      case _ =>
        val extensions = List(
          q"""def encode(ctx: BaboonCodecContext, value: IBaboonGenerated): io.circe.Json =
             |  if (value.isInstanceOf[$name]) {
             |    encode(ctx, value.asInstanceOf[$name])
             |  } else {
             |    throw new Exception(s"Expected to have ${name.name} type")
             |  }
             |
             |def decode(ctx: BaboonCodecContext, wire: io.circe.Json): Either[String, IBaboonGenerated] =
             |  decode(ctx, wire).map(Right(_)).left.map(Left.apply)""".stripMargin,
        )

        val adtParents = defn.id.owner match {
          case Owner.Adt(_) => List(q"$iBaboonAdtMemberMeta")
          case _            => List.empty

        }
        val extParents = List(q"$iBaboonJsonCodec[$iBaboonGenerated]") ++ adtParents

        val mm = if (addExtensions) {
          baseMethods ++ extensions
        } else {
          baseMethods
        }

        val baseParents = List(iName)
        val pp = if (addExtensions) {
          baseParents ++ extParents
        } else {
          baseParents
        }

        (pp, mm)
    }

    val cName = codecName(srcRef)
    q"""class ${cName.asName} extends ${parents.join(" with ")} {
       |  ${methods.join("\n\n").shift(4).trim}
       |
       |  ${csDomTrees.makeMeta(defn, isCodec = true).join("\n").shift(4).trim}
       |
       |  private lazy val LazyInstance: $iName = new $cName()
       |
       |  def instance: $iName = LazyInstance
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
        val branchNs            = q"${csTypeInfo.adtNsName(a.id)}"
        val branchName          = m.name.name
        val fqBranch            = q"$branchNs.$branchName"
        val branchNameRef       = q"${branchName.toLowerCase}"
        val routedBranchEncoder = q"${fqBranch}_JsonCodec.instance.encode(ctx, $branchNameRef)"

        val branchEncoder = if (target.language.wrappedAdtBranchCodecs) {
          routedBranchEncoder
        } else {
          wrapAdtBranchEncoder(branchName, routedBranchEncoder)
        }

        val branchValue = if (target.language.wrappedAdtBranchCodecs) {
          q"wire"
        } else {
          q"head.value"
        }

        (
          q"""if (value.isInstanceOf[$fqBranch]) {
             |  return $branchEncoder
             |}""".stripMargin,
          q"""if (head.label == "$branchName") {
             |  return ${fqBranch}_JsonCodec.instance.decode(ctx, $branchValue).right.get
             |}""".stripMargin,
        )

    }

    (
      q"""${branches.map(_._1).join("\n")}
         |
         |throw new IllegalArgumentException(s"Cannot encode $value: unexpected subclass")
         |""".stripMargin,
      q"""val asObject = wire.asObject
         |if (asObject.isEmpty) {
         |  throw new IllegalArgumentException(s"Cannot decode $wire to ${name.name}: object expected")
         |}
         |val head = asObject.get.head
         |
         |${branches.map(_._2).join("\n")}
         |
         |throw new IllegalArgumentException(s"Cannot decode $wire to ${name.name}: no matching value")
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
         |    $name.withName(str.trim) match {
         |      case Some(result) => Right(result)
         |      case None => Left(s"Cannot decode $wire to ${name.name}: no matching value")
         |    }
         |  case None => Left(s"Cannot decode $wire to ${name.name}: string expected")
         |}
         |""".stripMargin,
    )
  }

  private def genDtoBodies(name: ScValue.ScType, d: Typedef.Dto): (TextTree[ScValue], TextTree[ScValue]) = {
    val fields = d.fields.map {
      f =>
        val fieldRef = q"value.${f.name.name.capitalize}"
        val enc      = mkEncoder(f.tpe, fieldRef)
        val dec      = mkDecoder(f.tpe, q"""asObject("$${f.name.name}")""")
        (
          q"""io.circe.Json.obj("$${f.name.name}" -> $enc)""",
          q"${f.name.name.capitalize}: $dec",
        )
    }

    val mainEnc = q"""io.circe.Json.obj(
                     |${fields.map(_._1).join(",\n").shift(4)}
                     |)""".stripMargin

    val fullEnc = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        wrapAdtBranchEncoder(d.id.name.name, mainEnc)
      case _ => mainEnc
    }

    val encBody = q"""return $fullEnc"""

    val fullDec = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs => q"wire.asObject.get.head.value.asObject.get"
      case _                                                      => q"wire.asObject.get"
    }

    val decBody =
      q"""val asObject = $fullDec
         |
         |if (asObject.isEmpty) {
         |  throw new IllegalArgumentException(s"Cannot decode $wire to ${name.name}: object expected")
         |}
         |
         |Right(new $name(
         |${fields.map(_._2).join(",\n").shift(4)}
         |))
         |""".stripMargin

    (encBody, decBody)
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[ScValue]): TextTree[ScValue] = {
    def encodeKey(tpe: TypeRef, ref: TextTree[ScValue]): TextTree[ScValue] = {
      tpe.id match {
        case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.format($ref)"
        case _: TypeId.Builtin                         => q"io.circe.Json.fromString($ref.toString)"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum | _: Typedef.Foreign =>
                  val targetTpe = trans.asScTypeKeepForeigns(uid, domain, evo)
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
          case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
            q"""io.circe.Json.fromString($baboonTimeFormats.format($ref))"""
          case _: TypeId.BuiltinScalar =>
            q"""io.circe.Json.fromString($ref.toString())"""
          case u: TypeId.User =>
            val targetTpe = codecName(trans.asScTypeKeepForeigns(u, domain, evo))
            q"""$targetTpe.instance.encode(ctx, $ref)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            if (csTypeInfo.isScValueType(c.args.head, domain)) {
              q"""if ($ref.isEmpty) io.circe.Json.Null else ${mkEncoder(c.args.head, trans.deNull(c.args.head, domain, ref))}"""
            } else {
              q"""$ref.map(v => ${mkEncoder(c.args.head, v)}).getOrElse(io.circe.Json.Null)"""
            }

          case TypeId.Builtins.map =>
            val keyEnc   = encodeKey(c.args.head, q"e._1")
            val valueEnc = mkEncoder(c.args.last, q"e._2")
            q"""io.circe.Json.fromMap($ref.map(e => ($keyEnc, $valueEnc)))"""
          case TypeId.Builtins.lst =>
            q"""io.circe.Json.fromValues($ref.map(e => ${mkEncoder(c.args.head, e)}))"""
          case TypeId.Builtins.set =>
            q"""io.circe.Json.fromValues($ref.map(e => ${mkEncoder(c.args.head, e)}))"""
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def mkDecoder(tpe: TypeRef, ref: TextTree[ScValue]): TextTree[ScValue] = {
    def mkReader(bs: TypeId.BuiltinScalar): TextTree[ScValue] = {
      val fref = q"$ref.get"
      bs match {
        case TypeId.Builtins.bit                       => q"""$fref.asBoolean.get"""
        case TypeId.Builtins.i08                       => q"""$fref.asNumber.flatMap(_.toInt).get"""
        case TypeId.Builtins.i16                       => q"""$fref.asNumber.flatMap(_.toShort).get"""
        case TypeId.Builtins.i32                       => q"""$fref.asNumber.flatMap(_.toInt).get"""
        case TypeId.Builtins.i64                       => q"""$fref.asNumber.flatMap(_.toLong).get"""
        case TypeId.Builtins.u08                       => q"""$fref.asNumber.flatMap(_.toByte).get"""
        case TypeId.Builtins.u16                       => q"""$fref.asNumber.flatMap(_.toShort).get"""
        case TypeId.Builtins.u32                       => q"""$fref.asNumber.flatMap(_.toInt).get"""
        case TypeId.Builtins.u64                       => q"""$fref.asNumber.flatMap(_.toLong).get"""
        case TypeId.Builtins.f32                       => q"""$fref.asNumber.flatMap(_.toFloat).get"""
        case TypeId.Builtins.f64                       => q"""$fref.asNumber.flatMap(_.toDouble).get"""
        case TypeId.Builtins.f128                      => q"""$fref.asDecimal.get"""
        case TypeId.Builtins.str                       => q"""$fref.asString.get"""
        case TypeId.Builtins.uid                       => q"""java.util.UUID.fromString($fref.asString.get)"""
        case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"""$baboonTimeFormats.parse($fref.asString.get)"""
        case other                                     => throw new RuntimeException(s"BUG: Unexpected type: $other")
      }
    }

    def decodeKey(tpe: TypeRef, ref: TextTree[ScValue]): TextTree[ScValue] = {
      tpe.id match {
        case TypeId.Builtins.bit                       => q"""io.circe.Json.fromBoolean($ref.toBoolean)"""
        case TypeId.Builtins.i08                       => q"""io.circe.Json.fromInt($ref.toByte)"""
        case TypeId.Builtins.i16                       => q"""io.circe.Json.fromShort($ref.toShort)"""
        case TypeId.Builtins.i32                       => q"""io.circe.Json.fromInt($ref.toInt)"""
        case TypeId.Builtins.i64                       => q"""io.circe.Json.fromLong($ref.toLong)"""
        case TypeId.Builtins.u08                       => q"""io.circe.Json.fromByte($ref.toByte)"""
        case TypeId.Builtins.u16                       => q"""io.circe.Json.fromShort($ref.toShort)"""
        case TypeId.Builtins.u32                       => q"""io.circe.Json.fromInt($ref.toInt)"""
        case TypeId.Builtins.u64                       => q"""io.circe.Json.fromLong($ref.toLong)"""
        case TypeId.Builtins.f32                       => q"""io.circe.Json.fromFloat($ref.toFloat)"""
        case TypeId.Builtins.f64                       => q"""io.circe.Json.fromDouble($ref.toDouble)"""
        case TypeId.Builtins.f128                      => q"""io.circe.Json.fromDecimal($ref.toBigDecimal)"""
        case TypeId.Builtins.str                       => ref
        case TypeId.Builtins.uid                       => q"""io.circe.Json.fromString($ref.toString)"""
        case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"""io.circe.Json.fromString($baboonTimeFormats.format($ref))"""
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum | _: Typedef.Foreign =>
                  val targetTpe =
                    trans.asScTypeKeepForeigns(uid, domain, evo)
                  q"""${targetTpe}_JsonCodec.instance.decode(ctx, $ref).right.get"""
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
        val targetTpe = trans.asScTypeKeepForeigns(u, domain, evo)
        q"""${targetTpe}_JsonCodec.instance.decode(ctx, $ref).right.get"""

      case TypeRef.Constructor(id, args) =>
        id match {
          case TypeId.Builtins.opt if csTypeInfo.isScValueType(args.head, domain) =>
            q"""$BaboonTools.readNullableValueType($ref, v => ${mkDecoder(args.head, v)})"""

          case TypeId.Builtins.opt =>
            q"""$BaboonTools.readNullableReferentialType($ref, v => ${mkDecoder(args.head, v)})"""

          case TypeId.Builtins.map =>
            val keyDec    = decodeKey(args.head, q"kv._1")
            val keyType   = trans.asScRef(args.head, domain, evo)
            val valueDec  = mkDecoder(args.last, q"kv._2")
            val valueType = trans.asScRef(args.last, domain, evo)
            q"""$ref.map(kv => ($keyDec, $valueDec)).toMap"""

          case TypeId.Builtins.lst =>
            q"""$ref.map(e => ${mkDecoder(args.head, e)}).toList"""

          case TypeId.Builtins.set =>
            q"""$ref.map(e => ${mkDecoder(args.head, e)}).toSet"""

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }

  }

  def codecName(name: ScValue.ScType): ScValue.ScType = {
    ScValue.ScType(name.pkg, s"${name.name}_JsonCodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User, name: ScValue.ScType): CodecMeta = {
    val fix = csDomTrees.metaMethodFlags(defn, isCodec = false)
    val member =
      q"""def $fix$iBaboonJsonCodec[$name] codec_JSON(): $iName =
         |  ${codecName(name)}.instance
         |""".stripMargin
    CodecMeta(member)
  }
}
