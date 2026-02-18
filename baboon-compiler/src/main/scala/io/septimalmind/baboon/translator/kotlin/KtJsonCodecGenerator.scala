package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.CompilerTarget.KtTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.kotlin.KtCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.kotlin.KtDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.kotlin.KtTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class KtJsonCodecGenerator(
  trans: KtTypeTranslator,
  target: KtTarget,
  domain: Domain,
  evo: BaboonEvolution,
  ktDomainTreeTools: KtDomainTreeTools,
) extends KtCodecTranslator {

  override def translate(defn: DomainMember.User, ktRef: KtValue.KtType, srcRef: KtValue.KtType): Option[TextTree[KtValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto      => Some(genDtoBodies(ktRef, d))
        case _: Typedef.Enum     => Some(genEnumBodies(ktRef))
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
          genCodec(defn, ktRef, srcRef, enc, dec)
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: KtValue.KtType,
    srcRef: KtValue.KtType,
    enc: TextTree[KtValue],
    dec: TextTree[KtValue],
  ): TextTree[KtValue] = {
    val isEncoderEnabled = domain.version == evo.latest
    val encodeMethod =
      if (isEncoderEnabled) {
        List(
          q"""override fun encode(ctx: $baboonCodecContext, value: $name): $jsonElement {
             |  ${enc.shift(2).trim}
             |}
             |""".stripMargin.trim
        )
      } else Nil
    val decodeMethod =
      List(
        q"""override fun decode(ctx: $baboonCodecContext, wire: $jsonElement): $name {
           |  ${dec.shift(2).trim}
           |}""".stripMargin.trim
      )

    val baseMethods = encodeMethod ++ decodeMethod
    val cName       = codecName(srcRef)
    val meta        = renderMeta(defn, ktDomainTreeTools.makeCodecMeta(defn))

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecBase<$name>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecBase<$name>"
        case _ if defn.isAdt                                => q"$baboonJsonCodecBaseGeneratedAdt<$name>"
        case _                                              => q"$baboonJsonCodecBaseGenerated<$name>"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecNoEncoder<$name>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecNoEncoder<$name>"
        case _ if defn.isAdt                                => q"$baboonJsonCodecNoEncoderGeneratedAdt<$name>"
        case _                                              => q"$baboonJsonCodecNoEncoderGenerated<$name>"
      }
    }

    q"""object ${cName.asName} : $cParent {
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${meta.joinN().shift(2).trim}
       |}
       |""".stripMargin
  }

  private def genForeignBodies(name: KtValue.KtType): (TextTree[KtValue], TextTree[KtValue]) = {
    (
      q"""throw $javaIllegalArgumentException("${name.name} is a foreign type")""",
      q"""throw $javaIllegalArgumentException("${name.name} is a foreign type")""",
    )
  }

  private def wrapAdtBranchEncoder(
    branchName: String,
    tree: TextTree[KtValue],
  ): TextTree[KtValue] = {
    q"""$buildJsonObject { put("$branchName", $tree) }"""
  }

  private def genAdtBodies(name: KtValue.KtType, adt: Typedef.Adt): (TextTree[KtValue], TextTree[KtValue]) = {
    val branches = adt.dataMembers(domain).map {
      m =>
        val branchName = m.name.name
        val fqBranch   = q"${adt.id.name.name}.$branchName"
        val branchRef  = q"branchVal"

        val routedBranchEncoder = q"${fqBranch}_JsonCodec.encode(ctx, $branchRef)"
        val branchEncoder = if (target.language.wrappedAdtBranchCodecs) {
          routedBranchEncoder
        } else {
          wrapAdtBranchEncoder(branchName, routedBranchEncoder)
        }

        (
          q"""is $fqBranch -> {
             |  val $branchRef = value as $fqBranch
             |  $branchEncoder
             |}""".stripMargin,
          q""""$branchName" -> ${fqBranch}_JsonCodec.decode(ctx, entry.value)""",
        )
    }

    (
      q"""return when (value) {
         |  ${branches.map(_._1).joinN().shift(2).trim}
         |}""".stripMargin,
      q"""val jsonObj = wire.jsonObject
         |val entry = jsonObj.entries.firstOrNull() ?: throw IllegalArgumentException("Cannot decode to ${name.name}: empty json object")
         |return when (entry.key) {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |  else -> throw IllegalArgumentException("Cannot decode to ${name.name}: unknown key " + entry.key)
         |}""".stripMargin,
    )
  }

  private def genEnumBodies(name: KtValue.KtType): (TextTree[KtValue], TextTree[KtValue]) = {
    (
      q"""return $jsonPrimitive(value.name)""",
      q"""val str = wire.jsonPrimitive.content
         |return $name.parse(str.trim()) ?: throw IllegalArgumentException("Cannot decode to ${name.name}: no matching value for $$str")""".stripMargin,
    )
  }

  private def genDtoBodies(name: KtValue.KtType, d: Typedef.Dto): (TextTree[KtValue], TextTree[KtValue]) = {
    val encFields = d.fields.map {
      f =>
        val fieldRef = q"value.${f.name.name}"
        val enc      = mkEncoder(f.tpe, fieldRef)
        q"""put("${f.name.name}", $enc)"""
    }

    val decFields = d.fields.map {
      f =>
        val dec = mkDecoder(f.name.name, f.tpe, q"jsonObj")
        q"${f.name.name} = $dec"
    }

    val mainEnc = q"""$buildJsonObject {
                     |  ${encFields.join("\n").shift(2).trim}
                     |}""".stripMargin

    val encBody = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs => q"return ${wrapAdtBranchEncoder(d.id.name.name, mainEnc)}"
      case _                                                      => q"return $mainEnc"
    }

    val decBody = if (d.fields.nonEmpty) {
      q"""val jsonObj = wire.jsonObject
         |return $name(
         |  ${decFields.join(",\n").shift(2).trim}
         |)""".stripMargin
    } else {
      q"""return $name()"""
    }

    (encBody, decBody)
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[KtValue]): TextTree[KtValue] = {
    def encodeKey(tpe: TypeRef, ref: TextTree[KtValue]): TextTree[KtValue] = {
      tpe.id match {
        case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.format($ref)"
        case _: TypeId.Builtin                         => q"$ref.toString()"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum =>
                  q"$ref.name"
                case f: Typedef.Foreign =>
                  f.bindings.get(BaboonLang.Kotlin) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      encodeKey(aliasedRef, ref)
                    case _ =>
                      q"$ref.toString()"
                  }
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
          case TypeId.Builtins.uid   => q"$jsonPrimitive($ref.toString())"
          case TypeId.Builtins.tsu   => q"$jsonPrimitive($baboonTimeFormats.formatTsu($ref))"
          case TypeId.Builtins.tso   => q"$jsonPrimitive($baboonTimeFormats.formatTso($ref))"
          case TypeId.Builtins.bit   => q"$jsonPrimitive($ref)"
          case TypeId.Builtins.i08   => q"$jsonPrimitive($ref.toInt())"
          case TypeId.Builtins.i16   => q"$jsonPrimitive($ref.toInt())"
          case TypeId.Builtins.i32   => q"$jsonPrimitive($ref)"
          case TypeId.Builtins.i64   => q"$jsonPrimitive($ref)"
          case TypeId.Builtins.u08   => q"$jsonPrimitive($ref.toInt())"
          case TypeId.Builtins.u16   => q"$jsonPrimitive($ref.toInt())"
          case TypeId.Builtins.u32   => q"$jsonPrimitive($ref.toLong())"
          case TypeId.Builtins.u64   => q"$jsonPrimitive($ref.toLong())"
          case TypeId.Builtins.f32   => q"$jsonPrimitive($ref)"
          case TypeId.Builtins.f64   => q"$jsonPrimitive($ref)"
          case TypeId.Builtins.f128  => q"$jsonPrimitive($ref.toPlainString())"
          case TypeId.Builtins.str   => q"$jsonPrimitive($ref)"
          case TypeId.Builtins.bytes => q"$jsonPrimitive($ref.toHexString())"
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Kotlin) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref)
                  case _ =>
                    val targetTpe = codecName(trans.toKtTypeRefKeepForeigns(u, domain, evo))
                    q"$targetTpe.encode(ctx, $ref)"
                }
              case _ =>
                val targetTpe = codecName(trans.toKtTypeRefKeepForeigns(u, domain, evo))
                q"$targetTpe.encode(ctx, $ref)"
            }
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""$ref?.let { ${mkEncoder(c.args.head, q"it")} } ?: $jsonNull"""
          case TypeId.Builtins.map =>
            val keyEnc   = encodeKey(c.args.head, q"e.key")
            val valueEnc = mkEncoder(c.args.last, q"e.value")
            q"""$buildJsonObject { $ref.forEach { e -> put($keyEnc, $valueEnc) } }"""
          case TypeId.Builtins.lst =>
            q"""$buildJsonArray { $ref.forEach { e -> add(${mkEncoder(c.args.head, q"e")}) } }"""
          case TypeId.Builtins.set =>
            q"""$buildJsonArray { $ref.forEach { e -> add(${mkEncoder(c.args.head, q"e")}) } }"""
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def mkDecoder(fieldName: String, tpe: TypeRef, jsonObjRef: TextTree[KtValue]): TextTree[KtValue] = {
    def decodeElement(tpe: TypeRef, ref: TextTree[KtValue]): TextTree[KtValue] = {
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case TypeId.Builtins.bit   => q"$ref.jsonPrimitive.boolean"
            case TypeId.Builtins.i08   => q"$ref.jsonPrimitive.int.toByte()"
            case TypeId.Builtins.i16   => q"$ref.jsonPrimitive.int.toShort()"
            case TypeId.Builtins.i32   => q"$ref.jsonPrimitive.int"
            case TypeId.Builtins.i64   => q"$ref.jsonPrimitive.long"
            case TypeId.Builtins.u08   => q"$ref.jsonPrimitive.int.toUByte()"
            case TypeId.Builtins.u16   => q"$ref.jsonPrimitive.int.toUShort()"
            case TypeId.Builtins.u32   => q"$ref.jsonPrimitive.long.toUInt()"
            case TypeId.Builtins.u64   => q"$ref.jsonPrimitive.long.toULong()"
            case TypeId.Builtins.f32   => q"$ref.jsonPrimitive.float"
            case TypeId.Builtins.f64   => q"$ref.jsonPrimitive.double"
            case TypeId.Builtins.f128  => q"java.math.BigDecimal($ref.jsonPrimitive.content)"
            case TypeId.Builtins.str   => q"$ref.jsonPrimitive.content"
            case TypeId.Builtins.bytes => q"$ktByteString.fromHexString($ref.jsonPrimitive.content)"
            case TypeId.Builtins.uid   => q"java.util.UUID.fromString($ref.jsonPrimitive.content)"
            case TypeId.Builtins.tsu   => q"$baboonTimeFormats.parseTsu($ref.jsonPrimitive.content)"
            case TypeId.Builtins.tso   => q"$baboonTimeFormats.parseTso($ref.jsonPrimitive.content)"
            case u: TypeId.User =>
              domain.defs.meta.nodes(u) match {
                case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                  f.bindings.get(BaboonLang.Kotlin) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      decodeElement(aliasedRef, ref)
                    case _ =>
                      val targetTpe = codecName(trans.toKtTypeRefKeepForeigns(u, domain, evo))
                      q"$targetTpe.decode(ctx, $ref)"
                  }
                case _ =>
                  val targetTpe = codecName(trans.toKtTypeRefKeepForeigns(u, domain, evo))
                  q"$targetTpe.decode(ctx, $ref)"
              }
            case o =>
              throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
        case c: TypeRef.Constructor =>
          c.id match {
            case TypeId.Builtins.opt =>
              q"""if ($ref == $jsonNull || $ref.toString() == "null") null else ${decodeElement(c.args.head, ref)}"""
            case TypeId.Builtins.lst =>
              q"$ref.jsonArray.map { e -> ${decodeElement(c.args.head, q"e")} }"
            case TypeId.Builtins.set =>
              q"$ref.jsonArray.map { e -> ${decodeElement(c.args.head, q"e")} }.toSet()"
            case TypeId.Builtins.map =>
              val keyDec   = decodeKey(c.args.head, q"e.key")
              val valueDec = decodeElement(c.args.last, q"e.value")
              q"$ref.jsonObject.entries.associate { e -> ($keyDec) to $valueDec }"
            case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
      }
    }

    def decodeKey(tpe: TypeRef, ref: TextTree[KtValue]): TextTree[KtValue] = {
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case TypeId.Builtins.bit   => q"$ref.toBoolean()"
            case TypeId.Builtins.i08   => q"$ref.toByte()"
            case TypeId.Builtins.i16   => q"$ref.toShort()"
            case TypeId.Builtins.i32   => q"$ref.toInt()"
            case TypeId.Builtins.i64   => q"$ref.toLong()"
            case TypeId.Builtins.u08   => q"$ref.toUByte()"
            case TypeId.Builtins.u16   => q"$ref.toUShort()"
            case TypeId.Builtins.u32   => q"$ref.toUInt()"
            case TypeId.Builtins.u64   => q"$ref.toULong()"
            case TypeId.Builtins.f32   => q"$ref.toFloat()"
            case TypeId.Builtins.f64   => q"$ref.toDouble()"
            case TypeId.Builtins.f128  => q"java.math.BigDecimal($ref)"
            case TypeId.Builtins.str   => q"$ref"
            case TypeId.Builtins.bytes => q"$ktByteString.fromHexString($ref)"
            case TypeId.Builtins.uid   => q"java.util.UUID.fromString($ref)"
            case TypeId.Builtins.tsu   => q"$baboonTimeFormats.parseTsu($ref)"
            case TypeId.Builtins.tso   => q"$baboonTimeFormats.parseTso($ref)"
            case u: TypeId.User =>
              domain.defs.meta.nodes(u) match {
                case ud: DomainMember.User =>
                  ud.defn match {
                    case _: Typedef.Enum =>
                      val targetTpe = trans.toKtTypeRefKeepForeigns(u, domain, evo)
                      q"$targetTpe.parse($ref) ?: throw IllegalArgumentException(\"Cannot parse enum key: \" + $ref)"
                    case f: Typedef.Foreign =>
                      f.bindings.get(BaboonLang.Kotlin) match {
                        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                          decodeKey(aliasedRef, ref)
                        case _ =>
                          val targetTpe = codecName(trans.toKtTypeRefKeepForeigns(u, domain, evo))
                          q"$targetTpe.decode(ctx, $jsonPrimitive($ref))"
                      }
                    case o => throw new RuntimeException(s"BUG: Unexpected key usertype: $o")
                  }
                case o => throw new RuntimeException(s"BUG: Type/usertype mismatch: $o")
              }
            case o => throw new RuntimeException(s"BUG: Unexpected key type: $o")
          }
        case _ => throw new RuntimeException(s"Collection cannot be key: $tpe")
      }
    }

    tpe match {
      case TypeRef.Constructor(id, args) if id.name.name == "opt" =>
        q"""$jsonObjRef["$fieldName"]?.let { v -> if (v == $jsonNull || v.toString() == "null") null else ${decodeElement(args.head, q"v")} }"""
      case _ =>
        q"""${decodeElement(tpe, q"""$jsonObjRef["$fieldName"]!!""")}"""
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
    KtValue.KtType(name.pkg, s"${name.name}_JsonCodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User, name: KtValue.KtType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"val codecJson: $baboonJsonCodec<$name> by lazy { ${codecName(name)} }"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Kotlin) &&
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Json"
}
