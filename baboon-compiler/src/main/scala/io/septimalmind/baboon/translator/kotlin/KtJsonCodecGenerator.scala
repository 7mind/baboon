package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.CompilerTarget.KtTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.kotlin.KtCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.kotlin.KtDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.kotlin.KtTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class KtJsonCodecGenerator(
  trans: KtTypeTranslator,
  target: KtTarget,
  domain: Domain,
  evo: BaboonEvolution,
  ktDomainTreeTools: KtDomainTreeTools,
  ktTypes: KtTypes,
) extends KtCodecTranslator {

  override def translate(defn: DomainMember.User, ktRef: KtValue.KtType, srcRef: KtValue.KtType): Option[TextTree[KtValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto  => Some(genDtoBodies(ktRef, d))
        case _: Typedef.Enum => Some(genEnumBodies(ktRef))
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
          q"""override fun encode(ctx: $baboonCodecContext, instance: $name): $jsonElement {
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

    val anyHelpers: List[TextTree[KtValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil
    val baseMethods                         = encodeMethod ++ decodeMethod ++ anyHelpers
    val cName                               = codecName(srcRef)
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
             |  val $branchRef = instance
             |  $branchEncoder
             |}""".stripMargin,
          q""""$branchName" -> ${fqBranch}_JsonCodec.decode(ctx, entry.value)""",
        )
    }

    (
      q"""return when (instance) {
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
      q"""return $jsonPrimitive(instance.name)""",
      q"""val str = wire.jsonPrimitive.content
         |return $name.parse(str.trim()) ?: throw IllegalArgumentException("Cannot decode to ${name.name}: no matching value for $$str")""".stripMargin,
    )
  }

  private def genDtoBodies(name: KtValue.KtType, d: Typedef.Dto): (TextTree[KtValue], TextTree[KtValue]) = {
    val encFields = d.fields.map {
      f =>
        val fieldRef = q"instance.${f.name.name}"
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
                // M19/PR-60: id types — emit canonical toString (single- or multi-field).
                case d: Typedef.Dto if d.isIdentifier =>
                  q"$ref.toString()"
                // M19/PR-60: single-primitive-field wrappers — peel and recurse.
                case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
                  val inner = d.fields.head
                  encodeKey(inner.tpe, q"$ref.${inner.name.name}")
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
          case TypeId.Builtins.f128  => if (ktTypes.multiplatform) q"$jsonPrimitive($ref.toString())" else q"$jsonPrimitive($ref.toPlainString())"
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
      case a: TypeRef.Any => mkAnyEncoder(a, ref)
    }
  }

  private def mkDecoder(fieldName: String, tpe: TypeRef, jsonObjRef: TextTree[KtValue]): TextTree[KtValue] = {
    def decodeElement(tpe: TypeRef, ref: TextTree[KtValue]): TextTree[KtValue] = {
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case TypeId.Builtins.bit => q"$ref.jsonPrimitive.boolean"
            case TypeId.Builtins.i08 => q"$ref.jsonPrimitive.int.toByte()"
            case TypeId.Builtins.i16 => q"$ref.jsonPrimitive.int.toShort()"
            case TypeId.Builtins.i32 => q"$ref.jsonPrimitive.int"
            case TypeId.Builtins.i64 => q"$ref.jsonPrimitive.long"
            case TypeId.Builtins.u08 => q"$ref.jsonPrimitive.int.toUByte()"
            case TypeId.Builtins.u16 => q"$ref.jsonPrimitive.int.toUShort()"
            case TypeId.Builtins.u32 => q"$ref.jsonPrimitive.long.toUInt()"
            case TypeId.Builtins.u64 => q"$ref.jsonPrimitive.long.toULong()"
            case TypeId.Builtins.f32 => q"$ref.jsonPrimitive.float"
            case TypeId.Builtins.f64 => q"$ref.jsonPrimitive.double"
            case TypeId.Builtins.f128 =>
              if (ktTypes.multiplatform) q"${ktTypes.ktBigDecimal}.fromString($ref.jsonPrimitive.content)" else q"java.math.BigDecimal($ref.jsonPrimitive.content)"
            case TypeId.Builtins.str   => q"$ref.jsonPrimitive.content"
            case TypeId.Builtins.bytes => q"$ktByteString.fromHexString($ref.jsonPrimitive.content)"
            case TypeId.Builtins.uid =>
              if (ktTypes.multiplatform) q"kotlin.uuid.Uuid.parse($ref.jsonPrimitive.content)" else q"java.util.UUID.fromString($ref.jsonPrimitive.content)"
            case TypeId.Builtins.tsu => q"$baboonTimeFormats.parseTsu($ref.jsonPrimitive.content)"
            case TypeId.Builtins.tso => q"$baboonTimeFormats.parseTso($ref.jsonPrimitive.content)"
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
        case a: TypeRef.Any => mkAnyDecoder(a, ref)
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
            case TypeId.Builtins.f128  => if (ktTypes.multiplatform) q"${ktTypes.ktBigDecimal}.fromString($ref)" else q"java.math.BigDecimal($ref)"
            case TypeId.Builtins.str   => q"$ref"
            case TypeId.Builtins.bytes => q"$ktByteString.fromHexString($ref)"
            case TypeId.Builtins.uid   => if (ktTypes.multiplatform) q"kotlin.uuid.Uuid.parse($ref)" else q"java.util.UUID.fromString($ref)"
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
                    // M19/PR-60: id types — call parseRepr and unwrap Right.
                    // PR-F (M24): throw BaboonCodecException.DecoderFailure on Left for
                    // cross-language malformed-key consistency (replaces unchecked cast).
                    case d: Typedef.Dto if d.isIdentifier =>
                      val targetTpe   = trans.toKtTypeRefKeepForeigns(u, domain, evo)
                      val nestedCodec = KtValue.KtType(targetTpe.pkg, s"${targetTpe.name}Codec")
                      q"""when (val __r = $nestedCodec.parseRepr($ref)) { is $baboonEither.Right -> __r.value; is $baboonEither.Left -> throw $baboonCodecException.DecoderFailure("malformed key: " + $ref) }"""
                    // M19/PR-60: single-primitive-field wrappers — peel and recurse, then construct.
                    case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
                      val inner     = d.fields.head
                      val targetTpe = trans.toKtTypeRefKeepForeigns(u, domain, evo)
                      val innerDec  = decodeKey(inner.tpe, ref)
                      q"$targetTpe($innerDec)"
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

  // Deep walk (mirrors Scala/C#/Rust/Kt-UEBA `hasAnyField`): a codec object needs the any-field
  // helpers if any direct or nested-via-Constructor-arg field has type `any`.
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
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[KtValue]): TextTree[KtValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"encodeAnyField(ctx, ${expectedHex}.toByte(), $staticDom, $staticVer, $staticTid, $ref)"
  }

  // Decode delegates to the per-codec-object `decodeAnyField` helper. JSON decode never cross-
  // converts (always returns `AnyOpaqueJson` from JSON wire); user calls `facade.decodeAny(opaque)`
  // for typed resolution. No `ctx` / no static fallbacks needed at the decode site.
  private def mkAnyDecoder(a: TypeRef.Any, ref: TextTree[KtValue]): TextTree[KtValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"decodeAnyField(${expectedHex}.toByte(), $ref)"
  }

  // Static fallbacks for the cross-format facade helper (`uebaToJson`). The wire `meta` may omit
  // components that are pinned by the field's static declaration; the codec emits whatever is
  // statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade.buildSyntheticTypeMeta` for the merge semantics. Per spec table:
  //   A=(null,null,null), B=(currentDomain,null,null), C=(currentDomain,currentVersion,null),
  //   D1=(null,null,underlyingFqid), D2=(currentDomain,null,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // Duplicated across Scala/C#/Rust/Kt-UEBA/Kt-JSON — extraction deferred (see PR 4.2 ledger
  // entry's DRY analysis): textual emission diverges by language flavor.
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

  // Per-codec-object helpers consolidating the any-field JSON envelope encode/decode (kind check,
  // cross-format conversion via facade, envelope `$ak/$ad/$av/$at/$c` build & disassemble).
  // Emitted at most once per codec object that has any any-bearing fields. Mirrors PR 5.2's
  // `KtUEBACodecGenerator.anyFieldHelpers` and PR 3.3's C# JSON helper shape. PR-06-D08 lesson:
  // `AnyMetaCodec.writeJson` returns `JsonElement` typed but always produces a `JsonObject` —
  // we cast to `JsonObject` to add the `$c` envelope key. The const `'$c'` source escape uses
  // Kotlin's `${'$'}c` form so the literal compiles to `$c` (matching the runtime constant
  // contract).
  private def anyFieldHelpers: TextTree[KtValue] = {
    q"""private val anyEnvelopeContentKey: String = "$${'$$'}c"
       |
       |private fun encodeAnyField(
       |    ctx: $baboonCodecContext,
       |    expectedKind: Byte,
       |    staticDomain: String?,
       |    staticVersion: String?,
       |    staticTypeid: String?,
       |    value: $baboonAnyOpaque,
       |): $jsonElement {
       |    if (value.meta.kind != expectedKind) {
       |        throw $baboonCodecException.EncoderFailure(
       |            "any: meta-kind 0x" + (value.meta.kind.toInt() and 0xFF).toString(16).padStart(2, '0') +
       |            " does not match field-declared 0x" + (expectedKind.toInt() and 0xFF).toString(16).padStart(2, '0')
       |        )
       |    }
       |    val anyInner: $jsonElement = when (value) {
       |        is $baboonAnyOpaqueJson -> value.json
       |        is $baboonAnyOpaqueUeba -> {
       |            val anyFacade = ctx.facade ?: throw $baboonCodecException.EncoderFailure(
       |                "Cannot encode AnyOpaqueUeba into JSON without a facade reference. Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), or supply AnyOpaqueJson directly."
       |            )
       |            val anyConvResult = anyFacade.uebaToJson(value.meta, value.bytes, staticDomain, staticVersion, staticTypeid)
       |            when (anyConvResult) {
       |                is $baboonEither.Left -> throw anyConvResult.value
       |                is $baboonEither.Right -> anyConvResult.value
       |            }
       |        }
       |    }
       |    val anyMetaJson = $baboonAnyMetaCodec.writeJson(value.meta) as $jsonObject
       |    return $jsonObject(anyMetaJson.toMutableMap().apply { put(anyEnvelopeContentKey, anyInner) })
       |}
       |
       |private fun decodeAnyField(expectedKind: Byte, wire: $jsonElement): $baboonAnyOpaqueJson {
       |    val anyMetaResult = $baboonAnyMetaCodec.readJson(wire)
       |    val anyMeta = when (anyMetaResult) {
       |        is $baboonEither.Left -> throw anyMetaResult.value
       |        is $baboonEither.Right -> anyMetaResult.value
       |    }
       |    if (anyMeta.kind != expectedKind) {
       |        throw $baboonCodecException.DecoderFailure(
       |            "any: wire kind 0x" + (anyMeta.kind.toInt() and 0xFF).toString(16).padStart(2, '0') +
       |            " does not match field-declared 0x" + (expectedKind.toInt() and 0xFF).toString(16).padStart(2, '0')
       |        )
       |    }
       |    val anyContent = (wire as? $jsonObject)?.get(anyEnvelopeContentKey)
       |        ?: throw $baboonCodecException.DecoderFailure(
       |            "any: JSON envelope missing '" + anyEnvelopeContentKey + "' content key"
       |        )
       |    return $baboonAnyOpaqueJson(anyMeta, anyContent)
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
