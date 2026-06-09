package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.scl.ScCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.scl.ScDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
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
        case d: Typedef.Dto  => Some(genDtoBodies(csRef, d))
        case _: Typedef.Enum => Some(genEnumBodies(csRef))
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
  // Per-codec-object helpers consolidating the any-field envelope encode (JSON) and decode (JSON);
  // emitted at most once per codec object and called from every field-level any site. See
  // spec §172 (factor any-field encode/decode into helpers) and the symmetric UEBA helpers
  // in `ScUEBACodecGenerator.anyFieldHelpers`.
  private def anyFieldHelpers: TextTree[ScValue] = {
    q"""private def encodeAnyField(
       |  ctx: $baboonCodecContext,
       |  expectedKind: $scByte,
       |  staticDomain: $scOption[$scString],
       |  staticVersion: $scOption[$scString],
       |  staticTypeid: $scOption[$scString],
       |  value: $baboonAnyOpaque,
       |): $circeJson = {
       |  if (value.meta.kind != expectedKind) {
       |    throw $baboonCodecException.EncoderFailure(s"any: meta-kind mismatch on encode: expected 0x$${(expectedKind & 0xFF).toHexString}, got 0x$${(value.meta.kind & 0xFF).toHexString}")
       |  }
       |  val innerJson: $circeJson = value match {
       |    case anyJson: $baboonAnyOpaqueJson =>
       |      anyJson.json
       |    case anyUeba: $baboonAnyOpaqueUeba =>
       |      val f = ctx.facade.getOrElse(
       |        throw $baboonCodecException.EncoderFailure(
       |          "Cannot encode AnyOpaqueUeba into JSON without a facade reference. Pass BaboonCodecContext.WithFacade(useIndices, facade) into encode(), or supply AnyOpaqueJson directly."
       |        )
       |      )
       |      f.uebaToJson(anyUeba.meta, anyUeba.bytes, staticDomain, staticVersion, staticTypeid) match {
       |        case Right(j) => j
       |        case Left(e)  => throw e
       |      }
       |  }
       |  $baboonAnyMetaCodec.writeJson(value.meta).mapObject(_.add("$$c", innerJson))
       |}
       |
       |private def decodeAnyField(expectedKind: $scByte, wire: $circeJson): $scEither[$javaThrowable, $baboonAnyOpaqueJson] = {
       |  $baboonAnyMetaCodec.readJson(wire) match {
       |    case Left(e) => Left(e)
       |    case Right(meta) =>
       |      if (meta.kind != expectedKind) {
       |        Left($baboonCodecException.DecoderFailure(s"any: meta-kind mismatch: expected 0x$${(expectedKind & 0xFF).toHexString}, got 0x$${(meta.kind & 0xFF).toHexString}"))
       |      } else {
       |        wire.hcursor.downField("$$c").as[$circeJson] match {
       |          case Right(content) => Right($baboonAnyOpaqueJson(meta, content))
       |          case Left(err)      => Left($baboonCodecException.DecoderFailure(s"any: missing or unreadable content key: $${err.getMessage}"))
       |        }
       |      }
       |  }
       |}""".stripMargin
  }

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
        case _ if defn.ownedByAdt                           => q"$baboonJsonCodecBaseGeneratedAdt[$name, $iName]"
        case _                                              => q"$baboonJsonCodecBaseGenerated[$name, $iName]"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecNoEncoder[$name, $iName]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecNoEncoder[$name, $iName]"
        case _ if defn.ownedByAdt                           => q"$baboonJsonCodecNoEncoderGeneratedAdt[$name, $iName]"
        case _                                              => q"$baboonJsonCodecNoEncoderGenerated[$name, $iName]"
      }
    }

    val meta                                = renderMeta(defn, scDomainTreeTools.makeCodecMeta(defn))
    val anyHelpers: List[TextTree[ScValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil
    val tail                                = (anyHelpers ++ meta).joinNN()
    q"""object ${cName.name} extends $cParent {
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${tail.shift(2).trim}
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
        val branchNameRef       = q"${escapeScKeyword(branchName.toLowerCase)}"
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
        val escapedName = escapeScKeyword(f.name.name)
        val fieldRef    = q"value.$escapedName"
        val enc         = mkEncoder(f.tpe, fieldRef)
        val dec         = decoder(f.name.name, f.tpe, q"jsonObject")
        (
          q""""${f.name.name}" -> $enc""",
          q"$escapedName <- $dec",
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
           |} yield $name(${d.fields.map(f => escapeScKeyword(f.name.name)).mkString(",")})
           |""".stripMargin
      } else q"Right($name())"

    val jsonObjectPattern = if (d.fields.nonEmpty) "jsonObject" else "_"
    val decBody =
      q"""wire.asObject match {
         |  case Some($jsonObjectPattern) => ${decoderForExpr.shift(4).trim}
         |  case _ => Left(new $javaIllegalArgumentException("Cannot decode $$wire to ${name.name}: object expected"))
         |}
         |""".stripMargin

    (encBody, decBody)
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[ScValue]): TextTree[ScValue] = {
    def encodeKey(tpe: TypeRef, ref: TextTree[ScValue]): TextTree[ScValue] = {
      tpe.id match {
        // PR-28.4 (M28): the runtime exposes `formatTsu`/`formatTso` only
        // (no generic `format`). Per-type dispatch matches the value-side
        // emission at line 357-358 and PR-28.3 ±HH:MM canonicalisation contract.
        case TypeId.Builtins.tsu => q"$baboonTimeFormats.formatTsu($ref)"
        case TypeId.Builtins.tso => q"$baboonTimeFormats.formatTso($ref)"
        // PR-28.1 (M28): u64 map keys must carry the canonical unsigned wire form.
        // `Long.toString(-1L)` yields "-1" (two's-complement signed); the canonical
        // u64 max representation is "18446744073709551615". Symmetric with the
        // value-side `Json.fromBigInt(BaboonBinTools.toUnsignedBigInt(_))`.
        case TypeId.Builtins.u64 => q"_root_.java.lang.Long.toUnsignedString($ref)"
        case _: TypeId.Builtin   => q"$ref.toString"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum =>
                  // Enum JSON encoder emits Json.fromString(value.toString); map keys must
                  // be String, so emit toString directly (symmetric with the decoder's
                  // `circeDecodeKeyString` path for enums). PR-66-D02 fix.
                  q"$ref.toString"
                case f: Typedef.Foreign =>
                  f.bindings.get(BaboonLang.Scala) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      encodeKey(aliasedRef, ref)
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
                      // PR-I.1a (M24 Phase 3.1): Custom-foreign map keys route through
                      // the emitted `<Foreign>_KeyCodec` extension hook. The default
                      // impl is identity for stringy foreigns; non-stringy foreigns
                      // require host registration before first use.
                      val keyCodecHost = ScValue.ScType(
                        trans.toScTypeRefKeepForeigns(uid, domain, evo).pkg,
                        s"${trans.toScTypeRefKeepForeigns(uid, domain, evo).name}_KeyCodec",
                      )
                      q"$keyCodecHost.instance.encodeKey($ref)"
                    case None =>
                      throw new RuntimeException(s"BUG: Foreign type $uid has no Scala binding")
                  }
                // M19/PR-60: id types — emit canonical toString (single- or multi-field).
                case d: Typedef.Dto if d.isIdentifier =>
                  q"$ref.toString"
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
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Scala) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref)
                  case _ =>
                    val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
                    q"$targetTpe.instance.encode(ctx, $ref)"
                }
              case _ =>
                val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
                q"$targetTpe.instance.encode(ctx, $ref)"
            }
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
            q"$circeJson.obj($ref.toList.sortBy(_._1.toString).map(e => ($keyEnc, $valueEnc)): _*)"
          case TypeId.Builtins.lst =>
            q"$circeJson.fromValues($ref.map(e => ${mkEncoder(c.args.head, q"e")}))"
          case TypeId.Builtins.set =>
            q"$circeJson.fromValues($ref.map(e => ${mkEncoder(c.args.head, q"e")}))"
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any =>
        val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
        val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
        val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
        q"""encodeAnyField(ctx, $expectedHex.toByte, $staticDom, $staticVer, $staticTid, $ref)"""
    }
  }

  // Static fallbacks for the cross-format facade helpers (`jsonToUebaBytes` / `uebaToJson`).
  // The wire `meta` may omit components that are pinned by the field's static declaration; the
  // codec emits whatever is statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade.buildSyntheticTypeMeta` for the merge semantics.
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[ScValue], TextTree[ScValue], TextTree[ScValue]) = {
    val none                     = q"_root_.scala.None"
    def some(s: String)          = q"""_root_.scala.Some("$s")"""
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
                // PR-28.1 (M28): u64 map keys require KeyDecoder[Long]; the value-side
                // `decodeLong: Decoder[Long]` was previously emitted here by mistake,
                // causing generated code to fail type-check at the `circeKeyDecoder.instance(...)`
                // site. Closes M26-N02(b) follow-up.
                case TypeId.Builtins.u64 => q"$baboonDecodeKeyU64"

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
                    case _: Typedef.Enum =>
                      // Route through `codecName` so the emitted node is a real imported
                      // `<X>_JsonCodec` ScType (harvested by the ScType-only import collector,
                      // ScBaboonTranslator.renderTree) instead of a bare string-concatenated
                      // suffix that never resolves from a nested package (D5).
                      val targetTpe = codecName(trans.toScTypeRefKeepForeigns(uid, domain, evo))
                      // PR-F (M24): malformed map-key consistency — replace silent `.toOption`
                      // with explicit Right/Left match that throws BaboonCodecException.DecoderFailure
                      // on Left so cross-language behaviour is uniform.
                      q"""$circeKeyDecoder.instance(s => $targetTpe.instance.decode(ctx, $circeJson.fromString(s)) match { case Right(v) => Some(v); case Left(_) => throw $baboonCodecException.DecoderFailure(s"malformed key: $$s") })"""
                    case f: Typedef.Foreign =>
                      f.bindings.get(BaboonLang.Scala) match {
                        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                          getKeyDecoder(aliasedRef)
                        case _ =>
                          // PR-I.1a (M24 Phase 3.1): Custom-foreign map keys route through
                          // the emitted `<Foreign>_KeyCodec` extension hook. Behaviour is
                          // uniform across stringy and non-stringy foreigns: the host's
                          // decodeKey is invoked, and any thrown exception is normalized
                          // to BaboonCodecException.DecoderFailure (PR-F invariant).
                          val targetTpe    = trans.toScTypeRefKeepForeigns(uid, domain, evo)
                          val keyCodecHost = ScValue.ScType(targetTpe.pkg, s"${targetTpe.name}_KeyCodec")
                          q"""$circeKeyDecoder.instance(s => try { Some($keyCodecHost.instance.decodeKey(s)) } catch { case e: Exception => throw $baboonCodecException.DecoderFailure(s"malformed key: $$s", e) })"""
                      }
                    // M19/PR-60: id types — call the parser-based round trip.
                    // PR-F (M24): throw BaboonCodecException.DecoderFailure on parse failure.
                    case d: Typedef.Dto if d.isIdentifier =>
                      val targetTpe   = trans.toScTypeRefKeepForeigns(uid, domain, evo)
                      val nestedCodec = ScValue.ScType(targetTpe.pkg, s"${targetTpe.name}Codec", targetTpe.inObject)
                      q"""$circeKeyDecoder.instance(s => $nestedCodec.parseRepr(s) match { case Right(v) => Some(v); case Left(_) => throw $baboonCodecException.DecoderFailure(s"malformed key: $$s") })"""
                    // M19/PR-60: single-primitive-field wrappers — peel and recurse, then construct.
                    case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
                      val inner       = d.fields.head
                      val targetTpe   = trans.toScTypeRefKeepForeigns(uid, domain, evo)
                      val innerKeyDec = getKeyDecoder(inner.tpe)
                      q"$circeKeyDecoder.instance(s => $innerKeyDec.apply(s).map(v => $targetTpe(${inner.name.name} = v)))"
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
              domain.defs.meta.nodes(u) match {
                case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                  f.bindings.get(BaboonLang.Scala) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      getDecoder(aliasedRef)
                    case _ =>
                      // Route through `codecName` so the `<X>_JsonCodec` node is a real
                      // imported ScType (mirroring the encode path) — emitting it as a
                      // bare string suffix on the foreign type leaves it unimported and
                      // additionally drags the companion-less foreign type into the import
                      // set (D4).
                      val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
                      q"$targetTpe.circeDecoder"
                  }
                case _ =>
                  val targetTpe = codecName(trans.toScTypeRefKeepForeigns(u, domain, evo))
                  q"$targetTpe.circeDecoder"
              }
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
        case a: TypeRef.Any =>
          val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
          val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
          // Lift the helper's Either[Throwable, AnyOpaqueJson] into circe's Decoder.Result
          // (Either[DecodingFailure, AnyOpaqueJson]) so this slot is shape-compatible with the
          // surrounding `field.flatMap(v => $decoder(v.hcursor))` template. Decoder is symmetric
          // — no facade plumbing needed (decode never cross-converts).
          q"""$circeDecoder.instance(c => decodeAnyField($expectedHex.toByte, c.value).left.map(t => $circeDecodingFailure(t.getMessage, c.history)))"""
      }
    }

    val field   = q"getField($jsonObjectRef, \"$fieldName\")"
    val decoder = getDecoder(tpe)
    q"$field.flatMap(v => $decoder(v.hcursor))"
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
    ScValue.ScType(name.pkg, s"${name.name}_JsonCodec", name.inObject)
  }

  override def codecMeta(defn: DomainMember.User, name: ScValue.ScType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"def codecJson: $baboonJsonCodec[$name] = ${codecName(name)}.instance"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Scala) &&
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Json"
}
