package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.java.JvCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.java.JvDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class JvJsonCodecGenerator(
  trans: JvTypeTranslator,
  target: JvTarget,
  domain: Domain,
  evo: BaboonEvolution,
  jvDomainTreeTools: JvDomainTreeTools,
) extends JvCodecTranslator {

  override def translate(defn: DomainMember.User, jvRef: JvValue.JvType, srcRef: JvValue.JvType): Option[TextTree[JvValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto  => Some(genDtoBodies(jvRef, d))
        case _: Typedef.Enum => Some(genEnumBodies(jvRef))
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
          genCodec(defn, jvRef, srcRef, enc, dec)
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: JvValue.JvType,
    srcRef: JvValue.JvType,
    enc: TextTree[JvValue],
    dec: TextTree[JvValue],
  ): TextTree[JvValue] = {
    val isEncoderEnabled = domain.version == evo.latest
    val encodeMethod =
      if (isEncoderEnabled) {
        List(
          q"""@Override
             |public $jsonNode encode($baboonCodecContext ctx, $name value) {
             |  ${enc.shift(2).trim}
             |}
             |""".stripMargin.trim
        )
      } else Nil
    val decodeMethod =
      List(
        q"""@Override
           |public $name decode($baboonCodecContext ctx, $jsonNode wire) {
           |  ${dec.shift(2).trim}
           |}""".stripMargin.trim
      )

    val anyHelpers: List[TextTree[JvValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil
    val baseMethods                         = encodeMethod ++ decodeMethod ++ anyHelpers
    val cName                               = codecName(srcRef, defn.defn.id.owner)
    val meta                                = renderMeta(defn, jvDomainTreeTools.makeCodecMeta(defn))

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

    q"""public final class ${cName.asName} extends $cParent {
       |  private ${cName.asName}() {}
       |
       |  public static final ${cName.asName} INSTANCE = new ${cName.asName}();
       |
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${meta.joinN().shift(2).trim}
       |}
       |""".stripMargin
  }

  private def genForeignBodies(name: JvValue.JvType): (TextTree[JvValue], TextTree[JvValue]) = {
    (
      q"""throw new $javaIllegalArgumentException("${name.name} is a foreign type");""",
      q"""throw new $javaIllegalArgumentException("${name.name} is a foreign type");""",
    )
  }

  private def wrapAdtBranchEncoder(
    branchName: String,
    tree: TextTree[JvValue],
  ): TextTree[JvValue] = {
    q"""$jsonNodeFactory.instance.objectNode().set("$branchName", $tree)"""
  }

  private def genAdtBodies(name: JvValue.JvType, adt: Typedef.Adt): (TextTree[JvValue], TextTree[JvValue]) = {
    val branches = adt.dataMembers(domain).map {
      m =>
        val branchName = m.name.name
        val fqBranch   = trans.toJvTypeRefKeepForeigns(m, domain, evo)
        val branchRef  = q"branchVal"

        val routedBranchEncoder = q"${codecName(fqBranch, m.owner)}.INSTANCE.encode(ctx, $branchRef)"
        val branchEncoder = if (target.language.wrappedAdtBranchCodecs) {
          routedBranchEncoder
        } else {
          wrapAdtBranchEncoder(branchName, routedBranchEncoder)
        }

        (
          q"""if (value instanceof $fqBranch $branchRef) {
             |  return $branchEncoder;
             |}""".stripMargin,
          q"""case "$branchName":
             |  return ${codecName(fqBranch, m.owner)}.INSTANCE.decode(ctx, entry.getValue());""".stripMargin,
        )
    }

    (
      q"""${branches.map(_._1).joinN().trim}
         |throw new $javaIllegalArgumentException("Cannot encode to ${name.name}: unknown subtype " + value.getClass().getName());""".stripMargin,
      q"""$objectNode jsonObj = ($objectNode) wire;
         |var fields = jsonObj.fields();
         |if (!fields.hasNext()) {
         |  throw new $javaIllegalArgumentException("Cannot decode to ${name.name}: empty json object");
         |}
         |var entry = fields.next();
         |switch (entry.getKey()) {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |  default:
         |    throw new $javaIllegalArgumentException("Cannot decode to ${name.name}: unknown key " + entry.getKey());
         |}""".stripMargin,
    )
  }

  private def genEnumBodies(name: JvValue.JvType): (TextTree[JvValue], TextTree[JvValue]) = {
    (
      q"""return new $textNode(value.name());""",
      q"""var str = wire.textValue();
         |var parsed = $name.parse(str.trim());
         |if (parsed == null) {
         |  throw new $javaIllegalArgumentException("Cannot decode to ${name.name}: no matching value for " + str);
         |}
         |return parsed;""".stripMargin,
    )
  }

  private def genDtoBodies(name: JvValue.JvType, d: Typedef.Dto): (TextTree[JvValue], TextTree[JvValue]) = {
    val encFields = d.fields.map {
      f =>
        val fieldRef = q"value.${f.name.name}()"
        val enc      = mkEncoder(f.tpe, fieldRef)
        q"""obj.set("${f.name.name}", $enc);"""
    }

    val decFields = d.fields.map {
      f =>
        mkDecoder(f.name.name, f.tpe, q"jsonObj")
    }

    val mainEnc = q"""var obj = $jsonNodeFactory.instance.objectNode();
                     |${encFields.joinN().trim}
                     |return obj;""".stripMargin

    val encBody = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        q"""var obj = $jsonNodeFactory.instance.objectNode();
           |${encFields.joinN().trim}
           |return ${wrapAdtBranchEncoder(d.id.name.name, q"obj")};""".stripMargin
      case _ => mainEnc
    }

    val decBody = if (d.fields.nonEmpty) {
      q"""var jsonObj = ($objectNode) wire;
         |return new $name(
         |  ${decFields.join(",\n").shift(2).trim}
         |);""".stripMargin
    } else {
      q"""return new $name();"""
    }

    (encBody, decBody)
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[JvValue], depth: Int = 0): TextTree[JvValue] = {
    val varName = s"e$depth"
    val objName = s"obj$depth"
    val arrName = s"arr$depth"

    def encodeKey(tpe: TypeRef, ref: TextTree[JvValue]): TextTree[JvValue] = {
      tpe.id match {
        case TypeId.Builtins.tsu   => q"$baboonTimeFormats.formatTsu($ref)"
        case TypeId.Builtins.tso   => q"$baboonTimeFormats.formatTso($ref)"
        case TypeId.Builtins.uid   => q"$ref.toString()"
        case TypeId.Builtins.f128  => q"$ref.toPlainString()"
        case TypeId.Builtins.bytes => q"$ref.toHex()"
        case _: TypeId.Builtin     => q"String.valueOf($ref)"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum =>
                  q"$ref.name()"
                case f: Typedef.Foreign =>
                  f.bindings.get(BaboonLang.Java) match {
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
          case TypeId.Builtins.uid   => q"new $textNode($ref.toString())"
          case TypeId.Builtins.tsu   => q"new $textNode($baboonTimeFormats.formatTsu($ref))"
          case TypeId.Builtins.tso   => q"new $textNode($baboonTimeFormats.formatTso($ref))"
          case TypeId.Builtins.bit   => q"$booleanNode.valueOf($ref)"
          case TypeId.Builtins.i08   => q"$shortNode.valueOf((short) $ref)"
          case TypeId.Builtins.i16   => q"$shortNode.valueOf($ref)"
          case TypeId.Builtins.i32   => q"$intNode.valueOf($ref)"
          case TypeId.Builtins.i64   => q"$longNode.valueOf($ref)"
          case TypeId.Builtins.u08   => q"$shortNode.valueOf($ref)"
          case TypeId.Builtins.u16   => q"$intNode.valueOf($ref)"
          case TypeId.Builtins.u32   => q"$longNode.valueOf($ref)"
          case TypeId.Builtins.u64   => q"new $textNode(Long.toUnsignedString($ref))"
          case TypeId.Builtins.f32   => q"$floatNode.valueOf($ref)"
          case TypeId.Builtins.f64   => q"$doubleNode.valueOf($ref)"
          case TypeId.Builtins.f128  => q"new $textNode($ref.toPlainString())"
          case TypeId.Builtins.str   => q"new $textNode($ref)"
          case TypeId.Builtins.bytes => q"new $textNode($ref.toHex())"
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Java) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, depth)
                  case _ =>
                    val targetTpe = codecName(trans.toJvTypeRefKeepForeigns(u, domain, evo), u.owner)
                    q"$targetTpe.INSTANCE.encode(ctx, $ref)"
                }
              case _ =>
                val targetTpe = codecName(trans.toJvTypeRefKeepForeigns(u, domain, evo), u.owner)
                q"$targetTpe.INSTANCE.encode(ctx, $ref)"
            }
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""$ref.isPresent() ? ${mkEncoder(c.args.head, q"$ref.get()", depth + 1)} : $nullNode.getInstance()"""
          case TypeId.Builtins.map =>
            val keyEnc      = encodeKey(c.args.head, q"$varName.getKey()")
            val valueEnc    = mkEncoder(c.args.last, q"$varName.getValue()", depth + 1)
            val sortedName  = s"sortedEntries$depth"
            // BAB-J03: sort entrySet by stringified key before emit. Insulates against
            // user-supplied Map collections (HashMap, ImmutableCollections$MapN, ...) whose
            // iteration order depends on hash codes / runtime state. Mirrors Scala's
            // sortBy(_._1.toString) contract from PR-48 / BAB-J01.
            q"""((java.util.function.Supplier<$jsonNode>) () -> { var $objName = $jsonNodeFactory.instance.objectNode(); var $sortedName = new java.util.ArrayList<>($ref.entrySet()); $sortedName.sort((a, b) -> String.valueOf(a.getKey()).compareTo(String.valueOf(b.getKey()))); for (var $varName : $sortedName) { $objName.set($keyEnc, $valueEnc); } return $objName; }).get()"""
          case TypeId.Builtins.lst =>
            q"""((java.util.function.Supplier<$jsonNode>) () -> { var $arrName = $jsonNodeFactory.instance.arrayNode(); for (var $varName : $ref) { $arrName.add(${mkEncoder(
                c.args.head,
                q"$varName",
                depth + 1,
              )}); } return $arrName; }).get()"""
          case TypeId.Builtins.set =>
            q"""((java.util.function.Supplier<$jsonNode>) () -> { var $arrName = $jsonNodeFactory.instance.arrayNode(); for (var $varName : $ref) { $arrName.add(${mkEncoder(
                c.args.head,
                q"$varName",
                depth + 1,
              )}); } return $arrName; }).get()"""
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref)
    }
  }

  private def mkDecoder(fieldName: String, tpe: TypeRef, jsonObjRef: TextTree[JvValue]): TextTree[JvValue] = {
    def decodeElement(tpe: TypeRef, ref: TextTree[JvValue], depth: Int): TextTree[JvValue] = {
      val varName = s"e$depth"
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case TypeId.Builtins.bit   => q"$ref.booleanValue()"
            case TypeId.Builtins.i08   => q"(byte) $ref.intValue()"
            case TypeId.Builtins.i16   => q"(short) $ref.intValue()"
            case TypeId.Builtins.i32   => q"$ref.intValue()"
            case TypeId.Builtins.i64   => q"($ref.isTextual() ? Long.parseLong($ref.textValue()) : $ref.longValue())"
            case TypeId.Builtins.u08   => q"(short) $ref.intValue()"
            case TypeId.Builtins.u16   => q"$ref.intValue()"
            case TypeId.Builtins.u32   => q"$ref.longValue()"
            case TypeId.Builtins.u64   => q"($ref.isTextual() ? Long.parseUnsignedLong($ref.textValue()) : $ref.longValue())"
            case TypeId.Builtins.f32   => q"(float) $ref.doubleValue()"
            case TypeId.Builtins.f64   => q"$ref.doubleValue()"
            case TypeId.Builtins.f128  => q"($ref.isTextual() ? new $jvBigDecimal($ref.textValue()) : $ref.decimalValue())"
            case TypeId.Builtins.str   => q"$ref.textValue()"
            case TypeId.Builtins.bytes => q"$jvByteString.fromHex($ref.textValue())"
            case TypeId.Builtins.uid   => q"$jvUid.fromString($ref.textValue())"
            case TypeId.Builtins.tsu   => q"$baboonTimeFormats.parseTsu($ref.textValue())"
            case TypeId.Builtins.tso   => q"$baboonTimeFormats.parseTso($ref.textValue())"
            case u: TypeId.User =>
              domain.defs.meta.nodes(u) match {
                case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                  f.bindings.get(BaboonLang.Java) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      decodeElement(aliasedRef, ref, depth)
                    case _ =>
                      val targetTpe = codecName(trans.toJvTypeRefKeepForeigns(u, domain, evo), u.owner)
                      q"$targetTpe.INSTANCE.decode(ctx, $ref)"
                  }
                case _ =>
                  val targetTpe = codecName(trans.toJvTypeRefKeepForeigns(u, domain, evo), u.owner)
                  q"$targetTpe.INSTANCE.decode(ctx, $ref)"
              }
            case o =>
              throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
        case c: TypeRef.Constructor =>
          c.id match {
            case TypeId.Builtins.opt =>
              q"""$ref == null || $ref.isNull() ? java.util.Optional.empty() : java.util.Optional.of(${decodeElement(c.args.head, ref, depth + 1)})"""
            case TypeId.Builtins.lst =>
              val elemDec = decodeElement(c.args.head, q"$varName", depth + 1)
              q"""((java.util.function.Supplier<$jvList<${trans.asJvBoxedRef(c.args.head, domain, evo)}>>) () -> { var lst = new $jvArrayList<${trans.asJvBoxedRef(
                  c.args.head,
                  domain,
                  evo,
                )}>(); for (var $varName : (Iterable<$jsonNode>) () -> $ref.elements()) { lst.add($elemDec); } return lst; }).get()"""
            case TypeId.Builtins.set =>
              val elemDec = decodeElement(c.args.head, q"$varName", depth + 1)
              q"""((java.util.function.Supplier<$jvSet<${trans.asJvBoxedRef(c.args.head, domain, evo)}>>) () -> { var set = new $jvLinkedHashSet<${trans.asJvBoxedRef(
                  c.args.head,
                  domain,
                  evo,
                )}>(); for (var $varName : (Iterable<$jsonNode>) () -> $ref.elements()) { set.add($elemDec); } return set; }).get()"""
            case TypeId.Builtins.map =>
              val keyDec   = decodeKey(c.args.head, q"$varName.getKey()")
              val valueDec = decodeElement(c.args.last, q"$varName.getValue()", depth + 1)
              q"""((java.util.function.Supplier<$jvMap<${trans.asJvBoxedRef(c.args.head, domain, evo)}, ${trans.asJvBoxedRef(
                  c.args.last,
                  domain,
                  evo,
                )}>>) () -> { var map = new $jvLinkedHashMap<${trans.asJvBoxedRef(c.args.head, domain, evo)}, ${trans.asJvBoxedRef(
                  c.args.last,
                  domain,
                  evo,
                )}>(); for (var $varName : (Iterable<java.util.Map.Entry<String, $jsonNode>>) () -> $ref.fields()) { map.put($keyDec, $valueDec); } return map; }).get()"""
            case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
        case a: TypeRef.Any => mkAnyDecoder(a, ref)
      }
    }

    def decodeKey(tpe: TypeRef, ref: TextTree[JvValue]): TextTree[JvValue] = {
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case TypeId.Builtins.bit   => q"Boolean.parseBoolean($ref)"
            case TypeId.Builtins.i08   => q"Byte.parseByte($ref)"
            case TypeId.Builtins.i16   => q"Short.parseShort($ref)"
            case TypeId.Builtins.i32   => q"Integer.parseInt($ref)"
            case TypeId.Builtins.i64   => q"Long.parseLong($ref)"
            case TypeId.Builtins.u08   => q"Short.parseShort($ref)"
            case TypeId.Builtins.u16   => q"Integer.parseInt($ref)"
            case TypeId.Builtins.u32   => q"Long.parseLong($ref)"
            case TypeId.Builtins.u64   => q"Long.parseUnsignedLong($ref)"
            case TypeId.Builtins.f32   => q"Float.parseFloat($ref)"
            case TypeId.Builtins.f64   => q"Double.parseDouble($ref)"
            case TypeId.Builtins.f128  => q"new $jvBigDecimal($ref)"
            case TypeId.Builtins.str   => q"$ref"
            case TypeId.Builtins.bytes => q"$jvByteString.fromHex($ref)"
            case TypeId.Builtins.uid   => q"$jvUid.fromString($ref)"
            case TypeId.Builtins.tsu   => q"$baboonTimeFormats.parseTsu($ref)"
            case TypeId.Builtins.tso   => q"$baboonTimeFormats.parseTso($ref)"
            case u: TypeId.User =>
              domain.defs.meta.nodes(u) match {
                case ud: DomainMember.User =>
                  ud.defn match {
                    case _: Typedef.Enum =>
                      val targetTpe = trans.toJvTypeRefKeepForeigns(u, domain, evo)
                      q"""$targetTpe.parse($ref)"""
                    case f: Typedef.Foreign =>
                      f.bindings.get(BaboonLang.Java) match {
                        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                          decodeKey(aliasedRef, ref)
                        case _ =>
                          val targetTpe = codecName(trans.toJvTypeRefKeepForeigns(u, domain, evo), u.owner)
                          q"$targetTpe.INSTANCE.decode(ctx, new $textNode($ref))"
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
        val fieldNode = q"""$jsonObjRef.get("$fieldName")"""
        q"""((java.util.function.Supplier<java.util.Optional<${trans.asJvBoxedRef(
            args.head,
            domain,
            evo,
          )}>>) () -> { var v = $fieldNode; return v == null || v.isNull() ? java.util.Optional.empty() : java.util.Optional.of(${decodeElement(
            args.head,
            q"v",
            0,
          )}); }).get()"""
      case _ =>
        q"""${decodeElement(tpe, q"""$jsonObjRef.get("$fieldName")""", 0)}"""
    }
  }

  // Deep walk (mirrors Scala/C#/Rust/Kotlin/Java-UEBA `hasAnyField`): a codec object needs the
  // any-field helpers if any direct or nested-via-Constructor-arg field has type `any`.
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
  // See `anyStaticFallbacks` for the per-variant table.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[JvValue]): TextTree[JvValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"encodeAnyField(ctx, (byte)$expectedHex, $staticDom, $staticVer, $staticTid, $ref)"
  }

  // Decode delegates to the per-codec-object `decodeAnyField` helper. JSON decode never cross-
  // converts (always returns `AnyOpaqueJson` from JSON wire); user calls `facade.decodeAny(opaque)`
  // for typed resolution. No `ctx` / no static fallbacks needed at the decode site.
  private def mkAnyDecoder(a: TypeRef.Any, ref: TextTree[JvValue]): TextTree[JvValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"decodeAnyField((byte)$expectedHex, $ref)"
  }

  // Static fallbacks for the cross-format facade helper (`uebaToJson`). The wire `meta` may omit
  // components that are pinned by the field's static declaration; the codec emits whatever is
  // statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade.buildSyntheticTypeMeta` for the merge semantics. Per spec table:
  //   A=(null,null,null), B=(currentDomain,null,null), C=(currentDomain,currentVersion,null),
  //   D1=(null,null,underlyingFqid), D2=(currentDomain,null,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // Duplicated across Scala/C#/Rust/Kotlin/Java — extraction deferred (textual emission diverges
  // by language flavor; see PR 4.2 ledger entry's DRY analysis). 7th instance.
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[JvValue], TextTree[JvValue], TextTree[JvValue]) = {
    val none                     = q"(String)null"
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
  // Emitted at most once per codec object that has any any-bearing fields. Mirrors PR 3.3's C#
  // `EncodeAnyField`/`DecodeAnyField` helpers and PR 5.3's Kotlin shape. PR-08-D06 lesson:
  // `AnyMetaCodec.writeJson` returns `JsonNode` typed but always produces an `ObjectNode` — we
  // cast to `ObjectNode` to add the `$c` envelope key. Kind-check on encode runs before any
  // envelope construction.
  private def anyFieldHelpers: TextTree[JvValue] = {
    q"""private $jsonNode encodeAnyField(
       |    $baboonCodecContext ctx,
       |    byte expectedKind,
       |    String staticDomain,
       |    String staticVersion,
       |    String staticTypeid,
       |    $baboonAnyOpaque value
       |) {
       |  if (value.meta().kind() != expectedKind) {
       |    throw new $baboonCodecException.EncoderFailure(
       |        "any: meta-kind 0x" + String.format("%02x", value.meta().kind() & 0xFF) +
       |        " does not match field-declared 0x" + String.format("%02x", expectedKind & 0xFF));
       |  }
       |  $jsonNode anyInner;
       |  if (value instanceof $baboonAnyOpaqueJson anyJson) {
       |    anyInner = anyJson.json();
       |  } else if (value instanceof $baboonAnyOpaqueUeba anyUeba) {
       |    var anyFacade = ctx.facade();
       |    if (anyFacade == null) {
       |      throw new $baboonCodecException.EncoderFailure(
       |          "Cannot encode AnyOpaqueUeba into JSON without a facade reference. Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), or supply AnyOpaqueJson directly.");
       |    }
       |    var anyConvResult = anyFacade.uebaToJson(anyUeba.meta(), anyUeba.bytes(), staticDomain, staticVersion, staticTypeid);
       |    if (anyConvResult instanceof $baboonEither.Left<$baboonCodecException, $jsonNode> anyConvL) {
       |      throw anyConvL.value();
       |    }
       |    anyInner = (($baboonEither.Right<$baboonCodecException, $jsonNode>) anyConvResult).value();
       |  } else {
       |    throw new $baboonCodecException.EncoderFailure(
       |        "unexpected AnyOpaque subclass: " + value.getClass().getName());
       |  }
       |  var anyEnvelope = ($objectNode) $baboonAnyMetaCodec.writeJson(value.meta());
       |  anyEnvelope.set($baboonAnyMetaCodec.ANY_CONTENT_KEY, anyInner);
       |  return anyEnvelope;
       |}
       |
       |private $baboonAnyOpaqueJson decodeAnyField(byte expectedKind, $jsonNode wire) {
       |  if (wire == null) {
       |    throw new $baboonCodecException.DecoderFailure(
       |        "any: missing JSON envelope (null token)");
       |  }
       |  var anyMetaResult = $baboonAnyMetaCodec.readJson(wire);
       |  if (anyMetaResult instanceof $baboonEither.Left<$baboonCodecException, $baboonAnyMeta> anyMetaL) {
       |    throw anyMetaL.value();
       |  }
       |  var anyMeta = (($baboonEither.Right<$baboonCodecException, $baboonAnyMeta>) anyMetaResult).value();
       |  if (anyMeta.kind() != expectedKind) {
       |    throw new $baboonCodecException.DecoderFailure(
       |        "any: wire kind 0x" + String.format("%02x", anyMeta.kind() & 0xFF) +
       |        " does not match field-declared 0x" + String.format("%02x", expectedKind & 0xFF));
       |  }
       |  var anyContent = wire.get($baboonAnyMetaCodec.ANY_CONTENT_KEY);
       |  if (anyContent == null) {
       |    throw new $baboonCodecException.DecoderFailure(
       |        "any: JSON envelope missing '" + $baboonAnyMetaCodec.ANY_CONTENT_KEY + "' content key");
       |  }
       |  return new $baboonAnyOpaqueJson(anyMeta, anyContent);
       |}""".stripMargin
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
    JvValue.JvType(realPkg, s"$prefixStr${name.name}_JsonCodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User, name: JvValue.JvType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"public static final $baboonJsonCodec<$name> codecJson = ${codecName(name, defn.defn.id.owner)}.INSTANCE;"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Java) &&
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Json"
}
