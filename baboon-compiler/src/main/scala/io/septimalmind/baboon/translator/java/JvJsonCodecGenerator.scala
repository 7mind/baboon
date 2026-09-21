package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.java.JvCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.java.JvDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.translator.AnyFieldPlan
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class JvJsonCodecGenerator(
  trans: JvTypeTranslator,
  domainTypes: JvDomainTypes,
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

    val baseMethods = encodeMethod ++ decodeMethod
    val cName       = codecName(srcRef, defn.defn.id.owner)
    val meta        = renderMeta(defn, jvDomainTreeTools.makeCodecMeta(defn))

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
        val fqBranch   = domainTypes.toJvTypeRefKeepForeigns(m)
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
        val javaName = JvTypeTranslator.escapeJvKeyword(f.name.name)
        val fieldRef = q"value.$javaName()"
        val enc      = mkEncoder(f.tpe, fieldRef)
        // Wire key is always the original model name (T1 contract).
        q"""obj.set("${f.name.name}", $enc);"""
    }

    val decFields = d.fields.map {
      f =>
        // Wire key is always the original model name; the decoder returns a value
        // passed to the record constructor whose component names are escaped.
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
        // PR-28.1-D02 (M28): u64 map keys carry the canonical unsigned wire form.
        // String.valueOf(long) emits the signed representation, which collides with
        // decode-side Long.parseUnsignedLong (rejects leading minus). Mirrors the
        // Scala-side PR-28.1 fix.
        case TypeId.Builtins.u64 => q"Long.toUnsignedString($ref)"
        case _: TypeId.Builtin   => q"String.valueOf($ref)"
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
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
                      // PR-I.1b (M24 Phase 3.1): Custom-foreign map keys route through
                      // the emitted `<Foreign>_KeyCodecHost.instance()` extension hook.
                      val hostTpe = JvValue.JvType(
                        domainTypes.toJvTypeRefKeepForeigns(uid).pkg,
                        s"${domainTypes.toJvTypeRefKeepForeigns(uid).name}_KeyCodecHost",
                      )
                      q"$hostTpe.instance().encodeKey($ref)"
                    case None =>
                      throw new RuntimeException(s"BUG: Foreign type $uid has no Java binding")
                  }
                // M19/PR-60: id types — emit canonical toString (single- or multi-field).
                case d: Typedef.Dto if d.isIdentifier =>
                  q"$ref.toString()"
                // M19/PR-60: single-primitive-field wrappers — peel and recurse via record accessor.
                case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
                  val inner = d.fields.head
                  encodeKey(inner.tpe, q"$ref.${inner.name.name}()")
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
          case b: TypeId.BuiltinScalar => JvScalarCodecEmitter.jsonEncode(b, ref)
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Java) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, depth)
                  case _ =>
                    val targetTpe = codecName(domainTypes.toJvTypeRefKeepForeigns(u), u.owner)
                    q"$targetTpe.INSTANCE.encode(ctx, $ref)"
                }
              case _ =>
                val targetTpe = codecName(domainTypes.toJvTypeRefKeepForeigns(u), u.owner)
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
            val keyEnc     = encodeKey(c.args.head, q"$varName.getKey()")
            val valueEnc   = mkEncoder(c.args.last, q"$varName.getValue()", depth + 1)
            val sortedName = s"sortedEntries$depth"
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
            case b: TypeId.BuiltinScalar => JvScalarCodecEmitter.jsonDecode(b, ref)
            case u: TypeId.User =>
              domain.defs.meta.nodes(u) match {
                case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                  f.bindings.get(BaboonLang.Java) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      decodeElement(aliasedRef, ref, depth)
                    case _ =>
                      val targetTpe = codecName(domainTypes.toJvTypeRefKeepForeigns(u), u.owner)
                      q"$targetTpe.INSTANCE.decode(ctx, $ref)"
                  }
                case _ =>
                  val targetTpe = codecName(domainTypes.toJvTypeRefKeepForeigns(u), u.owner)
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
              q"""((java.util.function.Supplier<$jvList<${domainTypes.asJvBoxedRef(c.args.head)}>>) () -> { var lst = new $jvArrayList<${domainTypes.asJvBoxedRef(
                  c.args.head,
                )}>(); for (var $varName : (Iterable<$jsonNode>) () -> $ref.elements()) { lst.add($elemDec); } return lst; }).get()"""
            case TypeId.Builtins.set =>
              val elemDec = decodeElement(c.args.head, q"$varName", depth + 1)
              q"""((java.util.function.Supplier<$jvSet<${domainTypes.asJvBoxedRef(c.args.head)}>>) () -> { var set = new $jvLinkedHashSet<${domainTypes.asJvBoxedRef(
                  c.args.head,
                )}>(); for (var $varName : (Iterable<$jsonNode>) () -> $ref.elements()) { set.add($elemDec); } return set; }).get()"""
            case TypeId.Builtins.map =>
              val keyDec   = decodeKey(c.args.head, q"$varName.getKey()")
              val valueDec = decodeElement(c.args.last, q"$varName.getValue()", depth + 1)
              q"""((java.util.function.Supplier<$jvMap<${domainTypes.asJvBoxedRef(c.args.head)}, ${domainTypes.asJvBoxedRef(
                  c.args.last,
                )}>>) () -> { var map = new $jvLinkedHashMap<${domainTypes.asJvBoxedRef(c.args.head)}, ${domainTypes.asJvBoxedRef(
                  c.args.last,
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
                      val targetTpe = domainTypes.toJvTypeRefKeepForeigns(u)
                      q"""$targetTpe.parse($ref)"""
                    case f: Typedef.Foreign =>
                      f.bindings.get(BaboonLang.Java) match {
                        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                          decodeKey(aliasedRef, ref)
                        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
                          // PR-I.1b (M24 Phase 3.1): Custom-foreign map keys route through
                          // the emitted `<Foreign>_KeyCodecHost.instance()` extension hook.
                          // catch (Exception e) — NOT Throwable (PR-I-D01 pattern guidance):
                          // Throwable would swallow Error (OOM/StackOverflow), which we want to
                          // propagate to fail-fast.
                          val srcRef   = domainTypes.toJvTypeRefKeepForeigns(u)
                          val derefTpe = domainTypes.asJvType(u)
                          val hostTpe  = JvValue.JvType(srcRef.pkg, s"${srcRef.name}_KeyCodecHost")
                          q"""((java.util.function.Supplier<$derefTpe>) () -> { try { return $hostTpe.instance().decodeKey($ref); } catch (Exception e) { throw new $baboonCodecException.DecoderFailure("malformed key: " + $ref, e); } }).get()"""
                        case None =>
                          throw new RuntimeException(s"BUG: Foreign type $u has no Java binding")
                      }
                    // M19/PR-60: id types — call parseRepr and unwrap Right.
                    // PR-F (M24): throw BaboonCodecException.DecoderFailure on Left for
                    // cross-language malformed-key consistency (replaces unchecked cast).
                    case d: Typedef.Dto if d.isIdentifier =>
                      val targetTpe   = domainTypes.toJvTypeRefKeepForeigns(u)
                      val nestedCodec = JvValue.JvType(targetTpe.pkg, s"${targetTpe.name}Codec")
                      q"""((java.util.function.Supplier<$targetTpe>) () -> { var __e = $nestedCodec.parseRepr($ref); if (__e instanceof $baboonEither.Right<?,?> __r) { return ($targetTpe) __r.value(); } throw new $baboonCodecException.DecoderFailure("malformed key: " + $ref); }).get()"""
                    // M19/PR-60: single-primitive-field wrappers — peel and recurse, then construct.
                    case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
                      val inner     = d.fields.head
                      val targetTpe = domainTypes.toJvTypeRefKeepForeigns(u)
                      val innerDec  = decodeKey(inner.tpe, ref)
                      q"new $targetTpe($innerDec)"
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
        q"""((java.util.function.Supplier<java.util.Optional<${domainTypes.asJvBoxedRef(
            args.head,
          )}>>) () -> { var v = $fieldNode; return v == null || v.isNull() ? java.util.Optional.empty() : java.util.Optional.of(${decodeElement(
            args.head,
            q"v",
            0,
          )}); }).get()"""
      case _ =>
        q"""${decodeElement(tpe, q"""$jsonObjRef.get("$fieldName")""", 0)}"""
    }
  }

  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[JvValue]): TextTree[JvValue] = {
    val plan                                               = AnyFieldPlan.forField(a, domain)
    val expectedHex                                        = "0x%02x".format(plan.kind & 0xFF)
    def fallback(value: Option[String]): TextTree[JvValue] = value.fold(q"(String) null")(v => q"""(String) "$v"""")
    q"$baboonAnyJsonCodec.encode(ctx, (byte) $expectedHex, ${fallback(plan.staticDomain)}, ${fallback(plan.staticVersion)}, ${fallback(plan.staticTypeId)}, $ref)"
  }

  private def mkAnyDecoder(a: TypeRef.Any, ref: TextTree[JvValue]): TextTree[JvValue] = {
    val expectedHex = "0x%02x".format(AnyFieldPlan.forField(a, domain).kind & 0xFF)
    q"$baboonAnyJsonCodec.decode((byte) $expectedHex, $ref)"
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
    val realPkg     = domainTypes.effectiveJvPkg(owner)
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
