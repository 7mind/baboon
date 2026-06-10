package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.{CodecArguments, CodecMeta}
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSTypeOrigin
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSJsonCodecGenerator(
  trans: CSTypeTranslator,
  csDomTrees: CSDomainTreeTools,
  target: CSTarget,
  domain: Domain,
  evo: BaboonEvolution,
  csTypeInfo: CSTypeInfo,
) extends CSCodecTranslator {
  override def translate(defn: DomainMember.User, csRef: CSValue.CSType, srcRef: CSValue.CSType): Option[TextTree[CSValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto =>
          Some(genDtoBodies(csRef, d))
        case _: Typedef.Enum =>
          Some(genEnumBodies(csRef))
        case a: Typedef.Adt =>
          Some(genAdtBodies(csRef, a))
        case f: Typedef.Foreign =>
          f.bindings.get(BaboonLang.Cs) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
            case _                                                                  => Some(genForeignBodies(csRef))
          }
        case _: Typedef.Contract =>
          None
        case _: Typedef.Service =>
          None
      }).map {
        case (enc, dec) =>
          // plumbing reference leaks
          val insulatedEnc =
            q"""if (this != LazyInstance.Value)
               |{
               |    return LazyInstance.Value.Encode(ctx, value);
               |}
               |
               |$enc
               |""".stripMargin.trim

          val insulatedDec =
            q"""if (this != LazyInstance.Value)
               |{
               |    return LazyInstance.Value.Decode(ctx, wire);
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
    } else {
      None
    }
  }

  private def genCodec(
    defn: DomainMember.User,
    name: CSValue.CSType,
    srcRef: CSValue.CSType,
    enc: TextTree[CSValue],
    dec: TextTree[CSValue],
  ): TextTree[CSValue] = {
    val isEncoderEnabled = target.language.enableDeprecatedEncoders || domain.version == evo.latest
    val isAdtMember = defn.id.owner match {
      case Owner.Adt(_) => true
      case _            => false
    }

    val encoderMethods = if (isEncoderEnabled) {
      List(
        q"""public override $nsJToken Encode($baboonCodecContext ctx, $name value)
           |{
           |    ${enc.shift(4).trim}
           |}
           |""".stripMargin
      )
    } else {
      List.empty
    }

    val decoderMethods = List(
      q"""public override $name Decode($baboonCodecContext ctx, $nsJToken wire)
         |{
         |    ${dec.shift(4).trim}
         |}""".stripMargin
    )

    val cName = codecName(srcRef, CSTypeOrigin(defn.id, domain))
    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecBase<$name, $cName>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecBase<$name, $cName>"
        case _ if isAdtMember                               => q"$baboonJsonCodecBaseGeneratedAdt<$name, $cName>"
        case _                                              => q"$baboonJsonCodecBaseGenerated<$name, $cName>"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecNoEncoder<$name, $cName>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecNoEncoder<$name, $cName>"
        case _ if isAdtMember                               => q"$baboonJsonCodecNoEncoderGeneratedAdt<$name, $cName>"
        case _                                              => q"$baboonJsonCodecNoEncoderGenerated<$name, $cName>"
      }
    }

    val anyHelpers: List[TextTree[CSValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil
    val methods                             = encoderMethods ++ decoderMethods ++ anyHelpers

    q"""public class ${cName.asName} : $cParent
       |{
       |    ${methods.join("\n\n").shift(4).trim}
       |
       |    ${csDomTrees.makeCodecMeta(defn).join("\n").shift(4).trim}
       |}
       |""".stripMargin
  }

  private def genForeignBodies(
    name: CSValue.CSType
  ): (TextTree[Nothing], TextTree[Nothing]) = {
    (
      q"""throw new ArgumentException("${name.name} is a foreign type");""",
      q"""throw new ArgumentException("${name.name} is a foreign type");""",
    )
  }

  private def wrapAdtBranchEncoder(
    branchName: String,
    tree: TextTree[CSValue],
  ): TextTree[CSValue] = {
    q"""new $nsJObject(new $nsJProperty("$branchName", $tree))"""
  }

  private def genAdtBodies(name: CSValue.CSType, a: Typedef.Adt): (TextTree[CSValue], TextTree[Nothing]) = {

    val branches = a.dataMembers(domain).map {
      m =>
        val branchNs      = q"${escapeCsKeyword(csTypeInfo.adtNsName(a.id))}"
        val branchName    = m.name.name
        val fqBranch      = q"$branchNs.${escapeCsKeyword(branchName)}"
        val branchNameRef = q"${escapeCsKeyword(branchName.toLowerCase)}"

        val branchTpe           = trans.asCsType(m, domain, evo)
        val branchCodec         = codecName(branchTpe, CSTypeOrigin(m, domain))
        val routedBranchEncoder = q"$branchCodec.Instance.Encode(ctx, $branchNameRef)"

        val branchEncoder = if (target.language.wrappedAdtBranchCodecs) {
          routedBranchEncoder
        } else {
          wrapAdtBranchEncoder(branchName, routedBranchEncoder)
        }

        val branchValue = if (target.language.wrappedAdtBranchCodecs) {
          q"wire"
        } else {
          q"head.Value"
        }

        (
          q"""if (value is $fqBranch $branchNameRef)
             |{
             |    return $branchEncoder;
             |}""".stripMargin,
          q"""if (head.Name == "$branchName")
             |{
             |    return ${fqBranch}_JsonCodec.Instance.Decode(ctx, $branchValue);
             |}""".stripMargin,
        )

    }

    (
      q"""${branches.map(_._1).join("\n")}
         |
         |throw new $csArgumentException($$"Cannot encode {value}: unexpected subclass");
         |""".stripMargin,
      q"""var asObject = wire.Value<JObject>();
         |if (asObject == null)
         |{
         |    throw new ArgumentException($$"Cannot decode {wire} to ${name.name}: object expected");
         |}
         |var head = asObject.Properties().First();
         |
         |${branches.map(_._2).join("\n")}
         |
         |throw new ArgumentException($$"Cannot decode {wire} to ${name.name}: no matching value");
         |""".stripMargin,
    )
  }

  private def genEnumBodies(
    name: CSValue.CSType
  ): (TextTree[CSValue.CSType], TextTree[CSValue.CSType]) = {
    (
      q"return $nsJValue.CreateString(value.ToString());",
      q"""var asStr = wire.Value<String>()?.Trim('"');
         |if (asStr == null)
         |{
         |    throw new $csArgumentException($$"Cannot decode {wire} to ${name.name}: string expected");
         |}
         |
         |if ($name.TryParse(asStr, out $name result) && result.ToString() == asStr)
         |{
         |    return result;
         |}
         |
         |throw new $csArgumentException($$"Cannot decode {wire} to ${name.name}: no matching value");
         |""".stripMargin,
    )
  }

  private def genDtoBodies(name: CSValue.CSType, d: Typedef.Dto): (TextTree[CSValue], TextTree[CSValue]) = {
    val fields = d.fields.map {
      f =>
        val fieldRef = q"value.${escapeCsKeyword(f.name.name.capitalize)}"
        val enc      = mkEncoder(f.tpe, fieldRef)
        // The string literals below are on-wire JSON keys — they MUST stay the
        // original model field name (no `@` escaping; `@` is source-only).
        val dec      = mkDecoder(f.tpe, q"""asObject["${f.name.name}"]""")
        (
          q"""new $nsJProperty("${f.name.name}", $enc)""",
          q"${escapeCsKeyword(f.name.name.capitalize)}: $dec",
        )
    }

    val mainEnc = q"""new $nsJObject(
                     |${fields.map(_._1).join(",\n").shift(4)}
                     |)""".stripMargin

    val fullEnc = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        wrapAdtBranchEncoder(d.id.name.name, mainEnc)
      case _ => mainEnc
    }

    val encBody = q"""return $fullEnc;"""

    val fullDec = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs => q"wire.Value<JObject>()!.Properties().First().Value.Value<JObject>()"
      case _                                                      => q"wire.Value<JObject>()"
    }

    val decBody =
      q"""var asObject = $fullDec;
         |
         |if (asObject == null)
         |{
         |    throw new ArgumentException($$"Cannot decode {wire} to ${name.name}: object expected");
         |}
         |
         |return new $name(
         |${fields.map(_._2).join(",\n").shift(4)}
         |);
         |""".stripMargin

    (encBody, decBody)
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[CSValue], codecArgs: CodecArguments = CodecArguments.empty): TextTree[CSValue] = {
    def encodeKey(tpe: TypeRef, ref: TextTree[CSValue]): TextTree[CSValue] = {
      tpe.id match {
        // PR-28.3 (M28): tso always renders ±HH:MM (UTC = "+00:00"); tsu renders trailing 'Z'.
        case TypeId.Builtins.tsu => q"$baboonTimeFormats.TsuToString($ref)"
        case TypeId.Builtins.tso => q"$baboonTimeFormats.TsoToString($ref)"
        // PR-26.5 (M26) — C# `bool.ToString()` returns "True"/"False" (capitalized),
        // diverging from the canonical lowercase wire form emitted by the other 9
        // backends (Scala/Rust/Java/Kotlin/KMP/TS/Dart/Swift/Python). Force lowercase
        // for cross-language byte-identity. Closes the bit-key arm of PR-G-D01.
        case TypeId.Builtins.bit => q"$ref.ToString().ToLowerInvariant()"
        case _: TypeId.Builtin   => q"$ref.ToString()"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum =>
                  q"""${codecName(uid)}.Instance.Encode(ctx, $ref).ToString($nsFormatting.None)"""
                case f: Typedef.Foreign =>
                  f.bindings.get(BaboonLang.Cs) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      encodeKey(aliasedRef, ref)
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
                      // PR-I.1c (M24 Phase 3.1): Custom-foreign map keys route through
                      // the emitted `<Foreign>_KeyCodecHost.Instance` extension hook.
                      // `.asDerived` is mandatory: it tells `isUpgradeable` to resolve
                      // higher twins via `asCsTypeKeepForeigns` (preserving the user's
                      // package), not via `asCsType` (which would deref to e.g. `System`).
                      val srcRef  = trans.asCsTypeKeepForeigns(uid, domain, evo)
                      val hostTpe = CSValue.CSType(srcRef.pkg, s"${srcRef.name}_KeyCodecHost", srcRef.fq, CSTypeOrigin(uid, domain).asDerived)
                      q"$hostTpe.Instance.EncodeKey($ref)"
                    case None =>
                      throw new RuntimeException(s"BUG: Foreign type $uid has no C# binding")
                  }
                // M19/PR-60: id types — emit canonical ToString (single- or multi-field).
                case d: Typedef.Dto if d.isIdentifier =>
                  q"$ref.ToString()"
                // M19/PR-60: single-primitive-field wrappers — peel and recurse.
                case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
                  val inner = d.fields.head
                  encodeKey(inner.tpe, q"$ref.${escapeCsKeyword(inner.name.name.capitalize)}")
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
          case TypeId.Builtins.bytes =>
            q"new $nsJValue($ref.Encode())"
          case TypeId.Builtins.uid =>
            q"new $nsJValue($ref.ToString())"
          // PR-28.3 (M28): tso always renders ±HH:MM (UTC = "+00:00"); tsu renders trailing 'Z'.
          case TypeId.Builtins.tsu =>
            q"new $nsJValue($baboonTimeFormats.TsuToString($ref))"
          case TypeId.Builtins.tso =>
            q"new $nsJValue($baboonTimeFormats.TsoToString($ref))"
          case _: TypeId.BuiltinScalar =>
            q"new $nsJValue($ref)"
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Cs) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, codecArgs)
                  case _ =>
                    val targetTpe = codecName(u)
                    q"""$targetTpe.Instance.Encode(ctx, $ref)"""
                }
              case _ =>
                val targetTpe = codecName(u)
                q"""$targetTpe.Instance.Encode(ctx, $ref)"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            val arg = codecArgs.arg("v")
            if (csTypeInfo.isCSValueType(c.args.head, domain)) {
              q"$BaboonTools.WriteOptionVal($ref, $arg => ${mkEncoder(c.args.head, arg, codecArgs.next)})"
            } else {
              q"$BaboonTools.WriteOptionRef($ref, $arg => ${mkEncoder(c.args.head, arg, codecArgs.next)})"
            }
          case TypeId.Builtins.map =>
            val arg      = codecArgs.arg("kv")
            val keyEnc   = encodeKey(c.args.head, q"$arg.Key")
            val valueEnc = mkEncoder(c.args.last, q"$arg.Value", codecArgs.next)
            q"$BaboonTools.WriteMap($ref, $arg => new $nsJProperty($keyEnc, $valueEnc))"
          case TypeId.Builtins.lst =>
            val arg = codecArgs.arg("i")
            q"$BaboonTools.WriteSeq($ref, $arg => ${mkEncoder(c.args.head, arg, codecArgs.next)})"
          case TypeId.Builtins.set =>
            val arg = codecArgs.arg("i")
            q"$BaboonTools.WriteSeq($ref, $arg => ${mkEncoder(c.args.head, arg, codecArgs.next)})"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref)
    }
  }

  private def mkDecoder(tpe: TypeRef, ref: TextTree[CSValue], codecArgs: CodecArguments = CodecArguments.empty): TextTree[CSValue] = {
    def mkReader(bs: TypeId.BuiltinScalar): TextTree[CSValue] = {
      val fref = q"$ref!"
      bs match {
        case TypeId.Builtins.bit                       => q"$fref.Value<$csBoolean>()!"
        case TypeId.Builtins.i08                       => q"$fref.Value<$csSByte>()!"
        case TypeId.Builtins.i16                       => q"$fref.Value<$csInt16>()!"
        case TypeId.Builtins.i32                       => q"$fref.Value<$csInt32>()!"
        case TypeId.Builtins.i64                       => q"$fref.Value<$csInt64>()!"
        case TypeId.Builtins.u08                       => q"$fref.Value<$csByte>()!"
        case TypeId.Builtins.u16                       => q"$fref.Value<$csUInt16>()!"
        case TypeId.Builtins.u32                       => q"$fref.Value<$csUInt32>()!"
        case TypeId.Builtins.u64                       => q"$fref.Value<$csUInt64>()!"
        case TypeId.Builtins.f32                       => q"$fref.Value<$csSingle>()!"
        case TypeId.Builtins.f64                       => q"$fref.Value<$csDouble>()!"
        case TypeId.Builtins.f128                      => q"$BaboonTools.ReadDecimalLenient($fref)"
        case TypeId.Builtins.str                       => q"$fref.Value<$csString>()!"
        case TypeId.Builtins.bytes                     => q"$csByteString.Parse($fref.Value<$csString>()!)"
        case TypeId.Builtins.uid                       => q"$csGuid.Parse($fref.Value<$csString>()!)"
        case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.FromString($fref.Value<$csString>()!)"
        case other                                     => throw new RuntimeException(s"BUG: Unexpected type: $other")
      }
    }

    def decodeKey(tpe: TypeRef, ref: TextTree[CSValue]): TextTree[CSValue] = {
      tpe.id match {
        case TypeId.Builtins.bit                       => q"Boolean.Parse($ref)"
        case TypeId.Builtins.i08                       => q"SByte.Parse($ref)"
        case TypeId.Builtins.i16                       => q"Int16.Parse($ref)"
        case TypeId.Builtins.i32                       => q"Int32.Parse($ref)"
        case TypeId.Builtins.i64                       => q"Int64.Parse($ref)"
        case TypeId.Builtins.u08                       => q"Byte.Parse($ref)"
        case TypeId.Builtins.u16                       => q"UInt16.Parse($ref)"
        case TypeId.Builtins.u32                       => q"UInt32.Parse($ref)"
        case TypeId.Builtins.u64                       => q"UInt64.Parse($ref)"
        case TypeId.Builtins.f32                       => q"Single.Parse($ref)"
        case TypeId.Builtins.f64                       => q"Double.Parse($ref)"
        case TypeId.Builtins.f128                      => q"Decimal.Parse($ref)"
        case TypeId.Builtins.str                       => ref
        case TypeId.Builtins.bytes                     => q"$csByteString.Parse($ref)"
        case TypeId.Builtins.uid                       => q"$csGuid.Parse($ref)"
        case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.FromString($ref)"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum =>
                  q"""${codecName(uid)}.Instance.Decode(ctx, new $nsJValue($ref!))"""
                case f: Typedef.Foreign =>
                  f.bindings.get(BaboonLang.Cs) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      decodeKey(aliasedRef, ref)
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
                      // PR-I.1c (M24 Phase 3.1): Custom-foreign map keys route through
                      // the emitted `<Foreign>_KeyCodecHost.Instance` extension hook.
                      // catch (Exception e) — NOT broader (PR-I-D01 pattern guidance):
                      // narrower than Throwable-equivalents would still propagate Errors.
                      // `.asDerived` is mandatory (see encoder note above).
                      val srcRef   = trans.asCsTypeKeepForeigns(uid, domain, evo)
                      val derefTpe = trans.asCsType(uid, domain, evo)
                      val hostTpe  = CSValue.CSType(srcRef.pkg, s"${srcRef.name}_KeyCodecHost", srcRef.fq, CSTypeOrigin(uid, domain).asDerived)
                      q"""((System.Func<$derefTpe>)(() => { try { return $hostTpe.Instance.DecodeKey($ref); } catch (Exception e) { throw new $baboonCodecException.DecoderFailure("malformed key: " + $ref, e); } }))()"""
                    case None =>
                      throw new RuntimeException(s"BUG: Foreign type $uid has no C# binding")
                  }
                // M19/PR-60: id types — call ParseRepr and unwrap Right.
                // PR-F (M24): throw BaboonCodecException.DecoderFailure on Left for
                // cross-language malformed-key consistency (replaces unchecked cast).
                case d: Typedef.Dto if d.isIdentifier =>
                  val targetTpe      = trans.asCsTypeKeepForeigns(uid, domain, evo)
                  val codecClassName = CSValue.CSType(targetTpe.pkg, s"${targetTpe.name}Codec", targetTpe.fq, targetTpe.origin)
                  q"""($codecClassName.ParseRepr($ref) switch { $either<string, $targetTpe>.Right __r => __r.Value, _ => throw new $baboonCodecException.DecoderFailure("malformed key: " + $ref) })"""
                // M19/PR-60: single-primitive-field wrappers — peel and recurse, then construct.
                case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
                  val inner     = d.fields.head
                  val targetTpe = trans.asCsTypeKeepForeigns(uid, domain, evo)
                  val innerDec  = decodeKey(inner.tpe, ref)
                  q"new $targetTpe(${escapeCsKeyword(inner.name.name.capitalize)}: $innerDec)"
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
      case TypeRef.Scalar(bs: TypeId.BuiltinScalar) =>
        mkReader(bs)

      case TypeRef.Scalar(u: TypeId.User) =>
        domain.defs.meta.nodes(u) match {
          case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
            f.bindings.get(BaboonLang.Cs) match {
              case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                mkDecoder(aliasedRef, ref, codecArgs)
              case _ =>
                q"""${codecName(u)}.Instance.Decode(ctx, $ref!)"""
            }
          case _ =>
            q"""${codecName(u)}.Instance.Decode(ctx, $ref!)"""
        }

      case TypeRef.Constructor(id, args) =>
        id match {
          case TypeId.Builtins.opt if csTypeInfo.isCSValueType(args.head, domain) =>
            val arg = codecArgs.arg("v")
            q"""$BaboonTools.ReadNullableValueType($ref, $arg => ${mkDecoder(args.head, arg, codecArgs.next)})""".stripMargin

          case TypeId.Builtins.opt =>
            val arg = codecArgs.arg("v")
            q"""$BaboonTools.ReadNullableReferentialType($ref, $arg => ${mkDecoder(args.head, arg, codecArgs.next)})"""

          case TypeId.Builtins.map =>
            val arg       = codecArgs.arg("kv")
            val keyDec    = decodeKey(args.head, arg)
            val keyType   = trans.asCsRef(args.head, domain, evo)
            val valueDec  = mkDecoder(args.last, arg, codecArgs.next)
            val valueType = trans.asCsRef(args.last, domain, evo)
            q"""$BaboonTools.ReadJsonDict<$keyType, $valueType>($ref, $arg => $keyDec, $arg => $valueDec)"""

          case TypeId.Builtins.lst =>
            val arg = codecArgs.arg("i")
            q"""$BaboonTools.ReadJsonList($ref, $arg => ${mkDecoder(args.head, arg, codecArgs.next)})"""

          case TypeId.Builtins.set =>
            val arg = codecArgs.arg("i")
            q"""$BaboonTools.ReadJsonSet($ref, $arg => ${mkDecoder(args.head, arg, codecArgs.next)})"""

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a, ref)
    }

  }

  // Per-codec-object helpers consolidating the any-field JSON envelope encode/decode (kind check,
  // cross-format conversion via facade, envelope `$ak/$ad/$av/$at/$c` build & disassemble).
  // Emitted at most once per codec object that has any any-bearing fields. Mirrors PR 2.3
  // (`ScJsonCodecGenerator.anyFieldHelpers`) and PR 3.2's UEBA helper shape.
  private def anyFieldHelpers: TextTree[CSValue] = {
    q"""private const string AnyEnvelopeContentKey = "$$c";
       |
       |private $nsJToken EncodeAnyField(
       |    $baboonCodecContext ctx,
       |    byte expectedKind,
       |    $csString? staticDomain,
       |    $csString? staticVersion,
       |    $csString? staticTypeid,
       |    $baboonAnyOpaque value)
       |{
       |    if (value.Meta.Kind != expectedKind)
       |    {
       |        throw new $baboonCodecException.EncoderFailure(
       |            $$"any: meta-kind 0x{value.Meta.Kind & 0xFF:x2} does not match field-declared 0x{expectedKind & 0xFF:x2}");
       |    }
       |    $nsJToken anyInner;
       |    if (value is $baboonAnyOpaqueJson anyJson)
       |    {
       |        anyInner = anyJson.Json;
       |    }
       |    else if (value is $baboonAnyOpaqueUeba anyUeba)
       |    {
       |        var f = ctx.Facade ?? throw new $baboonCodecException.EncoderFailure(
       |            "Cannot encode AnyOpaqueUeba into JSON without a facade reference. Pass BaboonCodecContext.WithFacade(useIndices, facade) into Encode(), or supply AnyOpaqueJson directly.");
       |        var anyConvResult = f.UebaToJson(anyUeba.Meta, anyUeba.Bytes, staticDomain, staticVersion, staticTypeid);
       |        if (anyConvResult is $either<$baboonCodecException, $nsJToken>.Left anyConvL)
       |        {
       |            throw anyConvL.Value;
       |        }
       |        anyInner = (($either<$baboonCodecException, $nsJToken>.Right)anyConvResult).Value;
       |    }
       |    else
       |    {
       |        throw new $baboonCodecException.EncoderFailure(
       |            $$"unexpected AnyOpaque subclass: {value.GetType()}");
       |    }
       |    var anyEnvelope = ($nsJObject)$baboonAnyMetaCodec.WriteJson(value.Meta);
       |    anyEnvelope.Add(AnyEnvelopeContentKey, anyInner);
       |    return anyEnvelope;
       |}
       |
       |private $baboonAnyOpaqueJson DecodeAnyField(byte expectedKind, $nsJToken? wire)
       |{
       |    if (wire is null)
       |    {
       |        throw new $baboonCodecException.DecoderFailure(
       |            "any: missing JSON envelope (null token)");
       |    }
       |    var anyMetaResult = $baboonAnyMetaCodec.ReadJson(wire);
       |    if (anyMetaResult is $either<$baboonCodecException, $baboonAnyMeta>.Left anyMetaL)
       |    {
       |        throw anyMetaL.Value;
       |    }
       |    var anyMeta = (($either<$baboonCodecException, $baboonAnyMeta>.Right)anyMetaResult).Value;
       |    if (anyMeta.Kind != expectedKind)
       |    {
       |        throw new $baboonCodecException.DecoderFailure(
       |            $$"any: wire kind 0x{anyMeta.Kind & 0xFF:x2} does not match field-declared 0x{expectedKind & 0xFF:x2}");
       |    }
       |    var anyContent = wire[AnyEnvelopeContentKey];
       |    if (anyContent is null)
       |    {
       |        throw new $baboonCodecException.DecoderFailure(
       |            $$"any: JSON envelope missing '{AnyEnvelopeContentKey}' content key");
       |    }
       |    return new $baboonAnyOpaqueJson(anyMeta, anyContent);
       |}""".stripMargin
  }

  // Deep walk (mirrors PR 3.2's UEBA `hasAnyField` and PR 2.3's Scala helper): a codec object needs
  // the any-field helpers if any direct or nested-via-Constructor-arg field has type `any`.
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

  // Encode delegates to the per-codec-object `EncodeAnyField` helper; this site wires the expected
  // kind byte and the field's static (codec-gen-time) fallbacks for cross-format meta resolution.
  // See `anyStaticFallbacks` below.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[CSValue]): TextTree[CSValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"EncodeAnyField(ctx, ($csByte)$expectedHex, $staticDom, $staticVer, $staticTid, $ref)"
  }

  // Decode delegates to the per-codec-object `DecodeAnyField` helper. JSON decode never cross-
  // converts (always returns `AnyOpaqueJson` from JSON wire); user calls `facade.DecodeAny(opaque)`
  // for typed resolution. No `ctx` / no static fallbacks needed at the decode site.
  private def mkAnyDecoder(a: TypeRef.Any, ref: TextTree[CSValue]): TextTree[CSValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"DecodeAnyField(($csByte)$expectedHex, $ref)"
  }

  // Static fallbacks for the cross-format facade helper (`UebaToJson`). The wire `meta` may omit
  // components that are pinned by the field's static declaration; the codec emits whatever is
  // statically known so the facade can fill the gaps. See `BaboonCodecsFacade.BuildSyntheticTypeMeta`
  // for the merge semantics. Per spec table:
  //   A=(None,None,None), B=(currentDomain,None,None), C=(currentDomain,currentVersion,None),
  //   D1=(None,None,underlyingFqid), D2=(currentDomain,None,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[CSValue], TextTree[CSValue], TextTree[CSValue]) = {
    val none                     = q"($csString?)null"
    def some(s: String)          = q"""($csString?)"$s""""
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

  def codecName(id: TypeId.User): CSValue.CSType = {
    codecName(trans.asCsTypeKeepForeigns(id, domain, evo), CSTypeOrigin(id, domain))
  }

  def codecName(name: CSValue.CSType, origin: CSTypeOrigin.TypeInDomain): CSValue.CSType = {
    CSValue.CSType(name.pkg, s"${name.name}_JsonCodec", name.fq, origin.asDerived)
  }

  override def codecMeta(defn: DomainMember.User, name: CSValue.CSType): Option[CSCodecTranslator.CodecMeta] = {
    if (isActive(defn.id)) {
      val fix = csDomTrees.metaMethodFlags(defn, isCodec = false)
      val member =
        q"""public$fix$iBaboonJsonCodec<$name> Codec_JSON()
           |{
           |    return ${codecName(name, CSTypeOrigin(defn.id, domain))}.Instance;
           |}""".stripMargin
      Some(CodecMeta(member))
    } else {
      None
    }
  }

  def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Cs) &&
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "json"
}
