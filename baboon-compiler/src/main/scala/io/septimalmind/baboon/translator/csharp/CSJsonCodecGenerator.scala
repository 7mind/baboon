package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.{CodecArguments, CodecMeta}
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSTypeOrigin
import io.septimalmind.baboon.typer.model.*
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
        case _: Typedef.Foreign =>
          Some(genForeignBodies(csRef))
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

    val methods = encoderMethods ++ decoderMethods

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
        val branchNs      = q"${csTypeInfo.adtNsName(a.id)}"
        val branchName    = m.name.name
        val fqBranch      = q"$branchNs.$branchName"
        val branchNameRef = q"${branchName.toLowerCase}"

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
      q"""var asStr = wire.Value<String>()?.ToLower().Trim('"');
         |if (asStr == null)
         |{
         |    throw new $csArgumentException($$"Cannot decode {wire} to ${name.name}: string expected");
         |}
         |
         |if ($name.TryParse(asStr, true, out $name result))
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
        val fieldRef = q"value.${f.name.name.capitalize}"
        val enc      = mkEncoder(f.tpe, fieldRef)
        val dec      = mkDecoder(f.tpe, q"""asObject["${f.name.name}"]""")
        (
          q"""new $nsJProperty("${f.name.name}", $enc)""",
          q"${f.name.name.capitalize}: $dec",
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
        case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.ToString($ref)"
        case _: TypeId.Builtin                         => q"$ref.ToString()"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum | _: Typedef.Foreign =>
                  q"""${codecName(uid)}.Instance.Encode(ctx, $ref).ToString($nsFormatting.None)"""
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
          case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
            q"new $nsJValue($baboonTimeFormats.ToString($ref))"
          case _: TypeId.BuiltinScalar =>
            q"new $nsJValue($ref)"
          case u: TypeId.User =>
            val targetTpe = codecName(u)
            q"""$targetTpe.Instance.Encode(ctx, $ref)"""
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
        case TypeId.Builtins.f128                      => q"$fref.Value<$csDecimal>()!"
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
                case _: Typedef.Enum | _: Typedef.Foreign =>
                  q"""${codecName(uid)}.Instance.Decode(ctx, new $nsJValue($ref!))"""
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
        q"""${codecName(u)}.Instance.Decode(ctx, $ref!)"""

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
    }

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
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "json"
}
