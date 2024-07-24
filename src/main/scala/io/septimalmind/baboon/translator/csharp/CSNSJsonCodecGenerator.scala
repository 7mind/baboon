package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSNSJsonCodecGenerator(trans: CSTypeTranslator, tools: CSDefnTools)
  extends CSCodecTranslator {
  override def translate(defn: DomainMember.User,
                         csRef: CSValue.CSType,
                         srcRef: CSValue.CSType,
                         domain: Domain,
                        ): TextTree[CSValue] = {
    val version = domain.version
    val (enc, dec) = defn.defn match {
      case d: Typedef.Dto =>
        genDtoBodies(csRef, domain, d)
      case _: Typedef.Enum =>
        genEnumBodies(csRef)
      case a: Typedef.Adt =>
        genAdtBodies(csRef, a)
      case _: Typedef.Foreign =>
        genForeignBodies(csRef)
    }

    // plumbing reference leaks
    val insulatedEnc =
      q"""if (this == instance.Value)
         |{
         |    ${enc.shift(4).trim}
         |}
         |
         |return instance.Value.Encode(value);""".stripMargin
    val insulatedDec =
      q"""if (this == instance.Value)
         |{
         |    ${dec.shift(4).trim}
         |}
         |
         |return instance.Value.Decode(wire);""".stripMargin

    genCodec(
      defn,
      csRef,
      srcRef,
      version,
      insulatedEnc,
      insulatedDec,
      !defn.defn.isInstanceOf[Typedef.Foreign]
    )
  }

  private def genCodec(defn: DomainMember.User,
                       name: CSValue.CSType,
                       srcRef: CSValue.CSType,
                       version: Version,
                       enc: TextTree[CSValue],
                       dec: TextTree[CSValue],
                       addExtensions: Boolean,
                      ): TextTree[CSValue] = {
    val iName = q"$iBaboonJsonCodec<$name>"
    val baseMethods = List(
      q"""public virtual $nsJToken Encode($name value)
         |{
         |    ${enc.shift(4).trim}
         |}
         |
         |public virtual $name Decode($nsJToken wire)
         |{
         |    ${dec.shift(4).trim}
         |}""".stripMargin)

    val (parents, methods) = defn.defn match {
      case _: Typedef.Enum =>
        (List(q"$iBaboonJsonCodec<$name>"), baseMethods)
      case _ =>
        val extensions = List(
          q"""public virtual $nsJToken Encode($iBaboonGenerated value)
             |{
             |    if (value is not $name dvalue)
             |        throw new Exception("Expected to have ${name.name} type");
             |    return Encode(dvalue);
             |}
             |
             |$iBaboonGenerated IBaboonValueCodec<$iBaboonGenerated, $nsJToken>.Decode($nsJToken wire)
             |{
             |    return Decode(wire);
             |}""".stripMargin
        )
        val extParents = List(q"$iBaboonJsonCodec<$iBaboonGenerated>")

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
    q"""public class $cName : ${parents.join(", ")}
       |{
       |    ${methods.join("\n").shift(4).trim}
       |
       |    ${tools.makeMeta(defn, version).join("\n").shift(4).trim}
       |
       |    private static $csLazy<$iName> instance = new $csLazy<$iName>(() => new $cName());
       |
       |    public static $iName Instance { get { return instance.Value; } set { instance = new $csLazy<$iName
       |    >(() => value); } }
       |}
     """.stripMargin
  }

  private def genForeignBodies(name: CSValue.CSType): (TextTree[Nothing], TextTree[Nothing]) = {
    (
      q"""throw new ArgumentException($$"${name.name} is a foreign type");""",
      q"""throw new ArgumentException($$"${name.name} is a foreign type");"""
    )
  }

  private def genAdtBodies(name: CSValue.CSType, a: Typedef.Adt): (TextTree[CSValue.CSType], TextTree[Nothing]) = {
    val branches = a.members.toList.map { m =>
      val branchNs = q"${trans.adtNsName(a.id)}"
      val branchName = m.name.name
      val fqBranch = q"$branchNs.$branchName"

      (
        q"""if (value is $fqBranch)
           |{
           |    return new $nsJObject(new $nsJProperty("$branchName", ${fqBranch}_JsonCodec.Instance.Encode(($fqBranch)value)));
           |}""".stripMargin,
        q"""if (head.Name == "$branchName")
           |{
           |    return ${fqBranch}_JsonCodec.Instance.Decode(head.Value);
           |}""".stripMargin
      )

    }

    (
      q"""${branches.map(_._1).join("\n")}
         |
         |throw new ${csArgumentException}($$"Cannot encode {value}: unexpected subclass");
           """.stripMargin,
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
         """.stripMargin
    )
  }

  private def genEnumBodies(name: CSValue.CSType): (TextTree[CSValue.CSType], TextTree[CSValue.CSType]) = {
    (
      q"return $nsJValue.CreateString(value.ToString());",
      q"""var asStr = wire.Value<String>()?.ToLower().Trim('"');
         |if (asStr == null)
         |{
         |    throw new $csArgumentException($$"Cannot decode {wire} to ${name.name}: string expected");
         |}
         |
         |$name result;
         |if ($name.TryParse(asStr, true, out result))
         |{
         |    return result;
         |}
         |
         |throw new $csArgumentException($$"Cannot decode {wire} to ${name.name}: no matching value");""".stripMargin
    )
  }

  private def genDtoBodies(name: CSValue.CSType,
                           domain: Domain,
                           d: Typedef.Dto): (TextTree[CSValue], TextTree[CSValue]) = {
    val fields = d.fields.map { f =>
      val fieldRef = q"value.${f.name.name.capitalize}"
      val enc = mkEncoder(f.tpe, domain, fieldRef)
      val dec = mkDecoder(f.tpe, domain, q"""asObject["${f.name.name}"]""")
      (
        q"""new $nsJProperty("${f.name.name}", $enc)""",
        q"${f.name.name.capitalize}: $dec",
      )
    }

    (
      q"""return new $nsJObject(
         |${fields.map(_._1).join(",\n").shift(4)}
         |);""".stripMargin,
      q"""var asObject = wire.Value<JObject>();
         |
         |if (asObject == null)
         |{
         |    throw new ArgumentException($$"Cannot decode {wire} to ${name.name}: object expected");
         |}
         |
         |return new $name(
         |${fields.map(_._2).join(",\n").shift(4)}
         |);
         """.stripMargin
    )
  }

  private def mkEncoder(tpe: TypeRef,
                        domain: Domain,
                        ref: TextTree[CSValue]): TextTree[CSValue] = {
    def encodeKey(tpe: TypeRef,
                  domain: Domain,
                  ref: TextTree[CSValue]): TextTree[CSValue] = {
      tpe.id match {
        case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
          q"$csDateTimeFormats.ToString($ref)"
        case _: TypeId.Builtin =>
          q"$ref.ToString()"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum | _: Typedef.Foreign =>
                  val targetTpe = trans.toCsTypeRefNoDeref(uid, domain)
                  q"""${targetTpe}_JsonCodec.Instance.Encode($ref).ToString($nsFormatting.None)"""
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
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.uid =>
            q"new $nsJValue($ref.ToString())"
          case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
            q"new $nsJValue($csDateTimeFormats.ToString($ref))"
          case _: TypeId.BuiltinScalar =>
            q"new $nsJValue($ref)"
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toCsTypeRefNoDeref(u, domain))
            q"""${targetTpe}.Instance.Encode($ref)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"$ref == null ? $nsJValue.CreateNull() : ${mkEncoder(c.args.head, domain, trans.deNull(c.args.head, ref))}"
          case TypeId.Builtins.map =>
            val keyEnc = encodeKey(c.args.head, domain, q"e.Key")
            val valueEnc = mkEncoder(c.args.last, domain, q"e.Value")
            q"new $nsJObject($ref.Select(e => new $nsJProperty($keyEnc, $valueEnc)))"
          case TypeId.Builtins.lst =>
            q"new $nsJArray($ref.Select(e => ${mkEncoder(c.args.head, domain, q"e")}))"
          case TypeId.Builtins.set =>
            q"new $nsJArray($ref.Select(e => ${mkEncoder(c.args.head, domain, q"e")}))"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def mkDecoder(tpe: TypeRef,
                        domain: Domain,
                        ref: TextTree[CSValue],
                       ): TextTree[CSValue] = {
    def mkReader(bs: TypeId.BuiltinScalar): TextTree[CSValue] = {
      val fref = q"$ref!"
      bs match {
        case TypeId.Builtins.bit =>
          q"$fref.Value<Boolean>()!"
        case TypeId.Builtins.i08 =>
          q"$fref.Value<SByte>()!"
        case TypeId.Builtins.i16 =>
          q"$fref.Value<Int16>()!"
        case TypeId.Builtins.i32 =>
          q"$fref.Value<Int32>()!"
        case TypeId.Builtins.i64 =>
          q"$fref.Value<Int64>()!"
        case TypeId.Builtins.u08 =>
          q"$fref.Value<Byte>()!"
        case TypeId.Builtins.u16 =>
          q"$fref.Value<UInt16>()!"
        case TypeId.Builtins.u32 =>
          q"$fref.Value<UInt32>()!"
        case TypeId.Builtins.u64 =>
          q"$fref.Value<UInt64>()!"
        case TypeId.Builtins.f32 =>
          q"$fref.Value<Single>()!"
        case TypeId.Builtins.f64 =>
          q"$fref.Value<Double>()!"
        case TypeId.Builtins.f128 =>
          q"$fref.Value<Decimal>()!"
        case TypeId.Builtins.str =>
          q"$fref.Value<$csString>()!"
        case TypeId.Builtins.uid =>
          q"$csGuid.Parse($fref.Value<$csString>()!)"
        case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
          q"$csDateTimeFormats.FromString($fref.Value<$csString>()!)"
        case o =>
          throw new RuntimeException(s"BUG: Unexpected type: $o")
      }
    }

    def decodeKey(tpe: TypeRef,
                  domain: Domain,
                  ref: TextTree[CSValue]): TextTree[CSValue] = {
      tpe.id match {
        case TypeId.Builtins.bit =>
          q"Boolean.Parse($ref)"
        case TypeId.Builtins.i08 =>
          q"SByte.Parse($ref)"
        case TypeId.Builtins.i16 =>
          q"Int16.Parse($ref)"
        case TypeId.Builtins.i32 =>
          q"Int32.Parse($ref)"
        case TypeId.Builtins.i64 =>
          q"Int64.Parse($ref)"
        case TypeId.Builtins.u08 =>
          q"Byte.Parse($ref)"
        case TypeId.Builtins.u16 =>
          q"UInt16.Parse($ref)"
        case TypeId.Builtins.u32 =>
          q"UInt32.Parse($ref)"
        case TypeId.Builtins.u64 =>
          q"UInt64.Parse($ref)"
        case TypeId.Builtins.f32 =>
          q"Single.Parse($ref)"
        case TypeId.Builtins.f64 =>
          q"Double.Parse($ref)"
        case TypeId.Builtins.f128 =>
          q"Decimal.Parse($ref)"
        case TypeId.Builtins.str =>
          ref
        case TypeId.Builtins.uid =>
          q"$csGuid.Parse($ref)"
        case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
          q"$csDateTimeFormats.FromString($ref)"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum | _: Typedef.Foreign =>
                  val targetTpe = trans.toCsTypeRefNoDeref(uid, domain)
                  q"""${targetTpe}_JsonCodec.Instance.Decode(new $nsJValue($ref!))"""
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
      case TypeRef.Scalar(id) =>
        id match {
          case bs: TypeId.BuiltinScalar =>
            mkReader(bs)
          case u: TypeId.User =>
            val targetTpe = trans.toCsTypeRefNoDeref(u, domain)
            q"""${targetTpe}_JsonCodec.Instance.Decode($ref!)"""
        }
      case TypeRef.Constructor(id, args) =>
        id match {
          case TypeId.Builtins.opt if trans.isCSValueType(args.head) =>
            q"""$BaboonTools.ReadNullableValue($ref, t => ${mkDecoder(args.head, domain, q"t")})""".stripMargin

          case TypeId.Builtins.opt =>
            q"""$BaboonTools.ReadValue($ref, t => ${mkDecoder(args.head, domain, q"t")})"""

          case TypeId.Builtins.map =>
            val keyRef = args.head
            val valueRef = args.last
            val keyDec = decodeKey(args.head, domain, q"kv.Name")
            val valueDec = mkDecoder(args.last, domain, q"kv.Value")
            q"""$ref!.Value<$nsJObject>()!.Properties().Select(kv => new $csKeyValuePair<${trans.asCsRef(keyRef, domain)}, ${trans.asCsRef(valueRef, domain)}>($keyDec, $valueDec)).ToImmutableDictionary()"""

          case TypeId.Builtins.lst =>
            q"""$ref!.Value<$nsJArray>()!.Select(e => ${mkDecoder(args.head, domain, q"e")}).ToImmutableList()"""

          case TypeId.Builtins.set =>
            q"""$ref!.Value<$nsJArray>()!.Select(e => ${mkDecoder(args.head, domain, q"e")}).ToImmutableHashSet()"""

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }

  }

  def codecName(name: CSValue.CSType): CSValue.CSType = {
    CSValue.CSType(name.pkg, s"${name.name}_JsonCodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User,
                         name: CSValue.CSType): CSCodecTranslator.CodecMeta = {
    val member =
      q"""public IBaboonJsonCodec<$name> Codec_JSON()
         |{
         |    return ${codecName(name)}.Instance;
         |}""".stripMargin
    CodecMeta(member)
  }

  def metaField(): TextTree[CSValue] = q"IBaboonCodecAbstract json";
}
