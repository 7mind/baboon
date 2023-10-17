package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSNSJsonCodecGenerator(trans: CSTypeTranslator)
    extends CSCodecTranslator {
  override def translate(defn: DomainMember.User,
                         name: CSValue.CSType,
                         version: Version,
  ): TextTree[CSValue] = {

    val (enc, dec) = defn.defn match {
      case d: Typedef.Dto =>
        val fields = d.fields.map { f =>
          val fieldRef = q"instance.${f.name.name.capitalize}"
          val enc = mkEncoder(f.tpe, version, fieldRef)
          val dec = mkDecoder(
            f.tpe,
            version,
            q"""asObject["${f.name.name}"]""",
            removeNull = true
          )
          (
            q"""new $nsJProperty("${f.name.name}", $enc)""",
            q"${f.name.name.capitalize}: $dec",
          )
        }

        (q"""return new $nsJObject(
            |${fields.map(_._1).join(",\n").shift(4)}
            |);""".stripMargin, q"""var asObject = wire.Value<JObject>();
                                   |
                                   |if (asObject == null)
                                   |{
                                   |    throw new ArgumentException($$"Cannot decode {wire} to ${name.name}: object expected");
                                   |}
                                   |
                                   |return new $name(
                                   |${fields.map(_._2).join(",\n").shift(4)}
                                   |);
           """.stripMargin)

      case e: Typedef.Enum =>
        val branches = e.members.toList.map { m =>
          q"""if (asStr == ${name}.${m.name}.ToString().ToLower())
             |{
             |   return ${name}.${m.name};
             |}""".stripMargin
        }

        (
          q"return $nsJValue.CreateString(instance.ToString());",
          q"""var asStr = wire.Value<String>()?.ToLower();
             |if (asStr == null)
             |{
             |    throw new ${csArgumentException}($$"Cannot decode {wire} to ${name.name}: string expected");
             |}
             |
             |${branches.join("\n")}
             |
             |throw new ${csArgumentException}($$"Cannot decode {wire} to ${name.name}: no matching value");""".stripMargin
        )

      case a: Typedef.Adt =>
        val branches = a.members.toList.map { m =>
          val branchNs = q"${trans.adtNsName(a.id)}"
          val branchName = m.name.name
          val fqBranch = q"$branchNs.$branchName"

          (q"""if (instance is $fqBranch)
              |{
              |    return new ${nsJObject}(new ${nsJProperty}("$branchName"), ${fqBranch}_JsonCodec.Instance.Encode(($fqBranch)instance));
              |}""".stripMargin, q"""if (head.Name == "$branchName")
                                    |{
                                    |    return ${fqBranch}_JsonCodec.Instance.Decode(head.Value);
                                    |}""".stripMargin)

        }

        (q"""${branches.map(_._1).join("\n")}
            |
            |throw new ${csArgumentException}($$"Cannot encode {instance}: unexpected subclass");
           """.stripMargin, q"""var asObject = wire.Value<JObject>();
                               |if (asObject == null)
                               |{
                               |    throw new ArgumentException($$"Cannot decode {wire} to ${name.name}: object expected");
                               |}
                               |var head = asObject.Properties().First();
                               |
                               |${branches.map(_._2).join("\n")}
                               |
                               |throw new ArgumentException($$"Cannot decode {wire} to ${name.name}: no matching value");
           """.stripMargin)

    }

    val cName = codecName(name)
    q"""public class $cName : $iBaboonJsonCodec<$name>
       |{
       |    public $nsJToken Encode($name instance)
       |    {
       |        ${enc.shift(8).trim}
       |    }
       |
       |    public $name Decode($nsJToken wire)
       |    {
       |        ${dec.shift(8).trim}
       |    }
       |
       |    private static $csLazy<$cName> instance = new $csLazy<$cName>(() => new $cName());
       |
       |    public static $cName Instance { get { return instance.Value; } }
       |}
     """.stripMargin

  }

  private def mkEncoder(tpe: TypeRef,
                        version: Version,
                        ref: TextTree[CSValue]): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case _: TypeId.BuiltinScalar => q"new ${nsJValue}($ref)"
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toCsVal(u, version))
            q"""${targetTpe}.Instance.Encode($ref)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"$ref == null ? $nsJValue.CreateNull() : ${mkEncoder(c.args.head, version, ref)}"
          case TypeId.Builtins.map =>
            q"new $nsJObject($ref.Select(e => new $nsJProperty(e.Key.ToString(), ${mkEncoder(c.args.last, version, q"e.Value")})))"
          case TypeId.Builtins.lst =>
            q"new $nsJArray($ref.Select(e => ${mkEncoder(c.args.head, version, q"e")}))"
          case TypeId.Builtins.set =>
            q"new $nsJArray($ref.Select(e => ${mkEncoder(c.args.head, version, q"e")}))"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def mkDecoder(tpe: TypeRef,
                        version: Version,
                        ref: TextTree[CSValue],
                        removeNull: Boolean,
  ): TextTree[CSValue] = {

    def mkReader(bs: TypeId.BuiltinScalar): TextTree[CSValue] = {
      val fref = if (removeNull) {
        q"$ref!"
      } else {
        q"$ref?"
      }

      val out = bs match {
        case TypeId.Builtins.bit =>
          q"$fref.Value<Boolean>()"
        case TypeId.Builtins.i08 =>
          q"$fref.Value<SByte>()"
        case TypeId.Builtins.i16 =>
          q"$fref.Value<Int16>()"
        case TypeId.Builtins.i32 =>
          q"$fref.Value<Int32>()"
        case TypeId.Builtins.i64 =>
          q"$fref.Value<Int64>()"
        case TypeId.Builtins.u08 =>
          q"$fref.Value<Byte>()"
        case TypeId.Builtins.u16 =>
          q"$fref.Value<UInt16>()"
        case TypeId.Builtins.u32 =>
          q"$fref.Value<UInt32>()"
        case TypeId.Builtins.u64 =>
          q"$fref.Value<UInt64>()"
        case TypeId.Builtins.f32 =>
          q"$fref.Value<Single>()"
        case TypeId.Builtins.f64 =>
          q"$fref.Value<Double>()"
        case TypeId.Builtins.f128 =>
          q"$fref.Value<Decimal>()"
        case TypeId.Builtins.str =>
          q"$fref.Value<String>()"
        case TypeId.Builtins.tsu =>
          q"$fref.Value<DateTime>()"
        case TypeId.Builtins.tso =>
          q"$fref.Value<DateTime>()"
        case o =>
          throw new RuntimeException(s"BUG: Unexpected type: $o")
      }

      if (removeNull) {
        q"$out!"
      } else {
        out
      }
    }

    def decodeKey(tpe: TypeRef, ref: TextTree[CSValue]): TextTree[CSValue] = {
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
        case TypeId.Builtins.tsu =>
          q"DateTime.Parse($ref)"
        case TypeId.Builtins.tso =>
          q"DateTime.Parse($ref)"
        case o =>
          throw new RuntimeException(s"BUG: Unexpected type: $o")
      }
    }

    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case bs: TypeId.BuiltinScalar =>
            mkReader(bs)
          case u: TypeId.User =>
            val targetTpe = trans.toCsVal(u, version)
            q"""${targetTpe}_JsonCodec.Instance.Decode($ref!)"""
        }
      case TypeRef.Constructor(id, args) =>
        id match {
          case TypeId.Builtins.opt =>
            q"""$ref!.Type == $nsJTokenType.Null ? null : ${mkDecoder(
              args.head,
              version,
              ref,
              removeNull = false
            )}"""

          case TypeId.Builtins.map =>
            q"""$ref!.Value<$nsJObject>()!.Properties().Select(kv => $csKeyValuePair.Create(${decodeKey(
              args.head,
              q"kv.Name"
            )}, ${mkDecoder(args.last, version, q"kv.Value", removeNull = true)})).ToImmutableDictionary()"""

          case TypeId.Builtins.lst =>
            q"""$ref!.Value<$nsJArray>()!.Select(e => ${mkDecoder(
              args.head,
              version,
              q"e",
              removeNull = true
            )}).ToImmutableList()"""
          case TypeId.Builtins.set =>
            q"""$ref!.Value<$nsJArray>()!.Select(e => ${mkDecoder(
              args.head,
              version,
              q"e",
              removeNull = true
            )}).ToImmutableHashSet()"""
          case o =>
//            q"null"
            throw new RuntimeException(s"BUG: Unexpected type: $o")

        }
    }

  }

  private def codecName(name: CSValue.CSType) = {
    CSValue.CSType(name.pkg, s"${name.name}_JsonCodec", name.fq)
  }
}
