package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSCodecTranslator {
  def translate(defn: DomainMember.User,
                name: CSValue.CSType,
                version: Version): TextTree[CSValue]
}

class CSNSJsonCodecGenerator(trans: CSTypeTranslator)
    extends CSCodecTranslator {

  private def mkEncoder(tpe: TypeRef,
                        version: Version,
                        ref: TextTree[CSValue]): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case _: TypeId.BuiltinScalar => q"new ${nsJValue}($ref)"
          case u: TypeId.User =>
            val targetTpe = trans.toCsVal(u, version)
            q"""${targetTpe}_JsonCodec.Instance.Encode($ref)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"$ref == null ? $nsJValue.CreateNull() : ${mkEncoder(c.args.head, version, ref)}"
          case TypeId.Builtins.map =>
            q"$nsJValue.CreateNull()"
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

  override def translate(defn: DomainMember.User,
                         name: CSValue.CSType,
                         version: Version,
  ): TextTree[CSValue] = {

    /*
bs match {
case bit  =>
case i08  =>
case i16  =>
case i32  =>
case i64  =>
case u08  =>
case u16  =>
case u32  =>
case u64  =>
case f32  =>
case f64  =>
case f128 =>
case str  =>
case tsu  =>
case tso  =>
}
    case map =>
    case opt =>
    case lst =>
    case set =>

     */
    val (enc, dec) = defn.defn match {
      case d: Typedef.Dto =>
        val branches = d.fields.map { f =>
          val fieldRef = q"instance.${f.name.name.capitalize}"
          val enc = mkEncoder(f.tpe, version, fieldRef)
          (
            q"""new $nsJProperty("${f.name.name}", $enc)""",
            q"throw new NotImplementedException();"
          )
        }

        (q"""return new $nsJObject(
             |${branches.map(_._1).join(",\n").shift(4)}
             |);""".stripMargin, q"throw new NotImplementedException();")

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
             |    throw new ${csArgumentException}($$"Cannot convert {wire} to ${name.name}: string expected");
             |}
             |
             |${branches.join("\n")}
             |
             |throw new ${csArgumentException}($$"Cannot convert {wire} to ${name.name}: no matching value");""".stripMargin
        )

      case a: Typedef.Adt =>
        val branches = a.members.toList.map { m =>
          val branchNs = q"${trans.adtNsName(a.id)}"
          val branchName = m.name.name
          val fqBranch = q"$branchNs.$branchName"

          (q"""if (instance is $fqBranch)
             |{
             |    return new ${nsJObject}(new ${nsJProperty}("${m.name.name}"), ${fqBranch}_JsonCodec.Instance.Encode(($fqBranch)instance));
             |}""".stripMargin, q"""if (head.Name == "B1")
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
             |    throw new ArgumentException($$"Cannot convert {wire} to ${name.name}: object expected");
             |}
             |var head = asObject.Properties().First();
             |
             |${branches.map(_._2).join("\n")}
             |
             |throw new ArgumentException($$"Cannot convert {wire} to ${name.name}: no matching value");
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

  private def codecName(name: CSValue.CSType) = {
    CSValue.CSType(name.pkg, s"${name.name}_JsonCodec", name.fq)
  }
}
