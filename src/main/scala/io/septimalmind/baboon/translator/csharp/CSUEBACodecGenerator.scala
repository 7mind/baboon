package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSUEBACodecGenerator(trans: CSTypeTranslator) extends CSCodecTranslator {

  override def translate(defn: DomainMember.User,
                         name: CSValue.CSType,
                         version: Version): TextTree[CSValue] = {
    val (enc, dec) = defn.defn match {
      case d: Typedef.Dto =>
        val branches = d.fields.map { f =>
          val fieldRef = q"instance.${f.name.name.capitalize}"
          val enc = mkEncoder(f.tpe, version, fieldRef)
//          val dec = mkDecoder(
//            f.tpe,
//            version,
//            q"""asObject["${f.name.name}"]""",
//            removeNull = true
//          )
//          (
//            q"""new $nsJProperty("${f.name.name}", $enc)""",
//            q"${f.name.name.capitalize}: $dec",
//          )
          (enc, q"""null""")
        }

        val fenc =
          q"""${branches
               .map(_._1)
               .join(";\n")};""".stripMargin

//        val fdec =
//          q"""return new $name(
//             |${branches.map(_._2).join(",\n").shift(4)}
//             |);
//               """.stripMargin
        val fdec = q"throw new $csNotImplementedException();"
        (fenc, fdec)

      case e: Typedef.Enum =>
        val branches = e.members.zipWithIndex.toList.map {
          case (m, idx) =>
            (q"""if (instance == ${name}.${m.name})
             |{
             |   writer.Write((byte)${idx.toString});
             |   return;
             |}""".stripMargin, q"""if (asByte == ${idx.toString})
                 |{
                 |   return ${name}.${m.name};
                 |}""".stripMargin)
        }

        (
          q"""${branches.map(_._1).join("\n")}
             |
             |throw new ${csArgumentException}($$"Cannot encode {instance} to ${name.name}: no matching value");""".stripMargin,
          q"""byte asByte = wire.ReadByte();
             |
             |${branches.map(_._2).join("\n")}
             |
             |throw new ${csArgumentException}($$"Cannot decode {wire} to ${name.name}: no matching value");""".stripMargin,
        )
      case a: Typedef.Adt =>
        val branches = a.members.zipWithIndex.toList.map {
          case (m, idx) =>
            val branchNs = q"${trans.adtNsName(a.id)}"
            val branchName = m.name.name
            val fqBranch = q"$branchNs.$branchName"
            val cName = codecName(trans.toCsVal(m, version))

            (q"""if (instance is $fqBranch)
                 |{
                 |   writer.Write((byte)${idx.toString});
                 |   return;
                 |}""".stripMargin, q"""if (asByte == ${idx.toString})
                 |{
                 |   return $cName.Instance.Decode(wire);
                 |}""".stripMargin)
        }

        (
          q"""${branches.map(_._1).join("\n")}
             |
             |throw new ${csArgumentException}($$"Cannot encode {instance} to ${name.name}: no matching value");""".stripMargin,
          q"""byte asByte = wire.ReadByte();
             |
             |${branches.map(_._2).join("\n")}
             |
             |throw new ${csArgumentException}($$"Cannot decode {wire} to ${name.name}: no matching value");""".stripMargin,
        )

    }
    val cName = codecName(name)
    q"""public class $cName : $iBaboonBinCodec<$name>
       |{
       |    public void Encode($binaryWriter writer, $name instance)
       |    {
       |        ${enc.shift(8).trim}
       |    }
       |    public $name Decode($binaryReader wire) {
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
    CSValue.CSType(name.pkg, s"${name.name}_UEBACodec", name.fq)
  }

  private def deNull(tpe: TypeRef,
                     ref: TextTree[CSValue]): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.str =>
                ref
              case _ =>
                q"$ref.Value"
            }
          case _ =>
            q"$ref!"
        }
      case _ =>
        q"$ref!"
    }
  }

  private def mkEncoder(tpe: TypeRef,
                        version: Version,
                        ref: TextTree[CSValue]): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit =>
                q"writer.Write($ref)"
              case TypeId.Builtins.i08 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.i16 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.i32 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.i64 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.u08 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.u16 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.u32 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.u64 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.f32 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.f64 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.f128 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.str =>
                q"writer.Write($ref)"
              case TypeId.Builtins.tsu =>
                q"writer.Write($ref.ToString())"
              case TypeId.Builtins.tso =>
                q"writer.Write($ref.ToString())"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toCsVal(u, version))
            q"""${targetTpe}.Instance.Encode(writer, $ref)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""if ($ref == null)
               |{
               |    writer.Write((byte)0);
               |} else
               |{
               |   writer.Write((byte)1);
               |   ${mkEncoder(c.args.head, version, deNull(c.args.head, ref))
                 .shift(4)
                 .trim};
               |}""".stripMargin
          case TypeId.Builtins.map =>
            q"""writer.Write($ref.Count());
               |foreach (var kv in $ref)
               |{
               |    ${mkEncoder(c.args.head, version, q"kv.Key").shift(4).trim};
               |    ${mkEncoder(c.args.last, version, q"kv.Value")
                 .shift(4)
                 .trim};
               |}""".stripMargin
          case TypeId.Builtins.lst =>
            q"""writer.Write($ref.Count());
               |foreach (var i in $ref)
               |{
               |    ${mkEncoder(c.args.head, version, q"i").shift(4).trim};
               |}""".stripMargin
          case TypeId.Builtins.set =>
            q"""writer.Write($ref.Count());
               |foreach (var i in $ref)
               |{
               |    ${mkEncoder(c.args.head, version, q"i").shift(4).trim};
               |}""".stripMargin
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }
}
