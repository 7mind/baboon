package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSBinaryBaboonCodecGenerator(trans: CSTypeTranslator)
    extends CSCodecTranslator {

  override def translate(defn: DomainMember.User,
                         name: CSValue.CSType,
                         version: Version): TextTree[CSValue] = {
    val (enc, dec) = defn.defn match {
      case d: Typedef.Dto =>
//        val branches = d.fields.map { f =>
//          val fieldRef = q"instance.${f.name.name.capitalize}"
//          val enc = mkEncoder(f.tpe, version, fieldRef)
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
//        }
//
//        (
//          q"""return new $nsJObject(
//             |${branches.map(_._1).join(",\n").shift(4)}
//             |);""".stripMargin,
//          q"""var asObject = wire.Value<JObject>();
//             |
//             |if (asObject == null)
//             |{
//             |    throw new ArgumentException($$"Cannot decode {wire} to ${name.name}: object expected");
//             |}
//             |
//             |return new $name(
//             |${branches.map(_._2).join(",\n").shift(4)}
//             |);
//       """.stripMargin)

        (
          q"throw new $csNotImplementedException();",
          q"throw new $csNotImplementedException();"
        )
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
    CSValue.CSType(name.pkg, s"${name.name}_BBCodec", name.fq)
  }

}
