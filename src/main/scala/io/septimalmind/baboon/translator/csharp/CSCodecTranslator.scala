package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.typer.model.{DomainMember, Typedef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*
import CSBaboonTranslator.*

trait CSCodecTranslator {
  def translate(defn: DomainMember.User,
                name: CSValue.CSType): TextTree[CSValue]
}

class CSNSJsonCodecGenerator extends CSCodecTranslator {
  override def translate(defn: DomainMember.User,
                         name: CSValue.CSType): TextTree[CSValue] = {

    val (enc, dec) = defn.defn match {
      case d: Typedef.Dto =>
        (
          q"throw new NotImplementedException();",
          q"throw new NotImplementedException();"
        )

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
             |    throw new ArgumentException($$"Cannot convert {wire} to ${name.name}: string expected");
             |}
             |
             |${branches.join("\n")}
             |
             |throw new ArgumentException($$"Cannot convert {wire} to ${name.name}: no matching value");""".stripMargin
        )

      case a: Typedef.Adt =>
        (
          q"throw new NotImplementedException();",
          q"throw new NotImplementedException();"
        )

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
       |}
     """.stripMargin

  }

  private def codecName(name: CSValue.CSType) = {
    CSValue.CSType(name.pkg, s"${name.name}_JsonCodec", name.fq)
  }
}
