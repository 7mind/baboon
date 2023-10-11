package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.typer.model.{DomainMember, Typedef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*
import CSBaboonTranslator.*

trait CSCodecTranslator {
  def translate(defn: DomainMember.User,
                name: CSValue.CSType): TextTree[CSValue]
}

class CSNSJsonCodecGenerator(trans: CSTypeTranslator)
    extends CSCodecTranslator {
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
