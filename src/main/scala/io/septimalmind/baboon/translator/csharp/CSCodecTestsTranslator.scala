package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSCodecTestsTranslator {
  def translate(definition: DomainMember.User,
                csRef: CSValue.CSType,
                srcRef: CSValue.CSType,
                domain: Domain,
                evo: BaboonEvolution): Option[TextTree[CSValue]]
}

object CSCodecTestsTranslator {
  final class Impl(codecs: Set[CSCodecTranslator],
                   typeTranslator: CSTypeTranslator,
                   logger: BLogger,
                   enquiries: BaboonEnquiries,
                   compilerOptions: CompilerOptions)
      extends CSCodecTestsTranslator {
    override def translate(definition: DomainMember.User,
                           csRef: CSValue.CSType,
                           srcRef: CSValue.CSType,
                           domain: Domain,
                           evo: BaboonEvolution,
    ): Option[TextTree[CSValue]] = {

      val codecTestName = definition.id.owner match {
        case Owner.Toplevel => srcRef.name
        case Owner.Adt(id)  => s"${id.name.name}__${srcRef.name}"
        case Owner.Ns(path) =>
          s"${path.map(_.name).mkString("_")}__${srcRef.name}"
      }

      val testClassName =
        CSValue
          .CSType(srcRef.pkg, s"${codecTestName}__Codec_Test", srcRef.fq)
          .asName

      definition match {
        case d if enquiries.hasForeignType(d, domain)         => None
        case d if enquiries.isRecursiveTypedef(d, domain)     => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _                                                =>
          //val init = fieldsInitialization(definition, srcRef, domain, evo)
          //${testFields(definition, srcRef, domain, evo).shift(2).trim}
          val testClass =
            q"""[$nunitTestFixture]
               |public class $testClassName
               |{
               |  #nullable disable
               |  ${tests(definition, srcRef, domain, evo)}
               |}
               |""".stripMargin
          Some(testClass)
      }
    }

//    private def testFields(definition: DomainMember.User,
//                           srcRef: CSValue.CSType,
//                           domain: Domain,
//                           evo: BaboonEvolution): TextTree[CSValue] = {
//      definition.defn match {
//        case adt: Typedef.Adt =>
//          adt
//            .dataMembers(domain)
//            .map(_.name.name)
//            .map(
//              n =>
//                q"private ${typeTranslator.asCsType(adt.id, domain, evo)} ${n.toLowerCase};"
//            )
//            .toList
//            .join("\n")
//            .shift(2)
//            .trim
//        case _ => q"private $srcRef ${srcRef.name.toLowerCase};"
//      }
//    }

    private def fieldsInitialization(
      definition: DomainMember.User,
      srcRef: CSValue.CSType,
      domain: Domain,
      evolution: BaboonEvolution
    ): TextTree[CSValue] = {
      definition.defn match {
        case adt: Typedef.Adt =>
          adt
            .dataMembers(domain)
            .map { member =>
              val tpe = typeTranslator
                .asCsType(adt.id, domain, evolution)
              if (compilerOptions.csUseCompactAdtForm) {
                q"$tpe fixture_${member.name.name.toLowerCase} = $tpe.${member.name.name}.Random();"
              } else {
                q"$tpe fixture_${member.name.name.toLowerCase} = $tpe.Random();"
              }
            }
            .toList
            .join("\n")
        case enum: Typedef.Enum =>
          q"var fixture = $testValuesGenerator.NextRandomEnum<${enum.id.name.name}>();"
        case _ =>
          q"var fixture = ${typeTranslator.asCsType(definition.id, domain, evolution)}.Random();"
      }
    }

    private def tests(definition: DomainMember.User,
                      srcRef: CSValue.CSType,
                      domain: Domain,
                      evo: BaboonEvolution,
    ): TextTree[CSValue] = {
      val init = fieldsInitialization(definition, srcRef, domain, evo)
      codecs
        .map {
          case jsonCodec: CSNSJsonCodecGenerator =>
            val body =
              jsonCodecAssertions(jsonCodec, definition, srcRef, domain, evo)

            q"""[Test]
               |public void jsonCodecTest()
               |{
               |    jsonCodecTestImpl($baboonCodecContext.Default);
               |}
               |
               |private void jsonCodecTestImpl($baboonCodecContext context) {
               |    ${init.shift(4).trim}
               |    ${body.shift(4).trim}
               |}
               |""".stripMargin
          case uebaCodec: CSUEBACodecGenerator =>
            val body =
              uebaCodecAssertions(uebaCodec, definition, srcRef, domain, evo)

            q"""[Test]
               |public void uebaCodecTestNoIndex()
               |{
               |    uebaCodecTestImpl($baboonCodecContext.Compact);
               |}
               |
               |[Test]
               |public void uebaCodecTestIndexed()
               |{
               |    uebaCodecTestImpl($baboonCodecContext.Indexed);
               |}
               |
               |private void uebaCodecTestImpl($baboonCodecContext context) {
               |    ${init.shift(4).trim}
               |    ${body.shift(4).trim}
               |}
               |""".stripMargin
          case unknown =>
            logger.message(
              s"Tests generating for ${unknown.codecName(srcRef)} codec of type $srcRef is not supported"
            )
            q""
        }
        .toList
        .join("\n")
        .shift(2)
        .trim
    }

    private def jsonCodecAssertions(codec: CSNSJsonCodecGenerator,
                                    definition: DomainMember.User,
                                    srcRef: CSValue.CSType,
                                    domain: Domain,
                                    evo: BaboonEvolution,
    ): TextTree[CSValue] = {
      definition.defn match {
        case adt: Typedef.Adt =>
          adt
            .dataMembers(domain)
            .map { member =>
              val typeRef =
                typeTranslator.toCsTypeRefNoDeref(adt.id, domain, evo)
              val codecName = codec.codecName(typeRef)
              val fieldName = q"fixture_${member.name.name.toLowerCase}"
              val serialized = q"${fieldName}_json"
              val decoded = q"${fieldName}_decoded"
              q"""var $serialized = $codecName.Instance.Encode(context, $fieldName);
                 |var $decoded = $codecName.Instance.Decode(context, $serialized);
                 |Assert.AreEqual($fieldName, $decoded);
                 |""".stripMargin
            }
            .toList
            .join("\n")
        case _ =>
          val codecName = codec.codecName(srcRef)
          val fieldName = q"fixture"
          val serialized = q"${fieldName}_json"
          val decoded = q"${fieldName}_decoded"
          q"""var $serialized = $codecName.Instance.Encode(context, $fieldName);
             |var $decoded = $codecName.Instance.Decode(context, $serialized);
             |Assert.AreEqual($fieldName, $decoded);
             |""".stripMargin
      }
    }

    private def uebaCodecAssertions(codec: CSUEBACodecGenerator,
                                    definition: DomainMember.User,
                                    srcRef: CSValue.CSType,
                                    domain: Domain,
                                    evo: BaboonEvolution,
    ): TextTree[CSValue] = {
      definition.defn match {
        case adt: Typedef.Adt =>
          adt
            .dataMembers(domain)
            .map { member =>
              val typeRef =
                typeTranslator.toCsTypeRefNoDeref(adt.id, domain, evo)
              val codecName = codec.codecName(typeRef)
              val fieldName = q"fixture_${member.name.name.toLowerCase}"
              val serialized = q"${fieldName}_bytes"
              val decoded = q"${fieldName}_decoded"
              val readStream = q"${fieldName}_readMemoryStream"
              val binaryReader = q"${fieldName}_binaryReader"
              q"""using ($memoryStream writeMemoryStream = new $memoryStream())
                 |{
                 |  using ($binaryWriter binaryWriter = new $binaryWriter(writeMemoryStream))
                 |  {
                 |    $codecName.Instance.Encode(context, binaryWriter, $fieldName);
                 |  }
                 |  writeMemoryStream.Flush();
                 |  var $serialized = writeMemoryStream.ToArray();
                 |  var $readStream = new MemoryStream($serialized);
                 |  var $binaryReader = new BinaryReader($readStream);
                 |  var $decoded = $codecName.Instance.Decode(context, $binaryReader);
                 |  Assert.AreEqual($fieldName, $decoded);
                 |}
                 |""".stripMargin
            }
            .toList
            .join("\n")

        case _ =>
          val codecName = codec.codecName(srcRef)
          val fieldName = q"fixture"
          val serialized = q"${fieldName}_bytes"
          val decoded = q"${fieldName}_decoded"
          q"""using ($memoryStream writeMemoryStream = new $memoryStream())
             |{
             |  using ($binaryWriter binaryWriter = new $binaryWriter(writeMemoryStream))
             |  {
             |    $codecName.Instance.Encode(context, binaryWriter, $fieldName);
             |  }
             |  writeMemoryStream.Flush();
             |
             |  var $serialized = writeMemoryStream.ToArray();
             |  var readMemoryStream = new MemoryStream($serialized);
             |  var binaryReader = new BinaryReader(readMemoryStream);
             |  var $decoded = $codecName.Instance.Decode(context, binaryReader);
             |  Assert.AreEqual($fieldName, $decoded);
             |}
             |""".stripMargin
      }
    }
  }
}
