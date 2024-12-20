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
        case Owner.Adt(id) => s"${id.name.name}__${srcRef.name}"
        case Owner.Ns(path) =>
          s"${path.map(_.name).mkString("_")}__${srcRef.name}"
      }

      val testClassName =
        CSValue
          .CSType(srcRef.pkg, s"${codecTestName}__Codec_Test", srcRef.fq)
          .asName

      definition match {
        case d if enquiries.hasForeignType(d, domain) => None
        case d if enquiries.isRecursiveTypedef(d, domain) => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _ =>
          val testClass =
            q"""// ReSharper disable InconsistentNaming
               |// ReSharper disable MemberHidesStaticFromOuterClass
               |#pragma warning disable CS0108
               |#nullable enable
               |
               |[$nunitTestFixture]
               |public class $testClassName
               |{
               |  ${tests(definition, srcRef, domain, evo)}
               |}
               |""".stripMargin
          Some(testClass)
      }
    }

    private def fieldsInitialization(
                                      definition: DomainMember.User,
                                      domain: Domain,
                                      evolution: BaboonEvolution
                                    ): TextTree[CSValue] = {
      definition.defn match {
        case enum: Typedef.Enum =>
          q"var fixture = $testValuesGenerator.NextRandomEnum<${enum.id.name.name}>();"
        case _: Typedef.Adt =>
          q"var fixtures = ${typeTranslator.asCsType(definition.id, domain, evolution)}_Fixture.RandomAll();"
        case _ =>
          q"var fixture = ${typeTranslator.asCsType(definition.id, domain, evolution)}_Fixture.Random();"
      }
    }

    private def tests(definition: DomainMember.User,
                      srcRef: CSValue.CSType,
                      domain: Domain,
                      evo: BaboonEvolution,
                     ): TextTree[CSValue] = {
      val init = fieldsInitialization(definition, domain, evo)
      codecs
        .map {
          case jsonCodec: CSNSJsonCodecGenerator =>
            val body =
              jsonCodecAssertions(jsonCodec, definition, srcRef, domain, evo)

            q"""[Test]
               |public void jsonCodecTest()
               |{
               |    for (int i = 0; i < 500; i++)
               |    {
               |        jsonCodecTestImpl($baboonCodecContext.Default);
               |    }
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
               |    for (int i = 0; i < 500; i++)
               |    {
               |        uebaCodecTestImpl($baboonCodecContext.Compact);
               |    }
               |}
               |
               |[Test]
               |public void uebaCodecTestIndexed()
               |{
               |    for (int i = 0; i < 500; i++)
               |    {
               |        uebaCodecTestImpl($baboonCodecContext.Indexed);
               |    }
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
      def jsonTest: TextTree[CSValue.CSType] = {
        val codecName = codec.codecName(srcRef)
        val fieldName = q"fixture"
        val serialized = q"${fieldName}Json"
        val decoded = q"${fieldName}Decoded"
        q"""var $serialized = $codecName.Instance.Encode(context, $fieldName);
           |var $decoded = $codecName.Instance.Decode(context, $serialized);
           |Assert.That($fieldName, Is.EqualTo($decoded));
           |""".stripMargin
      }

      definition.defn match {
        case adt: Typedef.Adt =>
          q"""foreach (var fixture in fixtures) {
             |    ${jsonTest.shift(4).trim}
             |}
             |""".stripMargin
        case _ =>
          jsonTest
      }
    }

    private def uebaCodecAssertions(codec: CSUEBACodecGenerator,
                                    definition: DomainMember.User,
                                    srcRef: CSValue.CSType,
                                    domain: Domain,
                                    evo: BaboonEvolution,
                                   ): TextTree[CSValue] = {
      def binaryTest: TextTree[CSValue.CSType] = {
        val codecName = codec.codecName(srcRef)
        val fieldName = q"fixture"
        val serialized = q"${fieldName}Bytes"
        val decoded = q"${fieldName}Decoded"
        q"""using ($memoryStream writeMemoryStream = new $memoryStream())
           |{
           |    using ($binaryWriter binaryWriter = new $binaryWriter(writeMemoryStream))
           |    {
           |        $codecName.Instance.Encode(context, binaryWriter, $fieldName);
           |    }
           |    writeMemoryStream.Flush();
           |
           |    var $serialized = writeMemoryStream.ToArray();
           |    var readMemoryStream = new MemoryStream($serialized);
           |    var binaryReader = new BinaryReader(readMemoryStream);
           |    var $decoded = $codecName.Instance.Decode(context, binaryReader);
           |    Assert.That($fieldName, Is.EqualTo($decoded));
           |}
           |""".stripMargin
      }

      definition.defn match {
        case _: Typedef.Adt =>
          q"""foreach (var fixture in fixtures) {
             |    ${binaryTest.shift(4).trim}
             |}
             |""".stripMargin
        case _ =>
          binaryTest
      }
    }
  }
}
