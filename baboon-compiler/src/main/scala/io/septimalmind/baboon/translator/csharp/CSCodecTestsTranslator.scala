package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSTypeOrigin
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSCodecTestsTranslator {
  def translate(
    definition: DomainMember.User,
    csRef: CSValue.CSType,
    srcRef: CSValue.CSType,
  ): Option[TextTree[CSValue]]
}

object CSCodecTestsTranslator {
  final class Impl(
    codecs: Set[CSCodecTranslator],
    typeTranslator: CSTypeTranslator,
    logger: BLogger,
    enquiries: BaboonEnquiries,
    target: CSTarget,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends CSCodecTestsTranslator {
    override def translate(
      definition: DomainMember.User,
      csRef: CSValue.CSType,
      srcRef: CSValue.CSType,
    ): Option[TextTree[CSValue]] = {
      val isLatestVersion = domain.version == evo.latest

      definition match {
        case d if enquiries.hasForeignType(d, domain, BaboonLang.Cs)              => None
        case d if enquiries.isRecursiveTypedef(d, domain)                       => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef]                   => None
        case _ if !target.language.enableDeprecatedEncoders && !isLatestVersion => None
        case _ =>
          val testClass =
            q"""[$nunitTestFixture]
               |public class ${srcRef.name}_Tests
               |{
               |  ${makeTest(definition, srcRef)}
               |}
               |""".stripMargin
          Some(testClass)
      }
    }

    private def makeTest(definition: DomainMember.User, srcRef: CSValue.CSType): TextTree[CSValue] = {
      val fixture = makeFixture(definition, domain, evo)
      codecs
        .filter(_.isActive(definition.id)).map {
          case jsonCodec: CSJsonCodecGenerator =>
            val body = jsonCodecAssertions(jsonCodec, definition, srcRef)
            q"""[Test]
               |public void jsonCodecTest()
               |{
               |    for (var i = 0; i < ${target.generic.codecTestIterations.toString}; i++)
               |    {
               |        jsonCodecTestImpl($baboonCodecContext.Default, "default");
               |    }
               |}
               |
               |private void jsonCodecTestImpl($baboonCodecContext context, $csString clue) {
               |    ${fixture.shift(4).trim}
               |    ${body.shift(4).trim}
               |}
               |""".stripMargin

          case uebaCodec: CSUEBACodecGenerator =>
            val body = uebaCodecAssertions(uebaCodec, definition, srcRef)
            q"""[Test]
               |public void uebaCodecTestNoIndex()
               |{
               |    for (var i = 0; i < ${target.generic.codecTestIterations.toString}; i++)
               |    {
               |        uebaCodecTestImpl($baboonCodecContext.Compact, "compact");
               |    }
               |}
               |
               |[Test]
               |public void uebaCodecTestIndexed()
               |{
               |    for (var i = 0; i < ${target.generic.codecTestIterations.toString}; i++)
               |    {
               |        uebaCodecTestImpl($baboonCodecContext.Indexed, "indexed");
               |    }
               |}
               |
               |private void uebaCodecTestImpl($baboonCodecContext context, $csString clue) {
               |    ${fixture.shift(4).trim}
               |    ${body.shift(4).trim}
               |}
               |""".stripMargin

          case unknown =>
            logger.message(s"Cannot create codec tests (${unknown.id}) for unsupported type $srcRef")
            q""
        }.toList.join("\n").shift(2).trim
    }

    private def makeFixture(
      definition: DomainMember.User,
      domain: Domain,
      evolution: BaboonEvolution,
    ): TextTree[CSValue] = {
      definition.defn match {
        case e: Typedef.Enum => q"var fixture = $baboonFixture.NextRandomEnum<${e.id.name.name}>();"
        case _: Typedef.Adt  => q"var fixtures = ${typeTranslator.asCsType(definition.id, domain, evolution)}_Fixture.RandomAll();"
        case _               => q"var fixture = ${typeTranslator.asCsType(definition.id, domain, evolution)}_Fixture.Random();"
      }
    }

    private def jsonCodecAssertions(codec: CSJsonCodecGenerator, definition: DomainMember.User, srcRef: CSValue.CSType): TextTree[CSValue] = {
      def jsonTest: TextTree[CSValue.CSType] = {
        val codecName  = codec.codecName(srcRef, CSTypeOrigin(definition.id, domain))
        val fieldName  = q"fixture"
        val serialized = q"${fieldName}Json"
        val decoded    = q"${fieldName}Decoded"

        q"""var $serialized = $codecName.Instance.Encode(context, $fieldName);
           |var $decoded = $codecName.Instance.Decode(context, $serialized);
           |Assert.That($fieldName, Is.EqualTo($decoded));
           |$BaboonTestTools.WriteBinaryFile($$"./../../../../../target/cs/json-{clue}/${definition.id.render}.json", $csEncoding.UTF8.GetBytes($nsJsonConvert.SerializeObject($serialized)));
           |""".stripMargin.trim
      }

      definition.defn match {
        case _: Typedef.Adt =>
          q"""foreach (var fixture in fixtures) {
             |    ${jsonTest.shift(4).trim}
             |}
             |""".stripMargin
        case _ =>
          jsonTest
      }
    }

    private def uebaCodecAssertions(codec: CSUEBACodecGenerator, definition: DomainMember.User, srcRef: CSValue.CSType): TextTree[CSValue] = {
      def binaryTest: TextTree[CSValue.CSType] = {
        val codecName  = codec.codecName(srcRef, CSTypeOrigin(definition.id, domain))
        val fieldName  = q"fixture"
        val serialized = q"${fieldName}Bytes"
        val decoded    = q"${fieldName}Decoded"

        q"""using (var writeMemoryStream = new $memoryStream())
           |{
           |    using (var binaryWriter = new $binaryWriter(writeMemoryStream))
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
           |    
           |    $BaboonTestTools.WriteBinaryFile($$"./../../../../../target/cs/ueba-{clue}/${definition.id.render}.uebin", $serialized);
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
