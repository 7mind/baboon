package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, BaboonLang, Domain, DomainMember, Typedef}
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait JvCodecTestsTranslator {
  def translate(
    definition: DomainMember.User,
    jvRef: JvValue.JvType,
    srcRef: JvValue.JvType,
  ): Option[TextTree[JvValue]]
}

object JvCodecTestsTranslator {
  final class Impl(
    codecs: Set[JvCodecTranslator],
    typeTranslator: JvTypeTranslator,
    logger: BLogger,
    enquiries: BaboonEnquiries,
    target: JvTarget,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends JvCodecTestsTranslator {
    override def translate(
      definition: DomainMember.User,
      jvRef: JvValue.JvType,
      srcRef: JvValue.JvType,
    ): Option[TextTree[JvValue]] = {
      val isLatestVersion = domain.version == evo.latest

      definition match {
        case d if enquiries.hasForeignType(d, domain, BaboonLang.Java) => None
        case d if enquiries.isRecursiveTypedef(d, domain)     => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _ if !isLatestVersion                            => None
        case _ =>
          val testClass =
            q"""public class ${srcRef.name}_tests {
               |  ${makeTest(definition, srcRef)}
               |}
               |""".stripMargin
          Some(testClass)
      }
    }

    private def makeTest(definition: DomainMember.User, srcRef: JvValue.JvType): TextTree[JvValue] = {
      val fixture = makeFixture(definition, domain, evo)
      codecs
        .filter(_.isActive(definition.id)).map {
          case jsonCodec: JvJsonCodecGenerator =>
            val body      = jsonCodecAssertions(definition)
            val codecName = jsonCodec.codecName(srcRef, definition.defn.id.owner)

            q"""
             |@$jvTestAnnotation
             |public void ${srcRef.name}_should_support_JSON_codec() throws Exception {
             |  for (int i = 0; i < ${target.generic.codecTestIterations.toString}; i++) {
             |    jsonCodecTestImpl($baboonCodecContext.Default, $baboonRandomFactory.defaultFactory(), "default");
             |  }
             |}
             |
             |private void jsonCodecTestImpl($baboonCodecContext context, $baboonRandom rnd, String clue) throws Exception {
             |  ${fixture.shift(2).trim}
             |  ${body.shift(2).trim}
             |}
             |
             |private $jsonNode jsonCompare($baboonCodecContext context, $srcRef fixture, String clue) throws Exception {
             |  var codec = $codecName.INSTANCE;
             |  $jsonNode fixtureJson = codec.encode(context, fixture);
             |  $srcRef fixtureDecoded = codec.decode(context, fixtureJson);
             |  $jvAssertions.assertEquals(fixture, fixtureDecoded, clue);
             |  return fixtureJson;
             |}
             |"""

          case uebaCodec: JvUEBACodecGenerator =>
            val body      = uebaCodecAssertions(definition)
            val codecName = uebaCodec.codecName(srcRef, definition.defn.id.owner)
            q"""
             |@$jvTestAnnotation
             |public void ${srcRef.name}_should_support_UEBA_codec() throws Exception {
             |  for (int i = 0; i < ${target.generic.codecTestIterations.toString}; i++) {
             |    uebaCodecTestImpl($baboonCodecContext.Default, $baboonRandomFactory.defaultFactory(), "default");
             |  }
             |}
             |
             |private void uebaCodecTestImpl($baboonCodecContext context, $baboonRandom rnd, String clue) throws Exception {
             |  ${fixture.shift(2).trim}
             |  ${body.shift(2).trim}
             |}
             |
             |private byte[] uebaCompare($baboonCodecContext context, $srcRef fixture, String clue) throws Exception {
             |  var codec = $codecName.INSTANCE;
             |  var baos = new $byteArrayOutputStream();
             |  var dos = new $binaryOutput(baos);
             |  codec.encode(context, dos, fixture);
             |
             |  byte[] bytes = baos.toByteArray();
             |
             |  var bais = new $byteArrayInputStream(bytes);
             |  var dis = new $binaryInput(bais);
             |  $srcRef dec = codec.decode(context, dis);
             |  $jvAssertions.assertEquals(fixture, dec, clue);
             |
             |  return bytes;
             |}
             |"""

          case unknown =>
            logger.message(s"Cannot create codec tests (${unknown.codecName(srcRef, definition.defn.id.owner)}) for unsupported type $srcRef")
            q""
        }.toList.map(_.stripMargin.trim).joinNN().shift(2).trim
    }

    private def makeFixture(
      definition: DomainMember.User,
      domain: Domain,
      evolution: BaboonEvolution,
    ): TextTree[JvValue] = {
      definition.defn match {
        case e: Typedef.Enum =>
          q"var fixture = ${e.id.name.name}.all().get(rnd.nextInt(${e.id.name.name}.all().size()));"
        case _: Typedef.Adt =>
          q"var fixtures = ${typeTranslator.asJvType(definition.id, domain, evolution)}_Fixture.randomAll(rnd);"
        case _ =>
          q"var fixture = ${typeTranslator.asJvType(definition.id, domain, evolution)}_Fixture.random(rnd);"
      }
    }

    private def jsonCodecAssertions(definition: DomainMember.User): TextTree[JvValue] = {
      definition.defn match {
        case _: Typedef.Adt =>
          q"""for (var fixture : fixtures) {
             |  jsonCompare(context, fixture, clue);
             |}
             |""".stripMargin.trim
        case _ => q"jsonCompare(context, fixture, clue);"
      }
    }

    private def uebaCodecAssertions(definition: DomainMember.User): TextTree[JvValue] = {
      definition.defn match {
        case _: Typedef.Adt =>
          q"""for (var fixture : fixtures) {
             |  uebaCompare(context, fixture, clue);
             |}
             |""".stripMargin.trim
        case _ => q"uebaCompare(context, fixture, clue);"
      }
    }
  }
}
