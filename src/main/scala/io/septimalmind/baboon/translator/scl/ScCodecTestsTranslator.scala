package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember, Typedef}
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait ScCodecTestsTranslator {
  def translate(
    definition: DomainMember.User,
    csRef: ScValue.ScType,
    srcRef: ScValue.ScType,
  ): Option[TextTree[ScValue]]
}

object ScCodecTestsTranslator {
  final class Impl(
    codecs: Set[ScCodecTranslator],
    typeTranslator: ScTypeTranslator,
    logger: BLogger,
    enquiries: BaboonEnquiries,
    target: ScTarget,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends ScCodecTestsTranslator {
    override def translate(
      definition: DomainMember.User,
      csRef: ScValue.ScType,
      srcRef: ScValue.ScType,
    ): Option[TextTree[ScValue]] = {
      val isLatestVersion = domain.version == evo.latest

      definition match {
        case d if enquiries.hasForeignType(d, domain)         => None
        case d if enquiries.isRecursiveTypedef(d, domain)     => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _ if !isLatestVersion                            => None
        case _ =>
          val testClass =
            q"""class ${srcRef.name}_tests extends $anyFlatSpec {
               |  ${makeTest(definition, srcRef)}
               |}
               |""".stripMargin
          Some(testClass)
      }
    }

    private def makeTest(definition: DomainMember.User, srcRef: ScValue.ScType): TextTree[ScValue] = {
      val fixture = makeFixture(definition, domain, evo)
      codecs.map {
        case jsonCodec: ScJsonCodecGenerator =>
          val body = jsonCodecAssertions(jsonCodec, definition, srcRef)
          q"""
             |"${srcRef.toString}" should "support JSON codec" in {
             |  (0 to ${target.generic.codecTestIterations.toString}).foreach {
             |    _ =>
             |    jsonCodecTestImpl($baboonCodecContext.Default, $baboonRandom.default())  
             |  }
             |}
             |
             |def jsonCodecTestImpl(context: $baboonCodecContext, rnd: $baboonRandom): $scUnit = {
             |  ${fixture.shift(2).trim}
             |  ${body.shift(2).trim}
             |}
             |""".stripMargin.trim

        case uebaCodec: ScUEBACodecGenerator =>
          val body = uebaCodecAssertions(uebaCodec, definition, srcRef)
          q"""
             |"${srcRef.toString}" should "support UEBA codec" in {
             |  (0 to ${target.generic.codecTestIterations.toString}).foreach {
             |    _ =>
             |    uebaCodecTestImpl($baboonCodecContext.Default, $baboonRandom.default())  
             |  }
             |}
             |
             |def uebaCodecTestImpl(context: $baboonCodecContext, rnd: $baboonRandom): $scUnit = {
             |  ${fixture.shift(2).trim}
             |  ${body.shift(2).trim}
             |}
             |""".stripMargin

        case unknown =>
          logger.message(s"Cannot create codec tests (${unknown.codecName(srcRef)}) for unsupported type $srcRef")
          q""
      }.toList.join("\n").shift(2).trim
    }

    private def makeFixture(
      definition: DomainMember.User,
      domain: Domain,
      evolution: BaboonEvolution,
    ): TextTree[ScValue] = {
      definition.defn match {
        case e: Typedef.Enum => q"val fixture = rnd.randomElement(${e.id.name.name}.all)"
        case _: Typedef.Adt  => q"val fixtures = ${typeTranslator.asScType(definition.id, domain, evolution)}_Fixture.randomAll(rnd)"
        case _               => q"val fixture = ${typeTranslator.asScType(definition.id, domain, evolution)}_Fixture.random(rnd)"
      }
    }

    private def jsonCodecAssertions(codec: ScJsonCodecGenerator, definition: DomainMember.User, srcRef: ScValue.ScType): TextTree[ScValue] = {
      def jsonTest: TextTree[ScValue.ScType] = {
        val codecName  = codec.codecName(srcRef)
        val fieldName  = q"fixture"
        val serialized = q"${fieldName}Json"
        val decoded    = q"${fieldName}Decoded"
        q"""val $serialized = $codecName.instance.encode(context, $fieldName)
           |val $decoded = $codecName.instance.decode(context, $serialized).toOption.get
           |assert($fieldName == $decoded)
           |""".stripMargin.trim
      }

      definition.defn match {
        case _: Typedef.Adt =>
          q"""fixtures.foreach {
             |  fixture =>
             |    ${jsonTest.shift(4).trim}
             |}
             |""".stripMargin.trim
        case _ =>
          jsonTest
      }
    }

    private def uebaCodecAssertions(codec: ScUEBACodecGenerator, definition: DomainMember.User, srcRef: ScValue.ScType): TextTree[ScValue] = {
      def binaryTest: TextTree[ScValue.ScType] = {
        val codecName  = codec.codecName(srcRef)
        val fieldName  = q"fixture"
        val serialized = q"${fieldName}Bytes"
        val decoded    = q"${fieldName}Decoded"

        q"""val baos = new java.io.ByteArrayOutputStream()
           |val dos = new java.io.DataOutputStream(baos)
           |$codecName.instance.encode(context, dos, $fieldName);
           |val $serialized = baos.toByteArray
           |val bais = new java.io.ByteArrayInputStream($serialized)
           |val dis = new java.io.DataInputStream(bais)
           |val $decoded = $codecName.instance.decode(context, dis)
           |assert($fieldName == $decoded)
           |""".stripMargin.trim
      }

      definition.defn match {
        case _: Typedef.Adt =>
          q"""fixtures.foreach {
             |  fixture =>
             |    ${binaryTest.shift(4).trim}
             |}
             |""".stripMargin.trim
        case _ =>
          binaryTest
      }
    }
  }
}
