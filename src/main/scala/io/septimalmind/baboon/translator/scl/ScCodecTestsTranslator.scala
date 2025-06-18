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
          val body      = jsonCodecAssertions(definition, srcRef)
          val codecName = jsonCodec.codecName(srcRef)

          q"""
             |"${srcRef.toString}" should "support JSON codec" in {
             |  (0 to ${target.generic.codecTestIterations.toString}).foreach {
             |    _ =>
             |    jsonCodecTestImpl($baboonCodecContext.Default, $baboonRandom.default(), "default")  
             |  }
             |}
             |
             |"${srcRef.toString}" should "load JSON produced by C# codecs" in {
             |  testCsJson($baboonCodecContext.Default, "default")
             |}
             |
             |def jsonCodecTestImpl(context: $baboonCodecContext, rnd: $baboonRandom, clue: $scString): $scUnit = {
             |  ${fixture.shift(2).trim}
             |  ${body.shift(2).trim}
             |}
             |
             |def jsonCompare(context: $baboonCodecContext, fixture: $srcRef): $scUnit = {
             |  val fixtureJson    = $codecName.instance.encode(context, fixture)
             |  val fixtureDecoded = $codecName.instance.decode(context, fixtureJson).toOption.get
             |  assert(fixture == fixtureDecoded)
             |}
             |
             |def testCsJson(context: $baboonCodecContext, clue: $scString): $scUnit = {
             |  val tpeid = "${definition.id.render}"
             |  val f     = new $javaFile(s"./../target/cs/json-$$clue/$$tpeid.json")
             |  assert(f.exists())
             |  val b = $javaNioFiles.readAllBytes(f.toPath)
             |  import io.circe.parser.parse
             |  val dec = $codecName.instance.decode(context, parse(new $scString(b, $javaNioStandardCharsets.UTF_8)).toOption.get).toOption
             |  assert(dec.nonEmpty)
             |  jsonCompare(context, dec.get)
             |}
             |"""

        case uebaCodec: ScUEBACodecGenerator =>
          val body      = uebaCodecAssertions(uebaCodec, definition, srcRef)
          val codecName = uebaCodec.codecName(srcRef)
          q"""
             |"${srcRef.toString}" should "support UEBA codec" in {
             |  (0 to ${target.generic.codecTestIterations.toString}).foreach {
             |    _ =>
             |    uebaCodecTestImpl($baboonCodecContext.Default, $baboonRandom.default(), "default")  
             |  }
             |}
             |
             |def uebaCodecTestImpl(context: $baboonCodecContext, rnd: $baboonRandom, clue: $scString): $scUnit = {
             |  ${fixture.shift(2).trim}
             |  ${body.shift(2).trim}
             |}
             |
             |"${srcRef.toString}" should "load UEBA produced by C# codecs" in {
             |  testCsUeba($baboonCodecContext.Indexed, "indexed")
             |  testCsUeba($baboonCodecContext.Compact, "compact")
             |}
             |
             |def testCsUeba(context: $baboonCodecContext, clue: $scString): $scUnit = {
             |  val tpeid = "${definition.id.render}"
             |  val f     = new $javaFile(s"./../target/cs/ueba-$$clue/$$tpeid.uebin")
             |  assert(f.exists())
             |  val b = $javaNioFiles.readAllBytes(f.toPath)
             |  val bais = new java.io.ByteArrayInputStream(b)
             |  val dis = new $binaryInput(bais)
             |  val dec = $codecName.instance.decode(context, dis)
             |  uebaCompare(context, dec)
             |}
             |
             |def uebaCompare(context: $baboonCodecContext, fixture: $srcRef): $scUnit = {
             |  val baos = new java.io.ByteArrayOutputStream()
             |  val dos = new $binaryOutput(baos)
             |  $codecName.instance.encode(context, dos, fixture)
             |  val bytes = baos.toByteArray
             |  val bais = new java.io.ByteArrayInputStream(bytes)
             |  val dis = new $binaryInput(bais)
             |  val dec = $codecName.instance.decode(context, dis)
             |  assert(fixture == dec)
             |}
             |"""

        case unknown =>
          logger.message(s"Cannot create codec tests (${unknown.codecName(srcRef)}) for unsupported type $srcRef")
          q""
      }.toList.map(_.stripMargin.trim).join("\n\n").shift(2).trim
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

    private def jsonCodecAssertions(definition: DomainMember.User, srcRef: ScValue.ScType): TextTree[ScValue] = {
      definition.defn match {
        case _: Typedef.Adt =>
          q"""fixtures.foreach {
             |  fixture =>
             |    jsonCompare(context, fixture)
             |}
             |""".stripMargin.trim
        case _ =>
          q"jsonCompare(context, fixture)"
      }
    }

    private def uebaCodecAssertions(codec: ScUEBACodecGenerator, definition: DomainMember.User, srcRef: ScValue.ScType): TextTree[ScValue] = {
      definition.defn match {
        case _: Typedef.Adt =>
          q"""fixtures.foreach {
             |  fixture =>
             |    uebaCompare(context, fixture)
             |}
             |""".stripMargin.trim
        case _ =>
          q"uebaCompare(context, fixture)"
      }
    }
  }
}
