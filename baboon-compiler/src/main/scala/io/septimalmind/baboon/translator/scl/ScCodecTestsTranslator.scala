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
      codecs.filter(_.isActive(definition.id)).map {
        case jsonCodec: ScJsonCodecGenerator =>
          val body      = jsonCodecAssertions(definition)
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
             |  testScJson($baboonCodecContext.Default, "default")
             |}
             |
             |def jsonCodecTestImpl(context: $baboonCodecContext, rnd: $baboonRandom, clue: $scString): $scUnit = {
             |  ${fixture.shift(2).trim}
             |  ${body.shift(2).trim}
             |}
             |
             |def jsonCompare(context: $baboonCodecContext, fixture: $srcRef, clue: $scString): io.circe.Json = {
             |  val fixtureJson    = $codecName.instance.encode(context, fixture)
             |  val fixtureDecoded = $codecName.instance.decode(context, fixtureJson).toOption.get
             |  assert(fixture == fixtureDecoded, s"$$clue")
             |  fixtureJson
             |}
             |
             |def testScJson(context: $baboonCodecContext, clue: $scString): $scUnit = {
             |  val tpeid = "${definition.id.render}"
             |  val f     = new $javaFile(s"./../target/cs/json-$$clue/$$tpeid.json")
             |  assume(f.exists())
             |  val b = $javaNioFiles.readAllBytes(f.toPath)
             |  import io.circe.parser.parse
             |  val csJson = parse(new $scString(b, $javaNioStandardCharsets.UTF_8)).toOption.get
             |  val dec = $codecName.instance.decode(context, csJson).toOption
             |  assert(dec.nonEmpty)
             |  jsonCompare(context, dec.get, clue)
             |}
             |"""

        case uebaCodec: ScUEBACodecGenerator =>
          val body      = uebaCodecAssertions(definition)
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
             |  testScUeba($baboonCodecContext.Indexed, "indexed")
             |  testScUeba($baboonCodecContext.Compact, "compact")
             |}
             |
             |def testScUeba(context: $baboonCodecContext, clue: $scString): $scUnit = {
             |  val tpeid = "${definition.id.render}"
             |  val f     = new $javaFile(s"./../target/cs/ueba-$$clue/$$tpeid.uebin")
             |  assume(f.exists())
             |  val csUebaBytes = $javaNioFiles.readAllBytes(f.toPath)
             |  val bais = new java.io.ByteArrayInputStream(csUebaBytes)
             |  val dis = new $binaryInput(bais)
             |  val dec = $codecName.instance.decode(context, dis)
             |  val sclUebaBytes = uebaCompare(context, dec, clue)
             |  assert(csUebaBytes.length == sclUebaBytes.length, s"$$clue")
             |}
             |
             |def uebaCompare(context: $baboonCodecContext, fixture: $srcRef, clue: $scString): $scArray[$scByte] = {
             |  val baos = new java.io.ByteArrayOutputStream()
             |  val dos = new $binaryOutput(baos)
             |  $codecName.instance.encode(context, dos, fixture)
             |  
             |  val bytes = baos.toByteArray
             |  
             |  val bais = new java.io.ByteArrayInputStream(bytes)
             |  val dis = new $binaryInput(bais)
             |  val dec = $codecName.instance.decode(context, dis)
             |  assert(fixture == dec, s"$$clue")
             |  
             |  bytes
             |}
             |"""

        case unknown =>
          logger.message(s"Cannot create codec tests (${unknown.codecName(srcRef)}) for unsupported type $srcRef")
          q""
      }.toList.map(_.stripMargin.trim).joinNN().shift(2).trim
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

    private def jsonCodecAssertions(definition: DomainMember.User): TextTree[ScValue] = {
      definition.defn match {
        case _: Typedef.Adt =>
          q"""fixtures.foreach {
             |  fixture =>
             |    jsonCompare(context, fixture, clue)
             |}
             |""".stripMargin.trim
        case _ => q"jsonCompare(context, fixture, clue)"
      }
    }

    private def uebaCodecAssertions(definition: DomainMember.User): TextTree[ScValue] = {
      definition.defn match {
        case _: Typedef.Adt =>
          q"""fixtures.foreach {
             |  fixture =>
             |    uebaCompare(context, fixture, clue)
             |}
             |""".stripMargin.trim
        case _ => q"uebaCompare(context, fixture, clue)"
      }
    }
  }
}
