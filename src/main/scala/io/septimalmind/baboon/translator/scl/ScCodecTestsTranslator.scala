package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember}
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree

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
    import ScValue.*
    private val scTrue = Lit("true")
    private val scFalse = Lit("false")

    override def translate(
      definition: DomainMember.User,
      csRef: ScValue.ScType,
      srcRef: ScValue.ScType,
    ): Option[TextTree[ScValue]] = {
      val isLatestVersion = domain.version == evo.latest

      definition match {
        case d if enquiries.hasForeignType(d, domain)                           => None
        case d if enquiries.isRecursiveTypedef(d, domain)                       => None
        case d if d.defn.isInstanceOf[io.septimalmind.baboon.typer.model.Typedef.NonDataTypedef] => None
        case _ if !target.language.enableDeprecatedEncoders && !isLatestVersion => None
        case _ =>
          val testClassName = s"${srcRef.name}_Tests"
          val testClass =
            ScValue.Tree(
              List(
                ScValue.Line(s"class $testClassName extends $anyFunSuite {"),
                ScValue.Line(s"  import ${srcRef.name}._"),
                makeTest(definition, srcRef).indented(2),
                ScValue.Line(s"}"),
              )
            )
          Some(testClass)
      }
    }

    private def makeTest(definition: DomainMember.User, srcRef: ScValue.ScType): TextTree[ScValue] = {
      // TODO: implement makeFixture by adapting from CS and ScCodecFixtureTranslator
      val fixture = makeFixture(definition, domain, evo)

      val codecTests = codecs.map {
        case jsonCodec: ScJsonCodecTranslator =>
          val body = jsonCodecAssertions(jsonCodec, definition, srcRef)
          ScValue.Tree(
            List(
              ScValue.Line(s"""test("json codec test for ${srcRef.name}") { """),
              ScValue.Line(s"  for (i <- 0 until ${target.generic.codecTestIterations}) {"),
              ScValue.Line(s"    jsonCodecTestImpl(${ScValue.baboonCodecContext}.Default)"),
              ScValue.Line(s"  }"),
              ScValue.Line(s"}"),
              ScValue.Line(s""),
              ScValue.Line(s"private def jsonCodecTestImpl(context: ${ScValue.baboonCodecContext}): Unit = {"),
              fixture.indented(4),
              body.indented(4),
              ScValue.Line(s"}"),
            )
          )
        case uebaCodec: ScUebaCodecTranslator =>
          val body = uebaCodecAssertions(uebaCodec, definition, srcRef)
          ScValue.Tree(
            List(
              ScValue.Line(s"""test("ueba codec test (no index) for ${srcRef.name}") { """),
              ScValue.Line(s"  for (i <- 0 until ${target.generic.codecTestIterations}) {"),
              ScValue.Line(s"    uebaCodecTestImpl(${ScValue.baboonCodecContext}.Compact)"),
              ScValue.Line(s"  }"),
              ScValue.Line(s"}"),
              ScValue.Line(s""),
              ScValue.Line(s"""test("ueba codec test (indexed) for ${srcRef.name}") { """),
              ScValue.Line(s"  for (i <- 0 until ${target.generic.codecTestIterations}) {"),
              ScValue.Line(s"    uebaCodecTestImpl(${ScValue.baboonCodecContext}.Indexed)"),
              ScValue.Line(s"  }"),
              ScValue.Line(s"}"),
              ScValue.Line(s""),
              ScValue.Line(s"private def uebaCodecTestImpl(context: ${ScValue.baboonCodecContext}): Unit = {"),
              fixture.indented(4),
              body.indented(4),
              ScValue.Line(s"}"),
            )
          )
        case unknown =>
          logger.message(s"Cannot create codec tests (${unknown.codecName(srcRef)}) for unsupported type $srcRef")
          ScValue.Empty
      }.toList

      ScValue.Tree(codecTests.filterNot(_.isEmpty))
    }

    private def makeFixture(
      definition: DomainMember.User,
      domain: Domain,
      evolution: BaboonEvolution,
    ): TextTree[ScValue] = {
      definition.defn match {
        case e: io.septimalmind.baboon.typer.model.Typedef.Enum =>
          // Enums are handled directly by BaboonRandom in ScCodecFixtureTranslator as well
          val enumType = typeTranslator.asScType(e.id, domain, evolution)
          Line(s"val fixture = $baboonRandom.nextRandomEnum[${enumType.name}]()")
        case adt: io.septimalmind.baboon.typer.model.Typedef.Adt =>
          val fixtureObject = s"${typeTranslator.asScType(adt.id, domain, evolution).name}_Fixture"
          Line(s"val fixtures = $fixtureObject.randomAll($baboonRandom)")
        case dto: io.septimalmind.baboon.typer.model.Typedef.Dto =>
          val fixtureObject = s"${typeTranslator.asScType(dto.id, domain, evolution).name}_Fixture"
          Line(s"val fixture = $fixtureObject.random($baboonRandom)")
        case other => // Should ideally not happen if initial checks are correct
          logger.warn(s"makeFixture called with unexpected definition type: $other for ${definition.id.name.name}")
          Empty
      }
    }

    private def jsonCodecAssertions(
      codec: ScJsonCodecTranslator,
      definition: DomainMember.User,
      srcRef: ScValue.ScType
    ): TextTree[ScValue] = {
      // TODO: Implement Scala specific JSON assertions
      def jsonTest(isAdtMember: Boolean): TextTree[ScValue] = {
        val codecName = codec.codecName(srcRef)
        val fieldName = if (isAdtMember) "adtMemberFixture" else "fixture"
        val serialized = ScValue.L(s"${fieldName}Json")
        val decoded = ScValue.L(s"${fieldName}Decoded")
        ScValue.Tree(
          List(
            ScValue.Line(s"val $serialized = $codecName.encode(context, $fieldName)"),
            ScValue.Line(s"val $decoded = $codecName.decode(context, $serialized)"),
            ScValue.Line(s"assert($fieldName == $decoded)"),
          )
        )
      }

      definition.defn match {
        case _: io.septimalmind.baboon.typer.model.Typedef.Adt =>
          ScValue.Tree(
            List(
              ScValue.Line(s"fixtures.foreach { adtMemberFixture => "),
              jsonTest(true).indented(2),
              ScValue.Line(s"}"),
            )
          )
        case _ =>
          jsonTest(false)
      }
    }

    private def uebaCodecAssertions(
      codec: ScUebaCodecTranslator,
      definition: DomainMember.User,
      srcRef: ScValue.ScType
    ): TextTree[ScValue] = {
      // TODO: Implement Scala specific UEBA assertions
      // This will involve using something like ByteArrayInputStream/OutputStream and DataInputStream/OutputStream
      def binaryTest(isAdtMember: Boolean): TextTree[ScValue] = {
        val codecName = codec.codecName(srcRef)
        val fieldName = if (isAdtMember) "adtMemberFixture" else "fixture"
        val serialized = ScValue.L(s"${fieldName}Bytes")
        val decoded = ScValue.L(s"${fieldName}Decoded")

        ScValue.Tree(
          List(
            ScValue.Line(s"val baos = new java.io.ByteArrayOutputStream()"),
            ScValue.Line(s"val dos = new java.io.DataOutputStream(baos)"),
            ScValue.Line(s"$codecName.encode(context, dos, $fieldName)"),
            ScValue.Line(s"dos.flush()"),
            ScValue.Line(s"val $serialized = baos.toByteArray()"),
            ScValue.Line(s"val bais = new java.io.ByteArrayInputStream($serialized)"),
            ScValue.Line(s"val dis = new java.io.DataInputStream(bais)"),
            ScValue.Line(s"val $decoded = $codecName.decode(context, dis)"),
            ScValue.Line(s"assert($fieldName == $decoded)"),
          )
        )
      }

      definition.defn match {
        case _: io.septimalmind.baboon.typer.model.Typedef.Adt =>
          ScValue.Tree(
            List(
              ScValue.Line(s"fixtures.foreach { adtMemberFixture => "),
              binaryTest(true).indented(2),
              ScValue.Line(s"}"),
            )
          )
        case _ =>
          binaryTest(false)
      }
    }

    private def anyFunSuite = tpe("org.scalatest.funsuite.AnyFunSuite")
    private def baboonRandom = ScValue.baboonRandom
    private def baboonCodecContext = ScValue.baboonCodecContext
  }
}
