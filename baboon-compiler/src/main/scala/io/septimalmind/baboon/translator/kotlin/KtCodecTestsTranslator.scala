package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.CompilerTarget.KtTarget
import io.septimalmind.baboon.translator.kotlin.KtTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, BaboonLang, Domain, DomainMember, Typedef}
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait KtCodecTestsTranslator {
  def translate(
    definition: DomainMember.User,
    ktRef: KtValue.KtType,
    srcRef: KtValue.KtType,
  ): Option[TextTree[KtValue]]
}

object KtCodecTestsTranslator {
  final class Impl(
    codecs: Set[KtCodecTranslator],
    typeTranslator: KtTypeTranslator,
    logger: BLogger,
    enquiries: BaboonEnquiries,
    target: KtTarget,
    domain: Domain,
    evo: BaboonEvolution,
    ktTypes: KtTypes,
  ) extends KtCodecTestsTranslator {
    import ktTypes.*

    override def translate(
      definition: DomainMember.User,
      ktRef: KtValue.KtType,
      srcRef: KtValue.KtType,
    ): Option[TextTree[KtValue]] = {
      val isLatestVersion = domain.version == evo.latest

      definition match {
        case d if enquiries.hasForeignType(d, domain, BaboonLang.Kotlin) => None
        case d if enquiries.isRecursiveTypedef(d, domain)                => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef]            => None
        case _ if !isLatestVersion                                       => None
        case _ =>
          val testClass =
            q"""class ${srcRef.name}_tests {
               |  ${makeTest(definition, srcRef)}
               |}
               |""".stripMargin
          Some(testClass)
      }
    }

    private def makeTest(definition: DomainMember.User, srcRef: KtValue.KtType): TextTree[KtValue] = {
      // PR-07-D01 (Kotlin analog): per-codec fixture variants. UEBA codec test uses the
      // `AnyOpaqueUeba`-bearing fixture (`random`); JSON codec test uses the
      // `AnyOpaqueJson`-bearing fixture (`randomJson`). Each fixture matches its codec's native
      // any-field branch so round-trip avoids cross-format conversion and never needs a
      // `BaboonCodecContext.withFacade` ctx.
      val uebaFixture = makeFixture(definition, domain, evo, useJsonAny = false)
      val jsonFixture = makeFixture(definition, domain, evo, useJsonAny = true)
      codecs
        .filter(_.isActive(definition.id)).map {
          case jsonCodec: KtJsonCodecGenerator =>
            val body      = jsonCodecAssertions(definition)
            val codecName = jsonCodec.codecName(srcRef)

            q"""
             |@$ktTestAnnotation
             |fun `${srcRef.name} should support JSON codec`() {
             |  repeat(${target.generic.codecTestIterations.toString}) {
             |    jsonCodecTestImpl($baboonCodecContext.Default, $baboonRandomFactory.default(), "default")
             |  }
             |}
             |
             |private fun jsonCodecTestImpl(context: $baboonCodecContext, rnd: $baboonRandom, clue: String) {
             |  ${jsonFixture.shift(2).trim}
             |  ${body.shift(2).trim}
             |}
             |
             |private fun jsonCompare(context: $baboonCodecContext, fixture: $srcRef, clue: String): $jsonElement {
             |  val fixtureJson = $codecName.encode(context, fixture)
             |  val fixtureDecoded = $codecName.decode(context, fixtureJson)
             |  $ktAssertEquals(fixture, fixtureDecoded, clue)
             |  return fixtureJson
             |}
             |"""

          case uebaCodec: KtUEBACodecGenerator =>
            val body      = uebaCodecAssertions(definition)
            val codecName = uebaCodec.codecName(srcRef)
            q"""
             |@$ktTestAnnotation
             |fun `${srcRef.name} should support UEBA codec`() {
             |  repeat(${target.generic.codecTestIterations.toString}) {
             |    uebaCodecTestImpl($baboonCodecContext.Default, $baboonRandomFactory.default(), "default")
             |  }
             |}
             |
             |private fun uebaCodecTestImpl(context: $baboonCodecContext, rnd: $baboonRandom, clue: String) {
             |  ${uebaFixture.shift(2).trim}
             |  ${body.shift(2).trim}
             |}
             |
             |private fun uebaCompare(context: $baboonCodecContext, fixture: $srcRef, clue: String): ByteArray {
             |  ${if (ktTypes.multiplatform) {
                q"""val dos = $binaryOutput()
                   |  $codecName.instance.encode(context, dos, fixture)
                   |
                   |  val bytes = dos.toByteArray()
                   |
                   |  val dis = $binaryInput(bytes)""".stripMargin
              } else {
                q"""val baos = $byteArrayOutputStream()
                   |  val dos = $binaryOutput(baos)
                   |  $codecName.instance.encode(context, dos, fixture)
                   |
                   |  val bytes = baos.toByteArray()
                   |
                   |  val bais = $byteArrayInputStream(bytes)
                   |  val dis = $binaryInput(bais)""".stripMargin
              }}
             |  val dec = $codecName.instance.decode(context, dis)
             |  $ktAssertEquals(fixture, dec, clue)
             |
             |  return bytes
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
      useJsonAny: Boolean,
    ): TextTree[KtValue] = {
      val randomMethod    = if (useJsonAny) "randomJson" else "random"
      val randomAllMethod = if (useJsonAny) "randomAllJson" else "randomAll"
      definition.defn match {
        case e: Typedef.Enum => q"val fixture = rnd.randomElement(${e.id.name.name}.all())"
        case _: Typedef.Adt  => q"val fixtures = ${typeTranslator.asKtType(definition.id, domain, evolution)}_Fixture.$randomAllMethod(rnd)"
        case _               => q"val fixture = ${typeTranslator.asKtType(definition.id, domain, evolution)}_Fixture.$randomMethod(rnd)"
      }
    }

    private def jsonCodecAssertions(definition: DomainMember.User): TextTree[KtValue] = {
      definition.defn match {
        case _: Typedef.Adt =>
          q"""fixtures.forEach { fixture ->
             |  jsonCompare(context, fixture, clue)
             |}
             |""".stripMargin.trim
        case _ => q"jsonCompare(context, fixture, clue)"
      }
    }

    private def uebaCodecAssertions(definition: DomainMember.User): TextTree[KtValue] = {
      definition.defn match {
        case _: Typedef.Adt =>
          q"""fixtures.forEach { fixture ->
             |  uebaCompare(context, fixture, clue)
             |}
             |""".stripMargin.trim
        case _ => q"uebaCompare(context, fixture, clue)"
      }
    }
  }
}
