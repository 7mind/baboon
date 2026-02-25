package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, BaboonLang, Domain, DomainMember, Typedef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*
import TsTypes.{baboonRandom, tsBaboonBinReader, tsBaboonBinWriter, tsBaboonCodecContext, tsReadFile, tsString}

trait TsCodecTestsTranslator {
  def translate(
    definition: DomainMember.User,
    tsRef: TsValue.TsType,
    srcRef: TsValue.TsType,
  ): Option[TextTree[TsValue]]
}

object TsCodecTestsTranslator {
  final class Impl(
    codecs: Set[TsCodecTranslator],
    typeTranslator: TsTypeTranslator,
    enquiries: BaboonEnquiries,
    target: TsTarget,
    domain: Domain,
    evo: BaboonEvolution,
    tsFileTools: TsFileTools,
  ) extends TsCodecTestsTranslator {

    override def translate(
      definition: DomainMember.User,
      tsRef: TsValue.TsType,
      srcRef: TsValue.TsType,
    ): Option[TextTree[TsValue]] = {
      val isLatestVersion = domain.version == evo.latest

      definition match {
        case d if enquiries.hasForeignType(d, domain, BaboonLang.Typescript) => None
        case d if enquiries.isRecursiveTypedef(d, domain)                    => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef]                => None
        case _ if !isLatestVersion                                           => None
        case _ =>
          val tests = makeTests(definition, srcRef)
          if (tests.isEmpty) None else Some(tests)
      }
    }

    private def makeTests(definition: DomainMember.User, srcRef: TsValue.TsType): TextTree[TsValue] = {
      val testFnName        = typeTranslator.camelToKebab(srcRef.name).replace('-', '_')
      val fixtureMethodName = s"random_${typeTranslator.camelToKebab(definition.id.name.name).replace('-', '_')}"
      val fixtureModule = typeTranslator.toTsModule(
        definition.id,
        domain.version,
        evo,
        tsFileTools.fixturesBasePkg,
        ".fixture",
      )
      val fixtureFn    = TsValue.TsType(fixtureModule, fixtureMethodName)
      val fixtureFnAll = TsValue.TsType(fixtureModule, s"${fixtureMethodName}_all")

      val valuesRef = TsValue.TsType(srcRef.moduleId, s"${srcRef.name}_values")

      val jsonTests = codecs
        .filter(_.isActive(definition.id)).collect {
          case jsonCodecGenerator: TsJsonCodecGenerator =>
            val jsonCodec = jsonCodecGenerator.codecName(srcRef)
            val jsonRoundTrip = definition.defn match {
              case _: Typedef.Adt =>
                q"""const fixtures = $fixtureFnAll(rnd);
                   |for (const fixture of fixtures) {
                   |    const json = $jsonCodec.instance.encode($tsBaboonCodecContext.Default, fixture);
                   |    const decoded = $jsonCodec.instance.decode($tsBaboonCodecContext.Default, json);
                   |    expect(decoded).toStrictEqual(fixture);
                   |}""".stripMargin
              case _: Typedef.Enum =>
                q"""for (const fixture of $valuesRef) {
                   |    const json = $jsonCodec.instance.encode($tsBaboonCodecContext.Default, fixture);
                   |    const decoded = $jsonCodec.instance.decode($tsBaboonCodecContext.Default, json);
                   |    expect(decoded).toStrictEqual(fixture);
                   |}""".stripMargin
              case _ =>
                q"""const fixture = $fixtureFn(rnd);
                   |const json = $jsonCodec.instance.encode($tsBaboonCodecContext.Default, fixture);
                   |const decoded = $jsonCodec.instance.decode($tsBaboonCodecContext.Default, json);
                   |expect(decoded).toStrictEqual(fixture);""".stripMargin
            }

            q"""test("${testFnName}_json_codec", () => {
               |    for (let i = 0; i < ${target.generic.codecTestIterations.toString}; i++) {
               |        const rnd = new $baboonRandom();
               |        ${jsonRoundTrip.shift(8).trim}
               |    }
               |});
               |
               |test("test_cs_json", () => {
               |    const path = '../target/cs/json-default/${definition.id.render}.json'
               |    const content = $tsReadFile(path, 'utf-8')
               |    const csJson = JSON.parse(content)
               |    const decoded = $jsonCodec.instance.decode($tsBaboonCodecContext.Default, csJson)
               |    const fixtureJson = $jsonCodec.instance.encode($tsBaboonCodecContext.Default, decoded)
               |    const fixtureDecoded = $jsonCodec.instance.decode($tsBaboonCodecContext.Default, fixtureJson)
               |    expect(decoded).toStrictEqual(fixtureDecoded)
               |});
               |""".stripMargin
        }.toList

      val uebaTests = codecs
        .filter(_.isActive(definition.id)).collect {
          case uebaCodecGenerator: TsUEBACodecGenerator =>
            val binCodec = uebaCodecGenerator.codecName(srcRef)
            val uebaRoundTrip = definition.defn match {
              case _: Typedef.Adt =>
                q"""const fixtures = $fixtureFnAll(rnd);
                   |for (const fixture of fixtures) {
                   |    const writer = new $tsBaboonBinWriter();
                   |    $binCodec.instance.encode(ctx, fixture, writer);
                   |    const reader = new $tsBaboonBinReader(writer.toBytes());
                   |    const decoded = $binCodec.instance.decode(ctx, reader);
                   |    expect(decoded).toStrictEqual(fixture);
                   |}""".stripMargin
              case _: Typedef.Enum =>
                q"""for (const fixture of $valuesRef) {
                   |    const writer = new $tsBaboonBinWriter();
                   |    $binCodec.instance.encode(ctx, fixture, writer);
                   |    const reader = new $tsBaboonBinReader(writer.toBytes());
                   |    const decoded = $binCodec.instance.decode(ctx, reader);
                   |    expect(decoded).toStrictEqual(fixture);
                   |}""".stripMargin
              case _ =>
                q"""const fixture = $fixtureFn(rnd);
                   |const writer = new $tsBaboonBinWriter();
                   |$binCodec.instance.encode(ctx, fixture, writer);
                   |const reader = new $tsBaboonBinReader(writer.toBytes());
                   |const decoded = $binCodec.instance.decode(ctx, reader);
                   |expect(decoded).toStrictEqual(fixture);""".stripMargin
            }

            q"""test("${testFnName}_ueba_codec", () => {
               |    const ctx = $tsBaboonCodecContext.Default;
               |    for (let i = 0; i < ${target.generic.codecTestIterations.toString}; i++) {
               |        const rnd = new $baboonRandom();
               |        ${uebaRoundTrip.shift(8).trim}
               |    }
               |})
               |
               |test('test-cs-ueba', () => {
               |    testUeba($tsBaboonCodecContext.Indexed, 'indexed');
               |    testUeba($tsBaboonCodecContext.Compact, 'compact');
               |});
               |
               |function testUeba(ctx: $tsBaboonCodecContext, clue: $tsString) {
               |    const path = `../target/cs/ueba-$${clue}/${definition.id.render}.uebin`
               |    const content = $tsReadFile(path)
               |    const reader = new $tsBaboonBinReader(content)
               |    const writer = new $tsBaboonBinWriter()
               |    const decoded = $binCodec.instance.decode(ctx, reader)
               |    $binCodec.instance.encode(ctx, decoded, writer)
               |    const fixtureDecoded = $binCodec.instance.decode(ctx, new $tsBaboonBinReader(writer.toBytes()))
               |    expect(decoded).toStrictEqual(fixtureDecoded)
               |}
               |""".stripMargin
        }.toList

      (jsonTests ++ uebaTests).joinNN()
    }
  }
}
