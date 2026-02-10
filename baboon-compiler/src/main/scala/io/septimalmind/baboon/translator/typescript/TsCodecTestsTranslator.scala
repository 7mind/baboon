package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember, Typedef}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait TsCodecTestsTranslator {
  def translate(
    definition: DomainMember.User,
    tsRef: TsValue.TsType,
    srcRef: TsValue.TsType,
    fixtureModule: TsValue.TsModuleId,
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
  ) extends TsCodecTestsTranslator {

    import TsTypes.{baboonRandom, baboonCodecContext, baboonBinWriter, baboonBinReader}

    override def translate(
      definition: DomainMember.User,
      tsRef: TsValue.TsType,
      srcRef: TsValue.TsType,
      fixtureModule: TsValue.TsModuleId,
    ): Option[TextTree[TsValue]] = {
      val isLatestVersion = domain.version == evo.latest

      definition match {
        case d if enquiries.hasForeignType(d, domain)         => None
        case d if enquiries.isRecursiveTypedef(d, domain)     => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _ if !isLatestVersion                            => None
        case _ =>
          val tests = makeTests(definition, srcRef, fixtureModule)
          if (tests.isEmpty) None else Some(tests)
      }
    }

    private def makeTests(definition: DomainMember.User, srcRef: TsValue.TsType, fixtureModule: TsValue.TsModuleId): TextTree[TsValue] = {
      val testFnName = typeTranslator.camelToKebab(srcRef.name).replace('-', '_')
      val fixtureMethodName = s"random_${typeTranslator.camelToKebab(definition.id.name.name).replace('-', '_')}"
      val fixtureFn = TsValue.TsType(fixtureModule, fixtureMethodName)
      val fixtureFnAll = TsValue.TsType(fixtureModule, s"${fixtureMethodName}_all")

      val encodeJson = TsValue.TsType(srcRef.module, s"encode_${srcRef.name}_json")
      val decodeJson = TsValue.TsType(srcRef.module, s"decode_${srcRef.name}_json")
      val encodeUeba = TsValue.TsType(srcRef.module, s"encode_${srcRef.name}_ueba")
      val decodeUeba = TsValue.TsType(srcRef.module, s"decode_${srcRef.name}_ueba")
      val valuesRef = TsValue.TsType(srcRef.module, s"${srcRef.name}_values")

      val jsonTests = codecs.filter(_.isActive(definition.id)).collect {
        case _: TsJsonCodecGenerator =>
          val jsonRoundTrip = definition.defn match {
            case _: Typedef.Adt =>
              q"""const fixtures = ${fixtureFnAll}(rnd);
                 |for (const fixture of fixtures) {
                 |    const json = ${encodeJson}(fixture);
                 |    const decoded = ${decodeJson}(json);
                 |    expect(decoded).toStrictEqual(fixture);
                 |}""".stripMargin
            case _: Typedef.Enum =>
              q"""for (const fixture of ${valuesRef}) {
                 |    const json = ${encodeJson}(fixture);
                 |    const decoded = ${decodeJson}(json);
                 |    expect(decoded).toStrictEqual(fixture);
                 |}""".stripMargin
            case _ =>
              q"""const fixture = ${fixtureFn}(rnd);
                 |const json = ${encodeJson}(fixture);
                 |const decoded = ${decodeJson}(json);
                 |expect(decoded).toStrictEqual(fixture);""".stripMargin
          }

          q"""test("${testFnName}_json_codec", () => {
             |    for (let i = 0; i < ${target.generic.codecTestIterations.toString}; i++) {
             |        const rnd = new ${baboonRandom}();
             |        ${jsonRoundTrip.shift(8).trim}
             |    }
             |});""".stripMargin
      }.toList

      val uebaTests = codecs.filter(_.isActive(definition.id)).collect {
        case _: TsUEBACodecGenerator =>
          val uebaRoundTrip = definition.defn match {
            case _: Typedef.Adt =>
              q"""const fixtures = ${fixtureFnAll}(rnd);
                 |for (const fixture of fixtures) {
                 |    const writer = new ${baboonBinWriter}();
                 |    ${encodeUeba}(fixture, ctx, writer);
                 |    const reader = new ${baboonBinReader}(writer.toBytes());
                 |    const decoded = ${decodeUeba}(ctx, reader);
                 |    expect(decoded).toStrictEqual(fixture);
                 |}""".stripMargin
            case _: Typedef.Enum =>
              q"""for (const fixture of ${valuesRef}) {
                 |    const writer = new ${baboonBinWriter}();
                 |    ${encodeUeba}(fixture, ctx, writer);
                 |    const reader = new ${baboonBinReader}(writer.toBytes());
                 |    const decoded = ${decodeUeba}(ctx, reader);
                 |    expect(decoded).toStrictEqual(fixture);
                 |}""".stripMargin
            case _ =>
              q"""const fixture = ${fixtureFn}(rnd);
                 |const writer = new ${baboonBinWriter}();
                 |${encodeUeba}(fixture, ctx, writer);
                 |const reader = new ${baboonBinReader}(writer.toBytes());
                 |const decoded = ${decodeUeba}(ctx, reader);
                 |expect(decoded).toStrictEqual(fixture);""".stripMargin
          }

          q"""test("${testFnName}_ueba_codec", () => {
             |    const ctx = ${baboonCodecContext}.Default;
             |    for (let i = 0; i < ${target.generic.codecTestIterations.toString}; i++) {
             |        const rnd = new ${baboonRandom}();
             |        ${uebaRoundTrip.shift(8).trim}
             |    }
             |});""".stripMargin
      }.toList

      (jsonTests ++ uebaTests).joinNN()
    }
  }
}
