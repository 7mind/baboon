package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.CompilerTarget.DtTarget
import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, BaboonLang, Domain, DomainMember, Typedef}
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait DtCodecTestsTranslator {
  def translate(
    definition: DomainMember.User,
    dtRef: DtValue.DtType,
    srcRef: DtValue.DtType,
    testPath: String,
    typePath: String,
    fixturePath: String,
  ): Option[TextTree[DtValue]]
}

object DtCodecTestsTranslator {
  final class Impl(
    codecs: Set[DtCodecTranslator],
    typeTranslator: DtTypeTranslator,
    logger: BLogger,
    enquiries: BaboonEnquiries,
    target: DtTarget,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends DtCodecTestsTranslator {
    override def translate(
      definition: DomainMember.User,
      dtRef: DtValue.DtType,
      srcRef: DtValue.DtType,
      testPath: String,
      typePath: String,
      fixturePath: String,
    ): Option[TextTree[DtValue]] = {
      val isLatestVersion = domain.version == evo.latest

      definition match {
        case d if enquiries.hasForeignType(d, domain, BaboonLang.Dart) => None
        case d if enquiries.isRecursiveTypedef(d, domain)     => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _ if !isLatestVersion                            => None
        case _ =>
          val testBody           = makeTest(definition, srcRef)
          val typeImport         = makeTestToLibImport(testPath, typePath)
          val needsFixtureImport = !definition.defn.isInstanceOf[Typedef.Enum]
          val fixtureImportLine = if (needsFixtureImport) {
            val fixtureImport = makeTestToLibImport(testPath, fixturePath)
            s"import '$fixtureImport';"
          } else ""
          val testFile =
            q"""import 'package:test/test.dart';
               |import 'package:baboon_runtime/baboon_runtime.dart';
               |import 'package:baboon_runtime/baboon_fixture.dart';
               |import 'dart:io';
               |import 'dart:convert';
               |import 'dart:typed_data';
               |import '$typeImport';
               |$fixtureImportLine
               |
               |void main() {
               |  group('${srcRef.name}', () {
               |    ${testBody.shift(4).trim}
               |  });
               |}
               |""".stripMargin
          Some(testFile)
      }
    }

    private def makeTestToLibImport(testPath: String, libPath: String): String = {
      val testDir  = testPath.split('/').init
      val upLevels = testDir.length + 1 // +1 to escape out of the test/ directory
      val prefix   = "../" * upLevels + "lib/"
      prefix + libPath
    }

    private def makeTest(definition: DomainMember.User, srcRef: DtValue.DtType): TextTree[DtValue] = {
      val fixture = makeFixture(definition, domain, evo)
      codecs
        .filter(_.isActive(definition.id)).map {
          codec =>
            val body = codec.id match {
              case "Json" =>
                val assertions = jsonCodecAssertions(definition, srcRef)
                val crossRead  = crossLanguageJsonRead(definition, srcRef)
                val crossWrite = crossLanguageJsonWrite(definition, srcRef)

                q"""
               |test('JSON codec round-trip', () {
               |  for (var i = 0; i < ${target.generic.codecTestIterations.toString}; i++) {
               |    final rnd = $baboonRandomFactory.create();
               |    ${fixture.shift(4).trim}
               |    ${assertions.shift(4).trim}
               |  }
               |});
               |
               |${crossRead.shift(0).trim}
               |
               |${crossWrite.shift(0).trim}
               |"""

              case "Ueba" =>
                val compactAssertions = uebaCodecAssertions(definition, srcRef, q"$baboonCodecContext.compact")
                val indexedAssertions = uebaCodecAssertions(definition, srcRef, q"$baboonCodecContext.indexed")
                val crossRead         = crossLanguageUebaRead(definition, srcRef)
                val crossWrite        = crossLanguageUebaWrite(definition, srcRef)

                q"""
               |test('UEBA compact codec round-trip', () {
               |  for (var i = 0; i < ${target.generic.codecTestIterations.toString}; i++) {
               |    final rnd = $baboonRandomFactory.create();
               |    ${fixture.shift(4).trim}
               |    ${compactAssertions.shift(4).trim}
               |  }
               |});
               |
               |test('UEBA indexed codec round-trip', () {
               |  for (var i = 0; i < ${target.generic.codecTestIterations.toString}; i++) {
               |    final rnd = $baboonRandomFactory.create();
               |    ${fixture.shift(4).trim}
               |    ${indexedAssertions.shift(4).trim}
               |  }
               |});
               |
               |${crossRead.shift(0).trim}
               |
               |${crossWrite.shift(0).trim}
               |"""

              case unknown =>
                logger.message(s"Cannot create codec tests ($unknown) for unsupported type $srcRef")
                q""
            }
            body.stripMargin.trim
        }.toList.joinNN().shift(2).trim
    }

    private def makeFixture(
      definition: DomainMember.User,
      domain: Domain,
      evolution: BaboonEvolution,
    ): TextTree[DtValue] = {
      val dtType      = typeTranslator.asDtType(definition.id, domain, evolution)
      val fixtureName = s"${definition.id.name.name.capitalize}_Fixture"
      definition.defn match {
        case e: Typedef.Enum =>
          q"final fixture = $dtType.values[rnd.nextIntRange($dtType.values.length)];"
        case _: Typedef.Adt =>
          q"final fixtures = $fixtureName.randomAll(rnd);"
        case _ =>
          q"final fixture = $fixtureName.random(rnd);"
      }
    }

    private def jsonCodecAssertions(definition: DomainMember.User, srcRef: DtValue.DtType): TextTree[DtValue] = {
      val codecName = DtValue.DtType(srcRef.pkg, s"${srcRef.name}_JsonCodec")
      definition.defn match {
        case _: Typedef.Adt =>
          q"""for (final fixture in fixtures) {
             |  final encoded = $codecName.instance.encode($baboonCodecContext.defaultCtx, fixture);
             |  final decoded = $codecName.instance.decode($baboonCodecContext.defaultCtx, encoded);
             |  expect(decoded, equals(fixture));
             |}
             |""".stripMargin.trim
        case _ =>
          q"""final encoded = $codecName.instance.encode($baboonCodecContext.defaultCtx, fixture);
             |final decoded = $codecName.instance.decode($baboonCodecContext.defaultCtx, encoded);
             |expect(decoded, equals(fixture));""".stripMargin
      }
    }

    private def uebaCodecAssertions(
      definition: DomainMember.User,
      srcRef: DtValue.DtType,
      context: TextTree[DtValue],
    ): TextTree[DtValue] = {
      val codecName = DtValue.DtType(srcRef.pkg, s"${srcRef.name}_UebaCodec")
      definition.defn match {
        case _: Typedef.Adt =>
          q"""for (final fixture in fixtures) {
             |  final writer = $baboonBinTools.createWriter();
             |  $codecName.instance.encode($context, writer, fixture);
             |  final bytes = writer.toBytes();
             |  final reader = $baboonBinTools.createReader(bytes);
             |  final decoded = $codecName.instance.decode($context, reader);
             |  expect(decoded, equals(fixture));
             |}
             |""".stripMargin.trim
        case _ =>
          q"""final writer = $baboonBinTools.createWriter();
             |$codecName.instance.encode($context, writer, fixture);
             |final bytes = writer.toBytes();
             |final reader = $baboonBinTools.createReader(bytes);
             |final decoded = $codecName.instance.decode($context, reader);
             |expect(decoded, equals(fixture));""".stripMargin
      }
    }

    private def crossLanguageJsonRead(definition: DomainMember.User, srcRef: DtValue.DtType): TextTree[DtValue] = {
      val codecName = DtValue.DtType(srcRef.pkg, s"${srcRef.name}_JsonCodec")
      val typeId    = definition.id.render
      val languages = List("cs", "scala", "rust", "typescript", "kotlin", "java", "swift")
      val readTests = languages.map {
        lang =>
          q"""test('Cross-language JSON reading from $lang', () {
             |  final file = File('../../../../../target/$lang/json-default/$typeId.json');
             |  if (file.existsSync()) {
             |    final content = file.readAsStringSync();
             |    final json = jsonDecode(content);
             |    final decoded = $codecName.instance.decode($baboonCodecContext.defaultCtx, json);
             |    expect(decoded, isNotNull);
             |  }
             |}, skip: !File('../../../../../target/$lang/json-default/$typeId.json').existsSync());
             |""".stripMargin
      }
      readTests.joinNN()
    }

    private def crossLanguageJsonWrite(definition: DomainMember.User, srcRef: DtValue.DtType): TextTree[DtValue] = {
      val codecName = DtValue.DtType(srcRef.pkg, s"${srcRef.name}_JsonCodec")
      val typeId    = definition.id.render
      q"""test('Cross-language JSON writing', () {
         |  final rnd = $baboonRandomFactory.create();
         |  ${makeFixture(definition, domain, evo).shift(2).trim}
         |  ${makeJsonWriteBody(definition, srcRef).shift(2).trim}
         |});
         |""".stripMargin
    }

    private def makeJsonWriteBody(definition: DomainMember.User, srcRef: DtValue.DtType): TextTree[DtValue] = {
      val codecName = DtValue.DtType(srcRef.pkg, s"${srcRef.name}_JsonCodec")
      val typeId    = definition.id.render
      val fixtureVar = definition.defn match {
        case _: Typedef.Adt => "fixtures.first"
        case _              => "fixture"
      }
      q"""final outFile = File('../../../../../target/dart/json-default/$typeId.json');
         |outFile.parent.createSync(recursive: true);
         |final encoded = $codecName.instance.encode($baboonCodecContext.defaultCtx, $fixtureVar);
         |outFile.writeAsStringSync(jsonEncode(encoded));""".stripMargin
    }

    private def crossLanguageUebaRead(definition: DomainMember.User, srcRef: DtValue.DtType): TextTree[DtValue] = {
      val codecName = DtValue.DtType(srcRef.pkg, s"${srcRef.name}_UebaCodec")
      val typeId    = definition.id.render
      val languages = List("cs", "scala", "rust", "typescript", "kotlin", "java", "swift")
      val readTests = languages.map {
        lang =>
          q"""test('Cross-language UEBA reading from $lang', () {
             |  final file = File('../../../../../target/$lang/ueba-default/$typeId.ueba');
             |  if (file.existsSync()) {
             |    final bytes = file.readAsBytesSync();
             |    final reader = $baboonBinTools.createReader(bytes);
             |    final decoded = $codecName.instance.decode($baboonCodecContext.compact, reader);
             |    expect(decoded, isNotNull);
             |  }
             |}, skip: !File('../../../../../target/$lang/ueba-default/$typeId.ueba').existsSync());
             |""".stripMargin
      }
      readTests.joinNN()
    }

    private def crossLanguageUebaWrite(definition: DomainMember.User, srcRef: DtValue.DtType): TextTree[DtValue] = {
      val codecName = DtValue.DtType(srcRef.pkg, s"${srcRef.name}_UebaCodec")
      val typeId    = definition.id.render
      q"""test('Cross-language UEBA writing', () {
         |  final rnd = $baboonRandomFactory.create();
         |  ${makeFixture(definition, domain, evo).shift(2).trim}
         |  ${makeUebaWriteBody(definition, srcRef).shift(2).trim}
         |});
         |""".stripMargin
    }

    private def makeUebaWriteBody(definition: DomainMember.User, srcRef: DtValue.DtType): TextTree[DtValue] = {
      val codecName = DtValue.DtType(srcRef.pkg, s"${srcRef.name}_UebaCodec")
      val typeId    = definition.id.render
      val fixtureVar = definition.defn match {
        case _: Typedef.Adt => "fixtures.first"
        case _              => "fixture"
      }
      q"""final outFile = File('../../../../../target/dart/ueba-default/$typeId.ueba');
         |outFile.parent.createSync(recursive: true);
         |final writer = $baboonBinTools.createWriter();
         |$codecName.instance.encode($baboonCodecContext.compact, writer, $fixtureVar);
         |outFile.writeAsBytesSync(writer.toBytes());""".stripMargin
    }
  }
}
