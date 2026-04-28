package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.CompilerTarget.SwTarget
import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.translator.swift.SwValue.SwType
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember, Typedef}
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait SwCodecTestsTranslator {
  def translate(
    definition: DomainMember.User,
    swRef: SwType,
    srcRef: SwType,
    testPath: String,
    typePath: String,
    fixturePath: String,
  ): Option[TextTree[SwValue]]
}

object SwCodecTestsTranslator {
  final class Impl(
    codecs: Set[SwCodecTranslator],
    typeTranslator: SwTypeTranslator,
    logger: BLogger,
    enquiries: BaboonEnquiries,
    target: SwTarget,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends SwCodecTestsTranslator {
    override def translate(
      definition: DomainMember.User,
      swRef: SwType,
      srcRef: SwType,
      testPath: String,
      typePath: String,
      fixturePath: String,
    ): Option[TextTree[SwValue]] = {
      val isLatestVersion = domain.version == evo.latest

      definition match {
        case d if enquiries.hasForeignType(d, domain)         => None
        case d if enquiries.isRecursiveTypedef(d, domain)     => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _ if !isLatestVersion                            => None
        case _ =>
          val testBody      = makeTest(definition, srcRef)
          val testClassName = srcRef.name.replace(".", "_")
          val moduleName    = typeTranslator.domainModuleName(domain.id, domain.version, evo)
          val testFile =
            q"""import XCTest
               |import Foundation
               |import BaboonRuntime
               |@testable import $moduleName
               |
               |final class ${testClassName}_Tests: XCTestCase {
               |    override class func setUp() {
               |        super.setUp()
               |        assertCrossLanguageFixtureRootExists()
               |    }
               |
               |    ${testBody.shift(4).trim}
               |}
               |""".stripMargin
          Some(testFile)
      }
    }

    private def makeTest(definition: DomainMember.User, srcRef: SwType): TextTree[SwValue] = {
      // PR-07-D01 (Swift analog): per-codec fixture variants. UEBA codec test uses the
      // `AnyOpaque.ueba`-bearing fixture (`random`); JSON codec test uses the
      // `AnyOpaque.json`-bearing fixture (`randomJson`). Each fixture matches its codec's native
      // any-field branch so round-trip avoids cross-format conversion and never needs a
      // `BaboonCodecContext.withFacade` ctx.
      val uebaFixture = makeFixture(definition, domain, evo, useJsonAny = false)
      val jsonFixture = makeFixture(definition, domain, evo, useJsonAny = true)
      codecs
        .filter(_.isActive(definition.id)).map {
          codec =>
            val body = codec.id match {
              case "Json" =>
                val assertions = jsonCodecAssertions(definition, srcRef)
                val crossRead  = crossLanguageJsonRead(definition, srcRef)
                val crossWrite = crossLanguageJsonWrite(definition, srcRef)

                q"""
               |func testJsonRoundTrip() throws {
               |    for _ in 0..<${target.generic.codecTestIterations.toString} {
               |        let rnd = $baboonRandomFactory.create()
               |        ${jsonFixture.shift(8).trim}
               |        ${assertions.shift(8).trim}
               |    }
               |}
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
               |func testUebaCompactRoundTrip() throws {
               |    for _ in 0..<${target.generic.codecTestIterations.toString} {
               |        let rnd = $baboonRandomFactory.create()
               |        ${uebaFixture.shift(8).trim}
               |        ${compactAssertions.shift(8).trim}
               |    }
               |}
               |
               |func testUebaIndexedRoundTrip() throws {
               |    for _ in 0..<${target.generic.codecTestIterations.toString} {
               |        let rnd = $baboonRandomFactory.create()
               |        ${uebaFixture.shift(8).trim}
               |        ${indexedAssertions.shift(8).trim}
               |    }
               |}
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
        }.toList.joinNN().shift(4).trim
    }

    private def makeFixture(
      definition: DomainMember.User,
      domain: Domain,
      evolution: BaboonEvolution,
      useJsonAny: Boolean,
    ): TextTree[SwValue] = {
      val swType          = typeTranslator.asSwType(definition.id, domain, evolution)
      val fixtureName     = typeTranslator.fixtureClassName(definition.id, domain, evolution)
      val randomMethod    = if (useJsonAny) "randomJson" else "random"
      val randomAllMethod = if (useJsonAny) "randomAllJson" else "randomAll"
      definition.defn match {
        case _: Typedef.Enum =>
          q"let fixture = $swType.all[rnd.nextIntRange($swType.all.count)]"
        case _: Typedef.Adt =>
          q"let fixtures = $fixtureName.$randomAllMethod(rnd)"
        case _ =>
          q"let fixture = $fixtureName.$randomMethod(rnd)"
      }
    }

    private def jsonCodecAssertions(definition: DomainMember.User, srcRef: SwType): TextTree[SwValue] = {
      val codecName = SwType(srcRef.pkg, s"${srcRef.name}_JsonCodec")
      definition.defn match {
        case _: Typedef.Adt =>
          q"""for fixture in fixtures {
             |    let encoded = $codecName.instance.encode($baboonCodecContext.defaultCtx, fixture)
             |    let decoded = try $codecName.instance.decode($baboonCodecContext.defaultCtx, encoded)
             |    XCTAssertEqual(decoded, fixture)
             |}""".stripMargin.trim
        case _ =>
          q"""let encoded = $codecName.instance.encode($baboonCodecContext.defaultCtx, fixture)
             |let decoded = try $codecName.instance.decode($baboonCodecContext.defaultCtx, encoded)
             |XCTAssertEqual(decoded, fixture)""".stripMargin
      }
    }

    private def uebaCodecAssertions(
      definition: DomainMember.User,
      srcRef: SwType,
      context: TextTree[SwValue],
    ): TextTree[SwValue] = {
      val codecName = SwType(srcRef.pkg, s"${srcRef.name}_UebaCodec")
      definition.defn match {
        case _: Typedef.Adt =>
          q"""for fixture in fixtures {
             |    let writer = $baboonBinTools.createWriter()
             |    $codecName.instance.encode($context, writer, fixture)
             |    let bytes = writer.toData()
             |    let reader = $baboonBinTools.createReader(bytes)
             |    let decoded = try $codecName.instance.decode($context, reader)
             |    XCTAssertEqual(decoded, fixture)
             |}""".stripMargin.trim
        case _ =>
          q"""let writer = $baboonBinTools.createWriter()
             |$codecName.instance.encode($context, writer, fixture)
             |let bytes = writer.toData()
             |let reader = $baboonBinTools.createReader(bytes)
             |let decoded = try $codecName.instance.decode($context, reader)
             |XCTAssertEqual(decoded, fixture)""".stripMargin
      }
    }

    private def crossLanguageJsonRead(definition: DomainMember.User, srcRef: SwType): TextTree[SwValue] = {
      val codecName = SwType(srcRef.pkg, s"${srcRef.name}_JsonCodec")
      val typeId    = definition.id.render
      val languages = List("cs", "scala", "rust", "typescript", "kotlin", "java", "dart")
      val readTests = languages.map {
        lang =>
          q"""func testCrossLanguageJsonReadFrom_$lang() throws {
             |    let url = URL(fileURLWithPath: crossLanguageFixturePath("$lang", "$typeId.json", "json-default"))
             |    guard FileManager.default.fileExists(atPath: url.path) else { return }
             |    let data = try Data(contentsOf: url)
             |    let json = try JSONSerialization.jsonObject(with: data, options: [.fragmentsAllowed])
             |    let decoded = try $codecName.instance.decode($baboonCodecContext.defaultCtx, json)
             |    XCTAssertNotNil(decoded)
             |}
             |""".stripMargin
      }
      readTests.joinNN()
    }

    private def crossLanguageJsonWrite(definition: DomainMember.User, srcRef: SwType): TextTree[SwValue] = {
      val codecName = SwType(srcRef.pkg, s"${srcRef.name}_JsonCodec")
      val typeId    = definition.id.render
      q"""func testCrossLanguageJsonWrite() throws {
         |    let rnd = $baboonRandomFactory.create()
         |    ${makeFixture(definition, domain, evo, useJsonAny = true).shift(4).trim}
         |    ${makeJsonWriteBody(definition, srcRef).shift(4).trim}
         |}
         |""".stripMargin
    }

    private def makeJsonWriteBody(definition: DomainMember.User, srcRef: SwType): TextTree[SwValue] = {
      val codecName = SwType(srcRef.pkg, s"${srcRef.name}_JsonCodec")
      val typeId    = definition.id.render
      val fixtureVar = definition.defn match {
        case _: Typedef.Adt => "fixtures.first!"
        case _              => "fixture"
      }
      q"""let outPath = crossLanguageFixturePath("swift", "$typeId.json", "json-default")
         |let outDir = URL(fileURLWithPath: outPath).deletingLastPathComponent()
         |try FileManager.default.createDirectory(at: outDir, withIntermediateDirectories: true)
         |let outFile = URL(fileURLWithPath: outPath)
         |let encoded = $codecName.instance.encode($baboonCodecContext.defaultCtx, $fixtureVar)
         |let jsonData = try JSONSerialization.data(withJSONObject: encoded, options: [.sortedKeys, .fragmentsAllowed])
         |try jsonData.write(to: outFile)""".stripMargin
    }

    private def crossLanguageUebaRead(definition: DomainMember.User, srcRef: SwType): TextTree[SwValue] = {
      val codecName = SwType(srcRef.pkg, s"${srcRef.name}_UebaCodec")
      val typeId    = definition.id.render
      val languages = List("cs", "scala", "rust", "typescript", "kotlin", "java", "dart")
      val readTests = languages.map {
        lang =>
          q"""func testCrossLanguageUebaReadFrom_$lang() throws {
             |    let url = URL(fileURLWithPath: crossLanguageFixturePath("$lang", "$typeId.ueba", "ueba-default"))
             |    guard FileManager.default.fileExists(atPath: url.path) else { return }
             |    let bytes = try Data(contentsOf: url)
             |    let reader = $baboonBinTools.createReader(bytes)
             |    let decoded = try $codecName.instance.decode($baboonCodecContext.compact, reader)
             |    XCTAssertNotNil(decoded)
             |}
             |""".stripMargin
      }
      readTests.joinNN()
    }

    private def crossLanguageUebaWrite(definition: DomainMember.User, srcRef: SwType): TextTree[SwValue] = {
      val codecName = SwType(srcRef.pkg, s"${srcRef.name}_UebaCodec")
      val typeId    = definition.id.render
      q"""func testCrossLanguageUebaWrite() throws {
         |    let rnd = $baboonRandomFactory.create()
         |    ${makeFixture(definition, domain, evo, useJsonAny = false).shift(4).trim}
         |    ${makeUebaWriteBody(definition, srcRef).shift(4).trim}
         |}
         |""".stripMargin
    }

    private def makeUebaWriteBody(definition: DomainMember.User, srcRef: SwType): TextTree[SwValue] = {
      val codecName = SwType(srcRef.pkg, s"${srcRef.name}_UebaCodec")
      val typeId    = definition.id.render
      val fixtureVar = definition.defn match {
        case _: Typedef.Adt => "fixtures.first!"
        case _              => "fixture"
      }
      q"""let outPath = crossLanguageFixturePath("swift", "$typeId.ueba", "ueba-default")
         |let outDir = URL(fileURLWithPath: outPath).deletingLastPathComponent()
         |try FileManager.default.createDirectory(at: outDir, withIntermediateDirectories: true)
         |let outFile = URL(fileURLWithPath: outPath)
         |let writer = $baboonBinTools.createWriter()
         |$codecName.instance.encode($baboonCodecContext.compact, writer, $fixtureVar)
         |try writer.toData().write(to: outFile)""".stripMargin
    }
  }
}
