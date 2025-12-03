package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.PyType
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember, Typedef}
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait PyCodecTestTranslator {
  def translate(defn: DomainMember.User, pyRef: PyType, srcRef: PyType): Option[TextTree[PyValue]]
}

object PyCodecTestTranslator {
  final class PyCodecTestTranslatorImpl(
    fixtureTranslator: PyCodecFixtureTranslator,
    typeTranslator: PyTypeTranslator,
    codecs: Set[PyCodecTranslator],
    enquiries: BaboonEnquiries,
    evolution: BaboonEvolution,
    pyFileTools: PyFileTools,
    pyTarget: PyTarget,
    logger: BLogger,
    domain: Domain,
  ) extends PyCodecTestTranslator {
    override def translate(
      defn: DomainMember.User,
      pyRef: PyType,
      srcRef: PyType,
    ): Option[TextTree[PyValue]] = {
      val isLatestVersion = domain.version == evolution.latest

      defn match {
        case d if enquiries.hasForeignType(d, domain)         => None
        case d if enquiries.isRecursiveTypedef(d, domain)     => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _ if !isLatestVersion                            => None
        case _ =>
          val testClass =
            q"""class Test_${srcRef.name}_Tests($pyTestCase):
               |    ${makeTest(defn, srcRef)}
               |""".stripMargin
          Some(testClass)
      }
    }

    private def makeTest(defn: DomainMember.User, srcRef: PyType): TextTree[PyValue] = {
      val fixture = makeFixture(defn)
      codecs.map {
        case jsonCodec: PyJsonCodecGenerator =>
          val codec = jsonCodec.codecType(defn.id)
          val body  = jsonCodecAssertions(defn)
          q"""def test_json_codec(self):
             |    for _ in range(${pyTarget.generic.codecTestIterations.toString}):
             |        self.json_codec_test_impl()
             |
             |def test_load_json_produced_by_cs_codecs(self):
             |    self.cs_json_test("default")
             |
             |def cs_json_test(self, clue):
             |    tpeid = "${defn.id.render}"
             |    with open(f"../target/cs/json-{clue}/{tpeid}.json", encoding="utf-8") as f:
             |        cs_json = f.read()
             |        decoded = $codec.instance().decode(cs_json)
             |        self.json_compare(decoded)
             |    
             |def json_codec_test_impl(self):
             |    ${fixture.shift(4).trim}
             |    ${body.shift(4).trim}    
             |
             |def json_compare(self, fixture):
             |    fixtureJson    = $codec.instance().encode(fixture)
             |    fixtureDecoded = $codec.instance().decode(fixtureJson)
             |    self.assertEqual(fixture, fixtureDecoded)
             |""".stripMargin
        case uebaCodec: PyUEBACodecGenerator =>
          val codec = uebaCodec.codecType(defn.id)
          val body  = uebaCodecAssertions(defn)
          q"""def test_ueba_codec(self):
             |    for _ in range(${pyTarget.generic.codecTestIterations.toString}):
             |        self.ueba_codec_test_impl($baboonCodecContext.default())
             |
             |def ueba_codec_test_impl(self, context):
             |    ${fixture.shift(4).trim}
             |    ${body.shift(4).trim}
             |
             |def test_ueba_produced_by_cs_codecs(self):
             |    self.cs_ueba_test($baboonCodecContext.indexed(), "indexed")
             |    self.cs_ueba_test($baboonCodecContext.compact(), "compact")
             |
             |def cs_ueba_test(self, context, clue):
             |    tpeid = "${defn.id.render}"
             |    with open(f"../target/cs/ueba-{clue}/{tpeid}.uebin", "rb") as f:
             |        cs_uebin = f.read()
             |        memory_stream = $pyBytesIO()
             |        input_stream = $baboonLEDataInputStream(memory_stream)
             |        memory_stream.write(cs_uebin)
             |        memory_stream.seek(0)
             |        decoded = $codec.instance().decode(context, input_stream)
             |        self.ueba_compare(context, decoded)
             |
             |def ueba_compare(self, context, obj):
             |    memory_stream = $pyBytesIO()
             |    output_stream = $baboonLEDataOutputStream(memory_stream)
             |    $codec.instance().encode(context, output_stream, obj)
             |    memory_stream.seek(0)
             |    input_stream = $baboonLEDataInputStream(memory_stream)
             |    decoded = $codec.instance().decode(context, input_stream)
             |    self.assertEqual(obj, decoded)
             |
             |""".stripMargin
        case unknown =>
          logger.message(s"Cannot create codec tests (${unknown.id}) for unsupported type $srcRef")
          q""
      }.toList.map(_.stripMargin.trim).joinNN().shift(4).trim
    }

    private def jsonCodecAssertions(defn: DomainMember.User): TextTree[PyValue] = {
      defn.defn match {
        case _: Typedef.Adt =>
          q"""for fixture in fixtures:
             |    self.json_compare(fixture)
             |""".stripMargin
        case _ => q"self.json_compare(fixture)"
      }
    }

    private def uebaCodecAssertions(defn: DomainMember.User): TextTree[PyValue] = {
      defn.defn match {
        case _: Typedef.Adt =>
          q"""for fixture in fixtures:
             |    self.ueba_compare(context, fixture)
             |""".stripMargin.trim
        case _ =>
          q"self.ueba_compare(context, fixture)"
      }
    }

    private def makeFixture(defn: DomainMember.User): TextTree[PyValue] = {
      defn.defn match {
        case _: Typedef.Enum =>
          val enumTpe = typeTranslator.asPyType(defn.id, domain, evolution, pyFileTools.definitionsBasePkg)
          q"fixture = $baboonFixture.next_random_enum($enumTpe)"
        case _: Typedef.Adt =>
          val fixtureType = fixtureTranslator.fixtureType(defn.id)
          q"fixtures = $fixtureType.random_all()"
        case _ =>
          val fixtureType = fixtureTranslator.fixtureType(defn.id)
          q"fixture = $fixtureType.random()"
      }
    }
  }
}
