package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSCodecTestsTranslator {
  def translate(definition: DomainMember.User,
                csRef: CSValue.CSType,
                srcRef: CSValue.CSType,
                domain: Domain,
                evo: BaboonEvolution): Option[TextTree[CSValue]]
}

object CSCodecTestsTranslator {
  final class Impl(codecs: Set[CSCodecTranslator],
                   typeTranslator: CSTypeTranslator,
                   logger: BLogger,
                   enquiries: BaboonEnquiries,
  ) extends CSCodecTestsTranslator {
    override def translate(definition: DomainMember.User,
                           csRef: CSValue.CSType,
                           srcRef: CSValue.CSType,
                           domain: Domain,
                           evo: BaboonEvolution,
    ): Option[TextTree[CSValue]] = {

      val codecTestName = definition.id.owner match {
        case Owner.Toplevel => srcRef.name
        case Owner.Adt(id)  => s"${id.name.name}__${srcRef.name}"
        case Owner.Ns(path) =>
          s"${path.map(_.name).mkString("_")}__${srcRef.name}"
      }

      val testClassName =
        CSValue.CSType(srcRef.pkg, s"${codecTestName}__Codec_Test", srcRef.fq)

      definition match {
        case d if enquiries.hasForeignType(d, domain)         => None
        case d if enquiries.isRecursiveTypedef(d, domain)     => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _ =>
          val testClass =
            q"""[${nunitTestFixture}]
               |public class $testClassName
               |{
               |  #nullable disable
               |
               |  private ${autofixtureFixture} fixture;
               |
               |  ${testFields(definition, srcRef, domain)}
               |
               |  public $testClassName()
               |  {
               |    fixture = new ${autofixtureFixture}();
               |    fixture.Customize(new ${autofixtureImmutableCollectionsCustomization}());
               |    fixture.Customizations.Add(new ${baboonTest_TruncatedRandomDateTimeSequenceGenerator}());
               |    fixture.Customizations.Add(new ${baboonTest_EnumDictionaryBuilder}());
               |  }
               |
               |  [${nunitOneTimeSetUp}]
               |  public void Setup()
               |  {
               |    ${fieldsInitialization(definition, srcRef, domain, evo)}
               |  }
               |
               |  ${tests(definition, srcRef, domain, evo)}
               |}
               |""".stripMargin
          Some(testClass)
      }
    }

    private def testFields(definition: DomainMember.User,
                           srcRef: CSValue.CSType,
                           domain: Domain): TextTree[CSValue] = {
      definition.defn match {
        case adt: Typedef.Adt =>
          adt
            .dataMembers(domain)
            .map(_.name.name)
            .map(n => q"private ${adt.id.name.name} ${n.toLowerCase};")
            .toList
            .join("\n")
            .shift(2)
            .trim
        case _ => q"private ${srcRef.name} ${srcRef.name.toLowerCase};"
      }
    }

    private def fieldsInitialization(
      definition: DomainMember.User,
      srcRef: CSValue.CSType,
      domain: Domain,
      evo: BaboonEvolution
    ): TextTree[CSValue] = {
      definition.defn match {
        case adt: Typedef.Adt =>
          val adtMembersNamespace = typeTranslator
            .toCsPkg(domain.id, domain.version, evo)
            .parts
            .mkString(".") + s".${typeTranslator.adtNsName(adt.id)}"

          adt
            .dataMembers(domain)
            .map { member =>
              q"""fixture.Register<${adt.id.name.name}>(() => fixture.Create<$adtMembersNamespace.${member.name.name}>());
               |${member.name.name.toLowerCase} = fixture.Create<${adt.id.name.name}>();
               |""".stripMargin
            }
            .toList
            .join("\n")
            .shift(4)
            .trim
        case _ =>
          q"${srcRef.name.toLowerCase} = fixture.Create<${srcRef.name}>();"
      }
    }

    private def tests(definition: DomainMember.User,
                      srcRef: CSValue.CSType,
                      domain: Domain,
                      evo: BaboonEvolution,
    ): TextTree[CSValue] = {
      codecs
        .map {
          case jsonCodec: CSNSJsonCodecGenerator =>
            q"""[Test]
             |public void jsonCodecTest()
             |{
             |    ${jsonCodecAssertions(
                 jsonCodec,
                 definition,
                 srcRef,
                 domain,
                 evo
               ).shift(4).trim}
             |}
             |""".stripMargin
          case uebaCodec: CSUEBACodecGenerator =>
            q"""[Test]
             |public void uebaCodecTest()
             |{
             |    ${uebaCodecAssertions(
                 uebaCodec,
                 definition,
                 srcRef,
                 domain,
                 evo
               ).shift(4).trim}
             |}
             |""".stripMargin
          case unknown =>
            logger.message(
              s"Tests generating for ${unknown.codecName(srcRef)} codec of type $srcRef is not supported"
            )
            q""
        }
        .toList
        .join("\n")
        .shift(2)
        .trim
    }

    private def jsonCodecAssertions(codec: CSNSJsonCodecGenerator,
                                    definition: DomainMember.User,
                                    srcRef: CSValue.CSType,
                                    domain: Domain,
                                    evo: BaboonEvolution,
    ): TextTree[CSValue] = {
      definition.defn match {
        case adt: Typedef.Adt =>
          adt
            .dataMembers(domain)
            .map { member =>
              val typeRef =
                typeTranslator.toCsTypeRefNoDeref(adt.id, domain, evo)
              val codecName = codec.codecName(typeRef)
              val fieldName = member.name.name.toLowerCase
              val serialized = s"${fieldName}_json"
              val decoded = s"${fieldName}_decoded"
              q"""var $serialized = $codecName.Instance.Encode($fieldName);
               |var $decoded = $codecName.Instance.Decode($serialized);
               |Assert.AreEqual($fieldName, $decoded);
               |""".stripMargin
            }
            .toList
            .join("\n")
            .shift(6)
            .trim
        case _ =>
          val codecName = codec.codecName(srcRef)
          val fieldName = srcRef.name.toLowerCase
          val serialized = s"${fieldName}_json"
          val decoded = s"${fieldName}_decoded"
          q"""var $serialized = $codecName.Instance.Encode($fieldName);
             |var $decoded = $codecName.Instance.Decode($serialized);
             |Assert.AreEqual($fieldName, $decoded);
             |""".stripMargin
      }
    }

    private def uebaCodecAssertions(codec: CSUEBACodecGenerator,
                                    definition: DomainMember.User,
                                    srcRef: CSValue.CSType,
                                    domain: Domain,
                                    evo: BaboonEvolution,
    ): TextTree[CSValue] = {
      definition.defn match {
        case adt: Typedef.Adt =>
          adt
            .dataMembers(domain)
            .map { member =>
              val typeRef =
                typeTranslator.toCsTypeRefNoDeref(adt.id, domain, evo)
              val codecName = codec.codecName(typeRef)
              val fieldName = member.name.name.toLowerCase
              val serialized = s"${fieldName}_bytes"
              val decoded = s"${fieldName}_decoded"
              val readStream = s"${fieldName}_readMemoryStream"
              val binaryReader = s"${fieldName}_binaryReader"
              q"""using (${memoryStream} writeMemoryStream = new ${memoryStream}())
               |{
               |  using ($binaryWriter binaryWriter = new $binaryWriter(writeMemoryStream))
               |  {
               |   $codecName.Instance.Encode(binaryWriter, $fieldName);
               |  }
               |  writeMemoryStream.Flush();
               |  var $serialized = writeMemoryStream.GetBuffer();
               |  var $readStream = new MemoryStream($serialized);
               |  var $binaryReader = new BinaryReader($readStream);
               |  var $decoded = $codecName.Instance.Decode($binaryReader);
               |  Assert.AreEqual($fieldName, $decoded);
               |}
               |""".stripMargin
            }
            .toList
            .join("\n")
            .shift(4)
            .trim
        case _ =>
          val codecName = codec.codecName(srcRef)
          val fieldName = srcRef.name.toLowerCase
          val serialized = s"${fieldName}_bytes"
          val decoded = s"${fieldName}_decoded"
          q"""using (${memoryStream} writeMemoryStream = new ${memoryStream}())
             |{
             |  using (${binaryWriter} binaryWriter = new ${binaryWriter}(writeMemoryStream))
             |  {
             |    $codecName.Instance.Encode(binaryWriter, $fieldName);
             |  }
             |  writeMemoryStream.Flush();
             |
             |  var $serialized = writeMemoryStream.GetBuffer();
             |  var readMemoryStream = new MemoryStream($serialized);
             |  var binaryReader = new BinaryReader(readMemoryStream);
             |  var $decoded = $codecName.Instance.Decode(binaryReader);
             |  Assert.AreEqual($fieldName, $decoded);
             |}
             |""".stripMargin.shift(2).trim
      }
    }

  }
}
