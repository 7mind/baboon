package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
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
                   compilerOptions: CompilerOptions)
      extends CSCodecTestsTranslator {
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
        CSValue
          .CSType(srcRef.pkg, s"${codecTestName}__Codec_Test", srcRef.fq)
          .asName

      definition match {
        case d if enquiries.hasForeignType(d, domain)         => None
        case d if enquiries.isRecursiveTypedef(d, domain)     => None
        case d if d.defn.isInstanceOf[Typedef.NonDataTypedef] => None
        case _ =>
          val init = fieldsInitialization(definition, srcRef, domain, evo)
          val testClass =
            q"""[$nunitTestFixture]
               |public class $testClassName
               |{
               |  #nullable disable
               |  ${testFields(definition, srcRef, domain, evo).shift(2).trim}
               |  private $baboonCodecContext context = $baboonCodecContext.Default;
               |
               |  [$nunitOneTimeSetUp]
               |  public void Setup()
               |  {
               |      ${init.shift(4).trim}
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
                           domain: Domain,
                           evo: BaboonEvolution): TextTree[CSValue] = {
      definition.defn match {
        case adt: Typedef.Adt =>
          adt
            .dataMembers(domain)
            .map(_.name.name)
            .map(
              n =>
                q"private ${typeTranslator.asCsType(adt.id, domain, evo)} ${n.toLowerCase};"
            )
            .toList
            .join("\n")
            .shift(2)
            .trim
        case _ => q"private $srcRef ${srcRef.name.toLowerCase};"
      }
    }

    private def fieldsInitialization(
      definition: DomainMember.User,
      srcRef: CSValue.CSType,
      domain: Domain,
      evolution: BaboonEvolution
    ): TextTree[CSValue] = {
      definition.defn match {
        case adt: Typedef.Adt =>
          adt
            .dataMembers(domain)
            .map { member =>
              if (compilerOptions.csUseCompactAdtForm) {
                q"${member.name.name.toLowerCase} = ${typeTranslator.asCsType(adt.id, domain, evolution)}.${member.name.name}.Random();"
              } else {
                q"${member.name.name.toLowerCase} =  ${typeTranslator.asCsType(member, domain, evolution)}.Random();"
              }
            }
            .toList
            .join("\n")
            .shift(4)
            .trim
        case enum: Typedef.Enum =>
          q"${srcRef.name.toLowerCase} = $testValuesGenerator.NextRandomEnum<${enum.id.name.name}>();"
        case _ =>
          q"${srcRef.name.toLowerCase} = ${typeTranslator.asCsType(definition.id, domain, evolution)}.Random();"
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
              q"""var $serialized = $codecName.Instance.Encode(this.context, $fieldName);
                 |var $decoded = $codecName.Instance.Decode(this.context, $serialized);
                 |Assert.AreEqual($fieldName, $decoded);
                 |""".stripMargin
            }
            .toList
            .join("\n")
        case _ =>
          val codecName = codec.codecName(srcRef)
          val fieldName = srcRef.name.toLowerCase
          val serialized = s"${fieldName}_json"
          val decoded = s"${fieldName}_decoded"
          q"""var $serialized = $codecName.Instance.Encode(this.context, $fieldName);
             |var $decoded = $codecName.Instance.Decode(this.context, $serialized);
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
              q"""using ($memoryStream writeMemoryStream = new $memoryStream())
                 |{
                 |  using ($binaryWriter binaryWriter = new $binaryWriter(writeMemoryStream))
                 |  {
                 |    $codecName.Instance.Encode(this.context, binaryWriter, $fieldName);
                 |  }
                 |  writeMemoryStream.Flush();
                 |  var $serialized = writeMemoryStream.GetBuffer();
                 |  var $readStream = new MemoryStream($serialized);
                 |  var $binaryReader = new BinaryReader($readStream);
                 |  var $decoded = $codecName.Instance.Decode(this.context, $binaryReader);
                 |  Assert.AreEqual($fieldName, $decoded);
                 |}
                 |""".stripMargin
            }
            .toList
            .join("\n")
        case _ =>
          val codecName = codec.codecName(srcRef)
          val fieldName = srcRef.name.toLowerCase
          val serialized = s"${fieldName}_bytes"
          val decoded = s"${fieldName}_decoded"
          q"""using ($memoryStream writeMemoryStream = new $memoryStream())
             |{
             |  using ($binaryWriter binaryWriter = new $binaryWriter(writeMemoryStream))
             |  {
             |    $codecName.Instance.Encode(this.context, binaryWriter, $fieldName);
             |  }
             |  writeMemoryStream.Flush();
             |
             |  var $serialized = writeMemoryStream.GetBuffer();
             |  var readMemoryStream = new MemoryStream($serialized);
             |  var binaryReader = new BinaryReader(readMemoryStream);
             |  var $decoded = $codecName.Instance.Decode(this.context, binaryReader);
             |  Assert.AreEqual($fieldName, $decoded);
             |}
             |""".stripMargin
      }
    }
  }
}
