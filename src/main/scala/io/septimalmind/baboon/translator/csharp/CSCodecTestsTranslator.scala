package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.typer.model.{Domain, DomainMember, Typedef}
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSCodecTestsTranslator {
  def translate(
                 definition: DomainMember.User,
                 csRef: CSValue.CSType,
                 srcRef: CSValue.CSType,
                 domain: Domain
               ): TextTree[CSValue]
}

object CSCodecTestsTranslator {
  final class Impl(
                    codecs: Set[CSCodecTranslator],
                    typeTranslator: CSTypeTranslator,
                    logger: BLogger
                  ) extends CSCodecTestsTranslator {
    override def translate(
                            definition: DomainMember.User,
                            csRef: CSValue.CSType,
                            srcRef: CSValue.CSType,
                            domain: Domain,
                          ): TextTree[CSValue] = {
      val testClassName = CSValue.CSType(srcRef.pkg, s"${srcRef.name}_Codec_Test", srcRef.fq)

      q"""using NUnit.Framework;
         |using AutoFixture;
         |using System.IO;
         |using System;
         |
         |[TestFixture]
         |public class $testClassName
         |{
         |  private Fixture fixture;
         |
         |  ${testFields(definition, csRef, srcRef)}
         |
         |  public $testClassName()
         |  {
         |    fixture = new Fixture();
         |    fixture.Customize(new ImmutableCollectionsCustomization());
         |  }
         |
         |  [OneTimeSetUp]
         |  public void Setup()
         |  {
         |    ${fieldsInitialization(definition, csRef, srcRef, domain)}
         |  }
         |
         |  ${tests(definition, srcRef, domain)}
         |}
         |""".stripMargin
    }

    private def testFields(
                            definition: DomainMember.User,
                            csRef: CSValue.CSType,
                            srcRef: CSValue.CSType
                          ): TextTree[CSValue] = {
      definition.defn match {
        case _: Typedef.Foreign => q"private ${csRef.name} ${srcRef.name.toLowerCase};"
        case Typedef.Adt(root, members) =>
          members.map(_.name.name)
            .map(n => q"private ${root.name.name} ${n.toLowerCase};")
            .toList.join("\n").shift(2).trim
        case _ => q"private ${srcRef.name} ${srcRef.name.toLowerCase};"
      }
    }

    private def fieldsInitialization(
                                      definition: DomainMember.User,
                                      csRef: CSValue.CSType,
                                      srcRef: CSValue.CSType,
                                      domain: Domain
                                    ): TextTree[CSValue] = {
      definition.defn match {
        case _: Typedef.Foreign =>
          q"${srcRef.name.toLowerCase} = fixture.Create<${csRef.name}>();"
        case Typedef.Adt(root, members) =>
          val adtMembersNamespace = typeTranslator.toCsPkg(domain.id, domain.version).parts.mkString(".") + s".${root.name.name.toLowerCase}"
          members.map { member =>
            q"""fixture.Register<${root.name.name}>(() => fixture.Create<$adtMembersNamespace.${member.name.name}>());
               |${member.name.name.toLowerCase} = fixture.Create<${root.name.name}>();
               |""".stripMargin
          }.toList.join("\n").shift(4).trim
        case _ =>
          q"${srcRef.name.toLowerCase} = fixture.Create<${srcRef.name}>();"
      }
    }

    private def tests(
                       definition: DomainMember.User,
                       srcRef: CSValue.CSType,
                       domain: Domain,
                     ): TextTree[CSValue] = {
      codecs.map {
        case jsonCodec: CSNSJsonCodecGenerator =>
          q"""[Test]
             |public void jsonCodecTest()
             |{
             |  ${jsonCodecAssertions(jsonCodec, definition, srcRef, domain)}
             |}
             |""".stripMargin
        case uebaCodec: CSUEBACodecGenerator =>
          q"""[Test]
             |public void uebaCodecTest()
             |{
             |  ${uebaCodecAssertions(uebaCodec, definition, srcRef, domain)}
             |}
             |""".stripMargin
        case unknown =>
          logger.message(
            s"Tests generating for ${unknown.codecName(srcRef)} codec of type $srcRef is not supported"
          )
          q""
      }.toList.join("\n").shift(2).trim
    }

    private def jsonCodecAssertions(
                                     codec: CSNSJsonCodecGenerator,
                                     definition: DomainMember.User,
                                     srcRef: CSValue.CSType,
                                     domain: Domain
                                   ): TextTree[CSValue] = {
      definition.defn match {
        case Typedef.Adt(root, members) =>
          members.map { member =>
            val typeRef = typeTranslator.toCsTypeRefNoDeref(root, domain)
            val codecName = codec.codecName(typeRef)
            val fieldName = member.name.name.toLowerCase
            val serialized = s"${fieldName}_json"
            val decoded = s"${fieldName}_decoded"
            q"""var $serialized = $codecName.Instance.Encode($fieldName);
               |var $decoded = $codecName.Instance.Decode($serialized);
               |Assert.AreEqual($fieldName, $decoded);
               |""".stripMargin
          }.toList.join("\n").shift(4).trim
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

    private def uebaCodecAssertions(
                                     codec: CSUEBACodecGenerator,
                                     definition: DomainMember.User,
                                     srcRef: CSValue.CSType,
                                     domain: Domain
                                   ): TextTree[CSValue] = {
      definition.defn match {
        case Typedef.Adt(root, members) =>
          val assertions = members.map { member =>
            val typeRef = typeTranslator.toCsTypeRefNoDeref(root, domain)
            val codecName = codec.codecName(typeRef)
            val fieldName = member.name.name.toLowerCase
            val serialized = s"${fieldName}_bytes"
            val decoded = s"${fieldName}_decoded"
            val readStream = s"${fieldName}_readMemoryStream"
            val binaryReader = s"${fieldName}_binaryReader"
            q"""using (BinaryWriter binaryWriter = new BinaryWriter(writeMemoryStream))
               |{
               |  $codecName.Instance.Encode(binaryWriter, $fieldName);
               |}
               |writeMemoryStream.Flush();
               |var $serialized = writeMemoryStream.GetBuffer();
               |var $readStream = new MemoryStream($serialized);
               |var $binaryReader = new BinaryReader($readStream);
               |var $decoded = $codecName.Instance.Decode($binaryReader);
               |Assert.AreEqual($fieldName, $decoded);
               |""".stripMargin
          }.toList.join("\n").shift(6).trim

          q"""using (MemoryStream writeMemoryStream = new MemoryStream())
             |{
             |  $assertions
             |}
             |""".stripMargin.shift(4).trim
        case _ =>
          val codecName = codec.codecName(srcRef)
          val fieldName = srcRef.name.toLowerCase
          val serialized = s"${fieldName}_bytes"
          val decoded = s"${fieldName}_decoded"
          q"""using (MemoryStream writeMemoryStream = new MemoryStream())
             |{
             |  using (BinaryWriter binaryWriter = new BinaryWriter(writeMemoryStream))
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
