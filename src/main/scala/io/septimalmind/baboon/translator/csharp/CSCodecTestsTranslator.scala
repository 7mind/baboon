package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.typer.model.{Domain, DomainMember, Field, TypeId, TypeRef, Typedef}
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

import scala.annotation.tailrec

trait CSCodecTestsTranslator {
  def translate(
                 definition: DomainMember.User,
                 csRef: CSValue.CSType,
                 srcRef: CSValue.CSType,
                 domain: Domain
               ): Option[TextTree[CSValue]]
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
                          ): Option[TextTree[CSValue]] = {
      val testClassName = CSValue.CSType(srcRef.pkg, s"${srcRef.name}_Codec_Test", srcRef.fq)

      if (hasForeignType(definition, domain)) None
      else {
        val testClass =
          q"""using NUnit.Framework;
             |using AutoFixture;
             |using System.IO;
             |using System;
             |using Baboon.Runtime.Shared;
             |
             |[TestFixture]
             |public class $testClassName
             |{
             |  #nullable disable
             |
             |  private Fixture fixture;
             |
             |  ${testFields(definition, csRef, srcRef)}
             |
             |  public $testClassName()
             |  {
             |    fixture = new Fixture();
             |    fixture.Customize(new ImmutableCollectionsCustomization());
             |    fixture.Customizations.Add(new TruncatedRandomDateTimeSequenceGenerator());
             |    fixture.Customizations.Add(new EnumDictionaryBuilder());
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
        Some(testClass)
      }
    }

    private def testFields(
                            definition: DomainMember.User,
                            csRef: CSValue.CSType,
                            srcRef: CSValue.CSType
                          ): TextTree[CSValue] = {
      definition.defn match {
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
          }.toList.join("\n").shift(6).trim
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
          members.map { member =>
            val typeRef = typeTranslator.toCsTypeRefNoDeref(root, domain)
            val codecName = codec.codecName(typeRef)
            val fieldName = member.name.name.toLowerCase
            val serialized = s"${fieldName}_bytes"
            val decoded = s"${fieldName}_decoded"
            val readStream = s"${fieldName}_readMemoryStream"
            val binaryReader = s"${fieldName}_binaryReader"
            q"""using (MemoryStream writeMemoryStream = new MemoryStream())
               |{
               |  using (BinaryWriter binaryWriter = new BinaryWriter(writeMemoryStream))
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
          }.toList.join("\n").shift(4).trim
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

    private def hasForeignType(definition: DomainMember.User, domain: Domain): Boolean = {
      @tailrec
      def collectForeignType(toCheck: List[Typedef], foreignType: Option[TypeId]): Option[TypeId] = {
        (toCheck, foreignType) match {
          case (_, Some(tpe)) => Some(tpe)
          case (Nil, fType) => fType
          case (head :: tail, None) =>
            head match {
              case dto: Typedef.Dto =>
                val fieldsTypes = dto.fields.map(_.tpe)
                val moreToCheck = fieldsTypes.flatMap {
                  case TypeRef.Scalar(id) =>
                    List(domain.defs.meta.nodes(id) match {
                      case _: DomainMember.Builtin => None
                      case u: DomainMember.User => Some(u.defn)
                    }).flatten
                  case TypeRef.Constructor(_, args) =>
                    args.map(_.id).map(domain.defs.meta.nodes(_)).toList.flatMap {
                      case _: DomainMember.Builtin => None
                      case u: DomainMember.User => Some(u.defn)
                    }
                }
                collectForeignType(tail ++ moreToCheck, foreignType)
              case adt: Typedef.Adt =>
                val dtos = adt.members
                  .map(tpeId => domain.defs.meta.nodes(tpeId)).toList
                  .collect { case DomainMember.User(_, dto@Typedef.Dto(_, _), _) => dto }
                collectForeignType(tail ++ dtos, foreignType)
              case f: Typedef.Foreign => collectForeignType(tail, Some(f.id))
              case _: Typedef.Enum => collectForeignType(tail, foreignType)
            }
        }
      }

      collectForeignType(List(definition.defn), None).nonEmpty
    }
  }
}
