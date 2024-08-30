package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

import scala.annotation.tailrec

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
      }

      val testClassName =
        CSValue.CSType(srcRef.pkg, s"${codecTestName}__Codec_Test", srcRef.fq)

      definition match {
        case d if hasForeignType(d, domain)                   => None
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

    private def hasForeignType(definition: DomainMember.User,
                               domain: Domain): Boolean = {
      def processFields(foreignType: Option[TypeId],
                        tail: List[Typedef],
                        f: List[Field]) = {
        val fieldsTypes = f.map(_.tpe)
        val moreToCheck = fieldsTypes.flatMap {
          case TypeRef.Scalar(id) =>
            List(domain.defs.meta.nodes(id) match {
              case _: DomainMember.Builtin => None
              case u: DomainMember.User    => Some(u.defn)
            }).flatten
          case TypeRef.Constructor(_, args) =>
            args
              .map(_.id)
              .map(domain.defs.meta.nodes(_))
              .toList
              .flatMap {
                case _: DomainMember.Builtin => None
                case u: DomainMember.User    => Some(u.defn)
              }
        }
        collectForeignType(tail ++ moreToCheck, foreignType)
      }

      @tailrec
      def collectForeignType(toCheck: List[Typedef],
                             foreignType: Option[TypeId]): Option[TypeId] = {
        (toCheck, foreignType) match {
          case (_, Some(tpe)) => Some(tpe)
          case (Nil, fType)   => fType
          case (head :: tail, None) =>
            head match {
              case dto: Typedef.Dto =>
                processFields(foreignType, tail, dto.fields)
              case c: Typedef.Contract =>
                processFields(foreignType, tail, c.fields)
              case adt: Typedef.Adt =>
                val dtos = adt.members
                  .map(tpeId => domain.defs.meta.nodes(tpeId))
                  .toList
                  .collect {
                    case DomainMember.User(_, dto: Typedef.Dto, _)      => dto
                    case DomainMember.User(_, dto: Typedef.Contract, _) => dto
                  }
                collectForeignType(tail ++ dtos, foreignType)
              case f: Typedef.Foreign => collectForeignType(tail, Some(f.id))
              case _: Typedef.Enum    => collectForeignType(tail, foreignType)
            }
        }
      }

      collectForeignType(List(definition.defn), None).nonEmpty
    }
  }
}
