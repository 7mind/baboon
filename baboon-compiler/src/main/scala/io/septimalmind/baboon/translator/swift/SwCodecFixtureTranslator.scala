package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait SwCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[SwValue]]
  def fixtureTpe(definition: DomainMember.User): Option[TextTree[SwValue]]
}

object SwCodecFixtureTranslator {
  final class Impl(
    translator: SwTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends SwCodecFixtureTranslator {

    override def translate(
      definition: DomainMember.User
    ): Option[TextTree[SwValue]] = {
      definition.defn match {
        case _ if enquiries.hasForeignType(definition, domain)     => None
        case _ if enquiries.isRecursiveTypedef(definition, domain) => None
        case dto: Typedef.Dto                                      => Some(doTranslateDto(dto))
        case adt: Typedef.Adt                                      => Some(doTranslateAdt(adt))
        case _: Typedef.Contract                                   => None
        case _: Typedef.Enum                                       => None
        case _: Typedef.Foreign                                    => None
        case _: Typedef.Service                                    => None
      }
    }

    override def fixtureTpe(definition: DomainMember.User): Option[TextTree[SwValue]] = {
      defnFixtureId(definition)
    }

    private def defnFixtureId(definition: DomainMember): Option[TextTree[SwValue]] = {
      definition match {
        case _: DomainMember.Builtin => None
        case u: DomainMember.User =>
          u.defn match {
            case _ if enquiries.hasForeignType(u, domain)     => None
            case _ if enquiries.isRecursiveTypedef(u, domain) => None
            case _: Typedef.Contract                          => None
            case _: Typedef.Enum                              => None
            case _: Typedef.Foreign                           => None
            case _: Typedef.Service                           => None
            case dto: Typedef.Dto                             => Some(fixtureTpeName(dto.id))
            case adt: Typedef.Adt                             => Some(fixtureTpeName(adt.id))
          }
      }
    }

    private def fixtureTpeName(id: TypeId.User): TextTree[SwValue] = {
      q"${translator.fixtureClassName(id, domain, evo)}"
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[SwValue] = {
      val fullType = translator.toSwTypeRefKeepForeigns(dto.id, domain, evo)

      val generatedFields = dto.fields.map { f =>
        q"${translator.escapeSwiftKeyword(f.name.name)}: ${genType(f.tpe)}"
      }

      q"""public class ${fixtureTpeName(dto.id)} {
         |    public static func random(_ rnd: $baboonRandom) -> $fullType {
         |        return $fullType(
         |            ${generatedFields.join(",\n").shift(12).trim}
         |        )
         |    }
         |}
         |""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[SwValue] = {
      val fullType = translator.toSwTypeRefKeepForeigns(adt.id, domain, evo)
      val members = adt.members.toList
        .flatMap(domain.defs.meta.nodes.get)
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val membersFixtures    = members.sortBy(_.id.toString).map(doTranslateDto)
      val membersDirectCalls = members.sortBy(_.id.toString).map { dto =>
        val caseName = dto.id.name.name.head.toLower.toString + dto.id.name.name.tail
        q".$caseName(${fixtureTpeName(dto.id)}.random(rnd))"
      }

      q"""${membersFixtures.joinN()}
         |
         |public class ${fixtureTpeName(adt.id)} {
         |    public static func random(_ rnd: $baboonRandom) -> $fullType {
         |        return rnd.oneOf(randomAll(rnd))
         |    }
         |
         |    public static func randomAll(_ rnd: $baboonRandom) -> [$fullType] {
         |        return [
         |            ${membersDirectCalls.join(",\n").shift(12).trim}
         |        ]
         |    }
         |}
         |""".stripMargin
    }

    private def genType(tpe: TypeRef): TextTree[SwValue] = {
      def gen(tpe: TypeRef): TextTree[SwValue] = {
        tpe match {
          case tpe: TypeRef.Scalar => genScalar(tpe)
          case TypeRef.Constructor(id, args) =>
            id match {
              case Builtins.lst => q"rnd.mkList { ${gen(args.head)} }"
              case Builtins.set => q"rnd.mkSet { ${gen(args.head)} }"
              case Builtins.map => q"rnd.mkMap({ ${gen(args(0))} }, { ${gen(args(1))} })"
              case Builtins.opt => q"rnd.mkOptional { ${gen(args.head)} }"
              case t            => throw new IllegalArgumentException(s"Unexpected collection type: $t")
            }
        }
      }

      gen(tpe)
    }

    private def genScalar(tpe: TypeRef.Scalar): TextTree[SwValue] = {
      tpe.id match {
        case TypeId.Builtins.i08 => q"rnd.nextI08()"
        case TypeId.Builtins.i16 => q"rnd.nextI16()"
        case TypeId.Builtins.i32 => q"rnd.nextI32()"
        case TypeId.Builtins.i64 => q"rnd.nextI64()"

        case TypeId.Builtins.u08 => q"rnd.nextU08()"
        case TypeId.Builtins.u16 => q"rnd.nextU16()"
        case TypeId.Builtins.u32 => q"rnd.nextU32()"
        case TypeId.Builtins.u64 => q"rnd.nextU64()"

        case TypeId.Builtins.f32  => q"rnd.nextF32()"
        case TypeId.Builtins.f64  => q"rnd.nextF64()"
        case TypeId.Builtins.f128 => q"rnd.nextDecimal()"

        case TypeId.Builtins.str   => q"rnd.nextString()"
        case TypeId.Builtins.bytes => q"rnd.nextBytes()"
        case TypeId.Builtins.uid   => q"rnd.nextUuid()"

        case TypeId.Builtins.tsu => q"rnd.nextTsu()"
        case TypeId.Builtins.tso => q"rnd.nextTso()"

        case TypeId.Builtins.bit => q"rnd.nextBool()"

        case u: TypeId.User if enquiries.isEnum(tpe, domain) =>
          val enumType = translator.toSwTypeRefKeepForeigns(u, domain, evo)
          q"rnd.mkEnum($enumType.all)"
        case u: TypeId.User =>
          val fixturePkg = translator.effectiveSwPkg(u.owner, domain, evo)
          val fixtureClassName = translator.fixtureClassName(u, domain, evo)
          val fixtureFileName = s"${translator.toSnakeCase(translator.toSwTypeRefKeepForeigns(u, domain, evo).name)}_fixture"
          val fixtureType      = SwValue.SwType(fixturePkg, fixtureClassName, importAs = Some(fixtureFileName))
          q"$fixtureType.random(rnd)"

        case t => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
