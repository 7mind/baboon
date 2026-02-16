package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait JvCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[JvValue]]
  def fixtureTpe(definition: DomainMember.User): Option[TextTree[JvValue]]
}

object JvCodecFixtureTranslator {
  final class Impl(
    target: JvTarget,
    translator: JvTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends JvCodecFixtureTranslator {

    override def translate(
      definition: DomainMember.User
    ): Option[TextTree[JvValue]] = {
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

    override def fixtureTpe(definition: DomainMember.User): Option[TextTree[JvValue]] = {
      target.output.fixturesOutput.flatMap {
        _ =>
          definition.defn.id.owner match {
            case o: Owner.Adt =>
              for {
                did <- defnFixtureId(definition)
                oid <- defnFixtureId(domain.defs.meta.nodes(o.id))
              } yield {
                q"$oid.$did"
              }
            case _ => defnFixtureId(definition)
          }
      }
    }

    private def defnFixtureId(definition: DomainMember): Option[TextTree[JvValue]] = {
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

    private def fixtureTpeName(id: TypeId): TextTree[JvValue] = {
      q"${id.name.name.capitalize}_Fixture"
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[JvValue] = {
      val generatedFields = dto.fields.map(f => genType(f.tpe))
      val fullType        = translator.toJvTypeRefKeepForeigns(dto.id, domain, evo)

      q"""public final class ${fixtureTpeName(dto.id)} {
         |  public static $fullType random($baboonRandom rnd) {
         |    return new $fullType(
         |      ${generatedFields.join(",\n").shift(6).trim}
         |    );
         |  }
         |}
         |""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[JvValue] = {
      val fullType = translator.toJvTypeRefKeepForeigns(adt.id, domain, evo)
      val members = adt.members.toList
        .flatMap(domain.defs.meta.nodes.get)
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val membersFixtures    = members.sortBy(_.id.toString).map(doTranslateDto)
      val membersGenerators  = members.sortBy(_.id.toString).map(dto => q"${dto.id.name.name}_Fixture::random")
      val membersDirectCalls = members.sortBy(_.id.toString).map(dto => q"${dto.id.name.name}_Fixture.random(rnd)")

      q"""public final class ${fixtureTpeName(adt.id)} {
         |  public static $fullType random($baboonRandom rnd) {
         |    return rnd.oneOf($jvList.of(
         |      ${membersGenerators.join(",\n").shift(6).trim}
         |    ));
         |  }
         |
         |  public static $jvList<$fullType> randomAll($baboonRandom rnd) {
         |    return $jvList.of(
         |      ${membersDirectCalls.join(",\n").shift(6).trim}
         |    );
         |  }
         |
         |  ${membersFixtures.joinN().shift(2).trim}
         |}
         |""".stripMargin
    }

    private def genType(tpe: TypeRef): TextTree[JvValue] = {
      def gen(tpe: TypeRef): TextTree[JvValue] = {
        tpe match {
          case tpe: TypeRef.Scalar => genScalar(tpe)
          case TypeRef.Constructor(id, args) =>
            id match {
              case Builtins.lst => q"rnd.mkList(() -> ${gen(args.head)})"
              case Builtins.set => q"rnd.mkSet(() -> ${gen(args.head)})"
              case Builtins.map => q"rnd.mkMap(() -> ${gen(args(0))}, () -> ${gen(args(1))})"
              case Builtins.opt => q"rnd.mkOptional(() -> ${gen(args.head)})"
              case t            => throw new IllegalArgumentException(s"Unexpected collection type: $t")
            }
        }
      }

      gen(tpe)
    }

    private def genScalar(tpe: TypeRef.Scalar): TextTree[JvValue] = {
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
        case TypeId.Builtins.f128 => q"rnd.nextF128()"

        case TypeId.Builtins.str   => q"rnd.nextString()"
        case TypeId.Builtins.bytes => q"rnd.nextByteString()"
        case TypeId.Builtins.uid   => q"rnd.nextUid()"

        case TypeId.Builtins.tsu => q"rnd.nextTsu()"
        case TypeId.Builtins.tso => q"rnd.nextTso()"

        case TypeId.Builtins.bit => q"rnd.nextBit()"

        case TypeId.User(_, _, name) if enquiries.isEnum(tpe, domain) => q"rnd.mkEnum(${name.name}.class, ${name.name}.values())"
        case u: TypeId.User                                           => q"${u.name.name}_Fixture.random(rnd)"

        case t => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
