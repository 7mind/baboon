package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait ScCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[ScValue]]
  def fixtureTpe(definition: DomainMember.User): Option[TextTree[ScValue]]
}

object ScCodecFixtureTranslator {
  final class ScRandomMethodTranslatorImpl(
    target: ScTarget,
    translator: ScTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends ScCodecFixtureTranslator {

    override def translate(
      definition: DomainMember.User
    ): Option[TextTree[ScValue]] = {
      definition.defn match {
        case _ if enquiries.hasForeignType(definition, domain)     => None
        case _ if enquiries.isRecursiveTypedef(definition, domain) => None

        case dto: Typedef.Dto =>
          Some(doTranslateDto(dto))

        case adt: Typedef.Adt =>
          Some(doTranslateAdt(adt))

        case _: Typedef.Contract => None
        case _: Typedef.Enum     => None
        case _: Typedef.Foreign  => None
        case _: Typedef.Service  => None
      }
    }

    override def fixtureTpe(definition: DomainMember.User): Option[TextTree[ScValue]] = {
      definition.defn.id.owner match {
        case o: Owner.Adt =>
          for {
            did <- defnFixtureId(definition)
            oid <- defnFixtureId(domain.defs.meta.nodes(o.id))
          } yield {
            q"$oid.$did"
          }
        case _ =>
          defnFixtureId(definition)
      }

    }

    private def defnFixtureId(definition: DomainMember): Option[TextTree[ScValue]] = {
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

            case dto: Typedef.Dto =>
              Some(fixtureTpe(dto.id))

            case adt: Typedef.Adt =>
              Some(fixtureTpe(adt.id))
          }
      }

    }

    private def fixtureTpe(id: TypeId): TextTree[ScValue] = {
      q"${id.name.name.capitalize}_Fixture"
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[ScValue] = {
      val generatedFields = dto.fields.map(f => genType(f.tpe))
      val fullType        = translator.toScTypeRefKeepForeigns(dto.id, domain, evo)

      q"""object ${fixtureTpe(dto.id)} extends $baboonFixture[$fullType] {
         |  def random(rnd: $baboonRandom): $fullType =
         |    $fullType(
         |      ${generatedFields.join(",\n").shift(6).trim}
         |    )
         |}
         |""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[ScValue] = {
      val members = adt.members.toList
        .flatMap(m => domain.defs.meta.nodes.get(m))
        .collect { case DomainMember.User(_, d: Typedef.Dto, _) => d }

      val membersFixtures =
        members.sortBy(_.id.toString).map(dto => doTranslateDto(dto))

      val membersGenerators = members.sortBy(_.id.toString).map[TextTree[ScValue]] {
        dto =>
          val memberFixture =
            q"${dto.id.name.name}"

          q"${memberFixture}_Fixture.random"
      }

//      val membersBranches = membersGenerators.zipWithIndex.map {
//        case (generator, idx) =>
//          q"""case ${idx.toString} => $generator""".stripMargin
//      }

      q"""object ${fixtureTpe(adt.id)} extends $baboonAdtFixture[${adt.id.name.name}] {
         |  def random(rnd: $baboonRandom): ${adt.id.name.name} = {
         |    rnd.oneOf($scList(
         |      ${membersGenerators.join(",\n").shift(6).trim}
         |    ))
         |  }
         |
         |  def randomAll(rnd: $baboonRandom): $scList[${adt.id.name.name}] = {
         |    $scList(
         |      ${membersGenerators.map(g => q"$g(rnd)").join(",\n").shift(6).trim}
         |    )
         |  }
         |  
         |  ${membersFixtures.join("\n").shift(2).trim}
         |}
         |""".stripMargin
    }

    private def genType(tpe: TypeRef): TextTree[ScValue] = {
      def gen(tpe: TypeRef): TextTree[ScValue] = {
        tpe match {
          case tpe: TypeRef.Scalar => genScalar(tpe)
          case TypeRef.Constructor(id, args) =>
            id match {
              case Builtins.lst =>
                q"""rnd.mkList(${gen(args.head)})"""

              case Builtins.set =>
                q"""rnd.mkSet(${gen(args.head)})"""

              case Builtins.map =>
                q"""rnd.mkMap(${gen(args(0))}, ${gen(args(1))})"""

              case Builtins.opt =>
                q"""rnd.mkOption(${gen(args.head)})"""

              case t =>
                throw new IllegalArgumentException(s"Unexpected collection type: $t")
            }
        }
      }

      gen(tpe)
    }

    private def genScalar(tpe: TypeRef.Scalar): TextTree[ScValue] = {

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

        case TypeId.Builtins.str => q"rnd.nextString()"
        case TypeId.Builtins.uid => q"rnd.nextUid()"

        case TypeId.Builtins.tsu => q"rnd.nextTsu()"
        case TypeId.Builtins.tso => q"rnd.nextTso()"

        case TypeId.Builtins.bit => q"rnd.nextBit()"

        case TypeId.User(_, _, name) if enquiries.isEnum(tpe, domain) => q"rnd.mkEnum(${name.name})"
        case u: TypeId.User                                           => q"${u.name.name}_Fixture.random(rnd)"

        case t =>
          throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }

  }

}
