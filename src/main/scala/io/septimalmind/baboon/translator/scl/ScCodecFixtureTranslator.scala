package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import izumi.fundamentals.platform.strings.TextTree

trait ScCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[ScValue]]
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
        case _: Typedef.Contract                                   => None
        case _: Typedef.Enum                                       => None
        case _: Typedef.Foreign                                    => None
        case _: Typedef.Service                                    => None

        case dto: Typedef.Dto =>
          Some(doTranslateDto(dto))

        case adt: Typedef.Adt =>
          Some(doTranslateAdt(adt))
      }
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[ScValue] = {
      val generatedFields = dto.fields.map(f => genType(f.tpe))
      val fullType        = translator.asScTypeKeepForeigns(dto.id, domain, evo)

      q"""object ${dto.id.name.name.capitalize}_Fixture {
         |  def random(): $fullType =
         |    new $fullType(
         |      ${generatedFields.join(",\n").shift(12).trim}
         |    )
         |}
         |""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[ScValue] = {
      val members = adt.members.toList
        .flatMap(m => domain.defs.meta.nodes.get(m))
        .collect { case DomainMember.User(_, d: Typedef.Dto, _) => d }

      val membersFixtures = if (target.language.useCompactAdtForm) {
        members.sortBy(_.id.toString).map(dto => doTranslateDto(dto))
      } else {
        List.empty[TextTree[ScValue]]
      }

      val membersGenerators = members.sortBy(_.id.toString).map[TextTree[ScValue]] {
        dto =>
          val memberFixture = if (target.language.useCompactAdtForm) {
            q"${dto.id.name.name}"
          } else {
            q"${translator.asScTypeKeepForeigns(dto.id, domain, evo)}"
          }
          q"${memberFixture}_Fixture.random()"
      }

      val membersBranches = membersGenerators.zipWithIndex.map {
        case (generator, idx) =>
          q"""if (rnd == ${idx.toString}) {
             |  return $generator;
             |}
             |""".stripMargin
      }

      q"""object ${adt.id.name.name}_Fixture {
         |  def random(): ${adt.id.name.name} = {
         |    val rnd = scala.util.Random.nextInt(${members.size.toString})
         |    ${membersBranches.join("\n").shift(8).trim}
         |    throw new IllegalArgumentException()
         |  }
         |
         |  def randomAll(): Seq[${adt.id.name.name}] =
         |    Seq(
         |      ${membersGenerators.join(",\n").shift(12).trim}
         |    )
         |
         |  ${membersFixtures.join("\n").shift(4).trim}
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
                val argTpe = renderCollectionTypeArgument(args.head)
                q"""scala.util.Random.shuffle(Seq.fill(scala.util.Random.nextInt(20))(${gen(args.head)}))"""

              case Builtins.set =>
                val argTpe = renderCollectionTypeArgument(args.head)
                q"""scala.util.Random.shuffle(Set.fill(scala.util.Random.nextInt(20))(${gen(args.head)}))"""

              case Builtins.map =>
                val keyTpe   = renderCollectionTypeArgument(args(0))
                val valueTpe = renderCollectionTypeArgument(args(1))
                val entry    = q"($keyTpe, $valueTpe)"
                q"""scala.util.Random.shuffle(Map.fill(scala.util.Random.nextInt(20))(${gen(args(0))}, ${gen(args(1))}))"""

              case Builtins.opt =>
                q"${gen(args.head)}"

              case t =>
                throw new IllegalArgumentException(s"Unexpected collection type: $t")
            }
        }
      }

      gen(tpe)
    }

    private def renderCollectionTypeArgument(
      tpe: TypeRef
    ): TextTree[ScValue] = {
      def render(tpe: TypeRef): TextTree[ScValue] = {
        tpe match {
          case TypeRef.Constructor(Builtins.opt, args) => q"${render(args.head)}?"
          case TypeRef.Constructor(Builtins.map, args) => q"Map[${render(args.head)}, ${render(args.last)}]"
          case TypeRef.Constructor(id, args)           => q"${translator.asScType(id, domain, evo)}[${render(args.head)}]"
          case TypeRef.Scalar(id)                      => q"${translator.asScType(id, domain, evo)}"
        }
      }

      render(tpe)
    }

    private def genScalar(tpe: TypeRef.Scalar): TextTree[ScValue] = {
      tpe.id match {
        case TypeId.Builtins.i08 => q"scala.util.Random.nextByte()"
        case TypeId.Builtins.i16 => q"scala.util.Random.nextShort()"
        case TypeId.Builtins.i32 => q"scala.util.Random.nextInt()"
        case TypeId.Builtins.i64 => q"scala.util.Random.nextLong()"

        case TypeId.Builtins.u08 => q"scala.util.Random.nextByte()"
        case TypeId.Builtins.u16 => q"scala.util.Random.nextShort()"
        case TypeId.Builtins.u32 => q"scala.util.Random.nextInt()"
        case TypeId.Builtins.u64 => q"scala.util.Random.nextLong()"

        case TypeId.Builtins.f32  => q"scala.util.Random.nextFloat()"
        case TypeId.Builtins.f64  => q"scala.util.Random.nextDouble()"
        case TypeId.Builtins.f128 => q"scala.math.BigDecimal(scala.util.Random.nextDouble())"

        case TypeId.Builtins.str => q"""scala.util.Random.alphanumeric.take(10).mkString""""
        case TypeId.Builtins.uid => q"""java.util.UUID.randomUUID().toString"""
        case TypeId.Builtins.tsu => q"""new java.time.Instant(scala.util.Random.nextLong())"""
        case TypeId.Builtins.tso => q"""new java.time.OffsetDateTime(scala.util.Random.nextLong())"""

        case TypeId.Builtins.bit => q"scala.util.Random.nextBoolean()"

        case TypeId.User(_, _, name) if enquiries.isEnum(tpe, domain) => q"scala.util.Random.shuffle(${name.name}.values).head"
        case TypeId.User(_, _, name)                                  => q"${name.name}_Fixture.random()"

        case t =>
          throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
