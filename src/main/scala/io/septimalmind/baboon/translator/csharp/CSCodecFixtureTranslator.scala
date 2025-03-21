package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerOptions
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait CSCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[CSValue]]
}

object CSCodecFixtureTranslator {
  final class CSRandomMethodTranslatorImpl(options: CompilerOptions, translator: CSTypeTranslator, enquiries: BaboonEnquiries, domain: Domain, evo: BaboonEvolution)
    extends CSCodecFixtureTranslator {

    override def translate(
      definition: DomainMember.User
    ): Option[TextTree[CSValue]] = {
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

    private def doTranslateDto(dto: Typedef.Dto): TextTree[CSValue] = {
      val generatedFields = dto.fields.map(f => genType(f.tpe))
      val fullType        = translator.toCsTypeRefNoDeref(dto.id, domain, evo)

      q"""public static class ${dto.id.name.name.capitalize}_Fixture
         |{
         |    public static $fullType Random()
         |    {
         |        return new $fullType(
         |            ${generatedFields.join(",\n").shift(12).trim}
         |        );
         |    }
         |}
         |""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[CSValue] = {
      val members = adt.members.toList
        .flatMap(m => domain.defs.meta.nodes.get(m))
        .collect { case DomainMember.User(_, d: Typedef.Dto, _) => d }

      val membersFixtures = if (options.csOptions.useCompactAdtForm) {
        members.sortBy(_.id.toString).map(dto => doTranslateDto(dto))
      } else {
        List.empty[TextTree[CSValue]]
      }

      val membersGenerators = members.sortBy(_.id.toString).map[TextTree[CSValue]] {
        dto =>
          val memberFixture = if (options.csOptions.useCompactAdtForm) {
            q"${dto.id.name.name}"
          } else {
            q"${translator.toCsTypeRefNoDeref(dto.id, domain, evo)}"
          }
          q"${memberFixture}_Fixture.Random()"
      }

      val membersBranches = membersGenerators.zipWithIndex.map {
        case (generator, idx) =>
          q"""if (rnd == ${idx.toString})
             |{
             |    return $generator;
             |}
             |""".stripMargin
      }

      q"""public static class ${adt.id.name.name}_Fixture
         |{
         |    public static ${adt.id.name.name} Random() {
         |        var rnd = $baboonFixture.NextInt32(${members.size.toString});
         |        ${membersBranches.join("\n").shift(8).trim}
         |        throw new $csArgumentException();
         |    }
         |
         |    public static $csList<${adt.id.name.name}> RandomAll() {
         |        return new $csList<${adt.id.name.name}>
         |        {
         |            ${membersGenerators.join(",\n").shift(12).trim}
         |        };
         |    }
         |
         |    ${membersFixtures.join("\n").shift(4).trim}
         |}
         |""".stripMargin
    }

    private def genType(tpe: TypeRef): TextTree[CSValue] = {
      def gen(tpe: TypeRef): TextTree[CSValue] = {
        tpe match {
          case tpe: TypeRef.Scalar => genScalar(tpe)
          case TypeRef.Constructor(id, args) =>
            id match {
              case Builtins.lst =>
                val argTpe = renderCollectionTypeArgument(args.head)
                q"""$baboonFixture.FillList<$argTpe>($baboonFixture.NextInt32(20), () => ${gen(args.head)})"""

              case Builtins.set =>
                val argTpe = renderCollectionTypeArgument(args.head)
                q"""$baboonFixture.FillSet<$argTpe>($baboonFixture.NextInt32(20), () => ${gen(args.head)})"""

              case Builtins.map =>
                val keyTpe   = renderCollectionTypeArgument(args(0))
                val valueTpe = renderCollectionTypeArgument(args(1))
                val entry    = q"new $csKeyValuePair<$keyTpe, $valueTpe>(${gen(args(0))}, ${gen(args(1))})"
                q"$baboonFixture.FillDict<$keyTpe, $valueTpe>($baboonFixture.NextInt32(20), () => $entry)"

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
    ): TextTree[CSValue] = {
      def render(tpe: TypeRef): TextTree[CSValue] = {
        tpe match {
          case TypeRef.Constructor(Builtins.opt, args) => q"${render(args.head)}?"
          case TypeRef.Constructor(Builtins.map, args) => q"${translator.asCsType(Builtins.map, domain, evo)}<${render(args.head)}, ${render(args.last)}>"
          case TypeRef.Constructor(id, args)           => q"${translator.asCsType(id, domain, evo)}<${render(args.head)}>"
          case TypeRef.Scalar(id)                      => q"${translator.asCsType(id, domain, evo)}"
        }
      }

      render(tpe)
    }

    private def genScalar(tpe: TypeRef.Scalar): TextTree[CSValue] = {
      tpe.id match {
        case TypeId.Builtins.i08 => q"$baboonFixture.NextSByte()"
        case TypeId.Builtins.i16 => q"$baboonFixture.NextInt16()"
        case TypeId.Builtins.i32 => q"$baboonFixture.NextInt32()"
        case TypeId.Builtins.i64 => q"$baboonFixture.NextInt64()"

        case TypeId.Builtins.u08 => q"$baboonFixture.NextByte()"
        case TypeId.Builtins.u16 => q"$baboonFixture.NextUInt16()"
        case TypeId.Builtins.u32 => q"$baboonFixture.NextUInt32()"
        case TypeId.Builtins.u64 => q"$baboonFixture.NextUInt64()"

        case TypeId.Builtins.f32  => q"$baboonFixture.NextSingle()"
        case TypeId.Builtins.f64  => q"$baboonFixture.NextDouble()"
        case TypeId.Builtins.f128 => q"$baboonFixture.NextDecimal()"

        case TypeId.Builtins.str => q"$baboonFixture.NextString()"
        case TypeId.Builtins.uid => q"$baboonFixture.NextGuid()"
        case TypeId.Builtins.tsu => q"$baboonFixture.NextRpDateTime()"
        case TypeId.Builtins.tso => q"$baboonFixture.NextRpDateTime()"

        case TypeId.Builtins.bit => q"$baboonFixture.NextBoolean()"

        case TypeId.User(_, _, name) if translator.isEnum(tpe, domain) => q"$baboonFixture.NextRandomEnum<${name.name}>()"
        case TypeId.User(_, _, name)                                   => q"${name.name}_Fixture.Random()"

        case t =>
          throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
