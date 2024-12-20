package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait CSCodecFixtureTranslator {
  def translate(definition: DomainMember.User,
                domain: Domain,
                evo: BaboonEvolution): Option[TextTree[CSValue]]
}

object CSCodecFixtureTranslator {
  final class CSRandomMethodTranslatorImpl(options: CompilerOptions,
                                           translator: CSTypeTranslator,
                                           enquiries: BaboonEnquiries)
    extends CSCodecFixtureTranslator {

    override def translate(definition: DomainMember.User, domain: Domain, evo: BaboonEvolution): Option[TextTree[CSValue]] = {
      definition.defn match {
        case dto: Typedef.Dto => doTranslateDto(definition, dto, domain, evo, None)
        case adt: Typedef.Adt => doTranslateAdt(definition, adt, domain, evo)
        case _: Typedef.Contract => None
        case _: Typedef.Enum => None
        case _: Typedef.Foreign => None
      }
    }

    private def doTranslateDto(
                                definition: DomainMember.User,
                                dto: Typedef.Dto,
                                domain: Domain,
                                evo: BaboonEvolution,
                                adt: Option[TypeId],
                              ): Option[TextTree[CSValue]] = {

      definition match {
        case d if enquiries.hasForeignType(d, domain) => None
        case d if enquiries.isRecursiveTypedef(d, domain) => None
        case _ =>
          val generatedFields = dto.fields.map(f => genType(f.tpe, domain, evo))

          // adt member full type is different for compact and non-compact forms
          val fullType = if (options.csUseCompactAdtForm) {
            s"${adt.map(t => s"${t.name.name}.").getOrElse("")}${dto.id.name.name}"
          } else {
            dto.id.name.name
          }

          val fixture =
            q"""public static class ${adt.map(t => s"${t.name.name}_").getOrElse("")}${dto.id.name.name}_Fixture
               |{
               |    public static $fullType Random()
               |    {
               |        return new $fullType(
               |            ${generatedFields.join(",\n").shift(12).trim}
               |        );
               |    }
               |}
               |""".stripMargin
          Some(fixture)
      }

    }

    private def doTranslateAdt(
                                definition: DomainMember.User,
                                adt: Typedef.Adt,
                                domain: Domain,
                                evo: BaboonEvolution
                              ): Option[TextTree[CSValue]] = {
      definition match {
        case d if enquiries.hasForeignType(d, domain) => None
        case d if enquiries.isRecursiveTypedef(d, domain) => None
        case _ =>
          val members = adt.members.toList
            .flatMap(m => domain.defs.meta.nodes.get(m))
            .collect { case DomainMember.User(_, d: Typedef.Dto, _) => d }

          val membersFixtures = if (options.csUseCompactAdtForm) {
            members.sortBy(_.id.toString).flatMap(dto => doTranslateDto(definition, dto, domain, evo, Some(adt.id)))
          } else {
            List.empty[TextTree[CSValue]]
          }

          val membersGenerators = members.sortBy(_.id.toString).map[TextTree[CSValue]] {
            dto =>
              val memberFixture = if (options.csUseCompactAdtForm) {
                q"${adt.id.name.name}_${dto.id.name.name}"
              } else {
                q"${translator.toCsTypeRefNoDeref(dto.id, domain, evo)}"
              }
              q"${memberFixture}_Fixture.Random()"
          }

          val membersBranches = membersGenerators.zipWithIndex.map{case (generator, idx) =>
            q"""if (rnd == ${idx.toString}) {
               |    return $generator;
               |}
               |""".stripMargin
          }

          val fixture: TextTree[CSValue] =
            q"""public static class ${adt.id.name.name}_Fixture
               |{
               |    public static ${adt.id.name.name} Random() {
               |        var rnd = $testValuesGenerator.NextInt32(${members.size.toString});
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

          Some(fixture)
      }
    }

    private def genType(tpe: TypeRef,
                        domain: Domain,
                        evo: BaboonEvolution): TextTree[CSValue] = {
      def gen(tpe: TypeRef): TextTree[CSValue] = {
        tpe match {
          case tpe: TypeRef.Scalar => genScalar(tpe, domain)
          case TypeRef.Constructor(id, args) =>
            id match {
              case Builtins.lst =>
                val argTpe = renderCollectionTypeArgument(args.head, domain, evo)
                q"""$testValuesGenerator.FillList<$argTpe>($testValuesGenerator.NextInt32(20), () => ${gen(args.head)})"""

              case Builtins.set =>
                val argTpe = renderCollectionTypeArgument(args.head, domain, evo)
                q"""$testValuesGenerator.FillSet<$argTpe>($testValuesGenerator.NextInt32(20), () => ${gen(args.head)})"""

              case Builtins.map =>
                val keyTpe = renderCollectionTypeArgument(args(0), domain, evo)
                val valueTpe = renderCollectionTypeArgument(args(1), domain, evo)
                val entry = q"new $csKeyValuePair<$keyTpe, $valueTpe>(${gen(args(0))}, ${gen(args(1))})"
                q"$testValuesGenerator.FillDict<$keyTpe, $valueTpe>($testValuesGenerator.NextInt32(20), () => $entry)"

              case Builtins.opt => q"${gen(args.head)}"

              case t => throw new IllegalArgumentException(s"Unexpected collection type: $t")
            }
        }
      }

      gen(tpe)
    }

    private def renderCollectionTypeArgument(
                                              tpe: TypeRef,
                                              domain: Domain,
                                              evo: BaboonEvolution
                                            ): TextTree[CSValue] = {
      def render(tpe: TypeRef): TextTree[CSValue] = {
        tpe match {
          case TypeRef.Constructor(Builtins.opt, args) =>
            q"${render(args.head)}?"
          case TypeRef.Constructor(Builtins.map, args) =>
            q"${translator.asCsType(Builtins.map, domain, evo)}<${render(args(0))}, ${render(args(1))}>"
          case TypeRef.Constructor(id, args) =>
            q"${translator.asCsType(id, domain, evo)}<${render(args.head)}>"
          case TypeRef.Scalar(id) => translator.asCsType(id, domain, evo)
        }
      }

      render(tpe)
    }

    private def genScalar(tpe: TypeRef.Scalar,
                          domain: Domain): TextTree[CSValue] = {
      tpe.id match {
        case TypeId.Builtins.i08 => q"$testValuesGenerator.NextSByte()"
        case TypeId.Builtins.i16 => q"$testValuesGenerator.NextInt16()"
        case TypeId.Builtins.i32 => q"$testValuesGenerator.NextInt32()"
        case TypeId.Builtins.i64 => q"$testValuesGenerator.NextInt64()"

        case TypeId.Builtins.u08 => q"$testValuesGenerator.NextByte()"
        case TypeId.Builtins.u16 => q"$testValuesGenerator.NextUInt16()"
        case TypeId.Builtins.u32 => q"$testValuesGenerator.NextUInt32()"
        case TypeId.Builtins.u64 => q"$testValuesGenerator.NextUInt64()"

        case TypeId.Builtins.f32 => q"$testValuesGenerator.NextSingle()"
        case TypeId.Builtins.f64 => q"$testValuesGenerator.NextDouble()"
        case TypeId.Builtins.f128 => q"$testValuesGenerator.NextDecimal()"

        case TypeId.Builtins.str => q"$testValuesGenerator.NextString()"
        case TypeId.Builtins.uid => q"$testValuesGenerator.NextGuid()"
        case TypeId.Builtins.tsu => q"$testValuesGenerator.NextRpDateTime()"
        case TypeId.Builtins.tso => q"$testValuesGenerator.NextRpDateTime()"

        case TypeId.Builtins.bit => q"$testValuesGenerator.NextBoolean()"

        case TypeId.User(_, _, name) =>
          if (translator.isEnum(tpe, domain)) {
            q"$testValuesGenerator.NextRandomEnum<${name.name}>()"
          } else {
            q"${name.name}_Fixture.Random()"
          }

        case t =>
          throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
