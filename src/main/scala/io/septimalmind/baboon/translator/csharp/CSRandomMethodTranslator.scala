package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.testValuesGenerator
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps

trait CSRandomMethodTranslator {
  def translateDtoRandom(definition: DomainMember.User,
                         dto: Typedef.Dto,
                         domain: Domain,
                         evo: BaboonEvolution): Option[TextTree[CSValue]]

  def translateAdtRandom(definition: DomainMember.User,
                         adt: Typedef.Adt,
                         domain: Domain,
                         evo: BaboonEvolution): Option[TextTree[CSValue]]
}

object CSRandomMethodTranslator {
  final class CSRandomMethodTranslatorImpl(translator: CSTypeTranslator,
                                           enquiries: BaboonEnquiries)
      extends CSRandomMethodTranslator {
    private val rnd = new Random()

    override def translateDtoRandom(
      definition: DomainMember.User,
      dto: Typedef.Dto,
      domain: Domain,
      evo: BaboonEvolution
    ): Option[TextTree[CSValue]] = {

      definition match {
        case d if enquiries.hasForeignType(d, domain)     => None
        case d if enquiries.isRecursiveTypedef(d, domain) => None
        case _ =>
          val generatedFields = dto.fields.map(f => genType(f.tpe, domain, evo))
          val method =
            q"""#nullable enable
               |#pragma warning disable CS0108
               |public static ${dto.id.name.name} Random() {
               |  return new ${dto.id.name.name}(
               |    ${generatedFields.join(",\n").shift(4).trim}
               |  );
               |}
               |#nullable disable
               |""".stripMargin.shift(2).trim
          Some(method)
      }

    }

    override def translateAdtRandom(
      definition: DomainMember.User,
      adt: Typedef.Adt,
      domain: Domain,
      evo: BaboonEvolution
    ): Option[TextTree[CSValue]] = {
      definition match {
        case d if enquiries.hasForeignType(d, domain)     => None
        case d if enquiries.isRecursiveTypedef(d, domain) => None
        case _ =>
          val adtDtos = adt.members
            .map(m => domain.defs.meta.nodes(m))
            .toList
            .collect { case DomainMember.User(_, d: Typedef.Dto, _) => d }
          val toGenerate = adtDtos(Random.between(0, adtDtos.size))
          val method =
            q"""public static ${adt.id.name.name} Random() {
               |  return ${translator.asCsType(toGenerate.id, domain, evo)}.Random();
               |}
               |""".stripMargin.shift(2).trim
          Some(method)
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
                val argTpe =
                  renderCollectionTypeArgument(args.head, domain, evo)
                val listValues =
                  (0 until rndSize).map(_ => gen(args.head)).join(", ")
                q"ImmutableList.Create<$argTpe>($listValues)".shift(2).trim
              case Builtins.set =>
                val argTpe =
                  renderCollectionTypeArgument(args.head, domain, evo)
                val setValues =
                  (0 until rndSize).map(_ => gen(args.head)).join(", ")
                q"ImmutableHashSet.Create<$argTpe>($setValues)".shift(2)
              case Builtins.map =>
                val keyTpe = renderCollectionTypeArgument(args(0), domain, evo)
                val valueTpe =
                  renderCollectionTypeArgument(args(1), domain, evo)
                val mapValues = (0 until rndSize)
                  .map(
                    _ =>
                      q"new KeyValuePair<$keyTpe, $valueTpe>(${gen(args(0))}, ${gen(args(1))})"
                  )
                  .pipe(
                    values =>
                      q"new List<KeyValuePair<$keyTpe, $valueTpe>> { ${values.join(", ")} }"
                  )
                q"$testValuesGenerator.CreateImmutableDictionary<$keyTpe, $valueTpe>($mapValues)"
                  .shift(2)
              case Builtins.opt => q"${gen(args.head)}"
              case t =>
                throw new IllegalArgumentException(
                  s"Unexpected collection type: $t"
                )
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

        case TypeId.Builtins.f32  => q"$testValuesGenerator.NextSingle()"
        case TypeId.Builtins.f64  => q"$testValuesGenerator.NextDouble()"
        case TypeId.Builtins.f128 => q"$testValuesGenerator.NextDecimal()"

        case TypeId.Builtins.str => q"$testValuesGenerator.NextString()"
        case TypeId.Builtins.uid => q"$testValuesGenerator.NextGuid()"
        case TypeId.Builtins.tsu => q"$testValuesGenerator.NextRpDateTime()"
        case TypeId.Builtins.tso => q"$testValuesGenerator.NextRpDateTime()"

        case TypeId.Builtins.bit => q"$testValuesGenerator.NextBoolean()"

        case TypeId.User(_, _, name) =>
          if (translator.isEnum(tpe, domain))
            q"$testValuesGenerator.NextRandomEnum<${name.name}>()"
          else q"${name.name}.Random()"

        case t =>
          throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }

    private def rndSize: Int = rnd.between(0, 20)
  }
}
