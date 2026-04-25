package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait CSCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[CSValue]]
}

object CSCodecFixtureTranslator {
  final class CSRandomMethodTranslatorImpl(
    translator: CSTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    lineage: BaboonLineage,
    evo: BaboonEvolution,
    csTypeInfo: CSTypeInfo,
  ) extends CSCodecFixtureTranslator {

    override def translate(
      definition: DomainMember.User
    ): Option[TextTree[CSValue]] = {
      if (csTypeInfo.eliminated(definition.id, domain.version, lineage)) {
        None
      } else {
        definition.defn match {
          case _ if enquiries.hasForeignType(definition, domain, BaboonLang.Cs) => None
          case _ if enquiries.isRecursiveTypedef(definition, domain)            => None
          case _: Typedef.Contract                                              => None
          case _: Typedef.Enum                                                  => None
          case _: Typedef.Foreign                                               => None
          case _: Typedef.Service                                               => None

          case dto: Typedef.Dto =>
            Some(doTranslateDto(dto))

          case adt: Typedef.Adt =>
            Some(doTranslateAdt(adt))
        }
      }
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[CSValue] = {
      val generatedFields = dto.fields.map(f => genType(f.tpe))
      val fullType        = translator.asCsType(dto.id, domain, evo)

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
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val membersFixtures = members.sortBy(_.id.toString).map(dto => doTranslateDto(dto))

      val membersGenerators = members.sortBy(_.id.toString).map[TextTree[CSValue]] {
        dto =>
          val memberFixture = q"${dto.id.name.name}"
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
        BaboonEnquiries.resolveBaboonRef(tpe, domain, BaboonLang.Cs) match {
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
          case a: TypeRef.Any => genAnyFixture(a)
        }
      }

      gen(tpe)
    }

    // Branch-matching fixtures (mirroring Scala's PR-07-D01 fix) belong to PR 3.4. PR 3.2 emits the
    // `AnyOpaqueUeba` branch only — auto-tests for the JSON path will need a `randomJson` companion
    // alongside this `genType` callsite, plus a `FixtureFormat` selector. For now both UEBA and JSON
    // tests round-trip the UEBA branch; the JSON test path will pass once PR 3.4 splits the fixture.
    // Bytes are empty: the fixture only has to compile and yield a value with the right meta-kind;
    // the encoder asserts `meta.Kind == expectedKind` and copies the bytes through verbatim.
    private val FixtureAnyPayloadTypeId: String = "my.test.AnyFixturePayload"

    private def genAnyFixture(a: TypeRef.Any): TextTree[CSValue] = {
      val hasUnderlying = a.underlying.isDefined
      val kindHex       = "0x%02x".format(AnyVariant.metaKindByte(a.variant, hasUnderlying) & 0xFF)
      val domainStr     = q""""${domain.id.toString}""""
      val versionStr    = q""""${domain.version.v.toString}""""
      val typeidStr     = q""""$FixtureAnyPayloadTypeId""""

      val (domainExpr, versionExpr, typeidExpr) = a.variant match {
        case AnyVariant.Global =>
          val tid = if (hasUnderlying) q"($csString?)null" else q"($csString?)$typeidStr"
          (q"($csString?)$domainStr", q"($csString?)$versionStr", tid)
        case AnyVariant.ThisDom =>
          val tid = if (hasUnderlying) q"($csString?)null" else q"($csString?)$typeidStr"
          (q"($csString?)null", q"($csString?)$versionStr", tid)
        case AnyVariant.Current =>
          val tid = if (hasUnderlying) q"($csString?)null" else q"($csString?)$typeidStr"
          (q"($csString?)null", q"($csString?)null", tid)
      }

      q"new $baboonAnyOpaqueUeba(new $baboonAnyMeta(($csByte)$kindHex, $domainExpr, $versionExpr, $typeidExpr), $csArray.Empty<$csByte>())"
    }

    private def renderCollectionTypeArgument(
      tpe: TypeRef
    ): TextTree[CSValue] = {
      def render(tpe: TypeRef): TextTree[CSValue] = {
        BaboonEnquiries.resolveBaboonRef(tpe, domain, BaboonLang.Cs) match {
          case TypeRef.Constructor(Builtins.opt, args) => q"${render(args.head)}?"
          case TypeRef.Constructor(Builtins.map, args) => q"${translator.asCsType(Builtins.map, domain, evo)}<${render(args.head)}, ${render(args.last)}>"
          case TypeRef.Constructor(id, args)           => q"${translator.asCsType(id, domain, evo)}<${render(args.head)}>"
          case TypeRef.Scalar(id)                      => q"${translator.asCsType(id, domain, evo)}"
          // Mirrors `CSTypeTranslator.asCsRef` — a collection element of type `any` is `AnyOpaque`.
          case _: TypeRef.Any                          => q"$baboonAnyOpaque"
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

        case TypeId.Builtins.str   => q"$baboonFixture.NextString()"
        case TypeId.Builtins.bytes => q"$baboonFixture.NextByteString()"
        case TypeId.Builtins.uid   => q"$baboonFixture.NextGuid()"
        case TypeId.Builtins.tsu   => q"$baboonFixture.NextRpDateTime()"
        case TypeId.Builtins.tso   => q"$baboonFixture.NextRpDateTime()"

        case TypeId.Builtins.bit => q"$baboonFixture.NextBoolean()"

        case id: TypeId.User if enquiries.isEnum(tpe, domain) => q"$baboonFixture.NextRandomEnum<${translator.asCsType(id, domain, evo).fullyQualified}>()"
        case TypeId.User(_, _, name)                          => q"${name.name}_Fixture.Random()"

        case t =>
          throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
