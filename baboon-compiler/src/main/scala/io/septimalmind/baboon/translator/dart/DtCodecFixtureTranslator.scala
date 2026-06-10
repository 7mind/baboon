package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait DtCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[DtValue]]
  def fixtureTpe(definition: DomainMember.User): Option[TextTree[DtValue]]
}

object DtCodecFixtureTranslator {
  // Branch selector for any-field fixture payloads. PR-07-D01 (Dart analog): the auto-generated
  // round-trip test exercises a fixture through a single codec; if the fixture's `AnyOpaque`
  // branch matches the codec direction, no facade is needed and equality holds. We emit two
  // parallel methods per DTO (`random` for the UEBA branch, `randomJson` for the JSON branch)
  // so each test path picks its native branch. Recursive calls into nested user-type fixtures
  // must propagate the same choice. Mirrors PR 2.4 / 3.4 / 4.3 / 5.4 / 6.4 / 7.4 patterns.
  private sealed trait FixtureFormat
  private case object FixUeba extends FixtureFormat
  private case object FixJson extends FixtureFormat

  final class Impl(
    translator: DtTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends DtCodecFixtureTranslator {

    override def translate(
      definition: DomainMember.User
    ): Option[TextTree[DtValue]] = {
      definition.defn match {
        case _ if enquiries.hasForeignType(definition, domain, BaboonLang.Dart) => None
        case _ if enquiries.isRecursiveTypedef(definition, domain)              => None
        case dto: Typedef.Dto                                                   => Some(doTranslateDto(dto))
        case adt: Typedef.Adt                                                   => Some(doTranslateAdt(adt))
        case _: Typedef.Contract                                                => None
        case _: Typedef.Enum                                                    => None
        case _: Typedef.Foreign                                                 => None
        case _: Typedef.Service                                                 => None
      }
    }

    override def fixtureTpe(definition: DomainMember.User): Option[TextTree[DtValue]] = {
      defnFixtureId(definition)
    }

    private def defnFixtureId(definition: DomainMember): Option[TextTree[DtValue]] = {
      definition match {
        case _: DomainMember.Builtin => None
        case u: DomainMember.User =>
          u.defn match {
            case _ if enquiries.hasForeignType(u, domain, BaboonLang.Dart) => None
            case _ if enquiries.isRecursiveTypedef(u, domain)              => None
            case _: Typedef.Contract                                       => None
            case _: Typedef.Enum                                           => None
            case _: Typedef.Foreign                                        => None
            case _: Typedef.Service                                        => None
            case dto: Typedef.Dto                                          => Some(fixtureTpeName(dto.id))
            case adt: Typedef.Adt                                          => Some(fixtureTpeName(adt.id))
          }
      }
    }

    private def fixtureTpeName(id: TypeId): TextTree[DtValue] = {
      q"${id.name.name.capitalize}_Fixture"
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[DtValue] = {
      val fullType = translator.toDtTypeRefKeepForeigns(dto.id, domain, evo)

      def body(format: FixtureFormat): TextTree[DtValue] = {
        val generatedFields = dto.fields.map { f =>
          val dartName = translator.escapeDartKeyword(f.name.name)
          q"$dartName: ${genType(f.tpe, format)}"
        }
        q"""return $fullType(
           |  ${generatedFields.join(",\n").shift(2).trim}
           |);""".stripMargin
      }

      q"""class ${fixtureTpeName(dto.id)} {
         |  static $fullType random($baboonRandom rnd) {
         |    ${body(FixUeba).shift(4).trim}
         |  }
         |
         |  static $fullType randomJson($baboonRandom rnd) {
         |    ${body(FixJson).shift(4).trim}
         |  }
         |}
         |""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[DtValue] = {
      val fullType = translator.toDtTypeRefKeepForeigns(adt.id, domain, evo)
      val members = adt.members.toList
        .flatMap(domain.defs.meta.nodes.get)
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val sortedMembers          = members.sortBy(_.id.toString)
      val membersFixtures        = sortedMembers.map(doTranslateDto)
      val membersDirectCalls     = sortedMembers.map(dto => q"${dto.id.name.name.capitalize}_Fixture.random(rnd)")
      val membersDirectCallsJson = sortedMembers.map(dto => q"${dto.id.name.name.capitalize}_Fixture.randomJson(rnd)")

      q"""${membersFixtures.joinN()}
         |
         |class ${fixtureTpeName(adt.id)} {
         |  static $fullType random($baboonRandom rnd) => rnd.oneOf(randomAll(rnd));
         |
         |  static $fullType randomJson($baboonRandom rnd) => rnd.oneOf(randomAllJson(rnd));
         |
         |  static List<$fullType> randomAll($baboonRandom rnd) => [
         |    ${membersDirectCalls.join(",\n").shift(4).trim}
         |  ];
         |
         |  static List<$fullType> randomAllJson($baboonRandom rnd) => [
         |    ${membersDirectCallsJson.join(",\n").shift(4).trim}
         |  ];
         |}
         |""".stripMargin
    }

    private def genType(tpe: TypeRef, format: FixtureFormat): TextTree[DtValue] = {
      def gen(tpe: TypeRef): TextTree[DtValue] = {
        BaboonEnquiries.resolveBaboonRef(tpe, domain, BaboonLang.Dart) match {
          case tpe: TypeRef.Scalar => genScalar(tpe, format)
          case TypeRef.Constructor(id, args) =>
            id match {
              case Builtins.lst => q"rnd.mkList(() => ${gen(args.head)})"
              case Builtins.set => q"rnd.mkSet(() => ${gen(args.head)})"
              case Builtins.map => q"rnd.mkMap(() => ${gen(args(0))}, () => ${gen(args(1))})"
              case Builtins.opt => q"rnd.mkNullable(() => ${gen(args.head)})"
              case t            => throw new IllegalArgumentException(s"Unexpected collection type: $t")
            }
          case a: TypeRef.Any => genAnyFixture(a, format)
        }
      }

      gen(tpe)
    }

    // Stable, declaration-driven `AnyOpaque` fixture value. The meta must match the field's
    // declared variant exactly — encoder validates the kind byte. UEBA branch uses an empty
    // `Uint8List(0)` payload; JSON branch uses `null` (Dart `Object?` allows null as canonical
    // empty payload). PR-07-D01 (Dart analog): branch must match codec direction so round-trip
    // avoids cross-format conversion (which requires `withFacade` ctx). Mirrors C#/Java/Kotlin/
    // Rust/TS precedent.
    private val FixtureAnyPayloadTypeId: String = "my.test.AnyFixturePayload"

    private def genAnyFixture(a: TypeRef.Any, format: FixtureFormat): TextTree[DtValue] = {
      val hasUnderlying = a.underlying.isDefined
      val kindByte      = AnyVariant.metaKindByte(a.variant, hasUnderlying) & 0xFF
      val kindHex       = "0x%02x".format(kindByte)
      val domainStr     = q""""${domain.id.toString}""""
      val versionStr    = q""""${domain.version.v.toString}""""
      val typeidStr     = q""""$FixtureAnyPayloadTypeId""""
      val nullTree      = q"null"
      val tid           = if (hasUnderlying) nullTree else typeidStr
      val (domainExpr, versionExpr, typeidExpr) = a.variant match {
        case AnyVariant.Global  => (domainStr, versionStr, tid)
        case AnyVariant.ThisDom => (nullTree, versionStr, tid)
        case AnyVariant.Current => (nullTree, nullTree, tid)
      }
      val meta = q"$baboonAnyMeta($kindHex, $domainExpr, $versionExpr, $typeidExpr)"
      format match {
        case FixUeba => q"$baboonAnyOpaqueUeba($meta, $dtUint8List(0))"
        case FixJson => q"$baboonAnyOpaqueJson($meta, null)"
      }
    }

    private def genScalar(tpe: TypeRef.Scalar, format: FixtureFormat): TextTree[DtValue] = {
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
          val enumType = translator.toDtTypeRefKeepForeigns(u, domain, evo)
          q"rnd.mkEnum($enumType.values)"
        case u: TypeId.User =>
          val fixturePkg = translator.effectiveDtPkg(u.owner, domain, evo)
          val importAsFileName = u.owner match {
            case Owner.Adt(adtId) => translator.toSnakeCase(adtId.name.name)
            case _                => translator.toSnakeCase(u.name.name)
          }
          val fixtureType = DtValue.DtType(fixturePkg, s"${u.name.name.capitalize}_Fixture", importAs = Some(s"${importAsFileName}_fixture"))
          // Propagate the codec branch into nested user-type fixtures so any-fields nested in
          // sub-DTOs/ADTs match the same codec direction. See PR-07-D01 (Dart analog).
          val method = format match {
            case FixUeba => "random"
            case FixJson => "randomJson"
          }
          q"$fixtureType.$method(rnd)"

        case t => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
