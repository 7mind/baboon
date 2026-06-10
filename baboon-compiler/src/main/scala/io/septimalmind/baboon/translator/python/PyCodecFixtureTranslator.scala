package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.translator.python.PyKeywords.escapePyKeyword
import io.septimalmind.baboon.translator.python.PyTypes.{baboonAnyMeta, baboonAnyOpaqueJson, baboonAnyOpaqueUeba, baboonFixture, pyList, pyStaticMethod}
import io.septimalmind.baboon.translator.python.PyValue.PyType
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.{BaboonEvolution, BaboonLang, Domain, DomainMember, TypeId, TypeRef, Typedef}
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait PyCodecFixtureTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[PyValue]]
  def fixtureType(tid: TypeId.User): PyType
}

object PyCodecFixtureTranslator {
  // Branch selector for any-field fixture payloads. PR-07-D01 (Python analog): the auto-generated
  // round-trip test exercises a fixture through a single codec; if the fixture's `AnyOpaque`
  // branch matches the codec direction, no facade is needed and equality holds. We emit two
  // parallel methods per DTO (`random` for the UEBA branch, `random_json` for the JSON branch)
  // so each test path picks its native branch. Recursive calls into nested user-type fixtures
  // must propagate the same choice. Mirrors PR 2.4 / 3.4 / 4.3 / 5.4 / 6.4 / 7.4 / 8.4 / 9.4.
  private sealed trait FixtureFormat
  private case object FixUeba extends FixtureFormat
  private case object FixJson extends FixtureFormat

  final class PyCodecFixtureTranslatorImpl(
    typeTranslator: PyTypeTranslator,
    enquiries: BaboonEnquiries,
    evolution: BaboonEvolution,
    pyFileTools: PyFileTools,
    domain: Domain,
  ) extends PyCodecFixtureTranslator {
    // Synthetic typeid for `any` fixture payloads in the A/B/C variants where the wire format
    // carries the typeid string. The string is opaque from the wire-format perspective; the
    // test-emitted JSON-branch parallel fixtures use the same string for symmetry.
    private val AnyFixturePayloadTypeId: String = "my.test.AnyFixturePayload"

    override def translate(defn: DomainMember.User): Option[TextTree[PyValue]] = {
      defn.defn match {
        case _ if enquiries.hasForeignType(defn, domain, BaboonLang.Py) => None
        case _ if enquiries.isRecursiveTypedef(defn, domain)            => None
        case _: Typedef.Contract                                        => None
        case _: Typedef.Enum                                            => None
        case _: Typedef.Foreign                                         => None
        case _: Typedef.Service                                         => None
        case dto: Typedef.Dto                                           => Some(doTranslateDto(dto))
        case adt: Typedef.Adt                                           => Some(doTranslateAdt(adt))
      }
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[PyValue] = {
      def fields(format: FixtureFormat): TextTree[PyValue] =
        // Use the keyword-escaped attribute name as the constructor kwarg.
        dto.fields.map(f => q"${if (PyKeywords.isKeyword(f.name.name)) s"${f.name.name}_" else f.name.name}=${genType(f.tpe, format)}").join(",\n")

      val dtoType = typeTranslator
        .asPyType(dto.id, domain, evolution, pkgBase = pyFileTools.definitionsBasePkg)

      q"""class ${dto.id.name.name.capitalize}_Fixture:
         |    @$pyStaticMethod
         |    def random() -> $dtoType:
         |        return $dtoType(
         |            ${fields(FixUeba).shift(12).trim}
         |        )
         |
         |    @$pyStaticMethod
         |    def random_json() -> $dtoType:
         |        return $dtoType(
         |            ${fields(FixJson).shift(12).trim}
         |        )
         |""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[PyValue] = {
      val members = adt.members.toList
        .flatMap(m => domain.defs.meta.nodes.get(m))
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val sortedMembers      = members.sortBy(_.id.toString)
      val membersFixtures    = sortedMembers.map(dto => doTranslateDto(dto))
      val membersGenerators  = sortedMembers.map(dto => q"${dto.id.name.name.capitalize}_Fixture.random()")
      val membersGeneratorsJ = sortedMembers.map(dto => q"${dto.id.name.name.capitalize}_Fixture.random_json()")

      val adtType = typeTranslator
        .asPyType(adt.id, domain, evolution, pkgBase = pyFileTools.definitionsBasePkg)

      q"""class ${adt.id.name.name.capitalize}_Fixture:
         |    @$pyStaticMethod
         |    def random() -> $adtType:
         |        return $baboonFixture.oneof(${adt.id.name.name.capitalize}_Fixture.random_all())
         |
         |    @$pyStaticMethod
         |    def random_json() -> $adtType:
         |        return $baboonFixture.oneof(${adt.id.name.name.capitalize}_Fixture.random_all_json())
         |
         |    @$pyStaticMethod
         |    def random_all() -> $pyList[$adtType]:
         |        return [
         |            ${membersGenerators.join(",\n").shift(12).trim}
         |        ]
         |
         |    @$pyStaticMethod
         |    def random_all_json() -> $pyList[$adtType]:
         |        return [
         |            ${membersGeneratorsJ.join(",\n").shift(12).trim}
         |        ]
         |
         |${membersFixtures.joinN().trim}
         |""".stripMargin
    }

    private def genType(tpe: TypeRef, format: FixtureFormat): TextTree[PyValue] = {
      BaboonEnquiries.resolveBaboonRef(tpe, domain, BaboonLang.Py) match {
        case s: TypeRef.Scalar =>
          s.id match {
            case TypeId.Builtins.i08 => q"$baboonFixture.next_byte()"
            case TypeId.Builtins.i16 => q"$baboonFixture.next_i16()"
            case TypeId.Builtins.i32 => q"$baboonFixture.next_i32()"
            case TypeId.Builtins.i64 => q"$baboonFixture.next_i64()"

            case TypeId.Builtins.u08 => q"$baboonFixture.next_ubyte()"
            case TypeId.Builtins.u16 => q"$baboonFixture.next_u16()"
            case TypeId.Builtins.u32 => q"$baboonFixture.next_u32()"
            case TypeId.Builtins.u64 => q"$baboonFixture.next_u64()"

            case TypeId.Builtins.f32  => q"$baboonFixture.next_f32()"
            case TypeId.Builtins.f64  => q"$baboonFixture.next_f64()"
            case TypeId.Builtins.f128 => q"$baboonFixture.next_f128()"

            case TypeId.Builtins.str => q"$baboonFixture.next_string()"
            case TypeId.Builtins.uid => q"$baboonFixture.next_uuid()"
            case TypeId.Builtins.tsu => q"$baboonFixture.next_datetime()"
            case TypeId.Builtins.tso => q"$baboonFixture.next_datetime()"

            case TypeId.Builtins.bit => q"$baboonFixture.next_bool()"

            case TypeId.Builtins.bytes => q"$baboonFixture.next_bytes()"

            case id: TypeId.User if enquiries.isEnum(tpe, domain) =>
              val tpe = typeTranslator.asPyType(id, domain, evolution, pyFileTools.definitionsBasePkg)
              q"$baboonFixture.next_random_enum($tpe)"
            case u: TypeId.User =>
              // Propagate the codec branch into nested user-type fixtures so any-fields nested in
              // sub-DTOs/ADTs match the same codec direction. See PR-07-D01 (Python analog).
              val method = format match {
                case FixUeba => "random"
                case FixJson => "random_json"
              }
              q"${fixtureType(u)}.$method()"
            case t => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
          }
        case TypeRef.Constructor(id, args) =>
          id match {
            case Builtins.lst => q"$baboonFixture.next_list(lambda: ${genType(args.head, format)})"
            case Builtins.set => q"$baboonFixture.next_set(lambda: ${genType(args.head, format)})"
            case Builtins.map => q"$baboonFixture.next_dict(lambda: ${genType(args.head, format)}, lambda: ${genType(args.last, format)})"
            case Builtins.opt => q"$baboonFixture.next_optional(lambda: ${genType(args.head, format)})"
            case t            => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
          }
        // Variant-aware any-field fixture. The meta carries the components the wire format puts
        // on-wire — i.e. those whose bit is set in the kind byte. For D-variants
        // (`underlying.isDefined`) typeid is statically known via the surrounding type position,
        // so it is NOT on the wire and meta-typeid is None. For A/B/C variants the typeid is on
        // the wire and the fixture stamps a synthetic type-id string. The branch is selected by
        // `format`: `FixUeba` emits `AnyOpaqueUeba(meta, b'')`; `FixJson` emits
        // `AnyOpaqueJson(meta, None)` (Python's idiomatic null in `Any` position; mirrors the
        // Swift `NSNull()` decision in PR 9.4 — generated JSON encoders accept `None` as the
        // `$c` content payload).
        case a: TypeRef.Any =>
          val hasUnderlying = a.underlying.isDefined
          val kindByte      = AnyVariant.metaKindByte(a.variant, hasUnderlying) & 0xFF
          val kindHex       = "0x%02x".format(kindByte)
          val currentDom    = domain.id.toString
          val currentVer    = domain.version.v.toString
          val typeidStr     = q"""typeid="$AnyFixturePayloadTypeId""""
          val typeidNull    = q"typeid=None"
          val typeidArg     = if (hasUnderlying) typeidNull else typeidStr
          val domainStr     = q"""domain="$currentDom""""
          val domainNull    = q"domain=None"
          val versionStr    = q"""version="$currentVer""""
          val versionNull   = q"version=None"
          val (domainArg, versionArg) = a.variant match {
            case AnyVariant.Global  => (domainStr, versionStr)
            case AnyVariant.ThisDom => (domainNull, versionStr)
            case AnyVariant.Current => (domainNull, versionNull)
          }
          val meta = q"$baboonAnyMeta(kind=$kindHex, $domainArg, $versionArg, $typeidArg)"
          format match {
            case FixUeba => q"$baboonAnyOpaqueUeba($meta, b'')"
            case FixJson => q"$baboonAnyOpaqueJson($meta, None)"
          }
      }
    }

    override def fixtureType(tid: TypeId.User): PyType = {
      val typeName = s"${tid.name.name.capitalize}_Fixture"
      val pyModuleId = typeTranslator
        .toPyModule(tid, domain.version, evolution, pyFileTools.fixturesBasePkg)
        .withModuleName(typeName)
      PyType(pyModuleId, typeName)
    }
  }
}
