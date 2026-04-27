package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.translator.python.PyTypes.{baboonAnyMeta, baboonAnyOpaqueUeba, baboonFixture, pyList, pyStaticMethod}
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
  final class PyCodecFixtureTranslatorImpl(
    typeTranslator: PyTypeTranslator,
    enquiries: BaboonEnquiries,
    evolution: BaboonEvolution,
    pyFileTools: PyFileTools,
    domain: Domain,
  ) extends PyCodecFixtureTranslator {
    // Synthetic typeid for `any` fixture payloads in the A/B/C variants where the wire format
    // carries the typeid string. The string is opaque from the wire-format perspective; PR 10.4
    // will register a real codec for this typeid in the JSON-branch parallel fixtures.
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
      val generatedFields = dto.fields.map(f => q"${f.name.name}=${genType(f.tpe)}")
      val dtoType = typeTranslator
        .asPyType(dto.id, domain, evolution, pkgBase = pyFileTools.definitionsBasePkg)

      q"""class ${dto.id.name.name.capitalize}_Fixture:
         |    @$pyStaticMethod
         |    def random() -> $dtoType:
         |        return $dtoType(
         |            ${generatedFields.join(",\n").shift(12).trim}
         |        )
         |""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[PyValue] = {
      val members = adt.members.toList
        .flatMap(m => domain.defs.meta.nodes.get(m))
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val membersFixtures   = members.sortBy(_.id.toString).map(dto => doTranslateDto(dto))
      val membersGenerators = members.sortBy(_.id.toString).map(dto => q"${dto.id.name.name.capitalize}_Fixture.random()")

      val adtType = typeTranslator
        .asPyType(adt.id, domain, evolution, pkgBase = pyFileTools.definitionsBasePkg)

      q"""class ${adt.id.name.name.capitalize}_Fixture:
         |    @$pyStaticMethod
         |    def random() -> $adtType:
         |        return $baboonFixture.oneof(${adt.id.name.name.capitalize}_Fixture.random_all())
         |
         |    @$pyStaticMethod
         |    def random_all() -> $pyList[$adtType]:
         |        return [
         |            ${membersGenerators.join(",\n").shift(12).trim}
         |        ]
         |
         |${membersFixtures.joinN().trim}
         |""".stripMargin
    }

    private def genType(tpe: TypeRef): TextTree[PyValue] = {
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
            case u: TypeId.User => q"${fixtureType(u)}.random()"
            case t              => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
          }
        case TypeRef.Constructor(id, args) =>
          id match {
            case Builtins.lst => q"$baboonFixture.next_list(lambda: ${genType(args.head)})"
            case Builtins.set => q"$baboonFixture.next_set(lambda: ${genType(args.head)})"
            case Builtins.map => q"$baboonFixture.next_dict(lambda: ${genType(args.head)}, lambda: ${genType(args.last)})"
            case Builtins.opt => q"$baboonFixture.next_optional(lambda: ${genType(args.head)})"
            case t            => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
          }
        // Variant-aware UEBA-branch fixture: emits `AnyOpaqueUeba(AnyMeta(kind=0xKK, ...), b'')`.
        // The meta carries the components the wire format puts on-wire — i.e. those whose bit
        // is set in the kind byte. For D-variants (`underlying.isDefined`) typeid is statically
        // known via the surrounding type position, so it is NOT on the wire and meta-typeid is
        // None. For A/B/C variants the typeid is on the wire and the fixture stamps a synthetic
        // type-id string (`AnyFixturePayloadTypeId`). Mirrors `DtCodecFixtureTranslator.genAnyFixture`
        // / `JvCodecFixtureTranslator` shape. PR 10.4 will introduce the `FixtureFormat` ADT and
        // a parallel `random_json` method emitting `AnyOpaqueJson` for the JSON branch.
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
          q"$baboonAnyOpaqueUeba($baboonAnyMeta(kind=$kindHex, $domainArg, $versionArg, $typeidArg), b'')"
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
