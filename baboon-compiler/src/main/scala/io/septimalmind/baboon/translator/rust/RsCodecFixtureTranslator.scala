package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.{escapeRustTypeName, toSnakeCase, toSnakeCaseRaw}
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait RsCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[RsValue]]
}

object RsCodecFixtureTranslator {
  final class RsCodecFixtureTranslatorImpl(
    target: RsTarget,
    translator: RsTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends RsCodecFixtureTranslator {

    override def translate(definition: DomainMember.User): Option[TextTree[RsValue]] = {
      definition.defn match {
        case _ if enquiries.hasForeignType(definition, domain, BaboonLang.Rust) => None
        case _ if enquiries.isRecursiveTypedef(definition, domain)              => None
        case dto: Typedef.Dto                                                   => Some(doTranslateDto(dto))
        case adt: Typedef.Adt                                                   => Some(doTranslateAdt(adt))
        case _: Typedef.Contract                                                => None
        case _: Typedef.Enum                                                    => None
        case _: Typedef.Foreign                                                 => None
        case _: Typedef.Service                                                 => None
      }
    }

    // Branch selector for any-field fixture payloads. PR-07-D01 (Rust analog): the auto-test
    // round-trips a fixture through a single codec; if the fixture's `AnyOpaque` branch matches
    // the codec direction, no facade is needed and equality holds. We emit two parallel
    // functions per DTO (`random_<n>` for the UEBA branch, `random_<n>_json` for the JSON
    // branch) so each test path picks its native branch. Recursive calls into nested user-type
    // fixtures must propagate the same choice.
    private sealed trait FixtureFormat
    private case object FixUeba extends FixtureFormat
    private case object FixJson extends FixtureFormat

    private def doTranslateDto(dto: Typedef.Dto): TextTree[RsValue] = {
      val fullType = translator.toRsTypeRefKeepForeigns(dto.id, domain, evo)

      def body(format: FixtureFormat): TextTree[RsValue] = {
        val generatedFields = dto.fields.map {
          f =>
            q"${toSnakeCase(f.name.name)}: ${genType(f.tpe, format)},"
        }
        q"""$fullType {
           |    ${generatedFields.joinN().shift(4).trim}
           |}""".stripMargin
      }

      val rndParam = if (dto.fields.isEmpty) "_rnd" else "rnd"

      q"""pub fn ${fixtureFnName(dto.id)}($rndParam: &mut crate::baboon_fixture::BaboonRandom) -> $fullType {
         |    ${body(FixUeba).shift(4).trim}
         |}
         |
         |pub fn ${fixtureFnNameJson(dto.id)}($rndParam: &mut crate::baboon_fixture::BaboonRandom) -> $fullType {
         |    ${body(FixJson).shift(4).trim}
         |}""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[RsValue] = {
      val adtName = translator.asRsType(adt.id, domain, evo)
      val members = adt.members.toList
        .flatMap(domain.defs.meta.nodes.get)
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val membersFixtures = members.sortBy(_.id.toString).flatMap(d => List(doTranslateDtoPrivate(d, FixUeba), doTranslateDtoPrivate(d, FixJson)))

      val membersGenerators = members.sortBy(_.id.toString).map {
        dto =>
          val branchName = escapeRustTypeName(dto.id.name.name.capitalize)
          q"$adtName::$branchName(${fixtureFnName(dto.id)}(rnd))"
      }
      val membersGeneratorsJson = members.sortBy(_.id.toString).map {
        dto =>
          val branchName = escapeRustTypeName(dto.id.name.name.capitalize)
          q"$adtName::$branchName(${fixtureFnNameJson(dto.id)}(rnd))"
      }

      val randomAllEntries     = membersGenerators.map(g => q"$g,")
      val randomAllEntriesJson = membersGeneratorsJson.map(g => q"$g,")

      q"""pub fn ${fixtureFnName(adt.id)}(rnd: &mut crate::baboon_fixture::BaboonRandom) -> $adtName {
         |    let all = ${fixtureFnName(adt.id)}_all(rnd);
         |    let idx = rnd.next_usize(all.len());
         |    all.into_iter().nth(idx).unwrap()
         |}
         |
         |pub fn ${fixtureFnName(adt.id)}_all(rnd: &mut crate::baboon_fixture::BaboonRandom) -> Vec<$adtName> {
         |    vec![
         |        ${randomAllEntries.joinN().shift(8).trim}
         |    ]
         |}
         |
         |pub fn ${fixtureFnNameJson(adt.id)}(rnd: &mut crate::baboon_fixture::BaboonRandom) -> $adtName {
         |    let all = ${fixtureFnNameJson(adt.id)}_all(rnd);
         |    let idx = rnd.next_usize(all.len());
         |    all.into_iter().nth(idx).unwrap()
         |}
         |
         |pub fn ${fixtureFnNameJson(adt.id)}_all(rnd: &mut crate::baboon_fixture::BaboonRandom) -> Vec<$adtName> {
         |    vec![
         |        ${randomAllEntriesJson.joinN().shift(8).trim}
         |    ]
         |}
         |
         |${membersFixtures.joinNN()}""".stripMargin
    }

    private def doTranslateDtoPrivate(dto: Typedef.Dto, format: FixtureFormat): TextTree[RsValue] = {
      val generatedFields = dto.fields.map {
        f =>
          q"${toSnakeCase(f.name.name)}: ${genType(f.tpe, format)},"
      }
      val fullType = translator.toRsTypeRefKeepForeigns(dto.id, domain, evo)
      val rndParam = if (dto.fields.isEmpty) "_rnd" else "rnd"
      val name = format match {
        case FixUeba => fixtureFnName(dto.id)
        case FixJson => fixtureFnNameJson(dto.id)
      }

      q"""fn $name($rndParam: &mut crate::baboon_fixture::BaboonRandom) -> $fullType {
         |    $fullType {
         |        ${generatedFields.joinN().shift(8).trim}
         |    }
         |}""".stripMargin
    }

    private def fixtureFnName(id: TypeId): String = {
      s"random_${toSnakeCaseRaw(id.name.name)}"
    }

    private def fixtureFnNameJson(id: TypeId): String = {
      s"random_${toSnakeCaseRaw(id.name.name)}_json"
    }

    private def genType(tpe: TypeRef, format: FixtureFormat): TextTree[RsValue] = {
      BaboonEnquiries.resolveBaboonRef(tpe, domain, BaboonLang.Rust) match {
        case tpe: TypeRef.Scalar => genScalar(tpe, format)
        case TypeRef.Constructor(id, args) =>
          id match {
            case Builtins.lst =>
              q"""{ let n = rnd.next_usize(5); (0..n).map(|_| ${genType(args.head, format)}).collect::<Vec<_>>() }"""
            case Builtins.set =>
              q"""{ let n = rnd.next_usize(5); (0..n).map(|_| ${genType(args.head, format)}).collect::<std::collections::BTreeSet<_>>() }"""
            case Builtins.map =>
              q"""{ let n = rnd.next_usize(5); (0..n).map(|_| (${genType(args(0), format)}, ${genType(
                  args(1),
                  format,
                )})).collect::<std::collections::BTreeMap<_, _>>() }"""
            case Builtins.opt =>
              q"if rnd.next_bit() { Some(${genType(args.head, format)}) } else { None }"
            case t => throw new IllegalArgumentException(s"Unexpected collection type: $t")
          }
        case a: TypeRef.Any => genAnyFixture(a, format)
      }
    }

    // Stable, declaration-driven `AnyOpaque` value. Meta must match the field's declared variant
    // exactly — encoder validates the kind byte. UEBA branch uses empty bytes; JSON branch uses
    // `serde_json::Value::Null`. The typeid string is only used by typed-variant kinds (D1/D2/D3)
    // where `hasUnderlying` is false on the meta side.
    private val FixtureAnyPayloadTypeId: String = "my.test.AnyFixturePayload"

    private def genAnyFixture(a: TypeRef.Any, format: FixtureFormat): TextTree[RsValue] = {
      val hasUnderlying = a.underlying.isDefined
      val kindHex       = "0x%02x".format(AnyVariant.metaKindByte(a.variant, hasUnderlying) & 0xFF)

      val domainStr  = q""""${domain.id.toString}""""
      val versionStr = q""""${domain.version.v.toString}""""
      val typeidStr  = q""""$FixtureAnyPayloadTypeId""""

      val (domainExpr, versionExpr, typeidExpr) = a.variant match {
        case AnyVariant.Global =>
          val tid = if (hasUnderlying) q"None" else q"Some($typeidStr.to_string())"
          (q"Some($domainStr.to_string())", q"Some($versionStr.to_string())", tid)
        case AnyVariant.ThisDom =>
          val tid = if (hasUnderlying) q"None" else q"Some($typeidStr.to_string())"
          (q"None", q"Some($versionStr.to_string())", tid)
        case AnyVariant.Current =>
          val tid = if (hasUnderlying) q"None" else q"Some($typeidStr.to_string())"
          (q"None", q"None", tid)
      }

      val meta =
        q"""crate::any_opaque::AnyMeta::new($kindHex, $domainExpr, $versionExpr, $typeidExpr)
           |    .expect("BUG: codegen-emitted any-fixture meta must be valid")""".stripMargin

      // PR-07-D01 (Rust analog): branch must match codec direction so round-trip avoids cross-
      // format conversion (which requires `with_facade` ctx). UEBA codec test uses
      // `AnyOpaqueUeba(meta, [])`; JSON codec test uses `AnyOpaqueJson(meta, Value::Null)`. Both
      // are wire-equivalent to themselves under their native codec — equality holds.
      format match {
        case FixUeba =>
          q"""crate::any_opaque::AnyOpaque::Ueba(crate::any_opaque::AnyOpaqueUeba::new(
             |    ${meta.shift(4).trim},
             |    Vec::new(),
             |))""".stripMargin
        case FixJson =>
          q"""crate::any_opaque::AnyOpaque::Json(crate::any_opaque::AnyOpaqueJson::new(
             |    ${meta.shift(4).trim},
             |    serde_json::Value::Null,
             |))""".stripMargin
      }
    }

    private def genScalar(tpe: TypeRef.Scalar, format: FixtureFormat): TextTree[RsValue] = {
      tpe.id match {
        case TypeId.Builtins.i08   => q"rnd.next_i08()"
        case TypeId.Builtins.i16   => q"rnd.next_i16()"
        case TypeId.Builtins.i32   => q"rnd.next_i32()"
        case TypeId.Builtins.i64   => q"rnd.next_i64()"
        case TypeId.Builtins.u08   => q"rnd.next_u08()"
        case TypeId.Builtins.u16   => q"rnd.next_u16()"
        case TypeId.Builtins.u32   => q"rnd.next_u32()"
        case TypeId.Builtins.u64   => q"rnd.next_u64()"
        case TypeId.Builtins.f32   => q"rnd.next_f32()"
        case TypeId.Builtins.f64   => q"rnd.next_f64()"
        case TypeId.Builtins.f128  => q"rnd.next_f128()"
        case TypeId.Builtins.str   => q"rnd.next_string()"
        case TypeId.Builtins.bytes => q"rnd.next_bytes()"
        case TypeId.Builtins.uid   => q"rnd.next_uid()"
        case TypeId.Builtins.tsu   => q"rnd.next_tsu()"
        case TypeId.Builtins.tso   => q"rnd.next_tso()"
        case TypeId.Builtins.bit   => q"rnd.next_bit()"

        case u: TypeId.User if enquiries.isEnum(tpe, domain) =>
          val enumType = translator.asRsType(u, domain, evo)
          q"rnd.mk_enum(&$enumType::all())"
        case u: TypeId.User =>
          // Propagate the codec branch into nested user-type fixtures so any-fields nested in
          // sub-DTOs/ADTs match the same codec direction. See PR-07-D01 (Rust analog).
          val name = format match {
            case FixUeba => fixtureFnName(u)
            case FixJson => fixtureFnNameJson(u)
          }
          q"super::$name(rnd)"
        case t => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
