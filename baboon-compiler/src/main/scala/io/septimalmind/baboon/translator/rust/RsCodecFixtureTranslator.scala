package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.{toSnakeCase, toSnakeCaseRaw}
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

    private def doTranslateDto(dto: Typedef.Dto): TextTree[RsValue] = {
      val generatedFields = dto.fields.map {
        f =>
          q"${toSnakeCase(f.name.name)}: ${genType(f.tpe)},"
      }
      val fullType = translator.toRsTypeRefKeepForeigns(dto.id, domain, evo)
      val rndParam = if (dto.fields.isEmpty) "_rnd" else "rnd"

      q"""pub fn ${fixtureFnName(dto.id)}($rndParam: &mut crate::baboon_fixture::BaboonRandom) -> $fullType {
         |    $fullType {
         |        ${generatedFields.joinN().shift(8).trim}
         |    }
         |}""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[RsValue] = {
      val adtName = translator.asRsType(adt.id, domain, evo)
      val members = adt.members.toList
        .flatMap(domain.defs.meta.nodes.get)
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val membersFixtures = members.sortBy(_.id.toString).map(doTranslateDtoPrivate)
      val membersGenerators = members.sortBy(_.id.toString).map {
        dto =>
          val branchName = dto.id.name.name.capitalize
          q"$adtName::$branchName(${fixtureFnName(dto.id)}(rnd))"
      }

      val randomAllEntries = membersGenerators.map(g => q"$g,")

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
         |${membersFixtures.joinNN()}""".stripMargin
    }

    private def doTranslateDtoPrivate(dto: Typedef.Dto): TextTree[RsValue] = {
      val generatedFields = dto.fields.map {
        f =>
          q"${toSnakeCase(f.name.name)}: ${genType(f.tpe)},"
      }
      val fullType = translator.toRsTypeRefKeepForeigns(dto.id, domain, evo)
      val rndParam = if (dto.fields.isEmpty) "_rnd" else "rnd"

      q"""fn ${fixtureFnName(dto.id)}($rndParam: &mut crate::baboon_fixture::BaboonRandom) -> $fullType {
         |    $fullType {
         |        ${generatedFields.joinN().shift(8).trim}
         |    }
         |}""".stripMargin
    }

    private def fixtureFnName(id: TypeId): String = {
      s"random_${toSnakeCaseRaw(id.name.name)}"
    }

    private def genType(tpe: TypeRef): TextTree[RsValue] = {
      BaboonEnquiries.resolveBaboonRef(tpe, domain, BaboonLang.Rust) match {
        case tpe: TypeRef.Scalar => genScalar(tpe)
        case TypeRef.Constructor(id, args) =>
          id match {
            case Builtins.lst =>
              q"""{ let n = rnd.next_usize(5); (0..n).map(|_| ${genType(args.head)}).collect::<Vec<_>>() }"""
            case Builtins.set =>
              q"""{ let n = rnd.next_usize(5); (0..n).map(|_| ${genType(args.head)}).collect::<std::collections::BTreeSet<_>>() }"""
            case Builtins.map =>
              q"""{ let n = rnd.next_usize(5); (0..n).map(|_| (${genType(args(0))}, ${genType(args(1))})).collect::<std::collections::BTreeMap<_, _>>() }"""
            case Builtins.opt =>
              q"if rnd.next_bit() { Some(${genType(args.head)}) } else { None }"
            case t => throw new IllegalArgumentException(s"Unexpected collection type: $t")
          }
        case a: TypeRef.Any => genAnyFixture(a)
      }
    }

    // Branch-matching fixtures (mirroring Scala's PR-07-D01 fix and C# PR 3.4) belong to PR 4.3.
    // PR 4.2 emits the `AnyOpaqueUeba` branch only — auto-tests for the JSON path will need a
    // companion that emits `AnyOpaqueJson`, plus a `FixtureFormat` selector. For now both UEBA
    // and JSON tests round-trip via the UEBA branch; the JSON path test will pass once PR 4.3
    // splits the fixture. Bytes are empty: the fixture only has to compile and yield a value
    // with the right meta-kind; the encoder asserts `meta.kind == expectedKind` and copies the
    // bytes through verbatim. `AnyMeta::new` returns Result; `expect` surfaces logic errors
    // clearly (the codegen-emitted variants are always valid by construction).
    private val FixtureAnyPayloadTypeId: String = "my.test.AnyFixturePayload"

    private def genAnyFixture(a: TypeRef.Any): TextTree[RsValue] = {
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

      q"""crate::any_opaque::AnyOpaque::Ueba(crate::any_opaque::AnyOpaqueUeba::new(
         |    crate::any_opaque::AnyMeta::new($kindHex, $domainExpr, $versionExpr, $typeidExpr)
         |        .expect("BUG: codegen-emitted any-fixture meta must be valid"),
         |    Vec::new(),
         |))""".stripMargin
    }

    private def genScalar(tpe: TypeRef.Scalar): TextTree[RsValue] = {
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
          q"super::${fixtureFnName(u)}(rnd)"
        case t => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
