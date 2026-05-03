package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.typescript.TsTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait TsCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[TsValue]]
}

object TsCodecFixtureTranslator {
  // Branch selector for any-field fixture payloads. PR-07-D01 (TS analog): the auto-test
  // round-trips a fixture through a single codec; if the fixture's `AnyOpaque` branch matches
  // the codec direction, no facade is needed and equality holds. We emit two parallel functions
  // per DTO (`random_*` for the UEBA branch, `random_*_json` for the JSON branch) so each test
  // path picks its native branch. Recursive calls into nested user-type fixtures must propagate
  // the same choice. Mirrors PR 2.4 / 3.4 / 4.3 / 5.4 / 6.4 patterns.
  private sealed trait FixtureFormat
  private case object FixUeba extends FixtureFormat
  private case object FixJson extends FixtureFormat

  final class TsCodecFixtureTranslatorImpl(
    translator: TsTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    evo: BaboonEvolution,
    tsFileTools: TsFileTools,
  ) extends TsCodecFixtureTranslator {

    import TsTypes.baboonRandom

    override def translate(definition: DomainMember.User): Option[TextTree[TsValue]] = {
      definition.defn match {
        case _ if enquiries.hasForeignType(definition, domain, BaboonLang.Typescript) => None
        case _ if enquiries.isRecursiveTypedef(definition, domain)                    => None
        case dto: Typedef.Dto                                                         => Some(doTranslateDto(dto))
        case adt: Typedef.Adt                                                         => Some(doTranslateAdt(adt))
        case _: Typedef.Contract                                                      => None
        case _: Typedef.Enum                                                          => None
        case _: Typedef.Foreign                                                       => None
        case _: Typedef.Service                                                       => None
      }
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[TsValue] = {
      val fullType = translator.asTsTypeKeepForeigns(dto.id, domain, evo, tsFileTools.definitionsBasePkg)

      def body(format: FixtureFormat): TextTree[TsValue] = {
        val generatedFields = dto.fields.map(f => q"${genType(f.tpe, format)},")
        q"""return new $fullType (
           |    ${generatedFields.joinN().shift(4).trim}
           |)""".stripMargin
      }

      q"""export function ${fixtureFnName(dto.id, FixUeba)}(rnd: $baboonRandom): $fullType {
         |    ${body(FixUeba).shift(4).trim}
         |}
         |
         |export function ${fixtureFnName(dto.id, FixJson)}(rnd: $baboonRandom): $fullType {
         |    ${body(FixJson).shift(4).trim}
         |}""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[TsValue] = {
      val adtName = translator.asTsType(adt.id, domain, evo, tsFileTools.definitionsBasePkg)
      val members = adt.members.toList
        .flatMap(domain.defs.meta.nodes.get)
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val sortedMembers   = members.sortBy(_.id.toString)
      val membersFixtures = sortedMembers.map(doTranslateDtoPrivate)
      val uebaCalls       = sortedMembers.map(dto => q"${fixtureFnName(dto.id, FixUeba)}(rnd)")
      val jsonCalls       = sortedMembers.map(dto => q"${fixtureFnName(dto.id, FixJson)}(rnd)")
      val uebaAllEntries  = uebaCalls.map(g => q"$g,")
      val jsonAllEntries  = jsonCalls.map(g => q"$g,")

      q"""export function ${fixtureFnName(adt.id, FixUeba)}(rnd: $baboonRandom): $adtName {
         |    const all = ${fixtureFnName(adt.id, FixUeba)}_all(rnd);
         |    const idx = rnd.nextUSize(all.length);
         |    return all[idx]!;
         |}
         |
         |export function ${fixtureFnName(adt.id, FixUeba)}_all(rnd: $baboonRandom): $adtName[] {
         |    return [
         |        ${uebaAllEntries.joinN().shift(8).trim}
         |    ];
         |}
         |
         |export function ${fixtureFnName(adt.id, FixJson)}(rnd: $baboonRandom): $adtName {
         |    const all = ${fixtureFnName(adt.id, FixJson)}_all(rnd);
         |    const idx = rnd.nextUSize(all.length);
         |    return all[idx]!;
         |}
         |
         |export function ${fixtureFnName(adt.id, FixJson)}_all(rnd: $baboonRandom): $adtName[] {
         |    return [
         |        ${jsonAllEntries.joinN().shift(8).trim}
         |    ];
         |}
         |
         |${membersFixtures.joinNN()}""".stripMargin
    }

    private def doTranslateDtoPrivate(dto: Typedef.Dto): TextTree[TsValue] = {
      val fullType = translator.asTsTypeKeepForeigns(dto.id, domain, evo, tsFileTools.definitionsBasePkg)

      def body(format: FixtureFormat): TextTree[TsValue] = {
        val generatedFields = dto.fields.map(f => q"${genType(f.tpe, format)},")
        q"""return new $fullType (
           |    ${generatedFields.joinN().shift(4).trim}
           |)""".stripMargin
      }

      q"""function ${fixtureFnName(dto.id, FixUeba)}(rnd: $baboonRandom): $fullType {
         |    ${body(FixUeba).shift(4).trim}
         |}
         |
         |function ${fixtureFnName(dto.id, FixJson)}(rnd: $baboonRandom): $fullType {
         |    ${body(FixJson).shift(4).trim}
         |}""".stripMargin
    }

    private def fixtureFnName(id: TypeId, format: FixtureFormat): String = {
      val base = s"random_${translator.camelToKebab(id.name.name).replace('-', '_')}"
      format match {
        case FixUeba => base
        case FixJson => s"${base}_json"
      }
    }

    private def fixtureFnRef(id: TypeId.User, format: FixtureFormat): TsValue.TsType = {
      val userType      = translator.asTsTypeKeepForeigns(id, domain, evo, tsFileTools.fixturesBasePkg)
      val partsList     = userType.moduleId.path
      val fixtureModule = TsValue.TsModuleId(partsList.init :+ (partsList.last + ".fixture"))
      TsValue.TsType(fixtureModule, fixtureFnName(id, format))
    }

    private def genType(tpe: TypeRef, format: FixtureFormat): TextTree[TsValue] = {
      BaboonEnquiries.resolveBaboonRef(tpe, domain, BaboonLang.Typescript) match {
        case tpe: TypeRef.Scalar => genScalar(tpe, format)
        case TypeRef.Constructor(id, args) =>
          id match {
            case Builtins.lst =>
              q"Array.from({ length: rnd.nextUSize(5) }, () => ${genType(args.head, format)})"
            case Builtins.set =>
              q"new Set(Array.from({ length: rnd.nextUSize(5) }, () => ${genType(args.head, format)}))"
            case Builtins.map =>
              val isRecord = translator.isStringKeyMap(TypeRef.Constructor(id, args))
              if (isRecord)
                q"Object.fromEntries(Array.from({ length: rnd.nextUSize(5) }, () => [${genType(args(0), format)}, ${genType(args(1), format)}] as const))"
              else
                q"new Map(Array.from({ length: rnd.nextUSize(5) }, () => [${genType(args(0), format)}, ${genType(args(1), format)}] as const))"
            case Builtins.opt =>
              q"(rnd.nextBit() ? ${genType(args.head, format)} : undefined)"
            case t => throw new IllegalArgumentException(s"Unexpected collection type: $t")
          }
        case a: TypeRef.Any => genAnyFixture(a, format)
      }
    }

    // Stable, declaration-driven `AnyOpaque` fixture value. The meta must match the field's
    // declared variant exactly — encoder validates the kind byte. UEBA branch uses an empty
    // `Uint8Array(0)` payload; JSON branch uses `null` (the canonical empty unknown payload).
    // PR-07-D01 (TS analog): branch must match codec direction so round-trip avoids cross-format
    // conversion (which requires `withFacade` ctx). Mirrors Java/Kotlin/Rust precedent.
    private val FixtureAnyPayloadTypeId: String = "my.test.AnyFixturePayload"

    private def genAnyFixture(a: TypeRef.Any, format: FixtureFormat): TextTree[TsValue] = {
      val hasUnderlying = a.underlying.isDefined
      val kindByte      = AnyVariant.metaKindByte(a.variant, hasUnderlying) & 0xFF
      val kindHex       = "0x%02x".format(kindByte)
      val domainStr     = q""""${domain.id.toString}""""
      val versionStr    = q""""${domain.version.v.toString}""""
      val typeidStr     = q""""$FixtureAnyPayloadTypeId""""
      val nullTree      = q"null"
      val (domainExpr, versionExpr, typeidExpr) = a.variant match {
        case AnyVariant.Global =>
          val tid = if (hasUnderlying) nullTree else typeidStr
          (domainStr, versionStr, tid)
        case AnyVariant.ThisDom =>
          val tid = if (hasUnderlying) nullTree else typeidStr
          (nullTree, versionStr, tid)
        case AnyVariant.Current =>
          val tid = if (hasUnderlying) nullTree else typeidStr
          (nullTree, nullTree, tid)
      }
      val meta = q"$tsBaboonCreateAnyMeta($kindHex, $domainExpr, $versionExpr, $typeidExpr)"
      format match {
        case FixUeba => q"$tsBaboonAnyOpaqueUebaCtor($meta, new Uint8Array(0))"
        case FixJson => q"$tsBaboonAnyOpaqueJsonCtor($meta, null)"
      }
    }

    private def genScalar(tpe: TypeRef.Scalar, format: FixtureFormat): TextTree[TsValue] = {
      tpe.id match {
        case TypeId.Builtins.i08   => q"rnd.nextI08()"
        case TypeId.Builtins.i16   => q"rnd.nextI16()"
        case TypeId.Builtins.i32   => q"rnd.nextI32()"
        case TypeId.Builtins.i64   => q"rnd.nextI64()"
        case TypeId.Builtins.u08   => q"rnd.nextU08()"
        case TypeId.Builtins.u16   => q"rnd.nextU16()"
        case TypeId.Builtins.u32   => q"rnd.nextU32()"
        case TypeId.Builtins.u64   => q"rnd.nextU64()"
        case TypeId.Builtins.f32   => q"rnd.nextF32()"
        case TypeId.Builtins.f64   => q"rnd.nextF64()"
        case TypeId.Builtins.f128  => q"rnd.nextF128()"
        case TypeId.Builtins.str   => q"rnd.nextString()"
        case TypeId.Builtins.bytes => q"rnd.nextBytes()"
        case TypeId.Builtins.uid   => q"rnd.nextUid()"
        case TypeId.Builtins.tsu =>
          translator.asTsType(TypeId.Builtins.tsu, domain, evo) match {
            case TsTypes.tsString => q"rnd.nextTsu().toISOString()"
            case TsTypes.tsDate   => q"rnd.nextTsu().date"
            case _                => q"rnd.nextTsu()"
          }
        case TypeId.Builtins.tso =>
          translator.asTsType(TypeId.Builtins.tso, domain, evo) match {
            case TsTypes.tsString => q"rnd.nextTso().toISOString()"
            case TsTypes.tsDate   => q"rnd.nextTso().date"
            case _                => q"rnd.nextTso()"
          }
        case TypeId.Builtins.bit => q"rnd.nextBit()"

        case u: TypeId.User if enquiries.isEnum(tpe, domain) =>
          val enumType   = translator.asTsType(u, domain, evo, tsFileTools.definitionsBasePkg)
          val enumValues = TsValue.TsType(enumType.moduleId, s"${enumType.name}_values")
          q"rnd.mkEnum($enumValues)"
        case u: TypeId.User =>
          // Propagate the codec branch into nested user-type fixtures so any-fields nested in
          // sub-DTOs/ADTs match the same codec direction. See PR-07-D01 (TS analog).
          val fnRef = fixtureFnRef(u, format)
          q"$fnRef(rnd)"
        case t => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
