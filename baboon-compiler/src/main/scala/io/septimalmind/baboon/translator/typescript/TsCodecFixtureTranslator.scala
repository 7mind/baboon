package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait TsCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[TsValue]]
}

object TsCodecFixtureTranslator {
  final class TsCodecFixtureTranslatorImpl(
    translator: TsTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends TsCodecFixtureTranslator {

    import TsTypes.baboonRandom

    override def translate(definition: DomainMember.User): Option[TextTree[TsValue]] = {
      definition.defn match {
        case _ if enquiries.hasForeignType(definition, domain, BaboonLang.Typescript) => None
        case _ if enquiries.isRecursiveTypedef(definition, domain) => None
        case dto: Typedef.Dto                                      => Some(doTranslateDto(dto))
        case adt: Typedef.Adt                                      => Some(doTranslateAdt(adt))
        case _: Typedef.Contract                                   => None
        case _: Typedef.Enum                                       => None
        case _: Typedef.Foreign                                    => None
        case _: Typedef.Service                                    => None
      }
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[TsValue] = {
      val generatedFields = dto.fields.map {
        f =>
          q"${f.name.name}: ${genType(f.tpe)},"
      }
      val fullType = translator.toTsTypeRefKeepForeigns(dto.id, domain, evo)

      q"""export function ${fixtureFnName(dto.id)}(rnd: $baboonRandom): $fullType {
         |    return {
         |        ${generatedFields.joinN().shift(8).trim}
         |    };
         |}""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[TsValue] = {
      val adtName = translator.asTsType(adt.id, domain, evo)
      val members = adt.members.toList
        .flatMap(domain.defs.meta.nodes.get)
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val membersFixtures = members.sortBy(_.id.toString).map(doTranslateDtoPrivate)
      val membersGenerators = members.sortBy(_.id.toString).map {
        dto =>
          val branchName = dto.id.name.name
          val factoryFn  = TsValue.TsType(adtName.module, s"${adtName.name}_$branchName")
          q"$factoryFn(${fixtureFnName(dto.id)}(rnd))"
      }

      val randomAllEntries = membersGenerators.map(g => q"$g,")

      q"""export function ${fixtureFnName(adt.id)}(rnd: $baboonRandom): $adtName {
         |    const all = ${fixtureFnName(adt.id)}_all(rnd);
         |    const idx = rnd.nextUSize(all.length);
         |    return all[idx];
         |}
         |
         |export function ${fixtureFnName(adt.id)}_all(rnd: $baboonRandom): $adtName[] {
         |    return [
         |        ${randomAllEntries.joinN().shift(8).trim}
         |    ];
         |}
         |
         |${membersFixtures.joinNN()}""".stripMargin
    }

    private def doTranslateDtoPrivate(dto: Typedef.Dto): TextTree[TsValue] = {
      val generatedFields = dto.fields.map {
        f =>
          q"${f.name.name}: ${genType(f.tpe)},"
      }
      val fullType = translator.toTsTypeRefKeepForeigns(dto.id, domain, evo)

      q"""function ${fixtureFnName(dto.id)}(rnd: $baboonRandom): $fullType {
         |    return {
         |        ${generatedFields.joinN().shift(8).trim}
         |    };
         |}""".stripMargin
    }

    private def fixtureFnName(id: TypeId): String = {
      s"random_${translator.camelToKebab(id.name.name).replace('-', '_')}"
    }

    private def fixtureFnRef(id: TypeId.User): TsValue.TsType = {
      val userType      = translator.toTsTypeRefKeepForeigns(id, domain, evo)
      val partsList     = userType.module.parts.toList
      val fixtureModule = TsValue.TsModuleId(NEList.unsafeFrom(partsList.init :+ (partsList.last + ".fixture")))
      TsValue.TsType(fixtureModule, fixtureFnName(id))
    }

    private def genType(tpe: TypeRef): TextTree[TsValue] = {
      BaboonEnquiries.resolveBaboonRef(tpe, domain, BaboonLang.Typescript) match {
        case tpe: TypeRef.Scalar => genScalar(tpe)
        case TypeRef.Constructor(id, args) =>
          id match {
            case Builtins.lst =>
              q"Array.from({ length: rnd.nextUSize(5) }, () => ${genType(args.head)})"
            case Builtins.set =>
              q"new Set(Array.from({ length: rnd.nextUSize(5) }, () => ${genType(args.head)}))"
            case Builtins.map =>
              q"new Map(Array.from({ length: rnd.nextUSize(5) }, () => [${genType(args(0))}, ${genType(args(1))}] as const))"
            case Builtins.opt =>
              q"(rnd.nextBit() ? ${genType(args.head)} : undefined)"
            case t => throw new IllegalArgumentException(s"Unexpected collection type: $t")
          }
      }
    }

    private def genScalar(tpe: TypeRef.Scalar): TextTree[TsValue] = {
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
        case TypeId.Builtins.tsu   => q"rnd.nextTsu()"
        case TypeId.Builtins.tso   => q"rnd.nextTso()"
        case TypeId.Builtins.bit   => q"rnd.nextBit()"

        case u: TypeId.User if enquiries.isEnum(tpe, domain) =>
          val enumType   = translator.asTsType(u, domain, evo)
          val enumValues = TsValue.TsType(enumType.module, s"${enumType.name}_values")
          q"rnd.mkEnum($enumValues)"
        case u: TypeId.User =>
          val fnRef = fixtureFnRef(u)
          q"$fnRef(rnd)"
        case t => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
