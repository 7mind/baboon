package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait SwCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[SwValue]]
  def fixtureTpe(definition: DomainMember.User): Option[TextTree[SwValue]]
}

object SwCodecFixtureTranslator {
  // Branch selector for any-field fixture payloads. PR-07-D01 (Swift analog): the auto-generated
  // round-trip test exercises a fixture through a single codec; if the fixture's `AnyOpaque`
  // branch matches the codec direction, no facade is needed and equality holds. We emit two
  // parallel methods per DTO (`random` for the UEBA branch, `randomJson` for the JSON branch)
  // so each test path picks its native branch. Recursive calls into nested user-type fixtures
  // must propagate the same choice. Mirrors PR 2.4 / 3.4 / 4.3 / 5.4 / 6.4 / 7.4 / 8.4 patterns.
  private sealed trait FixtureFormat
  private case object FixUeba extends FixtureFormat
  private case object FixJson extends FixtureFormat

  final class Impl(
    translator: SwTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends SwCodecFixtureTranslator {

    override def translate(
      definition: DomainMember.User
    ): Option[TextTree[SwValue]] = {
      definition.defn match {
        case _ if enquiries.hasForeignType(definition, domain)     => None
        case _ if enquiries.isRecursiveTypedef(definition, domain) => None
        case dto: Typedef.Dto                                      => Some(doTranslateDto(dto))
        case adt: Typedef.Adt                                      => Some(doTranslateAdt(adt))
        case _: Typedef.Contract                                   => None
        case _: Typedef.Enum                                       => None
        case _: Typedef.Foreign                                    => None
        case _: Typedef.Service                                    => None
      }
    }

    override def fixtureTpe(definition: DomainMember.User): Option[TextTree[SwValue]] = {
      defnFixtureId(definition)
    }

    private def defnFixtureId(definition: DomainMember): Option[TextTree[SwValue]] = {
      definition match {
        case _: DomainMember.Builtin => None
        case u: DomainMember.User =>
          u.defn match {
            case _ if enquiries.hasForeignType(u, domain)     => None
            case _ if enquiries.isRecursiveTypedef(u, domain) => None
            case _: Typedef.Contract                          => None
            case _: Typedef.Enum                              => None
            case _: Typedef.Foreign                           => None
            case _: Typedef.Service                           => None
            case dto: Typedef.Dto                             => Some(fixtureTpeName(dto.id))
            case adt: Typedef.Adt                             => Some(fixtureTpeName(adt.id))
          }
      }
    }

    private def fixtureTpeName(id: TypeId.User): TextTree[SwValue] = {
      q"${translator.fixtureClassName(id, domain, evo)}"
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[SwValue] = {
      val fullType = translator.toSwTypeRefKeepForeigns(dto.id, domain, evo)

      def fields(format: FixtureFormat): TextTree[SwValue] = {
        dto.fields.map { f =>
          q"${translator.escapeSwiftKeyword(f.name.name)}: ${genType(f.tpe, format)}"
        }.join(",\n")
      }

      q"""public class ${fixtureTpeName(dto.id)} {
         |    public static func random(_ rnd: $baboonRandom) -> $fullType {
         |        return $fullType(
         |            ${fields(FixUeba).shift(12).trim}
         |        )
         |    }
         |
         |    public static func randomJson(_ rnd: $baboonRandom) -> $fullType {
         |        return $fullType(
         |            ${fields(FixJson).shift(12).trim}
         |        )
         |    }
         |}
         |""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[SwValue] = {
      val fullType = translator.toSwTypeRefKeepForeigns(adt.id, domain, evo)
      val members = adt.members.toList
        .flatMap(domain.defs.meta.nodes.get)
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val sortedMembers   = members.sortBy(_.id.toString)
      val membersFixtures = sortedMembers.map(doTranslateDto)
      def caseCalls(method: String): List[TextTree[SwValue]] = sortedMembers.map { dto =>
        val caseName = dto.id.name.name.head.toLower.toString + dto.id.name.name.tail
        q".$caseName(${fixtureTpeName(dto.id)}.$method(rnd))"
      }
      val membersDirectCalls     = caseCalls("random")
      val membersDirectCallsJson = caseCalls("randomJson")

      q"""${membersFixtures.joinN()}
         |
         |public class ${fixtureTpeName(adt.id)} {
         |    public static func random(_ rnd: $baboonRandom) -> $fullType {
         |        return rnd.oneOf(randomAll(rnd))
         |    }
         |
         |    public static func randomJson(_ rnd: $baboonRandom) -> $fullType {
         |        return rnd.oneOf(randomAllJson(rnd))
         |    }
         |
         |    public static func randomAll(_ rnd: $baboonRandom) -> [$fullType] {
         |        return [
         |            ${membersDirectCalls.join(",\n").shift(12).trim}
         |        ]
         |    }
         |
         |    public static func randomAllJson(_ rnd: $baboonRandom) -> [$fullType] {
         |        return [
         |            ${membersDirectCallsJson.join(",\n").shift(12).trim}
         |        ]
         |    }
         |}
         |""".stripMargin
    }

    private def genType(tpe: TypeRef, format: FixtureFormat): TextTree[SwValue] = {
      def gen(tpe: TypeRef): TextTree[SwValue] = {
        tpe match {
          case tpe: TypeRef.Scalar => genScalar(tpe, format)
          case TypeRef.Constructor(id, args) =>
            id match {
              case Builtins.lst => q"rnd.mkList { ${gen(args.head)} }"
              case Builtins.set => q"rnd.mkSet { ${gen(args.head)} }"
              case Builtins.map => q"rnd.mkMap({ ${gen(args(0))} }, { ${gen(args(1))} })"
              case Builtins.opt => q"rnd.mkOptional { ${gen(args.head)} }"
              case t            => throw new IllegalArgumentException(s"Unexpected collection type: $t")
            }
          case a: TypeRef.Any => genAnyFixture(a, format)
        }
      }

      gen(tpe)
    }

    // Stable, declaration-driven `AnyOpaque` fixture value. The meta must match the field's
    // declared variant exactly — encoder validates the kind byte. UEBA branch uses an empty
    // `Data()` payload; JSON branch uses `NSNull()` (Swift's idiomatic null in `Any` position;
    // mirrors the `?? NSNull()` decision in PR 9.3 for JSON-null content payloads — see the
    // `[String: Any]` deletion-on-nil gotcha noted in `SwJsonCodecGenerator.encodeAnyField`).
    // PR-07-D01 (Swift analog): branch must match codec direction so round-trip avoids
    // cross-format conversion (which requires `withFacade` ctx).
    private val FixtureAnyPayloadTypeId: String = "my.test.AnyFixturePayload"

    private def genAnyFixture(a: TypeRef.Any, format: FixtureFormat): TextTree[SwValue] = {
      val hasUnderlying = a.underlying.isDefined
      val kindByte      = AnyVariant.metaKindByte(a.variant, hasUnderlying) & 0xFF
      val kindHex       = "0x%02x".format(kindByte)
      val domainStr     = q""""${domain.id.toString}""""
      val versionStr    = q""""${domain.version.v.toString}""""
      val typeidStr     = q""""$FixtureAnyPayloadTypeId""""
      val nilTree       = q"nil"
      val tid           = if (hasUnderlying) nilTree else typeidStr
      val (domainExpr, versionExpr, typeidExpr) = a.variant match {
        case AnyVariant.Global  => (domainStr, versionStr, tid)
        case AnyVariant.ThisDom => (nilTree, versionStr, tid)
        case AnyVariant.Current => (nilTree, nilTree, tid)
      }
      // `AnyMeta.init` is `throws` (validates the kind/bit-mask invariants); fixture generation
      // happens at runtime so we wrap with `try!` — invariants are constructed deterministically
      // from the field's variant so the throw can never fire here.
      val meta = q"try! $baboonAnyMeta(kind: $kindHex, domain: $domainExpr, version: $versionExpr, typeid: $typeidExpr)"
      format match {
        case FixUeba => q"$baboonAnyOpaque.ueba(meta: $meta, bytes: Data())"
        case FixJson => q"$baboonAnyOpaque.json(meta: $meta, json: NSNull())"
      }
    }

    private def genScalar(tpe: TypeRef.Scalar, format: FixtureFormat): TextTree[SwValue] = {
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
          val enumType = translator.toSwTypeRefKeepForeigns(u, domain, evo)
          q"rnd.mkEnum($enumType.all)"
        case u: TypeId.User =>
          val fixturePkg       = translator.effectiveSwPkg(u.owner, domain, evo)
          val fixtureClassName = translator.fixtureClassName(u, domain, evo)
          val fixtureFileName  = s"${translator.toSnakeCase(translator.toSwTypeRefKeepForeigns(u, domain, evo).name)}_fixture"
          val fixtureType      = SwValue.SwType(fixturePkg, fixtureClassName, importAs = Some(fixtureFileName))
          // Propagate the codec branch into nested user-type fixtures so any-fields nested in
          // sub-DTOs/ADTs match the same codec direction. See PR-07-D01 (Swift analog).
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
