package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeId.Builtins
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait JvCodecFixtureTranslator {
  def translate(definition: DomainMember.User): Option[TextTree[JvValue]]
  def fixtureTpe(definition: DomainMember.User): Option[TextTree[JvValue]]
}

object JvCodecFixtureTranslator {
  // Branch selector for any-field fixture payloads. PR-07-D01 (Java analog): the auto-test
  // round-trips a fixture through a single codec; if the fixture's `AnyOpaque` branch matches
  // the codec direction, no facade is needed and equality holds. We emit two parallel methods
  // per DTO (`random` for the UEBA branch, `randomJson` for the JSON branch) so each test path
  // picks its native branch. Recursive calls into nested user-type fixtures must propagate the
  // same choice.
  private sealed trait FixtureFormat
  private case object FixUeba extends FixtureFormat
  private case object FixJson extends FixtureFormat

  final class Impl(
    target: JvTarget,
    translator: JvTypeTranslator,
    enquiries: BaboonEnquiries,
    domain: Domain,
    evo: BaboonEvolution,
  ) extends JvCodecFixtureTranslator {

    override def translate(
      definition: DomainMember.User
    ): Option[TextTree[JvValue]] = {
      definition.defn match {
        case _ if enquiries.hasForeignType(definition, domain, BaboonLang.Java) => None
        case _ if enquiries.isRecursiveTypedef(definition, domain)              => None
        case dto: Typedef.Dto                                                   => Some(doTranslateDto(dto))
        case adt: Typedef.Adt                                                   => Some(doTranslateAdt(adt))
        case _: Typedef.Contract                                                => None
        case _: Typedef.Enum                                                    => None
        case _: Typedef.Foreign                                                 => None
        case _: Typedef.Service                                                 => None
      }
    }

    override def fixtureTpe(definition: DomainMember.User): Option[TextTree[JvValue]] = {
      target.output.fixturesOutput.flatMap {
        _ =>
          definition.defn.id.owner match {
            case o: Owner.Adt =>
              for {
                did <- defnFixtureId(definition)
                oid <- defnFixtureId(domain.defs.meta.nodes(o.id))
              } yield {
                q"$oid.$did"
              }
            case _ => defnFixtureId(definition)
          }
      }
    }

    private def defnFixtureId(definition: DomainMember): Option[TextTree[JvValue]] = {
      definition match {
        case _: DomainMember.Builtin => None
        case u: DomainMember.User =>
          u.defn match {
            case _ if enquiries.hasForeignType(u, domain, BaboonLang.Java) => None
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

    private def fixtureTpeName(id: TypeId): TextTree[JvValue] = {
      q"${id.name.name.capitalize}_Fixture"
    }

    private def doTranslateDto(dto: Typedef.Dto): TextTree[JvValue] = {
      val fullType = translator.toJvTypeRefKeepForeigns(dto.id, domain, evo)

      def body(format: FixtureFormat): TextTree[JvValue] = {
        val generatedFields = dto.fields.map(f => genType(f.tpe, format))
        q"""return new $fullType(
           |  ${generatedFields.join(",\n").shift(2).trim}
           |);""".stripMargin
      }

      q"""public final class ${fixtureTpeName(dto.id)} {
         |  public static $fullType random($baboonRandom rnd) {
         |    ${body(FixUeba).shift(4).trim}
         |  }
         |
         |  public static $fullType randomJson($baboonRandom rnd) {
         |    ${body(FixJson).shift(4).trim}
         |  }
         |}
         |""".stripMargin
    }

    private def doTranslateAdt(adt: Typedef.Adt): TextTree[JvValue] = {
      val fullType = translator.toJvTypeRefKeepForeigns(adt.id, domain, evo)
      val members = adt.members.toList
        .flatMap(domain.defs.meta.nodes.get)
        .collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d }

      val sortedMembers          = members.sortBy(_.id.toString)
      val membersFixtures        = sortedMembers.map(doTranslateDto)
      val membersGenerators      = sortedMembers.map(dto => q"${dto.id.name.name}_Fixture::random")
      val membersGeneratorsJson  = sortedMembers.map(dto => q"${dto.id.name.name}_Fixture::randomJson")
      val membersDirectCalls     = sortedMembers.map(dto => q"${dto.id.name.name}_Fixture.random(rnd)")
      val membersDirectCallsJson = sortedMembers.map(dto => q"${dto.id.name.name}_Fixture.randomJson(rnd)")

      q"""public final class ${fixtureTpeName(adt.id)} {
         |  public static $fullType random($baboonRandom rnd) {
         |    return rnd.oneOf($jvList.of(
         |      ${membersGenerators.join(",\n").shift(6).trim}
         |    ));
         |  }
         |
         |  public static $fullType randomJson($baboonRandom rnd) {
         |    return rnd.oneOf($jvList.of(
         |      ${membersGeneratorsJson.join(",\n").shift(6).trim}
         |    ));
         |  }
         |
         |  public static $jvList<$fullType> randomAll($baboonRandom rnd) {
         |    return $jvList.of(
         |      ${membersDirectCalls.join(",\n").shift(6).trim}
         |    );
         |  }
         |
         |  public static $jvList<$fullType> randomAllJson($baboonRandom rnd) {
         |    return $jvList.of(
         |      ${membersDirectCallsJson.join(",\n").shift(6).trim}
         |    );
         |  }
         |
         |  ${membersFixtures.joinN().shift(2).trim}
         |}
         |""".stripMargin
    }

    private def genType(tpe: TypeRef, format: FixtureFormat): TextTree[JvValue] = {
      def gen(tpe: TypeRef): TextTree[JvValue] = {
        BaboonEnquiries.resolveBaboonRef(tpe, domain, BaboonLang.Java) match {
          case tpe: TypeRef.Scalar => genScalar(tpe, format)
          case TypeRef.Constructor(id, args) =>
            id match {
              case Builtins.lst => q"rnd.mkList(() -> ${gen(args.head)})"
              case Builtins.set => q"rnd.mkSet(() -> ${gen(args.head)})"
              case Builtins.map => q"rnd.mkMap(() -> ${gen(args(0))}, () -> ${gen(args(1))})"
              case Builtins.opt => q"rnd.mkOptional(() -> ${gen(args.head)})"
              case t            => throw new IllegalArgumentException(s"Unexpected collection type: $t")
            }
          case a: TypeRef.Any => genAnyFixture(a, format)
        }
      }

      gen(tpe)
    }

    // Stable, declaration-driven `AnyOpaque` fixture value. We don't randomise the meta because the
    // meta must match the field's declared variant exactly — encoder validates the kind byte. UEBA
    // branch uses empty bytes; JSON branch uses Jackson `NullNode.getInstance()`.
    //
    // The typeid string is only used by untyped-variant kinds (A/B/C) where the wire carries a
    // typeid; D1/D2/D3 have `hasUnderlying=true` and the meta typeid component is absent.
    private val FixtureAnyPayloadTypeId: String = "my.test.AnyFixturePayload"

    private def genAnyFixture(a: TypeRef.Any, format: FixtureFormat): TextTree[JvValue] = {
      val hasUnderlying = a.underlying.isDefined
      val kindHex       = "0x%02x".format(AnyVariant.metaKindByte(a.variant, hasUnderlying) & 0xFF)
      val domainStr     = q""""${domain.id.toString}""""
      val versionStr    = q""""${domain.version.v.toString}""""
      val typeidStr     = q""""$FixtureAnyPayloadTypeId""""

      val nullTree = q"null"
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

      val meta = q"new $baboonAnyMeta((byte)$kindHex, $domainExpr, $versionExpr, $typeidExpr)"
      // PR-07-D01 (Java analog): branch must match codec direction so round-trip avoids cross-
      // format conversion (which requires `withFacade` ctx). UEBA codec test uses
      // `AnyOpaqueUeba(meta, new byte[0])`; JSON codec test uses
      // `AnyOpaqueJson(meta, NullNode.getInstance())`. Both are wire-equivalent to themselves
      // under their native codec — equality holds.
      format match {
        case FixUeba => q"new $baboonAnyOpaqueUeba($meta, new byte[0])"
        case FixJson => q"new $baboonAnyOpaqueJson($meta, $nullNode.getInstance())"
      }
    }

    private def genScalar(tpe: TypeRef.Scalar, format: FixtureFormat): TextTree[JvValue] = {
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
        case TypeId.Builtins.f128 => q"rnd.nextF128()"

        case TypeId.Builtins.str   => q"rnd.nextString()"
        case TypeId.Builtins.bytes => q"rnd.nextByteString()"
        case TypeId.Builtins.uid   => q"rnd.nextUid()"

        case TypeId.Builtins.tsu => q"rnd.nextTsu()"
        case TypeId.Builtins.tso => q"rnd.nextTso()"

        case TypeId.Builtins.bit => q"rnd.nextBit()"

        case TypeId.User(_, _, name) if enquiries.isEnum(tpe, domain) => q"rnd.mkEnum(${name.name}.class, ${name.name}.values())"
        case u: TypeId.User                                           =>
          // Propagate the codec branch into nested user-type fixtures so any-fields nested in
          // sub-DTOs/ADTs match the same codec direction. See PR-07-D01 (Java analog).
          val method = format match {
            case FixUeba => q"random"
            case FixJson => q"randomJson"
          }
          q"${u.name.name}_Fixture.$method(rnd)"

        case t => throw new IllegalArgumentException(s"Unexpected scalar type: $t")
      }
    }
  }
}
