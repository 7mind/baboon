package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.rust.RsValue.RsType
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait RsDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[RsDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[RsDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[RsDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[RsDefnTranslator.Output]]
}

object RsDefnTranslator {
  final case class Output(
    path: String,
    tree: TextTree[RsValue],
    crate: RsValue.RsCrateId,
    product: CompilerProduct,
    doNotModify: Boolean = false,
    isModFile: Boolean   = false,
    isAdt: Boolean       = false,
  )

  class RsDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: RsTarget,
    domain: Domain,
    evo: BaboonEvolution,
    rsFiles: RsFileTools,
    trans: RsTypeTranslator,
    codecs: Set[RsCodecTranslator],
    codecTests: RsCodecTestsTranslator,
    codecsFixture: RsCodecFixtureTranslator,
    enquiries: BaboonEnquiries,
    wiringTranslator: RsServiceWiringTranslator,
    rsDomainTreeTools: RsDomainTreeTools,
    rsTypes: RsTypes,
  ) extends RsDefnTranslator[F] {

    private val baboonIdReprCursor: RsValue.RsType = rsTypes.baboonIdReprCursor

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr       = makeRepr(defn)
      val codecTrees = codecs.toList.flatMap(t => t.translate(defn, trans.asRsType(defn.id, domain, evo), trans.toRsTypeRefKeepForeigns(defn.id, domain, evo)).toList)
      val metaTrees  = makeMetaImpls(defn)
      val allDefs    = (repr +: codecTrees ++: metaTrees).joinNN()

      val mainOutput = Output(
        getOutputPath(defn),
        allDefs,
        trans.toRsCrate(domain.id, domain.version, evo),
        CompilerProduct.Definition,
        isAdt = defn.defn.isInstanceOf[Typedef.Adt],
      )

      val wiringOutput = wiringTranslator
        .translate(defn).map {
          wiringTree =>
            Output(
              getOutputPath(defn, suffix = Some("_wiring")),
              wiringTree,
              trans.toRsCrate(domain.id, domain.version, evo),
              CompilerProduct.Definition,
            )
        }.toList

      val clientOutput = wiringTranslator
        .translateClient(defn).map {
          clientTree =>
            Output(
              getOutputPath(defn, suffix = Some("_client")),
              clientTree,
              trans.toRsCrate(domain.id, domain.version, evo),
              CompilerProduct.Definition,
            )
        }.toList

      F.pure(mainOutput :: wiringOutput ::: clientOutput)
    }

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateFixtures(defn)
      }
    }

    private def doTranslateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val fixtureTreeOpt = codecsFixture.translate(defn)
      F.pure(fixtureTreeOpt.map {
        fixtureTree =>
          Output(
            getOutputPath(defn, suffix = Some("_fixture")),
            fixtureTree,
            trans.toRsCrate(domain.id, domain.version, evo),
            CompilerProduct.Fixture,
          )
      }.toList)
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTest(defn)
      }
    }

    private def doTranslateTest(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val csTypeRef   = trans.asRsType(defn.id, domain, evo)
      val srcRef      = trans.toRsTypeRefKeepForeigns(defn.id, domain, evo)
      val testTreeOpt = codecTests.translate(defn, csTypeRef, srcRef)
      F.pure(testTreeOpt.map {
        testTree =>
          Output(
            getOutputPath(defn, suffix = Some("_tests")),
            testTree,
            trans.toRsCrate(domain.id, domain.version, evo),
            CompilerProduct.Test,
          )
      }.toList)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map {
        tree =>
          val fbase = rsFiles.basename(domain, evo)
          Output(
            s"$fbase/baboon_service_rt.rs",
            tree,
            trans.toRsCrate(domain.id, domain.version, evo),
            CompilerProduct.Definition,
          )
      }.toList
      F.pure(result)
    }

    private def makeMetaImpls(defn: DomainMember.User): List[TextTree[RsValue]] = {
      val isLatestVersion = domain.version == evo.latest
      val name            = trans.asRsType(defn.id, domain, evo)

      defn.defn match {
        case _: Typedef.Dto =>
          List(rsDomainTreeTools.makeBaboonGeneratedImpl(defn, name, isLatestVersion))

        case _: Typedef.Enum =>
          List(rsDomainTreeTools.makeBaboonGeneratedImpl(defn, name, isLatestVersion))

        case adt: Typedef.Adt =>
          val parentImpl = rsDomainTreeTools.makeBaboonGeneratedImpl(defn, name, isLatestVersion)
          val branchImpls = adt.dataMembers(domain).toList.flatMap {
            mid =>
              domain.defs.meta.nodes.get(mid) match {
                case Some(mdefn: DomainMember.User) =>
                  val branchName = trans.asRsType(mdefn.id, domain, evo)
                  List(rsDomainTreeTools.makeBaboonGeneratedImpl(mdefn, branchName, isLatestVersion))
                case _ => Nil
              }
          }
          parentImpl :: branchImpls

        case _: Typedef.Contract => Nil
        case _: Typedef.Service  => Nil
        case _: Typedef.Foreign  => Nil
      }
    }

    private def makeRepr(defn: DomainMember.User): TextTree[RsValue] = {
      val name = trans.asRsType(defn.id, domain, evo)

      defn.defn match {
        case dto: Typedef.Dto =>
          makeDtoRepr(dto, name)

        case e: Typedef.Enum =>
          makeEnumRepr(e, name)

        case adt: Typedef.Adt =>
          makeAdtRepr(adt, name)

        case _: Typedef.Contract =>
          makeContractRepr(defn, name)

        case _: Typedef.Service =>
          makeServiceRepr(defn, name)

        case _: Typedef.Foreign =>
          q""
      }
    }

    // ----- Identifier toString + parse_repr emission (PR-57c) -----
    // Spec contract: docs/spec/identifier-repr.md. Mirrors ScDefnTranslator and
    // JvDefnTranslator/KtDefnTranslator sections but uses Rust idioms:
    //   - `impl std::fmt::Display` (not ToString — Display is the canonical idiom)
    //   - free function `pub fn parse_repr(s: &str) -> Result<Self, String>`
    //     (NOT `impl FromStr` — Q-FU-4 keeps `.parse()` undiscoverable)
    //   - `Result<T, String>` (not `Either`)
    private sealed trait IdentifierFieldKind
    private object IdentifierFieldKind {
      case object Bit              extends IdentifierFieldKind
      case object SignedInt        extends IdentifierFieldKind /* i08/i16/i32/i64 */
      case object UnsignedSmallInt extends IdentifierFieldKind /* u08/u16/u32 */
      case object UnsignedLong     extends IdentifierFieldKind /* u64 */
      case object Str              extends IdentifierFieldKind
      case object Uid              extends IdentifierFieldKind
      case object Tsu              extends IdentifierFieldKind
      case object Tso              extends IdentifierFieldKind
      case object Bytes            extends IdentifierFieldKind
      final case class NestedId(id: TypeId.User) extends IdentifierFieldKind
    }

    private def identifierFieldKind(tpe: TypeRef): IdentifierFieldKind = {
      tpe match {
        case TypeRef.Scalar(b: TypeId.BuiltinScalar) =>
          import TypeId.Builtins.*
          b match {
            case `bit`                         => IdentifierFieldKind.Bit
            case `i08` | `i16` | `i32` | `i64` => IdentifierFieldKind.SignedInt
            case `u08` | `u16` | `u32`         => IdentifierFieldKind.UnsignedSmallInt
            case `u64`                         => IdentifierFieldKind.UnsignedLong
            case `str`                         => IdentifierFieldKind.Str
            case `uid`                         => IdentifierFieldKind.Uid
            case `tsu`                         => IdentifierFieldKind.Tsu
            case `tso`                         => IdentifierFieldKind.Tso
            case `bytes`                       => IdentifierFieldKind.Bytes
            case other =>
              throw new IllegalStateException(s"Identifier field has unsupported scalar $other; validator should have rejected this.")
          }
        case TypeRef.Scalar(uid: TypeId.User) =>
          IdentifierFieldKind.NestedId(uid)
        case other =>
          throw new IllegalStateException(s"Identifier field has unsupported TypeRef $other; validator should have rejected this.")
      }
    }

    private def signedTypeName(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.i08) => "i08"
        case TypeRef.Scalar(TypeId.Builtins.i16) => "i16"
        case TypeRef.Scalar(TypeId.Builtins.i32) => "i32"
        case TypeRef.Scalar(TypeId.Builtins.i64) => "i64"
        case other                               => throw new IllegalStateException(s"signedTypeName on non-signed-int: $other")
      }
    }

    private def signedRangeCheck(tpe: TypeRef, varName: String): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.i08) => s"$varName >= -128i64 && $varName <= 127i64"
        case TypeRef.Scalar(TypeId.Builtins.i16) => s"$varName >= -32768i64 && $varName <= 32767i64"
        case TypeRef.Scalar(TypeId.Builtins.i32) => s"$varName >= -2147483648i64 && $varName <= 2147483647i64"
        case TypeRef.Scalar(TypeId.Builtins.i64) => "true"
        case other                               => throw new IllegalStateException(s"signedRangeCheck on non-signed-int: $other")
      }
    }

    private def signedNarrowFn(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.i08) => "as i8"
        case TypeRef.Scalar(TypeId.Builtins.i16) => "as i16"
        case TypeRef.Scalar(TypeId.Builtins.i32) => "as i32"
        case TypeRef.Scalar(TypeId.Builtins.i64) => "as i64"
        case other                               => throw new IllegalStateException(s"signedNarrowFn on non-signed-int: $other")
      }
    }

    private def unsignedSmallTypeName(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => "u08"
        case TypeRef.Scalar(TypeId.Builtins.u16) => "u16"
        case TypeRef.Scalar(TypeId.Builtins.u32) => "u32"
        case other                               => throw new IllegalStateException(s"unsignedSmallTypeName on non-u08/u16/u32: $other")
      }
    }

    private def unsignedSmallRangeCheck(tpe: TypeRef, varName: String): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => s"$varName <= 255u64"
        case TypeRef.Scalar(TypeId.Builtins.u16) => s"$varName <= 65535u64"
        case TypeRef.Scalar(TypeId.Builtins.u32) => s"$varName <= 4294967295u64"
        case other                               => throw new IllegalStateException(s"unsignedSmallRangeCheck on non-u08/u16/u32: $other")
      }
    }

    private def unsignedSmallNarrowFn(tpe: TypeRef): String = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.u08) => "as u8"
        case TypeRef.Scalar(TypeId.Builtins.u16) => "as u16"
        case TypeRef.Scalar(TypeId.Builtins.u32) => "as u32"
        case other                               => throw new IllegalStateException(s"unsignedSmallNarrowFn on non-u08/u16/u32: $other")
      }
    }

    private def renderFieldValueExpr(rsFieldName: String, kind: IdentifierFieldKind): TextTree[RsValue] = {
      kind match {
        case IdentifierFieldKind.Bit              => q"crate::baboon_identifier_repr::bit_to_string(self.$rsFieldName)"
        // Rust's primitive Display for signed integers produces canonical signed
        // decimal; for u08/u16/u32 it produces unsigned decimal natively (Rust
        // u-types are actually unsigned).
        case IdentifierFieldKind.SignedInt        => q"self.$rsFieldName.to_string()"
        case IdentifierFieldKind.UnsignedSmallInt => q"self.$rsFieldName.to_string()"
        case IdentifierFieldKind.UnsignedLong     => q"crate::baboon_identifier_repr::u64_to_string(self.$rsFieldName)"
        case IdentifierFieldKind.Str              => q"crate::baboon_identifier_repr::escape_str(&self.$rsFieldName)"
        // uuid::Uuid Display is the lowercase 36-char hyphenated form per RFC 4122.
        case IdentifierFieldKind.Uid              => q"self.$rsFieldName.to_string()"
        case IdentifierFieldKind.Tsu              => q"crate::baboon_identifier_repr::tsu_to_string(&self.$rsFieldName)"
        case IdentifierFieldKind.Tso              => q"crate::baboon_identifier_repr::tso_to_string(&self.$rsFieldName)"
        case IdentifierFieldKind.Bytes            => q"crate::baboon_identifier_repr::bytes_to_hex(&self.$rsFieldName)"
        case IdentifierFieldKind.NestedId(_)      => q"""format!("{{{}}}", self.$rsFieldName)"""
      }
    }

    private def renderIdentifierDisplay(dto: Typedef.Dto, name: RsType): TextTree[RsValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString
      val header     = s"$simpleName:$versionStr#"

      // Build a single format-string-free expression that concatenates the
      // canonical pieces. Rust's `+` for &str needs left-side String, so we
      // build via String::push_str — equivalent but explicit.
      val pushes = dto.fields.zipWithIndex.flatMap {
        case (f, idx) =>
          val srcFieldName = f.name.name
          val rsFieldName  = toSnakeCase(srcFieldName)
          val kind         = identifierFieldKind(f.tpe)
          val valExpr      = renderFieldValueExpr(rsFieldName, kind)
          val sep          = if (idx == 0) q"" else q"""out.push(':');"""
          List(
            sep,
            q"""out.push_str("$srcFieldName:");""",
            q"""out.push_str(&($valExpr));""",
          )
      }

      val pushBlock = if (pushes.nonEmpty) pushes.joinN() else q""

      q"""impl std::fmt::Display for ${name.asName} {
         |    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
         |        let mut out = String::new();
         |        out.push_str("$header");
         |        ${pushBlock.shift(8).trim}
         |        f.write_str(&out)
         |    }
         |}""".stripMargin
    }

    private def renderIdentifierParseRepr(dto: Typedef.Dto, name: RsType): TextTree[RsValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString

      val fieldDecoders: List[TextTree[RsValue]] = dto.fields.zipWithIndex.map {
        case (f, idx) =>
          val srcFieldName = f.name.name
          val valVar       = s"${toSnakeCase(srcFieldName)}_v"
          val rawVar       = s"${toSnakeCase(srcFieldName)}_raw"
          val isLast       = idx == dto.fields.length - 1
          val kind         = identifierFieldKind(f.tpe)
          val tpe          = trans.asRsRef(f.tpe, domain, evo)

          val parseHead =
            q"""crate::baboon_identifier_repr::parse_field_name(cursor, "$srcFieldName")?;"""

          val parseValue: TextTree[RsValue] = kind match {
            case IdentifierFieldKind.Bit =>
              q"""let $rawVar = cursor.read_until_structural();
                 |let $valVar: $tpe = crate::baboon_identifier_repr::parse_bit(&$rawVar)?;""".stripMargin
            case IdentifierFieldKind.SignedInt =>
              val typeName    = signedTypeName(f.tpe)
              val rangeCheck  = signedRangeCheck(f.tpe, "v")
              val narrow      = signedNarrowFn(f.tpe)
              // i64 covers the full Long range — rangeCheck returns "true" — so
              // no range-check block is needed (mirrors prior backends D01 fix).
              val rangeBlock =
                if (rangeCheck == "true") q""
                else
                  q"""if !($rangeCheck) {
                     |    return Err(format!("$typeName out of range for field $srcFieldName: {}", $rawVar));
                     |}""".stripMargin
              q"""let $rawVar = cursor.read_until_structural();
                 |let v: i64 = $rawVar.parse::<i64>()
                 |    .map_err(|_| format!("could not parse signed integer for field $srcFieldName: {}", $rawVar))?;
                 |${rangeBlock}
                 |let $valVar: $tpe = v $narrow;""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              val typeName    = unsignedSmallTypeName(f.tpe)
              val rangeCheck  = unsignedSmallRangeCheck(f.tpe, "v")
              val narrow      = unsignedSmallNarrowFn(f.tpe)
              q"""let $rawVar = cursor.read_until_structural();
                 |if !$rawVar.is_empty() && (
                 |    $rawVar.starts_with('+') || $rawVar.starts_with('-')
                 |) {
                 |    return Err(format!("unsigned value has leading sign for field $srcFieldName: {}", $rawVar));
                 |}
                 |let v: u64 = $rawVar.parse::<u64>()
                 |    .map_err(|_| format!("could not parse unsigned integer for field $srcFieldName: {}", $rawVar))?;
                 |if !($rangeCheck) {
                 |    return Err(format!("$typeName out of range for field $srcFieldName: {}", $rawVar));
                 |}
                 |let $valVar: $tpe = v $narrow;""".stripMargin
            case IdentifierFieldKind.UnsignedLong =>
              q"""let $rawVar = cursor.read_until_structural();
                 |if !$rawVar.is_empty() && (
                 |    $rawVar.starts_with('+') || $rawVar.starts_with('-')
                 |) {
                 |    return Err(format!("unsigned value has leading sign for field $srcFieldName: {}", $rawVar));
                 |}
                 |let $valVar: $tpe = $rawVar.parse::<u64>()
                 |    .map_err(|_| format!("could not parse u64 for field $srcFieldName: {}", $rawVar))?;""".stripMargin
            case IdentifierFieldKind.Str =>
              q"""let $valVar: $tpe = cursor.read_str_field()?;""".stripMargin
            case IdentifierFieldKind.Uid =>
              q"""let $rawVar = cursor.read_until_structural();
                 |if !crate::baboon_identifier_repr::is_canonical_uid(&$rawVar) {
                 |    return Err(format!("uid not in canonical lowercase form for field $srcFieldName: {}", $rawVar));
                 |}
                 |let $valVar: $tpe = $rawVar.parse::<uuid::Uuid>()
                 |    .map_err(|_| format!("could not parse uid for field $srcFieldName: {}", $rawVar))?;""".stripMargin
            case IdentifierFieldKind.Tsu =>
              q"""let $rawVar = cursor.read_fixed(24)?;
                 |let $valVar: $tpe = crate::baboon_identifier_repr::parse_tsu_repr(&$rawVar)?;""".stripMargin
            case IdentifierFieldKind.Tso =>
              q"""let $rawVar = cursor.read_fixed(29)?;
                 |let $valVar: $tpe = crate::baboon_identifier_repr::parse_tso_repr(&$rawVar)?;""".stripMargin
            case IdentifierFieldKind.Bytes =>
              q"""let $rawVar = cursor.read_until_structural();
                 |let $valVar: $tpe = crate::baboon_identifier_repr::parse_bytes_hex(&$rawVar)?;""".stripMargin
            case IdentifierFieldKind.NestedId(uid) =>
              val nestedTpe = trans.toRsTypeRefKeepForeigns(uid, domain, evo)
              // The nested type's parse_repr_cursor lives in
              // `<typename_snake>_repr_codec` inside its own module — see
              // renderIdentifierParseRepr for naming rationale.
              val nestedCodecModName = s"${toSnakeCase(nestedTpe.name)}_repr_codec"
              val nestedFqPath = (nestedTpe.crate.parts.toSeq :+ nestedCodecModName :+ "parse_repr_cursor").mkString("::")
              q"""cursor.expect('{')?;
                 |let $valVar: $nestedTpe = $nestedFqPath(cursor)?;
                 |cursor.expect('}')?;""".stripMargin
          }

          val sep = if (isLast) q"" else q"""cursor.expect(':')?;"""

          q"""$parseHead
             |$parseValue
             |$sep""".stripMargin.trim
      }

      val ctorArgs = dto.fields.map {
        f =>
          val rsFieldName = toSnakeCase(f.name.name)
          q"$rsFieldName: ${rsFieldName}_v,"
      }

      val ctor =
        if (ctorArgs.nonEmpty)
          q"""${name.asName} {
             |    ${ctorArgs.joinN().shift(4).trim}
             |}""".stripMargin
        else q"${name.asName} {}"

      val body = (fieldDecoders :+ q"Ok($ctor)").joinNN()

      // Codec helpers live in a per-type-named inner module so the parent
      // package module's `pub use *` glob re-export does NOT collide across
      // sibling identifier types (each `pub mod foo_repr_codec` is unique).
      // Callers reach these via
      //   `crate::<pkg>::<typename_snake>::<typename_snake>_repr_codec::parse_repr(s)`.
      // Two entry points:
      //   - parse_repr(s: &str) — owns the cursor, requires fully consumed input.
      //   - parse_repr_cursor(cursor: &mut Cursor) — partial parse, used by
      //     nested-id dispatch from the parent codec.
      val codecModName = s"${toSnakeCase(simpleName)}_repr_codec"
      // `use super::*` pulls in the parent module's bindings: the `<TypeName>`
      // struct itself, plus any external `use uuid::Uuid` / chrono / nested
      // type imports rendered at the top of this file. Inner Rust modules
      // do NOT inherit parent-scope imports.
      q"""pub mod $codecModName {
         |    use super::*;
         |
         |    /// Parse the canonical identifier repr per docs/spec/identifier-repr.md.
         |    /// Schema-directed parser: walks declared field order and dispatches per
         |    /// field type. Returns Err on any malformed input.
         |    pub fn parse_repr(s: &str) -> Result<${name.asName}, String> {
         |        let mut cursor = crate::baboon_identifier_repr::Cursor::new(s);
         |        let inner = parse_repr_cursor(&mut cursor)?;
         |        if !cursor.at_end() {
         |            return Err(format!("unexpected trailing input at {}", cursor.position()));
         |        }
         |        Ok(inner)
         |    }
         |
         |    pub fn parse_repr_cursor(cursor: &mut crate::baboon_identifier_repr::Cursor<'_>) -> Result<${name.asName}, String> {
         |        crate::baboon_identifier_repr::parse_header(cursor, "$simpleName", "$versionStr")?;
         |        ${body.shift(8).trim}
         |    }
         |}""".stripMargin
    }

    private def makeDtoRepr(dto: Typedef.Dto, name: RsType): TextTree[RsValue] = {
      val fields = dto.fields.map {
        f =>
          val rawT       = trans.asRsRef(f.tpe, domain, evo)
          val t          = if (needsBox(f.tpe)) q"Box<$rawT>" else rawT
          val serdeAttrs = fieldSerdeAttributes(f)
          val attrLine   = if (serdeAttrs.nonEmpty) serdeAttrs.joinN() else q""
          q"""$attrLine
             |pub ${toSnakeCase(f.name.name)}: $t,""".stripMargin.trim
      }
      val fieldsList = if (fields.nonEmpty) fields.joinN() else q""

      val derives  = dtoDerives(dto)
      val ordImpls = dtoOrdImpls(dto, name)

      val customSerialize = if (isWrappedAdtBranch(dto)) {
        val branchName = dto.id.name.name
        val hasFields  = dto.fields.nonEmpty

        val innerFields = dto.fields.map {
          f =>
            val rawT       = trans.asRsRef(f.tpe, domain, evo)
            val t          = if (needsBox(f.tpe)) q"Box<$rawT>" else rawT
            val serdeAttrs = fieldSerdeAttributes(f)
            val attrLine   = if (serdeAttrs.nonEmpty) serdeAttrs.joinN() else q""
            q"""$attrLine
               |${toSnakeCase(f.name.name)}: &'a $t,""".stripMargin.trim
        }
        val innerFieldsList = if (innerFields.nonEmpty) innerFields.joinN() else q""

        val fieldAssignments = dto.fields.map {
          f =>
            val fld = toSnakeCase(f.name.name)
            q"$fld: &self.$fld,"
        }
        val fieldAssignmentsList = if (fieldAssignments.nonEmpty) fieldAssignments.joinN() else q""

        val fieldsStructDecl = if (hasFields) q"struct Fields<'a>" else q"struct Fields"

        q"""
           |
           |impl serde::Serialize for ${name.asName} {
           |    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
           |        use serde::ser::SerializeMap;
           |        #[derive(serde::Serialize)]
           |        $fieldsStructDecl {
           |            ${innerFieldsList.shift(12).trim}
           |        }
           |        let fields = Fields {
           |            ${fieldAssignmentsList.shift(12).trim}
           |        };
           |        let mut map = serializer.serialize_map(Some(1))?;
           |        map.serialize_entry("$branchName", &fields)?;
           |        map.end()
           |    }
           |}""".stripMargin
      } else {
        q""
      }

      val structDef = if (dto.fields.isEmpty) {
        q"""$derives
           |pub struct ${name.asName} {}""".stripMargin
      } else {
        q"""$derives
           |pub struct ${name.asName} {
           |    ${fieldsList.shift(4).trim}
           |}""".stripMargin
      }

      // Identifier toString + parse_repr emission (PR-57c / spec:
      // docs/spec/identifier-repr.md). Emitted only when `dto.isIdentifier`.
      // The `parse_repr` free function lives at module scope (sibling to the
      // struct, addressable as `crate::<pkg>::<typename_snake>::parse_repr`)
      // — NOT as `impl FromStr` (Q-FU-4: keeps `.parse()` undiscoverable).
      val identifierImpls: TextTree[RsValue] =
        if (dto.isIdentifier) {
          val display   = renderIdentifierDisplay(dto, name)
          val parseRepr = renderIdentifierParseRepr(dto, name)
          q"""$display
             |
             |$parseRepr""".stripMargin
        } else q""

      // PR-61 (M19.3): per-key-type serde adapter module so this DTO can appear
      // as a JSON map key. See userMapKeyAdapterPath / fieldSerdeAttributes.
      val mapKeyAdapter: TextTree[RsValue] =
        if (isUserMapKeyEligibleDto(dto)) renderUserMapKeyAdapter(dto, name)
        else q""

      q"""$structDef
         |
         |$ordImpls$customSerialize
         |
         |$identifierImpls
         |
         |$mapKeyAdapter""".stripMargin
    }

    /** PR-61 (M19.3) — emit a per-key-type serde adapter module as a sibling of
      * the struct, generic over the map value type V. Used by fields of shape
      * `BTreeMap<Self, V>` via `#[serde(with = "<this_module>")]`.
      *
      * String form on the wire:
      *   - id types         → `format!("{}", k)` (Display from PR-57c) ↔ parse_repr
      *   - single-primitive → inner primitive's Display ↔ inner primitive parse
      *
      * Approach (b) per PR-61 brief: the wrapper's value-position Serialize is
      * NOT overridden — wrappers/ids continue to JSON-serialize as objects when
      * they appear in value position. Only the map-key path is rerouted.
      */
    private def renderUserMapKeyAdapter(dto: Typedef.Dto, name: RsType): TextTree[RsValue] = {
      val modName = s"${toSnakeCase(name.name)}_as_map_key"

      // Encode: per-key Self → String.
      val encodeKey: TextTree[RsValue] =
        if (dto.isIdentifier) q"format!(\"{}\", k)"
        else {
          // Single-primitive-field wrapper (possibly nested): peel to the leaf
          // scalar and use its Display or dedicated serializer.
          // peelWrapperChain is guaranteed to return Some here because
          // isUserMapKeyEligibleDto already approved this DTO.
          val (leafTpe, leafPath, _) = peelWrapperChain(dto, name).getOrElse(
            throw new IllegalStateException(s"renderUserMapKeyAdapter: peelWrapperChain returned None for ${name.name}")
          )
          leafTpe match {
            case TypeRef.Scalar(TypeId.Builtins.bytes) =>
              // Vec<u8> doesn't implement Display; reuse the runtime hex helper.
              q"crate::baboon_identifier_repr::bytes_to_hex(&k.$leafPath)"
            case TypeRef.Scalar(TypeId.Builtins.tsu) =>
              q"crate::baboon_runtime::time_formats::format_tsu(&k.$leafPath)"
            case TypeRef.Scalar(TypeId.Builtins.tso) =>
              q"crate::baboon_runtime::time_formats::format_tso(&k.$leafPath)"
            case _ =>
              q"format!(\"{}\", k.$leafPath)"
          }
        }

      // Decode: per-key String → Self (with Result<_, String> error type).
      val decodeKey: TextTree[RsValue] =
        if (dto.isIdentifier) {
          val codecMod = s"${toSnakeCase(name.name)}_repr_codec"
          q"$codecMod::parse_repr(&s)"
        } else {
          // Peel to the leaf scalar, parse it, then rewrap through every layer
          // from innermost to outermost via chained .map calls.
          val (leafTpe, _, wrappers) = peelWrapperChain(dto, name).getOrElse(
            throw new IllegalStateException(s"renderUserMapKeyAdapter: peelWrapperChain returned None for ${name.name}")
          )
          val leafT = trans.asRsRef(leafTpe, domain, evo)
          // Parse via str::parse; for primitive scalars Rust's FromStr matches
          // Display's wire form. Bytes/timestamps require dedicated parsers.
          val parseExpr: TextTree[RsValue] = leafTpe match {
            case TypeRef.Scalar(TypeId.Builtins.bytes) =>
              q"crate::baboon_identifier_repr::parse_bytes_hex(&s)"
            case TypeRef.Scalar(TypeId.Builtins.tsu) =>
              q"crate::baboon_runtime::time_formats::parse_tsu(&s)"
            case TypeRef.Scalar(TypeId.Builtins.tso) =>
              q"crate::baboon_runtime::time_formats::parse_tso(&s)"
            case _ =>
              q"s.parse::<$leafT>().map_err(|e| format!(\"{}\", e))"
          }
          // wrappers is ordered innermost-first; chain .map(|v| Wrapper { field: v })
          wrappers.foldLeft(parseExpr) {
            case (acc, (fieldSnake, wrapRs)) =>
              q"$acc.map(|v| ${wrapRs.asName} { $fieldSnake: v })"
          }
        }

      q"""pub mod $modName {
         |    use super::*;
         |    use serde::Deserialize as _BaboonDeserialize;
         |    use std::collections::BTreeMap;
         |
         |    pub fn serialize<S, V>(map: &BTreeMap<${name.asName}, V>, serializer: S) -> Result<S::Ok, S::Error>
         |    where
         |        S: serde::Serializer,
         |        V: serde::Serialize,
         |    {
         |        use serde::ser::SerializeMap;
         |        let mut m = serializer.serialize_map(Some(map.len()))?;
         |        for (k, v) in map.iter() {
         |            let key_str: String = $encodeKey;
         |            m.serialize_entry(&key_str, v)?;
         |        }
         |        m.end()
         |    }
         |
         |    pub fn deserialize<'de, __De, V>(deserializer: __De) -> Result<BTreeMap<${name.asName}, V>, __De::Error>
         |    where
         |        __De: serde::Deserializer<'de>,
         |        V: serde::Deserialize<'de>,
         |    {
         |        let raw: BTreeMap<String, V> = BTreeMap::<String, V>::deserialize(deserializer)?;
         |        let mut out: BTreeMap<${name.asName}, V> = BTreeMap::new();
         |        for (s, v) in raw.into_iter() {
         |            let key = $decodeKey
         |                .map_err(serde::de::Error::custom)?;
         |            out.insert(key, v);
         |        }
         |        Ok(out)
         |    }
         |}""".stripMargin
    }

    private def isWrappedAdtBranch(dto: Typedef.Dto): Boolean = {
      target.language.wrappedAdtBranchCodecs && (dto.id.owner match {
        case Owner.Adt(_) => true
        case _            => false
      })
    }

    private def dtoDerives(dto: Typedef.Dto): TextTree[RsValue] = {
      val hasNonOrd      = dto.fields.exists(f => hasDirectFloat(f.tpe))
      val hasUnorderable = dto.fields.exists(f => hasAnyField(f.tpe))
      val wrappedBranch  = isWrappedAdtBranch(dto)
      // Three independent axes:
      // - hasNonOrd (floats): PartialEq/Eq/Ord/PartialOrd are emitted manually via total_cmp.
      // - hasUnorderable (any): PartialEq is derived (AnyOpaque has PartialEq); no Eq/Ord because
      //   serde_json::Value has no total ordering and JSON-source bytes can be byte-different but
      //   semantically equal — Eq is misleading.
      // - wrappedBranch: encoded as an inline ADT branch — no Serialize derive (handled by ADT).
      val serdeDerives =
        if (wrappedBranch) "serde::Deserialize"
        else "serde::Serialize, serde::Deserialize"

      val cmpDerives =
        if (hasNonOrd) ""
        else if (hasUnorderable) "PartialEq, "
        else "PartialEq, Eq, PartialOrd, Ord, "

      q"#[derive(Clone, Debug, ${cmpDerives}${serdeDerives})]"
    }

    private def dtoOrdImpls(dto: Typedef.Dto, name: RsType): TextTree[RsValue] = {
      val hasNonOrd      = dto.fields.exists(f => hasDirectFloat(f.tpe))
      val hasUnorderable = dto.fields.exists(f => hasAnyField(f.tpe))
      if (hasUnorderable) {
        // `any` fields carry payloads (UEBA bytes / serde_json::Value) without total ordering.
        // Skip Ord/Eq entirely; users get only `PartialEq` (derived) for value comparison.
        // The manual `total_cmp` path used for floats is not applicable here.
        q""
      } else if (!hasNonOrd) {
        q""
      } else {
        val fieldComparisons = dto.fields.map {
          f =>
            val fld = toSnakeCase(f.name.name)
            if (hasDirectFloat(f.tpe)) {
              q"""match self.$fld.total_cmp(&other.$fld) {
                 |    std::cmp::Ordering::Equal => {},
                 |    ord => return ord,
                 |}""".stripMargin
            } else {
              q"""match self.$fld.cmp(&other.$fld) {
                 |    std::cmp::Ordering::Equal => {},
                 |    ord => return ord,
                 |}""".stripMargin
            }
        }
        q"""impl PartialEq for ${name.asName} {
           |    fn eq(&self, other: &Self) -> bool {
           |        self.cmp(other) == std::cmp::Ordering::Equal
           |    }
           |}
           |
           |impl Eq for ${name.asName} {}
           |
           |impl PartialOrd for ${name.asName} {
           |    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
           |        Some(self.cmp(other))
           |    }
           |}
           |
           |impl Ord for ${name.asName} {
           |    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
           |        ${fieldComparisons.joinN().shift(8).trim}
           |        std::cmp::Ordering::Equal
           |    }
           |}""".stripMargin
      }
    }

    private def hasDirectFloat(tpe: TypeRef): Boolean = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.f32) => true
        case TypeRef.Scalar(TypeId.Builtins.f64) => true
        case TypeRef.Constructor(_, args)        => args.exists(hasDirectFloat)
        case _                                   => false
      }
    }

    private def hasAnyField(tpe: TypeRef): Boolean = {
      tpe match {
        case _: TypeRef.Any         => true
        case _: TypeRef.Scalar      => false
        case c: TypeRef.Constructor => c.args.exists(hasAnyField)
      }
    }

    private def needsBox(tpe: TypeRef): Boolean = {
      tpe match {
        case TypeRef.Scalar(u: TypeId.User) =>
          domain.defs.meta.nodes.get(u).exists {
            case m: DomainMember.User => enquiries.isRecursiveTypedef(m, domain)
            case _                    => false
          }
        case TypeRef.Constructor(_, args) => args.exists(needsBox)
        case _                            => false
      }
    }

    private def fieldSerdeAttributes(f: Field): List[TextTree[RsValue]] = {
      val attrs = scala.collection.mutable.ListBuffer.empty[TextTree[RsValue]]
      // Rename field to original name if we snake_cased it
      val originalName = f.name.name
      val rustName     = toSnakeCase(originalName)
      if (rustName != originalName) {
        attrs += q"""#[serde(rename = "$originalName")]"""
      }

      // Special serde for bytes (hex encoding)
      trans.needsHexSerde(f.tpe) match {
        case Some(RsTypeTranslator.HexSerdeKind.Direct) =>
          attrs += q"""#[serde(with = "crate::baboon_runtime::hex_bytes")]"""
        case Some(RsTypeTranslator.HexSerdeKind.Optional) =>
          attrs += q"""#[serde(with = "crate::baboon_runtime::opt_hex_bytes")]"""
        case None =>
      }
      // Special serde for Decimal (as number)
      if (trans.needsDecimalSerde(f.tpe)) {
        attrs += q"""#[serde(with = "crate::baboon_runtime::decimal_as_number")]"""
      }
      // Special serde for timestamps
      f.tpe match {
        case TypeRef.Scalar(TypeId.Builtins.tsu) =>
          attrs += q"""#[serde(with = "crate::baboon_runtime::tsu_serde")]"""
        case TypeRef.Scalar(TypeId.Builtins.tso) =>
          attrs += q"""#[serde(with = "crate::baboon_runtime::tso_serde")]"""
        case _ =>
      }
      // Lenient deserialization for fields containing i64/u64 (accepts both numbers and strings)
      if (trans.needsLenientSerde(f.tpe)) {
        attrs += q"""#[serde(deserialize_with = "crate::baboon_runtime::lenient_numeric::deserialize")]"""
      }
      // PR-61 (M19.3 / PR-60-D06): user-typed map keys.
      // serde_json's default Serialize for `BTreeMap<K, V>` rejects non-string-shaped K
      // with `key must be a string`. For `map[K, V]` where K is a user `id` (M18) or
      // single-primitive-field `data` wrapper (M19), redirect through a per-K adapter
      // module that converts K↔String via the existing Display / parse_repr machinery
      // (id types) or by peeling to the inner primitive (single-field wrappers). The
      // adapter module is emitted next to K's struct (see `userMapKeyAdapter` below).
      userMapKeyAdapterPath(f.tpe).foreach { path =>
        attrs += q"""#[serde(with = "$path")]"""
      }

      attrs.toList
    }

    /** PR-61 (M19.3): if `f` is a top-level `map[K, V]` whose K is an eligible user
      * key, return the FQ Rust path of the adapter module sibling to K's struct.
      * Eligibility mirrors PR-59's validator (`isEligibleKey`): id types are eligible
      * unconditionally; non-id `data` requires single-primitive-field shape with no
      * contracts. We assume the validator has already rejected ineligible cases —
      * here we only decide whether to attach the `#[serde(with = ...)]` attribute.
      *
      * Limited to the top-level `map[K, V]` shape (no `Option<BTreeMap<K, V>>` or
      * nested `BTreeMap<K1, BTreeMap<K2, V>>` — adapter modules are field-scoped
      * and serde's `with` attribute is applied to the field's full type).
      */
    private def userMapKeyAdapterPath(tpe: TypeRef): Option[String] = tpe match {
      case TypeRef.Constructor(TypeId.Builtins.map, args) =>
        args.head match {
          case TypeRef.Scalar(uid: TypeId.User) =>
            domain.defs.meta.nodes.get(uid) match {
              case Some(DomainMember.User(_, dto: Typedef.Dto, _, _)) if isUserMapKeyEligibleDto(dto) =>
                val rsT     = trans.toRsTypeRefKeepForeigns(uid, domain, evo)
                val modName = s"${toSnakeCase(rsT.name)}_as_map_key"
                Some((rsT.crate.parts.toSeq :+ modName).mkString("::"))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

    /** Mirrors `BaboonValidator.isEligibleKey` — DTO branch only. id types are
      * always eligible (Q-M19-6); single-primitive-field non-contract DTOs are
      * eligible if the inner field is a primitive scalar or a recursively-eligible
      * nested wrapper (no opt/collection, no float wrappers per Q-M19-2).
      */
    private def isUserMapKeyEligibleDto(dto: Typedef.Dto): Boolean = {
      if (dto.isIdentifier) true
      else if (dto.contracts.nonEmpty) false
      else if (dto.fields.size != 1) false
      else dto.fields.head.tpe match {
        case _: TypeRef.Constructor => false
        case TypeRef.Scalar(b: TypeId.BuiltinScalar) =>
          b match {
            case TypeId.Builtins.f32 | TypeId.Builtins.f64 | TypeId.Builtins.f128 => false
            case _                                                                 => true
          }
        case TypeRef.Scalar(u: TypeId.User) =>
          domain.defs.meta.nodes.get(u) match {
            case Some(DomainMember.User(_, nested: Typedef.Dto, _, _)) => isUserMapKeyEligibleDto(nested)
            case _                                                      => false
          }
        case _ => false
      }
    }

    /** For a non-identifier single-field wrapper DTO, peel through nested
      * single-field wrappers to find the leaf builtin scalar TypeRef and the
      * accessor path ("f1.f2.f3") and reconstruction chain (innermost-first list
      * of (fieldSnakeName, RsType) pairs for wrapping back up on decode).
      *
      * Returns None for identifier types (those use parse_repr, not peeling).
      * The returned path is relative to `k` for encode: `k.<path>`.
      * The wrappers list is ordered innermost-to-outermost for .map chaining.
      */
    private def peelWrapperChain(dto: Typedef.Dto, rsName: RsType): Option[(TypeRef.Scalar, String, List[(String, RsType)])] = {
      if (dto.isIdentifier) None
      else if (dto.contracts.nonEmpty || dto.fields.size != 1) None
      else {
        val field      = dto.fields.head
        val fieldSnake = toSnakeCase(field.name.name)
        field.tpe match {
          case s @ TypeRef.Scalar(_: TypeId.BuiltinScalar) =>
            Some((s, fieldSnake, List((fieldSnake, rsName))))
          case TypeRef.Scalar(u: TypeId.User) =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, nested: Typedef.Dto, _, _)) =>
                val nestedRs = trans.toRsTypeRefKeepForeigns(u, domain, evo)
                peelWrapperChain(nested, nestedRs).map {
                  case (leafTpe, deepPath, innerWrappers) =>
                    (leafTpe, s"$fieldSnake.$deepPath", innerWrappers :+ (fieldSnake, rsName))
                }
              case _ => None
            }
          case _ => None
        }
      }
    }

    private def makeEnumRepr(e: Typedef.Enum, name: RsType): TextTree[RsValue] = {
      val variants = e.members.map {
        m =>
          q"${EnumWireStyle.wireName(m.name)},"
      }.toList

      val parseCases = e.members.map {
        m =>
          q""""${EnumWireStyle.wireName(m.name)}" => Ok(${name.asName}::${EnumWireStyle.wireName(m.name)}),"""
      }.toList

      val displayCases = e.members.map {
        m =>
          q"""${name.asName}::${EnumWireStyle.wireName(m.name)} => write!(f, "${EnumWireStyle.wireName(m.name)}"),"""
      }.toList

      val allVariants = e.members.map {
        m =>
          q"${name.asName}::${EnumWireStyle.wireName(m.name)},"
      }.toList

      q"""#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
         |pub enum ${name.asName} {
         |    ${variants.joinN().shift(4).trim}
         |}
         |
         |impl ${name.asName} {
         |    pub fn parse(s: &str) -> Result<Self, String> {
         |        match s {
         |            ${parseCases.joinN().shift(12).trim}
         |            _ => Err(format!("Unknown variant: {}", s)),
         |        }
         |    }
         |
         |    pub fn all() -> Vec<${name.asName}> {
         |        vec![
         |            ${allVariants.joinN().shift(12).trim}
         |        ]
         |    }
         |}
         |
         |impl std::fmt::Display for ${name.asName} {
         |    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
         |        match self {
         |            ${displayCases.joinN().shift(12).trim}
         |        }
         |    }
         |}
         |
         |impl std::str::FromStr for ${name.asName} {
         |    type Err = String;
         |    fn from_str(s: &str) -> Result<Self, Self::Err> {
         |        Self::parse(s)
         |    }
         |}""".stripMargin
    }

    private def makeAdtRepr(adt: Typedef.Adt, name: RsType): TextTree[RsValue] = {
      val dataMembers = adt.dataMembers(domain)
      // First, generate structs for each branch
      val branchStructs = dataMembers.map {
        mid =>
          domain.defs.meta.nodes(mid) match {
            case mdefn: DomainMember.User =>
              mdefn.defn match {
                case dto: Typedef.Dto =>
                  val branchName = trans.asRsType(dto.id, domain, evo)
                  makeDtoRepr(dto, branchName)
                case other =>
                  throw new RuntimeException(s"BUG: ADT member should be Dto, got: $other")
              }
            case other =>
              throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
          }
      }

      val branchCodecs = dataMembers.flatMap {
        mid =>
          domain.defs.meta.nodes(mid) match {
            case mdefn: DomainMember.User =>
              codecs.toList.flatMap(_.translate(mdefn, trans.asRsType(mdefn.id, domain, evo), trans.toRsTypeRefKeepForeigns(mdefn.id, domain, evo)).toList)
            case _ => Nil
          }
      }

      // Generate the ADT enum
      val variants = dataMembers.map {
        mid =>
          val branchName = mid.name.name.capitalize
          val branchType = trans.asRsType(mid, domain, evo)
          q"$branchName(${branchType.asName}),"
      }

      // Custom serde for ADT: serialize as {"BranchName": { ... }}
      val serImpl = if (target.language.wrappedAdtBranchCodecs) {
        val serBranches = dataMembers.map {
          mid =>
            val branchName = mid.name.name.capitalize
            q"""${name.asName}::$branchName(v) => v.serialize(serializer),"""
        }
        q"""impl serde::Serialize for ${name.asName} {
           |    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
           |        match self {
           |            ${serBranches.toList.joinN().shift(12).trim}
           |        }
           |    }
           |}""".stripMargin
      } else {
        val serBranches = dataMembers.map {
          mid =>
            val branchName = mid.name.name.capitalize
            q"""${name.asName}::$branchName(v) => {
               |    map.serialize_entry("$branchName", v)?;
               |}""".stripMargin
        }
        q"""impl serde::Serialize for ${name.asName} {
           |    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
           |        use serde::ser::SerializeMap;
           |        let mut map = serializer.serialize_map(Some(1))?;
           |        match self {
           |            ${serBranches.toList.joinN().shift(12).trim}
           |        }
           |        map.end()
           |    }
           |}""".stripMargin
      }

      val deBranches = dataMembers.map {
        mid =>
          val branchName = mid.name.name.capitalize
          q""""$branchName" => Ok(${name.asName}::$branchName(map.next_value()?)),"""
      }

      val branchNames    = dataMembers.map(_.name.name.capitalize)
      val branchNamesLit = branchNames.map(n => s""""$n"""").mkString(", ")

      val displayBranches = dataMembers.map {
        mid =>
          val branchName = mid.name.name.capitalize
          q"""${name.asName}::$branchName(v) => write!(f, "${name.name}::$branchName({:?})", v),"""
      }

      q"""${branchStructs.toList.joinNN()}
         |
         |${branchCodecs.toList.joinNN()}
         |
         |#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
         |pub enum ${name.asName} {
         |    ${variants.toList.joinN().shift(4).trim}
         |}
         |
         |$serImpl
         |
         |impl<'de> serde::Deserialize<'de> for ${name.asName} {
         |    fn deserialize<__De: serde::Deserializer<'de>>(deserializer: __De) -> Result<Self, __De::Error> {
         |        struct AdtVisitor;
         |        impl<'de> serde::de::Visitor<'de> for AdtVisitor {
         |            type Value = ${name.asName};
         |            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
         |                write!(f, "a single-key map representing ${name.name}")
         |            }
         |            fn visit_map<__M: serde::de::MapAccess<'de>>(self, mut map: __M) -> Result<Self::Value, __M::Error> {
         |                let key: String = map.next_key()?
         |                    .ok_or_else(|| serde::de::Error::custom("expected single-key map for ADT"))?;
         |                match key.as_str() {
         |                    ${deBranches.toList.joinN().shift(20).trim}
         |                    _ => Err(serde::de::Error::unknown_variant(&key, &[$branchNamesLit])),
         |                }
         |            }
         |        }
         |        deserializer.deserialize_map(AdtVisitor)
         |    }
         |}
         |
         |impl std::fmt::Display for ${name.asName} {
         |    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
         |        match self {
         |            ${displayBranches.toList.joinN().shift(12).trim}
         |        }
         |    }
         |}
         |
         |impl std::error::Error for ${name.asName} {}""".stripMargin
    }

    private def makeContractRepr(defn: DomainMember.User, name: RsType): TextTree[RsValue] = {
      val contract = defn.defn.asInstanceOf[Typedef.Contract]
      val methods = contract.fields.map {
        f =>
          val t = trans.asRsRef(f.tpe, domain, evo)
          q"fn ${toSnakeCase(f.name.name)}(&self) -> &$t;"
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""
      q"""pub trait ${name.asName} {
         |    ${body.shift(4).trim}
         |}""".stripMargin
    }

    private def makeServiceRepr(defn: DomainMember.User, name: RsType): TextTree[RsValue] = {
      val resolved    = ServiceResultResolver.resolve(domain, "rust", target.language.serviceResult, target.language.pragmas)
      val resolvedCtx = ServiceContextResolver.resolve(domain, "rust", target.language.serviceContext, target.language.pragmas)
      val ctxParam = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => ""
        case ResolvedServiceContext.AbstractContext(tn, pn) => s"$pn: $tn, "
        case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$pn: $tn, "
      }
      val service = defn.defn.asInstanceOf[Typedef.Service]
      val methods = service.methods.map {
        m =>
          val inType  = trans.asRsRef(m.sig, domain, evo)
          val outType = m.out.map(trans.asRsRef(_, domain, evo))
          val errType = m.err.map(trans.asRsRef(_, domain, evo))
          val rsFqName: RsValue => String = {
            case t: RsValue.RsType     => if (t.predef) t.name else (t.crate.parts :+ t.name).mkString("::")
            case t: RsValue.RsTypeName => t.name
          }
          val inStr   = inType.mapRender(rsFqName)
          val outStr  = outType.map(_.mapRender(rsFqName)).getOrElse("")
          val errStr  = errType.map(_.mapRender(rsFqName))
          val retStr  = resolved.renderReturnType(outStr, errStr, "()")
          val asyncKw = if (target.language.asyncServices) "async " else ""
          q"${asyncKw}fn ${toSnakeCase(m.name.name)}(&self, ${ctxParam}arg: $inStr) -> $retStr;"
      }
      val genericParam = resolvedCtx match {
        case ResolvedServiceContext.AbstractContext(tn, _) => s"<$tn>"
        case _                                             => ""
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""
      q"""pub trait ${name.asName}$genericParam {
         |    ${body.shift(4).trim}
         |}""".stripMargin
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase    = rsFiles.basename(domain, evo)
      val baseName = escapeRustModuleName(toSnakeCaseFileName(defn.id.name.name))
      val fname    = s"$baseName${suffix.getOrElse("")}.rs"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(n => escapeRustModuleName(n.name.toLowerCase)).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${escapeRustModuleName(toSnakeCaseFileName(id.name.name))}/$fname"
      }
    }
  }

  def toSnakeCaseFileName(s: String): String = {
    toSnakeCaseRaw(s)
  }

  private val rustKeywords: Set[String] = Set(
    "type",
    "self",
    "super",
    "crate",
    "mod",
    "fn",
    "let",
    "mut",
    "ref",
    "match",
    "if",
    "else",
    "while",
    "for",
    "loop",
    "break",
    "continue",
    "return",
    "struct",
    "enum",
    "trait",
    "impl",
    "use",
    "pub",
    "as",
    "in",
    "where",
    "async",
    "await",
    "dyn",
    "move",
    "static",
    "const",
    "unsafe",
    "extern",
    "true",
    "false",
  )

  def isRustKeyword(s: String): Boolean = rustKeywords.contains(s)

  def escapeRustKeyword(s: String): String = {
    if (isRustKeyword(s)) s"r#$s" else s
  }

  /** Escape a Rust keyword for use as a module/file name.
    * Uses readable alternative names instead of `r#` prefix to avoid issues with filenames and compound identifiers.
    */
  def escapeRustModuleName(s: String): String = {
    s match {
      case "in"                    => "input"
      case kw if isRustKeyword(kw) => s"${kw}_"
      case other                   => other
    }
  }

  def toSnakeCase(s: String): String = {
    escapeRustKeyword(toSnakeCaseRaw(s))
  }

  def toSnakeCaseRaw(s: String): String = {
    val result = new StringBuilder
    var i      = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c.isUpper) {
        if (i > 0 && (s.charAt(i - 1).isLower || s.charAt(i - 1).isDigit)) {
          result += '_'
        } else if (i > 0 && i + 1 < s.length && s.charAt(i + 1).isLower && s.charAt(i - 1).isUpper) {
          result += '_'
        }
        result += c.toLower
      } else {
        result += c
      }
      i += 1
    }
    result.toString()
  }
}
