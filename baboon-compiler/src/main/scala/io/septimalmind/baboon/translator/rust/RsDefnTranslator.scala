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
    rsTrees: RsTreeTools,
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

    /** Prepend `///` doc lines before a tree when `docs` is non-empty.
      * Returns the tree unchanged when `docs` is empty.
      */
    private def prependDocs(docs: Docs, tree: TextTree[RsValue]): TextTree[RsValue] = {
      val block = rsTrees.renderDocs(docs, "")
      if (block.isEmpty) tree else q"${block}$tree"
    }

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
          prependDocs(defn.docs, makeDtoRepr(dto, name))

        case e: Typedef.Enum =>
          prependDocs(defn.docs, makeEnumRepr(e, name))

        case adt: Typedef.Adt =>
          makeAdtRepr(adt, name, defn.docs)

        case _: Typedef.Contract =>
          prependDocs(defn.docs, makeContractRepr(defn, name))

        case _: Typedef.Service =>
          makeServiceRepr(defn, name)

        case f: Typedef.Foreign =>
          makeForeignKeyCodecRepr(f, name)
      }
    }

    /** PR-I.3 (M24 Phase 3.3) — emit a `<Foreign>_KeyCodec` extension hook
      * + sibling `<foreign>_as_map_key` serde adapter module for every
      * Custom-mapped Rust foreign declaration. The host registers an
      * implementation at boot via `register_<foreign>_keycodec`; serde
      * routes map keys through it via the adapter module.
      *
      * Stringy customs ({"String", "&str", "std::string::String"}) get a
      * default identity impl so the common case works out of the box.
      * Non-stringy customs get a stub default whose `decode_key` returns
      * Err(<FQN-bearing diagnostic>) — host MUST register before first use;
      * `encode_key` panics with the same diagnostic since serializers don't
      * accept Result on the encode side.
      *
      * BaboonRef-mapped foreigns skip emission — the existing recursion into
      * the aliased type covers the codec needs.
      */
    private def makeForeignKeyCodecRepr(f: Typedef.Foreign, name: RsType): TextTree[RsValue] = {
      f.bindings.get(BaboonLang.Rust) match {
        case None                                                               => q""
        case Some(Typedef.ForeignEntry(_, _: Typedef.ForeignMapping.BaboonRef)) => q""
        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) =>
          val srcRef        = trans.toRsTypeRefKeepForeigns(f.id, domain, evo)
          val foreignName   = srcRef.name
          val snake         = toSnakeCase(foreignName)
          val codecTrait    = s"${foreignName}_KeyCodec"
          val defaultStruct = s"Default${foreignName}_KeyCodec"
          val staticName    = s"${snake.toUpperCase}_KEYCODEC"
          val getterName    = s"${snake}_keycodec"
          val registerName  = s"register_${snake}_keycodec"
          val adapterMod    = s"${snake}_as_map_key"
          val hostFqn       = (srcRef.crate.parts.toSeq :+ getterName).mkString("::")
          val derefedTpe    = trans.asRsType(f.id, domain, evo)
          // Stringy allowlist: only the actual Rust string type spellings the
          // codegen emits (`std::string::String` is what `asRsType` produces for
          // stringy Customs; `String` and `&str` are accepted as user-friendly
          // aliases). PR-I-D06 pattern: per-language allowlist; no dead alts.
          val isStringy = decl == "std::string::String" || decl == "String" || decl == "&str"

          val defaultImpl = if (isStringy) {
            q"""pub struct $defaultStruct;
               |impl $codecTrait for $defaultStruct {
               |    fn encode_key(&self, value: &${derefedTpe.asName}) -> String { value.clone() }
               |    fn decode_key(&self, s: &str) -> Result<${derefedTpe.asName}, String> { Ok(s.to_owned()) }
               |}""".stripMargin
          } else {
            // Non-stringy DefaultImpl: encode_key panics with FQN-bearing message
            // (serde::Serializer cannot consume Result, and a String stub would
            // produce malformed wire data). decode_key returns Err with the same
            // message so deserializers surface a clear "register me" diagnostic.
            // The panic-on-encode is intentional: silently emitting bogus keys
            // is worse than a crash that points the host operator at the fix.
            val msg = s"$hostFqn() is not registered; call $registerName(impl) at app boot."
            q"""pub struct $defaultStruct;
               |impl $codecTrait for $defaultStruct {
               |    fn encode_key(&self, _value: &${derefedTpe.asName}) -> String {
               |        panic!("$msg")
               |    }
               |    fn decode_key(&self, _s: &str) -> Result<${derefedTpe.asName}, String> {
               |        Err("$msg".to_string())
               |    }
               |}""".stripMargin
          }

          q"""pub trait $codecTrait: Send + Sync {
             |    fn encode_key(&self, value: &${derefedTpe.asName}) -> String;
             |    fn decode_key(&self, s: &str) -> Result<${derefedTpe.asName}, String>;
             |}
             |
             |${defaultImpl.trim}
             |
             |// PR-26.2 (M26) — last-wins concurrency contract (cross-backend parity).
             |// `RwLock<Option<Arc<dyn _>>>` allows re-registration to overwrite (last-wins),
             |// matching Scala/Java/Kotlin/KMP/C#/Dart/TypeScript/Swift/Python. Previous
             |// `OnceLock<Box<dyn _>>` silently no-op'd on re-register (PR-I.3-N01).
             |// Note: `Arc::new` is not const (Rust ≤ 1.75), so we lazily init under the
             |// write lock on first read. `RwLock::new(None)` IS const (Rust ≥ 1.63).
             |static $staticName: std::sync::RwLock<Option<std::sync::Arc<dyn $codecTrait>>> =
             |    std::sync::RwLock::new(None);
             |
             |pub fn $registerName(impl_: std::sync::Arc<dyn $codecTrait>) {
             |    *$staticName.write().expect("poisoned RwLock") = Some(impl_);
             |}
             |
             |pub fn $getterName() -> std::sync::Arc<dyn $codecTrait> {
             |    if let Some(ref c) = *$staticName.read().expect("poisoned RwLock") {
             |        return c.clone();
             |    }
             |    let mut w = $staticName.write().expect("poisoned RwLock");
             |    if w.is_none() {
             |        *w = Some(std::sync::Arc::new($defaultStruct));
             |    }
             |    w.as_ref().expect("just initialized").clone()
             |}
             |
             |pub mod $adapterMod {
             |    use super::*;
             |    use std::collections::BTreeMap;
             |
             |    pub fn serialize<S, V>(map: &BTreeMap<${derefedTpe.asName}, V>, serializer: S) -> Result<S::Ok, S::Error>
             |    where
             |        S: serde::Serializer,
             |        V: serde::Serialize,
             |    {
             |        use serde::ser::SerializeMap;
             |        let mut m = serializer.serialize_map(Some(map.len()))?;
             |        for (k, v) in map.iter() {
             |            let s: String = super::$getterName().encode_key(k);
             |            m.serialize_entry(&s, v)?;
             |        }
             |        m.end()
             |    }
             |
             |    pub fn deserialize<'de, __De, V>(deserializer: __De) -> Result<BTreeMap<${derefedTpe.asName}, V>, __De::Error>
             |    where
             |        __De: serde::Deserializer<'de>,
             |        V: serde::Deserialize<'de>,
             |    {
             |        use serde::Deserialize as _BaboonDeserialize;
             |        let raw: BTreeMap<String, V> = BTreeMap::<String, V>::deserialize(deserializer)?;
             |        let mut out: BTreeMap<${derefedTpe.asName}, V> = BTreeMap::new();
             |        for (s, v) in raw.into_iter() {
             |            let key = super::$getterName().decode_key(&s)
             |                .map_err(|e| serde::de::Error::custom(format!("malformed key: {}", e)))?;
             |            out.insert(key, v);
             |        }
             |        Ok(out)
             |    }
             |}""".stripMargin
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
      case object Bit extends IdentifierFieldKind
      case object SignedInt extends IdentifierFieldKind /* i08/i16/i32/i64 */
      case object UnsignedSmallInt extends IdentifierFieldKind /* u08/u16/u32 */
      case object UnsignedLong extends IdentifierFieldKind /* u64 */
      case object Str extends IdentifierFieldKind
      case object Uid extends IdentifierFieldKind
      case object Tsu extends IdentifierFieldKind
      case object Tso extends IdentifierFieldKind
      case object Bytes extends IdentifierFieldKind
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
        case IdentifierFieldKind.Bit => q"crate::baboon_identifier_repr::bit_to_string(self.$rsFieldName)"
        // Rust's primitive Display for signed integers produces canonical signed
        // decimal; for u08/u16/u32 it produces unsigned decimal natively (Rust
        // u-types are actually unsigned).
        case IdentifierFieldKind.SignedInt        => q"self.$rsFieldName.to_string()"
        case IdentifierFieldKind.UnsignedSmallInt => q"self.$rsFieldName.to_string()"
        case IdentifierFieldKind.UnsignedLong     => q"crate::baboon_identifier_repr::u64_to_string(self.$rsFieldName)"
        case IdentifierFieldKind.Str              => q"crate::baboon_identifier_repr::escape_str(&self.$rsFieldName)"
        // uuid::Uuid Display is the lowercase 36-char hyphenated form per RFC 4122.
        case IdentifierFieldKind.Uid         => q"self.$rsFieldName.to_string()"
        case IdentifierFieldKind.Tsu         => q"crate::baboon_identifier_repr::tsu_to_string(&self.$rsFieldName)"
        case IdentifierFieldKind.Tso         => q"crate::baboon_identifier_repr::tso_to_string(&self.$rsFieldName)"
        case IdentifierFieldKind.Bytes       => q"crate::baboon_identifier_repr::bytes_to_hex(&self.$rsFieldName)"
        case IdentifierFieldKind.NestedId(_) => q"""format!("{{{}}}", self.$rsFieldName)"""
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
              val typeName   = signedTypeName(f.tpe)
              val rangeCheck = signedRangeCheck(f.tpe, "v")
              val narrow     = signedNarrowFn(f.tpe)
              // i64 covers the full Long range — rangeCheck returns "true" — so
              // no range-check block is needed (mirrors prior backends D01 fix).
              val rangeBlock =
                if (rangeCheck == "true") q""
                else
                  q"""if !($rangeCheck) {
                     |    return Err(format!("$typeName out of range for field $srcFieldName: {}", $rawVar));
                     |}""".stripMargin
              q"""let $rawVar = cursor.read_until_structural();
                 |// Spec §5.4: signed integers must not carry a leading '+'.
                 |if $rawVar.starts_with('+') {
                 |    return Err(format!("signed integer must not have leading '+' for field $srcFieldName: {}", $rawVar));
                 |}
                 |let v: i64 = $rawVar.parse::<i64>()
                 |    .map_err(|_| format!("could not parse signed integer for field $srcFieldName: {}", $rawVar))?;
                 |$rangeBlock
                 |let $valVar: $tpe = v $narrow;""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              val typeName   = unsignedSmallTypeName(f.tpe)
              val rangeCheck = unsignedSmallRangeCheck(f.tpe, "v")
              val narrow     = unsignedSmallNarrowFn(f.tpe)
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
              val nestedFqPath       = (nestedTpe.crate.parts.toSeq :+ nestedCodecModName :+ "parse_repr_cursor").mkString("::")
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
          val fieldEx    = q"""$attrLine
             |pub ${toSnakeCase(f.name.name)}: $t,""".stripMargin.trim
          prependDocs(f.docs, fieldEx)
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

      // D21 (T53): emit `impl <Contract> for <Host>` so the host is type-level
      // associated with every contract it carries (`is C` / `has contract C`).
      // Without this the host merely duplicates the contract's fields but is NOT
      // usable AS the trait. See makeContractImpls.
      val contractImpls: TextTree[RsValue] = makeContractImpls(dto, name)

      q"""$structDef
         |
         |$ordImpls$customSerialize
         |
         |$identifierImpls
         |
         |$contractImpls
         |
         |$mapKeyAdapter""".stripMargin
    }

    /** D21 (T53) — for each contract a host DTO carries (`dto.contracts`, which the
      * typer fills with the full transitive supertrait closure), emit
      * `impl <Contract> for <Host>` wiring the trait's accessor methods to the
      * host struct's duplicated fields. This gives the host the trait at the type
      * level, matching the host↔contract coupling the C#/Scala lanes carry via
      * `: Contract` / `extends Contract`.
      *
      * Constraints handled:
      *   - ADT-owned contracts are NEVER emitted as standalone traits (the
      *     `translate` entry skips `Owner.Adt`), so `impl <S2> for <B1>` would
      *     reference a non-existent trait. We skip any contract whose owner is an
      *     ADT. The accessible supertrait portion (toplevel/namespaced contracts
      *     in the same transitive closure) is still emitted.
      *   - The contract trait declares only its OWN `contract.fields` (NOT the
      *     transitive parent fields — `makeContractRepr` emits no supertrait
      *     bound), so each `impl` wires exactly those own fields. The host carries
      *     them all (field duplication), so every accessor resolves.
      *   - A field that triggers `needsBox` is stored as `Box<T>` on the host but
      *     the trait method returns `&T`; we emit `self.field.as_ref()` so the
      *     reference type matches.
      *
      * If a contract's own field is somehow absent from the host (should not
      * happen — the typer guarantees field duplication), we omit that single
      * `impl` rather than emit code that fails to compile.
      */
    private def makeContractImpls(dto: Typedef.Dto, name: RsType): TextTree[RsValue] = {
      val hostFieldNames: Set[String] = dto.fields.map(f => toSnakeCase(f.name.name)).toSet

      val impls: List[TextTree[RsValue]] = dto.contracts.flatMap {
        cid =>
          domain.defs.meta.nodes.get(cid) match {
            case Some(DomainMember.User(_, c: Typedef.Contract, _, _)) =>
              cid.owner match {
                // ADT-owned contracts are not emitted as standalone traits — skip.
                case Owner.Adt(_) => None
                case _ =>
                  val contractTpe = trans.asRsType(cid, domain, evo)
                  // Every own contract field must be a duplicated host field; if
                  // not, omit this impl rather than emit non-compiling code.
                  if (c.fields.forall(f => hostFieldNames.contains(toSnakeCase(f.name.name)))) {
                    val methods = c.fields.map {
                      f =>
                        val rsName = toSnakeCase(f.name.name)
                        val rawT   = trans.asRsRef(f.tpe, domain, evo)
                        val retT   = if (needsBox(f.tpe)) q"Box<$rawT>" else rawT
                        val body   = if (needsBox(f.tpe)) q"self.$rsName.as_ref()" else q"&self.$rsName"
                        q"fn $rsName(&self) -> &$retT { $body }"
                    }
                    val body = if (methods.nonEmpty) methods.joinN() else q""
                    Some(
                      q"""impl $contractTpe for ${name.asName} {
                         |    ${body.shift(4).trim}
                         |}""".stripMargin
                    )
                  } else None
              }
            case _ => None
          }
      }

      if (impls.nonEmpty) impls.joinNN() else q""
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

      // Helper: detect whether a leaf TypeRef.Scalar(User) refers to a
      // Custom-mapped Foreign typedef. PR-I.3 routes such leaves through
      // the foreign's `<foreign>_keycodec()` extension hook rather than
      // the host type's Display/FromStr.
      def customForeignKeyCodecGetter(leafTpe: TypeRef): Option[String] = leafTpe match {
        case TypeRef.Scalar(uid: TypeId.User) =>
          domain.defs.meta.nodes.get(uid) match {
            case Some(DomainMember.User(_, fdef: Typedef.Foreign, _, _)) =>
              fdef.bindings.get(BaboonLang.Rust) match {
                case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
                  val srcRef = trans.toRsTypeRefKeepForeigns(uid, domain, evo)
                  val getter = s"${toSnakeCase(srcRef.name)}_keycodec"
                  Some((srcRef.crate.parts.toSeq :+ getter).mkString("::"))
                case _ => None
              }
            case _ => None
          }
        case _ => None
      }

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
          customForeignKeyCodecGetter(leafTpe) match {
            case Some(getterPath) =>
              // PR-I.3 (M24 Phase 3.3): foreign leaf → route through host getter.
              // `getterPath` is fully qualified (starts with `crate::`).
              q"$getterPath().encode_key(&k.$leafPath)"
            case None =>
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
          // PR-I.3: foreign leaves route through the foreign's `<foreign>_keycodec()`
          // hook so the host can plug in arbitrary string<->host encodings.
          val parseExpr: TextTree[RsValue] = customForeignKeyCodecGetter(leafTpe) match {
            case Some(getterPath) =>
              q"$getterPath().decode_key(&s)"
            case None =>
              leafTpe match {
                case TypeRef.Scalar(TypeId.Builtins.bytes) =>
                  q"crate::baboon_identifier_repr::parse_bytes_hex(&s)"
                case TypeRef.Scalar(TypeId.Builtins.tsu) =>
                  q"crate::baboon_runtime::time_formats::parse_tsu(&s)"
                case TypeRef.Scalar(TypeId.Builtins.tso) =>
                  q"crate::baboon_runtime::time_formats::parse_tso(&s)"
                case _ =>
                  q"s.parse::<$leafT>().map_err(|e| format!(\"{}\", e))"
              }
          }
          // wrappers is ordered innermost-first; chain .map(|v| Wrapper { field: v })
          // Emit the full RsType (not .asName) so the import collector in
          // RsBaboonTranslator.renderTree picks up `use crate::...::Wrapper;` for
          // intermediate wrappers that are not otherwise referenced as field types
          // in this file. RsType with fq=false renders as the bare name (see the
          // mapRender branch on RsValue.RsType in RsBaboonTranslator), so this stays
          // unqualified at the call site while still registering the import.
          wrappers.foldLeft(parseExpr) {
            case (acc, (fieldSnake, wrapRs)) =>
              q"$acc.map(|v| $wrapRs { $fieldSnake: v })"
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
         |                .map_err(|e| serde::de::Error::custom(format!("malformed key: {}", e)))?;
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
      val hasBareFloat   = dto.fields.exists(f => isBareFloat(f.tpe))
      val hasWrappedFlt  = dto.fields.exists(f => hasFloatRecursive(f.tpe) && !isBareFloat(f.tpe))
      val hasUnorderable = dto.fields.exists(f => hasAnyField(f.tpe))
      val wrappedBranch  = isWrappedAdtBranch(dto)
      // Four independent axes:
      // - hasBareFloat: a field's static type is exactly `f32`/`f64` — Eq/Ord can be honoured by
      //   a manual impl using `f64::total_cmp`. We emit that impl in `dtoOrdImpls` and suppress the
      //   auto-derive for Eq/Ord (manual impl supersedes derive). Rationale: pkg0 fixtures
      //   (`set[T6_D2]` etc.) require T6_D2 to be `Ord`. NaN-soundness is a known follow-up.
      // - hasWrappedFlt: a field's static type contains `f32`/`f64` only via a generic constructor
      //   (e.g. `Option<f64>`, `Vec<f64>`, `BTreeMap<…, f64>`) or via a transitively float-bearing
      //   user type. `total_cmp` is a method of `f64`, not of `Option<f64>` — manual impls cannot
      //   be synthesized soundly. Drop Eq/Ord entirely; rely on `derive(PartialEq, PartialOrd)`.
      //   This is the BAB-R04 fix.
      // - hasUnorderable (any): PartialEq is derived (AnyOpaque has PartialEq); no Eq/Ord because
      //   serde_json::Value has no total ordering and JSON-source bytes can be byte-different but
      //   semantically equal — Eq is misleading.
      // - wrappedBranch: encoded as an inline ADT branch — no Serialize derive (handled by ADT).
      val serdeDerives =
        if (wrappedBranch) "serde::Deserialize"
        else "serde::Serialize, serde::Deserialize"

      val cmpDerives =
        if (hasWrappedFlt) "PartialEq, PartialOrd, "
        else if (hasBareFloat) "" // manual PartialEq/Eq/PartialOrd/Ord via dtoOrdImpls
        else if (hasUnorderable) "PartialEq, "
        else "PartialEq, Eq, PartialOrd, Ord, "

      q"#[derive(Clone, Debug, $cmpDerives$serdeDerives)]"
    }

    private def dtoOrdImpls(dto: Typedef.Dto, name: RsType): TextTree[RsValue] = {
      val hasBareFloat  = dto.fields.exists(f => isBareFloat(f.tpe))
      val hasWrappedFlt = dto.fields.exists(f => hasFloatRecursive(f.tpe) && !isBareFloat(f.tpe))
      if (hasWrappedFlt) {
        // BAB-R04: wrapped-float field types (Option<f64>, Vec<f64>, …) cannot have manual
        // total_cmp synthesized — `total_cmp` is a method of `f64` only. Rely on derive(PartialEq,
        // PartialOrd) (see dtoDerives); no manual Eq/Ord — they are unsound for NaN regardless.
        q""
      } else if (hasBareFloat) {
        // Bare-float fields: synthesize Eq/Ord via `f64::total_cmp` so float-bearing structs can
        // appear inside `BTreeSet<T>` / `BTreeMap<T, _>` (pkg0 cross-version fixtures rely on
        // this). Long-term unsoundness for NaN is a known follow-up.
        val fieldComparisons = dto.fields.map {
          f =>
            val fld = toSnakeCase(f.name.name)
            if (isBareFloat(f.tpe)) {
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
      } else q""
    }

    /** True iff `tpe` is exactly the scalar `f32` or `f64` (no wrapping). */
    private def isBareFloat(tpe: TypeRef): Boolean = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.f32) => true
      case TypeRef.Scalar(TypeId.Builtins.f64) => true
      case _                                   => false
    }

    /** True iff `tpe` reaches an `f32`/`f64` either directly, through a generic
      * constructor (`opt`, `lst`, `set`, `map`), or transitively through a referenced
      * user `data`/`adt` typedef. Enums and foreigns are never float-bearing.
      *
      * Transitive propagation is required because Rust's `derive(Eq, Ord)` needs the
      * field types to themselves implement `Eq, Ord`. Per BAB-R04 a float-bearing struct
      * derives only `PartialEq, PartialOrd`; any struct that contains it as a field
      * (directly, in a collection, or inside another wrapping struct) inherits the
      * same constraint and must also drop `Eq, Ord`.
      *
      * Cycle protection: typedefs may contain self-references (recursive ADTs); we
      * track visited user ids to terminate.
      */
    private def hasFloatRecursive(tpe: TypeRef): Boolean =
      hasFloatRecursive0(tpe, Set.empty)

    private def hasFloatRecursive0(tpe: TypeRef, seen: Set[TypeId.User]): Boolean = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.f32) => true
        case TypeRef.Scalar(TypeId.Builtins.f64) => true
        case TypeRef.Scalar(u: TypeId.User) =>
          if (seen.contains(u)) false
          else {
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, defn, _, _)) =>
                userDefnHasFloat(defn, seen + u)
              case _ => false
            }
          }
        case TypeRef.Constructor(_, args) => args.exists(hasFloatRecursive0(_, seen))
        case _                            => false
      }
    }

    private def userDefnHasFloat(defn: Typedef, seen: Set[TypeId.User]): Boolean = defn match {
      case dto: Typedef.Dto =>
        dto.fields.exists(f => hasFloatRecursive0(f.tpe, seen))
      case adt: Typedef.Adt =>
        adt.dataMembers(domain).exists {
          mid =>
            domain.defs.meta.nodes.get(mid) match {
              case Some(DomainMember.User(_, branchDefn, _, _)) => userDefnHasFloat(branchDefn, seen + mid)
              case _                                            => false
            }
        }
      case _ => false // enums, foreigns, contracts, services, aliases — never float-bearing here
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
      userMapKeyAdapterPath(f.tpe).foreach {
        path =>
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
              // PR-I.3 (M24 Phase 3.3): direct foreign map key — route through
              // the foreign's emitted `<foreign>_as_map_key` adapter (Custom only;
              // BaboonRef-aliased foreigns reuse the aliased type's serde path).
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Rust) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
                    val rsT     = trans.toRsTypeRefKeepForeigns(uid, domain, evo)
                    val modName = s"${toSnakeCase(rsT.name)}_as_map_key"
                    Some((rsT.crate.parts.toSeq :+ modName).mkString("::"))
                  case _ => None
                }
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

    /** Mirrors `BaboonValidator.isEligibleKey` — DTO, Enum, and Foreign branches.
      * id types are always eligible (Q-M19-6); single-primitive-field non-contract DTOs are
      * eligible if the inner field is a primitive scalar, an enum, a foreign type, or a
      * recursively-eligible nested wrapper (no opt/collection, no float wrappers per Q-M19-2).
      */
    private def isUserMapKeyEligibleDto(dto: Typedef.Dto): Boolean = {
      if (dto.isIdentifier) true
      else if (dto.contracts.nonEmpty) false
      else if (dto.fields.size != 1) false
      else
        dto.fields.head.tpe match {
          case _: TypeRef.Constructor => false
          case TypeRef.Scalar(b: TypeId.BuiltinScalar) =>
            b match {
              case TypeId.Builtins.f32 | TypeId.Builtins.f64 | TypeId.Builtins.f128 => false
              case _                                                                => true
            }
          case TypeRef.Scalar(u: TypeId.User) =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, nested: Typedef.Dto, _, _)) => isUserMapKeyEligibleDto(nested)
              // Q-M19-7: enums round-trip via Display / parse; foreign types assume the host
              // provides compatible Display / FromStr (PR-I will route through a dedicated KeyCodec).
              case Some(DomainMember.User(_, _: Typedef.Enum, _, _))    => true
              case Some(DomainMember.User(_, _: Typedef.Foreign, _, _)) => true
              case _                                                    => false
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
          case s @ TypeRef.Scalar(u: TypeId.User) =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, nested: Typedef.Dto, _, _)) =>
                val nestedRs = trans.toRsTypeRefKeepForeigns(u, domain, evo)
                peelWrapperChain(nested, nestedRs).map {
                  case (leafTpe, deepPath, innerWrappers) =>
                    (leafTpe, s"$fieldSnake.$deepPath", innerWrappers :+ (fieldSnake, rsName))
                }
              // Enum and Foreign leaves: treat the user scalar itself as the leaf.
              // encode → format!("{}", k.<field>); decode → s.parse::<ForeignType>()
              // then rewrap. trans.asRsRef dereferences Foreign → its host type for parse.
              case Some(DomainMember.User(_, _: Typedef.Enum, _, _)) =>
                Some((s, fieldSnake, List((fieldSnake, rsName))))
              case Some(DomainMember.User(_, _: Typedef.Foreign, _, _)) =>
                Some((s, fieldSnake, List((fieldSnake, rsName))))
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

    private def makeAdtRepr(adt: Typedef.Adt, name: RsType, adtDocs: Docs): TextTree[RsValue] = {
      val dataMembers = adt.dataMembers(domain)
      // First, generate structs for each branch (with branch-level docs prepended)
      val branchStructs = dataMembers.map {
        mid =>
          domain.defs.meta.nodes(mid) match {
            case mdefn: DomainMember.User =>
              mdefn.defn match {
                case dto: Typedef.Dto =>
                  val branchName = trans.asRsType(dto.id, domain, evo)
                  prependDocs(mdefn.docs, makeDtoRepr(dto, branchName))
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
          // wireVariantName: original model name (capitalized) used in serde string literals.
          // rsVariantName: keyword-escaped Rust identifier used in source code.
          val wireVariantName = mid.name.name.capitalize
          val rsVariantName   = escapeRustTypeName(wireVariantName)
          val branchType      = trans.asRsType(mid, domain, evo)
          q"$rsVariantName(${branchType.asName}),"
      }

      // Custom serde for ADT: serialize as {"BranchName": { ... }}
      val serImpl = if (target.language.wrappedAdtBranchCodecs) {
        val serBranches = dataMembers.map {
          mid =>
            val rsVariantName = escapeRustTypeName(mid.name.name.capitalize)
            q"""${name.asName}::$rsVariantName(v) => v.serialize(serializer),"""
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
            val wireVariantName = mid.name.name.capitalize
            val rsVariantName   = escapeRustTypeName(wireVariantName)
            q"""${name.asName}::$rsVariantName(v) => {
               |    map.serialize_entry("$wireVariantName", v)?;
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
          val wireVariantName = mid.name.name.capitalize
          val rsVariantName   = escapeRustTypeName(wireVariantName)
          q""""$wireVariantName" => Ok(${name.asName}::$rsVariantName(map.next_value()?)),"""
      }

      // branchNames for serde error messages use wire names (original model names).
      val branchNames    = dataMembers.map(_.name.name.capitalize)
      val branchNamesLit = branchNames.map(n => s""""$n"""").mkString(", ")

      val displayBranches = dataMembers.map {
        mid =>
          val wireVariantName = mid.name.name.capitalize
          val rsVariantName   = escapeRustTypeName(wireVariantName)
          q"""${name.asName}::$rsVariantName(v) => write!(f, "${name.name}::$wireVariantName({:?})", v),"""
      }

      val adtDocBlock = rsTrees.renderDocs(adtDocs, "")
      // BAB-R04: if any branch DTO has a wrapped-float (or transitively float-bearing) field, that
      // branch dropped `Eq, Ord` — and the wrapping ADT enum cannot derive `Eq, Ord` either, since
      // `derive(Eq, Ord)` on `enum Foo { Branch(BranchDto) }` needs `BranchDto: Eq + Ord`.
      // Branches with only bare-float fields keep `Eq, Ord` via manual `total_cmp` impls.
      val adtBranchHasWrappedFloat = adt.dataMembers(domain).exists {
        mid =>
          domain.defs.meta.nodes.get(mid) match {
            case Some(DomainMember.User(_, dto: Typedef.Dto, _, _)) =>
              dto.fields.exists(f => hasFloatRecursive(f.tpe) && !isBareFloat(f.tpe))
            case _ => false
          }
      }
      val adtCmpDerives =
        if (adtBranchHasWrappedFloat) "PartialEq, PartialOrd"
        else "PartialEq, Eq, PartialOrd, Ord"
      q"""${branchStructs.toList.joinNN()}
         |
         |${branchCodecs.toList.joinNN()}
         |
         |${adtDocBlock}#[derive(Clone, Debug, $adtCmpDerives)]
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
          val t       = trans.asRsRef(f.tpe, domain, evo)
          val methodEx = q"fn ${toSnakeCase(f.name.name)}(&self) -> &$t;"
          prependDocs(f.docs, methodEx)
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
          val inStr    = inType.mapRender(rsFqName)
          val outStr   = outType.map(_.mapRender(rsFqName)).getOrElse("")
          val errStr   = errType.map(_.mapRender(rsFqName))
          val retStr   = resolved.renderReturnType(outStr, errStr, "()")
          val asyncKw  = if (target.language.asyncServices) "async " else ""
          val methodEx = q"${asyncKw}fn ${toSnakeCase(m.name.name)}(&self, ${ctxParam}arg: $inStr) -> $retStr;"
          prependDocs(m.docs, methodEx)
      }
      val genericParam = resolvedCtx match {
        case ResolvedServiceContext.AbstractContext(tn, _) => s"<$tn>"
        case _                                             => ""
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""
      val traitTree = q"""pub trait ${name.asName}$genericParam {
                         |    ${body.shift(4).trim}
                         |}""".stripMargin
      prependDocs(defn.docs, traitTree)
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
    // strict keywords (current Rust edition)
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
    // reserved keywords (not yet active but rejected by the compiler)
    "abstract",
    "become",
    "box",
    "do",
    "final",
    "macro",
    "override",
    "priv",
    "try",
    "typeof",
    "unsized",
    "virtual",
    "yield",
  )

  /** Keywords that CANNOT be escaped with the `r#` raw-identifier prefix in Rust.
    * These are path-segment pseudo-keywords that the compiler treats specially:
    * `self` (value path component), `super` (parent path), `crate` (crate root),
    * `Self` (self type alias). Any identifier position using these must rename instead.
    */
  private val nonRawEscapable: Set[String] = Set("self", "super", "crate", "Self")

  def isRustKeyword(s: String): Boolean = rustKeywords.contains(s)

  def escapeRustKeyword(s: String): String = {
    if (nonRawEscapable.contains(s)) s"${s}_"
    else if (isRustKeyword(s)) s"r#$s"
    else s
  }

  /** Escape a Rust keyword for use as a module/file name.
    * Uses readable alternative names instead of `r#` prefix to avoid issues with filenames and compound identifiers.
    */
  def escapeRustModuleName(s: String): String = {
    s match {
      case "in"                                => "input"
      case kw if nonRawEscapable.contains(kw) => s"${kw}_"
      case kw if isRustKeyword(kw)             => s"${kw}_"
      case other                               => other
    }
  }

  /** Escape a Rust type/struct/enum/variant name produced by `.capitalize`.
    * PascalCase names from `.capitalize` will not clash with lowercase Rust keywords
    * in practice, but we apply the escape for correctness. The `nonRawEscapable`
    * path-segment keywords (`Self`) capitalized become `Self_` to avoid conflicts
    * with the built-in `Self` type alias.
    */
  def escapeRustTypeName(s: String): String = {
    if (nonRawEscapable.contains(s)) s"${s}_"
    else if (isRustKeyword(s)) s"r#$s"
    else s
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
