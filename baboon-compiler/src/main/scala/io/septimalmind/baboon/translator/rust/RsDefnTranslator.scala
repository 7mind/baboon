package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.rust.RsValue.RsType
import io.septimalmind.baboon.typer.BaboonEnquiries
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
  ) extends RsDefnTranslator[F] {

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr       = makeRepr(defn)
      val codecTrees = codecs.toList.flatMap(t => t.translate(defn, trans.asRsType(defn.id, domain, evo), trans.toRsTypeRefKeepForeigns(defn.id, domain, evo)).toList)
      val allDefs    = (repr +: codecTrees).joinNN()

      val mainOutput = Output(
        getOutputPath(defn),
        allDefs,
        trans.toRsCrate(domain.id, domain.version, evo),
        CompilerProduct.Definition,
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

      F.pure(mainOutput :: wiringOutput)
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

      q"""$derives
         |pub struct ${name.asName} {
         |    ${fieldsList.shift(4).trim}
         |}
         |
         |$ordImpls$customSerialize""".stripMargin
    }

    private def isWrappedAdtBranch(dto: Typedef.Dto): Boolean = {
      target.language.wrappedAdtBranchCodecs && (dto.id.owner match {
        case Owner.Adt(_) => true
        case _            => false
      })
    }

    private def dtoDerives(dto: Typedef.Dto): TextTree[RsValue] = {
      val hasNonOrd     = dto.fields.exists(f => hasDirectFloat(f.tpe))
      val wrappedBranch = isWrappedAdtBranch(dto)
      (hasNonOrd, wrappedBranch) match {
        case (_, true) if hasNonOrd =>
          q"#[derive(Clone, Debug, serde::Deserialize)]"
        case (_, true) =>
          q"#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Deserialize)]"
        case (true, false) =>
          q"#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]"
        case (false, false) =>
          q"#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]"
      }
    }

    private def dtoOrdImpls(dto: Typedef.Dto, name: RsType): TextTree[RsValue] = {
      val hasNonOrd = dto.fields.exists(f => hasDirectFloat(f.tpe))
      if (!hasNonOrd) {
        q""
      } else {
        val fieldComparisons = dto.fields.map {
          f =>
            val fld = toSnakeCase(f.name.name)
            if (hasDirectFloat(f.tpe)) {
              q"""match baboon_total_cmp_ser(&self.$fld, &other.$fld) {
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
           |        fn baboon_total_cmp_ser<T: serde::Serialize>(a: &T, b: &T) -> std::cmp::Ordering {
           |            let ja = serde_json::to_string(a).unwrap_or_default();
           |            let jb = serde_json::to_string(b).unwrap_or_default();
           |            ja.cmp(&jb)
           |        }
           |        ${fieldComparisons.joinN().shift(8).trim}
           |        std::cmp::Ordering::Equal
           |    }
           |}""".stripMargin
      }
    }

    private def hasDirectFloat(tpe: TypeRef): Boolean = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.f32)  => true
        case TypeRef.Scalar(TypeId.Builtins.f64)  => true
        case TypeRef.Scalar(TypeId.Builtins.f128) => true
        case TypeRef.Constructor(_, args)         => args.exists(hasDirectFloat)
        case _                                    => false
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

      attrs.toList
    }

    private def makeEnumRepr(e: Typedef.Enum, name: RsType): TextTree[RsValue] = {
      val variants = e.members.map {
        m =>
          q"${m.name.capitalize},"
      }.toList

      val parseCases = e.members.map {
        m =>
          q""""${m.name.capitalize}" => Ok(${name.asName}::${m.name.capitalize}),"""
      }.toList

      val displayCases = e.members.map {
        m =>
          q"""${name.asName}::${m.name.capitalize} => write!(f, "${m.name.capitalize}"),"""
      }.toList

      val allVariants = e.members.map {
        m =>
          q"${name.asName}::${m.name.capitalize},"
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
          val branchType = trans.asRsType(mid, domain, evo)
          q""""$branchName" => {
             |    let v: ${branchType.asName} = serde_json::from_value(value).map_err(serde::de::Error::custom)?;
             |    Ok(${name.asName}::$branchName(v))
             |}""".stripMargin
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
         |    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
         |        let map: std::collections::BTreeMap<String, serde_json::Value> = serde::Deserialize::deserialize(deserializer)?;
         |        let (key, value) = map.into_iter().next()
         |            .ok_or_else(|| serde::de::Error::custom("expected single-key map for ADT"))?;
         |        match key.as_str() {
         |            ${deBranches.toList.joinN().shift(12).trim}
         |            _ => Err(serde::de::Error::custom(format!("unknown ADT branch: {}", key))),
         |        }
         |    }
         |}""".stripMargin
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
          val outStr = outType.map(_.mapRender(rsFqName)).getOrElse("")
          val errStr = errType.map(_.mapRender(rsFqName))
          val retStr = resolved.renderReturnType(outStr, errStr, "()")
          q"fn ${toSnakeCase(m.name.name)}(&self, ${ctxParam}arg: $inType) -> $retStr;"
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
      val fbase = rsFiles.basename(domain, evo)
      val fname = s"${toSnakeCaseFileName(defn.id.name.name)}${suffix.getOrElse("")}.rs"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${toSnakeCaseFileName(id.name.name)}/$fname"
      }
    }
  }

  def toSnakeCaseFileName(s: String): String = {
    toSnakeCaseRaw(s)
  }

  def escapeRustKeyword(s: String): String = {
    s match {
      case "type" | "self" | "super" | "crate" | "mod" | "fn" | "let" | "mut" | "ref" | "match" | "if" | "else" | "while" | "for" | "loop" | "break" | "continue" |
          "return" | "struct" | "enum" | "trait" | "impl" | "use" | "pub" | "as" | "in" | "where" | "async" | "await" | "dyn" | "move" | "static" | "const" | "unsafe" |
          "extern" | "true" | "false" =>
        s"r#$s"
      case _ => s
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
        if (i > 0 && s.charAt(i - 1).isLower) {
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
