package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.IdentifierFieldKind
import io.septimalmind.baboon.translator.typescript.TsTypes.{tsBaboonAdtMemberMeta, tsBaboonDecoderFailure, tsBaboonEncoderFailure, tsBaboonGenerated, tsBaboonGeneratedLatest, tsBaboonIdReprBitToString, tsBaboonIdReprBytesToHex, tsBaboonIdReprCursor, tsBaboonIdReprEscapeStr, tsBaboonIdReprIsCanonicalUid, tsBaboonIdReprParseBit, tsBaboonIdReprParseBytesHex, tsBaboonIdReprParseFieldName, tsBaboonIdReprParseHeader, tsBaboonIdReprParseTso, tsBaboonIdReprParseTsu, tsBaboonIdReprTsoToString, tsBaboonIdReprTsuToString, tsBaboonIdReprU64ToString, tsBaboonRuntimeShared}
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.typescript.TsValue.TsType
import io.septimalmind.baboon.typer.EnumWireStyle
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait TsDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[TsDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[TsDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[TsDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[TsDefnTranslator.Output]]
  def translateDispatcher(): F[NEList[BaboonIssue], List[TsDefnTranslator.Output]]
}

object TsDefnTranslator {
  final case class ExportedSymbol(name: String, typeOnly: Boolean)

  final case class Output(
    path: String,
    tree: TextTree[TsValue],
    module: TsValue.TsModuleId,
    product: CompilerProduct,
    doNotModify: Boolean                  = false,
    isBarrel: Boolean                     = false,
    imports: List[TsType]                 = Nil,
    exports: Option[List[ExportedSymbol]] = None,
  )

  final case class DefnRepr(
    defn: TextTree[TsValue],
    codecs: List[TextTree[TsValue]],
    exports: List[ExportedSymbol] = Nil,
  )

  class TsDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: TsTarget,
    domain: Domain,
    evo: BaboonEvolution,
    typeTranslator: TsTypeTranslator,
    domainTypes: TsDomainTypes,
    codecs: Set[TsCodecTranslator],
    codecTests: TsCodecTestsTranslator,
    codecsFixture: TsCodecFixtureTranslator,
    tsFileTools: TsFileTools,
    tsDomainTreeTools: TsDomainTreeTools,
    wiringTranslator: TsServiceWiringTranslator,
    tsTrees: TsTreeTools,
  ) extends TsDefnTranslator[F] {

    /** Prepend a Javadoc-style `/** … */` doc comment block before a tree when
      * `docs` is non-empty. Returns the tree unchanged when `docs` is empty.
      */
    private def prependDocs(docs: Docs, tree: TextTree[TsValue]): TextTree[TsValue] = {
      val block = tsTrees.renderDocs(docs, "")
      if (block.isEmpty) tree else q"$block$tree"
    }

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateFixtures(defn)
      }
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTest(defn)
      }
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      F.pure(wiringTranslator.translateServiceRt().toList)
    }

    override def translateDispatcher(): F[NEList[BaboonIssue], List[Output]] = {
      F.pure(wiringTranslator.translateDispatcher().toList)
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr   = makeFullRepr(defn)
      val wiring = wiringTranslator.translate(defn)
      val client = wiringTranslator.translateClient(defn)
      val all = List(
        Output(
          getOutputPath(defn),
          repr.defn,
          domainTypes.toTsModule(defn.id, tsFileTools.definitionsBasePkg),
          CompilerProduct.Definition,
          exports = Some(repr.exports),
        )
      ) ++ wiring ++ client
      F.pure(all)
    }

    private def makeFullRepr(defn: DomainMember.User): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      def obsoletePrevious(tree: TextTree[TsValue]): TextTree[TsValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""/** @deprecated Version ${domain.version.v.toString} is deprecated, you should migrate to ${evo.latest.v.toString} */
             |$tree""".stripMargin
        }
      }

      val tsTypeRef = domainTypes.asTsType(defn.id, tsFileTools.definitionsBasePkg)
      val srcRef    = domainTypes.asTsTypeKeepForeigns(defn.id, tsFileTools.definitionsBasePkg)

      val renderedCodecs =
        codecs.toList
          .flatMap(t => t.translate(defn, tsTypeRef, srcRef).map(tree => (obsoletePrevious(tree), ExportedSymbol(t.codecName(srcRef).name, typeOnly = false))))

      val repr = makeRepr(defn, tsTypeRef, isLatestVersion)

      val defnWithDocs = prependDocs(defn.docs, repr.defn)
      val allDefs      = (List(defnWithDocs) ++ renderedCodecs.map(_._1)).joinNN()

      DefnRepr(allDefs, Nil, repr.exports ++ renderedCodecs.map(_._2))
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: TsValue.TsType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val repr = defn.defn match {
        case dto: Typedef.Dto    => makeDtoRepr(defn, dto, name, isLatestVersion)
        case e: Typedef.Enum     => makeEnumRepr(e)
        case adt: Typedef.Adt    => makeAdtRepr(defn, adt, name)
        case _: Typedef.Contract => makeContractRepr(defn, name)
        case _: Typedef.Service  => makeServiceRepr(defn, name)
        case f: Typedef.Foreign  => makeForeignKeyCodecRepr(f, name)
      }
      val declared = defn.defn match {
        case dto: Typedef.Dto =>
          List(ExportedSymbol(name.name, typeOnly = false)) ++
          (if (dto.isIdentifier) List(ExportedSymbol(name.name.head.toLower.toString + name.name.tail + "Codec", typeOnly = false)) else Nil)
        case _: Typedef.Enum =>
          val enumName = typeTranslator.escapeTsKeyword(defn.id.name.name)
          List(enumName, s"${enumName}_values", s"${enumName}_parse").map(ExportedSymbol(_, typeOnly = false))
        case _: Typedef.Adt      => List(ExportedSymbol(name.name, typeOnly = false))
        case _: Typedef.Contract => List(ExportedSymbol(name.name, typeOnly = true))
        case _: Typedef.Service  => List(ExportedSymbol(typeTranslator.serviceInterfaceName(name.name), typeOnly = true))
        case f: Typedef.Foreign =>
          f.bindings.get(BaboonLang.Typescript) match {
            case Some(Typedef.ForeignEntry(_, _: Typedef.ForeignMapping.Custom)) =>
              val foreignName = domainTypes.asTsTypeKeepForeigns(f.id, tsFileTools.definitionsBasePkg).name
              List(ExportedSymbol(s"${foreignName}_KeyCodec", typeOnly = true), ExportedSymbol(s"${foreignName}_KeyCodecHost", typeOnly = false))
            case _ => Nil
          }
      }
      repr.copy(exports = declared ++ repr.exports)
    }

    /** PR-I.1d (M24 Phase 3.1) — emit a `<Foreign>_KeyCodec` extension hook for
      * every Custom-mapped TypeScript foreign declaration. The host application
      * registers an implementation at boot which the JSON codec then uses to
      * encode/decode map keys. For BaboonRef-mapped foreigns we emit nothing —
      * the existing recursion into the aliased type covers the codec needs.
      *
      * Stringy foreigns (`string`) get a default identity impl so the common
      * case works out of the box. Non-stringy foreigns get a stub default that
      * throws BaboonDecoderFailure with an FQN-bearing diagnostic referring to
      * the Host const (PR-I.1b-D01 lesson).
      */
    private def makeForeignKeyCodecRepr(f: Typedef.Foreign, name: TsType): DefnRepr = {
      f.bindings.get(BaboonLang.Typescript) match {
        case None                                                                  => DefnRepr(q"", Nil)
        case Some(Typedef.ForeignEntry(_, _: Typedef.ForeignMapping.BaboonRef))    => DefnRepr(q"", Nil)
        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) =>
          // The codec/host names use the foreign's declared name (`FStr`) — not the deref'd
          // mapped name (`string`) that `name` resolves to. The `value: $name` field type
          // continues to use `name` so the signature reflects the host language type.
          val srcRef       = domainTypes.asTsTypeKeepForeigns(f.id, tsFileTools.definitionsBasePkg)
          val codecName    = s"${srcRef.name}_KeyCodec"
          val hostName     = s"${srcRef.name}_KeyCodecHost"
          val defaultName  = s"_default${srcRef.name}_KeyCodec"
          val instanceName = s"_${srcRef.name}_KeyCodec_instance"
          // Stringy allowlist (PR-I-D06 pattern guidance: only the language's allowlist; no dead alternatives).
          val isStringy = decl == "string"
          // FQN for the diagnostic — TS modules don't have classical fully-qualified names like
          // Java/Scala/C#, so we use `<module-path>.<host>` which is copy-pasteable into a
          // debugger expression and unambiguous across multi-version emission (PR-I-D03).
          val hostFqn = s"${srcRef.moduleId.path.mkString(".")}.$hostName"
          val defaultImpl = if (isStringy) {
            q"""const $defaultName: $codecName = {
               |    encodeKey: (v) => v,
               |    decodeKey: (s) => s,
               |};""".stripMargin
          } else {
            q"""const $defaultName: $codecName = {
               |    encodeKey: (_v) => { throw new $tsBaboonEncoderFailure("$hostFqn is not registered; call $hostFqn.register(impl) at app boot."); },
               |    decodeKey: (_s) => { throw new $tsBaboonDecoderFailure("$hostFqn is not registered; call $hostFqn.register(impl) at app boot."); },
               |};""".stripMargin
          }
          val tree =
            q"""export interface $codecName {
               |    encodeKey(value: $name): string;
               |    decodeKey(s: string): $name;
               |}
               |
               |$defaultImpl
               |
               |let $instanceName: $codecName = $defaultName;
               |
               |export const $hostName = {
               |    register(impl: $codecName): void { $instanceName = impl; },
               |    get instance(): $codecName { return $instanceName; },
               |};""".stripMargin
          DefnRepr(tree, Nil)
      }
    }

    private def doTranslateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val fixtureTreeOpt = codecsFixture.translate(defn)
      F.pure(fixtureTreeOpt.map {
        fixtureTree =>
          val fixtureModule = getOutputModule(defn, suffix = Some(".fixture"), tsFileTools.fixturesBasePkg)
          Output(
            getOutputPath(defn, suffix = Some(".fixture")),
            fixtureTree,
            fixtureModule,
            CompilerProduct.Fixture,
            exports = Some {
              val base = s"random_${typeTranslator.camelToKebab(defn.id.name.name).replace('-', '_')}"
              val names = defn.defn match {
                case _: Typedef.Adt => List(base, s"${base}_json", s"${base}_all", s"${base}_json_all")
                case _              => List(base, s"${base}_json")
              }
              names.map(ExportedSymbol(_, typeOnly = false))
            },
          )
      }.toList)
    }

    private def doTranslateTest(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val tsTypeRef   = domainTypes.asTsType(defn.id)
      val srcRef      = domainTypes.asTsTypeKeepForeigns(defn.id, tsFileTools.definitionsBasePkg)
      val testTreeOpt = codecTests.translate(defn, tsTypeRef, srcRef)
      F.pure(testTreeOpt.map {
        testTree =>
          val testModule = getOutputModule(defn, suffix = Some(".test"), tsFileTools.testBasePkg)
          Output(
            getOutputPath(defn, suffix = Some(".test")),
            testTree,
            testModule,
            CompilerProduct.Test,
            exports = Some(Nil),
          )
      }.toList)
    }

    private def makeDtoRepr(
      defn: DomainMember.User,
      dto: Typedef.Dto,
      name: TsType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker         = if (isLatestVersion) tsBaboonGeneratedLatest else tsBaboonGenerated
      val mainMeta          = tsDomainTreeTools.makeDataMeta(defn)
      val codecMeta         = codecs.flatMap(_.codecMeta(defn, name))
      val meta              = mainMeta ++ codecMeta
      val fieldsNameAndType = dto.fields.map(f => f.name -> domainTypes.asTsRef(f.tpe, tsFileTools.definitionsBasePkg))
      val contractParents   = dto.contracts.map(c => domainTypes.asTsTypeKeepForeigns(c, tsFileTools.definitionsBasePkg))
      val adtContracts = dto.id.owner match {
        case Owner.Adt(id) =>
          domain.defs.meta.nodes(id) match {
            case u: DomainMember.User => u.defn.asInstanceOf[Typedef.Adt].contracts.map(tid => domainTypes.asTsType(tid, tsFileTools.definitionsBasePkg))
            case other                => throw new RuntimeException(s"BUG: missing/wrong adt: $id => $other")
          }
        case _ => Seq.empty
      }
      // PR-25.8 / PR-22-D02: ADT-branch DTOs additionally implement `BaboonAdtMemberMeta` so the
      // `BaboonTypeMeta.from(value, useAdtIdentifier=true)` path on the runtime facade can
      // structurally detect the ADT-branch shape and pull `baboonAdtTypeIdentifier()` instead of
      // silently falling back to the concrete-branch type identifier.
      val adtMemberMarker = if (defn.ownedByAdt) Seq(tsBaboonAdtMemberMeta) else Seq.empty
      val parents         = adtContracts ++ contractParents ++ adtMemberMarker :+ genMarker

      val fields = fieldsNameAndType.map {
        case (name, tpe) =>
          q"private readonly _${name.name}: $tpe;"
      }

      // ADT-branch DTOs carry a `$$type` discriminant — a const-literal of the branch's own type
      // identifier — so the union (`A | B`) is a TS discriminated union narrowable via
      // `value.$$type === '…'`. It is inline-initialized (not a constructor param, not in `dto.fields`),
      // so codecs / toJSON / with / fromPlain never touch it and the wire format is unchanged. The
      // `$$` escapes to a literal `$` in the emitted source. (`$$type` cannot collide with a baboon
      // field name — identifiers are alphanumeric and cannot start with `$`.)
      val discriminatorField: List[TextTree[TsValue]] =
        if (defn.ownedByAdt) List(q"public readonly $$type = '${defn.id.toString}' as const;") else Nil

      val getters = dto.fields.map {
        f =>
          val tpe = domainTypes.asTsRef(f.tpe, tsFileTools.definitionsBasePkg)
          // The public getter is an accessor identifier; escape it for a keyword-named field
          // (`class` -> `class_`) so the codecs' `value.<getter>` reads (which assume the escaped
          // accessor name) resolve. The backing private field `_${name}` is `_`-prefixed and always
          // legal, so it stays raw; the wire key (toJSON / decode `obj["…"]`) keeps the original name.
          val getter =
            q"""public get ${typeTranslator.escapeTsKeyword(f.name.name)}(): $tpe {
               |    return this._${f.name.name};
               |}""".stripMargin
          prependDocs(f.docs, getter)
      }

      // The constructor PARAMETER is a binding identifier — illegal as a TS reserved word — so escape it
      // (e.g. `default` -> `default_`). The constructor is invoked positionally everywhere (`new $name(...)`
      // in with/fromPlain/codecs), so the rename is local: only the param declaration and its `this._x = x`
      // RHS reference must agree. The private field (`_${name}`) and all object-literal/member-access wire
      // keys keep the raw name, so the wire format is unchanged.
      val constrcutorParams =
        dto.fields.map(f => q"${typeTranslator.escapeTsKeyword(f.name.name)}: ${domainTypes.asTsRef(f.tpe, tsFileTools.definitionsBasePkg)}").join(", ")

      val constructorInside = fieldsNameAndType.map {
        case (n, _) =>
          q"this._${n.name} = ${typeTranslator.escapeTsKeyword(n.name)}"
      }.joinN()

      val implementsClause = if (parents.nonEmpty) q"implements ${parents.map(tpe => q"$tpe").join(", ")}" else q""

      val toJsonFields = dto.fields.map {
        f =>
          val ref = s"this._${f.name.name}"
          q"${f.name.name}: ${toJsonFieldExpr(f.tpe, ref)}"
      }

      val toJsonMethod =
        q"""public toJSON(): Record<string, unknown> {
           |    return {
           |        ${toJsonFields.join(",\n").shift(8).trim}
           |    };
           |}""".stripMargin

      val withParamFields = dto.fields.map {
        f =>
          q"${f.name.name}?: ${domainTypes.asTsRef(f.tpe, tsFileTools.definitionsBasePkg)}"
      }

      val withArgs = dto.fields.map {
        f =>
          q"'${f.name.name}' in overrides ? overrides.${f.name.name}! : this._${f.name.name}"
      }

      val withMethod =
        q"""public with(overrides: {${withParamFields.join("; ")}}): $name {
           |    return new $name(
           |        ${withArgs.join(",\n").shift(8).trim}
           |    );
           |}""".stripMargin

      val fromPlainParamFields = dto.fields.map {
        f =>
          q"${f.name.name}: ${domainTypes.asTsRef(f.tpe, tsFileTools.definitionsBasePkg)}"
      }

      val fromPlainArgs = dto.fields.map {
        f =>
          q"obj.${f.name.name}"
      }

      val fromPlainMethod =
        q"""public static fromPlain(obj: {${fromPlainParamFields.join("; ")}}): $name {
           |    return new $name(
           |        ${fromPlainArgs.join(",\n").shift(8).trim}
           |    );
           |}""".stripMargin

      // Identifier toString + parseRepr emission (PR-57d / spec:
      // docs/spec/identifier-repr.md). Emitted only when `dto.isIdentifier`.
      // The toString method lives on the class; the parser lives on a sibling
      // exported `<typeName>Codec` object (Q-FU-4: NOT a static on the class).
      val identifierToStringTree: TextTree[TsValue] =
        if (dto.isIdentifier) renderIdentifierToString(dto, name) else q""
      val identifierCodec: TextTree[TsValue] =
        if (dto.isIdentifier) renderIdentifierCodecObject(dto, name) else q""

      val membersBlock: TextTree[TsValue] =
        if (dto.isIdentifier)
          q"""${meta.joinN().trim}
             |
             |${identifierToStringTree.trim}""".stripMargin
        else meta.joinN().trim

      val classBody =
        q"""export class $name $implementsClause {
           |    ${(discriminatorField ++ fields).joinN().shift(4).trim}
           |
           |    constructor($constrcutorParams) {
           |        ${constructorInside.shift(8).trim}
           |    }
           |
           |    ${getters.joinN().shift(4).trim}
           |
           |    ${toJsonMethod.shift(4).trim}
           |
           |    ${withMethod.shift(4).trim}
           |
           |    ${fromPlainMethod.shift(4).trim}
           |
           |    ${membersBlock.shift(4).trim}
           |}""".stripMargin.trim

      val combined =
        if (dto.isIdentifier)
          q"""$classBody
             |
             |$identifierCodec""".stripMargin
        else classBody

      DefnRepr(combined, Nil)
    }

    private def toJsonFieldExpr(tpe: TypeRef, ref: String): TextTree[TsValue] = {
      tpe match {
        case TypeRef.Scalar(TypeId.Builtins.bytes) =>
          q"Array.from($ref)"
        case TypeRef.Scalar(_) =>
          q"$ref"
        case TypeRef.Constructor(TypeId.Builtins.set, _) =>
          q"Array.from($ref)"
        case TypeRef.Constructor(TypeId.Builtins.map, args) =>
          val isRecord = typeTranslator.isStringKeyMap(tpe)
          if (isRecord) q"$ref" // already a Record — JSON-friendly as-is
          else {
            val keyIsString = args.head match {
              case TypeRef.Scalar(TypeId.Builtins.str) => true
              case TypeRef.Scalar(TypeId.Builtins.uid) => true
              case _                                   => false
            }
            if (keyIsString) q"Object.fromEntries($ref)"
            else q"Array.from($ref.entries())"
          }
        case TypeRef.Constructor(TypeId.Builtins.lst, _) =>
          q"$ref"
        case TypeRef.Constructor(TypeId.Builtins.opt, args) =>
          val inner = toJsonFieldExpr(args.head, ref)
          if (inner.toString == ref) q"$ref"
          else q"$ref !== undefined ? $inner : undefined"
        case _ =>
          q"$ref"
      }
    }

    private def makeEnumRepr(enum: Typedef.Enum): DefnRepr = {
      // D9: escape through escapeTsKeyword so a model enum whose name matches a TS reserved word
      // does not produce invalid TS. Identity for PascalCase names — existing fixtures byte-identical.
      val enumName        = typeTranslator.escapeTsKeyword(enum.id.name.name)
      val lowercaseValues = target.language.enumLowercaseValues
      // The TS-side enum member identifier. In lowercase mode it is the (escaped) raw member name
      // (D9: escape — critical here because the verbatim member name can coincide with a TS keyword
      // like `type`/`in`); in canonical mode it is the PascalCase wire name. Computed ONCE and reused
      // for BOTH the declaration and the `_values` array (D10: the array previously referenced the
      // PascalCase wire name, which in lowercase mode is an UNDECLARED member). Identity for
      // PascalCase members — existing fixtures byte-identical.
      val memberIdents = enum.members.toList.map {
        m =>
          typeTranslator.enumMemberIdentifier(m.name)
      }
      val branches = enum.members.toList.zip(memberIdents).map {
        case (m, ident) =>
          val pascal = EnumWireStyle.wireName(m.name)
          val value  = if (lowercaseValues) pascal.toLowerCase else pascal
          // Wire values remain the original name so JSON round-trips are unaffected.
          q"$ident = \"$value\""
      }
      val parseComparison = if (lowercaseValues) "v === s.toLowerCase()" else "v === s"
      DefnRepr(
        q"""export enum $enumName {
           |    ${branches.join(",\n").shift(4).trim}
           |}
           |
           |export const ${enumName}_values: ReadonlyArray<$enumName> = [
           |    ${memberIdents.map(ident => q"$enumName.$ident").join(",\n").shift(4).trim}
           |] as const;
           |
           |export function ${enumName}_parse(s: string): $enumName {
           |    const found = ${enumName}_values.find(v => $parseComparison);
           |    if (found === undefined) {
           |        throw new $tsBaboonDecoderFailure("Unknown $enumName variant: " + s);
           |    }
           |    return found;
           |}""".stripMargin,
        Nil,
      )
    }

    private def makeAdtRepr(defn: DomainMember.User, adt: Typedef.Adt, name: TsType): DefnRepr = {
      val mainMeta  = tsDomainTreeTools.makeDataMeta(defn)
      val codecMeta = codecs.flatMap(_.codecMeta(defn, name))
      val meta      = mainMeta ++ codecMeta
      val memberTrees = adt.members.map {
        mid =>
          domain.defs.meta.nodes(mid) match {
            case mdefn: DomainMember.User => makeFullRepr(mdefn)
            case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
          }
      }

      // Branch references use the prefixed branch symbol (`<Adt>_<Branch>`) via asTsType. The `isX`
      // instanceof type guards are intentionally dropped — narrowing is done on the union members
      // directly (instanceof, or a discriminator) and the guards were unused internally and by tests.
      val branchRefs = adt.members.toList.map(m => q"${domainTypes.asTsType(m, tsFileTools.definitionsBasePkg)}")

      DefnRepr(
        q"""export type $name = ${branchRefs.join(" | ")}
           |
           |export const $name = {
           |    ${meta.join(",\n").shift(4).trim}
           |} as const
           |
           |${memberTrees.map(_.defn).toList.joinNN().trim}
           |""".stripMargin,
        Nil,
        memberTrees.toList.flatMap(_.exports),
      )
    }

    private def makeContractRepr(defn: DomainMember.User, name: TsType): DefnRepr = {
      val contract = defn.defn.asInstanceOf[Typedef.Contract]
      val methods = contract.fields.map {
        f =>
          val t      = domainTypes.asTsRef(f.tpe, tsFileTools.definitionsBasePkg)
          val member = q"readonly ${f.name.name}: $t;"
          prependDocs(f.docs, member)
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""
      DefnRepr(
        q"""export interface ${name.name} {
           |    ${body.shift(4).trim}
           |}""".stripMargin,
        Nil,
      )
    }

    private def makeServiceRepr(defn: DomainMember.User, name: TsType): DefnRepr = {
      val resolved    = ServiceResultResolver.resolve(domain, "typescript", target.language.serviceResult, target.language.pragmas)
      val resolvedCtx = ServiceContextResolver.resolve(domain, "typescript", target.language.serviceContext, target.language.pragmas)
      val isAsync     = target.language.asyncServices
      val ctxParam = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => ""
        case ResolvedServiceContext.AbstractContext(tn, pn) => s"$pn: $tn, "
        case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$pn: $tn, "
      }
      val service = defn.defn.asInstanceOf[Typedef.Service]
      val methods = service.methods.map {
        m =>
          val inType  = domainTypes.asTsRef(m.sig, tsFileTools.definitionsBasePkg)
          val outType = m.out.map(domainTypes.asTsRef(_, tsFileTools.definitionsBasePkg))
          val errType = m.err.map(domainTypes.asTsRef(_, tsFileTools.definitionsBasePkg))

          val baseRetTree: TextTree[TsValue] = if (resolved.noErrors || errType.isEmpty) {
            outType.getOrElse(q"void")
          } else {
            val isBuiltinEither = resolved.resultType.contains("BaboonEither")
            if (isBuiltinEither) {
              val outTree        = outType.getOrElse(q"void")
              val errTree        = errType.getOrElse(q"void")
              val resultTypeName = resolved.resultType.getOrElse("")
              val resultTypeRef  = TsValue.TsType(tsBaboonRuntimeShared, resultTypeName)
              val pat            = resolved.pattern.getOrElse("")
              val expanded       = expandPattern(pat, errTree, outTree)
              q"$resultTypeRef$expanded"
            } else {
              q"any"
            }
          }
          val retTree: TextTree[TsValue] = if (isAsync) q"Promise<$baseRetTree>" else baseRetTree
          val methodSig                  = q"${m.name.name}(${ctxParam}arg: $inType): $retTree;"
          prependDocs(m.docs, methodSig)
      }
      val genericParam = resolvedCtx match {
        case ResolvedServiceContext.AbstractContext(tn, _) => s"<$tn>"
        case _                                             => ""
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""
      DefnRepr(
        q"""export interface ${typeTranslator.serviceInterfaceName(name.name)}$genericParam {
           |    ${body.shift(4).trim}
           |}""".stripMargin,
        Nil,
      )
    }

    private def expandPattern(pat: String, errTree: TextTree[TsValue], outTree: TextTree[TsValue]): TextTree[TsValue] = {
      val placeholderRegex = "\\$(error|success)".r
      val segments         = scala.collection.mutable.ListBuffer.empty[TextTree[TsValue]]
      var lastEnd          = 0
      for (m <- placeholderRegex.findAllMatchIn(pat)) {
        if (m.start > lastEnd) {
          segments += q"${pat.substring(lastEnd, m.start)}"
        }
        m.group(1) match {
          case "error"   => segments += errTree
          case "success" => segments += outTree
        }
        lastEnd = m.end
      }
      if (lastEnd < pat.length) {
        segments += q"${pat.substring(lastEnd)}"
      }
      segments.reduce((a, b) => q"$a$b")
    }

    // ----- Identifier toString + parseRepr emission (PR-57d) -----
    // Spec: docs/spec/identifier-repr.md. Mirrors RsDefnTranslator + JvDefnTranslator
    // patterns but uses TS idioms:
    //   - `toString(): string` method on the class (Q-FU-4)
    //   - exported `<typeName>Codec` object literal with `parseRepr` /
    //     `parseReprCursor` static methods (Q-FU-4: NOT a static on the class —
    //     keeps `MyId.parseRepr` undiscoverable in autocomplete).
    private def signedTypeName(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => "i08"
      case TypeRef.Scalar(TypeId.Builtins.i16) => "i16"
      case TypeRef.Scalar(TypeId.Builtins.i32) => "i32"
      case other                               => throw new IllegalStateException(s"signedTypeName on non-signed-int: $other")
    }

    private def signedRangeCheck(tpe: TypeRef, varName: String): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => s"$varName >= -128 && $varName <= 127"
      case TypeRef.Scalar(TypeId.Builtins.i16) => s"$varName >= -32768 && $varName <= 32767"
      case TypeRef.Scalar(TypeId.Builtins.i32) => s"$varName >= -2147483648 && $varName <= 2147483647"
      case other                               => throw new IllegalStateException(s"signedRangeCheck on non-signed-int: $other")
    }

    private def unsignedSmallTypeName(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => "u08"
      case TypeRef.Scalar(TypeId.Builtins.u16) => "u16"
      case TypeRef.Scalar(TypeId.Builtins.u32) => "u32"
      case other                               => throw new IllegalStateException(s"unsignedSmallTypeName on non-u08/u16/u32: $other")
    }

    private def unsignedSmallRangeCheck(tpe: TypeRef, varName: String): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => s"$varName >= 0 && $varName <= 255"
      case TypeRef.Scalar(TypeId.Builtins.u16) => s"$varName >= 0 && $varName <= 65535"
      case TypeRef.Scalar(TypeId.Builtins.u32) => s"$varName >= 0 && $varName <= 4294967295"
      case other                               => throw new IllegalStateException(s"unsignedSmallRangeCheck on non-u08/u16/u32: $other")
    }

    private def renderFieldValueExprTs(tsFieldName: String, kind: IdentifierFieldKind): TextTree[TsValue] = {
      kind match {
        case IdentifierFieldKind.Bit              => q"$tsBaboonIdReprBitToString(this._$tsFieldName)"
        case IdentifierFieldKind.SignedInt        => q"this._$tsFieldName.toString()"
        case IdentifierFieldKind.SignedLong       => q"this._$tsFieldName.toString()"
        case IdentifierFieldKind.UnsignedSmallInt => q"this._$tsFieldName.toString()"
        case IdentifierFieldKind.UnsignedLong     => q"$tsBaboonIdReprU64ToString(this._$tsFieldName)"
        case IdentifierFieldKind.Str              => q"$tsBaboonIdReprEscapeStr(this._$tsFieldName)"
        case IdentifierFieldKind.Uid              => q"this._$tsFieldName"
        case IdentifierFieldKind.Tsu              => q"$tsBaboonIdReprTsuToString(this._$tsFieldName)"
        case IdentifierFieldKind.Tso              => q"$tsBaboonIdReprTsoToString(this._$tsFieldName)"
        case IdentifierFieldKind.Bytes            => q"$tsBaboonIdReprBytesToHex(this._$tsFieldName)"
        case IdentifierFieldKind.NestedId(_)      => q"""("{" + this._$tsFieldName.toString() + "}")"""
      }
    }

    private def renderIdentifierToString(dto: Typedef.Dto, name: TsType): TextTree[TsValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString
      val header     = s"$simpleName:$versionStr#"

      val fieldExprs: List[TextTree[TsValue]] = dto.fields.map {
        f =>
          val srcFieldName = f.name.name
          val kind         = IdentifierFieldKind.classify(f.tpe)
          val valueExpr    = renderFieldValueExprTs(srcFieldName, kind)
          q""""$srcFieldName:" + ($valueExpr)"""
      }

      val joinedFields =
        if (fieldExprs.isEmpty) q""""""""
        else fieldExprs.toSeq.join(""" + ":" + """)

      q"""public toString(): string {
         |    return "$header" + $joinedFields;
         |}""".stripMargin
    }

    private def renderIdentifierCodecObject(dto: Typedef.Dto, name: TsType): TextTree[TsValue] = {
      val simpleName = name.name
      val versionStr = domain.version.toString
      // Codec object name follows lower-camel convention from the type name:
      // PointId → pointIdCodec
      val codecObjName = name.name.head.toLower.toString + name.name.tail + "Codec"

      val fieldDecoders: List[TextTree[TsValue]] = dto.fields.zipWithIndex.map {
        case (f, idx) =>
          val srcFieldName = f.name.name
          val rawVar       = s"${srcFieldName}_raw"
          val valVar       = s"${srcFieldName}_v"
          val resVar       = s"${srcFieldName}_r"
          val isLast       = idx == dto.fields.length - 1
          val kind         = IdentifierFieldKind.classify(f.tpe)

          val parseHead =
            q"""const ${srcFieldName}_fnr = $tsBaboonIdReprParseFieldName(cursor, "$srcFieldName");
               |if (${srcFieldName}_fnr.tag === "Left") return { tag: "Left", value: ${srcFieldName}_fnr.value };""".stripMargin

          val parseValue: TextTree[TsValue] = kind match {
            case IdentifierFieldKind.Bit =>
              q"""const $rawVar = cursor.readUntilStructural();
                 |const $resVar = $tsBaboonIdReprParseBit($rawVar);
                 |if ($resVar.tag === "Left") return { tag: "Left", value: $resVar.value };
                 |const $valVar: boolean = $resVar.value;""".stripMargin
            case IdentifierFieldKind.SignedInt =>
              val typeName   = signedTypeName(f.tpe)
              val parsedVar  = s"${srcFieldName}_n"
              val rangeCheck = signedRangeCheck(f.tpe, parsedVar)
              q"""const $rawVar = cursor.readUntilStructural();
                 |if (!/^-?[0-9]+$$/.test($rawVar)) {
                 |    return { tag: "Left", value: "could not parse signed integer for field $srcFieldName: " + $rawVar };
                 |}
                 |const $parsedVar = parseInt($rawVar, 10);
                 |if (Number.isNaN($parsedVar)) {
                 |    return { tag: "Left", value: "could not parse signed integer for field $srcFieldName: " + $rawVar };
                 |}
                 |if (!($rangeCheck)) {
                 |    return { tag: "Left", value: "$typeName out of range for field $srcFieldName: " + $rawVar };
                 |}
                 |const $valVar: number = $parsedVar;""".stripMargin
            case IdentifierFieldKind.SignedLong =>
              // i64 in TS is bigint; range check is always true so elided per
              // PR-57a-D01 carryover (no dead `if (!(true))` block).
              q"""const $rawVar = cursor.readUntilStructural();
                 |if (!/^-?[0-9]+$$/.test($rawVar)) {
                 |    return { tag: "Left", value: "could not parse i64 for field $srcFieldName: " + $rawVar };
                 |}
                 |let $valVar: bigint;
                 |try {
                 |    $valVar = BigInt($rawVar);
                 |} catch (_e) {
                 |    return { tag: "Left", value: "could not parse i64 for field $srcFieldName: " + $rawVar };
                 |}
                 |if ($valVar < -9223372036854775808n || $valVar > 9223372036854775807n) {
                 |    return { tag: "Left", value: "i64 out of range for field $srcFieldName: " + $rawVar };
                 |}""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              val typeName   = unsignedSmallTypeName(f.tpe)
              val parsedVar  = s"${srcFieldName}_n"
              val rangeCheck = unsignedSmallRangeCheck(f.tpe, parsedVar)
              q"""const $rawVar = cursor.readUntilStructural();
                 |if ($rawVar.length > 0 && ($rawVar[0] === "+" || $rawVar[0] === "-")) {
                 |    return { tag: "Left", value: "unsigned value has leading sign for field $srcFieldName: " + $rawVar };
                 |}
                 |if (!/^[0-9]+$$/.test($rawVar)) {
                 |    return { tag: "Left", value: "could not parse unsigned integer for field $srcFieldName: " + $rawVar };
                 |}
                 |const $parsedVar = parseInt($rawVar, 10);
                 |if (Number.isNaN($parsedVar)) {
                 |    return { tag: "Left", value: "could not parse unsigned integer for field $srcFieldName: " + $rawVar };
                 |}
                 |if (!($rangeCheck)) {
                 |    return { tag: "Left", value: "$typeName out of range for field $srcFieldName: " + $rawVar };
                 |}
                 |const $valVar: number = $parsedVar;""".stripMargin
            case IdentifierFieldKind.UnsignedLong =>
              q"""const $rawVar = cursor.readUntilStructural();
                 |if ($rawVar.length > 0 && ($rawVar[0] === "+" || $rawVar[0] === "-")) {
                 |    return { tag: "Left", value: "unsigned value has leading sign for field $srcFieldName: " + $rawVar };
                 |}
                 |if (!/^[0-9]+$$/.test($rawVar)) {
                 |    return { tag: "Left", value: "could not parse u64 for field $srcFieldName: " + $rawVar };
                 |}
                 |let $valVar: bigint;
                 |try {
                 |    $valVar = BigInt($rawVar);
                 |} catch (_e) {
                 |    return { tag: "Left", value: "could not parse u64 for field $srcFieldName: " + $rawVar };
                 |}
                 |if ($valVar < 0n || $valVar > 18446744073709551615n) {
                 |    return { tag: "Left", value: "u64 out of range for field $srcFieldName: " + $rawVar };
                 |}""".stripMargin
            case IdentifierFieldKind.Str =>
              q"""const $resVar = cursor.readStrField();
                 |if ($resVar.tag === "Left") return { tag: "Left", value: $resVar.value };
                 |const $valVar: string = $resVar.value;""".stripMargin
            case IdentifierFieldKind.Uid =>
              q"""const $rawVar = cursor.readUntilStructural();
                 |if (!$tsBaboonIdReprIsCanonicalUid($rawVar)) {
                 |    return { tag: "Left", value: "uid not in canonical lowercase form for field $srcFieldName: " + $rawVar };
                 |}
                 |const $valVar: string = $rawVar;""".stripMargin
            case IdentifierFieldKind.Tsu =>
              q"""const ${srcFieldName}_rrf = cursor.readFixed(24);
                 |if (${srcFieldName}_rrf.tag === "Left") return { tag: "Left", value: ${srcFieldName}_rrf.value };
                 |const $rawVar = ${srcFieldName}_rrf.value;
                 |const $resVar = $tsBaboonIdReprParseTsu($rawVar);
                 |if ($resVar.tag === "Left") return { tag: "Left", value: $resVar.value };
                 |const $valVar = $resVar.value;""".stripMargin
            case IdentifierFieldKind.Tso =>
              q"""const ${srcFieldName}_rrf = cursor.readFixed(29);
                 |if (${srcFieldName}_rrf.tag === "Left") return { tag: "Left", value: ${srcFieldName}_rrf.value };
                 |const $rawVar = ${srcFieldName}_rrf.value;
                 |const $resVar = $tsBaboonIdReprParseTso($rawVar);
                 |if ($resVar.tag === "Left") return { tag: "Left", value: $resVar.value };
                 |const $valVar = $resVar.value;""".stripMargin
            case IdentifierFieldKind.Bytes =>
              q"""const $rawVar = cursor.readUntilStructural();
                 |const $resVar = $tsBaboonIdReprParseBytesHex($rawVar);
                 |if ($resVar.tag === "Left") return { tag: "Left", value: $resVar.value };
                 |const $valVar: Uint8Array = $resVar.value;""".stripMargin
            case IdentifierFieldKind.NestedId(uid) =>
              val nestedTpe       = domainTypes.asTsTypeKeepForeigns(uid, tsFileTools.definitionsBasePkg)
              val nestedCodecName = nestedTpe.name.head.toLower.toString + nestedTpe.name.tail + "Codec"
              val nestedCodecRef  = TsType(nestedTpe.moduleId, nestedCodecName)
              q"""const ${srcFieldName}_ro = cursor.expect("{");
                 |if (${srcFieldName}_ro.tag === "Left") return { tag: "Left", value: ${srcFieldName}_ro.value };
                 |const $resVar = $nestedCodecRef.parseReprCursor(cursor);
                 |if ($resVar.tag === "Left") return { tag: "Left", value: $resVar.value };
                 |const $valVar = $resVar.value;
                 |const ${srcFieldName}_rc = cursor.expect("}");
                 |if (${srcFieldName}_rc.tag === "Left") return { tag: "Left", value: ${srcFieldName}_rc.value };""".stripMargin
          }

          val sep =
            if (isLast) q""
            else
              q"""const ${srcFieldName}_rsep = cursor.expect(":");
                 |if (${srcFieldName}_rsep.tag === "Left") return { tag: "Left", value: ${srcFieldName}_rsep.value };""".stripMargin

          q"""$parseHead
             |$parseValue
             |$sep""".stripMargin.trim
      }

      val ctorArgs = dto.fields.map {
        f =>
          q"${f.name.name}_v"
      }

      val ctor =
        if (ctorArgs.nonEmpty)
          q"""new $name(
             |  ${ctorArgs.join(",\n").shift(2).trim}
             |)""".stripMargin
        else q"new $name()"

      val body = (fieldDecoders :+ q"""return { tag: "Right", value: $ctor };""").joinNN()

      q"""export const $codecObjName = {
         |    /** Parse the canonical identifier repr per docs/spec/identifier-repr.md.
         |     * Schema-directed parser: walks declared field order and dispatches per
         |     * field type. Returns `{tag:"Left",value:msg}` on any malformed input. */
         |    parseRepr(s: string): { readonly tag: "Left"; readonly value: string } | { readonly tag: "Right"; readonly value: $name } {
         |        const cursor = new $tsBaboonIdReprCursor(s);
         |        const inner = $codecObjName.parseReprCursor(cursor);
         |        if (inner.tag === "Left") return inner;
         |        if (!cursor.atEnd()) {
         |            return { tag: "Left", value: "unexpected trailing input at " + cursor.position() };
         |        }
         |        return inner;
         |    },
         |
         |    parseReprCursor(cursor: $tsBaboonIdReprCursor): { readonly tag: "Left"; readonly value: string } | { readonly tag: "Right"; readonly value: $name } {
         |        const __h = $tsBaboonIdReprParseHeader(cursor, "$simpleName", "$versionStr");
         |        if (__h.tag === "Left") return { tag: "Left", value: __h.value };
         |        ${body.shift(8).trim}
         |    },
         |} as const;""".stripMargin
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = tsFileTools.basename(domain, evo)
      val fname = s"${defn.id.name.name}${suffix.getOrElse("")}.ts"

      defn.defn match {
        // A service's own interface relocates to `<serviceDir>/service.ts`
        // (serviceDir = nsPrefix.lower ++ [kebab(serviceName)]). This must match
        // `TsTypeTranslator.toTsModule`'s service-type module so references resolve.
        case svc: Typedef.Service =>
          val serviceDir = typeTranslator.serviceDirSegments(svc).mkString("/")
          s"$fbase/$serviceDir/service${suffix.getOrElse("")}.ts"
        case _ =>
          defn.defn.id.owner match {
            case Owner.Toplevel => s"$fbase/$fname"
            case Owner.Ns(path) => s"$fbase/${domainTypes.renderNsOwnerPath(path).mkString("/")}/$fname"
            case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}.$fname"
          }
      }
    }

    private def getOutputModule(defn: DomainMember.User, suffix: Option[String], pkgBase: List[String] = Nil): TsValue.TsModuleId = {
      val path = pkgBase ++ getOutputPath(defn, suffix).stripSuffix(".ts").split("/")
      TsValue.TsModuleId(path)
    }
  }
}
