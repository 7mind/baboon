package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
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
  final case class Output(
    path: String,
    tree: TextTree[TsValue],
    module: TsValue.TsModuleId,
    product: CompilerProduct,
    doNotModify: Boolean = false,
    isBarrel: Boolean    = false,
  )

  final case class DefnRepr(
    defn: TextTree[TsValue],
    codecs: List[TextTree[TsValue]],
  )

  class TsDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: TsTarget,
    domain: Domain,
    evo: BaboonEvolution,
    typeTranslator: TsTypeTranslator,
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
      if (block.isEmpty) tree else q"${block}$tree"
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
          typeTranslator.toTsModule(defn.id, domain, evo, tsFileTools.definitionsBasePkg),
          CompilerProduct.Definition,
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

      val tsTypeRef = typeTranslator.asTsType(defn.id, domain, evo, tsFileTools.definitionsBasePkg)
      val srcRef    = typeTranslator.asTsTypeKeepForeigns(defn.id, domain, evo, tsFileTools.definitionsBasePkg)

      val codecTrees =
        codecs.toList
          .flatMap(t => t.translate(defn, tsTypeRef, srcRef).toList)
          .map(obsoletePrevious)

      val repr = makeRepr(defn, tsTypeRef, isLatestVersion)

      val defnWithDocs = prependDocs(defn.docs, repr.defn)
      val allDefs      = (List(defnWithDocs) ++ codecTrees).joinNN()

      DefnRepr(allDefs, Nil)
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: TsValue.TsType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      defn.defn match {
        case dto: Typedef.Dto    => makeDtoRepr(defn, dto, name, isLatestVersion)
        case e: Typedef.Enum     => makeEnumRepr(e)
        case adt: Typedef.Adt    => makeAdtRepr(defn, adt, name)
        case _: Typedef.Contract => makeContractRepr(defn, name)
        case _: Typedef.Service  => makeServiceRepr(defn, name)
        case f: Typedef.Foreign  => makeForeignKeyCodecRepr(f, name)
      }
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
          val srcRef       = typeTranslator.asTsTypeKeepForeigns(f.id, domain, evo, tsFileTools.definitionsBasePkg)
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
          )
      }.toList)
    }

    private def doTranslateTest(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val tsTypeRef   = typeTranslator.asTsType(defn.id, domain, evo)
      val srcRef      = typeTranslator.asTsTypeKeepForeigns(defn.id, domain, evo, tsFileTools.definitionsBasePkg)
      val testTreeOpt = codecTests.translate(defn, tsTypeRef, srcRef)
      F.pure(testTreeOpt.map {
        testTree =>
          val testModule = getOutputModule(defn, suffix = Some(".test"), tsFileTools.testBasePkg)
          Output(
            getOutputPath(defn, suffix = Some(".test")),
            testTree,
            testModule,
            CompilerProduct.Test,
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
      val fieldsNameAndType = dto.fields.map(f => f.name -> typeTranslator.asTsRef(f.tpe, domain, evo, tsFileTools.definitionsBasePkg))
      val contractParents   = dto.contracts.map(c => typeTranslator.asTsTypeKeepForeigns(c, domain, evo, tsFileTools.definitionsBasePkg))
      val adtContracts = dto.id.owner match {
        case Owner.Adt(id) =>
          domain.defs.meta.nodes(id) match {
            case u: DomainMember.User => u.defn.asInstanceOf[Typedef.Adt].contracts.map(tid => typeTranslator.asTsType(tid, domain, evo, tsFileTools.definitionsBasePkg))
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

      val getters = dto.fields.map {
        f =>
          val tpe  = typeTranslator.asTsRef(f.tpe, domain, evo, tsFileTools.definitionsBasePkg)
          val getter =
            q"""public get ${f.name.name}(): $tpe {
               |    return this._${f.name.name};
               |}""".stripMargin
          prependDocs(f.docs, getter)
      }

      val constrcutorParams = dto.fields.map(f => q"${f.name.name}: ${typeTranslator.asTsRef(f.tpe, domain, evo, tsFileTools.definitionsBasePkg)}").join(", ")

      val constructorInside = fieldsNameAndType.map {
        case (n, _) =>
          q"this._${n.name} = ${n.name}"
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
          q"${f.name.name}?: ${typeTranslator.asTsRef(f.tpe, domain, evo, tsFileTools.definitionsBasePkg)}"
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
          q"${f.name.name}: ${typeTranslator.asTsRef(f.tpe, domain, evo, tsFileTools.definitionsBasePkg)}"
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
           |    ${fields.joinN().shift(4).trim}
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
      val enumName        = enum.id.name.name
      val lowercaseValues = target.language.enumLowercaseValues
      val branches = enum.members.map {
        m =>
          val pascal = EnumWireStyle.wireName(m.name)
          val value  = if (lowercaseValues) pascal.toLowerCase else pascal
          val ident  = if (lowercaseValues) m.name else pascal
          q"$ident = \"$value\""
      }.toSeq
      val pascalNames     = enum.members.map(m => EnumWireStyle.wireName(m.name))
      val parseComparison = if (lowercaseValues) "v === s.toLowerCase()" else "v === s"
      DefnRepr(
        q"""export enum $enumName {
           |    ${branches.join(",\n").shift(4).trim}
           |}
           |
           |export const ${enumName}_values: ReadonlyArray<$enumName> = [
           |    ${pascalNames.map(pn => q"$enumName.$pn").toList.join(",\n").shift(4).trim}
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

      val typeGuards = adt.members.toList.flatMap {
        mid =>
          domain.defs.meta.nodes(mid) match {
            case DomainMember.User(_, _: Typedef.Dto, _, _) =>
              val branchName = mid.name.name
              Some(q"export function is$branchName(value: $name): value is $branchName { return value instanceof $branchName; }")
            case _ => None // skip contracts/interfaces — instanceof doesn't work on them
          }
      }

      DefnRepr(
        q"""export type $name = ${adt.members.toList.map(m => q"${m.name.name}").join(" | ")}
           |
           |export const $name = {
           |    ${meta.join(",\n").shift(4).trim}
           |} as const
           |
           |${typeGuards.joinN().trim}
           |
           |${memberTrees.map(_.defn).toList.joinNN().trim}
           |""".stripMargin,
        Nil,
      )
    }

    private def makeContractRepr(defn: DomainMember.User, name: TsType): DefnRepr = {
      val contract = defn.defn.asInstanceOf[Typedef.Contract]
      val methods = contract.fields.map {
        f =>
          val t      = typeTranslator.asTsRef(f.tpe, domain, evo, tsFileTools.definitionsBasePkg)
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
          val inType  = typeTranslator.asTsRef(m.sig, domain, evo, tsFileTools.definitionsBasePkg)
          val outType = m.out.map(typeTranslator.asTsRef(_, domain, evo, tsFileTools.definitionsBasePkg))
          val errType = m.err.map(typeTranslator.asTsRef(_, domain, evo, tsFileTools.definitionsBasePkg))

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
        q"""export interface ${name.name}$genericParam {
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
    private sealed trait IdentifierFieldKind
    private object IdentifierFieldKind {
      case object Bit extends IdentifierFieldKind
      case object SignedInt extends IdentifierFieldKind /* i08/i16/i32 — number */
      case object SignedLong extends IdentifierFieldKind /* i64 — bigint */
      case object UnsignedSmallInt extends IdentifierFieldKind /* u08/u16/u32 — number */
      case object UnsignedLong extends IdentifierFieldKind /* u64 — bigint */
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
            case `bit`                 => IdentifierFieldKind.Bit
            case `i08` | `i16` | `i32` => IdentifierFieldKind.SignedInt
            case `i64`                 => IdentifierFieldKind.SignedLong
            case `u08` | `u16` | `u32` => IdentifierFieldKind.UnsignedSmallInt
            case `u64`                 => IdentifierFieldKind.UnsignedLong
            case `str`                 => IdentifierFieldKind.Str
            case `uid`                 => IdentifierFieldKind.Uid
            case `tsu`                 => IdentifierFieldKind.Tsu
            case `tso`                 => IdentifierFieldKind.Tso
            case `bytes`               => IdentifierFieldKind.Bytes
            case other =>
              throw new IllegalStateException(s"Identifier field has unsupported scalar $other; validator should have rejected this.")
          }
        case TypeRef.Scalar(uid: TypeId.User) =>
          IdentifierFieldKind.NestedId(uid)
        case other =>
          throw new IllegalStateException(s"Identifier field has unsupported TypeRef $other; validator should have rejected this.")
      }
    }

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
          val kind         = identifierFieldKind(f.tpe)
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
          val kind         = identifierFieldKind(f.tpe)

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
              val nestedTpe       = typeTranslator.asTsTypeKeepForeigns(uid, domain, evo, tsFileTools.definitionsBasePkg)
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
            case Owner.Ns(path) => s"$fbase/${typeTranslator.renderNsOwnerPath(path, domain).mkString("/")}/$fname"
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
