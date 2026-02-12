package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.typescript.TsTypes.{tsBaboonGenerated, tsBaboonGeneratedLatest, tsBaboonRuntimeShared}
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.typescript.TsValue.TsType
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
  ) extends TsDefnTranslator[F] {

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

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr = makeFullRepr(defn)
      val wiring = wiringTranslator.translate(defn)
      val all = List(
        Output(
          getOutputPath(defn),
          repr.defn,
          typeTranslator.toTsModule(defn.id, domain.version, evo, tsFileTools.definitionsBasePkg),
          CompilerProduct.Definition,
        )
      ) ++ wiring
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

      val allDefs = (List(repr.defn) ++ codecTrees).joinNN()

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
        case _: Typedef.Foreign  => DefnRepr(q"", Nil)
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
      val parents = adtContracts ++ contractParents :+ genMarker

      val fields = fieldsNameAndType.map {
        case (name, tpe) =>
          q"private readonly _${name.name}: $tpe;"
      }

      val getters = fieldsNameAndType.map {
        case (name, tpe) =>
          q"""public get ${name.name}(): $tpe {
             |    return this._${name.name};
             |}""".stripMargin
      }

      val constrcutorParams = dto.fields.map(f => q"${f.name.name}: ${typeTranslator.asTsRef(f.tpe, domain, evo, tsFileTools.definitionsBasePkg)}").join(", ")

      val constructorInside = fieldsNameAndType.map {
        case (n, _) =>
          q"this._${n.name} = ${n.name}"
      }.joinN()

      val implementsClause = if (parents.nonEmpty) q"implements ${parents.map(tpe => q"$tpe").join(", ")}" else q""

      DefnRepr(
        q"""export class $name $implementsClause {
           |    ${fields.joinN().shift(4).trim}
           |
           |    constructor($constrcutorParams) {
           |        ${constructorInside.shift(8).trim}
           |    }
           |
           |    ${getters.joinN().shift(4).trim}
           |
           |    ${meta.joinN().shift(4).trim}
           |}""".stripMargin.trim,
        Nil,
      )
    }

    private def makeEnumRepr(enum: Typedef.Enum): DefnRepr = {
      val enumName      = enum.id.name.name
      val branchesNames = enum.members.map(_.name)
      val branches      = branchesNames.map(name => q"$name = \"$name\"").toSeq
      DefnRepr(
        q"""export enum $enumName {
           |    ${branches.join(",\n").shift(4).trim}
           |}
           |
           |export const ${enumName}_values: ReadonlyArray<$enumName> = [
           |    ${branchesNames.map(branchName => q"$enumName.$branchName").toList.join(",\n").shift(4).trim}
           |] as const;
           |
           |export function ${enumName}_parse(s: string): $enumName {
           |    const found = ${enumName}_values.find(v => v === s);
           |    if (found === undefined) {
           |        throw new Error("Unknown $enumName variant: " + s);
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

      DefnRepr(
        q"""export type $name = ${adt.members.toList.map(m => q"${m.name.name}").join(" | ")}
           |
           |export const $name = {
           |    ${meta.join(",\n").shift(4).trim}
           |} as const
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
          val t = typeTranslator.asTsRef(f.tpe, domain, evo, tsFileTools.definitionsBasePkg)
          q"readonly ${f.name.name}: $t;"
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

          val retTree: TextTree[TsValue] = if (resolved.noErrors || errType.isEmpty) {
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
          q"${m.name.name}(${ctxParam}arg: $inType): $retTree;"
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

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = tsFileTools.basename(domain, evo)
      val fname = s"${defn.id.name.name}${suffix.getOrElse("")}.ts"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}.$fname"
      }
    }

    private def getOutputModule(defn: DomainMember.User, suffix: Option[String], pkgBase: List[String] = Nil): TsValue.TsModuleId = {
      val path = pkgBase ++ getOutputPath(defn, suffix).stripSuffix(".ts").split("/")
      TsValue.TsModuleId(path)
    }
  }
}
