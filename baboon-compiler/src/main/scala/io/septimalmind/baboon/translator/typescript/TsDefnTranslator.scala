package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.ServiceResultResolver
import io.septimalmind.baboon.translator.typescript.TsValue.TsType
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait TsDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[TsDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[TsDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[TsDefnTranslator.Output]]
}

object TsDefnTranslator {
  final case class Output(
    path: String,
    tree: TextTree[TsValue],
    module: TsValue.TsModuleId,
    product: CompilerProduct,
    doNotModify: Boolean = false,
    isBarrel: Boolean = false,
  )

  class TsDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: TsTarget,
    domain: Domain,
    evo: BaboonEvolution,
    tsFiles: TsFileTools,
    trans: TsTypeTranslator,
    codecs: Set[TsCodecTranslator],
    codecTests: TsCodecTestsTranslator,
    codecsFixture: TsCodecFixtureTranslator,
    enquiries: BaboonEnquiries,
  ) extends TsDefnTranslator[F] {

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.defn match {
        case _: Typedef.Foreign => F.pure(List.empty)
        case _ =>
          val repr = makeRepr(defn)
          val codecTrees = codecs.toList.flatMap(t => t.translate(defn, trans.asTsType(defn.id, domain, evo), trans.toTsTypeRefKeepForeigns(defn.id, domain, evo)).toList)
          val allDefs = (repr +: codecTrees).joinNN()

          F.pure(
            List(
              Output(
                getOutputPath(defn),
                allDefs,
                trans.toTsModule(domain.id, domain.version, evo),
                CompilerProduct.Definition,
              )
            )
          )
      }
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
          val fixtureModule = getOutputModule(defn, suffix = Some(".fixture"))
          Output(
            getOutputPath(defn, suffix = Some(".fixture")),
            fixtureTree,
            fixtureModule,
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
      val tsTypeRef = trans.asTsType(defn.id, domain, evo)
      val srcRef    = trans.toTsTypeRefKeepForeigns(defn.id, domain, evo)
      val fixtureModule = getOutputModule(defn, suffix = Some(".fixture"))
      val testTreeOpt = codecTests.translate(defn, tsTypeRef, srcRef, fixtureModule)
      F.pure(testTreeOpt.map {
        testTree =>
          val testModule = getOutputModule(defn, suffix = Some(".test"))
          Output(
            getOutputPath(defn, suffix = Some(".test")),
            testTree,
            testModule,
            CompilerProduct.Test,
          )
      }.toList)
    }

    private def makeRepr(defn: DomainMember.User): TextTree[TsValue] = {
      val name = trans.asTsType(defn.id, domain, evo)

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

    private def makeDtoRepr(dto: Typedef.Dto, name: TsType): TextTree[TsValue] = {
      val fields = dto.fields.map { f =>
        val t = trans.asTsRef(f.tpe, domain, evo)
        f.tpe match {
          case TypeRef.Constructor(TypeId.Builtins.opt, _) =>
            q"readonly ${f.name.name}?: $t;"
          case _ =>
            q"readonly ${f.name.name}: $t;"
        }
      }
      val fieldsList = if (fields.nonEmpty) fields.joinN() else q""

      q"""export interface ${name.asName} {
         |    ${fieldsList.shift(4).trim}
         |}""".stripMargin
    }

    private def makeEnumRepr(e: Typedef.Enum, name: TsType): TextTree[TsValue] = {
      val variants = e.members.map { m =>
        q""""${m.name.capitalize}""""
      }.toList

      val unionType = variants.map(v => v).reduce((a, b) => q"$a | $b")

      val allValues = e.members.map { m =>
        q""""${m.name.capitalize}","""
      }.toList

      q"""export type ${name.asName} = $unionType;
         |
         |export const ${name.asName}_values: ReadonlyArray<${name.asName}> = [
         |    ${allValues.joinN().shift(4).trim}
         |] as const;
         |
         |export function ${name.asName}_parse(s: string): ${name.asName} {
         |    const found = ${name.asName}_values.find(v => v === s);
         |    if (found === undefined) {
         |        throw new Error("Unknown ${name.name} variant: " + s);
         |    }
         |    return found;
         |}""".stripMargin
    }

    private def makeAdtRepr(adt: Typedef.Adt, name: TsType): TextTree[TsValue] = {
      val dataMembers = adt.dataMembers(domain)

      val branchInterfaces = dataMembers.map { mid =>
        domain.defs.meta.nodes(mid) match {
          case mdefn: DomainMember.User =>
            mdefn.defn match {
              case dto: Typedef.Dto =>
                val branchName = trans.asTsType(dto.id, domain, evo)
                makeDtoRepr(dto, branchName)
              case other =>
                throw new RuntimeException(s"BUG: ADT member should be Dto, got: $other")
            }
          case other =>
            throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
        }
      }

      val branchCodecs = dataMembers.flatMap { mid =>
        domain.defs.meta.nodes(mid) match {
          case mdefn: DomainMember.User =>
            codecs.toList.flatMap(_.translate(mdefn, trans.asTsType(mdefn.id, domain, evo), trans.toTsTypeRefKeepForeigns(mdefn.id, domain, evo)).toList)
          case _ => Nil
        }
      }

      val wrappedTypes = dataMembers.map { mid =>
        val branchName = mid.name.name
        val branchType = trans.asTsType(mid, domain, evo)
        q"""{ readonly _tag: "$branchName"; readonly value: ${branchType.asName} }"""
      }

      val unionType = wrappedTypes.toList.reduce((a, b) => q"$a | $b")

      val factoryFns = dataMembers.map { mid =>
        val branchName = mid.name.name
        val branchType = trans.asTsType(mid, domain, evo)
        q"""export function ${name.asName}_$branchName(value: ${branchType.asName}): ${name.asName} {
           |    return { _tag: "$branchName", value };
           |}""".stripMargin
      }

      q"""${branchInterfaces.toList.joinNN()}
         |
         |${branchCodecs.toList.joinNN()}
         |
         |export type ${name.asName} = $unionType;
         |
         |${factoryFns.toList.joinNN()}""".stripMargin
    }

    private def makeContractRepr(defn: DomainMember.User, name: TsType): TextTree[TsValue] = {
      val contract = defn.defn.asInstanceOf[Typedef.Contract]
      val methods = contract.fields.map { f =>
        val t = trans.asTsRef(f.tpe, domain, evo)
        q"readonly ${f.name.name}: $t;"
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""
      q"""export interface ${name.asName} {
         |    ${body.shift(4).trim}
         |}""".stripMargin
    }

    private def makeServiceRepr(defn: DomainMember.User, name: TsType): TextTree[TsValue] = {
      val resolved = ServiceResultResolver.resolve(domain, "typescript", target.language.serviceResult, target.language.pragmas)
      val service  = defn.defn.asInstanceOf[Typedef.Service]
      val methods = service.methods.map { m =>
        val inType  = trans.asTsRef(m.sig, domain, evo)
        val outType = m.out.map(trans.asTsRef(_, domain, evo))
        val errType = m.err.map(trans.asTsRef(_, domain, evo))

        val retTree: TextTree[TsValue] = if (resolved.noErrors || errType.isEmpty) {
          outType.getOrElse(q"void")
        } else {
          val tsRender: TsValue => String = {
            case t: TsValue.TsType     => t.name
            case t: TsValue.TsTypeName => t.name
          }
          val outStr  = outType.map(_.mapRender(tsRender)).getOrElse("")
          val errStr  = errType.map(_.mapRender(tsRender))
          val retStr  = resolved.renderReturnType(outStr, errStr, "void")
          q"$retStr"
        }
        q"${m.name.name}(arg: $inType): $retTree;"
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""
      q"""export interface ${name.asName} {
         |    ${body.shift(4).trim}
         |}""".stripMargin
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = tsFiles.basename(domain, evo)
      val fname = s"${trans.camelToKebab(defn.id.name.name)}${suffix.getOrElse("")}.ts"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${trans.camelToKebab(id.name.name)}/$fname"
      }
    }

    private def getOutputModule(defn: DomainMember.User, suffix: Option[String]): TsValue.TsModuleId = {
      val path = getOutputPath(defn, suffix).stripSuffix(".ts")
      TsValue.TsModuleId(NEList.unsafeFrom(path.split('/').toList))
    }
  }
}
