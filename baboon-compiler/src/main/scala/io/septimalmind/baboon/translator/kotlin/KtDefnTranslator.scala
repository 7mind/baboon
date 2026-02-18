package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.KtTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.kotlin.KtValue.KtType
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait KtDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[KtDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[KtDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[KtDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[KtDefnTranslator.Output]]
}

object KtDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: KtType,
    tpeKeepForeigns: KtType,
    tpeId: TextTree[KtValue],
    trees: Map[String, TextTree[KtValue]],
  )

  final case class DefnRepr(
    defn: TextTree[KtValue],
    codecs: List[CodecReg],
  )

  final case class Output(
    path: String,
    tree: TextTree[KtValue],
    pkg: KtValue.KtPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[KtValue]])] = Nil,
  )

  class KtDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: KtTarget,
    domain: Domain,
    evo: BaboonEvolution,
    ktFiles: KtFileTools,
    ktTrees: KtTreeTools,
    trans: KtTypeTranslator,
    codecs: Set[KtCodecTranslator],
    codecTests: KtCodecTestsTranslator,
    codecsFixture: KtCodecFixtureTranslator,
    wiringTranslator: KtServiceWiringTranslator,
    ktDomainTreeTools: KtDomainTreeTools,
  ) extends KtDefnTranslator[F] {
    import KtTypes.*

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr = makeFullRepr(defn, inPkg = true)

      val registrations = codecs.toList.map(codec => codec.id -> repr.codecs.flatMap(reg => reg.trees.get(codec.id).map(expr => q"${reg.tpeId}, $expr")))

      val mainOutput = Output(
        getOutputPath(defn),
        repr.defn,
        trans.toKtPkg(domain.id, domain.version, evo),
        CompilerProduct.Definition,
        codecReg = registrations,
      )

      val wiringOutput = wiringTranslator
        .translate(defn).map {
          wiringTree =>
            val pkg     = trans.toKtPkg(domain.id, domain.version, evo)
            val wrapped = ktTrees.inPkg(pkg.parts.toSeq, wiringTree)
            Output(
              getOutputPath(defn, suffix = Some("Wiring")),
              wrapped,
              pkg,
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
      val fixtureTreeOut = makeFixtureRepr(defn).map {
        fixtureTreeWithPkg =>
          Output(
            getOutputPath(defn, suffix = Some("Fixture")),
            fixtureTreeWithPkg,
            trans.toKtPkg(domain.id, domain.version, evo),
            CompilerProduct.Fixture,
          )
      }

      F.pure(fixtureTreeOut.toList)
    }

    private def makeFixtureRepr(defn: DomainMember.User): Option[TextTree[KtValue]] = {
      val srcRef = trans.toKtTypeRefKeepForeigns(defn.id, domain, evo)
      val ns     = srcRef.pkg.parts

      val fixtureTree        = codecsFixture.translate(defn)
      val fixtureTreeWithPkg = fixtureTree.map(t => ktTrees.inPkg(ns.toSeq, t))

      fixtureTreeWithPkg
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTest(defn)
      }
    }

    private def doTranslateTest(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val codecTestOut = makeTestRepr(defn).map {
        codecTestWithPkg =>
          Output(
            getOutputPath(defn, suffix = Some("Tests")),
            codecTestWithPkg,
            trans.toKtPkg(domain.id, domain.version, evo),
            CompilerProduct.Test,
          )
      }

      F.pure(codecTestOut.toList)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map {
        tree =>
          val pkg     = trans.toKtPkg(domain.id, domain.version, evo)
          val wrapped = ktTrees.inPkg(pkg.parts.toSeq, tree)
          val fbase   = ktFiles.basename(domain, evo)
          Output(
            s"$fbase/BaboonServiceRt.kt",
            wrapped,
            pkg,
            CompilerProduct.Definition,
          )
      }.toList
      F.pure(result)
    }

    private def makeTestRepr(defn: DomainMember.User): Option[TextTree[KtValue]] = {
      val ktTypeRef = trans.asKtType(defn.id, domain, evo)
      val srcRef    = trans.toKtTypeRefKeepForeigns(defn.id, domain, evo)
      val ns        = srcRef.pkg.parts

      val testTree        = codecTests.translate(defn, ktTypeRef, srcRef)
      val testTreeWithPkg = testTree.map(t => ktTrees.inPkg(ns.toSeq, t))

      testTreeWithPkg
    }

    private def makeFullRepr(
      defn: DomainMember.User,
      inPkg: Boolean,
    ): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      def deprecatePrevious(tree: TextTree[KtValue]): TextTree[KtValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""@Deprecated("Version ${domain.version.v.toString} is deprecated, you should migrate to ${evo.latest.v.toString}")
             |$tree""".stripMargin
        }
      }

      val ktTypeRef = trans.asKtType(defn.id, domain, evo)
      val srcRef    = trans.toKtTypeRefKeepForeigns(defn.id, domain, evo)

      val repr = makeRepr(defn, ktTypeRef, isLatestVersion)

      val codecTrees =
        codecs.toList
          .flatMap(t => t.translate(defn, ktTypeRef, srcRef).toList)
          .map(deprecatePrevious)

      val defnRepr = deprecatePrevious(repr.defn)

      assert(defn.id.pkg == domain.id)

      val ns = srcRef.pkg.parts

      val allDefs = (defnRepr +: codecTrees).joinNN()
      val content = if (inPkg) ktTrees.inPkg(ns.toSeq, allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => Nil
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap(
              codec =>
                if (codec.isActive(d.id)) List(codec.id -> q"$baboonLazy { ${codec.codecName(srcRef).copy(fq = true)} }")
                else Nil
            )
          List(CodecReg(defn.id, ktTypeRef, srcRef, q"\"${defn.id.toString}\"", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      DefnRepr(content, allRegs)
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: KtValue.KtType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = ktDomainTreeTools.makeDataMeta(defn)
      val codecMeta = codecs.flatMap(_.codecMeta(defn, name).map(_.member))

      defn.defn match {
        case dto: Typedef.Dto =>
          val contractFieldNames = collectContractFieldNames(dto.contracts)
          val params = dto.fields.map {
            f =>
              val t      = trans.asKtNullableRef(f.tpe, domain, evo)
              val prefix = if (contractFieldNames.contains(f.name.name)) "override val" else "val"
              q"$prefix ${f.name.name}: $t"
          }
          val paramsList      = if (params.nonEmpty) params.join(",\n") else q""
          val contractParents = dto.contracts.map(c => trans.toKtTypeRefKeepForeigns(c, domain, evo))
          val (adtParent, adtMarker) = dto.id.owner match {
            case Owner.Adt(id) => (Some(trans.toKtTypeRefKeepForeigns(id, domain, evo)), Seq(iBaboonAdtMemberMeta))
            case _             => (None, Seq.empty)
          }
          val interfaceParents = (adtMarker ++ contractParents :+ genMarker).distinct

          val parentsList = adtParent match {
            case Some(adtType) =>
              if (interfaceParents.nonEmpty) q" : $adtType(), ${interfaceParents.map(t => q"$t").join(", ")}"
              else q" : $adtType()"
            case None =>
              if (interfaceParents.nonEmpty) q" : ${interfaceParents.map(t => q"$t").join(", ")}"
              else q""
          }

          val objectMetaFields = mainMeta.map(_.valueField) ++ codecMeta
          val classMetaFields  = mainMeta.map(mt => q"override ${mt.refValueField}")

          val hasFields    = params.nonEmpty
          val classKeyword = if (hasFields) "data class" else "class"

          val paramsBlock = if (hasFields) {
            q"""(
               |  ${paramsList.shift(2).trim}
               |)""".stripMargin
          } else q"" // no-arg class

          val emptyClassMethods = if (!hasFields) {
            q"""
               |override fun equals(other: Any?): Boolean = other is ${name.asName}
               |override fun hashCode(): Int = ${name.asName.hashCode.toString}
               |override fun toString(): String = "${name.asName}()"
               |""".stripMargin
          } else q""

          DefnRepr(
            q"""$classKeyword ${name.asName}$paramsBlock$parentsList {
               |  ${classMetaFields.joinN().shift(2).trim}
               |  ${emptyClassMethods.shift(2).trim}
               |
               |  companion object {
               |    ${objectMetaFields.joinN().shift(4).trim}
               |  }
               |}""".stripMargin,
            Nil,
          )

        case e: Typedef.Enum =>
          val cases = e.members.map {
            m =>
              val obj = m.name.capitalize
              q"$obj"
          }.toList

          val parseCases = e.members.map {
            m =>
              val obj = m.name.capitalize
              q"\"$obj\" to $obj"
          }.toList

          DefnRepr(
            q"""enum class ${name.asName} {
               |  ${cases.join(",\n").shift(2).trim};
               |
               |  companion object : $baboonEnum<${name.asName}> {
               |    private val byName = mapOf(
               |      ${parseCases.join(",\n").shift(6).trim}
               |    )
               |    override fun parse(s: String): ${name.asName}? = byName[s]
               |    override fun all(): List<${name.asName}> = entries.toList()
               |  }
               |}""".stripMargin,
            Nil,
          )

        case adt: Typedef.Adt =>
          val contractParents = adt.contracts.map(c => trans.toKtTypeRefKeepForeigns(c, domain, evo))
          val parents         = (contractParents :+ genMarker).distinct
          val parentsList     = if (parents.nonEmpty) q" : ${parents.map(t => q"$t").join(", ")}" else q""

          val memberTrees = adt.members.map {
            mid =>
              domain.defs.meta.nodes(mid) match {
                case mdefn: DomainMember.User => makeFullRepr(mdefn, inPkg = false)
                case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
              }
          }

          val objectMetaFields = mainMeta.map(_.valueField) ++ codecMeta
          val classMetaFields  = mainMeta.map(mt => q"override ${mt.refValueField}")

          DefnRepr(
            q"""sealed class ${name.asName}$parentsList {
               |  ${classMetaFields.joinN().shift(2).trim}
               |
               |  ${memberTrees.map(_.defn).toList.joinNN().shift(2).trim}
               |
               |  companion object {
               |    ${objectMetaFields.joinN().shift(4).trim}
               |  }
               |}""".stripMargin,
            Nil,
          )

        case contract: Typedef.Contract =>
          val methods = contract.fields.map {
            f =>
              val t = trans.asKtNullableRef(f.tpe, domain, evo)
              q"val ${f.name.name}: $t"
          }
          val contractParents = contract.contracts.map(c => trans.toKtTypeRefKeepForeigns(c, domain, evo))
          val parents         = (contractParents :+ genMarker).distinct
          val parentsList     = if (parents.nonEmpty) q" : ${parents.map(t => q"$t").join(", ")}" else q""
          val body            = if (methods.nonEmpty) methods.joinN() else q""
          DefnRepr(
            q"""interface ${name.asName}$parentsList {
               |  ${body.shift(2).trim}
               |}""".stripMargin,
            Nil,
          )

        case service: Typedef.Service =>
          val resolved    = ServiceResultResolver.resolve(domain, "kotlin", target.language.serviceResult, target.language.pragmas)
          val resolvedCtx = ServiceContextResolver.resolve(domain, "kotlin", target.language.serviceContext, target.language.pragmas)
          val ctxParam = resolvedCtx match {
            case ResolvedServiceContext.NoContext               => ""
            case ResolvedServiceContext.AbstractContext(tn, pn) => s"$pn: $tn, "
            case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$pn: $tn, "
          }
          val methods = service.methods.map {
            m =>
              val in  = trans.asKtRef(m.sig, domain, evo)
              val out = m.out.map(trans.asKtRef(_, domain, evo))
              val err = m.err.map(trans.asKtRef(_, domain, evo))
              val ktFqName: KtValue => String = {
                case t: KtValue.KtType     => if (t.predef) t.name else (t.pkg.parts :+ t.name).mkString(".")
                case t: KtValue.KtTypeName => t.name
              }
              val outStr = out.map(_.mapRender(ktFqName)).getOrElse("")
              val errStr = err.map(_.mapRender(ktFqName))
              val retStr = resolved.renderReturnType(outStr, errStr, "Unit")
              q"fun ${m.name.name}(${ctxParam}arg: $in): $retStr"
          }
          val typeParams = Seq(
            resolved.traitTypeParam,
            resolvedCtx match {
              case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
              case _                                             => None
            },
          ).flatten
          val traitTypeParam = if (typeParams.nonEmpty) typeParams.mkString("<", ", ", ">") else ""
          val body           = if (methods.nonEmpty) methods.joinN() else q""
          val resultImportHint = resolved.resultType
            .filter(_ != "Unit").map {
              rt =>
                val ktType = KtValue.KtType(baboonRuntimePkg, rt)
                q"private typealias _${service.id.name.name}SvcResult<L, R> = $ktType<L, R>\n\n"
            }.getOrElse(q"")
          DefnRepr(
            q"""${resultImportHint}interface ${name.asName}$traitTypeParam {
               |  ${body.shift(2).trim}
               |}""".stripMargin,
            Nil,
          )

        case _: Typedef.Foreign => DefnRepr(q"", Nil)
      }
    }

    private def collectContractFieldNames(contracts: List[TypeId.User]): Set[String] = {
      contracts.flatMap {
        contractId =>
          domain.defs.meta.nodes.get(contractId) match {
            case Some(DomainMember.User(_, ct: Typedef.Contract, _, _)) =>
              ct.fields.map(_.name.name) ++ collectContractFieldNames(ct.contracts)
            case _ => Seq.empty
          }
      }.toSet
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = ktFiles.basename(domain, evo)
      val fname = s"${defn.id.name.name}${suffix.getOrElse("")}.kt"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("_")}.$fname"
        case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}.$fname"
      }
    }
  }
}
