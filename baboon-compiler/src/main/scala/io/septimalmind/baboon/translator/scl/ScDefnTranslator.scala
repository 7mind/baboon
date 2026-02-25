package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.scl.ScValue.ScType
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait ScDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[ScDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[ScDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[ScDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[ScDefnTranslator.Output]]
}

object ScDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: ScType,
    tpeKeepForeigns: ScType,
    tpeId: TextTree[ScValue],
    trees: Map[String, TextTree[ScValue]],
  )

  final case class DefnRepr(
    defn: TextTree[ScValue],
    codecs: List[CodecReg],
  )

  final case class Output(
    path: String,
    tree: TextTree[ScValue],
    pkg: ScValue.ScPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[ScValue]])] = Nil,
  )

  class ScDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: ScTarget,
    domain: Domain,
    evo: BaboonEvolution,
    scFiles: ScFileTools,
    scTrees: ScTreeTools,
    trans: ScTypeTranslator,
    codecs: Set[ScCodecTranslator],
    codecTests: ScCodecTestsTranslator,
    codecsFixture: ScCodecFixtureTranslator,
    wiringTranslator: ScServiceWiringTranslator,
    scDomainTreeTools: ScDomainTreeTools,
  ) extends ScDefnTranslator[F] {
    import ScTypes.*

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr = makeFullRepr(defn, inNs = true)

      val registrations = codecs.toList.map(codec => codec.id -> repr.codecs.flatMap(reg => reg.trees.get(codec.id).map(expr => q"${reg.tpeId}, $expr")))

      val mainOutput = Output(
        getOutputPath(defn),
        repr.defn,
        trans.toScPkg(domain.id, domain.version, evo),
        CompilerProduct.Definition,
        codecReg = registrations,
      )

      val wiringOutput = wiringTranslator
        .translate(defn).map {
          wiringTree =>
            val pkg     = trans.toScPkg(domain.id, domain.version, evo)
            val wrapped = scTrees.inNs(pkg.parts.toSeq, wiringTree)
            Output(
              getOutputPath(defn, suffix = Some("_Wiring")),
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
        fixtureTreeWithNs =>
          Output(
            getOutputPath(defn, suffix = Some("_Fixture")),
            fixtureTreeWithNs,
            trans.toScPkg(domain.id, domain.version, evo),
            CompilerProduct.Fixture,
          )
      }

      F.pure(fixtureTreeOut.toList)
    }
    private def makeFixtureRepr(defn: DomainMember.User): Option[TextTree[ScValue]] = {
      val srcRef = trans.toScTypeRefKeepForeigns(defn.id, domain, evo)
      val ns     = srcRef.pkg.parts

      val fixtureTree       = codecsFixture.translate(defn)
      val fixtureTreeWithNs = fixtureTree.map(t => scTrees.inNs(ns.toSeq, t))

      fixtureTreeWithNs
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTest(defn)
      }
    }
    private def doTranslateTest(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val codecTestOut = makeTestRepr(defn).map {
        codecTestWithNS =>
          Output(
            getOutputPath(defn, suffix = Some("_Tests")),
            codecTestWithNS,
            trans.toScPkg(domain.id, domain.version, evo),
            CompilerProduct.Test,
          )
      }

      F.pure(codecTestOut.toList)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map {
        tree =>
          val pkg     = trans.toScPkg(domain.id, domain.version, evo)
          val wrapped = scTrees.inNs(pkg.parts.toSeq, tree)
          val fbase   = scFiles.basename(domain, evo)
          Output(
            s"$fbase/BaboonServiceRt.scala",
            wrapped,
            pkg,
            CompilerProduct.Definition,
          )
      }.toList
      F.pure(result)
    }

    private def makeTestRepr(defn: DomainMember.User): Option[TextTree[ScValue]] = {
      val csTypeRef = trans.asScType(defn.id, domain, evo)
      val srcRef    = trans.toScTypeRefKeepForeigns(defn.id, domain, evo)
      val ns        = srcRef.pkg.parts

      val testTree       = codecTests.translate(defn, csTypeRef, srcRef)
      val testTreeWithNs = testTree.map(t => scTrees.inNs(ns.toSeq, t))

      testTreeWithNs
    }

    private def makeFullRepr(
      defn: DomainMember.User,
      inNs: Boolean,
    ): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      def obsoletePrevious(tree: TextTree[ScValue]): TextTree[ScValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""@${ScTypes.deprecated}("Version ${domain.version.v.toString} is deprecated, you should migrate to ${evo.latest.v.toString}")
             |$tree""".stripMargin
        }
      }

      val scTypeRef = trans.asScType(defn.id, domain, evo)
      val srcRef    = trans.toScTypeRefKeepForeigns(defn.id, domain, evo)

      val repr = makeRepr(defn, scTypeRef, isLatestVersion)

      val codecTrees =
        codecs.toList
          .flatMap(t => t.translate(defn, scTypeRef, srcRef).toList)
          .map(obsoletePrevious)

      val defnRepr = obsoletePrevious(repr.defn)

      assert(defn.id.pkg == domain.id)

      val ns = srcRef.pkg.parts

      val allDefs = (defnRepr +: codecTrees).joinNN()
      val content = if (inNs) scTrees.inNs(ns.toSeq, allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => Nil
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap(
              codec =>
                if (codec.isActive(d.id)) List(codec.id -> q"$baboonLazy(${codec.codecName(srcRef)})")
                else Nil
            )
          List(CodecReg(defn.id, scTypeRef, srcRef, q"\"${defn.id.toString}\"", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      DefnRepr(content, allRegs)
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: ScValue.ScType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = scDomainTreeTools.makeDataMeta(defn)
      val codecMeta = codecs.flatMap(_.codecMeta(defn, name).map(_.member))

      defn.defn match {
        case dto: Typedef.Dto =>
          val params = dto.fields.map {
            f =>
              val t = trans.asScRef(f.tpe, domain, evo)
              q"${f.name.name}: $t"
          }
          val paramsList      = if (params.nonEmpty) params.join(",\n") else q""
          val contractParents = dto.contracts.map(c => trans.toScTypeRefKeepForeigns(c, domain, evo))
          val adtParents = dto.id.owner match {
            case Owner.Adt(id) => Seq(trans.toScTypeRefKeepForeigns(id, domain, evo), iBaboonAdtMemberMeta)
            case _             => Seq.empty
          }
          val parents          = adtParents ++ contractParents :+ genMarker
          val extendsClauseDto = if (parents.nonEmpty) q" extends ${parents.map(t => q"$t").join(" with ")}" else q""

          val objectMetaFields = mainMeta.map(_.valueField) ++ codecMeta
          val classMetaFields  = mainMeta.map(mt => q"override ${mt.refValueField}")

          DefnRepr(
            q"""final case class ${name.name}(
               |  ${paramsList.shift(2).trim}
               |)$extendsClauseDto {
               |  ${classMetaFields.joinN().shift(2).trim}
               |}
               |
               |object ${name.name} {
               |  ${objectMetaFields.joinN().shift(2).trim}
               |}""".stripMargin,
            Nil,
          )

        case e: Typedef.Enum =>
          val traitTree = q"sealed trait ${name.name}"

          val cases = e.members.map {
            m =>
              val obj = m.name.capitalize
              q"case object $obj extends ${name.name}"
          }.toList

          val parseCases = e.members.map {
            m =>
              val obj = m.name.capitalize
              q"case \"$obj\" => Some($obj)"
          }.toList

          val names = e.members.map {
            m =>
              val obj = m.name.capitalize
              q"$obj"
          }.toList

          val companion =
            q"""object ${name.name} extends $baboonEnum[${name.name}] {
               |  ${cases.joinN().shift(2).trim}
               |  
               |  def parse(s: $scString): $scOption[${name.name}] = {
               |    s match {
               |      ${parseCases.joinN().shift(6).trim}
               |      case _ => None
               |    }
               |  }
               |  
               |  def all: $scList[${name.name}] = $scList(
               |    ${names.join(",\n").shift(4).trim}
               |  ) 
               |}""".stripMargin
          DefnRepr(Seq(traitTree, companion).joinNN(), Nil)

        case adt: Typedef.Adt =>
          val parents          = adt.contracts.map(c => trans.toScTypeRefKeepForeigns(c, domain, evo)) :+ genMarker
          val extendsClauseAdt = if (parents.nonEmpty) q" extends ${parents.map(t => q"$t").join(" with ")}" else q""
          val sealedTrait      = q"""sealed trait ${name.name}$extendsClauseAdt""".stripMargin
          val memberTrees = adt.members.map {
            mid =>
              domain.defs.meta.nodes(mid) match {
                case mdefn: DomainMember.User => makeFullRepr(mdefn, inNs = false)
                case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
              }
          }

          val objectMetaFields = mainMeta.map(_.valueField) ++ codecMeta
          val classMetaFields  = mainMeta.map(mt => q"override ${mt.refValueField}")

          DefnRepr(
            q"""$sealedTrait {
               |  ${classMetaFields.joinN().shift(2).trim}
               |}
               |
               |object ${name.name} {
               |  ${memberTrees.map(_.defn).toList.joinNN().shift(2).trim}
               |  ${objectMetaFields.joinN().shift(2).trim}
               |}""".stripMargin,
            Nil,
          )

        case contract: Typedef.Contract =>
          val methods = contract.fields.map {
            f =>
              val t = trans.asScRef(f.tpe, domain, evo)
              q"def ${f.name.name}: $t"
          }
          val parents       = contract.contracts.map(c => trans.toScTypeRefKeepForeigns(c, domain, evo)) :+ genMarker
          val extendsClause = if (parents.nonEmpty) q" extends ${parents.map(t => q"$t").join(" with ")}" else q""
          val body          = if (methods.nonEmpty) methods.joinN() else q""
          DefnRepr(
            q"""trait ${name.name}$extendsClause {
               |    ${body.shift(4).trim}
               |}""".stripMargin,
            Nil,
          )

        case service: Typedef.Service =>
          val resolved    = ServiceResultResolver.resolve(domain, "scala", target.language.serviceResult, target.language.pragmas)
          val resolvedCtx = ServiceContextResolver.resolve(domain, "scala", target.language.serviceContext, target.language.pragmas)
          val ctxParam = resolvedCtx match {
            case ResolvedServiceContext.NoContext               => ""
            case ResolvedServiceContext.AbstractContext(tn, pn) => s"$pn: $tn, "
            case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$pn: $tn, "
          }
          val methods = service.methods.map {
            m =>
              val in  = trans.asScRef(m.sig, domain, evo)
              val out = m.out.map(trans.asScRef(_, domain, evo))
              val err = m.err.map(trans.asScRef(_, domain, evo))
              val scFqName: ScValue => String = {
                case t: ScValue.ScType     => if (t.predef) t.name else (t.pkg.parts :+ t.name).mkString(".")
              }
              val outStr = out.map(_.mapRender(scFqName)).getOrElse("")
              val errStr = err.map(_.mapRender(scFqName))
              val retStr = resolved.renderReturnType(outStr, errStr, "Unit")
              q"def ${m.name.name}(${ctxParam}arg: $in): $retStr"
          }
          val typeParams = Seq(
            resolved.traitTypeParam,
            resolvedCtx match {
              case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
              case _                                             => None
            },
          ).flatten
          val traitTypeParam = if (typeParams.nonEmpty) typeParams.mkString("[", ", ", "]") else ""
          val body           = if (methods.nonEmpty) methods.joinN() else q""
          DefnRepr(
            q"""trait ${name.name}$traitTypeParam {
               |    ${body.shift(4).trim}
               |}""".stripMargin,
            Nil,
          )

        case _: Typedef.Foreign => DefnRepr(q"", Nil)

      }
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = scFiles.basename(domain, evo)
      val fname = s"${defn.id.name.name}${suffix.getOrElse("")}.scala"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("_")}.$fname"
        case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}.$fname"
      }
    }
  }
}
