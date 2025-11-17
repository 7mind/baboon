package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
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
}

object ScDefnTranslator {
  case class Output(
    path: String,
    tree: TextTree[ScValue],
    pkg: ScValue.ScPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                = false,
    codecReg: Option[TextTree[ScValue]] = None,
  )

  class ScDefnTranslatorImpl[F[+_, +_]: Applicative2](
    domain: Domain,
    evo: BaboonEvolution,
    scFiles: ScFileTools,
    scTrees: ScTreeTools,
    trans: ScTypeTranslator,
    codecs: Set[ScCodecTranslator],
    codecTests: ScCodecTestsTranslator,
    codecsFixture: ScCodecFixtureTranslator,
  ) extends ScDefnTranslator[F] {
    import ScTypes.*

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val (content, reg) = makeFullRepr(defn, inNs = true)

      val registrations = Option(reg.map { case (_, reg) => q"register(new $baboonTypeCodecs($reg))" }).filterNot(_.isEmpty).map(_.join("\n"))

      F.pure(
        List(
          Output(
            getOutputPath(defn),
            content,
            trans.toScPkg(domain.id, domain.version, evo),
            CompilerProduct.Definition,
            codecReg = registrations,
          )
        )
      )
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
    ): (TextTree[ScValue], List[(ScType, TextTree[ScValue])]) = {
      val isLatestVersion = domain.version == evo.latest

      def obsoletePrevious(tree: TextTree[ScValue]): TextTree[ScValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""@${ScTypes.deprecated}("Version ${domain.version.version} is deprecated, you should migrate to ${evo.latest.version}")
             |$tree""".stripMargin
        }
      }

      val csTypeRef = trans.asScType(defn.id, domain, evo)
      val srcRef    = trans.toScTypeRefKeepForeigns(defn.id, domain, evo)

      val (defnReprBase, extraRegs) =
        makeRepr(defn, csTypeRef, isLatestVersion)

      val codecTrees =
        codecs.toList
          .flatMap(t => t.translate(defn, csTypeRef, srcRef).toList)
          .map(obsoletePrevious)

      val defnRepr = obsoletePrevious(defnReprBase)

      assert(defn.id.pkg == domain.id)

      val ns = srcRef.pkg.parts

      val allDefs = (defnRepr +: codecTrees).join("\n\n")
      val content = if (inNs) scTrees.inNs(ns.toSeq, allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef =>
          List.empty[(ScType, TextTree[ScValue])]
        case _ =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .map(codec => q"${codec.codecName(srcRef).copy(fq = true)}")
          val reg =
            (List(q"""\"${defn.id.toString}\"""") ++ codecsReg).join(", ")
          List(csTypeRef -> reg)
      }

      val allRegs = reg ++ extraRegs

      (content, allRegs)
    }

    private def makeRepr(defn: DomainMember.User, name: ScValue.ScType, isLatestVersion: Boolean): (TextTree[ScValue], List[(ScType, TextTree[ScValue])]) = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = List.empty[TextTree[ScValue]] // csDomTrees.makeMeta(defn, isCodec = false)
      val codecMeta = codecs.map(_.codecMeta(defn, name).member)
      // TODO:
      val meta = mainMeta ++ codecMeta

      val fixtureRef = codecsFixture.fixtureTpe(defn).map(id => q"implicit def fixture: $baboonFixture[${name.asName}] = $id").getOrElse(q"")

      val tree: TextTree[ScValue] = defn.defn match {
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

          q"""final case class ${name.asName}(
             |  ${paramsList.shift(2).trim}
             |)$extendsClauseDto
             |
             |object ${name.asName} {
             |  $fixtureRef
             |}""".stripMargin

        case e: Typedef.Enum =>
          val traitTree = q"sealed trait ${name.asName}"

          val cases = e.members.map {
            m =>
              val obj = m.name.capitalize
              q"case object $obj extends ${name.asName}"
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

          val companion = q"""object ${name.asName} extends $baboonEnum[${name.asName}] {
                             |  ${cases.join("\n").shift(2).trim}
                             |  
                             |  $fixtureRef
                             |
                             |  def parse(s: $scString): $scOption[${name.asName}] = {
                             |    s match {
                             |      ${parseCases.join("\n").shift(6).trim}
                             |      case _ => None
                             |    }
                             |  }
                             |  
                             |  def all: $scList[${name.asName}] = $scList(
                             |    ${names.join(",\n").shift(4).trim}
                             |  ) 
                             |}""".stripMargin
          Seq(traitTree, companion).join("\n\n")

        case adt: Typedef.Adt =>
          val parents          = adt.contracts.map(c => trans.toScTypeRefKeepForeigns(c, domain, evo)) :+ genMarker
          val extendsClauseAdt = if (parents.nonEmpty) q" extends ${parents.map(t => q"$t").join(" with ")}" else q""
          val sealedTrait      = q"""sealed trait ${name.asName}$extendsClauseAdt""".stripMargin
          val memberTrees = adt.members.map {
            mid =>
              domain.defs.meta.nodes(mid) match {
                case mdefn: DomainMember.User => makeFullRepr(mdefn, inNs = false)
                case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
              }
          }

          val adtFixtureRef = codecsFixture.fixtureTpe(defn).map(id => q"implicit def fixture: $baboonAdtFixture[${name.asName}] = $id").getOrElse(q"")

          val fullTree =
            q"""$sealedTrait
               |
               |object ${name.asName} {
               |  ${memberTrees.map(_._1).toList.join("\n\n").shift(2).trim}
               |  $adtFixtureRef
               |}""".stripMargin
//          val regs = memberTrees.flatMap(_._2)
          fullTree

        case contract: Typedef.Contract =>
          val methods = contract.fields.map {
            f =>
              val t = trans.asScRef(f.tpe, domain, evo)
              q"def ${f.name.name}: $t"
          }
          val parents       = contract.contracts.map(c => trans.toScTypeRefKeepForeigns(c, domain, evo)) :+ genMarker
          val extendsClause = if (parents.nonEmpty) q" extends ${parents.map(t => q"$t").join(" with ")}" else q""
          val body          = if (methods.nonEmpty) methods.join("\n") else q""
          q"""trait ${name.asName}$extendsClause {
             |    ${body.shift(4).trim}
             |}""".stripMargin

        case service: Typedef.Service =>
          val methods = service.methods.map {
            m =>
              val in  = trans.asScRef(m.sig, domain, evo)
              val out = m.out.map(trans.asScRef(_, domain, evo))
              val err = m.err.map(trans.asScRef(_, domain, evo))
              val ret = (out, err) match {
                case (Some(o), Some(e)) => q"$scEither[$e, $o]"
                case (None, Some(e))    => q"$scEither[$e, $scUnit]"
                case (Some(o), None)    => o
                case _                  => q"$scUnit"
              }
              q"def ${m.name.name}(arg: $in): $ret"
          }
          val body = if (methods.nonEmpty) methods.join("\n") else q""
          q"""trait ${name.asName} {
             |    ${body.shift(4).trim}
             |}""".stripMargin

        case _: Typedef.Foreign =>
          q""
      }
      (tree, List.empty)
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
