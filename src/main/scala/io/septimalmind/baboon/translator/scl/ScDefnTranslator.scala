package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.scl.ScValue.{ScPackageId, ScType}
import io.septimalmind.baboon.typer.TypeInfo
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait ScDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[ScDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[ScDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[ScDefnTranslator.Output]]
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

  class CSDefnTranslatorImpl[F[+_, +_]: Applicative2 /* This impl has no errors right now */ ](
    target: ScTarget,
    domain: Domain,
    evo: BaboonEvolution,
    types: TypeInfo,
    scFiles: ScFileTools,
    scTrees: ScTreeTools,
    trans: ScTypeTranslator,
  ) extends ScDefnTranslator[F] {
    import ScTypes.*

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[Output]] = {
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

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[Output]] = ???

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue.TranslationIssue], List[Output]] = ???

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

      val csTypeRef = trans.toScTypeRefDeref(defn.id, domain, evo)
      val srcRef    = trans.toScTypeRefNoDeref(defn.id, domain, evo)

      val (defnReprBase, extraRegs) =
        makeRepr(defn, csTypeRef, isLatestVersion)

      val codecTrees = Seq.empty[TextTree[ScValue]]
//        codecs.toList
//        .flatMap(t => t.translate(defn, csTypeRef, srcRef).toList)
//        .map(obsoletePrevious)

      val defnRepr = obsoletePrevious(defnReprBase)

      assert(defn.id.pkg == domain.id)

      val ns = srcRef.pkg.parts

      val allDefs = (defnRepr +: codecTrees).join("\n\n")
      val content = if (inNs) scTrees.inNs(ns.toSeq, allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef =>
          List.empty[(ScType, TextTree[ScValue])]
        case _ =>
          val codecsReg = Seq.empty[TextTree[ScType]]
//          codecs.toList
//            .sortBy(_.getClass.getName)
//            .map(codec => q"new Lazy<$iBaboonCodecData>(() => ${codec.codecName(srcRef).copy(fq = true)}.Instance)")
          val reg =
            (List(q"""\"${defn.id.toString}\"""") ++ codecsReg).join(", ")
          List(csTypeRef -> reg)
      }

      val allRegs = reg ++ extraRegs

      (content, allRegs)
    }

    private def makeRepr(defn: DomainMember.User, name: ScValue.ScType, isLatestVersion: Boolean): (TextTree[ScValue], List[(ScType, TextTree[ScValue])]) = {
      ???
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = scFiles.basename(domain, evo)
      val fname = s"${defn.id.name.name.capitalize}${suffix.getOrElse("")}.scala"
      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString(".")}.$fname"
        case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}.$fname"
      }
    }
  }

}
