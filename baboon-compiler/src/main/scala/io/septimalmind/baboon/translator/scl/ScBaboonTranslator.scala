package io.septimalmind.baboon.translator.scl

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources, scl}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class ScBaboonTranslator[F[+_, +_]: Error2](
  trans: ScTypeTranslator,
  convTransFac: ScConversionTranslator.Factory[F],
  defnTranslator: Subcontext[ScDefnTranslator[F]],
  target: ScTarget,
  scTreeTools: ScTreeTools,
  scFiles: ScFileTools,
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime()
      fixture    <- sharedFixture()
      rendered = (translated ++ runtime ++ fixture).map {
        o =>
          val content = renderTree(o)
          (o.path, OutputFile(content, o.product))
      }
      unique <- F.fromEither(rendered.toUniqueMap(c => BaboonIssue.of(TranslationIssue.NonUniqueOutputFiles(c))))
    } yield {
      Sources(unique)
    }
  }

  private def translateFamily(
    family: BaboonFamily
  ): Out[List[ScDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[ScDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: (DomainMember.User) => F[NEList[BaboonIssue], List[ScDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[ScDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[ScDefnTranslator.Output]] = {
    val evo = lineage.evolution
    defnTranslator.provide(domain).provide(evo).produce().use {
      defnTranslator =>
        for {
          defnSources     <- translateProduct(domain, CompilerProduct.Definition, defnTranslator.translate)
          fixturesSources <- translateProduct(domain, CompilerProduct.Fixture, defnTranslator.translateFixtures)
          testsSources    <- translateProduct(domain, CompilerProduct.Test, defnTranslator.translateTests)

          conversionSources <- {
            if (target.output.products.contains(CompilerProduct.Conversion)) {
              val evosToCurrent = evo.diffs.keySet.filter(_.to == domain.version)
              generateConversions(domain, lineage, evosToCurrent, defnSources)
            } else {
              F.pure(List.empty)
            }
          }
          meta <- {
            if (target.language.writeEvolutionDict) {
              generateMeta(domain, lineage)
            } else {
              F.pure(List.empty)
            }
          }
        } yield {
          defnSources ++
          conversionSources ++
          fixturesSources ++
          testsSources ++
          meta
        }
    }
  }

  private def generateMeta(domain: Domain, lineage: BaboonLineage): Out[List[ScDefnTranslator.Output]] = {
    val basename = scFiles.basename(domain, lineage.evolution)
    val pkg      = trans.toScPkg(domain.id, domain.version, lineage.evolution)

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q"""unmodified.put("${tid.toString}", $scList(${version.sameIn.map(_.version).map(s => q"\"$s\"").toList.join(", ")}))"""
      }

    scala.collection.mutable.Map.empty[String, String]
    val metaTree =
      q"""object BaboonMetadata extends $baboonMeta {
         |  private val unmodified = ${scMutMap.fullyQualified}.empty[$scString, $scList[$scString]]
         |  
         |  ${entries.join("\n").shift(2).trim}
         |
         |  def unmodifiedSince(typeIdString: $scString): $scList[$scString] = {
         |    unmodified(typeIdString);
         |  }
         |}""".stripMargin

    val metaSource = Seq(metaTree).join("\n\n")
    val meta       = scTreeTools.inNs(pkg.parts.toSeq, metaSource)
    val metaOutput = ScDefnTranslator.Output(s"$basename/BaboonMetadata.scala", meta, pkg, CompilerProduct.Definition)

    F.pure(List(metaOutput))
  }

  private def sharedFixture(): Out[List[ScDefnTranslator.Output]] = {
    F.pure(List.empty)

  }

  private def renderTree(o: ScDefnTranslator.Output): String = {
    // TODO: better representation
    // TODO: omit predef
    val usedTypes = o.tree.values.collect { case t: ScValue.ScType => t }.distinct
      .filterNot(_.fq)
      .filterNot(_.pkg == o.pkg)
      .filterNot(t => t.pkg.parts.startsWith(o.pkg.parts))
      .sortBy(_.toString)

//    println((o.path, o.pkg))
//    println(usedTypes.map(t => (t, t.fq)))
    val imports = usedTypes.toSeq.map {
      p => q"import ${p.pkg.parts.mkString(".")}.${p.name}"
    }.join("\n")

    val full = if (o.doNotModify) {
      o.tree
    } else {
      Seq(
        Seq(imports),
        Seq(o.tree),
      ).flatten.join("\n\n")
    }

    full.mapRender {
      case t: ScValue.ScTypeName =>
        t.name

      case t: ScValue.ScType if !t.fq =>
        if (o.pkg == t.pkg || !t.pkg.parts.startsWith(o.pkg.parts)) {
          t.name
        } else {
          (t.pkg.parts :+ t.name).mkString(".")
        }

      case t: ScValue.ScType =>
        (t.pkg.parts :+ t.name).mkString(".")
    }
  }

  private def sharedRuntime(): Out[List[scl.ScDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Runtime)) {

      val sharedOutput = ScDefnTranslator.Output(
        s"BaboonRuntimeShared.scala",
        TextTree.text(
          IzResources
            .readAsString("baboon-runtime/scala/BaboonRuntimeShared.scala").get
            // TODO: hack, we always force escape processing in text tree
            .replace("""[\\s:-]""", """[\\\\s:-]""")
        ),
        ScTypes.baboonRuntimePkg,
        CompilerProduct.Runtime,
        doNotModify = true,
      )
      F.pure(List(sharedOutput))
    } else {
      F.pure(List.empty)
    }
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
    defnOut: List[ScDefnTranslator.Output],
  ): Out[List[scl.ScDefnTranslator.Output]] = {
    val pkg = trans.toScPkg(domain.id, domain.version, lineage.evolution)

    for {
      convs <-
        F.flatSequenceAccumErrors {
          lineage.evolution.rules
            .filter(kv => toCurrent.contains(kv._1))
            .map {
              case (srcVer, rules) =>
                convTransFac(
                  pkg    = pkg,
                  srcDom = lineage.versions(srcVer.from),
                  domain = domain,
                  rules  = rules,
                  evo    = lineage.evolution,
                ).makeConvs()
            }
        }
    } yield {
      val conversionRegs = convs.flatMap(_.reg.iterator.toSeq).toSeq
      val missing        = convs.flatMap(_.missing.iterator.toSeq).toSeq

      val converter =
        q"""trait RequiredConversions {
           |    ${missing.join("\n").shift(4).trim}
           |}
           |
           |class BaboonConversions(required: RequiredConversions) extends $abstractBaboonConversions {
           |    ${conversionRegs.join("\n").shift(4).trim}
           |
           |    override def versionsFrom: $scList[$scString] = $scList(${toCurrent.map(_.from.version).map(v => s"\"$v\"").mkString(", ")})
           |    override def versionTo: $scString = "${domain.version.version}"
           |}""".stripMargin

      // Scala codecs definitions (stub syntax)
      val codecs =
        q"""object BaboonCodecs extends $abstractBaboonCodecs {
           |    // register codecs
           |    ${defnOut.flatMap(_.codecReg).join("\n").shift(4).trim}
           |}""".stripMargin

      val basename = scFiles.basename(domain, lineage.evolution)

      val runtimeSource = Seq(converter, codecs).join("\n\n")
      val runtime       = scTreeTools.inNs(pkg.parts.toSeq, runtimeSource)
      val runtimeOutput = ScDefnTranslator.Output(
        s"$basename/BaboonRuntime.scala",
        runtime,
        pkg,
        CompilerProduct.Conversion,
      )

      val convertersOutput = convs.map {
        conv =>
          ScDefnTranslator.Output(
            s"$basename/${conv.fname}",
            conv.conv,
            pkg,
            CompilerProduct.Conversion,
          )
      }

      List(runtimeOutput) ++ convertersOutput
    }
  }
}

object ScBaboonTranslator {
  case class RenderedConversion(
    fname: String,
    conv: TextTree[ScValue],
    reg: Option[TextTree[ScValue]],
    missing: Option[TextTree[ScValue]],
  )
}
