package io.septimalmind.baboon.translator.kotlin

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.KtTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.kotlin.KtTypes.*
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class KtBaboonTranslator[F[+_, +_]: Error2](
  trans: KtTypeTranslator,
  convTransFac: KtConversionTranslator.Factory[F],
  defnTranslator: Subcontext[KtDefnTranslator[F]],
  target: KtTarget,
  ktTreeTools: KtTreeTools,
  ktFiles: KtFileTools,
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
  ): Out[List[KtDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[KtDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: DomainMember.User => F[NEList[BaboonIssue], List[KtDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[KtDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[KtDefnTranslator.Output]] = {
    val evo = lineage.evolution
    defnTranslator.provide(domain).provide(evo).produce().use {
      defnTranslator =>
        for {
          defnSources     <- translateProduct(domain, CompilerProduct.Definition, defnTranslator.translate)
          fixturesSources <- translateProduct(domain, CompilerProduct.Fixture, defnTranslator.translateFixtures)
          testsSources    <- translateProduct(domain, CompilerProduct.Test, defnTranslator.translateTests)
          serviceRt       <- defnTranslator.translateServiceRt()

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
          serviceRt ++
          meta
        }
    }
  }

  private def generateMeta(domain: Domain, lineage: BaboonLineage): Out[List[KtDefnTranslator.Output]] = {
    val basename = ktFiles.basename(domain, lineage.evolution)
    val pkg      = trans.toKtPkg(domain.id, domain.version, lineage.evolution)

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q"""unmodified["${tid.toString}"] = listOf(${version.sameIn.map(_.v.toString).map(s => q"\"$s\"").toList.join(", ")})"""
      }

    val metaTree =
      q"""object BaboonMetadata : $baboonMeta {
         |  private val unmodified = mutableMapOf<String, List<String>>()
         |
         |  init {
         |    ${entries.joinN().shift(4).trim}
         |  }
         |
         |  override fun sameInVersions(typeId: String): List<String> {
         |    return unmodified[typeId] ?: emptyList()
         |  }
         |}""".stripMargin

    val meta       = ktTreeTools.inPkg(pkg.parts.toSeq, metaTree)
    val metaOutput = KtDefnTranslator.Output(s"$basename/BaboonMetadata.kt", meta, pkg, CompilerProduct.Definition)

    F.pure(List(metaOutput))
  }

  private def sharedFixture(): Out[List[KtDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      F.pure(
        List(
          KtDefnTranslator.Output(
            "BaboonFixtureShared.kt",
            TextTree.text(IzResources.readAsString("baboon-runtime/kotlin/BaboonFixtureShared.kt").get),
            KtTypes.baboonFixturePkg,
            CompilerProduct.FixtureRuntime,
            doNotModify = true,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def renderTree(o: KtDefnTranslator.Output): String = {
    val rendered = o.tree.mapRender {
      case t: KtValue.KtTypeName => t.name
      case t: KtValue.KtType if !t.fq =>
        if (o.pkg == t.pkg || !t.pkg.parts.startsWith(o.pkg.parts)) {
          t.name
        } else {
          (t.pkg.parts :+ t.name).mkString(".")
        }
      case t: KtValue.KtType => (t.pkg.parts :+ t.name).mkString(".")
    }

    if (o.doNotModify) {
      rendered
    } else {
      val usedTypes = o.tree.values.collect { case t: KtValue.KtType => t }.distinct
        .filterNot(_.predef)
        .filterNot(_.fq)
        .filterNot(_.pkg == o.pkg)
        .filterNot(t => t.pkg.parts.startsWith(o.pkg.parts))
        .sortBy(_.toString)

      val kotlinxJsonExtImports = if (usedTypes.exists(_.pkg == kotlinxJsonPkg)) {
        List(
          "import kotlinx.serialization.json.jsonObject",
          "import kotlinx.serialization.json.jsonPrimitive",
          "import kotlinx.serialization.json.jsonArray",
          "import kotlinx.serialization.json.boolean",
          "import kotlinx.serialization.json.int",
          "import kotlinx.serialization.json.long",
          "import kotlinx.serialization.json.float",
          "import kotlinx.serialization.json.double",
        )
      } else Nil

      val importLines = usedTypes.map {
        p =>
          val importName = if (p.name.contains('.')) p.name.split('.').head else p.name
          s"import ${p.pkg.parts.mkString(".")}.$importName"
      }.distinct ++ kotlinxJsonExtImports

      if (importLines.isEmpty) {
        rendered
      } else {
        val importsBlock = importLines.mkString("\n")
        val lines        = rendered.split("\n", 2)
        if (lines.length == 2 && lines(0).startsWith("package ")) {
          s"${lines(0)}\n\n$importsBlock\n\n${lines(1).stripLeading()}"
        } else {
          s"$importsBlock\n\n$rendered"
        }
      }
    }
  }

  private def sharedRuntime(): Out[List[KtDefnTranslator.Output]] = {
    def rt(path: String, resource: String, preprocessResource: String => String = identity): KtDefnTranslator.Output = {
      KtDefnTranslator.Output(
        path,
        TextTree.text(IzResources.readAsString(resource).map(preprocessResource).get),
        KtTypes.baboonRuntimePkg,
        CompilerProduct.Runtime,
        doNotModify = true,
      )
    }
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          rt("BaboonByteString.kt", "baboon-runtime/kotlin/BaboonByteString.kt"),
          rt("BaboonCodecs.kt", "baboon-runtime/kotlin/BaboonCodecs.kt"),
          rt("BaboonCodecsFacade.kt", "baboon-runtime/kotlin/BaboonCodecsFacade.kt"),
          rt("BaboonConversions.kt", "baboon-runtime/kotlin/BaboonConversions.kt"),
          rt("BaboonEither.kt", "baboon-runtime/kotlin/BaboonEither.kt"),
          rt("BaboonExceptions.kt", "baboon-runtime/kotlin/BaboonExceptions.kt"),
          rt("BaboonRuntimeShared.kt", "baboon-runtime/kotlin/BaboonRuntimeShared.kt"),
          rt("BaboonServiceWiring.kt", "baboon-runtime/kotlin/BaboonServiceWiring.kt"),
          rt("BaboonTools.kt", "baboon-runtime/kotlin/BaboonTools.kt"),
          rt("BaboonTimeFormats.kt", "baboon-runtime/kotlin/BaboonTimeFormats.kt"),
        )
      )
    } else {
      F.pure(List.empty)
    }
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
    defnOut: List[KtDefnTranslator.Output],
  ): Out[List[KtDefnTranslator.Output]] = {
    val pkg = trans.toKtPkg(domain.id, domain.version, lineage.evolution)

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
                ).makeConvs
            }
        }
    } yield {
      val conversionRegs = convs.flatMap(_.reg.iterator.toSeq).toSeq
      val missing        = convs.flatMap(_.missing.iterator.toSeq).toSeq

      val converter =
        q"""interface RequiredConversions {
           |    ${missing.joinN().shift(4).trim}
           |}
           |
           |class BaboonConversions(private val required: RequiredConversions) : $baboonAbstractConversions() {
           |    init {
           |      ${conversionRegs.joinN().shift(6).trim}
           |    }
           |
           |    override val versionsFrom: List<String> = listOf(${toCurrent.map(_.from.v.toString).map(v => s"\"$v\"").mkString(", ")})
           |    override val versionTo: String = "${domain.version.v.toString}"
           |}""".stripMargin

      import izumi.fundamentals.collections.IzCollections.*
      val regsMap = defnOut.flatMap(_.codecReg).toMultimap.view.mapValues(_.flatten).toMap

      val codecs = regsMap.map {
        case (codecId, regs) =>
          q"""object BaboonCodecs${codecId.capitalize} : ${abstractBaboonCodec(codecId)}() {
             |  init {
             |    ${regs.toList.map(c => q"register($c)").joinN().shift(4).trim}
             |  }
             |}""".stripMargin
      }.toList.joinNN()

      val basename = ktFiles.basename(domain, lineage.evolution)

      val runtimeSource = Seq(converter, codecs).joinNN()
      val runtime       = ktTreeTools.inPkg(pkg.parts.toSeq, runtimeSource)
      val runtimeOutput = KtDefnTranslator.Output(
        s"$basename/BaboonRuntime.kt",
        runtime,
        pkg,
        CompilerProduct.Conversion,
      )

      val convertersOutput = convs.map {
        conv =>
          KtDefnTranslator.Output(
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

object KtBaboonTranslator {
  case class RenderedConversion(
    fname: String,
    conv: TextTree[KtValue],
    reg: Option[TextTree[KtValue]],
    missing: Option[TextTree[KtValue]],
  )
}
