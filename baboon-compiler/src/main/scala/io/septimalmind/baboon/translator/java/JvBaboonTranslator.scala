package io.septimalmind.baboon.translator.java

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class JvBaboonTranslator[F[+_, +_]: Error2](
  trans: JvTypeTranslator,
  convTransFac: JvConversionTranslator.Factory[F],
  defnTranslator: Subcontext[JvDefnTranslator[F]],
  target: JvTarget,
  jvTreeTools: JvTreeTools,
  jvFiles: JvFileTools,
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
  ): Out[List[JvDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[JvDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: DomainMember.User => F[NEList[BaboonIssue], List[JvDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[JvDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[JvDefnTranslator.Output]] = {
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

  private def generateMeta(domain: Domain, lineage: BaboonLineage): Out[List[JvDefnTranslator.Output]] = {
    val basename = jvFiles.basename(domain, lineage.evolution)
    val pkg      = trans.toJvPkg(domain.id, domain.version, lineage.evolution)

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q"""unmodified.put("${tid.toString}", $jvList.of(${version.sameIn.map(_.v.toString).map(s => q"\"$s\"").toList.join(", ")}));"""
      }

    val metaTree =
      q"""public final class BaboonMetadata implements $baboonMeta {
         |  private static final $jvMap<String, $jvList<String>> unmodified = new java.util.HashMap<>();
         |
         |  static {
         |    ${entries.joinN().shift(4).trim}
         |  }
         |
         |  @Override
         |  public $jvList<String> sameInVersions(String typeId) {
         |    return unmodified.getOrDefault(typeId, $jvList.of());
         |  }
         |}""".stripMargin

    val meta       = jvTreeTools.inPkg(pkg.parts.toSeq, metaTree)
    val metaOutput = JvDefnTranslator.Output(s"$basename/BaboonMetadata.java", meta, pkg, CompilerProduct.Definition)

    F.pure(List(metaOutput))
  }

  private def sharedFixture(): Out[List[JvDefnTranslator.Output]] = {
    def fix(path: String, resource: String): JvDefnTranslator.Output = {
      JvDefnTranslator.Output(
        path,
        TextTree.text(IzResources.readAsString(resource).get),
        JvTypes.baboonFixturePkg,
        CompilerProduct.FixtureRuntime,
        doNotModify = true,
      )
    }
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      F.pure(
        List(
          fix("BaboonRandom.java", "baboon-runtime/java/BaboonRandom.java"),
          fix("BaboonRandomFactory.java", "baboon-runtime/java/BaboonRandomFactory.java"),
          fix("BaboonRandomImpl.java", "baboon-runtime/java/BaboonRandomImpl.java"),
        )
      )
    } else F.pure(Nil)
  }

  private def renderTree(o: JvDefnTranslator.Output): String = {
    val rendered = o.tree.mapRender {
      case t: JvValue.JvTypeName => t.name
      case t: JvValue.JvType if !t.fq =>
        if (o.pkg == t.pkg || !t.pkg.parts.startsWith(o.pkg.parts)) {
          t.name
        } else {
          (t.pkg.parts :+ t.name).mkString(".")
        }
      case t: JvValue.JvType => (t.pkg.parts :+ t.name).mkString(".")
    }

    if (o.doNotModify) {
      rendered
    } else {
      val usedTypes = o.tree.values.collect { case t: JvValue.JvType => t }.distinct
        .filterNot(_.predef)
        .filterNot(_.fq)
        .filterNot(_.pkg == o.pkg)
        .filterNot(t => t.pkg.parts.startsWith(o.pkg.parts))
        .sortBy(_.toString)

      val importLines = usedTypes.map {
        p =>
          val importName = if (p.name.contains('.')) p.name.split('.').head else p.name
          s"import ${p.pkg.parts.mkString(".")}.$importName;"
      }.distinct

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

  private def sharedRuntime(): Out[List[JvDefnTranslator.Output]] = {
    def rt(path: String, resource: String, preprocessResource: String => String = identity): JvDefnTranslator.Output = {
      JvDefnTranslator.Output(
        path,
        TextTree.text(IzResources.readAsString(resource).map(preprocessResource).get),
        JvTypes.baboonRuntimePkg,
        CompilerProduct.Runtime,
        doNotModify = true,
      )
    }
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          rt("BaboonGenerated.java", "baboon-runtime/java/BaboonGenerated.java"),
          rt("BaboonGeneratedLatest.java", "baboon-runtime/java/BaboonGeneratedLatest.java"),
          rt("BaboonAdtMemberMeta.java", "baboon-runtime/java/BaboonAdtMemberMeta.java"),
          rt("BaboonMeta.java", "baboon-runtime/java/BaboonMeta.java"),
          rt("BaboonCodecContext.java", "baboon-runtime/java/BaboonCodecContext.java"),
          rt("Lazy.java", "baboon-runtime/java/Lazy.java"),
          rt("ByteString.java", "baboon-runtime/java/ByteString.java"),
          rt("BaboonJsonCodec.java", "baboon-runtime/java/BaboonJsonCodec.java"),
          rt("BaboonBinCodec.java", "baboon-runtime/java/BaboonBinCodec.java"),
          rt("BaboonBinCodecIndexed.java", "baboon-runtime/java/BaboonBinCodecIndexed.java"),
          rt("BaboonBinTools.java", "baboon-runtime/java/BaboonBinTools.java"),
          rt("LEDataInputStream.java", "baboon-runtime/java/LEDataInputStream.java"),
          rt("LEDataOutputStream.java", "baboon-runtime/java/LEDataOutputStream.java"),
          rt("BaboonTimeFormats.java", "baboon-runtime/java/BaboonTimeFormats.java"),
          rt("AbstractConversion.java", "baboon-runtime/java/AbstractConversion.java"),
          rt("AbstractBaboonConversions.java", "baboon-runtime/java/AbstractBaboonConversions.java"),
          rt("AbstractBaboonJsonCodecs.java", "baboon-runtime/java/AbstractBaboonJsonCodecs.java"),
          rt("AbstractBaboonUebaCodecs.java", "baboon-runtime/java/AbstractBaboonUebaCodecs.java"),
          rt("BaboonEither.java", "baboon-runtime/java/BaboonEither.java"),
          rt("BaboonException.java", "baboon-runtime/java/BaboonException.java"),
          rt("BaboonMethodId.java", "baboon-runtime/java/BaboonMethodId.java"),
          rt("BaboonWiringError.java", "baboon-runtime/java/BaboonWiringError.java"),
          rt("BaboonWiringException.java", "baboon-runtime/java/BaboonWiringException.java"),
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
    defnOut: List[JvDefnTranslator.Output],
  ): Out[List[JvDefnTranslator.Output]] = {
    val pkg = trans.toJvPkg(domain.id, domain.version, lineage.evolution)

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

      val missingIface = if (missing.nonEmpty) {
        q"""public interface RequiredConversions {
           |    ${missing.joinN().shift(4).trim}
           |}
           |""".stripMargin
      } else q""

      val ctorParam = if (missing.nonEmpty) {
        q"private final RequiredConversions required;"
      } else q""

      val ctorParamDecl = if (missing.nonEmpty) "RequiredConversions required" else ""
      val ctorBody = if (missing.nonEmpty) {
        q"this.required = required;"
      } else q""

      val converter =
        q"""public class BaboonConversions extends $baboonAbstractConversions {
           |    $ctorParam
           |
           |    ${missingIface.shift(4).trim}
           |
           |    public BaboonConversions($ctorParamDecl) {
           |      ${ctorBody.shift(6).trim}
           |      ${conversionRegs.joinN().shift(6).trim}
           |    }
           |
           |    @Override public $jvList<String> versionsFrom() { return $jvList.of(${toCurrent.map(_.from.v.toString).map(v => s"\"$v\"").mkString(", ")}); }
           |    @Override public String versionTo() { return "${domain.version.v.toString}"; }
           |}""".stripMargin

      import izumi.fundamentals.collections.IzCollections.*
      val regsMap = defnOut.flatMap(_.codecReg).toMultimap.view.mapValues(_.flatten).toMap

      val basename = jvFiles.basename(domain, lineage.evolution)

      val converterWrapped = jvTreeTools.inPkg(pkg.parts.toSeq, converter)
      val converterOutput = JvDefnTranslator.Output(
        s"$basename/BaboonConversions.java",
        converterWrapped,
        pkg,
        CompilerProduct.Conversion,
      )

      val codecOutputs = regsMap.map {
        case (codecId, regs) =>
          val className = s"BaboonCodecs${codecId.capitalize}"
          val codecTree =
            q"""public class $className extends ${abstractBaboonCodec(codecId)} {
               |  public $className() {
               |    ${regs.toList.map(c => q"register($c);").joinN().shift(4).trim}
               |  }
               |}""".stripMargin
          val wrapped = jvTreeTools.inPkg(pkg.parts.toSeq, codecTree)
          JvDefnTranslator.Output(
            s"$basename/$className.java",
            wrapped,
            pkg,
            CompilerProduct.Conversion,
          )
      }.toList

      val convertersOutput = convs.map {
        conv =>
          JvDefnTranslator.Output(
            s"$basename/${conv.fname}",
            conv.conv,
            pkg,
            CompilerProduct.Conversion,
          )
      }

      List(converterOutput) ++ codecOutputs ++ convertersOutput
    }
  }
}

object JvBaboonTranslator {
  case class RenderedConversion(
    fname: String,
    conv: TextTree[JvValue],
    reg: Option[TextTree[JvValue]],
    missing: Option[TextTree[JvValue]],
  )
}
