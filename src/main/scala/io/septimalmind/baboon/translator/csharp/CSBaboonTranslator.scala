package io.septimalmind.baboon.translator.csharp

import distage.Subcontext
import io.septimalmind.baboon.BaboonCompiler.CompilerTargets
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.translator.{
  BaboonAbstractTranslator,
  OutputFile,
  Sources
}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.{CompilerOptions, RuntimeGenOpt}
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSBaboonTranslator(trans: CSTypeTranslator,
                         handler: Subcontext[IndividualConversionHandler],
                         options: CompilerOptions,
                         tools: CSDefnTools,
                         translator: Subcontext[CSDefnTranslator],
) extends BaboonAbstractTranslator {

  type Out[T] = Either[NEList[BaboonIssue.TranslationIssue], T]

  override def translate(targets: CompilerTargets,
                         family: BaboonFamily): Out[Sources] = {
    for {
      translated <- doTranslate(targets, family)
      runtime <- sharedRuntime()
      testRuntime <- sharedTestRuntime(targets)
      //      meta <- buildMeta(family)
      toRender = options.csOptions.generic.runtime match {
        case RuntimeGenOpt.Only    => runtime
        case RuntimeGenOpt.With    => runtime ++ translated ++ testRuntime
        case RuntimeGenOpt.Without => translated
      }
      rendered = toRender.map { o =>
        val content = renderTree(o)
        (o.path, OutputFile(content, isTest = o.isTest))
      }
      unique <- (rendered /*++ meta.map {
        case (k, v) => (k, OutputFile(v, isTest = false))
      }*/ ).toUniqueMap(c => NEList(BaboonIssue.NonUniqueOutputFiles(c)))
    } yield {
      Sources(unique)
    }
  }

  //  private def buildMeta(family: BaboonFamily): Out[List[(String, String)]] = {
  //
  //    val data = family.domains.toSeq.flatMap {
  //      case (_, lineage) =>
  //        lineage.versions.toSeq.map {
  //          case (ver, _) =>
  //            VersionMeta(lineage.pkg.path.mkString("."), ver.version)
  //        }
  //
  //    }
  //    val meta: OutputMeta = OutputMeta(data.toList)
  //    val json = meta.asJson.spaces2
  //
  //    Right(List((s"baboon-meta.json", json)))
  //  }

  private def renderTree(o: CSDefnTranslator.Output): String = {
    val alwaysAvailable: Set[CSPackageId] =
      if (options.csOptions.disregardImplicitUsings) {
        Set.empty
      } else {
        Set(csSystemPkg, csCollectionsGenericPkg, csLinqPkg)
      }

    val forcedUses: Set[CSPackageId] =
      if (options.csOptions.disregardImplicitUsings) {
        Set(csLinqPkg)
      } else {
        Set.empty
      }

    val usedPackages = o.tree.values
      .collect {
        case t: CSValue.CSType => t
      }
      .flatMap { t =>
        if (!t.fq) {
          Seq(t.pkg)
        } else {
          Seq.empty
        }
      }
      .distinct
      .sortBy(_.parts.mkString("."))

    val available = Set(o.pkg)
    val requiredPackages = Set.empty
    val allPackages =
      (requiredPackages ++ usedPackages ++ forcedUses)
        .diff(available ++ alwaysAvailable)

    val imports = allPackages.toSeq
      .map { p =>
        if (p.isStatic) {
          q"using static ${p.parts.mkString(".")};"
        } else {
          q"using ${p.parts.mkString(".")};"
        }

      }
      .join("\n")

    val full =
      Seq(
        Seq(q"#nullable enable"),
        Seq(q"#pragma warning disable 612,618"), // deprecation warnings
        Seq(imports),
        Seq(o.tree)
      ).flatten
        .join("\n\n")

    full.mapRender {
      case t: CSValue.CSTypeName =>
        t.name
      case t: CSValue.CSType if !t.fq =>
        if (o.pkg == t.pkg || !t.pkg.parts.startsWith(o.pkg.parts)) {
          t.name
        } else {
          (t.pkg.parts :+ t.name).mkString(".")
        }

      case t: CSValue.CSType =>
        (t.pkg.parts :+ t.name).mkString(".")
    }
  }

  private def doTranslate(
    targets: CompilerTargets,
    family: BaboonFamily
  ): Out[List[CSDefnTranslator.Output]] = {
    // TODO: fix .toSeq.toList

    family.domains.toSeq.toList.map {
      case (_, lineage) =>
        translateLineage(targets, lineage)
    }.biFlatten
  }

  private def translateLineage(
    targets: CompilerTargets,
    lineage: BaboonLineage
  ): Out[List[CSDefnTranslator.Output]] = {

    lineage.versions.toSeq.toList.map {
      case (_, domain) =>
        translateDomain(domain, targets, lineage)
    }.biFlatten
  }

  private def translateDomain(domain: Domain,
                              targets: CompilerTargets,
                              lineage: BaboonLineage,
  ): Out[List[CSDefnTranslator.Output]] = {
    val evo = lineage.evolution
    translator.provide(domain).provide(evo).produce().use { defnTranslator =>
      for {
        defnSources <- domain.defs.meta.nodes.toList.map {
          case (_, defn: DomainMember.User) =>
            defnTranslator.translate(defn)
          case _ => Right(List.empty)
        }.biFlatten

        defnTests <- if (targets.testOutput.nonEmpty) {
          domain.defs.meta.nodes.toList.map {
            case (_, defn: DomainMember.User) =>
              defnTranslator.translateTests(defn)
            case _ => Right(List.empty)
          }.biFlatten
        } else {
          Right(List.empty)
        }

        evosToCurrent = evo.diffs.keySet.filter(_.to == domain.version)
        conversionSources <- if (options.csOptions.generic.generateConversions) {
          generateConversions(domain, lineage, evosToCurrent, defnSources)
        } else {
          Right(List.empty)
        }

        meta <- if (options.csOptions.csWriteEvolutionDict) {
          generateMeta(domain, lineage)
        } else {
          Right(List.empty)
        }
      } yield {
        defnSources.map(_.output) ++
          conversionSources ++
          meta ++
          defnTests.map(_.output)
      }
    }

  }

  private def generateMeta(domain: Domain,
                           lineage: BaboonLineage,
  ): Out[List[CSDefnTranslator.Output]] = {
    val basename = tools.basename(domain, lineage.evolution, options)
    val pkg = trans.toCsPkg(domain.id, domain.version, lineage.evolution)

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q"""_unmodified.Add("${tid.toString}", "${version.version}");"""
      }

    val metaTree =
      q"""public sealed class BaboonMeta : $iBaboonMeta
         |{
         |    private BaboonMeta()
         |    {
         |        ${entries.join("\n").shift(8).trim}
         |    }
         |
         |    public String UnmodifiedSince(String typeIdString)
         |    {
         |        return _unmodified[typeIdString];
         |    }
         |
         |    private readonly $csDict<$csString, $csString> _unmodified = new ();
         |
         |    internal static $csLazy<BaboonMeta> LazyInstance = new $csLazy<BaboonMeta>(() => new BaboonMeta());
         |
         |    public static BaboonMeta Instance { get { return LazyInstance.Value; } }
         |}""".stripMargin

    val metaSource = Seq(metaTree).join("\n\n")
    val meta = tools.inNs(pkg.parts.toSeq, metaSource)

    val metaOutput = CSDefnTranslator.Output(
      s"$basename/BaboonMeta.cs",
      meta,
      pkg,
      isTest = false
    )

    Right(List(metaOutput))
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
    defnOut: List[CSDefnTranslator.OutputExt]
  ): Out[List[CSDefnTranslator.Output]] = {
    val pkg = trans.toCsPkg(domain.id, domain.version, lineage.evolution)

    for {
      convs <- lineage.evolution.rules
        .filter(kv => toCurrent.contains(kv._1))
        .map {
          case (srcVer, rules) =>
            handler
              .provide(pkg)
              .provide(srcVer.from)
              .provide[Domain]("current")(domain)
              .provide[Domain]("source")(lineage.versions(srcVer.from))
              .provide(rules)
              .provide(lineage.evolution)
              .produce()
              .use(_.makeConvs())
        }
        .biFlatten
    } yield {
      val conversionRegs = convs.flatMap(_.reg.iterator.toSeq).toSeq
      val missing = convs.flatMap(_.missing.iterator.toSeq).toSeq

      val converter =
        q"""public interface RequiredConversions {
           |    ${missing.join("\n").shift(4).trim}
           |}
           |
           |public sealed class BaboonConversions : $abstractBaboonConversions
           |{
           |    public BaboonConversions(RequiredConversions requiredConversions)
           |    {
           |        ${conversionRegs.join("\n").shift(8).trim}
           |    }
           |
           |    override public $csList<$csString> VersionsFrom()
           |    {
           |        return new $csList<$csString> { ${toCurrent
             .map(_.from.version)
             .map(v => s"""\"$v\"""")
             .mkString(", ")} };
           |    }
           |
           |    override public $csString VersionTo()
           |    {
           |        return "${domain.version.version}";
           |    }
           |}""".stripMargin

      val codecs =
        q"""public sealed class BaboonCodecs : $abstractBaboonCodecs
           |{
           |    private BaboonCodecs()
           |    {
           |        ${defnOut.map(_.codecReg).join("\n").shift(8).trim}
           |    }
           |
           |    internal static $csLazy<BaboonCodecs> LazyInstance = new $csLazy<BaboonCodecs>(() => new BaboonCodecs());
           |
           |    public static BaboonCodecs Instance { get { return LazyInstance.Value; } }
           |}""".stripMargin

      val basename = tools.basename(domain, lineage.evolution, options)

      val runtimeSource = Seq(converter, codecs).join("\n\n")
      val runtime = tools.inNs(pkg.parts.toSeq, runtimeSource)
      val runtimeOutput = CSDefnTranslator.Output(
        s"$basename/BaboonRuntime.cs",
        runtime,
        pkg,
        isTest = false
      )

      val convertersOutput = convs.map { conv =>
        CSDefnTranslator.Output(
          s"$basename/${conv.fname}",
          conv.conv,
          pkg,
          isTest = false
        )
      }

      List(runtimeOutput) ++ convertersOutput
    }
  }

  private def sharedRuntime(): Out[List[CSDefnTranslator.Output]] = {
    val sharedOutput = CSDefnTranslator.Output(
      s"BaboonRuntimeShared.cs",
      TextTree.text(
        IzResources.readAsString("baboon-runtime/cs/BaboonRuntimeShared.cs").get
      ),
      CSTypes.baboonRtPkg,
      isTest = false
    )

    val timeOutput = CSDefnTranslator.Output(
      s"BaboonTime.cs",
      TextTree.text(
        IzResources.readAsString("baboon-runtime/cs/BaboonTime.cs").get
      ),
      CSTypes.baboonTimePkg,
      isTest = false
    )

    Right(List(sharedOutput, timeOutput))
  }

  private def sharedTestRuntime(
    targets: CompilerTargets
  ): Out[List[CSDefnTranslator.Output]] = Right {
    if (targets.testOutput.isEmpty) {
      List()
    } else {
      List(
        CSDefnTranslator.Output(
          "BaboonTestRuntimeShared.cs",
          TextTree.text(
            IzResources
              .readAsString("baboon-runtime/cs/BaboonTestRuntimeShared.cs")
              .get
          ),
          CSTypes.baboonTestRtPkg,
          isTest = true
        )
      )
    }
  }

}

object CSBaboonTranslator {
  case class RenderedConversion(fname: String,
                                conv: TextTree[CSValue],
                                reg: Option[TextTree[CSValue]],
                                missing: Option[TextTree[CSValue]],
  )

}
