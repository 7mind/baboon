package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.PyModuleId
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.{BaboonFamily, BaboonLineage, Domain, DomainMember, EvolutionStep}
import izumi.distage.Subcontext
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import io.septimalmind.baboon.translator.BaboonRuntimeResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.platform.strings.TextTree.Quote

class PyBaboonTranslator[F[+_, +_]: Error2](
  translator: Subcontext[PyDefnTranslator[F]],
  convTransFactory: PyConversionTranslator.Factory[F],
  target: PyTarget,
  pyFileTools: PyFileTools,
  typeTranslator: PyTypeTranslator,
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): Out[Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime()
      testHelper <- sharedTestHelper()
      rendered = (translated ++ runtime ++ testHelper).map {
        o =>
          val content = renderTree(o)
          (o.path, OutputFile(content, o.product))
      }
      unique <- F.fromEither(rendered.toUniqueMap(c => BaboonIssue.of(TranslationIssue.NonUniqueOutputFiles(c))))
    } yield Sources(unique)
  }

  private def translateFamily(
    family: BaboonFamily
  ): Out[List[PyDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[PyDefnTranslator.Output]] = {
    for {
      perVersion <- F.flatSequenceAccumErrors {
        lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
      }
      facade <- generateDomainFacade(lineage)
    } yield perVersion ++ facade
  }

  private def generateDomainFacade(lineage: BaboonLineage): Out[List[PyDefnTranslator.Output]] = {
    if (!target.language.generateDomainFacade || !target.output.products.contains(CompilerProduct.Conversion)) {
      F.pure(List.empty)
    } else {
      val domainIdStr   = lineage.pkg.path.mkString(".")
      val facadeName    = "Domain" + lineage.pkg.path.map(s => s.capitalize).mkString + "Facade"
      val moduleIdParts = lineage.pkg.path.toList.map(_.toLowerCase) :+ "domain_facade"
      val facadeModule  = PyValue.PyModuleId(NEList.unsafeFrom(moduleIdParts))
      val basename      = lineage.pkg.path.map(_.toLowerCase).mkString("/")

      val sortedVersions = lineage.versions.iterator.toList.sortBy(_._1)

      // Per-version import metadata: (import lines, alias names, version string)
      val versionImports = sortedVersions.map {
        case (version, _) =>
          val isLatest    = version == lineage.evolution.latest
          val vPrefix     = if (isLatest) "" else version.format(prefix = "v", delimiter = "_") + "."
          val suffix      = if (isLatest) "" else s"_${version.format(prefix = "v", delimiter = "_")}"
          val jsonAlias   = s"BaboonCodecsJson$suffix"
          val uebaAlias   = s"BaboonCodecsUeba$suffix"
          val metaAlias   = s"BaboonMetadata$suffix"
          val runtimeMod  = s".${vPrefix}baboon_runtime"
          val metadataMod = s".${vPrefix}baboon_metadata"
          val jsonImport  =
            if (isLatest) s"from $runtimeMod import BaboonCodecsJson, BaboonCodecsUeba"
            else s"from $runtimeMod import BaboonCodecsJson as $jsonAlias, BaboonCodecsUeba as $uebaAlias"
          val metaImport  =
            if (isLatest) s"from $metadataMod import BaboonMetadata"
            else s"from $metadataMod import BaboonMetadata as $metaAlias"
          (jsonImport, metaImport, jsonAlias, uebaAlias, metaAlias, version.v.toString)
      }

      val importLines = versionImports.flatMap { case (ji, mi, _, _, _, _) => List(ji, mi) }.mkString("\n")

      val registerCalls = versionImports.map {
        case (_, _, jsonAlias, uebaAlias, metaAlias, versionStr) =>
          q"""self.register(
             |    $baboonDomainVersion("$domainIdStr", "$versionStr"),
             |    codecs_json=lambda: ${jsonAlias}.instance(),
             |    codecs_bin=lambda: ${uebaAlias}.instance(),
             |    meta=lambda: ${metaAlias}(),
             |)""".stripMargin
      }

      val facadeTree =
        q"""$importLines
           |
           |
           |class $facadeName($baboonCodecsFacade):
           |    def __init__(self):
           |        super().__init__()
           |        ${registerCalls.map(_.shift(8).trim).joinN().shift(8).trim}
           |""".stripMargin

      val facadeOutput = PyDefnTranslator.Output(
        s"$basename/domain_facade.py",
        facadeTree,
        facadeModule,
        CompilerProduct.Conversion,
      )

      F.pure(List(facadeOutput))
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[PyDefnTranslator.Output]] = {
    val evo = lineage.evolution
    translator.provide(domain).provide(evo).produce().use {
      defnTranslator =>
        for {
          defnSources     <- translateProduct(domain, CompilerProduct.Definition, defnTranslator.translate)
          fixturesSources <- translateProduct(domain, CompilerProduct.Fixture, defnTranslator.translateFixtures)
          testsSources    <- translateProduct(domain, CompilerProduct.Test, defnTranslator.translateTests)
          serviceRt       <- defnTranslator.translateServiceRt()
          initPyFile       = if (domain.version == lineage.evolution.latest) Nil else List(genInitPy(defnSources, domain))

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
          meta ++
          initPyFile
        }
    }
  }

  private def genInitPy(definitions: List[PyDefnTranslator.Output], domain: Domain): PyDefnTranslator.Output = {
    val initPyModule  = typeTranslator.toPyModule(domain.id)
    val versionStr    = domain.version.format(prefix = "v", delimiter = "_")
    val fileName      = "__init__.py"
    val initPyDir     = initPyModule.path.toList :+ versionStr
    val expectedDepth = initPyDir.size + 1

    val sameLevelModules = definitions
      .filter(o => o.path.split("/").length == expectedDepth)
      .map(o => q"${o.module.module}")

    val importTree = if (sameLevelModules.nonEmpty) {
      q"""from . import (
         |   ${sameLevelModules.join(",\n").shift(4)}
         |)""".stripMargin
    } else {
      q""
    }

    val path = initPyDir :+ fileName
    PyDefnTranslator.Output(
      path.mkString("/"),
      importTree,
      initPyModule,
      CompilerProduct.Definition,
    )
  }

  private def sharedRuntime(): Out[List[PyDefnTranslator.Output]] = {
    def rt(path: String, resource: String): PyDefnTranslator.Output = {
      PyDefnTranslator.Output(
        path,
        TextTree.text(BaboonRuntimeResources.read(resource)),
        pyBaboonSharedRuntimeModule,
        CompilerProduct.Runtime,
      )
    }

    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          rt("baboon_runtime_shared.py", "baboon-runtime/python/baboon_runtime_shared.py"),
          rt("baboon_codecs_facade.py", "baboon-runtime/python/baboon_codecs_facade.py"),
          rt("baboon_conversions.py", "baboon-runtime/python/baboon_conversions.py"),
          rt("baboon_exceptions.py", "baboon-runtime/python/baboon_exceptions.py"),
          rt("baboon_codecs.py", "baboon-runtime/python/baboon_codecs.py"),
          rt("baboon_service_wiring.py", "baboon-runtime/python/baboon_service_wiring.py"),
          rt("baboon_any_opaque.py", "baboon-runtime/python/baboon_any_opaque.py"),
          PyDefnTranslator.Output(
            "baboon_identifier_repr.py",
            // `verbatim` (not `text`): the file contains backslash characters
            // (escape-handling) that would crash Scala's
            // StringContext.processEscapes if routed through `text`.
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/python/baboon_identifier_repr.py")),
            pyBaboonSharedRuntimeModule,
            CompilerProduct.Runtime,
          ),
          PyDefnTranslator.Output("__init__.py", TextTree.text(""), pyBaboonSharedRuntimeModule, CompilerProduct.Runtime),
        )
      )
    } else F.pure(Nil)
  }

  private def sharedTestHelper(): Out[List[PyDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Test)) {
      F.pure(
        List(
          PyDefnTranslator.Output(
            "cross_language_fixture_path.py",
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/python/cross_language_fixture_path.py")),
            pyCrossLanguageFixturePathModule,
            CompilerProduct.Test,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def renderTree(o: PyDefnTranslator.Output): String = {
    val usedTypes = o.tree.values.collect { case t: PyValue.PyType => t }
      .filterNot(_.moduleId == pyBuiltins)
      .filterNot(_.moduleId == o.module)
      .distinct

    val (versioned, usual) = usedTypes.partition(_.versioned)

    // Versioned (cross-version) refs were previously emitted as `from <pkg> import v<X>`
    // followed by attribute access `v<X>.<ns>.<module>.<Type>`. That works when the version
    // package's `__init__.py` re-exports the leaf module, but breaks for namespaced types
    // (`v<X>.<ns>.<module>` where intermediate namespace dirs are implicit-namespace packages
    // without `__init__.py`s). Instead, emit per-leaf direct imports `from <full module path>
    // import <Symbol> as <alias>` and rewrite the dereference to use `<alias>`. The alias is
    // derived from the versioned type's flattened path so it is unique within the rendered
    // module.
    val versionedAliases: Map[PyValue.PyType, (String, String, String)] = versioned.map {
      t =>
        val parts      = t.name.split('.').toList
        val symbolName = parts.last // class name within the type's `.py` module
        val moduleFqn  = t.moduleId.path.mkString(".")
        val alias      = parts.mkString("_")
        t -> (moduleFqn, symbolName, alias)
    }.toMap

    val aliasMap = buildAliasMap(usual) ++ versionedAliases.map { case (t, (_, _, alias)) => t -> alias }

    val versionedImports = versionedAliases.toList.sortBy { case (_, (mod, sym, _)) => (mod, sym) }.map {
      case (_, (moduleFqn, symbolName, alias)) =>
        q"from $moduleFqn import $symbolName as $alias"
    }

    val usualImportsByModule = usual.groupBy(_.moduleId).toList.sortBy { case (moduleId, types) => moduleId.path.size + types.size }.reverse.map {
      case (module, types) =>
        val typesString = types.map {
          t =>
            aliasMap.get(t).map(a => s"${t.name} as $a").getOrElse(t.name)
        }.mkString(", ")
        if (module == pyBaboonCodecsModule || module == pyBaboonSharedRuntimeModule || module == pyBaboonConversionsModule || module == pyBaboonServiceWiringModule || module == pyBaboonAnyOpaqueModule || module == pyBaboonExceptionsModule || module == pyBaboonIdReprModule || module == pyBaboonCodecsFacadeModule) {
          val baseString = pyFileTools.definitionsBasePkg.mkString(".")
          q"from $baseString.${module.module} import $typesString"
        } else if (module == pyCrossLanguageFixturePathModule) {
          // The cross-language fixture helper is emitted with CompilerProduct.Test,
          // so the file lands under testsOutput (testsBasePkg), NOT under
          // definitionsBasePkg. Imports must therefore be resolved against
          // testsBasePkg.
          val baseString = pyFileTools.testsBasePkg.mkString(".")
          q"from $baseString.${module.module} import $typesString"
        } else {
          q"from ${module.path.mkString(".")} import $typesString"
        }
    }

    val allImports = (usualImportsByModule ++ versionedImports).joinN()

    val full = Seq(allImports, o.tree).joinNN()

    full.mapRender {
      case t: PyValue.PyType => aliasMap.getOrElse(t, t.name)
    }
  }

  private def buildAliasMap(usedTypes: Seq[PyValue.PyType]): Map[PyValue.PyType, String] = {
    val conflicting = usedTypes.groupBy(_.name).filter(_._2.size > 1)
    conflicting.flatMap {
      case (name, group) =>
        val paths     = group.map(t => t -> t.moduleId.path.toList)
        val commonLen = paths.map(_._2).reduce((a, b) => a.zip(b).takeWhile { case (x, y) => x == y }.map(_._1)).size
        paths.map {
          case (t, path) =>
            val distinguishing = path.drop(commonLen).dropRight(1)
            val prefix =
              if (distinguishing.nonEmpty) distinguishing.mkString("_")
              else path.dropRight(1).lastOption.getOrElse("m")
            t -> s"${prefix}_$name"
        }
    }
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
    defnOut: List[PyDefnTranslator.Output],
  ): Out[List[PyDefnTranslator.Output]] = {
    val module = typeTranslator.toPyModule(domain.id)

    for {
      conversions <- F.flatSequenceAccumErrors {
        lineage.evolution.rules
          .filter(kv => toCurrent.contains(kv._1))
          .map {
            case (srcVer, rules) =>
              convTransFactory(
                srcDom    = lineage.versions(srcVer.from),
                domain    = domain,
                rules     = rules,
                evolution = lineage.evolution,
              ).makeConversions
          }
      }
    } yield {
      val conversionRegs = conversions.map(_.register).toList
      val missing        = conversions.flatMap(_.missing.iterator.toSeq).toSeq

      val missingTree = if (missing.isEmpty) q"pass" else missing.joinN()

      val converter =
        q"""class RequiredConversions($pyABC):
           |    ${missingTree.shift(4).trim}
           |
           |class BaboonConversions($baboonAbstractConversions):
           |    def __init__(self, required: RequiredConversions):
           |        super().__init__()
           |        self.required = required
           |        ${conversionRegs.joinN().shift(8).trim}
           |
           |    def versions_from(self) -> $pyList[$pyStr]:
           |        return [${toCurrent.map(_.from.v.toString).map(v => s"\"$v\"").mkString(", ")}]
           |
           |    def version_to(self) -> $pyStr: 
           |        return "${domain.version.v.toString}"
           |""".stripMargin

      val regsMap = defnOut.flatMap(_.codecReg).toMultimap.view.mapValues(_.flatten).toMap
      val codecs = regsMap.map {
        case (codecid, regs) =>
          val nme = q"BaboonCodecs${codecid.capitalize}"
          q"""class $nme (${abstractBaboonCodecs(codecid)}):
             |    def __init__(self):
             |        super().__init__()
             |        ${regs.toList.map(r => q"self.register($r)").joinN().shift(8).trim}
             |
             |    @$pyClassMethod
             |    @$pyCache
             |    def instance (cls):
             |        return cls()
             |""".stripMargin
      }.toList.joinNN()

      val basename = pyFileTools.basename(domain, lineage.evolution)

      val runtimeSource = Seq(converter, codecs).joinNN()
      val runtimeOutput = PyDefnTranslator.Output(
        s"$basename/baboon_runtime.py",
        runtimeSource,
        module,
        CompilerProduct.Conversion,
      )

      val convertersOutput = conversions.map {
        conv =>
          PyDefnTranslator.Output(
            s"$basename/${conv.fileName}",
            conv.conversionTree,
            module,
            CompilerProduct.Conversion,
          )
      }
      List(runtimeOutput) ++ convertersOutput
    }
  }

  private def generateMeta(domain: Domain, lineage: BaboonLineage): Out[List[PyDefnTranslator.Output]] = {
    val basename = pyFileTools.basename(domain, lineage.evolution)

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q""""${tid.toString}": [${version.sameIn.map(_.v.toString).map(s => q"\"$s\"").toList.join(", ")}]"""
      }

    val metaTree =
      q"""class BaboonMetadata($baboonMeta):
         |    def __init__(self) -> None:
         |        self.unmodified: dict[str, list[str]] = {
         |            ${entries.join(",\n").shift(12).trim}
         |        }
         |
         |    def unmodified_since(self, type_id_string: $pyStr) -> $pyList[$pyStr]:
         |        return self.unmodified.get(type_id_string, [])
         |
         |""".stripMargin

    val metaOutput = PyDefnTranslator.Output(
      s"$basename/baboon_metadata.py",
      metaTree,
      PyModuleId(NEList.unsafeFrom(domain.id.path.toList :+ "baboon_metadata")),
      CompilerProduct.Definition,
    )

    F.pure(List(metaOutput))
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: DomainMember.User => F[NEList[BaboonIssue], List[PyDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[PyDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }
}

object PyBaboonTranslator {
  case class RenderedConversion(
    fileName: String,
    conversionTree: TextTree[PyValue],
    register: TextTree[PyValue],
    missing: Option[TextTree[PyValue]],
  )
}
