package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.PyModuleId
import io.septimalmind.baboon.translator.{OutputFile, Sources, BaboonAbstractTranslator}
import io.septimalmind.baboon.typer.model.{EvolutionStep, DomainMember, BaboonLineage, BaboonFamily, Domain}
import izumi.distage.Subcontext
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
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
      rendered = (translated ++ runtime).map {
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
    F.flatSequenceAccumErrors {
      lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
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
        TextTree.text(IzResources.readAsString(resource).get),
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
          PyDefnTranslator.Output("__init__.py", TextTree.text(""), pyBaboonSharedRuntimeModule, CompilerProduct.Runtime)
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

    val versionPkgImports = versioned
      .map(t => t.moduleId.pathToVersion -> t.moduleId).toMap
      .map {
        case (path, module) =>
          q"from ${path.mkString(".")} import ${module.moduleVersionString.getOrElse("")}"
      }.toList

    val namespaceImports = versioned.filter {
      t =>
        val parts = t.name.split('.')
        parts.length > 3
    }.flatMap {
      t =>
        val parts         = t.name.split('.')
        val versionPart   = parts.head
        val namespacePart = parts(1)
        val modulePart    = parts(2)
        val pathToVersion = t.moduleId.pathToVersion
        val nsImport      = q"from ${(pathToVersion :+ versionPart).mkString(".")} import $namespacePart"
        val moduleImport  = q"from ${(pathToVersion :+ versionPart :+ namespacePart).mkString(".")} import $modulePart"
        List(nsImport, moduleImport)
    }.distinct.toList

    val usualImportsByModule = usual.groupBy(_.moduleId).toList.sortBy { case (moduleId, types) => moduleId.path.size + types.size }.reverse.map {
      case (module, types) =>
        if (module == pyBaboonCodecsModule || module == pyBaboonSharedRuntimeModule || module == pyBaboonConversionsModule) {
          val baseString  = pyFileTools.definitionsBasePkg.mkString(".")
          val typesString = types.map(_.name).mkString(", ")
          q"from $baseString.${module.module} import $typesString"
        } else {
          val typesString = types.map(_.name).mkString(", ")
          q"from ${module.path.mkString(".")} import $typesString"
        }
    }

    val allImports = (usualImportsByModule ++ versionPkgImports ++ namespaceImports).joinN()

    val full = Seq(allImports, o.tree).joinNN()

    full.mapRender {
      case t: PyValue.PyType => t.name
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
