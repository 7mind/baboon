package io.septimalmind.baboon.translator.typescript

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.typescript.TsTypes.{tsBaboonAnyOpaqueModule, tsBaboonIdReprModule, tsBaboonRuntimeShared, tsCrossLangFixtureModule, tsFixtureShared}
import io.septimalmind.baboon.translator.typescript.TsValue.{TsModuleId, TsType}
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import io.septimalmind.baboon.translator.BaboonRuntimeResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class TsBaboonTranslator[F[+_, +_]: Error2](
  convTransFac: TsConversionTranslator.Factory[F],
  defnTranslator: Subcontext[TsDefnTranslator[F]],
  target: TsTarget,
  tsFileTools: TsFileTools,
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime
      fixture    <- sharedFixture
      testHelper <- sharedTestHelper
      barrels     = generateBarrels(translated ++ runtime)
      rendered = (
        translated ++
          runtime ++
          fixture ++
          testHelper ++
          barrels
      ).map {
        o =>
          val content = renderTree(o)
          (o.path, OutputFile(content, o.product))
      }
      unique <- F.fromEither(rendered.toUniqueMap(c => BaboonIssue.of(TranslationIssue.NonUniqueOutputFiles(c))))
    } yield {
      Sources(unique)
    }
  }

  private def translateFamily(family: BaboonFamily): Out[List[TsDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(lineage: BaboonLineage): Out[List[TsDefnTranslator.Output]] = {
    for {
      perVersion <- F.flatSequenceAccumErrors {
        lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
      }
      facade = generateDomainFacade(lineage)
    } yield {
      perVersion ++ facade
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: DomainMember.User => F[NEList[BaboonIssue], List[TsDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[TsDefnTranslator.Output]] = {
    val evo = lineage.evolution
    defnTranslator.provide(domain).provide(evo).produce().use {
      defnTranslator =>
        for {
          defnSources     <- translateProduct(domain, CompilerProduct.Definition, defnTranslator.translate)
          fixturesSources <- translateProduct(domain, CompilerProduct.Fixture, defnTranslator.translateFixtures)
          testsSources    <- translateProduct(domain, CompilerProduct.Test, defnTranslator.translateTests)
          serviceRt       <- defnTranslator.translateServiceRt()
          dispatcher      <- defnTranslator.translateDispatcher()

          conversionSources <- {
            if (target.output.products.contains(CompilerProduct.Conversion)) {
              val evosToCurrent = evo.diffs.keySet.filter(_.to == domain.version)
              generateConversions(domain, lineage, evosToCurrent)
            } else {
              F.pure(List.empty)
            }
          }
        } yield {
          defnSources ++ serviceRt ++ dispatcher ++ conversionSources ++ fixturesSources ++ testsSources
        }
    }
  }

  private def generateDomainFacade(lineage: BaboonLineage): List[TsDefnTranslator.Output] = {
    if (!target.output.products.contains(CompilerProduct.Runtime)) return List.empty

    val evo            = lineage.evolution
    val latestVersion  = evo.latest
    val latestDomain   = lineage.versions(latestVersion)
    val latestBasename = tsFileTools.basename(latestDomain, evo)

    // Pascal-case domain name: "my.ok" → "MyOk"
    val pascalDomainId = latestDomain.id.path.map(_.capitalize).mkString
    val className      = s"Domain${pascalDomainId}Facade"
    val domainIdStr    = latestDomain.id.path.mkString(".")

    // Collect non-ADT-branch User types per version
    // Returns list of (tsImportPath, typeAlias, typeName, typeIdentifier)
    // typeAlias differs from typeName for older versions (to avoid import name conflicts)
    def collectTypes(domain: Domain): List[(String, String, String, String)] = {
      val versionStr    = domain.version.v.toString
      val isLatest      = domain.version == evo.latest
      val verSuffix     = versionStr.replace('.', '_')
      // Relative path prefix from facade file's dir (= latestBasename) to type's dir
      val relPrefix = if (isLatest) "." else s"./v$verSuffix"
      domain.defs.meta.nodes.toList.flatMap {
        case (_, defn: DomainMember.User) =>
          defn.id.owner match {
            case Owner.Adt(_) => None // skip ADT branches
            case _ =>
              defn.defn match {
                case _: Typedef.Dto | _: Typedef.Enum | _: Typedef.Adt =>
                  val typeName   = defn.id.name.name.capitalize
                  val typeAlias  = if (isLatest) typeName else s"${typeName}V$verSuffix"
                  val importPath = s"$relPrefix/$typeName"
                  val typeId     = defn.id.toString
                  Some((importPath, typeAlias, typeName, typeId))
                case _ => None
              }
          }
        case _ => None
      }
    }

    // Ordered versions (ascending)
    val orderedVersions = lineage.versions.toSeq.sortBy(_._1).map { case (_, domain) => domain }

    val sfx = target.language.importSuffix
    val sb  = new StringBuilder

    // Relative path back to the generated/ root (where BaboonSharedRuntime lives). The facade
    // file lives at `<latestBasename>/<className>.ts`, so depth = segments in `latestBasename`.
    val rootRel = "../" * latestBasename.split('/').length
    // Imports
    sb.append(s"import { AbstractBaboonJsonCodecs, AbstractBaboonUebaCodecs, BaboonBinCodec, BaboonJsonCodec, BaboonDomainVersion, BaboonMeta, Lazy } from '${rootRel}BaboonSharedRuntime$sfx';\n")
    sb.append(s"import type { AbstractBaboonConversions } from '${rootRel}BaboonSharedRuntime$sfx';\n")
    sb.append(s"import { BaboonCodecsFacade } from '${rootRel}BaboonCodecsFacade$sfx';\n")

    // Per-version imports and class definitions
    for (domain <- orderedVersions) {
      val types = collectTypes(domain)

      for ((importPath, typeAlias, typeName, _) <- types) {
        if (typeAlias == typeName) {
          sb.append(s"import { $typeName, ${typeName}_JsonCodec, ${typeName}_UEBACodec } from '$importPath$sfx';\n")
        } else {
          // Aliased import for non-latest versions to avoid name conflicts
          sb.append(s"import { $typeName as $typeAlias, ${typeName}_JsonCodec as ${typeAlias}_JsonCodec, ${typeName}_UEBACodec as ${typeAlias}_UEBACodec } from '$importPath$sfx';\n")
        }
      }
    }

    sb.append("\n")

    // Per-version codec/meta/conversions classes
    for (domain <- orderedVersions) {
      val versionStr   = domain.version.v.toString
      val verSuffix    = versionStr.replace('.', '_')
      val types        = collectTypes(domain)
      val verClassName = s"Domain${pascalDomainId}V${verSuffix}"

      // JSON codecs class
      sb.append(s"class ${verClassName}JsonCodecs extends AbstractBaboonJsonCodecs {\n")
      sb.append( "    constructor() {\n")
      sb.append( "        super();\n")
      for ((_, typeAlias, _, _) <- types) {
        sb.append(s"        this.register($typeAlias.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => ${typeAlias}_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));\n")
      }
      sb.append( "    }\n")
      sb.append( "}\n\n")

      // UEBA codecs class
      sb.append(s"class ${verClassName}UebaCodecs extends AbstractBaboonUebaCodecs {\n")
      sb.append( "    constructor() {\n")
      sb.append( "        super();\n")
      for ((_, typeAlias, _, _) <- types) {
        sb.append(s"        this.register($typeAlias.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => ${typeAlias}_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));\n")
      }
      sb.append( "    }\n")
      sb.append( "}\n\n")

      // Conversions class
      sb.append(s"class ${verClassName}Conversions implements AbstractBaboonConversions {\n")
      sb.append(s"    public versionsFrom(): string[] { return []; }\n")
      sb.append(s"""    public versionTo(): string { return '$versionStr'; }\n""")
      sb.append( "}\n\n")

      // Meta class
      sb.append(s"class ${verClassName}Meta implements BaboonMeta {\n")
      sb.append(s"""    public sameInVersions(_typeId: string): string[] { return ['$versionStr']; }\n""")
      sb.append( "}\n\n")
    }

    // Facade class
    sb.append(s"export class $className extends BaboonCodecsFacade {\n")
    sb.append( "    constructor() {\n")
    sb.append( "        super();\n")
    for (domain <- orderedVersions) {
      val versionStr   = domain.version.v.toString
      val verSuffix    = versionStr.replace('.', '_')
      val verClassName = s"Domain${pascalDomainId}V${verSuffix}"
      sb.append( "        this.register(\n")
      sb.append(s"""            new BaboonDomainVersion('$domainIdStr', '$versionStr'),\n""")
      sb.append(s"            () => new ${verClassName}JsonCodecs(),\n")
      sb.append(s"            () => new ${verClassName}UebaCodecs(),\n")
      sb.append(s"            () => new ${verClassName}Conversions(),\n")
      sb.append(s"            () => new ${verClassName}Meta(),\n")
      sb.append( "        );\n")
    }
    sb.append( "    }\n")
    sb.append( "}\n")

    val content    = sb.toString()
    val outputPath = s"$latestBasename/$className.ts"
    val moduleParts = tsFileTools.definitionsBasePkg ++ outputPath.stripSuffix(".ts").split('/').toList
    val moduleId   = TsValue.TsModuleId(moduleParts)

    List(
      TsDefnTranslator.Output(
        outputPath,
        TextTree.verbatim(content),
        moduleId,
        CompilerProduct.Runtime,
        doNotModify = true,
      )
    )
  }

  private def sharedRuntime: Out[List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          TsDefnTranslator.Output(
            "BaboonSharedRuntime.ts",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonSharedRuntime.ts")),
            tsBaboonRuntimeShared,
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          TsDefnTranslator.Output(
            "BaboonAnyOpaque.ts",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonAnyOpaque.ts")),
            tsBaboonRuntimeShared,
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          TsDefnTranslator.Output(
            "BaboonCodecsFacade.ts",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonCodecsFacade.ts")),
            tsBaboonRuntimeShared,
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
          TsDefnTranslator.Output(
            "baboon-identifier-repr.ts",
            // `verbatim` (not `text`): the file contains backslash characters
            // (escape-handling) that would crash Scala's
            // StringContext.processEscapes if routed through `text`.
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/baboon-identifier-repr.ts")),
            tsBaboonIdReprModule,
            CompilerProduct.Runtime,
            doNotModify = true,
          ),
        )
      )
    } else {
      F.pure(List.empty)
    }
  }

  private def sharedFixture: Out[List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      F.pure(
        List(
          TsDefnTranslator.Output(
            "BaboonSharedFixture.ts",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonSharedFixture.ts")),
            tsBaboonRuntimeShared,
            CompilerProduct.Runtime,
            doNotModify = true,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def sharedTestHelper: Out[List[TsDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Test)) {
      F.pure(
        List(
          TsDefnTranslator.Output(
            "CrossLanguageFixturePath.ts",
            TextTree.verbatim(BaboonRuntimeResources.read("baboon-runtime/typescript/CrossLanguageFixturePath.ts")),
            tsCrossLangFixtureModule,
            CompilerProduct.Test,
            doNotModify = true,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def renderTree(o: TsDefnTranslator.Output): String = {
    val sfx = target.language.importSuffix

    def relativeImportPath(srcModulePath: List[String], tgtModulePath: List[String]): String = {
      val srcDir          = srcModulePath.dropRight(1)
      val tgtDir          = tgtModulePath.dropRight(1)
      val tgtName         = tgtModulePath.last
      val commonPrefixLen = srcDir.zip(tgtDir).takeWhile { case (a, b) => a == b }.size
      val levelsUp        = srcDir.size - commonPrefixLen
      val remainingPath   = (tgtDir.drop(commonPrefixLen) :+ tgtName).mkString("/")
      if (levelsUp == 0) s"./$remainingPath$sfx" else s"${"../" * levelsUp}$remainingPath$sfx"
    }

    def baboonTypeImport(moduleId: TsModuleId, types: String): TextTree[TsValue] = {
      // The cross-language fixture helper is emitted with CompilerProduct.Test,
      // so the file lands under testsOutput (testBasePkg), NOT output
      // (definitionsBasePkg). Imports for it must therefore be resolved against
      // testBasePkg.
      val targetBasePkg =
        if (moduleId == tsCrossLangFixtureModule) tsFileTools.testBasePkg
        else tsFileTools.definitionsBasePkg
      if (o.module.path.startsWith(targetBasePkg)) {
        val afterBase    = o.module.path.drop(targetBasePkg.size)
        val pathToModule = (0 until afterBase.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToModule${moduleId.path.mkString("")}$sfx'"
      } else {
        val pathToCommonParent = (0 until o.module.path.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToCommonParent${targetBasePkg.mkString("/")}/${moduleId.path.mkString("")}$sfx'"
      }
    }

    def definitionImport(moduleId: TsModuleId, types: String): TextTree[TsValue] = {
      if (o.module.path.startsWith(tsFileTools.definitionsBasePkg)) {
        val importPath = relativeImportPath(o.module.path, moduleId.path)
        q"import {$types} from '$importPath'"
      } else {
        val pathToCommonParent = (0 until o.module.path.size - 1).map(_ => "../").mkString("")
        q"import {$types} from '$pathToCommonParent${moduleId.path.mkString("/")}$sfx'"
      }
    }

    def fixtureImport(moduleId: TsModuleId, types: String): TextTree[TsValue] = {
      val importPath = relativeImportPath(o.module.path, moduleId.path)
      q"import {$types} from '$importPath'"
    }

    val usedTypes = o.tree.values.collect { case t: TsValue.TsType => t }
      .filterNot(_.moduleId.path.isEmpty)
      .filterNot(_.moduleId == o.module)
      .filterNot(_.predef)
      .distinct

    val aliasMap = buildAliasMap(usedTypes)

    val typesByModule = usedTypes.groupBy(_.moduleId).toList.sortBy { case (moduleId, types) => moduleId.path.size + types.size }.reverse

    val importsByModule =
      typesByModule.flatMap {
        case (moduleId, types) =>
          // Partition into type-only and value-bearing
          val (typeOnlyTypes, valueTypes) = types.partition(_.typeOnly)

          def typeString(ts: Seq[TsType], perNameType: Boolean): String = ts.map {
            case t if aliasMap.contains(t)          => s"${if (perNameType) "type " else ""}${t.name} as ${aliasMap(t)}"
            case TsType(_, name, Some(alias), _, _) => s"${if (perNameType) "type " else ""}$name as $alias"
            case t: TsValue.TsType                  => s"${if (perNameType) "type " else ""}${t.name}"
          }.mkString(", ")

          def mkImportFor(tsList: Seq[TsType], importType: Boolean): Option[TextTree[TsValue]] = {
            if (tsList.isEmpty) None
            else {
              val typesString = typeString(tsList, perNameType = importType)
              val result = if (moduleId.path.startsWith(tsFileTools.definitionsBasePkg)) {
                definitionImport(moduleId, typesString)
              } else if (moduleId.path.startsWith(tsFileTools.fixturesBasePkg) && tsFileTools.fixturesBasePkg.nonEmpty) {
                fixtureImport(moduleId, typesString)
              } else if (moduleId == tsBaboonRuntimeShared || moduleId == tsFixtureShared || moduleId == tsBaboonAnyOpaqueModule || moduleId == tsBaboonIdReprModule || moduleId == tsCrossLangFixtureModule) {
                baboonTypeImport(moduleId, typesString)
              } else {
                q"import {$typesString} from '${moduleId.path.mkString("/")}'"
              }
              Some(result)
            }
          }

          List(
            mkImportFor(typeOnlyTypes, importType = true),
            mkImportFor(valueTypes, importType    = false),
          ).flatten
      }

    val allImports = importsByModule.joinN()

    val full = Seq(allImports, o.tree).joinNN()

    full.mapRender {
      case t: TsValue.TsType if aliasMap.contains(t) => aliasMap(t)
      case TsValue.TsType(_, _, Some(alias), _, _)   => alias
      case t: TsValue.TsType                         => t.name
    }
  }

  private def buildAliasMap(usedTypes: Seq[TsType]): Map[TsType, String] = {
    val conflicting = usedTypes.groupBy(_.name).filter(_._2.size > 1)
    conflicting.flatMap {
      case (name, group) =>
        val paths     = group.map(t => t -> t.moduleId.path)
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

  /** Extract type names that a file defines (types whose module matches the file's own module).
    * Returns a map from name to typeOnly flag (true = type-only, false = value-bearing).
    */
  private def exportedNames(output: TsDefnTranslator.Output): Map[String, Boolean] = {
    output.tree.values.collect {
      case t: TsValue.TsType if t.moduleId == output.module && !t.predef => t.name -> t.typeOnly
    }.toMap
  }

  /** Render a named re-export line, splitting type-only and value-bearing names into separate statements. */
  private def namedReexports(
    nameToTypeOnly: Map[String, Boolean],
    names: Iterable[String],
    renderName: String => String,
    from: String,
  ): List[TextTree[TsValue]] = {
    val sorted       = names.toList.sorted
    val typeOnly     = sorted.filter(n => nameToTypeOnly.getOrElse(n, false))
    val valueBearing = sorted.filterNot(n => nameToTypeOnly.getOrElse(n, false))
    List(
      if (typeOnly.nonEmpty) Some(TextTree.text[TsValue](s"export type { ${typeOnly.map(renderName).mkString(", ")} } from '$from';")) else None,
      if (valueBearing.nonEmpty) Some(TextTree.text[TsValue](s"export { ${valueBearing.map(renderName).mkString(", ")} } from '$from';")) else None,
    ).flatten
  }

  private def generateBarrels(outputs: List[TsDefnTranslator.Output]): List[TsDefnTranslator.Output] = {
    val sfx = target.language.importSuffix
    val definitionOutputs = outputs
      .filter(o => o.product == CompilerProduct.Definition || o.product == CompilerProduct.Runtime)
      .filterNot(_.isBarrel)
      .filter(_.path.endsWith(".ts"))

    // Group files by their direct parent directory
    val byDir = definitionOutputs.groupBy {
      o =>
        val idx = o.path.lastIndexOf('/')
        if (idx >= 0) o.path.substring(0, idx) else ""
    }

    // Generate per-directory barrels with collision detection.
    // Skip root directory (runtime-only files, not useful in barrel).
    val perDirBarrels = byDir.toList.filter(_._1.nonEmpty).sortBy(_._1).flatMap {
      case (dir, files) =>
        val sortedFiles = files.sortBy(_.path)

        // Collect exported names per file and detect collisions across files in this directory
        val fileExports = sortedFiles.map(f => (f, exportedNames(f)))
        val nameCount   = fileExports.flatMap(_._2.keys).groupBy(identity).view.mapValues(_.size).toMap
        val colliding   = nameCount.filter(_._2 > 1).keySet

        val reexports = if (colliding.isEmpty) {
          // No collisions — simple export * for all files
          sortedFiles.map {
            f =>
              val fname = f.path.drop(dir.length + 1).stripSuffix(".ts")
              TextTree.text[TsValue](s"export * from './$fname$sfx';")
          }
        } else {
          // Has collisions — files with colliding names get individual qualified re-exports,
          // files without collisions get export *
          fileExports.flatMap {
            case (f, nameMap) =>
              val names        = nameMap.keySet
              val fname        = f.path.drop(dir.length + 1).stripSuffix(".ts")
              val myCollisions = names.intersect(colliding)
              if (myCollisions.isEmpty) {
                List(TextTree.text[TsValue](s"export * from './$fname$sfx';"))
              } else {
                val safeNames = names -- colliding
                val qualifier = fname.replace('.', '_').replace('-', '_').replace('/', '_')
                val safeExports = if (safeNames.nonEmpty) {
                  namedReexports(nameMap, safeNames, identity, s"./$fname$sfx")
                } else Nil
                val qualifiedExports = namedReexports(nameMap, myCollisions, n => s"$n as ${qualifier}_$n", s"./$fname$sfx")
                safeExports ++ qualifiedExports
              }
          }
        }

        if (reexports.nonEmpty) {
          val barrelPath   = s"$dir/index.ts"
          val barrelModule = TsModuleId(tsFileTools.definitionsBasePkg ++ barrelPath.stripSuffix(".ts").split('/').toList)
          val tree         = reexports.reduce((a, b) => q"$a\n$b")
          Some(
            TsDefnTranslator.Output(
              barrelPath,
              tree,
              barrelModule,
              CompilerProduct.Definition,
              doNotModify = true,
              isBarrel    = true,
            )
          )
        } else None
    }

    perDirBarrels
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
  ): Out[List[TsDefnTranslator.Output]] = {
    for {
      convs <-
        F.flatSequenceAccumErrors {
          lineage.evolution.rules
            .filter(kv => toCurrent.contains(kv._1))
            .map {
              case (srcVer, rules) =>
                convTransFac(
                  srcDom = lineage.versions(srcVer.from),
                  domain = domain,
                  rules  = rules,
                  evo    = lineage.evolution,
                ).makeConvs
            }
        }
    } yield {
      val basename = tsFileTools.basename(domain, lineage.evolution)
      convs.toList.map {
        conv =>
          val outputPath  = s"$basename/${conv.fname}"
          val moduleParts = tsFileTools.definitionsBasePkg ++ outputPath.stripSuffix(".ts").split('/').toList
          val convModule  = TsModuleId(moduleParts)
          TsDefnTranslator.Output(
            outputPath,
            conv.conv,
            convModule,
            CompilerProduct.Conversion,
          )
      }
    }
  }
}
