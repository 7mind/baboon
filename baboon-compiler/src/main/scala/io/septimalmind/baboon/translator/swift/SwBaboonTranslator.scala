package io.septimalmind.baboon.translator.swift

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.SwTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class SwBaboonTranslator[F[+_, +_]: Error2](
  trans: SwTypeTranslator,
  convTransFac: SwConversionTranslator.Factory[F],
  defnTranslator: Subcontext[SwDefnTranslator[F]],
  target: SwTarget,
  swTreeTools: SwTreeTools,
  swFiles: SwFileTools,
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime()
      fixture    <- sharedFixture()
      allOutputs = translated ++ runtime ++ fixture
      rendered = allOutputs.map {
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
  ): Out[List[SwDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[SwDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: DomainMember.User => F[NEList[BaboonIssue], List[SwDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[SwDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[SwDefnTranslator.Output]] = {
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
          val namespaceDecls = generateNamespaceDeclarations(domain, evo)

          defnSources ++
          conversionSources ++
          fixturesSources ++
          testsSources ++
          serviceRt ++
          meta ++
          namespaceDecls
        }
    }
  }

  private def generateNamespaceDeclarations(domain: Domain, evo: BaboonEvolution): List[SwDefnTranslator.Output] = {
    val nsPaths = domain.defs.meta.nodes.values.collect {
      case DomainMember.User(_, defn, _, _) =>
        defn.id.owner match {
          case Owner.Ns(path) => Some(path)
          case _              => None
        }
    }.flatten.toSet

    if (nsPaths.isEmpty) return Nil

    val allPrefixes = nsPaths.flatMap { path =>
      (1 to path.length).map(path.take)
    }.toList.sortBy(_.length)

    val declarations = allPrefixes.distinct.sortBy(_.length).map { prefix =>
      if (prefix.length == 1) {
        val name = trans.escapeSwiftKeyword(prefix.head.name.toLowerCase)
        q"public enum $name {}"
      } else {
        val parentPath = prefix.init.map(s => trans.escapeSwiftKeyword(s.name.toLowerCase)).mkString(".")
        val name       = trans.escapeSwiftKeyword(prefix.last.name.toLowerCase)
        q"extension $parentPath { public enum $name {} }"
      }
    }

    if (declarations.isEmpty) return Nil

    val basename = swFiles.basename(domain, evo)
    val pkg      = trans.toSwPkg(domain.id, domain.version, evo)

    List(SwDefnTranslator.Output(
      s"$basename/baboon_namespaces.swift",
      declarations.joinN(),
      pkg,
      CompilerProduct.Definition,
    ))
  }

  private def generateMeta(domain: Domain, lineage: BaboonLineage): Out[List[SwDefnTranslator.Output]] = {
    val basename = swFiles.basename(domain, lineage.evolution)
    val versionSuffix = outputVersionSuffix(domain, lineage.evolution)
    val pkg      = trans.toSwPkg(domain.id, domain.version, lineage.evolution)
    val domainSuffix      = domainClassSuffix(domain)
    val domainFileSuffix  = trans.toSnakeCase(domainSuffix)
    val metadataClassName = s"BaboonMetadata_${domainSuffix}$versionSuffix"

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q""""${tid.toString}": [${version.sameIn.map(_.v.toString).map(s => q""""$s"""").toList.join(", ")}],"""
      }

    val metaTree =
      q"""public class $metadataClassName {
         |    public static let unmodified: [String: [String]] = [
         |        ${entries.joinN().shift(8).trim}
         |    ]
         |
         |    public func sameInVersions(_ typeId: String) -> [String] {
         |        return $metadataClassName.unmodified[typeId] ?? []
         |    }
         |}""".stripMargin

    val metaOutput = SwDefnTranslator.Output(
      s"$basename/baboon_metadata_${domainFileSuffix}$versionSuffix.swift",
      metaTree,
      pkg,
      CompilerProduct.Definition,
      imports = Set("BaboonRuntime"),
    )

    F.pure(List(metaOutput))
  }

  private def sharedFixture(): Out[List[SwDefnTranslator.Output]] = {
    def fix(path: String, resource: String): SwDefnTranslator.Output = {
      SwDefnTranslator.Output(
        path,
        TextTree.text(IzResources.readAsString(resource).get),
        SwTypes.baboonFixturePkg,
        CompilerProduct.FixtureRuntime,
        doNotModify = true,
      )
    }
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      F.pure(
        List(
          fix("BaboonRuntime/baboon_fixture.swift", "baboon-runtime/swift/baboon_fixture.swift"),
        )
      )
    } else F.pure(Nil)
  }

  private def renderTree(o: SwDefnTranslator.Output): String = {
    val rendered = o.tree.mapRender {
      case t: SwValue.SwTypeName => trans.escapeSwiftKeyword(t.name)
      case t: SwValue.SwType if t.fq =>
        val moduleName = t.pkg.parts.head
        val escapedName = t.name.split('.').map(trans.escapeSwiftKeyword).mkString(".")
        s"$moduleName.$escapedName"
      case t: SwValue.SwType =>
        t.name.split('.').map(trans.escapeSwiftKeyword).mkString(".")
    }

    if (o.doNotModify) {
      rendered
    } else {
      val usedTypes = o.tree.values.collect { case t: SwValue.SwType => t }.distinct
        .filterNot(_.predef)
        .filterNot(_.fq)
        .sortBy(_.toString)

      val needsFoundation = usedTypes.exists(_.pkg == swFoundationPkg) ||
        usedTypes.exists(t => t.pkg == baboonRuntimePkg || t.pkg == baboonFixturePkg)

      val importLines = collection.mutable.LinkedHashSet[String]()
      if (needsFoundation) importLines += "import Foundation"
      o.imports.toList.sorted.foreach(m => importLines += s"import $m")

      if (importLines.nonEmpty) {
        importLines.mkString("\n") + "\n\n" + rendered
      } else {
        rendered
      }
    }
  }

  private def sharedRuntime(): Out[List[SwDefnTranslator.Output]] = {
    def rt(path: String, resource: String): SwDefnTranslator.Output = {
      SwDefnTranslator.Output(
        path,
        TextTree.text(IzResources.readAsString(resource).get),
        SwTypes.baboonRuntimePkg,
        CompilerProduct.Runtime,
        doNotModify = true,
      )
    }
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          rt("BaboonRuntime/baboon_runtime.swift", "baboon-runtime/swift/baboon_runtime.swift"),
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
    defnOut: List[SwDefnTranslator.Output],
  ): Out[List[SwDefnTranslator.Output]] = {
    val pkg = trans.toSwPkg(domain.id, domain.version, lineage.evolution)

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
        q"""public protocol RequiredConversions {
           |    ${missing.joinN().shift(4).trim}
           |}
           |""".stripMargin
      } else q""

      val ctorParam = if (missing.nonEmpty) {
        q"public let required: RequiredConversions"
      } else q""

      val ctorParamDecl = if (missing.nonEmpty) "_ required: RequiredConversions" else ""
      val ctorPrefix = if (missing.nonEmpty) "" else "override "

      val basename = swFiles.basename(domain, lineage.evolution)
      val versionSuffix = outputVersionSuffix(domain, lineage.evolution)
      val domainSuffix = domainClassSuffix(domain)
      val domainFileSuffix = trans.toSnakeCase(domainSuffix)
      val conversionsClassName = s"BaboonConversions_${domainSuffix}$versionSuffix"

      val oldVersionModules = toCurrent.map(step => trans.domainModuleName(domain.id, step.from, lineage.evolution)).toSet
      val conversionImports = Set("BaboonRuntime") ++ oldVersionModules

      val converter =
        q"""public class $conversionsClassName: $baboonAbstractConversions {
           |    $ctorParam
           |
           |    ${missingIface.shift(4).trim}
           |
           |    ${ctorPrefix}public init($ctorParamDecl) {
           |        ${if (missing.nonEmpty) q"self.required = required" else q""}
           |        super.init()
           |        ${conversionRegs.joinN().shift(8).trim}
           |    }
           |
           |    override public var versionsFrom: [String] { [${toCurrent.map(_.from.v.toString).map(v => s""""$v"""").mkString(", ")}] }
           |    override public var versionTo: String { "${domain.version.v.toString}" }
           |}""".stripMargin

      import izumi.fundamentals.collections.IzCollections.*
      val regsMap = defnOut.flatMap(_.codecReg).toMultimap.view.mapValues(_.flatten).toMap

      val converterOutput = SwDefnTranslator.Output(
        s"$basename/baboon_conversions_${domainFileSuffix}$versionSuffix.swift",
        converter,
        pkg,
        CompilerProduct.Conversion,
        imports = conversionImports,
      )

      val codecOutputs = regsMap.map {
        case (codecId, regs) =>
          val className = s"BaboonCodecs${codecId.capitalize}_${domainSuffix}$versionSuffix"
          val codecTree =
            q"""public class $className: ${SwValue.SwType(baboonRuntimePkg, s"AbstractBaboon${codecId}Codecs")} {
               |    override public init() {
               |        super.init()
               |        ${regs.toList.map(c => q"register($c)").joinN().shift(8).trim}
               |    }
               |}""".stripMargin
          SwDefnTranslator.Output(
            s"$basename/${trans.toSnakeCase(className)}.swift",
            codecTree,
            pkg,
            CompilerProduct.Conversion,
            imports = conversionImports,
          )
      }.toList

      val convertersOutput = convs.map {
        conv =>
          SwDefnTranslator.Output(
            s"$basename/${conv.fname}",
            conv.conv,
            pkg,
            CompilerProduct.Conversion,
            imports = conversionImports,
          )
      }

      List(converterOutput) ++ codecOutputs ++ convertersOutput
    }
  }

  private def outputVersionSuffix(domain: Domain, evolution: BaboonEvolution): String = {
    if (evolution.latest == domain.version) {
      ""
    } else {
      val normalizedVersion = domain.version.v.toString.map {
        case c if c.isLetterOrDigit => c
        case _                      => '_'
      }
      s"_v_$normalizedVersion"
    }
  }

  private def domainClassSuffix(domain: Domain): String = {
    domain.id.path.toList.map(toTypeSegment).mkString("_")
  }

  private def toTypeSegment(raw: String): String = {
    val normalized = raw.map {
      case c if c.isLetterOrDigit => c
      case _                      => '_'
    }
    val prefixed = if (normalized.nonEmpty && normalized.head.isDigit) {
      s"T_$normalized"
    } else {
      normalized
    }
    val parts = prefixed.split('_').toList.filter(_.nonEmpty)
    if (parts.isEmpty) {
      "T"
    } else {
      parts.map(p => p.head.toUpper + p.tail).mkString("_")
    }
  }
}

object SwBaboonTranslator {
  case class RenderedConversion(
    fname: String,
    conv: TextTree[SwValue],
    reg: Option[TextTree[SwValue]],
    missing: Option[TextTree[SwValue]],
  )
}
