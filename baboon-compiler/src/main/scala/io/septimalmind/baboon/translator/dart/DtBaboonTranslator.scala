package io.septimalmind.baboon.translator.dart

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.DtTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import io.septimalmind.baboon.translator.BaboonRuntimeResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class DtBaboonTranslator[F[+_, +_]: Error2](
  trans: DtTypeTranslator,
  convTransFac: DtConversionTranslator.Factory[F],
  defnTranslator: Subcontext[DtDefnTranslator[F]],
  target: DtTarget,
  dtTreeTools: DtTreeTools,
  dtFiles: DtFileTools,
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
  ): Out[List[DtDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[DtDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: DomainMember.User => F[NEList[BaboonIssue], List[DtDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[DtDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[DtDefnTranslator.Output]] = {
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

  private def generateMeta(domain: Domain, lineage: BaboonLineage): Out[List[DtDefnTranslator.Output]] = {
    val basename = dtFiles.basename(domain, lineage.evolution)
    val pkg      = trans.toDtPkg(domain.id, domain.version, lineage.evolution)

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q"""'${tid.toString}': [${version.sameIn.map(_.v.toString).map(s => q"'$s'").toList.join(", ")}],"""
      }

    val metaTree =
      q"""class BaboonMetadata {
         |  static const Map<String, List<String>> _unmodified = {
         |    ${entries.joinN().shift(4).trim}
         |  };
         |
         |  List<String> sameInVersions(String typeId) {
         |    return _unmodified[typeId] ?? [];
         |  }
         |}""".stripMargin

    val metaOutput = DtDefnTranslator.Output(s"$basename/baboon_metadata.dart", metaTree, pkg, CompilerProduct.Definition)

    F.pure(List(metaOutput))
  }

  private def sharedFixture(): Out[List[DtDefnTranslator.Output]] = {
    def fix(path: String, resource: String): DtDefnTranslator.Output = {
      DtDefnTranslator.Output(
        path,
        TextTree.text(BaboonRuntimeResources.read(resource)),
        DtTypes.baboonFixturePkg,
        CompilerProduct.FixtureRuntime,
        doNotModify = true,
      )
    }
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      F.pure(
        List(
          fix("baboon_fixture.dart", "baboon-runtime/dart/baboon_fixture.dart")
        )
      )
    } else F.pure(Nil)
  }

  private def renderTree(o: DtDefnTranslator.Output): String = {
    if (o.doNotModify) {
      return o.tree.mapRender {
        case t: DtValue.DtTypeName => trans.escapeDartKeyword(t.name)
        case t: DtValue.DtType if t.fq =>
          (t.pkg.parts :+ trans.escapeDartKeyword(t.name)).mkString(".")
        case t: DtValue.DtType =>
          trans.escapeDartKeyword(t.name)
      }
    }

    val currentDir      = o.path.split('/').dropRight(1).mkString("/")
    val currentFileName = o.path.split('/').last.stripSuffix(".dart")
    val allUsedTypes = o.tree.values.collect { case t: DtValue.DtType => t }.distinct
      .filterNot(_.predef)
      .sortBy(_.toString)

    val (fqTypes, normalTypes) = allUsedTypes.partition(_.fq)

    val filePrefixMap = buildFilePrefixMap(normalTypes)
    val fqPrefixMap   = buildFqPrefixMap(fqTypes)

    val rendered = o.tree.mapRender {
      case t: DtValue.DtTypeName => trans.escapeDartKeyword(t.name)
      case t: DtValue.DtType if t.fq =>
        fqPrefixMap.get(fqFileKey(t)) match {
          case Some(prefix) => s"$prefix.${trans.escapeDartKeyword(t.name)}"
          case None         => trans.escapeDartKeyword(t.name)
        }
      case t: DtValue.DtType =>
        filePrefixMap.get(dtFileKey(t)) match {
          case Some(prefix) => s"$prefix.${trans.escapeDartKeyword(t.name)}"
          case None         => trans.escapeDartKeyword(t.name)
        }
    }

    val normalImports = normalTypes.flatMap {
      p => resolveImport(p, o.module, currentFileName, filePrefixMap)
    }
    val fqImports = fqTypes.flatMap {
      p => resolveFqImport(p, currentDir, fqPrefixMap)
    }
    val importLines = (normalImports ++ fqImports).distinct.sorted

    if (importLines.isEmpty) {
      rendered
    } else {
      val importsBlock = importLines.mkString("\n")
      s"$importsBlock\n\n$rendered"
    }
  }

  private def dtFileKey(t: DtValue.DtType): String = {
    val fileName = t.importAs.getOrElse(trans.toSnakeCase(t.name))
    s"${t.pkg.parts.toList.mkString("/")}/$fileName"
  }

  private def fqFileKey(t: DtValue.DtType): String = {
    val fileName = t.importAs.getOrElse(trans.toSnakeCase(t.name))
    s"${t.pkg.parts.toList.mkString("/")}/$fileName"
  }

  private def buildFqPrefixMap(fqTypes: Seq[DtValue.DtType]): Map[String, String] = {
    fqTypes.map {
      t =>
        val key        = fqFileKey(t)
        val fileName   = t.importAs.getOrElse(trans.toSnakeCase(t.name))
        val pkgParts   = t.pkg.parts.toList
        val versionIdx = pkgParts.indexWhere(p => p.startsWith("v") && p.length > 1 && p.lift(1).exists(_.isDigit))
        val prefixParts =
          if (versionIdx >= 0) pkgParts.drop(versionIdx) :+ fileName
          else pkgParts :+ fileName
        key -> prefixParts.mkString("_")
    }.toMap
  }

  private def moduleSegmentToFilesystem(segment: String): String = {
    if (segment.matches("v\\d+(_\\d+)*")) {
      segment.stripPrefix("v").replace('_', '.')
    } else segment
  }

  private def resolveFqImport(
    t: DtValue.DtType,
    currentDir: String,
    fqPrefixMap: Map[String, String],
  ): Option[String] = {
    if (t.pkg == dartCorePkg) {
      None // dart:core is implicitly imported
    } else if (t.pkg == baboonRuntimePkg) {
      Some("import 'package:baboon_runtime/baboon_runtime.dart';")
    } else if (t.pkg == baboonFixturePkg) {
      Some("import 'package:baboon_runtime/baboon_fixture.dart';")
    } else {
      val fileName     = t.importAs.getOrElse(trans.toSnakeCase(t.name))
      val typePath     = t.pkg.parts.toList.map(moduleSegmentToFilesystem).mkString("/")
      val fullFilePath = s"$typePath/$fileName.dart"
      val relativePath = makeRelativePath(currentDir, fullFilePath)
      val key          = fqFileKey(t)
      val prefix       = fqPrefixMap.getOrElse(key, "fq")
      Some(s"import '$relativePath' as $prefix;")
    }
  }

  private def buildFilePrefixMap(usedTypes: Seq[DtValue.DtType]): Map[String, String] = {
    val nameConflicts = usedTypes
      .groupBy(t => trans.escapeDartKeyword(t.name))
      .filter(_._2.size > 1)
      .keySet

    val conflictingFileKeys = usedTypes
      .filter(t => nameConflicts.contains(trans.escapeDartKeyword(t.name)))
      .map(dtFileKey)
      .distinct

    conflictingFileKeys.map {
      key =>
        val parts  = key.split('/').filter(_.nonEmpty)
        val prefix = parts.takeRight(2).mkString("_")
        key -> prefix
    }.toMap
  }

  private def resolveImport(
    t: DtValue.DtType,
    currentModule: DtValue.DtPackageId,
    currentFileName: String,
    filePrefixMap: Map[String, String],
  ): Option[String] = {
    if (t.pkg == dartCorePkg) {
      None // dart:core is implicitly imported
    } else if (t.pkg == baboonRuntimePkg) {
      Some("import 'package:baboon_runtime/baboon_runtime.dart';")
    } else if (t.pkg == baboonFixturePkg) {
      Some("import 'package:baboon_runtime/baboon_fixture.dart';")
    } else if (t.pkg == dartTypedDataPkg) {
      Some("import 'dart:typed_data';")
    } else if (t.pkg == dartConvertPkg) {
      Some("import 'dart:convert';")
    } else if (t.pkg == dartIoPkg) {
      Some("import 'dart:io';")
    } else {
      val typePath    = t.pkg.parts.toList.mkString("/")
      val fileName    = t.importAs.getOrElse(trans.toSnakeCase(t.name))
      val currentPath = currentModule.parts.toList.mkString("/")
      val asClause    = filePrefixMap.get(dtFileKey(t)).map(p => s" as $p").getOrElse("")
      if (typePath == currentPath) {
        if (fileName == currentFileName) {
          None // Skip self-import
        } else {
          Some(s"import '$fileName.dart'$asClause;")
        }
      } else {
        val relativePath = makeRelativePath(currentPath, s"$typePath/$fileName.dart")
        Some(s"import '$relativePath'$asClause;")
      }
    }
  }

  private def makeRelativePath(fromDir: String, toFile: String): String = {
    val fromParts = fromDir.split('/').toList
    val toParts   = toFile.split('/').toList

    val commonLen = fromParts.zip(toParts).takeWhile { case (a, b) => a == b }.length
    val ups       = fromParts.length - commonLen
    val remaining = toParts.drop(commonLen)

    val prefix = if (ups == 0) "./" else "../" * ups
    prefix + remaining.mkString("/")
  }

  private def sharedRuntime(): Out[List[DtDefnTranslator.Output]] = {
    def rt(path: String, resource: String): DtDefnTranslator.Output = {
      DtDefnTranslator.Output(
        path,
        TextTree.text(BaboonRuntimeResources.read(resource)),
        DtTypes.baboonRuntimePkg,
        CompilerProduct.Runtime,
        doNotModify = true,
      )
    }
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          rt("baboon_runtime.dart", "baboon-runtime/dart/baboon_runtime.dart")
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
    defnOut: List[DtDefnTranslator.Output],
  ): Out[List[DtDefnTranslator.Output]] = {
    val pkg = trans.toDtPkg(domain.id, domain.version, lineage.evolution)

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
        q"""abstract class RequiredConversions {
           |  ${missing.joinN().shift(2).trim}
           |}
           |""".stripMargin
      } else q""

      val ctorParam = if (missing.nonEmpty) {
        q"final RequiredConversions required;"
      } else q""

      val ctorParamDecl = if (missing.nonEmpty) "this.required" else ""
      val ctorBody = if (missing.nonEmpty) {
        q""
      } else q""

      val converter =
        q"""$missingIface
           |class BaboonConversions extends $baboonAbstractConversions {
           |  $ctorParam
           |
           |  BaboonConversions($ctorParamDecl) {
           |    ${ctorBody.shift(4).trim}
           |    ${conversionRegs.joinN().shift(4).trim}
           |  }
           |
           |  List<String> get versionsFrom => [${toCurrent.map(_.from.v.toString).map(v => s"'$v'").mkString(", ")}];
           |  String get versionTo => '${domain.version.v.toString}';
           |}""".stripMargin

      import izumi.fundamentals.collections.IzCollections.*
      val regsMap = defnOut.flatMap(_.codecReg).toMultimap.view.mapValues(_.flatten).toMap

      val basename = dtFiles.basename(domain, lineage.evolution)

      val converterOutput = DtDefnTranslator.Output(
        s"$basename/baboon_conversions.dart",
        converter,
        pkg,
        CompilerProduct.Conversion,
      )

      val codecOutputs = regsMap.map {
        case (codecId, regs) =>
          val className = s"BaboonCodecs${codecId.capitalize}"
          val codecTree =
            q"""class $className extends ${DtValue.DtType(baboonRuntimePkg, s"AbstractBaboon${codecId}Codecs")} {
               |  $className() {
               |    ${regs.toList.map(c => q"register($c);").joinN().shift(4).trim}
               |  }
               |}""".stripMargin
          DtDefnTranslator.Output(
            s"$basename/${trans.toSnakeCase(className)}.dart",
            codecTree,
            pkg,
            CompilerProduct.Conversion,
          )
      }.toList

      val convertersOutput = convs.map {
        conv =>
          DtDefnTranslator.Output(
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

object DtBaboonTranslator {
  case class RenderedConversion(
    fname: String,
    conv: TextTree[DtValue],
    reg: Option[TextTree[DtValue]],
    missing: Option[TextTree[DtValue]],
  )
}
