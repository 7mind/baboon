package io.septimalmind.baboon.translator.csharp

import distage.Subcontext
import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.{CSOptions, CompilerOptions, CompilerProduct}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSBaboonTranslator[F[+_, +_]: Error2](
  trans: CSTypeTranslator,
  handler: CSConversionTranslator.Factory[F],
  target: CSTarget,
  csTrees: CSTreeTools,
  csFiles: CSFileTools,
  translator: Subcontext[CSDefnTranslator[F]],
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue.TranslationIssue], T]

  override def translate(family: BaboonFamily): Out[Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime()
      fixture    <- sharedFixture()
      rendered = (translated ++ runtime ++ fixture).map {
        o =>
          val content = renderTree(o)
          (o.path, OutputFile(content, o.product))
      }
      unique <- F.fromEither(rendered.toUniqueMap(c => NEList(BaboonIssue.NonUniqueOutputFiles(c))))
    } yield {
      Sources(unique)
    }
  }

  private def renderTree(o: CSDefnTranslator.Output): String = {
    val alwaysAvailable: Set[CSPackageId] = if (target.language.disregardImplicitUsings) {
      Set.empty
    } else {
      Set(csSystemPkg, csCollectionsGenericPkg, csLinqPkg)
    }

    val forcedUses: Set[CSPackageId] = if (target.language.disregardImplicitUsings) {
      Set(csLinqPkg, csCollectionsImmutablePkg, csCollectionsGenericPkg)
    } else {
      Set(csCollectionsImmutablePkg, csCollectionsGenericPkg)
    }

    val usedPackages = o.tree.values.collect { case t: CSValue.CSType => t.pkg }.distinct
      .sortBy(_.parts.mkString("."))

    val available        = Set(o.pkg)
    val requiredPackages = Set.empty
    val allPackages      = (requiredPackages ++ usedPackages ++ forcedUses).diff(available ++ alwaysAvailable)

    val imports = allPackages.toSeq.map {
      case p if p.isStatic => q"using static ${p.parts.mkString(".")};"
      case p               => q"using ${p.parts.mkString(".")};"
    }.join("\n")

    val full = if (o.doNotModify) {
      o.tree
    } else {
      Seq(
        Seq(q"""#region resharper
               |// ReSharper disable InconsistentNaming
               |// ReSharper disable CheckNamespace
               |// ReSharper disable IdentifierTypo
               |// ReSharper disable ClassWithVirtualMembersNeverInherited.Global
               |// ReSharper disable MemberHidesStaticFromOuterClass
               |// ReSharper disable BuiltInTypeReferenceStyle
               |// ReSharper disable RedundantNameQualifier
               |// ReSharper disable ArrangeObjectCreationWhenTypeEvident
               |// ReSharper disable ArrangeAccessorOwnerBody
               |// ReSharper disable RedundantExtendsListEntry
               |// ReSharper disable ConvertToUsingDeclaration
               |// ReSharper disable RedundantSuppressNullableWarningExpression
               |// ReSharper disable RedundantNullableDirective
               |// Re-Sharper disable EmptyStatement
               |// ReSharper disable ArrangeNamespaceBody
               |// ReSharper disable RedundantUsingDirective
               |// ReSharper disable UnusedMemberInSuper.Global
               |// ReSharper disable UnusedType.Global
               |// ReSharper disable RedundantTypeArgumentsOfMethod
               |// ReSharper disable SuggestVarOrType_Elsewhere
               |// ReSharper disable SuggestVarOrType_SimpleTypes
               |// ReSharper disable SuggestVarOrType_BuiltInTypes
               |// ReSharper disable UseCollectionExpression
               |// ReSharper disable UnusedMember.Global
               |// ReSharper disable MemberCanBePrivate.Global
               |// ReSharper disable ArrangeRedundantParentheses
               |// ReSharper disable ConvertClosureToMethodGroup
               |// ReSharper disable MemberCanBeMadeStatic.Local
               |// ReSharper disable MemberCanBeMadeStatic.Global
               |// ReSharper disable RedundantEmptyObjectOrCollectionInitializer
               |// ReSharper disable RedundantTypeDeclarationBody
               |// ReSharper disable RedundantEmptyObjectCreationArgumentList
               |// ReSharper disable ConvertIfStatementToSwitchStatement
               |// ReSharper disable InvertIf
               |// ReSharper disable RedundantCast
               |// ReSharper disable EmptyConstructor
               |// ReSharper disable ConvertIfStatementToReturnStatement
               |// ReSharper disable AccessToStaticMemberViaDerivedType
               |// ReSharper disable VariableHidesOuterVariable
               |// ReSharper disable MergeConditionalExpression
               |#endregion
               |""".stripMargin.trim),
        Seq(q"""#region warnings
               |#pragma warning disable CS0612 // do not warn on obsolete attr
               |#pragma warning disable CS0618 // do not warn on obsolete attr with message
               |#pragma warning disable CS0108 // do not warn about hiding inherited member
               |#pragma warning disable CA1822 // do not warn about members which may be made static
               |#endregion
               """.stripMargin.trim), // deprecation warnings
        Seq(q"#nullable enable"),
        Seq(imports),
        Seq(o.tree),
      ).flatten.join("\n\n")
    }

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

  private def translateFamily(
    family: BaboonFamily
  ): Out[List[CSDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      // TODO: fix .toSeq.toList
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[CSDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: (DomainMember.User) => F[NEList[BaboonIssue.TranslationIssue], List[CSDefnTranslator.Output]],
  ): F[NEList[BaboonIssue.TranslationIssue], List[CSDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Definition)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[CSDefnTranslator.Output]] = {
    val evo = lineage.evolution
    translator.provide(domain).provide(evo).produce().use {
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

  private def generateMeta(domain: Domain, lineage: BaboonLineage): Out[List[CSDefnTranslator.Output]] = {
    val basename = csFiles.basename(domain, lineage.evolution)
    val pkg      = trans.toCsPkg(domain.id, domain.version, lineage.evolution)

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
         |    private readonly $csIDictionary<$csString, $csString> _unmodified = new $csDictionary<$csString, $csString>();
         |
         |    private static readonly $csLazy<BaboonMeta> LazyInstance = new $csLazy<BaboonMeta>(() => new BaboonMeta());
         |
         |    public static BaboonMeta Instance { get { return LazyInstance.Value; } }
         |}""".stripMargin

    val metaSource = Seq(metaTree).join("\n\n")
    val meta       = csTrees.inNs(pkg.parts.toSeq, metaSource)
    val metaOutput = CSDefnTranslator.Output(s"$basename/BaboonMeta.cs", meta, pkg, CompilerProduct.Definition)

    F.pure(List(metaOutput))
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
    defnOut: List[CSDefnTranslator.Output],
  ): Out[List[CSDefnTranslator.Output]] = {
    val pkg = trans.toCsPkg(domain.id, domain.version, lineage.evolution)

    for {
      convs <-
        F.flatSequenceAccumErrors {
          lineage.evolution.rules
            .filter(kv => toCurrent.contains(kv._1))
            .map {
              case (srcVer, rules) =>
                handler(
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
        q"""public interface RequiredConversions {
           |    ${missing.join("\n").shift(4).trim}
           |}
           |
           |public sealed class BaboonConversions : $abstractBaboonConversions
           |{
           |    // ReSharper disable once UnusedParameter.Local
           |    public BaboonConversions(RequiredConversions requiredConversions)
           |    {
           |        ${conversionRegs.join("\n").shift(8).trim}
           |    }
           |
           |    public override $csList<$csString> VersionsFrom()
           |    {
           |        return new $csList<$csString> { ${toCurrent.map(_.from.version).map(v => s"\"$v\"").mkString(", ")} };
           |    }
           |
           |    public override $csString VersionTo()
           |    {
           |        return "${domain.version.version}";
           |    }
           |}""".stripMargin

      val codecs =
        q"""public sealed class BaboonCodecs : $abstractBaboonCodecs
           |{
           |    private BaboonCodecs()
           |    {
           |        ${defnOut.flatMap(_.codecReg).join("\n").shift(8).trim}
           |    }
           |
           |    private static readonly $csLazy<BaboonCodecs> LazyInstance = new $csLazy<BaboonCodecs>(() => new BaboonCodecs());
           |
           |    public static BaboonCodecs Instance { get { return LazyInstance.Value; } }
           |}""".stripMargin

      val basename = csFiles.basename(domain, lineage.evolution)

      val runtimeSource = Seq(converter, codecs).join("\n\n")
      val runtime       = csTrees.inNs(pkg.parts.toSeq, runtimeSource)
      val runtimeOutput = CSDefnTranslator.Output(
        s"$basename/BaboonRuntime.cs",
        runtime,
        pkg,
        CompilerProduct.Conversion,
      )

      val convertersOutput = convs.map {
        conv =>
          CSDefnTranslator.Output(
            s"$basename/${conv.fname}",
            conv.conv,
            pkg,
            CompilerProduct.Conversion,
          )
      }

      List(runtimeOutput) ++ convertersOutput
    }
  }

  private def sharedRuntime(): Out[List[CSDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      val sharedOutput = CSDefnTranslator.Output(
        s"BaboonRuntimeShared.cs",
        TextTree.text(IzResources.readAsString("baboon-runtime/cs/BaboonRuntimeShared.cs").get),
        CSTypes.baboonRuntimePkg,
        CompilerProduct.Runtime,
        doNotModify = true,
      )

      val timeOutput = CSDefnTranslator.Output(
        s"BaboonTime.cs",
        TextTree.text(IzResources.readAsString("baboon-runtime/cs/BaboonTime.cs").get),
        CSTypes.baboonTimePkg,
        CompilerProduct.Runtime,
        doNotModify = true,
      )

      F.pure(List(sharedOutput, timeOutput))
    } else {
      F.pure(List.empty)
    }
  }

  private def sharedFixture(): Out[List[CSDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      val testRuntime = CSDefnTranslator.Output(
        "BaboonFixtureShared.cs",
        TextTree.text(IzResources.readAsString("baboon-runtime/cs/BaboonFixtureShared.cs").get),
        CSTypes.baboonFixturePkg,
        CompilerProduct.FixtureRuntime,
        doNotModify = true,
      )

      F.pure(List(testRuntime))
    } else {
      F.pure(List.empty)
    }
  }
}

object CSBaboonTranslator {
  case class RenderedConversion(
    fname: String,
    conv: TextTree[CSValue],
    reg: Option[TextTree[CSValue]],
    missing: Option[TextTree[CSValue]],
  )
}
