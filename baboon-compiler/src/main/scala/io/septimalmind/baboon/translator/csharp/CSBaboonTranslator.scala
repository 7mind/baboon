package io.septimalmind.baboon.translator.csharp

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.csharp.CSDefnTranslator.OutputOrigin
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import io.septimalmind.baboon.translator.BaboonRuntimeResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSBaboonTranslator[F[+_, +_]: Error2](
  trans: CSTypeTranslator,
  convTransFac: CSConversionTranslator.Factory[F],
  target: CSTarget,
  csTrees: CSTreeTools,
  csFiles: CSFileTools,
  translator: Subcontext[CSDefnTranslator[F]],
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): Out[Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime()
      fixture    <- sharedFixture()
      rendered = (translated ++ runtime ++ fixture).filterNot(_.tree.isEmpty).map {
        o =>
          val content = renderTree(o, family)
          (o.path, OutputFile(content, o.product))
      }
      unique <- F.fromEither(rendered.toUniqueMap(c => BaboonIssue.of(TranslationIssue.NonUniqueOutputFiles(c))))
    } yield {
      Sources(unique)
    }
  }

  private def renderTree(o: CSDefnTranslator.Output, family: BaboonFamily): String = {
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

    val usedPackages = o.tree.toList
      .flatMap(_.values).collect { case t: CSValue.CSType => t }
      .filterNot(t => trans.isUpgradeable(t, family).nonEmpty).map(_.pkg).distinct
      .sortBy(_.parts.mkString("."))

    val available        = Set(o.pkg)
    val requiredPackages = Set.empty
    val allPackages      = (requiredPackages ++ usedPackages ++ forcedUses).diff(available ++ alwaysAvailable)

    val imports = allPackages.toSeq.map {
      case p if p.isStatic => q"using static ${p.parts.mkString(".")};"
      case p               => q"using ${p.parts.mkString(".")};"
    }.join("\n")

    val full = if (o.doNotModify) {
      o.tree.toSeq
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
        o.tree.toSeq,
      ).flatten
    }

    full.join("\n\n").mapRender {
      case t: CSValue.CSTypeName =>
        t.name

      case t: CSValue.CSType =>
        renderType(t, o, family)
    }
  }

  def renderType(tpe: CSValue.CSType, o: CSDefnTranslator.Output, family: BaboonFamily): String = {
    trans.isUpgradeable(tpe, family) match {
      case Some(higherTwin) =>
        renderSimpleType(higherTwin, o)
      case None =>
        renderSimpleType(tpe, o)
    }
  }

  private def renderSimpleType(tpe: CSValue.CSType, o: CSDefnTranslator.Output) = {
    tpe match {
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
    translate: (DomainMember.User) => F[NEList[BaboonIssue], List[CSDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[CSDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
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
    translator
      .provide(domain)
      .provide(evo)
      .provide(lineage)
      .produce()
      .use {
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

  private def generateMeta(domain: Domain, lineage: BaboonLineage): Out[List[CSDefnTranslator.Output]] = {
    val basename = csFiles.basename(domain, lineage.evolution)
    val pkg      = trans.toCsPkg(domain.id, domain.version, lineage.evolution)

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q"""_unmodified.Add("${tid.toString}", new $csList<$csString> { ${version.sameIn.map(_.v.toString).map(s => q"\"$s\"").toList.join(", ")} });"""
      }

    val metaTree =
      q"""public sealed class BaboonMeta : $iBaboonMeta
         |{
         |    private BaboonMeta()
         |    {
         |        ${entries.join("\n").shift(8).trim}
         |    }
         |
         |    public $csIReadOnlyList<$csString> SameInVersions($csString typeIdString)
         |    {
         |        return _unmodified[typeIdString];
         |    }
         |
         |    private readonly $csIDictionary<$csString, $csList<$csString>> _unmodified = new $csDictionary<$csString, $csList<$csString>>();
         |
         |    private static readonly $csLazy<BaboonMeta> LazyInstance = new $csLazy<BaboonMeta>(() => new BaboonMeta());
         |
         |    public static BaboonMeta Instance { get { return LazyInstance.Value; } }
         |}""".stripMargin

    val metaSource = Seq(metaTree).join("\n\n")
    val meta       = csTrees.inNs(pkg.parts.toSeq, metaSource)
    val metaOutput = CSDefnTranslator.Output(
      s"$basename/BaboonMeta.cs",
      Some(meta),
      pkg,
      CompilerProduct.Definition,
      origin = OutputOrigin.Runtime,
    )

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
                convTransFac(
                  pkg     = pkg,
                  srcDom  = lineage.versions(srcVer.from),
                  domain  = domain,
                  rules   = rules,
                  lineage = lineage,
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
           |        return new $csList<$csString> { ${toCurrent.map(_.from.v.toString).map(v => s"\"$v\"").mkString(", ")} };
           |    }
           |
           |    public override $csString VersionTo()
           |    {
           |        return "${domain.version.v.toString}";
           |    }
           |}""".stripMargin

      import izumi.fundamentals.collections.IzCollections.*

      val regsMap = defnOut.flatMap(_.codecReg).toMultimap.view.mapValues(_.flatten).toMap

      val codecs = regsMap.map {
        case (codecid, regs) =>
          val nme = q"BaboonCodecs${codecid.capitalize}"
          q"""public sealed class $nme : ${abstractBaboonCodecs(codecid)}
             |{
             |    private $nme()
             |    {
             |        ${regs.toList.map(r => q"Register($r);").join("\n").shift(8).trim}
             |    }
             |
             |    private static readonly $csLazy<$nme> LazyInstance = new $csLazy<$nme>(() => new $nme());
             |
             |    public static $nme Instance { get { return LazyInstance.Value; } }
             |}""".stripMargin
      }.toList.join("\n\n")

      val basename = csFiles.basename(domain, lineage.evolution)

      val runtimeSource = Seq(converter, codecs).join("\n\n")
      val runtime       = csTrees.inNs(pkg.parts.toSeq, runtimeSource)
      val runtimeOutput = CSDefnTranslator.Output(
        s"$basename/BaboonRuntime.cs",
        Some(runtime),
        pkg,
        CompilerProduct.Conversion,
        origin = OutputOrigin.Runtime,
      )

      val convertersOutput = convs.map {
        conv =>
          CSDefnTranslator.Output(
            s"$basename/${conv.fname}",
            Some(conv.conv),
            pkg,
            CompilerProduct.Conversion,
            origin = OutputOrigin.Runtime,
          )
      }

      List(runtimeOutput) ++ convertersOutput
    }
  }

  private def sharedRuntime(): Out[List[CSDefnTranslator.Output]] = {
    def rt(path: String, res: String) = {
      CSDefnTranslator.Output(
        path,
        Some(TextTree.text(BaboonRuntimeResources.read(res))),
        CSTypes.baboonRuntimePkg,
        CompilerProduct.Runtime,
        doNotModify = true,
        origin      = OutputOrigin.Runtime,
      )
    }
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          rt(s"BaboonRuntimeShared.cs", "baboon-runtime/cs/BaboonRuntimeShared.cs"),
          rt(s"BaboonCodecs.cs", "baboon-runtime/cs/BaboonCodecs.cs"),
          rt(s"BaboonConversions.cs", "baboon-runtime/cs/BaboonConversions.cs"),
          rt(s"BaboonTools.cs", "baboon-runtime/cs/BaboonTools.cs"),
          rt(s"BaboonTime.cs", "baboon-runtime/cs/BaboonTime.cs"),
          rt(s"BaboonByteString.cs", "baboon-runtime/cs/BaboonByteString.cs"),
          rt(s"BaboonServiceWiring.cs", "baboon-runtime/cs/BaboonServiceWiring.cs"),
        )
      )
    } else {
      F.pure(List.empty)
    }
  }

  private def sharedFixture(): Out[List[CSDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      val testRuntime = CSDefnTranslator.Output(
        "BaboonFixtureShared.cs",
        Some(TextTree.text(BaboonRuntimeResources.read("baboon-runtime/cs/BaboonFixtureShared.cs"))),
        CSTypes.baboonFixturePkg,
        CompilerProduct.FixtureRuntime,
        doNotModify = true,
        origin      = OutputOrigin.Runtime,
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
