package io.septimalmind.baboon.translator.csharp

import distage.Subcontext
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.{CompilerOptions, CompilerProduct}
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSBaboonTranslator(
  trans: CSTypeTranslator,
  handler: Subcontext[IndividualConversionHandler],
  options: CompilerOptions,
  csTrees: CSTreeTools,
  csFiles: CSFileTools,
  translator: Subcontext[CSDefnTranslator],
) extends BaboonAbstractTranslator {

  type Out[T] = Either[NEList[BaboonIssue.TranslationIssue], T]

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
      unique <- rendered.toUniqueMap(c => NEList(BaboonIssue.NonUniqueOutputFiles(c)))
    } yield {
      Sources(unique)
    }
  }

  private def renderTree(o: CSDefnTranslator.Output): String = {
    val alwaysAvailable: Set[CSPackageId] = if (options.csOptions.disregardImplicitUsings) {
      Set.empty
    } else {
      Set(csSystemPkg, csCollectionsGenericPkg, csLinqPkg)
    }

    val forcedUses: Set[CSPackageId] = if (options.csOptions.disregardImplicitUsings) {
      Set(csLinqPkg)
    } else {
      Set.empty
    }

    val usedPackages = o.tree.values.collect { case t: CSValue.CSType if !t.fq => t.pkg }.distinct
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
               |// ReSharper disable EmptyStatement
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
        Seq(q"""#pragma warning disable 612,618
               |#pragma warning disable CS0108,CA1822
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
    // TODO: fix .toSeq.toList
    family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList.biFlatten
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[CSDefnTranslator.Output]] = {
    lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList.biFlatten
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[CSDefnTranslator.Output]] = {
    val evo = lineage.evolution
    translator.provide(domain).provide(evo).produce().use {
      defnTranslator =>
        for {
          defnSources <- {
            if (options.target.products.contains(CompilerProduct.Definition)) {
              domain.defs.meta.nodes.toList.map {
                case (_, defn: DomainMember.User) => defnTranslator.translate(defn)
                case _                            => Right(List.empty)
              }.biFlatten
            } else {
              Right(List.empty)
            }
          }

          fixturesSources <- {
            if (options.target.products.contains(CompilerProduct.Fixture)) {
              domain.defs.meta.nodes.toList.map {
                case (_, defn: DomainMember.User) => defnTranslator.translateFixtures(defn)
                case _                            => Right(List.empty)
              }.biFlatten
            } else {
              Right(List.empty)
            }
          }

          testsSources <- {
            if (options.target.products.contains(CompilerProduct.Test)) {
              domain.defs.meta.nodes.toList.map {
                case (_, defn: DomainMember.User) => defnTranslator.translateTests(defn)
                case _                            => Right(List.empty)
              }.biFlatten
            } else {
              Right(List.empty)
            }
          }

          conversionSources <- {
            if (options.target.products.contains(CompilerProduct.Conversion)) {
              val evosToCurrent = evo.diffs.keySet.filter(_.to == domain.version)
              generateConversions(domain, lineage, evosToCurrent, defnSources)
            } else {
              Right(List.empty)
            }
          }

          meta <- {
            if (options.csOptions.writeEvolutionDict) {
              generateMeta(domain, lineage)
            } else {
              Right(List.empty)
            }
          }
        } yield {
          defnSources.map(_.output) ++
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
         |    private readonly $csDict<$csString, $csString> _unmodified = new ();
         |
         |    private static readonly $csLazy<BaboonMeta> LazyInstance = new $csLazy<BaboonMeta>(() => new BaboonMeta());
         |
         |    public static BaboonMeta Instance { get { return LazyInstance.Value; } }
         |}""".stripMargin

    val metaSource = Seq(metaTree).join("\n\n")
    val meta       = csTrees.inNs(pkg.parts.toSeq, metaSource)
    val metaOutput = CSDefnTranslator.Output(s"$basename/BaboonMeta.cs", meta, pkg, CompilerProduct.Definition)

    Right(List(metaOutput))
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
    defnOut: List[CSDefnTranslator.OutputExt],
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
           |        ${defnOut.map(_.codecReg).join("\n").shift(8).trim}
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

    Right(List(sharedOutput, timeOutput))
  }

  private def sharedFixture(): Out[List[CSDefnTranslator.Output]] = {
    val testRuntime = CSDefnTranslator.Output(
      "BaboonFixtureShared.cs",
      TextTree.text(IzResources.readAsString("baboon-runtime/cs/BaboonFixtureShared.cs").get),
      CSTypes.baboonFixturePkg,
      CompilerProduct.FixtureRuntime,
      doNotModify = true,
    )

    Right(List(testRuntime))
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
