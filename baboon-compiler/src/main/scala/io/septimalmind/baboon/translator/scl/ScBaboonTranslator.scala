package io.septimalmind.baboon.translator.scl

import distage.Subcontext
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile, Sources, scl}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import io.septimalmind.baboon.translator.BaboonRuntimeResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class ScBaboonTranslator[F[+_, +_]: Error2](
  trans: ScTypeTranslator,
  convTransFac: ScConversionTranslator.Factory[F],
  defnTranslator: Subcontext[ScDefnTranslator[F]],
  target: ScTarget,
  scTreeTools: ScTreeTools,
  scFiles: ScFileTools,
) extends BaboonAbstractTranslator[F] {

  type Out[T] = F[NEList[BaboonIssue], T]

  override def translate(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    for {
      translated <- translateFamily(family)
      runtime    <- sharedRuntime()
      fixture    <- sharedFixture()
      testHelper <- sharedTestHelper()
      rendered = (translated ++ runtime ++ fixture ++ testHelper).map {
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
  ): Out[List[ScDefnTranslator.Output]] = {
    F.flatSequenceAccumErrors {
      family.domains.iterator.map { case (_, lineage) => translateLineage(lineage) }.toList
    }
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[ScDefnTranslator.Output]] = {
    for {
      perVersion <- F.flatSequenceAccumErrors {
        lineage.versions.iterator.map { case (_, domain) => translateDomain(domain, lineage) }.toList
      }
      facade <- generateDomainFacade(lineage)
    } yield perVersion ++ facade
  }

  private def generateDomainFacade(lineage: BaboonLineage): Out[List[ScDefnTranslator.Output]] = {
    if (!target.language.generateDomainFacade || !target.output.products.contains(CompilerProduct.Conversion)) {
      F.pure(List.empty)
    } else {
      val domainPkg    = trans.toScPkg(lineage.pkg, lineage.evolution.latest, omitVersion = true)
      val domainIdStr  = lineage.pkg.path.mkString(".")
      val facadeName   = "Domain" + lineage.pkg.path.map(s => s.capitalize).mkString + "Facade"

      val withMeta = target.language.writeEvolutionDict

      val registrations = lineage.versions.iterator.toList.sortBy(_._1).map {
        case (version, _) =>
          val versionedPkg = trans.toScPkg(lineage.pkg, version, lineage.evolution)
          // Use fully-qualified `_root_.…` references because every per-version package emits
          // identically-named singletons (`BaboonCodecsJson` / `…Ueba` / `BaboonMetadata`); a
          // simple import would collide and the renderer would silently use the same singleton
          // for every register call. Forcing FQN-with-_root_ sidesteps the import collector.
          val verPath        = versionedPkg.parts.mkString(".")
          val jsonCodecsRef  = q"_root_.$verPath.BaboonCodecsJson"
          val uebaCodecsRef  = q"_root_.$verPath.BaboonCodecsUeba"
          val domainVersionExpr =
            q"""$baboonDomainVersion("$domainIdStr", "${version.v.toString}")"""
          // `locally { … }` wrapper avoids Scala 2.13 + `-Wconf:any:error` rejection of
          // `val _ = …` for Unit-typed expressions inside object bodies. The 4-arg overload
          // (with `BaboonMetadata`) only exists when `writeEvolutionDict=true`; otherwise the
          // 3-arg overload is the only valid choice.
          if (withMeta) {
            val metadataRef = q"_root_.$verPath.BaboonMetadata"
            q"""locally { register($domainVersionExpr, $jsonCodecsRef, $uebaCodecsRef, $metadataRef); () }"""
          } else {
            q"""locally { register($domainVersionExpr, $jsonCodecsRef, $uebaCodecsRef); () }"""
          }
      }

      val facadeTree =
        q"""object $facadeName extends $baboonCodecsFacade {
           |  ${registrations.joinN().shift(2).trim}
           |}""".stripMargin

      val facadeWrapped = scTreeTools.inNs(domainPkg.parts.toSeq, facadeTree)
      val basename      = lineage.pkg.path.map(_.toLowerCase).mkString("/")
      val facadeOutput  = ScDefnTranslator.Output(
        s"$basename/$facadeName.scala",
        facadeWrapped,
        domainPkg,
        CompilerProduct.Conversion,
      )

      F.pure(List(facadeOutput))
    }
  }

  private def translateProduct(
    domain: Domain,
    p: CompilerProduct,
    translate: DomainMember.User => F[NEList[BaboonIssue], List[ScDefnTranslator.Output]],
  ): F[NEList[BaboonIssue], List[ScDefnTranslator.Output]] = {
    if (target.output.products.contains(p)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, defn: DomainMember.User) => translate(defn)
        case _                            => F.pure(List.empty)
      }
    } else {
      F.pure(List.empty)
    }
  }

  private def translateDomain(domain: Domain, lineage: BaboonLineage): Out[List[ScDefnTranslator.Output]] = {
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

  private def generateMeta(domain: Domain, lineage: BaboonLineage): Out[List[ScDefnTranslator.Output]] = {
    val basename = scFiles.basename(domain, lineage.evolution)
    val pkg      = trans.toScPkg(domain.id, domain.version, lineage.evolution)

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q"""unmodified.put("${tid.toString}", $scList(${version.sameIn.map(_.v.toString).map(s => q"\"$s\"").toList.join(", ")}))"""
      }

    val metaTree =
      q"""object BaboonMetadata extends $baboonMeta {
         |  private val unmodified = ${scMutMap.fullyQualified}.empty[$scString, $scList[$scString]]
         |  
         |  ${entries.joinN().shift(2).trim}
         |
         |  def sameInVersions(typeId: String): List[String] = {
         |      unmodified(typeId)
         |  }
         |}""".stripMargin

    val meta       = scTreeTools.inNs(pkg.parts.toSeq, metaTree)
    val metaOutput = ScDefnTranslator.Output(s"$basename/BaboonMetadata.scala", meta, pkg, CompilerProduct.Definition)

    F.pure(List(metaOutput))
  }

  private def sharedFixture(): Out[List[ScDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.FixtureRuntime)) {
      F.pure(
        List(
          ScDefnTranslator.Output(
            "BaboonFixtureShared.scala",
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/scala/BaboonFixtureShared.scala")),
            ScTypes.baboonFixturePkg,
            CompilerProduct.FixtureRuntime,
            doNotModify = true,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def sharedTestHelper(): Out[List[ScDefnTranslator.Output]] = {
    if (target.output.products.contains(CompilerProduct.Test)) {
      F.pure(
        List(
          ScDefnTranslator.Output(
            "CrossLanguageFixturePath.scala",
            TextTree.text(BaboonRuntimeResources.read("baboon-runtime/scala/CrossLanguageFixturePath.scala")),
            ScTypes.baboonRuntimePkg,
            CompilerProduct.Test,
            doNotModify = true,
          )
        )
      )
    } else F.pure(Nil)
  }

  private def renderTree(o: ScDefnTranslator.Output): String = {
    val usedTypes = o.tree.values.collect { case t: ScValue.ScType => t }
      .filterNot(_.predef)
      .filterNot(_.fq)
      .filterNot(_.pkg == o.pkg)
      .sortBy(_.toString)
      .distinct

    val (samePkg, otherPkg) = usedTypes.partition(_.pkg.parts.startsWith(o.pkg.parts))

    val (sameNameOtherPkgs, diffNameOtherPkgs) =
      otherPkg.partition(tpe => usedTypes.count(used => used.name == tpe.name && used.inObject == tpe.inObject) > 1)

    val (sameNameThisPkg, diffNameThisPkg) =
      samePkg.partition(tpe => usedTypes.count(used => used.name == tpe.name && used.inObject == tpe.inObject) > 1)

    val sameNameSet = (sameNameThisPkg ++ sameNameOtherPkgs).toSet

    val imports = (diffNameThisPkg ++ diffNameOtherPkgs)
      .groupBy(_.pkg)
      .map {
        case (pkg, types) =>
          val (objectTypes, nonObjectTypes) = types.partition(_.inObject.nonEmpty)
          val objectNames = objectTypes.collect { case ScValue.ScType(pkgId, _, Some(obj), _, _) if !pkgId.parts.startsWith(o.pkg.parts) => obj }.distinct
          val all         = (objectNames ++ nonObjectTypes.map(_.name)).distinct
          val allImports =
            if (all.size == 1) {
              q".${all.head}"
            } else {
              q".{${all.mkString(", ")}}"
            }
          if (all.nonEmpty) {
            q"import ${pkg.parts.mkString(".")}$allImports"
          } else q""
      }.toList.joinN()

    val mappedTree =
      o.tree.map {
        case tpe: ScValue.ScType =>
          if (sameNameSet.contains(tpe)) tpe.fullyQualified
          else tpe
      }

    val full = if (o.doNotModify) {
      mappedTree
    } else {
      q"""$imports
         |
         |$mappedTree""".stripMargin
    }

    val oPkgParts = o.pkg.parts.toList
    full.mapRender {
      case t: ScValue.ScType if t.fq =>
        val typeParts = (t.pkg.parts :+ t.name).toList
        if (typeParts.startsWith(oPkgParts)) typeParts.drop(oPkgParts.size).mkString(".")
        else typeParts.mkString(".")
      case ScValue.ScType(_, name, Some(inObject), _, _) => s"$inObject.$name"
      case t: ScValue.ScType                             => t.name
    }
  }

  private def sharedRuntime(): Out[List[scl.ScDefnTranslator.Output]] = {
    def rt(path: String, resource: String, preprocessResource: String => String = identity): ScDefnTranslator.Output = {
      ScDefnTranslator.Output(
        path,
        TextTree.text(preprocessResource(BaboonRuntimeResources.read(resource))),
        ScTypes.baboonRuntimePkg,
        CompilerProduct.Runtime,
        doNotModify = true,
      )
    }
    // TODO: fix in izumi, hack, we always force escape processing in text tree
    if (target.output.products.contains(CompilerProduct.Runtime)) {
      F.pure(
        List(
          rt("BaboonAnyOpaque.scala", "baboon-runtime/scala/BaboonAnyOpaque.scala"),
          rt("BaboonByteString.scala", "baboon-runtime/scala/BaboonByteString.scala", _.replace("""[\\s:-]""", """[\\\\s:-]""")),
          rt("BaboonCodecs.scala", "baboon-runtime/scala/BaboonCodecs.scala"),
          rt("BaboonCodecsFacade.scala", "baboon-runtime/scala/BaboonCodecsFacade.scala"),
          rt("BaboonConversions.scala", "baboon-runtime/scala/BaboonConversions.scala"),
          rt("BaboonExceptions.scala", "baboon-runtime/scala/BaboonExceptions.scala"),
          rt("BaboonRuntimeShared.scala", "baboon-runtime/scala/BaboonRuntimeShared.scala", _.replace("""\\.""", """\\\\.""")),
          rt("BaboonServiceWiring.scala", "baboon-runtime/scala/BaboonServiceWiring.scala"),
          rt("BaboonTools.scala", "baboon-runtime/scala/BaboonTools.scala"),
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
    defnOut: List[ScDefnTranslator.Output],
  ): Out[List[scl.ScDefnTranslator.Output]] = {
    val pkg = trans.toScPkg(domain.id, domain.version, lineage.evolution)

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

      val requiredParam =
        if (missing.isEmpty) q"@scala.annotation.unused required: RequiredConversions"
        else q"required: RequiredConversions"

      val converter =
        q"""trait RequiredConversions {
           |    ${missing.joinN().shift(4).trim}
           |}
           |
           |class BaboonConversions($requiredParam) extends $baboonAbstractConversions {
           |    ${conversionRegs.joinN().shift(4).trim}
           |
           |    override def versionsFrom: $scList[$scString] = $scList(${toCurrent.map(_.from.v.toString).map(v => s"\"$v\"").mkString(", ")})
           |    override def versionTo: $scString = "${domain.version.v.toString}"
           |}""".stripMargin

      import izumi.fundamentals.collections.IzCollections.*
      val regsMap = defnOut.flatMap(_.codecReg).toMultimap.view.mapValues(_.flatten).toMap

      val codecs = regsMap.map {
        case (codecId, regs) =>
          q"""object BaboonCodecs${codecId.capitalize} extends ${abstractBaboonCodec(codecId)}{
             |  ${regs.toList.map(c => q"register($c)").joinN().shift(2).trim}
             |}""".stripMargin
      }.toList.joinNN()

      val basename = scFiles.basename(domain, lineage.evolution)

      val runtimeSource = Seq(converter, codecs).joinNN()
      val runtime       = scTreeTools.inNs(pkg.parts.toSeq, runtimeSource)
      val runtimeOutput = ScDefnTranslator.Output(
        s"$basename/BaboonRuntime.scala",
        runtime,
        pkg,
        CompilerProduct.Conversion,
      )

      val convertersOutput = convs.map {
        conv =>
          ScDefnTranslator.Output(
            s"$basename/${conv.fname}",
            conv.conv,
            pkg,
            CompilerProduct.Conversion,
          )
      }

      List(runtimeOutput) ++ convertersOutput
    }
  }
}

object ScBaboonTranslator {
  case class RenderedConversion(
    fname: String,
    conv: TextTree[ScValue],
    reg: Option[TextTree[ScValue]],
    missing: Option[TextTree[ScValue]],
  )
}
