package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.java.JvValue.JvType
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait JvDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[JvDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[JvDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[JvDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[JvDefnTranslator.Output]]
}

object JvDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: JvType,
    tpeKeepForeigns: JvType,
    tpeId: TextTree[JvValue],
    trees: Map[String, TextTree[JvValue]],
  )

  final case class CodecDef(
    className: String,
    owner: Owner,
    tree: TextTree[JvValue],
  )

  final case class DefnRepr(
    defn: TextTree[JvValue],
    codecDefs: List[CodecDef],
    codecs: List[CodecReg],
  )

  final case class Output(
    path: String,
    tree: TextTree[JvValue],
    pkg: JvValue.JvPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[JvValue]])] = Nil,
  )

  class JvDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: JvTarget,
    domain: Domain,
    evo: BaboonEvolution,
    jvFiles: JvFileTools,
    jvTrees: JvTreeTools,
    trans: JvTypeTranslator,
    codecs: Set[JvCodecTranslator],
    codecTests: JvCodecTestsTranslator,
    codecsFixture: JvCodecFixtureTranslator,
    wiringTranslator: JvServiceWiringTranslator,
    jvDomainTreeTools: JvDomainTreeTools,
  ) extends JvDefnTranslator[F] {
    import JvTypes.*

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr = makeFullRepr(defn, inPkg = true)

      val registrations = codecs.toList.map(codec => codec.id -> repr.codecs.flatMap(reg => reg.trees.get(codec.id).map(expr => q"${reg.tpeId}, $expr")))

      val pkg = effectivePkg(defn.defn.id.owner)

      val mainOutput = Output(
        getOutputPath(defn),
        repr.defn,
        pkg,
        CompilerProduct.Definition,
        codecReg = registrations,
      )

      val codecOutputs = repr.codecDefs.map { codecDef =>
        val fbase = jvFiles.basename(domain, evo)
        val codecPkg = effectivePkg(codecDef.owner)
        val subdir = ownerSubdir(codecDef.owner)
        val fname = s"${codecDef.className}.java"
        val wrapped = jvTrees.inPkg(codecPkg.parts.toSeq, codecDef.tree)
        Output(
          s"$fbase$subdir/$fname",
          wrapped,
          codecPkg,
          CompilerProduct.Definition,
        )
      }

      val wiringOutput = wiringTranslator.translate(defn).map { wiringTree =>
        val wrapped = jvTrees.inPkg(pkg.parts.toSeq, wiringTree)
        Output(
          getOutputPath(defn, suffix = Some("Wiring")),
          wrapped,
          pkg,
          CompilerProduct.Definition,
        )
      }.toList

      F.pure(mainOutput :: (codecOutputs ++ wiringOutput))
    }

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateFixtures(defn)
      }
    }

    private def doTranslateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val fixtureTreeOut = makeFixtureRepr(defn).map {
        fixtureTreeWithPkg =>
          Output(
            getOutputPath(defn, suffix = Some("_Fixture")),
            fixtureTreeWithPkg,
            effectivePkg(defn.defn.id.owner),
            CompilerProduct.Fixture,
          )
      }

      F.pure(fixtureTreeOut.toList)
    }

    private def makeFixtureRepr(defn: DomainMember.User): Option[TextTree[JvValue]] = {
      val srcRef = trans.toJvTypeRefKeepForeigns(defn.id, domain, evo)
      val ns     = srcRef.pkg.parts

      val fixtureTree        = codecsFixture.translate(defn)
      val fixtureTreeWithPkg = fixtureTree.map(t => jvTrees.inPkg(ns.toSeq, t))

      fixtureTreeWithPkg
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTest(defn)
      }
    }

    private def doTranslateTest(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val codecTestOut = makeTestRepr(defn).map {
        codecTestWithPkg =>
          Output(
            getOutputPath(defn, suffix = Some("_tests")),
            codecTestWithPkg,
            effectivePkg(defn.defn.id.owner),
            CompilerProduct.Test,
          )
      }

      F.pure(codecTestOut.toList)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map { tree =>
        val pkg = trans.toJvPkg(domain.id, domain.version, evo)
        val wrapped = jvTrees.inPkg(pkg.parts.toSeq, tree)
        val fbase = jvFiles.basename(domain, evo)
        Output(
          s"$fbase/BaboonServiceRt.java",
          wrapped,
          pkg,
          CompilerProduct.Definition,
        )
      }.toList
      F.pure(result)
    }

    private def makeTestRepr(defn: DomainMember.User): Option[TextTree[JvValue]] = {
      val jvTypeRef = trans.asJvType(defn.id, domain, evo)
      val srcRef    = trans.toJvTypeRefKeepForeigns(defn.id, domain, evo)
      val ns        = srcRef.pkg.parts

      val testTree        = codecTests.translate(defn, jvTypeRef, srcRef)
      val testTreeWithPkg = testTree.map(t => jvTrees.inPkg(ns.toSeq, t))

      testTreeWithPkg
    }

    private def makeFullRepr(
      defn: DomainMember.User,
      inPkg: Boolean,
    ): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      def deprecatePrevious(tree: TextTree[JvValue]): TextTree[JvValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""@Deprecated
             |$tree""".stripMargin
        }
      }

      val jvTypeRef = trans.asJvType(defn.id, domain, evo)
      val srcRef    = trans.toJvTypeRefKeepForeigns(defn.id, domain, evo)

      val repr = makeRepr(defn, jvTypeRef, isLatestVersion)

      val ownCodecDefs = codecs.toList.flatMap { c =>
        c.translate(defn, jvTypeRef, srcRef).map { tree =>
          CodecDef(c.codecName(srcRef, defn.defn.id.owner).name, defn.defn.id.owner, deprecatePrevious(tree))
        }
      }

      val defnRepr = deprecatePrevious(repr.defn)

      assert(defn.id.pkg == domain.id)

      val ns = srcRef.pkg.parts

      val content = if (inPkg) jvTrees.inPkg(ns.toSeq, defnRepr) else defnRepr

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => Nil
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap(
              codec =>
                if (codec.isActive(d.id)) List(codec.id -> q"$baboonLazy.of(() -> ${codec.codecName(srcRef, defn.defn.id.owner)}.INSTANCE)")
                else Nil
            )
          List(CodecReg(defn.id, jvTypeRef, srcRef, q"\"${defn.id.toString}\"", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs
      val allCodecDefs = ownCodecDefs ++ repr.codecDefs

      DefnRepr(content, allCodecDefs, allRegs)
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: JvValue.JvType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = jvDomainTreeTools.makeDataMeta(defn)
      val codecMeta = codecs.flatMap(_.codecMeta(defn, name).map(_.member))

      defn.defn match {
        case dto: Typedef.Dto =>
          renderDto(dto, name, genMarker, mainMeta, codecMeta)

        case e: Typedef.Enum =>
          renderEnum(e, name, mainMeta, codecMeta)

        case adt: Typedef.Adt =>
          renderAdt(defn, adt, name, genMarker, mainMeta, codecMeta)

        case contract: Typedef.Contract =>
          renderContract(contract, name, genMarker)

        case service: Typedef.Service =>
          renderService(service, name)

        case _: Typedef.Foreign => DefnRepr(q"", Nil, Nil)
      }
    }

    private def renderDto(
      dto: Typedef.Dto,
      name: JvValue.JvType,
      genMarker: JvType,
      mainMeta: List[JvDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[JvValue]],
    ): DefnRepr = {
      val params = dto.fields.map {
        f =>
          val t = trans.asJvRef(f.tpe, domain, evo)
          q"$t ${f.name.name}"
      }
      val paramsList = if (params.nonEmpty) params.join(",\n") else q""

      val contractParents = dto.contracts.map(c => trans.toJvTypeRefKeepForeigns(c, domain, evo))
      val (adtMarker, adtParent) = dto.id.owner match {
        case Owner.Adt(adtId) =>
          val adtType = trans.toJvTypeRefKeepForeigns(adtId, domain, evo)
          (Seq(iBaboonAdtMemberMeta), Seq(adtType))
        case _ => (Seq.empty, Seq.empty)
      }

      val interfaceParents = (adtParent ++ adtMarker ++ contractParents :+ genMarker).distinct
      val implementsList = interfaceParents.map(t => q"$t").join(", ")

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta
      val hasFields = params.nonEmpty

      val paramsBlock = if (hasFields) {
        q"""(
           |  ${paramsList.shift(2).trim}
           |)""".stripMargin
      } else q"()"

      val emptyRecordMethods = if (!hasFields) {
        q"""
           |@Override
           |public boolean equals(Object other) {
           |  return other instanceof ${name.asName};
           |}
           |
           |@Override
           |public int hashCode() {
           |  return ${name.asName.hashCode.toString};
           |}
           |
           |@Override
           |public String toString() {
           |  return "${name.asName}()";
           |}
           |""".stripMargin
      } else q""

      DefnRepr(
        q"""public record ${name.asName}${paramsBlock} implements ${implementsList} {
           |  ${staticMetaFields.joinN().shift(2).trim}
           |  ${emptyRecordMethods.shift(2).trim}
           |}""".stripMargin,
        Nil,
        Nil,
      )
    }

    private def renderEnum(
      e: Typedef.Enum,
      name: JvValue.JvType,
      mainMeta: List[JvDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[JvValue]],
    ): DefnRepr = {
      val cases = e.members.map {
        m =>
          val obj = m.name.capitalize
          q"$obj"
      }.toList

      val parseCases = e.members.map {
        m =>
          val obj = m.name.capitalize
          q"case \"$obj\" -> $obj;"
      }.toList

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      DefnRepr(
        q"""public enum ${name.asName} implements $iBaboonGenerated {
           |  ${cases.join(",\n").shift(2).trim};
           |
           |  ${staticMetaFields.joinN().shift(2).trim}
           |
           |  public static ${name.asName} parse(String s) {
           |    return switch (s) {
           |      ${parseCases.joinN().shift(6).trim}
           |      default -> throw new IllegalArgumentException("Unknown enum value: " + s);
           |    };
           |  }
           |
           |  public static $jvList<${name.asName}> all() {
           |    return $jvList.of(${cases.join(", ")});
           |  }
           |}""".stripMargin,
        Nil,
        Nil,
      )
    }

    private def renderAdt(
      defn: DomainMember.User,
      adt: Typedef.Adt,
      name: JvValue.JvType,
      genMarker: JvType,
      mainMeta: List[JvDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[JvValue]],
    ): DefnRepr = {
      val contractParents = adt.contracts.map(c => trans.toJvTypeRefKeepForeigns(c, domain, evo))
      val parents = (contractParents :+ genMarker).distinct
      val parentsList = parents.map(t => q"$t").join(", ")

      val memberTrees = adt.members.map {
        mid =>
          domain.defs.meta.nodes(mid) match {
            case mdefn: DomainMember.User => makeFullRepr(mdefn, inPkg = false)
            case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
          }
      }

      val memberNames = adt.members.map { mid =>
        val memberName = mid.name.name.capitalize
        q"${name.asName}.$memberName"
      }.toList

      val permitsClause = q" permits ${memberNames.join(", ")}"

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      DefnRepr(
        q"""public sealed interface ${name.asName} extends ${parentsList}${permitsClause} {
           |  ${staticMetaFields.joinN().shift(2).trim}
           |
           |  ${memberTrees.map(_.defn).toList.joinNN().shift(2).trim}
           |}""".stripMargin,
        memberTrees.toList.flatMap(_.codecDefs),
        memberTrees.toList.flatMap(_.codecs),
      )
    }

    private def renderContract(
      contract: Typedef.Contract,
      name: JvValue.JvType,
      genMarker: JvType,
    ): DefnRepr = {
      val methods = contract.fields.map {
        f =>
          val t = trans.asJvRef(f.tpe, domain, evo)
          q"$t ${f.name.name}();"
      }
      val contractParents = contract.contracts.map(c => trans.toJvTypeRefKeepForeigns(c, domain, evo))
      val adtParent = contract.id.owner match {
        case Owner.Adt(adtId) => Seq(trans.toJvTypeRefKeepForeigns(adtId, domain, evo))
        case _                => Seq.empty
      }
      val parents = (adtParent ++ contractParents :+ genMarker).distinct
      val parentsList = parents.map(t => q"$t").join(", ")
      val body = if (methods.nonEmpty) methods.joinN() else q""
      val sealedModifier = contract.id.owner match {
        case Owner.Adt(_) => "non-sealed"
        case _            => ""
      }
      DefnRepr(
        q"""public $sealedModifier interface ${name.asName} extends ${parentsList} {
           |  ${body.shift(2).trim}
           |}""".stripMargin,
        Nil,
        Nil,
      )
    }

    private def renderService(
      service: Typedef.Service,
      name: JvValue.JvType,
    ): DefnRepr = {
      val resolved    = ServiceResultResolver.resolve(domain, "java", target.language.serviceResult, target.language.pragmas)
      val resolvedCtx = ServiceContextResolver.resolve(domain, "java", target.language.serviceContext, target.language.pragmas)
      val ctxParam = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => ""
        case ResolvedServiceContext.AbstractContext(tn, pn) => s"$tn $pn, "
        case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$tn $pn, "
      }
      val methods = service.methods.map {
        m =>
          val in     = trans.asJvRef(m.sig, domain, evo)
          val out    = m.out.map(trans.asJvRef(_, domain, evo))
          val err    = m.err.map(trans.asJvRef(_, domain, evo))
          val jvFqName: JvValue => String = {
            case t: JvValue.JvType     => if (t.predef) t.name else (t.pkg.parts :+ t.name).mkString(".")
            case t: JvValue.JvTypeName => t.name
          }
          val outStr = out.map(_.mapRender(jvFqName)).getOrElse("void")
          val errStr = err.map(_.mapRender(jvFqName))
          val retStr = resolved.renderReturnType(outStr, errStr, "void")
          q"$retStr ${m.name.name}(${ctxParam}$in arg);"
      }
      val typeParams = Seq(
        resolved.traitTypeParam,
        resolvedCtx match {
          case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
          case _                                            => None
        },
      ).flatten
      val traitTypeParam = if (typeParams.nonEmpty) typeParams.mkString("<", ", ", ">") else ""
      val body = if (methods.nonEmpty) methods.joinN() else q""
      DefnRepr(
        q"""public interface ${name.asName}$traitTypeParam {
           |  ${body.shift(2).trim}
           |}""".stripMargin,
        Nil,
        Nil,
      )
    }

    private def collectContractFieldNames(contracts: List[TypeId.User]): Set[String] = {
      contracts.flatMap { contractId =>
        domain.defs.meta.nodes.get(contractId) match {
          case Some(DomainMember.User(_, ct: Typedef.Contract, _, _)) =>
            ct.fields.map(_.name.name) ++ collectContractFieldNames(ct.contracts)
          case _ => Seq.empty
        }
      }.toSet
    }

    private def ownerSubdir(owner: Owner): String = owner match {
      case Owner.Toplevel => ""
      case Owner.Ns(path) => "/" + path.map(_.name.toLowerCase).mkString("/")
      case Owner.Adt(id)  => ownerSubdir(id.owner)
    }

    private def effectivePkg(owner: Owner): JvValue.JvPackageId = {
      trans.effectiveJvPkg(owner, domain, evo)
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = jvFiles.basename(domain, evo)
      val subdir = ownerSubdir(defn.defn.id.owner)
      val javaName = defn.id.name.name.capitalize
      val fname = s"$javaName${suffix.getOrElse("")}.java"
      s"$fbase$subdir/$fname"
    }
  }
}
