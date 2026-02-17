package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.swift.SwValue.SwType
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait SwDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[SwDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[SwDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[SwDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[SwDefnTranslator.Output]]
}

object SwDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: SwType,
    tpeKeepForeigns: SwType,
    tpeId: TextTree[SwValue],
    trees: Map[String, TextTree[SwValue]],
  )

  final case class DefnRepr(
    defn: TextTree[SwValue],
    codecs: List[CodecReg],
  )

  final case class Output(
    path: String,
    tree: TextTree[SwValue],
    module: SwValue.SwPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[SwValue]])] = Nil,
  )

  class SwDefnTranslatorImpl[F[+_, +_]: Applicative2](
    domain: Domain,
    evo: BaboonEvolution,
    swFiles: SwFileTools,
    swTrees: SwTreeTools,
    trans: SwTypeTranslator,
    codecs: Set[SwCodecTranslator],
    codecTests: SwCodecTestsTranslator,
    codecsFixture: SwCodecFixtureTranslator,
    wiringTranslator: SwServiceWiringTranslator,
    swDomainTreeTools: SwDomainTreeTools,
  ) extends SwDefnTranslator[F] {
    import SwTypes.*

    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr = makeFullRepr(defn, inLib = true)

      val registrations = codecs.toList.map(codec => codec.id -> repr.codecs.flatMap(reg => reg.trees.get(codec.id).map(expr => q"${reg.tpeId}, $expr")))

      val mainOutput = Output(
        getOutputPath(defn),
        repr.defn,
        getOutputModule(defn),
        CompilerProduct.Definition,
        codecReg = registrations,
      )

      F.pure(List(mainOutput))
    }

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateFixtures(defn)
      }
    }

    private def doTranslateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val fixtureTree = codecsFixture.translate(defn)
      val result = fixtureTree.map { tree =>
        Output(
          getOutputPath(defn, suffix = Some("_fixture")),
          tree,
          getOutputModule(defn),
          CompilerProduct.Fixture,
        )
      }.toList
      F.pure(result)
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTest(defn)
      }
    }

    private def doTranslateTest(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val swTypeRef   = trans.asSwType(defn.id, domain, evo)
      val srcRef      = trans.toSwTypeRefKeepForeigns(defn.id, domain, evo)
      val testPath    = getOutputPath(defn, suffix = Some("_test"))
      val typePath    = getOutputPath(defn)
      val fixturePath = getOutputPath(defn, suffix = Some("_fixture"))
      val testTree    = codecTests.translate(defn, swTypeRef, srcRef, testPath, typePath, fixturePath)
      val result = testTree.map { tree =>
        Output(
          testPath,
          tree,
          getOutputModule(defn),
          CompilerProduct.Test,
          doNotModify = true,
        )
      }.toList
      F.pure(result)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map { tree =>
        val pkg   = trans.toSwPkg(domain.id, domain.version, evo)
        val fbase = swFiles.basename(domain, evo)
        Output(
          s"$fbase/baboon_service_rt.swift",
          tree,
          pkg,
          CompilerProduct.Definition,
        )
      }.toList
      F.pure(result)
    }

    private def makeFullRepr(
      defn: DomainMember.User,
      inLib: Boolean,
    ): DefnRepr = {
      val isLatestVersion = domain.version == evo.latest

      def deprecatePrevious(tree: TextTree[SwValue]): TextTree[SwValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""@available(*, deprecated, message: "Version ${domain.version.v.toString} is deprecated, migrate to ${evo.latest.v.toString}")
             |$tree""".stripMargin
        }
      }

      val swTypeRef = trans.asSwType(defn.id, domain, evo)
      val srcRef    = trans.toSwTypeRefKeepForeigns(defn.id, domain, evo)

      val repr = makeRepr(defn, swTypeRef, isLatestVersion)

      val codecTrees =
        codecs.toList
          .flatMap(t => t.translate(defn, swTypeRef, srcRef).toList)
          .map(deprecatePrevious)

      val defnRepr = deprecatePrevious(repr.defn)

      assert(defn.id.pkg == domain.id)

      val allDefs = (defnRepr +: codecTrees).joinNN()
      val content = if (inLib) swTrees.inLib(allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => Nil
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap(
              codec =>
                if (codec.isActive(d.id)) List(codec.id -> q"{ () -> Any in ${codec.codecName(srcRef).copy(fq = true)} }")
                else Nil
            )
          List(CodecReg(defn.id, swTypeRef, srcRef, q""""${defn.id.toString}"""", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      DefnRepr(content, allRegs)
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: SwType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = swDomainTreeTools.makeDataMeta(defn)
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

        case _: Typedef.Service =>
          renderService(defn, name)

        case _: Typedef.Foreign => DefnRepr(q"", Nil)
      }
    }

    private def renderDto(
      dto: Typedef.Dto,
      name: SwType,
      genMarker: SwType,
      mainMeta: List[SwDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[SwValue]],
    ): DefnRepr = {
      val hasFields = dto.fields.nonEmpty

      val fieldDeclarations = dto.fields.map { f =>
        val t       = trans.asSwRef(f.tpe, domain, evo)
        val escaped = trans.escapeSwiftKeyword(f.name.name)
        q"let $escaped: $t"
      }

      val contractParents = dto.contracts.map(c => trans.toSwTypeRefKeepForeigns(c, domain, evo))
      val adtMemberProto = dto.id.owner match {
        case Owner.Adt(_) =>
          Seq(iBaboonAdtMemberMeta)
        case _ => Seq.empty
      }

      val allParents = (adtMemberProto ++ contractParents :+ genMarker).distinct
      val conformanceClause = allParents.map(t => q"$t").join(", ")

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      val fieldsBlock = if (hasFields) {
        fieldDeclarations.joinN()
      } else q""

      DefnRepr(
        q"""struct ${name.asName}: Equatable, Hashable, $conformanceClause {
           |  ${fieldsBlock.shift(4).trim}
           |
           |  ${staticMetaFields.joinN().shift(4).trim}
           |}""".stripMargin,
        Nil,
      )
    }

    private def renderEnum(
      e: Typedef.Enum,
      name: SwType,
      mainMeta: List[SwDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[SwValue]],
    ): DefnRepr = {
      val cases = e.members.map { m =>
        q"""case ${m.name} = "${m.name}""""
      }.toList

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      DefnRepr(
        q"""enum ${name.asName}: String, CaseIterable, $iBaboonGenerated {
           |  ${cases.joinN().shift(4).trim}
           |
           |  ${staticMetaFields.joinN().shift(4).trim}
           |
           |  static func parse(_ s: String) -> ${name.asName}? {
           |    return ${name.asName}(rawValue: s)
           |  }
           |
           |  static var all: [${name.asName}] { return Array(Self.allCases) }
           |}""".stripMargin,
        Nil,
      )
    }

    private def renderAdt(
      defn: DomainMember.User,
      adt: Typedef.Adt,
      name: SwType,
      genMarker: SwType,
      mainMeta: List[SwDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[SwValue]],
    ): DefnRepr = {
      val contractParents = adt.contracts.map(c => trans.toSwTypeRefKeepForeigns(c, domain, evo))
      val parents         = (contractParents :+ genMarker).distinct
      val conformanceClause = parents.map(t => q"$t").join(", ")

      val memberTrees = adt.members.map { mid =>
        domain.defs.meta.nodes(mid) match {
          case mdefn: DomainMember.User => makeFullRepr(mdefn, inLib = false)
          case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
        }
      }

      val memberDtos = memberTrees.map(_.defn).toList.joinNN()

      val enumCases = adt.members.map { mid =>
        val memberName = mid.name.name
        val caseName   = memberName.head.toLower.toString + memberName.tail
        val memberRef  = trans.toSwTypeRefKeepForeigns(mid, domain, evo)
        q"case $caseName($memberRef)"
      }.toList

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      DefnRepr(
        q"""$memberDtos
           |
           |enum ${name.asName}: Equatable, Hashable, $conformanceClause {
           |  ${enumCases.joinN().shift(4).trim}
           |
           |  ${staticMetaFields.joinN().shift(4).trim}
           |}""".stripMargin,
        memberTrees.toList.flatMap(_.codecs),
      )
    }

    private def renderContract(
      contract: Typedef.Contract,
      name: SwType,
      genMarker: SwType,
    ): DefnRepr = {
      val methods = contract.fields.map { f =>
        val t       = trans.asSwRef(f.tpe, domain, evo)
        val escaped = trans.escapeSwiftKeyword(f.name.name)
        q"var $escaped: $t { get }"
      }
      val contractParents = contract.contracts.map(c => trans.toSwTypeRefKeepForeigns(c, domain, evo))
      val parents         = (contractParents :+ genMarker).distinct
      val conformanceClause = parents.map(t => q"$t").join(", ")
      val body = if (methods.nonEmpty) methods.joinN() else q""

      DefnRepr(
        q"""protocol ${name.asName}: $conformanceClause {
           |  ${body.shift(4).trim}
           |}""".stripMargin,
        Nil,
      )
    }

    private def renderService(
      defn: DomainMember.User,
      name: SwType,
    ): DefnRepr = {
      val service = defn.defn.asInstanceOf[Typedef.Service]
      val methods = service.methods.map { m =>
        val in  = trans.asSwRef(m.sig, domain, evo)
        val out = m.out.map(trans.asSwRef(_, domain, evo))
        val retStr = out.map(o => q" -> $o").getOrElse(q"")
        q"func ${m.name.name}(arg: $in)$retStr"
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""

      DefnRepr(
        q"""protocol ${name.asName} {
           |  ${body.shift(4).trim}
           |}""".stripMargin,
        Nil,
      )
    }

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = swFiles.basename(domain, evo)
      val fname = s"${trans.toSnakeCase(defn.id.name.name)}${suffix.getOrElse("")}.swift"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${trans.toSnakeCase(id.name.name)}/$fname"
      }
    }

    private def getOutputModule(defn: DomainMember.User): SwValue.SwPackageId = {
      val basePkg = trans.toSwPkg(domain.id, domain.version, evo)
      defn.defn.id.owner match {
        case Owner.Toplevel => basePkg
        case Owner.Ns(path) => SwValue.SwPackageId(basePkg.parts ++ path.map(_.name.toLowerCase))
        case Owner.Adt(id)  => SwValue.SwPackageId(basePkg.parts :+ trans.toSnakeCase(id.name.name))
      }
    }
  }
}
