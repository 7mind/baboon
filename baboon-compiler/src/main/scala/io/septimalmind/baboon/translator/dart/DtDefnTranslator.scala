package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.dart.DtValue.DtType
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait DtDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[DtDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[DtDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[DtDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[DtDefnTranslator.Output]]
}

object DtDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: DtType,
    tpeKeepForeigns: DtType,
    tpeId: TextTree[DtValue],
    trees: Map[String, TextTree[DtValue]],
  )

  final case class DefnRepr(
    defn: TextTree[DtValue],
    codecs: List[CodecReg],
  )

  final case class Output(
    path: String,
    tree: TextTree[DtValue],
    module: DtValue.DtPackageId,
    product: CompilerProduct,
    doNotModify: Boolean                              = false,
    codecReg: List[(String, List[TextTree[DtValue]])] = Nil,
  )

  class DtDefnTranslatorImpl[F[+_, +_]: Applicative2](
    domain: Domain,
    evo: BaboonEvolution,
    dtFiles: DtFileTools,
    dtTrees: DtTreeTools,
    trans: DtTypeTranslator,
    codecs: Set[DtCodecTranslator],
    codecTests: DtCodecTestsTranslator,
    codecsFixture: DtCodecFixtureTranslator,
    wiringTranslator: DtServiceWiringTranslator,
    dtDomainTreeTools: DtDomainTreeTools,
  ) extends DtDefnTranslator[F] {
    import DtTypes.*

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
      val dtTypeRef = trans.asDtType(defn.id, domain, evo)
      val srcRef    = trans.toDtTypeRefKeepForeigns(defn.id, domain, evo)
      val testPath  = getOutputPath(defn, suffix = Some("_test"))
      val typePath  = getOutputPath(defn)
      val fixturePath = getOutputPath(defn, suffix = Some("_fixture"))
      val testTree  = codecTests.translate(defn, dtTypeRef, srcRef, testPath, typePath, fixturePath)
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
        val pkg   = trans.toDtPkg(domain.id, domain.version, evo)
        val fbase = dtFiles.basename(domain, evo)
        Output(
          s"$fbase/baboon_service_rt.dart",
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

      def deprecatePrevious(tree: TextTree[DtValue]): TextTree[DtValue] = {
        if (isLatestVersion || tree.isEmpty) {
          tree
        } else {
          q"""@Deprecated('Version ${domain.version.v.toString} is deprecated, you should migrate to ${evo.latest.v.toString}')
             |$tree""".stripMargin
        }
      }

      val dtTypeRef = trans.asDtType(defn.id, domain, evo)
      val srcRef    = trans.toDtTypeRefKeepForeigns(defn.id, domain, evo)

      val repr = makeRepr(defn, dtTypeRef, isLatestVersion)

      val codecTrees =
        codecs.toList
          .flatMap(t => t.translate(defn, dtTypeRef, srcRef).toList)
          .map(deprecatePrevious)

      val defnRepr = deprecatePrevious(repr.defn)

      assert(defn.id.pkg == domain.id)

      val allDefs = (defnRepr +: codecTrees).joinNN()
      val content = if (inLib) dtTrees.inLib(allDefs) else allDefs

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => Nil
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap(
              codec =>
                if (codec.isActive(d.id)) List(codec.id -> q"() => ${codec.codecName(srcRef).copy(fq = true)}")
                else Nil
            )
          List(CodecReg(defn.id, dtTypeRef, srcRef, q"'${defn.id.toString}'", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      DefnRepr(content, allRegs)
    }

    private def makeRepr(
      defn: DomainMember.User,
      name: DtType,
      isLatestVersion: Boolean,
    ): DefnRepr = {
      val genMarker = if (isLatestVersion) iBaboonGeneratedLatest else iBaboonGenerated
      val mainMeta  = dtDomainTreeTools.makeDataMeta(defn)
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
      name: DtType,
      genMarker: DtType,
      mainMeta: List[DtDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[DtValue]],
    ): DefnRepr = {
      val contractFieldNames = collectContractFieldNames(dto.contracts)
      val hasFields = dto.fields.nonEmpty

      val fieldDeclarations = dto.fields.map { f =>
        val t = trans.asDtRef(f.tpe, domain, evo)
        val overridePrefix = if (contractFieldNames.contains(f.name.name)) "@override " else ""
        f.tpe match {
          case TypeRef.Constructor(TypeId.Builtins.opt, _) =>
            q"${overridePrefix}final $t ${f.name.name};"
          case _ =>
            q"${overridePrefix}final $t ${f.name.name};"
        }
      }

      val constructorParams = dto.fields.map { f =>
        f.tpe match {
          case TypeRef.Constructor(TypeId.Builtins.opt, _) =>
            q"this.${f.name.name}"
          case _ =>
            q"required this.${f.name.name}"
        }
      }

      val contractParents = dto.contracts.map(c => trans.toDtTypeRefKeepForeigns(c, domain, evo))
      val (adtMarker, adtParent) = dto.id.owner match {
        case Owner.Adt(adtId) =>
          val adtType = trans.toDtTypeRefKeepForeigns(adtId, domain, evo)
          (Seq(iBaboonAdtMemberMeta), Seq(adtType))
        case _ => (Seq.empty, Seq.empty)
      }

      val interfaceParents = (adtParent ++ adtMarker ++ contractParents :+ genMarker).distinct
      val implementsList = interfaceParents.map(t => q"$t").join(", ")

      val extendsClause = dto.id.owner match {
        case Owner.Adt(_) =>
          val adtType = adtParent.head
          val ifaces = (adtMarker ++ contractParents :+ genMarker).distinct
          if (ifaces.nonEmpty) q" extends $adtType implements ${ifaces.map(t => q"$t").join(", ")}"
          else q" extends $adtType"
        case _ =>
          q" implements $implementsList"
      }

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      val constructorBlock = if (hasFields) {
        q"""const ${name.asName}({
           |  ${constructorParams.join(",\n").shift(2).trim}
           |});""".stripMargin
      } else {
        q"const ${name.asName}();"
      }

      val fieldsBlock = if (hasFields) {
        fieldDeclarations.joinN()
      } else q""

      val equalsBody = if (hasFields) {
        val fieldComparisons = dto.fields.map(f => q"baboonDeepEquals(${f.name.name}, other.${f.name.name})")
        q"""@override
           |bool operator ==(Object other) =>
           |  identical(this, other) ||
           |  other is ${name.asName} &&
           |  ${fieldComparisons.join(" &&\n  ").shift(2).trim};""".stripMargin
      } else {
        q"""@override
           |bool operator ==(Object other) => other is ${name.asName};""".stripMargin
      }

      val hashCodeBody = if (hasFields) {
        val hashParts = dto.fields.map(f => q"baboonDeepHashCode(${f.name.name})")
        q"""@override
           |int get hashCode => Object.hashAll([${hashParts.join(", ")}]);""".stripMargin
      } else {
        q"""@override
           |int get hashCode => ${name.asName.hashCode.toString};""".stripMargin
      }

      val toStringBody = if (hasFields) {
        val fieldStrings = dto.fields.map(f => q"${f.name.name}: $$${f.name.name}")
        q"""@override
           |String toString() => '${name.asName}(${fieldStrings.join(", ")})';""".stripMargin
      } else {
        q"""@override
           |String toString() => '${name.asName}()';""".stripMargin
      }

      DefnRepr(
        q"""class ${name.asName}$extendsClause {
           |  ${fieldsBlock.shift(2).trim}
           |
           |  ${constructorBlock.shift(2).trim}
           |
           |  ${staticMetaFields.joinN().shift(2).trim}
           |
           |  ${equalsBody.shift(2).trim}
           |
           |  ${hashCodeBody.shift(2).trim}
           |
           |  ${toStringBody.shift(2).trim}
           |}""".stripMargin,
        Nil,
      )
    }

    private def renderEnum(
      e: Typedef.Enum,
      name: DtType,
      mainMeta: List[DtDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[DtValue]],
    ): DefnRepr = {
      val cases = e.members.map { m =>
        q"${m.name}"
      }.toList

      val parseCases = e.members.map { m =>
        val obj = m.name.capitalize
        q"'$obj' => $obj,"
      }.toList

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      DefnRepr(
        q"""enum ${name.asName} implements $iBaboonGenerated {
           |  ${cases.join(",\n").shift(2).trim};
           |
           |  ${staticMetaFields.joinN().shift(2).trim}
           |
           |  static ${name.asName}? parse(String s) {
           |    return switch (s) {
           |      ${parseCases.joinN().shift(6).trim}
           |      _ => null,
           |    };
           |  }
           |
           |  static List<${name.asName}> get all => values;
           |}""".stripMargin,
        Nil,
      )
    }

    private def renderAdt(
      defn: DomainMember.User,
      adt: Typedef.Adt,
      name: DtType,
      genMarker: DtType,
      mainMeta: List[DtDomainTreeTools.MetaField],
      codecMeta: Iterable[TextTree[DtValue]],
    ): DefnRepr = {
      val contractParents = adt.contracts.map(c => trans.toDtTypeRefKeepForeigns(c, domain, evo))
      val parents = (contractParents :+ genMarker).distinct
      val implementsClause = if (parents.nonEmpty) q" implements ${parents.map(t => q"$t").join(", ")}" else q""

      val memberTrees = adt.members.map { mid =>
        domain.defs.meta.nodes(mid) match {
          case mdefn: DomainMember.User => makeFullRepr(mdefn, inLib = false)
          case other                    => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $other")
        }
      }

      val staticMetaFields = mainMeta.map(_.valueField) ++ codecMeta

      DefnRepr(
        q"""sealed class ${name.asName}$implementsClause {
           |  const ${name.asName}();
           |
           |  ${staticMetaFields.joinN().shift(2).trim}
           |}
           |
           |${memberTrees.map(_.defn).toList.joinNN()}""".stripMargin,
        memberTrees.toList.flatMap(_.codecs),
      )
    }

    private def renderContract(
      contract: Typedef.Contract,
      name: DtType,
      genMarker: DtType,
    ): DefnRepr = {
      val methods = contract.fields.map { f =>
        val t = trans.asDtRef(f.tpe, domain, evo)
        q"$t get ${f.name.name};"
      }
      val contractParents = contract.contracts.map(c => trans.toDtTypeRefKeepForeigns(c, domain, evo))
      val parents = (contractParents :+ genMarker).distinct
      val implementsClause = if (parents.nonEmpty) q" implements ${parents.map(t => q"$t").join(", ")}" else q""
      val body = if (methods.nonEmpty) methods.joinN() else q""

      DefnRepr(
        q"""abstract interface class ${name.asName}$implementsClause {
           |  ${body.shift(2).trim}
           |}""".stripMargin,
        Nil,
      )
    }

    private def renderService(
      defn: DomainMember.User,
      name: DtType,
    ): DefnRepr = {
      val service = defn.defn.asInstanceOf[Typedef.Service]
      val methods = service.methods.map { m =>
        val in  = trans.asDtRef(m.sig, domain, evo)
        val out = m.out.map(trans.asDtRef(_, domain, evo))
        val retStr = out.map(o => q"$o").getOrElse(q"void")
        q"$retStr ${m.name.name}($in arg);"
      }
      val body = if (methods.nonEmpty) methods.joinN() else q""

      DefnRepr(
        q"""abstract class ${name.asName} {
           |  ${body.shift(2).trim}
           |}""".stripMargin,
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

    private def getOutputPath(defn: DomainMember.User, suffix: Option[String] = None): String = {
      val fbase = dtFiles.basename(domain, evo)
      val fname = s"${trans.toSnakeCase(defn.id.name.name)}${suffix.getOrElse("")}.dart"

      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${trans.toSnakeCase(id.name.name)}/$fname"
      }
    }

    private def getOutputModule(defn: DomainMember.User): DtValue.DtPackageId = {
      val basePkg = trans.toDtPkg(domain.id, domain.version, evo)
      defn.defn.id.owner match {
        case Owner.Toplevel => basePkg
        case Owner.Ns(path) => DtValue.DtPackageId(basePkg.parts ++ path.map(_.name.toLowerCase))
        case Owner.Adt(id)  => DtValue.DtPackageId(basePkg.parts :+ trans.toSnakeCase(id.name.name))
      }
    }
  }
}
