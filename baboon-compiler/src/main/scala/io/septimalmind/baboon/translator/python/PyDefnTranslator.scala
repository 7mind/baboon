package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.{PyModuleId, PyType}
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Applicative2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait PyDefnTranslator[F[+_, +_]] {
  def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[PyDefnTranslator.Output]]
  def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[PyDefnTranslator.Output]]
  def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[PyDefnTranslator.Output]]
  def translateServiceRt(): F[NEList[BaboonIssue], List[PyDefnTranslator.Output]]
}

object PyDefnTranslator {
  final case class CodecReg(
    typeId: TypeId,
    tpe: PyType,
    tpeKeepForeigns: PyType,
    tpeId: TextTree[PyValue],
    trees: Map[String, TextTree[PyValue]],
  )

  final case class Output(
    path: String,
    tree: TextTree[PyValue],
    module: PyValue.PyModuleId,
    product: CompilerProduct,
    codecReg: List[(String, List[TextTree[PyValue]])] = List.empty,
  )

  final case class PyDefnRepr(
    defn: TextTree[PyValue],
    codecs: List[CodecReg],
  )

  final class PyDefnTranslatorImpl[F[+_, +_]: Applicative2](
    target: PyTarget,
    codecsFixture: PyCodecFixtureTranslator,
    codecsTests: PyCodecTestTranslator,
    typeTranslator: PyTypeTranslator,
    baboonEnquiries: BaboonEnquiries,
    codecs: Set[PyCodecTranslator],
    pyDomTrees: PyDomainTreeTools,
    evolution: BaboonEvolution,
    fileTools: PyFileTools,
    domain: Domain,
    wiringTranslator: PyServiceWiringTranslator,
  ) extends PyDefnTranslator[F] {
    override def translate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslate(defn)
      }
    }

    override def translateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateFixtures(defn)
      }
    }

    override def translateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      defn.id.owner match {
        case Owner.Adt(_) => F.pure(List.empty)
        case _            => doTranslateTests(defn)
      }
    }

    private def doTranslateTests(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val codecsTestsOut = codecsTests
        .translate(
          defn,
          typeTranslator.asPyType(defn.id, domain, evolution, fileTools.definitionsBasePkg),
          typeTranslator.asPyTypeKeepForeigns(defn.id, domain, evolution, fileTools.definitionsBasePkg),
        ).map(
          codecsTest =>
            Output(
              getOutputPath(defn, prefix = Some("test_")),
              codecsTest,
              typeTranslator.toPyModule(defn.id, domain.version, evolution, fileTools.testsBasePkg),
              CompilerProduct.Test,
            )
        )

      F.pure(codecsTestsOut.toList)
    }

    override def translateServiceRt(): F[NEList[BaboonIssue], List[Output]] = {
      val rtTree = wiringTranslator.translateServiceRt(domain)
      val result = rtTree.map { tree =>
        val fbase = fileTools.basename(domain, evolution)
        val serviceRtModule = {
          val pathToModule = domain.id.path.toList
          val fullPath     = fileTools.definitionsBasePkg ++ pathToModule ++ List("BaboonServiceRt")
          PyModuleId(NEList.unsafeFrom(fullPath))
        }
        Output(
          s"$fbase/BaboonServiceRt.py",
          tree,
          serviceRtModule,
          CompilerProduct.Definition,
        )
      }.toList
      F.pure(result)
    }

    private def doTranslateFixtures(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val fixtureTree = codecsFixture
        .translate(defn).map(
          fixture =>
            Output(
              getOutputPath(defn, suffix = Some("_Fixture")),
              fixture,
              typeTranslator.toPyModule(defn.id, domain.version, evolution, fileTools.fixturesBasePkg),
              CompilerProduct.Fixture,
            )
        )
      F.pure(fixtureTree.toList)
    }

    private def doTranslate(defn: DomainMember.User): F[NEList[BaboonIssue], List[Output]] = {
      val repr = makeFullRepr(defn)

      val regsPerCodec = codecs.toList.map(codecTranslator =>
        (codecTranslator.id, repr.codecs.flatMap(reg => reg.trees.get(codecTranslator.id).map(expr => q"${reg.tpeId}, $expr")))
      )

      val mainOutput = Output(
        getOutputPath(defn),
        repr.defn,
        typeTranslator.toPyModule(defn.id, domain.version, evolution, fileTools.definitionsBasePkg),
        CompilerProduct.Definition,
        codecReg = regsPerCodec,
      )

      val wiringOutput = wiringTranslator.translate(defn).map { wiringTree =>
        val wiringModule = typeTranslator
          .toPyModule(defn.id, domain.version, evolution, fileTools.definitionsBasePkg)
          .withModuleName(s"${defn.id.name.name}_Wiring")
        Output(
          getOutputPath(defn, suffix = Some("_Wiring")),
          wiringTree,
          wiringModule,
          CompilerProduct.Definition,
        )
      }.toList

      F.pure(mainOutput :: wiringOutput)
    }

    private def makeFullRepr(defn: DomainMember.User): PyDefnRepr = {
      val isLatestVersion = domain.version == evolution.latest

      def obsoletePrevious(tree: TextTree[PyValue]): TextTree[PyValue] = {
        if (isLatestVersion || tree.isEmpty) tree
        else {
          q"""@$deprecated("Version ${domain.version.toString} is obsolete, you should migrate to ${evolution.latest.toString}")
             |$tree""".stripMargin
        }
      }

      val pyRef  = typeTranslator.asPyType(defn.id, domain, evolution, fileTools.definitionsBasePkg)
      val srcRef = typeTranslator.asPyTypeKeepForeigns(defn.id, domain, evolution, fileTools.definitionsBasePkg)

      val repr       = mkRepr(defn, isLatestVersion)
      val defnRepr   = List(obsoletePrevious(repr.defn))
      val codecTrees = codecs.toList.flatMap(_.translate(defn, pyRef, srcRef)).map(obsoletePrevious)
      val allDefs    = (defnRepr ++ codecTrees).joinNN()

      val reg = defn.defn match {
        case _: Typedef.NonDataTypedef => List.empty[CodecReg]
        case d =>
          val codecsReg = codecs.toList
            .sortBy(_.getClass.getName)
            .flatMap {
              codec =>
                if (codec.isActive(d.id)) {
                  List(codec.id -> q"${codec.codecType(defn.id)}.instance")
                } else {
                  List.empty
                }
            }
          List(CodecReg(defn.id, pyRef, srcRef, q"\"${defn.id.toString}\"", codecsReg.toMap))
      }

      val allRegs = reg ++ repr.codecs

      assert(defn.id.pkg == domain.id)
      PyDefnRepr(allDefs, allRegs)
    }

    private def mkRepr(defn: DomainMember.User, isLatestVersion: Boolean): PyDefnRepr = {
      val genMarker       = if (isLatestVersion) baboonGeneratedLatest else baboonGenerated
      val codecMeta       = codecs.map(_.codecMeta(defn.id).member)
      val mainMeta        = pyDomTrees.makeDataMeta(defn) ++ codecMeta
      val jsonCodecActive = codecs.collectFirst { case jsonCodec: PyJsonCodecGenerator => jsonCodec }.exists(_.isActive(defn.id))
      defn.defn match {
        case dto: Typedef.Dto =>
          val dtoContracts     = dto.contracts
          val dtoContractsDefs = dtoContracts.flatMap(domain.defs.meta.nodes.get).collect { case DomainMember.User(_, c: Typedef.Contract, _, _) => c }
          val contractsFields  = dtoContractsDefs.flatMap(_.fields)

          val dtoFieldsTrees = genDtoFields(dto.fields, contractsFields.toSet)
          val dtoProperties  = genDtoProperties(contractsFields)

          val contractParents = dto.contracts.toSeq
          val adtParent = dto.id.owner match {
            case Owner.Adt(id) => List(id)
            case _             => Nil
          }
          val directParentsDefs = (adtParent ++ contractParents).flatMap(domain.defs.meta.nodes.get).collect { case u: DomainMember.User => u }

          val superclasses        = baboonEnquiries.collectParents(domain, directParentsDefs).toSet
          val uniqueContracts     = dtoContracts.filterNot(c1 => superclasses.contains(c1))
          val genMarkerParent     = if (adtParent.nonEmpty || contractParents.nonEmpty) Nil else List(genMarker)
          val adtMemberMetaParent = if (adtParent.isEmpty) Nil else List(baboonAdtMemberMeta)

          val superclassesTypes = (adtParent ++ uniqueContracts).map(c => typeTranslator.asPyType(c, domain, evolution, fileTools.definitionsBasePkg))

          val parentTypes = superclassesTypes ++ genMarkerParent ++ adtMemberMetaParent :+ pydanticBaseModel

          val parents = mkParents(parentTypes)

          val modelConfig = genDtoPydanticModelConf(dto.fields, dtoContracts.nonEmpty, jsonCodecActive)

          val members =
            List(
              Some(dtoFieldsTrees.joinN()),
              Some(modelConfig),
              dtoProperties.map(_.joinN()),
              Some(mainMeta.joinN()),
            ).flatten

          PyDefnRepr(
            q"""class ${dto.id.name.name.capitalize}($parents):
               |    ${members.joinNN().shift(4).trim}
               |""".stripMargin,
            List.empty,
          )

        case enum: Typedef.Enum =>
          val branches = enum.members.map(m => q"${m.name.capitalize} = \"${m.name.capitalize}\"").toSeq
          PyDefnRepr(
            q"""|class ${enum.id.name.name.capitalize}($pyEnum):
                |    ${branches.joinN().shift(4).trim}
                |""".stripMargin,
            List.empty,
          )
        case adt: Typedef.Adt =>
          val contracts       = adt.contracts.map(c => typeTranslator.asPyType(c, domain, evolution, fileTools.definitionsBasePkg))
          val defaultParents  = contracts ++ List(pydanticBaseModel)
          val genMarkerParent = if (adt.contracts.isEmpty) List(genMarker) else Nil
          val allParents      = defaultParents ++ genMarkerParent
          val parents         = mkParents(allParents)

          val memberTrees = adt.members.map(
            mid =>
              domain.defs.meta.nodes.get(mid) match {
                case Some(mdefn: DomainMember.User) => makeFullRepr(mdefn)
                case m                              => throw new RuntimeException(s"BUG: missing/wrong adt member: $mid => $m")
              }
          )

          val branches = memberTrees
            .map(_.defn)
            .toSeq

          val jsonCodec = if (jsonCodecActive) {
            Some(q"""
                    |__registry__: dict[str, type] = $pyDefaultDict()
                    |
                    |def __init_subclass__(cls, **kwargs):
                    |   super().__init_subclass__(**kwargs)
                    |   ${adt.id.name.name.capitalize}.__registry__[cls.__name__] = cls
                    |
                    |@$pydanticModelSerializer(mode='wrap')
                    |def serialize(self, serializer):
                    |    return {self.__class__.__name__: serializer(self)}
                    |
                    |@$pydanticModelValidator(mode="wrap")
                    |@$pyClassMethod
                    |def polymorphic(cls, values, handler):
                    |    if isinstance(values, dict) and len(values) == 1:
                    |        class_name = next(iter(values))
                    |        registry = ${adt.id.name.name.capitalize}.__registry__
                    |
                    |        if class_name in registry:
                    |            candidate = registry[class_name]
                    |            if issubclass(candidate, cls):
                    |                return candidate.model_validate(values[class_name])
                    |            else:
                    |                raise ValueError("not subcluss")
                    |
                    |    return handler(values)
                    |""".stripMargin)
          } else None

          val members = List(
            jsonCodec,
            Some(mainMeta.joinN()),
          ).flatten

          val regs = memberTrees.map(_.codecs)

          PyDefnRepr(
            q"""|class ${adt.id.name.name.capitalize}($parents):
                |    pass
                |
                |    ${members.joinNN().shift(4).trim}
                |
                |${branches.joinNN()}
                |""".stripMargin,
            regs.toList.flatten,
          )

        case contract: Typedef.Contract =>
          val contracts  = contract.contracts.map(c => typeTranslator.asPyType(c, domain, evolution, fileTools.definitionsBasePkg))
          val allParents = if (contract.contracts.isEmpty) List(genMarker, pyABC) ++ contracts else contracts
          val parents    = mkParents(allParents)
          val methods = contract.fields.map {
            f =>
              val tpe  = typeTranslator.asPyRef(f.tpe, domain, evolution, fileTools.definitionsBasePkg)
              val name = s"${f.name.name}"
              q"""@$pyAbstractMethod
                 |def $name(self) -> $tpe:
                 |    raise NotImplementedError
                 |""".stripMargin
          }
          val allMethods = if (methods.isEmpty) q"pass" else methods.joinN()
          PyDefnRepr(
            q"""|class ${contract.id.name.name.capitalize}($parents):
                |    ${allMethods.shift(4).trim}
                |""".stripMargin,
            List.empty,
          )
        case service: Typedef.Service =>
          val resolved    = ServiceResultResolver.resolve(domain, "python", target.language.serviceResult, target.language.pragmas)
          val resolvedCtx = ServiceContextResolver.resolve(domain, "python", target.language.serviceContext, target.language.pragmas)
          val ctxParam = resolvedCtx match {
            case ResolvedServiceContext.NoContext                => ""
            case ResolvedServiceContext.AbstractContext(tn, pn)  => s"$pn: $tn, "
            case ResolvedServiceContext.ConcreteContext(tn, pn)  => s"$pn: $tn, "
          }
          val methods = service.methods.map { m =>
            val inType  = typeTranslator.asPyRef(m.sig, domain, evolution, fileTools.definitionsBasePkg)
            val outType = m.out.map(typeTranslator.asPyRef(_, domain, evolution, fileTools.definitionsBasePkg))
            val errType = m.err.map(typeTranslator.asPyRef(_, domain, evolution, fileTools.definitionsBasePkg))
            val retAnnotation: TextTree[PyValue] = if (resolved.noErrors || errType.isEmpty) {
              outType.getOrElse(q"None")
            } else {
              val pyName: PyValue => String = { case t: PyValue.PyType => t.name }
              val outStr  = outType.map(_.mapRender(pyName)).getOrElse("")
              val errStr  = errType.map(_.mapRender(pyName))
              val retStr  = resolved.renderReturnType(outStr, errStr, "None")
              q"${"\""  + retStr + "\""}"
            }
            q"""|@$pyAbstractMethod
                |def ${m.name.name}(self, ${ctxParam}arg: $inType) -> $retAnnotation:
                |    raise NotImplementedError
                |""".stripMargin
          }
          val allMethods = if (methods.isEmpty) q"pass" else methods.joinN()
          val classBases = resolvedCtx match {
            case ResolvedServiceContext.AbstractContext(tn, _) =>
              q"$pyABC, Generic[$tn]"
            case _ =>
              q"$pyABC"
          }
          PyDefnRepr(
            q"""|class ${service.id.name.name.capitalize}($classBases):
                |    ${allMethods.shift(4).trim}
                |""".stripMargin,
            List.empty,
          )
        case _: Typedef.Foreign => PyDefnRepr(q"", List.empty)
      }

    }

    private def genDtoFields(dtoFields: List[Field], contractsFields: Set[Field]): List[TextTree[PyValue]] = {
      val fields = dtoFields.map {
        field =>
          val fieldName = field.name.name
          val fieldType = typeTranslator.asPyRef(field.tpe, domain, evolution, fileTools.definitionsBasePkg)

          if (contractsFields.contains(field)) {
            q"${fieldName}_: $fieldType = $pydanticField(alias='$fieldName', serialization_alias='$fieldName')"
          } else q"$fieldName: $fieldType"
      }
      if (fields.isEmpty) List(q"pass") else fields
    }

    private def genDtoProperties(contractsFields: List[Field]): Option[List[TextTree[PyValue]]] = {
      if (contractsFields.nonEmpty) {
        val properties = contractsFields
          .map(f => q"""@property
                       |def ${f.name.name}(self) -> ${typeTranslator.asPyRef(f.tpe, domain, evolution, fileTools.definitionsBasePkg)}:
                       |    return self.${f.name.name}_
                       |""".stripMargin)
        Some(properties)
      } else None
    }

    private def genDtoPydanticModelConf(dtoFields: List[Field], hasContracts: Boolean, jsonCodecActive: Boolean): TextTree[PyType] = {
      val frozen           = Some(q"frozen=True")
      val serializeByAlias = if (jsonCodecActive && hasContracts) Some(q"serialize_by_alias=True") else None
      val serializeJsonBytesAsHex =
        if (dtoFields.map(_.tpe.id).contains(TypeId.Builtins.bytes)) {
          List(
            q"ser_json_bytes='hex'",
            q"val_json_bytes='hex'",
          )
        } else Nil

      val serializeDecimalAsJsonNumber =
        if (dtoFields.map(_.tpe.id).contains(TypeId.Builtins.f128)) {
          List(q"json_encoders={Decimal: float}")
        } else Nil

      val configs = List(frozen, serializeByAlias, serializeJsonBytesAsHex, serializeDecimalAsJsonNumber).flatten

      q"""model_config = $pydanticConfigDict(
         |    ${configs.join(",\n").shift(4).trim}
         |)""".stripMargin
    }

    private def mkParents(refs: List[PyType]): TextTree[PyValue] = {
      if (refs.isEmpty) q"" else q"${refs.map(s => q"$s").join(", ")}"
    }

    private def getOutputPath(defn: DomainMember.User, prefix: Option[String] = None, suffix: Option[String] = None): String = {
      val fbase = fileTools.basename(domain, evolution)
      val fname = s"${prefix.getOrElse("")}${defn.id.name.name.capitalize}${suffix.getOrElse("")}.py"
      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}.$fname"
      }
    }
  }
}
