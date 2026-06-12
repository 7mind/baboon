package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.python.PyKeywords.escapePyKeyword
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.{PyModuleId, PyType}
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
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
    pyTreeTools: PyTreeTools,
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
      val result = rtTree.map {
        tree =>
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

      val regsPerCodec =
        codecs.toList.map(codecTranslator => (codecTranslator.id, repr.codecs.flatMap(reg => reg.trees.get(codecTranslator.id).map(expr => q"${reg.tpeId}, $expr"))))

      val mainOutput = Output(
        getOutputPath(defn),
        repr.defn,
        typeTranslator.toPyModule(defn.id, domain.version, evolution, fileTools.definitionsBasePkg),
        CompilerProduct.Definition,
        codecReg = regsPerCodec,
      )

      val wiringOutput = wiringTranslator
        .translate(defn).map {
          wiringTree =>
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

      val clientOutput = wiringTranslator
        .translateClient(defn).map {
          clientTree =>
            val clientModule = typeTranslator
              .toPyModule(defn.id, domain.version, evolution, fileTools.definitionsBasePkg)
              .withModuleName(s"${defn.id.name.name}_Client")
            Output(
              getOutputPath(defn, suffix = Some("_Client")),
              clientTree,
              clientModule,
              CompilerProduct.Definition,
            )
        }.toList

      F.pure(mainOutput :: (wiringOutput ++ clientOutput))
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

          // Identifier toString (`__repr__` / `__str__`) emission (PR-57d / spec:
          // docs/spec/identifier-repr.md). Emitted only when `dto.isIdentifier`.
          // The parser lives on a sibling `<TypeName>Codec` class
          // (Q-FU-4: keeps it discoverable but not as `MyId.parse_repr`).
          val identifierRepr: Option[TextTree[PyValue]] =
            if (dto.isIdentifier) Some(renderIdentifierRepr(dto)) else None
          val identifierCodec: Option[TextTree[PyValue]] =
            if (dto.isIdentifier) Some(renderIdentifierCodecClass(dto)) else None

          val classDocstring = pyTreeTools.renderClassDocstring(
            defn.docs,
            dto.fields.map(f => f.name.name -> f.docs),
            "    ",
          )
          val classDocTree: Option[TextTree[PyValue]] =
            if (classDocstring.isEmpty) None else Some(q"$classDocstring")

          val members =
            List(
              classDocTree,
              Some(dtoFieldsTrees.joinN()),
              Some(modelConfig),
              dtoProperties.map(_.joinN()),
              Some(mainMeta.joinN()),
              identifierRepr,
            ).flatten

          val classTree =
            q"""class ${dto.id.name.name.capitalize}($parents):
               |    ${members.joinNN().shift(4).trim}
               |""".stripMargin

          val combined = identifierCodec match {
            case Some(codecTree) =>
              q"""$classTree
                 |
                 |$codecTree
                 |""".stripMargin
            case None => classTree
          }

          PyDefnRepr(combined, List.empty)

        case enum: Typedef.Enum =>
          val branches = enum.members.map {
            m =>
              val wireName      = EnumWireStyle.wireName(m.name)
              // Escape Python keywords in enum member identifiers. The value (wire-string) stays
              // as the original wire name; only the Python attribute name gets the trailing `_`.
              val memberPyIdent = escapePyKeyword(wireName)
              q"$memberPyIdent = \"$wireName\""
          }.toSeq
          val enumDocstring = pyTreeTools.renderClassDocstring(defn.docs, Seq.empty, "    ")
          val enumDocTree: Seq[TextTree[PyValue]] =
            if (enumDocstring.isEmpty) Seq.empty else Seq(q"$enumDocstring")
          val enumMembers = (enumDocTree ++ branches).joinN()
          PyDefnRepr(
            q"""|class ${enum.id.name.name.capitalize}($pyEnum):
                |    ${enumMembers.shift(4).trim}
                |""".stripMargin,
            List.empty,
          )
        case adt: Typedef.Adt =>
          val adtContractDefs    = adt.contracts.flatMap(domain.defs.meta.nodes.get).collect { case u: DomainMember.User => u }
          val adtSuperclasses    = baboonEnquiries.collectParents(domain, adtContractDefs).toSet
          val uniqueAdtContracts = adt.contracts.filterNot(c => adtSuperclasses.contains(c))
          val contracts          = uniqueAdtContracts.map(c => typeTranslator.asPyType(c, domain, evolution, fileTools.definitionsBasePkg))
          val defaultParents     = contracts ++ List(pydanticBaseModel)
          val genMarkerParent    = if (adt.contracts.isEmpty) List(genMarker) else Nil
          val allParents         = defaultParents ++ genMarkerParent
          val parents            = mkParents(allParents)

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

          val adtDocstring = pyTreeTools.renderClassDocstring(defn.docs, Seq.empty, "    ")
          val adtDocTree: Option[TextTree[PyValue]] =
            if (adtDocstring.isEmpty) None else Some(q"$adtDocstring")

          val members = List(
            adtDocTree,
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
              val name = escapePyKeyword(f.name.name)
              q"""@$pyAbstractMethod
                 |def $name(self) -> $tpe:
                 |    raise NotImplementedError
                 |""".stripMargin
          }
          val contractDocstring = pyTreeTools.renderClassDocstring(defn.docs, Seq.empty, "    ")
          val contractDocTree: Option[TextTree[PyValue]] =
            if (contractDocstring.isEmpty) None else Some(q"$contractDocstring")
          val contractBody = contractDocTree.toList ++ (if (methods.isEmpty) List(q"pass") else methods)
          val allMethods = contractBody.joinN()
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
            case ResolvedServiceContext.NoContext               => ""
            case ResolvedServiceContext.AbstractContext(tn, pn) => s"$pn: $tn, "
            case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$pn: $tn, "
          }
          val methods = service.methods.map {
            m =>
              val inType  = typeTranslator.asPyRef(m.sig, domain, evolution, fileTools.definitionsBasePkg)
              val outType = m.out.map(typeTranslator.asPyRef(_, domain, evolution, fileTools.definitionsBasePkg))
              val errType = m.err.map(typeTranslator.asPyRef(_, domain, evolution, fileTools.definitionsBasePkg))
              val retAnnotation: TextTree[PyValue] = if (resolved.noErrors || errType.isEmpty) {
                outType.getOrElse(q"None")
              } else {
                val pyName: PyValue => String = { case t: PyValue.PyType => t.name }
                val outStr                    = outType.map(_.mapRender(pyName)).getOrElse("")
                val errStr                    = errType.map(_.mapRender(pyName))
                val retStr                    = resolved.renderReturnType(outStr, errStr, "None")
                q"${"\"" + retStr + "\""}"
              }
              val methodDocstring = pyTreeTools.renderMethodDocstring(m.docs, "        ")
              val methodDocTree: Option[TextTree[PyValue]] =
                if (methodDocstring.isEmpty) None else Some(q"$methodDocstring")
              val methodBodyParts: List[TextTree[PyValue]] =
                methodDocTree.toList :+ q"raise NotImplementedError"
              val methodBodyTree = methodBodyParts.joinN()
              val asyncKw        = if (target.language.asyncServices) "async " else ""
              val methodPyName   = escapePyKeyword(m.name.name)
              q"""|@$pyAbstractMethod
                  |${asyncKw}def $methodPyName(self, ${ctxParam}arg: $inType) -> $retAnnotation:
                  |    ${methodBodyTree.shift(4).trim}
                  |""".stripMargin
          }
          val serviceDocstring = pyTreeTools.renderClassDocstring(defn.docs, Seq.empty, "    ")
          val serviceDocTree: Option[TextTree[PyValue]] =
            if (serviceDocstring.isEmpty) None else Some(q"$serviceDocstring")
          val serviceBody = serviceDocTree.toList ++ (if (methods.isEmpty) List(q"pass") else methods)
          val allMethods = serviceBody.joinN()
          // Abstract mode: the service interface is `Generic[Ctx]`. `Generic`
          // and `TypeVar` must be imported (reference them as PyTypes so the
          // import pass emits `from typing import Generic, TypeVar`) and the
          // `Ctx = TypeVar("Ctx")` declaration emitted at module scope so the
          // `Generic[Ctx]` base binds. Concrete/none keep the bare `ABC` base
          // and emit nothing extra (byte-identical).
          val (classBases, ctxTypeVarDecl): (TextTree[PyValue], Option[TextTree[PyValue]]) = resolvedCtx match {
            case ResolvedServiceContext.AbstractContext(tn, _) =>
              (q"$pyABC, $pyGeneric[$tn]", Some(q"""$tn = $pyTypeVar("$tn")"""))
            case _ =>
              (q"$pyABC", None)
          }
          val serviceClass =
            q"""|class ${service.id.name.name.capitalize}($classBases):
                |    ${allMethods.shift(4).trim}
                |""".stripMargin
          PyDefnRepr(
            (ctxTypeVarDecl.toSeq :+ serviceClass).joinNN(),
            List.empty,
          )
        case f: Typedef.Foreign => makeForeignKeyCodecRepr(f)
      }

    }

    /** PR-I.2 (M24 Phase 3.2) — emit a `<Foreign>_KeyCodec` extension hook for
      * every Custom-mapped Python foreign declaration. The host application
      * registers an implementation at boot which the JSON codec then uses to
      * encode/decode map keys. For BaboonRef-mapped foreigns we emit nothing —
      * the existing recursion into the aliased type covers the codec needs.
      *
      * Stringy foreigns (`builtins.str` / `str`) get a default identity impl so
      * the common case works out of the box. Non-stringy foreigns get a stub
      * default that raises `BaboonCodecException.DecoderFailure` with an
      * FQN-bearing diagnostic referring to the Host class (PR-I.1b-D01 lesson).
      */
    private def makeForeignKeyCodecRepr(f: Typedef.Foreign): PyDefnRepr = {
      f.bindings.get(BaboonLang.Py) match {
        case None                                                                  => PyDefnRepr(q"", List.empty)
        case Some(Typedef.ForeignEntry(_, _: Typedef.ForeignMapping.BaboonRef))    => PyDefnRepr(q"", List.empty)
        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) =>
          // Names: the user-facing identifier (e.g. "FStr") is taken from the keep-foreigns
          // accessor; the runtime Python type is the deref'd `pyRef` (e.g. `str` for `builtins.str`).
          // Foreign types are not emitted as Python typealiases (see line 90+ of mkRepr — pre-PR
          // emitted q"" for Foreign), so the user-facing name "FStr" doesn't exist as a Python
          // identifier. Methods accept/return the runtime Python type directly.
          val srcRef          = typeTranslator.asPyTypeKeepForeigns(f.id, domain, evolution, fileTools.definitionsBasePkg)
          val pyRef           = typeTranslator.asPyType(f.id, domain, evolution, fileTools.definitionsBasePkg)
          val codecName       = s"${srcRef.name}_KeyCodec"
          val hostName        = s"${srcRef.name}_KeyCodecHost"
          val hostFqn         = s"${srcRef.moduleId.path.toList.mkString(".")}.$hostName"
          val instanceVar     = s"_${codecName}_instance"
          val defaultImplName = s"_Default$codecName"
          // Stringy allowlist (PR-I-D06 pattern guidance: only the language's allowlist; no dead alternatives).
          val isStringy = decl == "builtins.str" || decl == "str"
          val defaultImpl = if (isStringy) {
            q"""class $defaultImplName:
               |    def encode_key(self, value: $pyRef) -> $pyStr:
               |        return value
               |    def decode_key(self, s: $pyStr) -> $pyRef:
               |        return s""".stripMargin
          } else {
            q"""class $defaultImplName:
               |    def encode_key(self, value: $pyRef) -> $pyStr:
               |        raise $baboonCodecException.DecoderFailure(f"$hostFqn is not registered; call $hostFqn.register(impl) at app boot.")
               |    def decode_key(self, s: $pyStr) -> $pyRef:
               |        raise $baboonCodecException.DecoderFailure(f"$hostFqn is not registered; call $hostFqn.register(impl) at app boot.")""".stripMargin
          }
          val tree =
            q"""class $codecName($pyProtocol):
               |    def encode_key(self, value: $pyRef) -> $pyStr: ...
               |    def decode_key(self, s: $pyStr) -> $pyRef: ...
               |
               |$defaultImpl
               |
               |$instanceVar: $codecName = $defaultImplName()
               |
               |class $hostName:
               |    @$pyStaticMethod
               |    def register(impl: $codecName) -> None:
               |        global $instanceVar
               |        $instanceVar = impl
               |    @$pyStaticMethod
               |    def instance() -> $codecName:
               |        return $instanceVar""".stripMargin
          PyDefnRepr(tree, List.empty)
      }
    }

    private def genDtoFields(dtoFields: List[Field], contractsFields: Set[Field]): List[TextTree[PyValue]] = {
      val fields = dtoFields.map {
        field =>
          val fieldName   = field.name.name
          val fieldType   = typeTranslator.asPyRef(field.tpe, domain, evolution, fileTools.definitionsBasePkg)
          // A trailing `_` suffix is needed when either the field is a Python keyword (PEP 8
          // convention) or the field implements a contract abstract method (to avoid a Python
          // name-clash with the @property accessor). In both cases a pydantic alias preserves the
          // original model name as the wire key.
          val needsAlias  = contractsFields.contains(field) || PyKeywords.isKeyword(fieldName)

          if (needsAlias) {
            q"${fieldName}_: $fieldType = $pydanticField(alias='$fieldName', serialization_alias='$fieldName')"
          } else q"$fieldName: $fieldType"
      }
      if (fields.isEmpty) List(q"pass") else fields
    }

    private def genDtoProperties(contractsFields: List[Field]): Option[List[TextTree[PyValue]]] = {
      if (contractsFields.nonEmpty) {
        val properties = contractsFields
          .map {
            f =>
              val fieldName    = f.name.name
              // The backing pydantic field is always `${fieldName}_` (see genDtoFields).
              // The property accessor uses the keyword-escaped name so the method declaration is
              // valid Python (e.g. `def class_(self)` instead of `def class(self)`).
              val propertyName = escapePyKeyword(fieldName)
              q"""@property
                 |def $propertyName(self) -> ${typeTranslator.asPyRef(f.tpe, domain, evolution, fileTools.definitionsBasePkg)}:
                 |    return self.${fieldName}_
                 |""".stripMargin
          }
        Some(properties)
      } else None
    }

    private def genDtoPydanticModelConf(dtoFields: List[Field], hasContracts: Boolean, jsonCodecActive: Boolean): TextTree[PyType] = {
      val frozen             = Some(q"frozen=True")
      val hasKeywordField    = dtoFields.exists(f => PyKeywords.isKeyword(f.name.name))
      val serializeByAlias   = if (jsonCodecActive && (hasContracts || hasKeywordField)) Some(q"serialize_by_alias=True") else None
      // populate_by_name=True allows constructing instances using the Python attribute name (the
      // escaped name, e.g. `class_`) instead of the alias (the original model name `class`). This
      // is required when any field is a Python keyword, because the alias cannot be used as a
      // constructor keyword argument in generated Python code.
      val populateByName     = if (hasKeywordField) Some(q"populate_by_name=True") else None
      val serializeJsonBytesAsHex =
        if (dtoFields.map(_.tpe.id).contains(TypeId.Builtins.bytes)) {
          List(
            q"ser_json_bytes='hex'",
            q"val_json_bytes='hex'",
          )
        } else Nil

      val serializeDecimalAsJsonNumber =
        if (dtoFields.map(_.tpe.id).contains(TypeId.Builtins.f128)) {
          List(q"json_encoders={Decimal: str}")
        } else Nil

      // pydantic doesn't know how to validate the runtime `AnyOpaque` ABC; opt into
      // arbitrary-types when any field carries an `any`-typed payload (direct or nested via a
      // constructor like `lst[any]` / `opt[any]` / `map[str, any]`).
      def hasAnyType(tpe: TypeRef): Boolean = tpe match {
        case _: TypeRef.Any         => true
        case _: TypeRef.Scalar      => false
        case c: TypeRef.Constructor => c.args.exists(hasAnyType)
      }
      val arbitraryTypesAllowed =
        if (dtoFields.exists(f => hasAnyType(f.tpe))) Some(q"arbitrary_types_allowed=True") else None

      val configs = List(frozen, serializeByAlias, populateByName, serializeJsonBytesAsHex, serializeDecimalAsJsonNumber, arbitraryTypesAllowed).flatten

      q"""model_config = $pydanticConfigDict(
         |    ${configs.join(",\n").shift(4).trim}
         |)""".stripMargin
    }

    private def mkParents(refs: List[PyType]): TextTree[PyValue] = {
      if (refs.isEmpty) q"" else q"${refs.map(s => q"$s").join(", ")}"
    }

    // ----- Identifier toString + parse_repr emission (PR-57d) -----
    // Spec: docs/spec/identifier-repr.md. Mirrors the per-language emitters
    // in PR-57a/b/c. Python idioms:
    //   - `__repr__(self) -> str` method on the dataclass
    //   - sibling `<TypeName>Codec` class with `@staticmethod parse_repr` (Q-FU-4)
    //   - snake_case method names per PEP 8
    private sealed trait IdentifierFieldKind
    private object IdentifierFieldKind {
      case object Bit extends IdentifierFieldKind
      case object SignedInt extends IdentifierFieldKind /* i08/i16/i32 */
      case object SignedLong extends IdentifierFieldKind /* i64 */
      case object UnsignedSmallInt extends IdentifierFieldKind /* u08/u16/u32 */
      case object UnsignedLong extends IdentifierFieldKind /* u64 */
      case object Str extends IdentifierFieldKind
      case object Uid extends IdentifierFieldKind
      case object Tsu extends IdentifierFieldKind
      case object Tso extends IdentifierFieldKind
      case object Bytes extends IdentifierFieldKind
      final case class NestedId(id: TypeId.User) extends IdentifierFieldKind
    }

    private def identifierFieldKindPy(tpe: TypeRef): IdentifierFieldKind = {
      tpe match {
        case TypeRef.Scalar(b: TypeId.BuiltinScalar) =>
          import TypeId.Builtins.*
          b match {
            case `bit`                 => IdentifierFieldKind.Bit
            case `i08` | `i16` | `i32` => IdentifierFieldKind.SignedInt
            case `i64`                 => IdentifierFieldKind.SignedLong
            case `u08` | `u16` | `u32` => IdentifierFieldKind.UnsignedSmallInt
            case `u64`                 => IdentifierFieldKind.UnsignedLong
            case `str`                 => IdentifierFieldKind.Str
            case `uid`                 => IdentifierFieldKind.Uid
            case `tsu`                 => IdentifierFieldKind.Tsu
            case `tso`                 => IdentifierFieldKind.Tso
            case `bytes`               => IdentifierFieldKind.Bytes
            case other =>
              throw new IllegalStateException(s"Identifier field has unsupported scalar $other; validator should have rejected this.")
          }
        case TypeRef.Scalar(uid: TypeId.User) =>
          IdentifierFieldKind.NestedId(uid)
        case other =>
          throw new IllegalStateException(s"Identifier field has unsupported TypeRef $other; validator should have rejected this.")
      }
    }

    private def signedTypeNamePy(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => "i08"
      case TypeRef.Scalar(TypeId.Builtins.i16) => "i16"
      case TypeRef.Scalar(TypeId.Builtins.i32) => "i32"
      case other                               => throw new IllegalStateException(s"signedTypeNamePy on non-signed-int: $other")
    }

    private def signedRangeCheckPy(tpe: TypeRef, varName: String): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i08) => s"-128 <= $varName <= 127"
      case TypeRef.Scalar(TypeId.Builtins.i16) => s"-32768 <= $varName <= 32767"
      case TypeRef.Scalar(TypeId.Builtins.i32) => s"-2147483648 <= $varName <= 2147483647"
      case other                               => throw new IllegalStateException(s"signedRangeCheckPy on non-signed-int: $other")
    }

    private def unsignedSmallTypeNamePy(tpe: TypeRef): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => "u08"
      case TypeRef.Scalar(TypeId.Builtins.u16) => "u16"
      case TypeRef.Scalar(TypeId.Builtins.u32) => "u32"
      case other                               => throw new IllegalStateException(s"unsignedSmallTypeNamePy on non-u08/u16/u32: $other")
    }

    private def unsignedSmallRangeCheckPy(tpe: TypeRef, varName: String): String = tpe match {
      case TypeRef.Scalar(TypeId.Builtins.u08) => s"0 <= $varName <= 255"
      case TypeRef.Scalar(TypeId.Builtins.u16) => s"0 <= $varName <= 65535"
      case TypeRef.Scalar(TypeId.Builtins.u32) => s"0 <= $varName <= 4294967295"
      case other                               => throw new IllegalStateException(s"unsignedSmallRangeCheckPy on non-u08/u16/u32: $other")
    }

    private def renderIdentifierFieldValueExprPy(pyFieldName: String, kind: IdentifierFieldKind): TextTree[PyValue] = {
      kind match {
        case IdentifierFieldKind.Bit              => q"$baboonIdReprBitToString(self.$pyFieldName)"
        case IdentifierFieldKind.SignedInt        => q"str(self.$pyFieldName)"
        case IdentifierFieldKind.SignedLong       => q"str(self.$pyFieldName)"
        case IdentifierFieldKind.UnsignedSmallInt => q"str(self.$pyFieldName)"
        case IdentifierFieldKind.UnsignedLong     => q"$baboonIdReprU64ToString(self.$pyFieldName)"
        case IdentifierFieldKind.Str              => q"$baboonIdReprEscapeStr(self.$pyFieldName)"
        // UUID's str() is the lowercase canonical form per RFC 4122.
        case IdentifierFieldKind.Uid         => q"str(self.$pyFieldName)"
        case IdentifierFieldKind.Tsu         => q"$baboonIdReprTsuToString(self.$pyFieldName)"
        case IdentifierFieldKind.Tso         => q"$baboonIdReprTsoToString(self.$pyFieldName)"
        case IdentifierFieldKind.Bytes       => q"$baboonIdReprBytesToHex(self.$pyFieldName)"
        case IdentifierFieldKind.NestedId(_) => q"""f'{${"\"{\""}}{self.$pyFieldName!r}{${"\"}\""}}'""".stripMargin
      }
    }

    private def renderIdentifierRepr(dto: Typedef.Dto): TextTree[PyValue] = {
      val simpleName = dto.id.name.name.capitalize
      val versionStr = domain.version.toString
      val header     = s"$simpleName:$versionStr#"

      val fieldExprs: List[TextTree[PyValue]] = dto.fields.map {
        f =>
          val srcFieldName     = f.name.name
          val escapedFieldName = escapePyKeyword(srcFieldName)
          val kind             = identifierFieldKindPy(f.tpe)
          val valueExpr        = renderIdentifierFieldValueExprPy(escapedFieldName, kind)
          q""""$srcFieldName:" + ($valueExpr)"""
      }

      val joinedFields =
        if (fieldExprs.isEmpty) q""""""""
        else fieldExprs.toSeq.join(""" + ":" + """)

      q"""def __repr__(self) -> str:
         |    return "$header" + $joinedFields
         |
         |def __str__(self) -> str:
         |    return self.__repr__()
         |""".stripMargin
    }

    private def renderIdentifierCodecClass(dto: Typedef.Dto): TextTree[PyValue] = {
      val simpleName     = dto.id.name.name.capitalize
      val versionStr     = domain.version.toString
      val codecClassName = s"${simpleName}Codec"

      val fieldDecoders: List[TextTree[PyValue]] = dto.fields.zipWithIndex.map {
        case (f, idx) =>
          val srcFieldName = f.name.name
          val rawVar       = s"${srcFieldName}_raw"
          val valVar       = s"${srcFieldName}_v"
          val resVar       = s"${srcFieldName}_r"
          val isLast       = idx == dto.fields.length - 1
          val kind         = identifierFieldKindPy(f.tpe)

          val parseHead =
            q"""${srcFieldName}_fnr = $baboonIdReprParseFieldName(cursor, "$srcFieldName")
               |if isinstance(${srcFieldName}_fnr, $baboonLeftType):
               |    return ${srcFieldName}_fnr""".stripMargin

          val parseValue: TextTree[PyValue] = kind match {
            case IdentifierFieldKind.Bit =>
              q"""$rawVar = cursor.read_until_structural()
                 |$resVar = $baboonIdReprParseBit($rawVar)
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value""".stripMargin
            case IdentifierFieldKind.SignedInt =>
              val typeName   = signedTypeNamePy(f.tpe)
              val parsedVar  = s"${srcFieldName}_n"
              val rangeCheck = signedRangeCheckPy(f.tpe, parsedVar)
              q"""$rawVar = cursor.read_until_structural()
                 |# Spec §5.4: signed integers must not carry a leading '+'.
                 |if $rawVar and $rawVar[0] == '+':
                 |    return $baboonLeftType(f"signed integer must not have leading '+' for field $srcFieldName: {$rawVar}")
                 |try:
                 |    $parsedVar = int($rawVar)
                 |except ValueError:
                 |    return $baboonLeftType(f"could not parse signed integer for field $srcFieldName: {$rawVar}")
                 |if not ($rangeCheck):
                 |    return $baboonLeftType(f"$typeName out of range for field $srcFieldName: {$rawVar}")
                 |$valVar = $parsedVar""".stripMargin
            case IdentifierFieldKind.SignedLong =>
              // Python ints are arbitrary precision. i64 range check elided per
              // PR-57a-D01 carryover (no dead `if not (always-true)` block).
              q"""$rawVar = cursor.read_until_structural()
                 |# Spec §5.4: signed integers must not carry a leading '+'.
                 |if $rawVar and $rawVar[0] == '+':
                 |    return $baboonLeftType(f"signed integer must not have leading '+' for field $srcFieldName: {$rawVar}")
                 |try:
                 |    $valVar = int($rawVar)
                 |except ValueError:
                 |    return $baboonLeftType(f"could not parse i64 for field $srcFieldName: {$rawVar}")""".stripMargin
            case IdentifierFieldKind.UnsignedSmallInt =>
              val typeName   = unsignedSmallTypeNamePy(f.tpe)
              val parsedVar  = s"${srcFieldName}_n"
              val rangeCheck = unsignedSmallRangeCheckPy(f.tpe, parsedVar)
              q"""$rawVar = cursor.read_until_structural()
                 |if $rawVar and $rawVar[0] in ('+', '-'):
                 |    return $baboonLeftType(f"unsigned value has leading sign for field $srcFieldName: {$rawVar}")
                 |try:
                 |    $parsedVar = int($rawVar)
                 |except ValueError:
                 |    return $baboonLeftType(f"could not parse unsigned integer for field $srcFieldName: {$rawVar}")
                 |if not ($rangeCheck):
                 |    return $baboonLeftType(f"$typeName out of range for field $srcFieldName: {$rawVar}")
                 |$valVar = $parsedVar""".stripMargin
            case IdentifierFieldKind.UnsignedLong =>
              val parsedVar = s"${srcFieldName}_n"
              q"""$rawVar = cursor.read_until_structural()
                 |if $rawVar and $rawVar[0] in ('+', '-'):
                 |    return $baboonLeftType(f"unsigned value has leading sign for field $srcFieldName: {$rawVar}")
                 |try:
                 |    $parsedVar = int($rawVar)
                 |except ValueError:
                 |    return $baboonLeftType(f"could not parse u64 for field $srcFieldName: {$rawVar}")
                 |if not (0 <= $parsedVar <= 18446744073709551615):
                 |    return $baboonLeftType(f"u64 out of range for field $srcFieldName: {$rawVar}")
                 |$valVar = $parsedVar""".stripMargin
            case IdentifierFieldKind.Str =>
              q"""$resVar = cursor.read_str_field()
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value""".stripMargin
            case IdentifierFieldKind.Uid =>
              q"""$rawVar = cursor.read_until_structural()
                 |if not $baboonIdReprIsCanonicalUid($rawVar):
                 |    return $baboonLeftType(f"uid not in canonical lowercase form for field $srcFieldName: {$rawVar}")
                 |try:
                 |    $valVar = $pyUuid($rawVar)
                 |except ValueError:
                 |    return $baboonLeftType(f"could not parse uid for field $srcFieldName: {$rawVar}")""".stripMargin
            case IdentifierFieldKind.Tsu =>
              q"""${srcFieldName}_rrf = cursor.read_fixed(24)
                 |if isinstance(${srcFieldName}_rrf, $baboonLeftType):
                 |    return ${srcFieldName}_rrf
                 |$rawVar = ${srcFieldName}_rrf.value
                 |$resVar = $baboonIdReprParseTsu($rawVar)
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value""".stripMargin
            case IdentifierFieldKind.Tso =>
              q"""${srcFieldName}_rrf = cursor.read_fixed(29)
                 |if isinstance(${srcFieldName}_rrf, $baboonLeftType):
                 |    return ${srcFieldName}_rrf
                 |$rawVar = ${srcFieldName}_rrf.value
                 |$resVar = $baboonIdReprParseTso($rawVar)
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value""".stripMargin
            case IdentifierFieldKind.Bytes =>
              q"""$rawVar = cursor.read_until_structural()
                 |$resVar = $baboonIdReprParseBytesHex($rawVar)
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value""".stripMargin
            case IdentifierFieldKind.NestedId(uid) =>
              val nestedTpe   = typeTranslator.asPyTypeKeepForeigns(uid, domain, evolution, fileTools.definitionsBasePkg)
              val nestedCodec = PyType(nestedTpe.moduleId, s"${nestedTpe.name}Codec")
              q"""${srcFieldName}_ro = cursor.expect("{")
                 |if isinstance(${srcFieldName}_ro, $baboonLeftType):
                 |    return ${srcFieldName}_ro
                 |$resVar = $nestedCodec.parse_repr_cursor(cursor)
                 |if isinstance($resVar, $baboonLeftType):
                 |    return $resVar
                 |$valVar = $resVar.value
                 |${srcFieldName}_rc = cursor.expect("}")
                 |if isinstance(${srcFieldName}_rc, $baboonLeftType):
                 |    return ${srcFieldName}_rc""".stripMargin
          }

          val sep =
            if (isLast) q""
            else
              q"""${srcFieldName}_rsep = cursor.expect(":")
                 |if isinstance(${srcFieldName}_rsep, $baboonLeftType):
                 |    return ${srcFieldName}_rsep""".stripMargin

          q"""$parseHead
             |$parseValue
             |$sep""".stripMargin.trim
      }

      val ctorArgs = dto.fields.map {
        f =>
          // Use the keyword-escaped attribute name as the constructor kwarg.
          // Non-keyword fields: `foo=foo_v` (unchanged from before).
          // Keyword fields: `class_=class_v` (requires populate_by_name=True in model_config).
          val attrName = if (PyKeywords.isKeyword(f.name.name)) s"${f.name.name}_" else f.name.name
          q"$attrName=${f.name.name}_v"
      }

      val ctor =
        if (ctorArgs.nonEmpty)
          q"""$simpleName(
             |    ${ctorArgs.join(",\n").shift(4).trim}
             |)""".stripMargin
        else q"$simpleName()"

      val body = (fieldDecoders :+ q"return $baboonRightType($ctor)").joinNN()

      val bodyForEmpty = if (fieldDecoders.isEmpty) {
        // Empty-fields id: just emit the construction so we don't end up with
        // an empty function body (Python requires at least `pass` or a stmt).
        q"return $baboonRightType($ctor)"
      } else body

      q"""class $codecClassName:
         |    \"\"\"Schema-directed parser for the canonical identifier repr per
         |    docs/spec/identifier-repr.md. NOT a method on the dataclass — Q-FU-4.
         |    \"\"\"
         |
         |    @$pyStaticMethod
         |    def parse_repr(s: str) -> $baboonEitherType:
         |        cursor = $baboonIdReprCursor(s)
         |        inner = $codecClassName.parse_repr_cursor(cursor)
         |        if isinstance(inner, $baboonLeftType):
         |            return inner
         |        if not cursor.at_end():
         |            return $baboonLeftType(f"unexpected trailing input at {cursor.position()}")
         |        return inner
         |
         |    @$pyStaticMethod
         |    def parse_repr_cursor(cursor: $baboonIdReprCursor) -> $baboonEitherType:
         |        h = $baboonIdReprParseHeader(cursor, "$simpleName", "$versionStr")
         |        if isinstance(h, $baboonLeftType):
         |            return h
         |        ${bodyForEmpty.shift(8).trim}
         |""".stripMargin
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
