package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.{PyModuleId, PyType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait PyServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[PyValue]]

  def translateServiceRt(domain: Domain): Option[TextTree[PyValue]]
}

object PyServiceWiringTranslator {
  class Impl(
    target: PyTarget,
    typeTranslator: PyTypeTranslator,
    codecs: Set[PyCodecTranslator],
    domain: Domain,
    evolution: BaboonEvolution,
    fileTools: PyFileTools,
  ) extends PyServiceWiringTranslator {

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "python", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "python", target.language.serviceContext, target.language.pragmas)

    private val hasJsonCodecs: Boolean = codecs.exists(_.isInstanceOf[PyJsonCodecGenerator])
    private val hasUebaCodecs: Boolean = codecs.exists(_.isInstanceOf[PyUEBACodecGenerator])

    private def jsonCodecType(typeId: TypeId.User): PyType = {
      val typeName = s"${typeId.name.name.capitalize}_JsonCodec"
      val moduleId = typeTranslator.toPyModule(typeId, domain.version, evolution, fileTools.definitionsBasePkg)
      PyType(moduleId, typeName)
    }

    private def uebaCodecType(typeId: TypeId.User): PyType = {
      val typeName = s"${typeId.name.name.capitalize}_UEBACodec"
      val moduleId = typeTranslator.toPyModule(typeId, domain.version, evolution, fileTools.definitionsBasePkg)
      PyType(moduleId, typeName)
    }

    private lazy val serviceRtModule: PyModuleId = {
      val pathToModule = domain.id.path.toList
      val fullPath     = fileTools.definitionsBasePkg ++ pathToModule ++ List("BaboonServiceRt")
      PyModuleId(NEList.unsafeFrom(fullPath))
    }

    private lazy val ibaboonServiceRtType: PyType = PyType(serviceRtModule, "IBaboonServiceRt")

    override def translateServiceRt(domain: Domain): Option[TextTree[PyValue]] = {
      if (resolved.noErrors) return None

      val hasServices = domain.defs.meta.nodes.values.exists {
        case DomainMember.User(_, _: Typedef.Service, _, _) => true
        case _                                               => false
      }
      if (!hasServices) return None

      val rtTrait: TextTree[PyValue] =
        q"""class $ibaboonServiceRtType($pyABC):
           |    @$pyAbstractMethod
           |    def pure(self, value):
           |        raise NotImplementedError
           |
           |    @$pyAbstractMethod
           |    def fail(self, error):
           |        raise NotImplementedError
           |
           |    @$pyAbstractMethod
           |    def left_map(self, value, f):
           |        raise NotImplementedError
           |
           |    @$pyAbstractMethod
           |    def flat_map(self, value, f):
           |        raise NotImplementedError
           |""".stripMargin

      val isBuiltinEither = resolved.resultType.exists(t => t == "BaboonEither" || t.endsWith(".BaboonEither"))

      val defaultImpl: Option[TextTree[PyValue]] = if (isBuiltinEither) {
        Some(
          q"""class BaboonServiceRtDefault($ibaboonServiceRtType):
             |    def pure(self, value):
             |        return $baboonRightType(value)
             |
             |    def fail(self, error):
             |        return $baboonLeftType(error)
             |
             |    def left_map(self, value, f):
             |        if isinstance(value, $baboonLeftType):
             |            return $baboonLeftType(f(value.value))
             |        return value
             |
             |    def flat_map(self, value, f):
             |        if isinstance(value, $baboonLeftType):
             |            return value
             |        return f(value.value)
             |""".stripMargin
        )
      } else None

      Some(Seq[Option[TextTree[PyValue]]](Some(rtTrait), defaultImpl).flatten.joinNN())
    }

    override def translate(defn: DomainMember.User): Option[TextTree[PyValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val methods =
            if (resolved.noErrors) generateNoErrorsWiring(service)
            else generateErrorsWiring(service)
          Some(methods)
        case _ => None
      }
    }

    private def ctxParamDecl: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext               => ""
      case ResolvedServiceContext.AbstractContext(tn, pn) => s"$pn: $tn, "
      case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$pn: $tn, "
    }

    private def ctxArgPass: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext              => ""
      case ResolvedServiceContext.AbstractContext(_, pn) => s"$pn, "
      case ResolvedServiceContext.ConcreteContext(_, pn) => s"$pn, "
    }

    // ========== noErrors mode ==========

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[PyValue] = {
      val svcName = service.id.name.name
      val svcType = typeTranslator.asPyType(service.id, domain, evolution, fileTools.definitionsBasePkg)

      val jsonFn = if (hasJsonCodecs)
        Some(generateNoErrorsJsonFn(service, svcName, svcType))
      else None

      val uebaFn = if (hasUebaCodecs)
        Some(generateNoErrorsUebaFn(service, svcName, svcType))
      else None

      Seq(jsonFn, uebaFn).flatten.joinNN()
    }

    private def generateNoErrorsJsonFn(service: Typedef.Service, svcName: String, svcType: PyType): TextTree[PyValue] = {
      val cases = service.methods.map { m =>
        val inCodec = jsonCodecType(m.sig.id.asInstanceOf[TypeId.User])

        val encodeAndReturn = m.out match {
          case Some(outRef) =>
            val outCodec = jsonCodecType(outRef.id.asInstanceOf[TypeId.User])
            q"""result = impl.${m.name.name}(${ctxArgPass}decoded)
               |try:
               |    return $outCodec.instance().encode(ctx, result)
               |except Exception as e:
               |    raise $baboonWiringException($baboonEncoderFailed(method, e))""".stripMargin
          case None =>
            q"""impl.${m.name.name}(${ctxArgPass}decoded)
               |return "null"
               |""".stripMargin
        }

        q"""if method.method_name == "${m.name.name}":
           |    try:
           |        decoded = $inCodec.instance().decode(ctx, data)
           |    except Exception as e:
           |        raise $baboonWiringException($baboonDecoderFailed(method, e))
           |    ${encodeAndReturn.shift(4).trim}""".stripMargin
      }.joinN()

      q"""def invoke_json_$svcName(method: $baboonMethodId, data: str, impl: $svcType, ${ctxParamDecl}ctx: $baboonCodecContext) -> str:
         |    ${cases.shift(4).trim}
         |    raise $baboonWiringException($baboonNoMatchingMethod(method))
         |""".stripMargin
    }

    private def generateNoErrorsUebaFn(service: Typedef.Service, svcName: String, svcType: PyType): TextTree[PyValue] = {
      val cases = service.methods.map { m =>
        val inCodec = uebaCodecType(m.sig.id.asInstanceOf[TypeId.User])

        val encodeAndReturn = m.out match {
          case Some(outRef) =>
            val outCodec = uebaCodecType(outRef.id.asInstanceOf[TypeId.User])
            q"""result = impl.${m.name.name}(${ctxArgPass}decoded)
               |try:
               |    output_stream = $pyBytesIO()
               |    writer = $baboonLEDataOutputStream(output_stream)
               |    $outCodec.instance().encode(ctx, writer, result)
               |    return output_stream.getvalue()
               |except Exception as e:
               |    raise $baboonWiringException($baboonEncoderFailed(method, e))""".stripMargin
          case None =>
            q"""impl.${m.name.name}(${ctxArgPass}decoded)
               |return bytes()
               |""".stripMargin
        }

        q"""if method.method_name == "${m.name.name}":
           |    try:
           |        reader = $baboonLEDataInputStream($pyBytesIO(data))
           |        decoded = $inCodec.instance().decode(ctx, reader)
           |    except Exception as e:
           |        raise $baboonWiringException($baboonDecoderFailed(method, e))
           |    ${encodeAndReturn.shift(4).trim}""".stripMargin
      }.joinN()

      q"""def invoke_ueba_$svcName(method: $baboonMethodId, data: bytes, impl: $svcType, ${ctxParamDecl}ctx: $baboonCodecContext) -> bytes:
         |    ${cases.shift(4).trim}
         |    raise $baboonWiringException($baboonNoMatchingMethod(method))
         |""".stripMargin
    }

    // ========== errors mode ==========

    private def generateErrorsWiring(service: Typedef.Service): TextTree[PyValue] = {
      val svcName = service.id.name.name
      val svcType = typeTranslator.asPyType(service.id, domain, evolution, fileTools.definitionsBasePkg)

      val jsonFn = if (hasJsonCodecs)
        Some(generateErrorsJsonFn(service, svcName, svcType))
      else None

      val uebaFn = if (hasUebaCodecs)
        Some(generateErrorsUebaFn(service, svcName, svcType))
      else None

      Seq(jsonFn, uebaFn).flatten.joinNN()
    }

    private def generateErrorsJsonFn(service: Typedef.Service, svcName: String, svcType: PyType): TextTree[PyValue] = {
      val cases = service.methods.map { m =>
        val inCodec = jsonCodecType(m.sig.id.asInstanceOf[TypeId.User])
        val hasErrType = m.err.isDefined && !resolved.noErrors

        val decodeStep =
          q"""try:
             |    decoded = $inCodec.instance().decode(ctx, data)
             |    input_val = rt.pure(decoded)
             |except Exception as e:
             |    input_val = rt.fail($baboonDecoderFailed(method, e))""".stripMargin

        val callAndEncodeStep = m.out match {
          case Some(outRef) =>
            val outCodec = jsonCodecType(outRef.id.asInstanceOf[TypeId.User])

            val callBody = if (hasErrType) {
              q"""def _call(v):
                 |    try:
                 |        call_result = impl.${m.name.name}(${ctxArgPass}v)
                 |        return rt.left_map(call_result, lambda err: $baboonCallFailed(method, err))
                 |    except Exception as e:
                 |        return rt.fail($baboonCallFailed(method, e))""".stripMargin
            } else {
              q"""def _call(v):
                 |    try:
                 |        return rt.pure(impl.${m.name.name}(${ctxArgPass}v))
                 |    except Exception as e:
                 |        return rt.fail($baboonCallFailed(method, e))""".stripMargin
            }

            q"""$callBody
               |output = rt.flat_map(input_val, _call)
               |def _encode(v):
               |    try:
               |        return rt.pure($outCodec.instance().encode(ctx, v))
               |    except Exception as e:
               |        return rt.fail($baboonEncoderFailed(method, e))
               |return rt.flat_map(output, _encode)""".stripMargin

          case None =>
            val callBody = if (hasErrType) {
              q"""def _call(v):
                 |    try:
                 |        call_result = impl.${m.name.name}(${ctxArgPass}v)
                 |        return rt.left_map(call_result, lambda err: $baboonCallFailed(method, err))
                 |    except Exception as e:
                 |        return rt.fail($baboonCallFailed(method, e))""".stripMargin
            } else {
              q"""def _call(v):
                 |    try:
                 |        impl.${m.name.name}(${ctxArgPass}v)
                 |        return rt.pure("null")
                 |    except Exception as e:
                 |        return rt.fail($baboonCallFailed(method, e))""".stripMargin
            }

            q"""$callBody
               |return rt.flat_map(input_val, _call)""".stripMargin
        }

        q"""if method.method_name == "${m.name.name}":
           |    ${decodeStep.shift(4).trim}
           |    ${callAndEncodeStep.shift(4).trim}""".stripMargin
      }.joinN()

      q"""def invoke_json_$svcName(method: $baboonMethodId, data: str, impl: $svcType, rt: $ibaboonServiceRtType, ${ctxParamDecl}ctx: $baboonCodecContext):
         |    ${cases.shift(4).trim}
         |    return rt.fail($baboonNoMatchingMethod(method))
         |""".stripMargin
    }

    private def generateErrorsUebaFn(service: Typedef.Service, svcName: String, svcType: PyType): TextTree[PyValue] = {
      val cases = service.methods.map { m =>
        val inCodec = uebaCodecType(m.sig.id.asInstanceOf[TypeId.User])
        val hasErrType = m.err.isDefined && !resolved.noErrors

        val decodeStep =
          q"""try:
             |    reader = $baboonLEDataInputStream($pyBytesIO(data))
             |    decoded = $inCodec.instance().decode(ctx, reader)
             |    input_val = rt.pure(decoded)
             |except Exception as e:
             |    input_val = rt.fail($baboonDecoderFailed(method, e))""".stripMargin

        val callAndEncodeStep = m.out match {
          case Some(outRef) =>
            val outCodec = uebaCodecType(outRef.id.asInstanceOf[TypeId.User])

            val callBody = if (hasErrType) {
              q"""def _call(v):
                 |    try:
                 |        call_result = impl.${m.name.name}(${ctxArgPass}v)
                 |        return rt.left_map(call_result, lambda err: $baboonCallFailed(method, err))
                 |    except Exception as e:
                 |        return rt.fail($baboonCallFailed(method, e))""".stripMargin
            } else {
              q"""def _call(v):
                 |    try:
                 |        return rt.pure(impl.${m.name.name}(${ctxArgPass}v))
                 |    except Exception as e:
                 |        return rt.fail($baboonCallFailed(method, e))""".stripMargin
            }

            q"""$callBody
               |output = rt.flat_map(input_val, _call)
               |def _encode(v):
               |    try:
               |        output_stream = $pyBytesIO()
               |        writer = $baboonLEDataOutputStream(output_stream)
               |        $outCodec.instance().encode(ctx, writer, v)
               |        return rt.pure(output_stream.getvalue())
               |    except Exception as e:
               |        return rt.fail($baboonEncoderFailed(method, e))
               |return rt.flat_map(output, _encode)""".stripMargin

          case None =>
            val callBody = if (hasErrType) {
              q"""def _call(v):
                 |    try:
                 |        call_result = impl.${m.name.name}(${ctxArgPass}v)
                 |        return rt.left_map(call_result, lambda err: $baboonCallFailed(method, err))
                 |    except Exception as e:
                 |        return rt.fail($baboonCallFailed(method, e))""".stripMargin
            } else {
              q"""def _call(v):
                 |    try:
                 |        impl.${m.name.name}(${ctxArgPass}v)
                 |        return rt.pure(bytes())
                 |    except Exception as e:
                 |        return rt.fail($baboonCallFailed(method, e))""".stripMargin
            }

            q"""$callBody
               |return rt.flat_map(input_val, _call)""".stripMargin
        }

        q"""if method.method_name == "${m.name.name}":
           |    ${decodeStep.shift(4).trim}
           |    ${callAndEncodeStep.shift(4).trim}""".stripMargin
      }.joinN()

      q"""def invoke_ueba_$svcName(method: $baboonMethodId, data: bytes, impl: $svcType, rt: $ibaboonServiceRtType, ${ctxParamDecl}ctx: $baboonCodecContext):
         |    ${cases.shift(4).trim}
         |    return rt.fail($baboonNoMatchingMethod(method))
         |""".stripMargin
    }
  }
}
