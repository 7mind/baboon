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

    private def hasActiveJsonCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[PyJsonCodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def hasActiveUebaCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[PyUEBACodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

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

    // JSON encode/decode for both User types (via generated codec) and BuiltinScalar (inline).
    // Python JSON codecs take/return `str` (raw JSON). For builtins, use json.loads/json.dumps.
    private def jsonDecodeExpr(id: TypeId, data: TextTree[PyValue]): TextTree[PyValue] = id match {
      case u: TypeId.User          => q"${jsonCodecType(u)}.instance().decode(ctx, $data)"
      case _: TypeId.BuiltinScalar => q"$pyJsonLoads($data)"
      case other                   => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def jsonEncodeExpr(id: TypeId, value: TextTree[PyValue]): TextTree[PyValue] = id match {
      case u: TypeId.User          => q"${jsonCodecType(u)}.instance().encode(ctx, $value)"
      case _: TypeId.BuiltinScalar => q"$pyJsonDumps($value)"
      case other                   => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaDecodeExpr(id: TypeId, reader: TextTree[PyValue]): TextTree[PyValue] = id match {
      case u: TypeId.User => q"${uebaCodecType(u)}.instance().decode(ctx, $reader)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$reader.read_bool()"
          case TypeId.Builtins.i08   => q"$reader.read_byte()"
          case TypeId.Builtins.i16   => q"$reader.read_i16()"
          case TypeId.Builtins.i32   => q"$reader.read_i32()"
          case TypeId.Builtins.i64   => q"$reader.read_i64()"
          case TypeId.Builtins.u08   => q"$reader.read_ubyte()"
          case TypeId.Builtins.u16   => q"$reader.read_u16()"
          case TypeId.Builtins.u32   => q"$reader.read_u32()"
          case TypeId.Builtins.u64   => q"$reader.read_u64()"
          case TypeId.Builtins.f32   => q"$reader.read_f32()"
          case TypeId.Builtins.f64   => q"$reader.read_f64()"
          case TypeId.Builtins.f128  => q"$reader.read_f128()"
          case TypeId.Builtins.str   => q"$reader.read_string()"
          case TypeId.Builtins.uid   => q"$reader.read_uuid()"
          case TypeId.Builtins.tsu   => q"$reader.read_datetime()"
          case TypeId.Builtins.tso   => q"$reader.read_datetime()"
          case TypeId.Builtins.bytes => q"$reader.read_bytes()"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaEncodeStmt(id: TypeId, writer: TextTree[PyValue], value: TextTree[PyValue]): TextTree[PyValue] = id match {
      case u: TypeId.User => q"${uebaCodecType(u)}.instance().encode(ctx, $writer, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$writer.write_bool($value)"
          case TypeId.Builtins.i08   => q"$writer.write_byte($value)"
          case TypeId.Builtins.i16   => q"$writer.write_i16($value)"
          case TypeId.Builtins.i32   => q"$writer.write_i32($value)"
          case TypeId.Builtins.i64   => q"$writer.write_i64($value)"
          case TypeId.Builtins.u08   => q"$writer.write_ubyte($value)"
          case TypeId.Builtins.u16   => q"$writer.write_u16($value)"
          case TypeId.Builtins.u32   => q"$writer.write_u32($value)"
          case TypeId.Builtins.u64   => q"$writer.write_u64($value)"
          case TypeId.Builtins.f32   => q"$writer.write_f32($value)"
          case TypeId.Builtins.f64   => q"$writer.write_f64($value)"
          case TypeId.Builtins.f128  => q"$writer.write_f128($value)"
          case TypeId.Builtins.str   => q"$writer.write_str($value)"
          case TypeId.Builtins.uid   => q"$writer.write_uuid($value)"
          case TypeId.Builtins.tsu   => q"$writer.write_datetime($value)"
          case TypeId.Builtins.tso   => q"$writer.write_datetime($value)"
          case TypeId.Builtins.bytes => q"$writer.write_bytes($value)"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
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
        case _                                              => false
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

    /** Emits the cross-domain Muxer-entry wrapper classes for a service.
      *
      * Each wrapper structurally satisfies the runtime `IBaboonJsonService` /
      * `IBaboonUebaService` Protocol (PEP 544) — no explicit inheritance is
      * needed, just the right `service_name` attribute and `invoke` method.
      * Extra dependencies the underlying `invoke_json_X` / `invoke_ueba_X`
      * functions consume (`rt` in errors mode, plus any service-context
      * parameter) are baked at construction time so the runtime
      * `IBaboon*Service.invoke(method, data, ctx)` contract stays uniform.
      * Python has no `--py-async-services` flag, so the wrappers' `invoke`
      * return type is plain `str` / `bytes` (noErrors mode) or unannotated
      * (errors mode, where the underlying invoker already returns the
      * configured result container).
      */
    private def generateServiceWrappers(service: Typedef.Service, svcType: PyType): TextTree[PyValue] = {
      val jsonWrapper =
        if (hasActiveJsonCodecs(service)) Some(generateOneWrapper(service, svcType, isJson = true))
        else None
      val uebaWrapper =
        if (hasActiveUebaCodecs(service)) Some(generateOneWrapper(service, svcType, isJson = false))
        else None
      Seq(jsonWrapper, uebaWrapper).flatten.joinNN()
    }

    private def generateOneWrapper(service: Typedef.Service, svcType: PyType, isJson: Boolean): TextTree[PyValue] = {
      val svcName     = service.id.name.name
      val wrapperName = s"${svcName}${if (isJson) "_JsonService" else "_UebaService"}"
      val invokerFn   = s"${if (isJson) "invoke_json_" else "invoke_ueba_"}$svcName"
      val wireType    = if (isJson) "str" else "bytes"
      val retAnnot: TextTree[PyValue] = if (resolved.noErrors) q"$wireType" else q""

      // Extra constructor parameters: `rt` (errors mode only) and any
      // service-context. Mirror the order used by the underlying invoker:
      // (impl, [rt,] [svc_ctx,]).
      val rtParam: Option[(String, TextTree[PyValue])] =
        if (resolved.noErrors) None else Some(("rt", q"$ibaboonServiceRtType"))
      val svcCtxParam: Option[(String, TextTree[PyValue], String)] = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => None
        case ResolvedServiceContext.AbstractContext(tn, pn) => Some((pn, q"$tn", pn))
        case ResolvedServiceContext.ConcreteContext(tn, pn) => Some((pn, q"$tn", pn))
      }

      val ctorParams: List[TextTree[PyValue]] = {
        val base = List[TextTree[PyValue]](q"impl: $svcType")
        val withRt = rtParam match {
          case Some((n, tpe)) => base :+ q"$n: $tpe"
          case None           => base
        }
        svcCtxParam match {
          case Some((n, tpe, _)) => withRt :+ q"$n: $tpe"
          case None              => withRt
        }
      }
      val ctorAssigns: List[TextTree[PyValue]] = {
        val base = List[TextTree[PyValue]](q"self.impl = impl")
        val withRt = rtParam match {
          case Some((n, _)) => base :+ q"self.$n = $n"
          case None         => base
        }
        svcCtxParam match {
          case Some((n, _, _)) => withRt :+ q"self.$n = $n"
          case None            => withRt
        }
      }

      // Forwarded args to the underlying invoker:
      // (method, data, self.impl, [self.rt,] [self.svc_ctx,] ctx)
      val invokerArgs: List[TextTree[PyValue]] = {
        val base = List[TextTree[PyValue]](q"method", q"data", q"self.impl")
        val withRt = rtParam match {
          case Some((n, _)) => base :+ q"self.$n"
          case None         => base
        }
        val withSvcCtx = svcCtxParam match {
          case Some((n, _, _)) => withRt :+ q"self.$n"
          case None            => withRt
        }
        withSvcCtx :+ q"ctx"
      }

      // The wrapper carries no explicit Protocol inheritance — structural
      // typing via the `IBaboonJsonService` / `IBaboonUebaService` Protocols
      // suffices. We still reference the Protocol in a phantom annotation to
      // trigger the import (purely for code-navigability; type-checkers honour
      // the structural match regardless).
      val ifaceRef: PyType = if (isJson) ibaboonJsonService else ibaboonUebaService

      val invokeReturnArrow: TextTree[PyValue] =
        if (resolved.noErrors) q" -> $retAnnot" else q""

      q"""class $wrapperName:
         |    \"\"\"Adapter from $svcType to ${if (isJson) "IBaboonJsonService" else "IBaboonUebaService"} for use with the cross-domain
         |    ${if (isJson) "JsonMuxer" else "UebaMuxer"}. Structurally satisfies $ifaceRef.
         |    \"\"\"
         |    service_name: str = "$svcName"
         |
         |    def __init__(self, ${ctorParams.join(", ")}) -> None:
         |        ${ctorAssigns.join("\n").shift(8).trim}
         |
         |    def invoke(self, method: $baboonMethodId, data: $wireType, ctx: $baboonCodecContext)$invokeReturnArrow:
         |        return $invokerFn(${invokerArgs.join(", ")})
         |""".stripMargin
    }

    // ========== noErrors mode ==========

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[PyValue] = {
      val svcName = service.id.name.name
      val svcType = typeTranslator.asPyType(service.id, domain, evolution, fileTools.definitionsBasePkg)

      val jsonFn =
        if (hasActiveJsonCodecs(service))
          Some(generateNoErrorsJsonFn(service, svcName, svcType))
        else None

      val uebaFn =
        if (hasActiveUebaCodecs(service))
          Some(generateNoErrorsUebaFn(service, svcName, svcType))
        else None

      val wrappers = generateServiceWrappers(service, svcType)

      (Seq(jsonFn, uebaFn).flatten :+ wrappers).joinNN()
    }

    private def generateNoErrorsJsonFn(service: Typedef.Service, svcName: String, svcType: PyType): TextTree[PyValue] = {
      val cases = service.methods.map {
        m =>
          val encodeAndReturn = m.out match {
            case Some(outRef) =>
              val encExpr = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"result")
              q"""result = impl.${m.name.name}(${ctxArgPass}decoded)
                 |try:
                 |    return $encExpr
                 |except Exception as e:
                 |    raise $baboonWiringException($baboonEncoderFailed(method, e))""".stripMargin
            case None =>
              q"""impl.${m.name.name}(${ctxArgPass}decoded)
                 |return "null"
                 |""".stripMargin
          }

          val decExpr = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"data")
          q"""if method.method_name == "${m.name.name}":
             |    try:
             |        decoded = $decExpr
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
      val cases = service.methods.map {
        m =>
          val encodeAndReturn = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"writer", q"result")
              q"""result = impl.${m.name.name}(${ctxArgPass}decoded)
                 |try:
                 |    output_stream = $pyBytesIO()
                 |    writer = $baboonLEDataOutputStream(output_stream)
                 |    $encStmt
                 |    return output_stream.getvalue()
                 |except Exception as e:
                 |    raise $baboonWiringException($baboonEncoderFailed(method, e))""".stripMargin
            case None =>
              q"""impl.${m.name.name}(${ctxArgPass}decoded)
                 |return bytes()
                 |""".stripMargin
          }

          val decExpr = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"reader")
          q"""if method.method_name == "${m.name.name}":
             |    try:
             |        reader = $baboonLEDataInputStream($pyBytesIO(data))
             |        decoded = $decExpr
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

      val jsonFn =
        if (hasActiveJsonCodecs(service))
          Some(generateErrorsJsonFn(service, svcName, svcType))
        else None

      val uebaFn =
        if (hasActiveUebaCodecs(service))
          Some(generateErrorsUebaFn(service, svcName, svcType))
        else None

      val wrappers = generateServiceWrappers(service, svcType)

      (Seq(jsonFn, uebaFn).flatten :+ wrappers).joinNN()
    }

    private def generateErrorsJsonFn(service: Typedef.Service, svcName: String, svcType: PyType): TextTree[PyValue] = {
      val cases = service.methods.map {
        m =>
          val hasErrType = m.err.isDefined && !resolved.noErrors
          val decExpr    = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"data")

          val decodeStep =
            q"""try:
               |    decoded = $decExpr
               |    input_val = rt.pure(decoded)
               |except Exception as e:
               |    input_val = rt.fail($baboonDecoderFailed(method, e))""".stripMargin

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val encExpr = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"v")

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
                 |        return rt.pure($encExpr)
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
      val cases = service.methods.map {
        m =>
          val hasErrType = m.err.isDefined && !resolved.noErrors
          val decExpr    = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"reader")

          val decodeStep =
            q"""try:
               |    reader = $baboonLEDataInputStream($pyBytesIO(data))
               |    decoded = $decExpr
               |    input_val = rt.pure(decoded)
               |except Exception as e:
               |    input_val = rt.fail($baboonDecoderFailed(method, e))""".stripMargin

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"writer", q"v")

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
                 |        $encStmt
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
