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

  def translateClient(defn: DomainMember.User): Option[TextTree[PyValue]]
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

    private val isAsync: Boolean = target.language.asyncServices

    private val asyncPrefix: String = if (isAsync) "async " else ""
    private val awaitPrefix: String = if (isAsync) "await " else ""

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

    /** Emits a `${Service}Client` class holding user-supplied transport
      * callbacks plus a `BaboonCodecContext`, with one method per service
      * endpoint that encodes the input, calls the transport `(service, method,
      * data) -> data`, and decodes the output.
      *
      * Two methods are emitted per endpoint when both codecs are active: the
      * UEBA path keeps the bare endpoint name and round-trips `bytes` via
      * `transport_ueba`; the JSON path is `${method}_json` and round-trips
      * `str` via `transport_json`. Routing is by `(serviceName, methodName)`
      * string literals, matching the server-side wiring/muxer.
      *
      * The codec context is held on the client (`self.ctx`) and rebound to a
      * local `ctx` inside each method so the shared codec-resolution helpers
      * (`jsonEncodeExpr`, `uebaDecodeExpr`, …) resolve. When
      * `--py-async-services=true` the client methods become `async def` and
      * `await` the transport callbacks (typed `Callable[..., Awaitable[...]]`),
      * mirroring the async server wiring; otherwise they are synchronous.
      */
    override def translateClient(defn: DomainMember.User): Option[TextTree[PyValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val svcName = service.id.name.name
          val hasJson = hasActiveJsonCodecs(service)
          val hasUeba = hasActiveUebaCodecs(service)
          if (!hasJson && !hasUeba) return None

          val clientMethods = service.methods.flatMap {
            m =>
              val inType  = typeTranslator.asPyRef(m.sig, domain, evolution, fileTools.definitionsBasePkg)
              val outType = m.out.map(o => typeTranslator.asPyRef(o, domain, evolution, fileTools.definitionsBasePkg))
              val retAnnot: TextTree[PyValue] = outType.getOrElse(q"None")

              val uebaMethod = if (hasUeba) {
                val encStmt = uebaEncodeStmt(m.sig.id.asInstanceOf[TypeId.Scalar], q"writer", q"arg")
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val decExpr = uebaDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"reader")
                    q"""reader = $baboonLEDataInputStream($pyBytesIO(resp))
                       |return $decExpr""".stripMargin
                  case None => q"return None"
                }
                Some(
                  q"""${asyncPrefix}def ${m.name.name}(self, ${ctxParamDecl}arg: $inType) -> $retAnnot:
                     |    ctx = self.ctx
                     |    output_stream = $pyBytesIO()
                     |    writer = $baboonLEDataOutputStream(output_stream)
                     |    $encStmt
                     |    resp = ${awaitPrefix}self.transport_ueba("$svcName", "${m.name.name}", output_stream.getvalue())
                     |    ${decodeOut.shift(4).trim}
                     |""".stripMargin
                )
              } else None

              val jsonMethod = if (hasJson) {
                val encExpr = jsonEncodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"arg")
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val decExpr = jsonDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"resp")
                    q"return $decExpr"
                  case None => q"return None"
                }
                Some(
                  q"""${asyncPrefix}def ${m.name.name}_json(self, ${ctxParamDecl}arg: $inType) -> $retAnnot:
                     |    ctx = self.ctx
                     |    encoded = $encExpr
                     |    resp = ${awaitPrefix}self.transport_json("$svcName", "${m.name.name}", encoded)
                     |    ${decodeOut.shift(4).trim}
                     |""".stripMargin
                )
              } else None

              uebaMethod.toList ++ jsonMethod.toList
          }

          if (clientMethods.isEmpty) return None

          val uebaRet: TextTree[PyValue] = if (isAsync) q"$pyAwaitable[bytes]" else q"bytes"
          val jsonRet: TextTree[PyValue] = if (isAsync) q"$pyAwaitable[str]" else q"str"

          val ctorParams: List[TextTree[PyValue]] = {
            val ueba = if (hasUeba) List[TextTree[PyValue]](q"transport_ueba: $pyCallable[[str, str, bytes], $uebaRet]") else Nil
            val json = if (hasJson) List[TextTree[PyValue]](q"transport_json: $pyCallable[[str, str, str], $jsonRet]") else Nil
            ueba ++ json :+ q"ctx: $baboonCodecContext"
          }

          val ctorAssigns: List[TextTree[PyValue]] = {
            val ueba = if (hasUeba) List[TextTree[PyValue]](q"self.transport_ueba = transport_ueba") else Nil
            val json = if (hasJson) List[TextTree[PyValue]](q"self.transport_json = transport_json") else Nil
            ueba ++ json :+ q"self.ctx = ctx"
          }

          val clientTree =
            q"""class ${svcName}Client:
               |    \"\"\"RPC client for $svcName. Holds user-supplied transport callbacks
               |    `(service, method, data) -> data` and a $baboonCodecContext, and exposes one
               |    method per endpoint (UEBA: bare name / `bytes`; JSON: `_json` suffix / `str`).
               |    \"\"\"
               |    def __init__(self, ${ctorParams.join(", ")}) -> None:
               |        ${ctorAssigns.join("\n").shift(8).trim}
               |
               |    ${clientMethods.joinNN().shift(4).trim}
               |""".stripMargin

          Some(clientTree)
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
      * Under `--py-async-services=true` the wrapper's `invoke` is `async def`
      * and `await`s the underlying invoker; the return annotation stays plain
      * `str` / `bytes` (noErrors mode — an `async def -> str` resolves to `str`)
      * or unannotated (errors mode, where the underlying invoker already
      * returns the configured result container).
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
         |    ${asyncPrefix}def invoke(self, method: $baboonMethodId, data: $wireType, ctx: $baboonCodecContext)$invokeReturnArrow:
         |        return ${awaitPrefix}$invokerFn(${invokerArgs.join(", ")})
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
              q"""result = ${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded)
                 |try:
                 |    return $encExpr
                 |except Exception as e:
                 |    raise $baboonWiringException($baboonEncoderFailed(method, e))""".stripMargin
            case None =>
              q"""${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded)
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

      q"""${asyncPrefix}def invoke_json_$svcName(method: $baboonMethodId, data: str, impl: $svcType, ${ctxParamDecl}ctx: $baboonCodecContext) -> str:
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
              q"""result = ${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded)
                 |try:
                 |    output_stream = $pyBytesIO()
                 |    writer = $baboonLEDataOutputStream(output_stream)
                 |    $encStmt
                 |    return output_stream.getvalue()
                 |except Exception as e:
                 |    raise $baboonWiringException($baboonEncoderFailed(method, e))""".stripMargin
            case None =>
              q"""${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded)
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

      q"""${asyncPrefix}def invoke_ueba_$svcName(method: $baboonMethodId, data: bytes, impl: $svcType, ${ctxParamDecl}ctx: $baboonCodecContext) -> bytes:
         |    ${cases.shift(4).trim}
         |    raise $baboonWiringException($baboonNoMatchingMethod(method))
         |""".stripMargin
    }

    // ========== errors mode ==========

    /** Async errors-mode call+encode body.
      *
      * The synchronous errors path threads the result container through
      * `rt.flat_map(input_val, _call)` closures. That structure cannot host an
      * `await impl(...)` — a synchronous `flat_map` callback may not be a
      * coroutine. So, mirroring the TypeScript async errors path
      * (`TsServiceWiringTranslator.generateErrorsMethodBody`, `isAsync`
      * branch), we emit a linear body that short-circuits on the concrete
      * `BaboonLeft` and `await`s the impl directly. This couples the async
      * errors path to the builtin-either container (`BaboonLeft`/`BaboonRight`),
      * exactly as the TypeScript reference couples to its `{tag:'Left'}` shape.
      *
      * `encode` yields the success-encode block for the `m.out=Some` case (it
      * must `return rt.pure(...)` / `rt.fail(EncoderFailed)`); when `m.out` is
      * `None`, `voidReturn` (e.g. `rt.pure("null")`) is returned after the call.
      */
    private def asyncCallAndEncode(
      m: Typedef.MethodDef,
      hasErrType: Boolean,
      voidReturn: TextTree[PyValue],
      encode: TextTree[PyValue] => Option[TextTree[PyValue]],
    ): TextTree[PyValue] = {
      val shortCircuitInput =
        q"""if isinstance(input_val, $baboonLeftType):
           |    return input_val
           |decoded_v = input_val.value""".stripMargin

      val callStep = if (hasErrType) {
        q"""try:
           |    call_result = ${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded_v)
           |    output = rt.left_map(call_result, lambda err: $baboonCallFailed(method, err))
           |except Exception as e:
           |    return rt.fail($baboonCallFailed(method, e))
           |if isinstance(output, $baboonLeftType):
           |    return output
           |output_v = output.value""".stripMargin
      } else {
        q"""try:
           |    output_v = ${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded_v)
           |except Exception as e:
           |    return rt.fail($baboonCallFailed(method, e))""".stripMargin
      }

      val encodeStep = encode(q"output_v") match {
        case Some(block) => block
        case None        => q"return $voidReturn"
      }

      q"""$shortCircuitInput
         |$callStep
         |$encodeStep""".stripMargin
    }

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

          val callAndEncodeStep =
            if (isAsync)
              asyncCallAndEncode(
                m,
                hasErrType,
                voidReturn = q"rt.pure(\"null\")",
                encode = (v: TextTree[PyValue]) =>
                  m.out.map {
                    o =>
                      val encExpr = jsonEncodeExpr(o.id.asInstanceOf[TypeId.Scalar], v)
                      q"""try:
                         |    return rt.pure($encExpr)
                         |except Exception as e:
                         |    return rt.fail($baboonEncoderFailed(method, e))""".stripMargin
                  },
              )
            else
              m.out match {
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

      q"""${asyncPrefix}def invoke_json_$svcName(method: $baboonMethodId, data: str, impl: $svcType, rt: $ibaboonServiceRtType, ${ctxParamDecl}ctx: $baboonCodecContext):
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

          val callAndEncodeStep =
            if (isAsync)
              asyncCallAndEncode(
                m,
                hasErrType,
                voidReturn = q"rt.pure(bytes())",
                encode = (v: TextTree[PyValue]) =>
                  m.out.map {
                    o =>
                      val encStmt = uebaEncodeStmt(o.id.asInstanceOf[TypeId.Scalar], q"writer", v)
                      q"""try:
                         |    output_stream = $pyBytesIO()
                         |    writer = $baboonLEDataOutputStream(output_stream)
                         |    $encStmt
                         |    return rt.pure(output_stream.getvalue())
                         |except Exception as e:
                         |    return rt.fail($baboonEncoderFailed(method, e))""".stripMargin
                  },
              )
            else
              m.out match {
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

      q"""${asyncPrefix}def invoke_ueba_$svcName(method: $baboonMethodId, data: bytes, impl: $svcType, rt: $ibaboonServiceRtType, ${ctxParamDecl}ctx: $baboonCodecContext):
         |    ${cases.shift(4).trim}
         |    return rt.fail($baboonNoMatchingMethod(method))
         |""".stripMargin
    }
  }
}
