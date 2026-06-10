package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSTypeOrigin
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[CSValue]]

  def translateServiceRt(domain: Domain): Option[TextTree[CSValue]]

  def translateClient(defn: DomainMember.User): Option[TextTree[CSValue]]
}

object CSServiceWiringTranslator {
  class Impl(
    target: CSTarget,
    trans: CSTypeTranslator,
    codecs: Set[CSCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
  ) extends CSServiceWiringTranslator {

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "cs", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "cs", target.language.serviceContext, target.language.pragmas)

    // When the C# `asyncServices` flag is on, the service interface methods
    // return `Task<T>` (mirrored in CSDefnTranslator), so the impl call must be
    // awaited; the Invoke dispatchers and the muxer-wrapper `Invoke` become
    // `async Task<...>`, and the client transport callbacks/methods go async
    // too. When off (the default) the sync path is emitted byte-for-byte
    // unchanged. Same axis the Rust/TypeScript/Java backends already gate.
    private val isAsync: Boolean = target.language.asyncServices
    private val asyncKw: String  = if (isAsync) "async " else ""
    private val awaitKw: String  = if (isAsync) "await " else ""

    // Wrap a wire-shape return type in `Task<...>` under async; identity in
    // sync mode so the default path renders byte-for-byte unchanged.
    private def taskWrap(inner: TextTree[CSValue]): TextTree[CSValue] =
      if (isAsync) q"System.Threading.Tasks.Task<$inner>" else inner

    private def hasActiveJsonCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[CSJsonCodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def hasActiveUebaCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[CSUEBACodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def jsonCodecName(typeId: TypeId.User): CSValue.CSType = {
      val srcRef = trans.asCsTypeKeepForeigns(typeId, domain, evo)
      CSValue.CSType(srcRef.pkg, s"${srcRef.name}_JsonCodec", srcRef.fq, CSTypeOrigin(typeId, domain).asDerived)
    }

    private def uebaCodecName(typeId: TypeId.User): CSValue.CSType = {
      val srcRef = trans.asCsTypeKeepForeigns(typeId, domain, evo)
      CSValue.CSType(srcRef.pkg, s"${srcRef.name}_UEBACodec", srcRef.fq, CSTypeOrigin(typeId, domain).asDerived)
    }

    // JSON encode/decode for both User types (via generated codec) and BuiltinScalar (inline).
    private def jsonDecodeExpr(id: TypeId, wire: TextTree[CSValue]): TextTree[CSValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.Instance.Decode($codecCtxName, $wire)"
      case b: TypeId.BuiltinScalar =>
        val fref = q"$wire!"
        b match {
          case TypeId.Builtins.bit                       => q"$fref.Value<$csBoolean>()!"
          case TypeId.Builtins.i08                       => q"$fref.Value<$csSByte>()!"
          case TypeId.Builtins.i16                       => q"$fref.Value<$csInt16>()!"
          case TypeId.Builtins.i32                       => q"$fref.Value<$csInt32>()!"
          case TypeId.Builtins.i64                       => q"$fref.Value<$csInt64>()!"
          case TypeId.Builtins.u08                       => q"$fref.Value<$csByte>()!"
          case TypeId.Builtins.u16                       => q"$fref.Value<$csUInt16>()!"
          case TypeId.Builtins.u32                       => q"$fref.Value<$csUInt32>()!"
          case TypeId.Builtins.u64                       => q"$fref.Value<$csUInt64>()!"
          case TypeId.Builtins.f32                       => q"$fref.Value<$csSingle>()!"
          case TypeId.Builtins.f64                       => q"$fref.Value<$csDouble>()!"
          case TypeId.Builtins.f128                      => q"$BaboonTools.ReadDecimalLenient($fref)"
          case TypeId.Builtins.str                       => q"$fref.Value<$csString>()!"
          case TypeId.Builtins.bytes                     => q"$csByteString.Parse($fref.Value<$csString>()!)"
          case TypeId.Builtins.uid                       => q"$csGuid.Parse($fref.Value<$csString>()!)"
          case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.FromString($fref.Value<$csString>()!)"
          case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def jsonEncodeExpr(id: TypeId, value: TextTree[CSValue]): TextTree[CSValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.Instance.Encode($codecCtxName, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bytes   => q"new $nsJValue($value.Encode())"
          case TypeId.Builtins.uid     => q"new $nsJValue($value.ToString())"
          case TypeId.Builtins.tsu     => q"new $nsJValue($baboonTimeFormats.TsuToString($value))"
          case TypeId.Builtins.tso     => q"new $nsJValue($baboonTimeFormats.TsoToString($value))"
          case TypeId.Builtins.bit     => q"new $nsJValue($value.ToString().ToLowerInvariant())"
          case _: TypeId.BuiltinScalar => q"new $nsJValue($value)"
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaDecodeExpr(id: TypeId, br: TextTree[CSValue]): TextTree[CSValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.Instance.Decode($codecCtxName, $br)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                       => q"$br.ReadBoolean()"
          case TypeId.Builtins.i08                       => q"$br.ReadSByte()"
          case TypeId.Builtins.i16                       => q"$br.ReadInt16()"
          case TypeId.Builtins.i32                       => q"$br.ReadInt32()"
          case TypeId.Builtins.i64                       => q"$br.ReadInt64()"
          case TypeId.Builtins.u08                       => q"$br.ReadByte()"
          case TypeId.Builtins.u16                       => q"$br.ReadUInt16()"
          case TypeId.Builtins.u32                       => q"$br.ReadUInt32()"
          case TypeId.Builtins.u64                       => q"$br.ReadUInt64()"
          case TypeId.Builtins.f32                       => q"$br.ReadSingle()"
          case TypeId.Builtins.f64                       => q"$br.ReadDouble()"
          case TypeId.Builtins.f128                      => q"$br.ReadDecimal()"
          case TypeId.Builtins.str                       => q"$br.ReadString()"
          case TypeId.Builtins.bytes                     => q"$csByteString.ReadBytes($br)"
          case TypeId.Builtins.uid                       => q"new $csGuid($br.ReadBytes(16))"
          case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.DecodeFromBin($br)"
          case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaEncodeStmt(id: TypeId, bw: TextTree[CSValue], value: TextTree[CSValue]): TextTree[CSValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.Instance.Encode($codecCtxName, $bw, $value);"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bytes                     => q"$csByteString.WriteBytes($value, $bw);"
          case TypeId.Builtins.uid                       => q"$bw.Write($value.ToByteArray());"
          case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.EncodeToBin($value, $bw);"
          case _: TypeId.BuiltinScalar                   => q"$bw.Write($value);"
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def renderContainer(error: String, success: String): String = {
      val p = resolved.pattern.get.replace("$error", error).replace("$success", success)
      s"${resolved.resultType.get}$p"
    }

    override def translateServiceRt(domain: Domain): Option[TextTree[CSValue]] = {
      if (resolved.noErrors) return None

      val hasServices = domain.defs.meta.nodes.values.exists {
        case DomainMember.User(_, _: Typedef.Service, _, _) => true
        case _                                              => false
      }
      if (!hasServices) return None

      val ct = renderContainer _

      val rtInterface: TextTree[CSValue] =
        q"""public interface IBaboonServiceRt
           |{
           |    ${ct("L", "R")} Pure<L, R>(R value);
           |    ${ct("L", "R")} Fail<L, R>(L error);
           |    ${ct("C", "B")} LeftMap<A, B, C>(${ct("A", "B")} value, System.Func<A, C> f);
           |    ${ct("A", "C")} FlatMap<A, B, C>(${ct("A", "B")} value, System.Func<B, ${ct("A", "C")}> f);
           |}""".stripMargin

      val isBuiltinEither = resolved.resultType.contains("Baboon.Runtime.Shared.Either") ||
        resolved.resultType.contains("Either")

      val defaultImpl: Option[TextTree[CSValue]] = if (isBuiltinEither) {
        Some(
          q"""public class BaboonServiceRtDefault : IBaboonServiceRt
             |{
             |    public static readonly BaboonServiceRtDefault Instance = new BaboonServiceRtDefault();
             |
             |    public ${ct("L", "R")} Pure<L, R>(R value)
             |    {
             |        return new $either<L, R>.Right(value);
             |    }
             |
             |    public ${ct("L", "R")} Fail<L, R>(L error)
             |    {
             |        return new $either<L, R>.Left(error);
             |    }
             |
             |    public ${ct("C", "B")} LeftMap<A, B, C>($either<A, B> value, System.Func<A, C> f)
             |    {
             |        if (value is $either<A, B>.Left left)
             |        {
             |            return new $either<C, B>.Left(f(left.Value));
             |        }
             |        if (value is $either<A, B>.Right right)
             |        {
             |            return new $either<C, B>.Right(right.Value);
             |        }
             |        throw new System.InvalidOperationException("Unexpected Either variant");
             |    }
             |
             |    public ${ct("A", "C")} FlatMap<A, B, C>($either<A, B> value, System.Func<B, $either<A, C>> f)
             |    {
             |        if (value is $either<A, B>.Left left)
             |        {
             |            return new $either<A, C>.Left(left.Value);
             |        }
             |        if (value is $either<A, B>.Right right)
             |        {
             |            return f(right.Value);
             |        }
             |        throw new System.InvalidOperationException("Unexpected Either variant");
             |    }
             |}""".stripMargin
        )
      } else None

      Some(Seq[Option[TextTree[CSValue]]](Some(rtInterface), defaultImpl).flatten.join("\n\n"))
    }

    override def translate(defn: DomainMember.User): Option[TextTree[CSValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val methods =
            if (resolved.noErrors) generateNoErrorsWiring(service)
            else generateErrorsWiring(service)
          Some(methods)
        case _ => None
      }
    }

    /** Emits a `${Service}Client` class that turns typed endpoint calls into
      * encode -> transport -> decode round-trips. The client is transport-
      * agnostic: it holds user-supplied callbacks `(service, method, data) ->
      * result` (JSON: `string -> string`, UEBA: `byte[] -> byte[]`) and routes
      * each call by `(serviceName, methodName)`.
      *
      * One method per endpoint is emitted per active codec: the UEBA variant
      * keeps the bare method name, the JSON variant gets a `Json` suffix (the
      * same naming convention the Rust/TypeScript clients use). The service
      * context, when configured, is threaded as a leading parameter exactly as
      * in the server wiring; `BaboonCodecContext` is threaded as the trailing
      * `ctx` parameter (defaulting to `BaboonCodecContext.Default`).
      *
      * Under the `asyncServices` flag the client methods become `async` and
      * return `Task<T>` (or `Task` for void), the transport callbacks become
      * `Task`-returning delegates (`Func<…, Task<byte[]>>` /
      * `Func<…, Task<string>>`), and each method `await`s its transport call.
      * With the flag off (default) the client is emitted sync-only (methods
      * return `T` directly, not `Task<T>`) — the existing path, unchanged.
      */
    override def translateClient(defn: DomainMember.User): Option[TextTree[CSValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val svcName = service.id.name.name
          val hasJson = hasActiveJsonCodecs(service)
          val hasUeba = hasActiveUebaCodecs(service)
          if (!hasJson && !hasUeba) return None

          // Client method return type: `T`/`void` in sync mode, `Task<T>`/`Task`
          // under async. The transport callback becomes a `Task`-returning
          // delegate so the client `await`s it.
          val awaitTransport = if (isAsync) "await " else ""
          def clientRet(out: Option[TypeRef]): TextTree[CSValue] = out match {
            case Some(o) => taskWrap(trans.asCsRef(o, domain, evo))
            case None    => if (isAsync) q"System.Threading.Tasks.Task" else q"void"
          }

          val clientMethods = service.methods.flatMap {
            m =>
              val inRef   = trans.asCsRef(m.sig, domain, evo)
              val retType = clientRet(m.out)

              val uebaMethod = if (hasUeba) {
                val encodeIn = uebaEncodeStmt(m.sig.id.asInstanceOf[TypeId.Scalar], q"bw", q"arg")
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val decExpr = uebaDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"br")
                    q"""var ims = new $memoryStream(resp);
                       |var br = new $binaryReader(ims);
                       |return $decExpr;""".stripMargin
                  case None => q"return;"
                }
                Some(
                  q"""public $asyncKw$retType ${escapeCsKeyword(m.name.name.capitalize)}($ctxParamDecl$inRef arg, $baboonCodecContext $codecCtxName)
                     |{
                     |    var oms = new $memoryStream();
                     |    var bw = new $binaryWriter(oms);
                     |    $encodeIn
                     |    bw.Flush();
                     |    var resp = ${awaitTransport}_transportUeba(${ctxArgPass}"$svcName", "${m.name.name}", oms.ToArray());
                     |    ${decodeOut.shift(4).trim}
                     |}""".stripMargin
                )
              } else None

              val jsonMethod = if (hasJson) {
                val encodeIn = jsonEncodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"arg")
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val decExpr = jsonDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"$nsJToken.Parse(resp)")
                    q"return $decExpr;"
                  case None => q"return;"
                }
                Some(
                  q"""public $asyncKw$retType ${escapeCsKeyword(m.name.name.capitalize)}Json($ctxParamDecl$inRef arg, $baboonCodecContext $codecCtxName)
                     |{
                     |    var encoded = $encodeIn;
                     |    var resp = ${awaitTransport}_transportJson(${ctxArgPass}"$svcName", "${m.name.name}", encoded.ToString($nsFormatting.None));
                     |    ${decodeOut.shift(4).trim}
                     |}""".stripMargin
                )
              } else None

              uebaMethod.toList ++ jsonMethod.toList
          }

          if (clientMethods.isEmpty) return None

          val uebaTransportType =
            if (isAsync) q"System.Func<$ctxFuncTypePrefix$csString, $csString, byte[], System.Threading.Tasks.Task<byte[]>>"
            else q"System.Func<$ctxFuncTypePrefix$csString, $csString, byte[], byte[]>"
          val jsonTransportType =
            if (isAsync) q"System.Func<$ctxFuncTypePrefix$csString, $csString, $csString, System.Threading.Tasks.Task<$csString>>"
            else q"System.Func<$ctxFuncTypePrefix$csString, $csString, $csString, $csString>"

          val fieldDecls = List(
            if (hasUeba) Some(q"private readonly $uebaTransportType _transportUeba;") else None,
            if (hasJson) Some(q"private readonly $jsonTransportType _transportJson;") else None,
          ).flatten

          val ctorParams = List(
            if (hasUeba) Some(q"$uebaTransportType transportUeba") else None,
            if (hasJson) Some(q"$jsonTransportType transportJson") else None,
          ).flatten

          val ctorAssigns = List(
            if (hasUeba) Some(q"_transportUeba = transportUeba;") else None,
            if (hasJson) Some(q"_transportJson = transportJson;") else None,
          ).flatten

          val clientName = s"${svcName}Client"

          val clientTree =
            q"""public sealed class $clientName$genericParam
               |{
               |    ${fieldDecls.join("\n").shift(4).trim}
               |
               |    public $clientName(${ctorParams.join(", ")})
               |    {
               |        ${ctorAssigns.join("\n").shift(8).trim}
               |    }
               |
               |    ${clientMethods.join("\n\n").shift(4).trim}
               |}""".stripMargin

          Some(clientTree)
        case _ => None
      }
    }

    private def ctxParamDecl: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext               => ""
      case ResolvedServiceContext.AbstractContext(tn, pn) => s"$tn $pn, "
      case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$tn $pn, "
    }

    private def ctxArgPass: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext              => ""
      case ResolvedServiceContext.AbstractContext(_, pn) => s"$pn, "
      case ResolvedServiceContext.ConcreteContext(_, pn) => s"$pn, "
    }

    private def genericParam: String = resolvedCtx match {
      case ResolvedServiceContext.AbstractContext(tn, _) => s"<$tn>"
      case _                                             => ""
    }

    // The service-context type name (`abstract` introduces a generic param,
    // `type` pins a concrete type) and its parameter name. The codec context
    // is renamed away from the service-context parameter name (default `ctx`)
    // to avoid the duplicate-parameter collision; in `none` mode it stays
    // `ctx`, keeping that output byte-identical.
    private def svcCtxTypeName: Option[String] = resolvedCtx match {
      case ResolvedServiceContext.NoContext               => None
      case ResolvedServiceContext.AbstractContext(tn, _)  => Some(tn)
      case ResolvedServiceContext.ConcreteContext(tn, _)  => Some(tn)
    }
    private def svcCtxArgName: Option[String] = resolvedCtx match {
      case ResolvedServiceContext.NoContext               => None
      case ResolvedServiceContext.AbstractContext(_, pn)  => Some(pn)
      case ResolvedServiceContext.ConcreteContext(_, pn)  => Some(pn)
    }
    private def codecCtxName: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext => "ctx"
      case _                                => if (svcCtxArgName.contains("codecCtx")) "baboonCodecCtx" else "codecCtx"
    }
    // Type-args prefix for the transport delegates: the service context is
    // forwarded to the transport callback (`Func<Ctx, service, method, data,
    // …>`) so a caller can attach it to the outgoing request.
    private def ctxFuncTypePrefix: String = svcCtxTypeName.fold("")(t => s"$t, ")

    /** Emits the cross-domain Muxer-entry wrapper classes for a service.
      *
      * Each wrapper implements `IBaboonJsonService<R>` / `IBaboonUebaService<R>`
      * with R matching the underlying `${svcName}Wiring.InvokeJson` /
      * `InvokeUeba` return type (raw `string`/`byte[]` for noErrors=true,
      * container-wrapped for noErrors=false). Extra dependencies (`rt` for
      * the errors mode, and any service-context parameter) are baked at
      * construction time so the runtime `IBaboon*Service.Invoke(method,
      * data, ctx)` contract stays codec-flavour-symmetric and
      * language-uniform.
      *
      * Under the `asyncServices` flag R is `Task<string>` / `Task<byte[]>`
      * (or the Task-wrapped service-result container in errors mode); the
      * wrapper's `Invoke` forwards the already-`Task`-typed wiring result
      * straight through. The runtime `JsonMuxer<R>` / `UebaMuxer<R>` stay
      * parametric in R so both axes plug in without changing the contract.
      */
    private def generateServiceWrappers(
      service: Typedef.Service,
      jsonRetType: TextTree[CSValue],
      uebaRetType: TextTree[CSValue],
    ): Option[TextTree[CSValue]] = {
      val jsonWrapper =
        if (hasActiveJsonCodecs(service)) Some(generateOneWrapper(service, isJson = true, jsonRetType))
        else None
      val uebaWrapper =
        if (hasActiveUebaCodecs(service)) Some(generateOneWrapper(service, isJson = false, uebaRetType))
        else None
      val both = Seq(jsonWrapper, uebaWrapper).flatten
      if (both.isEmpty) None else Some(both.join("\n\n"))
    }

    private def generateOneWrapper(
      service: Typedef.Service,
      isJson: Boolean,
      retType: TextTree[CSValue],
    ): TextTree[CSValue] = {
      val svcName     = service.id.name.name
      val wireType    = if (isJson) q"$csString" else q"byte[]"
      val invokerName = if (isJson) "InvokeJson" else "InvokeUeba"
      val wrapperName = s"${svcName}${if (isJson) "JsonService" else "UebaService"}"
      val svcType: TextTree[CSValue] = q"I${svcName.capitalize}$genericParam"

      // The implemented runtime contract follows the context mode: `none` keeps
      // the historical context-free IBaboon*Service; `abstract`/`type` use the
      // context-carrying IBaboon*ServiceCtx so the service context is supplied
      // per `Invoke`, not baked into the constructor. `rt` (errors mode) is a
      // per-construction dependency and stays a field.
      val implementsClause: TextTree[CSValue] = svcCtxTypeName match {
        case None     => q"${if (isJson) iBaboonJsonService else iBaboonUebaService}<$retType>"
        case Some(tn) => q"${if (isJson) iBaboonJsonServiceCtx else iBaboonUebaServiceCtx}<$tn, $retType>"
      }

      val rtField: Option[(String, TextTree[CSValue])] =
        if (resolved.noErrors) None else Some(("rt", q"IBaboonServiceRt"))

      val fieldDecls: List[TextTree[CSValue]] =
        q"private readonly $svcType _impl;" ::
          rtField.toList.map { case (n, t) => q"private readonly $t _$n;" }

      val ctorParams: TextTree[CSValue] = {
        val all = q"$svcType impl" :: rtField.toList.map { case (n, t) => q"$t $n" }
        all.join(", ")
      }
      val ctorAssigns: List[TextTree[CSValue]] =
        q"_impl = impl;" :: rtField.toList.map { case (n, _) => q"_$n = $n;" }

      val invokerArgs: List[TextTree[CSValue]] = {
        val base       = List(q"method", q"data", q"_impl")
        val withRt     = rtField.fold(base)(_ => base :+ q"_rt")
        val withSvcCtx = svcCtxArgName.fold(withRt)(n => withRt :+ q"$n")
        withSvcCtx :+ q"$codecCtxName"
      }

      q"""public sealed class $wrapperName$genericParam : $implementsClause
         |{
         |    public $csString ServiceName => "$svcName";
         |    ${fieldDecls.join("\n").shift(4).trim}
         |
         |    public $wrapperName($ctorParams)
         |    {
         |        ${ctorAssigns.join("\n").shift(8).trim}
         |    }
         |
         |    public $retType Invoke($baboonMethodId method, $wireType data, $ctxParamDecl$baboonCodecContext $codecCtxName)
         |    {
         |        return ${svcName}Wiring.$invokerName$genericParam(${invokerArgs.join(", ")});
         |    }
         |}""".stripMargin
    }

    // ========== noErrors mode ==========

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[CSValue] = {
      val svcName = service.id.name.name

      val jsonMethod =
        if (hasActiveJsonCodecs(service))
          Some(generateNoErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasActiveUebaCodecs(service))
          Some(generateNoErrorsUebaMethod(service))
        else None

      val methods = Seq(jsonMethod, uebaMethod).flatten.join("\n\n")

      val wiringClass =
        q"""public static class ${svcName}Wiring
           |{
           |    ${methods.shift(4).trim}
           |}""".stripMargin

      val wrappers = generateServiceWrappers(
        service,
        jsonRetType = taskWrap(q"$csString"),
        uebaRetType = taskWrap(q"byte[]"),
      )

      Seq(Some(wiringClass), wrappers).flatten.join("\n\n")
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[CSValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"result")
              q"""var encoded = $encOut;
                 |return encoded.ToString($nsFormatting.None);""".stripMargin
            case None =>
              q"""return "null";"""
          }

          val callExpr = m.out match {
            case Some(_) => q"var result = ${awaitKw}impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);"
            case None    => q"${awaitKw}impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);"
          }

          q"""case "${m.name.name}":
             |{
             |    var wire = $nsJToken.Parse(data);
             |    var decoded = $decodeIn;
             |    $callExpr
             |    ${encodeOutput.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""public static $asyncKw${taskWrap(q"$csString")} InvokeJson$genericParam(
         |    $baboonMethodId method,
         |    $csString data,
         |    I${svcName.capitalize}$genericParam impl,
         |    $ctxParamDecl$baboonCodecContext $codecCtxName)
         |{
         |    switch (method.MethodName)
         |    {
         |        ${cases.shift(8).trim}
         |        default:
         |            throw new $baboonWiringException(new $baboonWiringError.NoMatchingMethod(method));
         |    }
         |}""".stripMargin
    }

    private def generateNoErrorsUebaMethod(service: Typedef.Service): TextTree[CSValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"br")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"bw", q"result")
              q"""var oms = new $memoryStream();
                 |var bw = new $binaryWriter(oms);
                 |$encStmt
                 |bw.Flush();
                 |return oms.ToArray();""".stripMargin
            case None =>
              q"""return new byte[0];"""
          }

          val callExpr = m.out match {
            case Some(_) => q"var result = ${awaitKw}impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);"
            case None    => q"${awaitKw}impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);"
          }

          q"""case "${m.name.name}":
             |{
             |    var ims = new $memoryStream(data);
             |    var br = new $binaryReader(ims);
             |    var decoded = $decodeIn;
             |    $callExpr
             |    ${encodeOutput.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""public static $asyncKw${taskWrap(q"byte[]")} InvokeUeba$genericParam(
         |    $baboonMethodId method,
         |    byte[] data,
         |    I${svcName.capitalize}$genericParam impl,
         |    $ctxParamDecl$baboonCodecContext $codecCtxName)
         |{
         |    switch (method.MethodName)
         |    {
         |        ${cases.shift(8).trim}
         |        default:
         |            throw new $baboonWiringException(new $baboonWiringError.NoMatchingMethod(method));
         |    }
         |}""".stripMargin
    }

    // ========== errors mode ==========

    private def generateErrorsWiring(service: Typedef.Service): TextTree[CSValue] = {
      val svcName = service.id.name.name

      val jsonMethod =
        if (hasActiveJsonCodecs(service))
          Some(generateErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasActiveUebaCodecs(service))
          Some(generateErrorsUebaMethod(service))
        else None

      val methods = Seq(jsonMethod, uebaMethod).flatten.join("\n\n")

      val wiringClass =
        q"""public static class ${svcName}Wiring
           |{
           |    ${methods.shift(4).trim}
           |}""".stripMargin

      // In errors mode the underlying InvokeJson_/InvokeUeba_ return shape is
      // wrapped in the service-result container (e.g. Either<…>) — the
      // wrapper's `Invoke` return type must mirror that for IBaboon*Service<R>
      // to type-check.
      val jsonContainer: String = ct("BaboonWiringError", "string")
      val uebaContainer: String = ct("BaboonWiringError", "byte[]")

      val wrappers = generateServiceWrappers(
        service,
        jsonRetType = taskWrap(q"$jsonContainer"),
        uebaRetType = taskWrap(q"$uebaContainer"),
      )

      Seq(Some(wiringClass), wrappers).flatten.join("\n\n")
    }

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def renderFq(tree: TextTree[CSValue]): String = tree.mapRender {
      case t: CSValue.CSType     => (t.pkg.parts :+ t.name).mkString(".")
      case t: CSValue.CSTypeName => t.name
    }

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[CSValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct("BaboonWiringError", "string")

      val cases = service.methods.map {
        m =>
          val inRef    = trans.asCsRef(m.sig, domain, evo)
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          if (isAsync) generateErrorsJsonCaseAsync(m, inRef, decodeIn)
          else {
          val decodeStep =
            q"""${ct("BaboonWiringError", renderFq(inRef))} input;
               |try
               |{
               |    var wire = $nsJToken.Parse(data);
               |    input = rt.Pure<$baboonWiringError, $inRef>($decodeIn);
               |}
               |catch ($csException ex)
               |{
               |    input = rt.Fail<$baboonWiringError, $inRef>(new $baboonWiringError.DecoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType   = trans.asCsRef(outRef, domain, evo)
              val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"v")

              val callBody = if (hasErrType) {
                val errType = trans.asCsRef(m.err.get, domain, evo)
                q"""try
                   |{
                   |    var callResult = impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}v);
                   |    return rt.LeftMap<$errType, $outType, $baboonWiringError>(
                   |        callResult, err => new $baboonWiringError.CallFailed(method, err));
                   |}
                   |catch ($csException ex)
                   |{
                   |    return rt.Fail<$baboonWiringError, $outType>(new $baboonWiringError.CallFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try
                   |{
                   |    return rt.Pure<$baboonWiringError, $outType>(impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}v));
                   |}
                   |catch ($csException ex)
                   |{
                   |    return rt.Fail<$baboonWiringError, $outType>(new $baboonWiringError.CallFailed(method, ex));
                   |}""".stripMargin
              }

              q"""var output = rt.FlatMap<$baboonWiringError, $inRef, $outType>(input, v =>
                 |{
                 |    ${callBody.shift(4).trim}
                 |});
                 |return rt.FlatMap<$baboonWiringError, $outType, $csString>(output, v =>
                 |{
                 |    try
                 |    {
                 |        var encoded = $encodeOut;
                 |        return rt.Pure<$baboonWiringError, $csString>(encoded.ToString($nsFormatting.None));
                 |    }
                 |    catch ($csException ex)
                 |    {
                 |        return rt.Fail<$baboonWiringError, $csString>(new $baboonWiringError.EncoderFailed(method, ex));
                 |    }
                 |});""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                val errType = trans.asCsRef(m.err.get, domain, evo)
                q"""try
                   |{
                   |    var callResult = impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}v);
                   |    return rt.LeftMap<$errType, $unit, $baboonWiringError>(
                   |        callResult, err => new $baboonWiringError.CallFailed(method, err));
                   |}
                   |catch ($csException ex)
                   |{
                   |    return rt.Fail<$baboonWiringError, $unit>(new $baboonWiringError.CallFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try
                   |{
                   |    impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}v);
                   |    return rt.Pure<$baboonWiringError, $unit>($unit.Default);
                   |}
                   |catch ($csException ex)
                   |{
                   |    return rt.Fail<$baboonWiringError, $unit>(new $baboonWiringError.CallFailed(method, ex));
                   |}""".stripMargin
              }

              q"""return rt.FlatMap<$baboonWiringError, $inRef, $csString>(input, v =>
                 |{
                 |    ${callBody.shift(4).trim}
                 |    return rt.Pure<$baboonWiringError, $csString>("null");
                 |});""".stripMargin
          }

          q"""case "${m.name.name}":
             |{
             |    ${decodeStep.shift(4).trim}
             |    ${callAndEncodeStep.shift(4).trim}
             |}""".stripMargin
          }
      }.join("\n")

      q"""public static $asyncKw${taskWrap(q"$wiringRetType")} InvokeJson$genericParam(
         |    $baboonMethodId method,
         |    $csString data,
         |    I${svcName.capitalize}$genericParam impl,
         |    IBaboonServiceRt rt,
         |    $ctxParamDecl$baboonCodecContext $codecCtxName)
         |{
         |    switch (method.MethodName)
         |    {
         |        ${cases.shift(8).trim}
         |        default:
         |            return rt.Fail<$baboonWiringError, $csString>(new $baboonWiringError.NoMatchingMethod(method));
         |    }
         |}""".stripMargin
    }

    /** Async errors-mode JSON case (one `switch` arm). Mirrors the TS reference:
      * the decoded input stays a raw value (never re-wrapped in the result
      * container) so the impl `Task<…>` can be `await`ed in straight-line code
      * — the abstract `IBaboonServiceRt` exposes no async bind, so the sync
      * `FlatMap(input, v => …)` closure form (which cannot `await`) is replaced
      * by linear try/await/early-return. Wrapping happens only at the
      * `rt.Pure` / `rt.Fail` boundaries, identical to the sync arm's results.
      */
    private def generateErrorsJsonCaseAsync(
      m: Typedef.MethodDef,
      inRef: TextTree[CSValue],
      decodeIn: TextTree[CSValue],
    ): TextTree[CSValue] = {
      val hasErrType = m.err.isDefined && !resolved.noErrors

      val decodeStep =
        q"""$inRef decoded;
           |try
           |{
           |    var wire = $nsJToken.Parse(data);
           |    decoded = $decodeIn;
           |}
           |catch ($csException ex)
           |{
           |    return rt.Fail<$baboonWiringError, $csString>(new $baboonWiringError.DecoderFailed(method, ex));
           |}""".stripMargin

      // Encode-after-call, expressed with the abstract `rt.FlatMap` so it stays
      // container-agnostic: the only async hop (the impl `Task`) is awaited
      // first, then the already-resolved container is threaded synchronously.
      val encodeFlatMap = m.out match {
        case Some(outRef) =>
          val outType   = trans.asCsRef(outRef, domain, evo)
          val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"v")
          q"""return rt.FlatMap<$baboonWiringError, $outType, $csString>(output, v =>
             |{
             |    try
             |    {
             |        var encoded = $encodeOut;
             |        return rt.Pure<$baboonWiringError, $csString>(encoded.ToString($nsFormatting.None));
             |    }
             |    catch ($csException ex)
             |    {
             |        return rt.Fail<$baboonWiringError, $csString>(new $baboonWiringError.EncoderFailed(method, ex));
             |    }
             |});""".stripMargin
        case None => q""
      }

      val callAndEncodeStep = m.out match {
        case Some(outRef) =>
          val outType = trans.asCsRef(outRef, domain, evo)

          val callStep = if (hasErrType) {
            val errType = trans.asCsRef(m.err.get, domain, evo)
            q"""${ct("BaboonWiringError", renderFq(outType))} output;
               |try
               |{
               |    var callResult = await impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);
               |    output = rt.LeftMap<$errType, $outType, $baboonWiringError>(
               |        callResult, err => new $baboonWiringError.CallFailed(method, err));
               |}
               |catch ($csException ex)
               |{
               |    return rt.Fail<$baboonWiringError, $csString>(new $baboonWiringError.CallFailed(method, ex));
               |}""".stripMargin
          } else {
            q"""${ct("BaboonWiringError", renderFq(outType))} output;
               |try
               |{
               |    var callResultValue = await impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);
               |    output = rt.Pure<$baboonWiringError, $outType>(callResultValue);
               |}
               |catch ($csException ex)
               |{
               |    return rt.Fail<$baboonWiringError, $csString>(new $baboonWiringError.CallFailed(method, ex));
               |}""".stripMargin
          }

          q"""$callStep
             |${encodeFlatMap.trim}""".stripMargin

        case None =>
          if (hasErrType) {
            val errType = trans.asCsRef(m.err.get, domain, evo)
            q"""try
               |{
               |    var callResult = await impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);
               |    var mapped = rt.LeftMap<$errType, $unit, $baboonWiringError>(
               |        callResult, err => new $baboonWiringError.CallFailed(method, err));
               |    return rt.FlatMap<$baboonWiringError, $unit, $csString>(mapped, v => rt.Pure<$baboonWiringError, $csString>("null"));
               |}
               |catch ($csException ex)
               |{
               |    return rt.Fail<$baboonWiringError, $csString>(new $baboonWiringError.CallFailed(method, ex));
               |}""".stripMargin
          } else {
            q"""try
               |{
               |    await impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);
               |    return rt.Pure<$baboonWiringError, $csString>("null");
               |}
               |catch ($csException ex)
               |{
               |    return rt.Fail<$baboonWiringError, $csString>(new $baboonWiringError.CallFailed(method, ex));
               |}""".stripMargin
          }
      }

      q"""case "${m.name.name}":
         |{
         |    ${decodeStep.shift(4).trim}
         |    ${callAndEncodeStep.shift(4).trim}
         |}""".stripMargin
    }

    private def generateErrorsUebaMethod(service: Typedef.Service): TextTree[CSValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct("BaboonWiringError", "byte[]")

      val cases = service.methods.map {
        m =>
          val inRef    = trans.asCsRef(m.sig, domain, evo)
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"br")

          if (isAsync) generateErrorsUebaCaseAsync(m, inRef, decodeIn)
          else {
          val decodeStep =
            q"""${ct("BaboonWiringError", renderFq(inRef))} input;
               |try
               |{
               |    var ms = new $memoryStream(data);
               |    var br = new $binaryReader(ms);
               |    input = rt.Pure<$baboonWiringError, $inRef>($decodeIn);
               |}
               |catch ($csException ex)
               |{
               |    input = rt.Fail<$baboonWiringError, $inRef>(new $baboonWiringError.DecoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType = trans.asCsRef(outRef, domain, evo)
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"bw", q"v")

              val callBody = if (hasErrType) {
                val errType = trans.asCsRef(m.err.get, domain, evo)
                q"""try
                   |{
                   |    var callResult = impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}v);
                   |    return rt.LeftMap<$errType, $outType, $baboonWiringError>(
                   |        callResult, err => new $baboonWiringError.CallFailed(method, err));
                   |}
                   |catch ($csException ex)
                   |{
                   |    return rt.Fail<$baboonWiringError, $outType>(new $baboonWiringError.CallFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try
                   |{
                   |    return rt.Pure<$baboonWiringError, $outType>(impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}v));
                   |}
                   |catch ($csException ex)
                   |{
                   |    return rt.Fail<$baboonWiringError, $outType>(new $baboonWiringError.CallFailed(method, ex));
                   |}""".stripMargin
              }

              q"""var output = rt.FlatMap<$baboonWiringError, $inRef, $outType>(input, v =>
                 |{
                 |    ${callBody.shift(4).trim}
                 |});
                 |return rt.FlatMap<$baboonWiringError, $outType, byte[]>(output, v =>
                 |{
                 |    try
                 |    {
                 |        var oms = new $memoryStream();
                 |        var bw = new $binaryWriter(oms);
                 |        $encStmt
                 |        bw.Flush();
                 |        return rt.Pure<$baboonWiringError, byte[]>(oms.ToArray());
                 |    }
                 |    catch ($csException ex)
                 |    {
                 |        return rt.Fail<$baboonWiringError, byte[]>(new $baboonWiringError.EncoderFailed(method, ex));
                 |    }
                 |});""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                val errType = trans.asCsRef(m.err.get, domain, evo)
                q"""try
                   |{
                   |    var callResult = impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}v);
                   |    return rt.LeftMap<$errType, $unit, $baboonWiringError>(
                   |        callResult, err => new $baboonWiringError.CallFailed(method, err));
                   |}
                   |catch ($csException ex)
                   |{
                   |    return rt.Fail<$baboonWiringError, $unit>(new $baboonWiringError.CallFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try
                   |{
                   |    impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}v);
                   |    return rt.Pure<$baboonWiringError, $unit>($unit.Default);
                   |}
                   |catch ($csException ex)
                   |{
                   |    return rt.Fail<$baboonWiringError, $unit>(new $baboonWiringError.CallFailed(method, ex));
                   |}""".stripMargin
              }

              q"""return rt.FlatMap<$baboonWiringError, $inRef, byte[]>(input, v =>
                 |{
                 |    ${callBody.shift(4).trim}
                 |    return rt.Pure<$baboonWiringError, byte[]>(new byte[0]);
                 |});""".stripMargin
          }

          q"""case "${m.name.name}":
             |{
             |    ${decodeStep.shift(4).trim}
             |    ${callAndEncodeStep.shift(4).trim}
             |}""".stripMargin
          }
      }.join("\n")

      q"""public static $asyncKw${taskWrap(q"$wiringRetType")} InvokeUeba$genericParam(
         |    $baboonMethodId method,
         |    byte[] data,
         |    I${svcName.capitalize}$genericParam impl,
         |    IBaboonServiceRt rt,
         |    $ctxParamDecl$baboonCodecContext $codecCtxName)
         |{
         |    switch (method.MethodName)
         |    {
         |        ${cases.shift(8).trim}
         |        default:
         |            return rt.Fail<$baboonWiringError, byte[]>(new $baboonWiringError.NoMatchingMethod(method));
         |    }
         |}""".stripMargin
    }

    /** Async errors-mode UEBA case (one `switch` arm). See
      * [[generateErrorsJsonCaseAsync]] for the await-then-thread rationale.
      */
    private def generateErrorsUebaCaseAsync(
      m: Typedef.MethodDef,
      inRef: TextTree[CSValue],
      decodeIn: TextTree[CSValue],
    ): TextTree[CSValue] = {
      val hasErrType = m.err.isDefined && !resolved.noErrors

      val decodeStep =
        q"""$inRef decoded;
           |try
           |{
           |    var ms = new $memoryStream(data);
           |    var br = new $binaryReader(ms);
           |    decoded = $decodeIn;
           |}
           |catch ($csException ex)
           |{
           |    return rt.Fail<$baboonWiringError, byte[]>(new $baboonWiringError.DecoderFailed(method, ex));
           |}""".stripMargin

      val encodeFlatMap = m.out match {
        case Some(outRef) =>
          val outType = trans.asCsRef(outRef, domain, evo)
          val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"bw", q"v")
          q"""return rt.FlatMap<$baboonWiringError, $outType, byte[]>(output, v =>
             |{
             |    try
             |    {
             |        var oms = new $memoryStream();
             |        var bw = new $binaryWriter(oms);
             |        $encStmt
             |        bw.Flush();
             |        return rt.Pure<$baboonWiringError, byte[]>(oms.ToArray());
             |    }
             |    catch ($csException ex)
             |    {
             |        return rt.Fail<$baboonWiringError, byte[]>(new $baboonWiringError.EncoderFailed(method, ex));
             |    }
             |});""".stripMargin
        case None => q""
      }

      val callAndEncodeStep = m.out match {
        case Some(outRef) =>
          val outType = trans.asCsRef(outRef, domain, evo)

          val callStep = if (hasErrType) {
            val errType = trans.asCsRef(m.err.get, domain, evo)
            q"""${ct("BaboonWiringError", renderFq(outType))} output;
               |try
               |{
               |    var callResult = await impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);
               |    output = rt.LeftMap<$errType, $outType, $baboonWiringError>(
               |        callResult, err => new $baboonWiringError.CallFailed(method, err));
               |}
               |catch ($csException ex)
               |{
               |    return rt.Fail<$baboonWiringError, byte[]>(new $baboonWiringError.CallFailed(method, ex));
               |}""".stripMargin
          } else {
            q"""${ct("BaboonWiringError", renderFq(outType))} output;
               |try
               |{
               |    var callResultValue = await impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);
               |    output = rt.Pure<$baboonWiringError, $outType>(callResultValue);
               |}
               |catch ($csException ex)
               |{
               |    return rt.Fail<$baboonWiringError, byte[]>(new $baboonWiringError.CallFailed(method, ex));
               |}""".stripMargin
          }

          q"""$callStep
             |${encodeFlatMap.trim}""".stripMargin

        case None =>
          if (hasErrType) {
            val errType = trans.asCsRef(m.err.get, domain, evo)
            q"""try
               |{
               |    var callResult = await impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);
               |    var mapped = rt.LeftMap<$errType, $unit, $baboonWiringError>(
               |        callResult, err => new $baboonWiringError.CallFailed(method, err));
               |    return rt.FlatMap<$baboonWiringError, $unit, byte[]>(mapped, v => rt.Pure<$baboonWiringError, byte[]>(new byte[0]));
               |}
               |catch ($csException ex)
               |{
               |    return rt.Fail<$baboonWiringError, byte[]>(new $baboonWiringError.CallFailed(method, ex));
               |}""".stripMargin
          } else {
            q"""try
               |{
               |    await impl.${escapeCsKeyword(m.name.name.capitalize)}(${ctxArgPass}decoded);
               |    return rt.Pure<$baboonWiringError, byte[]>(new byte[0]);
               |}
               |catch ($csException ex)
               |{
               |    return rt.Fail<$baboonWiringError, byte[]>(new $baboonWiringError.CallFailed(method, ex));
               |}""".stripMargin
          }
      }

      q"""case "${m.name.name}":
         |{
         |    ${decodeStep.shift(4).trim}
         |    ${callAndEncodeStep.shift(4).trim}
         |}""".stripMargin
    }
  }
}
