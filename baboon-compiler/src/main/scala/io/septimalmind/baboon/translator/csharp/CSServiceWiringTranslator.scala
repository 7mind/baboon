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
      case u: TypeId.User => q"${jsonCodecName(u)}.Instance.Decode(ctx, $wire)"
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
      case u: TypeId.User => q"${jsonCodecName(u)}.Instance.Encode(ctx, $value)"
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
      case u: TypeId.User => q"${uebaCodecName(u)}.Instance.Decode(ctx, $br)"
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
      case u: TypeId.User => q"${uebaCodecName(u)}.Instance.Encode(ctx, $bw, $value);"
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
      * C# wiring is currently sync-only — no `Task<…>` variant is emitted.
      * The runtime `JsonMuxer<R>` / `UebaMuxer<R>` stay parametric in R so
      * a future async axis could plug in without breaking the contract.
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
      val iface       = if (isJson) iBaboonJsonService else iBaboonUebaService
      val wrapperName = s"${svcName}${if (isJson) "JsonService" else "UebaService"}"
      val svcType: TextTree[CSValue] = q"I${svcName.capitalize}$genericParam"

      // Constructor and forwarded-args: every extra dependency consumed by
      // ${svc}Wiring.InvokeJson/InvokeUeba (`rt` in errors mode, plus any
      // service-context parameter) is baked at construction time so the
      // runtime IBaboon*Service<R> contract stays uniform across modes.
      val rtField: Option[(String, TextTree[CSValue])] =
        if (resolved.noErrors) None else Some(("rt", q"IBaboonServiceRt"))

      val svcCtxField: Option[(String, TextTree[CSValue])] = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => None
        case ResolvedServiceContext.AbstractContext(tn, pn) => Some((pn, q"$tn"))
        case ResolvedServiceContext.ConcreteContext(tn, pn) => Some((pn, q"$tn"))
      }

      val extraFields: List[(String, TextTree[CSValue])] = List(rtField, svcCtxField).flatten

      val fieldDecls: List[TextTree[CSValue]] =
        q"private readonly $svcType _impl;" ::
          extraFields.map { case (n, t) => q"private readonly $t _$n;" }

      val ctorParams: TextTree[CSValue] = {
        val all = q"$svcType impl" :: extraFields.map { case (n, t) => q"$t $n" }
        all.join(", ")
      }
      val ctorAssigns: List[TextTree[CSValue]] =
        q"_impl = impl;" :: extraFields.map { case (n, _) => q"_$n = $n;" }

      val invokerArgs: List[TextTree[CSValue]] = {
        val base = List(q"method", q"data", q"_impl")
        val withRt = rtField.fold(base)(_ => base :+ q"_rt")
        val withSvcCtx = svcCtxField.fold(withRt)(f => withRt :+ q"_${f._1}")
        withSvcCtx :+ q"ctx"
      }

      q"""public sealed class $wrapperName : $iface<$retType>
         |{
         |    public $csString ServiceName => "$svcName";
         |    ${fieldDecls.join("\n").shift(4).trim}
         |
         |    public $wrapperName($ctorParams)
         |    {
         |        ${ctorAssigns.join("\n").shift(8).trim}
         |    }
         |
         |    public $retType Invoke($baboonMethodId method, $wireType data, $baboonCodecContext ctx)
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
        jsonRetType = q"$csString",
        uebaRetType = q"byte[]",
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
            case Some(_) => q"var result = impl.${m.name.name}(${ctxArgPass}decoded);"
            case None    => q"impl.${m.name.name}(${ctxArgPass}decoded);"
          }

          q"""case "${m.name.name}":
             |{
             |    var wire = $nsJToken.Parse(data);
             |    var decoded = $decodeIn;
             |    $callExpr
             |    ${encodeOutput.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""public static $csString InvokeJson$genericParam(
         |    $baboonMethodId method,
         |    $csString data,
         |    I${svcName.capitalize}$genericParam impl,
         |    $ctxParamDecl$baboonCodecContext ctx)
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
            case Some(_) => q"var result = impl.${m.name.name}(${ctxArgPass}decoded);"
            case None    => q"impl.${m.name.name}(${ctxArgPass}decoded);"
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

      q"""public static byte[] InvokeUeba$genericParam(
         |    $baboonMethodId method,
         |    byte[] data,
         |    I${svcName.capitalize}$genericParam impl,
         |    $ctxParamDecl$baboonCodecContext ctx)
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
        jsonRetType = q"$jsonContainer",
        uebaRetType = q"$uebaContainer",
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
                   |    var callResult = impl.${m.name.name}(${ctxArgPass}v);
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
                   |    return rt.Pure<$baboonWiringError, $outType>(impl.${m.name.name}(${ctxArgPass}v));
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
                   |    var callResult = impl.${m.name.name}(${ctxArgPass}v);
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
                   |    impl.${m.name.name}(${ctxArgPass}v);
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
      }.join("\n")

      q"""public static $wiringRetType InvokeJson$genericParam(
         |    $baboonMethodId method,
         |    $csString data,
         |    I${svcName.capitalize}$genericParam impl,
         |    IBaboonServiceRt rt,
         |    $ctxParamDecl$baboonCodecContext ctx)
         |{
         |    switch (method.MethodName)
         |    {
         |        ${cases.shift(8).trim}
         |        default:
         |            return rt.Fail<$baboonWiringError, $csString>(new $baboonWiringError.NoMatchingMethod(method));
         |    }
         |}""".stripMargin
    }

    private def generateErrorsUebaMethod(service: Typedef.Service): TextTree[CSValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct("BaboonWiringError", "byte[]")

      val cases = service.methods.map {
        m =>
          val inRef    = trans.asCsRef(m.sig, domain, evo)
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"br")

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
                   |    var callResult = impl.${m.name.name}(${ctxArgPass}v);
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
                   |    return rt.Pure<$baboonWiringError, $outType>(impl.${m.name.name}(${ctxArgPass}v));
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
                   |    var callResult = impl.${m.name.name}(${ctxArgPass}v);
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
                   |    impl.${m.name.name}(${ctxArgPass}v);
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
      }.join("\n")

      q"""public static $wiringRetType InvokeUeba$genericParam(
         |    $baboonMethodId method,
         |    byte[] data,
         |    I${svcName.capitalize}$genericParam impl,
         |    IBaboonServiceRt rt,
         |    $ctxParamDecl$baboonCodecContext ctx)
         |{
         |    switch (method.MethodName)
         |    {
         |        ${cases.shift(8).trim}
         |        default:
         |            return rt.Fail<$baboonWiringError, byte[]>(new $baboonWiringError.NoMatchingMethod(method));
         |    }
         |}""".stripMargin
    }
  }
}
