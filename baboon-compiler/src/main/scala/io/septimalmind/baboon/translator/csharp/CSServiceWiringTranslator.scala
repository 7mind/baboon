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

    private val hasJsonCodecs: Boolean = codecs.exists(_.isInstanceOf[CSJsonCodecGenerator])
    private val hasUebaCodecs: Boolean = codecs.exists(_.isInstanceOf[CSUEBACodecGenerator])

    private def jsonCodecName(typeId: TypeId.User): CSValue.CSType = {
      val srcRef = trans.asCsTypeKeepForeigns(typeId, domain, evo)
      CSValue.CSType(srcRef.pkg, s"${srcRef.name}_JsonCodec", srcRef.fq, CSTypeOrigin(typeId, domain).asDerived)
    }

    private def uebaCodecName(typeId: TypeId.User): CSValue.CSType = {
      val srcRef = trans.asCsTypeKeepForeigns(typeId, domain, evo)
      CSValue.CSType(srcRef.pkg, s"${srcRef.name}_UEBACodec", srcRef.fq, CSTypeOrigin(typeId, domain).asDerived)
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

    // ========== noErrors mode ==========

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[CSValue] = {
      val svcName = service.id.name.name

      val jsonMethod =
        if (hasJsonCodecs)
          Some(generateNoErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasUebaCodecs)
          Some(generateNoErrorsUebaMethod(service))
        else None

      val methods = Seq(jsonMethod, uebaMethod).flatten.join("\n\n")

      q"""public static class ${svcName}Wiring
         |{
         |    ${methods.shift(4).trim}
         |}""".stripMargin
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[CSValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])
              q"""var encoded = $outCodec.Instance.Encode(ctx, result);
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
             |    var decoded = $inCodec.Instance.Decode(ctx, wire);
             |    $callExpr
             |    ${encodeOutput.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""public static $csString InvokeJson$genericParam(
         |    $baboonMethodId method,
         |    $csString data,
         |    $svcName.$svcName$genericParam impl,
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
          val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])
              q"""var oms = new $memoryStream();
                 |var bw = new $binaryWriter(oms);
                 |$outCodec.Instance.Encode(ctx, bw, result);
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
             |    var decoded = $inCodec.Instance.Decode(ctx, br);
             |    $callExpr
             |    ${encodeOutput.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""public static byte[] InvokeUeba$genericParam(
         |    $baboonMethodId method,
         |    byte[] data,
         |    $svcName.$svcName$genericParam impl,
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
        if (hasJsonCodecs)
          Some(generateErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasUebaCodecs)
          Some(generateErrorsUebaMethod(service))
        else None

      val methods = Seq(jsonMethod, uebaMethod).flatten.join("\n\n")

      q"""public static class ${svcName}Wiring
         |{
         |    ${methods.shift(4).trim}
         |}""".stripMargin
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
          val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])
          val inRef   = trans.asCsRef(m.sig, domain, evo)

          val decodeStep =
            q"""${ct("BaboonWiringError", renderFq(inRef))} input;
               |try
               |{
               |    var wire = $nsJToken.Parse(data);
               |    input = rt.Pure<$baboonWiringError, $inRef>($inCodec.Instance.Decode(ctx, wire));
               |}
               |catch ($csException ex)
               |{
               |    input = rt.Fail<$baboonWiringError, $inRef>(new $baboonWiringError.DecoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType  = trans.asCsRef(outRef, domain, evo)
              val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])

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
                 |        var encoded = $outCodec.Instance.Encode(ctx, v);
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
         |    $svcName.$svcName$genericParam impl,
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
          val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])
          val inRef   = trans.asCsRef(m.sig, domain, evo)

          val decodeStep =
            q"""${ct("BaboonWiringError", renderFq(inRef))} input;
               |try
               |{
               |    var ms = new $memoryStream(data);
               |    var br = new $binaryReader(ms);
               |    input = rt.Pure<$baboonWiringError, $inRef>($inCodec.Instance.Decode(ctx, br));
               |}
               |catch ($csException ex)
               |{
               |    input = rt.Fail<$baboonWiringError, $inRef>(new $baboonWiringError.DecoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType  = trans.asCsRef(outRef, domain, evo)
              val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])

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
                 |        $outCodec.Instance.Encode(ctx, bw, v);
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
         |    $svcName.$svcName$genericParam impl,
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
