package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait JvServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[JvValue]]
  def translateServiceRt(domain: Domain): Option[TextTree[JvValue]]
}

object JvServiceWiringTranslator {
  class Impl(
    target: JvTarget,
    trans: JvTypeTranslator,
    codecs: Set[JvCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
  ) extends JvServiceWiringTranslator {

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "java", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "java", target.language.serviceContext, target.language.pragmas)

    private val hasJsonCodecs: Boolean = codecs.exists(_.isInstanceOf[JvJsonCodecGenerator])
    private val hasUebaCodecs: Boolean = codecs.exists(_.isInstanceOf[JvUEBACodecGenerator])

    private val resultJvType: Option[JvValue.JvType] = resolved.resultType.map { rt =>
      JvValue.JvType(baboonRuntimePkg, rt)
    }

    private def jsonCodecName(typeId: TypeId.User): JvValue.JvType = {
      val srcRef = trans.toJvTypeRefKeepForeigns(typeId, domain, evo)
      val domainPkg = trans.toJvPkg(domain.id, domain.version, evo)
      val ownerPrefix = srcRef.pkg.parts.toSeq.drop(domainPkg.parts.toSeq.length)
      val prefixStr = if (ownerPrefix.nonEmpty) ownerPrefix.mkString("_") + "_" else ""
      val realPkg = trans.effectiveJvPkg(typeId.owner, domain, evo)
      JvValue.JvType(realPkg, s"${prefixStr}${srcRef.name}_JsonCodec", srcRef.fq)
    }

    private def uebaCodecName(typeId: TypeId.User): JvValue.JvType = {
      val srcRef = trans.toJvTypeRefKeepForeigns(typeId, domain, evo)
      val domainPkg = trans.toJvPkg(domain.id, domain.version, evo)
      val ownerPrefix = srcRef.pkg.parts.toSeq.drop(domainPkg.parts.toSeq.length)
      val prefixStr = if (ownerPrefix.nonEmpty) ownerPrefix.mkString("_") + "_" else ""
      val realPkg = trans.effectiveJvPkg(typeId.owner, domain, evo)
      JvValue.JvType(realPkg, s"${prefixStr}${srcRef.name}_UEBACodec", srcRef.fq)
    }

    private def renderContainer(error: String, success: String): String = {
      val p        = resolved.pattern.get.replace("$error", error).replace("$success", success)
      val typeName = resolved.hkt.map(_.name).getOrElse(resolved.resultType.get)
      s"$typeName$p"
    }

    private def renderConcreteContainer(error: String, success: String): String = {
      val p = resolved.pattern.get.replace("$error", error).replace("$success", success)
      s"${resolved.resultType.get}$p"
    }

    override def translateServiceRt(domain: Domain): Option[TextTree[JvValue]] = {
      if (resolved.noErrors) return None

      val hasServices = domain.defs.meta.nodes.values.exists {
        case DomainMember.User(_, _: Typedef.Service, _, _) => true
        case _                                               => false
      }
      if (!hasServices) return None

      val ct = renderContainer _

      val traitTypeParams = resolved.hkt match {
        case Some(h) => s"<${h.name}${h.signature}>"
        case None    => ""
      }

      val rtTrait: TextTree[JvValue] =
        q"""public interface IBaboonServiceRt$traitTypeParams {
           |  <L, R> ${ct("L", "R")} pure(R value);
           |  <L, R> ${ct("L", "R")} fail(L error);
           |  <A, B, C> ${ct("C", "B")} leftMap(${ct("A", "B")} value, java.util.function.Function<A, C> f);
           |  <A, B, C> ${ct("A", "C")} flatMap(${ct("A", "B")} value, java.util.function.Function<B, ${ct("A", "C")}> f);
           |}""".stripMargin

      Some(rtTrait)
    }

    override def translate(defn: DomainMember.User): Option[TextTree[JvValue]] = {
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

    private def genericParam: String = {
      val params = Seq(
        resolved.hkt.map(h => s"${h.name}${h.signature}"),
        resolvedCtx match {
          case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
          case _                                            => None
        },
      ).flatten
      if (params.nonEmpty) params.mkString("<", ", ", ">") else ""
    }

    private def svcTypeArg: String = {
      val params = Seq(
        resolved.hkt.map(_.name),
        resolvedCtx match {
          case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
          case _                                            => None
        },
      ).flatten
      if (params.nonEmpty) params.mkString("<", ", ", ">") else ""
    }

    private def rtTypeArg: String = resolved.hkt match {
      case Some(h) => s"<${h.name}>"
      case None    => ""
    }

    private def renderFq(tree: TextTree[JvValue]): String = tree.mapRender {
      case t: JvValue.JvType     => if (t.predef) t.name else (t.pkg.parts :+ t.name).mkString(".")
      case t: JvValue.JvTypeName => t.name
    }

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[JvValue] = {
      val svcName = service.id.name.name

      val jsonMethod = if (hasJsonCodecs)
        Some(generateNoErrorsJsonMethod(service))
      else None

      val uebaMethod = if (hasUebaCodecs)
        Some(generateNoErrorsUebaMethod(service))
      else None

      val methods = Seq(jsonMethod, uebaMethod).flatten.join("\n\n")

      q"""public final class ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}""".stripMargin
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[JvValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map { m =>
        val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])

        val encodeOutput = m.out match {
          case Some(outRef) =>
            val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])
            q"""var encoded = $outCodec.INSTANCE.encode(ctx, result);
               |yield encoded.toString();""".stripMargin
          case None =>
            q"""yield "null";"""
        }

        val callExpr = m.out match {
          case Some(_) => q"var result = impl.${m.name.name}(${ctxArgPass}decoded);"
          case None    => q"impl.${m.name.name}(${ctxArgPass}decoded);"
        }

        q"""case "${m.name.name}" -> {
           |  var mapper = new $objectMapper();
           |  $jsonNode wire = mapper.readTree(data);
           |  var decoded = $inCodec.INSTANCE.decode(ctx, wire);
           |  $callExpr
           |  ${encodeOutput.shift(2).trim}
           |}""".stripMargin
      }.join("\n")

      q"""public static $svcTypeArg String invokeJson$genericParam(
         |  $baboonMethodId method,
         |  String data,
         |  ${svcName}$svcTypeArg impl,
         |  ${ctxParamDecl}$baboonCodecContext ctx) throws Exception {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default ->
         |      throw new $baboonWiringException(new $baboonWiringError.NoMatchingMethod(method));
         |  };
         |}""".stripMargin
    }

    private def generateNoErrorsUebaMethod(service: Typedef.Service): TextTree[JvValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map { m =>
        val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])

        val encodeOutput = m.out match {
          case Some(outRef) =>
            val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])
            q"""var oms = new $byteArrayOutputStream();
               |var bw = new $binaryOutput(oms);
               |$outCodec.INSTANCE.encode(ctx, bw, result);
               |bw.flush();
               |yield oms.toByteArray();""".stripMargin
          case None =>
            q"yield new byte[0];"
        }

        val callExpr = m.out match {
          case Some(_) => q"var result = impl.${m.name.name}(${ctxArgPass}decoded);"
          case None    => q"impl.${m.name.name}(${ctxArgPass}decoded);"
        }

        q"""case "${m.name.name}" -> {
           |  var ims = new $byteArrayInputStream(data);
           |  var br = new $binaryInput(ims);
           |  var decoded = $inCodec.INSTANCE.decode(ctx, br);
           |  $callExpr
           |  ${encodeOutput.shift(2).trim}
           |}""".stripMargin
      }.join("\n")

      q"""public static $svcTypeArg byte[] invokeUeba$genericParam(
         |  $baboonMethodId method,
         |  byte[] data,
         |  ${svcName}$svcTypeArg impl,
         |  ${ctxParamDecl}$baboonCodecContext ctx) throws Exception {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default ->
         |      throw new $baboonWiringException(new $baboonWiringError.NoMatchingMethod(method));
         |  };
         |}""".stripMargin
    }

    private def generateErrorsWiring(service: Typedef.Service): TextTree[JvValue] = {
      val svcName = service.id.name.name

      val jsonMethod = if (hasJsonCodecs)
        Some(generateErrorsJsonMethod(service))
      else None

      val uebaMethod = if (hasUebaCodecs)
        Some(generateErrorsUebaMethod(service))
      else None

      val methods = Seq(jsonMethod, uebaMethod).flatten.join("\n\n")

      q"""public final class ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}""".stripMargin
    }

    private val bweFq: String = renderFq(q"$baboonWiringError")

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[JvValue] = {
      val svcName = service.id.name.name
      val wiringRetType = ct(bweFq, "String")

      val cases = service.methods.map { m =>
        val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])
        val inRef = trans.asJvRef(m.sig, domain, evo)

        val decodeStep =
          q"""${ct(bweFq, renderFq(inRef))} input;
             |try {
             |  var mapper = new $objectMapper();
             |  $jsonNode wire = mapper.readTree(data);
             |  input = rt.pure($inCodec.INSTANCE.decode(ctx, wire));
             |} catch (Throwable ex) {
             |  input = rt.fail(new $baboonWiringError.DecoderFailed(method, ex));
             |}""".stripMargin

        val hasErrType = m.err.isDefined && !resolved.noErrors

        val callAndEncodeStep = m.out match {
          case Some(outRef) =>
            val outType = trans.asJvRef(outRef, domain, evo)
            val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])

            val callBody = if (hasErrType) {
              q"""try {
                 |  var callResult = impl.${m.name.name}(${ctxArgPass}v);
                 |  return rt.leftMap(
                 |    callResult, err -> new $baboonWiringError.CallFailed(method, err));
                 |} catch (Throwable ex) {
                 |  return rt.fail(new $baboonWiringError.CallFailed(method, ex));
                 |}""".stripMargin
            } else {
              q"""try {
                 |  return rt.pure(impl.${m.name.name}(${ctxArgPass}v));
                 |} catch (Throwable ex) {
                 |  return rt.fail(new $baboonWiringError.CallFailed(method, ex));
                 |}""".stripMargin
            }

            q"""var output = rt.flatMap(input, v -> {
               |  ${callBody.shift(2).trim}
               |});
               |return rt.flatMap(output, v -> {
               |  try {
               |    var encoded = $outCodec.INSTANCE.encode(ctx, v);
               |    return rt.pure(encoded.toString());
               |  } catch (Throwable ex) {
               |    return rt.fail(new $baboonWiringError.EncoderFailed(method, ex));
               |  }
               |});""".stripMargin

          case None =>
            val callBody = if (hasErrType) {
              q"""try {
                 |  var callResult = impl.${m.name.name}(${ctxArgPass}v);
                 |  return rt.leftMap(
                 |    callResult, err -> new $baboonWiringError.CallFailed(method, err));
                 |} catch (Throwable ex) {
                 |  return rt.fail(new $baboonWiringError.CallFailed(method, ex));
                 |}""".stripMargin
            } else {
              q"""try {
                 |  impl.${m.name.name}(${ctxArgPass}v);
                 |  return rt.pure(null);
                 |} catch (Throwable ex) {
                 |  return rt.fail(new $baboonWiringError.CallFailed(method, ex));
                 |}""".stripMargin
            }

            q"""return rt.flatMap(input, v -> {
               |  ${callBody.shift(2).trim}
               |  return rt.pure("null");
               |});""".stripMargin
        }

        q"""case "${m.name.name}" -> {
           |  ${decodeStep.shift(2).trim}
           |  ${callAndEncodeStep.shift(2).trim}
           |}""".stripMargin
      }.join("\n")

      q"""public static $svcTypeArg $wiringRetType invokeJson$genericParam(
         |  $baboonMethodId method,
         |  String data,
         |  ${svcName}$svcTypeArg impl,
         |  IBaboonServiceRt$rtTypeArg rt,
         |  ${ctxParamDecl}$baboonCodecContext ctx) {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default -> rt.fail(new $baboonWiringError.NoMatchingMethod(method));
         |  };
         |}""".stripMargin
    }

    private def generateErrorsUebaMethod(service: Typedef.Service): TextTree[JvValue] = {
      val svcName = service.id.name.name
      val wiringRetType = ct(bweFq, "byte[]")

      val cases = service.methods.map { m =>
        val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])
        val inRef = trans.asJvRef(m.sig, domain, evo)

        val decodeStep =
          q"""${ct(bweFq, renderFq(inRef))} input;
             |try {
             |  var ims = new $byteArrayInputStream(data);
             |  var br = new $binaryInput(ims);
             |  input = rt.pure($inCodec.INSTANCE.decode(ctx, br));
             |} catch (Throwable ex) {
             |  input = rt.fail(new $baboonWiringError.DecoderFailed(method, ex));
             |}""".stripMargin

        val hasErrType = m.err.isDefined && !resolved.noErrors

        val callAndEncodeStep = m.out match {
          case Some(outRef) =>
            val outType = trans.asJvRef(outRef, domain, evo)
            val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])

            val callBody = if (hasErrType) {
              q"""try {
                 |  var callResult = impl.${m.name.name}(${ctxArgPass}v);
                 |  return rt.leftMap(
                 |    callResult, err -> new $baboonWiringError.CallFailed(method, err));
                 |} catch (Throwable ex) {
                 |  return rt.fail(new $baboonWiringError.CallFailed(method, ex));
                 |}""".stripMargin
            } else {
              q"""try {
                 |  return rt.pure(impl.${m.name.name}(${ctxArgPass}v));
                 |} catch (Throwable ex) {
                 |  return rt.fail(new $baboonWiringError.CallFailed(method, ex));
                 |}""".stripMargin
            }

            q"""var output = rt.flatMap(input, v -> {
               |  ${callBody.shift(2).trim}
               |});
               |return rt.flatMap(output, v -> {
               |  try {
               |    var oms = new $byteArrayOutputStream();
               |    var bw = new $binaryOutput(oms);
               |    $outCodec.INSTANCE.encode(ctx, bw, v);
               |    bw.flush();
               |    return rt.pure(oms.toByteArray());
               |  } catch (Throwable ex) {
               |    return rt.fail(new $baboonWiringError.EncoderFailed(method, ex));
               |  }
               |});""".stripMargin

          case None =>
            val callBody = if (hasErrType) {
              q"""try {
                 |  var callResult = impl.${m.name.name}(${ctxArgPass}v);
                 |  return rt.leftMap(
                 |    callResult, err -> new $baboonWiringError.CallFailed(method, err));
                 |} catch (Throwable ex) {
                 |  return rt.fail(new $baboonWiringError.CallFailed(method, ex));
                 |}""".stripMargin
            } else {
              q"""try {
                 |  impl.${m.name.name}(${ctxArgPass}v);
                 |  return rt.pure(null);
                 |} catch (Throwable ex) {
                 |  return rt.fail(new $baboonWiringError.CallFailed(method, ex));
                 |}""".stripMargin
            }

            q"""return rt.flatMap(input, v -> {
               |  ${callBody.shift(2).trim}
               |  return rt.pure(new byte[0]);
               |});""".stripMargin
        }

        q"""case "${m.name.name}" -> {
           |  ${decodeStep.shift(2).trim}
           |  ${callAndEncodeStep.shift(2).trim}
           |}""".stripMargin
      }.join("\n")

      q"""public static $svcTypeArg $wiringRetType invokeUeba$genericParam(
         |  $baboonMethodId method,
         |  byte[] data,
         |  ${svcName}$svcTypeArg impl,
         |  IBaboonServiceRt$rtTypeArg rt,
         |  ${ctxParamDecl}$baboonCodecContext ctx) {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default -> rt.fail(new $baboonWiringError.NoMatchingMethod(method));
         |  };
         |}""".stripMargin
    }
  }
}
