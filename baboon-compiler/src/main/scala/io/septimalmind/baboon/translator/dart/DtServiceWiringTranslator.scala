package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.CompilerTarget.DtTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait DtServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[DtValue]]
  def translateServiceRt(domain: Domain): Option[TextTree[DtValue]]
}

object DtServiceWiringTranslator {
  class Impl(
    target: DtTarget,
    trans: DtTypeTranslator,
    codecs: Set[DtCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
  ) extends DtServiceWiringTranslator {

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "dart", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "dart", target.language.serviceContext, target.language.pragmas)

    private val hasJsonCodecs: Boolean = codecs.exists(_.id == "Json")
    private val hasUebaCodecs: Boolean = codecs.exists(_.id == "Ueba")

    private def jsonCodecName(typeId: TypeId.User): DtValue.DtType = {
      val srcRef       = trans.toDtTypeRefKeepForeigns(typeId, domain, evo)
      val baseFileName = srcRef.importAs.getOrElse(trans.toSnakeCase(srcRef.name))
      DtValue.DtType(srcRef.pkg, s"${srcRef.name}_JsonCodec", srcRef.fq, importAs = Some(baseFileName))
    }

    private def uebaCodecName(typeId: TypeId.User): DtValue.DtType = {
      val srcRef       = trans.toDtTypeRefKeepForeigns(typeId, domain, evo)
      val baseFileName = srcRef.importAs.getOrElse(trans.toSnakeCase(srcRef.name))
      DtValue.DtType(srcRef.pkg, s"${srcRef.name}_UebaCodec", srcRef.fq, importAs = Some(baseFileName))
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

    override def translateServiceRt(domain: Domain): Option[TextTree[DtValue]] = {
      if (resolved.noErrors) return None

      val hasServices = domain.defs.meta.nodes.values.exists {
        case DomainMember.User(_, _: Typedef.Service, _, _) => true
        case _                                              => false
      }
      if (!hasServices) return None

      val ct = renderContainer _

      val traitTypeParams = resolved.hkt match {
        case Some(h) => s"<${h.name}${h.signature}>"
        case None    => ""
      }

      val rtTrait: TextTree[DtValue] =
        q"""abstract class IBaboonServiceRt$traitTypeParams {
           |  ${ct("L", "R")} pure<L, R>(R value);
           |  ${ct("L", "R")} fail<L, R>(L error);
           |  ${ct("C", "B")} leftMap<A, B, C>(${ct("A", "B")} value, C Function(A) f);
           |  ${ct("A", "C")} flatMap<A, B, C>(${ct("A", "B")} value, ${ct("A", "C")} Function(B) f);
           |}""".stripMargin

      Some(rtTrait)
    }

    override def translate(defn: DomainMember.User): Option[TextTree[DtValue]] = {
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

    private def renderFq(tree: TextTree[DtValue]): String = tree.mapRender {
      case t: DtValue.DtType     => if (t.predef) trans.escapeDartKeyword(t.name) else (t.pkg.parts :+ trans.escapeDartKeyword(t.name)).mkString(".")
      case t: DtValue.DtTypeName => trans.escapeDartKeyword(t.name)
    }

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[DtValue] = {
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

      q"""class ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}""".stripMargin
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[DtValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])
              q"""final encoded = $outCodec.instance.encode(ctx, result);
                 |return jsonEncode(encoded);""".stripMargin
            case None =>
              q"""return 'null';"""
          }

          val callExpr = m.out match {
            case Some(_) => q"final result = impl.${m.name.name}(${ctxArgPass}decoded);"
            case None    => q"impl.${m.name.name}(${ctxArgPass}decoded);"
          }

          q"""'${m.name.name}': () {
             |  final wire = jsonDecode(data);
             |  final decoded = $inCodec.instance.decode(ctx, wire);
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}
             |},""".stripMargin
      }.join("\n")

      q"""static String invokeJson(
         |  $baboonMethodId method,
         |  String data,
         |  $svcName impl,
         |  $ctxParamDecl$baboonCodecContext ctx) {
         |  final handlers = <String, String Function()>{
         |    ${cases.shift(4).trim}
         |  };
         |  final handler = handlers[method.methodName];
         |  if (handler == null) {
         |    throw $baboonWiringException($baboonWiringError.noMatchingMethod(method));
         |  }
         |  return handler();
         |}""".stripMargin
    }

    private def generateNoErrorsUebaMethod(service: Typedef.Service): TextTree[DtValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])
              q"""final writer = $baboonBinTools.createWriter();
                 |$outCodec.instance.encode(ctx, writer, result);
                 |return writer.toBytes();""".stripMargin
            case None =>
              q"return $dtUint8List(0);"
          }

          val callExpr = m.out match {
            case Some(_) => q"final result = impl.${m.name.name}(${ctxArgPass}decoded);"
            case None    => q"impl.${m.name.name}(${ctxArgPass}decoded);"
          }

          q"""'${m.name.name}': () {
             |  final reader = $baboonBinTools.createReader(data);
             |  final decoded = $inCodec.instance.decode(ctx, reader);
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}
             |},""".stripMargin
      }.join("\n")

      q"""static $dtUint8List invokeUeba(
         |  $baboonMethodId method,
         |  $dtUint8List data,
         |  $svcName impl,
         |  $ctxParamDecl$baboonCodecContext ctx) {
         |  final handlers = <String, $dtUint8List Function()>{
         |    ${cases.shift(4).trim}
         |  };
         |  final handler = handlers[method.methodName];
         |  if (handler == null) {
         |    throw $baboonWiringException($baboonWiringError.noMatchingMethod(method));
         |  }
         |  return handler();
         |}""".stripMargin
    }

    private def generateErrorsWiring(service: Typedef.Service): TextTree[DtValue] = {
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

      q"""class ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}""".stripMargin
    }

    private val bweFq: String = renderFq(q"$baboonWiringError")

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[DtValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "String")

      val cases = service.methods.map {
        m =>
          val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])
          val inRef   = trans.asDtRef(m.sig, domain, evo)

          val decodeStep =
            q"""${ct(bweFq, renderFq(inRef))} input;
               |try {
               |  final wire = jsonDecode(data);
               |  input = rt.pure($inCodec.instance.decode(ctx, wire));
               |} catch (ex) {
               |  input = rt.fail($baboonWiringError.decoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])

              val callBody = if (hasErrType) {
                q"""try {
                   |  final callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  return rt.pure(impl.${m.name.name}(${ctxArgPass}v));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              }

              q"""final output = rt.flatMap(input, (v) {
                 |  ${callBody.shift(2).trim}
                 |});
                 |return rt.flatMap(output, (v) {
                 |  try {
                 |    final encoded = $outCodec.instance.encode(ctx, v);
                 |    return rt.pure(jsonEncode(encoded));
                 |  } catch (ex) {
                 |    return rt.fail($baboonWiringError.encoderFailed(method, ex));
                 |  }
                 |});""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                q"""try {
                   |  final callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  impl.${m.name.name}(${ctxArgPass}v);
                   |  return rt.pure(null);
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              }

              q"""return rt.flatMap(input, (v) {
                 |  ${callBody.shift(2).trim}
                 |  return rt.pure('null');
                 |});""".stripMargin
          }

          q"""'${m.name.name}': () {
             |  ${decodeStep.shift(2).trim}
             |  ${callAndEncodeStep.shift(2).trim}
             |},""".stripMargin
      }.join("\n")

      q"""static $wiringRetType invokeJson(
         |  $baboonMethodId method,
         |  String data,
         |  $svcName impl,
         |  IBaboonServiceRt rt,
         |  $ctxParamDecl$baboonCodecContext ctx) {
         |  final handlers = <String, $wiringRetType Function()>{
         |    ${cases.shift(4).trim}
         |  };
         |  final handler = handlers[method.methodName];
         |  if (handler == null) {
         |    return rt.fail($baboonWiringError.noMatchingMethod(method));
         |  }
         |  return handler();
         |}""".stripMargin
    }

    private def generateErrorsUebaMethod(service: Typedef.Service): TextTree[DtValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "Uint8List")

      val cases = service.methods.map {
        m =>
          val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])
          val inRef   = trans.asDtRef(m.sig, domain, evo)

          val decodeStep =
            q"""${ct(bweFq, renderFq(inRef))} input;
               |try {
               |  final reader = $baboonBinTools.createReader(data);
               |  input = rt.pure($inCodec.instance.decode(ctx, reader));
               |} catch (ex) {
               |  input = rt.fail($baboonWiringError.decoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])

              val callBody = if (hasErrType) {
                q"""try {
                   |  final callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  return rt.pure(impl.${m.name.name}(${ctxArgPass}v));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              }

              q"""final output = rt.flatMap(input, (v) {
                 |  ${callBody.shift(2).trim}
                 |});
                 |return rt.flatMap(output, (v) {
                 |  try {
                 |    final writer = $baboonBinTools.createWriter();
                 |    $outCodec.instance.encode(ctx, writer, v);
                 |    return rt.pure(writer.toBytes());
                 |  } catch (ex) {
                 |    return rt.fail($baboonWiringError.encoderFailed(method, ex));
                 |  }
                 |});""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                q"""try {
                   |  final callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  impl.${m.name.name}(${ctxArgPass}v);
                   |  return rt.pure(null);
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              }

              q"""return rt.flatMap(input, (v) {
                 |  ${callBody.shift(2).trim}
                 |  return rt.pure($dtUint8List(0));
                 |});""".stripMargin
          }

          q"""'${m.name.name}': () {
             |  ${decodeStep.shift(2).trim}
             |  ${callAndEncodeStep.shift(2).trim}
             |},""".stripMargin
      }.join("\n")

      q"""static $wiringRetType invokeUeba(
         |  $baboonMethodId method,
         |  $dtUint8List data,
         |  $svcName impl,
         |  IBaboonServiceRt rt,
         |  $ctxParamDecl$baboonCodecContext ctx) {
         |  final handlers = <String, $wiringRetType Function()>{
         |    ${cases.shift(4).trim}
         |  };
         |  final handler = handlers[method.methodName];
         |  if (handler == null) {
         |    return rt.fail($baboonWiringError.noMatchingMethod(method));
         |  }
         |  return handler();
         |}""".stripMargin
    }
  }
}
