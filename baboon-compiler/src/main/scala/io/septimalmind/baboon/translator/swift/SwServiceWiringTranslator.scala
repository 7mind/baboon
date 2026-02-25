package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.CompilerTarget.SwTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.translator.swift.SwValue.SwType
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait SwServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[SwValue]]
  def translateServiceRt(domain: Domain): Option[TextTree[SwValue]]
}

object SwServiceWiringTranslator {
  class Impl(
    target: SwTarget,
    trans: SwTypeTranslator,
    codecs: Set[SwCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
  ) extends SwServiceWiringTranslator {

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "swift", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "swift", target.language.serviceContext, target.language.pragmas)

    private val hasJsonCodecs: Boolean = codecs.exists(_.id == "Json")
    private val hasUebaCodecs: Boolean = codecs.exists(_.id == "Ueba")

    private def jsonCodecName(typeId: TypeId.User): SwType = {
      val srcRef       = trans.toSwTypeRefKeepForeigns(typeId, domain, evo)
      val baseFileName = srcRef.importAs.getOrElse(trans.toSnakeCase(srcRef.name))
      SwType(srcRef.pkg, s"${srcRef.name}_JsonCodec", srcRef.fq, importAs = Some(baseFileName))
    }

    private def uebaCodecName(typeId: TypeId.User): SwType = {
      val srcRef       = trans.toSwTypeRefKeepForeigns(typeId, domain, evo)
      val baseFileName = srcRef.importAs.getOrElse(trans.toSnakeCase(srcRef.name))
      SwType(srcRef.pkg, s"${srcRef.name}_UebaCodec", srcRef.fq, importAs = Some(baseFileName))
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

    override def translateServiceRt(domain: Domain): Option[TextTree[SwValue]] = {
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

      val rtTrait: TextTree[SwValue] =
        q"""public protocol IBaboonServiceRt$traitTypeParams {
           |    func pure<L, R>(_ value: R) -> ${ct("L", "R")}
           |    func fail<L, R>(_ error: L) -> ${ct("L", "R")}
           |    func leftMap<A, B, C>(_ value: ${ct("A", "B")}, _ f: (A) -> C) -> ${ct("C", "B")}
           |    func flatMap<A, B, C>(_ value: ${ct("A", "B")}, _ f: (B) -> ${ct("A", "C")}) -> ${ct("A", "C")}
           |}""".stripMargin

      Some(rtTrait)
    }

    override def translate(defn: DomainMember.User): Option[TextTree[SwValue]] = {
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
      case ResolvedServiceContext.AbstractContext(tn, pn) => s"_ $pn: $tn, "
      case ResolvedServiceContext.ConcreteContext(tn, pn) => s"_ $pn: $tn, "
    }

    private def ctxArgPass: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext              => ""
      case ResolvedServiceContext.AbstractContext(_, pn) => s"$pn, "
      case ResolvedServiceContext.ConcreteContext(_, pn) => s"$pn, "
    }

    private def renderFq(tree: TextTree[SwValue]): String = tree.mapRender {
      case t: SwType             => if (t.predef) trans.escapeSwiftKeyword(t.name) else t.name
      case t: SwValue.SwTypeName => trans.escapeSwiftKeyword(t.name)
    }

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[SwValue] = {
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

      q"""public class ${svcName}Wiring {
         |    ${methods.shift(4).trim}
         |}""".stripMargin
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[SwValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])
              q"""let encoded = $outCodec.instance.encode(ctx, result)
                 |let jsonData = try JSONSerialization.data(withJSONObject: encoded, options: [.sortedKeys, .fragmentsAllowed])
                 |return String(data: jsonData, encoding: .utf8)!""".stripMargin
            case None =>
              q"""return "null" """
          }

          val callExpr = m.out match {
            case Some(_) => q"let result = impl.${m.name.name}(arg: ${ctxArgPass}decoded)"
            case None    => q"impl.${m.name.name}(arg: ${ctxArgPass}decoded)"
          }

          q""""${m.name.name}": {
             |    let wire = try JSONSerialization.jsonObject(with: data.data(using: .utf8)!, options: [.fragmentsAllowed])
             |    let decoded = try $inCodec.instance.decode(ctx, wire)
             |    $callExpr
             |    ${encodeOutput.shift(4).trim}
             |},""".stripMargin
      }.join("\n")

      q"""public static func invokeJson(
         |    _ method: $baboonMethodId,
         |    _ data: String,
         |    _ impl: $svcName,
         |    ${ctxParamDecl}_ ctx: $baboonCodecContext
         |) throws -> String {
         |    let handlers: [String: () throws -> String] = [
         |        ${cases.shift(8).trim}
         |    ]
         |    guard let handler = handlers[method.methodName] else {
         |        throw $baboonWiringException($baboonWiringError.noMatchingMethod(method))
         |    }
         |    return try handler()
         |}""".stripMargin
    }

    private def generateNoErrorsUebaMethod(service: Typedef.Service): TextTree[SwValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])
              q"""let writer = $baboonBinTools.createWriter()
                 |$outCodec.instance.encode(ctx, writer, result)
                 |return writer.toData()""".stripMargin
            case None =>
              q"return Data()"
          }

          val callExpr = m.out match {
            case Some(_) => q"let result = impl.${m.name.name}(arg: ${ctxArgPass}decoded)"
            case None    => q"impl.${m.name.name}(arg: ${ctxArgPass}decoded)"
          }

          q""""${m.name.name}": {
             |    let reader = $baboonBinTools.createReader(data)
             |    let decoded = try $inCodec.instance.decode(ctx, reader)
             |    $callExpr
             |    ${encodeOutput.shift(4).trim}
             |},""".stripMargin
      }.join("\n")

      q"""public static func invokeUeba(
         |    _ method: $baboonMethodId,
         |    _ data: Data,
         |    _ impl: $svcName,
         |    ${ctxParamDecl}_ ctx: $baboonCodecContext
         |) throws -> Data {
         |    let handlers: [String: () throws -> Data] = [
         |        ${cases.shift(8).trim}
         |    ]
         |    guard let handler = handlers[method.methodName] else {
         |        throw $baboonWiringException($baboonWiringError.noMatchingMethod(method))
         |    }
         |    return try handler()
         |}""".stripMargin
    }

    private def generateErrorsWiring(service: Typedef.Service): TextTree[SwValue] = {
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

      q"""public class ${svcName}Wiring {
         |    ${methods.shift(4).trim}
         |}""".stripMargin
    }

    private val bweFq: String = renderFq(q"$baboonWiringError")

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[SwValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "String")

      val cases = service.methods.map {
        m =>
          val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])
          val inRef   = trans.asSwRef(m.sig, domain, evo)

          val decodeStep =
            q"""var input: ${ct(bweFq, renderFq(inRef))}
               |do {
               |    let wire = try JSONSerialization.jsonObject(with: data.data(using: .utf8)!, options: [.fragmentsAllowed])
               |    input = rt.pure(try $inCodec.instance.decode(ctx, wire))
               |} catch {
               |    input = rt.fail($baboonWiringError.decoderFailed(method, error))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])

              val callBody = if (hasErrType) {
                q"""do {
                   |    let callResult = impl.${m.name.name}(arg: ${ctxArgPass}v)
                   |    return rt.leftMap(
                   |        callResult, { err in $baboonWiringError.callFailed(method, err) })
                   |} catch {
                   |    return rt.fail($baboonWiringError.callFailed(method, error))
                   |}""".stripMargin
              } else {
                q"""do {
                   |    return rt.pure(impl.${m.name.name}(arg: ${ctxArgPass}v))
                   |} catch {
                   |    return rt.fail($baboonWiringError.callFailed(method, error))
                   |}""".stripMargin
              }

              q"""let output = rt.flatMap(input) { v in
                 |    ${callBody.shift(4).trim}
                 |}
                 |return rt.flatMap(output) { v in
                 |    do {
                 |        let encoded = $outCodec.instance.encode(ctx, v)
                 |        let jsonData = try JSONSerialization.data(withJSONObject: encoded, options: [.sortedKeys, .fragmentsAllowed])
                 |        return rt.pure(String(data: jsonData, encoding: .utf8)!)
                 |    } catch {
                 |        return rt.fail($baboonWiringError.encoderFailed(method, error))
                 |    }
                 |}""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                q"""do {
                   |    let callResult = impl.${m.name.name}(arg: ${ctxArgPass}v)
                   |    return rt.leftMap(
                   |        callResult, { err in $baboonWiringError.callFailed(method, err) })
                   |} catch {
                   |    return rt.fail($baboonWiringError.callFailed(method, error))
                   |}""".stripMargin
              } else {
                q"""do {
                   |    impl.${m.name.name}(arg: ${ctxArgPass}v)
                   |    return rt.pure("null")
                   |} catch {
                   |    return rt.fail($baboonWiringError.callFailed(method, error))
                   |}""".stripMargin
              }

              q"""return rt.flatMap(input) { v in
                 |    ${callBody.shift(4).trim}
                 |}""".stripMargin
          }

          q""""${m.name.name}": {
             |    ${decodeStep.shift(4).trim}
             |    ${callAndEncodeStep.shift(4).trim}
             |},""".stripMargin
      }.join("\n")

      q"""public static func invokeJson(
         |    _ method: $baboonMethodId,
         |    _ data: String,
         |    _ impl: $svcName,
         |    _ rt: IBaboonServiceRt,
         |    ${ctxParamDecl}_ ctx: $baboonCodecContext
         |) -> $wiringRetType {
         |    let handlers: [String: () -> $wiringRetType] = [
         |        ${cases.shift(8).trim}
         |    ]
         |    guard let handler = handlers[method.methodName] else {
         |        return rt.fail($baboonWiringError.noMatchingMethod(method))
         |    }
         |    return handler()
         |}""".stripMargin
    }

    private def generateErrorsUebaMethod(service: Typedef.Service): TextTree[SwValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "Data")

      val cases = service.methods.map {
        m =>
          val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])
          val inRef   = trans.asSwRef(m.sig, domain, evo)

          val decodeStep =
            q"""var input: ${ct(bweFq, renderFq(inRef))}
               |do {
               |    let reader = $baboonBinTools.createReader(data)
               |    input = rt.pure(try $inCodec.instance.decode(ctx, reader))
               |} catch {
               |    input = rt.fail($baboonWiringError.decoderFailed(method, error))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])

              val callBody = if (hasErrType) {
                q"""do {
                   |    let callResult = impl.${m.name.name}(arg: ${ctxArgPass}v)
                   |    return rt.leftMap(
                   |        callResult, { err in $baboonWiringError.callFailed(method, err) })
                   |} catch {
                   |    return rt.fail($baboonWiringError.callFailed(method, error))
                   |}""".stripMargin
              } else {
                q"""do {
                   |    return rt.pure(impl.${m.name.name}(arg: ${ctxArgPass}v))
                   |} catch {
                   |    return rt.fail($baboonWiringError.callFailed(method, error))
                   |}""".stripMargin
              }

              q"""let output = rt.flatMap(input) { v in
                 |    ${callBody.shift(4).trim}
                 |}
                 |return rt.flatMap(output) { v in
                 |    do {
                 |        let writer = $baboonBinTools.createWriter()
                 |        $outCodec.instance.encode(ctx, writer, v)
                 |        return rt.pure(writer.toData())
                 |    } catch {
                 |        return rt.fail($baboonWiringError.encoderFailed(method, error))
                 |    }
                 |}""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                q"""do {
                   |    let callResult = impl.${m.name.name}(arg: ${ctxArgPass}v)
                   |    return rt.leftMap(
                   |        callResult, { err in $baboonWiringError.callFailed(method, err) })
                   |} catch {
                   |    return rt.fail($baboonWiringError.callFailed(method, error))
                   |}""".stripMargin
              } else {
                q"""do {
                   |    impl.${m.name.name}(arg: ${ctxArgPass}v)
                   |    return rt.pure(Data())
                   |} catch {
                   |    return rt.fail($baboonWiringError.callFailed(method, error))
                   |}""".stripMargin
              }

              q"""return rt.flatMap(input) { v in
                 |    ${callBody.shift(4).trim}
                 |}""".stripMargin
          }

          q""""${m.name.name}": {
             |    ${decodeStep.shift(4).trim}
             |    ${callAndEncodeStep.shift(4).trim}
             |},""".stripMargin
      }.join("\n")

      q"""public static func invokeUeba(
         |    _ method: $baboonMethodId,
         |    _ data: Data,
         |    _ impl: $svcName,
         |    _ rt: IBaboonServiceRt,
         |    ${ctxParamDecl}_ ctx: $baboonCodecContext
         |) -> $wiringRetType {
         |    let handlers: [String: () -> $wiringRetType] = [
         |        ${cases.shift(8).trim}
         |    ]
         |    guard let handler = handlers[method.methodName] else {
         |        return rt.fail($baboonWiringError.noMatchingMethod(method))
         |    }
         |    return handler()
         |}""".stripMargin
    }
  }
}
