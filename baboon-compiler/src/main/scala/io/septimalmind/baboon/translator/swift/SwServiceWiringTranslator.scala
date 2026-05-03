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

    private def hasActiveJsonCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.id == "Json" && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def hasActiveUebaCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.id == "Ueba" && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

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

    // JSON encode/decode for both User types (via generated codec) and BuiltinScalar (inline).
    // Swift wiring JSON: wire is Any (from JSONSerialization.jsonObject), encode returns Any.
    private def jsonDecodeExpr(id: TypeId, wire: TextTree[SwValue]): TextTree[SwValue] = id match {
      case u: TypeId.User => q"try ${jsonCodecName(u)}.instance.decode(ctx, $wire)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$wire as! Bool"
          case TypeId.Builtins.i08   => q"Int8(truncatingIfNeeded: ($wire as! NSNumber).intValue)"
          case TypeId.Builtins.i16   => q"Int16(truncatingIfNeeded: ($wire as! NSNumber).intValue)"
          case TypeId.Builtins.i32   => q"Int32(truncatingIfNeeded: ($wire as! NSNumber).intValue)"
          case TypeId.Builtins.i64   => q"($wire is String ? Int64($wire as! String)! : Int64(truncatingIfNeeded: ($wire as! NSNumber).int64Value))"
          case TypeId.Builtins.u08   => q"UInt8(truncatingIfNeeded: ($wire as! NSNumber).intValue)"
          case TypeId.Builtins.u16   => q"UInt16(truncatingIfNeeded: ($wire as! NSNumber).intValue)"
          case TypeId.Builtins.u32   => q"UInt32(truncatingIfNeeded: ($wire as! NSNumber).intValue)"
          case TypeId.Builtins.u64   => q"($wire is String ? UInt64($wire as! String)! : UInt64(truncatingIfNeeded: ($wire as! NSNumber).uint64Value))"
          case TypeId.Builtins.f32   => q"Float(($wire as! NSNumber).doubleValue)"
          case TypeId.Builtins.f64   => q"($wire as! NSNumber).doubleValue"
          case TypeId.Builtins.f128  => q"$baboonDecimal($wire is String ? $wire as! String : String(describing: $wire))"
          case TypeId.Builtins.str   => q"($wire as! String)"
          case TypeId.Builtins.uid   => q"UUID(uuidString: $wire as! String)!"
          case TypeId.Builtins.bytes => q"$baboonByteStringTools.fromHexString($wire as! String)"
          case TypeId.Builtins.tsu   => q"$baboonTimeFormats.parseUtc($wire as! String)"
          case TypeId.Builtins.tso   => q"$baboonTimeFormats.parseOffset($wire as! String)"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def jsonEncodeExpr(id: TypeId, value: TextTree[SwValue]): TextTree[SwValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.instance.encode(ctx, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                                             => q"$value"
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 => q"Int($value)"
          case TypeId.Builtins.i64                                             => q"String($value)"
          case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 => q"Int($value)"
          case TypeId.Builtins.u64                                             => q"String($value)"
          case TypeId.Builtins.f32                                             => q"Double($value)"
          case TypeId.Builtins.f64                                             => q"$value"
          case TypeId.Builtins.f128                                            => q"$value.stringValue"
          case TypeId.Builtins.str                                             => q"$value"
          case TypeId.Builtins.uid                                             => q"$value.uuidString"
          case TypeId.Builtins.bytes                                           => q"$baboonByteStringTools.toHexString($value)"
          case TypeId.Builtins.tsu                                             => q"$baboonTimeFormats.formatUtc($value)"
          case TypeId.Builtins.tso                                             => q"$baboonTimeFormats.formatOffset($value)"
          case other                                                           => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaDecodeExpr(id: TypeId, reader: TextTree[SwValue]): TextTree[SwValue] = id match {
      case u: TypeId.User => q"try ${uebaCodecName(u)}.instance.decode(ctx, $reader)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$reader.readBool()"
          case TypeId.Builtins.i08   => q"$reader.readI8()"
          case TypeId.Builtins.i16   => q"$reader.readI16()"
          case TypeId.Builtins.i32   => q"$reader.readI32()"
          case TypeId.Builtins.i64   => q"$reader.readI64()"
          case TypeId.Builtins.u08   => q"$reader.readU8()"
          case TypeId.Builtins.u16   => q"$reader.readU16()"
          case TypeId.Builtins.u32   => q"$reader.readU32()"
          case TypeId.Builtins.u64   => q"$reader.readU64()"
          case TypeId.Builtins.f32   => q"$reader.readF32()"
          case TypeId.Builtins.f64   => q"$reader.readF64()"
          case TypeId.Builtins.f128  => q"$reader.readDecimal()"
          case TypeId.Builtins.str   => q"try $reader.readString()"
          case TypeId.Builtins.bytes => q"try $reader.readBytes()"
          case TypeId.Builtins.uid   => q"try $reader.readUuid()"
          case TypeId.Builtins.tsu   => q"$reader.readTsu()"
          case TypeId.Builtins.tso   => q"$reader.readTso()"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaEncodeStmt(id: TypeId, writer: TextTree[SwValue], value: TextTree[SwValue]): TextTree[SwValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.encode(ctx, $writer, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$writer.writeBool($value)"
          case TypeId.Builtins.i08   => q"$writer.writeI8($value)"
          case TypeId.Builtins.i16   => q"$writer.writeI16($value)"
          case TypeId.Builtins.i32   => q"$writer.writeI32($value)"
          case TypeId.Builtins.i64   => q"$writer.writeI64($value)"
          case TypeId.Builtins.u08   => q"$writer.writeU8($value)"
          case TypeId.Builtins.u16   => q"$writer.writeU16($value)"
          case TypeId.Builtins.u32   => q"$writer.writeU32($value)"
          case TypeId.Builtins.u64   => q"$writer.writeU64($value)"
          case TypeId.Builtins.f32   => q"$writer.writeF32($value)"
          case TypeId.Builtins.f64   => q"$writer.writeF64($value)"
          case TypeId.Builtins.f128  => q"$writer.writeDecimal($value)"
          case TypeId.Builtins.str   => q"$writer.writeString($value)"
          case TypeId.Builtins.bytes => q"$writer.writeBytes($value)"
          case TypeId.Builtins.uid   => q"$writer.writeUuid($value)"
          case TypeId.Builtins.tsu   => q"$writer.writeTsu($value)"
          case TypeId.Builtins.tso   => q"$writer.writeTso($value)"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
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
        if (hasActiveJsonCodecs(service))
          Some(generateNoErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasActiveUebaCodecs(service))
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
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"result")
              q"""let encoded = $encodeOut
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
             |    let decoded = $decodeIn
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
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"reader")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"writer", q"result")
              q"""let writer = $baboonBinTools.createWriter()
                 |$encStmt
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
             |    let decoded = $decodeIn
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
        if (hasActiveJsonCodecs(service))
          Some(generateErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasActiveUebaCodecs(service))
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
          val inRef    = trans.asSwRef(m.sig, domain, evo)
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          val decodeStep =
            q"""var input: ${ct(bweFq, renderFq(inRef))}
               |do {
               |    let wire = try JSONSerialization.jsonObject(with: data.data(using: .utf8)!, options: [.fragmentsAllowed])
               |    input = rt.pure($decodeIn)
               |} catch {
               |    input = rt.fail($baboonWiringError.decoderFailed(method, error))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"v")

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
                 |        let encoded = $encodeOut
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
          val inRef    = trans.asSwRef(m.sig, domain, evo)
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"reader")

          val decodeStep =
            q"""var input: ${ct(bweFq, renderFq(inRef))}
               |do {
               |    let reader = $baboonBinTools.createReader(data)
               |    input = rt.pure($decodeIn)
               |} catch {
               |    input = rt.fail($baboonWiringError.decoderFailed(method, error))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"writer", q"v")

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
                 |        $encStmt
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
