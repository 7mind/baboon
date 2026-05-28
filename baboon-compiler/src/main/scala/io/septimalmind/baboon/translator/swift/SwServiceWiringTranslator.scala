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

    /** The fully scoped Swift type for the service protocol, e.g.
      * `notifications.Push` for a namespaced service or `AdminService` for a
      * top-level one. Used as the `impl` parameter / field type in the
      * generated `<Svc>Wiring.invoke{Json,Ueba}` static functions and in the
      * cross-domain wrapper classes — they live outside any `extension`
      * scope, so the bare local name `service.id.name.name` would fail to
      * resolve.
      */
    private def serviceImplType(service: Typedef.Service): TextTree[SwValue] =
      q"${trans.toSwTypeRefKeepForeigns(service.id, domain, evo)}"

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

      val wrappers = generateServiceWrappers(
        service,
        jsonRetType = q"String",
        uebaRetType = q"Data",
      )

      q"""public class ${svcName}Wiring {
         |    ${methods.shift(4).trim}
         |}
         |
         |$wrappers""".stripMargin
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[SwValue] = {
      val implType = serviceImplType(service)
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
         |    _ impl: $implType,
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
      val implType = serviceImplType(service)
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
         |    _ impl: $implType,
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

      val jsonRet: TextTree[SwValue] = q"${SwValue.SwTypeName(ct(bweFq, "String"))}"
      val uebaRet: TextTree[SwValue] = q"${SwValue.SwTypeName(ct(bweFq, "Data"))}"

      val wrappers = generateServiceWrappers(service, jsonRet, uebaRet)

      q"""public class ${svcName}Wiring {
         |    ${methods.shift(4).trim}
         |}
         |
         |$wrappers""".stripMargin
    }

    private val bweFq: String = renderFq(q"$baboonWiringError")

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[SwValue] = {
      val implType      = serviceImplType(service)
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
         |    _ impl: $implType,
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
      val implType      = serviceImplType(service)
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
         |    _ impl: $implType,
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

    /** Emits the cross-domain Muxer-entry wrapper classes for a service.
      *
      * Each wrapper conforms to `IBaboonJsonService` / `IBaboonUebaService`
      * with `associatedtype R` bound to the underlying
      * `<Svc>Wiring.invokeJson` / `invokeUeba` return type (raw `String`/
      * `Data` in noErrors mode, container-wrapped in errors mode). Extra
      * dependencies (`rt` for the errors mode, and any service-context
      * parameter) are baked at construction time so the runtime contract
      * `func invoke(method, data, ctx) throws -> R` stays
      * codec-flavour-symmetric.
      *
      * In errors mode the underlying `<Svc>Wiring.invokeJson` is
      * non-throwing — Swift permits a non-throwing call to satisfy a
      * `throws` protocol requirement, so the wrapper's `invoke` can declare
      * `throws` uniformly across modes.
      */
    private def generateServiceWrappers(
      service: Typedef.Service,
      jsonRetType: TextTree[SwValue],
      uebaRetType: TextTree[SwValue],
    ): TextTree[SwValue] = {
      val jsonWrapper =
        if (hasActiveJsonCodecs(service)) Some(generateOneWrapper(service, isJson = true, jsonRetType))
        else None
      val uebaWrapper =
        if (hasActiveUebaCodecs(service)) Some(generateOneWrapper(service, isJson = false, uebaRetType))
        else None
      Seq(jsonWrapper, uebaWrapper).flatten.join("\n\n")
    }

    private def generateOneWrapper(
      service: Typedef.Service,
      isJson: Boolean,
      retType: TextTree[SwValue],
    ): TextTree[SwValue] = {
      val svcName     = service.id.name.name
      val implType    = serviceImplType(service)
      val wrapperName = s"$svcName${if (isJson) "JsonService" else "UebaService"}"
      val wireType    = if (isJson) q"String" else q"Data"
      val invokerCall = if (isJson) q"${SwValue.SwTypeName(s"${svcName}Wiring")}.invokeJson" else q"${SwValue.SwTypeName(s"${svcName}Wiring")}.invokeUeba"
      val ifaceType   = if (isJson) ibaboonJsonService else ibaboonUebaService

      // Extra deps consumed by <Svc>Wiring.invoke{Json,Ueba}_X: rt in errors
      // mode and any service-context parameter. Baked at construction time so
      // the runtime IBaboon*Service contract stays uniform across modes.
      val rtField: Option[(String, TextTree[SwValue])] =
        if (resolved.noErrors) None else Some(("rt", q"IBaboonServiceRt"))

      val svcCtxField: Option[(String, TextTree[SwValue])] = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => None
        case ResolvedServiceContext.AbstractContext(tn, pn) => Some((pn, q"${SwValue.SwTypeName(tn)}"))
        case ResolvedServiceContext.ConcreteContext(tn, pn) => Some((pn, q"${SwValue.SwTypeName(tn)}"))
      }

      val extraFields: List[(String, TextTree[SwValue])] = List(rtField, svcCtxField).flatten

      val implFieldDecl: TextTree[SwValue] = q"private let impl: $implType"
      val extraFieldDecls: List[TextTree[SwValue]] = extraFields.map {
        case (name, tpe) => q"private let $name: $tpe"
      }

      val ctorParamList: TextTree[SwValue] = {
        val all = q"_ impl: $implType" :: extraFields.map { case (n, t) => q"$n: $t" }
        all.join(", ")
      }

      val ctorAssigns: List[TextTree[SwValue]] =
        q"self.impl = impl" :: extraFields.map { case (n, _) => q"self.$n = $n" }

      // Build the call to the underlying static wiring function. Argument order matches
      // generateNoErrors{Json,Ueba}Method / generateErrors{Json,Ueba}Method.
      val callArgs: List[TextTree[SwValue]] = {
        val base   = List[TextTree[SwValue]](q"method", q"data", q"self.impl")
        val withRt = rtField.fold(base)(_ => base :+ q"self.rt")
        val withSvcCtx = svcCtxField.fold(withRt)(f => withRt :+ q"self.${f._1}")
        withSvcCtx :+ q"ctx"
      }

      val callExpr: TextTree[SwValue] =
        if (resolved.noErrors) q"try $invokerCall(${callArgs.join(", ")})"
        else q"$invokerCall(${callArgs.join(", ")})"

      q"""public class $wrapperName: $ifaceType {
         |    public typealias R = $retType
         |    public let serviceName: String = "$svcName"
         |    $implFieldDecl
         |    ${extraFieldDecls.join("\n").shift(4).trim}
         |
         |    public init($ctorParamList) {
         |        ${ctorAssigns.join("\n").shift(8).trim}
         |    }
         |
         |    public func invoke(_ method: $baboonMethodId, _ data: $wireType, _ ctx: $baboonCodecContext) throws -> R {
         |        return $callExpr
         |    }
         |}""".stripMargin
    }
  }
}
