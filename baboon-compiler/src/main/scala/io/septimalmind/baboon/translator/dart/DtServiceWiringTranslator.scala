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

    // JSON encode/decode for both User types (via generated codec) and BuiltinScalar (inline).
    // wire is a Dart dynamic (json-decoded), encode returns a Dart dynamic.
    private def jsonDecodeExpr(id: TypeId, wire: TextTree[DtValue]): TextTree[DtValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.instance.decode(ctx, $wire)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                                             => q"$wire as bool"
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 => q"($wire as num).toInt()"
          case TypeId.Builtins.i64                                             => q"($wire is String ? int.parse($wire as String) : ($wire as num).toInt())"
          case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 => q"($wire as num).toInt()"
          case TypeId.Builtins.u64                       => q"($wire is String ? BigInt.parse($wire as String).toSigned(64).toInt() : ($wire as num).toInt())"
          case TypeId.Builtins.f32 | TypeId.Builtins.f64 => q"($wire as num).toDouble()"
          case TypeId.Builtins.f128                      => q"$baboonDecimal($wire is String ? $wire as String : $wire.toString())"
          case TypeId.Builtins.str                       => q"$wire as String"
          case TypeId.Builtins.uid                       => q"$wire as String"
          case TypeId.Builtins.bytes                     => q"$baboonByteStringTools.fromHexString($wire as String)"
          case TypeId.Builtins.tsu                       => q"$baboonTimeFormats.parseUtc($wire as String)"
          case TypeId.Builtins.tso                       => q"$baboonTimeFormats.parseOffset($wire as String)"
          case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def jsonEncodeExpr(id: TypeId, value: TextTree[DtValue]): TextTree[DtValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.instance.encode(ctx, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                                             => q"$value"
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 => q"$value"
          case TypeId.Builtins.i64                                             => q"$value.toString()"
          case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 => q"$value"
          case TypeId.Builtins.u64                                             => q"BigInt.from($value).toUnsigned(64).toString()"
          case TypeId.Builtins.f32 | TypeId.Builtins.f64                       => q"$value"
          case TypeId.Builtins.f128                                            => q"$value.value"
          case TypeId.Builtins.str                                             => q"$value"
          case TypeId.Builtins.uid                                             => q"$value"
          case TypeId.Builtins.bytes                                           => q"$value.toHexString()"
          case TypeId.Builtins.tsu                                             => q"$baboonTimeFormats.formatUtc($value)"
          case TypeId.Builtins.tso                                             => q"$baboonTimeFormats.formatOffset($value)"
          case other                                                           => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaDecodeExpr(id: TypeId, reader: TextTree[DtValue]): TextTree[DtValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.decode(ctx, $reader)"
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
          case TypeId.Builtins.str   => q"$reader.readString()"
          case TypeId.Builtins.bytes => q"$reader.readBytes()"
          case TypeId.Builtins.uid   => q"$reader.readUuid()"
          case TypeId.Builtins.tsu   => q"$reader.readTsu()"
          case TypeId.Builtins.tso   => q"$reader.readTso()"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaEncodeStmt(id: TypeId, writer: TextTree[DtValue], value: TextTree[DtValue]): TextTree[DtValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.encode(ctx, $writer, $value);"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$writer.writeBool($value);"
          case TypeId.Builtins.i08   => q"$writer.writeI8($value);"
          case TypeId.Builtins.i16   => q"$writer.writeI16($value);"
          case TypeId.Builtins.i32   => q"$writer.writeI32($value);"
          case TypeId.Builtins.i64   => q"$writer.writeI64($value);"
          case TypeId.Builtins.u08   => q"$writer.writeU8($value);"
          case TypeId.Builtins.u16   => q"$writer.writeU16($value);"
          case TypeId.Builtins.u32   => q"$writer.writeU32($value);"
          case TypeId.Builtins.u64   => q"$writer.writeU64($value);"
          case TypeId.Builtins.f32   => q"$writer.writeF32($value);"
          case TypeId.Builtins.f64   => q"$writer.writeF64($value);"
          case TypeId.Builtins.f128  => q"$writer.writeDecimal($value);"
          case TypeId.Builtins.str   => q"$writer.writeString($value);"
          case TypeId.Builtins.bytes => q"$writer.writeBytes($value);"
          case TypeId.Builtins.uid   => q"$writer.writeUuid($value);"
          case TypeId.Builtins.tsu   => q"$writer.writeTsu($value);"
          case TypeId.Builtins.tso   => q"$writer.writeTso($value);"
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
        if (hasActiveJsonCodecs(service))
          Some(generateNoErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasActiveUebaCodecs(service))
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
          val decodeIn = jsonDecodeExpr(m.sig.id, q"wire")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encodeOut = jsonEncodeExpr(outRef.id, q"result")
              q"""final encoded = $encodeOut;
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
             |  final decoded = $decodeIn;
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
          val decodeIn = uebaDecodeExpr(m.sig.id, q"reader")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id, q"writer", q"result")
              q"""final writer = $baboonBinTools.createWriter();
                 |$encStmt
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
             |  final decoded = $decodeIn;
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
        if (hasActiveJsonCodecs(service))
          Some(generateErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasActiveUebaCodecs(service))
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
          val inRef    = trans.asDtRef(m.sig, domain, evo)
          val decodeIn = jsonDecodeExpr(m.sig.id, q"wire")

          val decodeStep =
            q"""${ct(bweFq, renderFq(inRef))} input;
               |try {
               |  final wire = jsonDecode(data);
               |  input = rt.pure($decodeIn);
               |} catch (ex) {
               |  input = rt.fail($baboonWiringError.decoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val encodeOut = jsonEncodeExpr(outRef.id, q"v")

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
                 |    final encoded = $encodeOut;
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
          val inRef    = trans.asDtRef(m.sig, domain, evo)
          val decodeIn = uebaDecodeExpr(m.sig.id, q"reader")

          val decodeStep =
            q"""${ct(bweFq, renderFq(inRef))} input;
               |try {
               |  final reader = $baboonBinTools.createReader(data);
               |  input = rt.pure($decodeIn);
               |} catch (ex) {
               |  input = rt.fail($baboonWiringError.decoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id, q"writer", q"v")

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
                 |    $encStmt
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
