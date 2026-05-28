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

    private def hasActiveJsonCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[JvJsonCodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def hasActiveUebaCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[JvUEBACodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private val resultJvType: Option[JvValue.JvType] = resolved.resultType.map {
      rt =>
        JvValue.JvType(baboonRuntimePkg, rt)
    }

    private def jsonCodecName(typeId: TypeId.User): JvValue.JvType = {
      val srcRef      = trans.toJvTypeRefKeepForeigns(typeId, domain, evo)
      val domainPkg   = trans.toJvPkg(domain.id, domain.version, evo)
      val ownerPrefix = srcRef.pkg.parts.toSeq.drop(domainPkg.parts.toSeq.length)
      val prefixStr   = if (ownerPrefix.nonEmpty) ownerPrefix.mkString("_") + "_" else ""
      val realPkg     = trans.effectiveJvPkg(typeId.owner, domain, evo)
      JvValue.JvType(realPkg, s"$prefixStr${srcRef.name}_JsonCodec", srcRef.fq)
    }

    private def uebaCodecName(typeId: TypeId.User): JvValue.JvType = {
      val srcRef      = trans.toJvTypeRefKeepForeigns(typeId, domain, evo)
      val domainPkg   = trans.toJvPkg(domain.id, domain.version, evo)
      val ownerPrefix = srcRef.pkg.parts.toSeq.drop(domainPkg.parts.toSeq.length)
      val prefixStr   = if (ownerPrefix.nonEmpty) ownerPrefix.mkString("_") + "_" else ""
      val realPkg     = trans.effectiveJvPkg(typeId.owner, domain, evo)
      JvValue.JvType(realPkg, s"$prefixStr${srcRef.name}_UEBACodec", srcRef.fq)
    }

    // JSON encode/decode for both User types (via generated codec) and BuiltinScalar (inline).
    private def jsonDecodeExpr(id: TypeId, wire: TextTree[JvValue]): TextTree[JvValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.INSTANCE.decode(ctx, $wire)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$wire.booleanValue()"
          case TypeId.Builtins.i08   => q"(byte) $wire.intValue()"
          case TypeId.Builtins.i16   => q"(short) $wire.intValue()"
          case TypeId.Builtins.i32   => q"$wire.intValue()"
          case TypeId.Builtins.i64   => q"($wire.isTextual() ? Long.parseLong($wire.textValue()) : $wire.longValue())"
          case TypeId.Builtins.u08   => q"(short) $wire.intValue()"
          case TypeId.Builtins.u16   => q"$wire.intValue()"
          case TypeId.Builtins.u32   => q"$wire.longValue()"
          case TypeId.Builtins.u64   => q"($wire.isTextual() ? Long.parseUnsignedLong($wire.textValue()) : $wire.longValue())"
          case TypeId.Builtins.f32   => q"(float) $wire.doubleValue()"
          case TypeId.Builtins.f64   => q"$wire.doubleValue()"
          case TypeId.Builtins.f128  => q"($wire.isTextual() ? new $jvBigDecimal($wire.textValue()) : $wire.decimalValue())"
          case TypeId.Builtins.str   => q"$wire.textValue()"
          case TypeId.Builtins.bytes => q"$jvByteString.fromHex($wire.textValue())"
          case TypeId.Builtins.uid   => q"$jvUid.fromString($wire.textValue())"
          case TypeId.Builtins.tsu   => q"$baboonTimeFormats.parseTsu($wire.textValue())"
          case TypeId.Builtins.tso   => q"$baboonTimeFormats.parseTso($wire.textValue())"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def jsonEncodeExpr(id: TypeId, value: TextTree[JvValue]): TextTree[JvValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.INSTANCE.encode(ctx, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.uid   => q"new $textNode($value.toString())"
          case TypeId.Builtins.tsu   => q"new $textNode($baboonTimeFormats.formatTsu($value))"
          case TypeId.Builtins.tso   => q"new $textNode($baboonTimeFormats.formatTso($value))"
          case TypeId.Builtins.bit   => q"$booleanNode.valueOf($value)"
          case TypeId.Builtins.i08   => q"$shortNode.valueOf((short) $value)"
          case TypeId.Builtins.i16   => q"$shortNode.valueOf($value)"
          case TypeId.Builtins.i32   => q"$intNode.valueOf($value)"
          case TypeId.Builtins.i64   => q"$longNode.valueOf($value)"
          case TypeId.Builtins.u08   => q"$shortNode.valueOf($value)"
          case TypeId.Builtins.u16   => q"$intNode.valueOf($value)"
          case TypeId.Builtins.u32   => q"$longNode.valueOf($value)"
          case TypeId.Builtins.u64   => q"new $textNode(Long.toUnsignedString($value))"
          case TypeId.Builtins.f32   => q"$floatNode.valueOf($value)"
          case TypeId.Builtins.f64   => q"$doubleNode.valueOf($value)"
          case TypeId.Builtins.f128  => q"new $textNode($value.toPlainString())"
          case TypeId.Builtins.str   => q"new $textNode($value)"
          case TypeId.Builtins.bytes => q"new $textNode($value.toHex())"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaDecodeExpr(id: TypeId, br: TextTree[JvValue]): TextTree[JvValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.INSTANCE.decode(ctx, $br)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                       => q"$br.readByte() != 0"
          case TypeId.Builtins.i08                       => q"$br.readByte()"
          case TypeId.Builtins.i16                       => q"$br.readShort()"
          case TypeId.Builtins.i32                       => q"$br.readInt()"
          case TypeId.Builtins.i64                       => q"$br.readLong()"
          case TypeId.Builtins.u08                       => q"(short) ($br.readByte() & 0xFF)"
          case TypeId.Builtins.u16                       => q"($br.readShort() & 0xFFFF)"
          case TypeId.Builtins.u32                       => q"($br.readInt() & 0xFFFFFFFFL)"
          case TypeId.Builtins.u64                       => q"$br.readLong()"
          case TypeId.Builtins.f32                       => q"$br.readFloat()"
          case TypeId.Builtins.f64                       => q"$br.readDouble()"
          case TypeId.Builtins.f128                      => q"$baboonBinTools.readBigDecimal($br)"
          case TypeId.Builtins.str                       => q"$baboonBinTools.readString($br)"
          case TypeId.Builtins.bytes                     => q"$baboonBinTools.readByteString($br)"
          case TypeId.Builtins.uid                       => q"$baboonBinTools.readUid($br)"
          case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonBinTools.readTimestamp($br)"
          case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaEncodeStmt(id: TypeId, bw: TextTree[JvValue], value: TextTree[JvValue]): TextTree[JvValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.INSTANCE.encode(ctx, $bw, $value);"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                       => q"$bw.writeByte($value ? 1 : 0);"
          case TypeId.Builtins.i08                       => q"$bw.writeByte($value);"
          case TypeId.Builtins.i16                       => q"$bw.writeShort($value);"
          case TypeId.Builtins.i32                       => q"$bw.writeInt($value);"
          case TypeId.Builtins.i64                       => q"$bw.writeLong($value);"
          case TypeId.Builtins.u08                       => q"$bw.writeByte((byte) ($value & 0xFF));"
          case TypeId.Builtins.u16                       => q"$bw.writeShort((short) ($value & 0xFFFF));"
          case TypeId.Builtins.u32                       => q"$bw.writeInt((int) ($value & 0xFFFFFFFFL));"
          case TypeId.Builtins.u64                       => q"$bw.writeLong($value);"
          case TypeId.Builtins.f32                       => q"$bw.writeFloat($value);"
          case TypeId.Builtins.f64                       => q"$bw.writeDouble($value);"
          case TypeId.Builtins.f128                      => q"$baboonBinTools.writeBigDecimal($bw, $value);"
          case TypeId.Builtins.str                       => q"$baboonBinTools.writeString($bw, $value);"
          case TypeId.Builtins.bytes                     => q"$baboonBinTools.writeByteString($bw, $value);"
          case TypeId.Builtins.uid                       => q"$baboonBinTools.writeUid($bw, $value);"
          case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonBinTools.writeTimestamp($bw, $value);"
          case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
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

    override def translateServiceRt(domain: Domain): Option[TextTree[JvValue]] = {
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
          case _                                             => None
        },
      ).flatten
      if (params.nonEmpty) params.mkString("<", ", ", ">") else ""
    }

    private def svcTypeArg: String = {
      val params = Seq(
        resolved.hkt.map(_.name),
        resolvedCtx match {
          case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
          case _                                             => None
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

      val jsonMethod =
        if (hasActiveJsonCodecs(service))
          Some(generateNoErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasActiveUebaCodecs(service))
          Some(generateNoErrorsUebaMethod(service))
        else None

      val wrappers = generateServiceWrappers(
        service,
        isErrorsMode = false,
        jsonActive = hasActiveJsonCodecs(service),
        uebaActive = hasActiveUebaCodecs(service),
      )

      val parts   = Seq(jsonMethod, uebaMethod).flatten ++ wrappers.toSeq
      val methods = parts.join("\n\n")

      q"""public final class ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}""".stripMargin
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[JvValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"result")
              q"""var encoded = $encodeOut;
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
             |  var decoded = $decodeIn;
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}
             |}""".stripMargin
      }.join("\n")

      q"""public static $svcTypeArg String invokeJson$genericParam(
         |  $baboonMethodId method,
         |  String data,
         |  $svcName$svcTypeArg impl,
         |  $ctxParamDecl$baboonCodecContext ctx) throws Exception {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default ->
         |      throw new $baboonWiringException(new $baboonWiringError.NoMatchingMethod(method));
         |  };
         |}""".stripMargin
    }

    private def generateNoErrorsUebaMethod(service: Typedef.Service): TextTree[JvValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"br")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"bw", q"result")
              q"""var oms = new $byteArrayOutputStream();
                 |var bw = new $binaryOutput(oms);
                 |$encStmt
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
             |  var decoded = $decodeIn;
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}
             |}""".stripMargin
      }.join("\n")

      q"""public static $svcTypeArg byte[] invokeUeba$genericParam(
         |  $baboonMethodId method,
         |  byte[] data,
         |  $svcName$svcTypeArg impl,
         |  $ctxParamDecl$baboonCodecContext ctx) throws Exception {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default ->
         |      throw new $baboonWiringException(new $baboonWiringError.NoMatchingMethod(method));
         |  };
         |}""".stripMargin
    }

    private def generateErrorsWiring(service: Typedef.Service): TextTree[JvValue] = {
      val svcName = service.id.name.name

      val jsonMethod =
        if (hasActiveJsonCodecs(service))
          Some(generateErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasActiveUebaCodecs(service))
          Some(generateErrorsUebaMethod(service))
        else None

      val wrappers = generateServiceWrappers(
        service,
        isErrorsMode = true,
        jsonActive = hasActiveJsonCodecs(service),
        uebaActive = hasActiveUebaCodecs(service),
      )

      val parts   = Seq(jsonMethod, uebaMethod).flatten ++ wrappers.toSeq
      val methods = parts.join("\n\n")

      q"""public final class ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}""".stripMargin
    }

    private val bweFq: String = renderFq(q"$baboonWiringError")

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[JvValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "String")

      val cases = service.methods.map {
        m =>
          val inRef    = trans.asJvRef(m.sig, domain, evo)
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          val decodeStep =
            q"""${ct(bweFq, renderFq(inRef))} input;
               |try {
               |  var mapper = new $objectMapper();
               |  $jsonNode wire = mapper.readTree(data);
               |  input = rt.pure($decodeIn);
               |} catch (Throwable ex) {
               |  input = rt.fail(new $baboonWiringError.DecoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType   = trans.asJvRef(outRef, domain, evo)
              val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"v")

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
                 |yield rt.flatMap(output, v -> {
                 |  try {
                 |    var encoded = $encodeOut;
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

              q"""yield rt.flatMap(input, v -> {
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
         |  $svcName$svcTypeArg impl,
         |  IBaboonServiceRt$rtTypeArg rt,
         |  $ctxParamDecl$baboonCodecContext ctx) {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default -> rt.fail(new $baboonWiringError.NoMatchingMethod(method));
         |  };
         |}""".stripMargin
    }

    private def generateErrorsUebaMethod(service: Typedef.Service): TextTree[JvValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "byte[]")

      val cases = service.methods.map {
        m =>
          val inRef    = trans.asJvRef(m.sig, domain, evo)
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"br")

          val decodeStep =
            q"""${ct(bweFq, renderFq(inRef))} input;
               |try {
               |  var ims = new $byteArrayInputStream(data);
               |  var br = new $binaryInput(ims);
               |  input = rt.pure($decodeIn);
               |} catch (Throwable ex) {
               |  input = rt.fail(new $baboonWiringError.DecoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType = trans.asJvRef(outRef, domain, evo)
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"bw", q"v")

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
                 |yield rt.flatMap(output, v -> {
                 |  try {
                 |    var oms = new $byteArrayOutputStream();
                 |    var bw = new $binaryOutput(oms);
                 |    $encStmt
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

              q"""yield rt.flatMap(input, v -> {
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
         |  $svcName$svcTypeArg impl,
         |  IBaboonServiceRt$rtTypeArg rt,
         |  $ctxParamDecl$baboonCodecContext ctx) {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default -> rt.fail(new $baboonWiringError.NoMatchingMethod(method));
         |  };
         |}""".stripMargin
    }

    // ========== Cross-domain muxer-entry wrapper classes ==========
    //
    // Each service emits nested `JsonService` and `UebaService` classes that
    // implement `IBaboonJsonService<R>` / `IBaboonUebaService<R>` with R
    // matching the underlying invokeJson/invokeUeba return shape. Extra
    // dependencies the static invoker consumes (`rt` for errors mode, any
    // abstract/concrete service-context parameter) are baked at construction
    // time so the runtime contract stays uniform.
    //
    // Java has no native sync/async polymorphism via type-only mechanisms;
    // the current Java backend has no asyncServices flag. R is therefore
    // either the bare wire type (String/byte[]) in noErrors mode or the
    // service-result container in errors mode. Nesting the wrappers inside
    // ${Svc}Wiring (rather than emitting them as top-level files) keeps the
    // emission shape compatible with the existing one-output-per-service
    // dispatcher and matches Java's "helpers nested under the public class"
    // idiom.

    private def generateServiceWrappers(
      service: Typedef.Service,
      isErrorsMode: Boolean,
      jsonActive: Boolean,
      uebaActive: Boolean,
    ): Option[TextTree[JvValue]] = {
      if (!jsonActive && !uebaActive) return None
      val jsonWrapper = if (jsonActive) Some(generateOneWrapper(service, isJson = true, isErrorsMode = isErrorsMode))  else None
      val uebaWrapper = if (uebaActive) Some(generateOneWrapper(service, isJson = false, isErrorsMode = isErrorsMode)) else None
      val joined = Seq(jsonWrapper, uebaWrapper).flatten.join("\n\n")
      Some(joined)
    }

    private def generateOneWrapper(
      service: Typedef.Service,
      isJson: Boolean,
      isErrorsMode: Boolean,
    ): TextTree[JvValue] = {
      val svcName     = service.id.name.name
      val wireType    = if (isJson) "String" else "byte[]"
      val invokerFn   = if (isJson) "invokeJson" else "invokeUeba"
      val ifaceType   = if (isJson) iBaboonJsonService else iBaboonUebaService
      val wrapperName = if (isJson) "JsonService" else "UebaService"

      // Type parameters for the wrapper class. Mirrors `genericParam` from
      // the static invoker: in errors mode the HKT F (if present); in any
      // mode the abstract service-context type C (if present). These names
      // are stable across noErrors/errors so the wrapper R type can refer to
      // them.
      val classTypeParams: String = {
        val params = Seq(
          if (isErrorsMode) resolved.hkt.map(h => s"${h.name}${h.signature}") else None,
          resolvedCtx match {
            case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
            case _                                             => None
          },
        ).flatten
        if (params.nonEmpty) params.mkString("<", ", ", ">") else ""
      }

      // The type-arg list passed to the impl's interface type and the static
      // invoker, mirrors `svcTypeArg`.
      val svcImplTypeArg: String = {
        val params = Seq(
          if (isErrorsMode) resolved.hkt.map(_.name) else None,
          resolvedCtx match {
            case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
            case _                                             => None
          },
        ).flatten
        if (params.nonEmpty) params.mkString("<", ", ", ">") else ""
      }

      // R for IBaboon*Service<R>. In noErrors mode it's just the wire type.
      // In errors mode it's the service-result container wrapping the wire
      // type, e.g. `BaboonEither<BaboonWiringError, String>` or `F<…, …>`.
      val rType: String = {
        if (!isErrorsMode) wireType
        else renderContainer(bweFq, wireType)
      }

      // Extra deps to bake at construction time. `rt` only in errors mode.
      // Service context (abstract or concrete) in any mode when configured.
      val rtField: Option[(String, String)] =
        if (isErrorsMode) {
          val rtTypeArgLocal = resolved.hkt match {
            case Some(h) => s"<${h.name}>"
            case None    => ""
          }
          Some(("rt", s"IBaboonServiceRt$rtTypeArgLocal"))
        } else None

      val svcCtxField: Option[(String, String)] = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => None
        case ResolvedServiceContext.AbstractContext(tn, pn) => Some((pn, tn))
        case ResolvedServiceContext.ConcreteContext(tn, pn) => Some((pn, tn))
      }

      val extras: List[(String, String)] = List(rtField, svcCtxField).flatten

      val implFieldDecl: TextTree[JvValue] = q"private final $svcName$svcImplTypeArg impl;"
      val extraFieldDecls: Seq[TextTree[JvValue]] = extras.map { case (n, t) => q"private final $t $n;" }
      val allFieldDecls: TextTree[JvValue]        = (implFieldDecl +: extraFieldDecls).join("\n")

      val ctorParams: String = {
        val all = s"$svcName$svcImplTypeArg impl" :: extras.map { case (n, t) => s"$t $n" }
        all.mkString(", ")
      }
      val ctorAssigns: TextTree[JvValue] = {
        val all = q"this.impl = impl;" +: extras.map { case (n, _) => q"this.$n = $n;" }
        all.join("\n")
      }

      // Forward args to the static invoker: (method, data, impl, [rt,] [svcCtx,] ctx)
      val invokerArgs: String = {
        val base = List("method", "data", "this.impl")
        val withRt = rtField.fold(base)(_ => base :+ "this.rt")
        val withSvcCtx = svcCtxField.fold(withRt)(f => withRt :+ s"this.${f._1}")
        (withSvcCtx :+ "ctx").mkString(", ")
      }

      // The invoke method on noErrors-mode static methods declares `throws
      // Exception` — propagate that. Errors mode returns the container and
      // does not throw.
      val throwsClause = if (isErrorsMode) "" else " throws Exception"

      // The wrapper is a nested class inside ${Svc}Wiring; call the enclosing
      // class's static method directly. Type inference picks F/C from impl.
      q"""public static final class $wrapperName$classTypeParams implements ${ifaceType}<$rType> {
         |  ${allFieldDecls.shift(2).trim}
         |
         |  public $wrapperName($ctorParams) {
         |    ${ctorAssigns.shift(4).trim}
         |  }
         |
         |  @Override
         |  public String serviceName() {
         |    return "$svcName";
         |  }
         |
         |  @Override
         |  public $rType invoke($baboonMethodId method, $wireType data, $baboonCodecContext ctx)$throwsClause {
         |    return $invokerFn($invokerArgs);
         |  }
         |}""".stripMargin
    }
  }
}
