package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait JvServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[JvValue]]
  def translateClient(defn: DomainMember.User): Option[TextTree[JvValue]]
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
      case u: TypeId.User => q"${jsonCodecName(u)}.INSTANCE.decode($codecCtxName, $wire)"
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
      case u: TypeId.User => q"${jsonCodecName(u)}.INSTANCE.encode($codecCtxName, $value)"
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
      case u: TypeId.User => q"${uebaCodecName(u)}.INSTANCE.decode($codecCtxName, $br)"
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
      case u: TypeId.User => q"${uebaCodecName(u)}.INSTANCE.encode($codecCtxName, $bw, $value);"
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

    // ========== Client stub generation ==========
    //
    // Emits a `${Svc}Client` class holding host-supplied transport
    // callbacks (one per active wire format) plus a baked
    // BaboonCodecContext. Each service method encodes its input to the
    // wire form, routes through the transport by (serviceName,
    // methodName), and decodes the response. When the Java asyncServices
    // flag is on, methods return CompletableFuture<T> and the transport
    // is the async (CompletableFuture-returning) variant; otherwise the
    // synchronous variant is used and methods return the bare type.

    private def isAsync: Boolean = target.language.asyncServices

    // In errors mode the service-result container F already models the effect,
    // so the impl call and the rt.flatMap/leftMap composition stay synchronous
    // over F (mirroring the C# backend). Under asyncServices the WHOLE wiring
    // return type is wrapped in CompletableFuture<…> for uniformity with the
    // async server-dispatch contract, and each terminal container value is
    // lifted with CompletableFuture.completedFuture(…).
    private def errorsWiringReturnType(inner: String): TextTree[JvValue] =
      if (isAsync) q"$completableFuture<$inner>" else q"$inner"
    private def errorsFutureWrap(expr: TextTree[JvValue]): TextTree[JvValue] =
      if (isAsync) q"$completableFuture.completedFuture($expr)" else expr

    override def translateClient(defn: DomainMember.User): Option[TextTree[JvValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val svcName = service.id.name.name
          val hasJson = hasActiveJsonCodecs(service)
          val hasUeba = hasActiveUebaCodecs(service)
          if (!hasJson && !hasUeba) return None

          val clientMethods = service.methods.flatMap {
            m =>
              val inRef = trans.asJvRef(m.sig, domain, evo)

              val uebaMethod = if (hasUeba) Some(generateClientUebaMethod(svcName, m, inRef)) else None
              val jsonMethod = if (hasJson) Some(generateClientJsonMethod(svcName, m, inRef)) else None
              uebaMethod.toList ++ jsonMethod.toList
          }

          if (clientMethods.isEmpty) return None

          // Codec-context field/param name: renamed away from `ctx` only when a
          // service context occupies that slot (the codec exprs reference
          // `codecCtxName`); `none`/`concrete` keep `ctx`, byte-identical.
          val codecCtxParam = codecCtxName

          val fields = List(
            if (hasUeba) Some(uebaTransportFieldDecl) else None,
            if (hasJson) Some(jsonTransportFieldDecl) else None,
            Some(q"private final $baboonCodecContext $codecCtxParam;"),
          ).flatten

          val ctorParams = List(
            if (hasUeba) Some(uebaTransportParam("transportUeba")) else None,
            if (hasJson) Some(jsonTransportParam("transportJson")) else None,
            Some(q"$baboonCodecContext $codecCtxParam"),
          ).flatten

          val ctorAssigns = List(
            if (hasUeba) Some(q"this.transportUeba = transportUeba;") else None,
            if (hasJson) Some(q"this.transportJson = transportJson;") else None,
            Some(q"this.$codecCtxParam = $codecCtxParam;"),
          ).flatten

          // Convenience constructor defaulting ctx to BaboonCodecContext.Default.
          val convCtorParams = List(
            if (hasUeba) Some(uebaTransportParam("transportUeba")) else None,
            if (hasJson) Some(jsonTransportParam("transportJson")) else None,
          ).flatten
          val convCtorArgs = List(
            if (hasUeba) Some(q"transportUeba") else None,
            if (hasJson) Some(q"transportJson") else None,
            Some(q"$baboonCodecContext.Default"),
          ).flatten

          val convCtor =
            q"""public ${svcName}Client(${convCtorParams.join(", ")}) {
               |  this(${convCtorArgs.join(", ")});
               |}""".stripMargin

          Some(
            q"""public final class ${svcName}Client$ctxClassTypeParam {
               |  ${fields.join("\n").shift(2).trim}
               |
               |  public ${svcName}Client(${ctorParams.join(", ")}) {
               |    ${ctorAssigns.join("\n").shift(4).trim}
               |  }
               |
               |  ${convCtor.shift(2).trim}
               |
               |  ${clientMethods.join("\n\n").shift(2).trim}
               |}""".stripMargin
          )
        case _ => None
      }
    }

    // When a service context is active (abstract OR concrete) the transport
    // callbacks carry a leading service-context argument (forwarded by the
    // client) and use the Ctx-generic functional interfaces; the
    // service-context type is bound as their `<…>` type-arg. In `none` mode the
    // historical context-free transports are emitted byte-for-byte.
    private def ctxTransportTypeArg: String = svcCtxTypeName.fold("")(tn => s"<$tn>")
    private def uebaTransportType: TextTree[JvValue] =
      if (ctxOrConcreteActive) (if (isAsync) q"$baboonClientTransportUebaAsyncCtx$ctxTransportTypeArg" else q"$baboonClientTransportUebaSyncCtx$ctxTransportTypeArg")
      else if (isAsync) q"$baboonClientTransportUebaAsync" else q"$baboonClientTransportUebaSync"
    private def jsonTransportType: TextTree[JvValue] =
      if (ctxOrConcreteActive) (if (isAsync) q"$baboonClientTransportJsonAsyncCtx$ctxTransportTypeArg" else q"$baboonClientTransportJsonSyncCtx$ctxTransportTypeArg")
      else if (isAsync) q"$baboonClientTransportJsonAsync" else q"$baboonClientTransportJsonSync"
    private def uebaTransportFieldDecl: TextTree[JvValue] = q"private final $uebaTransportType transportUeba;"
    private def jsonTransportFieldDecl: TextTree[JvValue] = q"private final $jsonTransportType transportJson;"
    private def uebaTransportParam(name: String): TextTree[JvValue] = q"$uebaTransportType $name"
    private def jsonTransportParam(name: String): TextTree[JvValue] = q"$jsonTransportType $name"

    // Boxed return element type for a method (used inside CompletableFuture<...>
    // and as the sync return type). void output maps to Void/void respectively.
    private def clientReturnType(out: Option[TypeRef]): TextTree[JvValue] = {
      out match {
        case Some(outRef) =>
          val rendered = trans.asJvRef(outRef, domain, evo)
          if (isAsync) q"$completableFuture<$rendered>" else rendered
        case None =>
          if (isAsync) q"$completableFuture<Void>" else q"void"
      }
    }

    private def generateClientUebaMethod(
      svcName: String,
      m: Typedef.MethodDef,
      inRef: TextTree[JvValue],
    ): TextTree[JvValue] = {
      val retType    = clientReturnType(m.out)
      val methodName = m.name.name
      val encodeIn   = uebaEncodeStmt(m.sig.id.asInstanceOf[TypeId.Scalar], q"bw", q"arg")

      // Encode argument into a byte[] payload.
      val encodeBlock =
        q"""var oms = new $byteArrayOutputStream();
           |var bw = new $binaryOutput(oms);
           |$encodeIn
           |bw.flush();
           |byte[] payload = oms.toByteArray();""".stripMargin

      if (isAsync) {
        val decodeBody = m.out match {
          case Some(outRef) =>
            val decodeExpr = uebaDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"br")
            q"""var ims = new $byteArrayInputStream(resp);
               |var br = new $binaryInput(ims);
               |try {
               |  return $decodeExpr;
               |} catch (Exception ex) {
               |  throw new RuntimeException(ex);
               |}""".stripMargin
          case None => q"return null;"
        }
        // The synchronous UEBA encode block (binary-output writes + flush)
        // throws checked IOException; the sync variant already declares
        // `throws Exception`, so the async variant must too (the encode runs
        // synchronously before the returned CompletableFuture).
        q"""public $retType $methodName($ctxParamDecl$inRef arg) throws Exception {
           |  ${encodeBlock.shift(2).trim}
           |  return transportUeba.apply(${ctxArgPass}"$svcName", "$methodName", payload).thenApply(resp -> {
           |    ${decodeBody.shift(4).trim}
           |  });
           |}""".stripMargin
      } else {
        val decodeBody = m.out match {
          case Some(outRef) =>
            val decodeExpr = uebaDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"br")
            q"""var ims = new $byteArrayInputStream(resp);
               |var br = new $binaryInput(ims);
               |return $decodeExpr;""".stripMargin
          case None => q""
        }
        q"""public $retType $methodName($ctxParamDecl$inRef arg) throws Exception {
           |  ${encodeBlock.shift(2).trim}
           |  byte[] resp = transportUeba.apply(${ctxArgPass}"$svcName", "$methodName", payload);
           |  ${decodeBody.shift(2).trim}
           |}""".stripMargin
      }
    }

    private def generateClientJsonMethod(
      svcName: String,
      m: Typedef.MethodDef,
      inRef: TextTree[JvValue],
    ): TextTree[JvValue] = {
      val retType    = clientReturnType(m.out)
      val methodName = s"${m.name.name}Json"
      val routeName  = m.name.name
      val encodeIn   = jsonEncodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"arg")

      val encodeBlock = q"String payload = $encodeIn.toString();"

      if (isAsync) {
        val decodeBody = m.out match {
          case Some(outRef) =>
            val decodeExpr = jsonDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"wire")
            q"""try {
               |  var mapper = new $objectMapper();
               |  $jsonNode wire = mapper.readTree(resp);
               |  return $decodeExpr;
               |} catch (Exception ex) {
               |  throw new RuntimeException(ex);
               |}""".stripMargin
          case None => q"return null;"
        }
        q"""public $retType $methodName($ctxParamDecl$inRef arg) {
           |  $encodeBlock
           |  return transportJson.apply(${ctxArgPass}"$svcName", "$routeName", payload).thenApply(resp -> {
           |    ${decodeBody.shift(4).trim}
           |  });
           |}""".stripMargin
      } else {
        val decodeBody = m.out match {
          case Some(outRef) =>
            val decodeExpr = jsonDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"wire")
            q"""var mapper = new $objectMapper();
               |$jsonNode wire = mapper.readTree(resp);
               |return $decodeExpr;""".stripMargin
          case None => q""
        }
        q"""public $retType $methodName($ctxParamDecl$inRef arg) throws Exception {
           |  $encodeBlock
           |  String resp = transportJson.apply(${ctxArgPass}"$svcName", "$routeName", payload);
           |  ${decodeBody.shift(2).trim}
           |}""".stripMargin
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

    // The service-context type name (`abstract` introduces a generic param,
    // `type` pins a concrete type) and its parameter name. The codec context is
    // renamed away from the service-context parameter name (default `ctx`) to
    // avoid the duplicate-parameter collision; in `none` mode it stays `ctx`,
    // keeping that output byte-identical.
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
    // The codec-context parameter name. Renamed only when a service context is
    // present (it occupies the default `ctx` slot); `none` mode keeps `ctx`.
    private def codecCtxName: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext => "ctx"
      case _                                => if (svcCtxArgName.contains("codecCtx")) "baboonCodecCtx" else "codecCtx"
    }
    // Only the abstract service-context generic, as a `<Ctx>` clause; empty for
    // none/concrete. Used to make the client class generic.
    private def ctxClassTypeParam: String = resolvedCtx match {
      case ResolvedServiceContext.AbstractContext(tn, _) => s"<$tn>"
      case _                                             => ""
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

    private def ctxActive: Boolean = resolvedCtx match {
      case ResolvedServiceContext.AbstractContext(_, _) => true
      case _                                            => false
    }
    // True for both `abstract` and `type` (concrete) service-context modes — any
    // mode where the service context must be threaded per-invoke (`none` is
    // false, keeping its output byte-identical).
    private def ctxOrConcreteActive: Boolean = resolvedCtx match {
      case ResolvedServiceContext.NoContext => false
      case _                                => true
    }

    // Generic clause placed BEFORE the return type of the static `invoke*`
    // method. When the abstract service context is active, the full declaration
    // (HKT signature + `Ctx`) belongs here. When it is NOT active the historical
    // output is preserved byte-for-byte: the leading clause stays `svcTypeArg`
    // (just the HKT name) and the declaration form continues to appear after the
    // method name via [[staticGenericAfterName]].
    private def staticGenericDecl: String   = if (ctxActive) genericParam else svcTypeArg
    private def staticGenericAfterName: String = if (ctxActive) "" else genericParam

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
      val svcName    = service.id.name.name
      val returnType = if (isAsync) q"$completableFuture<String>" else q"String"
      val cases = service.methods.map {
        m =>
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          if (isAsync) {
            // The impl call returns CompletableFuture<Out> (or
            // CompletableFuture<Void> for void output); compose decode →
            // impl.method(...) → encode via thenApply so the whole arm yields a
            // CompletableFuture<String>. No synchronous impl.method() call.
            val composed = m.out match {
              case Some(outRef) =>
                val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"result")
                q"""yield impl.${m.name.name}(${ctxArgPass}decoded).thenApply(result -> {
                   |  var encoded = $encodeOut;
                   |  return encoded.toString();
                   |});""".stripMargin
              case None =>
                q"""yield impl.${m.name.name}(${ctxArgPass}decoded).thenApply(ignored -> "null");"""
            }

            q"""case "${m.name.name}" -> {
               |  var mapper = new $objectMapper();
               |  $jsonNode wire = mapper.readTree(data);
               |  var decoded = $decodeIn;
               |  ${composed.shift(2).trim}
               |}""".stripMargin
          } else {
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
          }
      }.join("\n")

      q"""public static $staticGenericDecl $returnType invokeJson$staticGenericAfterName(
         |  $baboonMethodId method,
         |  String data,
         |  $svcName$svcTypeArg impl,
         |  $ctxParamDecl$baboonCodecContext $codecCtxName) throws Exception {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default ->
         |      throw new $baboonWiringException(new $baboonWiringError.NoMatchingMethod(method));
         |  };
         |}""".stripMargin
    }

    private def generateNoErrorsUebaMethod(service: Typedef.Service): TextTree[JvValue] = {
      val svcName    = service.id.name.name
      val returnType = if (isAsync) q"$completableFuture<byte[]>" else q"byte[]"
      val cases = service.methods.map {
        m =>
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"br")

          if (isAsync) {
            // The impl call returns CompletableFuture<Out> (or
            // CompletableFuture<Void> for void output); compose decode →
            // impl.method(...) → encode via thenApply so the whole arm yields a
            // CompletableFuture<byte[]>. No synchronous impl.method() call. The
            // UEBA encode path throws checked IOException, so the thenApply
            // lambda wraps it (CompletableFuture lambdas cannot throw checked).
            val composed = m.out match {
              case Some(outRef) =>
                val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"bw", q"result")
                q"""yield impl.${m.name.name}(${ctxArgPass}decoded).thenApply(result -> {
                   |  try {
                   |    var oms = new $byteArrayOutputStream();
                   |    var bw = new $binaryOutput(oms);
                   |    $encStmt
                   |    bw.flush();
                   |    return oms.toByteArray();
                   |  } catch (Exception ex) {
                   |    throw new RuntimeException(ex);
                   |  }
                   |});""".stripMargin
              case None =>
                q"""yield impl.${m.name.name}(${ctxArgPass}decoded).thenApply(ignored -> new byte[0]);"""
            }

            q"""case "${m.name.name}" -> {
               |  var ims = new $byteArrayInputStream(data);
               |  var br = new $binaryInput(ims);
               |  var decoded = $decodeIn;
               |  ${composed.shift(2).trim}
               |}""".stripMargin
          } else {
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
          }
      }.join("\n")

      q"""public static $staticGenericDecl $returnType invokeUeba$staticGenericAfterName(
         |  $baboonMethodId method,
         |  byte[] data,
         |  $svcName$svcTypeArg impl,
         |  $ctxParamDecl$baboonCodecContext $codecCtxName) throws Exception {
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

              val containerExpr =
                q"""rt.flatMap(output, v -> {
                   |  try {
                   |    var encoded = $encodeOut;
                   |    return rt.pure(encoded.toString());
                   |  } catch (Throwable ex) {
                   |    return rt.fail(new $baboonWiringError.EncoderFailed(method, ex));
                   |  }
                   |})""".stripMargin
              q"""var output = rt.flatMap(input, v -> {
                 |  ${callBody.shift(2).trim}
                 |});
                 |yield ${errorsFutureWrap(containerExpr)};""".stripMargin

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

              val containerExpr =
                q"""rt.flatMap(input, v -> {
                   |  ${callBody.shift(2).trim}
                   |  return rt.pure("null");
                   |})""".stripMargin
              q"""yield ${errorsFutureWrap(containerExpr)};""".stripMargin
          }

          q"""case "${m.name.name}" -> {
             |  ${decodeStep.shift(2).trim}
             |  ${callAndEncodeStep.shift(2).trim}
             |}""".stripMargin
      }.join("\n")

      q"""public static $staticGenericDecl ${errorsWiringReturnType(wiringRetType)} invokeJson$staticGenericAfterName(
         |  $baboonMethodId method,
         |  String data,
         |  $svcName$svcTypeArg impl,
         |  IBaboonServiceRt$rtTypeArg rt,
         |  $ctxParamDecl$baboonCodecContext $codecCtxName) {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default -> ${errorsFutureWrap(q"rt.fail(new $baboonWiringError.NoMatchingMethod(method))")};
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

              val containerExpr =
                q"""rt.flatMap(output, v -> {
                   |  try {
                   |    var oms = new $byteArrayOutputStream();
                   |    var bw = new $binaryOutput(oms);
                   |    $encStmt
                   |    bw.flush();
                   |    return rt.pure(oms.toByteArray());
                   |  } catch (Throwable ex) {
                   |    return rt.fail(new $baboonWiringError.EncoderFailed(method, ex));
                   |  }
                   |})""".stripMargin
              q"""var output = rt.flatMap(input, v -> {
                 |  ${callBody.shift(2).trim}
                 |});
                 |yield ${errorsFutureWrap(containerExpr)};""".stripMargin

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

              val containerExpr =
                q"""rt.flatMap(input, v -> {
                   |  ${callBody.shift(2).trim}
                   |  return rt.pure(new byte[0]);
                   |})""".stripMargin
              q"""yield ${errorsFutureWrap(containerExpr)};""".stripMargin
          }

          q"""case "${m.name.name}" -> {
             |  ${decodeStep.shift(2).trim}
             |  ${callAndEncodeStep.shift(2).trim}
             |}""".stripMargin
      }.join("\n")

      q"""public static $staticGenericDecl ${errorsWiringReturnType(wiringRetType)} invokeUeba$staticGenericAfterName(
         |  $baboonMethodId method,
         |  byte[] data,
         |  $svcName$svcTypeArg impl,
         |  IBaboonServiceRt$rtTypeArg rt,
         |  $ctxParamDecl$baboonCodecContext $codecCtxName) {
         |  return switch (method.methodName()) {
         |    ${cases.shift(4).trim}
         |    default -> ${errorsFutureWrap(q"rt.fail(new $baboonWiringError.NoMatchingMethod(method))")};
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
    // Java has no native sync/async polymorphism via type-only mechanisms.
    // The --jv-async-services flag controls whether R is wrapped in CompletableFuture.
    // In sync mode (default), R is either the bare wire type (String/byte[]) in noErrors
    // mode or the service-result container in errors mode. In async mode, R is wrapped
    // as CompletableFuture<R>, and both the client interface (invokeJson/invokeUeba
    // method returns) and server dispatch logic (wraps handler calls in CompletableFuture)
    // use async. Nesting the wrappers inside ${Svc}Wiring (rather than emitting them as
    // top-level files) keeps the emission shape compatible with the existing
    // one-output-per-service dispatcher and matches Java's "helpers nested under the
    // public class" idiom.

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
      // When a service context is active the wrapper implements the
      // context-carrying contract and receives the service context PER-INVOKE
      // (not via the constructor). In `none` mode the historical context-free
      // contract is used, byte-for-byte.
      val ifaceType   =
        if (ctxOrConcreteActive) (if (isJson) iBaboonJsonServiceCtx else iBaboonUebaServiceCtx)
        else (if (isJson) iBaboonJsonService else iBaboonUebaService)
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
      // Under asyncServices the static invoker returns CompletableFuture<…>, so
      // R is wrapped to match (keeping the wrapper/server-dispatch contract
      // consistent with the async invoker shape).
      val rType: String = {
        val inner = if (!isErrorsMode) wireType else renderContainer(bweFq, wireType)
        if (isAsync) s"${renderFq(q"$completableFuture")}<$inner>" else inner
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

      // The service context is no longer baked at construction time — it is
      // supplied per `invoke`. `rt` (errors mode) remains a constructor field.
      val extras: List[(String, String)] = List(rtField).flatten

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

      // Forward args to the static invoker: (method, data, impl, [rt,] [svcCtx,] codecCtx).
      // The service context (when active) and the codec context are now both
      // `invoke` parameters rather than constructor fields.
      val invokerArgs: String = {
        val base       = List("method", "data", "this.impl")
        val withRt     = rtField.fold(base)(_ => base :+ "this.rt")
        val withSvcCtx = svcCtxArgName.fold(withRt)(pn => withRt :+ pn)
        (withSvcCtx :+ codecCtxName).mkString(", ")
      }

      // The invoke method on noErrors-mode static methods declares `throws
      // Exception` — propagate that. Errors mode returns the container and
      // does not throw.
      val throwsClause = if (isErrorsMode) "" else " throws Exception"

      // The implemented contract's type args: context-carrying contract is
      // `<SvcCtx, R>` (abstract or concrete); the context-free contract is `<R>`.
      val ifaceTypeArgs: String = svcCtxTypeName match {
        case Some(tn) => s"<$tn, $rType>"
        case None     => s"<$rType>"
      }

      // The `invoke` parameter list: the service context (when active) leads,
      // followed by the codec context (renamed away from `ctx` when it would
      // collide). In `none` mode this is `(method, data, ctx)`, unchanged.
      val invokeCtxParam: String = svcCtxTypeName match {
        case Some(tn) => s"$tn ${svcCtxArgName.get}, "
        case None     => ""
      }

      // The wrapper is a nested class inside ${Svc}Wiring; call the enclosing
      // class's static method directly. Type inference picks F/C from impl.
      q"""public static final class $wrapperName$classTypeParams implements ${ifaceType}$ifaceTypeArgs {
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
         |  public $rType invoke($baboonMethodId method, $wireType data, $invokeCtxParam$baboonCodecContext $codecCtxName)$throwsClause {
         |    return $invokerFn($invokerArgs);
         |  }
         |}""".stripMargin
    }
  }
}
