package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.CompilerTarget.KtTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.kotlin.KtTypes.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait KtServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[KtValue]]

  def translateClient(defn: DomainMember.User): Option[TextTree[KtValue]]

  def translateServiceRt(domain: Domain): Option[TextTree[KtValue]]
}

object KtServiceWiringTranslator {
  class Impl(
    target: KtTarget,
    trans: KtTypeTranslator,
    codecs: Set[KtCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
    ktTypes: KtTypes,
  ) extends KtServiceWiringTranslator {
    import ktTypes.*

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "kotlin", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "kotlin", target.language.serviceContext, target.language.pragmas)

    private def hasActiveJsonCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[KtJsonCodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def hasActiveUebaCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[KtUEBACodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private val resultKtType: Option[KtValue.KtType] = resolved.resultType.map {
      rt =>
        KtValue.KtType(baboonRuntimePkg, rt)
    }

    // Helper: create binary writer and get bytes
    private def mkWriterSetup(writerVar: String): TextTree[KtValue] = {
      if (ktTypes.multiplatform) {
        q"val $writerVar = $binaryOutput()"
      } else {
        q"""val ${writerVar}_stream = $byteArrayOutputStream()
           |val $writerVar = $binaryOutput(${writerVar}_stream)""".stripMargin
      }
    }

    private def mkWriterGetBytes(writerVar: String): TextTree[KtValue] = {
      if (ktTypes.multiplatform) q"$writerVar.toByteArray()"
      else q"${writerVar}_stream.toByteArray()"
    }

    // Helper: create binary reader from bytes
    private def mkReaderSetup(readerVar: String, dataExpr: String): TextTree[KtValue] = {
      if (ktTypes.multiplatform) {
        q"val $readerVar = $binaryInput($dataExpr)"
      } else {
        q"""val ${readerVar}_stream = $byteArrayInputStream($dataExpr)
           |val $readerVar = $binaryInput(${readerVar}_stream)""".stripMargin
      }
    }

    // Helper: create inline reader expression
    private def mkInlineReader(dataExpr: String): TextTree[KtValue] = {
      if (ktTypes.multiplatform) q"$binaryInput($dataExpr)"
      else q"$binaryInput($byteArrayInputStream($dataExpr))"
    }

    private def jsonCodecName(typeId: TypeId.User): KtValue.KtType = {
      val srcRef = trans.toKtTypeRefKeepForeigns(typeId, domain, evo)
      KtValue.KtType(srcRef.pkg, s"${srcRef.name}_JsonCodec", srcRef.fq)
    }

    private def uebaCodecName(typeId: TypeId.User): KtValue.KtType = {
      val srcRef = trans.toKtTypeRefKeepForeigns(typeId, domain, evo)
      KtValue.KtType(srcRef.pkg, s"${srcRef.name}_UEBACodec", srcRef.fq)
    }

    // JSON encode/decode for both User types (via generated codec) and BuiltinScalar (inline).
    private def jsonDecodeExpr(id: TypeId, wire: TextTree[KtValue]): TextTree[KtValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.decode(ctx, $wire)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit => q"$wire.jsonPrimitive.boolean"
          case TypeId.Builtins.i08 => q"$wire.jsonPrimitive.int.toByte()"
          case TypeId.Builtins.i16 => q"$wire.jsonPrimitive.int.toShort()"
          case TypeId.Builtins.i32 => q"$wire.jsonPrimitive.int"
          case TypeId.Builtins.i64 => q"$wire.jsonPrimitive.long"
          case TypeId.Builtins.u08 => q"$wire.jsonPrimitive.int.toUByte()"
          case TypeId.Builtins.u16 => q"$wire.jsonPrimitive.int.toUShort()"
          case TypeId.Builtins.u32 => q"$wire.jsonPrimitive.long.toUInt()"
          case TypeId.Builtins.u64 => q"$wire.jsonPrimitive.long.toULong()"
          case TypeId.Builtins.f32 => q"$wire.jsonPrimitive.float"
          case TypeId.Builtins.f64 => q"$wire.jsonPrimitive.double"
          case TypeId.Builtins.f128 =>
            if (ktTypes.multiplatform) q"${ktTypes.ktBigDecimal}.fromString($wire.jsonPrimitive.content)"
            else q"java.math.BigDecimal($wire.jsonPrimitive.content)"
          case TypeId.Builtins.str   => q"$wire.jsonPrimitive.content"
          case TypeId.Builtins.bytes => q"$ktByteString.fromHexString($wire.jsonPrimitive.content)"
          case TypeId.Builtins.uid =>
            if (ktTypes.multiplatform) q"kotlin.uuid.Uuid.parse($wire.jsonPrimitive.content)"
            else q"java.util.UUID.fromString($wire.jsonPrimitive.content)"
          case TypeId.Builtins.tsu => q"$baboonTimeFormats.parseTsu($wire.jsonPrimitive.content)"
          case TypeId.Builtins.tso => q"$baboonTimeFormats.parseTso($wire.jsonPrimitive.content)"
          case other               => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def jsonEncodeExpr(id: TypeId, value: TextTree[KtValue]): TextTree[KtValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.encode(ctx, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.uid => q"$jsonPrimitive($value.toString())"
          case TypeId.Builtins.tsu => q"$jsonPrimitive($baboonTimeFormats.formatTsu($value))"
          case TypeId.Builtins.tso => q"$jsonPrimitive($baboonTimeFormats.formatTso($value))"
          case TypeId.Builtins.bit => q"$jsonPrimitive($value)"
          case TypeId.Builtins.i08 => q"$jsonPrimitive($value.toInt())"
          case TypeId.Builtins.i16 => q"$jsonPrimitive($value.toInt())"
          case TypeId.Builtins.i32 => q"$jsonPrimitive($value)"
          case TypeId.Builtins.i64 => q"$jsonPrimitive($value)"
          case TypeId.Builtins.u08 => q"$jsonPrimitive($value.toInt())"
          case TypeId.Builtins.u16 => q"$jsonPrimitive($value.toInt())"
          case TypeId.Builtins.u32 => q"$jsonPrimitive($value.toLong())"
          case TypeId.Builtins.u64 => q"$jsonPrimitive($value.toLong())"
          case TypeId.Builtins.f32 => q"$jsonPrimitive($value)"
          case TypeId.Builtins.f64 => q"$jsonPrimitive($value)"
          case TypeId.Builtins.f128 =>
            if (ktTypes.multiplatform) q"$jsonPrimitive($value.toString())"
            else q"$jsonPrimitive($value.toPlainString())"
          case TypeId.Builtins.str   => q"$jsonPrimitive($value)"
          case TypeId.Builtins.bytes => q"$jsonPrimitive($value.toHexString())"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaDecodeExpr(id: TypeId, br: TextTree[KtValue]): TextTree[KtValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.decode(ctx, $br)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit => q"$br.readBoolean()"
          case TypeId.Builtins.i08 => q"$br.readByte()"
          case TypeId.Builtins.i16 => q"$br.readShort()"
          case TypeId.Builtins.i32 => q"$br.readInt()"
          case TypeId.Builtins.i64 => q"$br.readLong()"
          case TypeId.Builtins.u08 => q"$br.readByte().toUByte()"
          case TypeId.Builtins.u16 => q"$br.readShort().toUShort()"
          case TypeId.Builtins.u32 => q"$br.readInt().toUInt()"
          case TypeId.Builtins.u64 => q"$br.readLong().toULong()"
          case TypeId.Builtins.f32 => q"$br.readFloat()"
          case TypeId.Builtins.f64 => q"$br.readDouble()"
          case TypeId.Builtins.f128 =>
            if (ktTypes.multiplatform) q"$baboonBinTools.readBaboonDecimal($br)"
            else q"$baboonBinTools.readBigDecimal($br)"
          case TypeId.Builtins.str   => q"$baboonBinTools.readString($br)"
          case TypeId.Builtins.bytes => q"$baboonBinTools.readByteString($br)"
          case TypeId.Builtins.uid   => q"$baboonBinTools.readUid($br)"
          case TypeId.Builtins.tsu   => q"$baboonBinTools.readTimestamp($br)"
          case TypeId.Builtins.tso =>
            if (ktTypes.multiplatform) q"$baboonBinTools.readTimestampOffset($br)"
            else q"$baboonBinTools.readTimestamp($br)"
          case other => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaEncodeStmt(id: TypeId, bw: TextTree[KtValue], value: TextTree[KtValue]): TextTree[KtValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.encode(ctx, $bw, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit => q"$bw.writeBoolean($value)"
          case TypeId.Builtins.i08 => q"$bw.writeByte($value.toInt())"
          case TypeId.Builtins.i16 => q"$bw.writeShort($value.toInt())"
          case TypeId.Builtins.i32 => q"$bw.writeInt($value)"
          case TypeId.Builtins.i64 => q"$bw.writeLong($value)"
          case TypeId.Builtins.u08 => q"$bw.writeByte($value.toInt())"
          case TypeId.Builtins.u16 => q"$bw.writeShort($value.toInt())"
          case TypeId.Builtins.u32 => q"$bw.writeInt($value.toInt())"
          case TypeId.Builtins.u64 => q"$bw.writeLong($value.toLong())"
          case TypeId.Builtins.f32 => q"$bw.writeFloat($value)"
          case TypeId.Builtins.f64 => q"$bw.writeDouble($value)"
          case TypeId.Builtins.f128 =>
            if (ktTypes.multiplatform) q"$baboonBinTools.writeBaboonDecimal($bw, $value)"
            else q"$baboonBinTools.writeBigDecimal($bw, $value)"
          case TypeId.Builtins.str   => q"$baboonBinTools.writeString($bw, $value)"
          case TypeId.Builtins.bytes => q"$baboonBinTools.writeByteString($bw, $value)"
          case TypeId.Builtins.uid   => q"$baboonBinTools.writeUid($bw, $value)"
          case TypeId.Builtins.tsu   => q"$baboonBinTools.writeTimestamp($bw, $value)"
          case TypeId.Builtins.tso =>
            if (ktTypes.multiplatform) q"$baboonBinTools.writeTimestampOffset($bw, $value)"
            else q"$baboonBinTools.writeTimestamp($bw, $value)"
          case other => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
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

    override def translateServiceRt(domain: Domain): Option[TextTree[KtValue]] = {
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

      val rtTrait: TextTree[KtValue] =
        q"""interface IBaboonServiceRt$traitTypeParams {
           |  fun <L, R> pure(value: R): ${ct("L", "R")}
           |  fun <L, R> fail(error: L): ${ct("L", "R")}
           |  fun <A, B, C> leftMap(value: ${ct("A", "B")}, f: (A) -> C): ${ct("C", "B")}
           |  fun <A, B, C> flatMap(value: ${ct("A", "B")}, f: (B) -> ${ct("A", "C")}): ${ct("A", "C")}
           |}""".stripMargin

      val isBuiltinResult = resolved.resultType.exists(t => t == "Result" || t == "kotlin.Result")

      val defaultImpl: Option[TextTree[KtValue]] = if (!isBuiltinResult && resolved.resultType.exists(_.contains("Either"))) {
        val cct = renderConcreteContainer _
        val defaultRtTypeArg = resolved.hkt match {
          case Some(_) => "<Either>"
          case None    => ""
        }
        Some(
          q"""object BaboonServiceRtDefault : IBaboonServiceRt$defaultRtTypeArg {
             |  override fun <L, R> pure(value: R): ${cct("L", "R")} = Either.Right(value)
             |  override fun <L, R> fail(error: L): ${cct("L", "R")} = Either.Left(error)
             |  override fun <A, B, C> leftMap(value: ${cct("A", "B")}, f: (A) -> C): ${cct("C", "B")} = when (value) {
             |    is Either.Left -> Either.Left(f(value.value))
             |    is Either.Right -> Either.Right(value.value)
             |  }
             |  override fun <A, B, C> flatMap(value: ${cct("A", "B")}, f: (B) -> ${cct("A", "C")}): ${cct("A", "C")} = when (value) {
             |    is Either.Left -> Either.Left(value.value)
             |    is Either.Right -> f(value.value)
             |  }
             |}""".stripMargin
        )
      } else None

      val importHint = resultKtType.map {
        kt =>
          q"private typealias _BaboonServiceResultType<L, R> = $kt<L, R>"
      }

      Some(Seq[Option[TextTree[KtValue]]](importHint, Some(rtTrait), defaultImpl).flatten.join("\n\n"))
    }

    override def translate(defn: DomainMember.User): Option[TextTree[KtValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val methods =
            if (resolved.noErrors) generateNoErrorsWiring(service)
            else generateErrorsWiring(service)
          val importHint = resultKtType.map {
            kt =>
              q"private typealias _${service.id.name.name}WiringResult<L, R> = $kt<L, R>\n\n"
          }.getOrElse(q"")
          Some(q"$importHint$methods")
        case _ => None
      }
    }

    private def ctxParamDecl: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext               => ""
      case ResolvedServiceContext.AbstractContext(tn, pn) => s"$pn: $tn, "
      case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$pn: $tn, "
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

    private def renderFq(tree: TextTree[KtValue]): String = tree.mapRender {
      case t: KtValue.KtType     => if (t.predef) t.name else (t.pkg.parts :+ t.name).mkString(".")
      case t: KtValue.KtTypeName => t.name
    }

    // ========== noErrors mode ==========

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[KtValue] = {
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

      val wiringObj =
        q"""object ${svcName}Wiring {
           |  ${methods.shift(2).trim}
           |}""".stripMargin

      val wrappers = generateServiceWrappers(service, q"String", q"ByteArray")
      Seq(Some(wiringObj), wrappers).flatten.join("\n\n")
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[KtValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"result")
              q"""val encoded = $encodeOut
                 |encoded.toString()""".stripMargin
            case None =>
              q""""null""""
          }

          val callExpr = m.out match {
            case Some(_) => q"val result = impl.${m.name.name}(${ctxArgPass}decoded)"
            case None    => q"impl.${m.name.name}(${ctxArgPass}decoded)"
          }

          q""""${m.name.name}" -> {
             |  val wire = $kotlinxJson.parseToJsonElement(data)
             |  val decoded = $decodeIn
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}
             |}""".stripMargin
      }.join("\n")

      q"""fun invokeJson$genericParam(
         |  method: $baboonMethodId,
         |  data: String,
         |  impl: $svcName$svcTypeArg,
         |  ${ctxParamDecl}ctx: $baboonCodecContext): String {
         |  return when (method.methodName) {
         |    ${cases.shift(4).trim}
         |    else ->
         |      throw $baboonWiringException($baboonWiringError.NoMatchingMethod(method))
         |  }
         |}""".stripMargin
    }

    private def generateNoErrorsUebaMethod(service: Typedef.Service): TextTree[KtValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"br")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"bw", q"result")
              q"""${mkWriterSetup("bw")}
                 |$encStmt
                 |${mkWriterGetBytes("bw")}""".stripMargin
            case None =>
              q"ByteArray(0)"
          }

          val callExpr = m.out match {
            case Some(_) => q"val result = impl.${m.name.name}(${ctxArgPass}decoded)"
            case None    => q"impl.${m.name.name}(${ctxArgPass}decoded)"
          }

          q""""${m.name.name}" -> {
             |  ${mkReaderSetup("br", "data")}
             |  val decoded = $decodeIn
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}
             |}""".stripMargin
      }.join("\n")

      q"""fun invokeUeba$genericParam(
         |  method: $baboonMethodId,
         |  data: ByteArray,
         |  impl: $svcName$svcTypeArg,
         |  ${ctxParamDecl}ctx: $baboonCodecContext): ByteArray {
         |  return when (method.methodName) {
         |    ${cases.shift(4).trim}
         |    else ->
         |      throw $baboonWiringException($baboonWiringError.NoMatchingMethod(method))
         |  }
         |}""".stripMargin
    }

    // ========== errors mode ==========

    private def generateErrorsWiring(service: Typedef.Service): TextTree[KtValue] = {
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

      val wiringObj =
        q"""object ${svcName}Wiring {
           |  ${methods.shift(2).trim}
           |}""".stripMargin

      val jsonRet = q"${ct(bweFq, "String")}"
      val uebaRet = q"${ct(bweFq, "ByteArray")}"
      val wrappers = generateServiceWrappers(service, jsonRet, uebaRet)
      Seq(Some(wiringObj), wrappers).flatten.join("\n\n")
    }

    // ========== Service muxer wrappers ==========
    //
    // Emits per-service `${SvcName}JsonService` / `${SvcName}UebaService`
    // classes adapting the per-domain `${SvcName}Wiring.invokeJson` /
    // `invokeUeba` functions to the runtime `IBaboonJsonService<R>` /
    // `IBaboonUebaService<R>` contract. Extra dependencies the underlying
    // invoker needs (`rt` for noErrors=false, any service-context parameter)
    // are baked at construction time so the runtime contract stays uniform.

    private def generateServiceWrappers(
      service: Typedef.Service,
      jsonRet: TextTree[KtValue],
      uebaRet: TextTree[KtValue],
    ): Option[TextTree[KtValue]] = {
      val jsonWrapper =
        if (hasActiveJsonCodecs(service)) Some(generateOneWrapper(service, isJson = true, jsonRet))
        else None
      val uebaWrapper =
        if (hasActiveUebaCodecs(service)) Some(generateOneWrapper(service, isJson = false, uebaRet))
        else None
      val all = Seq(jsonWrapper, uebaWrapper).flatten
      if (all.isEmpty) None else Some(all.join("\n\n"))
    }

    private def generateOneWrapper(
      service: Typedef.Service,
      isJson: Boolean,
      retType: TextTree[KtValue],
    ): TextTree[KtValue] = {
      val svcName       = service.id.name.name
      val wireType: TextTree[KtValue] = if (isJson) q"String" else q"ByteArray"
      val ifaceType     = if (isJson) ibaboonJsonService else ibaboonUebaService
      val wrapperName   = s"$svcName${if (isJson) "JsonService" else "UebaService"}"
      val invokerName   = if (isJson) "invokeJson" else "invokeUeba"

      // Constructor-baked dependencies, in the order the wiring function expects them.
      val implType: TextTree[KtValue] = q"$svcName$svcTypeArg"

      val rtField: Option[(String, TextTree[KtValue])] = {
        if (resolved.noErrors) None
        else {
          // IBaboonServiceRt is emitted in the domain's root package, not in baboon.runtime.shared.
          val rootPkg = trans.toKtPkg(domain.id, domain.version, evo)
          Some(("rt", q"${KtValue.KtType(rootPkg, "IBaboonServiceRt")}$rtTypeArg"))
        }
      }

      val svcCtxField: Option[(String, TextTree[KtValue])] = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => None
        case ResolvedServiceContext.AbstractContext(tn, pn) => Some((pn, q"$tn"))
        case ResolvedServiceContext.ConcreteContext(tn, pn) => Some((pn, q"$tn"))
      }

      val extraCtorParams: List[(String, TextTree[KtValue])] = List(rtField, svcCtxField).flatten

      val ctorParamList: TextTree[KtValue] = {
        val all = q"private val impl: $implType" ::
          extraCtorParams.map { case (n, t) => q"private val $n: $t" }
        all.join(",\n")
      }

      val invokerArgs: List[TextTree[KtValue]] = {
        val base                       = List(q"method", q"data", q"impl")
        val withRt                     = rtField.fold(base)(_ => base :+ q"rt")
        val withSvcCtx                 = svcCtxField.fold(withRt)(f => withRt :+ q"${f._1}")
        withSvcCtx :+ q"ctx"
      }

      q"""class $wrapperName$genericParam(
         |  ${ctorParamList.shift(2).trim}
         |) : $ifaceType<$retType> {
         |  override val serviceName: String = "$svcName"
         |
         |  override fun invoke(method: $baboonMethodId, data: $wireType, ctx: $baboonCodecContext): $retType {
         |    return ${svcName}Wiring.$invokerName(${invokerArgs.join(", ")})
         |  }
         |}""".stripMargin
    }

    private val bweFq: String = renderFq(q"$baboonWiringError")

    // IBaboonServiceRt is always emitted in the domain root package; use the FQ name
    // so that wiring objects inside a namespace sub-package can still reference it.
    private val iBaboonServiceRtFq: String = {
      val rootPkg = trans.toKtPkg(domain.id, domain.version, evo)
      (rootPkg.parts.toList :+ "IBaboonServiceRt").mkString(".")
    }

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[KtValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "String")

      val cases = service.methods.map {
        m =>
          val inRef    = trans.asKtRef(m.sig, domain, evo)
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          val decodeStep =
            q"""val input: ${ct(bweFq, renderFq(inRef))} = try {
               |  val wire = $kotlinxJson.parseToJsonElement(data)
               |  rt.pure<$bweFq, $inRef>($decodeIn)
               |} catch (ex: Throwable) {
               |  rt.fail<$bweFq, $inRef>($bweFq.DecoderFailed(method, ex))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType   = trans.asKtRef(outRef, domain, evo)
              val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"v")

              val callBody = if (hasErrType) {
                val errType = trans.asKtRef(m.err.get, domain, evo)
                q"""try {
                   |  val callResult = impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.leftMap<$errType, $outType, $bweFq>(
                   |    callResult) { err -> $bweFq.CallFailed(method, err) }
                   |} catch (ex: Throwable) {
                   |  rt.fail<$bweFq, $outType>($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              } else {
                q"""try {
                   |  rt.pure<$bweFq, $outType>(impl.${m.name.name}(${ctxArgPass}v))
                   |} catch (ex: Throwable) {
                   |  rt.fail<$bweFq, $outType>($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              }

              q"""val output = rt.flatMap<$bweFq, $inRef, $outType>(input) { v ->
                 |  ${callBody.shift(2).trim}
                 |}
                 |rt.flatMap<$bweFq, $outType, String>(output) { v ->
                 |  try {
                 |    val encoded = $encodeOut
                 |    rt.pure<$bweFq, String>(encoded.toString())
                 |  } catch (ex: Throwable) {
                 |    rt.fail<$bweFq, String>($bweFq.EncoderFailed(method, ex))
                 |  }
                 |}""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                val errType = trans.asKtRef(m.err.get, domain, evo)
                q"""try {
                   |  val callResult = impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.leftMap<$errType, Unit, $bweFq>(
                   |    callResult) { err -> $bweFq.CallFailed(method, err) }
                   |} catch (ex: Throwable) {
                   |  rt.fail<$bweFq, Unit>($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              } else {
                q"""try {
                   |  impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.pure<$bweFq, Unit>(Unit)
                   |} catch (ex: Throwable) {
                   |  rt.fail<$bweFq, Unit>($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              }

              q"""rt.flatMap<$bweFq, $inRef, String>(input) { v ->
                 |  ${callBody.shift(2).trim}
                 |  rt.pure<$bweFq, String>("null")
                 |}""".stripMargin
          }

          q""""${m.name.name}" -> {
             |  ${decodeStep.shift(2).trim}
             |  ${callAndEncodeStep.shift(2).trim}
             |}""".stripMargin
      }.join("\n")

      q"""fun invokeJson$genericParam(
         |  method: $baboonMethodId,
         |  data: String,
         |  impl: $svcName$svcTypeArg,
         |  rt: $iBaboonServiceRtFq$rtTypeArg,
         |  ${ctxParamDecl}ctx: $baboonCodecContext): $wiringRetType {
         |  return when (method.methodName) {
         |    ${cases.shift(4).trim}
         |    else ->
         |      rt.fail<$bweFq, String>($bweFq.NoMatchingMethod(method))
         |  }
         |}""".stripMargin
    }

    private def generateErrorsUebaMethod(service: Typedef.Service): TextTree[KtValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "ByteArray")

      val cases = service.methods.map {
        m =>
          val inRef    = trans.asKtRef(m.sig, domain, evo)
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"br")

          val decodeStep =
            q"""val input: ${ct(bweFq, renderFq(inRef))} = try {
               |  ${mkReaderSetup("br", "data")}
               |  rt.pure<$bweFq, $inRef>($decodeIn)
               |} catch (ex: Throwable) {
               |  rt.fail<$bweFq, $inRef>($bweFq.DecoderFailed(method, ex))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType = trans.asKtRef(outRef, domain, evo)
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"bw", q"v")

              val callBody = if (hasErrType) {
                val errType = trans.asKtRef(m.err.get, domain, evo)
                q"""try {
                   |  val callResult = impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.leftMap<$errType, $outType, $bweFq>(
                   |    callResult) { err -> $bweFq.CallFailed(method, err) }
                   |} catch (ex: Throwable) {
                   |  rt.fail<$bweFq, $outType>($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              } else {
                q"""try {
                   |  rt.pure<$bweFq, $outType>(impl.${m.name.name}(${ctxArgPass}v))
                   |} catch (ex: Throwable) {
                   |  rt.fail<$bweFq, $outType>($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              }

              q"""val output = rt.flatMap<$bweFq, $inRef, $outType>(input) { v ->
                 |  ${callBody.shift(2).trim}
                 |}
                 |rt.flatMap<$bweFq, $outType, ByteArray>(output) { v ->
                 |  try {
                 |    ${mkWriterSetup("bw").shift(4).trim}
                 |    $encStmt
                 |    rt.pure<$bweFq, ByteArray>(${mkWriterGetBytes("bw")})
                 |  } catch (ex: Throwable) {
                 |    rt.fail<$bweFq, ByteArray>($bweFq.EncoderFailed(method, ex))
                 |  }
                 |}""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                val errType = trans.asKtRef(m.err.get, domain, evo)
                q"""try {
                   |  val callResult = impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.leftMap<$errType, Unit, $bweFq>(
                   |    callResult) { err -> $bweFq.CallFailed(method, err) }
                   |} catch (ex: Throwable) {
                   |  rt.fail<$bweFq, Unit>($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              } else {
                q"""try {
                   |  impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.pure<$bweFq, Unit>(Unit)
                   |} catch (ex: Throwable) {
                   |  rt.fail<$bweFq, Unit>($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              }

              q"""rt.flatMap<$bweFq, $inRef, ByteArray>(input) { v ->
                 |  ${callBody.shift(2).trim}
                 |  rt.pure<$bweFq, ByteArray>(ByteArray(0))
                 |}""".stripMargin
          }

          q""""${m.name.name}" -> {
             |  ${decodeStep.shift(2).trim}
             |  ${callAndEncodeStep.shift(2).trim}
             |}""".stripMargin
      }.join("\n")

      q"""fun invokeUeba$genericParam(
         |  method: $baboonMethodId,
         |  data: ByteArray,
         |  impl: $svcName$svcTypeArg,
         |  rt: $iBaboonServiceRtFq$rtTypeArg,
         |  ${ctxParamDecl}ctx: $baboonCodecContext): $wiringRetType {
         |  return when (method.methodName) {
         |    ${cases.shift(4).trim}
         |    else ->
         |      rt.fail<$bweFq, ByteArray>($bweFq.NoMatchingMethod(method))
         |  }
         |}""".stripMargin
    }

    // ========== Client stub generation ==========

    override def translateClient(defn: DomainMember.User): Option[TextTree[KtValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val svcName = service.id.name.name
          val hasUeba = hasActiveUebaCodecs(service)
          val hasJson = hasActiveJsonCodecs(service)
          if (!hasUeba && !hasJson) return None

          val clientMethods = service.methods.flatMap {
            m =>
              val inTypeRef  = trans.asKtRef(m.sig, domain, evo)
              val outTypeRef = m.out.map(t => trans.asKtRef(t, domain, evo))
              val retType    = outTypeRef.getOrElse(q"Unit")

              val uebaMethod = if (hasUeba) {
                val encodeIn = uebaEncodeStmt(m.sig.id.asInstanceOf[TypeId.Scalar], q"writer", q"arg")
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val decodeExpr = uebaDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"${mkInlineReader("resp")}")
                    q"return $decodeExpr"
                  case None => q"return Unit as $retType"
                }
                Some(
                  q"""suspend fun ${m.name.name}(arg: $inTypeRef, ctx: $baboonCodecContext = $baboonCodecContext.Default): $retType {
                     |  ${mkWriterSetup("writer").shift(2).trim}
                     |  $encodeIn
                     |  val resp = transportUeba("$svcName", "${m.name.name}", ${mkWriterGetBytes("writer")})
                     |  ${decodeOut.shift(2).trim}
                     |}""".stripMargin
                )
              } else None

              val jsonMethod = if (hasJson) {
                val encodeIn = jsonEncodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"arg")
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val decodeExpr = jsonDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"$kotlinxJson.parseToJsonElement(resp)")
                    q"return $decodeExpr"
                  case None => q"return Unit as $retType"
                }
                Some(
                  q"""suspend fun ${m.name.name}Json(arg: $inTypeRef, ctx: $baboonCodecContext = $baboonCodecContext.Default): $retType {
                     |  val encoded = $encodeIn.toString()
                     |  val resp = transportJson("$svcName", "${m.name.name}", encoded)
                     |  ${decodeOut.shift(2).trim}
                     |}""".stripMargin
                )
              } else None

              uebaMethod.toList ++ jsonMethod.toList
          }

          if (clientMethods.isEmpty) return None

          val transportFields = List(
            if (hasUeba) Some(q"private val transportUeba: suspend (service: String, method: String, data: ByteArray) -> ByteArray") else None,
            if (hasJson) Some(q"private val transportJson: suspend (service: String, method: String, data: String) -> String") else None,
          ).flatten

          val ctorParams = List(
            if (hasUeba) Some(q"transportUeba: suspend (service: String, method: String, data: ByteArray) -> ByteArray") else None,
            if (hasJson) Some(q"transportJson: suspend (service: String, method: String, data: String) -> String") else None,
          ).flatten

          val ctorAssigns = List(
            if (hasUeba) Some(q"this.transportUeba = transportUeba") else None,
            if (hasJson) Some(q"this.transportJson = transportJson") else None,
          ).flatten

          Some(
            q"""class ${svcName}Client(
               |  ${ctorParams.join(",\n").shift(2).trim}
               |) {
               |  ${transportFields.joinN().shift(2).trim}
               |
               |  init {
               |    ${ctorAssigns.joinN().shift(4).trim}
               |  }
               |
               |  ${clientMethods.joinNN().shift(2).trim}
               |}""".stripMargin
          )
        case _ => None
      }
    }
  }
}
