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

      q"""object ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}""".stripMargin
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[KtValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])
              q"""val encoded = $outCodec.encode(ctx, result)
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
             |  val decoded = $inCodec.decode(ctx, wire)
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
          val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])
              q"""${mkWriterSetup("bw")}
                 |$outCodec.instance.encode(ctx, bw, result)
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
             |  val decoded = $inCodec.instance.decode(ctx, br)
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

      q"""object ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}""".stripMargin
    }

    private val bweFq: String = renderFq(q"$baboonWiringError")

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[KtValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "String")

      val cases = service.methods.map {
        m =>
          val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])
          val inRef   = trans.asKtRef(m.sig, domain, evo)

          val decodeStep =
            q"""val input: ${ct(bweFq, renderFq(inRef))} = try {
               |  val wire = $kotlinxJson.parseToJsonElement(data)
               |  rt.pure<$bweFq, $inRef>($inCodec.decode(ctx, wire))
               |} catch (ex: Throwable) {
               |  rt.fail<$bweFq, $inRef>($bweFq.DecoderFailed(method, ex))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType  = trans.asKtRef(outRef, domain, evo)
              val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])

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
                 |    val encoded = $outCodec.encode(ctx, v)
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
         |  rt: IBaboonServiceRt$rtTypeArg,
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
          val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])
          val inRef   = trans.asKtRef(m.sig, domain, evo)

          val decodeStep =
            q"""val input: ${ct(bweFq, renderFq(inRef))} = try {
               |  ${mkReaderSetup("br", "data")}
               |  rt.pure<$bweFq, $inRef>($inCodec.instance.decode(ctx, br))
               |} catch (ex: Throwable) {
               |  rt.fail<$bweFq, $inRef>($bweFq.DecoderFailed(method, ex))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType  = trans.asKtRef(outRef, domain, evo)
              val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])

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
                 |    $outCodec.instance.encode(ctx, bw, v)
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
         |  rt: IBaboonServiceRt$rtTypeArg,
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
              val inTypeId   = m.sig.id.asInstanceOf[TypeId.User]
              val inTypeRef  = trans.asKtRef(m.sig, domain, evo)
              val outTypeRef = m.out.map(t => trans.asKtRef(t, domain, evo))
              val retType    = outTypeRef.getOrElse(q"Unit")

              val uebaMethod = if (hasUeba) {
                val inCodec = uebaCodecName(inTypeId)
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])
                    q"return $outCodec.instance.decode(ctx, ${mkInlineReader("resp")})"
                  case None => q"return Unit as $retType"
                }
                Some(
                  q"""suspend fun ${m.name.name}(arg: $inTypeRef, ctx: $baboonCodecContext = $baboonCodecContext.Default): $retType {
                     |  ${mkWriterSetup("writer").shift(2).trim}
                     |  $inCodec.instance.encode(ctx, writer, arg)
                     |  val resp = transportUeba("$svcName", "${m.name.name}", ${mkWriterGetBytes("writer")})
                     |  ${decodeOut.shift(2).trim}
                     |}""".stripMargin
                )
              } else None

              val jsonMethod = if (hasJson) {
                val inCodec = jsonCodecName(inTypeId)
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])
                    q"return $outCodec.decode($baboonCodecContext.Default, $kotlinxJson.parseToJsonElement(resp))"
                  case None => q"return Unit as $retType"
                }
                Some(
                  q"""suspend fun ${m.name.name}Json(arg: $inTypeRef): $retType {
                     |  val encoded = $inCodec.encode($baboonCodecContext.Default, arg).toString()
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
