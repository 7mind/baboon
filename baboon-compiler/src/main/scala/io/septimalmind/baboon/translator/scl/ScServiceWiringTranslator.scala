package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait ScServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[ScValue]]

  def translateServiceRt(domain: Domain): Option[TextTree[ScValue]]
}

object ScServiceWiringTranslator {
  class Impl(
    target: ScTarget,
    trans: ScTypeTranslator,
    codecs: Set[ScCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
  ) extends ScServiceWiringTranslator {

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "scala", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "scala", target.language.serviceContext, target.language.pragmas)

    private val hasJsonCodecs: Boolean = codecs.exists(_.isInstanceOf[ScJsonCodecGenerator])
    private val hasUebaCodecs: Boolean = codecs.exists(_.isInstanceOf[ScUEBACodecGenerator])

    private def jsonCodecName(typeId: TypeId.User): ScValue.ScType = {
      val srcRef = trans.toScTypeRefKeepForeigns(typeId, domain, evo)
      ScValue.ScType(srcRef.pkg, s"${srcRef.name}_JsonCodec", fq = srcRef.fq)
    }

    private def uebaCodecName(typeId: TypeId.User): ScValue.ScType = {
      val srcRef = trans.toScTypeRefKeepForeigns(typeId, domain, evo)
      ScValue.ScType(srcRef.pkg, s"${srcRef.name}_BinCodec", fq = srcRef.fq)
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

    override def translateServiceRt(domain: Domain): Option[TextTree[ScValue]] = {
      if (resolved.noErrors) return None

      val hasServices = domain.defs.meta.nodes.values.exists {
        case DomainMember.User(_, _: Typedef.Service, _, _) => true
        case _                                              => false
      }
      if (!hasServices) return None

      val ct = renderContainer _

      val traitTypeParams = resolved.hkt match {
        case Some(h) => s"[${h.name}${h.signature}]"
        case None    => ""
      }

      val rtTrait: TextTree[ScValue] =
        q"""trait IBaboonServiceRt$traitTypeParams {
           |  def pure[L, R](value: R): ${ct("L", "R")}
           |  def fail[L, R](error: L): ${ct("L", "R")}
           |  def leftMap[A, B, C](value: ${ct("A", "B")}, f: A => C): ${ct("C", "B")}
           |  def flatMap[A, B, C](value: ${ct("A", "B")}, f: B => ${ct("A", "C")}): ${ct("A", "C")}
           |}""".stripMargin

      val isBuiltinEither = resolved.resultType.exists(t => t == "Either" || t == "scala.util.Either")

      val defaultImpl: Option[TextTree[ScValue]] = if (isBuiltinEither) {
        val cct = renderConcreteContainer _
        val defaultRtTypeArg = resolved.hkt match {
          case Some(_) => "[Either]"
          case None    => ""
        }
        Some(
          q"""object BaboonServiceRtDefault extends IBaboonServiceRt$defaultRtTypeArg {
             |  def pure[L, R](value: R): ${cct("L", "R")} = Right(value)
             |  def fail[L, R](error: L): ${cct("L", "R")} = Left(error)
             |  def leftMap[A, B, C](value: ${cct("A", "B")}, f: A => C): ${cct("C", "B")} = value match {
             |    case Left(a) => Left(f(a))
             |    case Right(b) => Right(b)
             |  }
             |  def flatMap[A, B, C](value: ${cct("A", "B")}, f: B => ${cct("A", "C")}): ${cct("A", "C")} = value match {
             |    case Left(a) => Left(a)
             |    case Right(b) => f(b)
             |  }
             |}""".stripMargin
        )
      } else None

      Some(Seq[Option[TextTree[ScValue]]](Some(rtTrait), defaultImpl).flatten.join("\n\n"))
    }

    override def translate(defn: DomainMember.User): Option[TextTree[ScValue]] = {
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
      if (params.nonEmpty) params.mkString("[", ", ", "]") else ""
    }

    private def svcTypeArg: String = {
      val params = Seq(
        resolved.hkt.map(_.name),
        resolvedCtx match {
          case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
          case _                                             => None
        },
      ).flatten
      if (params.nonEmpty) params.mkString("[", ", ", "]") else ""
    }

    private def rtTypeArg: String = resolved.hkt match {
      case Some(h) => s"[${h.name}]"
      case None    => ""
    }

    private def renderFq(tree: TextTree[ScValue]): String = tree.mapRender {
      case t: ScValue.ScType     => if (t.predef) t.name else (t.pkg.parts :+ t.name).mkString(".")
    }

    // ========== noErrors mode ==========

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[ScValue] = {
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

      q"""object ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}""".stripMargin
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[ScValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])
              q"""val encoded = $outCodec.instance.encode(ctx, result)
                 |encoded.noSpaces""".stripMargin
            case None =>
              q""""null""""
          }

          val callExpr = m.out match {
            case Some(_) => q"val result = impl.${m.name.name}(${ctxArgPass}decoded)"
            case None    => q"impl.${m.name.name}(${ctxArgPass}decoded)"
          }

          q"""case "${m.name.name}" =>
             |  val json = $circeJson.fromString(data).fold(throw _, identity)
             |  val wire = io.circe.parser.parse(data).fold(throw _, identity)
             |  val decoded = $inCodec.instance.decode(ctx, wire).fold(s => throw new $baboonWiringException($baboonWiringError.DecoderFailed(method, new RuntimeException(s.mkString(", ")))), identity)
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}""".stripMargin
      }.join("\n")

      q"""def invokeJson$genericParam(
         |  method: $baboonMethodId,
         |  data: String,
         |  impl: $svcName$svcTypeArg,
         |  ${ctxParamDecl}ctx: $baboonCodecContext): String = {
         |  method.methodName match {
         |    ${cases.shift(4).trim}
         |    case _ =>
         |      throw new $baboonWiringException($baboonWiringError.NoMatchingMethod(method))
         |  }
         |}""".stripMargin
    }

    private def generateNoErrorsUebaMethod(service: Typedef.Service): TextTree[ScValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])
              q"""val oms = new $byteArrayOutputStream()
                 |val bw = new $binaryOutput(oms)
                 |$outCodec.instance.encode(ctx, bw, result)
                 |bw.flush()
                 |oms.toByteArray""".stripMargin
            case None =>
              q"Array.emptyByteArray"
          }

          val callExpr = m.out match {
            case Some(_) => q"val result = impl.${m.name.name}(${ctxArgPass}decoded)"
            case None    => q"impl.${m.name.name}(${ctxArgPass}decoded)"
          }

          q"""case "${m.name.name}" =>
             |  val ims = new java.io.ByteArrayInputStream(data)
             |  val br = new $binaryInput(ims)
             |  val decoded = $inCodec.instance.decode(ctx, br).fold(s => throw new $baboonWiringException($baboonWiringError.DecoderFailed(method, new RuntimeException(s.mkString(", ")))), identity)
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}""".stripMargin
      }.join("\n")

      q"""def invokeUeba$genericParam(
         |  method: $baboonMethodId,
         |  data: Array[Byte],
         |  impl: $svcName$svcTypeArg,
         |  ${ctxParamDecl}ctx: $baboonCodecContext): Array[Byte] = {
         |  method.methodName match {
         |    ${cases.shift(4).trim}
         |    case _ =>
         |      throw new $baboonWiringException($baboonWiringError.NoMatchingMethod(method))
         |  }
         |}""".stripMargin
    }

    // ========== errors mode ==========

    private def generateErrorsWiring(service: Typedef.Service): TextTree[ScValue] = {
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

      q"""object ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}""".stripMargin
    }

    private val bweFq: String = renderFq(q"$baboonWiringError")

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[ScValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "String")

      val cases = service.methods.map {
        m =>
          val inCodec = jsonCodecName(m.sig.id.asInstanceOf[TypeId.User])
          val inRef   = trans.asScRef(m.sig, domain, evo)

          val decodeStep =
            q"""val input: ${ct(bweFq, renderFq(inRef))} = try {
               |  val wire = io.circe.parser.parse(data).fold(throw _, identity)
               |  rt.pure[$bweFq, $inRef]($inCodec.instance.decode(ctx, wire).fold(throw _, identity))
               |} catch {
               |  case ex: Throwable =>
               |    rt.fail[$bweFq, $inRef]($bweFq.DecoderFailed(method, ex))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType  = trans.asScRef(outRef, domain, evo)
              val outCodec = jsonCodecName(outRef.id.asInstanceOf[TypeId.User])

              val callBody = if (hasErrType) {
                val errType = trans.asScRef(m.err.get, domain, evo)
                q"""try {
                   |  val callResult = impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.leftMap[$errType, $outType, $bweFq](
                   |    callResult, err => $bweFq.CallFailed(method, err))
                   |} catch {
                   |  case ex: Throwable =>
                   |    rt.fail[$bweFq, $outType]($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              } else {
                q"""try {
                   |  rt.pure[$bweFq, $outType](impl.${m.name.name}(${ctxArgPass}v))
                   |} catch {
                   |  case ex: Throwable =>
                   |    rt.fail[$bweFq, $outType]($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              }

              q"""val output = rt.flatMap[$bweFq, $inRef, $outType](input, v => {
                 |  ${callBody.shift(2).trim}
                 |})
                 |rt.flatMap[$bweFq, $outType, String](output, v => {
                 |  try {
                 |    val encoded = $outCodec.instance.encode(ctx, v)
                 |    rt.pure[$bweFq, String](encoded.noSpaces)
                 |  } catch {
                 |    case ex: Throwable =>
                 |      rt.fail[$bweFq, String]($bweFq.EncoderFailed(method, ex))
                 |  }
                 |})""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                val errType = trans.asScRef(m.err.get, domain, evo)
                q"""try {
                   |  val callResult = impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.leftMap[$errType, Unit, $bweFq](
                   |    callResult, err => $bweFq.CallFailed(method, err))
                   |} catch {
                   |  case ex: Throwable =>
                   |    rt.fail[$bweFq, Unit]($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              } else {
                q"""try {
                   |  impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.pure[$bweFq, Unit](())
                   |} catch {
                   |  case ex: Throwable =>
                   |    rt.fail[$bweFq, Unit]($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              }

              q"""rt.flatMap[$bweFq, $inRef, String](input, v => {
                 |  ${callBody.shift(2).trim}
                 |  rt.pure[$bweFq, String]("null")
                 |})""".stripMargin
          }

          q"""case "${m.name.name}" =>
             |  ${decodeStep.shift(2).trim}
             |  ${callAndEncodeStep.shift(2).trim}""".stripMargin
      }.join("\n")

      q"""def invokeJson$genericParam(
         |  method: $baboonMethodId,
         |  data: String,
         |  impl: $svcName$svcTypeArg,
         |  rt: IBaboonServiceRt$rtTypeArg,
         |  ${ctxParamDecl}ctx: $baboonCodecContext): $wiringRetType = {
         |  method.methodName match {
         |    ${cases.shift(4).trim}
         |    case _ =>
         |      rt.fail[$bweFq, String]($bweFq.NoMatchingMethod(method))
         |  }
         |}""".stripMargin
    }

    private def generateErrorsUebaMethod(service: Typedef.Service): TextTree[ScValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "Array[Byte]")

      val cases = service.methods.map {
        m =>
          val inCodec = uebaCodecName(m.sig.id.asInstanceOf[TypeId.User])
          val inRef   = trans.asScRef(m.sig, domain, evo)

          val decodeStep =
            q"""val input: ${ct(bweFq, renderFq(inRef))} = try {
               |  val ims = new java.io.ByteArrayInputStream(data)
               |  val br = new $binaryInput(ims)
               |  rt.pure[$bweFq, $inRef]($inCodec.instance.decode(ctx, br).fold(throw _, identity))
               |} catch {
               |  case ex: Throwable =>
               |    rt.fail[$bweFq, $inRef]($bweFq.DecoderFailed(method, ex))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType  = trans.asScRef(outRef, domain, evo)
              val outCodec = uebaCodecName(outRef.id.asInstanceOf[TypeId.User])

              val callBody = if (hasErrType) {
                val errType = trans.asScRef(m.err.get, domain, evo)
                q"""try {
                   |  val callResult = impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.leftMap[$errType, $outType, $bweFq](
                   |    callResult, err => $bweFq.CallFailed(method, err))
                   |} catch {
                   |  case ex: Throwable =>
                   |    rt.fail[$bweFq, $outType]($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              } else {
                q"""try {
                   |  rt.pure[$bweFq, $outType](impl.${m.name.name}(${ctxArgPass}v))
                   |} catch {
                   |  case ex: Throwable =>
                   |    rt.fail[$bweFq, $outType]($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              }

              q"""val output = rt.flatMap[$bweFq, $inRef, $outType](input, v => {
                 |  ${callBody.shift(2).trim}
                 |})
                 |rt.flatMap[$bweFq, $outType, Array[Byte]](output, v => {
                 |  try {
                 |    val oms = new $byteArrayOutputStream()
                 |    val bw = new $binaryOutput(oms)
                 |    $outCodec.instance.encode(ctx, bw, v)
                 |    bw.flush()
                 |    rt.pure[$bweFq, Array[Byte]](oms.toByteArray)
                 |  } catch {
                 |    case ex: Throwable =>
                 |      rt.fail[$bweFq, Array[Byte]]($bweFq.EncoderFailed(method, ex))
                 |  }
                 |})""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                val errType = trans.asScRef(m.err.get, domain, evo)
                q"""try {
                   |  val callResult = impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.leftMap[$errType, Unit, $bweFq](
                   |    callResult, err => $bweFq.CallFailed(method, err))
                   |} catch {
                   |  case ex: Throwable =>
                   |    rt.fail[$bweFq, Unit]($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              } else {
                q"""try {
                   |  impl.${m.name.name}(${ctxArgPass}v)
                   |  rt.pure[$bweFq, Unit](())
                   |} catch {
                   |  case ex: Throwable =>
                   |    rt.fail[$bweFq, Unit]($bweFq.CallFailed(method, ex))
                   |}""".stripMargin
              }

              q"""rt.flatMap[$bweFq, $inRef, Array[Byte]](input, v => {
                 |  ${callBody.shift(2).trim}
                 |  rt.pure[$bweFq, Array[Byte]](Array.emptyByteArray)
                 |})""".stripMargin
          }

          q"""case "${m.name.name}" =>
             |  ${decodeStep.shift(2).trim}
             |  ${callAndEncodeStep.shift(2).trim}""".stripMargin
      }.join("\n")

      q"""def invokeUeba$genericParam(
         |  method: $baboonMethodId,
         |  data: Array[Byte],
         |  impl: $svcName$svcTypeArg,
         |  rt: IBaboonServiceRt$rtTypeArg,
         |  ${ctxParamDecl}ctx: $baboonCodecContext): $wiringRetType = {
         |  method.methodName match {
         |    ${cases.shift(4).trim}
         |    case _ =>
         |      rt.fail[$bweFq, Array[Byte]]($bweFq.NoMatchingMethod(method))
         |  }
         |}""".stripMargin
    }
  }
}
