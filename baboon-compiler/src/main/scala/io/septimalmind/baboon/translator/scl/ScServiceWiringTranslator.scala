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

    private def hasActiveJsonCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[ScJsonCodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def hasActiveUebaCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[ScUEBACodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    // Returns true if any method in the service references a TypeId.User in its input or output,
    // which means ctx will be used in the generated encode/decode expressions.
    private def jsonCtxIsUsed(service: Typedef.Service): Boolean =
      service.methods.exists {
        m =>
          m.sig.id.isInstanceOf[TypeId.User] || m.out.exists(_.id.isInstanceOf[TypeId.User])
      }

    private def uebaCtxIsUsed(service: Typedef.Service): Boolean =
      service.methods.exists {
        m =>
          m.sig.id.isInstanceOf[TypeId.User] || m.out.exists(_.id.isInstanceOf[TypeId.User])
      }

    // Emits @nowarn only when ctx will NOT be used (to avoid "unused-params" warning).
    // When ctx IS used, the annotation would have nothing to suppress and would itself become
    // a fatal warning under -Wconf:cat=unused-nowarn:e.
    private def nowarnIfCtxUnused(ctxUsed: Boolean): String =
      if (ctxUsed) "" else "@scala.annotation.nowarn(\"cat=unused-params\")\n"

    private def jsonCodecName(typeId: TypeId.User): ScValue.ScType = {
      val srcRef = trans.toScTypeRefKeepForeigns(typeId, domain, evo)
      ScValue.ScType(srcRef.pkg, s"${srcRef.name}_JsonCodec", fq = srcRef.fq)
    }

    private def uebaCodecName(typeId: TypeId.User): ScValue.ScType = {
      val srcRef = trans.toScTypeRefKeepForeigns(typeId, domain, evo)
      ScValue.ScType(srcRef.pkg, s"${srcRef.name}_UEBACodec", fq = srcRef.fq)
    }

    // JSON encode/decode for both User types (via generated codec) and BuiltinScalar (inline).
    // Decode returns a `Either[List[Throwable], T]`-like expression via fold-on-throw.
    private def jsonDecodeExpr(id: TypeId, wire: TextTree[ScValue]): TextTree[ScValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.instance.decode(ctx, $wire).fold(throw _, identity)"
      case b: TypeId.BuiltinScalar =>
        val decoder = b match {
          case TypeId.Builtins.bit   => q"$circeDecodeBoolean"
          case TypeId.Builtins.i08   => q"$baboonDecodeByte"
          case TypeId.Builtins.i16   => q"$baboonDecodeShort"
          case TypeId.Builtins.i32   => q"$baboonDecodeInt"
          case TypeId.Builtins.i64   => q"$baboonDecodeLong"
          case TypeId.Builtins.u08   => q"$baboonDecodeByte"
          case TypeId.Builtins.u16   => q"$baboonDecodeShort"
          case TypeId.Builtins.u32   => q"$baboonDecodeInt"
          case TypeId.Builtins.u64   => q"$baboonDecodeLong"
          case TypeId.Builtins.f32   => q"$circeDecodeFloat"
          case TypeId.Builtins.f64   => q"$circeDecodeDouble"
          case TypeId.Builtins.f128  => q"$baboonDecodeBigDecimalLenient"
          case TypeId.Builtins.str   => q"$circeDecodeString"
          case TypeId.Builtins.bytes => q"$baboonDecodeByteString"
          case TypeId.Builtins.uid   => q"$circeDecodeUuid"
          case TypeId.Builtins.tsu   => q"$baboonDecodeTsu"
          case TypeId.Builtins.tso   => q"$baboonDecodeTso"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
        q"$decoder.decodeJson($wire).fold(e => throw e, identity)"
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def jsonEncodeExpr(id: TypeId, value: TextTree[ScValue]): TextTree[ScValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.instance.encode(ctx, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.uid   => q"$circeJson.fromString($value.toString())"
          case TypeId.Builtins.tsu   => q"$circeJson.fromString($baboonTimeFormats.formatTsu($value))"
          case TypeId.Builtins.tso   => q"$circeJson.fromString($baboonTimeFormats.formatTso($value))"
          case TypeId.Builtins.bit   => q"$circeJson.fromBoolean($value)"
          case TypeId.Builtins.i08   => q"$circeJson.fromInt($value.toInt)"
          case TypeId.Builtins.i16   => q"$circeJson.fromInt($value.toInt)"
          case TypeId.Builtins.i32   => q"$circeJson.fromInt($value)"
          case TypeId.Builtins.i64   => q"$circeJson.fromLong($value)"
          case TypeId.Builtins.u08   => q"$circeJson.fromInt(java.lang.Byte.toUnsignedInt($value))"
          case TypeId.Builtins.u16   => q"$circeJson.fromInt(java.lang.Short.toUnsignedInt($value))"
          case TypeId.Builtins.u32   => q"$circeJson.fromLong(java.lang.Integer.toUnsignedLong($value))"
          case TypeId.Builtins.u64   => q"$circeJson.fromBigInt($baboonBinTools.toUnsignedBigInt($value))"
          case TypeId.Builtins.f32   => q"$circeJson.fromFloat($value).get"
          case TypeId.Builtins.f64   => q"$circeJson.fromDouble($value).get"
          case TypeId.Builtins.f128  => q"$circeJson.fromBigDecimal($value)"
          case TypeId.Builtins.str   => q"$circeJson.fromString($value)"
          case TypeId.Builtins.bytes => q"$circeJson.fromString($value.toHexString)"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaDecodeExpr(id: TypeId, br: TextTree[ScValue]): TextTree[ScValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.decode(ctx, $br).fold(throw _, identity)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                       => q"$br.readBoolean()"
          case TypeId.Builtins.i08                       => q"$br.readByte()"
          case TypeId.Builtins.i16                       => q"$br.readShort()"
          case TypeId.Builtins.i32                       => q"$br.readInt()"
          case TypeId.Builtins.i64                       => q"$br.readLong()"
          case TypeId.Builtins.u08                       => q"$br.readByte()"
          case TypeId.Builtins.u16                       => q"$br.readShort()"
          case TypeId.Builtins.u32                       => q"$br.readInt()"
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

    private def uebaEncodeStmt(id: TypeId, bw: TextTree[ScValue], value: TextTree[ScValue]): TextTree[ScValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.encode(ctx, $bw, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                       => q"$bw.writeBoolean($value)"
          case TypeId.Builtins.i08                       => q"$bw.writeByte($value.toInt)"
          case TypeId.Builtins.i16                       => q"$bw.writeShort($value.toInt)"
          case TypeId.Builtins.i32                       => q"$bw.writeInt($value)"
          case TypeId.Builtins.i64                       => q"$bw.writeLong($value)"
          case TypeId.Builtins.u08                       => q"$bw.writeByte($value.toInt)"
          case TypeId.Builtins.u16                       => q"$bw.writeShort($value.toInt)"
          case TypeId.Builtins.u32                       => q"$bw.writeInt($value)"
          case TypeId.Builtins.u64                       => q"$bw.writeLong($value)"
          case TypeId.Builtins.f32                       => q"$bw.writeFloat($value)"
          case TypeId.Builtins.f64                       => q"$bw.writeDouble($value)"
          case TypeId.Builtins.f128                      => q"$baboonBinTools.writeBigDecimal($bw, $value)"
          case TypeId.Builtins.str                       => q"$baboonBinTools.writeString($bw, $value)"
          case TypeId.Builtins.bytes                     => q"$baboonBinTools.writeByteString($bw, $value)"
          case TypeId.Builtins.uid                       => q"$baboonBinTools.writeUid($bw, $value)"
          case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonBinTools.writeTimestamp($bw, $value)"
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
      case t: ScValue.ScType => if (t.predef) t.name else (t.pkg.parts :+ t.name).mkString(".")
    }

    /** Emits the cross-domain Muxer-entry wrapper classes for a service.
      *
      * Each wrapper extends `IBaboonJsonService[R]` / `IBaboonUebaService[R]`
      * with `R` matching the underlying `${Svc}Wiring.invokeJson` /
      * `invokeUeba` return type — `String` / `Array[Byte]` in noErrors mode,
      * `F[BaboonWiringError, String]` / `F[BaboonWiringError, Array[Byte]]`
      * in errors mode (where `F` is either the user's HKT or the concrete
      * result container). Extra dependencies the underlying invoker needs
      * (`rt` for errors mode, any service-context parameter) are baked at
      * construction time so the runtime `IBaboon*Service.invoke(method, data,
      * ctx)` contract stays uniform across modes.
      */
    private def generateServiceWrappers(service: Typedef.Service): TextTree[ScValue] = {
      val jsonWrapper =
        if (hasActiveJsonCodecs(service)) Some(generateOneWrapper(service, isJson = true))
        else None
      val uebaWrapper =
        if (hasActiveUebaCodecs(service)) Some(generateOneWrapper(service, isJson = false))
        else None
      Seq(jsonWrapper, uebaWrapper).flatten.join("\n\n")
    }

    private def generateOneWrapper(
      service: Typedef.Service,
      isJson: Boolean,
    ): TextTree[ScValue] = {
      val svcName     = service.id.name.name
      val wrapperName = s"$svcName${if (isJson) "JsonService" else "UebaService"}"
      val wireType    = if (isJson) "String" else "Array[Byte]"
      val invokerFn   = if (isJson) "invokeJson" else "invokeUeba"
      val ifaceType   = if (isJson) ibaboonJsonService else ibaboonUebaService

      // Underlying invoker's return shape (R).
      val retType: String = {
        if (resolved.noErrors) wireType
        else s"${ct(bweFq, wireType)}"
      }

      // Class type parameters: same as the wiring method's genericParam.
      val classTypeParams: String = genericParam

      // Constructor params, in the same order as the underlying invoker
      // (impl, [rt,] [svcCtx]); ctx is per-call and stays on `invoke`.
      val rtField: Option[(String, String)] =
        if (resolved.noErrors) None
        else Some(("rt", s"$iBaboonServiceRtFq$rtTypeArg"))

      val svcCtxField: Option[(String, String)] = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => None
        case ResolvedServiceContext.AbstractContext(tn, pn) => Some((pn, tn))
        case ResolvedServiceContext.ConcreteContext(tn, pn) => Some((pn, tn))
      }

      val implField: (String, String) = ("impl", s"$svcName$svcTypeArg")

      val ctorFields: List[(String, String)] =
        List(Some(implField), rtField, svcCtxField).flatten

      val ctorParamList: String =
        ctorFields.map { case (n, t) => s"$n: $t" }.mkString(", ")

      // Forwarded arg list for the underlying invoker call.
      val callArgs: String = {
        val base = List("method", "data", "impl")
        val withRt = rtField.fold(base)(_ => base :+ "rt")
        val withSvcCtx = svcCtxField.fold(withRt)(f => withRt :+ f._1)
        (withSvcCtx :+ "ctx").mkString(", ")
      }

      q"""class $wrapperName$classTypeParams($ctorParamList) extends $ifaceType[$retType] {
         |  val serviceName: String = "$svcName"
         |  def invoke(method: $baboonMethodId, data: $wireType, ctx: $baboonCodecContext): $retType =
         |    ${svcName}Wiring.$invokerFn($callArgs)
         |}""".stripMargin
    }

    // ========== noErrors mode ==========

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[ScValue] = {
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

      val wrappers = generateServiceWrappers(service)

      q"""object ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}
         |
         |$wrappers""".stripMargin
    }

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[ScValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"result")
              q"""val encoded = $encodeOut
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
             |  val decoded = $decodeIn
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}""".stripMargin
      }.join("\n")

      q"""${nowarnIfCtxUnused(jsonCtxIsUsed(service))}def invokeJson$genericParam(
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
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"br")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"bw", q"result")
              q"""val oms = new $byteArrayOutputStream()
                 |val bw = new $binaryOutput(oms)
                 |$encStmt
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
             |  val decoded = $decodeIn
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}""".stripMargin
      }.join("\n")

      q"""${nowarnIfCtxUnused(uebaCtxIsUsed(service))}def invokeUeba$genericParam(
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
        if (hasActiveJsonCodecs(service))
          Some(generateErrorsJsonMethod(service))
        else None

      val uebaMethod =
        if (hasActiveUebaCodecs(service))
          Some(generateErrorsUebaMethod(service))
        else None

      val methods = Seq(jsonMethod, uebaMethod).flatten.join("\n\n")

      val wrappers = generateServiceWrappers(service)

      q"""object ${svcName}Wiring {
         |  ${methods.shift(2).trim}
         |}
         |
         |$wrappers""".stripMargin
    }

    private val bweFq: String = renderFq(q"$baboonWiringError")

    // IBaboonServiceRt is always emitted in the domain root package; use the FQ name
    // so that wiring objects inside a namespace sub-package can still reference it.
    private val iBaboonServiceRtFq: String = {
      val rootPkg = trans.toScPkg(domain.id, domain.version, evo)
      (rootPkg.parts.toList :+ "IBaboonServiceRt").mkString(".")
    }

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[ScValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "String")

      val cases = service.methods.map {
        m =>
          val inRef    = trans.asScRef(m.sig, domain, evo)
          val decodeIn = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"wire")

          val decodeStep =
            q"""val input: ${ct(bweFq, renderFq(inRef))} = try {
               |  val wire = io.circe.parser.parse(data).fold(throw _, identity)
               |  rt.pure[$bweFq, $inRef]($decodeIn)
               |} catch {
               |  case ex: Throwable =>
               |    rt.fail[$bweFq, $inRef]($bweFq.DecoderFailed(method, ex))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType   = trans.asScRef(outRef, domain, evo)
              val encodeOut = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"v")

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
                 |    val encoded = $encodeOut
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

      q"""${nowarnIfCtxUnused(jsonCtxIsUsed(service))}def invokeJson$genericParam(
         |  method: $baboonMethodId,
         |  data: String,
         |  impl: $svcName$svcTypeArg,
         |  rt: $iBaboonServiceRtFq$rtTypeArg,
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
          val inRef    = trans.asScRef(m.sig, domain, evo)
          val decodeIn = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"br")

          val decodeStep =
            q"""val input: ${ct(bweFq, renderFq(inRef))} = try {
               |  val ims = new java.io.ByteArrayInputStream(data)
               |  val br = new $binaryInput(ims)
               |  rt.pure[$bweFq, $inRef]($decodeIn)
               |} catch {
               |  case ex: Throwable =>
               |    rt.fail[$bweFq, $inRef]($bweFq.DecoderFailed(method, ex))
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outType = trans.asScRef(outRef, domain, evo)
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"bw", q"v")

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
                 |    $encStmt
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

      q"""${nowarnIfCtxUnused(uebaCtxIsUsed(service))}def invokeUeba$genericParam(
         |  method: $baboonMethodId,
         |  data: Array[Byte],
         |  impl: $svcName$svcTypeArg,
         |  rt: $iBaboonServiceRtFq$rtTypeArg,
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
