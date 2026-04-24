package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.toSnakeCase
import io.septimalmind.baboon.translator.rust.RsTypes.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait RsServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[RsValue]]

  def translateClient(defn: DomainMember.User): Option[TextTree[RsValue]]

  def translateServiceRt(domain: Domain): Option[TextTree[RsValue]]
}

object RsServiceWiringTranslator {
  class Impl(
    target: RsTarget,
    trans: RsTypeTranslator,
    codecs: Set[RsCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
    rsTypes: RsTypes,
  ) extends RsServiceWiringTranslator {
    import rsTypes.*

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "rust", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "rust", target.language.serviceContext, target.language.pragmas)

    private def hasActiveJsonCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[RsJsonCodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def hasActiveUebaCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.isInstanceOf[RsUEBACodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private lazy val rtCrate: RsValue.RsCrateId = {
      val baseCrate = trans.toRsCrate(domain.id, domain.version, evo)
      RsValue.RsCrateId(izumi.fundamentals.collections.nonempty.NEList.unsafeFrom((baseCrate.parts :+ "baboon_service_rt").toList))
    }
    private lazy val ibaboonServiceRt: RsValue.RsType = RsValue.RsType(rtCrate, "IBaboonServiceRt")

    private def renderContainer(error: String, success: String): String = {
      val p = resolved.pattern.get.replace("$error", error).replace("$success", success)
      s"${resolved.resultType.get}$p"
    }

    override def translateServiceRt(domain: Domain): Option[TextTree[RsValue]] = {
      if (resolved.noErrors) return None

      val hasServices = domain.defs.meta.nodes.values.exists {
        case DomainMember.User(_, _: Typedef.Service, _, _) => true
        case _                                              => false
      }
      if (!hasServices) return None

      val ct = renderContainer _

      val rtTrait: TextTree[RsValue] =
        q"""pub trait IBaboonServiceRt {
           |    fn pure<L: 'static, R>(&self, value: R) -> ${ct("L", "R")};
           |    fn fail<L: 'static, R>(&self, error: L) -> ${ct("L", "R")};
           |    fn left_map<A: 'static, B, C: 'static, F: FnOnce(A) -> C>(&self, value: ${ct("A", "B")}, f: F) -> ${ct("C", "B")};
           |    fn flat_map<A: 'static, B, C, F: FnOnce(B) -> ${ct("A", "C")}>(&self, value: ${ct("A", "B")}, f: F) -> ${ct("A", "C")};
           |}""".stripMargin

      val isBuiltinResult = resolved.resultType.exists(_ == "Result")

      val defaultImpl: Option[TextTree[RsValue]] = if (isBuiltinResult) {
        Some(
          q"""pub struct BaboonServiceRtDefault;
             |
             |impl IBaboonServiceRt for BaboonServiceRtDefault {
             |    fn pure<L: 'static, R>(&self, value: R) -> ${ct("L", "R")} { Ok(value) }
             |    fn fail<L: 'static, R>(&self, error: L) -> ${ct("L", "R")} { Err(error) }
             |    fn left_map<A: 'static, B, C: 'static, F: FnOnce(A) -> C>(&self, value: ${ct("A", "B")}, f: F) -> ${ct("C", "B")} {
             |        match value {
             |            Err(a) => Err(f(a)),
             |            Ok(b) => Ok(b),
             |        }
             |    }
             |    fn flat_map<A: 'static, B, C, F: FnOnce(B) -> ${ct("A", "C")}>(&self, value: ${ct("A", "B")}, f: F) -> ${ct("A", "C")} {
             |        match value {
             |            Err(a) => Err(a),
             |            Ok(b) => f(b),
             |        }
             |    }
             |}""".stripMargin
        )
      } else None

      Some(Seq[Option[TextTree[RsValue]]](Some(rtTrait), defaultImpl).flatten.join("\n\n"))
    }

    override def translate(defn: DomainMember.User): Option[TextTree[RsValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val methods =
            if (resolved.noErrors) generateNoErrorsWiring(service)
            else generateErrorsWiring(service)
          Some(methods)
        case _ => None
      }
    }

    override def translateClient(defn: DomainMember.User): Option[TextTree[RsValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val svcName = service.id.name.name
          val hasJson = hasActiveJsonCodecs(service)
          val hasUeba = hasActiveUebaCodecs(service)
          if (!hasJson && !hasUeba) return None

          val clientMethods = service.methods.flatMap {
            m =>
              val inFq  = inTypeFq(m)
              val outFq = m.out.map(o => renderFq(q"${trans.asRsRef(o, domain, evo)}")).getOrElse("()")

              val uebaMethod = if (hasUeba) {
                val decodeResult = m.out match {
                  case Some(_) =>
                    q"""let mut cursor = std::io::Cursor::new(resp);
                       |Ok($baboonBinDecode::decode_ueba(&self.ctx, &mut cursor)?)""".stripMargin
                  case None => q"Ok(())"
                }
                Some(
                  q"""pub ${asyncKw}fn ${toSnakeCase(m.name.name)}(&self, ${ctxParamDecl}arg: $inFq) -> Result<$outFq, Box<dyn std::error::Error>> {
                     |    let mut buf = Vec::new();
                     |    $baboonBinEncode::encode_ueba(&arg, &self.ctx, &mut buf)?;
                     |    let resp = (self.transport_ueba)("$svcName", "${m.name.name}", &buf)$awaitSuffix?;
                     |    ${decodeResult.shift(4).trim}
                     |}""".stripMargin
                )
              } else None

              val jsonMethod = if (hasJson) {
                val decodeResult = m.out match {
                  case Some(_) =>
                    q"""let decoded: $outFq = serde_json::from_str(&resp)?;
                       |Ok(decoded)""".stripMargin
                  case None => q"Ok(())"
                }
                Some(
                  q"""pub ${asyncKw}fn ${toSnakeCase(m.name.name)}_json(&self, ${ctxParamDecl}arg: $inFq) -> Result<$outFq, Box<dyn std::error::Error>> {
                     |    let encoded = serde_json::to_string(&arg)?;
                     |    let resp = (self.transport_json)("$svcName", "${m.name.name}", &encoded)$awaitSuffix?;
                     |    ${decodeResult.shift(4).trim}
                     |}""".stripMargin
                )
              } else None

              uebaMethod.toList ++ jsonMethod.toList
          }

          if (clientMethods.isEmpty) return None

          // Build type params and fields
          val typeParams   = scala.collection.mutable.ListBuffer.empty[String]
          val whereClauses = scala.collection.mutable.ListBuffer.empty[String]
          val fields       = scala.collection.mutable.ListBuffer.empty[TextTree[RsValue]]
          val ctorParams   = scala.collection.mutable.ListBuffer.empty[TextTree[RsValue]]
          val ctorAssigns  = scala.collection.mutable.ListBuffer.empty[TextTree[RsValue]]

          if (hasUeba) {
            if (isAsync) {
              typeParams += "TU"
              whereClauses += s"TU: Fn(&str, &str, &[u8]) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<Vec<u8>, Box<dyn std::error::Error + Send + Sync>>> + Send>> + Send + Sync"
            } else {
              typeParams += "TU"
              whereClauses += "TU: Fn(&str, &str, &[u8]) -> Result<Vec<u8>, Box<dyn std::error::Error>>"
            }
            fields += q"transport_ueba: TU,"
            ctorParams += q"transport_ueba: TU"
            ctorAssigns += q"transport_ueba,"
          }
          if (hasJson) {
            if (isAsync) {
              typeParams += "TJ"
              whereClauses += s"TJ: Fn(&str, &str, &str) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<String, Box<dyn std::error::Error + Send + Sync>>> + Send>> + Send + Sync"
            } else {
              typeParams += "TJ"
              whereClauses += "TJ: Fn(&str, &str, &str) -> Result<String, Box<dyn std::error::Error>>"
            }
            fields += q"transport_json: TJ,"
            ctorParams += q"transport_json: TJ"
            ctorAssigns += q"transport_json,"
          }
          if (hasUeba) {
            fields += q"ctx: $baboonCodecContext,"
            ctorParams += q"ctx: $baboonCodecContext"
            ctorAssigns += q"ctx,"
          }

          val typeParamStr = typeParams.mkString(", ")
          val whereStr     = whereClauses.map(w => q"    $w,").toList.joinN()

          val clientTree =
            q"""pub struct ${svcName}Client<$typeParamStr>
               |where
               |${whereStr.shift(0).trim}
               |{
               |    ${fields.toList.joinN().shift(4).trim}
               |}
               |
               |impl<$typeParamStr> ${svcName}Client<$typeParamStr>
               |where
               |${whereStr.shift(0).trim}
               |{
               |    pub fn new(${ctorParams.toList.join(", ")}) -> Self {
               |        Self { ${ctorAssigns.toList.join(" ")} }
               |    }
               |
               |    ${clientMethods.joinNN().shift(4).trim}
               |}""".stripMargin

          Some(clientTree)
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

    private def genericParam: String = resolvedCtx match {
      case ResolvedServiceContext.AbstractContext(tn, _) => s"<$tn>"
      case _                                             => ""
    }

    private val isAsync: Boolean    = target.language.asyncServices
    private val asyncKw: String     = if (isAsync) "async " else ""
    private val awaitSuffix: String = if (isAsync) ".await" else ""

    private def implParam(svcType: RsValue.RsType): String = {
      val gp = genericParam
      if (isAsync) s"impl_: &(impl ${renderFq(q"$svcType")}$gp + Send + Sync)"
      else s"impl_: &dyn ${renderFq(q"$svcType")}$gp"
    }

    private def renderFq(tree: TextTree[RsValue]): String = tree.mapRender {
      case t: RsValue.RsType     => if (t.predef) t.name else (t.crate.parts :+ t.name).mkString("::")
      case t: RsValue.RsTypeName => t.name
    }

    private def inTypeFq(m: Typedef.MethodDef): String = {
      val ref = trans.toRsTypeRefKeepForeigns(m.sig.id.asInstanceOf[TypeId.User], domain, evo)
      renderFq(q"$ref")
    }

    // ========== noErrors mode ==========

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[RsValue] = {
      val svcName = service.id.name.name
      val svcType = trans.asRsType(service.id, domain, evo)

      val jsonFn =
        if (hasActiveJsonCodecs(service))
          Some(generateNoErrorsJsonFn(service, svcName, svcType))
        else None

      val uebaFn =
        if (hasActiveUebaCodecs(service))
          Some(generateNoErrorsUebaFn(service, svcName, svcType))
        else None

      Seq(jsonFn, uebaFn).flatten.joinNN()
    }

    private def generateNoErrorsJsonFn(service: Typedef.Service, svcName: String, svcType: RsValue.RsType): TextTree[RsValue] = {
      val cases = service.methods.map {
        m =>
          val inFq = inTypeFq(m)

          val encodeAndReturn = m.out match {
            case Some(_) =>
              q"""let result = impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}decoded)$awaitSuffix;
                 |serde_json::to_string(&result).map_err(|e| $baboonWiringError::EncoderFailed(method.clone(), Box::new(e)))""".stripMargin
            case None =>
              q"""impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}decoded)$awaitSuffix;
                 |Ok("null".to_string())""".stripMargin
          }

          q""""${m.name.name}" => {
             |    let decoded: $inFq = serde_json::from_str(data).map_err(|e| $baboonWiringError::DecoderFailed(method.clone(), Box::new(e)))?;
             |    ${encodeAndReturn.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""pub ${asyncKw}fn invoke_json_$svcName$genericParam(
         |    method: &$baboonMethodId,
         |    data: &str,
         |    ${implParam(svcType)},
         |    ${ctxParamDecl}_ctx: &$baboonCodecContext,
         |) -> Result<String, $baboonWiringError> {
         |    match method.method_name.as_str() {
         |        ${cases.shift(8).trim}
         |        _ => Err($baboonWiringError::NoMatchingMethod(method.clone())),
         |    }
         |}""".stripMargin
    }

    private def generateNoErrorsUebaFn(service: Typedef.Service, svcName: String, svcType: RsValue.RsType): TextTree[RsValue] = {
      val cases = service.methods.map {
        m =>
          val inFq = inTypeFq(m)

          val encodeAndReturn = m.out match {
            case Some(_) =>
              q"""let result = impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}decoded)$awaitSuffix;
                 |let mut out_buf = Vec::new();
                 |$baboonBinEncode::encode_ueba(&result, ctx, &mut out_buf).map_err(|e| $baboonWiringError::EncoderFailed(method.clone(), Box::new(e)))?;
                 |Ok(out_buf)""".stripMargin
            case None =>
              q"""impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}decoded)$awaitSuffix;
                 |Ok(Vec::new())""".stripMargin
          }

          q""""${m.name.name}" => {
             |    let mut cursor = std::io::Cursor::new(data);
             |    let decoded: $inFq = $baboonBinDecode::decode_ueba(ctx, &mut cursor).map_err(|e| $baboonWiringError::DecoderFailed(method.clone(), e))?;
             |    ${encodeAndReturn.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""pub ${asyncKw}fn invoke_ueba_$svcName$genericParam(
         |    method: &$baboonMethodId,
         |    data: &[u8],
         |    ${implParam(svcType)},
         |    ctx: &$baboonCodecContext,
         |) -> Result<Vec<u8>, $baboonWiringError> {
         |    match method.method_name.as_str() {
         |        ${cases.shift(8).trim}
         |        _ => Err($baboonWiringError::NoMatchingMethod(method.clone())),
         |    }
         |}""".stripMargin
    }

    // ========== errors mode ==========

    private val bweFq: String = renderFq(q"$baboonWiringError")

    private val errorIsPhantom: Boolean = !resolved.pattern.exists(_.contains("$error"))
    private val pureHint: String        = if (errorIsPhantom) "::<(), _>" else ""
    private val flatMapHint: String     = if (errorIsPhantom) "::<(), _, _, _>" else ""
    private def leftMapHint(m: Typedef.MethodDef): String =
      if (errorIsPhantom) {
        val errFq = m.err
          .map(errRef => renderFq(q"${trans.asRsRef(errRef, domain, evo)}"))
          .getOrElse("()")
        s"::<$errFq, _, _, _>"
      } else ""

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsWiring(service: Typedef.Service): TextTree[RsValue] = {
      val svcName = service.id.name.name
      val svcType = trans.asRsType(service.id, domain, evo)

      val jsonFn =
        if (hasActiveJsonCodecs(service))
          Some(generateErrorsJsonFn(service, svcName, svcType))
        else None

      val uebaFn =
        if (hasActiveUebaCodecs(service))
          Some(generateErrorsUebaFn(service, svcName, svcType))
        else None

      Seq(jsonFn, uebaFn).flatten.joinNN()
    }

    private def generateErrorsJsonFn(service: Typedef.Service, svcName: String, svcType: RsValue.RsType): TextTree[RsValue] = {
      val wiringRetType = ct(bweFq, "String")

      val cases = service.methods.map {
        m =>
          val inFq   = inTypeFq(m)
          val lmHint = leftMapHint(m)

          val decodeStep =
            q"""let input: ${ct(bweFq, inFq)} = match serde_json::from_str::<$inFq>(data) {
               |    Ok(v) => rt.pure$pureHint(v),
               |    Err(e) => rt.fail($bweFq::DecoderFailed(method.clone(), Box::new(e))),
               |};""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(_) =>
              val callBody = if (isAsync) {
                if (hasErrType) {
                  q"""let call_result = impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v).await;
                     |rt.left_map$lmHint(call_result, |err| $bweFq::CallFailed(method.clone(), Box::new(err)))""".stripMargin
                } else {
                  q"""let result = impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v).await;
                     |rt.pure$pureHint(result)""".stripMargin
                }
              } else {
                if (hasErrType) {
                  q"""match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v))) {
                     |    Ok(call_result) => rt.left_map$lmHint(call_result, |err| $bweFq::CallFailed(method.clone(), Box::new(err))),
                     |    Err(panic) => rt.fail($bweFq::CallFailed(method.clone(), panic)),
                     |}""".stripMargin
                } else {
                  q"""match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v))) {
                     |    Ok(result) => rt.pure$pureHint(result),
                     |    Err(panic) => rt.fail($bweFq::CallFailed(method.clone(), panic)),
                     |}""".stripMargin
                }
              }

              q"""let output = rt.flat_map$flatMapHint(input, |v| {
                 |    ${callBody.shift(4).trim}
                 |});
                 |rt.flat_map$flatMapHint(output, |v| {
                 |    match serde_json::to_string(&v) {
                 |        Ok(s) => rt.pure$pureHint(s),
                 |        Err(e) => rt.fail($bweFq::EncoderFailed(method.clone(), Box::new(e))),
                 |    }
                 |})""".stripMargin

            case None =>
              val callBody = if (isAsync) {
                if (hasErrType) {
                  q"""let call_result = impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v).await;
                     |rt.left_map$lmHint(call_result, |err| $bweFq::CallFailed(method.clone(), Box::new(err)))""".stripMargin
                } else {
                  q"""impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v).await;
                     |rt.pure$pureHint("null".to_string())""".stripMargin
                }
              } else {
                if (hasErrType) {
                  q"""match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v))) {
                     |    Ok(call_result) => rt.left_map$lmHint(call_result, |err| $bweFq::CallFailed(method.clone(), Box::new(err))),
                     |    Err(panic) => rt.fail($bweFq::CallFailed(method.clone(), panic)),
                     |}""".stripMargin
                } else {
                  q"""match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v))) {
                     |    Ok(_) => rt.pure$pureHint("null".to_string()),
                     |    Err(panic) => rt.fail($bweFq::CallFailed(method.clone(), panic)),
                     |}""".stripMargin
                }
              }

              q"""rt.flat_map$flatMapHint(input, |v| {
                 |    ${callBody.shift(4).trim}
                 |})""".stripMargin
          }

          q""""${m.name.name}" => {
             |    ${decodeStep.shift(4).trim}
             |    ${callAndEncodeStep.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""pub ${asyncKw}fn invoke_json_$svcName<Rt: $ibaboonServiceRt>$genericParam(
         |    method: &$baboonMethodId,
         |    data: &str,
         |    ${implParam(svcType)},
         |    rt: &Rt,
         |    ${ctxParamDecl}_ctx: &$baboonCodecContext,
         |) -> $wiringRetType {
         |    match method.method_name.as_str() {
         |        ${cases.shift(8).trim}
         |        _ => rt.fail($bweFq::NoMatchingMethod(method.clone())),
         |    }
         |}""".stripMargin
    }

    private def generateErrorsUebaFn(service: Typedef.Service, svcName: String, svcType: RsValue.RsType): TextTree[RsValue] = {
      val wiringRetType = ct(bweFq, "Vec<u8>")

      val cases = service.methods.map {
        m =>
          val inFq   = inTypeFq(m)
          val lmHint = leftMapHint(m)

          val decodeStep =
            q"""let input: ${ct(bweFq, inFq)} = {
               |    let mut cursor = std::io::Cursor::new(data);
               |    match $baboonBinDecode::decode_ueba(ctx, &mut cursor) {
               |        Ok(v) => rt.pure$pureHint(v),
               |        Err(e) => rt.fail($bweFq::DecoderFailed(method.clone(), e)),
               |    }
               |};""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(_) =>
              val callBody = if (isAsync) {
                if (hasErrType) {
                  q"""let call_result = impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v).await;
                     |rt.left_map$lmHint(call_result, |err| $bweFq::CallFailed(method.clone(), Box::new(err)))""".stripMargin
                } else {
                  q"""let result = impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v).await;
                     |rt.pure$pureHint(result)""".stripMargin
                }
              } else {
                if (hasErrType) {
                  q"""match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v))) {
                     |    Ok(call_result) => rt.left_map$lmHint(call_result, |err| $bweFq::CallFailed(method.clone(), Box::new(err))),
                     |    Err(panic) => rt.fail($bweFq::CallFailed(method.clone(), panic)),
                     |}""".stripMargin
                } else {
                  q"""match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v))) {
                     |    Ok(result) => rt.pure$pureHint(result),
                     |    Err(panic) => rt.fail($bweFq::CallFailed(method.clone(), panic)),
                     |}""".stripMargin
                }
              }

              q"""let output = rt.flat_map$flatMapHint(input, |v| {
                 |    ${callBody.shift(4).trim}
                 |});
                 |rt.flat_map$flatMapHint(output, |v| {
                 |    let mut out_buf = Vec::new();
                 |    match $baboonBinEncode::encode_ueba(&v, ctx, &mut out_buf) {
                 |        Ok(()) => rt.pure$pureHint(out_buf),
                 |        Err(e) => rt.fail($bweFq::EncoderFailed(method.clone(), Box::new(e))),
                 |    }
                 |})""".stripMargin

            case None =>
              val callBody = if (isAsync) {
                if (hasErrType) {
                  q"""let call_result = impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v).await;
                     |rt.left_map$lmHint(call_result, |err| $bweFq::CallFailed(method.clone(), Box::new(err)))""".stripMargin
                } else {
                  q"""impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v).await;
                     |rt.pure$pureHint(Vec::new())""".stripMargin
                }
              } else {
                if (hasErrType) {
                  q"""match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v))) {
                     |    Ok(call_result) => rt.left_map$lmHint(call_result, |err| $bweFq::CallFailed(method.clone(), Box::new(err))),
                     |    Err(panic) => rt.fail($bweFq::CallFailed(method.clone(), panic)),
                     |}""".stripMargin
                } else {
                  q"""match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}v))) {
                     |    Ok(_) => rt.pure$pureHint(Vec::new()),
                     |    Err(panic) => rt.fail($bweFq::CallFailed(method.clone(), panic)),
                     |}""".stripMargin
                }
              }

              q"""rt.flat_map$flatMapHint(input, |v| {
                 |    ${callBody.shift(4).trim}
                 |})""".stripMargin
          }

          q""""${m.name.name}" => {
             |    ${decodeStep.shift(4).trim}
             |    ${callAndEncodeStep.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""pub ${asyncKw}fn invoke_ueba_$svcName<Rt: $ibaboonServiceRt>$genericParam(
         |    method: &$baboonMethodId,
         |    data: &[u8],
         |    ${implParam(svcType)},
         |    rt: &Rt,
         |    ctx: &$baboonCodecContext,
         |) -> $wiringRetType {
         |    match method.method_name.as_str() {
         |        ${cases.shift(8).trim}
         |        _ => rt.fail($bweFq::NoMatchingMethod(method.clone())),
         |    }
         |}""".stripMargin
    }
  }
}
