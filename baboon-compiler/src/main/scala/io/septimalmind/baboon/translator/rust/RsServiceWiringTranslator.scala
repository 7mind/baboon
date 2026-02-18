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

  def translateServiceRt(domain: Domain): Option[TextTree[RsValue]]
}

object RsServiceWiringTranslator {
  class Impl(
    target: RsTarget,
    trans: RsTypeTranslator,
    codecs: Set[RsCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
  ) extends RsServiceWiringTranslator {

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "rust", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "rust", target.language.serviceContext, target.language.pragmas)

    private val hasJsonCodecs: Boolean = codecs.exists(_.isInstanceOf[RsJsonCodecGenerator])
    private val hasUebaCodecs: Boolean = codecs.exists(_.isInstanceOf[RsUEBACodecGenerator])

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
        if (hasJsonCodecs)
          Some(generateNoErrorsJsonFn(service, svcName, svcType))
        else None

      val uebaFn =
        if (hasUebaCodecs)
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
              q"""let result = impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}decoded);
                 |serde_json::to_string(&result).map_err(|e| $baboonWiringError::EncoderFailed(method.clone(), Box::new(e)))""".stripMargin
            case None =>
              q"""impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}decoded);
                 |Ok("null".to_string())""".stripMargin
          }

          q""""${m.name.name}" => {
             |    let decoded: $inFq = serde_json::from_str(data).map_err(|e| $baboonWiringError::DecoderFailed(method.clone(), Box::new(e)))?;
             |    ${encodeAndReturn.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""pub fn invoke_json_$svcName$genericParam(
         |    method: &$baboonMethodId,
         |    data: &str,
         |    impl_: &dyn $svcType$genericParam,
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
              q"""let result = impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}decoded);
                 |let mut out_buf = Vec::new();
                 |$baboonBinEncode::encode_ueba(&result, ctx, &mut out_buf).map_err(|e| $baboonWiringError::EncoderFailed(method.clone(), Box::new(e)))?;
                 |Ok(out_buf)""".stripMargin
            case None =>
              q"""impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}decoded);
                 |Ok(Vec::new())""".stripMargin
          }

          q""""${m.name.name}" => {
             |    let mut cursor = std::io::Cursor::new(data);
             |    let decoded: $inFq = $baboonBinDecode::decode_ueba(ctx, &mut cursor).map_err(|e| $baboonWiringError::DecoderFailed(method.clone(), e))?;
             |    ${encodeAndReturn.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""pub fn invoke_ueba_$svcName$genericParam(
         |    method: &$baboonMethodId,
         |    data: &[u8],
         |    impl_: &dyn $svcType$genericParam,
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
        if (hasJsonCodecs)
          Some(generateErrorsJsonFn(service, svcName, svcType))
        else None

      val uebaFn =
        if (hasUebaCodecs)
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
              val callBody = if (hasErrType) {
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
              val callBody = if (hasErrType) {
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

              q"""rt.flat_map$flatMapHint(input, |v| {
                 |    ${callBody.shift(4).trim}
                 |})""".stripMargin
          }

          q""""${m.name.name}" => {
             |    ${decodeStep.shift(4).trim}
             |    ${callAndEncodeStep.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""pub fn invoke_json_$svcName<Rt: $ibaboonServiceRt>$genericParam(
         |    method: &$baboonMethodId,
         |    data: &str,
         |    impl_: &dyn $svcType$genericParam,
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
              val callBody = if (hasErrType) {
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
              val callBody = if (hasErrType) {
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

              q"""rt.flat_map$flatMapHint(input, |v| {
                 |    ${callBody.shift(4).trim}
                 |})""".stripMargin
          }

          q""""${m.name.name}" => {
             |    ${decodeStep.shift(4).trim}
             |    ${callAndEncodeStep.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""pub fn invoke_ueba_$svcName<Rt: $ibaboonServiceRt>$genericParam(
         |    method: &$baboonMethodId,
         |    data: &[u8],
         |    impl_: &dyn $svcType$genericParam,
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
