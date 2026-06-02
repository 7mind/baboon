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

          // When a service context is active, the codec-context struct field is
          // renamed away from the default `ctx` so the per-method service-ctx
          // parameter (also `ctx` by default) does not collide. In `none` mode
          // the field keeps its historical name `ctx`, so output is unchanged.
          val clientCodecField: String = svcCtxArgName match {
            case None     => "ctx"
            case Some(pn) => if (pn == "codec_ctx") "baboon_codec_ctx" else "codec_ctx"
          }
          // The service ctx forwarded as the leading transport-callback arg
          // (the transport `Fn` type gains a matching leading parameter). Empty
          // in `none` mode, so the transport call site is byte-identical.
          val transportCtxArg: String = svcCtxArgName.fold("")(pn => s"$pn, ")

          val clientMethods = service.methods.flatMap {
            m =>
              val inFq  = inTypeFq(m)
              val outFq = m.out.map(o => renderFq(q"${trans.asRsRef(o, domain, evo)}")).getOrElse("()")

              val uebaMethod = if (hasUeba) {
                val decodeResult = m.out match {
                  case Some(_) =>
                    q"""let mut cursor = std::io::Cursor::new(resp);
                       |Ok($baboonBinDecode::decode_ueba(&self.$clientCodecField, &mut cursor)?)""".stripMargin
                  case None => q"Ok(())"
                }
                Some(
                  q"""pub ${asyncKw}fn ${toSnakeCase(m.name.name)}(&self, ${ctxParamDecl}arg: $inFq) -> Result<$outFq, Box<dyn std::error::Error>> {
                     |    let mut buf = Vec::new();
                     |    $baboonBinEncode::encode_ueba(&arg, &self.$clientCodecField, &mut buf)?;
                     |    let resp = (self.transport_ueba)(${transportCtxArg}"$svcName", "${m.name.name}", &buf)$awaitSuffix?;
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
                     |    let resp = (self.transport_json)(${transportCtxArg}"$svcName", "${m.name.name}", &encoded)$awaitSuffix?;
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

          // Leading service-ctx type on the transport closures. The abstract
          // service ctx is forwarded as the first transport-callback argument
          // so a caller can attach it to the outgoing request. Empty in `none`
          // mode, so the transport `Fn` signatures are byte-identical.
          val transportCtxType: String = ctxFuncTypePrefix

          if (hasUeba) {
            if (isAsync) {
              typeParams += "TU"
              whereClauses += s"TU: Fn(${transportCtxType}&str, &str, &[u8]) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<Vec<u8>, Box<dyn std::error::Error + Send + Sync>>> + Send>> + Send + Sync"
            } else {
              typeParams += "TU"
              whereClauses += s"TU: Fn(${transportCtxType}&str, &str, &[u8]) -> Result<Vec<u8>, Box<dyn std::error::Error>>"
            }
            fields += q"transport_ueba: TU,"
            ctorParams += q"transport_ueba: TU"
            ctorAssigns += q"transport_ueba,"
          }
          if (hasJson) {
            if (isAsync) {
              typeParams += "TJ"
              whereClauses += s"TJ: Fn(${transportCtxType}&str, &str, &str) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<String, Box<dyn std::error::Error + Send + Sync>>> + Send>> + Send + Sync"
            } else {
              typeParams += "TJ"
              whereClauses += s"TJ: Fn(${transportCtxType}&str, &str, &str) -> Result<String, Box<dyn std::error::Error>>"
            }
            fields += q"transport_json: TJ,"
            ctorParams += q"transport_json: TJ"
            ctorAssigns += q"transport_json,"
          }
          if (hasUeba) {
            fields += q"$clientCodecField: $baboonCodecContext,"
            ctorParams += q"$clientCodecField: $baboonCodecContext"
            ctorAssigns += q"$clientCodecField,"
          }
          // The abstract service ctx is a struct/impl generic so the transport
          // `Fn(Ctx, …)` bound and the per-method `ctx: Ctx` param resolve.
          // Concrete (`type`) mode pins a concrete type, so no generic is added.
          // `Ctx` is used only in bounds/params (never in a field), so a
          // variance-neutral `PhantomData<fn() -> Ctx>` marker is added to
          // satisfy Rust's E0392 (unused type parameter) — it stays Send+Sync
          // and Clone regardless of `Ctx`.
          absCtxTypeParam.foreach { tp =>
            typeParams += tp
            fields += q"_ctx_marker: std::marker::PhantomData<fn() -> $tp>,"
            ctorAssigns += q"_ctx_marker: std::marker::PhantomData,"
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

    // The service-context type name (`abstract` introduces a generic param,
    // `type` pins a concrete type) and its parameter name. The codec-context
    // param is renamed away from the service-context parameter name (default
    // `ctx`) to avoid the duplicate-parameter collision; in `none` mode it
    // stays `ctx`, keeping that output byte-identical.
    private def svcCtxTypeName: Option[String] = resolvedCtx match {
      case ResolvedServiceContext.NoContext              => None
      case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
      case ResolvedServiceContext.ConcreteContext(tn, _) => Some(tn)
    }
    private def svcCtxArgName: Option[String] = resolvedCtx match {
      case ResolvedServiceContext.NoContext              => None
      case ResolvedServiceContext.AbstractContext(_, pn) => Some(pn)
      case ResolvedServiceContext.ConcreteContext(_, pn) => Some(pn)
    }
    // The codec-context parameter name. In `none` mode it keeps the historical
    // names (`_ctx` in the JSON fns where it is unused, `ctx` in the UEBA fns
    // where it is consumed) so that output stays byte-identical. When a service
    // context is active, the codec ctx is renamed to `codec_ctx` to dodge the
    // collision with the abstract/concrete service-ctx param (default `ctx`); a
    // leading `_` is kept for the JSON fns (still unused there) to suppress the
    // unused-variable warning. If the service-ctx param is itself named
    // `codec_ctx`, fall back to `baboon_codec_ctx`.
    private def codecCtxName(isJson: Boolean): String = resolvedCtx match {
      case ResolvedServiceContext.NoContext => if (isJson) "_ctx" else "ctx"
      case _ =>
        val base = if (svcCtxArgName.contains("codec_ctx")) "baboon_codec_ctx" else "codec_ctx"
        if (isJson) s"_$base" else base
    }
    // Type-args prefix for the transport closures: the service context is
    // forwarded to the transport callback (`Fn(Ctx, &str, &str, …)`) so a
    // caller can attach it to the outgoing request.
    private def ctxFuncTypePrefix: String = svcCtxTypeName.fold("")(t => s"$t, ")

    // Single combined generic clause for the errors-mode free fns. The
    // abstract service ctx must share ONE `<...>` clause with `Rt` — Rust
    // rejects two adjacent generic clauses (`<Rt: …><Ctx>`). In `none` mode
    // there is no abstract ctx, so this collapses to `<Rt: …>` unchanged.
    // Kept as a TextTree (not a String) so the `IBaboonServiceRt` node still
    // renders via its short name + auto-collected `use` import, matching the
    // prior `<Rt: $ibaboonServiceRt>` splice byte-for-byte.
    private def errorsFnGenericClause: TextTree[RsValue] = resolvedCtx match {
      case ResolvedServiceContext.AbstractContext(tn, _) => q"<Rt: $ibaboonServiceRt, $tn>"
      case _                                             => q"<Rt: $ibaboonServiceRt>"
    }

    private val isAsync: Boolean    = target.language.asyncServices
    private val asyncKw: String     = if (isAsync) "async " else ""
    private val awaitSuffix: String = if (isAsync) ".await" else ""

    // ========== muxer wrapper structs ==========
    //
    // For every service S, emit `${S}JsonService` and `${S}UebaService` wrapper
    // structs that implement `IBaboon{Json,Ueba}Service<R>` by forwarding to
    // the existing per-service `invoke_{json,ueba}_<svc>` free function. Extra
    // dependencies (`rt` in errors mode, service-context when configured) are
    // baked at construction time so the trait-method signature stays uniform
    // across mode-axes — matching the TS reference. Wrappers are generic over
    // `Impl: <SvcTrait>` (static dispatch into the service impl) and, where
    // applicable, over `Rt: IBaboonServiceRt` and the abstract service-ctx
    // type param. The muxer holds them as `Box<dyn IBaboon*Service<R>>` so a
    // heterogeneous set of wrappers can be composed cross-domain.

    private case class WrapperSpec(
      service: Typedef.Service,
      svcName: String,
      svcType: RsValue.RsType,
      isJson: Boolean,
    )

    /** Returns the `R` (return-type) used by the underlying invoke function
      * and by the wrapper's `IBaboon*Service<R>` impl. In sync mode this is
      * the wire-shape result type (with the result container in errors mode);
      * in async mode it's wrapped in `Pin<Box<dyn Future<Output=…> + Send>>`.
      */
    private def wrapperRetType(isJson: Boolean): String = {
      val wire = if (isJson) "String" else "Vec<u8>"
      val base =
        if (resolved.noErrors) s"Result<$wire, $bweFq>"
        else ct(bweFq, wire)
      if (isAsync) s"std::pin::Pin<Box<dyn std::future::Future<Output = $base> + Send>>"
      else base
    }

    /** Render the abstract-context's type-parameter declaration (e.g. `SvcCtx`)
      * when context resolution names an abstract type.
      */
    private def absCtxTypeParam: Option[String] = resolvedCtx match {
      case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
      case _                                             => None
    }

    /** Render the field declaration for the bound service-context (concrete
      * or abstract) when the underlying invoke takes one.
      */
    private def svcCtxField: Option[(String, String)] = resolvedCtx match {
      case ResolvedServiceContext.NoContext               => None
      case ResolvedServiceContext.AbstractContext(tn, pn) => Some((pn, tn))
      case ResolvedServiceContext.ConcreteContext(tn, pn) => Some((pn, tn))
    }

    private def generateServiceWrappers(service: Typedef.Service): TextTree[RsValue] = {
      val svcName = service.id.name.name
      val svcType = trans.asRsType(service.id, domain, evo)
      val jsonWrapper =
        if (hasActiveJsonCodecs(service)) Some(generateOneWrapper(WrapperSpec(service, svcName, svcType, isJson = true)))
        else None
      val uebaWrapper =
        if (hasActiveUebaCodecs(service)) Some(generateOneWrapper(WrapperSpec(service, svcName, svcType, isJson = false)))
        else None
      Seq(jsonWrapper, uebaWrapper).flatten.joinNN()
    }

    private def generateOneWrapper(spec: WrapperSpec): TextTree[RsValue] = {
      val wrapperName = s"${spec.svcName}${if (spec.isJson) "JsonService" else "UebaService"}"
      val wireTypeStr = if (spec.isJson) "&str" else "&[u8]"
      val invokerFn   = if (spec.isJson) s"invoke_json_${toSnakeCase(spec.svcName)}"
                        else s"invoke_ueba_${toSnakeCase(spec.svcName)}"
      val retTypeStr  = wrapperRetType(spec.isJson)

      // `none` mode keeps the historical context-free trait (`IBaboon*Service`),
      // baking any dependency at construction; the service context (when active)
      // is supplied PER-INVOKE through the context-carrying `IBaboon*ServiceCtx`.
      val ctxActive: Boolean = svcCtxTypeName.isDefined
      val ifaceType: RsValue.RsType =
        if (ctxActive) { if (spec.isJson) ibaboonJsonServiceCtx else ibaboonUebaServiceCtx }
        else { if (spec.isJson) ibaboonJsonService else ibaboonUebaService }

      // The service trait the impl must satisfy. `genericParam` produces
      // `<SvcCtx>` only when ResolvedServiceContext is AbstractContext.
      val svcTraitRef: TextTree[RsValue] = q"${spec.svcType}$genericParam"

      // ---- type parameters (declaration list) ----
      val typeParamNames: List[String] = {
        val rt  = if (resolved.noErrors) Nil else List("Rt")
        val ctx = absCtxTypeParam.toList
        "Impl" :: rt ++ ctx
      }
      val typeParamDecl: String = typeParamNames.mkString("<", ", ", ">")

      // The trait reference: context-free `IBaboon*Service<R>`, or
      // context-carrying `IBaboon*ServiceCtx<Ctx, R>` (the service-ctx type is
      // the abstract generic or the concrete pinned type).
      val ifaceRef: TextTree[RsValue] =
        if (ctxActive) q"$ifaceType<${svcCtxTypeName.get}, $retTypeStr>"
        else q"$ifaceType<$retTypeStr>"

      // ---- where clauses (as TextTree fragments so symbols flow correctly) ----
      val sendSync   = if (isAsync) " + Send + Sync + 'static" else " + 'static"
      val implBound: TextTree[RsValue]               = q"Impl: $svcTraitRef$sendSync"
      val rtBound: Option[TextTree[RsValue]]         =
        if (resolved.noErrors) None else Some(q"Rt: $ibaboonServiceRt$sendSync")
      val absCtxBound: Option[TextTree[RsValue]] = absCtxTypeParam.map { p =>
        val tail = if (isAsync) ": Clone + Send + Sync + 'static" else ": Clone + 'static"
        q"$p$tail"
      }

      val whereLines: List[TextTree[RsValue]] =
        implBound :: rtBound.toList ++ absCtxBound.toList
      val whereBlock: TextTree[RsValue] = whereLines.map(l => q"    $l,").join("\n")

      // ---- struct fields and ctor ----
      // Context-free: the service ctx is baked as a field. Context-active: the
      // ctx arrives per-invoke, so it is NOT stored; an abstract `Ctx` generic
      // is then unused in fields, so a variance-neutral PhantomData marker is
      // added to satisfy Rust's E0392 (it stays Send+Sync+Clone regardless).
      val rtFieldOpt: Option[TextTree[RsValue]] =
        if (resolved.noErrors) None else Some(q"rt: Rt,")
      val storedCtxField: Option[(String, String)] = if (ctxActive) None else svcCtxField
      val ctxFieldOpt: Option[TextTree[RsValue]] = storedCtxField.map {
        case (pn, tn) => q"$pn: $tn,"
      }
      val markerFieldOpt: Option[TextTree[RsValue]] =
        if (ctxActive) absCtxTypeParam.map(p => q"_ctx_marker: std::marker::PhantomData<fn() -> $p>,")
        else None
      val fieldsBlock: TextTree[RsValue] =
        (q"impl_: Impl," :: rtFieldOpt.toList ++ ctxFieldOpt.toList ++ markerFieldOpt.toList).join("\n")

      val rtCtorParamOpt: Option[TextTree[RsValue]] =
        if (resolved.noErrors) None else Some(q"rt: Rt")
      val ctxCtorParamOpt: Option[TextTree[RsValue]] = storedCtxField.map {
        case (pn, tn) => q"$pn: $tn"
      }
      val ctorParams: TextTree[RsValue] =
        (q"impl_: Impl" :: rtCtorParamOpt.toList ++ ctxCtorParamOpt.toList).join(", ")

      val rtCtorAssignOpt: Option[String]  = if (resolved.noErrors) None else Some("rt,")
      val ctxCtorAssignOpt: Option[String] = storedCtxField.map { case (pn, _) => s"$pn," }
      val markerCtorAssignOpt: Option[String] =
        if (ctxActive && absCtxTypeParam.isDefined) Some("_ctx_marker: std::marker::PhantomData,") else None
      val ctorAssignsList: List[String]    = "impl_," :: rtCtorAssignOpt.toList ++ ctxCtorAssignOpt.toList ++ markerCtorAssignOpt.toList
      val ctorAssigns: String              = ctorAssignsList.mkString(" ")

      // ---- invoke() signature + body ----
      // Context-active adds a per-invoke `ctx: <SvcCtx>` param and renames the
      // codec ctx to `codec_ctx`. Context-free keeps the historical signature
      // (`invoke(&self, method, data, ctx: &BaboonCodecContext)`), byte-identical.
      val codecCtxParam: String = if (ctxActive) "codec_ctx" else "ctx"
      val invokeCtxParamDecl: String = svcCtxTypeName.fold("")(tn => s"ctx: $tn, ")

      val body: TextTree[RsValue] = if (isAsync) {
        // Async path: clone everything the future captures into owned
        // bindings, then return a `Pin<Box<Future + Send>>`. Requires
        // Impl/Rt/SvcCtx: Clone + Send + Sync + 'static.
        val cloneRt   = if (resolved.noErrors) "" else "let rt_clone = self.rt.clone();\n"
        val cloneStoredCtx = storedCtxField.fold("")(f => s"let svc_ctx_clone = self.${f._1}.clone();\n")
        val cloneInvokeCtx = if (ctxActive) "let svc_ctx_clone = ctx.clone();\n" else ""
        val dataOwn   = if (spec.isJson) "let data_owned: String = data.to_string();"
                        else "let data_owned: Vec<u8> = data.to_vec();"
        val dataRef   = if (spec.isJson) "data_owned.as_str()" else "data_owned.as_slice()"

        val futArgs: List[String] = {
          val base    = List("&method_owned", dataRef, "&impl_clone")
          val withRt  = if (resolved.noErrors) base else base :+ "&rt_clone"
          val withCtx = if (ctxActive || storedCtxField.isDefined) withRt :+ "svc_ctx_clone" else withRt
          withCtx :+ "&codec_ctx"
        }
        val futArgsStr = futArgs.mkString(", ")
        q"""let method_owned = method.clone();
           |let impl_clone = self.impl_.clone();
           |$cloneRt$cloneStoredCtx$cloneInvokeCtx$dataOwn
           |let codec_ctx = $codecCtxParam.clone();
           |Box::pin(async move {
           |    $invokerFn($futArgsStr).await
           |})""".stripMargin
      } else {
        val syncArgs: List[String] = {
          val base    = List("method", "data", "&self.impl_")
          val withRt  = if (resolved.noErrors) base else base :+ "&self.rt"
          val withCtx =
            if (ctxActive) withRt :+ "ctx"
            else storedCtxField.fold(withRt)(f => withRt :+ s"self.${f._1}.clone()")
          withCtx :+ codecCtxParam
        }
        q"$invokerFn(${syncArgs.mkString(", ")})"
      }

      q"""pub struct $wrapperName$typeParamDecl
         |where
         |${whereBlock.shift(0).trim}
         |{
         |    ${fieldsBlock.shift(4).trim}
         |}
         |
         |impl$typeParamDecl $wrapperName$typeParamDecl
         |where
         |${whereBlock.shift(0).trim}
         |{
         |    pub fn new($ctorParams) -> Self {
         |        Self { $ctorAssigns }
         |    }
         |}
         |
         |impl$typeParamDecl $ifaceRef for $wrapperName$typeParamDecl
         |where
         |${whereBlock.shift(0).trim}
         |{
         |    fn service_name(&self) -> &str { "${spec.svcName}" }
         |    fn invoke(&self, method: &$baboonMethodId, data: $wireTypeStr, ${invokeCtxParamDecl}$codecCtxParam: &$baboonCodecContext) -> $retTypeStr {
         |        ${body.shift(8).trim}
         |    }
         |}""".stripMargin
    }

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
      val ref = trans.asRsType(m.sig.id, domain, evo)
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

      val wrappers = generateServiceWrappers(service)

      (Seq(jsonFn, uebaFn).flatten :+ wrappers).joinNN()
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

      q"""pub ${asyncKw}fn invoke_json_${toSnakeCase(svcName)}$genericParam(
         |    method: &$baboonMethodId,
         |    data: &str,
         |    ${implParam(svcType)},
         |    ${ctxParamDecl}${codecCtxName(isJson = true)}: &$baboonCodecContext,
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
                 |$baboonBinEncode::encode_ueba(&result, ${codecCtxName(isJson = false)}, &mut out_buf).map_err(|e| $baboonWiringError::EncoderFailed(method.clone(), Box::new(e)))?;
                 |Ok(out_buf)""".stripMargin
            case None =>
              q"""impl_.${toSnakeCase(m.name.name)}(${ctxArgPass}decoded)$awaitSuffix;
                 |Ok(Vec::new())""".stripMargin
          }

          q""""${m.name.name}" => {
             |    let mut cursor = std::io::Cursor::new(data);
             |    let decoded: $inFq = $baboonBinDecode::decode_ueba(${codecCtxName(isJson = false)}, &mut cursor).map_err(|e| $baboonWiringError::DecoderFailed(method.clone(), e))?;
             |    ${encodeAndReturn.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""pub ${asyncKw}fn invoke_ueba_${toSnakeCase(svcName)}$genericParam(
         |    method: &$baboonMethodId,
         |    data: &[u8],
         |    ${implParam(svcType)},
         |    ${ctxParamDecl}${codecCtxName(isJson = false)}: &$baboonCodecContext,
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

      val wrappers = generateServiceWrappers(service)

      (Seq(jsonFn, uebaFn).flatten :+ wrappers).joinNN()
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

      q"""pub ${asyncKw}fn invoke_json_${toSnakeCase(svcName)}$errorsFnGenericClause(
         |    method: &$baboonMethodId,
         |    data: &str,
         |    ${implParam(svcType)},
         |    rt: &Rt,
         |    ${ctxParamDecl}${codecCtxName(isJson = true)}: &$baboonCodecContext,
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
               |    match $baboonBinDecode::decode_ueba(${codecCtxName(isJson = false)}, &mut cursor) {
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
                 |    match $baboonBinEncode::encode_ueba(&v, ${codecCtxName(isJson = false)}, &mut out_buf) {
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

      q"""pub ${asyncKw}fn invoke_ueba_${toSnakeCase(svcName)}$errorsFnGenericClause(
         |    method: &$baboonMethodId,
         |    data: &[u8],
         |    ${implParam(svcType)},
         |    rt: &Rt,
         |    ${ctxParamDecl}${codecCtxName(isJson = false)}: &$baboonCodecContext,
         |) -> $wiringRetType {
         |    match method.method_name.as_str() {
         |        ${cases.shift(8).trim}
         |        _ => rt.fail($bweFq::NoMatchingMethod(method.clone())),
         |    }
         |}""".stripMargin
    }
  }
}
