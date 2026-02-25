package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.typescript.TsTypes.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait TsServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TsDefnTranslator.Output]

  def translateServiceRt(): Option[TsDefnTranslator.Output]
}

object TsServiceWiringTranslator {
  class Impl(
    target: TsTarget,
    typeTranslator: TsTypeTranslator,
    codecs: Set[TsCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
    tsFileTools: TsFileTools,
  ) extends TsServiceWiringTranslator {

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "typescript", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "typescript", target.language.serviceContext, target.language.pragmas)

    private val jsonCodec: Option[TsCodecTranslator] = codecs.find(_.isInstanceOf[TsJsonCodecGenerator])
    private val binCodec: Option[TsCodecTranslator]  = codecs.find(_.isInstanceOf[TsUEBACodecGenerator])

    private val isBuiltinEither: Boolean = resolved.resultType.contains("BaboonEither")

    private def renderContainer(error: String, success: String): String = {
      if (isBuiltinEither) {
        val p = resolved.pattern.get.replace("$error", error).replace("$success", success)
        s"${resolved.resultType.get}$p"
      } else {
        "any"
      }
    }

    private val fbase: String = tsFileTools.basename(domain, evo)

    private val rtModule: TsValue.TsModuleId =
      TsValue.TsModuleId(tsFileTools.definitionsBasePkg ++ s"$fbase/baboon-service-rt".split('/').toList)

    private val ibaboonServiceRt: TsValue.TsType = TsValue.TsType(rtModule, "IBaboonServiceRt")

    override def translateServiceRt(): Option[TsDefnTranslator.Output] = {
      if (resolved.noErrors) return None

      val hasServices = domain.defs.meta.nodes.values.exists {
        case DomainMember.User(_, _: Typedef.Service, _, _) => true
        case _                                              => false
      }
      if (!hasServices) return None

      val ct = renderContainer _

      val rtTrait: TextTree[TsValue] =
        q"""export interface IBaboonServiceRt {
           |    pure<L, R>(value: R): ${ct("L", "R")};
           |    fail<L, R>(error: L): ${ct("L", "R")};
           |    leftMap<A, B, C>(value: ${ct("A", "B")}, f: (a: A) => C): ${ct("C", "B")};
           |    flatMap<A, B, C>(value: ${ct("A", "B")}, f: (b: B) => ${ct("A", "C")}): ${ct("A", "C")};
           |}""".stripMargin

      val defaultImpl: Option[TextTree[TsValue]] = if (isBuiltinEither) {
        Some(
          q"""export const BaboonServiceRtDefault: IBaboonServiceRt = {
             |    pure<L, R>(value: R): ${ct("L", "R")} { return { tag: 'Right', value }; },
             |    fail<L, R>(error: L): ${ct("L", "R")} { return { tag: 'Left', value: error }; },
             |    leftMap<A, B, C>(value: ${ct("A", "B")}, f: (a: A) => C): ${ct("C", "B")} {
             |        return (value as { readonly tag: string }).tag === 'Left'
             |            ? { tag: 'Left', value: f((value as { readonly value: A }).value) }
             |            : value as unknown as ${ct("C", "B")};
             |    },
             |    flatMap<A, B, C>(value: ${ct("A", "B")}, f: (b: B) => ${ct("A", "C")}): ${ct("A", "C")} {
             |        return (value as { readonly tag: string }).tag === 'Left'
             |            ? value as unknown as ${ct("A", "C")}
             |            : f((value as { readonly value: B }).value);
             |    },
             |};""".stripMargin
        )
      } else None

      // Include BaboonEither as TsType to trigger import (only for built-in)
      val importTrigger: Option[TextTree[TsValue]] = if (isBuiltinEither) {
        Some(q"type _BaboonEither<L, R> = $baboonEither<L, R>;")
      } else None

      val tree = Seq[Option[TextTree[TsValue]]](importTrigger, Some(rtTrait), defaultImpl).flatten.joinNN()

      Some(
        TsDefnTranslator.Output(
          s"$fbase/baboon-service-rt.ts",
          tree,
          rtModule,
          CompilerProduct.Definition,
        )
      )
    }

    override def translate(defn: DomainMember.User): Option[TsDefnTranslator.Output] = {
      defn.defn match {
        case service: Typedef.Service =>
          val methods =
            if (resolved.noErrors) generateNoErrorsWiring(service)
            else generateErrorsWiring(service)

          val wiringPath   = getWiringPath(defn)
          val wiringModule = TsValue.TsModuleId(tsFileTools.definitionsBasePkg ++ wiringPath.stripSuffix(".ts").split('/').toList)

          Some(
            TsDefnTranslator.Output(
              wiringPath,
              methods,
              wiringModule,
              CompilerProduct.Definition,
            )
          )
        case _ => None
      }
    }

    private def getWiringPath(defn: DomainMember.User): String = {
      val fname = s"${typeTranslator.camelToKebab(defn.defn.id.name.name)}-wiring.ts"
      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${typeTranslator.camelToKebab(id.name.name)}/$fname"
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

    // ========== noErrors mode ==========

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[TsValue] = {
      val svcType = typeTranslator.asTsType(service.id, domain, evo, tsFileTools.definitionsBasePkg)

      val jsonFn =
        jsonCodec match {
          case Some(codec) => Some(generateNoErrorsJsonFn(service, svcType, codec))
          case None        => None
        }

      val uebaFn =
        binCodec match {
          case Some(codec) => Some(generateNoErrorsUebaFn(service, svcType, codec))
          case None        => None
        }

      Seq(jsonFn, uebaFn).flatten.joinNN()
    }

    private def generateNoErrorsJsonFn(
      service: Typedef.Service,
      svcType: TsValue.TsType,
      jsonCodec: TsCodecTranslator,
    ): TextTree[TsValue] = {
      val cases = service.methods.map {
        m =>
          val tpe           = typeTranslator.asTsType(m.sig.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
          val inCodecDecode = jsonCodec.codecName(tpe)

          val encodeAndReturn = m.out match {
            case Some(outRef) =>
              val tpe            = typeTranslator.asTsType(outRef.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
              val outCodecEncode = jsonCodec.codecName(tpe)
              q"""const result = impl.${m.name.name}(${ctxArgPass}decoded);
                 |return JSON.stringify($outCodecEncode.instance.encode($tsBaboonCodecContext.Default, result));""".stripMargin
            case None =>
              q"""impl.${m.name.name}(${ctxArgPass}decoded);
                 |return "null";""".stripMargin
          }

          q"""case "${m.name.name}": {
             |    const decoded = $inCodecDecode.instance.decode($tsBaboonCodecContext.Default, JSON.parse(data));
             |    ${encodeAndReturn.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""export function invokeJson_${svcType.name}(
         |    method: $baboonMethodId,
         |    data: string,
         |    impl: $svcType,
         |    ${ctxParamDecl}ctx: $tsBaboonCodecContext
         |): string {
         |    switch (method.methodName) {
         |        ${cases.shift(8).trim}
         |        default:
         |            throw new $baboonWiringException({ tag: 'NoMatchingMethod', method });
         |    }
         |}""".stripMargin
    }

    private def generateNoErrorsUebaFn(
      service: Typedef.Service,
      svcType: TsValue.TsType,
      codec: TsCodecTranslator,
    ): TextTree[TsValue] = {
      val cases = service.methods.map {
        m =>
          val tpe           = typeTranslator.asTsType(m.sig.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
          val inCodecDecode = codec.codecName(tpe)

          val encodeAndReturn = m.out match {
            case Some(outRef) =>
              val tpe            = typeTranslator.asTsType(outRef.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
              val outCodecEncode = codec.codecName(tpe)
              q"""const result = impl.${m.name.name}(${ctxArgPass}decoded);
                 |const writer = new $tsBaboonBinWriter();
                 |$outCodecEncode.instance.encode(ctx, result, writer);
                 |return writer.toBytes();""".stripMargin
            case None =>
              q"""impl.${m.name.name}(${ctxArgPass}decoded);
                 |return new Uint8Array(0);""".stripMargin
          }

          q"""case "${m.name.name}": {
             |    const reader = new $tsBaboonBinReader(data);
             |    const decoded = $inCodecDecode.instance.decode(ctx, reader);
             |    ${encodeAndReturn.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""export function invokeUeba_${svcType.name}(
         |    method: $baboonMethodId,
         |    data: Uint8Array,
         |    impl: $svcType,
         |    ${ctxParamDecl}ctx: $tsBaboonCodecContext
         |): Uint8Array {
         |    switch (method.methodName) {
         |        ${cases.shift(8).trim}
         |        default:
         |            throw new $baboonWiringException({ tag: 'NoMatchingMethod', method });
         |    }
         |}""".stripMargin
    }

    // ========== errors mode ==========

    private lazy val containerTypeRef: Option[TsValue.TsType] = {
      if (isBuiltinEither) resolved.resultType.map(name => TsValue.TsType(tsBaboonRuntimeShared, name))
      else None
    }

    private def generateErrorsWiring(service: Typedef.Service): TextTree[TsValue] = {
      val svcType = typeTranslator.asTsType(service.id, domain, evo, tsFileTools.definitionsBasePkg)

      val jsonFn =
        jsonCodec match {
          case Some(codec) => Some(generateErrorsJsonFn(service, svcType, codec))
          case None        => None
        }

      val uebaFn =
        binCodec match {
          case Some(codec) => Some(generateErrorsUebaFn(service, svcType, codec))
          case None        => None
        }

      val importTrigger: Option[TextTree[TsValue]] = containerTypeRef.map {
        ref =>
          q"type _ContainerImport<L, R> = $ref<L, R>;"
      }

      (importTrigger.toSeq ++ jsonFn.toSeq ++ uebaFn.toSeq).joinNN()
    }

    private def generateErrorsJsonFn(
      service: Typedef.Service,
      svcType: TsValue.TsType,
      codec: TsCodecTranslator,
    ): TextTree[TsValue] = {
      val wiringRetType = renderContainer("BaboonWiringError", "string")

      val cases = service.methods.map {
        m =>
          val inTypeRef     = typeTranslator.asTsType(m.sig.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
          val inCodecDecode = codec.codecName(inTypeRef)

          val decodeStep =
            q"""let input: ${renderContainer("BaboonWiringError", inTypeRef.name)};
               |try {
               |    input = rt.pure<$baboonWiringError, $inTypeRef>($inCodecDecode.instance.decode($tsBaboonCodecContext.Default, JSON.parse(data)));
               |} catch (ex: unknown) {
               |    input = rt.fail<$baboonWiringError, $inTypeRef>({ tag: 'DecoderFailed', method, error: ex });
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outTypeRef     = typeTranslator.asTsType(outRef.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
              val outCodecEncode = codec.codecName(outTypeRef)

              val callBody = if (hasErrType) {
                q"""try {
                   |    const callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |    return rt.leftMap(callResult, (err) => ({ tag: 'CallFailed' as const, method, domainError: err }));
                   |} catch (ex: unknown) {
                   |    return rt.fail<$baboonWiringError, $outTypeRef>({ tag: 'CallFailed', method, domainError: ex });
                   |}""".stripMargin
              } else {
                q"""try {
                   |    return rt.pure<$baboonWiringError, $outTypeRef>(impl.${m.name.name}(${ctxArgPass}v));
                   |} catch (ex: unknown) {
                   |    return rt.fail<$baboonWiringError, $outTypeRef>({ tag: 'CallFailed', method, domainError: ex });
                   |}""".stripMargin
              }

              q"""const output = rt.flatMap(input, (v: $inTypeRef) => {
                 |    ${callBody.shift(4).trim}
                 |});
                 |return rt.flatMap(output, (v: $outTypeRef) => {
                 |    try {
                 |        return rt.pure<$baboonWiringError, string>(JSON.stringify($outCodecEncode.instance.encode($tsBaboonCodecContext.Default, v)));
                 |    } catch (ex: unknown) {
                 |        return rt.fail<$baboonWiringError, string>({ tag: 'EncoderFailed', method, error: ex });
                 |    }
                 |});""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                q"""try {
                   |    const callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |    return rt.leftMap(callResult, (err) => ({ tag: 'CallFailed' as const, method, domainError: err }));
                   |} catch (ex: unknown) {
                   |    return rt.fail<$baboonWiringError, void>({ tag: 'CallFailed', method, domainError: ex });
                   |}""".stripMargin
              } else {
                q"""try {
                   |    impl.${m.name.name}(${ctxArgPass}v);
                   |    return rt.pure<$baboonWiringError, string>("null");
                   |} catch (ex: unknown) {
                   |    return rt.fail<$baboonWiringError, string>({ tag: 'CallFailed', method, domainError: ex });
                   |}""".stripMargin
              }

              q"""return rt.flatMap(input, (v: $inTypeRef) => {
                 |    ${callBody.shift(4).trim}
                 |});""".stripMargin
          }

          q"""case "${m.name.name}": {
             |    ${decodeStep.shift(4).trim}
             |    ${callAndEncodeStep.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""export function invokeJson_${svcType.name}(
         |    method: $baboonMethodId,
         |    data: string,
         |    impl: $svcType,
         |    rt: $ibaboonServiceRt,
         |    ${ctxParamDecl}ctx: $tsBaboonCodecContext
         |): $wiringRetType {
         |    switch (method.methodName) {
         |        ${cases.shift(8).trim}
         |        default:
         |            return rt.fail<$baboonWiringError, string>({ tag: 'NoMatchingMethod', method });
         |    }
         |}""".stripMargin
    }

    private def generateErrorsUebaFn(
      service: Typedef.Service,
      svcType: TsValue.TsType,
      codec: TsCodecTranslator,
    ): TextTree[TsValue] = {
      val wiringRetType = renderContainer("BaboonWiringError", "Uint8Array")

      val cases = service.methods.map {
        m =>
          val inTypeRef     = typeTranslator.asTsType(m.sig.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
          val inCodecDecode = codec.codecName(inTypeRef)

          val decodeStep =
            q"""let input: ${renderContainer("BaboonWiringError", inTypeRef.name)};
               |try {
               |    const reader = new $tsBaboonBinReader(data);
               |    input = rt.pure<$baboonWiringError, $inTypeRef>($inCodecDecode.instance.decode(ctx, reader));
               |} catch (ex: unknown) {
               |    input = rt.fail<$baboonWiringError, $inTypeRef>({ tag: 'DecoderFailed', method, error: ex });
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val outTypeRef     = typeTranslator.asTsType(outRef.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
              val outCodecEncode = codec.codecName(outTypeRef)

              val callBody = if (hasErrType) {
                q"""try {
                   |    const callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |    return rt.leftMap(callResult, (err) => ({ tag: 'CallFailed' as const, method, domainError: err }));
                   |} catch (ex: unknown) {
                   |    return rt.fail<$baboonWiringError, $outTypeRef>({ tag: 'CallFailed', method, domainError: ex });
                   |}""".stripMargin
              } else {
                q"""try {
                   |    return rt.pure<$baboonWiringError, $outTypeRef>(impl.${m.name.name}(${ctxArgPass}v));
                   |} catch (ex: unknown) {
                   |    return rt.fail<$baboonWiringError, $outTypeRef>({ tag: 'CallFailed', method, domainError: ex });
                   |}""".stripMargin
              }

              q"""const output = rt.flatMap(input, (v: $inTypeRef) => {
                 |    ${callBody.shift(4).trim}
                 |});
                 |return rt.flatMap(output, (v: $outTypeRef) => {
                 |    try {
                 |        const writer = new $tsBaboonBinWriter();
                 |        $outCodecEncode.instance.encode(ctx, v, writer);
                 |        return rt.pure<$baboonWiringError, Uint8Array>(writer.toBytes());
                 |    } catch (ex: unknown) {
                 |        return rt.fail<$baboonWiringError, Uint8Array>({ tag: 'EncoderFailed', method, error: ex });
                 |    }
                 |});""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                q"""try {
                   |    const callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |    return rt.leftMap(callResult, (err) => ({ tag: 'CallFailed' as const, method, domainError: err }));
                   |} catch (ex: unknown) {
                   |    return rt.fail<$baboonWiringError, Uint8Array>({ tag: 'CallFailed', method, domainError: ex });
                   |}""".stripMargin
              } else {
                q"""try {
                   |    impl.${m.name.name}(${ctxArgPass}v);
                   |    return rt.pure<$baboonWiringError, Uint8Array>(new Uint8Array(0));
                   |} catch (ex: unknown) {
                   |    return rt.fail<$baboonWiringError, Uint8Array>({ tag: 'CallFailed', method, domainError: ex });
                   |}""".stripMargin
              }

              q"""return rt.flatMap(input, (v: $inTypeRef) => {
                 |    ${callBody.shift(4).trim}
                 |});""".stripMargin
          }

          q"""case "${m.name.name}": {
             |    ${decodeStep.shift(4).trim}
             |    ${callAndEncodeStep.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""export function invokeUeba_${svcType.name}(
         |    method: $baboonMethodId,
         |    data: Uint8Array,
         |    impl: $svcType,
         |    rt: $ibaboonServiceRt,
         |    ${ctxParamDecl}ctx: $tsBaboonCodecContext
         |): $wiringRetType {
         |    switch (method.methodName) {
         |        ${cases.shift(8).trim}
         |        default:
         |            return rt.fail<$baboonWiringError, Uint8Array>({ tag: 'NoMatchingMethod', method });
         |    }
         |}""".stripMargin
    }
  }
}
