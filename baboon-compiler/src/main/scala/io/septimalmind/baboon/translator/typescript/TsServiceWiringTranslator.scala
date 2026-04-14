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

  def translateClient(defn: DomainMember.User): Option[TsDefnTranslator.Output]

  def translateServiceRt(): Option[TsDefnTranslator.Output]

  def translateDispatcher(): Option[TsDefnTranslator.Output]
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

    private def activeJsonCodec(service: Typedef.Service): Option[TsCodecTranslator] = {
      codecs.find {
        c =>
          c.isInstanceOf[TsJsonCodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def activeBinCodec(service: Typedef.Service): Option[TsCodecTranslator] = {
      codecs.find {
        c =>
          c.isInstanceOf[TsUEBACodecGenerator] && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

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

    override def translateClient(defn: DomainMember.User): Option[TsDefnTranslator.Output] = {
      defn.defn match {
        case service: Typedef.Service =>
          val svcType   = typeTranslator.asTsType(service.id, domain, evo, tsFileTools.definitionsBasePkg)
          val jsonCodec = activeJsonCodec(service)
          val binCodec  = activeBinCodec(service)

          val clientMethods = service.methods.flatMap {
            m =>
              val inTypeRef = typeTranslator.asTsType(m.sig.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
              val inType    = typeTranslator.asTsRef(m.sig, domain, evo, tsFileTools.definitionsBasePkg)
              val outType   = m.out.map(typeTranslator.asTsRef(_, domain, evo, tsFileTools.definitionsBasePkg))
              val retType   = outType.getOrElse(q"void")

              val jsonMethod = jsonCodec.map {
                codec =>
                  val inCodec = codec.codecName(inTypeRef)
                  val decodeOut = m.out match {
                    case Some(outRef) =>
                      val outTypeRef = typeTranslator.asTsType(outRef.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
                      val outCodec   = codec.codecName(outTypeRef)
                      q"return $outCodec.instance.decode($tsBaboonCodecContext.Default, JSON.parse(resp)) as $retType;"
                    case None => q"return undefined as unknown as $retType;"
                  }
                  q"""public async ${m.name.name}Json(${ctxParamDecl}arg: $inType): Promise<$retType> {
                     |    const encoded = JSON.stringify($inCodec.instance.encode($tsBaboonCodecContext.Default, arg));
                     |    const resp = await this.transportJson("${svcType.name}", "${m.name.name}", encoded);
                     |    ${decodeOut.shift(4).trim}
                     |}""".stripMargin
              }

              val uebaMethod = binCodec.map {
                codec =>
                  val inCodec = codec.codecName(inTypeRef)
                  val decodeOut = m.out match {
                    case Some(outRef) =>
                      val outTypeRef = typeTranslator.asTsType(outRef.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
                      val outCodec   = codec.codecName(outTypeRef)
                      q"return $outCodec.instance.decode(ctx, new $tsBaboonBinReader(resp));"
                    case None => q"return undefined as unknown as $retType;"
                  }
                  q"""public async ${m.name.name}(${ctxParamDecl}arg: $inType, ctx: $tsBaboonCodecContext = $tsBaboonCodecContext.Default): Promise<$retType> {
                     |    const writer = new $tsBaboonBinWriter();
                     |    $inCodec.instance.encode(ctx, arg, writer);
                     |    const resp = await this.transportUeba("${svcType.name}", "${m.name.name}", writer.toBytes());
                     |    ${decodeOut.shift(4).trim}
                     |}""".stripMargin
              }

              uebaMethod.toList ++ jsonMethod.toList
          }

          if (clientMethods.isEmpty) return None

          val transportFields = List(
            binCodec.map(_ => q"private readonly transportUeba: (service: string, method: string, data: Uint8Array) => Promise<Uint8Array>"),
            jsonCodec.map(_ => q"private readonly transportJson: (service: string, method: string, data: string) => Promise<string>"),
          ).flatten

          val ctorParams = List(
            binCodec.map(_ => q"transportUeba: (service: string, method: string, data: Uint8Array) => Promise<Uint8Array>"),
            jsonCodec.map(_ => q"transportJson: (service: string, method: string, data: string) => Promise<string>"),
          ).flatten

          val ctorAssigns = List(
            binCodec.map(_ => q"this.transportUeba = transportUeba;"),
            jsonCodec.map(_ => q"this.transportJson = transportJson;"),
          ).flatten

          val clientTree =
            q"""export class ${svcType.name}Client {
               |    ${transportFields.joinN().shift(4).trim}
               |
               |    constructor(${ctorParams.join(", ")}) {
               |        ${ctorAssigns.joinN().shift(8).trim}
               |    }
               |
               |    ${clientMethods.joinNN().shift(4).trim}
               |}""".stripMargin

          val clientPath   = getClientPath(defn)
          val clientModule = TsValue.TsModuleId(tsFileTools.definitionsBasePkg ++ clientPath.stripSuffix(".ts").split('/').toList)

          Some(
            TsDefnTranslator.Output(
              clientPath,
              clientTree,
              clientModule,
              CompilerProduct.Definition,
            )
          )
        case _ => None
      }
    }

    override def translateDispatcher(): Option[TsDefnTranslator.Output] = {
      val services = domain.defs.meta.nodes.values.collect {
        case DomainMember.User(_, s: Typedef.Service, _, _) => s
      }.toList.sortBy(_.id.name.name)

      if (services.isEmpty) return None

      val binCodecActive  = services.exists(activeBinCodec(_).isDefined)
      val jsonCodecActive = services.exists(activeJsonCodec(_).isDefined)
      if (!binCodecActive && !jsonCodecActive) return None

      val retTypeUeba = if (isAsync) "Promise<Uint8Array>" else "Uint8Array"
      val retTypeJson = if (isAsync) "Promise<string>" else "string"

      val implFields = services.map {
        s =>
          val svcType = typeTranslator.asTsType(s.id, domain, evo, tsFileTools.definitionsBasePkg)
          q"${s.id.name.name}: $svcType"
      }

      val uebaFn = if (binCodecActive) {
        val cases = services.flatMap {
          s =>
            activeBinCodec(s).map {
              _ =>
                val svcType = typeTranslator.asTsType(s.id, domain, evo, tsFileTools.definitionsBasePkg)
                val wiringFnRef = TsValue.TsType(
                  TsValue.TsModuleId(tsFileTools.definitionsBasePkg ++ getWiringPathForService(s).stripSuffix(".ts").split('/').toList),
                  s"invokeUeba_${svcType.name}",
                )
                if (resolved.noErrors) {
                  q"""case "${s.id.name.name}":
                     |    return $awaitPrefix$wiringFnRef({ serviceName, methodName }, data, impls.${s.id.name.name}, ${ctxArgPass}ctx);""".stripMargin
                } else {
                  q"""case "${s.id.name.name}":
                     |    return $awaitPrefix$wiringFnRef({ serviceName, methodName }, data, impls.${s.id.name.name}, rt, ${ctxArgPass}ctx);""".stripMargin
                }
            }
        }

        val rtParam = if (resolved.noErrors) "" else s"rt: $ibaboonServiceRt, "

        Some(
          q"""export ${asyncPrefix}function dispatchUeba(
             |    serviceName: string,
             |    methodName: string,
             |    data: Uint8Array,
             |    impls: {${implFields.join("; ")}},
             |    $rtParam${ctxParamDecl}ctx: $tsBaboonCodecContext
             |): $retTypeUeba {
             |    switch (serviceName) {
             |        ${cases.joinN().shift(8).trim}
             |        default:
             |            throw new $baboonWiringException({ tag: 'NoMatchingMethod', method: { serviceName, methodName } });
             |    }
             |}""".stripMargin
        )
      } else None

      val jsonFn = if (jsonCodecActive) {
        val cases = services.flatMap {
          s =>
            activeJsonCodec(s).map {
              _ =>
                val svcType = typeTranslator.asTsType(s.id, domain, evo, tsFileTools.definitionsBasePkg)
                val wiringFnRef = TsValue.TsType(
                  TsValue.TsModuleId(tsFileTools.definitionsBasePkg ++ getWiringPathForService(s).stripSuffix(".ts").split('/').toList),
                  s"invokeJson_${svcType.name}",
                )
                if (resolved.noErrors) {
                  q"""case "${s.id.name.name}":
                     |    return $awaitPrefix$wiringFnRef({ serviceName, methodName }, data, impls.${s.id.name.name}, ${ctxArgPass}ctx);""".stripMargin
                } else {
                  q"""case "${s.id.name.name}":
                     |    return $awaitPrefix$wiringFnRef({ serviceName, methodName }, data, impls.${s.id.name.name}, rt, ${ctxArgPass}ctx);""".stripMargin
                }
            }
        }

        val rtParam = if (resolved.noErrors) "" else s"rt: $ibaboonServiceRt, "

        Some(
          q"""export ${asyncPrefix}function dispatchJson(
             |    serviceName: string,
             |    methodName: string,
             |    data: string,
             |    impls: {${implFields.join("; ")}},
             |    $rtParam${ctxParamDecl}ctx: $tsBaboonCodecContext
             |): $retTypeJson {
             |    switch (serviceName) {
             |        ${cases.joinN().shift(8).trim}
             |        default:
             |            throw new $baboonWiringException({ tag: 'NoMatchingMethod', method: { serviceName, methodName } });
             |    }
             |}""".stripMargin
        )
      } else None

      val tree             = Seq(uebaFn, jsonFn).flatten.joinNN()
      val dispatcherPath   = s"$fbase/baboon-dispatcher.ts"
      val dispatcherModule = TsValue.TsModuleId(tsFileTools.definitionsBasePkg ++ dispatcherPath.stripSuffix(".ts").split('/').toList)

      Some(
        TsDefnTranslator.Output(
          dispatcherPath,
          tree,
          dispatcherModule,
          CompilerProduct.Definition,
        )
      )
    }

    private def getWiringPath(defn: DomainMember.User): String = {
      val fname = s"${typeTranslator.camelToKebab(defn.defn.id.name.name)}-wiring.ts"
      defn.defn.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${typeTranslator.camelToKebab(id.name.name)}/$fname"
      }
    }

    private def getWiringPathForService(service: Typedef.Service): String = {
      val fname = s"${typeTranslator.camelToKebab(service.id.name.name)}-wiring.ts"
      service.id.owner match {
        case Owner.Toplevel => s"$fbase/$fname"
        case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString("/")}/$fname"
        case Owner.Adt(id)  => s"$fbase/${typeTranslator.camelToKebab(id.name.name)}/$fname"
      }
    }

    private def getClientPath(defn: DomainMember.User): String = {
      val fname = s"${typeTranslator.camelToKebab(defn.defn.id.name.name)}-client.ts"
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

    private val isAsync: Boolean = target.language.asyncServices

    private val asyncPrefix: String = if (isAsync) "async " else ""
    private val awaitPrefix: String = if (isAsync) "await " else ""

    // ========== noErrors mode ==========

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[TsValue] = {
      val svcType = typeTranslator.asTsType(service.id, domain, evo, tsFileTools.definitionsBasePkg)

      val jsonFn =
        activeJsonCodec(service) match {
          case Some(codec) => Some(generateNoErrorsJsonFn(service, svcType, codec))
          case None        => None
        }

      val uebaFn =
        activeBinCodec(service) match {
          case Some(codec) => Some(generateNoErrorsUebaFn(service, svcType, codec))
          case None        => None
        }

      Seq(jsonFn, uebaFn).flatten.joinNN()
    }

    private def noErrorsJsonRetType: String = if (isAsync) "Promise<string>" else "string"
    private def noErrorsUebaRetType: String = if (isAsync) "Promise<Uint8Array>" else "Uint8Array"

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
              q"""const result = ${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded);
                 |return JSON.stringify($outCodecEncode.instance.encode($tsBaboonCodecContext.Default, result));""".stripMargin
            case None =>
              q"""${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded);
                 |return "null";""".stripMargin
          }

          q"""case "${m.name.name}": {
             |    const decoded = $inCodecDecode.instance.decode($tsBaboonCodecContext.Default, JSON.parse(data));
             |    ${encodeAndReturn.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""export ${asyncPrefix}function invokeJson_${svcType.name}(
         |    method: $baboonMethodId,
         |    data: string,
         |    impl: $svcType,
         |    ${ctxParamDecl}ctx: $tsBaboonCodecContext
         |): $noErrorsJsonRetType {
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
              q"""const result = ${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded);
                 |const writer = new $tsBaboonBinWriter();
                 |$outCodecEncode.instance.encode(ctx, result, writer);
                 |return writer.toBytes();""".stripMargin
            case None =>
              q"""${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded);
                 |return new Uint8Array(0);""".stripMargin
          }

          q"""case "${m.name.name}": {
             |    const reader = new $tsBaboonBinReader(data);
             |    const decoded = $inCodecDecode.instance.decode(ctx, reader);
             |    ${encodeAndReturn.shift(4).trim}
             |}""".stripMargin
      }.join("\n")

      q"""export ${asyncPrefix}function invokeUeba_${svcType.name}(
         |    method: $baboonMethodId,
         |    data: Uint8Array,
         |    impl: $svcType,
         |    ${ctxParamDecl}ctx: $tsBaboonCodecContext
         |): $noErrorsUebaRetType {
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
        activeJsonCodec(service) match {
          case Some(codec) => Some(generateErrorsJsonFn(service, svcType, codec))
          case None        => None
        }

      val uebaFn =
        activeBinCodec(service) match {
          case Some(codec) => Some(generateErrorsUebaFn(service, svcType, codec))
          case None        => None
        }

      val importTrigger: Option[TextTree[TsValue]] = containerTypeRef.map {
        ref =>
          q"type _ContainerImport<L, R> = $ref<L, R>;"
      }

      (importTrigger.toSeq ++ jsonFn.toSeq ++ uebaFn.toSeq).joinNN()
    }

    private def errorsJsonRetType: String = {
      val base = renderContainer("BaboonWiringError", "string")
      if (isAsync) s"Promise<$base>" else base
    }

    private def errorsUebaRetType: String = {
      val base = renderContainer("BaboonWiringError", "Uint8Array")
      if (isAsync) s"Promise<$base>" else base
    }

    private def generateErrorsMethodBody(
      m: Typedef.MethodDef,
      codec: TsCodecTranslator,
      decodeExpr: TsValue.TsType => TextTree[TsValue],
      encodeExpr: (TsValue.TsType, TsValue.TsType) => TextTree[TsValue],
      wireType: String,
    ): TextTree[TsValue] = {
      val inTypeRef     = typeTranslator.asTsType(m.sig.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
      val inCodecDecode = codec.codecName(inTypeRef)
      val hasErrType    = m.err.isDefined && !resolved.noErrors

      if (isAsync) {
        val decodeStep =
          q"""let decoded: $inTypeRef;
             |try {
             |    decoded = ${decodeExpr(inCodecDecode)};
             |} catch (ex: unknown) {
             |    return rt.fail<$baboonWiringError, $wireType>({ tag: 'DecoderFailed', method, error: ex });
             |}""".stripMargin

        val callAndEncodeStep = m.out match {
          case Some(outRef) =>
            val outTypeRef     = typeTranslator.asTsType(outRef.id.asInstanceOf[TypeId.User], domain, evo, tsFileTools.definitionsBasePkg)
            val outCodecEncode = codec.codecName(outTypeRef)

            if (hasErrType) {
              q"""let output: $outTypeRef;
                 |try {
                 |    const callResult = await impl.${m.name.name}(${ctxArgPass}decoded);
                 |    const mapped = rt.leftMap(callResult, (err: unknown) => ({ tag: 'CallFailed' as const, method, domainError: err }));
                 |    if ((mapped as { readonly tag: string }).tag === 'Left') return mapped as unknown as ${renderContainer("BaboonWiringError", wireType)};
                 |    output = (mapped as { readonly value: $outTypeRef }).value;
                 |} catch (ex: unknown) {
                 |    return rt.fail<$baboonWiringError, $wireType>({ tag: 'CallFailed', method, domainError: ex });
                 |}
                 |try {
                 |    return rt.pure<$baboonWiringError, $wireType>(${encodeExpr(outCodecEncode, outTypeRef)});
                 |} catch (ex: unknown) {
                 |    return rt.fail<$baboonWiringError, $wireType>({ tag: 'EncoderFailed', method, error: ex });
                 |}""".stripMargin
            } else {
              q"""let output: $outTypeRef;
                 |try {
                 |    output = await impl.${m.name.name}(${ctxArgPass}decoded);
                 |} catch (ex: unknown) {
                 |    return rt.fail<$baboonWiringError, $wireType>({ tag: 'CallFailed', method, domainError: ex });
                 |}
                 |try {
                 |    return rt.pure<$baboonWiringError, $wireType>(${encodeExpr(outCodecEncode, outTypeRef)});
                 |} catch (ex: unknown) {
                 |    return rt.fail<$baboonWiringError, $wireType>({ tag: 'EncoderFailed', method, error: ex });
                 |}""".stripMargin
            }

          case None =>
            if (hasErrType) {
              q"""try {
                 |    const callResult = await impl.${m.name.name}(${ctxArgPass}decoded);
                 |    return rt.leftMap(callResult, (err: unknown) => ({ tag: 'CallFailed' as const, method, domainError: err })) as unknown as ${renderContainer(
                  "BaboonWiringError",
                  wireType,
                )};
                 |} catch (ex: unknown) {
                 |    return rt.fail<$baboonWiringError, $wireType>({ tag: 'CallFailed', method, domainError: ex });
                 |}""".stripMargin
            } else {
              q"""try {
                 |    await impl.${m.name.name}(${ctxArgPass}decoded);
                 |    return rt.pure<$baboonWiringError, $wireType>("null");
                 |} catch (ex: unknown) {
                 |    return rt.fail<$baboonWiringError, $wireType>({ tag: 'CallFailed', method, domainError: ex });
                 |}""".stripMargin
            }
        }

        q"""case "${m.name.name}": {
           |    ${decodeStep.shift(4).trim}
           |    ${callAndEncodeStep.shift(4).trim}
           |}""".stripMargin
      } else {
        val decodeStep =
          q"""let input: ${renderContainer("BaboonWiringError", inTypeRef.name)};
             |try {
             |    input = rt.pure<$baboonWiringError, $inTypeRef>(${decodeExpr(inCodecDecode)});
             |} catch (ex: unknown) {
             |    input = rt.fail<$baboonWiringError, $inTypeRef>({ tag: 'DecoderFailed', method, error: ex });
             |}""".stripMargin

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
               |        return rt.pure<$baboonWiringError, $wireType>(${encodeExpr(outCodecEncode, outTypeRef)});
               |    } catch (ex: unknown) {
               |        return rt.fail<$baboonWiringError, $wireType>({ tag: 'EncoderFailed', method, error: ex });
               |    }
               |});""".stripMargin

          case None =>
            val callBody = if (hasErrType) {
              q"""try {
                 |    const callResult = impl.${m.name.name}(${ctxArgPass}v);
                 |    return rt.leftMap(callResult, (err) => ({ tag: 'CallFailed' as const, method, domainError: err }));
                 |} catch (ex: unknown) {
                 |    return rt.fail<$baboonWiringError, $wireType>({ tag: 'CallFailed', method, domainError: ex });
                 |}""".stripMargin
            } else {
              q"""try {
                 |    impl.${m.name.name}(${ctxArgPass}v);
                 |    return rt.pure<$baboonWiringError, $wireType>("null");
                 |} catch (ex: unknown) {
                 |    return rt.fail<$baboonWiringError, $wireType>({ tag: 'CallFailed', method, domainError: ex });
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
      }
    }

    private def generateErrorsJsonFn(
      service: Typedef.Service,
      svcType: TsValue.TsType,
      codec: TsCodecTranslator,
    ): TextTree[TsValue] = {
      val cases = service.methods.map {
        m =>
          generateErrorsMethodBody(
            m,
            codec,
            decodeRef => q"$decodeRef.instance.decode($tsBaboonCodecContext.Default, JSON.parse(data))",
            (encodeRef, _) => q"JSON.stringify($encodeRef.instance.encode($tsBaboonCodecContext.Default, v))",
            "string",
          )
      }.join("\n")

      q"""export ${asyncPrefix}function invokeJson_${svcType.name}(
         |    method: $baboonMethodId,
         |    data: string,
         |    impl: $svcType,
         |    rt: $ibaboonServiceRt,
         |    ${ctxParamDecl}ctx: $tsBaboonCodecContext
         |): $errorsJsonRetType {
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
      val cases = service.methods.map {
        m =>
          generateErrorsMethodBody(
            m,
            codec,
            decodeRef => q"(() => { const reader = new $tsBaboonBinReader(data); return $decodeRef.instance.decode(ctx, reader); })()",
            (encodeRef, _) => q"(() => { const writer = new $tsBaboonBinWriter(); $encodeRef.instance.encode(ctx, v, writer); return writer.toBytes(); })()",
            "Uint8Array",
          )
      }.join("\n")

      q"""export ${asyncPrefix}function invokeUeba_${svcType.name}(
         |    method: $baboonMethodId,
         |    data: Uint8Array,
         |    impl: $svcType,
         |    rt: $ibaboonServiceRt,
         |    ${ctxParamDecl}ctx: $tsBaboonCodecContext
         |): $errorsUebaRetType {
         |    switch (method.methodName) {
         |        ${cases.shift(8).trim}
         |        default:
         |            return rt.fail<$baboonWiringError, Uint8Array>({ tag: 'NoMatchingMethod', method });
         |    }
         |}""".stripMargin
    }
  }
}
