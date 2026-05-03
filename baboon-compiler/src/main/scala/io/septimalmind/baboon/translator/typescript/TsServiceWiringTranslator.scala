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

    // Wire-kind discriminator used by generateErrorsMethodBody.
    private sealed trait WireKind
    private object WireKind {
      case object Json extends WireKind
      case object Ueba extends WireKind
    }

    // JSON encode/decode for both User types (via generated codec) and BuiltinScalar (inline).
    private def jsonDecodeExpr(id: TypeId, wire: TextTree[TsValue]): TextTree[TsValue] = id match {
      case u: TypeId.User =>
        val tsType = typeTranslator.asTsType(u, domain, evo, tsFileTools.definitionsBasePkg)
        val codec  = codecs.collectFirst { case c: TsJsonCodecGenerator => c }.get.codecName(tsType)
        q"$codec.instance.decode($tsBaboonCodecContext.Default, $wire)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit => q"$wire as boolean"
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 | TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 | TypeId.Builtins.f32 |
              TypeId.Builtins.f64 =>
            q"$wire as number"
          case TypeId.Builtins.i64 | TypeId.Builtins.u64 => q"BigInt($wire as string)"
          case TypeId.Builtins.f128                      => q"$tsBaboonDecimal.fromString($wire as string)"
          case TypeId.Builtins.str | TypeId.Builtins.uid => q"$wire as string"
          case TypeId.Builtins.bytes                     => q"$tsBinTools.hexDecode($wire as string)"
          case TypeId.Builtins.tsu =>
            target.language.timestampsUtcMode match {
              case "string" => q"$wire as string"
              case "date"   => q"new Date($wire as string)"
              case _        => q"$tsBaboonDateTimeUtc.fromISO($wire as string)"
            }
          case TypeId.Builtins.tso =>
            target.language.timestampsOffsetMode match {
              case "string" => q"$wire as string"
              case "date"   => q"new Date($wire as string)"
              case _        => q"$tsBaboonDateTimeOffset.fromISO($wire as string)"
            }
          case other => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def jsonEncodeExpr(id: TypeId, value: TextTree[TsValue]): TextTree[TsValue] = id match {
      case u: TypeId.User =>
        val tsType = typeTranslator.asTsType(u, domain, evo, tsFileTools.definitionsBasePkg)
        val codec  = codecs.collectFirst { case c: TsJsonCodecGenerator => c }.get.codecName(tsType)
        q"$codec.instance.encode($tsBaboonCodecContext.Default, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.i64 | TypeId.Builtins.u64 => q"$value.toString()"
          case TypeId.Builtins.f128                      => q"$value.toString()"
          case TypeId.Builtins.bytes                     => q"$tsBinTools.hexEncode($value)"
          case TypeId.Builtins.tsu =>
            target.language.timestampsUtcMode match {
              case "string" => value
              case _        => q"$value.toISOString()"
            }
          case TypeId.Builtins.tso =>
            target.language.timestampsOffsetMode match {
              case "string" => value
              case _        => q"$value.toISOString()"
            }
          case _ => value
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaDecodeExpr(id: TypeId, reader: TextTree[TsValue]): TextTree[TsValue] = id match {
      case u: TypeId.User =>
        val tsType = typeTranslator.asTsType(u, domain, evo, tsFileTools.definitionsBasePkg)
        val codec  = codecs.collectFirst { case c: TsUEBACodecGenerator => c }.get.codecName(tsType)
        q"$codec.instance.decode(ctx, $reader)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$tsBinTools.readBool($reader)"
          case TypeId.Builtins.i08   => q"$tsBinTools.readI8($reader)"
          case TypeId.Builtins.i16   => q"$tsBinTools.readI16($reader)"
          case TypeId.Builtins.i32   => q"$tsBinTools.readI32($reader)"
          case TypeId.Builtins.i64   => q"$tsBinTools.readI64($reader)"
          case TypeId.Builtins.u08   => q"$tsBinTools.readU8($reader)"
          case TypeId.Builtins.u16   => q"$tsBinTools.readU16($reader)"
          case TypeId.Builtins.u32   => q"$tsBinTools.readU32($reader)"
          case TypeId.Builtins.u64   => q"$tsBinTools.readU64($reader)"
          case TypeId.Builtins.f32   => q"$tsBinTools.readF32($reader)"
          case TypeId.Builtins.f64   => q"$tsBinTools.readF64($reader)"
          case TypeId.Builtins.f128  => q"$tsBinTools.readDecimal($reader)"
          case TypeId.Builtins.str   => q"$tsBinTools.readString($reader)"
          case TypeId.Builtins.bytes => q"$tsBinTools.readBytes($reader)"
          case TypeId.Builtins.uid   => q"$tsBinTools.readUuid($reader)"
          case TypeId.Builtins.tsu =>
            target.language.timestampsUtcMode match {
              case "string" => q"$tsBinTools.readTimestampUtc($reader).toISOString()"
              case "date"   => q"$tsBinTools.readTimestampUtc($reader).date"
              case _        => q"$tsBinTools.readTimestampUtc($reader)"
            }
          case TypeId.Builtins.tso =>
            target.language.timestampsOffsetMode match {
              case "string" => q"$tsBinTools.readTimestampOffset($reader).toISOString()"
              case "date"   => q"$tsBinTools.readTimestampOffset($reader).date"
              case _        => q"$tsBinTools.readTimestampOffset($reader)"
            }
          case other => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaEncodeStmt(id: TypeId, writer: TextTree[TsValue], value: TextTree[TsValue]): TextTree[TsValue] = id match {
      case u: TypeId.User =>
        val tsType = typeTranslator.asTsType(u, domain, evo, tsFileTools.definitionsBasePkg)
        val codec  = codecs.collectFirst { case c: TsUEBACodecGenerator => c }.get.codecName(tsType)
        q"$codec.instance.encode(ctx, $value, $writer);"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$tsBinTools.writeBool($writer, $value);"
          case TypeId.Builtins.i08   => q"$tsBinTools.writeI8($writer, $value);"
          case TypeId.Builtins.i16   => q"$tsBinTools.writeI16($writer, $value);"
          case TypeId.Builtins.i32   => q"$tsBinTools.writeI32($writer, $value);"
          case TypeId.Builtins.i64   => q"$tsBinTools.writeI64($writer, $value);"
          case TypeId.Builtins.u08   => q"$tsBinTools.writeU8($writer, $value);"
          case TypeId.Builtins.u16   => q"$tsBinTools.writeU16($writer, $value);"
          case TypeId.Builtins.u32   => q"$tsBinTools.writeU32($writer, $value);"
          case TypeId.Builtins.u64   => q"$tsBinTools.writeU64($writer, $value);"
          case TypeId.Builtins.f32   => q"$tsBinTools.writeF32($writer, $value);"
          case TypeId.Builtins.f64   => q"$tsBinTools.writeF64($writer, $value);"
          case TypeId.Builtins.f128  => q"$tsBinTools.writeDecimal($writer, $value);"
          case TypeId.Builtins.str   => q"$tsBinTools.writeString($writer, $value);"
          case TypeId.Builtins.bytes => q"$tsBinTools.writeBytes($writer, $value);"
          case TypeId.Builtins.uid   => q"$tsBinTools.writeUuid($writer, $value);"
          case TypeId.Builtins.tsu =>
            target.language.timestampsUtcMode match {
              case "string" => q"$tsBinTools.writeTimestampUtc($writer, $tsBaboonDateTimeUtc.fromISO($value));"
              case "date"   => q"$tsBinTools.writeTimestampUtc($writer, $tsBaboonDateTimeUtc.fromDate($value));"
              case _        => q"$tsBinTools.writeTimestampUtc($writer, $value);"
            }
          case TypeId.Builtins.tso =>
            target.language.timestampsOffsetMode match {
              case "string" => q"$tsBinTools.writeTimestampOffset($writer, $tsBaboonDateTimeOffset.fromISO($value));"
              case "date"   => q"$tsBinTools.writeTimestampOffset($writer, $tsBaboonDateTimeOffset.fromISO($value.toISOString()));"
              case _        => q"$tsBinTools.writeTimestampOffset($writer, $value);"
            }
          case other => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

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
              val inType  = typeTranslator.asTsRef(m.sig, domain, evo, tsFileTools.definitionsBasePkg)
              val outType = m.out.map(typeTranslator.asTsRef(_, domain, evo, tsFileTools.definitionsBasePkg))
              val retType = outType.getOrElse(q"void")

              val jsonMethod = jsonCodec.map {
                _ =>
                  val encodeInExpr = jsonEncodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"arg")
                  val decodeOut = m.out match {
                    case Some(outRef) =>
                      val decExpr = jsonDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"JSON.parse(resp)")
                      q"return $decExpr as $retType;"
                    case None => q"return undefined as unknown as $retType;"
                  }
                  q"""public async ${m.name.name}Json(${ctxParamDecl}arg: $inType): Promise<$retType> {
                     |    const encoded = JSON.stringify($encodeInExpr);
                     |    const resp = await this.transportJson("${svcType.name}", "${m.name.name}", encoded);
                     |    ${decodeOut.shift(4).trim}
                     |}""".stripMargin
              }

              val uebaMethod = binCodec.map {
                _ =>
                  val decodeOut = m.out match {
                    case Some(outRef) =>
                      val decExpr = uebaDecodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"new $tsBaboonBinReader(resp)")
                      q"return $decExpr;"
                    case None => q"return undefined as unknown as $retType;"
                  }
                  q"""public async ${m.name.name}(${ctxParamDecl}arg: $inType, ctx: $tsBaboonCodecContext = $tsBaboonCodecContext.Default): Promise<$retType> {
                     |    const writer = new $tsBaboonBinWriter();
                     |    ${uebaEncodeStmt(m.sig.id.asInstanceOf[TypeId.Scalar], q"writer", q"arg")};
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
        if (activeJsonCodec(service).isDefined) Some(generateNoErrorsJsonFn(service, svcType))
        else None

      val uebaFn =
        if (activeBinCodec(service).isDefined) Some(generateNoErrorsUebaFn(service, svcType))
        else None

      Seq(jsonFn, uebaFn).flatten.joinNN()
    }

    private def noErrorsJsonRetType: String = if (isAsync) "Promise<string>" else "string"
    private def noErrorsUebaRetType: String = if (isAsync) "Promise<Uint8Array>" else "Uint8Array"

    private def generateNoErrorsJsonFn(
      service: Typedef.Service,
      svcType: TsValue.TsType,
    ): TextTree[TsValue] = {
      val cases = service.methods.map {
        m =>
          val decodeInExpr = jsonDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"JSON.parse(data)")

          val encodeAndReturn = m.out match {
            case Some(outRef) =>
              val encodeOutExpr = jsonEncodeExpr(outRef.id.asInstanceOf[TypeId.Scalar], q"result")
              q"""const result = ${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded);
                 |return JSON.stringify($encodeOutExpr);""".stripMargin
            case None =>
              q"""${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded);
                 |return "null";""".stripMargin
          }

          q"""case "${m.name.name}": {
             |    const decoded = $decodeInExpr;
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
    ): TextTree[TsValue] = {
      val cases = service.methods.map {
        m =>
          val decodeInExpr = uebaDecodeExpr(m.sig.id.asInstanceOf[TypeId.Scalar], q"reader")

          val encodeAndReturn = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id.asInstanceOf[TypeId.Scalar], q"writer", q"result")
              q"""const result = ${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded);
                 |const writer = new $tsBaboonBinWriter();
                 |$encStmt
                 |return writer.toBytes();""".stripMargin
            case None =>
              q"""${awaitPrefix}impl.${m.name.name}(${ctxArgPass}decoded);
                 |return new Uint8Array(0);""".stripMargin
          }

          q"""case "${m.name.name}": {
             |    const reader = new $tsBaboonBinReader(data);
             |    const decoded = $decodeInExpr;
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
        if (activeJsonCodec(service).isDefined) Some(generateErrorsJsonFn(service, svcType))
        else None

      val uebaFn =
        if (activeBinCodec(service).isDefined) Some(generateErrorsUebaFn(service, svcType))
        else None

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
      wk: WireKind,
      wireType: String,
    ): TextTree[TsValue] = {
      val inTypeRef  = typeTranslator.asTsType(m.sig.id, domain, evo, tsFileTools.definitionsBasePkg)
      val hasErrType = m.err.isDefined && !resolved.noErrors

      def mkDecode(id: TypeId): TextTree[TsValue] = wk match {
        case WireKind.Json => jsonDecodeExpr(id, q"JSON.parse(data)")
        case WireKind.Ueba => q"(() => { const reader = new $tsBaboonBinReader(data); return ${uebaDecodeExpr(id, q"reader")}; })()"
      }

      def mkEncode(id: TypeId, v: TextTree[TsValue]): TextTree[TsValue] = wk match {
        case WireKind.Json => q"JSON.stringify(${jsonEncodeExpr(id, v)})"
        case WireKind.Ueba => q"(() => { const writer = new $tsBaboonBinWriter(); ${uebaEncodeStmt(id, q"writer", v)} return writer.toBytes(); })()"
      }

      if (isAsync) {
        val decodeStep =
          q"""let decoded: $inTypeRef;
             |try {
             |    decoded = ${mkDecode(m.sig.id.asInstanceOf[TypeId.Scalar])};
             |} catch (ex: unknown) {
             |    return rt.fail<$baboonWiringError, $wireType>({ tag: 'DecoderFailed', method, error: ex });
             |}""".stripMargin

        val callAndEncodeStep = m.out match {
          case Some(outRef) =>
            val outTypeRef = typeTranslator.asTsType(outRef.id, domain, evo, tsFileTools.definitionsBasePkg)

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
                 |    return rt.pure<$baboonWiringError, $wireType>(${mkEncode(outRef.id.asInstanceOf[TypeId.Scalar], q"output")});
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
                 |    return rt.pure<$baboonWiringError, $wireType>(${mkEncode(outRef.id.asInstanceOf[TypeId.Scalar], q"output")});
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
             |    input = rt.pure<$baboonWiringError, $inTypeRef>(${mkDecode(m.sig.id.asInstanceOf[TypeId.Scalar])});
             |} catch (ex: unknown) {
             |    input = rt.fail<$baboonWiringError, $inTypeRef>({ tag: 'DecoderFailed', method, error: ex });
             |}""".stripMargin

        val callAndEncodeStep = m.out match {
          case Some(outRef) =>
            val outTypeRef = typeTranslator.asTsType(outRef.id, domain, evo, tsFileTools.definitionsBasePkg)

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
               |        return rt.pure<$baboonWiringError, $wireType>(${mkEncode(outRef.id.asInstanceOf[TypeId.Scalar], q"v")});
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
    ): TextTree[TsValue] = {
      val cases = service.methods.map {
        m =>
          generateErrorsMethodBody(m, WireKind.Json, "string")
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
    ): TextTree[TsValue] = {
      val cases = service.methods.map {
        m =>
          generateErrorsMethodBody(m, WireKind.Ueba, "Uint8Array")
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
