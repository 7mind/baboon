package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.CompilerTarget.DtTarget
import io.septimalmind.baboon.translator.{ResolvedServiceContext, ResolvedServiceResult, ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait DtServiceWiringTranslator {
  def translate(defn: DomainMember.User): Option[TextTree[DtValue]]
  def translateClient(defn: DomainMember.User): Option[TextTree[DtValue]]
  def translateServiceRt(domain: Domain): Option[TextTree[DtValue]]

  /** Generic clause (`<Ctx>`) appended to the service-interface class name when
    * an `abstract` service-context mode is active; empty otherwise (incl. `none`
    * and concrete `type` mode). */
  def serviceInterfaceTypeParam: String

  /** Leading service-context method parameter declaration (e.g. `Ctx ctx, `)
    * spliced before each interface method's `arg`; empty in `none` mode. */
  def serviceMethodCtxParam: String
}

object DtServiceWiringTranslator {
  class Impl(
    target: DtTarget,
    trans: DtTypeTranslator,
    codecs: Set[DtCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
  ) extends DtServiceWiringTranslator {

    // When the Dart `asyncServices` flag is on, the service interface methods
    // return `Future<T>` (DtDefnTranslator, T80) and the always-async Dart
    // client already awaits the transport. The server dispatchers
    // `invokeJson`/`invokeUeba` must therefore `await` the now-`Future` impl
    // call and become `async`/`Future`-returning themselves. The runtime
    // `IBaboon*Service<R>.invoke` contract (a static resource) is unchanged —
    // asyncness is absorbed into the wrapper's `R` type parameter, which
    // becomes `Future<base>`, so the `invoke` override simply returns the
    // `Future` produced by the async dispatcher. Every helper below is a no-op
    // when the flag is off, keeping that output byte-identical to HEAD.
    private val isAsync: Boolean = target.language.asyncServices

    // `async` modifier on the static dispatcher and on each per-method handler
    // closure; `await` prefix on the impl call. Empty in sync mode.
    private val dispatcherAsyncKw: String = if (isAsync) " async" else ""
    private val awaitKw: String           = if (isAsync) "await " else ""

    // The wrapper's `R` type parameter / the static dispatcher's return type:
    // `Future<base>` in async mode (preserving the synchronous runtime
    // `IBaboon*Service.invoke` contract), identity otherwise.
    private def asyncRetType(base: TextTree[DtValue]): TextTree[DtValue] =
      if (isAsync) q"Future<$base>" else base

    private val resolved: ResolvedServiceResult =
      ServiceResultResolver.resolve(domain, "dart", target.language.serviceResult, target.language.pragmas)

    private val resolvedCtx: ResolvedServiceContext =
      ServiceContextResolver.resolve(domain, "dart", target.language.serviceContext, target.language.pragmas)

    private def hasActiveJsonCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.id == "Json" && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def hasActiveUebaCodecs(service: Typedef.Service): Boolean = {
      codecs.exists {
        c =>
          c.id == "Ueba" && service.methods.forall {
            m =>
              c.isActive(m.sig.id) && m.out.forall(o => c.isActive(o.id))
          }
      }
    }

    private def jsonCodecName(typeId: TypeId.User): DtValue.DtType = {
      val srcRef       = trans.toDtTypeRefKeepForeigns(typeId, domain, evo)
      val baseFileName = srcRef.importAs.getOrElse(trans.toSnakeCase(srcRef.name))
      DtValue.DtType(srcRef.pkg, s"${srcRef.name}_JsonCodec", srcRef.fq, importAs = Some(baseFileName))
    }

    private def uebaCodecName(typeId: TypeId.User): DtValue.DtType = {
      val srcRef       = trans.toDtTypeRefKeepForeigns(typeId, domain, evo)
      val baseFileName = srcRef.importAs.getOrElse(trans.toSnakeCase(srcRef.name))
      DtValue.DtType(srcRef.pkg, s"${srcRef.name}_UebaCodec", srcRef.fq, importAs = Some(baseFileName))
    }

    // JSON encode/decode for both User types (via generated codec) and BuiltinScalar (inline).
    // wire is a Dart dynamic (json-decoded), encode returns a Dart dynamic.
    private def jsonDecodeExpr(id: TypeId, wire: TextTree[DtValue]): TextTree[DtValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.instance.decode($codecCtxRef, $wire)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                                             => q"$wire as bool"
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 => q"($wire as num).toInt()"
          case TypeId.Builtins.i64                                             => q"($wire is String ? int.parse($wire as String) : ($wire as num).toInt())"
          case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 => q"($wire as num).toInt()"
          case TypeId.Builtins.u64                       => q"($wire is String ? BigInt.parse($wire as String).toSigned(64).toInt() : ($wire as num).toInt())"
          case TypeId.Builtins.f32 | TypeId.Builtins.f64 => q"($wire as num).toDouble()"
          case TypeId.Builtins.f128                      => q"$baboonDecimal($wire is String ? $wire as String : $wire.toString())"
          case TypeId.Builtins.str                       => q"$wire as String"
          case TypeId.Builtins.uid                       => q"$wire as String"
          case TypeId.Builtins.bytes                     => q"$baboonByteStringTools.fromHexString($wire as String)"
          case TypeId.Builtins.tsu                       => q"$baboonTimeFormats.parseUtc($wire as String)"
          case TypeId.Builtins.tso                       => q"$baboonTimeFormats.parseOffset($wire as String)"
          case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def jsonEncodeExpr(id: TypeId, value: TextTree[DtValue]): TextTree[DtValue] = id match {
      case u: TypeId.User => q"${jsonCodecName(u)}.instance.encode($codecCtxRef, $value)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit                                             => q"$value"
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 => q"$value"
          case TypeId.Builtins.i64                                             => q"$value.toString()"
          case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 => q"$value"
          case TypeId.Builtins.u64                                             => q"BigInt.from($value).toUnsigned(64).toString()"
          case TypeId.Builtins.f32 | TypeId.Builtins.f64                       => q"$value"
          case TypeId.Builtins.f128                                            => q"$value.value"
          case TypeId.Builtins.str                                             => q"$value"
          case TypeId.Builtins.uid                                             => q"$value"
          case TypeId.Builtins.bytes                                           => q"$value.toHexString()"
          case TypeId.Builtins.tsu                                             => q"$baboonTimeFormats.formatUtc($value)"
          case TypeId.Builtins.tso                                             => q"$baboonTimeFormats.formatOffset($value)"
          case other                                                           => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaDecodeExpr(id: TypeId, reader: TextTree[DtValue]): TextTree[DtValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.decode($codecCtxRef, $reader)"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$reader.readBool()"
          case TypeId.Builtins.i08   => q"$reader.readI8()"
          case TypeId.Builtins.i16   => q"$reader.readI16()"
          case TypeId.Builtins.i32   => q"$reader.readI32()"
          case TypeId.Builtins.i64   => q"$reader.readI64()"
          case TypeId.Builtins.u08   => q"$reader.readU8()"
          case TypeId.Builtins.u16   => q"$reader.readU16()"
          case TypeId.Builtins.u32   => q"$reader.readU32()"
          case TypeId.Builtins.u64   => q"$reader.readU64()"
          case TypeId.Builtins.f32   => q"$reader.readF32()"
          case TypeId.Builtins.f64   => q"$reader.readF64()"
          case TypeId.Builtins.f128  => q"$reader.readDecimal()"
          case TypeId.Builtins.str   => q"$reader.readString()"
          case TypeId.Builtins.bytes => q"$reader.readBytes()"
          case TypeId.Builtins.uid   => q"$reader.readUuid()"
          case TypeId.Builtins.tsu   => q"$reader.readTsu()"
          case TypeId.Builtins.tso   => q"$reader.readTso()"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
        }
      case other => throw new RuntimeException(s"BUG: Non-scalar type in service wiring: $other")
    }

    private def uebaEncodeStmt(id: TypeId, writer: TextTree[DtValue], value: TextTree[DtValue]): TextTree[DtValue] = id match {
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.encode($codecCtxRef, $writer, $value);"
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit   => q"$writer.writeBool($value);"
          case TypeId.Builtins.i08   => q"$writer.writeI8($value);"
          case TypeId.Builtins.i16   => q"$writer.writeI16($value);"
          case TypeId.Builtins.i32   => q"$writer.writeI32($value);"
          case TypeId.Builtins.i64   => q"$writer.writeI64($value);"
          case TypeId.Builtins.u08   => q"$writer.writeU8($value);"
          case TypeId.Builtins.u16   => q"$writer.writeU16($value);"
          case TypeId.Builtins.u32   => q"$writer.writeU32($value);"
          case TypeId.Builtins.u64   => q"$writer.writeU64($value);"
          case TypeId.Builtins.f32   => q"$writer.writeF32($value);"
          case TypeId.Builtins.f64   => q"$writer.writeF64($value);"
          case TypeId.Builtins.f128  => q"$writer.writeDecimal($value);"
          case TypeId.Builtins.str   => q"$writer.writeString($value);"
          case TypeId.Builtins.bytes => q"$writer.writeBytes($value);"
          case TypeId.Builtins.uid   => q"$writer.writeUuid($value);"
          case TypeId.Builtins.tsu   => q"$writer.writeTsu($value);"
          case TypeId.Builtins.tso   => q"$writer.writeTso($value);"
          case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
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

    override def translateServiceRt(domain: Domain): Option[TextTree[DtValue]] = {
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

      val rtTrait: TextTree[DtValue] =
        q"""abstract class IBaboonServiceRt$traitTypeParams {
           |  ${ct("L", "R")} pure<L, R>(R value);
           |  ${ct("L", "R")} fail<L, R>(L error);
           |  ${ct("C", "B")} leftMap<A, B, C>(${ct("A", "B")} value, C Function(A) f);
           |  ${ct("A", "C")} flatMap<A, B, C>(${ct("A", "B")} value, ${ct("A", "C")} Function(B) f);
           |}""".stripMargin

      Some(rtTrait)
    }

    override def translate(defn: DomainMember.User): Option[TextTree[DtValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          // When neither codec is active for any method on this service, both
          // the per-domain wiring class and the muxer-entry wrapper classes
          // would be empty — skip emission entirely so the service file stays
          // a clean abstract-interface declaration.
          if (!hasActiveJsonCodecs(service) && !hasActiveUebaCodecs(service)) None
          // Errors-mode wiring emission for Dart relies on `IBaboonServiceRt`
          // and FQ wire-error type names that the existing renderer does not
          // import-trigger; the dispatcher in this mode pre-dates the muxer
          // and is currently dead code (not invoked from any DefnTranslator
          // path). Keep it dead in errors mode so we don't introduce a
          // compile-time regression while shipping the no-errors muxer
          // wrappers. The errors-mode dispatcher fix is a separate PR.
          else if (!resolved.noErrors) None
          else Some(generateNoErrorsWiring(service))
        case _ => None
      }
    }

    // ========== Client stub generation ==========
    //
    // The client is independent of the errors/result axis (it never threads
    // `IBaboonServiceRt`): it only encodes the input, hands the wire bytes/string
    // to a user-supplied transport callback, and decodes the response. Dart is
    // inherently async, so every method returns `Future<T>` and awaits the
    // transport. The UEBA method carries the bare endpoint name; the JSON
    // method always carries a `Json` suffix — even when only JSON codecs are
    // active — to match the naming convention used by the other 8 backends.
    override def translateClient(defn: DomainMember.User): Option[TextTree[DtValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val svcName = service.id.name.name
          val hasUeba = hasActiveUebaCodecs(service)
          val hasJson = hasActiveJsonCodecs(service)
          if (!hasUeba && !hasJson) return None

          val clientMethods = service.methods.flatMap {
            m =>
              val dartMethodName = trans.escapeDartKeyword(m.name.name)
              val inTypeRef      = trans.asDtRef(m.sig, domain, evo)
              val retType        = m.out.map(o => trans.asDtRef(o, domain, evo)).getOrElse(q"void")

              val uebaMethod = if (hasUeba) {
                val encodeIn = uebaEncodeStmt(m.sig.id, q"writer", q"arg")
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val reader     = q"$baboonBinTools.createReader(resp)"
                    val decodeExpr = uebaDecodeExpr(outRef.id, reader)
                    q"return $decodeExpr;"
                  case None => q"return;"
                }
                Some(
                  q"""Future<$retType> $dartMethodName($ctxParamDecl$inTypeRef arg, [$baboonCodecContext $codecCtxName = $baboonCodecContext.defaultCtx]) async {
                     |  final writer = $baboonBinTools.createWriter();
                     |  $encodeIn
                     |  final resp = await _transportUeba($ctxArgPass'$svcName', '${m.name.name}', writer.toBytes());
                     |  ${decodeOut.shift(2).trim}
                     |}""".stripMargin
                )
              } else None

              val jsonMethodName = s"${dartMethodName}Json"
              val jsonMethod = if (hasJson) {
                val encodeIn = jsonEncodeExpr(m.sig.id, q"arg")
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val decodeExpr = jsonDecodeExpr(outRef.id, q"$dtJsonDecode(resp)")
                    q"return $decodeExpr;"
                  case None => q"return;"
                }
                Some(
                  q"""Future<$retType> $jsonMethodName($ctxParamDecl$inTypeRef arg, [$baboonCodecContext $codecCtxName = $baboonCodecContext.defaultCtx]) async {
                     |  final encoded = $dtJsonEncode($encodeIn);
                     |  final resp = await _transportJson($ctxArgPass'$svcName', '${m.name.name}', encoded);
                     |  ${decodeOut.shift(2).trim}
                     |}""".stripMargin
                )
              } else None

              uebaMethod.toList ++ jsonMethod.toList
          }

          if (clientMethods.isEmpty) return None

          // The abstract service context is forwarded as the leading argument of
          // the transport callback, so its function-type gains a matching leading
          // parameter. In `none`/concrete mode this is empty and the transport
          // signature is unchanged (byte-identical for `none`).
          val transportCtxParam: String = resolvedCtx match {
            case ResolvedServiceContext.NoContext               => ""
            case ResolvedServiceContext.AbstractContext(tn, _)  => s"$tn, "
            case ResolvedServiceContext.ConcreteContext(tn, _)  => s"$tn, "
          }

          val transportFields = List(
            if (hasUeba) Some(q"final Future<$dtUint8List> Function(${transportCtxParam}String service, String method, $dtUint8List data) _transportUeba;") else None,
            if (hasJson) Some(q"final Future<String> Function(${transportCtxParam}String service, String method, String data) _transportJson;") else None,
          ).flatten

          val ctorParams = List(
            if (hasUeba) Some(q"this._transportUeba") else None,
            if (hasJson) Some(q"this._transportJson") else None,
          ).flatten

          Some(
            q"""class ${svcName}Client$ctxTypeParamDecl {
               |  ${transportFields.joinN().shift(2).trim}
               |
               |  ${svcName}Client(${ctorParams.join(", ")});
               |
               |  ${clientMethods.join("\n\n").shift(2).trim}
               |}""".stripMargin
          )
        case _ => None
      }
    }

    private def ctxParamDecl: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext               => ""
      case ResolvedServiceContext.AbstractContext(tn, pn) => s"$tn $pn, "
      case ResolvedServiceContext.ConcreteContext(tn, pn) => s"$tn $pn, "
    }

    private def ctxArgPass: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext              => ""
      case ResolvedServiceContext.AbstractContext(_, pn) => s"$pn, "
      case ResolvedServiceContext.ConcreteContext(_, pn) => s"$pn, "
    }

    // Only `abstract` mode introduces a generic type parameter on the generated
    // service interface / client / wrapper. `type` (concrete) mode pins the
    // context to a concrete type, and `none` has no context — both empty here.
    private def ctxTypeName: Option[String] = resolvedCtx match {
      case ResolvedServiceContext.AbstractContext(tn, _) => Some(tn)
      case _                                             => None
    }
    private def ctxTypeParamDecl: String = ctxTypeName.fold("")(tn => s"<$tn>")

    override def serviceInterfaceTypeParam: String = ctxTypeParamDecl
    override def serviceMethodCtxParam: String     = ctxParamDecl

    // Codec-context parameter name. When a service context is active its
    // parameter name (default `ctx`) collides with the codec-context parameter
    // (historically also `ctx`), so the codec context is renamed away. In `none`
    // mode the name stays `ctx`, keeping that output byte-identical.
    private def codecCtxName: String = resolvedCtx match {
      case ResolvedServiceContext.NoContext => "ctx"
      case _                                => if (ctxArgPass.startsWith("codecCtx")) "baboonCodecCtx" else "codecCtx"
    }
    private def codecCtxRef: TextTree[DtValue] = TextTree.text[DtValue](codecCtxName)

    private def renderFq(tree: TextTree[DtValue]): String = tree.mapRender {
      case t: DtValue.DtType     => if (t.predef) trans.escapeDartKeyword(t.name) else (t.pkg.parts :+ trans.escapeDartKeyword(t.name)).mkString(".")
      case t: DtValue.DtTypeName => trans.escapeDartKeyword(t.name)
    }

    private def generateNoErrorsWiring(service: Typedef.Service): TextTree[DtValue] = {
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

      val wiringClass =
        q"""class ${svcName}Wiring {
           |  ${methods.shift(2).trim}
           |}""".stripMargin

      val wrappers = generateServiceWrappers(service, asyncRetType(noErrorsJsonRetTree), asyncRetType(noErrorsUebaRetTree))

      Seq(wiringClass, wrappers).filterNot(_.isEmpty).join("\n\n")
    }

    // Wrapper retType as a TextTree (not a String) so the muxer-wrapper's
    // `IBaboon*Service<R>` parameterisation can reference `BaboonWiringError`
    // through the symbol-tracked DtType — `renderFq` would render it as
    // `baboon.runtime.shared.BaboonWiringError`, which is not a valid Dart
    // identifier path (Dart imports the type bare).
    private def noErrorsJsonRetTree: TextTree[DtValue] = q"String"
    private def noErrorsUebaRetTree: TextTree[DtValue] = q"$dtUint8List"

    private def generateNoErrorsJsonMethod(service: Typedef.Service): TextTree[DtValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val dartMethodName = trans.escapeDartKeyword(m.name.name)
          val decodeIn       = jsonDecodeExpr(m.sig.id, q"wire")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encodeOut = jsonEncodeExpr(outRef.id, q"result")
              q"""final encoded = $encodeOut;
                 |return $dtJsonEncode(encoded);""".stripMargin
            case None =>
              q"""return 'null';"""
          }

          val callExpr = m.out match {
            case Some(_) => q"final result = ${awaitKw}impl.$dartMethodName(${ctxArgPass}decoded);"
            case None    => q"${awaitKw}impl.$dartMethodName(${ctxArgPass}decoded);"
          }

          q"""'${m.name.name}': ()$dispatcherAsyncKw {
             |  final wire = $dtJsonDecode(data);
             |  final decoded = $decodeIn;
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}
             |},""".stripMargin
      }.join("\n")

      val retType     = asyncRetType(q"String")
      val handlerType = asyncRetType(q"String")
      q"""static $retType invokeJson$ctxTypeParamDecl(
         |  $baboonMethodId method,
         |  String data,
         |  $svcName$ctxTypeParamDecl impl,
         |  $ctxParamDecl$baboonCodecContext $codecCtxName)$dispatcherAsyncKw {
         |  final handlers = <String, $handlerType Function()>{
         |    ${cases.shift(4).trim}
         |  };
         |  final handler = handlers[method.methodName];
         |  if (handler == null) {
         |    throw $baboonWiringException($baboonWiringError.noMatchingMethod(method));
         |  }
         |  return ${awaitKw}handler();
         |}""".stripMargin
    }

    private def generateNoErrorsUebaMethod(service: Typedef.Service): TextTree[DtValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val dartMethodName = trans.escapeDartKeyword(m.name.name)
          val decodeIn       = uebaDecodeExpr(m.sig.id, q"reader")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id, q"writer", q"result")
              q"""final writer = $baboonBinTools.createWriter();
                 |$encStmt
                 |return writer.toBytes();""".stripMargin
            case None =>
              q"return $dtUint8List(0);"
          }

          val callExpr = m.out match {
            case Some(_) => q"final result = ${awaitKw}impl.$dartMethodName(${ctxArgPass}decoded);"
            case None    => q"${awaitKw}impl.$dartMethodName(${ctxArgPass}decoded);"
          }

          q"""'${m.name.name}': ()$dispatcherAsyncKw {
             |  final reader = $baboonBinTools.createReader(data);
             |  final decoded = $decodeIn;
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}
             |},""".stripMargin
      }.join("\n")

      val retType     = asyncRetType(q"$dtUint8List")
      val handlerType = asyncRetType(q"$dtUint8List")
      q"""static $retType invokeUeba$ctxTypeParamDecl(
         |  $baboonMethodId method,
         |  $dtUint8List data,
         |  $svcName$ctxTypeParamDecl impl,
         |  $ctxParamDecl$baboonCodecContext $codecCtxName)$dispatcherAsyncKw {
         |  final handlers = <String, $handlerType Function()>{
         |    ${cases.shift(4).trim}
         |  };
         |  final handler = handlers[method.methodName];
         |  if (handler == null) {
         |    throw $baboonWiringException($baboonWiringError.noMatchingMethod(method));
         |  }
         |  return ${awaitKw}handler();
         |}""".stripMargin
    }

    private def generateErrorsWiring(service: Typedef.Service): TextTree[DtValue] = {
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

      val wiringClass =
        q"""class ${svcName}Wiring {
           |  ${methods.shift(2).trim}
           |}""".stripMargin

      val wrappers = generateServiceWrappers(service, errorsJsonRetTree, errorsUebaRetTree)

      Seq(wiringClass, wrappers).filterNot(_.isEmpty).join("\n\n")
    }

    // TextTree-form errors-mode return type for the muxer-wrapper signatures.
    // We need a TextTree (not a String) so the wrapper's
    // `IBaboon*Service<R>` parameterisation can reference `BaboonWiringError`
    // through the symbol-tracked DtType — `renderFq` would render it as
    // `baboon.runtime.shared.BaboonWiringError`, which is not a valid Dart
    // identifier path (Dart imports the type bare).
    //
    // These helpers are kept for forward-compat: the errors-mode dispatcher in
    // Dart is currently disabled at the `translate` entry point pending its
    // own fix (see comment there), so they're presently unreachable.
    private def errorsJsonRetTree: TextTree[DtValue] = errorsRetTreeFor(q"String")
    private def errorsUebaRetTree: TextTree[DtValue] = errorsRetTreeFor(q"$dtUint8List")

    private def errorsRetTreeFor(successType: TextTree[DtValue]): TextTree[DtValue] = {
      // Split `resolved.pattern` (e.g. `<$error, $success>`) around the two
      // placeholders so we can splice the symbol-tracked `BaboonWiringError`
      // (`$baboonWiringError`) and the success-type TextTree.
      val p           = resolved.pattern.get
      val typeName    = resolved.hkt.map(_.name).getOrElse(resolved.resultType.get)
      // Order-sensitive: pattern always references $error then $success in the
      // pattern shipped with the brief example. If reversed in user pragmas
      // we fall back to a literal substitution that keeps the order-of-args
      // intact.
      val errorIdx    = p.indexOf("$error")
      val successIdx  = p.indexOf("$success")
      if (errorIdx >= 0 && successIdx > errorIdx) {
        val pre  = p.substring(0, errorIdx)
        val mid  = p.substring(errorIdx + "$error".length, successIdx)
        val post = p.substring(successIdx + "$success".length)
        q"$typeName$pre$baboonWiringError$mid$successType$post"
      } else {
        // Defensive fallback — render via the existing string path.
        q"${ct(bweFq, successType.mapRender { case _ => "" })}"
      }
    }

    /** Emits the cross-domain Muxer-entry wrapper classes for a service.
      *
      * Each wrapper implements `IBaboonJsonService<R>` / `IBaboonUebaService<R>`
      * with R matching the underlying `${svc}Wiring.invokeJson`/`invokeUeba`
      * static-method return type (raw wire-type in noErrors mode,
      * result-container-wrapped in errors mode). Extra dependencies (`rt` in
      * errors mode and any service-context parameter) are baked at
      * construction time so the runtime `IBaboon*Service.invoke(method, data,
      * ctx)` contract stays codec-flavour-symmetric and language-uniform.
      *
      * Under `--dt-async-services` the underlying `invokeJson`/`invokeUeba`
      * static dispatchers are `async`/`Future`-returning (they `await` the
      * now-`Future` impl), so the wrapper's R becomes `Future<…>` — the
      * synchronous `IBaboon*Service.invoke` runtime contract is unchanged; the
      * `invoke` override just returns that `Future`. With the flag off the
      * wrapper's R is the underlying static-method's return type directly (no
      * Future wrapping), keeping that output byte-identical to HEAD.
      */
    private def generateServiceWrappers(
      service: Typedef.Service,
      jsonRetType: TextTree[DtValue],
      uebaRetType: TextTree[DtValue],
    ): TextTree[DtValue] = {
      val parts = Seq(
        if (hasActiveJsonCodecs(service)) Some(generateOneWrapper(service, isJson = true, jsonRetType)) else None,
        if (hasActiveUebaCodecs(service)) Some(generateOneWrapper(service, isJson = false, uebaRetType)) else None,
      ).flatten
      if (parts.isEmpty) q"" else parts.join("\n\n")
    }

    private def generateOneWrapper(
      service: Typedef.Service,
      isJson: Boolean,
      retType: TextTree[DtValue],
    ): TextTree[DtValue] = {
      val svcName     = service.id.name.name
      val wireType    = if (isJson) q"String"        else q"$dtUint8List"
      val invokerFn   = if (isJson) "invokeJson"     else "invokeUeba"
      val wrapperName = s"$svcName${if (isJson) "JsonService" else "UebaService"}"

      // The service context (when active) is supplied PER-INVOKE rather than
      // baked into the constructor, so the wrapper implements the
      // context-carrying IBaboon*ServiceCtx<Ctx, R> contract; `rt` (errors mode)
      // remains a per-construction field. In `none` mode the wrapper is
      // unchanged: it implements the context-free IBaboon*Service<R> and takes
      // `invoke(method, data, ctx)` — keeping that output byte-identical.
      val svcCtxTypeName: Option[String] = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => None
        case ResolvedServiceContext.AbstractContext(tn, _)  => Some(tn)
        case ResolvedServiceContext.ConcreteContext(tn, _)  => Some(tn)
      }
      val svcCtxArgName: Option[String] = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => None
        case ResolvedServiceContext.AbstractContext(_, pn)  => Some(pn)
        case ResolvedServiceContext.ConcreteContext(_, pn)  => Some(pn)
      }

      val implementsClause: TextTree[DtValue] = svcCtxTypeName match {
        case None     => q"${if (isJson) baboonJsonServiceIface else baboonUebaServiceIface}<$retType>"
        case Some(tn) => q"${if (isJson) baboonJsonServiceCtxIface else baboonUebaServiceCtxIface}<$tn, $retType>"
      }

      // `rt` (errors mode) is a per-construction dependency, so it stays a field;
      // the service context is now a per-invoke argument.
      val rtField: Option[(String, TextTree[DtValue])] =
        if (resolved.noErrors) None else Some(("rt", q"IBaboonServiceRt"))

      val implField: TextTree[DtValue]  = q"final $svcName$ctxTypeParamDecl _impl;"
      val extraFields: List[TextTree[DtValue]] = rtField.toList.map {
        case (name, tpe) => q"final $tpe _$name;"
      }

      // Per-field initializers in Dart's `this._x` constructor form keep the
      // fields final without an extra body, so the wrapper stays const-friendly
      // and the analyzer doesn't warn about unused setters.
      val ctorImplParam: TextTree[DtValue] = q"this._impl"
      val ctorExtraParams: List[TextTree[DtValue]] = rtField.toList.map {
        case (name, _) => q"this._$name"
      }
      val ctorParamList: TextTree[DtValue] =
        (ctorImplParam :: ctorExtraParams).join(", ")

      // Underlying static-method call: positional args match the
      // ${Svc}Wiring.invoke<Json|Ueba>(method, data, impl, [rt], [svcCtx], codecCtx)
      // signature emitted by generateNoErrors*Method / generateErrors*Method.
      // The service context (`ctx`) and codec context (`codecCtx`) are the
      // per-invoke parameters in context-active mode; in `none` mode the single
      // `ctx` is the codec context.
      val invokerArgs: List[TextTree[DtValue]] = {
        val base    = List[TextTree[DtValue]](q"method", q"data", q"_impl")
        val withRt  = rtField.fold(base)(_ => base :+ q"_rt")
        val withSvc = svcCtxArgName.fold(withRt)(pn => withRt :+ TextTree.text[DtValue](pn))
        withSvc :+ codecCtxRef
      }

      val allFields = (implField :: extraFields).join("\n")

      val invokeSig: TextTree[DtValue] = svcCtxTypeName match {
        case None     => q"$retType invoke($baboonMethodId method, $wireType data, $baboonCodecContext ctx)"
        case Some(tn) => q"$retType invoke($baboonMethodId method, $wireType data, $tn ${svcCtxArgName.get}, $baboonCodecContext $codecCtxName)"
      }

      q"""class $wrapperName$ctxTypeParamDecl implements $implementsClause {
         |  @override
         |  String get serviceName => '$svcName';
         |
         |  ${allFields.shift(2).trim}
         |
         |  $wrapperName($ctorParamList);
         |
         |  @override
         |  ${invokeSig.trim} {
         |    return ${svcName}Wiring.$invokerFn${if (svcCtxTypeName.isDefined) ctxTypeParamDecl else ""}(${invokerArgs.join(", ")});
         |  }
         |}""".stripMargin
    }

    private val bweFq: String = renderFq(q"$baboonWiringError")

    private def ct(error: String, success: String): String = renderContainer(error, success)

    private def generateErrorsJsonMethod(service: Typedef.Service): TextTree[DtValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "String")

      val cases = service.methods.map {
        m =>
          val dartMethodName = trans.escapeDartKeyword(m.name.name)
          val inRef          = trans.asDtRef(m.sig, domain, evo)
          val decodeIn       = jsonDecodeExpr(m.sig.id, q"wire")

          val decodeStep =
            q"""${ct(bweFq, renderFq(inRef))} input;
               |try {
               |  final wire = $dtJsonDecode(data);
               |  input = rt.pure($decodeIn);
               |} catch (ex) {
               |  input = rt.fail($baboonWiringError.decoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val encodeOut = jsonEncodeExpr(outRef.id, q"v")

              val callBody = if (hasErrType) {
                q"""try {
                   |  final callResult = impl.$dartMethodName(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  return rt.pure(impl.$dartMethodName(${ctxArgPass}v));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              }

              q"""final output = rt.flatMap(input, (v) {
                 |  ${callBody.shift(2).trim}
                 |});
                 |return rt.flatMap(output, (v) {
                 |  try {
                 |    final encoded = $encodeOut;
                 |    return rt.pure($dtJsonEncode(encoded));
                 |  } catch (ex) {
                 |    return rt.fail($baboonWiringError.encoderFailed(method, ex));
                 |  }
                 |});""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                q"""try {
                   |  final callResult = impl.$dartMethodName(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  impl.$dartMethodName(${ctxArgPass}v);
                   |  return rt.pure(null);
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              }

              q"""return rt.flatMap(input, (v) {
                 |  ${callBody.shift(2).trim}
                 |  return rt.pure('null');
                 |});""".stripMargin
          }

          q"""'${m.name.name}': () {
             |  ${decodeStep.shift(2).trim}
             |  ${callAndEncodeStep.shift(2).trim}
             |},""".stripMargin
      }.join("\n")

      q"""static $wiringRetType invokeJson$ctxTypeParamDecl(
         |  $baboonMethodId method,
         |  String data,
         |  $svcName$ctxTypeParamDecl impl,
         |  IBaboonServiceRt rt,
         |  $ctxParamDecl$baboonCodecContext $codecCtxName) {
         |  final handlers = <String, $wiringRetType Function()>{
         |    ${cases.shift(4).trim}
         |  };
         |  final handler = handlers[method.methodName];
         |  if (handler == null) {
         |    return rt.fail($baboonWiringError.noMatchingMethod(method));
         |  }
         |  return handler();
         |}""".stripMargin
    }

    private def generateErrorsUebaMethod(service: Typedef.Service): TextTree[DtValue] = {
      val svcName       = service.id.name.name
      val wiringRetType = ct(bweFq, "Uint8List")

      val cases = service.methods.map {
        m =>
          val dartMethodName = trans.escapeDartKeyword(m.name.name)
          val inRef          = trans.asDtRef(m.sig, domain, evo)
          val decodeIn       = uebaDecodeExpr(m.sig.id, q"reader")

          val decodeStep =
            q"""${ct(bweFq, renderFq(inRef))} input;
               |try {
               |  final reader = $baboonBinTools.createReader(data);
               |  input = rt.pure($decodeIn);
               |} catch (ex) {
               |  input = rt.fail($baboonWiringError.decoderFailed(method, ex));
               |}""".stripMargin

          val hasErrType = m.err.isDefined && !resolved.noErrors

          val callAndEncodeStep = m.out match {
            case Some(outRef) =>
              val encStmt = uebaEncodeStmt(outRef.id, q"writer", q"v")

              val callBody = if (hasErrType) {
                q"""try {
                   |  final callResult = impl.$dartMethodName(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  return rt.pure(impl.$dartMethodName(${ctxArgPass}v));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              }

              q"""final output = rt.flatMap(input, (v) {
                 |  ${callBody.shift(2).trim}
                 |});
                 |return rt.flatMap(output, (v) {
                 |  try {
                 |    final writer = $baboonBinTools.createWriter();
                 |    $encStmt
                 |    return rt.pure(writer.toBytes());
                 |  } catch (ex) {
                 |    return rt.fail($baboonWiringError.encoderFailed(method, ex));
                 |  }
                 |});""".stripMargin

            case None =>
              val callBody = if (hasErrType) {
                q"""try {
                   |  final callResult = impl.$dartMethodName(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  impl.$dartMethodName(${ctxArgPass}v);
                   |  return rt.pure(null);
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              }

              q"""return rt.flatMap(input, (v) {
                 |  ${callBody.shift(2).trim}
                 |  return rt.pure($dtUint8List(0));
                 |});""".stripMargin
          }

          q"""'${m.name.name}': () {
             |  ${decodeStep.shift(2).trim}
             |  ${callAndEncodeStep.shift(2).trim}
             |},""".stripMargin
      }.join("\n")

      q"""static $wiringRetType invokeUeba$ctxTypeParamDecl(
         |  $baboonMethodId method,
         |  $dtUint8List data,
         |  $svcName$ctxTypeParamDecl impl,
         |  IBaboonServiceRt rt,
         |  $ctxParamDecl$baboonCodecContext $codecCtxName) {
         |  final handlers = <String, $wiringRetType Function()>{
         |    ${cases.shift(4).trim}
         |  };
         |  final handler = handlers[method.methodName];
         |  if (handler == null) {
         |    return rt.fail($baboonWiringError.noMatchingMethod(method));
         |  }
         |  return handler();
         |}""".stripMargin
    }
  }
}
