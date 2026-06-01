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
}

object DtServiceWiringTranslator {
  class Impl(
    target: DtTarget,
    trans: DtTypeTranslator,
    codecs: Set[DtCodecTranslator],
    domain: Domain,
    evo: BaboonEvolution,
  ) extends DtServiceWiringTranslator {

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
      case u: TypeId.User => q"${jsonCodecName(u)}.instance.decode(ctx, $wire)"
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
      case u: TypeId.User => q"${jsonCodecName(u)}.instance.encode(ctx, $value)"
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
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.decode(ctx, $reader)"
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
      case u: TypeId.User => q"${uebaCodecName(u)}.instance.encode(ctx, $writer, $value);"
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
    // transport. When both codecs are active each endpoint gets a UEBA method
    // (default name) plus a JSON-suffixed variant.
    override def translateClient(defn: DomainMember.User): Option[TextTree[DtValue]] = {
      defn.defn match {
        case service: Typedef.Service =>
          val svcName = service.id.name.name
          val hasUeba = hasActiveUebaCodecs(service)
          val hasJson = hasActiveJsonCodecs(service)
          if (!hasUeba && !hasJson) return None

          val clientMethods = service.methods.flatMap {
            m =>
              val inTypeRef = trans.asDtRef(m.sig, domain, evo)
              val retType   = m.out.map(o => trans.asDtRef(o, domain, evo)).getOrElse(q"void")

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
                  q"""Future<$retType> ${m.name.name}($inTypeRef arg, [$baboonCodecContext ctx = $baboonCodecContext.defaultCtx]) async {
                     |  final writer = $baboonBinTools.createWriter();
                     |  $encodeIn
                     |  final resp = await _transportUeba('$svcName', '${m.name.name}', writer.toBytes());
                     |  ${decodeOut.shift(2).trim}
                     |}""".stripMargin
                )
              } else None

              val jsonMethodName = if (hasUeba) s"${m.name.name}Json" else m.name.name
              val jsonMethod = if (hasJson) {
                val encodeIn = jsonEncodeExpr(m.sig.id, q"arg")
                val decodeOut = m.out match {
                  case Some(outRef) =>
                    val decodeExpr = jsonDecodeExpr(outRef.id, q"$dtJsonDecode(resp)")
                    q"return $decodeExpr;"
                  case None => q"return;"
                }
                Some(
                  q"""Future<$retType> $jsonMethodName($inTypeRef arg, [$baboonCodecContext ctx = $baboonCodecContext.defaultCtx]) async {
                     |  final encoded = $dtJsonEncode($encodeIn);
                     |  final resp = await _transportJson('$svcName', '${m.name.name}', encoded);
                     |  ${decodeOut.shift(2).trim}
                     |}""".stripMargin
                )
              } else None

              uebaMethod.toList ++ jsonMethod.toList
          }

          if (clientMethods.isEmpty) return None

          val transportFields = List(
            if (hasUeba) Some(q"final Future<$dtUint8List> Function(String service, String method, $dtUint8List data) _transportUeba;") else None,
            if (hasJson) Some(q"final Future<String> Function(String service, String method, String data) _transportJson;") else None,
          ).flatten

          val ctorParams = List(
            if (hasUeba) Some(q"this._transportUeba") else None,
            if (hasJson) Some(q"this._transportJson") else None,
          ).flatten

          Some(
            q"""class ${svcName}Client {
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

      val wrappers = generateServiceWrappers(service, noErrorsJsonRetTree, noErrorsUebaRetTree)

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
          val decodeIn = jsonDecodeExpr(m.sig.id, q"wire")

          val encodeOutput = m.out match {
            case Some(outRef) =>
              val encodeOut = jsonEncodeExpr(outRef.id, q"result")
              q"""final encoded = $encodeOut;
                 |return $dtJsonEncode(encoded);""".stripMargin
            case None =>
              q"""return 'null';"""
          }

          val callExpr = m.out match {
            case Some(_) => q"final result = impl.${m.name.name}(${ctxArgPass}decoded);"
            case None    => q"impl.${m.name.name}(${ctxArgPass}decoded);"
          }

          q"""'${m.name.name}': () {
             |  final wire = $dtJsonDecode(data);
             |  final decoded = $decodeIn;
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}
             |},""".stripMargin
      }.join("\n")

      q"""static String invokeJson(
         |  $baboonMethodId method,
         |  String data,
         |  $svcName impl,
         |  $ctxParamDecl$baboonCodecContext ctx) {
         |  final handlers = <String, String Function()>{
         |    ${cases.shift(4).trim}
         |  };
         |  final handler = handlers[method.methodName];
         |  if (handler == null) {
         |    throw $baboonWiringException($baboonWiringError.noMatchingMethod(method));
         |  }
         |  return handler();
         |}""".stripMargin
    }

    private def generateNoErrorsUebaMethod(service: Typedef.Service): TextTree[DtValue] = {
      val svcName = service.id.name.name
      val cases = service.methods.map {
        m =>
          val decodeIn = uebaDecodeExpr(m.sig.id, q"reader")

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
            case Some(_) => q"final result = impl.${m.name.name}(${ctxArgPass}decoded);"
            case None    => q"impl.${m.name.name}(${ctxArgPass}decoded);"
          }

          q"""'${m.name.name}': () {
             |  final reader = $baboonBinTools.createReader(data);
             |  final decoded = $decodeIn;
             |  $callExpr
             |  ${encodeOutput.shift(2).trim}
             |},""".stripMargin
      }.join("\n")

      q"""static $dtUint8List invokeUeba(
         |  $baboonMethodId method,
         |  $dtUint8List data,
         |  $svcName impl,
         |  $ctxParamDecl$baboonCodecContext ctx) {
         |  final handlers = <String, $dtUint8List Function()>{
         |    ${cases.shift(4).trim}
         |  };
         |  final handler = handlers[method.methodName];
         |  if (handler == null) {
         |    throw $baboonWiringException($baboonWiringError.noMatchingMethod(method));
         |  }
         |  return handler();
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
      * Dart services are currently always synchronous (no `--dt-async-services`
      * flag exists), so the wrapper's R is the underlying static-method's
      * return type directly — no Future wrapping. If async services land later
      * the wrapper's R becomes `Future<…>` without changing the muxer contract.
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
      val ifaceType   = if (isJson) baboonJsonServiceIface else baboonUebaServiceIface
      val invokerFn   = if (isJson) "invokeJson"     else "invokeUeba"
      val wrapperName = s"$svcName${if (isJson) "JsonService" else "UebaService"}"

      // Constructor and forwarded args: every extra dependency consumed by the
      // underlying ${Svc}Wiring.invoke<Json|Ueba> static method (rt in errors
      // mode, plus any service-context parameter) is baked at construction time
      // so the runtime IBaboon*Service contract stays uniform across modes.
      val rtField: Option[(String, TextTree[DtValue])] =
        if (resolved.noErrors) None else Some(("rt", q"IBaboonServiceRt"))

      val svcCtxField: Option[(String, TextTree[DtValue])] = resolvedCtx match {
        case ResolvedServiceContext.NoContext               => None
        case ResolvedServiceContext.AbstractContext(tn, pn) => Some((pn, q"$tn"))
        case ResolvedServiceContext.ConcreteContext(tn, pn) => Some((pn, q"$tn"))
      }

      val extraCtorParams: List[(String, TextTree[DtValue])] = List(rtField, svcCtxField).flatten

      val implField: TextTree[DtValue]  = q"final $svcName _impl;"
      val extraFields: List[TextTree[DtValue]] = extraCtorParams.map {
        case (name, tpe) => q"final $tpe _$name;"
      }

      // Per-field initializers in Dart's `this._x` constructor form keep the
      // fields final without an extra body, so the wrapper stays const-friendly
      // and the analyzer doesn't warn about unused setters.
      val ctorImplParam: TextTree[DtValue] = q"this._impl"
      val ctorExtraParams: List[TextTree[DtValue]] = extraCtorParams.map {
        case (name, _) => q"this._$name"
      }
      val ctorParamList: TextTree[DtValue] =
        (ctorImplParam :: ctorExtraParams).join(", ")

      // Underlying static-method call: positional args match the existing
      // ${Svc}Wiring.invoke<Json|Ueba>(method, data, impl, [rt], [svcCtx], ctx)
      // signature emitted by generateNoErrors*Method / generateErrors*Method.
      val invokerArgs: List[TextTree[DtValue]] = {
        val base    = List[TextTree[DtValue]](q"method", q"data", q"_impl")
        val withRt  = rtField.fold(base)(_ => base :+ q"_rt")
        val withSvc = svcCtxField.fold(withRt)(f => withRt :+ q"_${f._1}")
        withSvc :+ q"ctx"
      }

      val allFields = (implField :: extraFields).join("\n")

      q"""class $wrapperName implements $ifaceType<$retType> {
         |  @override
         |  String get serviceName => '$svcName';
         |
         |  ${allFields.shift(2).trim}
         |
         |  $wrapperName($ctorParamList);
         |
         |  @override
         |  $retType invoke($baboonMethodId method, $wireType data, $baboonCodecContext ctx) {
         |    return ${svcName}Wiring.$invokerFn(${invokerArgs.join(", ")});
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
          val inRef    = trans.asDtRef(m.sig, domain, evo)
          val decodeIn = jsonDecodeExpr(m.sig.id, q"wire")

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
                   |  final callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  return rt.pure(impl.${m.name.name}(${ctxArgPass}v));
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
                   |  final callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  impl.${m.name.name}(${ctxArgPass}v);
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

      q"""static $wiringRetType invokeJson(
         |  $baboonMethodId method,
         |  String data,
         |  $svcName impl,
         |  IBaboonServiceRt rt,
         |  $ctxParamDecl$baboonCodecContext ctx) {
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
          val inRef    = trans.asDtRef(m.sig, domain, evo)
          val decodeIn = uebaDecodeExpr(m.sig.id, q"reader")

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
                   |  final callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  return rt.pure(impl.${m.name.name}(${ctxArgPass}v));
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
                   |  final callResult = impl.${m.name.name}(${ctxArgPass}v);
                   |  return rt.leftMap(
                   |    callResult, (err) => $baboonWiringError.callFailed(method, err));
                   |} catch (ex) {
                   |  return rt.fail($baboonWiringError.callFailed(method, ex));
                   |}""".stripMargin
              } else {
                q"""try {
                   |  impl.${m.name.name}(${ctxArgPass}v);
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

      q"""static $wiringRetType invokeUeba(
         |  $baboonMethodId method,
         |  $dtUint8List data,
         |  $svcName impl,
         |  IBaboonServiceRt rt,
         |  $ctxParamDecl$baboonCodecContext ctx) {
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
