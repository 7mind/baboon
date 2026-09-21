package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.CompilerTarget.SwTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.swift.SwCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.swift.SwDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.translator.AnyFieldPlan
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class SwJsonCodecGenerator(
  trans: SwTypeTranslator,
  domainTypes: SwDomainTypes,
  target: SwTarget,
  domain: Domain,
  evo: BaboonEvolution,
  swDomainTreeTools: SwDomainTreeTools,
) extends SwCodecTranslator {

  override def translate(defn: DomainMember.User, swRef: SwValue.SwType, srcRef: SwValue.SwType): Option[TextTree[SwValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto      => Some(genDtoBodies(swRef, d))
        case _: Typedef.Enum     => Some(genEnumBodies(swRef))
        case a: Typedef.Adt      => Some(genAdtBodies(swRef, a))
        case _: Typedef.Foreign  => Some(genForeignBodies(swRef))
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          genCodec(defn, swRef, srcRef, enc, dec)
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: SwValue.SwType,
    srcRef: SwValue.SwType,
    enc: TextTree[SwValue],
    dec: TextTree[SwValue],
  ): TextTree[SwValue] = {
    val isEncoderEnabled = domain.version == evo.latest
    val encReturnType    = "Any"
    val localName        = name.asDeclName
    val encodeMethod =
      if (isEncoderEnabled) {
        List(
          q"""public override func encode(_ ctx: $baboonCodecContext, _ value: $localName) -> $encReturnType {
             |    ${enc.shift(4).trim}
             |}
             |""".stripMargin.trim
        )
      } else Nil
    val decodeMethod =
      List(
        q"""public override func decode(_ ctx: $baboonCodecContext, _ wire: Any) throws -> $localName {
           |    ${dec.shift(4).trim}
           |}""".stripMargin.trim
      )

    val baseMethods = encodeMethod ++ decodeMethod
    val cName       = codecName(srcRef)
    val meta        = renderMeta(defn, swDomainTreeTools.makeCodecMeta(defn))

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecBase<$localName>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecBase<$localName>"
        case _ if defn.isAdt                                => q"$baboonJsonCodecBaseGeneratedAdt<$localName>"
        case _                                              => q"$baboonJsonCodecBaseGenerated<$localName>"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecNoEncoder<$localName>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecNoEncoder<$localName>"
        case _ if defn.isAdt                                => q"$baboonJsonCodecNoEncoderGeneratedAdt<$localName>"
        case _                                              => q"$baboonJsonCodecNoEncoderGenerated<$localName>"
      }
    }

    q"""public class ${cName.asDeclName}: $cParent {
       |    public static let instance = ${cName.asDeclName}()
       |    private override init() { super.init() }
       |
       |    ${baseMethods.joinNN().shift(4).trim}
       |
       |    ${meta.joinN().shift(4).trim}
       |}
       |""".stripMargin
  }

  private def genForeignBodies(name: SwValue.SwType): (TextTree[SwValue], TextTree[SwValue]) = {
    (
      q"""fatalError("${name.name} is a foreign type")""",
      q"""fatalError("${name.name} is a foreign type")""",
    )
  }

  private def wrapAdtBranchEncoder(
    branchName: String,
    tree: TextTree[SwValue],
  ): TextTree[SwValue] = {
    q"""["$branchName": $tree as Any]"""
  }

  private def genAdtBodies(name: SwValue.SwType, adt: Typedef.Adt): (TextTree[SwValue], TextTree[SwValue]) = {
    val branches = adt.dataMembers(domain).map {
      m =>
        val branchName = m.name.name
        val caseName   = trans.escapeSwiftKeyword(branchName.head.toLower.toString + branchName.tail)
        val fqBranch   = domainTypes.toSwTypeRefKeepForeigns(m)

        val routedBranchEncoder = q"${codecName(fqBranch)}.instance.encode(ctx, branchVal)"
        val branchEncoder = if (target.language.wrappedAdtBranchCodecs) {
          routedBranchEncoder
        } else {
          wrapAdtBranchEncoder(branchName, routedBranchEncoder)
        }

        (
          q"""case .$caseName(let branchVal):
             |    return $branchEncoder""".stripMargin,
          q"""case "$branchName":
             |    return .$caseName(try ${codecName(fqBranch)}.instance.decode(ctx, entryValue))""".stripMargin,
        )
    }

    (
      q"""switch value {
         |${branches.map(_._1).joinN().shift(0).trim}
         |}""".stripMargin,
      q"""guard let jsonObj = wire as? [String: Any] else {
         |    throw BaboonCodecError.invalidInput("Expected dictionary for ${name.name}")
         |}
         |guard let entry = jsonObj.first else {
         |    throw BaboonCodecError.invalidInput("Cannot decode to ${name.name}: empty json object")
         |}
         |let entryValue = entry.value
         |switch entry.key {
         |${branches.map(_._2).joinN().shift(0).trim}
         |default:
         |    throw BaboonCodecError.invalidInput("Cannot decode to ${name.name}: unknown key \\(entry.key)")
         |}""".stripMargin,
    )
  }

  private def genEnumBodies(name: SwValue.SwType): (TextTree[SwValue], TextTree[SwValue]) = {
    (
      q"""return value.rawValue""",
      q"""guard let str = wire as? String else {
         |    throw BaboonCodecError.invalidInput("Expected string for ${name.name}")
         |}
         |guard let parsed = ${name.asDeclName}.parse(str.trimmingCharacters(in: .whitespaces)) else {
         |    throw BaboonCodecError.invalidInput("Cannot decode to ${name.name}: no matching value for \\(str)")
         |}
         |return parsed""".stripMargin,
    )
  }

  private def genDtoBodies(name: SwValue.SwType, d: Typedef.Dto): (TextTree[SwValue], TextTree[SwValue]) = {
    val encFields = d.fields.map {
      f =>
        val escaped  = trans.escapeSwiftKeyword(f.name.name)
        val fieldRef = q"value.$escaped"
        val enc      = mkEncoder(f.tpe, fieldRef)
        q""""${f.name.name}": $enc as Any,"""
    }

    val decFields = d.fields.map {
      f =>
        val escaped          = trans.escapeSwiftKeyword(f.name.name)
        val (expr, mayThrow) = mkDecoder(f.name.name, f.tpe, q"jsonObj")
        if (mayThrow) q"$escaped: try $expr" else q"$escaped: $expr"
    }

    val encodedMap = if (encFields.nonEmpty) {
      q"""[
         |    ${encFields.joinN().shift(4).trim}
         |]""".stripMargin
    } else {
      q"[String: Any]()"
    }

    val mainEnc = q"return $encodedMap"

    val encBody = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val innerMap = encodedMap
        q"""return ${wrapAdtBranchEncoder(d.id.name.name, innerMap)}"""
      case _ => mainEnc
    }

    val decBody = if (d.fields.nonEmpty) {
      q"""guard let jsonObj = wire as? [String: Any] else {
         |    throw BaboonCodecError.invalidInput("Expected dictionary for ${name.name}")
         |}
         |return ${name.asDeclName}(
         |    ${decFields.join(",\n").shift(4).trim}
         |)""".stripMargin
    } else {
      q"""return ${name.asDeclName}()"""
    }

    (encBody, decBody)
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[SwValue], depth: Int = 0): TextTree[SwValue] = {
    def encodeKey(tpe: TypeRef, ref: TextTree[SwValue]): TextTree[SwValue] = {
      tpe.id match {
        case TypeId.Builtins.tsu   => q"$baboonTimeFormats.formatUtc($ref)"
        case TypeId.Builtins.tso   => q"$baboonTimeFormats.formatOffset($ref)"
        case TypeId.Builtins.uid   => q"$ref.uuidString"
        case TypeId.Builtins.f128  => q"$ref.stringValue"
        case TypeId.Builtins.bytes => q"$baboonByteStringTools.toHexString($ref)"
        case _: TypeId.Builtin     => q"String(describing: $ref)"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum    => q"$ref.rawValue"
                case f: Typedef.Foreign =>
                  // PR-I.2 (M24 Phase 3.2): Custom-foreign map keys route through
                  // the emitted `<Foreign>_KeyCodecHost.instance` extension hook.
                  // BaboonRef-mapped foreigns recurse into the aliased type at the
                  // call site (mkEncoder/decodeKey on a TypeRef.Scalar of TypeId.User
                  // resolves the alias before reaching this match), so they never
                  // surface here. Explicit Custom match (PR-I-D05 pattern guidance).
                  f.bindings.get(BaboonLang.Swift) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      encodeKey(aliasedRef, ref)
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
                      val srcRef = domainTypes.toSwTypeRefKeepForeigns(uid)
                      val host   = SwValue.SwType(srcRef.pkg, s"${srcRef.name}_KeyCodecHost")
                      q"$host.instance.encodeKey($ref)"
                    case None =>
                      throw new RuntimeException(s"BUG: Foreign type $uid has no Swift binding")
                  }
                // M19/PR-60: id types — emit canonical description (single- or multi-field).
                case d: Typedef.Dto if d.isIdentifier =>
                  q"$ref.description"
                // M19/PR-60: single-primitive-field wrappers — peel and recurse.
                case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
                  val inner = d.fields.head
                  encodeKey(inner.tpe, q"$ref.${inner.name.name}")
                case o => throw new RuntimeException(s"BUG: Unexpected key usertype: $o")
              }
            case o => throw new RuntimeException(s"BUG: Type/usertype mismatch: $o")
          }
        case o => throw new RuntimeException(s"BUG: Unexpected key type: $o")
      }
    }

    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case b: TypeId.BuiltinScalar => SwScalarCodecs.jsonEncode(b, ref)
          case u: TypeId.User =>
            val targetTpe = codecName(domainTypes.toSwTypeRefKeepForeigns(u))
            q"$targetTpe.instance.encode(ctx, $ref)"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""$ref == nil ? NSNull() : ${mkEncoder(c.args.head, q"$ref!", depth + 1)}"""
          case TypeId.Builtins.map =>
            val varName  = s"e$depth"
            val keyEnc   = encodeKey(c.args.head, q"$varName.key")
            val valueEnc = mkEncoder(c.args.last, q"$varName.value", depth + 1)
            q"""Dictionary(uniqueKeysWithValues: $ref.map { $varName in ($keyEnc, $valueEnc as Any) })"""
          case TypeId.Builtins.lst =>
            val varName = s"e$depth"
            q"""$ref.map { $varName in ${mkEncoder(c.args.head, q"$varName", depth + 1)} as Any }"""
          case TypeId.Builtins.set =>
            val varName = s"e$depth"
            q"""Array($ref).map { $varName in ${mkEncoder(c.args.head, q"$varName", depth + 1)} as Any }"""
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref)
    }
  }

  // Returns (expression, mayThrow) so the call site at line 175 can emit `try` only when needed.
  private def mkDecoder(fieldName: String, tpe: TypeRef, jsonObjRef: TextTree[SwValue]): (TextTree[SwValue], Boolean) = {
    // Returns (expression, mayThrow). `mayThrow` is true when the expression contains a throwing
    // call at its top level (i.e. needs `try` at the call site). Nested `try` inside closures
    // are handled inline — only the outermost `try` requirement is surfaced here.
    def decodeElement(tpe: TypeRef, ref: TextTree[SwValue], depth: Int): (TextTree[SwValue], Boolean) = {
      val varName = s"e$depth"
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case b: TypeId.BuiltinScalar =>
              val decoded = SwScalarCodecs.jsonDecode(b, ref)
              (decoded.expression, decoded.mayThrow)
            case u: TypeId.User =>
              val targetTpe = codecName(domainTypes.toSwTypeRefKeepForeigns(u))
              (q"$targetTpe.instance.decode(ctx, $ref)", true)
            case o =>
              throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
        case c: TypeRef.Constructor =>
          c.id match {
            case TypeId.Builtins.opt =>
              // Do NOT add inner `try` — if innerThrows is true, the enclosing `lst`/`map`/`set`
              // adds `try` at the closure-element site, which covers the throwing call inside the
              // ternary branch (Swift allows one outermost `try` to cover nested calls:
              // `try (cond ? f() : nil)` is valid). This avoids the `try try` double-emit.
              val (innerExpr, innerThrows) = decodeElement(c.args.head, ref, depth + 1)
              (q"""$ref is NSNull || $ref == nil ? nil : $innerExpr""", innerThrows)
            case TypeId.Builtins.lst =>
              val (elemDec, elemThrows) = decodeElement(c.args.head, q"$varName", depth + 1)
              if (elemThrows) {
                (q"""($ref as! [Any]).map { $varName in try $elemDec }""", true)
              } else {
                (q"""($ref as! [Any]).map { $varName in $elemDec }""", false)
              }
            case TypeId.Builtins.set =>
              val (elemDec, elemThrows) = decodeElement(c.args.head, q"$varName", depth + 1)
              if (elemThrows) {
                (q"""Set(($ref as! [Any]).map { $varName in try $elemDec })""", true)
              } else {
                (q"""Set(($ref as! [Any]).map { $varName in $elemDec })""", false)
              }
            case TypeId.Builtins.map =>
              val (keyDec, keyThr)   = decodeKey(c.args.head, q"$varName.key")
              val (valueDec, valThr) = decodeElement(c.args.last, q"$varName.value", depth + 1)
              val anyThr             = keyThr || valThr
              // `Dictionary(uniqueKeysWithValues:)` is `rethrows`: it re-throws whatever the
              // mapping closure throws. If either the key or value decode uses `try`, the outer
              // `Dictionary(...)` call must itself be marked with `try`.
              // Emit `try` independently for key and value — four cases: both/key-only/val-only/neither.
              val keyTok  = if (keyThr) q"try $keyDec" else q"$keyDec"
              val valTok  = if (valThr) q"try $valueDec" else q"$valueDec"
              val mapExpr = q"""Dictionary(uniqueKeysWithValues: ($ref as! [String: Any]).map { $varName in ($keyTok, $valTok) })"""
              (mapExpr, anyThr)
            case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
        case a: TypeRef.Any => (mkAnyDecoder(a, ref), true)
      }
    }

    // Returns (expression, mayThrow). `mayThrow` is true when the expression embeds `try` (i.e.
    // the key decoder itself can throw — Foreign-Custom keys delegate to `instance.decode` which
    // is `throws`; single-field wrappers over a throwing key type propagate the flag).
    def decodeKey(tpe: TypeRef, ref: TextTree[SwValue]): (TextTree[SwValue], Boolean) = {
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case TypeId.Builtins.bit   => (q"$ref == \"true\"", false)
            case TypeId.Builtins.i08   => (q"Int8($ref)!", false)
            case TypeId.Builtins.i16   => (q"Int16($ref)!", false)
            case TypeId.Builtins.i32   => (q"Int32($ref)!", false)
            case TypeId.Builtins.i64   => (q"Int64($ref)!", false)
            case TypeId.Builtins.u08   => (q"UInt8($ref)!", false)
            case TypeId.Builtins.u16   => (q"UInt16($ref)!", false)
            case TypeId.Builtins.u32   => (q"UInt32($ref)!", false)
            case TypeId.Builtins.u64   => (q"UInt64($ref)!", false)
            case TypeId.Builtins.f32   => (q"Float($ref)!", false)
            case TypeId.Builtins.f64   => (q"Double($ref)!", false)
            case TypeId.Builtins.f128  => (q"$baboonDecimal($ref)", false)
            case TypeId.Builtins.str   => (q"$ref", false)
            case TypeId.Builtins.uid   => (q"UUID(uuidString: $ref)!", false)
            case TypeId.Builtins.bytes => (q"$baboonByteStringTools.fromHexString($ref)", false)
            case TypeId.Builtins.tsu   => (q"$baboonTimeFormats.parseUtc($ref)", false)
            case TypeId.Builtins.tso   => (q"$baboonTimeFormats.parseOffset($ref)", false)
            case u: TypeId.User =>
              domain.defs.meta.nodes(u) match {
                case ud: DomainMember.User =>
                  ud.defn match {
                    case _: Typedef.Enum =>
                      // PR-F (M24): throw BaboonCodecException.decoderFailure on parse failure
                      // for cross-language malformed-key consistency (replaces forced unwrap).
                      val targetTpe = domainTypes.toSwTypeRefKeepForeigns(u)
                      (
                        q"""{ () throws -> $targetTpe in guard let __r = $targetTpe.parse($ref) else { throw $baboonCodecException.decoderFailure(\"malformed key: \\($ref)\", nil) }; return __r }()""",
                        true,
                      )
                    case f: Typedef.Foreign =>
                      // PR-I.2 (M24 Phase 3.2): Custom-foreign map keys route through
                      // the emitted `<Foreign>_KeyCodecHost.instance` extension hook.
                      // mayThrow=true — PR-B plumbing wraps the outer `try` based on
                      // (keyThr || valThr). `catch let e` (NOT broader) keeps fail-fast on
                      // fatal/control errors (PR-I-D01 pattern guidance).
                      f.bindings.get(BaboonLang.Swift) match {
                        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                          decodeKey(aliasedRef, ref)
                        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) =>
                          val foreignTpe = domainTypes.toSwTypeRefKeepForeigns(u)
                          val host       = SwValue.SwType(foreignTpe.pkg, s"${foreignTpe.name}_KeyCodecHost")
                          (
                            q"""{ () throws -> $foreignTpe in do { return try $host.instance.decodeKey($ref) } catch let e { throw $baboonCodecException.decoderFailure(\"malformed key: \\($ref)\", e) } }()""",
                            true,
                          )
                        case None =>
                          throw new RuntimeException(s"BUG: Foreign type $u has no Swift binding")
                      }
                    // M19/PR-60: id types — call parseRepr and unwrap .right.
                    // PR-F (M24): throw BaboonCodecException.decoderFailure on .left for
                    // cross-language malformed-key consistency (replaces fatalError).
                    case d: Typedef.Dto if d.isIdentifier =>
                      val targetTpe   = domainTypes.toSwTypeRefKeepForeigns(u)
                      val nestedCodec = SwValue.SwType(targetTpe.pkg, s"${targetTpe.name}Codec")
                      (
                        q"""{ () throws -> $targetTpe in guard case .right(let __r) = $nestedCodec.parseRepr($ref) else { throw $baboonCodecException.decoderFailure(\"malformed key: \\($ref)\", nil) }; return __r }()""",
                        true,
                      )
                    // M19/PR-60: single-primitive-field wrappers — peel and recurse, then construct.
                    case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
                      val inner                = d.fields.head
                      val targetTpe            = domainTypes.toSwTypeRefKeepForeigns(u)
                      val (innerDec, innerThr) = decodeKey(inner.tpe, ref)
                      (q"$targetTpe(${inner.name.name}: $innerDec)", innerThr)
                    case o => throw new RuntimeException(s"BUG: Unexpected key usertype: $o")
                  }
                case o => throw new RuntimeException(s"BUG: Type/usertype mismatch: $o")
              }
            case o => throw new RuntimeException(s"BUG: Unexpected key type: $o")
          }
        case _ => throw new RuntimeException(s"Collection cannot be key: $tpe")
      }
    }

    tpe match {
      case TypeRef.Constructor(id, args) if id.name.name == "opt" =>
        // Use force-unwrap `v!` — the closure body has already guarded against nil/NSNull, so
        // `v!` is safe and avoids `Any?`→`Any` coercion warnings in non-string decoders. The
        // `str` branch emits `(v! as! String)` (parens added) to silence Swift's forced-downcast
        // warning in optional context.
        val (innerExpr, innerThrows) = decodeElement(args.head, q"v!", 0)
        val innerWithTry             = if (innerThrows) q"try $innerExpr" else innerExpr
        val closureExpr              = q"""{ let v = $jsonObjRef["$fieldName"]; return v is NSNull || v == nil ? nil : $innerWithTry }()"""
        (closureExpr, innerThrows)
      case _ =>
        val (expr, throws) = decodeElement(tpe, q"""$jsonObjRef["$fieldName"]!""", 0)
        (expr, throws)
    }
  }

  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[SwValue]): TextTree[SwValue] = {
    val args = SwAnyFieldRendering.arguments(AnyFieldPlan.forField(a, domain))
    q"BaboonRuntime.BaboonAnyJsonFieldCodec.encodeAnyField(ctx, $args, $ref)"
  }

  private def mkAnyDecoder(a: TypeRef.Any, ref: TextTree[SwValue]): TextTree[SwValue] = {
    val kind = SwAnyFieldRendering.kind(AnyFieldPlan.forField(a, domain))
    q"BaboonRuntime.BaboonAnyJsonFieldCodec.decodeAnyField($kind, $ref)"
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[SwValue]] = {
    defn.defn match {
      case _: Typedef.Enum | _: Typedef.Foreign => meta.map(_.valueField)
      case _                                    => meta.map(_.refValueField)
    }
  }

  def codecName(name: SwValue.SwType): SwValue.SwType = {
    val baseFileName = name.importAs.getOrElse(trans.toSnakeCase(name.name))
    val localName    = name.localName.getOrElse(name.name)
    SwValue.SwType(name.pkg, s"${name.name}_JsonCodec", name.fq, importAs = Some(baseFileName), localName = Some(s"${localName}_JsonCodec"))
  }

  override def codecMeta(defn: DomainMember.User, name: SwValue.SwType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"public static let codecJson: $baboonJsonCodec<${name.asDeclName}> = ${codecName(name).asDeclName}.instance"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Json"
}
