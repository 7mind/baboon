package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.CompilerTarget.DtTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.dart.DtCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.dart.DtDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class DtJsonCodecGenerator(
  trans: DtTypeTranslator,
  target: DtTarget,
  domain: Domain,
  evo: BaboonEvolution,
  dtDomainTreeTools: DtDomainTreeTools,
) extends DtCodecTranslator {

  override def translate(defn: DomainMember.User, dtRef: DtValue.DtType, srcRef: DtValue.DtType): Option[TextTree[DtValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto  => Some(genDtoBodies(dtRef, d))
        case _: Typedef.Enum => Some(genEnumBodies(dtRef))
        case a: Typedef.Adt  => Some(genAdtBodies(dtRef, a))
        case f: Typedef.Foreign =>
          f.bindings.get(BaboonLang.Dart) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
            case _                                                                  => Some(genForeignBodies(dtRef))
          }
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          genCodec(defn, dtRef, srcRef, enc, dec)
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: DtValue.DtType,
    srcRef: DtValue.DtType,
    enc: TextTree[DtValue],
    dec: TextTree[DtValue],
  ): TextTree[DtValue] = {
    val isEncoderEnabled = domain.version == evo.latest
    val encReturnType = defn.defn match {
      case _: Typedef.Enum | _: Typedef.Foreign => "dynamic"
      case _                                    => "Map<String, dynamic>"
    }
    val encodeMethod =
      if (isEncoderEnabled) {
        List(
          q"""@override
             |$encReturnType encode($baboonCodecContext ctx, $name value) {
             |  ${enc.shift(2).trim}
             |}
             |""".stripMargin.trim
        )
      } else Nil
    val decodeMethod =
      List(
        q"""@override
           |$name decode($baboonCodecContext ctx, dynamic wire) {
           |  ${dec.shift(2).trim}
           |}""".stripMargin.trim
      )

    val anyHelpers: List[TextTree[DtValue]] = if (hasAnyField(defn)) List(anyFieldHelpers) else Nil
    val baseMethods                         = encodeMethod ++ decodeMethod ++ anyHelpers
    val cName                               = codecName(srcRef)
    val meta                                = renderMeta(defn, dtDomainTreeTools.makeCodecMeta(defn))

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecBase<$name>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecBase<$name>"
        case _ if defn.isAdt                                => q"$baboonJsonCodecBaseGeneratedAdt<$name>"
        case _                                              => q"$baboonJsonCodecBaseGenerated<$name>"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecNoEncoder<$name>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecNoEncoder<$name>"
        case _ if defn.isAdt                                => q"$baboonJsonCodecNoEncoderGeneratedAdt<$name>"
        case _                                              => q"$baboonJsonCodecNoEncoderGenerated<$name>"
      }
    }

    q"""class ${cName.asName} extends $cParent {
       |  const ${cName.asName}._();
       |  static const instance = ${cName.asName}._();
       |
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${meta.joinN().shift(2).trim}
       |}
       |""".stripMargin
  }

  private def genForeignBodies(name: DtValue.DtType): (TextTree[DtValue], TextTree[DtValue]) = {
    (
      q"""throw ArgumentError('${name.name} is a foreign type');""",
      q"""throw ArgumentError('${name.name} is a foreign type');""",
    )
  }

  private def wrapAdtBranchEncoder(
    branchName: String,
    tree: TextTree[DtValue],
  ): TextTree[DtValue] = {
    q"""{'$branchName': $tree}"""
  }

  private def genAdtBodies(name: DtValue.DtType, adt: Typedef.Adt): (TextTree[DtValue], TextTree[DtValue]) = {
    val branches = adt.dataMembers(domain).map {
      m =>
        val branchName = m.name.name
        val fqBranch   = trans.toDtTypeRefKeepForeigns(m, domain, evo)
        val branchRef  = q"branchVal"

        val routedBranchEncoder = q"${codecName(fqBranch)}.instance.encode(ctx, $branchRef)"
        val branchEncoder = if (target.language.wrappedAdtBranchCodecs) {
          routedBranchEncoder
        } else {
          wrapAdtBranchEncoder(branchName, routedBranchEncoder)
        }

        (
          q"""if (value is $fqBranch) {
             |  final $branchRef = value;
             |  return $branchEncoder;
             |}""".stripMargin,
          q"""'$branchName' => ${codecName(fqBranch)}.instance.decode(ctx, entry.value),""",
        )
    }

    (
      q"""${branches.map(_._1).joinN().trim}
         |throw ArgumentError('Cannot encode to ${name.name}: unknown subtype $${value.runtimeType}');""".stripMargin,
      q"""final jsonObj = wire as Map<String, dynamic>;
         |if (jsonObj.isEmpty) {
         |  throw ArgumentError('Cannot decode to ${name.name}: empty json object');
         |}
         |final entry = jsonObj.entries.first;
         |return switch (entry.key) {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |  _ => throw ArgumentError('Cannot decode to ${name.name}: unknown key $${entry.key}'),
         |};""".stripMargin,
    )
  }

  private def genEnumBodies(name: DtValue.DtType): (TextTree[DtValue], TextTree[DtValue]) = {
    (
      q"""return value.name;""",
      q"""final str = (wire as String).trim();
         |final parsed = $name.parse(str);
         |if (parsed == null) {
         |  throw ArgumentError('Cannot decode to ${name.name}: no matching value for $$str');
         |}
         |return parsed;""".stripMargin,
    )
  }

  private def genDtoBodies(name: DtValue.DtType, d: Typedef.Dto): (TextTree[DtValue], TextTree[DtValue]) = {
    val encFields = d.fields.map {
      f =>
        val fieldRef = q"value.${f.name.name}"
        val enc      = mkEncoder(f.tpe, fieldRef)
        q"""'${f.name.name}': $enc,"""
    }

    val decFields = d.fields.map {
      f =>
        q"${f.name.name}: ${mkDecoder(f.name.name, f.tpe, q"jsonObj")}"
    }

    val mainEnc = q"""return {
                     |  ${encFields.joinN().shift(2).trim}
                     |};""".stripMargin

    val encBody = d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val innerMap = q"""{
                          |  ${encFields.joinN().shift(2).trim}
                          |}""".stripMargin
        q"""return ${wrapAdtBranchEncoder(d.id.name.name, innerMap)};"""
      case _ => mainEnc
    }

    val decBody = if (d.fields.nonEmpty) {
      q"""final jsonObj = wire as Map<String, dynamic>;
         |return $name(
         |  ${decFields.join(",\n").shift(2).trim},
         |);""".stripMargin
    } else {
      q"""return $name();"""
    }

    (encBody, decBody)
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[DtValue], depth: Int = 0): TextTree[DtValue] = {
    def encodeKey(tpe: TypeRef, ref: TextTree[DtValue]): TextTree[DtValue] = {
      tpe.id match {
        case TypeId.Builtins.tsu   => q"$baboonTimeFormats.formatUtc($ref)"
        case TypeId.Builtins.tso   => q"$baboonTimeFormats.formatOffset($ref)"
        case TypeId.Builtins.uid   => q"$ref"
        case TypeId.Builtins.f128  => q"$ref.value"
        case TypeId.Builtins.bytes => q"$ref.toHexString()"
        case _: TypeId.Builtin     => q"$ref.toString()"
        case uid: TypeId.User =>
          domain.defs.meta.nodes(uid) match {
            case u: DomainMember.User =>
              u.defn match {
                case _: Typedef.Enum =>
                  q"$ref.name"
                case f: Typedef.Foreign =>
                  f.bindings.get(BaboonLang.Dart) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      encodeKey(aliasedRef, ref)
                    case _ =>
                      q"$ref.toString()"
                  }
                case o =>
                  throw new RuntimeException(s"BUG: Unexpected key usertype: $o")
              }
            case o =>
              throw new RuntimeException(s"BUG: Type/usertype mismatch: $o")
          }
        case o =>
          throw new RuntimeException(s"BUG: Unexpected key type: $o")
      }
    }

    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.bit                                                                   => q"$ref"
          case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 | TypeId.Builtins.i64 => q"$ref"
          case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 | TypeId.Builtins.u64 => q"$ref"
          case TypeId.Builtins.f32 | TypeId.Builtins.f64                                             => q"$ref"
          case TypeId.Builtins.f128                                                                  => q"$ref.value"
          case TypeId.Builtins.str                                                                   => q"$ref"
          case TypeId.Builtins.uid                                                                   => q"$ref"
          case TypeId.Builtins.bytes                                                                 => q"$ref.toHexString()"
          case TypeId.Builtins.tsu                                                                   => q"$baboonTimeFormats.formatUtc($ref)"
          case TypeId.Builtins.tso                                                                   => q"$baboonTimeFormats.formatOffset($ref)"
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Dart) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, depth)
                  case _ =>
                    val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                    q"$targetTpe.instance.encode(ctx, $ref)"
                }
              case _ =>
                val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                q"$targetTpe.instance.encode(ctx, $ref)"
            }
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            val nonNullRef = if (depth > 0) ref else q"$ref!"
            q"""$ref == null ? null : ${mkEncoder(c.args.head, nonNullRef, depth + 1)}"""
          case TypeId.Builtins.map =>
            val varName  = s"e$depth"
            val keyEnc   = encodeKey(c.args.head, q"$varName.key")
            val valueEnc = mkEncoder(c.args.last, q"$varName.value", depth + 1)
            q"""Map.fromEntries($ref.entries.map(($varName) => MapEntry($keyEnc, $valueEnc)))"""
          case TypeId.Builtins.lst =>
            val varName = s"e$depth"
            q"""$ref.map(($varName) => ${mkEncoder(c.args.head, q"$varName", depth + 1)}).toList()"""
          case TypeId.Builtins.set =>
            val varName = s"e$depth"
            q"""$ref.map(($varName) => ${mkEncoder(c.args.head, q"$varName", depth + 1)}).toList()"""
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref)
    }
  }

  private def mkDecoder(fieldName: String, tpe: TypeRef, jsonObjRef: TextTree[DtValue]): TextTree[DtValue] = {
    def decodeElement(tpe: TypeRef, ref: TextTree[DtValue], depth: Int): TextTree[DtValue] = {
      val varName = s"e$depth"
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case TypeId.Builtins.bit                                             => q"$ref as bool"
            case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 => q"($ref as num).toInt()"
            case TypeId.Builtins.i64                                             => q"($ref is String ? int.parse($ref as String) : ($ref as num).toInt())"
            case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 => q"($ref as num).toInt()"
            case TypeId.Builtins.u64                                             => q"($ref is String ? int.parse($ref as String) : ($ref as num).toInt())"
            case TypeId.Builtins.f32 | TypeId.Builtins.f64                       => q"($ref as num).toDouble()"
            case TypeId.Builtins.f128                                            => q"$baboonDecimal($ref is String ? $ref as String : $ref.toString())"
            case TypeId.Builtins.str                                             => q"$ref as String"
            case TypeId.Builtins.uid                                             => q"$ref as String"
            case TypeId.Builtins.bytes                                           => q"$baboonByteStringTools.fromHexString($ref as String)"
            case TypeId.Builtins.tsu                                             => q"$baboonTimeFormats.parseUtc($ref as String)"
            case TypeId.Builtins.tso                                             => q"$baboonTimeFormats.parseOffset($ref as String)"
            case u: TypeId.User =>
              domain.defs.meta.nodes(u) match {
                case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                  f.bindings.get(BaboonLang.Dart) match {
                    case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                      decodeElement(aliasedRef, ref, depth)
                    case _ =>
                      val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                      q"$targetTpe.instance.decode(ctx, $ref)"
                  }
                case _ =>
                  val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                  q"$targetTpe.instance.decode(ctx, $ref)"
              }
            case o =>
              throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
        case c: TypeRef.Constructor =>
          c.id match {
            case TypeId.Builtins.opt =>
              q"""$ref == null ? null : ${decodeElement(c.args.head, ref, depth + 1)}"""
            case TypeId.Builtins.lst =>
              val elemDec = decodeElement(c.args.head, q"$varName", depth + 1)
              q"""($ref as List).map(($varName) => $elemDec).toList()"""
            case TypeId.Builtins.set =>
              val elemDec = decodeElement(c.args.head, q"$varName", depth + 1)
              q"""($ref as List).map(($varName) => $elemDec).toSet()"""
            case TypeId.Builtins.map =>
              val keyDec   = decodeKey(c.args.head, q"$varName.key")
              val valueDec = decodeElement(c.args.last, q"$varName.value", depth + 1)
              q"""Map.fromEntries(($ref as Map<String, dynamic>).entries.map(($varName) => MapEntry($keyDec, $valueDec)))"""
            case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
          }
        case a: TypeRef.Any => mkAnyDecoder(a, ref)
      }
    }

    def decodeKey(tpe: TypeRef, ref: TextTree[DtValue]): TextTree[DtValue] = {
      tpe match {
        case TypeRef.Scalar(id) =>
          id match {
            case TypeId.Builtins.bit                                                                   => q"$ref == 'true'"
            case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 | TypeId.Builtins.i64 => q"int.parse($ref)"
            case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 | TypeId.Builtins.u64 => q"int.parse($ref)"
            case TypeId.Builtins.f32                                                                   => q"double.parse($ref)"
            case TypeId.Builtins.f64                                                                   => q"double.parse($ref)"
            case TypeId.Builtins.f128                                                                  => q"$baboonDecimal($ref)"
            case TypeId.Builtins.str                                                                   => q"$ref"
            case TypeId.Builtins.uid                                                                   => q"$ref"
            case TypeId.Builtins.bytes                                                                 => q"$baboonByteStringTools.fromHexString($ref)"
            case TypeId.Builtins.tsu                                                                   => q"$baboonTimeFormats.parseUtc($ref)"
            case TypeId.Builtins.tso                                                                   => q"$baboonTimeFormats.parseOffset($ref)"
            case u: TypeId.User =>
              domain.defs.meta.nodes(u) match {
                case ud: DomainMember.User =>
                  ud.defn match {
                    case _: Typedef.Enum =>
                      val targetTpe = trans.toDtTypeRefKeepForeigns(u, domain, evo)
                      q"$targetTpe.parse($ref)!"
                    case f: Typedef.Foreign =>
                      f.bindings.get(BaboonLang.Dart) match {
                        case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                          decodeKey(aliasedRef, ref)
                        case _ =>
                          val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                          q"$targetTpe.instance.decode(ctx, $ref)"
                      }
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
        q"""$jsonObjRef['$fieldName'] == null ? null : ${decodeElement(args.head, q"""$jsonObjRef['$fieldName']""", 0)}"""
      case _ =>
        q"""${decodeElement(tpe, q"""$jsonObjRef['$fieldName']""", 0)}"""
    }
  }

  // Deep walk (mirrors Scala/C#/Rust/Kotlin/Java/TS/Dart-UEBA `hasAnyField`): a codec class needs
  // the any-field helpers if any direct or nested-via-Constructor-arg field has type `any`.
  private def hasAnyField(defn: DomainMember.User): Boolean = {
    def hasAny(tpe: TypeRef): Boolean = tpe match {
      case _: TypeRef.Any         => true
      case _: TypeRef.Scalar      => false
      case c: TypeRef.Constructor => c.args.exists(hasAny)
    }
    defn.defn match {
      case d: Typedef.Dto => d.fields.exists(f => hasAny(f.tpe))
      case _              => false
    }
  }

  // Encode delegates to the per-codec-class `encodeAnyField` helper. This site wires the expected
  // kind byte and the field's static (codec-gen-time) fallbacks for cross-format meta resolution.
  // See `anyStaticFallbacks` for the per-variant table.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[DtValue]): TextTree[DtValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"encodeAnyField(ctx, $expectedHex, $staticDom, $staticVer, $staticTid, $ref)"
  }

  // Decode delegates to the per-codec-class `decodeAnyField` helper. JSON decode never cross-
  // converts (always returns `AnyOpaqueJson` from JSON wire); user calls `facade.decodeAny(opaque)`
  // for typed resolution. No `ctx` / no static fallbacks needed at the decode site.
  private def mkAnyDecoder(a: TypeRef.Any, ref: TextTree[DtValue]): TextTree[DtValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"decodeAnyField($expectedHex, $ref)"
  }

  // Static fallbacks for the cross-format facade helper (`uebaToJson`). The wire `meta` may omit
  // components that are pinned by the field's static declaration; the codec emits whatever is
  // statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade._buildSyntheticTypeMeta` for the merge semantics. Per spec table:
  //   A=(null,null,null), B=(currentDomain,null,null), C=(currentDomain,currentVersion,null),
  //   D1=(null,null,underlyingFqid), D2=(currentDomain,null,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  // Duplicated across Scala/C#/Rust/Kotlin/Java/TS/Dart — extraction deferred (textual emission
  // diverges by language flavor; see PR 4.2 ledger entry's DRY analysis). 11th instance.
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[DtValue], TextTree[DtValue], TextTree[DtValue]) = {
    val none                     = q"null"
    def some(s: String)          = q""""$s""""
    val currentDomain: String    = domain.id.toString
    val currentDomainVer: String = domain.version.v.toString
    val typeidStatic = a.underlying match {
      case Some(u) => some(u.id.toString)
      case None    => none
    }
    val (domainStatic, versionStatic) = a.variant match {
      case AnyVariant.Global  => (none, none)
      case AnyVariant.ThisDom => (some(currentDomain), none)
      case AnyVariant.Current => (some(currentDomain), some(currentDomainVer))
    }
    (domainStatic, versionStatic, typeidStatic)
  }

  // Per-codec-class helpers consolidating the any-field JSON envelope encode/decode (kind check,
  // cross-format conversion via facade, envelope `$ak/$ad/$av/$at/$c` build & disassemble).
  // Emitted at most once per codec class that has any any-bearing field. Mirrors PR 6.3's Java
  // helper shape (closest precedent: JsonNode-style API) and PR 7.3's TS shape. Kind-check on
  // encode runs before any envelope construction.
  //
  // Cast-via-`as`: `ctx.facade` returns the `BaboonCodecsFacadeBase` opaque base (chosen by PR 8.1
  // to break the `baboon_runtime.dart` <-> `baboon_codecs_facade.dart` import cycle). The concrete
  // facade class carries `uebaToJson`; the cast is safe because `BaboonCodecContext.withFacade`
  // is the only construction path and accepts the same hierarchy.
  private def anyFieldHelpers: TextTree[DtValue] = {
    q"""Object? encodeAnyField(
       |    $baboonCodecContext ctx,
       |    int expectedKind,
       |    String? staticDomain,
       |    String? staticVersion,
       |    String? staticTypeid,
       |    $baboonAnyOpaque value,
       |) {
       |  if (value.meta.kind != expectedKind) {
       |    throw $baboonEncoderFailure(
       |      'any: meta-kind 0x' + (value.meta.kind & 0xFF).toRadixString(16).padLeft(2, '0') +
       |      ' does not match field-declared 0x' + (expectedKind & 0xFF).toRadixString(16).padLeft(2, '0'),
       |    );
       |  }
       |  Object? anyInner;
       |  switch (value) {
       |    case $baboonAnyOpaqueJson(:final json):
       |      anyInner = json;
       |    case $baboonAnyOpaqueUeba(:final meta, :final bytes):
       |      final anyFacadeBase = ctx.facade;
       |      if (anyFacadeBase == null) {
       |        throw $baboonEncoderFailure(
       |          'Cannot encode AnyOpaqueUeba into JSON without a facade reference. '
       |          'Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), '
       |          'or supply AnyOpaqueJson directly.',
       |        );
       |      }
       |      // Downcast to the concrete facade — the marker base is empty by design (PR 8.1
       |      // import-cycle break). Construction goes through BaboonCodecContext.withFacade
       |      // which only accepts BaboonCodecsFacadeBase, but real callers pass BaboonCodecsFacade.
       |      final anyFacade = anyFacadeBase as $baboonCodecsFacade;
       |      final anyConvResult = anyFacade.uebaToJson(
       |        meta,
       |        bytes,
       |        staticDomain: staticDomain,
       |        staticVersion: staticVersion,
       |        staticTypeid: staticTypeid,
       |      );
       |      switch (anyConvResult) {
       |        case $baboonLeft(:final value):
       |          throw value;
       |        case $baboonRight(:final value):
       |          anyInner = value;
       |      }
       |  }
       |  final anyEnvelope = $baboonAnyMetaCodec.writeJson(value.meta);
       |  anyEnvelope[$baboonAnyMetaCodec.ANY_CONTENT_KEY] = anyInner;
       |  return anyEnvelope;
       |}
       |
       |$baboonAnyOpaque decodeAnyField(int expectedKind, Object? wire) {
       |  if (wire is! Map) {
       |    throw $baboonDecoderFailure('any: JSON envelope must be an object');
       |  }
       |  final anyMetaResult = $baboonAnyMetaCodec.readJson(wire);
       |  final $baboonAnyMeta anyMeta;
       |  switch (anyMetaResult) {
       |    case $baboonLeft(:final value):
       |      throw value;
       |    case $baboonRight(:final value):
       |      anyMeta = value;
       |  }
       |  if (anyMeta.kind != expectedKind) {
       |    throw $baboonDecoderFailure(
       |      'any: wire kind 0x' + (anyMeta.kind & 0xFF).toRadixString(16).padLeft(2, '0') +
       |      ' does not match field-declared 0x' + (expectedKind & 0xFF).toRadixString(16).padLeft(2, '0'),
       |    );
       |  }
       |  if (!wire.containsKey($baboonAnyMetaCodec.ANY_CONTENT_KEY)) {
       |    throw $baboonDecoderFailure(
       |      "any: JSON envelope missing '" + $baboonAnyMetaCodec.ANY_CONTENT_KEY + "' content key",
       |    );
       |  }
       |  final anyContent = wire[$baboonAnyMetaCodec.ANY_CONTENT_KEY];
       |  return $baboonAnyOpaqueJson(anyMeta, anyContent);
       |}""".stripMargin
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[DtValue]] = {
    val (asValue, asRef): (MetaField => TextTree[DtValue], MetaField => TextTree[DtValue]) = (
      m => if (m.isCodecData) m.valueGetter else m.valueField,
      m => if (m.isCodecData) m.refValueGetter else m.refValueField,
    )
    defn.defn match {
      case _: Typedef.Enum => meta.map(asValue)
      case f: Typedef.Foreign =>
        f.bindings.get(BaboonLang.Dart) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => meta.map(asRef)
          case _                                                                  => meta.map(asValue)
        }
      case _ => meta.map(asRef)
    }
  }

  def codecName(name: DtValue.DtType): DtValue.DtType = {
    val baseFileName = name.importAs.getOrElse(trans.toSnakeCase(name.name))
    DtValue.DtType(name.pkg, s"${name.name}_JsonCodec", name.fq, importAs = Some(baseFileName))
  }

  override def codecMeta(defn: DomainMember.User, name: DtValue.DtType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"static final $baboonJsonCodec<$name> codecJson = ${codecName(name).asName}.instance;"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Dart) &&
    target.language.generateJsonCodecs && (target.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Json"
}
