package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.CompilerTarget.DtTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.dart.DtCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.dart.DtDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
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

    val baseMethods = encodeMethod ++ decodeMethod
    val cName       = codecName(srcRef)
    val meta        = renderMeta(defn, dtDomainTreeTools.makeCodecMeta(defn))

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
            q"""$ref == null ? null : ${mkEncoder(c.args.head, q"$ref!", depth + 1)}"""
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

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[DtValue]] = {
    defn.defn match {
      case _: Typedef.Enum => meta.map(_.valueField)
      case f: Typedef.Foreign =>
        f.bindings.get(BaboonLang.Dart) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => meta.map(_.refValueField)
          case _                                                                  => meta.map(_.valueField)
        }
      case _ => meta.map(_.refValueField)
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
