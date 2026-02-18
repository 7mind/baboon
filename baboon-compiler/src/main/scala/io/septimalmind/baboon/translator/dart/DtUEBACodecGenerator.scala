package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.CompilerTarget.DtTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.dart.DtCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.dart.DtDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.translator.dart.DtValue.DtType
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class DtUEBACodecGenerator(
  trans: DtTypeTranslator,
  target: DtTarget,
  domain: Domain,
  evo: BaboonEvolution,
  dtDomainTreeTools: DtDomainTreeTools,
) extends DtCodecTranslator {

  override def translate(
    defn: DomainMember.User,
    dtRef: DtType,
    srcRef: DtType,
  ): Option[TextTree[DtValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto      => Some(genDtoBodies(dtRef, d))
        case e: Typedef.Enum     => Some(genEnumBodies(dtRef, e))
        case a: Typedef.Adt      => Some(genAdtBodies(dtRef, a))
        case f: Typedef.Foreign =>
          f.bindings.get(BaboonLang.Dart) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
            case _ => Some(genForeignBodies(dtRef))
          }
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          val branchDecoder = defn.defn match {
            case d: Typedef.Dto => genBranchDecoder(dtRef, d)
            case _              => None
          }

          genCodec(
            defn,
            dtRef,
            srcRef,
            enc,
            dec,
            branchDecoder,
          )
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: DtType,
    srcRef: DtType,
    enc: TextTree[DtValue],
    dec: TextTree[DtValue],
    branchDecoder: Option[TextTree[DtValue]],
  ): TextTree[DtValue] = {
    val isEncoderEnabled = domain.version == evo.latest
    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = d.fields.filter(f => domain.refMeta(f.tpe).len.isVariable)
        val comment = varlens.map(f => q"// ${f.toString}").joinN()
        q"""$comment
           |return ${varlens.size.toString};""".stripMargin

      case _: Typedef.Enum    => q"""return 0;"""
      case _: Typedef.Adt     => q"""return 0;"""
      case _: Typedef.Foreign => q"""throw ArgumentError('${name.name} is a foreign type');"""

      case d: Typedef.Contract =>
        throw new IllegalArgumentException(s"BUG: contract codec should not be rendered: $d")
      case d: Typedef.Service =>
        throw new IllegalArgumentException(s"BUG: service codec should not be rendered: $d")
    }

    val indexGetter =
      q"""@override
         |int get indexElementsCount {
         |  ${indexBody.shift(2).trim}
         |}""".stripMargin

    val encoderMethods = if (isEncoderEnabled) {
      List(
        q"""@override
           |void encode($baboonCodecContext ctx, $baboonBinWriter writer, $name value) {
           |  ${enc.shift(2).trim}
           |}
           |""".stripMargin
      )
    } else Nil

    val decoderMethods = List(
      q"""@override
         |$name decode($baboonCodecContext ctx, $baboonBinReader reader) {
         |  ${dec.shift(2).trim}
         |}""".stripMargin
    )

    val baseMethods = encoderMethods ++ decoderMethods
      ++ branchDecoder.map {
        body =>
          q"""$name decodeBranch($baboonCodecContext ctx, $baboonBinReader reader) {
             |  ${body.shift(2).trim}
             |}""".stripMargin
      }.toList ++ List(indexGetter)

    val cName = codecName(srcRef)

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => baboonBinCodecBase
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => baboonBinCodecBase
        case _ if defn.isAdt                                => baboonBinCodecBaseGeneratedAdt
        case _                                              => baboonBinCodecBaseGenerated
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => baboonBinCodecNoEncoder
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => baboonBinCodecNoEncoder
        case _ if defn.isAdt                                => baboonBinCodecNoEncoderGeneratedAdt
        case _                                              => baboonBinCodecNoEncoderGenerated
      }
    }

    val meta = renderMeta(defn, dtDomainTreeTools.makeCodecMeta(defn))

    q"""class ${cName.asName} extends $cParent<$name> with $baboonBinCodecIndexed {
       |  const ${cName.asName}._();
       |  static const instance = ${cName.asName}._();
       |
       |  ${baseMethods.joinNN().shift(2).trim}
       |
       |  ${meta.joinN().shift(2).trim}
       |}
     """.stripMargin
  }

  private def genForeignBodies(name: DtType): (TextTree[DtValue], TextTree[DtValue]) = {
    (
      q"""throw ArgumentError('${name.name} is a foreign type');""",
      q"""throw ArgumentError('${name.name} is a foreign type');""",
    )
  }

  private def genAdtBodies(name: DtType, adt: Typedef.Adt): (TextTree[DtValue], TextTree[DtValue]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList.map {
      case (m, idx) =>
        val branchName = m.name.name

        val adtRef = trans.toDtTypeRefKeepForeigns(m, domain, evo)
        val cName  = codecName(adtRef)

        val castedName = branchName.substring(0, 1).toLowerCase + branchName.substring(1)

        val encBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.instance.encode(ctx, writer, $castedName);"""
        } else {
          q"""writer.writeU8(${idx.toString});
             |$cName.instance.encode(ctx, writer, $castedName);
           """.stripMargin
        }

        val decBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""return ($cName.instance as $cName).decodeBranch(ctx, reader);"""
        } else {
          q"""return $cName.instance.decode(ctx, reader);"""
        }

        (
          q"""if (value is $adtRef) {
             |  final $castedName = value;
             |  ${encBody.shift(2).trim}
             |}""".stripMargin,
          q"""case ${idx.toString}:
             |  ${decBody.shift(2).trim}""".stripMargin,
        )
    }

    val encElse = q"""throw ArgumentError('Cannot encode to ${name.name}: unexpected type $${value.runtimeType}');"""

    val encBranches = branches.map(_._1)
    val encChain = encBranches match {
      case Nil => encElse
      case _ =>
        val elseIfBranches = encBranches.tail.map {
          b =>
            q" else $b"
        }
        val chain = (Seq(encBranches.head) ++ elseIfBranches).join("")
        q"""$chain else {
           |  $encElse
           |}
           |""".stripMargin
    }

    (
      encChain,
      q"""final asByte = reader.readU8();
         |
         |switch (asByte) {
         |  ${branches.map(_._2).joinN().shift(2).trim}
         |  default:
         |    throw ArgumentError('Cannot decode to ${name.name}: no matching value for ordinal $$asByte');
         |}
         |""".stripMargin,
    )
  }

  private def genEnumBodies(name: DtType, e: Typedef.Enum): (TextTree[DtValue], TextTree[DtValue]) = {
    val encBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"case $name.${m.name}: writer.writeU8(${idx.toString}); break;"
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"case ${idx.toString}: return $name.${m.name};"
    }

    (
      q"""switch (value) {
         |  ${encBranches.joinN().shift(2)}
         |}
         """.stripMargin,
      q"""final asByte = reader.readU8();
         |
         |switch (asByte) {
         |  ${decBranches.joinN().shift(2)}
         |  default:
         |    throw ArgumentError('Cannot decode to ${name.name}: no matching value for ordinal $$asByte');
         |}
         |""".stripMargin,
    )
  }

  private def genBranchDecoder(
    name: DtType,
    d: Typedef.Dto,
  ): Option[TextTree[DtValue]] = {
    d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(name, fields.map(_._2)))
      case _ =>
        None
    }
  }

  private def genDtoBodies(name: DtType, dto: Typedef.Dto): (TextTree[DtValue], TextTree[DtValue]) = {
    val fields = fieldsOf(dto)

    val noIndex = Seq(
      q"writer.writeU8(header);",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).joinN()

    val fenc =
      q"""var header = 0;
         |
         |if (ctx.useIndices) {
         |  header = header | 1;
         |  writer.writeU8(header);
         |  final buffer = $baboonBinWriter();
         |  ${fields.map(_._3).joinN().shift(2).trim}
         |  writer.writeAll(buffer.toBytes());
         |} else {
         |  ${noIndex.shift(2).trim}
         |}
         |""".stripMargin

    val fdec = dtoDec(name, fields.map(_._2))

    def adtBranchIndex(id: TypeId.User) = {
      domain.defs.meta
        .nodes(id)
        .asInstanceOf[DomainMember.User]
        .defn
        .asInstanceOf[Typedef.Adt]
        .dataMembers(domain)
        .zipWithIndex
        .find(_._1 == dto.id)
        .get
        ._2
    }

    val enc = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)

        q"""writer.writeU8(${idx.toString});
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)
        q"""final marker = reader.readU8();
           |assert(marker == ${idx.toString});
           |return decodeBranch(ctx, reader);""".stripMargin
      case _ => fdec
    }
    (enc, dec)
  }

  private def dtoDec(name: DtType, fields: List[TextTree[DtValue]]): TextTree[DtValue] = {
    val fieldAssignments = if (fields.isEmpty) {
      q""
    } else {
      q"""${fields.join(",\n").shift(2).trim},"""
    }

    q"""final index = readIndex(ctx, reader);
       |if (ctx.useIndices) assert(index.length == indexElementsCount);
       |return $name(
       |  $fieldAssignments
       |);
       |""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[DtValue], TextTree[DtValue], TextTree[DtValue])] = {
    dto.fields.map {
      field =>
        val fieldRef   = q"value.${field.name.name}"
        val enc        = mkEncoder(field.tpe, fieldRef, q"writer")
        val bufferEnc  = mkEncoder(field.tpe, fieldRef, q"buffer")
        val decoder    = mkDecoder(field.tpe)
        val decodeTree = q"${field.name.name}: $decoder"

        val w = domain.refMeta(field.tpe).len match {
          case BinReprLen.Fixed(bytes) =>
            q"""{
               |  // ${field.toString}
               |  final before = buffer.position;
               |  ${bufferEnc.shift(2).trim}
               |  final after = buffer.position;
               |  final length = after - before;
               |  assert(length == ${bytes.toString});
               |}""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = v match {
              case BinReprLen.Unknown() =>
                q"""assert(after >= before, 'Got after=$$after, before=$$before');"""

              case BinReprLen.Alternatives(variants) =>
                q"""assert({${variants.mkString(", ")}}.contains(length), 'Got length=$$length');"""

              case BinReprLen.Range(min, max) =>
                (
                  Seq(q"""assert(length >= ${min.toString}, 'Got length=$$length');""") ++
                  max.toSeq.map(m => q"""assert(length <= ${m.toString}, 'Got length=$$length');""")
                ).joinN()
            }

            q"""{
               |  // ${field.toString}
               |  final before = buffer.position;
               |  writer.writeI32(before);
               |  ${bufferEnc.shift(2).trim}
               |  final after = buffer.position;
               |  final length = after - before;
               |  writer.writeI32(length);
               |  ${sanityChecks.shift(2).trim}
               |}""".stripMargin
        }

        (enc, decodeTree, w)
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[DtValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit => q"reader.readBool()"
              case TypeId.Builtins.i08 => q"reader.readI8()"
              case TypeId.Builtins.i16 => q"reader.readI16()"
              case TypeId.Builtins.i32 => q"reader.readI32()"
              case TypeId.Builtins.i64 => q"reader.readI64()"
              case TypeId.Builtins.u08 => q"reader.readU8()"
              case TypeId.Builtins.u16 => q"reader.readU16()"
              case TypeId.Builtins.u32 => q"reader.readU32()"
              case TypeId.Builtins.u64 => q"reader.readU64()"
              case TypeId.Builtins.f32 => q"reader.readF32()"
              case TypeId.Builtins.f64 => q"reader.readF64()"

              case TypeId.Builtins.f128  => q"reader.readDecimal()"
              case TypeId.Builtins.str   => q"reader.readString()"
              case TypeId.Builtins.bytes => q"reader.readBytes()"

              case TypeId.Builtins.uid => q"reader.readUuid()"
              case TypeId.Builtins.tsu => q"reader.readTsu()"
              case TypeId.Builtins.tso => q"reader.readTso()"

              case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Dart) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ =>
                    val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                    q"""$targetTpe.instance.decode(ctx, reader)"""
                }
              case _ =>
                val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                q"""$targetTpe.instance.decode(ctx, reader)"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            val innerDecoder = mkDecoder(c.args.head)
            q"""(reader.readBool() ? $innerDecoder : null)"""
          case TypeId.Builtins.map =>
            val keyDecoder   = mkDecoder(c.args.head)
            val valueDecoder = mkDecoder(c.args.last)
            q"""Map.fromEntries(List.generate(reader.readI32(), (_) => MapEntry($keyDecoder, $valueDecoder)))"""
          case TypeId.Builtins.lst =>
            q"""List.generate(reader.readI32(), (_) => ${mkDecoder(c.args.head)})"""
          case TypeId.Builtins.set =>
            q"""Set.of(List.generate(reader.readI32(), (_) => ${mkDecoder(c.args.head)}))"""
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[DtValue], wref: TextTree[DtValue]): TextTree[DtValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit   => q"$wref.writeBool($ref);"
              case TypeId.Builtins.i08   => q"$wref.writeI8($ref);"
              case TypeId.Builtins.i16   => q"$wref.writeI16($ref);"
              case TypeId.Builtins.i32   => q"$wref.writeI32($ref);"
              case TypeId.Builtins.i64   => q"$wref.writeI64($ref);"
              case TypeId.Builtins.u08   => q"$wref.writeU8($ref);"
              case TypeId.Builtins.u16   => q"$wref.writeU16($ref);"
              case TypeId.Builtins.u32   => q"$wref.writeU32($ref);"
              case TypeId.Builtins.u64   => q"$wref.writeU64($ref);"
              case TypeId.Builtins.f32   => q"$wref.writeF32($ref);"
              case TypeId.Builtins.f64   => q"$wref.writeF64($ref);"
              case TypeId.Builtins.f128  => q"$wref.writeDecimal($ref);"
              case TypeId.Builtins.str   => q"$wref.writeString($ref);"
              case TypeId.Builtins.bytes => q"$wref.writeBytes($ref);"
              case TypeId.Builtins.uid   => q"$wref.writeUuid($ref);"
              case TypeId.Builtins.tsu   => q"$wref.writeTsu($ref);"
              case TypeId.Builtins.tso   => q"$wref.writeTso($ref);"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Dart) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, wref)
                  case _ =>
                    val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                    q"""$targetTpe.instance.encode(ctx, $wref, $ref);"""
                }
              case _ =>
                val targetTpe = codecName(trans.toDtTypeRefKeepForeigns(u, domain, evo))
                q"""$targetTpe.instance.encode(ctx, $wref, $ref);"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""if ($ref == null) {
               |  $wref.writeBool(false);
               |} else {
               |  $wref.writeBool(true);
               |  ${mkEncoder(c.args.head, q"$ref!", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.map =>
            q"""$wref.writeI32($ref.length);
               |for (final entry in $ref.entries) {
               |  ${mkEncoder(c.args.head, q"entry.key", wref).shift(2).trim}
               |  ${mkEncoder(c.args.last, q"entry.value", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.lst =>
            q"""$wref.writeI32($ref.length);
               |for (final item in $ref) {
               |  ${mkEncoder(c.args.head, q"item", wref).shift(2).trim}
               |}""".stripMargin

          case TypeId.Builtins.set =>
            q"""$wref.writeI32($ref.length);
               |for (final item in $ref) {
               |  ${mkEncoder(c.args.head, q"item", wref).shift(2).trim}
               |}""".stripMargin

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
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

  def codecName(name: DtType): DtType = {
    val baseFileName = name.importAs.getOrElse(trans.toSnakeCase(name.name))
    DtType(name.pkg, s"${name.name}_UebaCodec", name.fq, importAs = Some(baseFileName))
  }

  override def codecMeta(defn: DomainMember.User, name: DtType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"static final $baboonBinCodec<$name> codecUeba = ${codecName(name).asName}.instance;"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Dart) &&
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
