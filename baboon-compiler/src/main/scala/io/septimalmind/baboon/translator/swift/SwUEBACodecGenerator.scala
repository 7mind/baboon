package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.CompilerTarget.SwTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.swift.SwCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.swift.SwDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.translator.swift.SwValue.SwType
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class SwUEBACodecGenerator(
  trans: SwTypeTranslator,
  target: SwTarget,
  domain: Domain,
  evo: BaboonEvolution,
  swDomainTreeTools: SwDomainTreeTools,
) extends SwCodecTranslator {

  override def translate(
    defn: DomainMember.User,
    swRef: SwType,
    srcRef: SwType,
  ): Option[TextTree[SwValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto      => Some(genDtoBodies(swRef, d))
        case e: Typedef.Enum     => Some(genEnumBodies(swRef, e))
        case a: Typedef.Adt      => Some(genAdtBodies(swRef, a))
        case _: Typedef.Foreign  => Some(genForeignBodies(swRef))
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          val branchDecoder = defn.defn match {
            case d: Typedef.Dto => genBranchDecoder(swRef, d)
            case _              => None
          }

          genCodec(
            defn,
            swRef,
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
    name: SwType,
    srcRef: SwType,
    enc: TextTree[SwValue],
    dec: TextTree[SwValue],
    branchDecoder: Option[TextTree[SwValue]],
  ): TextTree[SwValue] = {
    val isEncoderEnabled = domain.version == evo.latest
    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = d.fields.filter(f => domain.refMeta(f.tpe).len.isVariable)
        val comment = varlens.map(f => q"// ${f.toString}").joinN()
        q"""$comment
           |return ${varlens.size.toString}""".stripMargin

      case _: Typedef.Enum    => q"""return 0"""
      case _: Typedef.Adt     => q"""return 0"""
      case _: Typedef.Foreign => q"""fatalError("${name.name} is a foreign type")"""

      case d: Typedef.Contract =>
        throw new IllegalArgumentException(s"BUG: contract codec should not be rendered: $d")
      case d: Typedef.Service =>
        throw new IllegalArgumentException(s"BUG: service codec should not be rendered: $d")
    }

    val indexGetter =
      q"""override var indexElementsCount: Int {
         |    ${indexBody.shift(4).trim}
         |}""".stripMargin

    val encoderMethods = if (isEncoderEnabled) {
      List(
        q"""override func encode(_ ctx: $baboonCodecContext, _ writer: $baboonBinWriter, _ value: $name) {
           |    ${enc.shift(4).trim}
           |}
           |""".stripMargin
      )
    } else Nil

    val decoderMethods = List(
      q"""override func decode(_ ctx: $baboonCodecContext, _ reader: $baboonBinReader) throws -> $name {
         |    ${dec.shift(4).trim}
         |}""".stripMargin
    )

    val baseMethods = encoderMethods ++ decoderMethods
      ++ branchDecoder.map {
        body =>
          q"""func decodeBranch(_ ctx: $baboonCodecContext, _ reader: $baboonBinReader) throws -> $name {
             |    ${body.shift(4).trim}
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

    val meta = renderMeta(defn, swDomainTreeTools.makeCodecMeta(defn))

    q"""class ${cName.asName}: $cParent<$name>, $baboonBinCodecIndexed {
       |    static let instance = ${cName.asName}()
       |    private override init() { super.init() }
       |
       |    ${baseMethods.joinNN().shift(4).trim}
       |
       |    ${meta.joinN().shift(4).trim}
       |}
       |""".stripMargin
  }

  private def genForeignBodies(name: SwType): (TextTree[SwValue], TextTree[SwValue]) = {
    (
      q"""fatalError("${name.name} is a foreign type")""",
      q"""fatalError("${name.name} is a foreign type")""",
    )
  }

  private def genAdtBodies(name: SwType, adt: Typedef.Adt): (TextTree[SwValue], TextTree[SwValue]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList.map {
      case (m, idx) =>
        val branchName = m.name.name
        val caseName   = branchName.head.toLower.toString + branchName.tail

        val adtRef = trans.toSwTypeRefKeepForeigns(m, domain, evo)
        val cName  = codecName(adtRef)

        val encBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.instance.encode(ctx, writer, branchVal)"""
        } else {
          q"""writer.writeU8(${idx.toString})
             |$cName.instance.encode(ctx, writer, branchVal)""".stripMargin
        }

        val decBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""return .$caseName(try ($cName.instance as! $cName).decodeBranch(ctx, reader))"""
        } else {
          q"""return .$caseName(try $cName.instance.decode(ctx, reader))"""
        }

        (
          q"""case .$caseName(let branchVal):
             |    ${encBody.shift(4).trim}""".stripMargin,
          q"""case ${idx.toString}:
             |    ${decBody.shift(4).trim}""".stripMargin,
        )
    }

    (
      q"""switch value {
         |${branches.map(_._1).joinN().shift(0).trim}
         |}""".stripMargin,
      q"""let asByte = reader.readU8()
         |
         |switch asByte {
         |${branches.map(_._2).joinN().shift(0).trim}
         |default:
         |    throw BaboonCodecError.invalidInput("Cannot decode to ${name.name}: no matching value for ordinal \\(asByte)")
         |}""".stripMargin,
    )
  }

  private def genEnumBodies(name: SwType, e: Typedef.Enum): (TextTree[SwValue], TextTree[SwValue]) = {
    val encBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"case .${m.name}: writer.writeU8(${idx.toString})"
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"case ${idx.toString}: return $name.${m.name}"
    }

    (
      q"""switch value {
         |${encBranches.joinN().shift(0).trim}
         |}""".stripMargin,
      q"""let asByte = reader.readU8()
         |
         |switch asByte {
         |${decBranches.joinN().shift(0).trim}
         |default:
         |    throw BaboonCodecError.invalidInput("Cannot decode to ${name.name}: no matching value for ordinal \\(asByte)")
         |}""".stripMargin,
    )
  }

  private def genBranchDecoder(
    name: SwType,
    d: Typedef.Dto,
  ): Option[TextTree[SwValue]] = {
    d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(name, fields.map(_._2)))
      case _ =>
        None
    }
  }

  private def genDtoBodies(name: SwType, dto: Typedef.Dto): (TextTree[SwValue], TextTree[SwValue]) = {
    val fields = fieldsOf(dto)

    val noIndex = Seq(
      q"writer.writeU8(header)",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).joinN()

    val fenc =
      q"""var header: UInt8 = 0
         |
         |if ctx.useIndices {
         |    header = header | 1
         |    writer.writeU8(header)
         |    let buffer = $baboonBinWriter()
         |    ${fields.map(_._3).joinN().shift(4).trim}
         |    writer.writeAll(buffer.toData())
         |} else {
         |    ${noIndex.shift(4).trim}
         |}""".stripMargin

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

        q"""writer.writeU8(${idx.toString})
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)
        q"""let marker = reader.readU8()
           |assert(marker == ${idx.toString})
           |return try decodeBranch(ctx, reader)""".stripMargin
      case _ => fdec
    }
    (enc, dec)
  }

  private def dtoDec(name: SwType, fields: List[TextTree[SwValue]]): TextTree[SwValue] = {
    val fieldAssignments = if (fields.isEmpty) {
      q""
    } else {
      q"""${fields.join(",\n").shift(4).trim}"""
    }

    q"""let index = try readIndex(ctx, reader)
       |if ctx.useIndices { assert(index.count == indexElementsCount) }
       |return $name(
       |    $fieldAssignments
       |)""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[SwValue], TextTree[SwValue], TextTree[SwValue])] = {
    dto.fields.map {
      field =>
        val escaped    = trans.escapeSwiftKeyword(field.name.name)
        val fieldRef   = q"value.$escaped"
        val enc        = mkEncoder(field.tpe, fieldRef, q"writer")
        val bufferEnc  = mkEncoder(field.tpe, fieldRef, q"buffer")
        val decoder    = mkDecoder(field.tpe)
        val decodeTree = q"$escaped: $decoder"

        val w = domain.refMeta(field.tpe).len match {
          case BinReprLen.Fixed(bytes) =>
            q"""{
               |    // ${field.toString}
               |    let before = buffer.position
               |    ${bufferEnc.shift(4).trim}
               |    let after = buffer.position
               |    let length = after - before
               |    assert(length == ${bytes.toString})
               |}""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = v match {
              case BinReprLen.Unknown() =>
                q"""assert(after >= before, "Got after=\\(after), before=\\(before)")"""

              case BinReprLen.Alternatives(variants) =>
                q"""assert([${variants.mkString(", ")}].contains(length), "Got length=\\(length)")"""

              case BinReprLen.Range(min, max) =>
                (
                  Seq(q"""assert(length >= ${min.toString}, "Got length=\\(length)")""") ++
                  max.toSeq.map(m => q"""assert(length <= ${m.toString}, "Got length=\\(length)")""")
                ).joinN()
            }

            q"""{
               |    // ${field.toString}
               |    let before = buffer.position
               |    writer.writeI32(Int32(before))
               |    ${bufferEnc.shift(4).trim}
               |    let after = buffer.position
               |    let length = after - before
               |    writer.writeI32(Int32(length))
               |    ${sanityChecks.shift(4).trim}
               |}""".stripMargin
        }

        (enc, decodeTree, w)
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[SwValue] = {
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
            val targetTpe = codecName(trans.toSwTypeRefKeepForeigns(u, domain, evo))
            q"""try $targetTpe.instance.decode(ctx, reader)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            val innerDecoder = mkDecoder(c.args.head)
            q"""(reader.readBool() ? $innerDecoder : nil)"""
          case TypeId.Builtins.map =>
            val keyDecoder   = mkDecoder(c.args.head)
            val valueDecoder = mkDecoder(c.args.last)
            q"""try Dictionary(uniqueKeysWithValues: (0..<Int(reader.readI32())).map { _ in ($keyDecoder, $valueDecoder) })"""
          case TypeId.Builtins.lst =>
            q"""try (0..<Int(reader.readI32())).map { _ in ${mkDecoder(c.args.head)} }"""
          case TypeId.Builtins.set =>
            q"""Set(try (0..<Int(reader.readI32())).map { _ in ${mkDecoder(c.args.head)} })"""
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[SwValue], wref: TextTree[SwValue]): TextTree[SwValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit   => q"$wref.writeBool($ref)"
              case TypeId.Builtins.i08   => q"$wref.writeI8($ref)"
              case TypeId.Builtins.i16   => q"$wref.writeI16($ref)"
              case TypeId.Builtins.i32   => q"$wref.writeI32($ref)"
              case TypeId.Builtins.i64   => q"$wref.writeI64($ref)"
              case TypeId.Builtins.u08   => q"$wref.writeU8($ref)"
              case TypeId.Builtins.u16   => q"$wref.writeU16($ref)"
              case TypeId.Builtins.u32   => q"$wref.writeU32($ref)"
              case TypeId.Builtins.u64   => q"$wref.writeU64($ref)"
              case TypeId.Builtins.f32   => q"$wref.writeF32($ref)"
              case TypeId.Builtins.f64   => q"$wref.writeF64($ref)"
              case TypeId.Builtins.f128  => q"$wref.writeDecimal($ref)"
              case TypeId.Builtins.str   => q"$wref.writeString($ref)"
              case TypeId.Builtins.bytes => q"$wref.writeBytes($ref)"
              case TypeId.Builtins.uid   => q"$wref.writeUuid($ref)"
              case TypeId.Builtins.tsu   => q"$wref.writeTsu($ref)"
              case TypeId.Builtins.tso   => q"$wref.writeTso($ref)"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toSwTypeRefKeepForeigns(u, domain, evo))
            q"""$targetTpe.instance.encode(ctx, $wref, $ref)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""if let unwrapped = $ref {
               |    $wref.writeBool(true)
               |    ${mkEncoder(c.args.head, q"unwrapped", wref).shift(4).trim}
               |} else {
               |    $wref.writeBool(false)
               |}""".stripMargin

          case TypeId.Builtins.map =>
            q"""$wref.writeI32(Int32($ref.count))
               |for (key, value) in $ref {
               |    ${mkEncoder(c.args.head, q"key", wref).shift(4).trim}
               |    ${mkEncoder(c.args.last, q"value", wref).shift(4).trim}
               |}""".stripMargin

          case TypeId.Builtins.lst =>
            q"""$wref.writeI32(Int32($ref.count))
               |for item in $ref {
               |    ${mkEncoder(c.args.head, q"item", wref).shift(4).trim}
               |}""".stripMargin

          case TypeId.Builtins.set =>
            q"""$wref.writeI32(Int32($ref.count))
               |for item in $ref {
               |    ${mkEncoder(c.args.head, q"item", wref).shift(4).trim}
               |}""".stripMargin

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[SwValue]] = {
    defn.defn match {
      case _: Typedef.Enum | _: Typedef.Foreign => meta.map(_.valueField)
      case _                                    => meta.map(_.refValueField)
    }
  }

  def codecName(name: SwType): SwType = {
    val baseFileName = name.importAs.getOrElse(trans.toSnakeCase(name.name))
    SwType(name.pkg, s"${name.name}_UebaCodec", name.fq, importAs = Some(baseFileName))
  }

  override def codecMeta(defn: DomainMember.User, name: SwType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"static let codecUeba: $baboonBinCodec<$name> = ${codecName(name).asName}.instance"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
