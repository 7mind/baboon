package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.CompilerTarget.SwTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.swift.SwCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.swift.SwDomainTreeTools.MetaField
import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.translator.swift.SwValue.SwType
import io.septimalmind.baboon.typer.EnumWireStyle
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.translator.{AnyFieldPlan, UebaLayoutPlan, UebaLengthCheckRenderer}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class SwUEBACodecGenerator(
  trans: SwTypeTranslator,
  target: SwTarget,
  domain: Domain,
  evo: BaboonEvolution,
  swDomainTreeTools: SwDomainTreeTools,
) extends SwCodecTranslator {
  private val layout = new UebaLayoutPlan(domain)

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
        val varlens = layout.indexedFields(d)
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
      q"""public var indexElementsCount: Int {
         |    ${indexBody.shift(4).trim}
         |}""".stripMargin

    val localName = name.asDeclName
    val encoderMethods = if (isEncoderEnabled) {
      List(
        q"""public override func encode(_ ctx: $baboonCodecContext, _ writer: $baboonBinWriter, _ value: $localName) {
           |    ${enc.shift(4).trim}
           |}
           |""".stripMargin
      )
    } else Nil

    val decoderMethods = List(
      q"""public override func decode(_ ctx: $baboonCodecContext, _ reader: $baboonBinReader) throws -> $localName {
         |    ${dec.shift(4).trim}
         |}""".stripMargin
    )

    val baseMethods = encoderMethods ++ decoderMethods
      ++ branchDecoder.map {
        body =>
          q"""func decodeBranch(_ ctx: $baboonCodecContext, _ reader: $baboonBinReader) throws -> $localName {
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

    q"""public class ${cName.asDeclName}: $cParent<$localName>, $baboonBinCodecIndexed {
       |    public static let instance = ${cName.asDeclName}()
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
        val caseName   = trans.escapeSwiftKeyword(branchName.head.toLower.toString + branchName.tail)

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
        val pascal        = EnumWireStyle.wireName(m.name)
        val escapedPascal = trans.escapeSwiftKeyword(pascal)
        q"case .$escapedPascal: writer.writeU8(${idx.toString})"
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val pascal        = EnumWireStyle.wireName(m.name)
        val escapedPascal = trans.escapeSwiftKeyword(pascal)
        q"case ${idx.toString}: return .$escapedPascal"
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

    val enc = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, dto.id)

        q"""writer.writeU8(${idx.toString})
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, dto.id)
        q"""let marker = reader.readU8()
           |guard marker == ${idx.toString} else { throw BaboonCodecError.invalidInput("Unexpected UEBA ADT branch marker: " + String(marker)) }
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

    q"""let indexCount = try consumeIndex(ctx, reader)
       |if ctx.useIndices && indexCount != indexElementsCount { throw BaboonCodecError.invalidInput("Unexpected UEBA index count: " + String(indexCount)) }
       |return ${name.asDeclName}(
       |    $fieldAssignments
       |)""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[SwValue], TextTree[SwValue], TextTree[SwValue])] = {
    layout.fields(dto).map {
      case UebaLayoutPlan.FieldLayout(field, length) =>
        val escaped             = trans.escapeSwiftKeyword(field.name.name)
        val fieldRef            = q"value.$escaped"
        val enc                 = mkEncoder(field.tpe, fieldRef, q"writer")
        val bufferEnc           = mkEncoder(field.tpe, fieldRef, q"buffer")
        val (decoder, mayThrow) = mkDecoder(field.tpe)
        val decodeTree          = if (mayThrow) q"$escaped: try $decoder" else q"$escaped: $decoder"

        val w = length match {
          case BinReprLen.Fixed(bytes) =>
            q"""do {
               |    // ${field.toString}
               |    let before = buffer.position
               |    ${bufferEnc.shift(4).trim}
               |    let after = buffer.position
               |    let length = after - before
               |    ${lengthChecks(BinReprLen.Fixed(bytes)).shift(4).trim}
               |}""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = lengthChecks(v)

            q"""do {
               |    // ${field.toString}
               |    let before = buffer.position
               |    guard before <= ${UebaLayoutPlan.MaxIndexValue.toString} else { fatalError("UEBA index offset exceeds i32") }
               |    writer.writeI32(Int32(before))
               |    ${bufferEnc.shift(4).trim}
               |    let after = buffer.position
               |    let length = after - before
               |    ${sanityChecks.shift(4).trim}
               |    writer.writeI32(Int32(length))
               |}""".stripMargin
        }

        (enc, decodeTree, w)
    }
  }

  private def lengthChecks(length: BinReprLen): TextTree[SwValue] =
    UebaLengthCheckRenderer.render[SwValue](
      length,
      equalTo = bytes => q"length == ${bytes.toString}",
      oneOf   = bytes => q"[${bytes.mkString(", ")}].contains(length)",
      enforce = condition => q"""guard $condition else { fatalError("Invalid UEBA field length: " + String(length)) }""",
    )

  // Returns (expression, mayThrow). `mayThrow` is true when the expression contains a top-level
  // throwing call that needs `try` at the parent site (e.g. inside a closure body or ternary).
  private def mkDecoder(tpe: TypeRef): (TextTree[SwValue], Boolean) = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            val decoded = SwScalarCodecs.uebaDecode(s, q"reader")
            (decoded.expression, decoded.mayThrow)
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toSwTypeRefKeepForeigns(u, domain, evo))
            (q"$targetTpe.instance.decode(ctx, reader)", true)
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            val (innerDecoder, innerThrows) = mkDecoder(c.args.head)
            // Do NOT add inner `try` — if innerThrows is true, the parent adds `try` at the call
            // site which covers throwing calls inside the ternary branch (Swift allows outermost
            // `try` to cover nested throwing calls: `try (cond ? f() : nil)` is valid).
            (q"""(reader.readBool() ? $innerDecoder : nil)""", innerThrows)
          case TypeId.Builtins.map =>
            val (keyDecoder, keyThrows)     = mkDecoder(c.args.head)
            val (valueDecoder, valueThrows) = mkDecoder(c.args.last)
            val keyExpr                     = if (keyThrows) q"try $keyDecoder" else keyDecoder
            val valueExpr                   = if (valueThrows) q"try $valueDecoder" else valueDecoder
            (q"""Dictionary(uniqueKeysWithValues: (0..<Int(reader.readI32())).map { _ in ($keyExpr, $valueExpr) })""", keyThrows || valueThrows)
          case TypeId.Builtins.lst =>
            val (elemDecoder, elemThrows) = mkDecoder(c.args.head)
            val elemExpr                  = if (elemThrows) q"try $elemDecoder" else elemDecoder
            (q"""(0..<Int(reader.readI32())).map { _ in $elemExpr }""", elemThrows)
          case TypeId.Builtins.set =>
            val (elemDecoder, elemThrows) = mkDecoder(c.args.head)
            val elemExpr                  = if (elemThrows) q"try $elemDecoder" else elemDecoder
            (q"""Set((0..<Int(reader.readI32())).map { _ in $elemExpr })""", elemThrows)
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => (mkAnyDecoder(a), true)
    }
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[SwValue], wref: TextTree[SwValue]): TextTree[SwValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            SwScalarCodecs.uebaEncode(s, wref, ref)
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
      case a: TypeRef.Any => mkAnyEncoder(a, ref, wref)
    }
  }

  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[SwValue], wref: TextTree[SwValue]): TextTree[SwValue] = {
    val args = SwAnyFieldRendering.arguments(AnyFieldPlan.forField(a, domain))
    q"BaboonRuntime.BaboonAnyUebaFieldCodec.encodeAnyField(ctx, $wref, $args, $ref)"
  }

  private def mkAnyDecoder(a: TypeRef.Any): TextTree[SwValue] = {
    val kind = SwAnyFieldRendering.kind(AnyFieldPlan.forField(a, domain))
    q"BaboonRuntime.BaboonAnyUebaFieldCodec.decodeAnyField(reader, $kind)"
  }

  private def renderMeta(defn: DomainMember.User, meta: List[MetaField]): List[TextTree[SwValue]] = {
    defn.defn match {
      case _: Typedef.Enum | _: Typedef.Foreign => meta.map(_.valueField)
      case _                                    => meta.map(_.refValueField)
    }
  }

  def codecName(name: SwType): SwType = {
    val baseFileName = name.importAs.getOrElse(trans.toSnakeCase(name.name))
    val localName    = name.localName.getOrElse(name.name)
    SwType(name.pkg, s"${name.name}_UebaCodec", name.fq, importAs = Some(baseFileName), localName = Some(s"${localName}_UebaCodec"))
  }

  override def codecMeta(defn: DomainMember.User, name: SwType): Option[CodecMeta] = {
    if (isActive(defn.id)) {
      Some(CodecMeta(q"public static let codecUeba: $baboonBinCodec<${name.asDeclName}> = ${codecName(name).asDeclName}.instance"))
    } else None
  }

  override def isActive(id: TypeId): Boolean = {
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
