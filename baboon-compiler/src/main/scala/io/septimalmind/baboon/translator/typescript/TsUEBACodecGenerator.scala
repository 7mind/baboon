package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.translator.{UebaLayoutPlan, UebaLengthCheckRenderer}
import io.septimalmind.baboon.translator.typescript.TsTypes.*
import io.septimalmind.baboon.translator.typescript.TsValue.TsType
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class TsUEBACodecGenerator(
  typeTranslator: TsTypeTranslator,
  target: TsTarget,
  domain: Domain,
  evo: BaboonEvolution,
  enquiries: BaboonEnquiries,
  tsFileTools: TsFileTools,
  tsDomainTreeTools: TsDomainTreeTools,
) extends TsCodecTranslator {
  private val layout    = new UebaLayoutPlan(domain)
  private val scalarOps = new TsScalarCodecOps(target)
  override def translate(defn: DomainMember.User, tsRef: TsValue.TsType, srcRef: TsValue.TsType): Option[TextTree[TsValue]] = {
    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto     => Some(genDtoCodec(srcRef, d))
        case e: Typedef.Enum    => Some(genEnumCodec(srcRef, e))
        case a: Typedef.Adt     => Some(genAdtCodec(a))
        case f: Typedef.Foreign =>
          // Mirror C# (CSUEBACodecGenerator): a Custom-bound foreign emits a value codec class whose
          // encode/decode throw by default; the host overrides it via `lazyInstance`. Containing types
          // are no longer suppressed — they route the foreign field through this codec.
          f.bindings.get(BaboonLang.Typescript) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(_, _))) => Some(genForeignBodies(srcRef))
            case _                                                                  => None
          }
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          val branchDecoder = defn.defn match {
            case d: Typedef.Dto => genBranchDecoder(tsRef, d)
            case _              => None
          }
          genCodec(defn, tsRef, srcRef, enc, dec, branchDecoder)
      }
    } else None
  }

  private def genCodec(
    defn: DomainMember.User,
    name: TsType,
    srcRef: TsType,
    enc: TextTree[TsValue],
    dec: TextTree[TsValue],
    branchDecoder: Option[TextTree[TsValue]],
  ): TextTree[TsValue] = {
    // Codec class is named after the keep-foreigns source ref (the baboon type name), not the mapped
    // value type `name`; for foreigns the latter is the host type. They coincide for non-foreigns.
    val cName = codecName(srcRef)

    val encodeMethod =
      List(
        q"""public encode(ctx: $tsBaboonCodecContext, value: $name, writer: $tsBaboonBinWriter): void {
           |    if (this !== $cName.lazyInstance.value) {
           |        $cName.lazyInstance.value.encode(ctx, value, writer); return;
           |    }
           |
           |    ${enc.shift(4).trim}
           |}
           |""".stripMargin.trim
      )

    val decodeMethod =
      List(
        q"""public decode(ctx: $tsBaboonCodecContext, reader: $tsBaboonBinReader): $name {
           |    if (this !== $cName .lazyInstance.value) {
           |        return $cName.lazyInstance.value.decode(ctx, reader)
           |    }
           |
           |    ${dec.shift(4).trim}
           |}""".stripMargin.trim
      )

    val baseMethods = encodeMethod ++ decodeMethod ++
      branchDecoder.map(tree => q"""public decodeBranch(ctx: $tsBaboonCodecContext, reader: $tsBaboonBinReader) {
                                   |    ${tree.shift(4).trim}
                                   |}""".stripMargin)

    val meta = tsDomainTreeTools.makeCodecMeta(defn, codecName(srcRef))

    q"""export class $cName {
       |    ${baseMethods.joinNN().shift(4).trim}
       |
       |    ${meta.joinN().shift(4).trim}
       |
       |    public static lazyInstance = new $tsBaboonLazy(() => new $cName())
       |    public static get instance(): $cName {
       |        return $cName.lazyInstance.value
       |    }
       |}""".stripMargin
  }

  // Throwing default bodies for a Custom-bound foreign UEBA value codec (mirrors C#). The host
  // overrides via `<F>_UEBACodec.lazyInstance`; the guard in `genCodec` routes to the override.
  private def genForeignBodies(srcRef: TsValue.TsType): (TextTree[TsValue], TextTree[TsValue]) = {
    val fqn = s"${(srcRef.moduleId.path :+ codecName(srcRef).name).mkString(".")}"
    val msg = s"$fqn is a foreign type with no built-in codec; provide one via $fqn.lazyInstance = new Lazy(() => yourCodec)."
    (
      q"""throw new $tsBaboonEncoderFailure("$msg");""",
      q"""throw new $tsBaboonDecoderFailure("$msg");""",
    )
  }

  private def genBranchDecoder(
    name: TsType,
    d: Typedef.Dto,
  ): Option[TextTree[TsValue]] = {
    d.id.owner match {
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(d, name, fields.map(_._2)))
      case _ =>
        None
    }
  }

  private def dtoDec(dto: Typedef.Dto, name: TsType, fields: List[TextTree[TsValue]]): TextTree[TsValue] = {
    val indexCount = layout.indexedFields(dto).size
    val ctorFields = dto.fields.map {
      f =>
        q"${typeTranslator.escapeTsKeyword(f.name.name)},"
    }
    q"""const indexCount = $tsBinTools.consumeIndex(reader, ${indexCount.toString});
       |if (ctx.useIndices && indexCount !== ${indexCount.toString}) throw new $tsBaboonDecoderFailure("Unexpected UEBA index count: " + indexCount);
       |${fields.joinN().trim}
       |return new $name(
       |    ${ctorFields.joinN().shift(4).trim}
       |);""".stripMargin
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[TsValue], TextTree[TsValue], TextTree[TsValue])] = {
    layout.fields(dto).map {
      case UebaLayoutPlan.FieldLayout(field, length) =>
        val fieldName        = field.name.name
        val escapedFieldName = typeTranslator.escapeTsKeyword(fieldName)
        // Property access uses the escaped getter name; local var name is also escaped to avoid
        // emitting `const default = ...` or `const class = ...` which are TS syntax errors.
        val enc     = mkEncoder(field.tpe, q"value.$escapedFieldName", "writer")
        val fakeEnc = mkEncoder(field.tpe, q"value.$escapedFieldName", "buffer")
        val dec     = q"const $escapedFieldName = ${mkDecoder(field.tpe)};"
        val indexOffset =
          if (length.isVariable) q"""if (before > ${UebaLayoutPlan.MaxIndexValue.toString}) throw new $tsBaboonEncoderFailure("UEBA index offset exceeds i32");
                                    |$tsBinTools.writeI32(writer, before);""".stripMargin else q""
        val indexLength = if (length.isVariable) q"$tsBinTools.writeI32(writer, length);" else q""
        val w = q"""{
                   |    const before = buffer.position();
                   |    $indexOffset
                   |    $fakeEnc
                   |    const after = buffer.position();
                   |    const length = after - before;
                   |    ${lengthChecks(length).shift(4).trim}
                   |    $indexLength
                   |}""".stripMargin

        (enc, dec, w)
    }
  }

  private def lengthChecks(length: BinReprLen): TextTree[TsValue] =
    UebaLengthCheckRenderer.render[TsValue](
      length,
      equalTo = bytes => q"length === ${bytes.toString}",
      oneOf   = bytes => q"[${bytes.mkString(", ")}].includes(length)",
      enforce = condition => q"""if (!($condition)) throw new $tsBaboonEncoderFailure("Invalid UEBA field length: " + length);""",
    )

  private def genDtoCodec(name: TsValue.TsType, dto: Typedef.Dto): (TextTree[TsValue], TextTree[TsValue]) = {
    val fields = fieldsOf(dto)
    val noIndex = Seq(
      q"$tsBinTools.writeByte(writer, 0x00)",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).joinN()

    val fenc =
      q"""if (ctx.useIndices) {
         |    $tsBinTools.writeByte(writer, 0x01);
         |    const buffer = new $tsBaboonBinWriter();
         |    ${fields.map(_._3).joinN().shift(4).trim}
         |    writer.writeAll(buffer.toBytes());
         |} else {
         |    ${noIndex.shift(4).trim}
         |}""".stripMargin

    val fdec = dtoDec(dto, name, fields.map(_._2))

    val enc = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, dto.id)

        q"""$tsBinTools.writeByte(writer, ${idx.toString})
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, dto.id)
        q"""const marker = $tsBinTools.readByte(reader)
           |if (marker !== ${idx.toString}) { throw new $tsBaboonDecoderFailure("Expected ADT branch marker ${idx.toString}, got " + marker); }
           |return this.decodeBranch(ctx, reader)""".stripMargin
      case _ => fdec
    }
    (enc, dec)
  }

  private def genEnumCodec(name: TsValue.TsType, e: Typedef.Enum): (TextTree[TsValue], TextTree[TsValue]) = {
    val lowercaseValues = target.language.enumLowercaseValues
    val encBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val value = if (lowercaseValues) m.name.toLowerCase else EnumWireStyle.wireName(m.name)
        q"""case "$value": $tsBinTools.writeByte(writer, ${idx.toString}); break;"""
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val value = if (lowercaseValues) m.name.toLowerCase else EnumWireStyle.wireName(m.name)
        q"""case ${idx.toString}: return "$value" as ${name.name};"""
    }

    (
      q"""switch (value) {
         |    ${encBranches.joinN().shift(8).trim}
         |    default: throw new Error("Unknown enum variant: " + value);
         |}""".stripMargin,
      q"""const tag = $tsBinTools.readByte(reader);
         |switch (tag) {
         |    ${decBranches.joinN().shift(8).trim}
         |    default: throw new Error("Unknown enum variant tag: " + tag);
         |}""".stripMargin,
    )
  }

  private def genAdtCodec(adt: Typedef.Adt): (TextTree[TsValue], TextTree[TsValue]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList

    val encBranches = branches.map {
      case (mid, idx) =>
        val branchType = typeTranslator.asTsType(mid, domain, evo, tsFileTools.definitionsBasePkg)
        val codecType  = codecName(branchType)
        if (target.language.wrappedAdtBranchCodecs) {
          q"""if (value instanceof $branchType) {
             |    $codecType.instance.encode(ctx, value, writer);
             |    return;
             |}""".stripMargin
        } else {
          q"""if (value instanceof $branchType) {
             |    $tsBinTools.writeByte(writer, ${idx.toString});
             |    $codecType.instance.encode(ctx, value, writer);
             |    return;
             |}""".stripMargin
        }
    }

    val decBranches = branches.map {
      case (mid, idx) =>
        val branchType = typeTranslator.asTsType(mid, domain, evo, tsFileTools.definitionsBasePkg)
        val codecType  = codecName(branchType)
        if (target.language.wrappedAdtBranchCodecs) {
          q"""case ${idx.toString}: return $codecType.instance.decodeBranch(ctx, reader)"""
        } else {
          q"""case ${idx.toString}: return $codecType.instance.decode(ctx, reader)"""
        }
    }

    (
      q"""${encBranches.joinN().shift(4).trim}
         |throw new Error("Unhandled ADT branch: " + (value as {constructor?: {name?: string}}).constructor?.name);""".stripMargin,
      q"""const tag = $tsBinTools.readByte(reader);
         |switch (tag) {
         |    ${decBranches.joinN().shift(8).trim}
         |    default: throw new Error("Unknown ADT branch tag: " + tag);
         |}""".stripMargin,
    )
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[TsValue], writer: String): TextTree[TsValue] = {
    val w = q"$writer"
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case b: TypeId.BuiltinScalar => scalarOps.encodeUeba(b, w, ref)
          case u: TypeId.User =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Typescript) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, writer)
                  case _ =>
                    // keep-foreigns naming matches the emitted `<F>_UEBACodec` and the decoder branch.
                    val tsType = typeTranslator.asTsTypeKeepForeigns(u, domain, evo, tsFileTools.definitionsBasePkg)
                    val codec  = codecName(tsType)
                    q"$codec.instance.encode(ctx, $ref, $w);"
                }
              case _ =>
                val tsType = typeTranslator.asTsTypeDerefForeign(u, domain, evo, tsFileTools.definitionsBasePkg)
                val codec  = codecName(tsType)
                q"$codec.instance.encode(ctx, $ref, $w);"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case TypeRef.Constructor(cid, args) =>
        cid match {
          case TypeId.Builtins.opt =>
            q"""if ($ref === undefined) {
               |    $tsBinTools.writeByte($w, 0);
               |} else {
               |    $tsBinTools.writeByte($w, 1);
               |    ${mkEncoder(args.head, ref, writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.lst =>
            q"""$tsBinTools.writeI32($w, Array.from($ref).length);
               |for (const item of $ref) {
               |    ${mkEncoder(args.head, q"item", writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.set =>
            q"""$tsBinTools.writeI32($w, $ref.size);
               |for (const item of $ref) {
               |    ${mkEncoder(args.head, q"item", writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.map =>
            val isRecord = typeTranslator.isStringKeyMap(tpe)
            if (isRecord) {
              // Wrap in a block to avoid duplicate `const` declarations when
              // multiple Record-typed map fields exist in the same DTO
              q"""{
                 |    const entries = Object.entries($ref);
                 |    $tsBinTools.writeI32($w, entries.length);
                 |    for (const [k, v] of entries) {
                 |        ${mkEncoder(args.head, q"k", writer).shift(8).trim}
                 |        ${mkEncoder(args.last, q"v", writer).shift(8).trim}
                 |    }
                 |}""".stripMargin
            } else {
              q"""$tsBinTools.writeI32($w, $ref.size);
                 |for (const [k, v] of $ref) {
                 |    ${mkEncoder(args.head, q"k", writer).shift(4).trim}
                 |    ${mkEncoder(args.last, q"v", writer).shift(4).trim}
                 |}""".stripMargin
            }
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref, writer)
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[TsValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case b: TypeId.BuiltinScalar => scalarOps.decodeUeba(b, q"reader")
          case u: TypeId.User =>
            domain.defs.meta.nodes.get(u) match {
              case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
                f.bindings.get(BaboonLang.Typescript) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ =>
                    val tsType = typeTranslator.asTsTypeKeepForeigns(u, domain, evo, tsFileTools.definitionsBasePkg)
                    val codec  = codecName(tsType)
                    q"$codec.instance.decode(ctx, reader)"
                }
              case _ =>
                val tsType = typeTranslator.asTsTypeKeepForeigns(u, domain, evo, tsFileTools.definitionsBasePkg)
                val codec  = codecName(tsType)
                q"$codec.instance.decode(ctx, reader)"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case TypeRef.Constructor(cid, args) =>
        cid match {
          case TypeId.Builtins.opt =>
            q"($tsBinTools.readByte(reader) === 0 ? undefined : ${mkDecoder(args.head)})"
          case TypeId.Builtins.lst =>
            q"Array.from({ length: $tsBinTools.readI32(reader) }, () => ${mkDecoder(args.head)})"
          case TypeId.Builtins.set =>
            q"new Set(Array.from({ length: $tsBinTools.readI32(reader) }, () => ${mkDecoder(args.head)}))"
          case TypeId.Builtins.map =>
            val isRecord = typeTranslator.isStringKeyMap(tpe)
            if (isRecord) {
              q"Object.fromEntries(Array.from({ length: $tsBinTools.readI32(reader) }, () => [${mkDecoder(args.head)}, ${mkDecoder(args.last)}] as const))"
            } else {
              q"new Map(Array.from({ length: $tsBinTools.readI32(reader) }, () => [${mkDecoder(args.head)}, ${mkDecoder(args.last)}] as const))"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a)
    }
  }

  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[TsValue], writer: String): TextTree[TsValue] = {
    val plan = TsAnyFieldPlan.forField(a, domain)
    q"$tsEncodeAnyUebaField(ctx, $writer, ${plan.kindHex}, ${plan.staticDomain}, ${plan.staticVersion}, ${plan.staticTypeId}, $ref);"
  }

  private def mkAnyDecoder(a: TypeRef.Any): TextTree[TsValue] = {
    val plan = TsAnyFieldPlan.forField(a, domain)
    q"$tsDecodeAnyUebaField(reader, ${plan.kindHex})"
  }

  def codecName(name: TsValue.TsType): TsValue.TsType = {
    TsValue.TsType(name.moduleId, s"${name.name}_UEBACodec")
  }

  override def codecMeta(definition: DomainMember.User, name: TsValue.TsType): Option[TextTree[TsValue]] = {
    if (isActive(definition.id)) {
      definition.defn match {
        case _: Typedef.Adt =>
          Some(q"""binCodec(): ${codecName(name)} {
                  |    return ${codecName(name)}.instance
                  |}""".stripMargin)
        case _ =>
          Some(q"""public static binCodec(): ${codecName(name)} {
                  |    return ${codecName(name)}.instance
                  |}""".stripMargin)
      }
    } else None
  }

  override def isActive(id: TypeId): Boolean = TsCodecActivation.isActive(target, domain, id, TsCodecActivation.Ueba)

  override def id: String = "Ueba"
}
