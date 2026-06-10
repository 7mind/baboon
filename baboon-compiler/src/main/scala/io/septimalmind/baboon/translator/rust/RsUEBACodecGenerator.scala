package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.{escapeRustTypeName, toSnakeCase}
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class RsUEBACodecGenerator(
  trans: RsTypeTranslator,
  target: RsTarget,
  domain: Domain,
  evo: BaboonEvolution,
  enquiries: BaboonEnquiries,
) extends RsCodecTranslator {

  override def translate(defn: DomainMember.User, rsRef: RsValue.RsType, srcRef: RsValue.RsType): Option[TextTree[RsValue]] = {
    if (isActive(defn.id)) {
      val body = defn.defn match {
        case d: Typedef.Dto      => Some(genDtoCodec(defn, rsRef, d))
        case e: Typedef.Enum     => Some(genEnumCodec(defn, rsRef, e))
        case a: Typedef.Adt      => Some(genAdtCodec(defn, rsRef, a))
        case _: Typedef.Foreign  => None
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }
      // Prepend per-codec any-field helpers (free module-level functions) when the DTO has any
      // any-bearing field. Mirrors PR 3.2 (C# `EncodeAnyField`/`DecodeAnyField`) and PR 2.2
      // (Scala `encodeAnyField`/`decodeAnyField`). One emission per DTO file because the codec
      // generator's output is concatenated into the DTO's `.rs` module.
      body.map {
        b =>
          if (hasAnyField(defn)) {
            q"""$anyFieldHelpers
               |
               |$b""".stripMargin
          } else b
      }
    } else None
  }

  // Deep walk (mirrors Scala's hasAnyField): a codec object needs the any-field helpers if any
  // direct or nested-via-Constructor-arg field has type `any`.
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

  // Per-codec free module-level helpers consolidating the any-field framing, kind-check, and
  // buffer-then-write / read-then-skip paths. Mirrors `ScUEBACodecGenerator.anyFieldHelpers` and
  // `CSUEBACodecGenerator.anyFieldHelpers`. Wire layout (locked, see
  // docs/drafts/20260424-1738-any-opaque-fields.md §"Wire format"):
  //   length:i32 | meta-length:i32 | meta-kind:u8 | meta-strings | blob
  // length covers everything after itself; meta-length covers (kind + strings); blob runs the
  // rest. The helpers wrap `BaboonCodecError` into `std::io::Error` for the encoder (which
  // returns `std::io::Result<()>`) and into `Box<dyn std::error::Error>` for the decoder
  // (which already uses that error type).
  private def anyFieldHelpers: TextTree[RsValue] = {
    q"""fn encode_any_field(
       |    ctx: &crate::baboon_runtime::BaboonCodecContext,
       |    writer: &mut dyn std::io::Write,
       |    expected_kind: u8,
       |    static_domain: Option<&str>,
       |    static_version: Option<&str>,
       |    static_typeid: Option<&str>,
       |    value: &crate::any_opaque::AnyOpaque,
       |) -> std::io::Result<()> {
       |    if value.meta().kind != expected_kind {
       |        return Err(std::io::Error::new(
       |            std::io::ErrorKind::InvalidData,
       |            format!(
       |                "any: meta-kind 0x{:02x} does not match field-declared 0x{:02x}",
       |                value.meta().kind, expected_kind
       |            ),
       |        ));
       |    }
       |    let any_blob: Vec<u8> = match value {
       |        crate::any_opaque::AnyOpaque::Ueba(u) => u.bytes.clone(),
       |        crate::any_opaque::AnyOpaque::Json(j) => {
       |            let f = ctx.facade().ok_or_else(|| std::io::Error::new(
       |                std::io::ErrorKind::InvalidData,
       |                "Cannot encode AnyOpaque::Json into UEBA without a facade reference. Construct the codec context via BaboonCodecContext::with_facade(use_indices, facade), or supply AnyOpaque::Ueba directly."
       |            ))?;
       |            f.json_to_ueba_bytes(&j.meta, &j.json, static_domain, static_version, static_typeid)
       |                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{}", e)))?
       |        }
       |    };
       |    // Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
       |    let mut any_meta_buf: Vec<u8> = Vec::new();
       |    crate::any_opaque::any_meta_codec::write_bin(value.meta(), &mut any_meta_buf)?;
       |    let any_total_length: i32 = (4 + any_meta_buf.len() + any_blob.len()) as i32;
       |    crate::baboon_runtime::bin_tools::write_i32(writer, any_total_length)?;
       |    crate::baboon_runtime::bin_tools::write_i32(writer, any_meta_buf.len() as i32)?;
       |    writer.write_all(&any_meta_buf)?;
       |    writer.write_all(&any_blob)?;
       |    Ok(())
       |}
       |
       |fn decode_any_field(
       |    wire: &mut dyn std::io::Read,
       |    expected_kind: u8,
       |) -> Result<crate::any_opaque::AnyOpaqueUeba, Box<dyn std::error::Error>> {
       |    let any_total_length_i = crate::baboon_runtime::bin_tools::read_i32(wire)?;
       |    if any_total_length_i < 0 {
       |        return Err(format!(
       |            "any: negative total-length {}", any_total_length_i
       |        ).into());
       |    }
       |    let any_total_length = any_total_length_i as usize;
       |    let any_meta_length_i = crate::baboon_runtime::bin_tools::read_i32(wire)?;
       |    if any_meta_length_i < 0 {
       |        return Err(format!(
       |            "any: negative meta-length {}", any_meta_length_i
       |        ).into());
       |    }
       |    let any_meta_length = any_meta_length_i as usize;
       |    if any_total_length < 4 + any_meta_length {
       |        return Err(format!(
       |            "any: total-length {} smaller than 4 + meta-length {}",
       |            any_total_length, any_meta_length
       |        ).into());
       |    }
       |    let (any_meta, any_bytes_read) = crate::any_opaque::any_meta_codec::read_bin_with_length(wire)
       |        .map_err(|e| -> Box<dyn std::error::Error> { Box::new(e) })?;
       |    if any_bytes_read > any_meta_length {
       |        return Err(format!(
       |            "any: meta-bytes-read {} exceeded meta-length window {}",
       |            any_bytes_read, any_meta_length
       |        ).into());
       |    }
       |    if any_bytes_read < any_meta_length {
       |        // Forward-compat: skip future meta-extension bytes within the meta-length window.
       |        let mut any_skip = vec![0u8; any_meta_length - any_bytes_read];
       |        wire.read_exact(&mut any_skip)?;
       |    }
       |    if any_meta.kind != expected_kind {
       |        return Err(format!(
       |            "any: wire kind 0x{:02x} does not match field-declared 0x{:02x}",
       |            any_meta.kind, expected_kind
       |        ).into());
       |    }
       |    let any_blob_len = any_total_length - 4 - any_meta_length;
       |    let mut any_blob = vec![0u8; any_blob_len];
       |    wire.read_exact(&mut any_blob)?;
       |    Ok(crate::any_opaque::AnyOpaqueUeba::new(any_meta, any_blob))
       |}""".stripMargin
  }

  // Encode delegates to the per-DTO `encode_any_field` helper. This site wires the expected kind
  // byte and the field's static (codec-gen-time) fallbacks for cross-format meta resolution.
  // See `anyStaticFallbacks` for the per-variant table.
  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[RsValue], wref: TextTree[RsValue]): TextTree[RsValue] = {
    val expectedKind                      = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex                       = "0x%02x".format(expectedKind & 0xFF)
    val (staticDom, staticVer, staticTid) = anyStaticFallbacks(a)
    q"encode_any_field(ctx, $wref, ${expectedHex}u8, $staticDom, $staticVer, $staticTid, &$ref)?;"
  }

  // Decode delegates to the per-DTO `decode_any_field` helper, returning an `AnyOpaqueUeba`
  // wrapped into the `AnyOpaque::Ueba` enum variant so the surface field type matches
  // `RsTypeTranslator.asRsRef(TypeRef.Any) = AnyOpaque`.
  private def mkAnyDecoder(a: TypeRef.Any): TextTree[RsValue] = {
    val expectedKind = AnyVariant.metaKindByte(a.variant, a.underlying.isDefined)
    val expectedHex  = "0x%02x".format(expectedKind & 0xFF)
    q"crate::any_opaque::AnyOpaque::Ueba(decode_any_field(reader, ${expectedHex}u8)?)"
  }

  // Static fallbacks for the cross-format facade helpers (`json_to_ueba_bytes`/`ueba_to_json`).
  // The wire `meta` may omit components that are pinned by the field's static declaration; the
  // codec emits whatever is statically known so the facade can fill the gaps. See
  // `BaboonCodecsFacade::build_synthetic_type_meta` for the merge semantics. Per spec table:
  //   A=(None,None,None), B=(currentDomain,None,None), C=(currentDomain,currentVersion,None),
  //   D1=(None,None,underlyingFqid), D2=(currentDomain,None,underlyingFqid),
  //   D3=(currentDomain,currentVersion,underlyingFqid).
  private def anyStaticFallbacks(a: TypeRef.Any): (TextTree[RsValue], TextTree[RsValue], TextTree[RsValue]) = {
    val none                     = q"None"
    def some(s: String)          = q"""Some("$s")"""
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

  private def needsBox(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Scalar(u: TypeId.User) =>
        domain.defs.meta.nodes.get(u).exists {
          case m: DomainMember.User => enquiries.isRecursiveTypedef(m, domain)
          case _                    => false
        }
      case TypeRef.Constructor(_, args) => args.exists(needsBox)
      case _                            => false
    }
  }

  private def genIndexedImpl(defn: DomainMember.User, name: RsValue.RsType): TextTree[RsValue] = {
    val indexCount = defn.defn match {
      case d: Typedef.Dto =>
        d.fields.count(f => domain.refMeta(f.tpe).len.isVariable)
      case _: Typedef.Enum => 0
      case _: Typedef.Adt  => 0
      case _               => 0
    }

    q"""impl crate::baboon_runtime::BaboonBinCodecIndexed for ${name.asName} {
       |    fn index_elements_count(_ctx: &crate::baboon_runtime::BaboonCodecContext) -> u16 {
       |        ${indexCount.toString}
       |    }
       |}""".stripMargin
  }

  private def adtBranchIndex(adtId: TypeId.User, dtoId: TypeId): Int = {
    domain.defs.meta
      .nodes(adtId)
      .asInstanceOf[DomainMember.User]
      .defn
      .asInstanceOf[Typedef.Adt]
      .dataMembers(domain)
      .zipWithIndex
      .find(_._1 == dtoId)
      .get
      ._2
  }

  private def genDtoCodec(defn: DomainMember.User, name: RsValue.RsType, dto: Typedef.Dto): TextTree[RsValue] = {
    // Compact mode encoder: fields written directly to writer
    val encFields = dto.fields.map {
      f =>
        val fieldRef = q"value.${toSnakeCase(f.name.name)}"
        if (needsBox(f.tpe)) {
          mkEncoder(f.tpe, q"(*$fieldRef)", q"writer")
        } else {
          mkEncoder(f.tpe, fieldRef, q"writer")
        }
    }

    // Indexed mode encoder: fields written to buffer, index entries to main writer
    // Uses &mut buffer directly (not a long-lived binding) so borrow is dropped between calls,
    // allowing buffer.len() reads for variable-length field index entries.
    val indexedEncFields = dto.fields.map {
      f =>
        val fieldRef   = q"value.${toSnakeCase(f.name.name)}"
        val actualRef  = if (needsBox(f.tpe)) q"(*$fieldRef)" else fieldRef
        val fakeEnc    = mkEncoder(f.tpe, actualRef, q"&mut buffer")
        val isVariable = domain.refMeta(f.tpe).len.isVariable

        if (isVariable) {
          q"""{
             |    let before = buffer.len();
             |    crate::baboon_runtime::bin_tools::write_i32(writer, before as i32)?;
             |    $fakeEnc
             |    let after = buffer.len();
             |    let length = after - before;
             |    crate::baboon_runtime::bin_tools::write_i32(writer, length as i32)?;
             |}""".stripMargin
        } else {
          fakeEnc
        }
    }

    val decFields = dto.fields.map {
      f =>
        val decoder = mkDecoder(f.tpe)
        if (needsBox(f.tpe)) {
          q"let ${toSnakeCase(f.name.name)} = Box::new($decoder);"
        } else {
          q"let ${toSnakeCase(f.name.name)} = $decoder;"
        }
    }

    val ctorFields = dto.fields.map {
      f =>
        q"${toSnakeCase(f.name.name)},"
    }

    val indexedImpl = genIndexedImpl(defn, name)

    val encPrefix = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id, dto.id)
        Some(q"crate::baboon_runtime::bin_tools::write_byte(writer, ${idx.toString})?;")
      case _ => None
    }

    val encPrefixTree = encPrefix
      .map(p => q"""$p
                   |        """.stripMargin).getOrElse(q"")

    val branchDecodeFn = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        Some(
          q"""impl ${name.asName} {
             |    pub fn decode_ueba_branch(ctx: &crate::baboon_runtime::BaboonCodecContext, reader: &mut dyn std::io::Read) -> Result<Self, Box<dyn std::error::Error>> {
             |        use crate::baboon_runtime::BaboonBinDecode;
             |        let (_header, index) = <Self as crate::baboon_runtime::BaboonBinCodecIndexed>::read_index(ctx, reader)?;
             |        if ctx.use_indices() {
             |            assert_eq!(index.len(), <Self as crate::baboon_runtime::BaboonBinCodecIndexed>::index_elements_count(ctx) as usize);
             |        }
             |        ${decFields.joinN().shift(8).trim}
             |        Ok(${name.asName} {
             |            ${ctorFields.joinN().shift(12).trim}
             |        })
             |    }
             |}""".stripMargin
        )
      case _ => None
    }

    val decBody = dto.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id, dto.id)
        q"""let marker = crate::baboon_runtime::bin_tools::read_byte(reader)?;
           |assert_eq!(marker, ${idx.toString}, "Expected ADT branch marker ${idx.toString}, got {}", marker);
           |Self::decode_ueba_branch(ctx, reader)""".stripMargin
      case _ =>
        q"""let (_header, index) = <Self as crate::baboon_runtime::BaboonBinCodecIndexed>::read_index(ctx, reader)?;
           |if ctx.use_indices() {
           |    assert_eq!(index.len(), <Self as crate::baboon_runtime::BaboonBinCodecIndexed>::index_elements_count(ctx) as usize);
           |}
           |${decFields.joinN().shift(0).trim}
           |Ok(${name.asName} {
           |    ${ctorFields.joinN().shift(4).trim}
           |})""".stripMargin
    }

    val branchDecodeFnTree = branchDecodeFn
      .map(fn => q"""$fn
                    |""".stripMargin).getOrElse(q"")

    q"""$indexedImpl
       |
       |${branchDecodeFnTree}impl crate::baboon_runtime::BaboonBinEncode for ${name.asName} {
       |    fn encode_ueba(&self, ctx: &crate::baboon_runtime::BaboonCodecContext, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
       |        ${if (dto.fields.nonEmpty) "let value = self;" else "let _value = self;"}
       |        ${encPrefixTree}if ctx.use_indices() {
       |            crate::baboon_runtime::bin_tools::write_byte(writer, 0x01)?;
       |            let ${if (dto.fields.nonEmpty) "mut " else ""}buffer: Vec<u8> = Vec::new();
       |            ${indexedEncFields.joinN().shift(12).trim}
       |            writer.write_all(&buffer)?;
       |        } else {
       |            crate::baboon_runtime::bin_tools::write_byte(writer, 0x00)?;
       |            ${encFields.joinN().shift(12).trim}
       |        }
       |        Ok(())
       |    }
       |}
       |
       |impl crate::baboon_runtime::BaboonBinDecode for ${name.asName} {
       |    fn decode_ueba(ctx: &crate::baboon_runtime::BaboonCodecContext, reader: &mut dyn std::io::Read) -> Result<Self, Box<dyn std::error::Error>> {
       |        ${decBody.shift(8).trim}
       |    }
       |}""".stripMargin
  }

  private def genEnumCodec(defn: DomainMember.User, name: RsValue.RsType, e: Typedef.Enum): TextTree[RsValue] = {
    val encBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"""${name.asName}::${EnumWireStyle.wireName(m.name)} => crate::baboon_runtime::bin_tools::write_byte(writer, ${idx.toString})?,"""
    }

    val decBranches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        q"""${idx.toString} => Ok(${name.asName}::${EnumWireStyle.wireName(m.name)}),"""
    }

    val indexedImpl = genIndexedImpl(defn, name)

    q"""$indexedImpl
       |
       |impl crate::baboon_runtime::BaboonBinEncode for ${name.asName} {
       |    fn encode_ueba(&self, _ctx: &crate::baboon_runtime::BaboonCodecContext, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
       |        match self {
       |            ${encBranches.joinN().shift(12).trim}
       |        }
       |        Ok(())
       |    }
       |}
       |
       |impl crate::baboon_runtime::BaboonBinDecode for ${name.asName} {
       |    fn decode_ueba(_ctx: &crate::baboon_runtime::BaboonCodecContext, reader: &mut dyn std::io::Read) -> Result<Self, Box<dyn std::error::Error>> {
       |        let tag = crate::baboon_runtime::bin_tools::read_byte(reader)?;
       |        match tag {
       |            ${decBranches.joinN().shift(12).trim}
       |            _ => Err(format!("Unknown enum variant tag: {}", tag).into()),
       |        }
       |    }
       |}""".stripMargin
  }

  private def genAdtCodec(defn: DomainMember.User, name: RsValue.RsType, adt: Typedef.Adt): TextTree[RsValue] = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList

    val encBranches = branches.map {
      case (mid, idx) =>
        val branchName = escapeRustTypeName(mid.name.name.capitalize)
        if (target.language.wrappedAdtBranchCodecs) {
          q"""${name.asName}::$branchName(v) => {
             |    v.encode_ueba(ctx, writer)?;
             |}""".stripMargin
        } else {
          q"""${name.asName}::$branchName(v) => {
             |    crate::baboon_runtime::bin_tools::write_byte(writer, ${idx.toString})?;
             |    v.encode_ueba(ctx, writer)?;
             |}""".stripMargin
        }
    }

    val decBranches = branches.map {
      case (mid, idx) =>
        val branchName = escapeRustTypeName(mid.name.name.capitalize)
        val branchType = trans.asRsType(mid, domain, evo)
        if (target.language.wrappedAdtBranchCodecs) {
          q"""${idx.toString} => {
             |    let v = ${branchType.asName}::decode_ueba_branch(ctx, reader)?;
             |    Ok(${name.asName}::$branchName(v))
             |}""".stripMargin
        } else {
          q"""${idx.toString} => {
             |    let v = ${branchType.asName}::decode_ueba(ctx, reader)?;
             |    Ok(${name.asName}::$branchName(v))
             |}""".stripMargin
        }
    }

    val indexedImpl = genIndexedImpl(defn, name)

    q"""$indexedImpl
       |
       |impl crate::baboon_runtime::BaboonBinEncode for ${name.asName} {
       |    fn encode_ueba(&self, ctx: &crate::baboon_runtime::BaboonCodecContext, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
       |        match self {
       |            ${encBranches.joinN().shift(12).trim}
       |        }
       |        Ok(())
       |    }
       |}
       |
       |impl crate::baboon_runtime::BaboonBinDecode for ${name.asName} {
       |    fn decode_ueba(ctx: &crate::baboon_runtime::BaboonCodecContext, reader: &mut dyn std::io::Read) -> Result<Self, Box<dyn std::error::Error>> {
       |        let tag = crate::baboon_runtime::bin_tools::read_byte(reader)?;
       |        match tag {
       |            ${decBranches.joinN().shift(12).trim}
       |            _ => Err(format!("Unknown ADT branch tag: {}", tag).into()),
       |        }
       |    }
       |}""".stripMargin
  }

  // Refs passed to mkEncoder may be place expressions (value.field) or references (&T).
  // - .encode_ueba() works via auto-ref/auto-deref for any ref level
  // - .len() and .iter() work via auto-deref for any ref level
  // - match needs &$ref to avoid moving out of borrow; match ergonomics handles &&T
  private def mkEncoder(tpe: TypeRef, ref: TextTree[RsValue], writer: TextTree[RsValue] = q"writer"): TextTree[RsValue] = {
    BaboonEnquiries.resolveBaboonRef(tpe, domain, BaboonLang.Rust) match {
      case TypeRef.Scalar(_) =>
        q"$ref.encode_ueba(ctx, $writer)?;"
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""match &$ref {
               |    None => crate::baboon_runtime::bin_tools::write_byte($writer, 0)?,
               |    Some(v) => {
               |        crate::baboon_runtime::bin_tools::write_byte($writer, 1)?;
               |        ${mkEncoder(c.args.head, q"v", writer).shift(8).trim}
               |    }
               |}""".stripMargin
          case TypeId.Builtins.lst =>
            q"""crate::baboon_runtime::bin_tools::write_i32($writer, $ref.len() as i32)?;
               |for item in ($ref).iter() {
               |    ${mkEncoder(c.args.head, q"item", writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.set =>
            q"""crate::baboon_runtime::bin_tools::write_i32($writer, $ref.len() as i32)?;
               |for item in ($ref).iter() {
               |    ${mkEncoder(c.args.head, q"item", writer).shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.map =>
            q"""crate::baboon_runtime::bin_tools::write_i32($writer, $ref.len() as i32)?;
               |for (k, v) in ($ref).iter() {
               |    ${mkEncoder(c.args.head, q"k", writer).shift(4).trim}
               |    ${mkEncoder(c.args.last, q"v", writer).shift(4).trim}
               |}""".stripMargin
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref, writer)
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[RsValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.bit   => q"crate::baboon_runtime::bin_tools::read_bool(reader)?"
          case TypeId.Builtins.i08   => q"crate::baboon_runtime::bin_tools::read_i8(reader)?"
          case TypeId.Builtins.i16   => q"crate::baboon_runtime::bin_tools::read_i16(reader)?"
          case TypeId.Builtins.i32   => q"crate::baboon_runtime::bin_tools::read_i32(reader)?"
          case TypeId.Builtins.i64   => q"crate::baboon_runtime::bin_tools::read_i64(reader)?"
          case TypeId.Builtins.u08   => q"crate::baboon_runtime::bin_tools::read_u8(reader)?"
          case TypeId.Builtins.u16   => q"crate::baboon_runtime::bin_tools::read_u16(reader)?"
          case TypeId.Builtins.u32   => q"crate::baboon_runtime::bin_tools::read_u32(reader)?"
          case TypeId.Builtins.u64   => q"crate::baboon_runtime::bin_tools::read_u64(reader)?"
          case TypeId.Builtins.f32   => q"crate::baboon_runtime::bin_tools::read_f32(reader)?"
          case TypeId.Builtins.f64   => q"crate::baboon_runtime::bin_tools::read_f64(reader)?"
          case TypeId.Builtins.f128  => q"crate::baboon_runtime::bin_tools::read_decimal(reader)?"
          case TypeId.Builtins.str   => q"crate::baboon_runtime::bin_tools::read_string(reader)?"
          case TypeId.Builtins.bytes => q"crate::baboon_runtime::bin_tools::read_bytes(reader)?"
          case TypeId.Builtins.uid   => q"crate::baboon_runtime::bin_tools::read_uuid(reader)?"
          case TypeId.Builtins.tsu   => q"crate::baboon_runtime::bin_tools::read_timestamp_utc(reader)?"
          case TypeId.Builtins.tso   => q"crate::baboon_runtime::bin_tools::read_timestamp_offset(reader)?"
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Rust) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef)
                  case _ =>
                    val tpe = trans.asRsType(u, domain, evo)
                    q"${tpe.asName}::decode_ueba(ctx, reader)?"
                }
              case _ =>
                val tpe = trans.asRsType(u, domain, evo)
                q"${tpe.asName}::decode_ueba(ctx, reader)?"
            }
          case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""{
               |    let tag = crate::baboon_runtime::bin_tools::read_byte(reader)?;
               |    if tag == 0 { None } else { Some(${mkDecoder(c.args.head)}) }
               |}""".stripMargin
          case TypeId.Builtins.lst =>
            q"""{
               |    let count = crate::baboon_runtime::bin_tools::read_i32(reader)? as usize;
               |    (0..count).map(|_| Ok(${mkDecoder(c.args.head)})).collect::<Result<Vec<_>, Box<dyn std::error::Error>>>()?
               |}""".stripMargin
          case TypeId.Builtins.set =>
            q"""{
               |    let count = crate::baboon_runtime::bin_tools::read_i32(reader)? as usize;
               |    (0..count).map(|_| Ok(${mkDecoder(c.args.head)})).collect::<Result<std::collections::BTreeSet<_>, Box<dyn std::error::Error>>>()?
               |}""".stripMargin
          case TypeId.Builtins.map =>
            q"""{
               |    let count = crate::baboon_runtime::bin_tools::read_i32(reader)? as usize;
               |    (0..count).map(|_| {
               |        let k = ${mkDecoder(c.args.head)};
               |        let v = ${mkDecoder(c.args.last)};
               |        Ok((k, v))
               |    }).collect::<Result<std::collections::BTreeMap<_, _>, Box<dyn std::error::Error>>>()?
               |}""".stripMargin
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a)
    }
  }

  def codecName(name: RsValue.RsType): RsValue.RsType = {
    RsValue.RsType(name.crate, s"${name.name}_UEBACodec", name.fq)
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Rust) &&
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
