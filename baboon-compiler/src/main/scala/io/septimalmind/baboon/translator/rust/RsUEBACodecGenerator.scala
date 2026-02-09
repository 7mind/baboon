package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.toSnakeCase
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
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
      defn.defn match {
        case d: Typedef.Dto     => Some(genDtoCodec(rsRef, d))
        case e: Typedef.Enum    => Some(genEnumCodec(rsRef, e))
        case a: Typedef.Adt     => Some(genAdtCodec(rsRef, a))
        case _: Typedef.Foreign => None
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }
    } else None
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

  private def genDtoCodec(name: RsValue.RsType, dto: Typedef.Dto): TextTree[RsValue] = {
    val encFields = dto.fields.map { f =>
      val fieldRef = q"value.${toSnakeCase(f.name.name)}"
      if (needsBox(f.tpe)) {
        mkEncoder(f.tpe, q"(*$fieldRef)")
      } else {
        mkEncoder(f.tpe, fieldRef)
      }
    }

    val decFields = dto.fields.map { f =>
      val decoder = mkDecoder(f.tpe)
      if (needsBox(f.tpe)) {
        q"let ${toSnakeCase(f.name.name)} = Box::new($decoder);"
      } else {
        q"let ${toSnakeCase(f.name.name)} = $decoder;"
      }
    }

    val ctorFields = dto.fields.map { f =>
      q"${toSnakeCase(f.name.name)},"
    }

    q"""impl crate::baboon_runtime::BaboonBinEncode for ${name.asName} {
       |    fn encode_ueba(&self, ctx: &crate::baboon_runtime::BaboonCodecContext, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
       |        let value = self;
       |        crate::baboon_runtime::bin_tools::write_byte(writer, 0)?;
       |        ${encFields.joinN().shift(8).trim}
       |        Ok(())
       |    }
       |}
       |
       |impl crate::baboon_runtime::BaboonBinDecode for ${name.asName} {
       |    fn decode_ueba(ctx: &crate::baboon_runtime::BaboonCodecContext, reader: &mut dyn std::io::Read) -> Result<Self, Box<dyn std::error::Error>> {
       |        let _header = crate::baboon_runtime::bin_tools::read_byte(reader)?;
       |        ${decFields.joinN().shift(8).trim}
       |        Ok(${name.asName} {
       |            ${ctorFields.joinN().shift(12).trim}
       |        })
       |    }
       |}""".stripMargin
  }

  private def genEnumCodec(name: RsValue.RsType, e: Typedef.Enum): TextTree[RsValue] = {
    val encBranches = e.members.zipWithIndex.toList.map { case (m, idx) =>
      q"""${name.asName}::${m.name.capitalize} => crate::baboon_runtime::bin_tools::write_byte(writer, ${idx.toString})?,"""
    }

    val decBranches = e.members.zipWithIndex.toList.map { case (m, idx) =>
      q"""${idx.toString} => Ok(${name.asName}::${m.name.capitalize}),"""
    }

    q"""impl crate::baboon_runtime::BaboonBinEncode for ${name.asName} {
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
       |            _ => Err(format!("Unknown enum variant tag: {}", tag).into()),
       |        }
       |    }
       |}""".stripMargin
  }

  private def genAdtCodec(name: RsValue.RsType, adt: Typedef.Adt): TextTree[RsValue] = {
    val branches = adt.dataMembers(domain).zipWithIndex.toList

    val encBranches = branches.map { case (mid, idx) =>
      val branchName = mid.name.name.capitalize
      q"""${name.asName}::$branchName(v) => {
         |    crate::baboon_runtime::bin_tools::write_byte(writer, ${idx.toString})?;
         |    v.encode_ueba(ctx, writer)?;
         |}""".stripMargin
    }

    val decBranches = branches.map { case (mid, idx) =>
      val branchName = mid.name.name.capitalize
      val branchType = trans.asRsType(mid, domain, evo)
      q"""${idx.toString} => {
         |    let v = ${branchType.asName}::decode_ueba(ctx, reader)?;
         |    Ok(${name.asName}::$branchName(v))
         |}""".stripMargin
    }

    q"""impl crate::baboon_runtime::BaboonBinEncode for ${name.asName} {
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
  private def mkEncoder(tpe: TypeRef, ref: TextTree[RsValue]): TextTree[RsValue] = {
    tpe match {
      case TypeRef.Scalar(_) =>
        q"$ref.encode_ueba(ctx, writer)?;"
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""match &$ref {
               |    None => crate::baboon_runtime::bin_tools::write_byte(writer, 0)?,
               |    Some(v) => {
               |        crate::baboon_runtime::bin_tools::write_byte(writer, 1)?;
               |        ${mkEncoder(c.args.head, q"v").shift(8).trim}
               |    }
               |}""".stripMargin
          case TypeId.Builtins.lst =>
            q"""crate::baboon_runtime::bin_tools::write_i32(writer, $ref.len() as i32)?;
               |for item in ($ref).iter() {
               |    ${mkEncoder(c.args.head, q"item").shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.set =>
            q"""crate::baboon_runtime::bin_tools::write_i32(writer, $ref.len() as i32)?;
               |for item in ($ref).iter() {
               |    ${mkEncoder(c.args.head, q"item").shift(4).trim}
               |}""".stripMargin
          case TypeId.Builtins.map =>
            q"""crate::baboon_runtime::bin_tools::write_i32(writer, $ref.len() as i32)?;
               |for (k, v) in ($ref).iter() {
               |    ${mkEncoder(c.args.head, q"k").shift(4).trim}
               |    ${mkEncoder(c.args.last, q"v").shift(4).trim}
               |}""".stripMargin
          case o => throw new RuntimeException(s"BUG: Unexpected collection type: $o")
        }
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
            val tpe = trans.asRsType(u, domain, evo)
            q"${tpe.asName}::decode_ueba(ctx, reader)?"
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
    }
  }

  def codecName(name: RsValue.RsType): RsValue.RsType = {
    RsValue.RsType(name.crate, s"${name.name}_UEBACodec", name.fq)
  }

  override def isActive(id: TypeId): Boolean = {
    target.language.generateUebaCodecs && (target.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "Ueba"
}
