package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSUEBACodecGenerator(trans: CSTypeTranslator,
                           tools: CSDefnTools,
                           options: CompilerOptions)
    extends CSCodecTranslator {
  override def translate(defn: DomainMember.User,
                         csRef: CSValue.CSType,
                         srcRef: CSValue.CSType,
                         domain: Domain,
                         evo: BaboonEvolution): TextTree[CSValue] = {
    val version = domain.version
    val (enc, dec) = defn.defn match {
      case d: Typedef.Dto =>
        genDtoBodies(csRef, domain, d, evo)
      case e: Typedef.Enum =>
        genEnumBodies(csRef, e)
      case a: Typedef.Adt =>
        genAdtBodies(csRef, domain, a, evo)
      case _: Typedef.Foreign =>
        genForeignBodies(csRef)
    }

    // plumbing reference leaks
    val insulatedEnc =
      q"""
         |#pragma warning disable CS0162
         |if (this == instance.Value)
         |{
         |    ${enc.shift(4).trim}
         |    return;
         |}
         |#pragma warning disable CS0162
         |
         |instance.Value.Encode(writer, value);""".stripMargin
    val insulatedDec =
      q"""if (this == instance.Value)
         |{
         |    ${dec.shift(4).trim}
         |}
         |
         |return instance.Value.Decode(wire);""".stripMargin

    genCodec(
      defn,
      csRef,
      srcRef,
      version,
      insulatedEnc,
      insulatedDec,
      !defn.defn.isInstanceOf[Typedef.Foreign]
    )
  }

  private def genCodec(defn: DomainMember.User,
                       name: CSValue.CSType,
                       srcRef: CSValue.CSType,
                       version: Version,
                       enc: TextTree[CSValue],
                       dec: TextTree[CSValue],
                       addExtensions: Boolean,
  ): TextTree[CSValue] = {
    val iName = q"$iBaboonBinCodec<$name>"

    val baseMethods = List(
      q"""public virtual void Encode($binaryWriter writer, $name value)
         |{
         |    ${enc.shift(4).trim}
         |}
         |public virtual $name Decode($binaryReader wire) {
         |    ${dec.shift(4).trim}
         |}""".stripMargin
    )

    val (parents, methods) = defn.defn match {
      case _: Typedef.Enum =>
        (List(q"$iBaboonBinCodec<$name>"), baseMethods)
      case _ =>
        val extensions = List(
          q"""public virtual void Encode($binaryWriter writer, $iBaboonGenerated value)
             |{
             |    if (value is not $name dvalue)
             |        throw new Exception("Expected to have ${name.name} type");
             |    Encode(writer, dvalue);
             |}
             |
             |$iBaboonGenerated $iBaboonStreamCodec<$iBaboonGenerated, $binaryWriter, $binaryReader>.Decode($binaryReader wire)
             |{
             |    return Decode(wire);
             |}""".stripMargin
        )
        val extParents = List(q"$iBaboonBinCodec<$iBaboonGenerated>")

        val mm = if (addExtensions) {
          baseMethods ++ extensions
        } else {
          baseMethods
        }

        val baseParents = List(iName)
        val pp = if (addExtensions) {
          baseParents ++ extParents
        } else {
          baseParents
        }

        (pp, mm)
    }

    val cName = codecName(srcRef)

    q"""public class $cName : ${parents.join(", ")}
       |{
       |    ${methods.join("\n").shift(4).trim}
       |
       |    ${tools.makeMeta(defn, version).join("\n").shift(4).trim}
       |
       |    private static $csLazy<$iName> instance = new $csLazy<$iName>(() => new $cName());
       |
       |    public static $iName Instance { get { return instance.Value; } set { instance = new $csLazy<$iName>(() => value); } }
       |}
     """.stripMargin
  }

  private def genForeignBodies(name: CSValue.CSType) = {
    (
      q"""throw new ArgumentException($$"${name.name} is a foreign type");""",
      q"""throw new ArgumentException($$"${name.name} is a foreign type");"""
    )
  }

  private def genAdtBodies(name: CSValue.CSType,
                           domain: Domain,
                           a: Typedef.Adt,
                           evo: BaboonEvolution) = {
    val branches = a.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val branchNs = q"${trans.adtNsName(a.id)}"
        val branchName = m.name.name
        val fqBranch = q"$branchNs.$branchName"
        val cName = codecName(trans.toCsTypeRefNoDeref(m, domain, evo, options))
        val castedName = branchName.toLowerCase

        (q"""if (value is $fqBranch $castedName)
             |{
             |   writer.Write((byte)${idx.toString});
             |   $cName.Instance.Encode(writer, $castedName);
             |   return;
             |}""".stripMargin, q"""if (asByte == ${idx.toString})
             |{
             |   return $cName.Instance.Decode(wire);
             |}""".stripMargin)
    }

    (
      q"""${branches.map(_._1).join("\n")}
         |
         |throw new ${csArgumentException}($$"Cannot encode {value} to ${name.name}: no matching value");""".stripMargin,
      q"""byte asByte = wire.ReadByte();
         |
         |${branches.map(_._2).join("\n")}
         |
         |throw new ${csArgumentException}($$"Cannot decode {wire} to ${name.name}: no matching value");""".stripMargin,
    )
  }

  private def genEnumBodies(name: CSValue.CSType, e: Typedef.Enum) = {
    val branches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        (q"""if (value == $name.${m.name})
             |{
             |   writer.Write((byte)${idx.toString});
             |   return;
             |}""".stripMargin, q"""if (asByte == ${idx.toString})
             |{
             |   return $name.${m.name};
             |}""".stripMargin)
    }

    (
      q"""${branches.map(_._1).join("\n")}
         |
         |throw new $csArgumentException($$"Cannot encode {value} to ${name.name}: no matching value");""".stripMargin,
      q"""byte asByte = wire.ReadByte();
         |
         |${branches.map(_._2).join("\n")}
         |
         |throw new $csArgumentException($$"Cannot decode {wire} to ${name.name}: no matching value");""".stripMargin,
    )
  }

  private def genDtoBodies(name: CSValue.CSType,
                           domain: Domain,
                           d: Typedef.Dto,
                           evo: BaboonEvolution) = {
    val branches = d.fields.map { f =>
      val fieldRef = q"value.${f.name.name.capitalize}"
      val enc = mkEncoder(f.tpe, domain, fieldRef, evo)
      val dec = mkDecoder(f.tpe, domain, evo)
      (enc, dec)
    }

    val fenc =
      q"""${branches
           .map(_._1)
           .join(";\n")};""".stripMargin

    val fdec =
      q"""return new $name(
         |${branches.map(_._2).join(",\n").shift(4)}
         |);
         |""".stripMargin
    //val fdec = q"throw new $csNotImplementedException();"
    (fenc, fdec)
  }

  private def mkDecoder(tpe: TypeRef,
                        domain: Domain,
                        evo: BaboonEvolution): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit =>
                q"wire.ReadBoolean()"
              case TypeId.Builtins.i08 =>
                q"wire.ReadSByte()"
              case TypeId.Builtins.i16 =>
                q"wire.ReadInt16()"
              case TypeId.Builtins.i32 =>
                q"wire.ReadInt32()"
              case TypeId.Builtins.i64 =>
                q"wire.ReadInt64()"
              case TypeId.Builtins.u08 =>
                q"wire.ReadByte()"
              case TypeId.Builtins.u16 =>
                q"wire.ReadUInt16()"
              case TypeId.Builtins.u32 =>
                q"wire.ReadUInt32()"
              case TypeId.Builtins.u64 =>
                q"wire.ReadUInt64()"
              case TypeId.Builtins.f32 =>
                q"wire.ReadSingle()"
              case TypeId.Builtins.f64 =>
                q"wire.ReadDouble()"
              case TypeId.Builtins.f128 =>
                q"wire.ReadDecimal()"
              case TypeId.Builtins.str =>
                q"wire.ReadString()"
              case TypeId.Builtins.uid =>
                q"$csGuid.Parse(wire.ReadString())"
              case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
                q"$baboonTimeFormats.FromString(wire.ReadString())"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(
              trans.toCsTypeRefNoDeref(u, domain, evo, options)
            )
            q"""${targetTpe}.Instance.Decode(wire)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt if trans.isCSValueType(c.args.head) =>
            q"""$BaboonTools.ReadNullableValue(wire.ReadByte() == 0, () => ${mkDecoder(
                 c.args.head,
                 domain,
                 evo
               )})""".stripMargin

          case TypeId.Builtins.opt =>
            q"""(wire.ReadByte() == 0 ? null : ${mkDecoder(
                 c.args.head,
                 domain,
                 evo
               )})""".stripMargin

          case TypeId.Builtins.map =>
            val keyRef = c.args.head
            val valueRef = c.args.last
            val keyDecoder = mkDecoder(keyRef, domain, evo)
            val valueDecoder = mkDecoder(valueRef, domain, evo)
            q"""$csEnumerable.Range(0, wire.ReadInt32()).Select(idx => new $csKeyValuePair<${trans
              .asCsRef(keyRef, domain, evo, options)}, ${trans.asCsRef(
              valueRef,
              domain,
              evo,
              options
            )}>($keyDecoder, $valueDecoder)).ToImmutableDictionary()"""

          case TypeId.Builtins.lst =>
            q"""$csEnumerable.Range(0, wire.ReadInt32()).Select(idx => ${mkDecoder(
              c.args.head,
              domain,
              evo
            )}).ToImmutableList()"""

          case TypeId.Builtins.set =>
            q"""$csEnumerable.Range(0, wire.ReadInt32()).Select(idx => ${mkDecoder(
              c.args.head,
              domain,
              evo
            )}).ToImmutableHashSet()"""

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }

  }

  private def mkEncoder(tpe: TypeRef,
                        domain: Domain,
                        ref: TextTree[CSValue],
                        evo: BaboonEvolution,
  ): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit =>
                q"writer.Write($ref)"
              case TypeId.Builtins.i08 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.i16 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.i32 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.i64 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.u08 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.u16 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.u32 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.u64 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.f32 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.f64 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.f128 =>
                q"writer.Write($ref)"
              case TypeId.Builtins.str =>
                q"writer.Write($ref)"
              case TypeId.Builtins.uid =>
                q"writer.Write($ref.ToString())"
              case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
                q"writer.Write($baboonTimeFormats.ToString($ref))"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(
              trans.toCsTypeRefNoDeref(u, domain, evo, options)
            )
            q"""${targetTpe}.Instance.Encode(writer, $ref)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""if ($ref == null)
               |{
               |    writer.Write((byte)0);
               |} else
               |{
               |   writer.Write((byte)1);
               |   ${mkEncoder(
                 c.args.head,
                 domain,
                 trans.deNull(c.args.head, ref),
                 evo
               ).shift(4).trim};
               |}""".stripMargin
          case TypeId.Builtins.map =>
            q"""writer.Write($ref.Count());
               |foreach (var kv in $ref)
               |{
               |    ${mkEncoder(c.args.head, domain, q"kv.Key", evo)
                 .shift(4)
                 .trim};
               |    ${mkEncoder(c.args.last, domain, q"kv.Value", evo)
                 .shift(4)
                 .trim};
               |}""".stripMargin
          case TypeId.Builtins.lst =>
            q"""writer.Write($ref.Count());
               |foreach (var i in $ref)
               |{
               |    ${mkEncoder(c.args.head, domain, q"i", evo).shift(4).trim};
               |}""".stripMargin
          case TypeId.Builtins.set =>
            q"""writer.Write($ref.Count());
               |foreach (var i in $ref)
               |{
               |    ${mkEncoder(c.args.head, domain, q"i", evo).shift(4).trim};
               |}""".stripMargin
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  def codecName(name: CSValue.CSType): CSValue.CSType = {
    CSValue.CSType(name.pkg, s"${name.name}_UEBACodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User,
                         name: CSValue.CSType): CSCodecTranslator.CodecMeta = {
    val member =
      q"""public IBaboonBinCodec<$name> Codec_UEBA()
         |{
         |    return ${codecName(name)}.Instance;
         |}""".stripMargin
    CodecMeta(member)
  }

  def metaField(): TextTree[CSValue] = q"IBaboonCodecData Ueba";
}
