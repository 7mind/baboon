package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSUEBACodecGenerator(trans: CSTypeTranslator, tools: CSDefnTools)
    extends CSCodecTranslator {
  override def translate(defn: DomainMember.User,
                         name: CSValue.CSType,
                         version: Version): TextTree[CSValue] = {
    val (enc, dec) = defn.defn match {
      case d: Typedef.Dto =>
        val branches = d.fields.map { f =>
          val fieldRef = q"value.${f.name.name.capitalize}"
          val enc = mkEncoder(f.tpe, version, fieldRef)
          val dec = mkDecoder(f.tpe, version)
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
               """.stripMargin
        //val fdec = q"throw new $csNotImplementedException();"
        (fenc, fdec)

      case e: Typedef.Enum =>
        val branches = e.members.zipWithIndex.toList.map {
          case (m, idx) =>
            (q"""if (value == ${name}.${m.name})
             |{
             |   writer.Write((byte)${idx.toString});
             |   return;
             |}""".stripMargin, q"""if (asByte == ${idx.toString})
                 |{
                 |   return ${name}.${m.name};
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
      case a: Typedef.Adt =>
        val branches = a.members.zipWithIndex.toList.map {
          case (m, idx) =>
            val branchNs = q"${trans.adtNsName(a.id)}"
            val branchName = m.name.name
            val fqBranch = q"$branchNs.$branchName"
            val cName = codecName(trans.toCsVal(m, version))

            (q"""if (value is $fqBranch)
                 |{
                 |   writer.Write((byte)${idx.toString});
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
    val baseMethods = List(
      q"""public void Encode($binaryWriter writer, $name value)
         |{
         |    ${enc.shift(4).trim}
         |}
         |public $name Decode($binaryReader wire) {
         |    ${dec.shift(4).trim}
         |}""".stripMargin
    )

    val (parents, methods) = defn.defn match {
      case _: Typedef.Enum =>
        (List(q"$iBaboonBinCodec<$name>"), baseMethods)
      case _ =>
        (
          List(
            q"$iBaboonBinCodec<$name>",
            q"$iBaboonBinCodec<$iBaboonGenerated>"
          ),
          baseMethods ++ List(
            q"""public void Encode($binaryWriter writer, $iBaboonGenerated value)
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
        )
    }

    val cName = codecName(name)
    q"""public class $cName : ${parents.join(", ")}
       |{
       |    ${methods.join("\n").shift(4).trim}
       |
       |    ${tools.makeMeta(defn, version).join("\n").shift(4).trim}
       |
       |    private static $csLazy<$cName> instance = new $csLazy<$cName>(() => new $cName());
       |
       |    public static $cName Instance { get { return instance.Value; } }
       |}
     """.stripMargin
  }

  private def isCSValueType(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit =>
                true
              case TypeId.Builtins.i08 =>
                true
              case TypeId.Builtins.i16 =>
                true
              case TypeId.Builtins.i32 =>
                true
              case TypeId.Builtins.i64 =>
                true
              case TypeId.Builtins.u08 =>
                true
              case TypeId.Builtins.u16 =>
                true
              case TypeId.Builtins.u32 =>
                true
              case TypeId.Builtins.u64 =>
                true
              case TypeId.Builtins.f32 =>
                true
              case TypeId.Builtins.f64 =>
                true
              case TypeId.Builtins.f128 =>
                true
              case _ =>
                false
            }
          case _ => false
        }
      case _ => false
    }
  }
  private def mkDecoder(tpe: TypeRef, version: Version): TextTree[CSValue] = {
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
              case TypeId.Builtins.tsu =>
                q"$csDateTime.Parse(wire.ReadString())"
              case TypeId.Builtins.tso =>
                q"$csDateTime.Parse(wire.ReadString())"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toCsVal(u, version))
            q"""${targetTpe}.Instance.Decode(wire)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            if (isCSValueType(c.args.head)) {
              q"""$BaboonTools.ReadNullableValue(wire.ReadByte() == 0, () => ${mkDecoder(
                   c.args.head,
                   version
                 )})""".stripMargin
            } else {
              q"""(wire.ReadByte() == 0 ? null : ${mkDecoder(
                   c.args.head,
                   version
                 )})""".stripMargin
            }

          case TypeId.Builtins.map =>
            q"""$csEnumerable.Range(0, wire.ReadInt32()).Select(idx => $csKeyValuePair.Create(${mkDecoder(
              c.args.head,
              version
            )}, ${mkDecoder(c.args.last, version)})).ToImmutableDictionary()"""
          case TypeId.Builtins.lst =>
            q"""$csEnumerable.Range(0, wire.ReadInt32()).Select(idx => ${mkDecoder(
              c.args.head,
              version
            )}).ToImmutableList()"""
          case TypeId.Builtins.set =>
            q"""$csEnumerable.Range(0, wire.ReadInt32()).Select(idx => ${mkDecoder(
              c.args.head,
              version
            )}).ToImmutableHashSet()"""
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }

  }

  private def mkEncoder(tpe: TypeRef,
                        version: Version,
                        ref: TextTree[CSValue]): TextTree[CSValue] = {
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
              case TypeId.Builtins.tsu =>
                q"writer.Write($ref.ToString($csInvariantCulture.InvariantCulture))"
              case TypeId.Builtins.tso =>
                q"writer.Write($ref.ToString($csInvariantCulture.InvariantCulture))"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toCsVal(u, version))
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
               |   ${mkEncoder(c.args.head, version, deNull(c.args.head, ref))
                 .shift(4)
                 .trim};
               |}""".stripMargin
          case TypeId.Builtins.map =>
            q"""writer.Write($ref.Count());
               |foreach (var kv in $ref)
               |{
               |    ${mkEncoder(c.args.head, version, q"kv.Key").shift(4).trim};
               |    ${mkEncoder(c.args.last, version, q"kv.Value")
                 .shift(4)
                 .trim};
               |}""".stripMargin
          case TypeId.Builtins.lst =>
            q"""writer.Write($ref.Count());
               |foreach (var i in $ref)
               |{
               |    ${mkEncoder(c.args.head, version, q"i").shift(4).trim};
               |}""".stripMargin
          case TypeId.Builtins.set =>
            q"""writer.Write($ref.Count());
               |foreach (var i in $ref)
               |{
               |    ${mkEncoder(c.args.head, version, q"i").shift(4).trim};
               |}""".stripMargin
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  def codecName(name: CSValue.CSType): CSValue.CSType = {
    CSValue.CSType(name.pkg, s"${name.name}_UEBACodec", name.fq)
  }

  private def deNull(tpe: TypeRef,
                     ref: TextTree[CSValue]): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.str =>
                ref
              case _ =>
                q"$ref.Value"
            }
          case _ =>
            q"$ref!"
        }
      case _ =>
        q"$ref!"
    }
  }

  override def codecMeta(defn: DomainMember.User,
                         name: CSValue.CSType): CSCodecTranslator.CodecMeta = {
    val member = q"""public IBaboonBinCodec<$name> Codec_UEBA()
                    |{
                    |    return ${codecName(name)}.Instance;
                    |}""".stripMargin
    CodecMeta(member)
  }

  def metaField(): TextTree[CSValue] = q"IBaboonCodecAbstract ueba";
}
