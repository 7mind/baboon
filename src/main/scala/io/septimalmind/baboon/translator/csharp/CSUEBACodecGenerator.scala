package io.septimalmind.baboon.translator.csharp


import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*
import io.septimalmind.baboon.CompilerOptions

class CSUEBACodecGenerator(trans: CSTypeTranslator,
                           csDomTrees: CSDomainTreeTools,
                           options: CompilerOptions,
                           domain: Domain,
                           evo: BaboonEvolution)
    extends CSCodecTranslator {
  override def translate(defn: DomainMember.User,
                         csRef: CSValue.CSType,
                         srcRef: CSValue.CSType): Option[TextTree[CSValue]] = {
    (defn.defn match {
      case d: Typedef.Dto =>
        Some(genDtoBodies(csRef, d))
      case e: Typedef.Enum =>
        Some(genEnumBodies(csRef, e))
      case a: Typedef.Adt =>
        Some(genAdtBodies(csRef, a))
      case _: Typedef.Foreign =>
        Some(genForeignBodies(csRef))
      case _: Typedef.Contract =>
        None
    }).map {
      case (enc, dec) =>
        // plumbing reference leaks
        val insulatedEnc =
          q"""
             |#pragma warning disable CS0162
             |if (this == LazyInstance.Value)
             |{
             |    ${enc.shift(4).trim}
             |    return;
             |}
             |#pragma warning disable CS0162
             |
             |LazyInstance.Value.Encode(ctx, writer, value);""".stripMargin
        val insulatedDec =
          q"""if (this == LazyInstance.Value)
             |{
             |    ${dec.shift(4).trim}
             |}
             |
             |return LazyInstance.Value.Decode(ctx, wire);""".stripMargin

        val branchDecoder = defn.defn match {
          case d: Typedef.Dto =>
            genBranchDecoder(csRef, d)
          case _ =>
            None
        }

        genCodec(
          defn,
          csRef,
          srcRef,
          insulatedEnc,
          insulatedDec,
          !defn.defn.isInstanceOf[Typedef.Foreign],
          branchDecoder,
        )
    }
  }

  private def genCodec(defn: DomainMember.User,
                       name: CSValue.CSType,
                       srcRef: CSValue.CSType,
                       enc: TextTree[CSValue],
                       dec: TextTree[CSValue],
                       addExtensions: Boolean,
                       branchDecoder: Option[TextTree[CSValue]],
  ): TextTree[CSValue] = {
    val iName = q"$iBaboonBinCodec<$name>"

    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens =
          d.fields.filter(f => domain.refMeta(f.tpe).len.isVariable)

        val comment = varlens
          .map(f => q"// ${f.toString}")
          .join("\n")

        q"""$comment
           |return ${varlens.size.toString};""".stripMargin
      case d: Typedef.Contract =>
        throw new IllegalArgumentException(
          s"BUG: contract should not be rendered: $d"
        )
      case _: Typedef.Enum =>
        q"""return 0;"""
      case _: Typedef.Adt =>
        q"""return 0;"""
      case _: Typedef.Foreign =>
        q"""throw new ArgumentException($$"${name.name} is a foreign type");"""

    }

    val indexMethods = List(
      q"""public UInt16 IndexElementsCount(BaboonCodecContext ctx)
         |{
         |    ${indexBody.shift(4).trim}
         |}""".stripMargin
    )

    val baseMethods = List(
      q"""public virtual void Encode($baboonCodecContext ctx, $binaryWriter writer, $name value)
         |{
         |    ${enc.shift(4).trim}
         |}
         |public virtual $name Decode($baboonCodecContext ctx, $binaryReader wire) {
         |    ${dec.shift(4).trim}
         |}""".stripMargin
    ) ++ branchDecoder.map { body =>
      q"""internal $name DecodeBranch($baboonCodecContext ctx, $binaryReader wire) {
         |    ${body.shift(4).trim}
         |}""".stripMargin
    }.toList ++ indexMethods

    val (parents, methods) = defn.defn match {
      case _: Typedef.Enum =>
        (List(q"$iBaboonBinCodec<$name>"), baseMethods)
      case _ =>
        val extensions = List(
          q"""public virtual void Encode($baboonCodecContext ctx,$binaryWriter writer, $iBaboonGenerated value)
             |{
             |    if (value is not $name dvalue)
             |        throw new Exception("Expected to have ${name.name} type");
             |    Encode(ctx, writer, dvalue);
             |}""".stripMargin,
          q"""$iBaboonGenerated $iBaboonStreamCodec<$iBaboonGenerated, $binaryWriter, $binaryReader>.Decode($baboonCodecContext ctx, $binaryReader wire)
             |{
             |    return Decode(ctx, wire);
             |}""".stripMargin
        )

        val adtParents = defn.id.owner match {
          case Owner.Adt(_) => List(q"$iBaboonAdtMemberMeta")
          case _            => List.empty
        }
        val extParents = List(q"$iBaboonBinCodec<$iBaboonGenerated>") ++ adtParents

        val mm = if (addExtensions) {
          baseMethods ++ extensions
        } else {
          baseMethods
        }

        val baseParents = List(q"$iBaboonBinCodecIndexed", iName)
        val pp = if (addExtensions) {
          baseParents ++ extParents
        } else {
          baseParents
        }

        (pp, mm)
    }

    val cName = codecName(srcRef)

    q"""public class ${cName.asName} : ${parents.join(", ")}
       |{
       |    ${methods.join("\n\n").shift(4).trim}
       |
       |    ${csDomTrees
         .makeMeta(defn, isCodec = true)
         .join("\n")
         .shift(4)
         .trim}
       |
       |    internal static $csLazy<$iName> LazyInstance = new $csLazy<$iName>(() => new $cName());
       |
       |    public static $iName Instance { get { return LazyInstance.Value; } set { LazyInstance = new $csLazy<$iName>(() => value); } }
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
                           a: Typedef.Adt,
  ) = {
    val branches = a.dataMembers(domain).zipWithIndex.toList.map {
      case (m, idx) =>
        val branchNs = q"${trans.adtNsName(a.id)}"
        val branchName = m.name.name
        val fqBranch = q"$branchNs.$branchName"

        val adtRef = trans.toCsTypeRefNoDeref(m, domain, evo)
        val cName = codecName(adtRef)

        val castedName = branchName.toLowerCase

        val encBody = if (options.csOptions.csWrappedAdtBranchCodecs) {
          q"""$cName.Instance.Encode(ctx, writer, $castedName);"""
        } else {
          q"""writer.Write((byte)${idx.toString});
             |$cName.Instance.Encode(ctx, writer, $castedName);
           """.stripMargin
        }

        val decBody = if (options.csOptions.csWrappedAdtBranchCodecs) {
          q"""return (($cName)$cName.Instance).DecodeBranch(ctx, wire);"""
        } else {
          q"""return $cName.Instance.Decode(ctx, wire);"""
        }

        (q"""if (value is $fqBranch $castedName)
            |{
            |    ${encBody.shift(4).trim}
            |    return;
            |}""".stripMargin, q"""if (asByte == ${idx.toString})
                                  |{
                                  |    ${decBody.shift(4).trim}
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

  private def genBranchDecoder(
    name: CSValue.CSType,
    d: Typedef.Dto,
  ): Option[TextTree[CSValue]] = {

    d.id.owner match {
      case Owner.Adt(_) if options.csOptions.csWrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(name, fields.map({ case (a, b, _) => (a, b) })))
      case _ =>
        None
    }
  }

  private def genDtoBodies(name: CSValue.CSType,
                           d: Typedef.Dto) = {
    val fields = fieldsOf(d)

    val fenc =
      q"""byte header = 0b0000000;
         |
         |if (ctx.UseIndices)
         |{
         |    header |= 0b0000001;
         |    writer.Write(header);
         |    using ($memoryStream writeMemoryStream = new $memoryStream())
         |    {
         |        using ($binaryWriter fakeWriter = new $binaryWriter(writeMemoryStream))
         |        {
         |            ${fields.map(_._3).join("\n").shift(12).trim}
         |        }
         |        writeMemoryStream.Flush();
         |        writer.Write(writeMemoryStream.ToArray());
         |    }
         |} else {
         |    writer.Write(header);
         |    ${fields.map(_._1).join(";\n").shift(4).trim};
         |}
         |""".stripMargin

    val fdec =
      dtoDec(name, fields.map { case (a, b, _) => (a, b) })

    def adtBranchIndex(id: TypeId.User) = {
      domain.defs.meta
        .nodes(id)
        .asInstanceOf[DomainMember.User]
        .defn
        .asInstanceOf[Typedef.Adt]
        .dataMembers(domain)
        .zipWithIndex
        .find(_._1 == d.id)
        .get
        ._2
    }

    val enc = d.id.owner match {
      case Owner.Adt(id) if options.csOptions.csWrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)

        q"""writer.Write((byte)${idx.toString});
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = d.id.owner match {
      case Owner.Adt(id) if options.csOptions.csWrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)

        q"""byte marker = wire.ReadByte();
           |$debug.Assert(marker == ${idx.toString});
           |return DecodeBranch(ctx, wire);""".stripMargin
      case _ => fdec
    }

    (enc, dec)
  }

  private def dtoDec(name: CSValue.CSType,
                     fields: List[(TextTree[CSValue], TextTree[CSValue])]) = {
    q"""var index = ((IBaboonBinCodecIndexed)this).ReadIndex(ctx, wire);
       |
       |if (ctx.UseIndices)
       |{
       |    $debug.Assert(index.Count == IndexElementsCount(ctx));
       |}
       |
       |return new $name(
       |${fields.map(_._2).join(",\n").shift(4)}
       |);
       |""".stripMargin
  }

  private def fieldsOf(d: Typedef.Dto) = {
    d.fields.map { f =>
      val fieldRef = q"value.${f.name.name.capitalize}"
      val enc = mkEncoder(f.tpe, fieldRef,  q"writer")
      val fakeEnc = mkEncoder(f.tpe, fieldRef,  q"fakeWriter")
      val dec = mkDecoder(f.tpe)

      val w = domain.refMeta(f.tpe).len match {
        case BinReprLen.Fixed(bytes) =>
          q"""{
             |    // ${f.toString}
             |    var before = (uint)fakeWriter.BaseStream.Position;
             |    ${fakeEnc.shift(4).trim};
             |    var after = (uint)fakeWriter.BaseStream.Position;
             |    var length = after - before;
             |    $debug.Assert(length == ${bytes.toString});
             |}""".stripMargin

        case v: BinReprLen.Variable =>
          val sanityChecks = v match {
            case BinReprLen.Unknown() => q"$debug.Assert(length >= 0, $$\"Got length={length}\")";
            case BinReprLen.Alternatives(variants) =>
              q"$debug.Assert(new $csSet<uint>() { ${variants.mkString(", ")} }.Contains(length), $$\"Got length={length}\")"
            case BinReprLen.Range(min, max) =>
              (Seq(q"$debug.Assert(length >= ${min.toString}, $$\"Got length={length}\")") ++ max.toSeq
                .map(m => q"$debug.Assert(length <= ${m.toString}, $$\"Got length={length}\")"))
                .join(";\n")
          }

          q"""{
             |    // ${f.toString}
             |    var before = (uint)fakeWriter.BaseStream.Position;
             |    writer.Write(before);
             |    ${fakeEnc.shift(4).trim};
             |    var after = (uint)fakeWriter.BaseStream.Position;
             |    var length = after - before;
             |    writer.Write(length);
             |    ${sanityChecks.shift(4).trim};
             |}""".stripMargin
      }

      (enc, dec, w)
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[CSValue] = {
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
                q"new $csGuid(wire.ReadBytes(16))"
              case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
                q"$baboonTimeFormats.FromString(wire.ReadString())"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toCsTypeRefNoDeref(u, domain, evo))
            q"""$targetTpe.Instance.Decode(ctx, wire)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt
              if trans.isCSValueType(c.args.head, domain) =>
            q"""$BaboonTools.ReadNullableValueType(wire.ReadByte() == 0, () => ${mkDecoder(
                 c.args.head
               )})""".stripMargin

          case TypeId.Builtins.opt =>
            q"""(wire.ReadByte() == 0 ? null : ${mkDecoder(
                 c.args.head
               )})""".stripMargin

          case TypeId.Builtins.map =>
            val keyRef = c.args.head
            val valueRef = c.args.last
            val keyDecoder = mkDecoder(keyRef)
            val valueDecoder = mkDecoder(valueRef)
            q"""$csEnumerable.Range(0, wire.ReadInt32()).Select(idx => new $csKeyValuePair<${trans
              .asCsRef(keyRef, domain, evo)}, ${trans.asCsRef(
              valueRef,
              domain,
              evo,
            )}>($keyDecoder, $valueDecoder)).ToImmutableDictionary()"""

          case TypeId.Builtins.lst =>
            q"""$csEnumerable.Range(0, wire.ReadInt32()).Select(idx => ${mkDecoder(
              c.args.head
            )}).ToImmutableList()"""

          case TypeId.Builtins.set =>
            q"""$csEnumerable.Range(0, wire.ReadInt32()).Select(idx => ${mkDecoder(
              c.args.head
            )}).ToImmutableHashSet()"""

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }

  }

  private def mkEncoder(tpe: TypeRef,
                        ref: TextTree[CSValue],
                        wref: TextTree[CSValue],
  ): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.i08 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.i16 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.i32 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.i64 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.u08 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.u16 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.u32 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.u64 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.f32 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.f64 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.f128 =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.str =>
                q"$wref.Write($ref)"
              case TypeId.Builtins.uid =>
                q"$wref.Write($ref.ToByteArray())"
              case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
                q"$wref.Write($baboonTimeFormats.ToString($ref))"
              case o =>
                throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val targetTpe = codecName(trans.toCsTypeRefNoDeref(u, domain, evo))
            q"""$targetTpe.Instance.Encode(ctx, $wref, $ref)"""
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""if ($ref == null)
               |{
               |    $wref.Write((byte)0);
               |} else
               |{
               |   $wref.Write((byte)1);
               |   ${mkEncoder(
                 c.args.head,
                 trans.deNull(c.args.head, domain, ref),
                 wref
               ).shift(4).trim};
               |}""".stripMargin
          case TypeId.Builtins.map =>
            q"""$wref.Write($ref.Count);
               |foreach (var kv in $ref)
               |{
               |    ${mkEncoder(c.args.head, q"kv.Key", wref)
                 .shift(4)
                 .trim};
               |    ${mkEncoder(c.args.last, q"kv.Value", wref)
                 .shift(4)
                 .trim};
               |}""".stripMargin
          case TypeId.Builtins.lst =>
            q"""$wref.Write($ref.Count);
               |foreach (var i in $ref)
               |{
               |    ${mkEncoder(c.args.head, q"i", wref)
                 .shift(4)
                 .trim};
               |}""".stripMargin
          case TypeId.Builtins.set =>
            q"""$wref.Write($ref.Count);
               |foreach (var i in $ref)
               |{
               |    ${mkEncoder(c.args.head, q"i", wref)
                 .shift(4)
                 .trim};
               |}""".stripMargin
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  def codecType(): CSValue.CSType = iBaboonCodecData

  def codecName(name: CSValue.CSType): CSValue.CSType = {
    CSValue.CSType(name.pkg, s"${name.name}_UEBACodec", name.fq)
  }

  override def codecMeta(defn: DomainMember.User,
                         name: CSValue.CSType): CSCodecTranslator.CodecMeta = {
    val fix = csDomTrees.metaMethodFlags(defn, isCodec = false)

    val member =
      q"""public$fix$iBaboonBinCodec<$name> Codec_UEBA()
         |{
         |    return ${codecName(name)}.Instance;
         |}""".stripMargin
    CodecMeta(member)
  }

  def codecInterfaceProperty(): TextTree[CSValue] =
    q"public $iBaboonCodecData Ueba { get; }";

  def codecImplProperty(): TextTree[CSValue] =
    q"public $iBaboonCodecData Ueba => LazyUeba.Value;";

  def codecGenericImplField(): TextTree[CSValue] =
    q"Lazy<$iBaboonBinCodec<T>> LazyUeba";

  def codecImplField(): TextTree[CSValue] = q"Lazy<$iBaboonCodecData> LazyUeba";
}
