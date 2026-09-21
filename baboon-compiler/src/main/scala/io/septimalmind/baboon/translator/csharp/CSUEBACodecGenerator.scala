package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.csharp.CSCodecTranslator.{CodecArguments, CodecMeta}
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSTypeOrigin
import io.septimalmind.baboon.typer.{BaboonEnquiries, EnumWireStyle}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.translator.{AnyFieldPlan, UebaLayoutPlan, UebaLengthCheckRenderer}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*
import izumi.fundamentals.platform.strings.TextTree.style.c.*

class CSUEBACodecGenerator(
  domainTypes: CSDomainTypes,
  csDomTrees: CSDomainTreeTools,
  target: CSTarget,
  domain: Domain,
  evo: BaboonEvolution,
  csTypeInfo: CSTypeInfo,
) extends CSCodecTranslator {
  private val layout = new UebaLayoutPlan(domain)

  override def translate(
    defn: DomainMember.User,
    csRef: CSValue.CSType,
    srcRef: CSValue.CSType,
  ): Option[TextTree[CSValue]] = {
    val isLatestVersion = domain.version == evo.latest

    if (isActive(defn.id)) {
      (defn.defn match {
        case d: Typedef.Dto  => Some(genDtoBodies(csRef, d))
        case e: Typedef.Enum => Some(genEnumBodies(csRef, e))
        case a: Typedef.Adt  => Some(genAdtBodies(csRef, a))
        case f: Typedef.Foreign =>
          f.bindings.get(BaboonLang.Cs) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
            case _                                                                  => Some(genForeignBodies(csRef))
          }
        case _: Typedef.Contract => None
        case _: Typedef.Service  => None
      }).map {
        case (enc, dec) =>
          if (!isLatestVersion && !target.language.enableDeprecatedEncoders) {
            (q"""throw new Exception("Type ${defn.id.toString}@${domain.version.toString} is deprecated, encoder was not generated");""", dec)
          } else {
            (enc, dec)
          }
      }.map {
        case (enc, dec) =>
          // plumbing reference leaks
          val insulatedEnc =
            q"""if (this != LazyInstance.Value)
               |{
               |    LazyInstance.Value.Encode(ctx, writer, value);
               |    return;
               |}
               |
               |$enc
               |""".stripMargin.trim
          val insulatedDec =
            q"""if (this != LazyInstance.Value)
               |{
               |    return LazyInstance.Value.Decode(ctx, wire);
               |}
               |
               |$dec
               |""".stripMargin.trim

          val branchDecoder = defn.defn match {
            case d: Typedef.Dto => genBranchDecoder(csRef, d)
            case _              => None
          }

          genCodec(
            defn,
            csRef,
            srcRef,
            insulatedEnc,
            insulatedDec,
            branchDecoder,
          )
      }
    } else {
      None
    }
  }

  private def genCodec(
    defn: DomainMember.User,
    name: CSValue.CSType,
    srcRef: CSValue.CSType,
    enc: TextTree[CSValue],
    dec: TextTree[CSValue],
    branchDecoder: Option[TextTree[CSValue]],
  ): TextTree[CSValue] = {
    val isEncoderEnabled = target.language.enableDeprecatedEncoders || domain.version == evo.latest
    val isAdtMember = defn.id.owner match {
      case Owner.Adt(_) => true
      case _            => false
    }

    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = layout.indexedFields(d)
        val comment = varlens.map(f => q"// ${f.toString}").join("\n")
        q"""$comment
           |return ${varlens.size.toString};""".stripMargin

      case _: Typedef.Enum    => q"""return 0;"""
      case _: Typedef.Adt     => q"""return 0;"""
      case _: Typedef.Foreign => q"""throw new ArgumentException("${name.name} is a foreign type");"""

      case d: Typedef.Contract =>
        throw new IllegalArgumentException(s"BUG: contract codec should not be rendered: $d")
      case d: Typedef.Service =>
        throw new IllegalArgumentException(s"BUG: service codec should not be rendered: $d")
    }

    val indexMethods = List(
      q"""public override UInt16 IndexElementsCount(BaboonCodecContext ctx)
         |{
         |    ${indexBody.shift(4).trim}
         |}""".stripMargin
    )

    val encoderMethods = if (isEncoderEnabled) {
      List(
        q"""public override void Encode($baboonCodecContext ctx, $binaryWriter writer, $name value)
           |{
           |    ${enc.shift(4).trim}
           |}
           |""".stripMargin
      )
    } else {
      List.empty
    }

    val decoderMethods = List(
      q"""public override $name Decode($baboonCodecContext ctx, $binaryReader wire) {
         |    ${dec.shift(4).trim}
         |}""".stripMargin
    ) ++ branchDecoder.map {
      body =>
        q"""internal $name DecodeBranch($baboonCodecContext ctx, $binaryReader wire) {
           |    ${body.shift(4).trim}
           |}""".stripMargin
    }.toList

    val methods = indexMethods ++ encoderMethods ++ decoderMethods

    val cName = codecName(srcRef, CSTypeOrigin(defn.id, domain))
    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecBase<$name, $cName>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecBase<$name, $cName>"
        case _ if isAdtMember                               => q"$baboonBinCodecBaseGeneratedAdt<$name, $cName>"
        case _                                              => q"$baboonBinCodecBaseGenerated<$name, $cName>"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecNoEncoder<$name, $cName>"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecNoEncoder<$name, $cName>"
        case _ if isAdtMember                               => q"$baboonBinCodecNoEncoderGeneratedAdt<$name, $cName>"
        case _                                              => q"$baboonBinCodecNoEncoderGenerated<$name, $cName>"
      }
    }

    q"""public class ${cName.asName} : $cParent
       |{
       |    ${methods.join("\n\n").shift(4).trim}
       |
       |    ${csDomTrees.makeCodecMeta(defn).join("\n").shift(4).trim}
       |}
     """.stripMargin
  }

  private def genForeignBodies(name: CSValue.CSType) = {
    (
      q"""throw new ArgumentException("${name.name} is a foreign type");""",
      q"""throw new ArgumentException("${name.name} is a foreign type");""",
    )
  }

  private def genAdtBodies(name: CSValue.CSType, a: Typedef.Adt) = {
    val branches = a.dataMembers(domain).zipWithIndex.toList.map {
      case (m, idx) =>
        val branchNs   = q"${escapeCsKeyword(csTypeInfo.adtNsName(a.id))}"
        val branchName = m.name.name
        val fqBranch   = q"$branchNs.${escapeCsKeyword(branchName)}"

        val cName = codecName(m)

        // Suffix with the branch index so the pattern-capture local can never equal the
        // enclosing `value` encoder parameter (D44) nor collide with a case-only-differing
        // sibling. escapeCsKeyword is retained (D1 reserved-word fix); the capture is internal
        // only (never on the wire), so the rename is wire-safe.
        val castedName = s"${escapeCsKeyword(branchName.toLowerCase)}_$idx"

        val encBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""$cName.Instance.Encode(ctx, writer, $castedName);"""
        } else {
          q"""writer.Write((byte)${idx.toString});
             |$cName.Instance.Encode(ctx, writer, $castedName);
           """.stripMargin
        }

        val decBody = if (target.language.wrappedAdtBranchCodecs) {
          q"""return (($cName)$cName.Instance).DecodeBranch(ctx, wire);"""
        } else {
          q"""return $cName.Instance.Decode(ctx, wire);"""
        }

        (
          q"""if (value is $fqBranch $castedName)
             |{
             |    ${encBody.shift(4).trim}
             |    return;
             |}""".stripMargin,
          q"""if (asByte == ${idx.toString})
             |{
             |    ${decBody.shift(4).trim}
             |}""".stripMargin,
        )
    }

    (
      q"""${branches.map(_._1).join("\n")}
         |
         |throw new $csArgumentException($$"Cannot encode {value} to ${name.name}: no matching value");""".stripMargin,
      q"""var asByte = wire.ReadByte();
         |
         |${branches.map(_._2).join("\n")}
         |
         |throw new $csArgumentException($$"Cannot decode {wire} to ${name.name}: no matching value");""".stripMargin,
    )
  }

  private def genEnumBodies(name: CSValue.CSType, e: Typedef.Enum) = {
    val branches = e.members.zipWithIndex.toList.map {
      case (m, idx) =>
        val obj = EnumWireStyle.wireName(m.name)
        (
          q"""if (value == $name.$obj)
             |{
             |   writer.Write((byte)${idx.toString});
             |   return;
             |}""".stripMargin,
          q"""if (asByte == ${idx.toString})
             |{
             |   return $name.$obj;
             |}""".stripMargin,
        )
    }

    (
      q"""${branches.map(_._1).join("\n")}
         |
         |throw new $csArgumentException($$"Cannot encode {value} to ${name.name}: no matching value");""".stripMargin,
      q"""var asByte = wire.ReadByte();
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
      case Owner.Adt(_) if target.language.wrappedAdtBranchCodecs =>
        val fields = fieldsOf(d)
        Some(dtoDec(name, fields.map { case (a, b, _) => (a, b) }))
      case _ =>
        None
    }
  }

  private def genDtoBodies(name: CSValue.CSType, d: Typedef.Dto) = {
    val fields = fieldsOf(d)

    val noIndex = Seq(
      q"writer.Write(header);",
      fields.map(_._1).joinCN().endC(),
    ).filterNot(_.isEmpty).join("\n")

    val index = if (target.language.generateIndexWriters) {
      q"""header |= 0b0000001;
         |writer.Write(header);
         |using (var writeMemoryStream = new $memoryStream())
         |{
         |    // ReSharper disable once UnusedVariable
         |    // ReSharper disable AccessToDisposedClosure
         |    using (var fakeWriter = new $binaryWriter(writeMemoryStream))
         |    {
         |        var w = ($iBaboonBinCodecIndexed)this;
         |        ${fields.map(_._3).join("\n").shift(8).trim}
         |    }
         |    writeMemoryStream.Flush();
         |    writer.Write(writeMemoryStream.ToArray());
         |}""".stripMargin
    } else {
      q"""throw new Exception("Index support wasn't enable during Baboon code generation");"""
    }

    val fenc =
      q"""byte header = 0b0000000;
         |
         |if (ctx.UseIndices)
         |{
         |    ${index.shift(4).trim}
         |} else {
         |    ${noIndex.shift(4).trim}
         |}
         |""".stripMargin

    val fdec =
      dtoDec(name, fields.map { case (a, b, _) => (a, b) })

    val enc = d.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, d.id)

        q"""writer.Write((byte)${idx.toString});
           |$fenc""".stripMargin
      case _ => fenc
    }

    val dec = d.id.owner match {
      case Owner.Adt(id) if target.language.wrappedAdtBranchCodecs =>
        val idx = layout.adtBranchIndex(id, d.id)

        q"""var marker = wire.ReadByte();
           |if (marker != ${idx.toString}) throw new $csArgumentException("Unexpected UEBA ADT branch marker: " + marker);
           |return DecodeBranch(ctx, wire);""".stripMargin
      case _ => fdec
    }

    (enc, dec)
  }

  private def dtoDec(name: CSValue.CSType, fields: List[(TextTree[CSValue], TextTree[CSValue])]) = {
    q"""var indexCount = ((IBaboonBinCodecIndexed)this).ConsumeIndex(ctx, wire);
       |
       |if (ctx.UseIndices)
       |{
       |    if (indexCount != IndexElementsCount(ctx)) throw new $csArgumentException("Unexpected UEBA index count: " + indexCount);
       |}
       |
       |return new $name(
       |${fields.map(_._2).join(",\n").shift(4)}
       |);
       |""".stripMargin
  }

  private def fieldsOf(d: Typedef.Dto) = {
    layout.fields(d).map {
      case UebaLayoutPlan.FieldLayout(f, length) =>
        val fieldRef = q"value.${escapeCsKeyword(f.name.name.capitalize)}"
        val enc      = mkEncoder(f.tpe, fieldRef, q"writer")
        val fakeEnc  = mkEncoder(f.tpe, fieldRef, q"fakeWriter")
        val dec      = mkDecoder(f.tpe, q"wire")

        val w = length match {
          case BinReprLen.Fixed(bytes) =>
            q"""// ${f.toString}
               |w.WriteIndexFixedLenField(fakeWriter, ${bytes.toString}, () =>
               |{
               |    ${fakeEnc.endC().shift(4).trim}        
               |});
             """.stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = lengthChecks(v)

            q"""// ${f.toString}
               |{
               |    var length = w.WriteIndexVarLenField(writer, fakeWriter, () =>
               |    {
               |        ${fakeEnc.endC().shift(8).trim}        
               |    });
               |    ${sanityChecks.shift(4).trim}
               |}
               |""".stripMargin
        }

        (enc, dec, w)
    }
  }

  private def lengthChecks(length: BinReprLen): TextTree[CSValue] =
    UebaLengthCheckRenderer.render[CSValue](
      length,
      equalTo = bytes => q"length == ${bytes.toString}",
      oneOf   = bytes => q"new $csSet<uint>() { ${bytes.mkString(", ")} }.Contains(length)",
      enforce = condition => q"""if (!($condition)) throw new $csArgumentException("Invalid UEBA field length: " + length);""",
    )

  private def mkAnyDecoder(a: TypeRef.Any, wref: TextTree[CSValue]): TextTree[CSValue] = {
    val expectedHex = "0x%02x".format(AnyFieldPlan.forField(a, domain).kind & 0xFF)
    q"$baboonAnyBinCodec.Decode($wref, ($csByte)$expectedHex)"
  }

  private def mkAnyEncoder(a: TypeRef.Any, ref: TextTree[CSValue], wref: TextTree[CSValue]): TextTree[CSValue] = {
    val plan                                               = AnyFieldPlan.forField(a, domain)
    val expectedHex                                        = "0x%02x".format(plan.kind & 0xFF)
    def fallback(value: Option[String]): TextTree[CSValue] = value.fold(q"($csString?)null")(v => q"""($csString?)"$v"""")
    q"$baboonAnyBinCodec.Encode(ctx, $wref, ($csByte)$expectedHex, ${fallback(plan.staticDomain)}, ${fallback(plan.staticVersion)}, ${fallback(plan.staticTypeId)}, $ref)"
  }

  private def mkDecoder(tpe: TypeRef, wref: TextTree[CSValue], codecArgs: CodecArguments = CodecArguments.empty): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            CSScalarCodecEmitter.uebaDecode(s, wref)
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Cs) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkDecoder(aliasedRef, wref, codecArgs)
                  case _ =>
                    q"""${codecName(u)}.Instance.Decode(ctx, $wref)"""
                }
              case _ =>
                q"""${codecName(u)}.Instance.Decode(ctx, $wref)"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt if csTypeInfo.isCSValueType(c.args.head, domain) =>
            q"""$BaboonTools.ReadNullableValueType($wref.ReadByte() == 0, () => ${mkDecoder(c.args.head, wref, codecArgs)})""".stripMargin

          case TypeId.Builtins.opt =>
            q"""($wref.ReadByte() == 0 ? null : ${mkDecoder(c.args.head, wref, codecArgs)})""".stripMargin

          case TypeId.Builtins.map =>
            val keyArg       = codecArgs.arg("wk")
            val keyDecoder   = mkDecoder(c.args.head, keyArg, codecArgs.next)
            val keyType      = domainTypes.asCsRef(c.args.head)
            val valueArg     = codecArgs.arg("wv")
            val valueDecoder = mkDecoder(c.args.last, valueArg, codecArgs.next)
            val valueType    = domainTypes.asCsRef(c.args.last)
            q"""$BaboonTools.ReadDict<$keyType, $valueType>($wref, $keyArg => $keyDecoder, $valueArg => $valueDecoder)"""
          case TypeId.Builtins.lst =>
            val arg = codecArgs.arg("wi")
            q"""$BaboonTools.ReadList($wref, $arg => ${mkDecoder(c.args.head, arg, codecArgs.next)})"""
          case TypeId.Builtins.set =>
            val arg = codecArgs.arg("wi")
            q"""$BaboonTools.ReadSet($wref, $arg => ${mkDecoder(c.args.head, arg, codecArgs.next)})"""
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyDecoder(a, wref)
    }

  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[CSValue], wref: TextTree[CSValue], codecArgs: CodecArguments = CodecArguments.empty): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            CSScalarCodecEmitter.uebaEncode(s, wref, ref)
          case u: TypeId.User =>
            domain.defs.meta.nodes(u) match {
              case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
                f.bindings.get(BaboonLang.Cs) match {
                  case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                    mkEncoder(aliasedRef, ref, wref, codecArgs)
                  case _ =>
                    q"""${codecName(u)}.Instance.Encode(ctx, $wref, $ref)"""
                }
              case _ =>
                q"""${codecName(u)}.Instance.Encode(ctx, $wref, $ref)"""
            }
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"""if ($ref == null)
               |{
               |    $wref.Write((byte)0);
               |}
               |else
               |{
               |    $wref.Write((byte)1);
               |    ${mkEncoder(c.args.head, domainTypes.deNull(c.args.head, ref), wref, codecArgs.next).endC().shift(4).trim}
               |}""".stripMargin

          case TypeId.Builtins.map =>
            val arg = codecArgs.arg("kv")
            q"""$wref.Write($ref.Count);
               |foreach (var $arg in $ref)
               |{
               |    ${mkEncoder(c.args.head, q"$arg.Key", wref, codecArgs.next).endC().shift(4).trim}
               |    ${mkEncoder(c.args.last, q"$arg.Value", wref, codecArgs.next).endC().shift(4).trim}
               |}""".stripMargin

          case TypeId.Builtins.lst =>
            val arg = codecArgs.arg("i")
            q"""$wref.Write($ref.Count);
               |foreach (var $arg in $ref)
               |{
               |    ${mkEncoder(c.args.head, arg, wref, codecArgs.next).endC().shift(4).trim}
               |}""".stripMargin

          case TypeId.Builtins.set =>
            val arg = codecArgs.arg("i")
            q"""$wref.Write($ref.Count);
               |foreach (var $arg in $ref)
               |{
               |    ${mkEncoder(c.args.head, arg, wref, codecArgs.next).endC().shift(4).trim}
               |}""".stripMargin

          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
      case a: TypeRef.Any => mkAnyEncoder(a, ref, wref)
    }
  }

  def codecName(id: TypeId.User): CSValue.CSType = {
    codecName(domainTypes.asCsTypeKeepForeigns(id), CSTypeOrigin(id, domain))
  }

  def codecName(name: CSValue.CSType, origin: CSTypeOrigin.TypeInDomain): CSValue.CSType = {
    CSValue.CSType(name.pkg, s"${name.name}_UEBACodec", name.fq, origin.asDerived)
  }

  override def codecMeta(defn: DomainMember.User, name: CSValue.CSType): Option[CSCodecTranslator.CodecMeta] = {
    if (isActive(defn.id)) {
      val fix = csDomTrees.metaMethodFlags(defn, isCodec = false)

      Some(
        CodecMeta(
          q"""public$fix$iBaboonBinCodec<$name> Codec_UEBA()
             |{
             |    return ${codecName(name, CSTypeOrigin(defn.id, domain))}.Instance;
             |}""".stripMargin
        )
      )
    } else {
      None
    }
  }

  def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Cs) &&
    target.language.generateUebaCodecs &&
    (target.language.generateUebaCodecsByDefault || domain.derivationRequests.getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "ueba"
}
