package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.PyType
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class PyUEBACodecGenerator(
  typeTranslator: PyTypeTranslator,
  treeTools: PyDomainTreeTools,
  evolution: BaboonEvolution,
  pyFileTools: PyFileTools,
  pyTarget: PyTarget,
  domain: Domain,
) extends PyCodecTranslator {
  override def translate(
    defn: DomainMember.User,
    pyRef: PyValue.PyType,
    srcRef: PyValue.PyType,
  ): Option[TextTree[PyValue]] = {
    (defn.defn match {
      case d: Typedef.Dto      => Some(genDtoBodies(pyRef, d))
      case e: Typedef.Enum     => Some(genEnumBodies(e))
      case a: Typedef.Adt      => Some(genAdtBodies(pyRef, a))
      case _: Typedef.Foreign  => Some(genForeignTypesBodies(pyRef))
      case _: Typedef.Service  => None
      case _: Typedef.Contract => None
    }).map {
      case (enc, dec) =>
        val branchDecoder = defn.defn match {
          case d: Typedef.Dto => genBranchDecoder(pyRef, d)
          case _              => None
        }
        genCodec(defn, pyRef, srcRef, enc, dec, branchDecoder)
    }
  }

  private def genCodec(
    defn: DomainMember.User,
    name: PyType,
    srcRef: PyType,
    enc: TextTree[PyValue],
    dec: TextTree[PyValue],
    branchDecoder: Option[TextTree[PyValue]],
  ): TextTree[PyValue] = {
    val isEncoderEnabled = pyTarget.language.enableDeprecatedEncoders || domain.version == evolution.latest
    val indexBody = defn.defn match {
      case d: Typedef.Dto =>
        val varlens = d.fields.filter(f => domain.refMeta(f.tpe).len.isVariable)
        val comment = varlens.map(f => q"# ${f.toString}").joinN()
        q"""$comment
           |return ${varlens.size.toString}""".stripMargin

      case _: Typedef.Enum    => q"return 0"
      case _: Typedef.Adt     => q"return 0"
      case _: Typedef.Foreign => q"""raise ValueError("$name is a foreign type")"""

      case d: Typedef.Contract => throw new IllegalArgumentException(s"BUG: contract codec should not be rendered: $d")
      case d: Typedef.Service  => throw new IllegalArgumentException(s"BUG: service codec should not be rendered: $d")
    }

    val indexMethods = List(
      q"""def index_elements_count(self, ctx: $baboonCodecContext) -> $pyInt:
         |    ${indexBody.shift(4).trim}
         |""".stripMargin
    )

    val encoderMethod = if (isEncoderEnabled) {
      List(
        q"""def encode(self, ctx: $baboonCodecContext, wire: $baboonLEDataOutputStream, value: $name):
           |    ${enc.shift(4).trim}
           |""".stripMargin
      )
    } else Nil

    val decoderMethod = List(
      q"""def decode(self, ctx: $baboonCodecContext, wire: $baboonLEDataInputStream) -> $name:
         |    ${dec.shift(4).trim}
         |""".stripMargin
    )

    val baseMethods = encoderMethod ++ decoderMethod ++
      branchDecoder.map {
        body =>
          q"""def decode_branch(self, ctx: $baboonCodecContext, wire: $baboonLEDataInputStream) -> $name: 
             |    ${body.shift(4).trim}
             |""".stripMargin
      } ++ indexMethods

    val cName = q"${srcRef.name}_UEBACodec"
    val cType = q"'${codecType(defn.id)}'"

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecBase[$name, $cType]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecBase[$name, $cType]"
        case _ if defn.isAdt                                => q"$baboonBinCodecGeneratedAdt[$name, $cType]"
        case _                                              => q"$baboonBinCodecGenerated[$name, $cType]"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonBinCodecNoEncoder[$name, $cType]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonBinCodecNoEncoder[$name, $cType]"
        case _ if defn.isAdt                                => q"$baboonBinCodecNoEncoderGeneratedAdt[$name, $cType]"
        case _                                              => q"$baboonBinCodecNoEncoderGenerated[$name, $cType]"
      }
    }

    val parents = List(cParent, q"$baboonBinCodecIndexed")

    q"""class $cName(${parents.join(", ")}):
       |    ${baseMethods.joinNN().shift(4).trim}
       |
       |    ${treeTools.makeCodecMeta(defn).joinN().shift(4).trim}
       |
       |    def target_type(self) -> $pyType:
       |        return $name
       |
       |    _lazy_instance: $baboonLazy['$cName'] = $baboonLazy(lambda: $cName())
       |""".stripMargin
  }

  private def genBranchDecoder(
    name: PyType,
    dto: Typedef.Dto,
  ): Option[TextTree[PyValue]] = {
    dto.id.owner match {
      case Owner.Adt(_) if pyTarget.language.wrappedAdtBranchCodecs =>
        val fieldsCodecs = fieldsOf(dto).map { case (enc, dec, _) => (enc, dec) }
        Some(genDtoDecoder(name, fieldsCodecs, dto))
      case _ => None
    }
  }

  private def genEnumBodies(enum: Typedef.Enum): (TextTree[PyValue], TextTree[PyValue]) = {
    val branches = enum.members.zipWithIndex.toList.map {
      case (m, i) =>
        (
          q"""if value.value == "${m.name}":
             |    wire.write_byte(${i.toString})
             |    return
             |""".stripMargin,
          q"""if as_byte == ${i.toString}:
             |    return ${enum.id.name.name}.${m.name}
             |""".stripMargin,
        )
    }

    (
      q"""${branches.map(_._1).joinN()}
         |
         |raise ValueError(f"Cannot encode {value} to ${enum.id.name.name}: no matching value")""".stripMargin,
      q"""as_byte = wire.read_byte()
         |
         |${branches.map(_._2).joinN()}
         |
         |raise ValueError(f"Cannot decode {wire} to ${enum.id.name.name}: no matching value")""".stripMargin,
    )
  }

  private def genAdtBodies(name: PyType, adt: Typedef.Adt): (TextTree[PyValue], TextTree[PyValue]) = {
    val branches = adt.dataMembers(domain).zipWithIndex.map {
      case (member, i) =>
        val cName = codecType(member)
        val encoder = {
          if (pyTarget.language.wrappedAdtBranchCodecs) {
            q"""if isinstance(value, ${member.name.name}):
               |    $cName.instance().encode(ctx, wire, value)
               |    return
               |""".stripMargin
          } else {
            q"""if isinstance(value, ${member.name.name}):
               |    wire.write_byte(${i.toString})
               |    $cName.instance().encode(ctx, wire, value)
               |    return
               |""".stripMargin
          }
        }
        val decoder = {
          if (pyTarget.language.wrappedAdtBranchCodecs) {
            q"""if as_byte == ${i.toString}:
               |    return $cName.instance().decode_branch(ctx, wire)
               |""".stripMargin
          } else {
            q"""if as_byte == ${i.toString}:
               |    return $cName.instance().decode(ctx, wire)
               |""".stripMargin
          }
        }
        (encoder, decoder)
    }

    (
      q"""${branches.map(_._1).joinN()}
         |
         |raise ValueError(f"Cannot encode {value} to $name: no matching value")
         |""".stripMargin,
      q"""as_byte = wire.read_byte()
         |
         |${branches.map(_._2).joinN()}
         |
         |raise ValueError(f"Cannot decode {wire} to $name: no matching value")
         |""".stripMargin,
    )
  }

  private def genDtoDecoder(name: PyValue.PyType, fields: List[(TextTree[PyValue], TextTree[PyValue])], dto: Typedef.Dto): TextTree[PyValue] = {
    val fieldsDecoders = dto.fields.zip(fields.map(_._2)).map { case (field, decoder) => q"${field.name.name}=$decoder" }
    q"""index = self.read_index(ctx, wire)
       |
       |if ctx.use_indices:
       |    assert len(index) == self.index_elements_count(ctx)
       |
       |return ${name.name}(
       |    ${fieldsDecoders.join(",\n").shift(4).trim}
       |)
       |""".stripMargin
  }

  private def genDtoBodies(name: PyType, dto: Typedef.Dto): (TextTree[PyValue], TextTree[PyValue]) = {
    def adtBranchIndex(id: TypeId.User) = {
      domain.defs.meta
        .nodes(id).asInstanceOf[DomainMember.User]
        .defn.asInstanceOf[Typedef.Adt]
        .dataMembers(domain)
        .zipWithIndex.find(_._1 == dto.id).get._2
    }

    val fields = fieldsOf(dto)

    val noIndex = Seq(
      q"wire.write_byte(header)",
      fields.map(_._1).joinN(),
    ).filterNot(_.isEmpty).join("\n")

    val fieldsEncoders =
      q"""header = 0b0000000
         |
         |if ctx.use_indices:
         |    header = (header | 0b0000001) & 0xFF
         |    wire.write_byte(header)
         |    write_memory_stream = $pyBytesIO()
         |    fake_writer = $baboonLEDataOutputStream(write_memory_stream)
         |    ${fields.map(_._3).join("\n").shift(4).trim}
         |    write_memory_stream.flush()
         |    wire.write(write_memory_stream.getvalue())
         |else:
         |    ${noIndex.shift(4).trim}
         |""".stripMargin

    val fieldsDecoders = genDtoDecoder(name, fields.map { case (a, b, _) => (a, b) }, dto)

    val enc = dto.id.owner match {
      case Owner.Adt(id) if pyTarget.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)
        q"""wire.write_byte(${idx.toString})
           |$fieldsEncoders""".stripMargin
      case _ => fieldsEncoders
    }

    val dec = dto.id.owner match {
      case Owner.Adt(id) if pyTarget.language.wrappedAdtBranchCodecs =>
        val idx = adtBranchIndex(id)
        q"""marker = wire.read_byte()
           |assert marker == ${idx.toString}
           |return self.decode_branch(ctx, wire)""".stripMargin
      case _ => fieldsDecoders
    }

    (enc, dec)
  }

  private def fieldsOf(dto: Typedef.Dto): List[(TextTree[PyValue], TextTree[PyValue], TextTree[PyValue])] = {
    dto.fields.map {
      f =>
        val fieldRef = q"value.${f.name.name}"
        val encoder  = mkEncoder(f.tpe, fieldRef, q"wire")
        val fakeEnc  = mkEncoder(f.tpe, fieldRef, q"fake_writer")
        val dec      = mkDecoder(f.tpe)

        val w = domain.refMeta(f.tpe).len match {
          case BinReprLen.Fixed(bytes) =>
            q"""# ${f.toString}
               |before = write_memory_stream.tell()
               |${fakeEnc.trim}
               |after = write_memory_stream.tell()
               |length = after - before
               |assert length == ${bytes.toString}
               |""".stripMargin

          case v: BinReprLen.Variable =>
            val sanityChecks = v match {
              case BinReprLen.Unknown() =>
                q"assert after >= before, f\"Got after={after}, before={before}\""

              case BinReprLen.Alternatives(variants) =>
                q"assert length in {${variants.mkString(", ")}}, f\"Got length={length}\""

              case BinReprLen.Range(min, max) =>
                List(
                  Some(q"assert length >= ${min.toString}, f\"Got length={length}\" "),
                  max.map(m => q"assert length <= ${m.toString}, $$\"Got length={length}\""),
                ).flatten.joinN()
            }

            q"""# ${f.toString}
               |before = write_memory_stream.tell()
               |wire.write_i32(before)
               |${fakeEnc.trim}
               |after = write_memory_stream.tell()
               |length = after - before
               |wire.write_i32(length)
               |${sanityChecks.trim}
               |""".stripMargin
        }
        (encoder, dec, w)
    }
  }

  private def mkEncoder(tpe: TypeRef, ref: TextTree[PyValue], writerRef: TextTree[PyValue]): TextTree[PyValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit => q"$writerRef.write_bool($ref)"
              case TypeId.Builtins.i08 => q"$writerRef.write_byte($ref)"
              case TypeId.Builtins.i16 => q"$writerRef.write_i16($ref)"
              case TypeId.Builtins.i32 => q"$writerRef.write_i32($ref)"
              case TypeId.Builtins.i64 => q"$writerRef.write_i64($ref)"
              case TypeId.Builtins.u08 => q"$writerRef.write_ubyte($ref)"
              case TypeId.Builtins.u16 => q"$writerRef.write_u16($ref)"
              case TypeId.Builtins.u32 => q"$writerRef.write_u32($ref)"
              case TypeId.Builtins.u64 => q"$writerRef.write_u64($ref)"
              case TypeId.Builtins.f32 => q"$writerRef.write_f32($ref)"
              case TypeId.Builtins.f64 => q"$writerRef.write_f64($ref)"

              case TypeId.Builtins.f128 => q"$writerRef.write_f128($ref)"
              case TypeId.Builtins.str  => q"$writerRef.write_str($ref)"

              case TypeId.Builtins.uid => q"$writerRef.write_uuid($ref)"
              case TypeId.Builtins.tsu => q"$writerRef.write_datetime($ref)"
              case TypeId.Builtins.tso => q"$writerRef.write_datetime($ref)"

              case TypeId.Builtins.bytes => q"$writerRef.write_bytes($ref)"

              case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User =>
            val target = codecType(u)
            q"$target.instance().encode(ctx, $writerRef, $ref)"
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"$writerRef.write_optional($ref, lambda v: ${mkEncoder(c.args.head, q"v", writerRef)})"
          case TypeId.Builtins.map =>
            val keyEncoder   = mkEncoder(c.args.head, q"v", writerRef)
            val valueEncoder = mkEncoder(c.args.last, q"v", writerRef)
            q"$writerRef.write_dict($ref, lambda v: $keyEncoder, lambda v: $valueEncoder)"
          case TypeId.Builtins.lst =>
            q"$writerRef.write_seq($ref, lambda v: ${mkEncoder(c.args.head, q"v", writerRef)})"
          case TypeId.Builtins.set =>
            q"$writerRef.write_seq($ref, lambda v: ${mkEncoder(c.args.head, q"v", writerRef)})"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def mkDecoder(tpe: TypeRef): TextTree[PyValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit => q"wire.read_bool()"
              case TypeId.Builtins.i08 => q"wire.read_byte()"
              case TypeId.Builtins.i16 => q"wire.read_i16()"
              case TypeId.Builtins.i32 => q"wire.read_i32()"
              case TypeId.Builtins.i64 => q"wire.read_i64()"
              case TypeId.Builtins.u08 => q"wire.read_ubyte()"
              case TypeId.Builtins.u16 => q"wire.read_u16()"
              case TypeId.Builtins.u32 => q"wire.read_u32()"
              case TypeId.Builtins.u64 => q"wire.read_u64()"
              case TypeId.Builtins.f32 => q"wire.read_f32()"
              case TypeId.Builtins.f64 => q"wire.read_f64()"

              case TypeId.Builtins.f128 => q"wire.read_f128()"
              case TypeId.Builtins.str  => q"wire.read_string()"

              case TypeId.Builtins.uid => q"wire.read_uuid()"
              case TypeId.Builtins.tsu => q"wire.read_datetime()"
              case TypeId.Builtins.tso => q"wire.read_datetime()"

              case TypeId.Builtins.bytes => q"wire.read_bytes()"

              case o => throw new RuntimeException(s"BUG: Unexpected type: $o")
            }
          case u: TypeId.User => q"${codecType(u)}.instance().decode(ctx, wire)"
        }
      case c: TypeRef.Constructor =>
        c.id match {
          case TypeId.Builtins.opt =>
            q"None if wire.read_byte() == 0 else ${mkDecoder(c.args.head)}"
          case TypeId.Builtins.map =>
            val keyDecoder   = mkDecoder(c.args.head)
            val valueDecoder = mkDecoder(c.args.last)
            q"{$keyDecoder: $valueDecoder for _ in range(wire.read_i32())}"
          case TypeId.Builtins.lst =>
            q"[${mkDecoder(c.args.head)} for _ in range(wire.read_i32())]"
          case TypeId.Builtins.set =>
            q"{${mkDecoder(c.args.head)} for _ in range(wire.read_i32())}"
          case o =>
            throw new RuntimeException(s"BUG: Unexpected type: $o")
        }
    }
  }

  private def genForeignTypesBodies(name: PyType): (TextTree[PyValue], TextTree[PyValue]) = {
    (
      q"""raise ValueError(f"${name.name} is a foreign type")""",
      q"""raise ValueError(f"${name.name} is a foreign type")""",
    )
  }

  override def codecType(tid: TypeId.User): PyType = {
    val typeName = s"${tid.name.name}_UEBACodec"
    val moduleId = typeTranslator
      .toPyModule(tid, domain.version, evolution, pyFileTools.definitionsBasePkg)
    PyType(moduleId, typeName)
  }

  override def codecMeta(tid: TypeId.User): PyCodecTranslator.CodecMeta = {
    val meta = q"""@$pyStaticMethod
                  |def codec_ueba():
                  |    return ${codecType(tid)}.instance()""".stripMargin
    PyCodecTranslator.CodecMeta(meta)
  }

  override def isActive(id: TypeId): Boolean = {
    pyTarget.language.generateUebaCodecs && (pyTarget.language.generateUebaCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "ueba"
}
