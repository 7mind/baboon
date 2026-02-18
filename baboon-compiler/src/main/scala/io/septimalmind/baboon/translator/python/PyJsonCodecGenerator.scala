package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.PyType
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

final class PyJsonCodecGenerator(
  typeTranslator: PyTypeTranslator,
  treeTools: PyDomainTreeTools,
  pyFileTools: PyFileTools,
  evolution: BaboonEvolution,
  pyTarget: PyTarget,
  domain: Domain,
) extends PyCodecTranslator {
  override def translate(defn: DomainMember.User, pyRef: PyType, srcRef: PyType): Option[TextTree[PyValue]] = {
    (defn.defn match {
      case _: Typedef.Dto      => Some(genDtoBodies(pyRef))
      case _: Typedef.Adt      => Some(genAdtBodies(pyRef))
      case _: Typedef.Enum     => Some(genEnumBodies(pyRef))
      case f: Typedef.Foreign =>
        f.bindings.get(BaboonLang.Py) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(_))) => None
          case _ => Some(genForeignTypesBodies(pyRef))
        }
      case _: Typedef.Service  => None
      case _: Typedef.Contract => None
    }).map {
      case (enc, dec) => genCodec(defn, pyRef, srcRef, enc, dec)
    }
  }

  private def genCodec(
    defn: DomainMember.User,
    name: PyType,
    srcRef: PyType,
    enc: TextTree[PyValue],
    dec: TextTree[PyValue],
  ): TextTree[PyValue] = {
    val isEncoderEnabled = pyTarget.language.enableDeprecatedEncoders || domain.version == evolution.latest
    val encodeMethod = if (isEncoderEnabled) {
      List(
        q"""def encode(self, context: $baboonCodecContext, value: $name) -> $pyStr:
           |    ${enc.shift(4).trim}
           |""".stripMargin.trim
      )
    } else Nil
    val decodeMethod =
      List(q"""def decode(self, context: $baboonCodecContext, wire: $pyStr) -> $name:
              |    ${dec.shift(4).trim}
              |""".stripMargin)
    val baseMethods = encodeMethod ++ decodeMethod
    val cName       = q"${srcRef.name}_JsonCodec"
    val cType       = q"'${codecType(defn.id)}'"

    val cParent = if (isEncoderEnabled) {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecBase[$name, $cType]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecBase[$name, $cType]"
        case _ if defn.isAdt                                => q"$baboonJsonCodecBaseGeneratedAdt[$name, $cType]"
        case _                                              => q"$baboonJsonCodecBaseGenerated[$name, $cType]"
      }
    } else {
      defn match {
        case DomainMember.User(_, _: Typedef.Enum, _, _)    => q"$baboonJsonCodecNoEncoder[$name, $cType]"
        case DomainMember.User(_, _: Typedef.Foreign, _, _) => q"$baboonJsonCodecNoEncoder[$name, $cType]"
        case _ if defn.isAdt                                => q"$baboonJsonCodecNoEncoderGeneratedAdt[$name, $cType]"
        case _                                              => q"$baboonJsonCodecNoEncoderGenerated[$name, $cType]"
      }
    }

    q"""class $cName($cParent):
       |    ${baseMethods.joinN().shift(4).trim}
       |
       |    ${treeTools.makeCodecMeta(defn).joinN().shift(4).trim}
       |
       |    def target_type(self) -> $pyType:
       |        return $name
       |
       |    _lazy_instance: $baboonLazy['$cName'] = $baboonLazy(lambda: $cName())
       |""".stripMargin
  }

  private def genForeignTypesBodies(name: PyType): (TextTree[PyValue], TextTree[PyValue]) = {
    (
      q"""raise ValueError(f"$name is a foreign type")""",
      q"""raise ValueError(f"$name is a foreign type")""",
    )
  }

  private def genEnumBodies(name: PyType): (TextTree[PyValue], TextTree[PyValue]) = {
    val encode = q"""return $pyJsonDumps(value.value)""".stripMargin
    val decode = q"""return $name($pyJsonLoads(wire))""".stripMargin
    (encode, decode)
  }

  private def genAdtBodies(name: PyType): (TextTree[PyValue], TextTree[PyValue]) = {
    val encode = q"""return value.model_dump_json()""".stripMargin
    val decode = q"""return $name.model_validate_json(wire)""".stripMargin
    (encode, decode)
  }

  private def genDtoBodies(name: PyType): (TextTree[PyValue], TextTree[PyValue]) = {
    val encode = q"""return value.model_dump_json()""".stripMargin
    val decode = q"""return $name.model_validate_json(wire)""".stripMargin
    (encode, decode)
  }

  override def codecType(tid: TypeId.User): PyType = {
    val typeName = s"${tid.name.name.capitalize}_JsonCodec"
    val moduleId = typeTranslator.toPyModule(tid, domain.version, evolution, pyFileTools.definitionsBasePkg)
    PyType(moduleId, typeName)
  }

  override def codecMeta(tid: TypeId.User): PyCodecTranslator.CodecMeta = {
    val meta = q"""@$pyStaticMethod
                  |def codec_json():
                  |    return ${codecType(tid)}.instance()""".stripMargin
    PyCodecTranslator.CodecMeta(meta)
  }

  override def isActive(id: TypeId): Boolean = {
    !BaboonEnquiries.isBaboonRefForeign(id, domain, BaboonLang.Py) &&
    pyTarget.language.generateJsonCodecs && (pyTarget.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "json"
}
