package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.PyType
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember, TypeId, Typedef}
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
      case _: Typedef.Foreign  => Some(genForeignTypesBodies(pyRef))
      case _: Typedef.Service  => None
      case _: Typedef.Contract => None
    }).map {
      case (enc, dec) => genCodec(defn, pyRef, srcRef, enc, dec)
    }
  }

  private def genCodec(
    defn: DomainMember.User,
    pyRef: PyType,
    srcRef: PyType,
    enc: TextTree[PyValue],
    dec: TextTree[PyValue],
  ): TextTree[PyValue] = {
    val baseMethods =
      q"""def encode(self, value: $pyRef) -> $pyStr:
         |    ${enc.shift(4).trim}
         |    
         |def decode(self, wire: $pyStr) -> $pyRef:
         |    ${dec.shift(4).trim}
         |""".stripMargin

    val codecParent = q"$baboonJsonCodec[$pyRef]"
    val codecName   = q"${srcRef.name}_JsonCodec"
    q"""class $codecName($codecParent):
       |    ${baseMethods.shift(4).trim}
       |    
       |    ${treeTools.makeCodecMeta(defn).joinN().shift(4).trim}
       |
       |    @$pyClassMethod
       |    @$pyCache
       |    def instance (cls):
       |        return cls()
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
    val typeName = s"${tid.name.name}_JsonCodec"
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
    pyTarget.language.generateJsonCodecs && (pyTarget.language.generateJsonCodecsByDefault || domain.derivationRequests
      .getOrElse(RawMemberMeta.Derived("json"), Set.empty[TypeId]).contains(id))
  }

  override def id: String = "json"
}
