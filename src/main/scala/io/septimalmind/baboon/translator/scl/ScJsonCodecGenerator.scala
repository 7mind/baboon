package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.translator.scl.ScCodecTranslator.CodecMeta
import io.septimalmind.baboon.typer.model.{BaboonEvolution, Domain, DomainMember}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class ScJsonCodecGenerator(
  trans: ScTypeTranslator,
  csDomTrees: ScTreeTools,
  target: ScTarget,
  domain: Domain,
  evo: BaboonEvolution,
  csTypeInfo: ScTypeInfo,
) extends ScCodecTranslator {

  override def translate(defn: DomainMember.User, csRef: ScValue.ScType, srcRef: ScValue.ScType): Option[TextTree[ScValue]] = None

  override def codecMeta(defn: DomainMember.User, name: ScValue.ScType): CodecMeta = CodecMeta(q"")

  override def codecType(): ScValue.ScType = ???

  override def codecName(name: ScValue.ScType): ScValue.ScType = {
    ScValue.ScType(name.pkg, s"${name.name}_JSONCodec", name.fq)
  }

  override def codecInterfaceProperty(): TextTree[ScValue] = ???

  override def codecImplProperty(): TextTree[ScValue] = ???

  override def codecGenericImplField(): TextTree[ScValue] = ???

  override def codecImplField(): TextTree[ScValue] = ???
}
