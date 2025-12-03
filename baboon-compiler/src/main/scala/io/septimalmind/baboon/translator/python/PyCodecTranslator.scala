package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.translator.python.PyCodecTranslator.CodecMeta
import io.septimalmind.baboon.translator.python.PyValue.PyType
import io.septimalmind.baboon.typer.model.{DomainMember, TypeId}
import izumi.fundamentals.platform.strings.TextTree

trait PyCodecTranslator {
  def translate(defn: DomainMember.User, pyRef: PyType, srcRef: PyType): Option[TextTree[PyValue]]
  def codecType(tid: TypeId.User): PyType
  def codecMeta(tid: TypeId.User): CodecMeta
  def isActive(id: TypeId): Boolean
  def id: String
}

object PyCodecTranslator {
  case class CodecMeta(member: TextTree[PyValue])
}
