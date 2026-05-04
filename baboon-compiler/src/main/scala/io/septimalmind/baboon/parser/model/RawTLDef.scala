package io.septimalmind.baboon.parser.model

sealed trait RawTLDef {
  def value: RawDefn
  def root: Boolean
  def setRoot(root: Boolean): RawTLDef
  def withPrefixDoc(d: Option[RawDocComment]): RawTLDef
}

object RawTLDef {
  case class Enum(root: Boolean, value: RawEnum) extends RawTLDef {
    override def setRoot(root: Boolean): Enum = this.copy(root = root)
    override def withPrefixDoc(d: Option[RawDocComment]): Enum =
      if (d.isEmpty) this else copy(value = value.copy(meta = value.meta.copy(docs = value.meta.docs.copy(prefix = d))))
  }
  case class DTO(root: Boolean, value: RawDto) extends RawTLDef {
    override def setRoot(root: Boolean): DTO = this.copy(root = root)
    override def withPrefixDoc(d: Option[RawDocComment]): DTO =
      if (d.isEmpty) this else copy(value = value.copy(meta = value.meta.copy(docs = value.meta.docs.copy(prefix = d))))
  }
  case class Identifier(root: Boolean, value: RawIdentifier) extends RawTLDef {
    override def setRoot(root: Boolean): Identifier = this.copy(root = root)
    override def withPrefixDoc(d: Option[RawDocComment]): Identifier =
      if (d.isEmpty) this else copy(value = value.copy(meta = value.meta.copy(docs = value.meta.docs.copy(prefix = d))))
  }
  case class ADT(root: Boolean, value: RawAdt) extends RawTLDef {
    override def setRoot(root: Boolean): ADT = this.copy(root = root)
    override def withPrefixDoc(d: Option[RawDocComment]): ADT =
      if (d.isEmpty) this else copy(value = value.copy(meta = value.meta.copy(docs = value.meta.docs.copy(prefix = d))))
  }
  case class Foreign(root: Boolean, value: RawForeign) extends RawTLDef {
    override def setRoot(root: Boolean): Foreign = this.copy(root = root)
    override def withPrefixDoc(d: Option[RawDocComment]): Foreign =
      if (d.isEmpty) this else copy(value = value.copy(meta = value.meta.copy(docs = value.meta.docs.copy(prefix = d))))
  }
  case class Contract(root: Boolean, value: RawContract) extends RawTLDef {
    override def setRoot(root: Boolean): Contract = this.copy(root = root)
    override def withPrefixDoc(d: Option[RawDocComment]): Contract =
      if (d.isEmpty) this else copy(value = value.copy(meta = value.meta.copy(docs = value.meta.docs.copy(prefix = d))))
  }
  case class Service(root: Boolean, value: RawService) extends RawTLDef {
    override def setRoot(root: Boolean): Service = this.copy(root = root)
    override def withPrefixDoc(d: Option[RawDocComment]): Service =
      if (d.isEmpty) this else copy(value = value.copy(meta = value.meta.copy(docs = value.meta.docs.copy(prefix = d))))
  }
  case class Namespace(value: RawNamespace) extends RawTLDef {
    override def setRoot(root: Boolean): Namespace       = this
    override def root: Boolean                           = false
    override def withPrefixDoc(d: Option[RawDocComment]): Namespace = this
  }
  case class Alias(root: Boolean, value: RawAlias) extends RawTLDef {
    override def setRoot(root: Boolean): Alias = this.copy(root = root)
    override def withPrefixDoc(d: Option[RawDocComment]): Alias =
      if (d.isEmpty) this else copy(value = value.copy(meta = value.meta.copy(docs = value.meta.docs.copy(prefix = d))))
  }
}
