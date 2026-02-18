package io.septimalmind.baboon.parser.model

import io.septimalmind.baboon.parser.model.RawDtoMember.ContractRef

sealed trait RawDefn {
  def name: RawTypeName
  def meta: RawNodeMeta
}

sealed trait RawDtoid {
  this: RawDefn =>
  def name: RawTypeName
  def members: Seq[RawDtoMember]
  def meta: RawNodeMeta
}
case class RawDto(name: RawTypeName, members: Seq[RawDtoMember], derived: Set[RawMemberMeta], meta: RawNodeMeta) extends RawDefn with RawDtoid

case class RawContract(name: RawTypeName, members: Seq[RawDtoMember], meta: RawNodeMeta) extends RawDefn with RawDtoid

case class RawEnum(name: RawTypeName, members: Seq[RawEnumMember], derived: Set[RawMemberMeta], meta: RawNodeMeta) extends RawDefn

case class RawAdt(name: RawTypeName, members: Seq[RawAdtMember], contracts: Seq[ContractRef], derived: Set[RawMemberMeta], meta: RawNodeMeta) extends RawDefn

case class RawForeign(name: RawTypeName, defns: List[RawForeignEntry], derived: Set[RawMemberMeta], meta: RawNodeMeta) extends RawDefn

case class RawNamespace(name: RawTypeName, defns: Seq[RawTLDef], meta: RawNodeMeta) extends RawDefn

case class RawService(name: RawTypeName, defns: Seq[RawFunc], meta: RawNodeMeta) extends RawDefn

case class RawForeignEntryAttr(name: String, value: String)

case class RawForeignEntryAttrs(attrs: List[RawForeignEntryAttr])

object RawForeignEntryAttrs {

  def empty: RawForeignEntryAttrs = RawForeignEntryAttrs(List.empty)

}

sealed trait RawForeignDecl
object RawForeignDecl {
  case class Custom(decl: String, attrs: RawForeignEntryAttrs) extends RawForeignDecl
  case class BaboonRef(typeRef: RawTypeRef) extends RawForeignDecl
}

case class RawForeignEntry(lang: String, decl: RawForeignDecl)

case class RawFunc(
  name: String,
  sig: Seq[RawFuncArg],
  meta: RawNodeMeta,
)

sealed trait RawFuncArg
object RawFuncArg {
  case class Ref(ref: RawTypeRef, marker: String, meta: RawNodeMeta) extends RawFuncArg
  case class Struct(defn: RawDefn) extends RawFuncArg
}

sealed trait RawMemberMeta
object RawMemberMeta {
  case class Derived(id: String) extends RawMemberMeta
  case class Was(id: RawTypeRef) extends RawMemberMeta
}
