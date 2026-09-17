package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.typer.model.{BaboonLang, Domain, DomainMember, Field, TypeId, TypeRef, Typedef}

private[python] final case class PyFieldPlan(wireName: String, attributeName: String, accessorName: String, constructorName: String) {
  def needsAlias: Boolean = wireName != attributeName
}

private[python] object PyFieldPlan {
  final case class SerializationFeatures(hasAny: Boolean, hasAdt: Boolean, hasUserKeyMap: Boolean, hasCustomForeignKey: Boolean) {
    def needsExplicitWalk: Boolean = hasAny || hasAdt || hasUserKeyMap
    def combine(other: SerializationFeatures): SerializationFeatures = SerializationFeatures(
      hasAny || other.hasAny,
      hasAdt || other.hasAdt,
      hasUserKeyMap || other.hasUserKeyMap,
      hasCustomForeignKey || other.hasCustomForeignKey,
    )
  }

  private val noFeatures = SerializationFeatures(false, false, false, false)

  def serializationFeatures(tpe: TypeRef, domain: Domain): SerializationFeatures = tpe match {
    case _: TypeRef.Any => noFeatures.copy(hasAny = true)
    case TypeRef.Scalar(id: TypeId.User) =>
      val isAdt = domain.defs.meta.nodes.get(id).exists {
        case DomainMember.User(_, _: Typedef.Adt, _, _) => true
        case _                                          => false
      }
      noFeatures.copy(hasAdt = isAdt)
    case _: TypeRef.Scalar => noFeatures
    case TypeRef.Constructor(id, args) =>
      val nested     = args.foldLeft(noFeatures)((features, ref) => features.combine(serializationFeatures(ref, domain)))
      val hasUserKey = id == TypeId.Builtins.map && args.head.isInstanceOf[TypeRef.Scalar] && args.head.id.isInstanceOf[TypeId.User]
      nested.combine(noFeatures.copy(hasUserKeyMap = hasUserKey, hasCustomForeignKey = hasUserKey && keyNeedsCustomForeignWrap(args.head, domain)))
  }

  private def keyNeedsCustomForeignWrap(tpe: TypeRef, domain: Domain): Boolean = tpe match {
    case TypeRef.Scalar(id: TypeId.User) =>
      domain.defs.meta.nodes.get(id) match {
        case Some(DomainMember.User(_, f: Typedef.Foreign, _, _)) =>
          f.bindings.get(BaboonLang.Py) match {
            case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(ref))) => keyNeedsCustomForeignWrap(ref, domain)
            case Some(Typedef.ForeignEntry(_, _: Typedef.ForeignMapping.Custom))      => true
            case _                                                                    => false
          }
        case Some(DomainMember.User(_, d: Typedef.Dto, _, _)) if d.fields.size == 1 && d.contracts.isEmpty =>
          keyNeedsCustomForeignWrap(d.fields.head.tpe, domain)
        case _ => false
      }
    case _ => false
  }

  def apply(field: Field, implementsContract: Boolean): PyFieldPlan = {
    val wireName  = field.name.name
    val escaped   = PyKeywords.escapePyKeyword(wireName)
    val attribute = if (implementsContract || PyKeywords.isKeyword(wireName)) s"${wireName}_" else wireName
    PyFieldPlan(wireName, attribute, escaped, escaped)
  }

  def forDto(domain: Domain, dto: Typedef.Dto): Map[Field, PyFieldPlan] = {
    val contractFields = dto.contracts
      .flatMap(domain.defs.meta.nodes.get).collect {
        case DomainMember.User(_, contract: Typedef.Contract, _, _) => contract.fields
      }.flatten.toSet
    dto.fields.map(field => field -> apply(field, contractFields.contains(field))).toMap
  }

  def containsAny(tpe: TypeRef): Boolean = tpe match {
    case _: TypeRef.Any         => true
    case _: TypeRef.Scalar      => false
    case c: TypeRef.Constructor => c.args.exists(containsAny)
  }
}
