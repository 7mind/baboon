package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

final class RsFieldRepresentation(domain: Domain, evo: BaboonEvolution, trans: RsTypeTranslator, enquiries: BaboonEnquiries) {
  private val recursiveTypes = domain.defs.meta.nodes.valuesIterator.collect {
    case m: DomainMember.User if enquiries.isRecursiveTypedef(m, domain) => m.id
  }.toSet

  private val references = domain.defs.meta.nodes.valuesIterator.collect {
    case m: DomainMember.User =>
      val refs = m.defn match {
        case dto: Typedef.Dto => dto.fields.map(_.tpe)
        case adt: Typedef.Adt => adt.dataMembers(domain).toList.map(TypeRef.Scalar.apply)
        case _                => Nil
      }
      m.id -> refs
  }.toMap

  private def reaches(seed: TypeRef => Boolean): Set[TypeId.User] = {
    var result  = Set.empty[TypeId.User]
    var changed = true
    while (changed) {
      val next = result ++ references.iterator.collect {
        case (id, refs) if refs.exists(ref => reachesRef(ref, result, seed)) => id
      }
      changed = next != result
      result  = next
    }
    result
  }

  private def reachesRef(ref: TypeRef, marked: Set[TypeId.User], seed: TypeRef => Boolean): Boolean = {
    seed(ref) || (ref match {
      case TypeRef.Scalar(id: TypeId.User) => marked.contains(id)
      case TypeRef.Constructor(_, args)    => args.exists(reachesRef(_, marked, seed))
      case _                               => false
    })
  }

  def isBareFloat(ref: TypeRef): Boolean = ref match {
    case TypeRef.Scalar(TypeId.Builtins.f32 | TypeId.Builtins.f64) => true
    case _                                                         => false
  }

  private val floatTypes = reaches(isBareFloat)
  private val anyTypes   = reaches(_.isInstanceOf[TypeRef.Any])

  def containsFloat(ref: TypeRef): Boolean = reachesRef(ref, floatTypes, isBareFloat)
  def containsAny(ref: TypeRef): Boolean   = reachesRef(ref, anyTypes, _.isInstanceOf[TypeRef.Any])

  def needsBox(tpe: TypeRef): Boolean = tpe match {
    case TypeRef.Scalar(id: TypeId.User) => recursiveTypes.contains(id)
    case TypeRef.Constructor(_, args)    => args.exists(needsBox)
    case _                               => false
  }

  def field(tpe: TypeRef): RsFieldRepresentation.Field = {
    val surface  = trans.asRsRef(tpe, domain, evo)
    val indirect = needsBox(tpe)
    RsFieldRepresentation.Field(surface, if (indirect) q"Box<$surface>" else surface, indirect)
  }
}

object RsFieldRepresentation {
  final case class Field(surface: TextTree[RsValue], stored: TextTree[RsValue], indirect: Boolean)
}
