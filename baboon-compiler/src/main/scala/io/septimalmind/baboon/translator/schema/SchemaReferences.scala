package io.septimalmind.baboon.translator.schema

import io.septimalmind.baboon.typer.model.*

final case class SchemaReferences(
  foreigns: Map[TypeId.User, Typedef.Foreign],
  enums: Map[TypeId.User, Typedef.Enum],
) {
  val resolutions: Map[TypeId.User, Option[TypeRef]] = foreigns.map { case (id, f) => id -> f.runtimeMapping }

  def resolve(ref: TypeRef): TypeRef = SchemaReferences.resolve(ref, resolutions)
}

object SchemaReferences {
  def prepare(domain: Domain): SchemaReferences = {
    val (foreigns, enums) = domain.defs.meta.nodes.valuesIterator.foldLeft(
      (Map.empty[TypeId.User, Typedef.Foreign], Map.empty[TypeId.User, Typedef.Enum])
    ) {
      case ((fs, es), DomainMember.User(_, f: Typedef.Foreign, _, _)) => (fs.updated(f.id, f), es)
      case ((fs, es), DomainMember.User(_, e: Typedef.Enum, _, _)) => (fs, es.updated(e.id, e))
      case (acc, _) => acc
    }
    SchemaReferences(foreigns, enums)
  }

  def resolve(ref: TypeRef, resolutions: Map[TypeId.User, Option[TypeRef]]): TypeRef = ref match {
    case TypeRef.Scalar(id: TypeId.User) =>
      resolutions.get(id) match {
        case Some(Some(target)) => resolve(target, resolutions)
        case _ => ref
      }
    case TypeRef.Constructor(id, args) => TypeRef.Constructor(id, args.map(resolve(_, resolutions)))
    case _ => ref
  }

  def name(id: TypeId.User): String =
    (id.pkg.path.toList ++ id.owner.asPseudoPkg :+ id.name.name).map(sanitize).mkString("_")

  def sanitize(s: String): String = s.replace("-", "_").replace(".", "_")
}
