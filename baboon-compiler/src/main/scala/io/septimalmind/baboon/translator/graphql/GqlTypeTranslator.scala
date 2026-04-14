package io.septimalmind.baboon.translator.graphql

import io.septimalmind.baboon.typer.model.*

class GqlTypeTranslator {

  def foreignTypeResolution(domain: Domain): Map[TypeId.User, Option[TypeRef]] = {
    domain.defs.meta.nodes.values.collect {
      case u: DomainMember.User =>
        u.defn match {
          case f: Typedef.Foreign => Some(f.id -> f.runtimeMapping)
          case _                 => None
        }
    }.flatten.toMap
  }

  def resolveTypeRef(ref: TypeRef, foreignResolutions: Map[TypeId.User, Option[TypeRef]]): TypeRef = {
    ref match {
      case TypeRef.Scalar(id: TypeId.User) =>
        foreignResolutions.get(id) match {
          case Some(Some(resolved)) => resolveTypeRef(resolved, foreignResolutions)
          case _                    => ref
        }
      case TypeRef.Constructor(id, args) =>
        TypeRef.Constructor(id, args.map(a => resolveTypeRef(a, foreignResolutions)))
      case _ => ref
    }
  }

  def scalarName(id: TypeId.BuiltinScalar): String = {
    id match {
      case TypeId.Builtins.bit => "Boolean"
      case TypeId.Builtins.str => "String"
      case TypeId.Builtins.i08 => "Int"
      case TypeId.Builtins.i16 => "Int"
      case TypeId.Builtins.i32 => "Int"
      case TypeId.Builtins.i64 => "BaboonInt64"
      case TypeId.Builtins.u08 => "Int"
      case TypeId.Builtins.u16 => "Int"
      case TypeId.Builtins.u32 => "Int"
      case TypeId.Builtins.u64 => "BaboonUInt64"
      case TypeId.Builtins.f32 => "Float"
      case TypeId.Builtins.f64 => "Float"
      case TypeId.Builtins.f128 => "BaboonFloat128"
      case TypeId.Builtins.uid => "ID"
      case TypeId.Builtins.tsu => "BaboonDateTimeUtc"
      case TypeId.Builtins.tso => "BaboonDateTimeOffset"
      case TypeId.Builtins.bytes => "BaboonBytes"
      case other => s"BaboonUnknown_${other.name.name}"
    }
  }

  def typeRefStr(ref: TypeRef): String = {
    ref match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        scalarName(id)
      case TypeRef.Scalar(id: TypeId.User) =>
        typeName(id)
      case TypeRef.Constructor(TypeId.Builtins.opt, args) =>
        typeRefStr(args.head)
      case TypeRef.Constructor(TypeId.Builtins.lst, args) =>
        s"[${typeRefStr(args.head)}!]"
      case TypeRef.Constructor(TypeId.Builtins.set, args) =>
        s"[${typeRefStr(args.head)}!]"
      case TypeRef.Constructor(TypeId.Builtins.map, args) =>
        s"[${mapEntryTypeName(args.head, args.tail.head)}!]"
      case other =>
        s"BaboonUnknown_${other.id.name.name}"
    }
  }

  def isOptional(ref: TypeRef): Boolean = {
    ref match {
      case TypeRef.Constructor(TypeId.Builtins.opt, _) => true
      case _ => false
    }
  }

  def fieldTypeStr(ref: TypeRef): String = {
    val base = typeRefStr(ref)
    if (isOptional(ref)) base else s"$base!"
  }

  def typeName(id: TypeId.User): String = {
    val parts = id.pkg.path.toList ++ id.owner.asPseudoPkg :+ id.name.name
    parts.map(sanitize).mkString("_")
  }

  def sanitize(s: String): String = {
    s.replace("-", "_").replace(".", "_")
  }

  /** Ensure a name is a valid GraphQL identifier: `[_A-Za-z][_0-9A-Za-z]*`.
    * Prepends `_` if it starts with a digit, replaces non-alphanumeric chars with `_`,
    * and escapes the `__` introspection prefix.
    */
  def sanitizeName(s: String): String = {
    val cleaned = s.map(c => if (c.isLetterOrDigit || c == '_') c else '_')
    val prefixed = if (cleaned.nonEmpty && cleaned.head.isDigit) s"_$cleaned" else cleaned
    if (prefixed.startsWith("__")) s"gql_$prefixed" else prefixed
  }

  private val forbiddenEnumValues = Set("true", "false", "null")

  /** Sanitize an enum member name for GraphQL: valid identifier + not a forbidden literal. */
  def sanitizeEnumValue(s: String): String = {
    val cleaned = sanitizeName(s)
    if (forbiddenEnumValues.contains(cleaned)) s"${cleaned}_" else cleaned
  }

  /** Flat identifier for a type ref, suitable for embedding in GraphQL type names. */
  def typeRefIdent(ref: TypeRef): String = {
    ref match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        scalarName(id)
      case TypeRef.Scalar(id: TypeId.User) =>
        typeName(id)
      case TypeRef.Constructor(TypeId.Builtins.opt, args) =>
        s"Opt_${typeRefIdent(args.head)}"
      case TypeRef.Constructor(TypeId.Builtins.lst, args) =>
        s"Lst_${typeRefIdent(args.head)}"
      case TypeRef.Constructor(TypeId.Builtins.set, args) =>
        s"Set_${typeRefIdent(args.head)}"
      case TypeRef.Constructor(TypeId.Builtins.map, args) =>
        s"Map_${typeRefIdent(args.head)}_${typeRefIdent(args.tail.head)}"
      case other =>
        s"Unknown_${other.id.name.name}"
    }
  }

  def mapEntryTypeName(keyRef: TypeRef, valRef: TypeRef): String = {
    s"BaboonMapEntry_${typeRefIdent(keyRef)}_${typeRefIdent(valRef)}"
  }

  def collectMapTypes(ref: TypeRef): Set[(TypeRef, TypeRef)] = {
    ref match {
      case TypeRef.Constructor(TypeId.Builtins.map, args) =>
        Set((args.head, args.tail.head)) ++
          collectMapTypes(args.head) ++
          collectMapTypes(args.tail.head)
      case TypeRef.Constructor(_, args) =>
        args.toList.flatMap(collectMapTypes).toSet
      case _ =>
        Set.empty
    }
  }
}
