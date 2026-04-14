package io.septimalmind.baboon.translator.openapi

import io.septimalmind.baboon.typer.model.*

class OasTypeTranslator {

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

  /** JSON Schema representation of a scalar Baboon type.
    *
    * Returns `(type, format, extra)` where `extra` may contain additional
    * properties like `"minimum": 0` for unsigned integers.
    */
  def scalarSchema(id: TypeId.BuiltinScalar): (String, Option[String], Map[String, String]) = {
    id match {
      case TypeId.Builtins.bit   => ("boolean", None, Map.empty)
      case TypeId.Builtins.str   => ("string", None, Map.empty)
      case TypeId.Builtins.i08   => ("integer", Some("int32"), Map.empty)
      case TypeId.Builtins.i16   => ("integer", Some("int32"), Map.empty)
      case TypeId.Builtins.i32   => ("integer", Some("int32"), Map.empty)
      case TypeId.Builtins.i64   => ("integer", Some("int64"), Map.empty)
      case TypeId.Builtins.u08   => ("integer", Some("int32"), Map("minimum" -> "0"))
      case TypeId.Builtins.u16   => ("integer", Some("int32"), Map("minimum" -> "0"))
      case TypeId.Builtins.u32   => ("integer", Some("int32"), Map("minimum" -> "0"))
      case TypeId.Builtins.u64   => ("integer", Some("int64"), Map("minimum" -> "0"))
      case TypeId.Builtins.f32   => ("number", Some("float"), Map.empty)
      case TypeId.Builtins.f64   => ("number", Some("double"), Map.empty)
      case TypeId.Builtins.f128  => ("string", Some("decimal"), Map.empty)
      case TypeId.Builtins.uid   => ("string", Some("uuid"), Map.empty)
      case TypeId.Builtins.tsu   => ("string", Some("date-time"), Map.empty)
      case TypeId.Builtins.tso   => ("string", Some("date-time"), Map.empty)
      case TypeId.Builtins.bytes => ("string", Some("byte"), Map.empty)
      case other => throw new IllegalArgumentException(s"Unexpected builtin scalar in OpenAPI backend: ${other.name.name}")
    }
  }

  /** Inline JSON Schema fragment for a type reference.
    *
    * Returns a JSON string (without surrounding braces or commas) that can be
    * embedded as a property schema or array items schema.
    */
  def typeRefSchema(ref: TypeRef): String = {
    ref match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        scalarSchemaJson(id)

      case TypeRef.Scalar(id: TypeId.User) =>
        s"""{"$$ref": "#/components/schemas/${escapeJson(schemaName(id))}"}"""

      case TypeRef.Constructor(TypeId.Builtins.opt, args) =>
        // nullable via oneOf [schema, null] (OpenAPI 3.1 / JSON Schema 2020-12)
        val inner = typeRefSchema(args.head)
        s"""{"oneOf": [$inner, {"type": "null"}]}"""

      case TypeRef.Constructor(TypeId.Builtins.lst, args) =>
        val items = typeRefSchema(args.head)
        s"""{"type": "array", "items": $items}"""

      case TypeRef.Constructor(TypeId.Builtins.set, args) =>
        val items = typeRefSchema(args.head)
        s"""{"type": "array", "items": $items, "uniqueItems": true}"""

      case TypeRef.Constructor(TypeId.Builtins.map, args) =>
        mapSchema(args.head, args.tail.head)
      case other =>
        throw new IllegalArgumentException(s"Unexpected type reference in OpenAPI backend: ${other.id.name.name}")
    }
  }

  /** JSON Schema for a map type.
    *
    * String-keyed maps become `{"type": "object", "additionalProperties": ...}`.
    * Non-string-keyed maps become arrays of `{key, value}` entry objects.
    */
  private def mapSchema(keyRef: TypeRef, valRef: TypeRef): String = {
    val valSchema = typeRefSchema(valRef)
    if (isStringKey(keyRef)) {
      s"""{"type": "object", "additionalProperties": $valSchema}"""
    } else {
      val keySchema = typeRefSchema(keyRef)
      val entrySchema =
        s"""{"type": "object", "required": ["key", "value"], "properties": {"key": $keySchema, "value": $valSchema}}"""
      s"""{"type": "array", "items": $entrySchema}"""
    }
  }

  private def isStringKey(ref: TypeRef): Boolean = {
    ref match {
      case TypeRef.Scalar(TypeId.Builtins.str) => true
      case TypeRef.Scalar(TypeId.Builtins.uid) => true
      case _                                   => false
    }
  }

  def scalarSchemaJson(id: TypeId.BuiltinScalar): String = {
    val (tpe, fmt, extra) = scalarSchema(id)
    val parts = List(s""""type": "$tpe"""") ++
      fmt.map(f => s""""format": "$f"""").toList ++
      extra.map { case (k, v) => s""""$k": $v""" }
    s"{${parts.mkString(", ")}}"
  }

  /** Generate the schema name for a user-defined type, following the same
    * conventions as the GraphQL backend: package path + owner path + type name,
    * joined with underscores.
    */
  def schemaName(id: TypeId.User): String = {
    val parts = id.pkg.path.toList ++ id.owner.asPseudoPkg :+ id.name.name
    parts.map(sanitize).mkString("_")
  }

  def sanitize(s: String): String = {
    s.replace("-", "_").replace(".", "_")
  }

  def escapeJson(s: String): String = {
    s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < 0x20 => f"\\u${c.toInt}%04x"
      case c    => c.toString
    }
  }
}
