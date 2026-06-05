package io.septimalmind.baboon.translator.openapi

import io.septimalmind.baboon.typer.model.*

class OasTypeTranslator {

  /** Return the cleaned description text for a `Docs` value, or `None` for
    * empty docs. Combines prefix and suffix with a newline separator when both
    * are present. The caller is responsible for JSON-escaping the result with
    * `escapeJson` before embedding it in a JSON string literal.
    */
  def renderOasDescription(docs: Docs): Option[String] = {
    val prefixText = docs.prefix.map(_.cleaned)
    val suffixText = docs.suffix.map(_.cleaned)
    (prefixText, suffixText) match {
      case (None, None)       => None
      case (Some(p), None)    => Some(p)
      case (None, Some(s))    => Some(s)
      case (Some(p), Some(s)) => Some(s"$p\n$s")
    }
  }

  def foreignTypeResolution(domain: Domain): Map[TypeId.User, Option[TypeRef]] = {
    domain.defs.meta.nodes.values.collect {
      case u: DomainMember.User =>
        u.defn match {
          case f: Typedef.Foreign => Some(f.id -> f.runtimeMapping)
          case _                  => None
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
      case other                 => throw new IllegalArgumentException(s"Unexpected builtin scalar in OpenAPI backend: ${other.name.name}")
    }
  }

  /** Inline JSON Schema fragment for the `AnyOpaque` JSON envelope.
    *
    * Properties are the locked `$ak`/`$ad`/`$av`/`$at`/`$c` keys; `$ak` and `$c`
    * are required, the three meta strings are kind-byte conditional and listed
    * as optional (kind-byte conditional `if`/`then` constraints are intentionally
    * omitted for readability — see the `description` for the kind-byte table).
    */
  val baboonAnySchema: String = {
    """{"type": "object", "title": "BaboonAny", """ +
    """"description": "Opaque any-envelope. JSON serialization of a baboon AnyOpaque value: """ +
    """{\"$ak\":<int>, \"$ad\"?:str, \"$av\"?:str, \"$at\"?:str, \"$c\":<inner>}. """ +
    """$ak kind byte: 0x07=A(any), 0x03=B(any[domain:this]), 0x01=C(any[domain:current]), """ +
    """0x06=D1(any[T]), 0x02=D2(any[domain:this,T]), 0x00=D3(any[domain:current,T]).", """ +
    """"properties": {""" +
    """"$ak": {"type": "integer", "minimum": 0, "maximum": 7}, """ +
    """"$ad": {"type": "string"}, """ +
    """"$av": {"type": "string"}, """ +
    """"$at": {"type": "string"}, """ +
    """"$c": {}""" +
    """}, "required": ["$ak", "$c"]}"""
  }

  /** Inline JSON Schema fragment for a type reference.
    *
    * Returns a JSON string (without surrounding braces or commas) that can be
    * embedded as a property schema or array items schema.
    */
  def typeRefSchema(ref: TypeRef, enumKeys: Set[TypeId.User] = Set.empty): String = {
    ref match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        scalarSchemaJson(id)

      case TypeRef.Scalar(id: TypeId.User) =>
        s"""{"$$ref": "#/components/schemas/${escapeJson(schemaName(id))}"}"""

      case TypeRef.Constructor(TypeId.Builtins.opt, args) =>
        // nullable via oneOf [schema, null] (OpenAPI 3.1 / JSON Schema 2020-12)
        val inner = typeRefSchema(args.head, enumKeys)
        s"""{"oneOf": [$inner, {"type": "null"}]}"""

      case TypeRef.Constructor(TypeId.Builtins.lst, args) =>
        val items = typeRefSchema(args.head, enumKeys)
        s"""{"type": "array", "items": $items}"""

      case TypeRef.Constructor(TypeId.Builtins.set, args) =>
        val items = typeRefSchema(args.head, enumKeys)
        s"""{"type": "array", "items": $items, "uniqueItems": true}"""

      case TypeRef.Constructor(TypeId.Builtins.map, args) =>
        mapSchema(args.head, args.tail.head, enumKeys)
      case _: TypeRef.Any =>
        // OpenAPI / JSON Schema fragment for the locked `AnyOpaque` JSON envelope.
        // Documents the on-wire keys ($ak/$ad/$av/$at/$c) and the kind-byte range
        // (0x00..0x07, see GraphQL `BaboonAny` description for the kind table).
        // Inlined directly rather than `$ref`-ed because OpenAPI emission is
        // schema-only (no shared component registry) and this fragment is small.
        baboonAnySchema
      case other =>
        throw new IllegalArgumentException(s"Unexpected type reference in OpenAPI backend: ${other.id.name.name}")
    }
  }

  /** JSON Schema for a map type.
    *
    * String-keyed maps become `{"type": "object", "additionalProperties": ...}`.
    * ENUM-keyed maps also become string-keyed objects (D6/T30: every backend's
    * JSON codec stringifies an enum map key to its wire-name and emits a
    * string-keyed JSON object, so the schema must declare a string-keyed object
    * — not an entry-array — to match the wire), with `propertyNames` constrained
    * to the enum component. Other non-string-keyed maps become arrays of
    * `{key, value}` entry objects.
    */
  private def mapSchema(keyRef: TypeRef, valRef: TypeRef, enumKeys: Set[TypeId.User]): String = {
    val valSchema = typeRefSchema(valRef, enumKeys)
    keyRef match {
      case TypeRef.Scalar(id: TypeId.User) if enumKeys.contains(id) =>
        val propertyNames = s"""{"$$ref": "#/components/schemas/${escapeJson(schemaName(id))}"}"""
        s"""{"type": "object", "additionalProperties": $valSchema, "propertyNames": $propertyNames}"""
      case _ if isStringKey(keyRef) =>
        s"""{"type": "object", "additionalProperties": $valSchema}"""
      case _ =>
        val keySchema = typeRefSchema(keyRef, enumKeys)
        val entrySchema =
          s"""{"type": "object", "required": ["key", "value"], "properties": {"key": $keySchema, "value": $valSchema}}"""
        s"""{"type": "array", "items": $entrySchema}"""
    }
  }

  /** The enum user-type ids of a domain — passed into `typeRefSchema` so enum
    * map keys can be reconciled to the string-keyed-object wire form (D6/T30).
    */
  def enumKeysOf(domain: Domain): Set[TypeId.User] =
    domain.defs.meta.nodes.values.collect {
      case u: DomainMember.User =>
        u.defn match {
          case e: Typedef.Enum => Some(e.id)
          case _               => None
        }
    }.flatten.toSet

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
      case '"'           => "\\\""
      case '\\'          => "\\\\"
      case '\n'          => "\\n"
      case '\r'          => "\\r"
      case '\t'          => "\\t"
      case c if c < 0x20 => f"\\u${c.toInt}%04x"
      case c             => c.toString
    }
  }
}
