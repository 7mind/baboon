package io.septimalmind.baboon.translator.openapi

import io.circe.{Json, Printer}
import io.septimalmind.baboon.translator.schema.{JsonSchema, SchemaReferences}
import io.septimalmind.baboon.typer.model.*

class OasTypeTranslator {

  /** Return the cleaned description text for a `Docs` value, or `None` for
    * empty docs. Combines prefix and suffix with a newline separator when both
    * are present. The caller embeds this raw text with `Json.fromString`.
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
    SchemaReferences.prepare(domain).resolutions
  }

  def resolveTypeRef(ref: TypeRef, foreignResolutions: Map[TypeId.User, Option[TypeRef]]): TypeRef = {
    SchemaReferences.resolve(ref, foreignResolutions)
  }

  /** JSON Schema representation of a scalar Baboon type.
    *
    * Returns `(type, format, extra)` where `extra` may contain additional
    * properties like `"minimum": 0` for unsigned integers.
    */
  private def scalarSchema(id: TypeId.BuiltinScalar): (String, Option[String], Map[String, Json]) = {
    id match {
      case TypeId.Builtins.bit   => ("boolean", None, Map.empty)
      case TypeId.Builtins.str   => ("string", None, Map.empty)
      case TypeId.Builtins.i08   => ("integer", Some("int32"), Map.empty)
      case TypeId.Builtins.i16   => ("integer", Some("int32"), Map.empty)
      case TypeId.Builtins.i32   => ("integer", Some("int32"), Map.empty)
      case TypeId.Builtins.i64   => ("integer", Some("int64"), Map.empty)
      case TypeId.Builtins.u08   => ("integer", Some("int32"), Map("minimum" -> Json.fromInt(0)))
      case TypeId.Builtins.u16   => ("integer", Some("int32"), Map("minimum" -> Json.fromInt(0)))
      case TypeId.Builtins.u32   => ("integer", Some("int32"), Map("minimum" -> Json.fromInt(0)))
      case TypeId.Builtins.u64   => ("integer", Some("int64"), Map("minimum" -> Json.fromInt(0)))
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
  val baboonAnySchemaValue: Json = Json.obj(
    "type"  -> Json.fromString("object"),
    "title" -> Json.fromString("BaboonAny"),
    "description" -> Json.fromString(
      "Opaque any-envelope. JSON serialization of a baboon AnyOpaque value: " +
      """{"$ak":<int>, "$ad"?:str, "$av"?:str, "$at"?:str, "$c":<inner>}. """ +
      "$ak kind byte: 0x07=A(any), 0x03=B(any[domain:this]), 0x01=C(any[domain:current]), " +
      "0x06=D1(any[T]), 0x02=D2(any[domain:this,T]), 0x00=D3(any[domain:current,T])."
    ),
    "properties" -> Json.obj(
      "$ak" -> Json.obj("type" -> Json.fromString("integer"), "minimum" -> Json.fromInt(0), "maximum" -> Json.fromInt(7)),
      "$ad" -> Json.obj("type" -> Json.fromString("string")),
      "$av" -> Json.obj("type" -> Json.fromString("string")),
      "$at" -> Json.obj("type" -> Json.fromString("string")),
      "$c"  -> Json.obj(),
    ),
    "required" -> Json.arr(Json.fromString("$ak"), Json.fromString("$c")),
  )

  private val fragmentPrinter = Printer.noSpaces.copy(colonRight = " ", objectCommaRight = " ", arrayCommaRight = " ")
  val baboonAnySchema: String = fragmentPrinter.print(baboonAnySchemaValue)

  /** Inline JSON Schema fragment for a type reference.
    *
    * Returns a JSON string (without surrounding braces or commas) that can be
    * embedded as a property schema or array items schema.
    */
  def typeRefSchema(ref: TypeRef, enumKeys: Set[TypeId.User] = Set.empty): String = {
    fragmentPrinter.print(typeRefSchemaValue(ref, enumKeys))
  }

  def typeRefSchemaValue(ref: TypeRef, enumKeys: Set[TypeId.User]): Json = {
    ref match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        scalarSchemaValue(id)

      case TypeRef.Scalar(id: TypeId.User) =>
        componentRef(id)

      case TypeRef.Constructor(TypeId.Builtins.opt, args) =>
        // nullable via oneOf [schema, null] (OpenAPI 3.1 / JSON Schema 2020-12)
        JsonSchema.nullable(typeRefSchemaValue(args.head, enumKeys))

      case TypeRef.Constructor(TypeId.Builtins.lst, args) =>
        JsonSchema.array(typeRefSchemaValue(args.head, enumKeys))

      case TypeRef.Constructor(TypeId.Builtins.set, args) =>
        JsonSchema.uniqueArray(typeRefSchemaValue(args.head, enumKeys))

      case TypeRef.Constructor(TypeId.Builtins.map, args) =>
        mapSchema(args.head, args.tail.head, enumKeys)
      case _: TypeRef.Any =>
        // OpenAPI / JSON Schema fragment for the locked `AnyOpaque` JSON envelope.
        // Documents the on-wire keys ($ak/$ad/$av/$at/$c) and the kind-byte range
        // (0x00..0x07, see GraphQL `BaboonAny` description for the kind table).
        // Inlined directly rather than `$ref`-ed because OpenAPI emission is
        // schema-only (no shared component registry) and this fragment is small.
        baboonAnySchemaValue
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
  private def mapSchema(keyRef: TypeRef, valRef: TypeRef, enumKeys: Set[TypeId.User]): Json = {
    val valSchema = typeRefSchemaValue(valRef, enumKeys)
    keyRef match {
      case TypeRef.Scalar(id: TypeId.User) if enumKeys.contains(id) =>
        JsonSchema.objectMap(valSchema, Some(componentRef(id)))
      case _ if isStringKey(keyRef) =>
        JsonSchema.objectMap(valSchema, None)
      case _ =>
        JsonSchema.entryMap(typeRefSchemaValue(keyRef, enumKeys), valSchema)
    }
  }

  /** The enum user-type ids of a domain — passed into `typeRefSchema` so enum
    * map keys can be reconciled to the string-keyed-object wire form (D6/T30).
    */
  def enumKeysOf(domain: Domain): Set[TypeId.User] =
    SchemaReferences.prepare(domain).enums.keySet

  private def isStringKey(ref: TypeRef): Boolean = {
    ref match {
      case TypeRef.Scalar(TypeId.Builtins.str) => true
      case TypeRef.Scalar(TypeId.Builtins.uid) => true
      case _                                   => false
    }
  }

  def scalarSchemaJson(id: TypeId.BuiltinScalar): String = {
    fragmentPrinter.print(scalarSchemaValue(id))
  }

  def scalarSchemaValue(id: TypeId.BuiltinScalar): Json = {
    val (tpe, fmt, extra) = scalarSchema(id)
    val parts = List("type" -> Json.fromString(tpe)) ++
      fmt.map(f => "format" -> Json.fromString(f)).toList ++
      extra.toList.sortBy(_._1)
    Json.obj(parts*)
  }

  def componentRef(id: TypeId.User): Json = Json.obj("$ref" -> Json.fromString(s"#/components/schemas/${schemaName(id)}"))

  /** Generate the schema name for a user-defined type, following the same
    * conventions as the GraphQL backend: package path + owner path + type name,
    * joined with underscores.
    */
  def schemaName(id: TypeId.User): String = {
    SchemaReferences.name(id)
  }

  def sanitize(s: String): String = {
    SchemaReferences.sanitize(s)
  }

  /** Return the canonical human-readable description of the ADT
    * discriminator-wrapper wire encoding.
    *
    * An ADT value is encoded as a single-key JSON object whose key is the
    * chosen branch's SHORT name and whose value is that branch's field object:
    * `{"<Branch>": { ...branch fields... }}`. Exactly one branch key is present.
    *
    * When `branchShortNames` is non-empty the description also enumerates the
    * known branch names. When the list is empty only the generic encoding
    * sentence is returned.
    *
    * The returned string is RAW (unescaped). Callers are responsible for
    * embedding it via `Json.fromString`.
    */
  def adtWrapperDoc(branchShortNames: List[String]): String = {
    val generic =
      """ADT discriminator-wrapper encoding: encoded as a single-key JSON object """ +
      """{"<Branch>": { ...branch fields... }} where the key is the branch's short name """ +
      """and the value is that branch's field object. Exactly one branch key is present."""
    if (branchShortNames.isEmpty) {
      generic
    } else {
      val enumerated = branchShortNames.mkString(", ")
      s"$generic Known branches: $enumerated."
    }
  }
}
