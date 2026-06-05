package io.septimalmind.baboon.translator.openapi

import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.wordspec.AnyWordSpec

/** D10 / T32 coverage gate for the OAS enum-keyed-map branch.
  *
  * T30 added the enum-key branch to [[OasTypeTranslator.typeRefSchema]] and
  * `OasBaboonTranslator.renderDto` (lines 149-151 of OasTypeTranslator.scala:
  * a map whose key is a user-defined enum is emitted as a string-keyed object
  * with `propertyNames` constrained to the enum component schema, matching the
  * on-wire form that every backend's JSON codec produces).
  *
  * This test pins that branch with a direct unit assertion so the OAS
  * string-keyed-object enum-key path is EXERCISED by `sbt test` (the
  * `test-gen-openapi` / `test-openapi` lanes only run swagger-parser syntax
  * validation, not semantic-golden checks on `propertyNames`).
  *
  * Four cases are covered:
  *   1. `map[Color, str]` — direct enum key → string-keyed object +
  *      `propertyNames` `$ref`-ing the Color component schema.
  *   2. `map[str, str]` — string key → plain additionalProperties object, no
  *      `propertyNames` (regression guard: the enum-key path must not fire for
  *      plain string keys).
  *   3. `map[i32, str]` — non-string non-enum key → entry-array form (regression
  *      guard: the enum-key path must not fire for non-string/non-enum keys).
  *   4. Foreign-rt-resolves-to-enum via Constructor arg-recursion: the production
  *      renderDto:165 path resolves the WHOLE field TypeRef.Constructor(map,
  *      [foreignKey, val]) via resolveTypeRef, which recurses through Constructor
  *      args (OasTypeTranslator.scala:40-42). This case builds that exact
  *      Constructor, calls resolveTypeRef on it, and asserts propertyNames — so
  *      a break of the Constructor arg-recursion WILL fail this case.
  */
class OasEnumKeyMapSchemaTest extends AnyWordSpec {

  private val tr = new OasTypeTranslator()

  // Minimal user-defined type ids ------------------------------------------

  private val testPkg  = Pkg(NEList("testpkg"))
  private val colorId  = TypeId.User(testPkg, Owner.Toplevel, TypeName("Color"))
  private val colorRef = TypeRef.Scalar(colorId)

  // The set of enum keys known to the domain — mirrors `enumKeysOf(domain)`.
  private val enumKeys: Set[TypeId.User] = Set(colorId)

  // Helpers -----------------------------------------------------------------

  private def mapSchema(keyRef: TypeRef, valRef: TypeRef): String =
    tr.typeRefSchema(TypeRef.Constructor(TypeId.Builtins.map, NEList(keyRef, valRef)), enumKeys)

  // Tests -------------------------------------------------------------------

  "OasTypeTranslator.typeRefSchema (enum-keyed map)" should {

    // Case 1: direct enum key ---

    "produce a string-keyed object with propertyNames for map[Color, str] (D6/T30)" in {
      val schema = mapSchema(colorRef, TypeRef.Scalar(TypeId.Builtins.str))

      // Must be a JSON object schema, not an array of entries
      assert(schema.contains(""""type": "object""""), s"expected type:object; got: $schema")

      // Must have additionalProperties for the value type (str)
      assert(schema.contains(""""additionalProperties""""), s"expected additionalProperties; got: $schema")
      assert(schema.contains(""""type": "string""""), s"expected string value schema; got: $schema")

      // Must constrain property names to the Color enum via propertyNames
      assert(schema.contains(""""propertyNames""""), s"expected propertyNames; got: $schema")
      // propertyNames should $ref the Color component schema
      val expectedRef = s""""$$ref": "#/components/schemas/${tr.schemaName(colorId)}""""
      assert(schema.contains(expectedRef), s"expected propertyNames to $$ref Color; got: $schema")
    }

    "produce a string-keyed object with propertyNames for map[Color, i32]" in {
      val schema = mapSchema(colorRef, TypeRef.Scalar(TypeId.Builtins.i32))
      assert(schema.contains(""""type": "object""""))
      assert(schema.contains(""""propertyNames""""))
      assert(schema.contains(tr.schemaName(colorId)))
      // value schema is integer
      assert(schema.contains(""""type": "integer""""))
    }

    // Case 2: string key — must NOT produce propertyNames ---

    "produce a plain additionalProperties object for map[str, str] (regression guard)" in {
      val schema = mapSchema(TypeRef.Scalar(TypeId.Builtins.str), TypeRef.Scalar(TypeId.Builtins.str))
      assert(schema.contains(""""type": "object""""), s"expected type:object; got: $schema")
      assert(schema.contains(""""additionalProperties""""), s"expected additionalProperties; got: $schema")
      assert(!schema.contains(""""propertyNames""""), s"must NOT have propertyNames for str key; got: $schema")
    }

    "produce a plain additionalProperties object for map[uid, str] (regression guard)" in {
      val schema = mapSchema(TypeRef.Scalar(TypeId.Builtins.uid), TypeRef.Scalar(TypeId.Builtins.str))
      assert(schema.contains(""""type": "object""""))
      assert(schema.contains(""""additionalProperties""""))
      assert(!schema.contains(""""propertyNames""""), s"must NOT have propertyNames for uid key; got: $schema")
    }

    // Case 3: non-string non-enum key — must produce entry-array form ---

    "produce an array-of-entries schema for map[i32, str] (regression guard)" in {
      val schema = mapSchema(TypeRef.Scalar(TypeId.Builtins.i32), TypeRef.Scalar(TypeId.Builtins.str))
      assert(!schema.startsWith("""{"type": "object""""), s"must NOT be type:object for i32 key; got: $schema")
      assert(schema.contains(""""type": "array""""), s"expected type:array; got: $schema")
      assert(schema.contains(""""key""""), s"expected entry-array key schema; got: $schema")
      assert(schema.contains(""""value""""), s"expected entry-array value schema; got: $schema")
      assert(!schema.contains(""""propertyNames""""), s"must NOT have propertyNames for i32 key; got: $schema")
    }

    // Case 4: foreign key inside a Constructor — exercises the Constructor arg-recursion branch ---
    // The production path (renderDto:165) resolves the WHOLE field type
    // TypeRef.Constructor(map, [foreignKey, val]) via resolveTypeRef.
    // resolveTypeRef's Constructor branch (OasTypeTranslator.scala:40-42) recurses
    // into each arg, converting foreignRef -> colorRef.
    // If that Constructor arg-recursion were no-op'd, foreignId would NOT be in
    // enumKeys, mapSchema would produce the entry-array form, and the propertyNames
    // assertion below would fail — i.e. this case PINS the invariant.

    "produce string-keyed-object schema when Constructor arg-recursion resolves a foreign key to an enum (D10 renderDto pre-resolution invariant)" in {
      // Simulate a foreign type `ForeignColor` whose rt = Color (an enum).
      val foreignId  = TypeId.User(testPkg, Owner.Toplevel, TypeName("ForeignColor"))
      val foreignRef = TypeRef.Scalar(foreignId)
      val valRef     = TypeRef.Scalar(TypeId.Builtins.str)

      // The foreign resolution map: ForeignColor -> Color
      val foreignResolutions: Map[TypeId.User, Option[TypeRef]] =
        Map(foreignId -> Some(colorRef))

      // Build the WHOLE map field type as the production renderDto path does:
      // TypeRef.Constructor(map, [foreignRef, valRef]).
      val mapConstructor = TypeRef.Constructor(TypeId.Builtins.map, NEList(foreignRef, valRef))

      // resolveTypeRef on the Constructor hits the Constructor arg-recursion branch
      // (OasTypeTranslator.scala:40-42): each arg is recursively resolved, so
      // foreignRef -> colorRef.  A no-op on that branch would leave foreignId
      // unresolved in the key position.
      val resolvedConstructor = tr.resolveTypeRef(mapConstructor, foreignResolutions)

      // Confirm the Constructor arg-recursion actually resolved the key arg.
      resolvedConstructor match {
        case TypeRef.Constructor(_, args) =>
          assert(
            args.head == colorRef,
            s"expected Constructor arg-recursion to resolve foreignRef -> colorRef; got head=${args.head}"
          )
        case other =>
          fail(s"expected a Constructor after resolveTypeRef; got $other")
      }

      // typeRefSchema on the resolved Constructor: the key is now colorRef, which IS
      // in enumKeys, so the result must be the string-keyed-object form with propertyNames.
      val schema = tr.typeRefSchema(resolvedConstructor, enumKeys)
      assert(
        schema.contains(""""propertyNames""""),
        s"expected propertyNames after Constructor arg-recursion resolution; got: $schema"
      )
      assert(
        schema.contains(tr.schemaName(colorId)),
        s"expected Color schema name after resolution; got: $schema"
      )
    }
  }
}
