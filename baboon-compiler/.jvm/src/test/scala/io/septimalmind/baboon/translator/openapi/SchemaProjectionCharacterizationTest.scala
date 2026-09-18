package io.septimalmind.baboon.translator.openapi

import io.circe.Json
import io.septimalmind.baboon.translator.graphql.GqlTypeTranslator
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.wordspec.AnyWordSpec

class SchemaProjectionCharacterizationTest extends AnyWordSpec {
  private val oas                    = new OasTypeTranslator
  private val gql                    = new GqlTypeTranslator
  private val pkg                    = Pkg(NEList("schema", "test"))
  private val alias                  = TypeId.User(pkg, Owner.Toplevel, TypeName("Alias"))
  private val color                  = TypeId.User(pkg, Owner.Toplevel, TypeName("Color"))
  private def parse(s: String): Json = io.circe.parser.parse(s).fold(throw _, identity)

  "Schema projections" should {
    "preserve scalar formats and nullable collection structure" in {
      assert(parse(oas.scalarSchemaJson(TypeId.Builtins.u64)) == parse("""{"type":"integer","format":"int64","minimum":0}"""))
      val ref = TypeRef.Constructor(TypeId.Builtins.opt, NEList(TypeRef.Constructor(TypeId.Builtins.set, NEList(TypeRef.Scalar(TypeId.Builtins.str)))))
      assert(parse(oas.typeRefSchema(ref)) == parse("""{"oneOf":[{"type":"array","items":{"type":"string"},"uniqueItems":true},{"type":"null"}]}"""))
      assert(gql.fieldTypeStr(ref) == "[String!]")
    }

    "preserve distinct enum and numeric map projections" in {
      val enumMap = TypeRef.Constructor(TypeId.Builtins.map, NEList(TypeRef.Scalar(color), TypeRef.Scalar(TypeId.Builtins.str)))
      val schema  = parse(oas.typeRefSchema(enumMap, Set(color)))
      assert(schema.hcursor.downField("propertyNames").get[String]("$ref").toOption.contains("#/components/schemas/schema_test_Color"))
      val numericMap = TypeRef.Constructor(TypeId.Builtins.map, NEList(TypeRef.Scalar(TypeId.Builtins.i32), TypeRef.Scalar(TypeId.Builtins.str)))
      assert(parse(oas.typeRefSchema(numericMap)).hcursor.get[String]("type").toOption.contains("array"))
      assert(gql.fieldTypeStr(numericMap) == "[BaboonMapEntry_Int_String!]!")
    }

    "resolve foreign aliases recursively without changing unresolved references" in {
      val resolutions = Map(alias -> Some(TypeRef.Scalar(color)))
      val ref         = TypeRef.Constructor(TypeId.Builtins.lst, NEList(TypeRef.Scalar(alias)))
      val expected    = TypeRef.Constructor(TypeId.Builtins.lst, NEList(TypeRef.Scalar(color)))
      assert(oas.resolveTypeRef(ref, resolutions) == expected)
      assert(gql.resolveTypeRef(ref, resolutions) == expected)
      assert(oas.resolveTypeRef(TypeRef.Scalar(color), resolutions) == TypeRef.Scalar(color))
      assert(gql.typeName(color) == oas.schemaName(color))
    }

    "retain the documented permissive any envelope" in {
      val schema = parse(oas.baboonAnySchema)
      assert(schema.hcursor.get[List[String]]("required").toOption.contains(List("$ak", "$c")))
      assert(schema.hcursor.downField("properties").downField("$ak").get[Int]("maximum").toOption.contains(7))
      assert(gql.renderGqlDescription(Docs.empty, "  ").isEmpty)
    }

    "render typed fragments only at the text boundary" in {
      val ref = TypeRef.Constructor(TypeId.Builtins.lst, NEList(TypeRef.Scalar(color)))
      assert(oas.typeRefSchemaValue(ref, Set(color)) == parse(oas.typeRefSchema(ref, Set(color))))
      assert(oas.scalarSchemaValue(TypeId.Builtins.u32) == parse(oas.scalarSchemaJson(TypeId.Builtins.u32)))
      assert(oas.baboonAnySchemaValue == parse(oas.baboonAnySchema))
      assert(gql.descriptionLiteral(List("first", "second"), "  ") == "  \"\"\"\nfirst\nsecond\n  \"\"\"\n")
      assert(gql.descriptionLiteral(List("single"), "  ") == "  \"single\"\n")
    }
  }
}
