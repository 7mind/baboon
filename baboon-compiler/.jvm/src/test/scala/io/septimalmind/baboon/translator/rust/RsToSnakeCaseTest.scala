package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.translator.rust.RsDefnTranslator.toSnakeCase
import org.scalatest.wordspec.AnyWordSpec

class RsToSnakeCaseTest extends AnyWordSpec {

  "RsDefnTranslator.toSnakeCase" should {

    "snake-case PascalCase identifiers" in {
      assert(toSnakeCase("PetStore") == "pet_store")
      assert(toSnakeCase("HTTPServer") == "http_server")
      assert(toSnakeCase("HTTP_Service") == "http_service")
    }

    "leave snake_case identifiers idempotent" in {
      assert(toSnakeCase("pet_store") == "pet_store")
    }

    "handle single-letter prefix + digit" in {
      assert(toSnakeCase("I1") == "i1")
      assert(toSnakeCase("I2") == "i2")
    }

    "handle all-caps acronyms" in {
      assert(toSnakeCase("IRS") == "irs")
    }

    "insert underscore between digit and following PascalCase word (PR-45-D01)" in {
      // Pre-fix: "Foo2Bar" -> "foo2bar" (digit suppressed boundary).
      // Post-fix: digit treated as word boundary -> "foo2_bar".
      assert(toSnakeCase("Foo2Bar") == "foo2_bar")
      assert(toSnakeCase("I2WithFoo") == "i2_with_foo")
      assert(toSnakeCase("V3HttpClient") == "v3_http_client")
    }

    "escape Rust keyword identifiers via r# raw-identifier prefix" in {
      assert(toSnakeCase("Type") == "r#type")
      assert(toSnakeCase("Match") == "r#match")
    }
  }
}
