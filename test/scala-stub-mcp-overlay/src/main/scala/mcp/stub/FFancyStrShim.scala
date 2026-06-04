// T12 MCP overlay shim.
//
// The mcp-stub-ok model maps `FFancyStr` as a foreign type to `java.lang.String`.
// The Scala codegen for service input/output method files imports `FFancyStr` from
// the model package (e.g. `import mcp.stub.{... FFancyStr ...}`), but since the
// type is foreign/erased, no `FFancyStr` object exists in the generated `mcp.stub`
// package. This shim adds the missing symbol so the service codec files compile.
//
// This is a test-only fixup. It does NOT affect the MCP server or dispatch logic.
// Provide `FFancyStr` as an object in `mcp.stub` so generated service codec
// files can import it. The field type is `java.lang.String` (the foreign-type
// mapping), but the generated import includes the unqualified `FFancyStr` name.
package mcp.stub

/** Placeholder object for `FFancyStr` (foreign type mapped to `java.lang.String`).
  *
  * The Scala codegen for service input files imports `FFancyStr` from `mcp.stub`
  * even though the field type is `java.lang.String`. This placeholder satisfies
  * the import. It is imported as a module (object) reference — not as a type.
  */
object FFancyStr {
  // Make the object "used" by the importer by providing an identity helper
  // that the generated decode/encode can reference if needed.
  @inline def wrap(s: java.lang.String): java.lang.String = s
  @inline def unwrap(s: java.lang.String): java.lang.String = s
}
