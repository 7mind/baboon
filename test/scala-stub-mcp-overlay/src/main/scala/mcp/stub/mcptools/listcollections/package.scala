// T12 MCP overlay shim.
//
// The generated `mcptools_listcollections.in.scala` references `Color_JsonCodec`
// without importing it. `Color_JsonCodec` is defined in package `mcp.stub`; this
// package object re-exports it into the `mcp.stub.mcptools.listcollections`
// sub-package so the generated file compiles without modification.
package mcp.stub.mcptools

package object listcollections {
  val Color_JsonCodec: mcp.stub.Color_JsonCodec.type = mcp.stub.Color_JsonCodec
}
