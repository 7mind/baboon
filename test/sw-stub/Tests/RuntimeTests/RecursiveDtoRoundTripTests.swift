// T23 / D8 regression — recursive value-type DTO codegen (reference indirection).
//
// Before T23 the Swift generator emitted every Baboon DTO as a value-type `struct` with each
// field a bare stored property. A recursive DTO such as the mcp-stub-ok stub's
// `Tree { value: i32; left: opt[Tree]; children: lst[Tree] }` therefore declared
// `let left: Tree?` INLINE inside `struct Tree`, giving the struct infinite size; swiftc
// rejected it with `value type 'Tree' cannot have a stored property that recursively contains
// it`. T23 boxes exactly the self-referential field(s) behind `@BaboonIndirect` (a
// reference indirection) so the struct stays finite-size, while `lst[Tree]` / `set` / `map`
// fields stay bare (they are heap-backed and never trigger the size error).
//
// This suite proves the boxed representation round-trips correctly and preserves value
// semantics across the box. Every assertion uses an UNCONDITIONAL throw (NOT `assert(...)`,
// which Swift compiles away under `-O`, making release-mode asserts vacuous). A negative
// control proves the check is live.

import XCTest
import Foundation
@testable import BaboonRuntime
@testable import McpStub

final class RecursiveDtoRoundTripTests: XCTestCase {

    private enum Check: Error { case failed(String) }

    private func require(_ cond: Bool, _ msg: String) throws {
        if !cond { throw Check.failed(msg) }
    }

    // A non-trivial recursive value: nested `left` chain (depth 3) + `children` lists of subtrees.
    private func sampleTree() -> Tree {
        let leaf = Tree(value: 1, left: nil, children: [])
        let mid  = Tree(value: 2, left: leaf, children: [Tree(value: 21, left: nil, children: [])])
        let deep = Tree(value: 3, left: mid,  children: [leaf, Tree(value: 31, left: leaf, children: [])])
        return Tree(value: 100, left: deep, children: [mid, leaf])
    }

    func testJsonRoundTripPreservesValueEquality() throws {
        let original = sampleTree()
        let ctx = BaboonCodecContext.compact
        let codec = Tree.codecJson as! BaboonJsonCodecBase<Tree>
        let data = try codec.encodeToJsonData(ctx, original)
        let wire = try JSONSerialization.jsonObject(with: data)
        let decoded = try codec.decode(ctx, wire)
        try require(decoded == original, "JSON round-trip must preserve value equality across the @BaboonIndirect box")
        // Walk the boxed `left` chain to its deepest leaf.
        try require(decoded.left?.left?.left?.value == 1, "deep boxed left chain must survive JSON round-trip")
        try require(decoded.children.count == 2, "heap-backed children list must survive JSON round-trip")
    }

    func testUebaRoundTripPreservesValueEquality() throws {
        let original = sampleTree()
        let ctx = BaboonCodecContext.indexed
        let codec = Tree.codecUeba as! BaboonBinCodecBase<Tree>
        let writer = BaboonBinWriter()
        codec.encode(ctx, writer, original)
        let reader = BaboonBinReader(writer.toData())
        let decoded = try codec.decode(ctx, reader)
        try require(decoded == original, "UEBA round-trip must preserve value equality across the @BaboonIndirect box")
        // original.left == deep; deep.left == mid; mid.children.first.value == 21.
        try require(decoded.left?.left?.children.first?.value == 21,
                    "value reached via boxed-left then heap-list must survive UEBA round-trip")
    }

    // Negative control — proves the equality/require machinery is live (not vacuous).
    // Mutating a copy must NOT affect the original: the `@BaboonIndirect` setter allocates a fresh
    // box, preserving value semantics. If equality were broken or the wrapper aliased shared
    // mutable state, these would fail.
    func testValueSemanticsAcrossBox_negativeControl() throws {
        let original = sampleTree()
        var mutated = original
        mutated.left = Tree(value: 999, left: nil, children: [])  // exercises the @BaboonIndirect setter
        try require(mutated != original, "a mutated copy must not equal the original (live equality check)")
        try require(original.left?.value == 3, "mutating the copy must not alias/disturb the original's boxed field")

        // Confirm the assertion harness itself throws on a false predicate — guarantees the
        // positive round-trip assertions above are not vacuously satisfied.
        var threw = false
        do { try require(false, "control") } catch { threw = true }
        try require(threw, "require(false) must throw — proves the checks are not vacuous")
    }
}
