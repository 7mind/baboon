// T55 / D22 — host-coupling + boxed-field guard for recursive-contract fixture.
//
// Generated symbols (baboon_rs_stub::recursive::contract::ok::*) are emitted by
// `mdl :build :test-gen-regular-adt`. Running `cargo test` from the source tree
// may fail with missing symbols; run from the codegen'd copy.
//
// Fixture model (recursive-contract-ok/recursive_contract.baboon):
//   - RecNode: self-referential DTO (next: opt[RecNode]) — makes RecNode part of
//     a type loop so isRecursiveTypedef=true and needsBox(RecNode)=true.
//   - Wrapper[T]: templated host with param-free field `node: RecNode` and
//     `has contract IWrapper`.
//   - IWrapper: extracted contract with field `node: RecNode`.
//   - IntWrapper = Wrapper[i32]: the concrete instantiation.
//
// What this file proves:
//   (a) The trait `IWrapper` is emitted with signature
//         fn node(&self) -> &RecNode;
//       (no Box<…> in the return type — D22 fix).
//   (b) `impl IWrapper for IntWrapper` compiles without E0053: the boxed host
//       field `Box<RecNode>` is accessed via `.as_ref()` which returns `&RecNode`,
//       matching the trait signature.
//   (c) A host-coupling guard `fn needs_iwrapper<T: IWrapper>(_: &T) {}`
//       called with `&IntWrapper{…}` compiles iff the impl exists.
//   (d) Reading through the trait accessor at runtime returns the correct value.
//
// Runtime checks use explicit panics (not debug_assert!), active in all profiles.

use baboon_rs_stub::recursive::contract::ok::{IWrapper, IntWrapper, RecNode};

// ── helpers ────────────────────────────────────────────────────────────────

/// D22/T55 host-coupling guard: monomorphises only for types that implement
/// IWrapper.  Calling with `&IntWrapper{…}` compiles iff `impl IWrapper for IntWrapper`
/// was emitted — the pre-D22 behaviour (E0053 from &Box<RecNode> return type in the
/// impl vs &RecNode in the trait) would prevent this compilation.
fn needs_iwrapper<T: IWrapper>(_: &T) {}

/// Read the `node` field through the IWrapper trait bound.
/// The return type `&RecNode` must match the trait declaration exactly.
fn read_node(b: &impl IWrapper) -> &RecNode {
    b.node()
}

// ── fixture struct to verify trait shape ──────────────────────────────────

struct FixWrapper { node: Box<RecNode> }
impl IWrapper for FixWrapper {
    fn node(&self) -> &RecNode { self.node.as_ref() }
}

// ── tests ─────────────────────────────────────────────────────────────────

#[test]
fn iwrapper_fixture_node_value_readable() {
    let fix = FixWrapper {
        node: Box::new(RecNode { value: 42, next: Box::new(None) }),
    };
    let node = read_node(&fix);
    if node.value != 42 {
        panic!("Expected node.value=42 from FixWrapper.node() but got {}", node.value);
    }
}

#[test]
fn intwrapper_implements_iwrapper_host_coupling_guard() {
    // D22/T55: `impl IWrapper for IntWrapper` is emitted by the generator.
    // `needs_iwrapper` would NOT compile without that impl — this is the
    // positive compile-time assertion for the boxed-contract-field fix.
    let wrapper = IntWrapper {
        node: Box::new(RecNode { value: 7, next: Box::new(None) }),
        extra: 99,
    };
    needs_iwrapper(&wrapper); // compiles iff `impl IWrapper for IntWrapper` exists
}

#[test]
fn intwrapper_iwrapper_node_readable_through_trait() {
    // D22/T55: `fn node(&self) -> &RecNode` must match both the trait signature
    // and the impl body `self.node.as_ref()` which returns &RecNode (not
    // &Box<RecNode>).  This test exercises the accessor at runtime.
    let wrapper = IntWrapper {
        node: Box::new(RecNode { value: 123, next: Box::new(None) }),
        extra: 0,
    };
    let node = read_node(&wrapper);
    if node.value != 123 {
        panic!("Expected node.value=123 via IWrapper but got {}", node.value);
    }
}

#[test]
fn intwrapper_field_types_correct() {
    // Verify struct field types at compile time: `node` is `Box<RecNode>`, `extra` is `i32`.
    let inner = RecNode { value: 1, next: Box::new(None) };
    let w = IntWrapper { node: Box::new(inner), extra: 5 };
    let _node: &Box<RecNode> = &w.node;
    let _extra: i32 = w.extra;
}
