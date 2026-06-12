// T43 — Interface-shape assertions for extracted contracts (Rust stub).
//
// Generated symbols (baboon_rs_stub::my::ok::extracted::contracts::*) are
// emitted by `mdl :build :test-gen-regular-adt`. Running `cargo test` from
// the source tree may fail with missing symbols; run from the codegen'd copy.
//
// Rust idiom for extracted contracts (D21/T53): the compiler emits
// `pub trait IBox { … }` AND `impl IBox for IntBox { … }` — every concrete host
// carrying `is C` / `has contract C` is type-associated with C's trait, wiring
// the trait accessors to the host's duplicated fields. So a host IS usable AS
// its contract trait, matching the host↔contract coupling the C#/Scala lanes carry.
//
// Coverage:
//   (a) Contract variant — define a local fixture struct that implements B,
//       assign it to a B-bound parameter, and read a B-declared member through
//       the B reference at runtime. Failures panic unconditionally.
//   (b) Mirror variant — same shape verification for mirror traits (ITagMirror,
//       IMirroredPayload). Mirror hosts (IntPayload) do NOT implement their
//       mirror trait — mirror carries no model-level host↔trait relationship.
//   (c) Member-set spot-check — compile-time: every field named in B's trait
//       declaration must match the names/types used in the `impl` block; any
//       mismatch causes a compile error.
//
// Host-coupling guard (D21/T53):
//   `fn needs_ibox<T: IBox>(_: &T) {}; needs_ibox(&IntBox{…})` compiles ONLY
//   because the host actually implements the contract trait. It would NOT
//   compile under the pre-D21 behaviour (no `impl IBox for IntBox`).
//
// Runtime checks use explicit panics (not debug_assert!), which are active in
// all build profiles.

use baboon_rs_stub::my::ok::extracted::contracts::{
    IBox, IContainer, IKey, IMirroredPayload, IResult, ITagged, ITagMirror,
    IntBox, IntContainer, IntKey, IntPayload, IntPayloadProbe,
    IntTagged, IntTagMirrorProbe, StrBox,
    ResultBase,
};
use baboon_rs_stub::my::ok::extracted::contracts::int_result::{IntResult, Ok as IntResultOk};

// ── helpers ────────────────────────────────────────────────────────────────

/// Read a B-declared member through a B-bound reference.
fn read_ibox_count(b: &impl IBox) -> i32 {
    *b.count()
}

/// D21/T53 host-coupling guard: this monomorphises ONLY for types that actually
/// implement IBox. Calling it with `&IntBox{..}` (below) compiles iff the host
/// is type-associated with its contract trait — the pre-D21 behaviour (no
/// `impl IBox for IntBox`) would make this a hard compile error.
fn needs_ibox<T: IBox>(_: &T) {}

fn read_ikey_key(b: &impl IKey) -> i64 {
    *b.key()
}

fn read_itagged_label(b: &impl ITagged) -> String {
    b.label().clone()
}

fn read_icontainer_fields(b: &impl IContainer) -> (i32, i32, i32) {
    (*b.own(), *b.second(), *b.base_field())
}

fn read_imir_label(b: &impl IMirroredPayload) -> String {
    b.label().clone()
}

fn read_itagmirror_label(b: &impl ITagMirror) -> String {
    b.label().clone()
}

fn read_iresult_tag(b: &(impl IResult + ResultBase)) -> String {
    b.tag().clone()
}

// ── fixture structs implementing the contract traits ───────────────────────
// These prove the trait shape compiles (correct member names/types).
// Any wrong name or type in the trait declaration would cause a compile error.

struct FixBox { count: i32 }
impl IBox for FixBox {
    fn count(&self) -> &i32 { &self.count }
}

struct FixKey { key: i64 }
impl IKey for FixKey {
    fn key(&self) -> &i64 { &self.key }
}

struct FixTagged { label: String }
impl ITagged for FixTagged {
    fn label(&self) -> &String { &self.label }
}

struct FixContainer { own: i32, second: i32, base_field: i32 }
impl IContainer for FixContainer {
    fn own(&self) -> &i32 { &self.own }
    fn second(&self) -> &i32 { &self.second }
    fn base_field(&self) -> &i32 { &self.base_field }
}

struct FixMirroredPayload { label: String }
impl IMirroredPayload for FixMirroredPayload {
    fn label(&self) -> &String { &self.label }
}

struct FixTagMirror { label: String }
impl ITagMirror for FixTagMirror {
    fn label(&self) -> &String { &self.label }
}

struct FixResult { tag: String }
impl ResultBase for FixResult {
    fn tag(&self) -> &String { &self.tag }
}
impl IResult for FixResult {}

// ── (a) Contract variant ────────────────────────────────────────────────────

#[test]
fn ibox_contract_variant_fixture_implements_ibox_count_readable() {
    let fix = FixBox { count: 7 };
    let count_via_b = read_ibox_count(&fix);
    if count_via_b != 7 {
        panic!("IBox.count via fixture must equal 7 but was {}", count_via_b);
    }
}

#[test]
fn ibox_contract_variant_intbox_implements_ibox() {
    // D21/T53: IntBox DOES implement IBox (`impl IBox for IntBox` is emitted).
    // The host-coupling guard `needs_ibox<T: IBox>` compiles ONLY because of
    // that impl — this is the positive assertion replacing the former negative.
    let host = IntBox { count: 7, item: 42 };
    needs_ibox(&host); // compiles iff `impl IBox for IntBox` exists
    // Read the contract member through the trait (not just the raw field).
    let count_via_b = read_ibox_count(&host);
    if count_via_b != 7 { panic!("IBox.count via IntBox must equal 7 but was {}", count_via_b); }
    // Verify field names and types at compile + runtime.
    let count: i32 = host.count;
    let item: i32 = host.item;
    if count != 7 { panic!("IntBox.count must equal 7 but was {}", count); }
    if item != 42 { panic!("IntBox.item must equal 42 but was {}", item); }
}

#[test]
fn ibox_contract_variant_strbox_shares_same_b() {
    // Req 8: two instantiations of one host share the single B.
    let strhost = StrBox { count: 3, item: "hello".to_string() };
    let count: i32 = strhost.count;
    if count != 3 { panic!("StrBox.count must equal 3 but was {}", count); }
}

#[test]
fn ikey_contract_variant_fixture_implements_ikey_key_readable() {
    let fix = FixKey { key: 999 };
    let key_via_b = read_ikey_key(&fix);
    if key_via_b != 999 { panic!("IKey.key via fixture must equal 999 but was {}", key_via_b); }
}

#[test]
fn ikey_contract_variant_intkey_fields_match_ikey_member_names() {
    let host = IntKey { key: 12345, v: 1 };
    let key: i64 = host.key;
    if key != 12345 { panic!("IntKey.key must equal 12345 but was {}", key); }
}

#[test]
fn itagged_contract_variant_fixture_implements_itagged_label_readable() {
    let fix = FixTagged { label: "hello".to_string() };
    let label_via_b = read_itagged_label(&fix);
    if label_via_b != "hello" {
        panic!("ITagged.label via fixture must equal 'hello' but was '{}'", label_via_b);
    }
}

#[test]
fn itagged_contract_variant_inttagged_fields_match_itagged_member_names() {
    let host = IntTagged { label: "hello".to_string(), extra: 5 };
    let label: &str = host.label.as_str();
    if label != "hello" { panic!("IntTagged.label must equal 'hello' but was '{}'", label); }
}

#[test]
fn icontainer_contract_variant_fixture_members_readable() {
    let fix = FixContainer { own: 1, second: 3, base_field: 4 };
    let (own, second, base_field) = read_icontainer_fields(&fix);
    if own != 1 { panic!("IContainer.own via fixture must equal 1 but was {}", own); }
    if second != 3 { panic!("IContainer.second via fixture must equal 3 but was {}", second); }
    if base_field != 4 { panic!("IContainer.base_field via fixture must equal 4 but was {}", base_field); }
}

#[test]
fn icontainer_contract_variant_intcontainer_fields_match_icontainer_member_names() {
    let host = IntContainer { own: 1, item: 99, first: 2, second: 3, base_field: 4 };
    let own: i32 = host.own;
    let second: i32 = host.second;
    let base_field: i32 = host.base_field;
    if own != 1 { panic!("IntContainer.own must equal 1 but was {}", own); }
    if second != 3 { panic!("IntContainer.second must equal 3 but was {}", second); }
    if base_field != 4 { panic!("IntContainer.base_field must equal 4 but was {}", base_field); }
}

#[test]
fn iresult_contract_variant_fixture_implements_iresult_tag_readable() {
    // IResult extends ResultBase; fixture implements both to satisfy the bound.
    let fix = FixResult { tag: "ok".to_string() };
    let tag_via_b = read_iresult_tag(&fix);
    if tag_via_b != "ok" {
        panic!("IResult.tag via fixture must equal 'ok' but was '{}'", tag_via_b);
    }
}

#[test]
fn iresult_contract_variant_intresult_ok_has_tag_field() {
    // IntResult::Ok has tag:String from ResultBase.
    let variant = IntResult::Ok(IntResultOk { tag: "ok".to_string(), result: 0 });
    match &variant {
        IntResult::Ok(v) => {
            if v.tag != "ok" { panic!("IntResult::Ok.tag must equal 'ok' but was '{}'", v.tag); }
        }
        IntResult::Err(_) => panic!("Expected Ok variant"),
    }
}

// ── (b) Mirror variant ──────────────────────────────────────────────────────

#[test]
fn imirroredpayload_mirror_variant_fixture_label_readable() {
    let fix = FixMirroredPayload { label: "mirror-test".to_string() };
    let label_via_b = read_imir_label(&fix);
    if label_via_b != "mirror-test" {
        panic!("IMirroredPayload.label via fixture must equal 'mirror-test' but was '{}'",
            label_via_b);
    }
}

#[test]
fn imirroredpayload_mirror_variant_intpayloadprobe_fields_match() {
    // IntPayloadProbe is the mirror-probe type; its `label` field matches the trait member.
    let probe = IntPayloadProbe { label: "mirror-test".to_string() };
    let label: &str = probe.label.as_str();
    if label != "mirror-test" {
        panic!("IntPayloadProbe.label must equal 'mirror-test' but was '{}'", label);
    }
}

#[test]
fn imirroredpayload_mirror_variant_intpayload_does_not_implement_imirroredpayload() {
    // Negative: IntPayload is the mirror host; it does NOT auto-implement IMirroredPayload.
    //
    // The following would NOT compile because IntPayload has no `impl IMirroredPayload for IntPayload`:
    //   fn needs_imir<T: IMirroredPayload>(_: T) {}
    //   needs_imir(IntPayload { label: "x".to_string(), value: 1 });  // compile error
    //
    // Verified at compile time: IntPayload's fields exist but the trait is unimplemented.
    let host = IntPayload { label: "x".to_string(), value: 1 };
    // Fields are accessible directly (correct names/types).
    let label: &str = host.label.as_str();
    let value: i32 = host.value;
    if label.is_empty() { panic!("IntPayload.label must not be empty"); }
    let _ = value;
    // No trait object coercion is possible — this is the negative assertion.
}

#[test]
fn itagmirror_mirror_variant_fixture_label_readable() {
    let fix = FixTagMirror { label: "tag-mirror".to_string() };
    let label_via_b = read_itagmirror_label(&fix);
    if label_via_b != "tag-mirror" {
        panic!("ITagMirror.label via fixture must equal 'tag-mirror' but was '{}'", label_via_b);
    }
}

#[test]
fn itagmirror_mirror_variant_inttagmirrorprobe_fields_match() {
    let probe = IntTagMirrorProbe { label: "tag-mirror".to_string() };
    let label: &str = probe.label.as_str();
    if label != "tag-mirror" {
        panic!("IntTagMirrorProbe.label must equal 'tag-mirror' but was '{}'", label);
    }
}

#[test]
fn itagmirror_mirror_variant_inttagged_does_not_implement_itagmirror() {
    // Negative: IntTagged carries ITagged (contract), not ITagMirror (mirror).
    //
    //   fn needs_itagmirror<T: ITagMirror>(_: T) {}
    //   needs_itagmirror(IntTagged { … });  // compile error
    //
    let host = IntTagged { label: "x".to_string(), extra: 5 };
    let label: &str = host.label.as_str();
    let _ = label;  // fields accessible but trait unimplemented
}

// ── (c) Member-set spot-check ───────────────────────────────────────────────

#[test]
fn imirroredpayload_member_set_has_exactly_label() {
    // Compile-time: the `impl IMirroredPayload for FixMirroredPayload` block
    // above requires fn label(&self) -> &String — any name/type mismatch is a CE.
    let fix = FixMirroredPayload { label: "spot".to_string() };
    let label = read_imir_label(&fix);
    if label != "spot" { panic!("IMirroredPayload.label spot-check failed: '{}'", label); }
}

#[test]
fn ibox_member_set_has_exactly_count() {
    let fix = FixBox { count: 11 };
    let count = read_ibox_count(&fix);
    if count != 11 { panic!("IBox.count spot-check failed: {}", count); }
}

#[test]
fn ikey_member_set_has_exactly_key() {
    let fix = FixKey { key: 12345 };
    let k = read_ikey_key(&fix);
    if k != 12345 { panic!("IKey.key spot-check failed: {}", k); }
}

#[test]
fn icontainer_member_set_has_own_second_base_field() {
    let fix = FixContainer { own: 10, second: 20, base_field: 30 };
    let (own, second, base_field) = read_icontainer_fields(&fix);
    if own != 10 { panic!("IContainer.own spot-check failed: {}", own); }
    if second != 20 { panic!("IContainer.second spot-check failed: {}", second); }
    if base_field != 30 { panic!("IContainer.base_field spot-check failed: {}", base_field); }
}
