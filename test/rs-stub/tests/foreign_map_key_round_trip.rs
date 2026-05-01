// NOTE: This test references generated symbols from the m19-ok/wrapper-around-foreign.baboon
// fixture (my.ok.m19.foreign.{FStr, ItemKey, Holder}) which are copied/generated into this
// stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/rs-stub/). Running `cargo test`
// directly from the source tree will fail with missing symbols; run the
// test suite from the codegen'd copy.
//
// Round-trip test for Custom-foreign map keys (PR-A / PR-65-D01 fix).
// Verifies that serde_json encode → decode round-trips cleanly when the
// map key is a wrapper DTO (ItemKey) whose single field is a Custom-foreign type (FStr)
// that maps to std::string::String in Rust.

use baboon_rs_stub::my::ok::m19::foreign::holder::Holder;
use baboon_rs_stub::my::ok::m19::foreign::item_key::ItemKey;
use std::collections::BTreeMap;

#[test]
fn holder_foreign_map_key_json_round_trip() {
    let mut m = BTreeMap::new();
    m.insert(ItemKey { v: "alpha".to_string() }, "1".to_string());
    m.insert(ItemKey { v: "beta".to_string() }, "2".to_string());
    let original = Holder { m };

    let json = serde_json::to_string(&original)
        .expect("serde_json::to_string should succeed");

    let decoded: Holder = serde_json::from_str(&json)
        .expect("serde_json::from_str should succeed");

    assert_eq!(decoded, original, "round-trip diverged: {:?} vs {:?}", decoded, original);
}

#[test]
fn holder_foreign_map_key_json_round_trip_empty() {
    let original = Holder { m: BTreeMap::new() };

    let json = serde_json::to_string(&original)
        .expect("serde_json::to_string should succeed");

    let decoded: Holder = serde_json::from_str(&json)
        .expect("serde_json::from_str should succeed");

    assert_eq!(decoded, original);
}
