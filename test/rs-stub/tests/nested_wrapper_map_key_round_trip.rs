// NOTE: This test references generated symbols from the
// m19-ok/nested-wrapper-of-wrapper.baboon fixture
// (my.ok.m19.nested.{ItemId, Outer, Holder}) which are copied/generated into
// this stub only by `mdl :build :test-gen-regular-adt` (rsync + codegen into
// target/test-regular/rs-stub/). Running `cargo test` directly from the
// source tree will fail with missing symbols; run the test suite from the
// codegen'd copy.
//
// Regression for [BAB-R03]: the typed-map-key deserialiser walks an unwrap
// chain `parsed.map(|v| Inner { f: v }).map(|v| Outer { f: v })` for
// nested single-field wrapper DTOs used as map keys. Earlier emission used
// `${wrapRs.asName}` which produced an `RsTypeName` leaf carrying only the
// bare name — the import collector keys off `RsType.crate`, so wrappers
// referenced solely through the unwrap chain were not registered, producing
// E0422 ("cannot find struct ... in this scope") in pathological schemas.
// The Holder fixture below uses `map[Outer, str]` where `Outer { v: ItemId }`
// and `ItemId { v: uid }`; a successful JSON round-trip through the
// `outer_as_map_key` adapter exercises the full wrap chain.

use baboon_rs_stub::my::ok::m19::nested::holder::Holder;
use baboon_rs_stub::my::ok::m19::nested::item_id::ItemId;
use baboon_rs_stub::my::ok::m19::nested::outer::Outer;
use std::collections::BTreeMap;
use uuid::Uuid;

#[test]
fn holder_nested_wrapper_map_key_json_round_trip() {
    let mut m = BTreeMap::new();
    m.insert(
        Outer {
            v: ItemId { v: Uuid::parse_str("00000000-0000-0000-0000-000000000001").unwrap() },
        },
        "a".to_string(),
    );
    m.insert(
        Outer {
            v: ItemId { v: Uuid::parse_str("00000000-0000-0000-0000-000000000002").unwrap() },
        },
        "b".to_string(),
    );
    let original = Holder { m };

    let json = serde_json::to_string(&original)
        .expect("serde_json::to_string should succeed");

    let decoded: Holder = serde_json::from_str(&json)
        .expect("serde_json::from_str should succeed");

    assert_eq!(decoded, original, "round-trip diverged: {:?} vs {:?}", decoded, original);
}

#[test]
fn holder_nested_wrapper_map_key_json_round_trip_empty() {
    let original = Holder { m: BTreeMap::new() };

    let json = serde_json::to_string(&original)
        .expect("serde_json::to_string should succeed");

    let decoded: Holder = serde_json::from_str(&json)
        .expect("serde_json::from_str should succeed");

    assert_eq!(decoded, original, "round-trip diverged: {:?} vs {:?}", decoded, original);
}
