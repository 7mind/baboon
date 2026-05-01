// PR-F (M24) — cross-language malformed map-key error consistency.
//
// Verifies that serde_json::from_str surfaces a "malformed key: ..." error when the
// JSON map key cannot be parsed back into the id type. The Rust deserializer routes
// the parse error through `serde::de::Error::custom(format!("malformed key: {}", e))`,
// so the resulting serde_json::Error message contains "malformed key".
//
// Uses the my.ok.m19.singleid fixture (id ItemId { v: uid }; root data Holder { m: map[ItemId, str] }).
// Generated symbols are produced by mdl :test-gen-regular-adt under target/test-regular/rs-stub/.

use baboon_rs_stub::my::ok::m19::singleid::holder::Holder;

#[test]
fn holder_json_decode_returns_malformed_key_for_bad_map_key() {
    let bad_json = r#"{"m":{"not_a_valid_id":"v"}}"#;
    let result: Result<Holder, _> = serde_json::from_str(bad_json);
    let err = result.expect_err("expected serde_json error for malformed map key");
    assert!(matches!(err.classify(), serde_json::error::Category::Data),
        "expected Data error category but got: {:?}", err.classify());
    let msg = err.to_string();
    assert!(msg.starts_with("malformed key: "),
        "expected message to start with 'malformed key: ' but got: {msg}");
}
