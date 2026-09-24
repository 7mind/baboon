// PR-F (M24) — cross-language malformed map-key error consistency.
//
// Verifies that `Holder::from_json` surfaces a "malformed key: ..." error when the
// JSON map key cannot be parsed back into the id type. The explicit JSON decoder routes
// the parse error through `BaboonCodecError::decoder_failure(format!("malformed key: {}", e))`,
// so the resulting error message starts with "malformed key".
//
// Uses the my.ok.m19.singleid fixture (id ItemId { v: uid }; root data Holder { m: map[ItemId, str] }).
// Generated symbols are produced by mdl :test-gen-regular-adt under target/test-regular/rs-stub/.

use baboon_rs_stub::my::ok::m19::singleid::holder::Holder;

#[test]
fn holder_json_decode_returns_malformed_key_for_bad_map_key() {
    let bad_json = r#"{"m":{"not_a_valid_id":"v"}}"#;
    let err = Holder::from_json(bad_json).expect_err("expected a decode error for malformed map key");
    let msg = match &err {
        baboon_rs_stub::any_opaque::BaboonCodecError::DecoderFailure { message, .. } => message.clone(),
        other => panic!("expected a DecoderFailure but got: {:?}", other),
    };
    assert!(msg.starts_with("malformed key: "),
        "expected message to start with 'malformed key: ' but got: {msg}");
}
