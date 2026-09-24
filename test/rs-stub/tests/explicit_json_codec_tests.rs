// NOTE: references generated symbols produced into this stub only by the rs-stub codegen path.
// Run from the codegen'd copy, not the source tree.
//
// The generated per-type round-trip tests can only exercise wire documents that the encoder
// itself produced, so three reader behaviours escape them entirely. All three were the serde
// derive's behaviour before the explicit codecs replaced it, and the explicit decoder has to
// keep them:
//
//   * an ABSENT optional field decodes to None — not the same thing as an explicit null, and
//     getting it wrong would reject valid documents from producers that omit empty fields;
//   * unknown keys are ignored, which is what makes forward-compatible readers possible;
//   * an absent REQUIRED field is an error rather than a default.
//
// `Holder` is used because it is any-bearing, which is the most intricate decode path.
#![allow(dead_code)]

use baboon_rs_stub::any_opaque::{AnyMeta, AnyOpaque, AnyOpaqueJson};
use baboon_rs_stub::baboon_runtime::BaboonCodecContext;
use baboon_rs_stub::my::ok::holder::Holder;

const DOMAIN_ID: &str = "my.ok";
const VERSION_STR: &str = "1.0.0";
const INNER_TYPE: &str = "my.ok/:#Inner";

fn any_json(kind: u8, domain: Option<&str>, version: Option<&str>, typeid: Option<&str>) -> serde_json::Value {
    let mut env = serde_json::Map::new();
    env.insert("$ak".to_string(), serde_json::Value::from(kind));
    if let Some(d) = domain {
        env.insert("$ad".to_string(), serde_json::Value::from(d));
    }
    if let Some(v) = version {
        env.insert("$av".to_string(), serde_json::Value::from(v));
    }
    if let Some(t) = typeid {
        env.insert("$at".to_string(), serde_json::Value::from(t));
    }
    env.insert("$c".to_string(), serde_json::json!({"x": 42}));
    serde_json::Value::Object(env)
}

/// A complete Holder document with every required `any` slot populated.
fn holder_wire() -> serde_json::Map<String, serde_json::Value> {
    let mut m = serde_json::Map::new();
    m.insert("fAny".to_string(), any_json(0x07, Some(DOMAIN_ID), Some(VERSION_STR), Some(INNER_TYPE)));
    m.insert("fDomainThis".to_string(), any_json(0x03, None, Some(VERSION_STR), Some(INNER_TYPE)));
    m.insert("fDomainCurrent".to_string(), any_json(0x01, None, None, Some(INNER_TYPE)));
    m.insert("fUnderlying".to_string(), any_json(0x06, Some(DOMAIN_ID), Some(VERSION_STR), None));
    m.insert("fThisUnderlying".to_string(), any_json(0x02, None, Some(VERSION_STR), None));
    m.insert("fCurrentUnderlying".to_string(), any_json(0x00, None, None, None));
    m.insert("fOpt".to_string(), serde_json::Value::Null);
    m.insert("fLst".to_string(), serde_json::Value::Array(vec![]));
    m.insert("fMapValue".to_string(), serde_json::Value::Object(serde_json::Map::new()));
    m
}

fn decode(wire: serde_json::Value) -> Result<Holder, String> {
    Holder::decode_json(&BaboonCodecContext::Compact, &wire).map_err(|e| format!("{}", e))
}

#[test]
fn absent_optional_field_decodes_to_none() {
    let mut wire = holder_wire();
    wire.remove("fOpt");
    let wire = serde_json::Value::Object(wire);

    let decoded = decode(wire.clone()).expect("an absent optional field must decode, not fail");
    assert!(decoded.f_opt.is_none());
}

#[test]
fn explicit_null_optional_field_decodes_to_none() {
    let wire = serde_json::Value::Object(holder_wire());
    let decoded = decode(wire.clone()).expect("an explicit null optional must decode");
    assert!(decoded.f_opt.is_none());
}

#[test]
fn unknown_keys_are_ignored() {
    let mut wire = holder_wire();
    wire.insert("somethingFromANewerVersion".to_string(), serde_json::json!({"nested": [1, 2, 3]}));
    let wire = serde_json::Value::Object(wire);

    decode(wire.clone()).expect("unknown keys must be ignored, not rejected");
}

#[test]
fn absent_required_field_is_an_error() {
    let mut wire = holder_wire();
    wire.remove("fAny");
    let wire = serde_json::Value::Object(wire);

    let err = decode(wire.clone()).expect_err("an absent required field must fail");
    assert!(err.contains("fAny"), "the error must name the missing field; got: {}", err);
}

#[test]
fn a_non_object_document_is_an_error() {
    let err = decode(serde_json::json!([1, 2, 3])).expect_err("an array is not a Holder");
    assert!(err.contains("expected a JSON object"), "got: {}", err);
}

#[test]
fn an_any_slot_whose_kind_contradicts_the_field_is_rejected() {
    let mut wire = holder_wire();
    // fUnderlying is declared D1 (0x06); hand it a variant-A envelope.
    wire.insert("fUnderlying".to_string(), any_json(0x07, Some(DOMAIN_ID), Some(VERSION_STR), Some(INNER_TYPE)));
    let err = decode(serde_json::Value::Object(wire)).expect_err("kind mismatch must fail");
    assert!(err.contains("does not match field-declared"), "got: {}", err);
}

#[test]
fn decoded_any_slots_come_back_as_the_json_branch() {
    let decoded = decode(serde_json::Value::Object(holder_wire())).expect("decode");
    match &decoded.f_any {
        AnyOpaque::Json(AnyOpaqueJson { meta, json }) => {
            assert_eq!(meta, &AnyMeta::new(0x07, Some(DOMAIN_ID.to_string()), Some(VERSION_STR.to_string()), Some(INNER_TYPE.to_string())).unwrap());
            assert_eq!(json, &serde_json::json!({"x": 42}));
        }
        other => panic!("a JSON payload must decode to the JSON branch, got {:?}", other),
    }
}
