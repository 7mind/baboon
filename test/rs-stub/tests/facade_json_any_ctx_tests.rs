// NOTE: This integration test references generated runtime and DTO symbols which are produced
// into this stub only by the rs-stub codegen path (rsync + codegen into
// target/test-regular/rs-stub/). Run from the codegen'd copy, not the source tree.
//
// `serde::Serialize` has no room for a codec context, so neither the derive nor the hand-written
// `Serialize` impls (ADTs, wrapped branches) can reach the facade that an `any` field holding a
// UEBA payload needs to transcode itself to JSON. The generated `encode_json(ctx)` resolves the
// `any` slots first — with the per-field kind byte and static fallbacks, which only the field
// site knows — and then hands the value to serde unchanged.
//
// Two properties are pinned here:
//   1. `facade.encode_to_json` transcodes UEBA-form `any` payloads when the context carries a
//      facade, across all six DSL variants plus the opt/lst/map-value nested positions. Before
//      the fix this failed regardless of context, because the dyn adapter discarded `ctx` and
//      called `serde_json::to_value` directly.
//   2. For values that carry no UEBA-form `any`, `encode_json` is byte-identical to the derive's
//      `serde_json::to_value`. This is the drift guard: the encoder deliberately does NOT
//      re-implement the JSON shape (renames, hex bytes, decimal-as-number, timestamps, user
//      map-key adapters, ADT wrapping) — serde still produces the document, so any divergence
//      here means the resolve step corrupted something it should have left alone.
#![allow(dead_code)]

use baboon_rs_stub::any_opaque::{AnyMeta, AnyOpaque, AnyOpaqueUeba};
use baboon_rs_stub::baboon_codecs_facade::BaboonCodecsFacade;
use baboon_rs_stub::baboon_fixture::BaboonRandom;
use baboon_rs_stub::baboon_runtime::{BaboonBinEncode, BaboonCodecContext};
use baboon_rs_stub::my::ok::domain_myok_facade::DomainMyOkFacade;
use baboon_rs_stub::my::ok::holder::Holder;
use baboon_rs_stub::my::ok::holder_fixture::{random_holder, random_holder_json};
use baboon_rs_stub::my::ok::inner::Inner;
use baboon_rs_stub::my::ok::inner_fixture::random_inner;
use std::collections::BTreeMap;
use std::sync::Arc;

const DOMAIN_ID: &str = "my.ok";
const VERSION_STR: &str = "1.0.0";
const INNER_TYPE: &str = "my.ok/:#Inner";

fn inner_bytes() -> Vec<u8> {
    let mut b = Vec::new();
    Inner { x: 42 }
        .encode_ueba(&BaboonCodecContext::Compact, &mut b)
        .expect("encode Inner");
    b
}

fn ueba(kind: u8, domain: Option<&str>, version: Option<&str>, typeid: Option<&str>) -> AnyOpaque {
    let meta = AnyMeta::new(
        kind,
        domain.map(|s| s.to_string()),
        version.map(|s| s.to_string()),
        typeid.map(|s| s.to_string()),
    )
    .expect("AnyMeta");
    AnyOpaque::Ueba(AnyOpaqueUeba::new(meta, inner_bytes()))
}

// Every field carries a UEBA payload whose meta resolves to the registered `Inner` codec — the
// untyped variants (A/B/C) carry the typeid on the wire, the D variants rely on the static
// fallbacks the codec generator emits at the field site.
fn holder_with_ueba_any() -> Holder {
    let mut map = BTreeMap::new();
    map.insert("k1".to_string(), ueba(0x07, Some(DOMAIN_ID), Some(VERSION_STR), Some(INNER_TYPE)));
    Holder {
        f_any: ueba(0x07, Some(DOMAIN_ID), Some(VERSION_STR), Some(INNER_TYPE)),
        f_domain_this: ueba(0x03, None, Some(VERSION_STR), Some(INNER_TYPE)),
        f_domain_current: ueba(0x01, None, None, Some(INNER_TYPE)),
        f_underlying: ueba(0x06, Some(DOMAIN_ID), Some(VERSION_STR), None),
        f_this_underlying: ueba(0x02, None, Some(VERSION_STR), None),
        f_current_underlying: ueba(0x00, None, None, None),
        f_opt: Some(ueba(0x07, Some(DOMAIN_ID), Some(VERSION_STR), Some(INNER_TYPE))),
        f_lst: vec![ueba(0x06, Some(DOMAIN_ID), Some(VERSION_STR), None)],
        f_map_value: map,
    }
}

fn facade_arc() -> Arc<BaboonCodecsFacade> {
    Arc::new(DomainMyOkFacade::new().facade)
}

#[test]
fn encode_to_json_with_facade_ctx_transcodes_ueba_any_payloads() {
    let dom = DomainMyOkFacade::new();
    let ctx = BaboonCodecContext::with_facade(false, facade_arc());

    let json = dom
        .facade
        .encode_to_json(&ctx, &holder_with_ueba_any())
        .expect("encode_to_json must transcode UEBA `any` payloads when the ctx carries a facade");

    // Every `any` slot must have become a JSON envelope carrying the inner document, not bytes.
    let content = json.get("$c").expect("envelope content key");
    for field in [
        "fAny",
        "fDomainThis",
        "fDomainCurrent",
        "fUnderlying",
        "fThisUnderlying",
        "fCurrentUnderlying",
    ] {
        let slot = content.get(field).unwrap_or_else(|| panic!("missing {}", field));
        let inner = slot.get("$c").unwrap_or_else(|| panic!("{} has no $c", field));
        assert_eq!(inner.get("x").and_then(|v| v.as_i64()), Some(42), "{} payload", field);
    }
    // Nested positions: option, list, map value.
    assert!(content.get("fOpt").and_then(|v| v.get("$c")).is_some(), "fOpt transcoded");
    assert!(content.get("fLst").and_then(|v| v.get(0)).and_then(|v| v.get("$c")).is_some(), "fLst transcoded");
    assert!(content.get("fMapValue").and_then(|v| v.get("k1")).and_then(|v| v.get("$c")).is_some(), "fMapValue transcoded");
}

#[test]
fn encode_to_json_without_facade_ctx_still_reports_the_missing_facade() {
    let dom = DomainMyOkFacade::new();
    let err = dom
        .facade
        .encode_to_json(&BaboonCodecContext::Compact, &holder_with_ueba_any())
        .err()
        .expect("a facade-less context cannot transcode a UEBA `any` payload");
    let msg = format!("{}", err);
    assert!(msg.contains("without a facade reference"), "got: {}", msg);
    assert!(msg.contains("with_facade"), "error must point at the workaround; got: {}", msg);
}

#[test]
fn encode_json_rejects_a_payload_whose_kind_contradicts_the_field() {
    let dom = DomainMyOkFacade::new();
    let ctx = BaboonCodecContext::with_facade(false, facade_arc());
    let mut bad = holder_with_ueba_any();
    // fUnderlying is declared D1 (0x06); hand it a variant-A payload.
    bad.f_underlying = ueba(0x07, Some(DOMAIN_ID), Some(VERSION_STR), Some(INNER_TYPE));

    let err = dom.facade.encode_to_json(&ctx, &bad).err().expect("kind mismatch must fail");
    let msg = format!("{}", err);
    assert!(msg.contains("meta-kind"), "got: {}", msg);
}

// ===== drift guard: encode_json must agree with the derive wherever no transcoding happens =====

#[test]
fn encode_json_matches_serde_derive_for_json_form_any() {
    let ctx = BaboonCodecContext::with_facade(false, facade_arc());
    let mut rnd = BaboonRandom::new();
    for _ in 0..32 {
        let value = random_holder_json(&mut rnd);
        let via_encoder = value.encode_json(&ctx).expect("encode_json");
        let via_derive = serde_json::to_value(&value).expect("to_value");
        assert_eq!(via_encoder, via_derive, "encode_json must not reshape a JSON-form value");
    }
}

#[test]
fn encode_json_matches_serde_derive_for_any_free_types() {
    let ctx = BaboonCodecContext::Compact;
    let mut rnd = BaboonRandom::new();
    for _ in 0..32 {
        let value = random_inner(&mut rnd);
        assert_eq!(
            value.encode_json(&ctx).expect("encode_json"),
            serde_json::to_value(&value).expect("to_value"),
            "an any-free type must route straight back to the derive",
        );
    }
}

#[test]
fn encode_json_on_a_ueba_form_value_is_the_only_thing_that_needs_a_facade() {
    // The raw serde path stays ctx-less and must keep refusing UEBA-form payloads: `to_json_value`
    // has no facade to reach, and silently emitting bytes would break cross-language readers.
    let mut rnd = BaboonRandom::new();
    let value = random_holder(&mut rnd);
    assert!(
        serde_json::to_value(&value).is_err(),
        "raw serde must still refuse a UEBA-form `any`",
    );
}
