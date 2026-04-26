// NOTE: This integration test references generated runtime symbols (AnyMeta,
// BaboonCodecsFacade, BaboonCodecError, ...) AND generated DTO symbols
// (my::ok::Holder, my::ok::Inner, ...) which are copied/generated into this stub
// only by the rs-stub codegen path (rsync + codegen into
// target/test-regular/rs-stub/). Running `cargo test` directly from the source
// tree may fail with missing symbols; run from the codegen'd copy.
//
// Round-trip and cross-format tests for `any` fields (issue #69 PR 4.3 / M4 close).
// Mirrors C#'s `AnyRoundTripTests` (PR 3.4) and Scala's `AnyRoundTripSpec` (PR 2.4).
// Exercises the `any-ok` fixture's six DSL variants (A=any, B=any[domain:this],
// C=any[domain:current], D1=any[Inner], D2=any[domain:this,Inner], D3=any[domain:current,Inner])
// plus the three nested positions (opt/lst/map-value).
#![allow(dead_code)]

use baboon_rs_stub::any_opaque::{AnyMeta, AnyOpaque, AnyOpaqueJson, AnyOpaqueUeba, BaboonCodecError};
use baboon_rs_stub::baboon_codecs_facade::{
    AbstractBaboonJsonCodecsImpl, AbstractBaboonUebaCodecsImpl, BaboonAnyBinCodec, BaboonAnyJsonCodec,
    BaboonAnyMeta, BaboonCodecsFacade, BaboonDomainVersion, BaboonGeneratedDyn,
};
use baboon_rs_stub::baboon_runtime::{BaboonBinDecode, BaboonBinEncode, BaboonCodecContext};
use baboon_rs_stub::my::ok::holder::Holder;
use baboon_rs_stub::my::ok::inner::Inner;
use std::collections::BTreeMap;
use std::io::{Cursor, Read, Write};
use std::sync::Arc;

const DOMAIN_ID: &str = "my.ok";
const VERSION_STR: &str = "1.0.0";
const INNER_TYPE: &str = "my.ok/:#Inner";

// ===== Inner -> facade adapter ===========================================================
//
// The Rust generator does not (yet) emit per-domain `BaboonCodecsJson`/`BaboonCodecsUeba`
// adapters analogous to C#'s `My.Ok.BaboonCodecsJson.Instance`. The cross-format helpers
// look up codecs by `(domain, version, typeid)` through the facade, so for the round-trip
// tests we wrap `Inner`'s `BaboonBinEncode`/`BaboonBinDecode` and serde derives in
// trait-object adapters and register them under `INNER_TYPE`.

#[derive(Debug, Clone, PartialEq)]
struct InnerDyn(Inner);

impl BaboonGeneratedDyn for InnerDyn {
    fn baboon_domain_version_dyn(&self) -> &str { VERSION_STR }
    fn baboon_domain_identifier_dyn(&self) -> &str { DOMAIN_ID }
    fn baboon_type_identifier_dyn(&self) -> &str { INNER_TYPE }
    fn baboon_same_in_versions_dyn(&self) -> Vec<String> { vec![VERSION_STR.to_string()] }
    fn as_any(&self) -> &dyn std::any::Any { self }
    fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self }
}

struct InnerBinCodec;

impl BaboonAnyBinCodec for InnerBinCodec {
    fn type_identifier(&self) -> &str { INNER_TYPE }

    fn encode_dyn(
        &self,
        ctx: &BaboonCodecContext,
        writer: &mut dyn Write,
        value: &dyn BaboonGeneratedDyn,
    ) -> Result<(), BaboonCodecError> {
        let v = value.as_any().downcast_ref::<InnerDyn>().ok_or_else(|| {
            BaboonCodecError::encoder_failure("InnerBinCodec.encode: value is not InnerDyn")
        })?;
        v.0.encode_ueba(ctx, writer).map_err(|e| {
            BaboonCodecError::encoder_failure(format!("InnerBinCodec.encode: {}", e))
        })
    }

    fn decode_dyn(
        &self,
        ctx: &BaboonCodecContext,
        reader: &mut dyn Read,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let inner = <Inner as BaboonBinDecode>::decode_ueba(ctx, reader).map_err(|e| {
            BaboonCodecError::decoder_failure(format!("InnerBinCodec.decode: {}", e))
        })?;
        Ok(Box::new(InnerDyn(inner)))
    }
}

struct InnerJsonCodec;

impl BaboonAnyJsonCodec for InnerJsonCodec {
    fn type_identifier(&self) -> &str { INNER_TYPE }

    fn encode_json_dyn(
        &self,
        _ctx: &BaboonCodecContext,
        value: &dyn BaboonGeneratedDyn,
    ) -> Result<serde_json::Value, BaboonCodecError> {
        let v = value.as_any().downcast_ref::<InnerDyn>().ok_or_else(|| {
            BaboonCodecError::encoder_failure("InnerJsonCodec.encode: value is not InnerDyn")
        })?;
        serde_json::to_value(&v.0)
            .map_err(|e| BaboonCodecError::encoder_failure(format!("InnerJsonCodec.encode: {}", e)))
    }

    fn decode_json_dyn(
        &self,
        _ctx: &BaboonCodecContext,
        wire: &serde_json::Value,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let inner: Inner = serde_json::from_value(wire.clone()).map_err(|e| {
            BaboonCodecError::decoder_failure(format!("InnerJsonCodec.decode: {}", e))
        })?;
        Ok(Box::new(InnerDyn(inner)))
    }
}

struct MyOkMeta;
impl BaboonAnyMeta for MyOkMeta {
    fn same_in_versions(&self, _type_id: &str) -> Vec<String> {
        vec![VERSION_STR.to_string()]
    }
}

fn fresh_facade() -> BaboonCodecsFacade {
    let f = BaboonCodecsFacade::new();
    let dv = BaboonDomainVersion::new(DOMAIN_ID, VERSION_STR);
    f.register_with_meta(
        dv,
        || {
            let mut t = AbstractBaboonJsonCodecsImpl::new();
            t.register(INNER_TYPE, || Arc::new(InnerJsonCodec) as Arc<dyn BaboonAnyJsonCodec>);
            Arc::new(t)
        },
        || {
            let mut t = AbstractBaboonUebaCodecsImpl::new();
            t.register(INNER_TYPE, || Arc::new(InnerBinCodec) as Arc<dyn BaboonAnyBinCodec>);
            Arc::new(t)
        },
        || Arc::new(MyOkMeta) as Arc<dyn BaboonAnyMeta>,
    );
    f
}

// ===== Meta builders for the six variants ================================================
fn meta_a() -> AnyMeta { AnyMeta::new(0x07, Some(DOMAIN_ID.into()), Some(VERSION_STR.into()), Some("opaque.Type".into())).unwrap() }
fn meta_b() -> AnyMeta { AnyMeta::new(0x03, None,                    Some(VERSION_STR.into()), Some("opaque.Type".into())).unwrap() }
fn meta_c() -> AnyMeta { AnyMeta::new(0x01, None,                    None,                      Some("opaque.Type".into())).unwrap() }
fn meta_d1() -> AnyMeta { AnyMeta::new(0x06, Some(DOMAIN_ID.into()), Some(VERSION_STR.into()), None).unwrap() }
fn meta_d2() -> AnyMeta { AnyMeta::new(0x02, None,                    Some(VERSION_STR.into()), None).unwrap() }
fn meta_d3() -> AnyMeta { AnyMeta::new(0x00, None,                    None,                      None).unwrap() }

// Encode an Inner via the generated UEBA codec — used when constructing AnyOpaqueUeba payloads
// so cross-convert tests have a real Inner to deserialize.
fn inner_to_ueba_bytes(inner: &Inner) -> Vec<u8> {
    let mut buf = Vec::new();
    inner.encode_ueba(&BaboonCodecContext::Compact, &mut buf).expect("encode Inner");
    buf
}

fn inner_to_json(inner: &Inner) -> serde_json::Value {
    serde_json::to_value(inner).expect("encode Inner json")
}

fn sample_inner() -> Inner {
    Inner { x: 42 }
}

// Build a complete Holder with one AnyOpaqueUeba per variant. UEBA round-trips natively.
fn build_ueba_holder() -> Holder {
    let inner_bytes = inner_to_ueba_bytes(&sample_inner());
    let mut map = BTreeMap::new();
    map.insert("k1".to_string(), AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_a(), vec![8])));
    Holder {
        f_any: AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_a(), vec![1, 2, 3])),
        f_domain_this: AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_b(), vec![4, 5])),
        f_domain_current: AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_c(), vec![6])),
        f_underlying: AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_d1(), inner_bytes.clone())),
        f_this_underlying: AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_d2(), inner_bytes.clone())),
        f_current_underlying: AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_d3(), inner_bytes.clone())),
        f_opt: Some(AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_a(), vec![7]))),
        f_lst: vec![AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_d1(), inner_bytes))],
        f_map_value: map,
    }
}

// Build a Holder using AnyOpaqueJson branches everywhere with arbitrary inner JSON content.
// Used as the "all native JSON branch" baseline for JSON round-trip tests.
fn build_json_native_holder() -> Holder {
    let arbitrary = serde_json::json!({"payload": 42});
    let inner_json = inner_to_json(&sample_inner());
    let mut map = BTreeMap::new();
    map.insert("k1".to_string(), AnyOpaque::Json(AnyOpaqueJson::new(meta_a(), arbitrary.clone())));
    Holder {
        f_any: AnyOpaque::Json(AnyOpaqueJson::new(meta_a(), arbitrary.clone())),
        f_domain_this: AnyOpaque::Json(AnyOpaqueJson::new(meta_b(), arbitrary.clone())),
        f_domain_current: AnyOpaque::Json(AnyOpaqueJson::new(meta_c(), arbitrary.clone())),
        f_underlying: AnyOpaque::Json(AnyOpaqueJson::new(meta_d1(), inner_json.clone())),
        f_this_underlying: AnyOpaque::Json(AnyOpaqueJson::new(meta_d2(), inner_json.clone())),
        f_current_underlying: AnyOpaque::Json(AnyOpaqueJson::new(meta_d3(), inner_json.clone())),
        f_opt: Some(AnyOpaque::Json(AnyOpaqueJson::new(meta_a(), arbitrary))),
        f_lst: vec![AnyOpaque::Json(AnyOpaqueJson::new(meta_d1(), inner_json))],
        f_map_value: map,
    }
}

// Build a Holder using AnyOpaqueJson branches with REAL Inner JSON for D variants and
// typeid=INNER_TYPE for A/B/C so cross-convert can resolve Inner via the registered facade.
fn build_json_holder_for_cross_convert() -> Holder {
    let inner_json = inner_to_json(&sample_inner());
    let typed_a = AnyMeta::new(0x07, Some(DOMAIN_ID.into()), Some(VERSION_STR.into()), Some(INNER_TYPE.into())).unwrap();
    let typed_b = AnyMeta::new(0x03, None,                    Some(VERSION_STR.into()), Some(INNER_TYPE.into())).unwrap();
    let typed_c = AnyMeta::new(0x01, None,                    None,                      Some(INNER_TYPE.into())).unwrap();
    let mut map = BTreeMap::new();
    map.insert("k1".to_string(), AnyOpaque::Json(AnyOpaqueJson::new(typed_a.clone(), inner_json.clone())));
    Holder {
        f_any: AnyOpaque::Json(AnyOpaqueJson::new(typed_a.clone(), inner_json.clone())),
        f_domain_this: AnyOpaque::Json(AnyOpaqueJson::new(typed_b, inner_json.clone())),
        f_domain_current: AnyOpaque::Json(AnyOpaqueJson::new(typed_c, inner_json.clone())),
        f_underlying: AnyOpaque::Json(AnyOpaqueJson::new(meta_d1(), inner_json.clone())),
        f_this_underlying: AnyOpaque::Json(AnyOpaqueJson::new(meta_d2(), inner_json.clone())),
        f_current_underlying: AnyOpaque::Json(AnyOpaqueJson::new(meta_d3(), inner_json.clone())),
        f_opt: Some(AnyOpaque::Json(AnyOpaqueJson::new(typed_a, inner_json.clone()))),
        f_lst: vec![AnyOpaque::Json(AnyOpaqueJson::new(meta_d1(), inner_json))],
        f_map_value: map,
    }
}

fn encode_ueba_bytes(value: &Holder, ctx: &BaboonCodecContext) -> Vec<u8> {
    let mut buf = Vec::new();
    value.encode_ueba(ctx, &mut buf).expect("encode Holder UEBA");
    buf
}

fn decode_ueba_bytes(bytes: &[u8]) -> Holder {
    let mut cursor = Cursor::new(bytes);
    <Holder as BaboonBinDecode>::decode_ueba(&BaboonCodecContext::Compact, &mut cursor)
        .expect("decode Holder UEBA")
}

// ===== 1. Per-variant UEBA round-trip ===================================================

#[test]
fn ueba_round_trip_all_six_variants_plus_nested_positions_preserve_content() {
    let original = build_ueba_holder();
    let bytes = encode_ueba_bytes(&original, &BaboonCodecContext::Compact);
    let decoded = decode_ueba_bytes(&bytes);
    assert_eq!(decoded, original);
}

#[test]
fn ueba_round_trip_with_use_indices_true_preserves_content() {
    let original = build_ueba_holder();
    let bytes = encode_ueba_bytes(&original, &BaboonCodecContext::Indexed);
    let decoded = decode_ueba_bytes(&bytes);
    assert_eq!(decoded, original);
}

#[test]
fn ueba_decode_yields_any_opaque_ueba_with_matching_kind_bytes() {
    let original = build_ueba_holder();
    let bytes = encode_ueba_bytes(&original, &BaboonCodecContext::Compact);
    let decoded = decode_ueba_bytes(&bytes);
    assert_eq!(decoded.f_any.meta().kind, 0x07, "f_any variant A");
    assert_eq!(decoded.f_domain_this.meta().kind, 0x03, "f_domain_this variant B");
    assert_eq!(decoded.f_domain_current.meta().kind, 0x01, "f_domain_current variant C");
    assert_eq!(decoded.f_underlying.meta().kind, 0x06, "f_underlying variant D1");
    assert_eq!(decoded.f_this_underlying.meta().kind, 0x02, "f_this_underlying variant D2");
    assert_eq!(decoded.f_current_underlying.meta().kind, 0x00, "f_current_underlying variant D3");
    assert!(matches!(decoded.f_any, AnyOpaque::Ueba(_)), "UEBA decode must yield AnyOpaque::Ueba");
}

// ===== 2. Per-variant JSON round-trip ===================================================

#[test]
fn json_round_trip_all_six_variants_plus_nested_positions_preserve_content() {
    let original = build_json_native_holder();
    let json = serde_json::to_value(&original).expect("encode JSON");
    let decoded: Holder = serde_json::from_value(json).expect("decode JSON");
    assert_eq!(decoded, original);
}

#[test]
fn json_decode_yields_any_opaque_json_with_matching_kind_bytes() {
    let original = build_json_native_holder();
    let json = serde_json::to_value(&original).expect("encode JSON");
    let decoded: Holder = serde_json::from_value(json).expect("decode JSON");
    assert!(matches!(decoded.f_any, AnyOpaque::Json(_)), "JSON decode must yield AnyOpaque::Json");
    assert_eq!(decoded.f_any.meta().kind, 0x07);
    assert_eq!(decoded.f_domain_this.meta().kind, 0x03);
    assert_eq!(decoded.f_domain_current.meta().kind, 0x01);
    assert_eq!(decoded.f_underlying.meta().kind, 0x06);
    assert_eq!(decoded.f_this_underlying.meta().kind, 0x02);
    assert_eq!(decoded.f_current_underlying.meta().kind, 0x00);
}

// ===== 3. Cross-format conversion via facade ============================================

#[test]
fn cross_format_json_holder_to_ueba_decode_round_trip() {
    // build_json_holder_for_cross_convert uses AnyOpaque::Json branches for ALL fields with
    // real Inner JSON; encoding to UEBA forces json_to_ueba_bytes per field. Decoding produces
    // AnyOpaque::Ueba branches. We then re-encode to UEBA without a facade and assert byte
    // equality — proves the cross-converted bytes match a native UEBA encode of the same value.
    let facade = Arc::new(fresh_facade());
    let ctx = BaboonCodecContext::with_facade(false, Arc::clone(&facade));
    let original = build_json_holder_for_cross_convert();
    let bytes = encode_ueba_bytes(&original, &ctx);
    let decoded = decode_ueba_bytes(&bytes);
    let rebytes = encode_ueba_bytes(&decoded, &BaboonCodecContext::Compact);
    assert_eq!(rebytes, bytes, "JSON->UEBA cross-convert produced non-canonical bytes");
}

#[test]
fn cross_format_ueba_holder_to_json_decode_round_trip() {
    // build_ueba_holder uses AnyOpaque::Ueba branches everywhere. Encoding to JSON triggers
    // ueba_to_json for each field via the JSON-path encoder (serde Serialize). For untyped
    // variants A/B/C the wire meta carries typeid; we substitute typeid=INNER_TYPE so the
    // registered Inner codec resolves and the bytes deserialize as Inner. D variants resolve
    // via static fallbacks emitted by codec gen.
    //
    // NOTE: The Rust JSON path is serde-derived on the struct, so the "JSON encoder ctx" is
    // not directly threaded — the Ueba branch's custom Serialize impl errors. Instead we drive
    // the cross-convert through the facade helper directly and assert the per-field result.
    let facade = fresh_facade();
    let inner_bytes = inner_to_ueba_bytes(&sample_inner());

    // f_any — variant A, all wire-meta present.
    let typed_meta_a = AnyMeta::new(0x07, Some(DOMAIN_ID.into()), Some(VERSION_STR.into()), Some(INNER_TYPE.into())).unwrap();
    let json_a = facade
        .ueba_to_json(&typed_meta_a, &inner_bytes, None, None, None)
        .expect("ueba_to_json A");
    assert_eq!(json_a, inner_to_json(&sample_inner()), "A cross-convert must yield Inner JSON");

    // f_underlying — variant D1, statics fill in typeid only.
    let json_d1 = facade
        .ueba_to_json(&meta_d1(), &inner_bytes, None, None, Some(INNER_TYPE))
        .expect("ueba_to_json D1");
    assert_eq!(json_d1, inner_to_json(&sample_inner()), "D1 cross-convert must yield Inner JSON");

    // f_current_underlying — variant D3, statics fill in domain/version/typeid.
    let json_d3 = facade
        .ueba_to_json(&meta_d3(), &inner_bytes, Some(DOMAIN_ID), Some(VERSION_STR), Some(INNER_TYPE))
        .expect("ueba_to_json D3");
    assert_eq!(json_d3, inner_to_json(&sample_inner()), "D3 cross-convert must yield Inner JSON");
}

#[test]
fn cross_format_d3_isolated_field_static_fallbacks_resolve_end_to_end() {
    // PR-06-D01 (Rust analog) regression: D3 has all-None meta on wire; the codec generator
    // emits (current_domain, current_version, underlying_fqid) as static fallbacks. Without
    // these the facade cannot resolve and cross-convert fails.
    let facade = Arc::new(fresh_facade());
    let ctx = BaboonCodecContext::with_facade(false, Arc::clone(&facade));
    let inner_json = inner_to_json(&sample_inner());
    let mut mixed = build_ueba_holder();
    mixed.f_current_underlying = AnyOpaque::Json(AnyOpaqueJson::new(meta_d3(), inner_json));

    // No error on encode means json_to_ueba_bytes succeeded for the D3 field (statics resolved).
    let bytes = encode_ueba_bytes(&mixed, &ctx);
    let decoded = decode_ueba_bytes(&bytes);
    assert_eq!(decoded.f_current_underlying.meta().kind, 0x00);
    let blob = match &decoded.f_current_underlying {
        AnyOpaque::Ueba(u) => u.bytes.clone(),
        _ => panic!("D3 cross-convert must yield AnyOpaque::Ueba"),
    };
    let mut cursor = Cursor::new(&blob);
    let inner = <Inner as BaboonBinDecode>::decode_ueba(&BaboonCodecContext::Compact, &mut cursor)
        .expect("Inner decode from D3 cross-convert payload");
    assert_eq!(inner, sample_inner(), "D3 cross-convert payload must decode as the original Inner");
}

// ===== 4. facade.decode_any end-to-end ===================================================

#[test]
fn decode_any_resolves_ueba_inner_to_typed_inner() {
    let facade = fresh_facade();
    let meta = AnyMeta::new(0x07, Some(DOMAIN_ID.into()), Some(VERSION_STR.into()), Some(INNER_TYPE.into())).unwrap();
    let opaque = AnyOpaque::Ueba(AnyOpaqueUeba::new(meta, inner_to_ueba_bytes(&sample_inner())));
    let resolved = facade.decode_any(&opaque).expect("decode_any (UEBA)");
    let inner_dyn = resolved.as_any().downcast_ref::<InnerDyn>().expect("downcast InnerDyn");
    assert_eq!(inner_dyn.0, sample_inner());
}

#[test]
fn decode_any_resolves_json_inner_to_typed_inner() {
    let facade = fresh_facade();
    let meta = AnyMeta::new(0x07, Some(DOMAIN_ID.into()), Some(VERSION_STR.into()), Some(INNER_TYPE.into())).unwrap();
    let opaque = AnyOpaque::Json(AnyOpaqueJson::new(meta, inner_to_json(&sample_inner())));
    let resolved = facade.decode_any(&opaque).expect("decode_any (JSON)");
    let inner_dyn = resolved.as_any().downcast_ref::<InnerDyn>().expect("downcast InnerDyn");
    assert_eq!(inner_dyn.0, sample_inner());
}

// ===== 5. Forward-compat: trailing meta-extension bytes inside meta-length window ========

#[test]
fn forward_compat_extra_meta_extension_bytes_are_skipped_on_ueba_decode() {
    // Encode a Holder normally, then surgically patch the FIRST any-field's meta-length to
    // claim 5 extra bytes, and splice 5 bytes into the meta block. The decoder must consume
    // the meta, observe the gap (any_meta_len - bytes_read), skip them, and continue parsing.
    let original = build_ueba_holder();
    let bytes = encode_ueba_bytes(&original, &BaboonCodecContext::Compact);

    // Layout of the first any-field on the wire (Compact, use_indices=false):
    // [1 byte header][i32 anyLength][i32 anyMetaLen][... metaBytes ...][... blob ...]
    const HEADER_LEN: usize = 1;
    const ANY_LEN_OFFSET: usize = HEADER_LEN; // 4 bytes
    const ANY_META_LEN_OFFSET: usize = HEADER_LEN + 4; // 4 bytes
    const ANY_META_START: usize = HEADER_LEN + 4 + 4;
    let orig_any_length = read_i32_le(&bytes, ANY_LEN_OFFSET);
    let orig_any_meta_len = read_i32_le(&bytes, ANY_META_LEN_OFFSET);

    let extension = [0x11u8, 0x22, 0x33, 0x44, 0x55];
    let new_any_meta_len = orig_any_meta_len + extension.len() as i32;
    let new_any_length = orig_any_length + extension.len() as i32;

    let orig_meta_slice = &bytes[ANY_META_START..ANY_META_START + orig_any_meta_len as usize];
    let orig_blob_and_rest_start = ANY_META_START + orig_any_meta_len as usize;
    let orig_blob_and_rest = &bytes[orig_blob_and_rest_start..];

    let mut patched = Vec::new();
    patched.push(bytes[0]); // header
    patched.extend_from_slice(&new_any_length.to_le_bytes());
    patched.extend_from_slice(&new_any_meta_len.to_le_bytes());
    patched.extend_from_slice(orig_meta_slice);
    patched.extend_from_slice(&extension);
    patched.extend_from_slice(orig_blob_and_rest);

    let decoded = decode_ueba_bytes(&patched);
    assert_eq!(decoded, original, "forward-compat decode must structurally match original");
}

fn read_i32_le(data: &[u8], offset: usize) -> i32 {
    i32::from_le_bytes([data[offset], data[offset + 1], data[offset + 2], data[offset + 3]])
}

// ===== 6. Fail-fast: missing-facade cross-convert ========================================

#[test]
fn encode_json_any_into_ueba_without_facade_fails_fast() {
    let mut mixed = build_ueba_holder();
    mixed.f_any = AnyOpaque::Json(AnyOpaqueJson::new(meta_a(), serde_json::json!({"x": 1})));
    let mut buf = Vec::new();
    let res = mixed.encode_ueba(&BaboonCodecContext::Compact, &mut buf);
    let err = res.err().expect("expected encode failure without facade");
    let msg = format!("{}", err);
    assert!(msg.contains("facade"), "error must mention 'facade'; got: {}", msg);
    assert!(msg.contains("with_facade"), "error must mention workaround 'with_facade'; got: {}", msg);
}

#[test]
fn encode_ueba_any_into_json_without_facade_fails_fast() {
    // The serde-derive JSON encoder routes through the custom `Serialize` impl which errors
    // on the Ueba branch. Bytes can only become JSON via facade cross-convert.
    let mut mixed = build_json_native_holder();
    mixed.f_any = AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_a(), vec![1, 2]));
    let res = serde_json::to_value(&mixed);
    assert!(res.is_err(), "JSON serialize of AnyOpaque::Ueba must fail");
    let msg = format!("{}", res.err().unwrap());
    // Custom Serialize impl includes the workaround pointer.
    assert!(
        msg.contains("ueba_to_json") || msg.contains("AnyOpaque::Ueba"),
        "error must point to the cross-convert path; got: {}",
        msg
    );
}

// ===== 7. JSON envelope shape lock-in ====================================================

#[test]
fn json_envelope_carries_ak_and_optional_ad_av_at_and_content_key() {
    // Sanity: the JSON envelope produced by the serde Serialize embeds the AnyMeta keys ($ak,
    // $ad?, $av?, $at?) alongside the $c content key. Any change to the envelope that drops
    // one of these would break cross-language interop.
    let original = build_json_native_holder();
    let token = serde_json::to_value(&original).expect("encode JSON");
    let obj = token.as_object().expect("Holder serialises to object");

    // f_any → variant A → all four meta keys + $c present.
    let any_field = obj.get("fAny").expect("fAny present").as_object().expect("fAny is object");
    assert!(any_field.contains_key("$ak"), "$ak must be present in fAny");
    assert!(any_field.contains_key("$ad"), "$ad must be present in fAny");
    assert!(any_field.contains_key("$av"), "$av must be present in fAny");
    assert!(any_field.contains_key("$at"), "$at must be present in fAny");
    assert!(any_field.contains_key("$c"), "$c must be present in fAny");
    assert_eq!(any_field.get("$ak").and_then(|v| v.as_u64()), Some(0x07));

    // f_current_underlying variant D3 → only $ak + $c (kind 0x00, no other meta on wire).
    let d3 = obj
        .get("fCurrentUnderlying")
        .expect("fCurrentUnderlying present")
        .as_object()
        .expect("fCurrentUnderlying is object");
    assert!(d3.contains_key("$ak"));
    assert!(d3.contains_key("$c"));
    assert!(!d3.contains_key("$ad"), "$ad must be absent for D3");
    assert!(!d3.contains_key("$av"), "$av must be absent for D3");
    assert!(!d3.contains_key("$at"), "$at must be absent for D3");
    assert_eq!(d3.get("$ak").and_then(|v| v.as_u64()), Some(0x00));

    // Sanity: the envelope keys appear exactly as documented (regression-proof key list).
    let expected: std::collections::HashSet<&str> = ["$ak", "$ad", "$av", "$at", "$c"].iter().copied().collect();
    let present: std::collections::HashSet<&str> = any_field.keys().map(|k| k.as_str()).collect();
    assert_eq!(present, expected, "fAny envelope must expose exactly $ak/$ad/$av/$at/$c");
}

#[test]
fn json_envelope_key_order_is_ak_first() {
    // PR-11-D05 (preserve_order): keys must serialise in insertion order. $ak is inserted first
    // — assert it appears first in the serialised string.
    let original = build_json_native_holder();
    let s = serde_json::to_string(&original).expect("serialize");
    // Find the start of fAny's object value, then assert "$ak" appears before "$ad"/"$c".
    let any_idx = s.find("\"fAny\":").expect("fAny key present");
    let suffix = &s[any_idx..];
    let ak = suffix.find("\"$ak\"").expect("$ak in fAny");
    let ad = suffix.find("\"$ad\"").expect("$ad in fAny");
    let c = suffix.find("\"$c\"").expect("$c in fAny");
    assert!(ak < ad, "$ak must precede $ad; got: {}", suffix);
    assert!(ad < c, "$ad must precede $c; got: {}", suffix);
}
