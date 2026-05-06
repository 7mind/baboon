// NOTE: This integration test references generated runtime symbols (AnyMeta,
// AnyMetaCodec, BaboonCodecsFacade, BaboonCodecError, ...) which are copied into
// this stub only by the rs-stub codegen path (rsync + codegen into
// target/test-regular/rs-stub/). Running `cargo test` directly from the source
// tree may fail with missing symbols; run from the codegen'd copy.

use baboon_rs_stub::any_opaque::{
    any_meta_codec, AnyMeta, AnyOpaque, AnyOpaqueJson, AnyOpaqueUeba, BaboonCodecError,
};
use baboon_rs_stub::baboon_codecs_facade::{
    BaboonAnyBinCodec, BaboonAnyConversion, BaboonAnyConversions, BaboonAnyJsonCodec,
    BaboonCodecsFacade, BaboonGeneratedDyn, BaboonTypeMeta,
};
use baboon_rs_stub::baboon_runtime::BaboonCodecContext;
use std::io::{Cursor, Read, Write};
use std::sync::Arc;

// (kind, domain?, version?, typeid?) for all six locked meta-kind bytes.
fn cases() -> Vec<(u8, Option<String>, Option<String>, Option<String>)> {
    vec![
        (0x07, Some("com.example.dom".into()), Some("1.2.3".into()), Some("MyType".into())), // A
        (0x03, None, Some("1.2.3".into()), Some("MyType".into())),                            // B
        (0x01, None, None, Some("MyType".into())),                                            // C
        (0x06, Some("com.example.dom".into()), Some("1.2.3".into()), None),                   // D1
        (0x02, None, Some("1.2.3".into()), None),                                             // D2
        (0x00, None, None, None),                                                             // D3
    ]
}

fn mk(kind: u8, d: Option<&str>, v: Option<&str>, t: Option<&str>) -> AnyMeta {
    AnyMeta::new(kind, d.map(String::from), v.map(String::from), t.map(String::from)).expect("AnyMeta::new")
}

// ===== AnyMeta construction invariants =====

#[test]
fn any_meta_kind0x07_requires_domain_present() {
    let res = AnyMeta::new(0x07, None, Some("v".into()), Some("t".into()));
    assert!(res.is_err());
}

#[test]
fn any_meta_kind0x03_requires_domain_absent() {
    let res = AnyMeta::new(0x03, Some("d".into()), Some("v".into()), Some("t".into()));
    assert!(res.is_err());
}

#[test]
fn any_meta_kind0x06_requires_typeid_absent() {
    let res = AnyMeta::new(0x06, Some("d".into()), Some("v".into()), Some("t".into()));
    assert!(res.is_err());
}

#[test]
fn any_meta_rejects_reserved_kind_0x04() {
    let res = AnyMeta::new(0x04, Some("d".into()), None, None);
    assert!(res.is_err());
}

#[test]
fn any_meta_rejects_reserved_kind_0x05() {
    let res = AnyMeta::new(0x05, Some("d".into()), None, Some("t".into()));
    assert!(res.is_err());
}

// ===== Binary round-trips =====

#[test]
fn write_bin_read_bin_round_trips_across_all_six_kinds() {
    for (kind, d, v, t) in cases() {
        let meta = AnyMeta::new(kind, d.clone(), v.clone(), t.clone()).expect("ctor");
        let mut buf = Vec::new();
        any_meta_codec::write_bin(&meta, &mut buf).expect("write_bin");
        let mut cursor = Cursor::new(&buf);
        let round = any_meta_codec::read_bin(&mut cursor).expect("read_bin");
        assert_eq!(round, meta, "binary round-trip failed for kind 0x{:x}", kind);
    }
}

#[test]
fn write_bin_kind_0x07_emits_7_bytes() {
    // 1 byte kind + (1 byte ULEB128 length + 1 byte UTF-8) × 3 = 7 bytes.
    let meta = mk(0x07, Some("a"), Some("b"), Some("c"));
    let mut buf = Vec::new();
    any_meta_codec::write_bin(&meta, &mut buf).expect("write_bin");
    assert_eq!(buf.len(), 7);
}

#[test]
fn write_bin_kind_0x00_emits_one_byte() {
    let meta = mk(0x00, None, None, None);
    let mut buf = Vec::new();
    any_meta_codec::write_bin(&meta, &mut buf).expect("write_bin");
    assert_eq!(buf.len(), 1);
}

#[test]
fn write_bin_handles_non_ascii_utf8() {
    let meta = mk(0x07, Some("dömäin"), Some("vér"), Some("Tüpe"));
    let mut buf = Vec::new();
    any_meta_codec::write_bin(&meta, &mut buf).expect("write_bin");
    let mut cursor = Cursor::new(&buf);
    let round = any_meta_codec::read_bin(&mut cursor).expect("read_bin");
    assert_eq!(round, meta);
}

#[test]
fn write_bin_handles_empty_string() {
    let meta = mk(0x07, Some(""), Some(""), Some(""));
    let mut buf = Vec::new();
    any_meta_codec::write_bin(&meta, &mut buf).expect("write_bin");
    let mut cursor = Cursor::new(&buf);
    let round = any_meta_codec::read_bin(&mut cursor).expect("read_bin");
    assert_eq!(round, meta);
}

#[test]
fn write_bin_handles_128_byte_string_uleb128_multibyte_prefix() {
    let s: String = "x".repeat(128);
    let meta = mk(0x07, Some(&s), Some(&s), Some(&s));
    let mut buf = Vec::new();
    any_meta_codec::write_bin(&meta, &mut buf).expect("write_bin");
    let mut cursor = Cursor::new(&buf);
    let round = any_meta_codec::read_bin(&mut cursor).expect("read_bin");
    assert_eq!(round, meta);
}

#[test]
fn read_bin_with_length_reports_bytes_consumed() {
    let meta = mk(0x07, Some("a"), Some("b"), Some("c"));
    let mut buf = Vec::new();
    any_meta_codec::write_bin(&meta, &mut buf).expect("write_bin");
    let mut cursor = Cursor::new(&buf);
    let (round, n) = any_meta_codec::read_bin_with_length(&mut cursor).expect("read_bin_with_length");
    assert_eq!(round, meta);
    assert_eq!(n, buf.len());
}

#[test]
fn read_bin_with_length_tolerates_5_trailing_extension_bytes() {
    // Mirrors the C# AnyMetaCodecTests skip-tolerance pattern: write meta + 5 trailing
    // bytes; read_bin_with_length reports the meta bytes-consumed correctly so the caller
    // can skip the remainder.
    let meta = mk(0x00, None, None, None);
    let mut buf = Vec::new();
    any_meta_codec::write_bin(&meta, &mut buf).expect("write_bin");
    let n_meta = buf.len();
    buf.extend_from_slice(&[0xAA, 0xBB, 0xCC, 0xDD, 0xEE]);
    let mut cursor = Cursor::new(&buf);
    let (round, bytes_read) = any_meta_codec::read_bin_with_length(&mut cursor).expect("read");
    assert_eq!(round, meta);
    assert_eq!(bytes_read, n_meta);
}

// ===== JSON round-trips =====

#[test]
fn write_json_read_json_round_trips_across_all_six_kinds() {
    for (kind, d, v, t) in cases() {
        let meta = AnyMeta::new(kind, d.clone(), v.clone(), t.clone()).expect("ctor");
        let json = any_meta_codec::write_json(&meta);
        let round = any_meta_codec::read_json(&json).expect("read_json");
        assert_eq!(round, meta, "json round-trip failed for kind 0x{:x}", kind);
    }
}

#[test]
fn write_json_always_returns_object() {
    for (kind, d, v, t) in cases() {
        let meta = AnyMeta::new(kind, d.clone(), v.clone(), t.clone()).expect("ctor");
        let json = any_meta_codec::write_json(&meta);
        assert!(json.is_object(), "write_json must always return object; got {:?}", json);
    }
}

#[test]
fn read_json_left_on_missing_required() {
    // kind 0x07 demands $ad/$av/$at; omit $ad → Left.
    let json = serde_json::json!({"$ak": 0x07, "$av": "1.0.0", "$at": "T"});
    assert!(any_meta_codec::read_json(&json).is_err());
}

#[test]
fn read_json_left_on_forbidden_present() {
    // kind 0x00 forbids all three; pass $ad → Left.
    let json = serde_json::json!({"$ak": 0x00, "$ad": "x"});
    assert!(any_meta_codec::read_json(&json).is_err());
}

#[test]
fn read_json_left_on_non_numeric_kind() {
    let json = serde_json::json!({"$ak": "not-a-number"});
    assert!(any_meta_codec::read_json(&json).is_err());
}

// ===== AnyOpaque content equality =====

#[test]
fn any_opaque_ueba_content_equality() {
    let a = AnyOpaqueUeba::new(mk(0x00, None, None, None), vec![1, 2, 3]);
    let b = AnyOpaqueUeba::new(mk(0x00, None, None, None), vec![1, 2, 3]);
    let c = AnyOpaqueUeba::new(mk(0x00, None, None, None), vec![1, 2, 4]);
    assert_eq!(a, b);
    assert_ne!(a, c);
}

#[test]
fn any_opaque_json_content_equality() {
    let a = AnyOpaqueJson::new(mk(0x00, None, None, None), serde_json::json!({"x": 1}));
    let b = AnyOpaqueJson::new(mk(0x00, None, None, None), serde_json::json!({"x": 1}));
    let c = AnyOpaqueJson::new(mk(0x00, None, None, None), serde_json::json!({"x": 2}));
    assert_eq!(a, b);
    assert_ne!(a, c);
}

// ===== Custom Serialize/Deserialize for AnyOpaque =====

#[test]
fn any_opaque_serialize_json_branch_emits_envelope() {
    let payload = serde_json::json!({"foo": 42});
    let opaque = AnyOpaque::Json(AnyOpaqueJson::new(
        mk(0x07, Some("d"), Some("v"), Some("t")),
        payload.clone(),
    ));
    let serialized = serde_json::to_value(&opaque).expect("serialize");
    let obj = serialized.as_object().expect("object");
    assert_eq!(obj.get("$ak"), Some(&serde_json::json!(7)));
    assert_eq!(obj.get("$ad"), Some(&serde_json::json!("d")));
    assert_eq!(obj.get("$av"), Some(&serde_json::json!("v")));
    assert_eq!(obj.get("$at"), Some(&serde_json::json!("t")));
    assert_eq!(obj.get("$c"), Some(&payload));
}

#[test]
fn any_opaque_serialize_ueba_branch_returns_error() {
    // The Ueba branch's bytes can't be JSON-serialized without facade access. The custom
    // Serialize impl errors instead of silently producing wrong wire bytes.
    let opaque = AnyOpaque::Ueba(AnyOpaqueUeba::new(mk(0x00, None, None, None), vec![1, 2, 3]));
    let result = serde_json::to_value(&opaque);
    assert!(result.is_err(), "Ueba branch must not JSON-serialize directly");
}

#[test]
fn any_opaque_deserialize_returns_json_branch() {
    let envelope = serde_json::json!({
        "$ak": 0x07,
        "$ad": "d",
        "$av": "v",
        "$at": "t",
        "$c": {"payload": "value"},
    });
    let parsed: AnyOpaque = serde_json::from_value(envelope.clone()).expect("deserialize");
    match parsed {
        AnyOpaque::Json(j) => {
            assert_eq!(j.meta, mk(0x07, Some("d"), Some("v"), Some("t")));
            assert_eq!(j.json, serde_json::json!({"payload": "value"}));
        }
        AnyOpaque::Ueba(_) => panic!("Deserialize must always return Json branch"),
    }
}

#[test]
fn any_opaque_deserialize_round_trip_json_branch() {
    let payload = serde_json::json!({"nested": {"x": 1}});
    let original = AnyOpaque::Json(AnyOpaqueJson::new(
        mk(0x07, Some("d"), Some("v"), Some("t")),
        payload.clone(),
    ));
    let serialized = serde_json::to_value(&original).expect("serialize");
    let round: AnyOpaque = serde_json::from_value(serialized).expect("deserialize");
    assert_eq!(round, original);
}

#[test]
fn any_opaque_deserialize_left_on_missing_kind() {
    let envelope = serde_json::json!({"$c": {}});
    assert!(serde_json::from_value::<AnyOpaque>(envelope).is_err());
}

#[test]
fn any_opaque_deserialize_left_on_missing_content() {
    let envelope = serde_json::json!({"$ak": 0x00});
    assert!(serde_json::from_value::<AnyOpaque>(envelope).is_err());
}

// ===== BaboonCodecContext + facade plumbing =====

#[test]
fn codec_context_with_facade_exposes_facade() {
    let facade = Arc::new(BaboonCodecsFacade::new());
    let ctx = BaboonCodecContext::with_facade(false, Arc::clone(&facade));
    assert!(ctx.facade().is_some());
    let returned = ctx.facade().unwrap();
    assert!(Arc::ptr_eq(returned, &facade));
}

#[test]
fn codec_context_default_indexed_compact_have_no_facade() {
    assert!(BaboonCodecContext::Default.facade().is_none());
    assert!(BaboonCodecContext::Indexed.facade().is_none());
    assert!(BaboonCodecContext::Compact.facade().is_none());
}

#[test]
fn codec_context_with_facade_use_indices_propagates() {
    let facade = Arc::new(BaboonCodecsFacade::new());
    let ctx_idx = BaboonCodecContext::with_facade(true, Arc::clone(&facade));
    let ctx_no_idx = BaboonCodecContext::with_facade(false, Arc::clone(&facade));
    assert!(ctx_idx.use_indices());
    assert!(!ctx_no_idx.use_indices());
}

// ===== Facade cross-format helpers (without registered codecs) =====

#[test]
fn json_to_ueba_bytes_left_on_incomplete_meta_with_no_static() {
    // kind 0x00 has no domain/version/typeid; without static fallback the synthetic-meta
    // build fails with a typed DecoderFailure listing all three missing slots.
    let facade = BaboonCodecsFacade::new();
    let meta = mk(0x00, None, None, None);
    let result =
        facade.json_to_ueba_bytes(&meta, &serde_json::Value::Null, None, None, None);
    match result {
        Err(BaboonCodecError::DecoderFailure { message, .. }) => {
            assert!(message.contains("domain"));
            assert!(message.contains("version"));
            assert!(message.contains("typeid"));
        }
        other => panic!("expected DecoderFailure, got {:?}", other),
    }
}

#[test]
fn ueba_to_json_left_on_incomplete_meta_with_no_static() {
    let facade = BaboonCodecsFacade::new();
    let meta = mk(0x00, None, None, None);
    let result = facade.ueba_to_json(&meta, &[], None, None, None);
    assert!(matches!(result, Err(BaboonCodecError::DecoderFailure { .. })));
}

#[test]
fn json_to_ueba_bytes_left_on_no_codec_registered() {
    // All meta complete; facade empty; expect CodecNotFound.
    let facade = BaboonCodecsFacade::new();
    let meta = mk(0x07, Some("dom"), Some("1.0.0"), Some("T"));
    let result = facade.json_to_ueba_bytes(&meta, &serde_json::Value::Null, None, None, None);
    assert!(matches!(result, Err(BaboonCodecError::CodecNotFound { .. })));
}

#[test]
fn ueba_to_json_left_on_no_codec_registered() {
    let facade = BaboonCodecsFacade::new();
    let meta = mk(0x07, Some("dom"), Some("1.0.0"), Some("T"));
    let result = facade.ueba_to_json(&meta, &[], None, None, None);
    assert!(matches!(result, Err(BaboonCodecError::CodecNotFound { .. })));
}

#[test]
fn json_to_ueba_bytes_static_fallback_b_resolves() {
    // Variant B: meta has version+typeid only; static_domain fills in. Codec lookup still
    // fails (no codec registered) but with CodecNotFound — *not* the meta-incomplete error,
    // proving the static-fallback merge happened.
    let facade = BaboonCodecsFacade::new();
    let meta = mk(0x03, None, Some("1.0.0"), Some("T"));
    let result =
        facade.json_to_ueba_bytes(&meta, &serde_json::Value::Null, Some("static.dom"), None, None);
    assert!(matches!(result, Err(BaboonCodecError::CodecNotFound { .. })));
}

#[test]
fn json_to_ueba_bytes_static_fallback_d3_resolves() {
    // Variant D3: meta empty (kind 0x00); all three statics fill in.
    let facade = BaboonCodecsFacade::new();
    let meta = mk(0x00, None, None, None);
    let result = facade.json_to_ueba_bytes(
        &meta,
        &serde_json::Value::Null,
        Some("static.dom"),
        Some("1.0.0"),
        Some("T"),
    );
    assert!(matches!(result, Err(BaboonCodecError::CodecNotFound { .. })));
}

#[test]
fn ueba_to_json_static_fallback_d3_resolves() {
    let facade = BaboonCodecsFacade::new();
    let meta = mk(0x00, None, None, None);
    let result = facade.ueba_to_json(&meta, &[], Some("static.dom"), Some("1.0.0"), Some("T"));
    assert!(matches!(result, Err(BaboonCodecError::CodecNotFound { .. })));
}

#[test]
fn cross_format_wire_meta_wins_over_static() {
    // Wire meta: kind 0x07, domain=wire.dom; static_domain=static.dom. The wire value should
    // win; the resolved domain in the error message must be "wire.dom".
    let facade = BaboonCodecsFacade::new();
    let meta = mk(0x07, Some("wire.dom"), Some("1.0.0"), Some("T"));
    let result =
        facade.json_to_ueba_bytes(&meta, &serde_json::Value::Null, Some("static.dom"), None, None);
    match result {
        Err(BaboonCodecError::CodecNotFound { message }) => {
            assert!(message.contains("wire.dom"), "expected wire override; message: {}", message);
            assert!(!message.contains("static.dom"), "static must lose; message: {}", message);
        }
        other => panic!("expected CodecNotFound, got {:?}", other),
    }
}

// ===== decode_any preserves Left =====

#[test]
fn decode_any_left_on_incomplete_meta() {
    let facade = BaboonCodecsFacade::new();
    let opaque = AnyOpaque::Ueba(AnyOpaqueUeba::new(mk(0x00, None, None, None), vec![]));
    assert!(matches!(facade.decode_any(&opaque), Err(BaboonCodecError::DecoderFailure { .. })));
}

#[test]
fn decode_any_left_on_no_codec_registered() {
    let facade = BaboonCodecsFacade::new();
    let opaque = AnyOpaque::Ueba(AnyOpaqueUeba::new(
        mk(0x07, Some("dom"), Some("1.0.0"), Some("T")),
        vec![],
    ));
    assert!(matches!(facade.decode_any(&opaque), Err(BaboonCodecError::CodecNotFound { .. })));
}

// ===== Single-version-domain getCodec regression (PR-07-D02) =====
//
// This test exercises the resolver path used for non-exact lookups when min == max == model.
// Without the PR-07-D02 fix, this would return "Unsupported domain version" rather than
// CodecNotFound (= reached the codec-table lookup, which is empty here).

#[test]
fn single_version_domain_get_codec_regression() {
    use baboon_rs_stub::baboon_codecs_facade::{
        AbstractBaboonJsonCodecsImpl, AbstractBaboonUebaCodecsImpl, BaboonAnyMeta,
        BaboonDomainVersion,
    };

    struct EmptyMeta;
    impl BaboonAnyMeta for EmptyMeta {
        fn same_in_versions(&self, _t: &str) -> Vec<String> {
            Vec::new()
        }
    }

    let facade = BaboonCodecsFacade::new();
    let dv = BaboonDomainVersion::new("my.ok", "1.0.0");
    facade.register_with_meta(
        dv.clone(),
        || Arc::new(AbstractBaboonJsonCodecsImpl::new()),
        || Arc::new(AbstractBaboonUebaCodecsImpl::new()),
        || Arc::new(EmptyMeta) as Arc<dyn BaboonAnyMeta>,
    );

    // Variant A meta points at the only registered version, exact=false (json_to_ueba_bytes
    // path). Without the !exact && model == max arm, this would yield "Unsupported domain
    // version" rather than the expected codec-table CodecNotFound.
    let meta = mk(0x07, Some("my.ok"), Some("1.0.0"), Some("Inner"));
    let result = facade.json_to_ueba_bytes(&meta, &serde_json::Value::Null, None, None, None);
    match result {
        Err(BaboonCodecError::CodecNotFound { message }) => {
            assert!(
                message.contains("Inner") || message.contains("No codec found"),
                "expected codec-table miss for Inner; got: {}",
                message
            );
            assert!(
                !message.contains("Unsupported domain version"),
                "PR-07-D02 regression: single-version domain fell through to Unsupported. \
                 message: {}",
                message
            );
        }
        other => panic!("expected CodecNotFound, got {:?}", other),
    }
}

// ===== PR-11 round-2 regression tests (D01–D06) =====
//
// Below: a minimal fake `Sample` value type implementing `BaboonGeneratedDyn` plus a fake
// codec pair so we can exercise encode/decode/convert end-to-end without the codegen.
// Mirrors what PR 4.2's emitter will produce.

#[derive(Debug, Clone, PartialEq)]
struct SampleV1 {
    payload: i32,
}

impl BaboonGeneratedDyn for SampleV1 {
    fn baboon_domain_version_dyn(&self) -> &str { "1.0.0" }
    fn baboon_domain_identifier_dyn(&self) -> &str { "my.dom" }
    fn baboon_type_identifier_dyn(&self) -> &str { "Sample" }
    fn baboon_same_in_versions_dyn(&self) -> Vec<String> { vec!["1.0.0".to_string()] }
    fn as_any(&self) -> &dyn std::any::Any { self }
    fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self }
}

#[derive(Debug, Clone, PartialEq)]
struct SampleV2 {
    payload: i32,
}

impl BaboonGeneratedDyn for SampleV2 {
    fn baboon_domain_version_dyn(&self) -> &str { "2.0.0" }
    fn baboon_domain_identifier_dyn(&self) -> &str { "my.dom" }
    fn baboon_type_identifier_dyn(&self) -> &str { "Sample" }
    // Min-compat distinct from current version: this exercises the D04 fix (was previously
    // collapsed to current version, suppressing $uv emission and has-min-compat=0 in binary).
    fn baboon_same_in_versions_dyn(&self) -> Vec<String> { vec!["1.0.0".to_string(), "2.0.0".to_string()] }
    fn as_any(&self) -> &dyn std::any::Any { self }
    fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self }
}

struct SampleBinCodec {
    type_id: String,
    version: String,
}

impl BaboonAnyBinCodec for SampleBinCodec {
    fn type_identifier(&self) -> &str { &self.type_id }
    fn encode_dyn(
        &self,
        _ctx: &BaboonCodecContext,
        writer: &mut dyn Write,
        value: &dyn BaboonGeneratedDyn,
    ) -> Result<(), BaboonCodecError> {
        // A real codec would dispatch on the concrete type; test fake just downcasts.
        let v = if let Some(s) = value.as_any().downcast_ref::<SampleV1>() {
            s.payload
        } else if let Some(s) = value.as_any().downcast_ref::<SampleV2>() {
            s.payload
        } else {
            return Err(BaboonCodecError::encoder_failure("unknown type"));
        };
        writer.write_all(&v.to_le_bytes()).map_err(|e| {
            BaboonCodecError::encoder_failure(format!("write payload: {e}"))
        })?;
        Ok(())
    }
    fn decode_dyn(
        &self,
        _ctx: &BaboonCodecContext,
        reader: &mut dyn Read,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let mut buf = [0u8; 4];
        reader.read_exact(&mut buf).map_err(|e| {
            BaboonCodecError::decoder_failure(format!("read payload: {e}"))
        })?;
        let payload = i32::from_le_bytes(buf);
        if self.version == "1.0.0" {
            Ok(Box::new(SampleV1 { payload }))
        } else {
            Ok(Box::new(SampleV2 { payload }))
        }
    }
}

struct SampleJsonCodec {
    type_id: String,
    version: String,
}

impl BaboonAnyJsonCodec for SampleJsonCodec {
    fn type_identifier(&self) -> &str { &self.type_id }
    fn encode_json_dyn(
        &self,
        _ctx: &BaboonCodecContext,
        value: &dyn BaboonGeneratedDyn,
    ) -> Result<serde_json::Value, BaboonCodecError> {
        let v = if let Some(s) = value.as_any().downcast_ref::<SampleV1>() {
            s.payload
        } else if let Some(s) = value.as_any().downcast_ref::<SampleV2>() {
            s.payload
        } else {
            return Err(BaboonCodecError::encoder_failure("unknown type"));
        };
        Ok(serde_json::json!({ "payload": v }))
    }
    fn decode_json_dyn(
        &self,
        _ctx: &BaboonCodecContext,
        wire: &serde_json::Value,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let payload = wire
            .get("payload")
            .and_then(|v| v.as_i64())
            .ok_or_else(|| BaboonCodecError::decoder_failure("missing payload"))?
            as i32;
        if self.version == "1.0.0" {
            Ok(Box::new(SampleV1 { payload }))
        } else {
            Ok(Box::new(SampleV2 { payload }))
        }
    }
}

fn register_sample_v1(facade: &BaboonCodecsFacade) {
    use baboon_rs_stub::baboon_codecs_facade::{
        AbstractBaboonJsonCodecsImpl, AbstractBaboonUebaCodecsImpl, BaboonAnyMeta,
        BaboonDomainVersion,
    };
    struct M;
    impl BaboonAnyMeta for M {
        fn same_in_versions(&self, _: &str) -> Vec<String> { vec!["1.0.0".to_string()] }
    }
    let dv = BaboonDomainVersion::new("my.dom", "1.0.0");
    facade.register_with_meta(
        dv,
        || {
            let mut t = AbstractBaboonJsonCodecsImpl::new();
            t.register("Sample", || {
                Arc::new(SampleJsonCodec { type_id: "Sample".into(), version: "1.0.0".into() })
                    as Arc<dyn BaboonAnyJsonCodec>
            });
            Arc::new(t)
        },
        || {
            let mut t = AbstractBaboonUebaCodecsImpl::new();
            t.register("Sample", || {
                Arc::new(SampleBinCodec {
                    type_id: "Sample".into(),
                    version: "1.0.0".into(),
                }) as Arc<dyn BaboonAnyBinCodec>
            });
            Arc::new(t)
        },
        || Arc::new(M) as Arc<dyn BaboonAnyMeta>,
    );
}

fn register_sample_v2(facade: &BaboonCodecsFacade) {
    use baboon_rs_stub::baboon_codecs_facade::{
        AbstractBaboonJsonCodecsImpl, AbstractBaboonUebaCodecsImpl, BaboonAnyMeta,
        BaboonDomainVersion,
    };
    struct M;
    impl BaboonAnyMeta for M {
        fn same_in_versions(&self, _: &str) -> Vec<String> { vec!["1.0.0".to_string(), "2.0.0".to_string()] }
    }
    let dv = BaboonDomainVersion::new("my.dom", "2.0.0");
    facade.register_with_meta(
        dv,
        || {
            let mut t = AbstractBaboonJsonCodecsImpl::new();
            t.register("Sample", || {
                Arc::new(SampleJsonCodec { type_id: "Sample".into(), version: "2.0.0".into() })
                    as Arc<dyn BaboonAnyJsonCodec>
            });
            Arc::new(t)
        },
        || {
            let mut t = AbstractBaboonUebaCodecsImpl::new();
            t.register("Sample", || {
                Arc::new(SampleBinCodec {
                    type_id: "Sample".into(),
                    version: "2.0.0".into(),
                }) as Arc<dyn BaboonAnyBinCodec>
            });
            Arc::new(t)
        },
        || Arc::new(M) as Arc<dyn BaboonAnyMeta>,
    );
}

// ----- D01: encode_to_bin writes the type-meta wire prefix -----

#[test]
fn pr11_d01_encode_to_bin_writes_type_meta_prefix() {
    let facade = BaboonCodecsFacade::new();
    register_sample_v1(&facade);
    let value = SampleV1 { payload: 42 };
    let bytes = facade
        .encode_to_bin(&BaboonCodecContext::Compact, &value)
        .expect("encode_to_bin");

    // First byte must be META_VERSION_1 (= 1). Without the D01 fix the buffer started
    // with the raw payload (0x2A 0x00 0x00 0x00).
    assert_eq!(bytes[0], BaboonTypeMeta::META_VERSION_1, "first byte must be meta-version");

    // Round-trip: read the meta back and verify identity.
    let mut cursor = Cursor::new(&bytes);
    let decoded = facade
        .decode_from_bin(&mut cursor)
        .expect("decode_from_bin (D01 + D02)");
    assert_eq!(decoded.baboon_type_identifier_dyn(), "Sample");
    let s = decoded.as_any().downcast_ref::<SampleV1>().expect("downcast SampleV1");
    assert_eq!(s.payload, 42);
}

// ----- D02: decode_from_bin / decode_from_json round-trip -----

#[test]
fn pr11_d02_decode_from_bin_round_trip() {
    let facade = BaboonCodecsFacade::new();
    register_sample_v1(&facade);
    let value = SampleV1 { payload: 7 };
    let bytes = facade.encode_to_bin(&BaboonCodecContext::Compact, &value).expect("encode");
    let decoded = facade.decode_from_bin_bytes(&bytes).expect("decode_from_bin_bytes");
    let s = decoded.as_any().downcast_ref::<SampleV1>().expect("downcast");
    assert_eq!(s.payload, 7);
}

#[test]
fn pr11_d02_decode_from_json_round_trip() {
    let facade = BaboonCodecsFacade::new();
    register_sample_v1(&facade);
    let value = SampleV1 { payload: 99 };
    let json = facade.encode_to_json(&value).expect("encode_to_json");
    let decoded = facade
        .decode_from_json(&json)
        .expect("decode_from_json")
        .expect("Some(value) for envelope");
    let s = decoded.as_any().downcast_ref::<SampleV1>().expect("downcast");
    assert_eq!(s.payload, 99);
}

#[test]
fn pr11_d02_decode_from_json_returns_none_for_non_envelope() {
    let facade = BaboonCodecsFacade::new();
    register_sample_v1(&facade);
    // Mirrors C# semantics: a non-envelope JSON yields Right(None), not an Err.
    let result = facade.decode_from_json(&serde_json::json!({"random": "object"}));
    match result {
        Ok(None) => {}
        Ok(Some(_)) => panic!("expected Ok(None), got Ok(Some(_))"),
        Err(e) => panic!("expected Ok(None), got Err({})", e),
    }
}

#[test]
fn pr11_d02_decode_from_json_str_works() {
    let facade = BaboonCodecsFacade::new();
    register_sample_v1(&facade);
    let value = SampleV1 { payload: 5 };
    let json = facade.encode_to_json(&value).expect("encode_to_json");
    let s = serde_json::to_string(&json).unwrap();
    let decoded = facade
        .decode_from_json_str(&s)
        .expect("decode_from_json_str")
        .expect("Some(value)");
    let v = decoded.as_any().downcast_ref::<SampleV1>().expect("downcast");
    assert_eq!(v.payload, 5);
}

// ----- D03: convert<TFrom, TTo> v1 -> v2 -----

struct SampleConversions;

struct V1ToV2;

impl BaboonAnyConversion for V1ToV2 {
    fn type_from(&self) -> std::any::TypeId { std::any::TypeId::of::<SampleV1>() }
    fn version_from(&self) -> &str { "1.0.0" }
    fn version_to(&self) -> &str { "2.0.0" }
    fn type_id(&self) -> &str { "Sample" }
}

impl BaboonAnyConversions for SampleConversions {
    fn find_conversions(
        &self,
        _value: &dyn BaboonGeneratedDyn,
    ) -> Vec<Arc<dyn BaboonAnyConversion>> {
        vec![Arc::new(V1ToV2) as Arc<dyn BaboonAnyConversion>]
    }
    fn convert(
        &self,
        value: Box<dyn BaboonGeneratedDyn>,
        _conversion: &dyn BaboonAnyConversion,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let any = value.into_any();
        let v1 = any
            .downcast::<SampleV1>()
            .map_err(|_| BaboonCodecError::converter_failure("not SampleV1"))?;
        Ok(Box::new(SampleV2 { payload: v1.payload * 10 }))
    }
}

#[test]
fn pr11_d03_convert_v1_to_v2() {
    let facade = BaboonCodecsFacade::new();
    register_sample_v1(&facade);
    register_sample_v2(&facade);
    facade.register_conversions(
        baboon_rs_stub::baboon_codecs_facade::BaboonDomainVersion::new("my.dom", "2.0.0"),
        || Arc::new(SampleConversions) as Arc<dyn BaboonAnyConversions>,
    );

    let v1 = Box::new(SampleV1 { payload: 3 }) as Box<dyn BaboonGeneratedDyn>;
    let converted = facade.convert(v1).expect("convert");
    assert_eq!(converted.baboon_domain_version_dyn(), "2.0.0");
    let s = converted.as_any().downcast_ref::<SampleV2>().expect("downcast V2");
    assert_eq!(s.payload, 30);
}

#[test]
fn pr11_d03_convert_typed_returns_target() {
    let facade = BaboonCodecsFacade::new();
    register_sample_v1(&facade);
    register_sample_v2(&facade);
    facade.register_conversions(
        baboon_rs_stub::baboon_codecs_facade::BaboonDomainVersion::new("my.dom", "2.0.0"),
        || Arc::new(SampleConversions) as Arc<dyn BaboonAnyConversions>,
    );

    let v1 = Box::new(SampleV1 { payload: 4 }) as Box<dyn BaboonGeneratedDyn>;
    let converted: Box<SampleV2> = facade.convert_typed::<SampleV2>(v1).expect("convert_typed");
    assert_eq!(converted.payload, 40);
}

#[test]
fn pr11_d03_convert_unknown_domain_yields_converter_failure() {
    let facade = BaboonCodecsFacade::new();
    let v1 = Box::new(SampleV1 { payload: 1 }) as Box<dyn BaboonGeneratedDyn>;
    let result = facade.convert(v1);
    assert!(matches!(result, Err(BaboonCodecError::ConverterFailure { .. })));
}

// ----- D04: BaboonTypeMeta synthesis with min-compat from same-in-versions[0] -----

#[test]
fn pr11_d04_min_compat_from_same_in_versions_first() {
    let facade = BaboonCodecsFacade::new();
    register_sample_v2(&facade);
    let value = SampleV2 { payload: 11 };
    let json = facade.encode_to_json(&value).expect("encode_to_json");
    let obj = json.as_object().expect("object");
    // SampleV2: domain_version=2.0.0, same_in[0]=1.0.0 → $uv must be present and == 1.0.0
    assert_eq!(obj.get("$d").and_then(|v| v.as_str()), Some("my.dom"));
    assert_eq!(obj.get("$v").and_then(|v| v.as_str()), Some("2.0.0"));
    assert_eq!(obj.get("$uv").and_then(|v| v.as_str()), Some("1.0.0"));
    assert_eq!(obj.get("$t").and_then(|v| v.as_str()), Some("Sample"));
}

#[test]
fn pr11_d04_no_uv_when_min_compat_equals_current() {
    let facade = BaboonCodecsFacade::new();
    register_sample_v1(&facade);
    let value = SampleV1 { payload: 1 };
    let json = facade.encode_to_json(&value).expect("encode_to_json");
    let obj = json.as_object().expect("object");
    // SampleV1: same_in[0]=1.0.0 == current → $uv suppressed
    assert!(obj.get("$uv").is_none(), "$uv must be omitted when min-compat == current");
}

#[test]
fn pr11_d04_min_compat_in_binary_prefix() {
    let facade = BaboonCodecsFacade::new();
    register_sample_v2(&facade);
    let value = SampleV2 { payload: 88 };
    let bytes = facade
        .encode_to_bin(&BaboonCodecContext::Compact, &value)
        .expect("encode_to_bin");
    // Read the wire prefix and confirm min_compat == "1.0.0", not "2.0.0".
    let mut cursor = Cursor::new(&bytes);
    let decoded = facade.decode_from_bin(&mut cursor).expect("decode");
    let v = decoded.as_any().downcast_ref::<SampleV2>().expect("downcast");
    assert_eq!(v.payload, 88);
}

// ----- D05: serde_json/preserve_order — JSON envelope key insertion order -----

#[test]
fn pr11_d05_any_opaque_json_preserves_key_insertion_order() {
    let payload = serde_json::json!({"foo": 42});
    let opaque = AnyOpaque::Json(AnyOpaqueJson::new(
        AnyMeta::new(0x07, Some("d".into()), Some("v".into()), Some("t".into())).unwrap(),
        payload,
    ));
    let serialized = serde_json::to_string(&opaque).expect("serialize");
    // With preserve_order: $ak, $ad, $av, $at, $c (insertion order).
    // Without (BTreeMap default): $ad, $ak, $at, $av, $c (lexical).
    let ak_pos = serialized.find("\"$ak\"").expect("$ak in output");
    let ad_pos = serialized.find("\"$ad\"").expect("$ad in output");
    let av_pos = serialized.find("\"$av\"").expect("$av in output");
    let at_pos = serialized.find("\"$at\"").expect("$at in output");
    let c_pos = serialized.find("\"$c\"").expect("$c in output");
    assert!(ak_pos < ad_pos, "expected $ak before $ad; got: {}", serialized);
    assert!(ad_pos < av_pos, "expected $ad before $av; got: {}", serialized);
    assert!(av_pos < at_pos, "expected $av before $at; got: {}", serialized);
    assert!(at_pos < c_pos, "expected $at before $c; got: {}", serialized);
}

// ----- D06: BaboonAnyConversions trait surface is callable -----

#[test]
fn pr11_d06_find_and_convert_via_trait() {
    let conv = SampleConversions;
    let v1 = SampleV1 { payload: 6 };
    let candidates = conv.find_conversions(&v1);
    assert_eq!(candidates.len(), 1);
    assert_eq!(candidates[0].version_to(), "2.0.0");
    assert_eq!(candidates[0].version_from(), "1.0.0");
    assert_eq!(candidates[0].type_from(), std::any::TypeId::of::<SampleV1>());
    assert_eq!(candidates[0].type_id(), "Sample");

    let result = conv
        .convert(Box::new(v1) as Box<dyn BaboonGeneratedDyn>, candidates[0].as_ref())
        .expect("convert");
    let v2 = result.as_any().downcast_ref::<SampleV2>().expect("V2");
    assert_eq!(v2.payload, 60);
}

// ===== PR-12 round-2 regression =====

// ----- D01: decode_any_field rejects negative i32 length-prefixes with a structured error.
//
// Without the fix, `read_i32(...) as usize` on a negative wire value casts to `0xFFFF_FFFF...`
// and downstream `4 + any_meta_length` panics in debug (overflow) or wraps in release. The
// fix reads i32 explicitly, checks `< 0`, and returns `Err("any: negative ...")`.
//
// We exercise the helper end-to-end by encoding a real Holder, then patching the first 4
// bytes after the compact-mode marker (= `total_length` of the first any-field) to a
// hand-crafted `0xFF 0xFF 0xFF 0xFF` (= -1 in two's-complement i32 LE).

#[test]
fn pr12_d01_decode_any_field_rejects_negative_total_length() {
    use baboon_rs_stub::baboon_runtime::{BaboonBinDecode, BaboonBinEncode};
    use baboon_rs_stub::my::ok::holder::Holder;

    let ctx = BaboonCodecContext::Compact;
    let mut rnd = baboon_rs_stub::baboon_fixture::BaboonRandom::new();
    let fixture = baboon_rs_stub::my::ok::random_holder(&mut rnd);
    let mut buf = Vec::new();
    fixture.encode_ueba(&ctx, &mut buf).expect("encode");

    // Compact mode: buf[0] = 0x00 marker, buf[1..5] = total_length(i32 LE) of f_any.
    assert!(buf.len() >= 5, "expected at least 5 bytes; got {}", buf.len());
    assert_eq!(buf[0], 0x00, "expected compact-mode marker");
    buf[1] = 0xFF;
    buf[2] = 0xFF;
    buf[3] = 0xFF;
    buf[4] = 0xFF;

    let mut cursor = Cursor::new(&buf);
    let res = <Holder as BaboonBinDecode>::decode_ueba(&ctx, &mut cursor);
    let err = res.err().expect("expected Err for corrupted negative length");
    let msg = format!("{}", err);
    assert!(
        msg.contains("negative"),
        "expected error message to contain 'negative'; got: {}", msg
    );
}

#[test]
fn pr12_d01_decode_any_field_rejects_negative_meta_length() {
    use baboon_rs_stub::baboon_runtime::{BaboonBinDecode, BaboonBinEncode};
    use baboon_rs_stub::my::ok::holder::Holder;

    let ctx = BaboonCodecContext::Compact;
    let mut rnd = baboon_rs_stub::baboon_fixture::BaboonRandom::new();
    let fixture = baboon_rs_stub::my::ok::random_holder(&mut rnd);
    let mut buf = Vec::new();
    fixture.encode_ueba(&ctx, &mut buf).expect("encode");

    // Compact mode: buf[5..9] = meta_length(i32 LE) of f_any. Patch to -1 LE.
    assert!(buf.len() >= 9, "expected at least 9 bytes; got {}", buf.len());
    buf[5] = 0xFF;
    buf[6] = 0xFF;
    buf[7] = 0xFF;
    buf[8] = 0xFF;

    let mut cursor = Cursor::new(&buf);
    let res = <Holder as BaboonBinDecode>::decode_ueba(&ctx, &mut cursor);
    let err = res.err().expect("expected Err for corrupted negative meta-length");
    let msg = format!("{}", err);
    assert!(
        msg.contains("negative"),
        "expected error message to contain 'negative'; got: {}", msg
    );
}
