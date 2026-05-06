// NOTE: This test references generated runtime symbols (BaboonCodecsFacade, BaboonCodecError,
// BaboonGeneratedDyn, ...) AND generated DTO symbols (my::ok::Inner) which are copied into this
// stub only by the rs-stub codegen path (rsync + codegen into target/test-regular/rs-stub/).
// Running `cargo test` directly from the source tree may fail with missing symbols; run from
// the codegen'd copy.
//
// Tests for decode_from_bin_latest / decode_from_json_latest / preload on BaboonCodecsFacade.
// Exercises the composition contract: encode -> decode_from_bin_latest -> assert Ok with expected
// value. Also covers the null-pass-through for decode_from_json_latest and the preload smoke test.

use baboon_rs_stub::any_opaque::BaboonCodecError;
use baboon_rs_stub::baboon_codecs_facade::{
    AbstractBaboonJsonCodecsImpl, AbstractBaboonUebaCodecsImpl, BaboonAnyBinCodec,
    BaboonAnyJsonCodec, BaboonAnyMeta, BaboonCodecsFacade, BaboonDomainVersion,
    BaboonGeneratedDyn,
};
use baboon_rs_stub::baboon_runtime::{
    BaboonBinDecode, BaboonBinEncode, BaboonCodecContext, BaboonGenerated, BaboonGeneratedLatest,
};
use baboon_rs_stub::my::ok::inner::Inner;
use std::io::{Read, Write};
use std::sync::Arc;

const DOMAIN_ID: &str = "my.ok";
const VERSION_STR: &str = "1.0.0";
const INNER_TYPE: &str = "my.ok/:#Inner";

// ===== InnerDyn adapter ==================================================================
//
// Wraps Inner in BaboonGeneratedDyn (same pattern as any_round_trip_tests.rs).

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

// BaboonGeneratedLatest is a marker supertrait of BaboonGenerated. Both are static-method
// traits with no instance methods, so the impl bodies are empty. Inner satisfies them.
// Here InnerDyn also satisfies them so convert_typed can downcast to it.
impl BaboonGenerated for InnerDyn {
    fn baboon_domain_version() -> &'static str { VERSION_STR }
    fn baboon_domain_identifier() -> &'static str { DOMAIN_ID }
    fn baboon_type_identifier() -> &'static str { INNER_TYPE }
}
impl BaboonGeneratedLatest for InnerDyn {}

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

fn sample_inner() -> Inner {
    Inner { x: 99 }
}

/// Encode an InnerDyn via the facade's encode_to_bin (writes the BaboonTypeMeta prefix).
fn encode_inner_via_facade(facade: &BaboonCodecsFacade) -> Vec<u8> {
    let val = InnerDyn(sample_inner());
    facade
        .encode_to_bin(&BaboonCodecContext::Compact, &val)
        .expect("encode_to_bin InnerDyn")
}

// ===== decode_from_bin_latest ===========================================================

#[test]
fn decode_from_bin_latest_round_trips_to_expected_inner() {
    let facade = fresh_facade();
    let bytes = encode_inner_via_facade(&facade);

    let result = facade
        .decode_from_bin_latest_bytes::<InnerDyn>(&bytes)
        .expect("decode_from_bin_latest_bytes");

    assert_eq!(result.0, sample_inner());
}

#[test]
fn decode_from_bin_latest_reader_overload_round_trips() {
    use std::io::Cursor;

    let facade = fresh_facade();
    let bytes = encode_inner_via_facade(&facade);
    let mut cursor = Cursor::new(&bytes[..]);

    let result = facade
        .decode_from_bin_latest::<InnerDyn, _>(&mut cursor)
        .expect("decode_from_bin_latest reader");

    assert_eq!(result.0, sample_inner());
}

#[test]
fn decode_from_bin_latest_returns_err_for_truncated_bytes() {
    let facade = fresh_facade();
    let bytes = encode_inner_via_facade(&facade);
    // Truncate to just the meta prefix — the payload decode will fail.
    let truncated = &bytes[..2];

    let result = facade.decode_from_bin_latest_bytes::<InnerDyn>(truncated);
    assert!(result.is_err(), "truncated bytes must yield Err");
}

// ===== decode_from_json_latest ==========================================================

#[test]
fn decode_from_json_latest_returns_none_for_non_envelope_json() {
    let facade = fresh_facade();
    let not_an_envelope = serde_json::json!({ "some": "object" });

    let result = facade
        .decode_from_json_latest::<InnerDyn>(&not_an_envelope)
        .expect("non-envelope must be Ok(None)");

    assert!(result.is_none(), "non-envelope must yield Ok(None)");
}

#[test]
fn decode_from_json_latest_round_trips_to_expected_inner() {
    let facade = fresh_facade();
    let val = InnerDyn(sample_inner());
    let json_envelope = facade
        .encode_to_json(&val)
        .expect("encode_to_json InnerDyn");

    let result = facade
        .decode_from_json_latest::<InnerDyn>(&json_envelope)
        .expect("decode_from_json_latest")
        .expect("Some");

    assert_eq!(result.0, sample_inner());
}

// ===== preload ==========================================================================

#[test]
fn preload_does_not_panic_and_warms_up_lazy_cells() {
    // preload is fire-and-forget; we cannot observe the background thread directly, but we
    // can confirm the call returns immediately without panicking and that a subsequent codec
    // lookup still works (the cells are either warm or lazily initialised on first use).
    let facade = fresh_facade();
    facade.preload();

    // Brief yield to let the thread run (not required for correctness; just increases
    // coverage of the warm-path on fast machines).
    std::thread::sleep(std::time::Duration::from_millis(5));

    let bytes = encode_inner_via_facade(&facade);
    let result = facade
        .decode_from_bin_latest_bytes::<InnerDyn>(&bytes)
        .expect("codec lookup works after preload");
    assert_eq!(result.0, sample_inner());
}
