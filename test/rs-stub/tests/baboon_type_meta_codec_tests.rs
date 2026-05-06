// NOTE: This integration test references generated runtime symbols (BaboonTypeMeta,
// BaboonCodecsFacade, BaboonGeneratedDyn, ...) which are copied into this stub
// only by the rs-stub codegen path (rsync + codegen into
// target/test-regular/rs-stub/). Running `cargo test` directly from the source
// tree may fail with missing symbols; run from the codegen'd copy.
//
// Tests for BaboonTypeMeta JSON write/read behaviour — specifically the numeric
// "$mv" option β semantics (MFACADE-PR-3-D04, MFACADE-PR-3-D05).

use baboon_rs_stub::baboon_codecs_facade::{
    AbstractBaboonJsonCodecsImpl, AbstractBaboonUebaCodecsImpl, BaboonAnyBinCodec,
    BaboonAnyJsonCodec, BaboonAnyMeta, BaboonCodecsFacade, BaboonDomainVersion,
    BaboonGeneratedDyn,
};
use baboon_rs_stub::baboon_runtime::BaboonCodecContext;
use baboon_rs_stub::any_opaque::BaboonCodecError;
use std::io::{Read, Write};
use std::sync::Arc;

// ---------------------------------------------------------------------------
// Minimal fake generated type for white-box tests that need a registered codec
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
struct TestVal {
    x: i32,
}

impl BaboonGeneratedDyn for TestVal {
    fn baboon_domain_version_dyn(&self) -> &str { "1.0.0" }
    fn baboon_domain_identifier_dyn(&self) -> &str { "test.dom" }
    fn baboon_type_identifier_dyn(&self) -> &str { "TestType" }
    fn baboon_same_in_versions_dyn(&self) -> Vec<String> { vec!["1.0.0".to_string()] }
    fn as_any(&self) -> &dyn std::any::Any { self }
    fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self }
}

struct TestBinCodec;

impl BaboonAnyBinCodec for TestBinCodec {
    fn type_identifier(&self) -> &str { "TestType" }
    fn encode_dyn(
        &self,
        _ctx: &BaboonCodecContext,
        writer: &mut dyn Write,
        value: &dyn BaboonGeneratedDyn,
    ) -> Result<(), BaboonCodecError> {
        let v = value
            .as_any()
            .downcast_ref::<TestVal>()
            .ok_or_else(|| BaboonCodecError::encoder_failure("wrong type"))?;
        writer.write_all(&v.x.to_le_bytes()).map_err(|e| {
            BaboonCodecError::encoder_failure(format!("write: {e}"))
        })
    }
    fn decode_dyn(
        &self,
        _ctx: &BaboonCodecContext,
        reader: &mut dyn Read,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let mut buf = [0u8; 4];
        reader.read_exact(&mut buf).map_err(|e| {
            BaboonCodecError::decoder_failure(format!("read: {e}"))
        })?;
        Ok(Box::new(TestVal { x: i32::from_le_bytes(buf) }))
    }
}

struct TestJsonCodec;

impl BaboonAnyJsonCodec for TestJsonCodec {
    fn type_identifier(&self) -> &str { "TestType" }
    fn encode_json_dyn(
        &self,
        _ctx: &BaboonCodecContext,
        value: &dyn BaboonGeneratedDyn,
    ) -> Result<serde_json::Value, BaboonCodecError> {
        let v = value
            .as_any()
            .downcast_ref::<TestVal>()
            .ok_or_else(|| BaboonCodecError::encoder_failure("wrong type"))?;
        Ok(serde_json::json!({ "x": v.x }))
    }
    fn decode_json_dyn(
        &self,
        _ctx: &BaboonCodecContext,
        wire: &serde_json::Value,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let x = wire
            .get("x")
            .and_then(|v| v.as_i64())
            .ok_or_else(|| BaboonCodecError::decoder_failure("missing x"))? as i32;
        Ok(Box::new(TestVal { x }))
    }
}

fn register_test_v1(facade: &BaboonCodecsFacade) {
    struct M;
    impl BaboonAnyMeta for M {
        fn same_in_versions(&self, _: &str) -> Vec<String> {
            vec!["1.0.0".to_string()]
        }
    }
    let dv = BaboonDomainVersion::new("test.dom", "1.0.0");
    facade.register_with_meta(
        dv,
        || {
            let mut t = AbstractBaboonJsonCodecsImpl::new();
            t.register("TestType", || {
                Arc::new(TestJsonCodec) as Arc<dyn BaboonAnyJsonCodec>
            });
            Arc::new(t)
        },
        || {
            let mut t = AbstractBaboonUebaCodecsImpl::new();
            t.register("TestType", || {
                Arc::new(TestBinCodec) as Arc<dyn BaboonAnyBinCodec>
            });
            Arc::new(t)
        },
        || Arc::new(M) as Arc<dyn BaboonAnyMeta>,
    );
}

// ---------------------------------------------------------------------------
// [MFACADE-PR-3-D04] writer emits numeric $mv
//
// facade.encode_to_json must include "$mv" as a JSON number (serde_json::Value::Number)
// equal to 1. The reader accepted string "1" in legacy envelopes; option β requires the
// writer to emit a numeric value so strict consumers that only accept numbers also work.
// ---------------------------------------------------------------------------

#[test]
fn type_meta_write_json_emits_mv_as_numeric_1() {
    let facade = BaboonCodecsFacade::new();
    register_test_v1(&facade);

    let value = TestVal { x: 42 };
    let json = facade
        .encode_to_json(&value)
        .expect("encode_to_json");

    let obj = json.as_object().expect("envelope must be a JSON object");

    assert!(
        matches!(obj.get("$mv"), Some(serde_json::Value::Number(_))),
        "$mv must be a JSON Number; got: {:?}",
        obj.get("$mv"),
    );
    assert_eq!(
        obj.get("$mv").and_then(|v| v.as_u64()),
        Some(1),
        "$mv must equal 1; got: {:?}",
        obj.get("$mv"),
    );
}

// ---------------------------------------------------------------------------
// [MFACADE-PR-3-D05] reader accepts numeric $mv
//
// decode_from_json must parse a JSON envelope where "$mv" is the number 1
// (not the string "1") and return a non-None result. An Ok(None) result would
// indicate the meta was rejected, which is the defect being pinned here.
// ---------------------------------------------------------------------------

#[test]
fn type_meta_read_json_accepts_numeric_mv_1() {
    let facade = BaboonCodecsFacade::new();
    register_test_v1(&facade);

    // Build a well-formed Baboon envelope with numeric $mv = 1.
    let envelope = serde_json::json!({
        "$mv": 1,
        "$d": "test.dom",
        "$v": "1.0.0",
        "$t": "TestType",
        "$c": { "x": 7 },
    });

    let result = facade.decode_from_json(&envelope);

    // If the reader accepted the numeric $mv, it parsed the meta and either
    // decoded successfully (Ok(Some(...))) or failed with a codec error (Err(...)).
    // Ok(None) means the meta was not recognised — that is the defect case.
    assert!(
        !matches!(result, Ok(None)),
        "decode_from_json must not return Ok(None) for a numeric $mv=1 envelope; \
         Ok(None) means the meta was rejected",
    );
}
