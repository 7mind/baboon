// Every runtime fails fast when a value's `baboon_min_reader_versions_dyn` lacks the tier the
// envelope needs (docs/forward-compat.md, "Envelope integration"). Generated types always carry all
// four tiers; this guards hand-written impls, which used to be silently written with the
// byte-identical bound in place of the missing one.
use baboon_rs_stub::any_opaque::BaboonCodecError;
use baboon_rs_stub::baboon_codecs_facade::{
    AbstractBaboonJsonCodecsImpl, AbstractBaboonUebaCodecsImpl, BaboonAnyBinCodec, BaboonAnyJsonCodec,
    BaboonAnyMeta, BaboonCodecsFacade, BaboonDomainVersion, BaboonGeneratedDyn,
};
use baboon_rs_stub::baboon_runtime::{BaboonCodecContext, BaboonEnvelopeVersion, ForwardWritePolicy};
use std::io::{Read, Write};
use std::sync::Arc;

const DOMAIN: &str = "t.d";
const VERSION: &str = "1.0.0";
const TYPE_ID: &str = "t.d/:#T";

/// A hand-written value whose min-reader table is chosen per test.
#[derive(Debug, Clone, PartialEq)]
struct NoTiers {
    tiers: Vec<(String, String)>,
}

impl NoTiers {
    fn none() -> Self { NoTiers { tiers: Vec::new() } }
    fn json_only() -> Self { NoTiers { tiers: vec![("json-additive".to_string(), VERSION.to_string())] } }
}

impl BaboonGeneratedDyn for NoTiers {
    fn baboon_domain_version_dyn(&self) -> &str { VERSION }
    fn baboon_domain_identifier_dyn(&self) -> &str { DOMAIN }
    fn baboon_type_identifier_dyn(&self) -> &str { TYPE_ID }
    fn baboon_same_in_versions_dyn(&self) -> Vec<String> { vec![VERSION.to_string()] }
    fn baboon_min_reader_versions_dyn(&self) -> Vec<(String, String)> { self.tiers.clone() }
    fn as_any(&self) -> &dyn std::any::Any { self }
    fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self }
}

struct NoTiersJsonCodec;
impl BaboonAnyJsonCodec for NoTiersJsonCodec {
    fn type_identifier(&self) -> &str { TYPE_ID }
    fn encode_json_dyn(&self, _ctx: &BaboonCodecContext, _value: &dyn BaboonGeneratedDyn) -> Result<serde_json::Value, BaboonCodecError> {
        Ok(serde_json::json!({}))
    }
    fn decode_json_dyn(&self, _ctx: &BaboonCodecContext, _wire: &serde_json::Value) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        Ok(Box::new(NoTiers::none()))
    }
}

struct NoTiersBinCodec;
impl BaboonAnyBinCodec for NoTiersBinCodec {
    fn type_identifier(&self) -> &str { TYPE_ID }
    fn encode_dyn(&self, _ctx: &BaboonCodecContext, writer: &mut dyn Write, _value: &dyn BaboonGeneratedDyn) -> Result<(), BaboonCodecError> {
        writer.write_all(&[0u8]).map_err(|e| BaboonCodecError::encoder_failure(format!("{}", e)))
    }
    fn decode_dyn(&self, _ctx: &BaboonCodecContext, reader: &mut dyn Read) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let mut b = [0u8; 1];
        reader.read_exact(&mut b).map_err(|e| BaboonCodecError::decoder_failure(format!("{}", e)))?;
        Ok(Box::new(NoTiers::none()))
    }
}

struct NoTiersMeta;
impl BaboonAnyMeta for NoTiersMeta {
    fn same_in_versions(&self, _type_id: &str) -> Vec<String> { vec![VERSION.to_string()] }
    fn forward_readable_versions(&self, _type_id: &str) -> Vec<(String, String)> { Vec::new() }
}

fn facade() -> BaboonCodecsFacade {
    let f = BaboonCodecsFacade::new();
    f.register_with_meta(
        BaboonDomainVersion::new(DOMAIN, VERSION),
        || {
            let mut t = AbstractBaboonJsonCodecsImpl::new();
            t.register(TYPE_ID, || Arc::new(NoTiersJsonCodec) as Arc<dyn BaboonAnyJsonCodec>);
            Arc::new(t)
        },
        || {
            let mut t = AbstractBaboonUebaCodecsImpl::new();
            t.register(TYPE_ID, || Arc::new(NoTiersBinCodec) as Arc<dyn BaboonAnyBinCodec>);
            Arc::new(t)
        },
        || Arc::new(NoTiersMeta) as Arc<dyn BaboonAnyMeta>,
    );
    f
}

#[test]
fn json_bound_missing_is_an_encoder_failure() {
    let err = facade().encode_to_json(&NoTiers::none()).err().expect("a value without the json-additive tier must not be silently encoded");
    assert!(format!("{:?}", err).contains("json-additive"), "got {:?}", err);
    // with the JSON bound present the value encodes normally
    assert!(facade().encode_to_json(&NoTiers::json_only()).is_ok());
}

#[test]
fn prefix_bound_missing_is_an_encoder_failure_for_v2() {
    let v2 = BaboonCodecContext::custom(false, ForwardWritePolicy::Strict, BaboonEnvelopeVersion::V2, None);
    // json-additive present, prefix-compact absent: v2 needs the prefix bound and must refuse
    let err = facade().encode_to_bin(&v2, &NoTiers::json_only()).err().expect("must fail");
    assert!(format!("{:?}", err).contains("prefix-compact"), "must fail because of the missing prefix tier, got {:?}", err);
    // the default v1/Strict context needs no prefix tier and encodes normally
    assert!(facade().encode_to_bin(&BaboonCodecContext::Compact, &NoTiers::json_only()).is_ok());
}
