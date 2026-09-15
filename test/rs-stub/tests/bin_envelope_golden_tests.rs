// Cross-language golden bytes for the top-level binary envelope (docs/spec/codec-envelope.md §2.1, §2.1.2, §2.1.3; docs/forward-compat.md, "Worked examples"). The same values must produce these exact bytes in every backend; the Scala and TypeScript stubs assert the same sequences structurally.
use baboon_rs_stub::baboon_runtime::{BaboonCodecContext, BaboonEnvelopeVersion, ForwardWritePolicy};
use baboon_rs_stub::baboon_codecs_facade::{BaboonCodecsFacade, BaboonGeneratedDyn};
use baboon_rs_stub::fwde2e::chain::chain_append::ChainAppend;
use baboon_rs_stub::fwde2e::chain::domain_fwde2echain_facade::DomainFwde2eChainFacade;
use baboon_rs_stub::fwde2e::fwd::domain_fwde2efwd_facade::DomainFwde2eFwdFacade;
use baboon_rs_stub::fwde2e::fwd::fwd_append_var::FwdAppendVar;
use baboon_rs_stub::fwde2e::fwd::fwd_enum_grows::FwdEnumGrows;
use baboon_rs_stub::fwde2e::fwd::fwd_enum_host::FwdEnumHost;
use baboon_rs_stub::fwde2e::fwd::fwd_stable::FwdStable;

fn hx(b: &[u8]) -> String {
    b.iter().map(|x| format!("{:02X}", x)).collect::<Vec<_>>().join(" ")
}

fn enc(f: &BaboonCodecsFacade, ctx: &BaboonCodecContext, v: &dyn BaboonGeneratedDyn) -> String {
    hx(&f.encode_to_bin(ctx, v).expect("encode_to_bin must succeed"))
}

fn v1_tolerant() -> BaboonCodecContext { BaboonCodecContext::custom(false, ForwardWritePolicy::Tolerant, BaboonEnvelopeVersion::V1, None) }
fn v2_compact() -> BaboonCodecContext { BaboonCodecContext::custom(false, ForwardWritePolicy::Strict, BaboonEnvelopeVersion::V2, None) }
fn v2_indexed() -> BaboonCodecContext { BaboonCodecContext::custom(true, ForwardWritePolicy::Strict, BaboonEnvelopeVersion::V2, None) }

#[test]
fn default_contexts_write_v1_strict() {
    assert_eq!(BaboonCodecContext::Compact.envelope_version(), BaboonEnvelopeVersion::V1);
    assert_eq!(BaboonCodecContext::Compact.forward_write_policy(), ForwardWritePolicy::Strict);
    assert_eq!(BaboonCodecContext::Indexed.envelope_version(), BaboonEnvelopeVersion::V1);
}

#[test]
fn same_in_versions_dyn_is_the_real_run() {
    // regression: the generated impl returned `[own version]` for every type, so Rust-written
    // envelopes of unchanged types elided the byte-identical bound that every other backend publishes
    assert_eq!(FwdStable { s: "s".into() }.baboon_same_in_versions_dyn(), vec!["1.0.0".to_string(), "2.0.0".to_string()]);
}

#[test]
fn envelopes_match_the_cross_language_golden_bytes() {
    let fwd = DomainFwde2eFwdFacade::new();
    let chain = DomainFwde2eChainFacade::new();
    let app = FwdAppendVar { a: 42, b: "hi".into(), t: Some("t".into()) };
    // FwdAppendVar, v1 Strict (default) compact: identical bound elided
    assert_eq!(enc(&fwd.facade, &BaboonCodecContext::Compact, &app), "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74");
    // FwdAppendVar, v1 Tolerant compact: prefix-compact bound 1.0.0 in the single slot
    assert_eq!(enc(&fwd.facade, &v1_tolerant(), &app), "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74");
    // FwdAppendVar, v2 compact: flags 0b10, readableMin 1.0.0
    assert_eq!(enc(&fwd.facade, &v2_compact(), &app), "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 02 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74");
    // FwdAppendVar, v2 indexed: flags 0 (prefix-any-mode bound is 2.0.0)
    assert_eq!(enc(&fwd.facade, &v2_indexed(), &app), "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 01 04 00 00 00 03 00 00 00 07 00 00 00 03 00 00 00 2A 00 00 00 02 68 69 01 01 74");
    // FwdStable, v1 Strict compact: byte-identical since 1.0.0 -> hasMinCompat 1
    assert_eq!(enc(&fwd.facade, &BaboonCodecContext::Compact, &FwdStable { s: "s".into() }), "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73");
    // FwdStable, v2 compact: flags 0b01, minCompat 1.0.0, readableMin elided
    assert_eq!(enc(&fwd.facade, &v2_compact(), &FwdStable { s: "s".into() }), "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73");
    // FwdEnumHost, v2 compact: flags 0, no bound
    assert_eq!(enc(&fwd.facade, &v2_compact(), &FwdEnumHost { e: FwdEnumGrows::C }), "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 18 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 45 6E 75 6D 48 6F 73 74 00 02");
    // ChainAppend 3.0.0, v2 compact: flags 0b10, readableMin 1.0.0
    assert_eq!(enc(&chain.facade, &v2_compact(), &ChainAppend { a: 1, b: Some("b".into()), c: Some("c".into()) }), "02 0C 66 77 64 65 32 65 2E 63 68 61 69 6E 05 33 2E 30 2E 30 02 05 31 2E 30 2E 30 1A 66 77 64 65 32 65 2E 63 68 61 69 6E 2F 3A 23 43 68 61 69 6E 41 70 70 65 6E 64 00 01 00 00 00 01 01 62 01 01 63");
}

#[test]
fn v2_envelope_round_trips_through_its_own_facade() {
    let fwd = DomainFwde2eFwdFacade::new();
    let app = FwdAppendVar { a: 42, b: "hi".into(), t: Some("t".into()) };
    let bytes = fwd.facade.encode_to_bin(&v2_compact(), &app).expect("encode");
    let decoded = fwd.facade.decode_from_bin(&mut bytes.as_slice()).expect("decode");
    assert_eq!(decoded.as_any().downcast_ref::<FwdAppendVar>(), Some(&app));
}
