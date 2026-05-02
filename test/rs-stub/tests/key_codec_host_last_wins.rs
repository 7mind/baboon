// PR-26.2 (M26) — KeyCodec Host last-wins concurrency contract regression test.
//
// Verifies that re-calling `register_f_str_keycodec(impl_)` overwrites the
// previously registered implementation (last-wins), matching the 8 other
// backends. Pre-fix, Rust used `OnceLock<Box<dyn _>>` whose `set()` returns
// `Err` on second call, which the emission silently ignored — so impl A
// remained installed and impl B was lost (silent no-op).
//
// Post-fix, the emission uses `RwLock<Option<Arc<dyn _>>>`. Re-registration
// overwrites the previous impl. This test asserts: register A → encode → A
// observed; register B → encode → B observed (NOT A).
//
// NOTE: This test references generated symbols from m19-ok/wrapper-around-foreign.baboon
// (`my::ok::m19::foreign::f_str::{FStr_KeyCodec, register_f_str_keycodec, ...}`)
// which are emitted into this stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/rs-stub/). Running `cargo test`
// directly from the source tree fails with missing symbols.
//
// Test isolation: this test mutates module-global state via `register_*` and
// must run serially with respect to other tests that depend on the registered
// impl. Cargo runs tests in a `tests/` integration test FILE in its own
// process by default — tests within this file share state, but other test
// files are isolated. We run register A → assert A → register B → assert B
// inside a single `#[test]` to keep ordering deterministic.

use baboon_rs_stub::my::ok::m19::foreign::f_str::{
    register_f_str_keycodec, FStr_KeyCodec,
};
use baboon_rs_stub::my::ok::m19::foreign::holder::Holder;
use baboon_rs_stub::my::ok::m19::foreign::item_key::ItemKey;
use std::collections::BTreeMap;
use std::sync::Arc;

struct PrefixCodec(&'static str);

impl FStr_KeyCodec for PrefixCodec {
    fn encode_key(&self, value: &String) -> String {
        format!("{}:{}", self.0, value)
    }
    fn decode_key(&self, s: &str) -> Result<String, String> {
        match s.strip_prefix(&format!("{}:", self.0)) {
            Some(rest) => Ok(rest.to_owned()),
            None => Err(format!("expected {}: prefix on {}", self.0, s)),
        }
    }
}

struct IdentityCodec;

impl FStr_KeyCodec for IdentityCodec {
    fn encode_key(&self, value: &String) -> String {
        value.clone()
    }
    fn decode_key(&self, s: &str) -> Result<String, String> {
        Ok(s.to_owned())
    }
}

// PR-26.2-D01: a Drop-guard that re-registers the identity impl on scope exit
// (including panic), so the global codec singleton does not leak a PrefixCodec
// into any test that runs later in the same process. Cargo isolates integration
// test FILES into separate processes, but tests within this file share state.
struct IdentityRestoreGuard;

impl Drop for IdentityRestoreGuard {
    fn drop(&mut self) {
        register_f_str_keycodec(Arc::new(IdentityCodec));
    }
}

#[test]
fn key_codec_host_last_wins() {
    let _guard = IdentityRestoreGuard;

    let mut m = BTreeMap::new();
    m.insert(ItemKey { v: "k".to_string() }, "v".to_string());
    let sample = Holder { m };

    // Register impl A and observe it in the encode wire form.
    register_f_str_keycodec(Arc::new(PrefixCodec("A")));
    let encoded_a = serde_json::to_string(&sample).expect("encode under A");
    assert!(
        encoded_a.contains("A:k"),
        "expected A: prefix in encoded wire form, got {}",
        encoded_a
    );

    // Register impl B and observe it (NOT A) in the new encode wire form.
    // Pre-fix: this re-register silently no-ops; encoded_b would still contain "A:k".
    register_f_str_keycodec(Arc::new(PrefixCodec("B")));
    let encoded_b = serde_json::to_string(&sample).expect("encode under B");
    assert!(
        encoded_b.contains("B:k"),
        "PR-26.2 last-wins regression: expected B: prefix after re-register, got {}. \
         Pre-fix Rust OnceLock<Box<dyn _>> silently no-ops on re-register.",
        encoded_b
    );
    assert!(
        !encoded_b.contains("A:k"),
        "PR-26.2 last-wins regression: A: prefix still present after B re-register, got {}",
        encoded_b
    );
}
