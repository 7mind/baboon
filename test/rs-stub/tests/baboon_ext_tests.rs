// Tests for the `BaboonExt` trait and `unmodified_since_version` free function defined in
// `baboon_runtime`. These helpers mirror C# `BaboonExt` / TS `domainVersion` /
// `baboonUnmodifiedSinceVersion` / `unmodifiedSinceVersion`.
//
// The concrete fixture types below replicate the minimum `BaboonGenerated + BaboonMeta`
// surface required by `BaboonExt`; they mirror what codegen would emit for a real domain type.

use baboon_rs_stub::baboon_codecs_facade::BaboonDomainVersion;
use baboon_rs_stub::baboon_runtime::{unmodified_since_version, BaboonExt, BaboonGenerated, BaboonMeta};

// ---------------------------------------------------------------------------
// Fixture: a minimal generated type implementing BaboonGenerated + BaboonMeta
// ---------------------------------------------------------------------------

struct FakeInner;

impl BaboonGenerated for FakeInner {
    fn baboon_domain_version() -> &'static str { "1.0.0" }
    fn baboon_domain_identifier() -> &'static str { "my.ok" }
    fn baboon_type_identifier() -> &'static str { "my.ok/:#Inner" }
}

impl BaboonMeta for FakeInner {
    fn same_in_versions(type_id: &str) -> &[&str] {
        match type_id {
            "my.ok/:#Inner" => &["1.0.0"],
            _ => &[],
        }
    }
}

// Fixture for a type that has been stable across multiple versions.
struct FakeEvolved;

impl BaboonGenerated for FakeEvolved {
    fn baboon_domain_version() -> &'static str { "3.0.0" }
    fn baboon_domain_identifier() -> &'static str { "my.ok" }
    fn baboon_type_identifier() -> &'static str { "my.ok/:#Evolved" }
}

impl BaboonMeta for FakeEvolved {
    fn same_in_versions(type_id: &str) -> &[&str] {
        match type_id {
            "my.ok/:#Evolved" => &["1.0.0", "2.0.0", "3.0.0"],
            _ => &[],
        }
    }
}

// ---------------------------------------------------------------------------
// Tests for BaboonExt::domain_version
// ---------------------------------------------------------------------------

#[test]
fn domain_version_returns_correct_identifier_and_version() {
    let inner = FakeInner;
    let dv: BaboonDomainVersion = inner.domain_version();
    assert_eq!(dv.domain_identifier, "my.ok");
    assert_eq!(dv.domain_version, "1.0.0");
}

#[test]
fn domain_version_evolved_type_returns_current_version() {
    let evolved = FakeEvolved;
    let dv: BaboonDomainVersion = evolved.domain_version();
    assert_eq!(dv.domain_identifier, "my.ok");
    assert_eq!(dv.domain_version, "3.0.0");
}

// ---------------------------------------------------------------------------
// Tests for BaboonExt::baboon_unmodified_since_version
// ---------------------------------------------------------------------------

#[test]
fn baboon_unmodified_since_version_returns_first_same_in_versions_entry() {
    let inner = FakeInner;
    let v = inner.baboon_unmodified_since_version();
    assert_eq!(v, "1.0.0");
}

#[test]
fn baboon_unmodified_since_version_evolved_type_returns_oldest_stable_version() {
    let evolved = FakeEvolved;
    let v = evolved.baboon_unmodified_since_version();
    // FakeEvolved has been stable since 1.0.0 even though the domain is now at 3.0.0.
    assert_eq!(v, "1.0.0");
}

// ---------------------------------------------------------------------------
// Tests for unmodified_since_version (free function)
// ---------------------------------------------------------------------------

#[test]
fn unmodified_since_version_free_fn_looks_up_by_type_id() {
    let v = unmodified_since_version::<FakeInner>("my.ok/:#Inner");
    assert_eq!(v, "1.0.0");
}

#[test]
fn unmodified_since_version_free_fn_returns_oldest_for_evolved_type() {
    let v = unmodified_since_version::<FakeEvolved>("my.ok/:#Evolved");
    assert_eq!(v, "1.0.0");
}
