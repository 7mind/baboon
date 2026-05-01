// NOTE: This test references generated code emitted by the Rust codegen path
// (baboon_identifier_repr runtime + identifier::ok::* types). It runs only
// from the rsync'd codegen copy under target/test-regular/rs-stub/. PR-57c
// mirror of the Kotlin/Java IdentifierReprTest suites.

use baboon_rs_stub::baboon_identifier_repr;
use baboon_rs_stub::identifier::ok::a as a_mod;
use baboon_rs_stub::identifier::ok::b as b_mod;
use baboon_rs_stub::identifier::ok::c as c_mod;
use baboon_rs_stub::identifier::ok::d as d_mod;
use baboon_rs_stub::identifier::ok::long_id as long_id_mod;
use baboon_rs_stub::identifier::ok::marker as marker_mod;
use baboon_rs_stub::identifier::ok::mixed as mixed_mod;
use baboon_rs_stub::identifier::ok::outer as outer_mod;
use baboon_rs_stub::identifier::ok::point_id as point_id_mod;
use baboon_rs_stub::identifier::ok::u_ints as u_ints_mod;

use chrono::{FixedOffset, TimeZone, Utc};
use uuid::Uuid;

// Spec §6.2: each of the 5 metacharacters escaped.
#[test]
fn escape_str_all_metacharacters() {
    let src = "\\#:{}";
    assert_eq!(
        "\\\\\\#\\:\\{\\}",
        baboon_identifier_repr::escape_str(src)
    );
}

// Spec §6.3: trailing single backslash escapes to `\\`.
#[test]
fn escape_str_trailing_backslash() {
    assert_eq!("foo\\\\", baboon_identifier_repr::escape_str("foo\\"));
}

// Spec §6.4: 4 backslashes round-trip.
#[test]
fn escape_str_all_backslashes() {
    assert_eq!(
        "\\\\\\\\\\\\\\\\",
        baboon_identifier_repr::escape_str("\\\\\\\\")
    );
}

// Spec §6.7: i64::MIN renders as plain signed decimal.
#[test]
fn long_id_roundtrip_i64_min() {
    let src = long_id_mod::LongId { x: i64::MIN };
    let s = src.to_string();
    assert_eq!("LongId:1.0.0#x:-9223372036854775808", s);

    let parsed = long_id_mod::long_id_repr_codec::parse_repr(&s).expect("parse_repr should succeed");
    assert_eq!(src, parsed);
}

#[test]
fn long_id_roundtrip_i64_max() {
    let src = long_id_mod::LongId { x: i64::MAX };
    let s = src.to_string();
    assert_eq!("LongId:1.0.0#x:9223372036854775807", s);

    let parsed = long_id_mod::long_id_repr_codec::parse_repr(&s).expect("parse_repr should succeed");
    assert_eq!(src, parsed);
}

// Spec §6.8: u64.MAX_VALUE renders as unsigned decimal via emitted helper.
#[test]
fn u64_to_string_max_value() {
    assert_eq!(
        "18446744073709551615",
        baboon_identifier_repr::u64_to_string(u64::MAX)
    );
}

#[test]
fn bytes_to_hex_empty() {
    assert_eq!("", baboon_identifier_repr::bytes_to_hex(&[]));
}

#[test]
fn bytes_to_hex_high_bytes() {
    assert_eq!(
        "fffe00",
        baboon_identifier_repr::bytes_to_hex(&[0xff, 0xfe, 0x00])
    );
}

#[test]
fn bit_to_string_lowercase() {
    assert_eq!("true", baboon_identifier_repr::bit_to_string(true));
    assert_eq!("false", baboon_identifier_repr::bit_to_string(false));
}

#[test]
fn tsu_to_string_24_chars() {
    let dt = Utc.with_ymd_and_hms(2026, 4, 29, 12, 34, 56).unwrap()
        + chrono::Duration::milliseconds(789);
    let s = baboon_identifier_repr::tsu_to_string(&dt);
    assert_eq!(24, s.len(), "tsu format must be 24 chars; got: {}", s);
    assert_eq!("2026-04-29T12:34:56.789Z", s);
}

#[test]
fn tso_to_string_29_chars_never_z() {
    let offset = FixedOffset::east_opt(2 * 3600).unwrap();
    let dt = offset
        .with_ymd_and_hms(2026, 4, 29, 12, 34, 56)
        .unwrap()
        + chrono::Duration::milliseconds(789);
    let s = baboon_identifier_repr::tso_to_string(&dt);
    assert_eq!(29, s.len());
    assert_eq!("2026-04-29T12:34:56.789+02:00", s);

    let utc_offset = FixedOffset::east_opt(0).unwrap();
    let utc_dt = utc_offset
        .with_ymd_and_hms(2026, 4, 29, 12, 34, 56)
        .unwrap()
        + chrono::Duration::milliseconds(789);
    let s_utc = baboon_identifier_repr::tso_to_string(&utc_dt);
    assert_eq!("2026-04-29T12:34:56.789+00:00", s_utc);
    assert!(!s_utc.contains('Z'), "tso must never use Z shorthand: {}", s_utc);
}

// Spec §6.9: multi-field flat round-trip.
#[test]
fn point_id_roundtrip_flat_multifield() {
    let src = point_id_mod::PointId { x: 1, label: "hello".to_string() };
    let s = src.to_string();
    assert_eq!("PointId:1.0.0#x:1:label:hello", s);

    let parsed = point_id_mod::point_id_repr_codec::parse_repr(&s).expect("parse_repr should succeed");
    assert_eq!(src, parsed);
}

// Spec §6.2: str with all 5 metacharacters round-trips.
#[test]
fn point_id_roundtrip_str_all_metacharacters() {
    let src = point_id_mod::PointId { x: 0, label: "\\#:{}".to_string() };
    let s = src.to_string();
    assert_eq!("PointId:1.0.0#x:0:label:\\\\\\#\\:\\{\\}", s);

    let parsed = point_id_mod::point_id_repr_codec::parse_repr(&s).expect("parse_repr should succeed");
    assert_eq!(src, parsed);
}

// Spec §6.1: empty str field round-trips.
#[test]
fn point_id_roundtrip_empty_str() {
    let src = point_id_mod::PointId { x: 42, label: "".to_string() };
    let s = src.to_string();
    assert_eq!("PointId:1.0.0#x:42:label:", s);

    let parsed = point_id_mod::point_id_repr_codec::parse_repr(&s).expect("parse_repr should succeed");
    assert_eq!(src, parsed);
}

#[test]
fn point_id_parse_repr_rejects_out_of_range_i32() {
    let bad = "PointId:1.0.0#x:2147483648:label:foo";
    let err = point_id_mod::point_id_repr_codec::parse_repr(bad).expect_err("expected parse error");
    assert!(err.contains("i32 out of range"), "got: {}", err);
}

// Spec §6.8: UInts with u64.MAX_VALUE round-trips.
#[test]
fn uints_roundtrip_u64_max() {
    let src = u_ints_mod::UInts { a: 0, b: 0, c: 0, d: u64::MAX };
    let s = src.to_string();
    assert!(
        s.contains("d:18446744073709551615"),
        "expected u64 MAX as 18446744073709551615; got: {}",
        s
    );

    let parsed = u_ints_mod::u_ints_repr_codec::parse_repr(&s).expect("parse_repr should succeed");
    assert_eq!(src, parsed);
}

#[test]
fn mixed_roundtrip_empty_bytes_and_utc_times() {
    let created = Utc.with_ymd_and_hms(2026, 4, 29, 12, 34, 56).unwrap()
        + chrono::Duration::milliseconds(789);
    let off = FixedOffset::east_opt(2 * 3600).unwrap();
    let scheduled = off
        .with_ymd_and_hms(2026, 4, 29, 12, 34, 56)
        .unwrap()
        + chrono::Duration::milliseconds(789);

    let src = mixed_mod::Mixed {
        active: true,
        id: Uuid::parse_str("de7b9e1e-5c93-45fe-beec-da99994f629a").unwrap(),
        payload: vec![],
        created,
        scheduled,
    };
    let s = src.to_string();
    assert!(
        s.contains("Mixed:1.0.0#active:true:id:de7b9e1e-5c93-45fe-beec-da99994f629a:"),
        "source: {}",
        s
    );
    assert!(
        s.contains(":payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00"),
        "source: {}",
        s
    );

    let parsed = mixed_mod::mixed_repr_codec::parse_repr(&s).expect("parse_repr should succeed");
    assert_eq!(src.active, parsed.active);
    assert_eq!(src.id, parsed.id);
    assert_eq!(parsed.payload.len(), 0);
    assert_eq!(src.created, parsed.created);
    assert_eq!(src.scheduled, parsed.scheduled);
}

#[test]
fn outer_roundtrip_nested_id() {
    let src = outer_mod::Outer {
        r#ref: point_id_mod::PointId { x: 7, label: "k".to_string() },
        tag: "t".to_string(),
    };
    let s = src.to_string();
    assert_eq!("Outer:1.0.0#ref:{PointId:1.0.0#x:7:label:k}:tag:t", s);

    let parsed = outer_mod::outer_repr_codec::parse_repr(&s).expect("parse_repr should succeed");
    assert_eq!(src, parsed);
}

// Spec §6.12: empty-fields id renders as `<Name>:<version>#`.
#[test]
fn marker_empty_fields() {
    let src = marker_mod::Marker {};
    assert_eq!("Marker:1.0.0#", src.to_string());

    let parsed = marker_mod::marker_repr_codec::parse_repr("Marker:1.0.0#").expect("parse_repr should succeed");
    assert_eq!(src, parsed);
}

#[test]
fn mixed_parse_repr_rejects_uid_mixed_case() {
    let bad = "Mixed:1.0.0#active:true:id:DE7B9E1E-5C93-45FE-BEEC-DA99994F629A:payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00";
    let err = mixed_mod::mixed_repr_codec::parse_repr(bad).expect_err("expected parse error");
    assert!(
        err.contains("uid not in canonical lowercase form"),
        "got: {}",
        err
    );
}

// Spec §5.5: bare `\` followed by non-metachar is a parse error.
#[test]
fn point_id_parse_repr_rejects_invalid_escape() {
    let bad = "PointId:1.0.0#x:0:label:foo\\zbar";
    let err = point_id_mod::point_id_repr_codec::parse_repr(bad).expect_err("expected parse error");
    assert!(err.contains("invalid escape"), "got: {}", err);
}

// Spec §5.5: trailing bare `\` is a parse error.
#[test]
fn point_id_parse_repr_rejects_trailing_backslash() {
    let bad = "PointId:1.0.0#x:0:label:foo\\";
    let err = point_id_mod::point_id_repr_codec::parse_repr(bad).expect_err("expected parse error");
    assert!(err.contains("trailing backslash"), "got: {}", err);
}

// Spec §6.10: 4-level deep nested-id round-trip — A → B → C → D.
#[test]
fn deep_nested_roundtrip_four_levels() {
    let src = a_mod::A {
        b: b_mod::B {
            c: c_mod::C {
                d: d_mod::D { x: 42 },
            },
        },
    };
    let s = src.to_string();
    assert_eq!(
        "A:1.0.0#b:{B:1.0.0#c:{C:1.0.0#d:{D:1.0.0#x:42}}}",
        s
    );

    let parsed = a_mod::a_repr_codec::parse_repr(&s).expect("parse_repr should succeed");
    assert_eq!(src, parsed);
}

// Spec §5.4 (D06): unsigned values MUST NOT have a leading `+`.
#[test]
fn uints_parse_repr_rejects_leading_plus() {
    let bad = "UInts:1.0.0#a:+1:b:2:c:3:d:4";
    let err = u_ints_mod::u_ints_repr_codec::parse_repr(bad).expect_err("expected parse error");
    assert!(err.contains("leading sign"), "got: {}", err);
}

// Spec §5.4 (PR-C): signed integer wire forms MUST NOT have a leading `+`.
#[test]
fn point_id_parse_repr_rejects_leading_plus_on_signed_int() {
    let bad = "PointId:1.0.0#x:+42:label:hello";
    let err = point_id_mod::point_id_repr_codec::parse_repr(bad).expect_err("expected parse error");
    assert!(err.contains("leading '+'"), "got: {}", err);
}

// Spec §5.4 (PR-C): i64 signed field must also reject leading `+`.
#[test]
fn long_id_parse_repr_rejects_leading_plus_on_i64() {
    let bad = "LongId:1.0.0#x:+1";
    let err = long_id_mod::long_id_repr_codec::parse_repr(bad).expect_err("expected parse error");
    assert!(err.contains("leading '+'"), "got: {}", err);
}

#[test]
fn parse_bytes_hex_rejects_uppercase() {
    let err =
        baboon_identifier_repr::parse_bytes_hex("AABB").expect_err("expected parse error");
    assert!(err.contains("non-lowercase or non-hex"), "got: {}", err);
}

#[test]
fn parse_bytes_hex_rejects_odd_length() {
    let err =
        baboon_identifier_repr::parse_bytes_hex("aab").expect_err("expected parse error");
    assert!(err.contains("odd-length"), "got: {}", err);
}

#[test]
fn is_canonical_uid_accepts_lowercase() {
    assert!(baboon_identifier_repr::is_canonical_uid(
        "de7b9e1e-5c93-45fe-beec-da99994f629a"
    ));
}

#[test]
fn is_canonical_uid_rejects_uppercase() {
    assert!(!baboon_identifier_repr::is_canonical_uid(
        "DE7B9E1E-5C93-45FE-BEEC-DA99994F629A"
    ));
}
