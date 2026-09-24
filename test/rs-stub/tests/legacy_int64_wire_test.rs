// 64-bit integers are written as decimal strings (docs/json-codecs.md, "64-bit integers").
// Readers stay lenient about the JSON-number form an older compiler produced, and that
// leniency is what keeps documents written before the change readable.
//
// Nothing writes numbers any more, so without this test the number arm of every decoder is
// dead as far as the suite is concerned.
//
// Uses the identifier.ok fixture (id LongId { x: i64 }; id UInts { a: u08, b: u16, c: u32,
// d: u64 }); generated symbols are produced by mdl :test-gen-regular-adt.

use baboon_rs_stub::identifier::ok::long_id::LongId;
use baboon_rs_stub::identifier::ok::u_ints::UInts;

#[test]
fn i64_decodes_from_the_legacy_numeric_form() {
    let decoded = LongId::from_json(r#"{"x":-9007199254740991}"#).expect("decode");
    assert_eq!(decoded.x, -9007199254740991i64);
}

#[test]
fn i64_decodes_from_the_string_form() {
    let decoded = LongId::from_json(r#"{"x":"-9223372036854775808"}"#).expect("decode");
    assert_eq!(decoded.x, i64::MIN);
}

#[test]
fn u64_decodes_from_the_legacy_numeric_form() {
    let decoded = UInts::from_json(r#"{"a":1,"b":2,"c":3,"d":42}"#).expect("decode");
    assert_eq!(decoded.d, 42u64);
}

#[test]
fn u64_decodes_from_the_string_form() {
    let decoded = UInts::from_json(r#"{"a":1,"b":2,"c":3,"d":"42"}"#).expect("decode");
    assert_eq!(decoded.d, 42u64);
}
