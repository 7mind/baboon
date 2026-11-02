// `bytes` JSON hex-decoding contract (issues #102 / #103): ASCII hex digits of either case,
// ASCII whitespace / ':' / '-' as separators, everything else a decode failure — never a panic,
// never wrong bytes.
//
// The JSON-codec cases use testpkg.pkg0 v3 `T1_D1 { f0: T1_D2, f1: i08, f3: str, f4: bytes, f5: opt[bytes], f6: map[bytes, str] }`.
// Generated symbols are produced by mdl :test-gen-regular-adt under target/test-regular/rs-stub/.

use baboon_rs_stub::any_opaque::BaboonCodecError;
use baboon_rs_stub::baboon_runtime::hex_bytes::hex_decode;
use baboon_rs_stub::baboon_runtime::json_tools::bytes_from_json;
use baboon_rs_stub::testpkg::pkg0::T1_D1;

const VALID: &[(&str, &[u8])] = &[
    ("", &[]),
    ("ABCD", &[0xAB, 0xCD]),
    ("abcd", &[0xAB, 0xCD]),
    ("aBcD", &[0xAB, 0xCD]),
    ("AB CD", &[0xAB, 0xCD]),
    ("AB:CD", &[0xAB, 0xCD]),
    ("AB-CD", &[0xAB, 0xCD]),
    ("A B C D", &[0xAB, 0xCD]),
    ("\tAB\r\nCD ", &[0xAB, 0xCD]),
    ("A:B-C D", &[0xAB, 0xCD]),
    ("00FF7f", &[0x00, 0xFF, 0x7F]),
    (" - : ", &[]),
];

const INVALID: &[&str] = &[
    "ZZ",
    "AQ",
    "+1",
    "0x01",
    "q80=",
    "ABC",
    "A",
    "-1",
    "\u{0663}\u{0663}",
    "\u{FF21}\u{FF21}",
    "a\u{e9}b",
    "AB\u{a0}CD",
];

fn decoder_failure_message(err: &BaboonCodecError) -> String {
    match err {
        BaboonCodecError::DecoderFailure { message, .. } => message.clone(),
        other => panic!("expected a DecoderFailure but got: {:?}", other),
    }
}

#[test]
fn hex_decode_accepts_valid_vectors() {
    for (input, expected) in VALID {
        assert_eq!(hex_decode(input).as_deref(), Ok(*expected), "input {:?}", input);
    }
}

#[test]
fn hex_decode_rejects_invalid_vectors() {
    for input in INVALID {
        let result = std::panic::catch_unwind(|| hex_decode(input));
        match result {
            Ok(Err(_)) => {}
            Ok(Ok(bytes)) => panic!("input {:?} decoded to {:02X?}, expected an error", input, bytes),
            Err(_) => panic!("input {:?} panicked, expected an error", input),
        }
    }
}

#[test]
fn bytes_from_json_reports_decoder_failure() {
    for (input, expected) in VALID {
        let decoded = bytes_from_json(&serde_json::Value::String(input.to_string()), "f");
        assert_eq!(decoded.ok().as_deref(), Some(*expected), "input {:?}", input);
    }
    for input in INVALID {
        let err = bytes_from_json(&serde_json::Value::String(input.to_string()), "f")
            .expect_err(&format!("input {:?} should fail", input));
        decoder_failure_message(&err);
    }
}

fn t1_d1_json(f4: &str, f5: &str) -> String {
    serde_json::json!({ "f0": { "f": "x" }, "f1": 1, "f3": "s", "f4": f4, "f5": f5, "f6": { "ab:CD": "v" } }).to_string()
}

#[test]
fn t1_d1_json_decode_accepts_separated_hex() {
    let decoded = T1_D1::from_json(&t1_d1_json("ab:cd", "00 FF-7f")).expect("valid hex should decode");
    assert_eq!(decoded.f4, vec![0xAB, 0xCD]);
    assert_eq!(decoded.f5, Some(vec![0x00, 0xFF, 0x7F]));
    assert_eq!(decoded.f6.get(&vec![0xAB, 0xCD]).map(String::as_str), Some("v"));
}

#[test]
fn t1_d1_json_decode_fails_on_malformed_hex() {
    for input in INVALID {
        let err = T1_D1::from_json(&t1_d1_json(input, "00")).expect_err(&format!("f4 {:?} should fail", input));
        decoder_failure_message(&err);
        let err = T1_D1::from_json(&t1_d1_json("00", input)).expect_err(&format!("f5 {:?} should fail", input));
        decoder_failure_message(&err);
    }
}
