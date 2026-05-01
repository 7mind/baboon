use baboon_conv_test_rs::convtest::testpkg::{AllBasicTypes, AnyShowcase, InnerPayload};
use baboon_conv_test_rs::convtest::m24foreign::{ForeignKeyHolder, ItemKey};
use baboon_conv_test_rs::any_opaque::AnyOpaque;
use baboon_conv_test_rs::baboon_runtime::{BaboonBinDecode, BaboonCodecContext};
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;

fn base_dir() -> PathBuf {
    PathBuf::from("../../target/compat-test")
}

fn read_json_file(source: &str) -> AllBasicTypes {
    let path = base_dir().join(format!("{}-json/all-basic-types.json", source));
    let json_str = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {:?}: {}", path, e));
    serde_json::from_str(&json_str)
        .unwrap_or_else(|e| panic!("Failed to parse JSON from {:?}: {}", path, e))
}

fn read_ueba_file(source: &str) -> AllBasicTypes {
    let path = base_dir().join(format!("{}-ueba/all-basic-types.ueba", source));
    let bytes = fs::read(&path)
        .unwrap_or_else(|e| panic!("Failed to read {:?}: {}", path, e));
    let ctx = BaboonCodecContext::Default;
    let mut cursor = std::io::Cursor::new(&bytes);
    AllBasicTypes::decode_ueba(&ctx, &mut cursor)
        .unwrap_or_else(|e| panic!("Failed to decode UEBA from {:?}: {}", path, e))
}

fn assert_basic_fields(data: &AllBasicTypes, label: &str) {
    println!("Successfully decoded {}: {}", label, data.vstr);
    assert_eq!(data.vstr, "Hello, Baboon!");
    assert_eq!(data.vi32, 123456);
    assert!(data.vbit);
}

// JSON deserialization tests
#[test]
fn test_read_rust_json() {
    let data = read_json_file("rust");
    assert_basic_fields(&data, "Rust JSON");
}

#[test]
fn test_read_scala_json() {
    let data = read_json_file("scala");
    assert_basic_fields(&data, "Scala JSON");
}

#[test]
fn test_read_cs_json() {
    let data = read_json_file("cs");
    assert_basic_fields(&data, "C# JSON");
}

#[test]
fn test_read_python_json() {
    let data = read_json_file("python");
    assert_basic_fields(&data, "Python JSON");
}

// UEBA deserialization tests
#[test]
fn test_read_rust_ueba() {
    let data = read_ueba_file("rust");
    assert_basic_fields(&data, "Rust UEBA");
}

#[test]
fn test_read_scala_ueba() {
    let data = read_ueba_file("scala");
    assert_basic_fields(&data, "Scala UEBA");
}

#[test]
fn test_read_cs_ueba() {
    let data = read_ueba_file("cs");
    assert_basic_fields(&data, "C# UEBA");
}

#[test]
fn test_read_python_ueba() {
    let data = read_ueba_file("python");
    assert_basic_fields(&data, "Python UEBA");
}

// Cross-language JSON equality
#[test]
fn test_rust_scala_json_equal() {
    let rust_data = read_json_file("rust");
    let scala_data = read_json_file("scala");
    assert_eq!(rust_data, scala_data, "Rust and Scala JSON data should be equal");
}

#[test]
fn test_rust_cs_json_equal() {
    let rust_data = read_json_file("rust");
    let cs_data = read_json_file("cs");
    assert_eq!(rust_data, cs_data, "Rust and C# JSON data should be equal");
}

#[test]
fn test_rust_python_json_equal() {
    let rust_data = read_json_file("rust");
    let python_data = read_json_file("python");
    assert_eq!(rust_data, python_data, "Rust and Python JSON data should be equal");
}

// Cross-language UEBA equality
#[test]
fn test_rust_scala_ueba_equal() {
    let rust_data = read_ueba_file("rust");
    let scala_data = read_ueba_file("scala");
    assert_eq!(rust_data, scala_data, "Rust and Scala UEBA data should be equal");
}

#[test]
fn test_rust_cs_ueba_equal() {
    let rust_data = read_ueba_file("rust");
    let cs_data = read_ueba_file("cs");
    assert_eq!(rust_data, cs_data, "Rust and C# UEBA data should be equal");
}

#[test]
fn test_rust_python_ueba_equal() {
    let rust_data = read_ueba_file("rust");
    let python_data = read_ueba_file("python");
    assert_eq!(rust_data, python_data, "Rust and Python UEBA data should be equal");
}

// TypeScript, Kotlin, Java, Dart
#[test]
fn test_read_typescript_json() {
    let data = read_json_file("typescript");
    assert_basic_fields(&data, "TypeScript JSON");
}

#[test]
fn test_read_kotlin_json() {
    let data = read_json_file("kotlin");
    assert_basic_fields(&data, "Kotlin JSON");
}

#[test]
fn test_read_java_json() {
    let data = read_json_file("java");
    assert_basic_fields(&data, "Java JSON");
}

#[test]
fn test_read_dart_json() {
    let data = read_json_file("dart");
    assert_basic_fields(&data, "Dart JSON");
}

#[test]
fn test_read_typescript_ueba() {
    let data = read_ueba_file("typescript");
    assert_basic_fields(&data, "TypeScript UEBA");
}

#[test]
fn test_read_kotlin_ueba() {
    let data = read_ueba_file("kotlin");
    assert_basic_fields(&data, "Kotlin UEBA");
}

#[test]
fn test_read_java_ueba() {
    let data = read_ueba_file("java");
    assert_basic_fields(&data, "Java UEBA");
}

#[test]
fn test_read_dart_ueba() {
    let data = read_ueba_file("dart");
    assert_basic_fields(&data, "Dart UEBA");
}

#[test]
fn test_read_swift_json() {
    let path = base_dir().join("swift-json/all-basic-types.json");
    if !path.exists() {
        println!("Skipping Swift JSON - file not found");
        return;
    }
    let data = read_json_file("swift");
    assert_basic_fields(&data, "Swift JSON");
}

#[test]
fn test_read_swift_ueba() {
    let path = base_dir().join("swift-ueba/all-basic-types.ueba");
    if !path.exists() {
        println!("Skipping Swift UEBA - file not found");
        return;
    }
    let data = read_ueba_file("swift");
    assert_basic_fields(&data, "Swift UEBA");
}

// -----------------------------------------------------------------------------
// AnyShowcase cross-language tests (M13 / PR 13.2)
// -----------------------------------------------------------------------------

fn expected_any_payloads() -> Vec<InnerPayload> {
    vec![
        InnerPayload { label: "variant-A".to_string(),  count: 1 },
        InnerPayload { label: "variant-B".to_string(),  count: 2 },
        InnerPayload { label: "variant-C".to_string(),  count: 3 },
        InnerPayload { label: "variant-D1".to_string(), count: 4 },
        InnerPayload { label: "variant-D2".to_string(), count: 5 },
        InnerPayload { label: "variant-D3".to_string(), count: 6 },
        InnerPayload { label: "opt-any".to_string(),    count: 7 },
        InnerPayload { label: "lst-any-0".to_string(),  count: 8 },
    ]
}

fn read_any_showcase_json(source: &str) -> AnyShowcase {
    let path = base_dir().join(format!("{}-json/any-showcase.json", source));
    let json_str = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {:?}: {}", path, e));
    serde_json::from_str(&json_str)
        .unwrap_or_else(|e| panic!("Failed to parse AnyShowcase JSON from {:?}: {}", path, e))
}

fn read_any_showcase_ueba(source: &str) -> AnyShowcase {
    let path = base_dir().join(format!("{}-ueba/any-showcase.ueba", source));
    let bytes = fs::read(&path)
        .unwrap_or_else(|e| panic!("Failed to read {:?}: {}", path, e));
    let ctx = BaboonCodecContext::Default;
    let mut cursor = std::io::Cursor::new(&bytes);
    AnyShowcase::decode_ueba(&ctx, &mut cursor)
        .unwrap_or_else(|e| panic!("Failed to decode AnyShowcase UEBA from {:?}: {}", path, e))
}

fn decode_inner(o: &AnyOpaque) -> InnerPayload {
    match o {
        AnyOpaque::Ueba(u) => {
            let mut cursor = std::io::Cursor::new(&u.bytes);
            InnerPayload::decode_ueba(&BaboonCodecContext::Compact, &mut cursor)
                .unwrap_or_else(|e| panic!("InnerPayload UEBA decode failed: {}", e))
        }
        AnyOpaque::Json(j) => serde_json::from_value(j.json.clone())
            .unwrap_or_else(|e| panic!("InnerPayload JSON decode failed: {}", e)),
    }
}

fn decode_all_payloads(v: &AnyShowcase) -> Vec<InnerPayload> {
    let opt = v.opt_any.as_ref().expect("optAny was None; expected Some");
    let lst0 = v.lst_any.first().expect("lstAny was empty; expected one element");
    vec![
        decode_inner(&v.v_any_a),
        decode_inner(&v.v_any_b),
        decode_inner(&v.v_any_c),
        decode_inner(&v.v_any_d1),
        decode_inner(&v.v_any_d2),
        decode_inner(&v.v_any_d3),
        decode_inner(opt),
        decode_inner(lst0),
    ]
}

fn assert_any_showcase(source: &str, fmt: &str, decoded: Vec<InnerPayload>) {
    let expected = expected_any_payloads();
    assert_eq!(decoded.len(), expected.len(), "{} {} payload count mismatch", source, fmt);
    for (i, (exp, got)) in expected.iter().zip(decoded.iter()).enumerate() {
        assert_eq!(exp, got, "{} {} payload {} mismatch", source, fmt, i);
    }
}

#[test]
fn test_any_showcase_rust_json() {
    let v = read_any_showcase_json("rust");
    assert_any_showcase("rust", "JSON", decode_all_payloads(&v));
}

#[test]
fn test_any_showcase_rust_ueba() {
    let v = read_any_showcase_ueba("rust");
    assert_any_showcase("rust", "UEBA", decode_all_payloads(&v));
}

#[test]
fn test_any_showcase_scala_json() {
    let v = read_any_showcase_json("scala");
    assert_any_showcase("scala", "JSON", decode_all_payloads(&v));
}

#[test]
fn test_any_showcase_scala_ueba() {
    let v = read_any_showcase_ueba("scala");
    assert_any_showcase("scala", "UEBA", decode_all_payloads(&v));
}

#[test]
fn test_any_showcase_cs_json() {
    let v = read_any_showcase_json("cs");
    assert_any_showcase("cs", "JSON", decode_all_payloads(&v));
}

#[test]
fn test_any_showcase_cs_ueba() {
    let v = read_any_showcase_ueba("cs");
    assert_any_showcase("cs", "UEBA", decode_all_payloads(&v));
}

#[test]
fn test_any_showcase_python_json() {
    let path = base_dir().join("python-json/any-showcase.json");
    if !path.exists() { println!("Skipping python any-showcase JSON - file not found"); return; }
    assert_any_showcase("python", "JSON", decode_all_payloads(&read_any_showcase_json("python")));
}

#[test]
fn test_any_showcase_python_ueba() {
    let path = base_dir().join("python-ueba/any-showcase.ueba");
    if !path.exists() { println!("Skipping python any-showcase UEBA - file not found"); return; }
    assert_any_showcase("python", "UEBA", decode_all_payloads(&read_any_showcase_ueba("python")));
}

#[test]
fn test_any_showcase_typescript_json() {
    let path = base_dir().join("typescript-json/any-showcase.json");
    if !path.exists() { println!("Skipping typescript any-showcase JSON - file not found"); return; }
    assert_any_showcase("typescript", "JSON", decode_all_payloads(&read_any_showcase_json("typescript")));
}

#[test]
fn test_any_showcase_typescript_ueba() {
    let path = base_dir().join("typescript-ueba/any-showcase.ueba");
    if !path.exists() { println!("Skipping typescript any-showcase UEBA - file not found"); return; }
    assert_any_showcase("typescript", "UEBA", decode_all_payloads(&read_any_showcase_ueba("typescript")));
}

#[test]
fn test_any_showcase_kotlin_json() {
    let path = base_dir().join("kotlin-json/any-showcase.json");
    if !path.exists() { println!("Skipping kotlin any-showcase JSON - file not found"); return; }
    assert_any_showcase("kotlin", "JSON", decode_all_payloads(&read_any_showcase_json("kotlin")));
}

#[test]
fn test_any_showcase_kotlin_ueba() {
    let path = base_dir().join("kotlin-ueba/any-showcase.ueba");
    if !path.exists() { println!("Skipping kotlin any-showcase UEBA - file not found"); return; }
    assert_any_showcase("kotlin", "UEBA", decode_all_payloads(&read_any_showcase_ueba("kotlin")));
}

#[test]
fn test_any_showcase_java_json() {
    let path = base_dir().join("java-json/any-showcase.json");
    if !path.exists() { println!("Skipping java any-showcase JSON - file not found"); return; }
    assert_any_showcase("java", "JSON", decode_all_payloads(&read_any_showcase_json("java")));
}

#[test]
fn test_any_showcase_java_ueba() {
    let path = base_dir().join("java-ueba/any-showcase.ueba");
    if !path.exists() { println!("Skipping java any-showcase UEBA - file not found"); return; }
    assert_any_showcase("java", "UEBA", decode_all_payloads(&read_any_showcase_ueba("java")));
}

#[test]
fn test_any_showcase_dart_json() {
    let path = base_dir().join("dart-json/any-showcase.json");
    if !path.exists() { println!("Skipping dart any-showcase JSON - file not found"); return; }
    assert_any_showcase("dart", "JSON", decode_all_payloads(&read_any_showcase_json("dart")));
}

#[test]
fn test_any_showcase_dart_ueba() {
    let path = base_dir().join("dart-ueba/any-showcase.ueba");
    if !path.exists() { println!("Skipping dart any-showcase UEBA - file not found"); return; }
    assert_any_showcase("dart", "UEBA", decode_all_payloads(&read_any_showcase_ueba("dart")));
}

#[test]
fn test_any_showcase_swift_json() {
    let path = base_dir().join("swift-json/any-showcase.json");
    if !path.exists() { println!("Skipping swift any-showcase JSON - file not found"); return; }
    assert_any_showcase("swift", "JSON", decode_all_payloads(&read_any_showcase_json("swift")));
}

#[test]
fn test_any_showcase_swift_ueba() {
    let path = base_dir().join("swift-ueba/any-showcase.ueba");
    if !path.exists() { println!("Skipping swift any-showcase UEBA - file not found"); return; }
    assert_any_showcase("swift", "UEBA", decode_all_payloads(&read_any_showcase_ueba("swift")));
}

#[test]
fn test_any_showcase_ueba_byte_identical_rust_scala() {
    let rust_bytes = fs::read(base_dir().join("rust-ueba/any-showcase.ueba")).expect("rust UEBA");
    let scala_bytes = fs::read(base_dir().join("scala-ueba/any-showcase.ueba")).expect("scala UEBA");
    assert_eq!(rust_bytes, scala_bytes, "Rust and Scala UEBA bytes diverged");
}

#[test]
fn test_any_showcase_ueba_byte_identical_rust_cs() {
    let rust_bytes = fs::read(base_dir().join("rust-ueba/any-showcase.ueba")).expect("rust UEBA");
    let cs_bytes = fs::read(base_dir().join("cs-ueba/any-showcase.ueba")).expect("cs UEBA");
    assert_eq!(rust_bytes, cs_bytes, "Rust and C# UEBA bytes diverged");
}

// -----------------------------------------------------------------------------
// PR-I.3 (M24 Phase 3.3) — Custom-foreign `<Foreign>_KeyCodec` extension hook
//
// The m24-foreign-keycodec fixture declares a stringy custom foreign `FStr`
// (mapped to `std::string::String` in Rust) used as the inner field of an
// `ItemKey` wrapper serving as a `map[ItemKey, str]` key. The wrapper's serde
// adapter routes encode/decode through `f_str_keycodec()`, whose default
// identity impl handles stringy foreigns out of the box. Wire form is
// `{"m":{"alpha":"v1","beta":"v2"}}` — byte-identical with the other 7
// compact-emit backends.
// -----------------------------------------------------------------------------

#[test]
fn test_pr_i3_foreign_keycodec_roundtrip() {
    let path = base_dir().join("rust-json/m24-foreign-keycodec.json");
    assert!(path.exists(), "Rust m24-foreign-keycodec fixture not found: {:?}", path);
    let json_str = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {:?}: {}", path, e));
    let decoded: ForeignKeyHolder = serde_json::from_str(&json_str)
        .unwrap_or_else(|e| panic!("Failed to decode m24-foreign-keycodec JSON: {}", e));
    let mut expected_map = BTreeMap::new();
    expected_map.insert(ItemKey { v: "alpha".to_string() }, "v1".to_string());
    expected_map.insert(ItemKey { v: "beta".to_string() },  "v2".to_string());
    let expected = ForeignKeyHolder { m: expected_map };
    assert_eq!(decoded, expected, "round-trip diverged: got {:?}, expected {:?}", decoded, expected);
}

#[test]
fn test_pr_i3_foreign_keycodec_canonical_wire_form() {
    let mut m = BTreeMap::new();
    m.insert(ItemKey { v: "alpha".to_string() }, "v1".to_string());
    m.insert(ItemKey { v: "beta".to_string() },  "v2".to_string());
    let sample = ForeignKeyHolder { m };
    let encoded = serde_json::to_string(&sample).expect("ForeignKeyHolder encode");
    let expected = r#"{"m":{"alpha":"v1","beta":"v2"}}"#;
    assert_eq!(encoded, expected, "FStr_KeyCodec wire form diverged");
}

// Cross-backend byte-identity for m24-foreign-keycodec.json across the seven
// compact-emit backends. Scala and Python emit pretty (whitespace) so they are
// excluded from byte-identity (semantic equivalence is enforced by per-language
// roundtrip tests).
fn read_foreign_keycodec_bytes(source: &str) -> Option<Vec<u8>> {
    let path = base_dir().join(format!("{}-json/m24-foreign-keycodec.json", source));
    if !path.exists() {
        return None;
    }
    Some(fs::read(&path).unwrap_or_else(|e| panic!("Failed to read {:?}: {}", path, e)))
}

#[test]
fn test_pr_i3_foreign_keycodec_byte_identical_rust_cs() {
    let rust = read_foreign_keycodec_bytes("rust").expect("rust m24-foreign-keycodec.json");
    let cs   = read_foreign_keycodec_bytes("cs").expect("cs m24-foreign-keycodec.json");
    assert_eq!(rust, cs, "Rust and C# m24-foreign-keycodec.json bytes diverged");
}

#[test]
fn test_pr_i3_foreign_keycodec_byte_identical_rust_java() {
    let rust = read_foreign_keycodec_bytes("rust").expect("rust m24-foreign-keycodec.json");
    let java = read_foreign_keycodec_bytes("java").expect("java m24-foreign-keycodec.json");
    assert_eq!(rust, java, "Rust and Java m24-foreign-keycodec.json bytes diverged");
}

#[test]
fn test_pr_i3_foreign_keycodec_byte_identical_rust_kotlin() {
    let rust   = read_foreign_keycodec_bytes("rust").expect("rust m24-foreign-keycodec.json");
    let kotlin = read_foreign_keycodec_bytes("kotlin").expect("kotlin m24-foreign-keycodec.json");
    assert_eq!(rust, kotlin, "Rust and Kotlin m24-foreign-keycodec.json bytes diverged");
}

#[test]
fn test_pr_i3_foreign_keycodec_byte_identical_rust_typescript() {
    let rust = read_foreign_keycodec_bytes("rust").expect("rust m24-foreign-keycodec.json");
    let ts   = read_foreign_keycodec_bytes("typescript").expect("typescript m24-foreign-keycodec.json");
    assert_eq!(rust, ts, "Rust and TypeScript m24-foreign-keycodec.json bytes diverged");
}
