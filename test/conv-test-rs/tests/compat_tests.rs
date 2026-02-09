use baboon_conv_test_rs::convtest::testpkg::AllBasicTypes;
use baboon_conv_test_rs::baboon_runtime::{BaboonBinDecode, BaboonCodecContext};
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
#[ignore = "Python has known precision issues: microsecond timestamps, f64 decimal, lowercase hex"]
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
