mod baboon_runtime;
include!("generated_mods.rs");

use chrono::{FixedOffset, TimeZone, Utc, NaiveDateTime};
use rust_decimal::Decimal;
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use uuid::Uuid;

use convtest::testpkg::AllBasicTypes;
use baboon_runtime::BaboonBinEncode;

fn create_sample_data() -> AllBasicTypes {
    AllBasicTypes {
        vi8: 42,
        vi16: 1234,
        vi32: 123456,
        vi64: 123456789,
        vu8: 200,
        vu16: 50000,
        vu32: 3000000000,
        vu64: 10000000000,
        vf32: 3.14159,
        vf64: 2.718281828,
        vf128: Decimal::from_str_exact("123456789.987654321").unwrap(),
        vstr: "Hello, Baboon!".to_string(),
        vbstr: vec![0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73], // "Hello Bytes"
        vuid: Uuid::parse_str("12345678-1234-5678-1234-567812345678").unwrap(),
        vbit: true,
        vtsu: {
            let dt = NaiveDateTime::from_timestamp_opt(1718451045, 123_000_000).unwrap();
            Utc.from_utc_datetime(&dt)
        },
        vtso: {
            let offset = FixedOffset::east_opt(2 * 3600).unwrap();
            let dt = NaiveDateTime::from_timestamp_opt(1718451045, 987_000_000).unwrap();
            offset.from_utc_datetime(&dt)
        },
        vopt_str: Some("optional value".to_string()),
        vlst_i32: vec![1, 2, 3, 4, 5],
        vset_str: vec!["apple".to_string(), "banana".to_string(), "cherry".to_string()]
            .into_iter()
            .collect(),
        vmap_str_i32: {
            let mut m = BTreeMap::new();
            m.insert("one".to_string(), 1);
            m.insert("two".to_string(), 2);
            m.insert("three".to_string(), 3);
            m
        },
        vopt_lst: Some(vec!["nested".to_string(), "list".to_string(), "values".to_string()]),
        vlst_opt: vec![Some(10), None, Some(20), Some(30)],
        vmap_lst: {
            let mut m = BTreeMap::new();
            m.insert("numbers".to_string(), vec![1i64, 2, 3]);
            m.insert("more".to_string(), vec![4i64, 5, 6]);
            m
        },
    }
}

fn main() {
    use rust_decimal::prelude::FromStr;

    let sample_data = create_sample_data();

    let base_dir = PathBuf::from("../../target/compat-test");
    let json_dir = base_dir.join("rust-json");
    let ueba_dir = base_dir.join("rust-ueba");

    fs::create_dir_all(&json_dir).expect("Failed to create JSON output directory");
    fs::create_dir_all(&ueba_dir).expect("Failed to create UEBA output directory");

    // Serialize to JSON
    let json_str = serde_json::to_string_pretty(&sample_data).expect("Failed to serialize to JSON");
    let json_path = json_dir.join("all-basic-types.json");
    fs::write(&json_path, &json_str).expect("Failed to write JSON file");
    println!("Written JSON to {:?}", json_path);

    // Serialize to UEBA
    let mut ueba_bytes = Vec::new();
    sample_data.baboon_encode(&mut ueba_bytes).expect("Failed to encode UEBA");
    let ueba_path = ueba_dir.join("all-basic-types.ueba");
    fs::write(&ueba_path, &ueba_bytes).expect("Failed to write UEBA file");
    println!("Written UEBA to {:?}", ueba_path);

    println!("Rust serialization complete!");
}
