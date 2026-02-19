use chrono::{FixedOffset, NaiveDate, NaiveTime, NaiveDateTime, TimeZone, Utc};
use rust_decimal::Decimal;
use std::collections::BTreeMap;
use std::fs;
use std::io::Cursor;
use std::path::PathBuf;

use baboon_conv_test_rs::convtest::testpkg::AllBasicTypes;
use baboon_conv_test_rs::baboon_runtime::{BaboonBinEncode, BaboonBinDecode, BaboonCodecContext};
use uuid::Uuid;

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
        vbstr: vec![0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73],
        vuid: Uuid::parse_str("12345678-1234-5678-1234-567812345678").unwrap(),
        vbit: true,
        vtsu: {
            let dt = NaiveDateTime::new(
                NaiveDate::from_ymd_opt(2024, 6, 15).unwrap(),
                NaiveTime::from_hms_milli_opt(12, 30, 45, 123).unwrap(),
            );
            Utc.from_utc_datetime(&dt)
        },
        vtso: {
            let offset = FixedOffset::east_opt(2 * 3600).unwrap();
            let dt = NaiveDateTime::new(
                NaiveDate::from_ymd_opt(2024, 6, 15).unwrap(),
                NaiveTime::from_hms_milli_opt(12, 30, 45, 987).unwrap(),
            );
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

fn write_json(data: &AllBasicTypes, output_dir: &str) {
    fs::create_dir_all(output_dir).expect("Failed to create output directory");
    let json_str = serde_json::to_string_pretty(data).expect("Failed to serialize to JSON");
    let json_path = PathBuf::from(output_dir).join("all-basic-types.json");
    fs::write(&json_path, &json_str).expect("Failed to write JSON file");
    println!("Written JSON to {:?}", json_path);
}

fn write_ueba(data: &AllBasicTypes, output_dir: &str) {
    let ctx = BaboonCodecContext::Default;
    fs::create_dir_all(output_dir).expect("Failed to create output directory");
    let mut ueba_bytes = Vec::new();
    data.encode_ueba(&ctx, &mut ueba_bytes).expect("Failed to encode UEBA");
    let ueba_path = PathBuf::from(output_dir).join("all-basic-types.ueba");
    fs::write(&ueba_path, &ueba_bytes).expect("Failed to write UEBA file");
    println!("Written UEBA to {:?}", ueba_path);
}

fn read_and_verify(file_path: &str) {
    let ctx = BaboonCodecContext::Default;
    let path = PathBuf::from(file_path);

    let data: AllBasicTypes = if file_path.ends_with(".json") {
        let json_str = fs::read_to_string(&path)
            .unwrap_or_else(|e| { eprintln!("Failed to read {:?}: {}", path, e); std::process::exit(1); });
        serde_json::from_str(&json_str)
            .unwrap_or_else(|e| { eprintln!("Failed to parse JSON from {:?}: {}", path, e); std::process::exit(1); })
    } else if file_path.ends_with(".ueba") {
        let bytes = fs::read(&path)
            .unwrap_or_else(|e| { eprintln!("Failed to read {:?}: {}", path, e); std::process::exit(1); });
        let mut cursor = Cursor::new(&bytes);
        AllBasicTypes::decode_ueba(&ctx, &mut cursor)
            .unwrap_or_else(|e| { eprintln!("Failed to decode UEBA from {:?}: {}", path, e); std::process::exit(1); })
    } else {
        eprintln!("Unknown file extension: {}", file_path);
        std::process::exit(1);
    };

    if data.vstr != "Hello, Baboon!" {
        eprintln!("vstr mismatch: expected 'Hello, Baboon!', got '{}'", data.vstr);
        std::process::exit(1);
    }
    if data.vi32 != 123456 {
        eprintln!("vi32 mismatch: expected 123456, got {}", data.vi32);
        std::process::exit(1);
    }
    if !data.vbit {
        eprintln!("vbit mismatch: expected true, got {}", data.vbit);
        std::process::exit(1);
    }

    // Roundtrip
    if file_path.ends_with(".json") {
        let re_encoded = serde_json::to_string(&data)
            .unwrap_or_else(|e| { eprintln!("JSON re-encode failed: {}", e); std::process::exit(1); });
        let re_decoded: AllBasicTypes = serde_json::from_str(&re_encoded)
            .unwrap_or_else(|e| { eprintln!("JSON roundtrip decode failed: {}", e); std::process::exit(1); });
        if data != re_decoded {
            eprintln!("JSON roundtrip mismatch");
            std::process::exit(1);
        }
    } else {
        let mut re_bytes = Vec::new();
        data.encode_ueba(&ctx, &mut re_bytes)
            .unwrap_or_else(|e| { eprintln!("UEBA re-encode failed: {}", e); std::process::exit(1); });
        let mut cursor = Cursor::new(&re_bytes);
        let re_decoded = AllBasicTypes::decode_ueba(&ctx, &mut cursor)
            .unwrap_or_else(|e| { eprintln!("UEBA roundtrip decode failed: {}", e); std::process::exit(1); });
        if data != re_decoded {
            eprintln!("UEBA roundtrip mismatch");
            std::process::exit(1);
        }
    }

    println!("OK");
}

fn run_legacy() {
    let sample_data = create_sample_data();

    let base_dir = PathBuf::from("../../target/compat-test");
    write_json(&sample_data, base_dir.join("rust-json").to_str().unwrap());
    write_ueba(&sample_data, base_dir.join("rust-ueba").to_str().unwrap());

    println!("Rust serialization complete!");
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    match args.first().map(|s| s.as_str()) {
        Some("write") => {
            let output_dir = &args[1];
            let format = &args[2];
            let sample_data = create_sample_data();
            match format.as_str() {
                "json" => write_json(&sample_data, output_dir),
                "ueba" => write_ueba(&sample_data, output_dir),
                _ => {
                    eprintln!("Unknown format: {}", format);
                    std::process::exit(1);
                }
            }
        }
        Some("read") => {
            read_and_verify(&args[1]);
        }
        _ => {
            run_legacy();
        }
    }
}
