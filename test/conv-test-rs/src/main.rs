use chrono::{FixedOffset, NaiveDate, NaiveTime, NaiveDateTime, TimeZone, Utc};
use rust_decimal::Decimal;
use std::collections::BTreeMap;
use std::fs;
use std::io::Cursor;
use std::path::PathBuf;

use baboon_conv_test_rs::convtest::testpkg::{AllBasicTypes, AnyShowcase, CompositeId, InnerPayload, ItemId, PointId, WireEnum};
use baboon_conv_test_rs::any_opaque::{AnyMeta, AnyOpaque, AnyOpaqueJson, AnyOpaqueUeba};
use baboon_conv_test_rs::baboon_runtime::{BaboonBinEncode, BaboonBinDecode, BaboonCodecContext};
use uuid::Uuid;

// Domain constants — match the convtest.testpkg domain at version 2.0.0 (where AnyShowcase + InnerPayload live).
const DOMAIN_ID: &str = "convtest.testpkg";
const DOMAIN_VER: &str = "2.0.0";
const INNER_TYPE_ID: &str = "convtest.testpkg/:#InnerPayload";

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
        // Non-Pascal-case enum member; canonical JSON wire form is "Cafe" (PR-35-D06 regression guard).
        v_wire_enum: WireEnum::Cafe,
        // Identifier (PR-57e). Wire form is `{"x": 42, "y": -7}` on JSON and two
        // i32 LE values on UEBA — byte-identical to a `data` of the same shape
        // per docs/spec/identifier-repr.md §1.3 / §7.
        v_point_id: PointId { x: 42, y: -7 },
        // PR-61 (M19.3) — id types as JSON map keys. Per PR-60 (M19.2) all id
        // types — single- or multi-field — use canonical repr toString as the
        // key form: e.g. `ItemId:2.0.0#v:00000000-0000-0000-0000-000000000001`.
        // Canonical deterministic uuids ensure cross-language byte-identity.
        vmap_item_id_u32: {
            let mut m = BTreeMap::new();
            m.insert(
                ItemId { v: Uuid::parse_str("00000000-0000-0000-0000-000000000001").unwrap() },
                1u32,
            );
            m.insert(
                ItemId { v: Uuid::parse_str("00000000-0000-0000-0000-000000000002").unwrap() },
                2u32,
            );
            m
        },
        vmap_composite_id_u32: {
            let mut m = BTreeMap::new();
            m.insert(
                CompositeId {
                    tenant: Uuid::parse_str("00000000-0000-0000-0000-0000000000aa").unwrap(),
                    user:   Uuid::parse_str("00000000-0000-0000-0000-0000000000bb").unwrap(),
                },
                100u32,
            );
            m.insert(
                CompositeId {
                    tenant: Uuid::parse_str("00000000-0000-0000-0000-0000000000cc").unwrap(),
                    user:   Uuid::parse_str("00000000-0000-0000-0000-0000000000dd").unwrap(),
                },
                200u32,
            );
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

// Expected logical InnerPayload contents per AnyShowcase slot, in deterministic order:
// [vAnyA, vAnyB, vAnyC, vAnyD1, vAnyD2, vAnyD3, optAny, lstAny[0]].
// Must match the Scala/C# fixture exactly so cross-language reads produce the same payloads.
fn expected_inner_payloads() -> Vec<InnerPayload> {
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

// Build an AnyShowcase fixture for the JSON wire format. Every slot uses AnyOpaque::Json so
// the serde Serialize impl can emit the locked envelope without requiring a facade-registered
// InnerPayload codec. Rust's codegen does not currently emit per-domain BaboonCodecsJson/Ueba
// aggregators (cf. Scala's BaboonCodecsJson / C#'s BaboonCodecsJson), so cross-format
// conversion through the facade is unavailable here. Single-branch emission is sufficient for
// the cross-language wire-format compatibility test (the per-language stub tests already
// cover cross-format conversion within Rust).
fn create_sample_any_showcase_json() -> AnyShowcase {
    let payloads = expected_inner_payloads();
    let to_json = |p: &InnerPayload| serde_json::to_value(p).expect("InnerPayload to_json failed");

    let meta_a  = AnyMeta::new(0x07, Some(DOMAIN_ID.to_string()), Some(DOMAIN_VER.to_string()), Some(INNER_TYPE_ID.to_string())).expect("metaA");
    let meta_b  = AnyMeta::new(0x03, None, Some(DOMAIN_VER.to_string()), Some(INNER_TYPE_ID.to_string())).expect("metaB");
    let meta_c  = AnyMeta::new(0x01, None, None, Some(INNER_TYPE_ID.to_string())).expect("metaC");
    let meta_d1 = AnyMeta::new(0x06, Some(DOMAIN_ID.to_string()), Some(DOMAIN_VER.to_string()), None).expect("metaD1");
    let meta_d2 = AnyMeta::new(0x02, None, Some(DOMAIN_VER.to_string()), None).expect("metaD2");
    let meta_d3 = AnyMeta::new(0x00, None, None, None).expect("metaD3");
    let meta_opt = AnyMeta::new(0x07, Some(DOMAIN_ID.to_string()), Some(DOMAIN_VER.to_string()), Some(INNER_TYPE_ID.to_string())).expect("metaOpt");
    let meta_lst = AnyMeta::new(0x06, Some(DOMAIN_ID.to_string()), Some(DOMAIN_VER.to_string()), None).expect("metaLst");

    AnyShowcase {
        v_any_a:  AnyOpaque::Json(AnyOpaqueJson::new(meta_a,  to_json(&payloads[0]))),
        v_any_b:  AnyOpaque::Json(AnyOpaqueJson::new(meta_b,  to_json(&payloads[1]))),
        v_any_c:  AnyOpaque::Json(AnyOpaqueJson::new(meta_c,  to_json(&payloads[2]))),
        v_any_d1: AnyOpaque::Json(AnyOpaqueJson::new(meta_d1, to_json(&payloads[3]))),
        v_any_d2: AnyOpaque::Json(AnyOpaqueJson::new(meta_d2, to_json(&payloads[4]))),
        v_any_d3: AnyOpaque::Json(AnyOpaqueJson::new(meta_d3, to_json(&payloads[5]))),
        opt_any:  Some(AnyOpaque::Json(AnyOpaqueJson::new(meta_opt, to_json(&payloads[6])))),
        lst_any:  vec![AnyOpaque::Json(AnyOpaqueJson::new(meta_lst, to_json(&payloads[7])))],
    }
}

// Build an AnyShowcase fixture for the UEBA wire format. Every slot uses AnyOpaque::Ueba with
// pre-computed InnerPayload UEBA bytes so the encode_ueba path reuses the bytes verbatim
// without requiring a facade-registered InnerPayload codec.
fn create_sample_any_showcase_ueba() -> AnyShowcase {
    let payloads = expected_inner_payloads();
    let ctx = BaboonCodecContext::Compact;
    let to_ueba = |p: &InnerPayload| -> Vec<u8> {
        let mut buf: Vec<u8> = Vec::new();
        p.encode_ueba(&ctx, &mut buf).expect("InnerPayload encode_ueba failed");
        buf
    };

    let meta_a  = AnyMeta::new(0x07, Some(DOMAIN_ID.to_string()), Some(DOMAIN_VER.to_string()), Some(INNER_TYPE_ID.to_string())).expect("metaA");
    let meta_b  = AnyMeta::new(0x03, None, Some(DOMAIN_VER.to_string()), Some(INNER_TYPE_ID.to_string())).expect("metaB");
    let meta_c  = AnyMeta::new(0x01, None, None, Some(INNER_TYPE_ID.to_string())).expect("metaC");
    let meta_d1 = AnyMeta::new(0x06, Some(DOMAIN_ID.to_string()), Some(DOMAIN_VER.to_string()), None).expect("metaD1");
    let meta_d2 = AnyMeta::new(0x02, None, Some(DOMAIN_VER.to_string()), None).expect("metaD2");
    let meta_d3 = AnyMeta::new(0x00, None, None, None).expect("metaD3");
    let meta_opt = AnyMeta::new(0x07, Some(DOMAIN_ID.to_string()), Some(DOMAIN_VER.to_string()), Some(INNER_TYPE_ID.to_string())).expect("metaOpt");
    let meta_lst = AnyMeta::new(0x06, Some(DOMAIN_ID.to_string()), Some(DOMAIN_VER.to_string()), None).expect("metaLst");

    AnyShowcase {
        v_any_a:  AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_a,  to_ueba(&payloads[0]))),
        v_any_b:  AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_b,  to_ueba(&payloads[1]))),
        v_any_c:  AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_c,  to_ueba(&payloads[2]))),
        v_any_d1: AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_d1, to_ueba(&payloads[3]))),
        v_any_d2: AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_d2, to_ueba(&payloads[4]))),
        v_any_d3: AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_d3, to_ueba(&payloads[5]))),
        opt_any:  Some(AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_opt, to_ueba(&payloads[6])))),
        lst_any:  vec![AnyOpaque::Ueba(AnyOpaqueUeba::new(meta_lst, to_ueba(&payloads[7])))],
    }
}

fn write_json_any(data: &AnyShowcase, output_dir: &str) {
    fs::create_dir_all(output_dir).expect("Failed to create output directory");
    let json_str = serde_json::to_string_pretty(data).expect("Failed to serialize AnyShowcase to JSON");
    let path = PathBuf::from(output_dir).join("any-showcase.json");
    fs::write(&path, &json_str).expect("Failed to write AnyShowcase JSON");
    println!("Written JSON to {:?}", path);
}

fn write_ueba_any(data: &AnyShowcase, output_dir: &str) {
    let ctx = BaboonCodecContext::Default;
    fs::create_dir_all(output_dir).expect("Failed to create output directory");
    let mut buf: Vec<u8> = Vec::new();
    data.encode_ueba(&ctx, &mut buf).expect("Failed to encode AnyShowcase UEBA");
    let path = PathBuf::from(output_dir).join("any-showcase.ueba");
    fs::write(&path, &buf).expect("Failed to write AnyShowcase UEBA");
    println!("Written UEBA to {:?}", path);
}

fn read_and_verify(file_path: &str) {
    if file_path.ends_with("any-showcase.json") || file_path.ends_with("any-showcase.ueba") {
        read_and_verify_any_showcase(file_path);
        return;
    }
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

fn read_and_verify_any_showcase(file_path: &str) {
    // Decode-side does not need a facade: AnyShowcase decode produces only same-branch
    // AnyOpaque values (JSON wire → AnyOpaque::Json, UEBA wire → AnyOpaque::Ueba). We then
    // decode the inner payload directly via InnerPayload's codec functions.
    let ctx = BaboonCodecContext::Default;
    let path = PathBuf::from(file_path);
    let data: AnyShowcase = if file_path.ends_with(".json") {
        let json_str = fs::read_to_string(&path)
            .unwrap_or_else(|e| { eprintln!("Failed to read {:?}: {}", path, e); std::process::exit(1); });
        serde_json::from_str(&json_str)
            .unwrap_or_else(|e| { eprintln!("AnyShowcase JSON decode failed: {}", e); std::process::exit(1); })
    } else {
        let bytes = fs::read(&path)
            .unwrap_or_else(|e| { eprintln!("Failed to read {:?}: {}", path, e); std::process::exit(1); });
        let mut cursor = Cursor::new(&bytes);
        AnyShowcase::decode_ueba(&ctx, &mut cursor)
            .unwrap_or_else(|e| { eprintln!("AnyShowcase UEBA decode failed: {}", e); std::process::exit(1); })
    };

    let expected = expected_inner_payloads();
    let decoded = decode_all_payloads(&data);
    for (i, (exp, got)) in expected.iter().zip(decoded.iter()).enumerate() {
        if exp != got {
            eprintln!("AnyShowcase payload {} mismatch: expected {:?}, got {:?}", i, exp, got);
            std::process::exit(1);
        }
    }
    println!("OK");
}

fn decode_inner(o: &AnyOpaque) -> InnerPayload {
    match o {
        AnyOpaque::Ueba(u) => {
            let mut cursor = Cursor::new(&u.bytes);
            InnerPayload::decode_ueba(&BaboonCodecContext::Compact, &mut cursor)
                .unwrap_or_else(|e| panic!("InnerPayload UEBA decode failed: {}", e))
        }
        AnyOpaque::Json(j) => {
            serde_json::from_value(j.json.clone())
                .unwrap_or_else(|e| panic!("InnerPayload JSON decode failed: {}", e))
        }
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

// PR-57e (M18.4e) — cross-language identifier repr (Display) byte-identity.
// Per spec §7 the repr form is a separate invariant from the JSON/UEBA wire bytes;
// we write it as a per-language artifact so the Scala-side test can assert all 10 backends
// produce byte-identical output for the same canonical PointId value.
fn write_point_id_repr(pid: &PointId, output_dir: &str) {
    fs::create_dir_all(output_dir).expect("Failed to create repr output directory");
    let path = PathBuf::from(output_dir).join("point-id.txt");
    // No trailing newline — exact byte match across all languages.
    fs::write(&path, format!("{}", pid)).expect("Failed to write PointId repr file");
    println!("Written repr to {:?}", path);
}

fn run_legacy() {
    let sample_data = create_sample_data();

    let base_dir = PathBuf::from("../../target/compat-test");
    let rust_json_dir = base_dir.join("rust-json");
    let rust_ueba_dir = base_dir.join("rust-ueba");
    let rust_repr_dir = base_dir.join("rust-repr");
    write_json(&sample_data, rust_json_dir.to_str().unwrap());
    write_ueba(&sample_data, rust_ueba_dir.to_str().unwrap());

    let any_json = create_sample_any_showcase_json();
    let any_ueba = create_sample_any_showcase_ueba();
    write_json_any(&any_json, rust_json_dir.to_str().unwrap());
    write_ueba_any(&any_ueba, rust_ueba_dir.to_str().unwrap());

    write_point_id_repr(&sample_data.v_point_id, rust_repr_dir.to_str().unwrap());

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
                "json" => {
                    write_json(&sample_data, output_dir);
                    write_json_any(&create_sample_any_showcase_json(), output_dir);
                }
                "ueba" => {
                    write_ueba(&sample_data, output_dir);
                    write_ueba_any(&create_sample_any_showcase_ueba(), output_dir);
                }
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
