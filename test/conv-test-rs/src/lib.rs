mod baboon_runtime;
include!("generated_mods.rs");

#[cfg(test)]
mod tests {
    use super::*;
    use convtest::testpkg::AllBasicTypes;
    use baboon_runtime::BaboonBinDecode;
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
        let mut cursor = std::io::Cursor::new(&bytes);
        AllBasicTypes::baboon_decode(&mut cursor)
            .unwrap_or_else(|e| panic!("Failed to decode UEBA from {:?}: {}", path, e))
    }

    fn assert_basic_fields(data: &AllBasicTypes, label: &str) {
        println!("Successfully decoded {}: {}", label, data.vstr);
        assert_eq!(data.vstr, "Hello, Baboon!");
        assert_eq!(data.vi32, 123456);
        assert!(data.vbit);
    }

    // JSON Tests
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

    // UEBA Tests
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

    // Cross-language JSON comparison
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

    // Cross-language UEBA comparison
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
}
