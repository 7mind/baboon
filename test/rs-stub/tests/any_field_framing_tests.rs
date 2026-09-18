use baboon_rs_stub::any_opaque::{any_field_codec::{decode_any_field, encode_any_field}, AnyMeta, AnyOpaque, AnyOpaqueUeba};
use baboon_rs_stub::baboon_runtime::BaboonCodecContext;
use std::io::{Cursor, Write};

#[test]
fn all_kinds_round_trip_with_stable_framing() {
    for kind in [0x00, 0x01, 0x02, 0x03, 0x06, 0x07] {
        let meta = AnyMeta::new(kind,
            (kind & 4 != 0).then(|| "domain".to_owned()),
            (kind & 2 != 0).then(|| "1.0.0".to_owned()),
            (kind & 1 != 0).then(|| "type".to_owned()),
        ).unwrap();
        let original = AnyOpaqueUeba::new(meta, vec![1, 2, 3]);
        let mut encoded = Vec::new();
        encode_any_field(&BaboonCodecContext::Default, &mut encoded, kind, None, None, None, &AnyOpaque::Ueba(original.clone())).unwrap();
        let decoded = decode_any_field(&mut Cursor::new(&encoded), kind).unwrap();
        assert_eq!(decoded, original);
        if kind == 0 {
            assert_eq!(encoded, vec![8, 0, 0, 0, 1, 0, 0, 0, 0, 1, 2, 3]);
        }
    }
}

#[test]
fn native_payload_is_written_without_a_payload_copy() {
    struct ObservingWriter { payload: *const u8, observed: bool }
    impl Write for ObservingWriter {
        fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
            self.observed |= bytes.as_ptr() == self.payload;
            Ok(bytes.len())
        }
        fn flush(&mut self) -> std::io::Result<()> { Ok(()) }
    }
    let inner = AnyOpaqueUeba::new(AnyMeta::new(0, None, None, None).unwrap(), vec![1, 2, 3]);
    let mut writer = ObservingWriter { payload: inner.bytes.as_ptr(), observed: false };
    encode_any_field(&BaboonCodecContext::Default, &mut writer, 0, None, None, None, &AnyOpaque::Ueba(inner)).unwrap();
    assert!(writer.observed);
}

#[test]
fn extension_bytes_are_skipped_and_length_errors_remain_distinct() {
    let wire = vec![10, 0, 0, 0, 3, 0, 0, 0, 0, 99, 98, 1, 2, 3];
    assert_eq!(decode_any_field(&mut Cursor::new(wire), 0).unwrap().bytes, vec![1, 2, 3]);
    let cases = [
        (vec![255, 255, 255, 255], "any: negative total-length -1"),
        (vec![8, 0, 0, 0, 255, 255, 255, 255], "any: negative meta-length -1"),
        (vec![4, 0, 0, 0, 1, 0, 0, 0], "any: total-length 4 smaller than 4 + meta-length 1"),
    ];
    for (wire, message) in cases {
        assert_eq!(decode_any_field(&mut Cursor::new(wire), 0).unwrap_err().to_string(), message);
    }
}
