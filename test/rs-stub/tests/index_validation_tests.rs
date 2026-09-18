use baboon_rs_stub::baboon_runtime::{BaboonBinCodecIndexed, BaboonCodecContext};
use std::io::Cursor;

struct TwoEntries;
impl BaboonBinCodecIndexed for TwoEntries {
    fn index_elements_count(_ctx: &BaboonCodecContext) -> u16 { 2 }
}

#[test]
fn invalid_entries_return_errors_instead_of_panicking() {
    for entries in [[0, 0, 1, 1], [-1, 1, 1, 1], [0, -1, 1, 1], [0, 2, 1, 1], [i32::MAX, 1, 0, 1]] {
        let mut bytes = vec![1u8];
        for value in entries { bytes.extend_from_slice(&value.to_le_bytes()); }
        assert!(TwoEntries::read_index(&BaboonCodecContext::Compact, &mut Cursor::new(bytes)).is_err());
    }
}

#[test]
fn valid_entries_preserve_gaps_and_leave_payload_unconsumed() {
    let mut bytes = vec![1u8];
    for value in [1i32, 2, 5, 3] { bytes.extend_from_slice(&value.to_le_bytes()); }
    bytes.push(42);
    let mut input = Cursor::new(bytes);
    let (_, entries) = TwoEntries::read_index(&BaboonCodecContext::Compact, &mut input).unwrap();
    assert_eq!(entries.len(), 2);
    assert_eq!((entries[0].offset, entries[0].length), (1, 2));
    assert_eq!((entries[1].offset, entries[1].length), (5, 3));
    assert_eq!(input.position(), 17);
}
