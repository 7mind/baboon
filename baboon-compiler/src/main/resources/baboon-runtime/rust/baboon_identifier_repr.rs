// Runtime helpers for the `id` toString / parseRepr machinery defined in
// docs/spec/identifier-repr.md. The Rust backend (PR-57c) uses these helpers
// from emitted code; conformance to the spec is the contract.
//
// Mirrors BaboonIdentifierRepr.{java,kt} (PR-57a/b) in API and behaviour.
// Result type: `Result<T, String>` (Rust idiomatic). NOT `Either`. Errors are
// human-readable strings — same content as the JVM-side helpers — so test
// assertions across backends can match on the same substrings.

#![allow(dead_code)]

use std::fmt::Write;

#[cfg(feature = "timestamps")]
use chrono::{DateTime, FixedOffset, NaiveDateTime, Offset, TimeZone, Utc};

// Defensive: numeric Char constants rather than quoted-character literals so the
// emitted source survives any future template-escape pass (mirrors PR-19-D01 /
// PR-20-D01 sister-bug lessons).
const BS:  char = '\\';
const HSH: char = '#';
const COL: char = ':';
const OBR: char = '{';
const CBR: char = '}';

/// Backslash-escape the 5 metacharacters per spec §4.2.
pub fn escape_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        if c == BS || c == HSH || c == COL || c == OBR || c == CBR {
            out.push(BS);
        }
        out.push(c);
    }
    out
}

/// Lowercase hex, no separators, per spec §3 / §4.4.
pub fn bytes_to_hex(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        let _ = write!(out, "{:02x}", b);
    }
    out
}

/// Render a `tsu` per spec §3: RFC 3339 UTC, milliseconds, trailing `Z`,
/// exactly 24 characters. Normalises the input to UTC.
#[cfg(feature = "timestamps")]
pub fn tsu_to_string(dt: &DateTime<Utc>) -> String {
    dt.format("%Y-%m-%dT%H:%M:%S%.3fZ").to_string()
}

/// Render a `tso` per spec §3: RFC 3339 with explicit `±HH:MM` offset,
/// milliseconds, exactly 29 characters. Never emits `Z` shorthand.
#[cfg(feature = "timestamps")]
pub fn tso_to_string(dt: &DateTime<FixedOffset>) -> String {
    dt.format("%Y-%m-%dT%H:%M:%S%.3f%:z").to_string()
}

/// Render an unsigned-i64 as decimal. Rust's u64 is natively unsigned; this is
/// just `to_string`. Wrapper exists for parity with JVM helpers (which carry
/// u64 in a signed Long).
pub fn u64_to_string(v: u64) -> String {
    v.to_string()
}

/// Render a `bit` per spec §3 — exact lowercase ASCII.
pub fn bit_to_string(b: bool) -> String {
    (if b { "true" } else { "false" }).to_string()
}

#[cfg(feature = "timestamps")]
pub fn parse_tsu_repr(s: &str) -> Result<DateTime<Utc>, String> {
    if s.len() != 24 {
        return Err(format!("tsu repr must be 24 chars, got {}", s.len()));
    }
    if !s.ends_with('Z') {
        return Err(format!("tsu repr must end with 'Z', got: {}", s));
    }
    // Strip the trailing 'Z' and parse the naive datetime; literal 'Z' on
    // chrono's RFC-3339 parser is recognised but the format we need is fixed
    // 24-char with 3-digit millis.
    let body = &s[..s.len() - 1];
    match NaiveDateTime::parse_from_str(body, "%Y-%m-%dT%H:%M:%S%.3f") {
        Ok(naive) => Ok(Utc.from_utc_datetime(&naive)),
        Err(_) => Err(format!("could not parse tsu: {}", s)),
    }
}

#[cfg(feature = "timestamps")]
pub fn parse_tso_repr(s: &str) -> Result<DateTime<FixedOffset>, String> {
    if s.len() != 29 {
        return Err(format!("tso repr must be 29 chars, got {}", s.len()));
    }
    match DateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S%.3f%:z") {
        Ok(dt) => Ok(dt),
        Err(_) => Err(format!("could not parse tso: {}", s)),
    }
}

/// Decode `bytes` from lowercase hex. Empty string is legal (empty bytes).
pub fn parse_bytes_hex(s: &str) -> Result<Vec<u8>, String> {
    if s.is_empty() {
        return Ok(Vec::new());
    }
    if (s.len() & 1) != 0 {
        return Err(format!("odd-length hex sequence: {}", s));
    }
    // Spec mandates lowercase; reject uppercase to keep exactly one canonical form.
    let bytes = s.as_bytes();
    for &b in bytes {
        if !((b >= b'0' && b <= b'9') || (b >= b'a' && b <= b'f')) {
            return Err(format!("non-lowercase or non-hex character in: {}", s));
        }
    }
    let mut out = Vec::with_capacity(s.len() / 2);
    let mut i = 0;
    while i < bytes.len() {
        let hi = hex_digit(bytes[i])
            .ok_or_else(|| format!("non-lowercase or non-hex character in: {}", s))?;
        let lo = hex_digit(bytes[i + 1])
            .ok_or_else(|| format!("non-lowercase or non-hex character in: {}", s))?;
        out.push((hi << 4) | lo);
        i += 2;
    }
    Ok(out)
}

fn hex_digit(b: u8) -> Option<u8> {
    if b >= b'0' && b <= b'9' { Some(b - b'0') }
    else if b >= b'a' && b <= b'f' { Some(10 + (b - b'a')) }
    else { None }
}

pub fn parse_bit(s: &str) -> Result<bool, String> {
    match s {
        "true" => Ok(true),
        "false" => Ok(false),
        other => Err(format!("expected 'true' or 'false' but found '{}'", other)),
    }
}

/// Lowercase canonical-form check for uid strings (spec §3 / §5.4):
/// `[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}`.
pub fn is_canonical_uid(s: &str) -> bool {
    let bytes = s.as_bytes();
    if bytes.len() != 36 {
        return false;
    }
    // Hyphen positions per RFC 4122 canonical form.
    const DASH_POS: [usize; 4] = [8, 13, 18, 23];
    for &p in DASH_POS.iter() {
        if bytes[p] != b'-' {
            return false;
        }
    }
    for (i, &b) in bytes.iter().enumerate() {
        if DASH_POS.contains(&i) {
            continue;
        }
        let is_lower_hex =
            (b >= b'0' && b <= b'9') || (b >= b'a' && b <= b'f');
        if !is_lower_hex {
            return false;
        }
    }
    true
}

/// Cursor-based parser for parseRepr decoders. Schema-directed; the caller
/// (the emitted `<TypeName>Codec.parse_repr`) drives the field sequence per
/// declared type and order.
///
/// Owns its source as a `&str` borrow. All `read_*` methods return `String`
/// (allocated) rather than `&str` to keep the public API simple — callers
/// usually need to parse the slice further (parse_bytes_hex, NaiveDateTime,
/// integer parsers) and the allocation cost is dominated by those parsers.
pub struct Cursor<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(source: &'a str) -> Self {
        Cursor { source, pos: 0 }
    }

    pub fn position(&self) -> usize {
        self.pos
    }

    pub fn at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn peek_byte(&self) -> Option<u8> {
        self.source.as_bytes().get(self.pos).copied()
    }

    pub fn expect(&mut self, c: char) -> Result<(), String> {
        // All structural metachars (`:`, `#`, `{`, `}`, `\`) are single-byte ASCII;
        // we operate on bytes for cursor efficiency.
        let cb = c as u32;
        assert!(cb < 128, "Cursor::expect only handles ASCII metachars");
        match self.peek_byte() {
            Some(b) if b as u32 == cb => {
                self.pos += 1;
                Ok(())
            }
            Some(b) => Err(format!(
                "expected '{}' at {} but found '{}'",
                c, self.pos, b as char
            )),
            None => Err(format!(
                "expected '{}' at {} but reached end of input",
                c, self.pos
            )),
        }
    }

    pub fn expect_literal(&mut self, lit: &str) -> Result<(), String> {
        let bytes = self.source.as_bytes();
        let lit_bytes = lit.as_bytes();
        if self.pos + lit_bytes.len() > bytes.len() {
            return Err(format!(
                "expected literal '{}' at {} but reached end of input",
                lit, self.pos
            ));
        }
        for i in 0..lit_bytes.len() {
            if bytes[self.pos + i] != lit_bytes[i] {
                return Err(format!("expected literal '{}' at {}", lit, self.pos));
            }
        }
        self.pos += lit_bytes.len();
        Ok(())
    }

    /// Read until the next bare metachar in `:#{}`. Backslash escapes are NOT
    /// processed here — see read_str_field for that. Used for primitive
    /// consumption (numbers, uuids, hex bytes).
    pub fn read_until_structural(&mut self) -> String {
        let bytes = self.source.as_bytes();
        let start = self.pos;
        while self.pos < bytes.len() {
            let b = bytes[self.pos];
            if b == COL as u8 || b == HSH as u8 || b == OBR as u8 || b == CBR as u8 {
                break;
            }
            self.pos += 1;
        }
        // SAFETY: only ASCII single-byte metachars are stop conditions; the slice
        // is therefore on a UTF-8 boundary.
        self.source[start..self.pos].to_string()
    }

    /// Consume exactly n characters (Unicode scalars, NOT bytes) as a fixed-width
    /// lexeme. Used for tsu/tso (which contain `:` characters inside the lexeme).
    /// Spec §5.4 mandates fixed-width consumption; tsu = 24 chars, tso = 29 chars.
    /// Both lexemes are pure ASCII so char count and byte count coincide, but we
    /// walk Unicode scalars for safety against malformed input.
    pub fn read_fixed(&mut self, n: usize) -> Result<String, String> {
        let s = &self.source[self.pos..];
        let mut taken = 0usize;
        let mut end_byte = 0usize;
        for (idx, ch) in s.char_indices() {
            if taken == n {
                end_byte = idx;
                break;
            }
            taken += 1;
            end_byte = idx + ch.len_utf8();
        }
        if taken < n {
            return Err(format!(
                "expected {} chars at {} but only {} remain",
                n, self.pos, taken
            ));
        }
        let result = s[..end_byte].to_string();
        self.pos += end_byte;
        Ok(result)
    }

    /// Read a `str` field value with backslash-unescaping per spec §5.5.
    ///
    /// All metachars are single-byte ASCII; we operate on bytes inside the
    /// scan loop and append non-metachar UTF-8 chars verbatim.
    pub fn read_str_field(&mut self) -> Result<String, String> {
        let bytes = self.source.as_bytes();
        let mut out = String::new();
        while self.pos < bytes.len() {
            let b = bytes[self.pos];
            if b == COL as u8 || b == HSH as u8 || b == OBR as u8 || b == CBR as u8 {
                return Ok(out);
            }
            if b == BS as u8 {
                if self.pos + 1 >= bytes.len() {
                    return Err(format!("trailing backslash at {}", self.pos));
                }
                let nxt = bytes[self.pos + 1];
                if nxt == BS as u8 || nxt == HSH as u8 || nxt == COL as u8
                    || nxt == OBR as u8 || nxt == CBR as u8
                {
                    out.push(nxt as char);
                    self.pos += 2;
                } else {
                    return Err(format!("invalid escape at {}", self.pos));
                }
            } else {
                // Walk the next UTF-8 char (which may be multi-byte) and append.
                let s = &self.source[self.pos..];
                if let Some((_, ch)) = s.char_indices().next() {
                    out.push(ch);
                    self.pos += ch.len_utf8();
                } else {
                    break;
                }
            }
        }
        Ok(out)
    }
}

/// Validate header of an identifier-repr: `<simpleName>:<version>#`.
pub fn parse_header(
    cursor: &mut Cursor<'_>,
    expected_simple_name: &str,
    expected_version: &str,
) -> Result<(), String> {
    let name_lit = cursor.read_until_structural();
    if name_lit != expected_simple_name {
        return Err(format!(
            "expected name '{}' but found '{}'",
            expected_simple_name, name_lit
        ));
    }
    cursor.expect(':')?;
    let ver_lit = cursor.read_until_structural();
    if ver_lit != expected_version {
        return Err(format!(
            "expected version '{}' but found '{}'",
            expected_version, ver_lit
        ));
    }
    cursor.expect('#')
}

/// Validate field-name segment: `<expectedFieldName>:`.
pub fn parse_field_name(
    cursor: &mut Cursor<'_>,
    expected_field_name: &str,
) -> Result<(), String> {
    let name = cursor.read_until_structural();
    if name != expected_field_name {
        return Err(format!(
            "expected field name '{}' but found '{}'",
            expected_field_name, name
        ));
    }
    cursor.expect(':')
}
