# UEBA (Ultra‑Efficient Binary Aggregate) format

This document captures the UEBA wire conventions used by generated Baboon codecs. Everything is little‑endian and intentionally simple so C# and Scala implementations stay identical.

## Framing

- Writers use `LEDataOutputStream`; readers use `LEDataInputStream`.
- No global envelope; each value is written directly in place (top‑level or nested).
- When index mode is enabled (`BaboonCodecContext.Indexed`), codecs may emit a one‑byte header with index data; otherwise, codecs keep payloads compact.

## Primitive encodings

- `bit`: 1 byte, 0 or 1.
- Signed integers (`i08`, `i16`, `i32`, `i64`): little‑endian two’s‑complement of the respective width.
- Unsigned integers (`u08`, `u16`, `u32`, `u64`): little‑endian representation of the unsigned value (read/write via standard signed primitives and reinterpret).
- Floats (`f32`, `f64`): IEEE‑754 little‑endian.
- `f128`: serialized as .NET `decimal` (16 bytes: lo, mid, hi, flags). Flags carry sign bit 31 and scale in bits 16–23; mantissa fits 96 bits.
- `str`: UTF‑8 bytes prefixed by a varint length (LEB128, 7 bits per byte, continuation bit 0x80).
- `bytes`: length `i32` then raw bytes.
- `uid`: 16 bytes in .NET GUID mixed‑endian layout (fields Data1/2/3 little‑endian, Data4 big‑endian).
- `tsu`/`tso`: two `i64` values (milliseconds since epoch, offset millis) plus one `i8` “kind” flag.

## Collections

- `opt[T]`: one `byte` tag (`0` = empty, `1` = present). Present payload encodes `T`.
- `lst[T]` / `set[T]`: `i32` count, then elements in order.
- `map[K, V]`: `i32` count, then key/value pairs in order: `K` then `V` per entry.

## User types

- Enums: encoded as `i32` of the discriminant.
- DTO/ADT/contract branches: fields encoded in declaration order.
- ADT branches: written as the branch payload followed by branch metadata when wrapped codecs are enabled; by default branches are emitted without an envelope (caller knows the concrete branch).

## Indexing

When `BaboonCodecContext.Indexed` is used and a codec implements `BaboonBinCodecIndexed`, payloads can start with a one‑byte header indicating index presence. Index entries are pairs of `i32` offset/length per indexed element; offsets must be monotonically increasing. This is mainly used by generated codecs with `generateIndexWriters` enabled on C#.

## Interop notes

- All numeric fields are little‑endian; do not rely on platform endianness.
- Strings and `ByteString` differ: `str` uses varint length; `bytes`/`ByteString` use fixed `i32` length.
- UUIDs follow .NET GUID layout; convert when interoperating with big‑endian UUIDs.
- Foreign types rely on user‑provided UEBA codecs; generated stubs must be replaced.

## Worked examples

### Simple DTO

Schema:
```
data Payment {
  amount: i32
  note: opt[str]
  tags: lst[u08]
}
```

Encoding of `{ amount = 42, note = Some("ok"), tags = [1,2] }`:

```
00          2A 00 00 00        ; i32 amount = 42
04          01                 ; opt tag = present
05          02                 ; varint len("ok") = 2
06          6F 6B              ; "ok"
08          02 00 00 00        ; lst count = 2
0C          01 02              ; elements
```

Field order is declaration order; there is no extra envelope.

### Map

```
data M { m: map[str, i32] }
```

`{ m = { "a" -> 7, "b" -> 9 } }`:
```
00          02 00 00 00        ; count = 2
04          01 61              ; key "a" length=1, bytes=61
06          07 00 00 00        ; value 7
0A          01 62              ; key "b" length=1, bytes=62
0C          09 00 00 00        ; value 9
```

### Index header (when enabled)

```
data R { left: lst[u08], right: lst[u08] }
```

Indexed encoding layout (example):
```
00          01                 ; header: index present (bit0)
01          02 00              ; index elements count (short)
03          05 00 00 00        ; offset of left payload
07          03 00 00 00        ; length of left payload
0B          08 00 00 00        ; offset of right payload
0F          02 00 00 00        ; length of right payload
13          ...left payload...
16          ...right payload...
```

Offsets are relative to the start of the payload (after the index section) and must be monotonically increasing.
