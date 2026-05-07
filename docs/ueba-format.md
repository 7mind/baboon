# UEBA (Ultra‑Efficient Binary Aggregate) format

This document captures the UEBA wire conventions used by generated Baboon codecs. Everything is little‑endian and intentionally simple so all backend implementations (C#, Scala, Rust, TypeScript, Python, Kotlin, Java, Dart) produce identical binary output.

UEBA has a sister format, [SICK](https://github.com/7mind/sick), which is an indexed binary storage for JSON.

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

## Any fields

> **Note:** the field-level `AnyMeta` envelope below is distinct from the
> top-level `BaboonTypeMeta` envelope (the metadata block that wraps every
> value emitted through `BaboonCodecsFacade.encodeToBin`). The top-level
> envelope is specified in [`docs/spec/codec-envelope.md`](spec/codec-envelope.md).

A field of type `any` (or any of its qualified forms — see [`docs/language-features.md#polymorphic-any-fields`](language-features.md#polymorphic-any-fields)) is encoded as a self‑describing length‑prefixed envelope so any reader can skip it without the inner codec:

```
any_field := length:i32        // bytes after this i32 (= 4 + meta-length + blob length)
             meta-length:i32   // bytes of (meta-kind + meta-strings)
             meta-kind:u8      // bitmask: bit 0 = typeid, bit 1 = version, bit 2 = domain
             meta:bytes        // length-prefixed UTF-8 strings, in fixed order:
                               //   domain  (iff bit 2 set)
                               //   version (iff bit 1 set)
                               //   typeid  (iff bit 0 set)
             blob:bytes        // UEBA-encoded payload; runs to (length - 4 - meta-length) bytes
```

Strings inside the meta block use the same `str` convention as the rest of UEBA (varint length + UTF‑8). All numeric fields are little‑endian signed `i32`.

Six valid kind bytes correspond to the six DSL forms:

| Variant | Kind | Meta on wire |
|---|---|---|
| `any` | `0x07` | domain + version + typeid |
| `any[domain:this]` | `0x03` | version + typeid |
| `any[domain:current]` | `0x01` | typeid |
| `any[T]` | `0x06` | domain + version |
| `any[domain:this, T]` | `0x02` | version |
| `any[domain:current, T]` | `0x00` | (none) |

Kinds `0x04` and `0x05` are reserved.

### Forward-compat skip

`length` and `meta-length` are both intentional. A reader that only knows about today's meta layout can:

- read `length` then seek `length` bytes ahead to skip the field entirely (e.g. a proxy forwarding bytes verbatim), OR
- read `meta-length` and seek to its end without parsing meta extensions added by future versions, then read the `blob`.

Both directions stay byte‑canonical: re-emitting an `AnyOpaqueUeba(meta, bytes)` produces identical bytes to the originating writer.

### Worked example

Schema:
```baboon
data InnerPayload : derived[ueba] {
  label: str
  count: i32
}

data Holder : derived[ueba] {
  f: any[InnerPayload]   // variant D1, kind 0x06
}
```

Encoding `{ f = AnyOpaqueUeba(meta = (kind=0x06, domain="my.ok", version="1.0.0", typeid=null), bytes = <UEBA(InnerPayload("hi", 7))>) }`:

```
00          18 00 00 00         ; length = 24 (everything after this i32)
04          0D 00 00 00         ; meta-length = 13 (kind + 2 strings + their varints)
08          06                  ; meta-kind = 0x06 (domain + version, no typeid)
09          05                  ; varint len("my.ok") = 5
0A          6D 79 2E 6F 6B      ; "my.ok"
0F          05                  ; varint len("1.0.0") = 5
10          31 2E 30 2E 30      ; "1.0.0"
15          02 68 69            ; blob: varint(2), "hi" (InnerPayload.label)
18          07 00 00 00         ; blob: i32 InnerPayload.count = 7
```

Layout sizes: `length` (4) + `meta-length` (4) + meta-kind (1) + meta-strings (12) + blob (7) = 28 bytes total. `length=24` covers bytes 04..1B (everything after the length i32). `meta-length=13` covers bytes 08..14 (the kind byte + the two length-prefixed strings).

Cross-format payloads (an `AnyOpaqueJson` written to UEBA, or an `AnyOpaqueUeba` written to JSON) require the encoder to have a codec registry on the context — pass one via `BaboonCodecContext.withFacade(useIndices, facade)`. Without it, the encoder fails fast.

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
