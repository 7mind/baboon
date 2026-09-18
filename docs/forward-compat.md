# Forward-compatibility metadata

Design history: [drafts/20260911-0937-forward-compat-metadata.md](drafts/20260911-0937-forward-compat-metadata.md).

Baboon has long emitted `sameIn` ranges (`baboonSameInVersions`, the `unmodified`
block of `baboon-meta.json`): the contiguous versions in which a type's encoding
is byte-identical. Clients use them to let an older version's codec read
newer-version blobs when nothing changed.

The compiler additionally computes **forward-readability ranges**: for every
type in every version V, the ascending contiguous run of versions W ≥ V whose
encoded data the V-version codec can decode, each with a **guarantee tier**.
"Can decode" is a guarantee over *every* value the newer version can emit —
data-dependent luck (e.g. "works unless the new enum member is sent") never
qualifies.

## Tiers

Linearly ordered; every tier implies all guarantees of the tiers below it.

| tier (wire name) | guarantee |
|---|---|
| `identical` | byte-identical encoding: both formats, any nesting position |
| `prefix-any-mode` | UEBA: prefix-read of compact **and** indexed blobs (all appended fields fixed-length); JSON: any position |
| `prefix-compact` | UEBA: prefix-read of **compact** blobs only (some appended field is variable-length); JSON: any position |
| `json-additive` | JSON only, any nesting position |
| *(absent)* | no guarantee |

What earns each tier (per evolution step, composed over contiguous chains by
taking the minimum):

- `identical` — own fields ordered-equal and every codec-relevant dependency
  identical.
- `prefix-*` — the old field sequence is a strict prefix of the new one
  (names and types equal, additions only at the tail), **and every other type
  in the old version's dependency closure is byte-identical**. Any nested
  change would desync the outer sequential read.
- `json-additive` — all old fields present with identical types (additions at
  any position, reordering allowed), dependencies at least `json-additive`.
  Sound because every generated JSON decoder is a tolerant reader (key lookup;
  unknown keys ignored — verified in all 9 backends, including serde without
  `deny_unknown_fields` and pydantic's default `extra='ignore'`).

Never forward-readable: field removals or type changes (forward reads narrow),
field renames (JSON key changes), enum member additions/reordering (positional
`u8` discriminants / unknown names), ADT branch additions, foreign-type
changes, type renames.

## The prefix-* client contract

The compiler guarantees the byte-prefix property. Acting on it is sound ONLY
when, at the call site:

1. the blob is decoded as the **top-level** value (never from a position inside
   a larger baboon stream — nested fields, collection elements);
2. the transport frames the blob (its end is known independently);
3. the cursor / remaining bytes are **discarded** after decoding — no
   fully-consumed assertion.

These are usage-site properties the compiler cannot check. `identical` and
`json-additive` carry no such conditions.

## Where the metadata lives

- `baboon-meta.json` (`--meta-write-evolution-json`): top-level
  `"forwardReadable"` key — `pkg → version → typeId → [{v, tier}, ...]`.
  The list is ascending, starts with the own version at `identical`, and tiers
  are non-increasing. This is the machine-readable source for distributing
  ranges to deployed clients (e.g. via a version-negotiation endpoint):
  per-type constants only reach binaries regenerated after the newer version
  was added.
- Generated code, per type (sibling of `baboonSameInVersions`): a
  `version → tier` map named `baboonForwardReadable` (language-idiomatic
  casing/shape: `BaboonForwardReadableValue`+`BaboonForwardReadable()` in C#,
  `BaboonForwardReadable` static + `baboonForwardReadable()` in TypeScript,
  `baboon_forward_readable` ClassVar in Python, `baboon_forward_readable_dyn()`
  `Vec<(String, String)>` in Rust, `baboonForwardReadableConst` in Dart, etc.).
- Generated domain metadata registry: `forwardReadableVersions(typeId)` next to
  `sameInVersions(typeId)`.

## Envelope integration (JSON)

The facade envelope publishes the JSON bound as `$rv` (`domainVersionReadableMin`):
the oldest domain version whose JSON codec can decode the payload under the
`json-additive` contract. It is the writer-side inverse of the forward
ranges — generated types carry `baboonMinReaderVersions` (tier → oldest reader
version; the `identical` bound equals `baboonSameInVersions.head`, the
`json-additive` bound feeds `$rv`). `$rv` is elided when equal to the effective
`$uv`, so envelopes of unchanged types are byte-identical to before; invariant
`$rv <= $uv <= $v`. Old readers ignore the key. A value whose
`baboonMinReaderVersions` lacks the tier a writer needs is an encoder failure in
every runtime — generated types always carry all four tiers, so this only
guards hand-written implementations.

Readers choose via `ForwardReadPolicy`:

- `Tolerant` (default): when `$v` is newer than every registered version,
  resolve the codec from `$rv` (falling back to `$uv`) — the payload is decoded
  with that version's codec and fields unknown to it are dropped.
- `Lossless`: ignore `$rv`; resolve from `$uv` only (pre-`$rv` behaviour).
  **Re-encoding intermediaries must use this**, otherwise they truncate data
  for downstream consumers that do understand the newer version.

Spec: `docs/spec/codec-envelope.md`.

## Envelope integration (UEBA)

The binary v1 envelope has a single bound slot, `domainVersionMinCompat`, and
its layout cannot grow without a `metaVersion` bump. Forward reads therefore
ride on the *value* of that slot, chosen by the WRITER through
`ForwardWritePolicy` on `BaboonCodecContext` (alongside the index mode, which
the bound depends on):

- `Strict` (default): the byte-identical bound — exactly what was written
  before the policy existed.
- `Tolerant`: the prefix-read bound from `baboonMinReaderVersions` for the
  payload's index mode: `prefix-compact` for compact payloads, `prefix-any-mode`
  for indexed ones. When no prefix relationship exists the bound equals the
  Strict one, so the envelope is byte-identical.

Readers (all nine runtimes) trust the bound: a payload from a version newer
than every registered one is decoded with the reader's **newest** codec as soon
as the bound is at or below it — not with the bound version's codec.
Readability is monotone along the chain (a suffix of a prefix chain is a prefix
chain), so the newest codec is always correct and loses the fewest fields. This
rule is the same for JSON `$uv`/`$rv` resolution.

What the v1 policy costs, and why it is opt-in on the writer: a v1 reader
cannot distinguish a Tolerant envelope from a byte-identical one.
`ForwardReadPolicy.Lossless` has no effect on v1 reads, and a re-encoding
intermediary older than the writer silently truncates the value. The prefix
client contract is satisfied structurally by the byte-array decode entry points
(the payload is the last element of the envelope; nothing asserts full
consumption); stream-based callers must discard the stream after a forward
decode.

### Envelope v2 — the JSON-equivalent binary layout

`BaboonCodecContext.envelopeVersion = V2` (default `V1`) switches the writer to
the `metaVersion = 2` layout (`docs/spec/codec-envelope.md` §2.1.3):
`02 | domainId | domainVersion | flags | [minCompat] | [readableMin] | typeId |
payload`. It carries both bounds with fixed meaning — `minCompat` is the
byte-identical bound, `readableMin` the prefix bound for the payload's index
mode — behind a flags byte (bit 0: minCompat present, bit 1: readableMin
present; elision rules as in JSON). Consequences:

- `ForwardWritePolicy` is irrelevant under v2; both bounds always travel.
- The reader's `ForwardReadPolicy` applies to binary exactly as to JSON:
  `Tolerant` decodes with the newest codec once `readableMin` reaches a
  registered version, `Lossless` requires `minCompat` to — so a re-encoding
  intermediary can protect itself, which v1 never allowed.
- Every reader in the nine runtimes accepts v1 and v2. A reader built before
  v2 existed rejects a v2 envelope as an unknown `metaVersion`, which is why the
  default stays v1: switch the writer only once the readers are current.
- Types with no prefix relationship produce a v2 envelope that is the v1
  envelope with `02` in front; the flag byte values coincide bit-for-bit.

## Worked examples: what changed on the wire

Everything below was produced by the generated Scala stub over the shared
`fwd-e2e-ok` and `fwd-e2e-chain-ok` models; the other eight runtimes emit the
same bytes and keys.

### The knobs

| knob | where | values | affects |
|---|---|---|---|
| `envelopeVersion` | writer's `BaboonCodecContext` | `V1` (default), `V2` | which binary layout is written: v1 (one bound slot) or v2 (both bounds, JSON-equivalent). No effect on JSON. |
| `ForwardWritePolicy` | writer's `BaboonCodecContext` (next to the index mode) | `Strict` (default), `Tolerant` | the value written into the v1 `domainVersionMinCompat` slot. No effect on JSON or on v2. |
| index mode | writer's `BaboonCodecContext` | compact, indexed | which prefix tier the writer consults for the prefix bound: `prefix-compact` or `prefix-any-mode` |
| `ForwardReadPolicy` | reader's facade | `Tolerant` (default), `Lossless` | whether a reader honours `readableMin` (JSON `$rv`, binary v2). No effect on binary v1 — that envelope carries one bound and the reader cannot tell how it was chosen. |

One reader rule changed for both formats: when the envelope's `$v` /
`domainVersion` is newer than every registered version and the applicable
bound is at or below the reader's newest version, the reader decodes with its
**newest** codec. Previously it took the bound version's `sameIn` run and
picked the highest member of that run, i.e. a codec byte-identical to the
bound version's — correct, but dropping every field added between the bound
and the reader's own version.

### Reading the examples

The writer is the 2.0.0 facade of `fwde2e.fwd`; the reader registers only
1.0.0. Every UEBA envelope starts with the same 18 bytes, abbreviated `HDR`:

```
01                                   metaVersion = 1
0A 66 77 64 65 32 65 2E 66 77 64     len 10, "fwde2e.fwd"
05 32 2E 30 2E 30                    len 5,  "2.0.0"
```

After `HDR` come the bound bytes — either `00` (elided; bound = 2.0.0) or
`01 05 31 2E 30 2E 30` (`hasMinCompat` = 1, then `"1.0.0"`) — then the
length-prefixed type identifier and the payload. Strings in the payload are
written as `len "text"` for readability; everything else is raw hex. The
`envelope v2` blocks are the same values written with
`BaboonCodecContext.envelopeVersion = V2`; they start with `02` instead of
`HDR`'s `01`, and their third field is the flags byte of §2.1.3 rather than
`hasMinCompat`.

### `FwdAppendVar` — a variable-length field appended at the tail

```
version "1.0.0"                version "2.0.0"
root data FwdAppendVar {       root data FwdAppendVar {
  a: i32                         a: i32
  b: str                         b: str
}                                t: opt[str]
                               }
```

Bounds: `identical` 2.0.0 · `prefix-any-mode` 2.0.0 · `prefix-compact` **1.0.0** ·
`json-additive` 1.0.0. The append earns only `prefix-compact` because `t` is
variable-length (see the indexed bytes below).

Value: `FwdAppendVar(42, "hi", Some("t"))`.

JSON — `$uv` elided (identical bound equals `$v`), `$rv` published:

```jsonc
{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdAppendVar","$rv":"1.0.0","$c":{"a":42,"b":"hi","t":"t"}}
//   1.0.0 reader, Tolerant → FwdAppendVar(42, "hi")   ("t" dropped)
//   1.0.0 reader, Lossless → refused (no codec)
```

UEBA, four writer modes:

```
Strict, compact                                          1.0.0 reader → refused
HDR | 00 | 19 "fwde2e.fwd/:#FwdAppendVar"
    | 00  2A 00 00 00  02 "hi"  01 01 "t"                mode=compact, a=42, b, t=Some("t")

Tolerant, compact                                        1.0.0 reader → FwdAppendVar(42, "hi")
HDR | 01 05 "1.0.0" | 19 "fwde2e.fwd/:#FwdAppendVar"
    | 00  2A 00 00 00  02 "hi"  01 01 "t"                payload identical; the 1.0.0 codec stops after b

Strict, indexed                                          1.0.0 reader → refused
HDR | 00 | 19 "fwde2e.fwd/:#FwdAppendVar"
    | 01                                                 mode=indexed
    | 04 00 00 00 03 00 00 00                            index entry for b: offset 4, length 3
    | 07 00 00 00 03 00 00 00                            index entry for t: offset 7, length 3
    | 2A 00 00 00  02 "hi"  01 01 "t"

Tolerant, indexed                                        1.0.0 reader → refused
(byte-identical to Strict, indexed: the prefix-any-mode bound is 2.0.0, so hasMinCompat stays 0)

envelope v2, compact                                     1.0.0 reader, Tolerant → FwdAppendVar(42, "hi")
02 | 0A "fwde2e.fwd" | 05 "2.0.0"                        1.0.0 reader, Lossless → refused
   | 02                                                  flags: minCompat elided (= 2.0.0), readableMin present
   | 05 "1.0.0"                                          readableMin = prefix-compact bound
   | 19 "fwde2e.fwd/:#FwdAppendVar"
   | 00  2A 00 00 00  02 "hi"  01 01 "t"

envelope v2, indexed                                     1.0.0 reader → refused (either policy)
02 | 0A "fwde2e.fwd" | 05 "2.0.0" | 00 | 19 "fwde2e.fwd/:#FwdAppendVar" | 01 | …index…  flags 0: prefix-any-mode bound is 2.0.0
```

The indexed layout is why the tier stops at `prefix-compact`: the index has no
on-wire count, the 1.0.0 decoder expects one entry (for `b`) and would read the
second entry's offset as the start of the fields.

### `FwdMidInsert` — a field inserted mid-sequence

```
version "1.0.0"                version "2.0.0"
root data FwdMidInsert {       root data FwdMidInsert {
  a: i32                         a: i32
  z: str                         m: opt[i32]
}                                z: str
                               }
```

Bounds: `identical` 2.0.0 · `prefix-any-mode` 2.0.0 · `prefix-compact` 2.0.0 ·
`json-additive` **1.0.0**. Positional layout broke; key lookup did not.

Value: `FwdMidInsert(7, Some(99), "z")`.

```jsonc
{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdMidInsert","$rv":"1.0.0","$c":{"a":7,"m":99,"z":"z"}}
//   1.0.0 reader, Tolerant → FwdMidInsert(7, "z")
//   1.0.0 reader, Lossless → refused (no codec)
```

```
Strict, compact  =  Tolerant, compact                    1.0.0 reader → refused
HDR | 00 | 19 "fwde2e.fwd/:#FwdMidInsert"
    | 00  07 00 00 00  01 63 00 00 00  01 "z"            a=7, m=Some(99), z

Strict, indexed  =  Tolerant, indexed                    1.0.0 reader → refused
HDR | 00 | 19 "fwde2e.fwd/:#FwdMidInsert"
    | 01 | 04 00 00 00 05 00 00 00 | 09 00 00 00 02 00 00 00
    | 07 00 00 00  01 63 00 00 00  01 "z"

envelope v2, compact                                     1.0.0 reader → refused (either policy)
02 | 0A "fwde2e.fwd" | 05 "2.0.0" | 00 | 19 "fwde2e.fwd/:#FwdMidInsert"
   | 00  07 00 00 00  01 63 00 00 00  01 "z"             flags 0: no bound to publish — the v1 bytes with 02 in front
```

No UEBA prefix bound exists, so `Tolerant` writes exactly the Strict bytes and
v2 carries no bound at all.

### `FwdStable` — unchanged

```
version "1.0.0"                version "2.0.0"
root data FwdStable {          import "1.0.0" { * } without { FwdAppendVar FwdMidInsert FwdEnumGrows }
  s: str                       // FwdStable is inherited from 1.0.0 unchanged
}
```

Bounds: every tier **1.0.0**.

Value: `FwdStable("s")`.

```jsonc
{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdStable","$uv":"1.0.0","$c":{"s":"s"}}
//   1.0.0 reader, Tolerant → FwdStable("s")
//   1.0.0 reader, Lossless → FwdStable("s")             ($uv reaches 1.0.0; $rv elided because it equals $uv)
```

```
Strict, compact  =  Tolerant, compact                    1.0.0 reader → FwdStable("s")
HDR | 01 05 "1.0.0" | 16 "fwde2e.fwd/:#FwdStable"
    | 00  01 "s"

Strict, indexed  =  Tolerant, indexed                    1.0.0 reader → FwdStable("s")
HDR | 01 05 "1.0.0" | 16 "fwde2e.fwd/:#FwdStable"
    | 01 | 00 00 00 00 02 00 00 00 | 01 "s"

envelope v2, compact                                     1.0.0 reader → FwdStable("s") under either policy
02 | 0A "fwde2e.fwd" | 05 "2.0.0" | 01 05 "1.0.0" | 16 "fwde2e.fwd/:#FwdStable" | 00 01 "s"
                                                         flags 0b01: minCompat 1.0.0; readableMin equal, elided
```

The bound is already 1.0.0 under `Strict`; `Tolerant` has nothing to lower.

### `FwdEnumHost` — the referenced enum grew

```
version "1.0.0"                version "2.0.0"
enum FwdEnumGrows { A B }      enum FwdEnumGrows { A B C }      // redefined
root data FwdEnumHost {        // FwdEnumHost itself is inherited from 1.0.0 unchanged;
  e: FwdEnumGrows              // it changes because its dependency changed
}
```

Bounds: every tier 2.0.0 — not forward-readable (positional `u8`
discriminant; a 1.0.0 decoder has no member 2).

Value: `FwdEnumHost(C)`.

```jsonc
{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdEnumHost","$c":{"e":"C"}}
//   1.0.0 reader, Tolerant → refused (no codec)         (no $uv, no $rv)
//   1.0.0 reader, Lossless → refused (no codec)
```

```
Strict, compact  =  Tolerant, compact                    1.0.0 reader → refused
HDR | 00 | 18 "fwde2e.fwd/:#FwdEnumHost"
    | 00  02                                             e = member index 2 (C)

Strict, indexed  =  Tolerant, indexed                    1.0.0 reader → refused
HDR | 00 | 18 "fwde2e.fwd/:#FwdEnumHost"
    | 01  02                                             indexed mode byte, no variable-length fields → no index entries

envelope v2, compact                                     1.0.0 reader → refused (either policy)
02 | 0A "fwde2e.fwd" | 05 "2.0.0" | 00 | 18 "fwde2e.fwd/:#FwdEnumHost" | 00 02
                                                         flags 0: not forward-readable, nothing to publish
```

### `ChainAppend` — three versions, two appends

```
version "1.0.0"            version "2.0.0"            version "3.0.0"
root data ChainAppend {    root data ChainAppend {    root data ChainAppend {
  a: i32                     a: i32                     a: i32
}                            b: opt[str]                b: opt[str]
                           }                            c: opt[str]
                                                      }
```

Bounds at 3.0.0: `identical` 3.0.0 · `prefix-any-mode` 3.0.0 · `prefix-compact`
**1.0.0** · `json-additive` 1.0.0 (both steps are `prefix-compact`; the chain
takes the minimum). Writer: the 3.0.0 facade of `fwde2e.chain`. Two readers:
one registering 1.0.0 and 2.0.0, one registering 1.0.0 only.

Value: `ChainAppend(1, Some("b"), Some("c"))`.

```jsonc
{"$mv":1,"$d":"fwde2e.chain","$v":"3.0.0","$t":"fwde2e.chain/:#ChainAppend","$rv":"1.0.0","$c":{"a":1,"b":"b","c":"c"}}
//   1.0.0+2.0.0 reader, Tolerant → ChainAppend(1, Some("b"))   newest codec (2.0.0); before this change: ChainAppend(1)
//   1.0.0+2.0.0 reader, Lossless → refused (no codec)           $uv is elided, i.e. 3.0.0
//   1.0.0 reader,       Tolerant → ChainAppend(1)
```

```
Strict, compact                                          1.0.0+2.0.0 reader → refused
01 | 0C "fwde2e.chain" | 05 "3.0.0" | 00 | 1A "fwde2e.chain/:#ChainAppend"
   | 00  01 00 00 00  01 01 "b"  01 01 "c"               a=1, b=Some("b"), c=Some("c")

Tolerant, compact                                        1.0.0+2.0.0 reader → ChainAppend(1, Some("b"))
01 | 0C "fwde2e.chain" | 05 "3.0.0" | 01 05 "1.0.0" | 1A "fwde2e.chain/:#ChainAppend"
   | 00  01 00 00 00  01 01 "b"  01 01 "c"               1.0.0 reader → ChainAppend(1)
                                                         before this change, 1.0.0+2.0.0 reader → ChainAppend(1)

envelope v2, compact                                     1.0.0+2.0.0 reader, Tolerant → ChainAppend(1, Some("b"))
02 | 0C "fwde2e.chain" | 05 "3.0.0"                      1.0.0+2.0.0 reader, Lossless → refused (minCompat 3.0.0)
   | 02 | 05 "1.0.0"                                     flags 0b10: minCompat elided (= 3.0.0), readableMin 1.0.0
   | 1A "fwde2e.chain/:#ChainAppend"
   | 00  01 00 00 00  01 01 "b"  01 01 "c"
```

### Summary

In these fixtures `Tolerant` changes exactly two envelopes — the compact
`FwdAppendVar` and the compact `ChainAppend` — and leaves every other
combination byte-for-byte as `Strict` writes it.

| format | writer policy | reader policy | bound the reader acts on | result for an older reader |
|---|---|---|---|---|
| JSON | n/a | `Tolerant` (default) | `$rv`, falling back to `$uv` | decodes whenever the type is json-additive-readable; unknown keys dropped |
| JSON | n/a | `Lossless` | `$uv` only | decodes only byte-identical payloads |
| UEBA v1 (default) | `Strict` (default) | either | `minCompat` = identical bound | decodes only byte-identical payloads — unchanged behaviour |
| UEBA v1 (default) | `Tolerant` | either | `minCompat` = prefix bound for the payload's mode | decodes prefix-readable payloads with its newest codec; trailing appended fields unread; indistinguishable from a byte-identical read |
| UEBA v2 | irrelevant | `Tolerant` (default) | `readableMin` (prefix bound), falling back to `minCompat` | decodes prefix-readable payloads with its newest codec; trailing appended fields unread |
| UEBA v2 | irrelevant | `Lossless` | `minCompat` only | decodes only byte-identical payloads |

## Relationship to sameIn

`identical` forward tiers imply membership in the `sameIn` run. Historically
the converse could fail: `deepSchemaRepr` sorted the flattened dependency repr
lines per field (and sorted ADT branch reprs), erasing member/field/branch
*order* inside dependencies and type-constructor argument order — so a type
whose dependency enum was merely reordered (or whose dep ADT's branches were
reordered, or whose dep's `map[K,V]` arguments were swapped) stayed
`unmodified` even though its UEBA bytes changed. This was fixed by making deep
hashing order- and structure-sensitive (signature scheme 2): dependency reprs
stay contiguous and internally ordered (determinism comes from sorting the
dependency *ids*), each field line carries its full type-ref rendering, and ADT
branch order — the UEBA discriminant order — is preserved. Regression coverage:
`ForwardCompatComparatorTest` ("sameIn must not overclaim byte-identity").

Two consequences of scheme 2:

- **Lockfiles** persist deepId-derived signatures. The lockfile format carries a
  `scheme` marker; a lockfile written by an older compiler (scheme 1, or no
  marker) is treated as incomparable — drift enforcement is skipped for that one
  run and the file is re-signed in place, even under `create-only`.
- **The M20 manual→sugared ADT rewrite is honestly classified**: the sugared
  expansion reorders branches (local members first), which shifts positional
  UEBA discriminants, so such a step is `deepModified` (forward tier
  `json-additive`), with a fully derivable `CopyAdtBranchByName` conversion —
  not byte-identical, as the branch-sorted hash used to claim.

## End-to-end coverage

- `ForwardCompatComparatorTest` (JVM, `sbt baboonJVM/test`) — tier
  classification, run composition, closure demotion, invariants; fixtures in
  `baboon-compiler/src/test/resources/fwd-compat-ok/`.
- `test/sc-stub/.../ForwardCompatSpec.scala` and
  `test/ts-stub/.../ForwardCompat.test.ts` — real cross-version decodes over
  the shared `fwd-e2e-ok` model: old codec reads new compact-UEBA prefix and
  new JSON; desync and grown-enum negative controls.
- `test/sc-stub/.../ForwardCompatEnvelopeSpec.scala` and
  `test/ts-stub/.../ForwardCompatEnvelope.test.ts` — envelope-level proof of
  `$rv`: a facade registering only 1.0.0 decodes 2.0.0 envelopes under
  `Tolerant` exactly where `$rv` allows, refuses under `Lossless`, and refuses
  when no bound was published; `$rv` elision and `readMeta` round-trip.
- `test/sc-stub/.../BinEnvelopeV2Spec.scala` and
  `test/ts-stub/.../BinEnvelopeV2.test.ts` — envelope v2: a hand-assembled v2
  envelope decodes under `Tolerant` and is refused under `Lossless` (fail-first:
  the previous reader rejected metaVersion 2 outright); the default context
  still writes byte-identical v1; the v2 writer's flags/bounds per type and
  index mode; round trips through the writer and old readers; the
  three-version chain with `Lossless` now enforceable; unknown flag bits and
  unknown metaVersions rejected.
- Golden envelope bytes in every other backend — `BinEnvelopeGoldenTests.cs`,
  `BinEnvelopeGoldenTest.kt` (JVM and KMP stubs), `BinEnvelopeGoldenTest.java`,
  `test_bin_envelope_golden.py`, `bin_envelope_golden_test.dart`,
  `BinEnvelopeGoldenTests.swift`, `bin_envelope_golden_tests.rs` — assert the
  exact byte sequences shown in the worked examples (v1 Strict, v1 Tolerant, v2
  compact and indexed, for `FwdAppendVar`, `FwdStable`, `FwdEnumHost` and the
  chain), the v1 default of the built-in contexts, and a v2 round trip through
  the writer's own facade. They exist because the first cross-language check
  found two asymmetries the per-language suites had not: the Rust per-type
  `baboon_same_in_versions_dyn` returned `[own version]` (so Rust-written
  envelopes of unchanged types elided the byte-identical bound that every other
  backend publishes), and the Python facade could not register a second domain
  version at all (`_register_version` sorted by a non-existent attribute).
- `test/sc-stub/.../ForwardCompatBinEnvelopeSpec.scala` and
  `test/ts-stub/.../ForwardCompatBinEnvelope.test.ts` — the UEBA writer policy:
  Strict envelopes are unchanged and refused by an old reader; Tolerant compact
  envelopes carry the `prefix-compact` bound and are decoded by the old reader
  with its own codec; json-additive-only, byte-identical and grown-enum types
  yield envelopes identical to Strict; an indexed context does not lower the
  bound for a variable-length appended field; and over the three-version
  `fwd-e2e-chain-ok` model a reader registering 1.0.0 and 2.0.0 decodes a 3.0.0
  envelope bound at 1.0.0 with its 2.0.0 codec (fail-first: the previous
  reader picked the bound version's codec and dropped the 2.0.0 field).

## Known gaps

- **`prefix-any-mode` has no runtime end-to-end test.** The tier requires an
  appended *fixed-length* field, and every fixed-length scalar is
  non-defaultable, so such a step needs a hand-written conversion — which the
  stub projects do not carry. The tier is covered at the compiler level
  (`ForwardCompatComparatorTest`, isolated `fwd-compat-ok` fixtures) and the
  runtime suites cover the negative side only: an indexed context does not
  lower the bound for a variable-length append.

Closed gaps, kept for the record:

- *Missing-tier handling* used to differ: Python, Swift and Rust fell back to
  the byte-identical bound when `baboonMinReaderVersions` lacked a tier, the
  other seven runtimes failed fast. All ten now fail fast (encoder failure),
  `baboonMinReaderVersions` has no default in any runtime's base type, and each
  of the three has a regression test with a hand-written value lacking the tiers.
- *Timestamp kind byte round trips* (https://github.com/7mind/baboon/issues/91):
  the C# `RpDateTime` keeps carrying its `DateTimeKind` on the wire (Local when
  the offset matches the writer's zone) — this is intentional and unchanged, as
  is the Scala side — so byte-for-byte equality of a `tso`/`tsu` across writers
  is not guaranteed (`docs/ueba-format.md`). The converter round-trip test
  (`RTCodecTest`) now falls back to a structural, equal-length comparison when
  bytes differ, which is the property the round trip actually promises. Python
  wrote `2` (Local) for every non-zero offset; it now writes `0` like the other
  non-.NET runtimes.

## Out of scope (recorded)

UEBA format evolution (on-wire index count + body length + skipping decoder)
that would make appended fields unconditionally UEBA-safe; an `opt`-removal
tier (decodes as `None` but silently lossy); per-format range splitting for
renames (byte-invisible to UEBA, breaking for JSON).
