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
`$rv <= $uv <= $v`. Old readers ignore the key.

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

What the policy costs, and why it is opt-in on the writer: a binary reader
cannot distinguish a Tolerant envelope from a byte-identical one.
`ForwardReadPolicy.Lossless` has no effect on binary reads, and a re-encoding
intermediary older than the writer silently truncates the value. The prefix
client contract is satisfied structurally by the byte-array decode entry points
(the payload is the last element of the envelope; nothing asserts full
consumption); stream-based callers must discard the stream after a forward
decode.

## Worked examples: what changed on the wire

Everything below was produced by the generated Scala stub over the shared
`fwd-e2e-ok` and `fwd-e2e-chain-ok` models; the other eight runtimes emit the
same bytes and keys.

### The knobs

| knob | where | values | affects |
|---|---|---|---|
| `ForwardWritePolicy` | writer's `BaboonCodecContext` (next to the index mode) | `Strict` (default), `Tolerant` | the value written into the UEBA `domainVersionMinCompat` slot. No effect on JSON. |
| index mode | writer's `BaboonCodecContext` | compact, indexed | which prefix tier a `Tolerant` writer consults: `prefix-compact` or `prefix-any-mode` |
| `ForwardReadPolicy` | reader's facade | `Tolerant` (default), `Lossless` | whether a JSON reader honours `$rv`. No effect on UEBA — the binary envelope carries one bound and the reader cannot tell how it was chosen. |

One reader rule changed for both formats: when the envelope's `$v` /
`domainVersion` is newer than every registered version and the applicable
bound is at or below the reader's newest version, the reader decodes with its
**newest** codec. Previously it took the bound version's `sameIn` run and
picked the highest member of that run, i.e. a codec byte-identical to the
bound version's — correct, but dropping every field added between the bound
and the reader's own version.

### The fixture types and their bounds

Writer-side bounds (`baboonMinReaderVersions`, emitted per type) for the
2.0.0 domain `fwde2e.fwd` and the 3.0.0 domain `fwde2e.chain`:

| type | evolution | `identical` | `prefix-any-mode` | `prefix-compact` | `json-additive` |
|---|---|---|---|---|---|
| `FwdAppendVar` | 1.0.0 `{a, b}` → 2.0.0 appends `t: opt[str]` | 2.0.0 | 2.0.0 | **1.0.0** | 1.0.0 |
| `FwdMidInsert` | inserts `m: opt[i32]` between `a` and `z` | 2.0.0 | 2.0.0 | 2.0.0 | **1.0.0** |
| `FwdStable` | unchanged | **1.0.0** | 1.0.0 | 1.0.0 | 1.0.0 |
| `FwdEnumHost` | its enum gained a member | 2.0.0 | 2.0.0 | 2.0.0 | 2.0.0 |
| `ChainAppend` | 1.0.0 `{a}` → 2.0.0 `+b: opt[str]` → 3.0.0 `+c: opt[str]` | 3.0.0 | 3.0.0 | **1.0.0** | 1.0.0 |

`FwdAppendVar` earns only `prefix-compact` because the appended field is
variable-length: an indexed 2.0.0 blob carries two index entries (for `b` and
`t`) while the 1.0.0 decoder expects one, and the index has no on-wire count.

### JSON

The JSON writer has no policy. It always publishes both bounds and elides each
when it carries no information: `$uv` when equal to `$v`, `$rv` when equal to
the effective `$uv`. The reader deployed with only 1.0.0:

```jsonc
// FwdAppendVar(42, "hi", Some("t")) — identical bound is 2.0.0 ($uv elided), json-additive bound 1.0.0
{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdAppendVar","$rv":"1.0.0","$c":{"a":42,"b":"hi","t":"t"}}
//   reader Tolerant  → FwdAppendVar(42, "hi")      (1.0.0 codec; "t" dropped)
//   reader Lossless  → refused (no codec)

// FwdMidInsert(7, Some(99), "z") — same shape: positional layout broke, key lookup did not
{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdMidInsert","$rv":"1.0.0","$c":{"a":7,"m":99,"z":"z"}}
//   reader Tolerant  → FwdMidInsert(7, "z")
//   reader Lossless  → refused (no codec)

// FwdStable("s") — byte-identical since 1.0.0: $uv carries it, $rv elided (equal to $uv)
{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdStable","$uv":"1.0.0","$c":{"s":"s"}}
//   reader Tolerant  → FwdStable("s")
//   reader Lossless  → FwdStable("s")

// FwdEnumHost(C) — not forward-readable: neither bound published (both equal $v)
{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdEnumHost","$c":{"e":"C"}}
//   reader Tolerant  → refused (no codec)
//   reader Lossless  → refused (no codec)
```

The three-version chain shows the reader-rule change. A reader registering
1.0.0 **and** 2.0.0 receives a 3.0.0 envelope bound at 1.0.0:

```jsonc
{"$mv":1,"$d":"fwde2e.chain","$v":"3.0.0","$t":"fwde2e.chain/:#ChainAppend","$rv":"1.0.0","$c":{"a":1,"b":"b","c":"c"}}
//   1.0.0+2.0.0 reader, Tolerant → ChainAppend(1, Some("b"))   — newest codec (2.0.0); before this change: ChainAppend(1)
//   1.0.0+2.0.0 reader, Lossless → refused (no codec)           — $uv is elided, i.e. 3.0.0
//   1.0.0 reader,       Tolerant → ChainAppend(1)
```

### UEBA

Envelope layout (`docs/spec/codec-envelope.md` § 2.1):
`metaVersion | domainId | domainVersion | hasMinCompat | [minCompat] | typeId | payload`.
The policy decides only the `hasMinCompat`/`minCompat` bytes; everything else,
payload included, is identical. `FwdAppendVar(42, "hi", Some("t"))` written by
the 2.0.0 facade in a compact context:

```
Strict (default):
01                                                          metaVersion 1
0A 66 77 64 65 32 65 2E 66 77 64                            "fwde2e.fwd"
05 32 2E 30 2E 30                                           "2.0.0"
00                                                          hasMinCompat = 0  → bound = 2.0.0 (identical bound, elided)
19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72   "fwde2e.fwd/:#FwdAppendVar"
00  2A 00 00 00  02 68 69  01 01 74                         payload: mode=compact, a=42, b="hi", t=Some("t")
   1.0.0 reader → refused (CodecNotFound)

Tolerant:
01
0A 66 77 64 65 32 65 2E 66 77 64
05 32 2E 30 2E 30
01 05 31 2E 30 2E 30                                        hasMinCompat = 1, minCompat = "1.0.0" (prefix-compact bound)
19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72
00  2A 00 00 00  02 68 69  01 01 74                         payload unchanged
   1.0.0 reader → FwdAppendVar(42, "hi")   (reads a, b; the remaining `01 01 74` is never consumed)
```

The same value in an **indexed** context is byte-identical under both
policies: the writer consults the `prefix-any-mode` bound, which is 2.0.0, so
`hasMinCompat` stays 0 and the 1.0.0 reader refuses. This is the case the
index mode exists for — the payload starts `01 | 04 00 00 00 03 00 00 00 | 07
00 00 00 03 00 00 00 | …`: two index entries where the 1.0.0 decoder would read
one.

Every combination, as seen by a reader that registers only 1.0.0
(`ForwardReadPolicy` is irrelevant for binary):

| value | Strict, compact | Tolerant, compact | Strict, indexed | Tolerant, indexed |
|---|---|---|---|---|
| `FwdAppendVar` | bound 2.0.0 → refused | **bound 1.0.0 → `FwdAppendVar(42,"hi")`** | bound 2.0.0 → refused | bound 2.0.0 → refused |
| `FwdMidInsert` | 2.0.0 → refused | 2.0.0 → refused (json-additive only; bytes identical to Strict) | 2.0.0 → refused | 2.0.0 → refused |
| `FwdStable` | 1.0.0 → decoded | 1.0.0 → decoded (bytes identical to Strict) | 1.0.0 → decoded | 1.0.0 → decoded |
| `FwdEnumHost` | 2.0.0 → refused | 2.0.0 → refused (bytes identical to Strict) | 2.0.0 → refused | 2.0.0 → refused |

So `Tolerant` changes exactly one envelope in this fixture — the one whose
type has a prefix relationship the payload's mode can honour — and leaves the
rest byte-for-byte as before.

The chain under `Tolerant`, compact (bound `01 05 31 2E 30 2E 30` = 1.0.0,
`$v` 3.0.0, payload `00 | 01 00 00 00 | 01 01 62 | 01 01 63`):

| reader registers | decodes as | before this change |
|---|---|---|
| 1.0.0, 2.0.0 | `ChainAppend(1, Some("b"))` — newest codec, `c` unread | `ChainAppend(1)` — the bound version's codec |
| 1.0.0 | `ChainAppend(1)` | `ChainAppend(1)` |
| 1.0.0, 2.0.0 (Strict envelope, bound 3.0.0) | refused | refused |

### Summary matrix

| format | writer policy | reader policy | bound the reader acts on | result for an older reader |
|---|---|---|---|---|
| JSON | n/a | `Tolerant` (default) | `$rv`, falling back to `$uv` | decodes whenever the type is json-additive-readable; unknown keys dropped |
| JSON | n/a | `Lossless` | `$uv` only | decodes only byte-identical payloads |
| UEBA | `Strict` (default) | either | `minCompat` = identical bound | decodes only byte-identical payloads — unchanged behaviour |
| UEBA | `Tolerant` | either | `minCompat` = prefix bound for the payload's mode | decodes prefix-readable payloads with its newest codec; trailing appended fields unread; indistinguishable from a byte-identical read |

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

## Out of scope (recorded)

UEBA format evolution (on-wire index count + body length + skipping decoder)
that would make appended fields unconditionally UEBA-safe; an `opt`-removal
tier (decodes as `None` but silently lossy); per-format range splitting for
renames (byte-invisible to UEBA, breaking for JSON).
