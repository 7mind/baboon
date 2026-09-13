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

## Out of scope (recorded)

UEBA format evolution (on-wire index count + body length + skipping decoder)
that would make appended fields unconditionally UEBA-safe; an `opt`-removal
tier (decodes as `None` but silently lossy); per-format range splitting for
renames (byte-invisible to UEBA, breaking for JSON).
