# Forward-compatibility metadata: design & implementation plan

Status: IMPLEMENTED (2026-09-13) — see docs/forward-compat.md for the user-facing
reference. Resolved during implementation: metadata name is `baboonForwardReadable`,
shape is a version->tier map (order is a compiler-side invariant, clients look up by
writer version); metagen key `forwardReadable` emits user types only; tier vocabulary
`identical` / `prefix-any-mode` / `prefix-compact` / `json-additive`. PREFIX_ANY_MODE
has comparator-level coverage only (fixed-length appends are never auto-derivable, so
no stub-matrix e2e without manual conversions). Discovered en route: `sameIn`
overclaims byte-identity when a dependency is merely reordered (deepSchemaRepr sorts
flattened dep reprs) — documented in ForwardCompatComparatorTest and
docs/forward-compat.md; the enum wire is a positional u8 (ueba-format.md corrected).

## 1. Problem

Clients already use the `sameIn` ranges (`BaboonEvolution.typesUnchangedSince`,
emitted as `baboonSameInVersions` per type and as the domain-wide `unmodified`
map) to implement forward compatibility: a codec generated for version V can
read a blob produced by version W > V when the type is byte-identical across
V..W.

Byte-identity is not the only legitimate forward-compat case. The motivating
example: v2 of a root DTO appends new fields after v1's fields. We want the
compiler to (1) classify which cross-version reads are safe, (2) compute
per-type forward-compatibility ranges, and (3) emit them alongside the
existing `sameIn` ranges.

"Forward-readable" throughout means: **codec for T@V can decode data emitted
by codec for T@W (W > V), guaranteed for any value the newer version can
produce.** Data-dependent safety (e.g. "works unless the new enum member is
actually sent") does not qualify.

## 2. Evidence audit (what the codecs actually do)

### JSON — decoders are tolerant readers

All generated JSON decoders look up known keys and never enumerate the object:

- C#: `asObject["field"]` per known field (`CSJsonCodecGenerator.scala:240`);
  missing/null token for `opt` decodes to `None` (`BaboonTools.cs:48-58`).
- TypeScript: `obj["fld"]` per field; `opt` treats `undefined`/`null` as
  absent (`TsJsonCodecGenerator.scala:124-133`).
- Scala: circe cursor `downField` walk (`ScJsonCodecGenerator.scala:280-294`).
- Rust: serde derive, no `deny_unknown_fields` anywhere → unknown keys ignored.
- Remaining backends (py/kt/kmp/jv/dt/sw) follow the same key-lookup shape
  (cross-language parity is enforced by the acceptance matrix); **PR-F1 must
  spot-verify each before the tier is finalized.**

⇒ Unknown extra keys are ignored in every backend; additive DTO changes are
JSON-forward-readable, at any nesting depth, and the property composes through
the dependency closure and across version steps.

### UEBA — sequential, schema-derived framing

- Compact mode (the default: `BaboonCodecContext.Default = Compact`,
  `BaboonCodecs.cs:53`): DTO body = 1 header byte + fields concatenated in
  declaration order. No count, no lengths.
- Indexed mode: header bit 0 set; index entries `(offset:u32, len:u32)` are
  written **only for variable-length fields** (`CSUEBACodecGenerator.scala:104-110`).
  The entry count is NOT on the wire — the reader loops its own compile-time
  `IndexElementsCount` (`BaboonCodecs.cs:155-180`).
- The generated decoder reads the index but decodes fields **sequentially**,
  never seeking (`CSUEBACodecGenerator.scala:361-373`).
- `ReadIndex` keys off the **wire's** header bit, not the reader's ctx, so a
  reader handles either mode of blob.

⇒ Consequences for a v1 reader of a v2 blob with appended fields D, E:

| context | compact blob | indexed blob |
|---|---|---|
| top-level, cursor discarded after decode | **decodes correctly**, leaves D,E unread | correct iff D,E all fixed-length (index shape unchanged); desyncs before field A if any appended field is var-len (reader under-reads the index) |
| nested field / collection element / anything read after it from the same stream | desyncs the outer decode | desyncs |

⇒ UEBA prefix-reads are sound only for the *outermost* decoded type, because
every nested decode reuses the cursor by construction. Everything *inside* the
prefix must be byte-identical.

## 3. Safe-case classification (per evolution step, per type)

Tiers form a strict linear order of guarantees. Each higher tier implies all
guarantees of the lower ones (appended-only ⊂ additive-anywhere; closure-
unchanged ⊂ closure-additive):

| tier | wire guarantee | conditions on step V→V+1 |
|---|---|---|
| `IDENTICAL` | both formats, any position | existing `unmodified` classification (shallowId+deepId equal) |
| `PREFIX_ANY_MODE` | UEBA compact+indexed **top-level framed reads only, cursor discarded**; JSON any position | own diff is appended-only (old field sequence is a *prefix* of the new one, name+type identical); all appended fields have `BinReprLen.Fixed`; every other type in T's old-version transitive closure is `unmodified` in this step |
| `PREFIX_COMPACT` | UEBA **compact blobs only**, top-level framed reads, cursor discarded; JSON any position | as above, but appended fields may be variable-length |
| `JSON_ADDITIVE` | JSON only, any position | own diff ops ⊆ {AddField, KeepField} (additions at any position); every type in T's old-version closure is `unmodified` or ≥ `JSON_ADDITIVE` in this step |
| `NONE` | — | anything else |

Explicitly NOT forward-readable (and why):

- **RemoveField** — old reader can't find the key (JSON) / bytes missing (UEBA).
  Removing an `opt` field happens to decode as `None` in all checked backends,
  but is silently lossy; excluded by decision (can become a separate tier later).
- **ChangeField / precision expansion** — forward direction is narrowing.
- **Field renames** — break JSON (key change). They are byte-invisible to UEBA,
  which would violate the linear tier order; by decision renames break all
  forward tiers (consistent with today's `sameIn`, which they also break).
  Refining to per-format ranges is a possible follow-up, not in scope.
- **Type renames** (`was[]`) — break all tiers, same rationale.
- **Enum member addition** — old decoder throws on the new name (JSON) /
  positional byte (UEBA). Only data-dependent-safe → excluded.
- **ADT branch addition** — same (unknown branch key / branch-index byte).
- **Foreign type changes** — hand-written codecs, no guarantee derivable.
  Unchanged foreign types participate normally via `IDENTICAL`.
- **Service/Contract** — no codecs; not part of the metadata.

Notes:
- `data`↔`id` flips are shallowId-invariant (see `BaboonEnquiries.scala:317`)
  and land in `IDENTICAL` automatically.
- Fields of `any` type: type-identity is required like any other field. PR-F1
  must verify the `AnyOpaque` envelope decode has no writer-version dependency;
  if unclear, types with `any` in the closure are conservatively capped at
  `IDENTICAL` and a defect is filed.

### Composition (why contiguous ranges are valid)

For a chain V→…→W, chainTier(T) = min over steps of stepTier(T):

- appended-only ∘ appended-only = appended-only (prefix property transitive);
- additive ∘ additive = additive (field sets grow monotonically, shared field
  types are step-invariant);
- closure conditions compose because each step's condition covers the (only
  growing) old-version closure;
- the decode-time argument is inductive: the old decoder touches only
  old-closure types, each of which is step-safe at the same or higher tier.

⇒ tiers are non-increasing in W. This is an invariant to assert in code:
per (type, version), the tier sequence over subsequent versions never rises.

## 4. Metadata model

New model types in `typer/model` (sibling of `UnmodifiedSince`,
`Typedef.scala:5`):

```scala
sealed trait ForwardCompatTier // IDENTICAL, PREFIX_ANY_MODE, PREFIX_COMPACT, JSON_ADDITIVE
                               // linearly ordered; NONE is represented by absence

case class ForwardReadable(
  typeId: TypeId,
  in: Version,                                  // reader version V
  readable: NEList[(Version, ForwardCompatTier)] // ascending; head = (V, IDENTICAL);
                                                 // tiers non-increasing; list stops
                                                 // before the first NONE step
)
```

`BaboonEvolution` gains:

```scala
typesForwardReadable: Map[Version, Map[TypeId, ForwardReadable]]
```

Relationship invariant (cheap sanity check in tests): the `IDENTICAL` prefix of
`readable` equals the suffix of the type's `sameIn` run at/above V.

## 5. Computation (`BaboonComparator`)

1. **Per-step classification.** In `compare` (or a sibling pass keyed by
   `EvolutionStep`), derive `stepTier: Map[TypeId, ForwardCompatTier]`:
   - Reuse `BaboonChanges` for `IDENTICAL` (the `unmodified` set).
   - For changed DTOs, inspect the already-computed `TypedefDiff.DtoDiff` ops:
     additive-only ⇒ candidate `JSON_ADDITIVE`; additionally compare old/new
     field sequences for the prefix property and appended fields' `BinReprLen`
     (via `domain.refMeta`) ⇒ candidate `PREFIX_*`. Changed enums/ADTs/foreigns
     ⇒ `NONE`.
   - Renamed types (in `changes.renamed`) ⇒ `NONE`.
   - **Closure fixpoint** over the old version's dependency graph
     (`defs.predecessors` / `enquiries.explode`): iterate to fixpoint lowering
     each type's tier to the minimum permitted by its direct refs' tiers
     (PREFIX_* requires refs = `IDENTICAL`; JSON_ADDITIVE requires refs ≥
     `JSON_ADDITIVE`). Terminates: tiers only decrease, finite lattice.
     Recursive types are handled by the fixpoint with no special casing.
2. **Suffix-run assembly**, mirroring `computeMinVersions`
   (`BaboonComparator.scala:101-165`) but walking versions **newest→oldest**:
   `readable(T, V) = (V, IDENTICAL) :: (stepTier(T, V→V+1) match { NONE => Nil;
   t => readable(T, V+1).map(cap at t) })`. Reuse the shared-buffer trick or
   plain immutable lists (version counts are small); capping preserves the
   non-increasing invariant by construction.
3. Wire into `evolve` next to `computeMinVersions`; result goes into the
   extended `BaboonEvolution`.

Non-goals in this pass: no changes to `BaboonRules`/conversions, no changes to
any wire format.

## 6. Emission

### 6.1 `baboon-meta.json` (`BaboonMetagen.scala`)

New top-level key alongside `"unmodified"`:

```json
"forwardReadable": {
  "<pkg>": {
    "<version>": {
      "<typeId>": [ {"v": "1.1.0", "tier": "identical"},
                    {"v": "1.2.0", "tier": "json-additive"} ]
    }
  }
}
```

Tier strings: `identical`, `prefix-any-mode`, `prefix-compact`,
`json-additive`. Circe encoders next to `Metadata.scala:23`.

### 6.2 Generated code — per-type constant + domain-wide map, all 9 backends

Mirror of the `baboonSameInVersions` surfaces. Suggested member name:
`baboonForwardReadable` (list of (version, tier) pairs in each language's
idiom; tier as string enum for uniformity with the JSON emission).

| backend | per-type site | domain-wide site | runtime interface |
|---|---|---|---|
| C# | `CSDomainTreeTools.scala:66` (`makeCodecMeta`) | `CSBaboonTranslator.scala:317` | `cs/BaboonRuntimeShared.cs` (`IBaboonGenerated`) |
| Scala | `ScDomainTreeTools.scala` | `ScBaboonTranslator.scala` | `scala/BaboonRuntimeShared.scala:13` (`BaboonGenerated`) |
| TypeScript | `TsDomainTreeTools.scala:30-38` | `TsBaboonTranslator.scala` | `typescript` runtime |
| Kotlin (+KMP) | `KtDomainTreeTools.scala:81` | `KtBaboonTranslator.scala:192` | `kotlin`, `kotlin-kmp` runtimes |
| Java | `JvDomainTreeTools.scala` | `JvBaboonTranslator.scala:174` | `java` runtime |
| Python | `PyDomainTreeTools.scala` | `PyBaboonTranslator.scala` | `python` runtime |
| Rust | dyn-trait fns (`RsBaboonTranslator.scala:231,265`) | same file | `rust` runtime |
| Dart | `DtDomainTreeTools.scala:95` (+ `DtDefnTranslator.scala:366,478`) | `DtBaboonTranslator.scala:208` | `dart` runtime |
| Swift | `SwDomainTreeTools.scala:88` (+ `SwDefnTranslator` sites) | `SwBaboonTranslator.scala:264` | `swift` runtime |

Emission is unconditional (parity with `sameIn`; no CLI flag). Runtime
interface members are implemented only by generated code, so extending them is
not a user-facing break.

Reminder: `sbt clean` after touching `baboon-compiler/src/main/resources/baboon-runtime/`
(PortableResource macro caches resource contents).

### 6.3 What emission does NOT solve

Per-type constants only reach binaries that were **re-generated** after the
newer version was added (same as `baboonSameInVersions`, which also lists
future versions in regenerated old-version namespaces). A client binary built
before v2 existed cannot contain v2 knowledge; distributing forward-compat
ranges to already-deployed clients (e.g. via the version-negotiation endpoint)
is the application's concern. `baboon-meta.json` is the machine-readable
source for that.

## 7. Client contract for the PREFIX_* tiers

The compiler guarantees the *byte prefix* property; soundness of acting on it
additionally requires, at the call site:

1. the blob is decoded as the top-level value (not read out of a larger
   baboon stream);
2. the transport frames the blob (caller knows its end independently);
3. the cursor / remaining bytes are discarded after decode — no
   fully-consumed assertion.

These are usage-site properties the compiler cannot check. They must be stated
in the emitted docs and in `docs/` (see PR-F5). JSON_ADDITIVE and IDENTICAL
carry no such conditions.

## 8. Testing

1. **Comparator unit tests** (JVM-only suite under the hidden
   `baboon-compiler/.jvm/src/test`, run via `sbt baboonJVM/test`): small
   multi-version inline models asserting, per type: tier sequences, run
   breaks (removal, change, rename, enum/ADT addition, foreign change,
   nested-dep change), closure lowering (dep gains a mid-position field ⇒
   host capped at JSON_ADDITIVE; dep appends ⇒ host NOT prefix), fixed vs
   var-len appends (PREFIX_ANY_MODE vs PREFIX_COMPACT), recursive types,
   `sameIn`-consistency invariant, tier monotonicity.
2. **Metagen shape test**: golden/shape assertion for the new JSON key.
3. **Cross-version runtime proof** (per backend, in stub projects):
   - JSON: encode with latest-version codec, decode with an older-version
     codec for a type whose metadata says ≥ `JSON_ADDITIVE`; assert field
     values survive.
   - UEBA prefix: compact-encode with latest codec, decode top-level with
     older codec for a `PREFIX_COMPACT` type; assert prefix fields correct.
     Negative control: assert the same read desyncs/fails for a `NONE` type
     (documents *why* the metadata gates it).
   - Pilot in two backends first (Scala + TypeScript), then replicate.
   Acceptance checks must be unconditional throws, not asserts (vacuous in
   C#/Dart/Swift/JVM by default — see memory/CLAUDE.md).
4. Full gates: `mdl :build :test` per PR; `mdl --seq :ci` before push
   (Kotlin daemon OOM under parallel matrix).

Test-model placement: extend the shared `baboon-compiler/src/test/resources/baboon/`
matrix models only if no cross-namespace hazard (D9); otherwise a dedicated
model dir + lane, like `mcp-stub-ok`.

## 9. PR breakdown

| PR | scope | gate |
|---|---|---|
| F1 | model types, comparator classification + fixpoint + runs, `BaboonEvolution` field, metagen emission, JVM unit tests, JSON-tolerance spot-verification of py/kt/kmp/jv/dt/sw decoders, `any`-envelope version-independence check | `sbt baboonJVM/test`, `mdl :build :test` |
| F2 | runtime interface + codegen emission, pilot backends (Scala, TypeScript) + stub compile/shape tests | `mdl :build :test` (sc/ts lanes) |
| F3 | remaining backends (cs, py, rs, kt, kmp, jv, dt, sw) — mechanical parity with F2 | full `mdl :build :test` |
| F4 | cross-version runtime proofs (§8.3) in pilot backends, then matrix-wide | `mdl --seq :ci` |
| F5 | docs: forward-compat semantics + client contract (`docs/`), note in `docs/ueba-format.md` | n/a |

Sizing note: F3 touches ~9 translators + 10 runtime dirs; keep each backend a
single commit; bundle any exhaustive-match updates per file (M29 pattern).

## 10. Explicitly out of scope (recorded decisions)

1. **UEBA format evolution** (on-wire index entry count, total-body length,
   seeking/skipping decoder, header flag bit + coordinated rollout). This is
   the only route to unconditional UEBA forward-compat for appended fields;
   it is its own design because the framing change itself breaks byte-identity
   with previously generated codecs even for unchanged types.
2. **`opt`-field-removal tier** (decodes as `None` everywhere checked, but
   silently lossy).
3. **Per-format range splitting for renames** (UEBA-identical / JSON-broken).
4. **"Unreferenced root" static check** (belt-and-suspenders flag that a type
   cannot occur in nested position in generated code) — cheap, can ride along
   with F1 if desired.

## 11. Open questions

1. Exact member naming across backends (`baboonForwardReadable` vs
   `baboonCanReadVersions`) — pick once at F2, keep uniform.
2. Should the metagen key also dump the per-step tier matrix (debugging aid),
   or only the assembled ranges? (Proposal: ranges only.)
3. `docs/ueba-format.md` says enums are `i32` discriminants while the C#
   generator writes a single byte (`CSUEBACodecGenerator.scala:242-267`) —
   pre-existing doc/impl discrepancy noticed during this audit; resolve in F5
   (verify against other backends and the acceptance fixtures; not touched by
   this plan otherwise).
