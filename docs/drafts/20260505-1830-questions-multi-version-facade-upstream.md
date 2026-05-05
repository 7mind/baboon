# Questions — proposal.md upstream (multi-version codec facade)

Dated 2026-05-05. Written before plan freezes; answer inline or amend.

## 1. Legacy byte range 0..15 — how many distinct layouts?

`tasks.md` M32 says "legacy formats occupy the 0..15 byte range". The proposal spec
defines `metaVersion = 1` with the layout in §2.1/§2.2. Two interpretations of the
1..15 reservation:

- **(a) one legacy layout**: production speaks only byte `1`, layout exactly per proposal,
  and `2..15` is just headroom we reserved for future legacy variants that never shipped.
- **(b) multiple legacy layouts**: production currently emits/accepts more than one byte
  in `1..15`, each with a different concrete layout, and we need to decode several.

Which is it? If (b), please list the bytes and where their layout specs live (or paste them).


answer: I have reserved 0..15 range but in fact only "1" is actually used

## 2. Unification target — pick one

Treating the proposal-format (byte `1`) as **legacy / production / must-not-break** and
baboon's current format (byte `16`) as **new / undeployed / mutable**:

- **(A) Hard unify at byte 1.** Rewrite baboon's `META_VERSION_1` from 16 → 1
  across all 11 runtimes; emit and decode the proposal layout exclusively. Simplest;
  one envelope on the wire. Requires that legacy is *only* byte 1 with proposal
  layout (i.e. answer to Q1 is (a)).
- **(B) Dual-decode, single-emit.** Keep emitting byte 16; extend `ReadMeta` to
  accept byte 1 too (since the structural layout is identical). Baboon-emitted
  payloads are visually distinct from legacy; legacy producers stay readable.
- **(C) Mode-switched emit.** Keep byte 16 by default but expose a flag /
  `BaboonCodecContext` setting that emits byte 1 with proposal layout for interop
  contexts. Decoders accept both. Most flexible; most surface.

If Q1 = (a), I recommend (A) — it gives you wire identity with the production
format you're trying to interop with, and the M32 reasoning ("avoid collision")
goes away because there is no other layout to collide with. If Q1 = (b), only (B)
or (C) make sense.

answer: it's not hard to rewrite,

## 3. Per-domain `DomainXFacade` — generate it?

Proposal §3.1 / §7.2 calls for the compiler to generate a per-domain registration
class with a parameterless ctor that calls `Register(...)` for every version of
the domain. Today the compiler emits per-codec-kind singletons
(`BaboonCodecs<json|ueba>`), but no class that bundles
`(domainVersion, codecsJson, codecsBin, conversions, meta)` into a single
`Register(...)` call against a `BaboonCodecsFacade`.

- **Generate `Domain<DomainId>Facade` per domain** with overloads for current
  version (full set) vs older versions (decode-only or skeleton)? This matches
  the proposal and removes hand-wiring at the application layer.
- Or **leave wiring to applications** and just document the manual recipe?

I recommend generating it — it's mechanical, the data is already in hand, and
without it the facade is hard to use in multi-version setups.

answer: generate, add a flag to disable

## 4. `IBaboonGeneratedLatest` marker — does it exist?

Proposal §4.3/§4.4 uses `where TO : class, IBaboonGeneratedLatest` to constrain
`Convert` and `DecodeFromBinLatest` to types in the most recent version of their
domain. C# `BaboonCodecsFacade.Convert<TFrom, TTo>` already references it
(`where TTo : IBaboonGeneratedLatest`), so it presumably already exists in the
runtime — but does the compiler actually mark generated classes in the latest
domain version with this interface? (Needs to grep generated stubs to confirm.)
If yes, `DecodeFromBinLatest<T>`/`DecodeFromJsonLatest<T>` are mechanical
add-ons. If no, that's a separate codegen change.

answer: We do emit IBaboonGeneratedLatest, you can check that.

## 5. Reference C# implementation availability

Proposal §9 cites a "working C# implementation that has been in production"
(files `BaboonDomainVersion.cs`, `BaboonCodecsFacade.cs`, `BaboonCodecException.cs`,
`Meta/BaboonTypeMeta.cs`, `Meta/BaboonTypeMetaCodec.cs`, `BaboonExt.cs`). Two
questions:

- Can I access those files (paste / point me at a path / commit) so I can
  compare semantics line-by-line before reimplementing or extending? In
  particular: ADT-detection-by-static-type rule (`typeof(T).IsAbstract ||
  typeof(T).IsInterface`) — baboon currently uses `IsInterface` only.
- Was the production envelope written with the proposal exactly, or has it
  drifted? (The exact production wire bytes are the source of truth, not the
  proposal text.)

answer: you can access these files, in  /tmp/exchange/Baboon. DO not add any project-specific details into this repo.

## 6. Logger / tracing surface

Proposal §10.2 punts on logger trait — recommends pure-exception API + adapter
layer. Baboon's current facade already returns `Either<BaboonCodecException, T>`
(no logger trait baked in). Confirm: keep `Either`-returning API as the contract,
do **not** add `FireLogger`-flavoured `TryConvert`/`Decode...Latest(logger, ...)`
overloads from proposal §4? My recommendation: skip them.

answer: no logging needed at this side

## 7. Open question on `$mv` JSON value type

Proposal §10.6 leaves "always write `$mv` vs elide for v1" open and recommends
**(a) always write**. Baboon currently elides on JSON write but reads it back
as a *string* (`byte.TryParse(mvStr, …)`). If we adopt always-write, do we:

- emit `"$mv": 1` (number) per proposal — and update reader to accept number,
- or `"$mv": "1"` (string) per current reader — and write string?

Number is cleaner; string matches current parse path. Cross-language note: a
JSON number-vs-string mismatch tends to bite Java/Kotlin Jackson and Swift
`JSONSerialization` first.

answer: number

## 8. Backwards-compat: existing baboon-emitted byte-16 streams

If any baboon test fixtures or downstream consumers already have byte-16 streams
captured, dropping byte-16 emission breaks them. Are there any captured fixtures
under `test/conv-test*/expected/*` or downstream that depend on byte 16
specifically? `test/cs-stub/BaboonTests/AnyMetaCodecTests.cs` (per the M32
commit) tests at byte 16 — those fixtures will need updating in lockstep with
any byte change.

answer: there is no downstream yet, but there is a problem: our new format supports various forms of metadata
but our legacy format only supports "all metadata together" - so we still need several versions I guess,
but we could reconcile version 16 with version 1.

## 9. Scope of "support proposal format AND new format"

To pin the success criterion: by "support" do you mean

- **decode-only** (legacy producers can write to baboon decoders), or
- **bidirectional** (baboon can also *emit* the legacy format on demand, e.g.
  for a migration window where legacy consumers must keep reading)?

If decode-only, the work is much smaller (one read path). If bidirectional, we
need a writer path + a context flag to select format.

answer: bidirectional of course

---

# Follow-up questions (Q10–Q14) — after reading reference impl

Dated 2026-05-05, after reading `/tmp/exchange/Baboon/`. The first nine answers
left ambiguities and the reference revealed semantic gaps. These pin them down.

## Q10. Pin Q2 — "rewrite to what?"

Q2 answer was "it's not hard to rewrite," — incomplete. Q1 says only byte `1`
is used in production. The natural completion is option (A) "hard unify at byte
1": rewrite `META_VERSION_1` from `16` → `1` across all 11 runtimes, drop byte
`16` entirely. The M32 bump becomes a no-op.

Confirm: **(A) byte-1 unification across all 11 runtimes, byte 16 retired**?
Or did you mean something else?

## Q11. Pin Q8 — "various forms of metadata" at top level?

Q8 mentioned "our new format supports various forms of metadata". I read all
11 in-tree top-level envelope writers — they all emit the same fixed shape
(`metaVersion` + `domain` + `version` + optional `minCompat` + `typeIdentifier`),
identical to proposal/byte 1. So the "various forms of metadata" phrasing
must refer to one of:

- **(a) field-level `AnyMeta`** envelope (the 6 kinds 0x00..0x07 in
  `docs/ueba-format.md` §"Any fields") — that's already shipped in 11 backends
  and is unrelated to the top-level envelope.
- **(b) a planned future shape for the top-level envelope** that hasn't
  landed yet (kind-tagged subset of the 4 fields, similar to AnyMeta).
- **(c) something else.**

Which is it? If (a), there's no top-level "various forms" — your top-level
envelope today is exactly the proposal's, and Q10=(A) is the right call. If
(b), I need the spec/sketch so the byte-1↔byte-16 reconciliation can be
designed in (e.g. byte 1 = "all-fields kind"; byte 16 = "kind-prefix" mode).

## Q12. Forward-compat reader semantics

Reference impl returns `None<BaboonTypeMeta>` on unknown `metaVersion` (both
bin and JSON). Baboon's `BaboonCodecsFacade.DecodeFromBin` currently THROWS
`DecoderFailure("Cannot decode binary type meta")` when meta is null
(`BaboonCodecsFacade.cs:425-426`). JSON path returns `null` (matches
reference).

Should I:
- **(A) match reference exactly** — bin path also returns "no envelope"
  (null/Option.None) for unknown `metaVersion`, callers distinguish "broken"
  vs "not for me"; **OR**
- **(B) keep current throwing behaviour on bin** — argument: a binary stream
  that's not an envelope is a hard error because there's no other way to
  read it.

Recommendation: (A). Lets future `metaVersion=N` payloads coexist with
older readers gracefully, matches JSON path, matches reference. Net: one
fewer exception subclass surfaced to callers on stream-source mismatch.

## Q13. C# `TypeIsAdt` — abstract-base ADTs

Reference: `type.IsAbstract || type.IsInterface`. Baboon C#: `IsInterface`
only (`baboon-runtime/cs/BaboonTypeMeta.cs:98`). If any user models compile
abstract-class ADTs (vs interface ADTs), baboon C# will use the concrete
branch identifier on encode through the parent type instead of the ADT
identifier — the encoded envelope's `typeIdentifier` will then mis-route
on decode.

Are abstract-class ADTs a thing in baboon's C# emit, or is `interface` the
only ADT shape? If abstract is possible, widen baboon to match reference.
Other backends use structural checks (`isInterface`, `isAbstract`-equivalent,
or codegen-marker traits) so this is a C#-specific gap.

## Q14. Per-domain facade — naming + flag

Q3 confirmed "generate, add a flag to disable". Concrete proposal:

- Class name template per backend: `Domain<DomainId>Facade` (PascalCase
  domain identifier with dots stripped). E.g. domain `my.dom` → class
  `DomainMyDomFacade`. Or do you want a different template?
- Compiler flag: `--generate-domain-facade=true` (default on). Or do you
  want it under `--cs-generate-domain-facade` / per-target prefix? The
  baboon CLI today has both shape (`--generate-json-codecs-by-default`
  is global; `--cs-write-evolution-dict` is target-prefixed). The
  facade is target-emitted but conceptually global (every target gets
  one), so I'd default to global flag.

Confirm name template and flag shape, or override.
