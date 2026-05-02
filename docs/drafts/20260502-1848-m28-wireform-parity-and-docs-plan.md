# M28 — Wire-form parity hardening + docs catch-up

**Date:** 2026-05-02 (continuing same-day from M27 close).
**Goal:** Close the three wire-form divergences M26 deferred (Scala u64 JSON `KeyDecoder`; cross-backend f64 number-to-string canonicalisation; cross-backend tso UTC-offset formatting). Lift m26 cross-language fixture from 5/8 to 8/8 builtin map-key types. Refresh user-facing docs for M18-M27 features. Wire-form correctness across 9 codec-emitting backends is non-negotiable.

## PR breakdown

### PR-28.1 — Scala u64 JSON KeyDecoder + value encoder

**Scope:** Scala only.
- `baboon-compiler/src/main/resources/baboon-runtime/scala/BaboonCodecs.scala` — add `decodeKeyU64: KeyDecoder[Long]` (parses string via `BigInt`/`toUnsignedBigInt`, narrows to `Long` two's-complement preserving).
- `ScJsonCodecGenerator.scala:447` — emit `decodeKeyU64` for u64 map keys (was `decodeLong`).
- Encode side: u64 map-key emit uses `java.lang.Long.toUnsignedString(_)` (matches existing `BaboonRuntimeShared.scala:132 u64ToString`).

**Acceptance:** Scala test fixture with `data Holder { mu64: map[u64, str] }` derived `[json]` compiles and round-trips `Long.MaxValue` → `"9223372036854775807"`, `-1L` → `"18446744073709551615"`, `0L` → `"0"`.

**Complexity:** Small. **Dependencies:** None.

### PR-28.2 — f64 cross-backend canonicalisation **[DEFERRED — second time]**

**Status (2026-05-02):** Deferred for the second time. First deferral was at M26 PR-26.5 (dropped f64 from the cross-language map-key fixture). The M28 audit confirms 6 of 9 backends already emit canonical `<int>.<frac>` form natively (Scala/Rust/Kotlin/Java/Dart/Python — verified by per-backend probes), but the remaining 3 (TypeScript / Swift / C#) cannot be retrofitted without an architectural decision the M28 wave cannot make in isolation. See defect `[PR-28.2-D01]` for the full per-backend audit table, runtime probe results, and resolution path. The m24 + m26 baselines are unaffected (no f64 field in either fixture today). The work is unblocked once the architectural choice (custom stringifier replacing `JSON.stringify` / `JSONSerialization`, vs schema-aware post-processor at the JSON-string layer, vs string-encoded f64 uniformly across backends) is made at the project level.

**Original goal (kept here for traceability):**

**Canonical:** `<int>.<frac>` form always. Integer-valued doubles render `"42.0"`. Scientific notation only when no fixed-point form fits IEEE-754 round-trip in ≤ 17 significant digits.

**Per-backend audit:**
| Backend | Current | Action |
|---|---|---|
| Scala | Circe `fromDouble` already `42.0`-form | Verify (likely no-op) |
| C# | `Double.ToString(InvariantCulture)` → `"42"` | Add runtime helper `BaboonDoubleFormat.format` forcing `.0` for integral |
| Rust | `serde_json` already `"42.0"`-form | Verify (likely no-op) |
| TypeScript | `JSON.stringify(42.0)` → `"42"` | Custom number-text emitter |
| Kotlin/Java | Jackson `42.0` | Verify (likely no-op) |
| Dart | `42.0.toString()` → `"42.0"` | Verify (likely no-op) |
| Swift | `JSONSerialization` → `42` | Pre-stringify or raw concat (mirrors PR-26.5 pattern) |
| Python | `str(42.0)` / `json.dumps` → `"42.0"` | Verify (likely no-op) |

Decoder MUST accept either form for forward compat.

**Complexity:** Large. **Dependencies:** None. Coordinated single PR across 9 backends.

### PR-28.3 — tso cross-backend UTC-offset canonicalisation

**Canonical:** `±HH:MM` always; UTC renders `+00:00` (NOT `Z`). Rationale: keeps `tso` ↔ `tsu` distinguishable on round-trip; `tsu` owns `Z` semantics.

**Per-backend audit:**
| Backend | Current UTC emit | Action |
|---|---|---|
| Scala (`XXX` formatter) | `Z` | Switch to `xxx` lower or `DateTimeFormatterBuilder.appendOffset("+HH:MM", "+00:00")` |
| Kotlin (same) | `Z` | Same fix |
| Java (same) | `Z` | Same fix |
| C# (`zzz`) | `+00:00` | Verify; ensure encoder uses `TszDefault` not `TsuDefault` for tso |
| Rust (`%:z`) | `+00:00` | Verify (no-op) |
| TypeScript | `Z` (offset 0 short-circuit at `BaboonSharedRuntime.ts:302-311`) | Drop short-circuit |
| Dart | `+HH:MM` already | Verify (no-op) |
| Swift | TBD audit | Likely same `Z`-shortcut removal |
| Python | TBD audit | TBD |

Decoder MUST accept both forms.

**Complexity:** Medium. **Dependencies:** None.

### PR-28.4 — Extend m26 fixture with mu64/mf64/mtso

**Scope:** Append three fields to `test/conv-test/m26-builtin-map-keys.baboon`:
```
mu64: map[u64, str]
mf64: map[f64, str]
mtso: map[tso, str]
```
Sample values:
- `mu64`: `{"18446744073709551615" -> "vu64max", "0" -> "vu64zero"}`
- `mf64`: `{"42.0" -> "vf42", "0.1" -> "vf0p1"}`
- `mtso`: `{"2026-05-02T12:00:00.000+00:00" -> "vtso_utc", "2026-05-02T12:00:00.000+05:30" -> "vtso_ist"}`

Update 9 backends' CompatMain. Regenerate canonical reference JSON; lock new md5.

**Acceptance:** All 9 backends byte-identical JSON; Swift parse-equivalent; UEBA 10-of-10 byte-identical; new md5 locked in tasks.md M26-N02.

**Complexity:** Medium. **Dependencies:** PR-28.1 + 28.2 + 28.3 ALL merged.

### PR-28.5 — Stale path fix in `docs/spec/identifier-repr.md`

**Scope:** Line 510. Note CLAUDE.md is already clean per audit; only the docs reference is stale (package directory `io/septimalmind/baboon/tests/` not the `.jvm/` segment).

**Complexity:** Trivial.

### PR-28.6 — Docs catch-up M18-M27

**Scope:** Three published doc files:
1. **`docs/language-features.md`**: new sections for `id` keyword (M18), ADT branch inheritance (M20), id+DTO map keys (M19).
2. **`docs/json-codecs.md`**: extend "Map keys" section; add "Custom-foreign key codecs (M24)" subsection; lock canonical f64/tso forms after PR-28.2/28.3 land.
3. **`README.md` Highlights**: add `id` types, ADT branch inheritance, cross-language wire-form parity matrix.

**Complexity:** Medium.

## Parallelisation plan

**Wave 1 (parallel):** PR-28.1, PR-28.2, PR-28.3, PR-28.5, PR-28.6 (M18/M19/M20/M24 sections only — defer f64/tso lock-in subsections to Wave 2).

**Wave 2 (sequenced after 28.1+28.2+28.3 merge):** PR-28.4 + PR-28.6 lock-in subsections.

## Verification gates

**Per-PR:** see PR sections.

**Milestone close:**
- `mdl :ci` PASS.
- Extended m26 md5 locked across 10 backends; recorded in M26-N02 amendment.
- `command grep -rn "\.jvm/src/test/scala/io/septimalmind/baboon/tests/IdentifierReprPropertyTest" docs/` empty.

## Cross-cutting architectural notes (for tasks.md)

- **M28-N01 — Wire-form canonical for f64 and tso.**
  - f64: JSON number rendered as `<int>.<frac>` always; integer-valued doubles render with `.0` suffix; scientific notation only when no fixed-point form fits IEEE-754 round-trip in ≤ 17 significant digits.
  - tso: ISO-8601 with `±HH:MM` offset always; UTC renders `+00:00`, NOT `Z`. `tsu` retains its existing `...Z` form.
  - Decoders MUST tolerate the alternative form (legacy `42` for f64, legacy `Z` for tso).

- **M28-N02 — Scala u64 JSON map-key encoding strategy.**
  - Encode: `java.lang.Long.toUnsignedString(v)`.
  - Decode: `BaboonJsonCodec.decodeKeyU64` parses via `BigInt`/`toUnsignedBigInt`, narrows to `Long`.
  - Symmetric with value-side `circeJson.fromBigInt(toUnsignedBigInt(_))`.

## Re-sized vs brief

- **CLAUDE.md path fix:** audit shows CLAUDE.md is already clean. Only `docs/spec/identifier-repr.md:510` has the stale package directory. PR-28.5 narrowed accordingly.
- **`tasks.md` M26-N02 line 415 has inverted Scala-vs-JS wording for tso UTC.** Correct facts go in M28-N01; the M26-N02 line itself updates when M28 closes.
