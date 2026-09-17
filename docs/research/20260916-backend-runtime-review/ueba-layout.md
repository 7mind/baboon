# Proposal 5: shared UEBA layout and release validation

Baseline: `1ef60401`. The initial scope was a C#/Scala pilot followed by all
remaining backends. The user then requested the same strict checks everywhere,
including release builds. This is therefore both a refactor and a deliberate
tightening of validation, not an output-preserving refactor as a whole.

## Implementation

- `UebaLayoutPlan` centralizes ordered field metadata, indexed-field selection,
  ADT branch ordinals and length constraints for all nine generators. It reuses
  `BinReprLen`; it does not cache layouts or change the wire representation.
- `UebaLengthCheckRenderer` shares predicate traversal and numeric comparisons.
  Membership syntax and error reporting remain language-specific.
- Indexed encoding validates fixed lengths, allowed alternatives and range
  bounds. Variable-length index entries require positive lengths and signed-i32
  offsets/lengths, matching the existing Scala/Kotlin reader domain.
- Index readers reject negative/out-of-range values, zero lengths and overlapping
  entries using widened arithmetic. Gaps remain valid. Decoders enforce the
  expected index count when indexing is required and validate wrapped ADT markers.
- These checks no longer depend on assertions/debug configuration. Swift's
  non-throwing encoder reports an invariant violation with unconditional
  `fatalError`; Swift decoding throws. Rust uses its existing `Result` contracts.
  Other backends use their ordinary exception/error mechanisms.
- Swift's read/consume paths now share one checked index loop, with a count-only
  path that avoids constructing index entries. TypeScript uses a common runtime
  index-consumption helper instead of duplicating the loop in generated DTOs.

Scope excludes wholesale primitive-decoder hardening, validation of compact
encoder field lengths, and checking index entries against actual decoded payload
boundaries. Timestamp formats and timestamp runtime implementations are unchanged.

## Reproduced defects corrected

- C# Release accepted invalid index entries, wrong fixed field lengths, missing
  required indices and corrupt wrapped markers. Added tests failed before the
  correction because expected exceptions were absent.
- Scala's corrupt wrapped marker escaped its `Either` contract as an
  `AssertionError`; it now returns `Left`.
- TypeScript ignored malformed index entries and missing required indices,
  accepted a custom codec with the wrong fixed length, and treated custom
  `useIndices=true` contexts as compact because it compared context identity.
  All four regression tests failed against the baseline for those reasons.
- Actual generated Scala and Kotlin wrapped codecs failed to decode branch 128
  from the valid bytes `[0x80, 0]`: signed byte reads were compared to unsigned
  ordinals. Both failures were reproduced independently before correcting the
  reads. A persistent model and harness test ordinals 0, 127 and 128 and reject a
  mismatched marker; the harness is included in `mdl :test`.

## Measurements

`cloc` 2.10, `--skip-uniqueness`, production translators plus embedded runtimes;
tests, documentation, comments and blank lines excluded:

| State | Translator code | Runtime code | Total |
|---|---:|---:|---:|
| Baseline | 40,838 | 28,483 | 69,321 |
| Completed implementation | 40,804 | 28,506 | 69,310 |

Net reduction: **11 production lines** (34 fewer translator lines, 23 additional
runtime lines). The substantive change is replacing independent layout policies
with one shared policy while adding enforcement. Generated output intentionally
changes. Execution time and memory consumption were not benchmarked; additional
validation is not claimed as a performance improvement.

The same 51-configuration corpus still contains 42,180 generated files, with no
added or removed paths. Its source bytes increased from 134,966,493 to 138,105,295
(+3,138,802; 2.3%) because more checks are emitted. This corpus includes generated
tests, fixtures and repeated runtime copies; it is not a deployed binary size.

## Validation

- Before editing: JVM/Scala.js compilation and the 19 existing shared-planner
  tests passed. Captured 42,180 generated files across 51 configurations.
- The initial C#/Scala layout-only extraction preserved every generated file.
- C#/Scala enforcement stage: C# Release passed 594 tests in each ADT mode;
  Scala passed 519 regular tests (166 expected cancellations) and 685 wrapped
  tests. Six new behavior-level layout-planner tests passed on the compiler.
- The later predicate-renderer extraction was checked independently: all 42,180
  files across the 51 configurations were byte-identical before and after it.
- The first cross-language run exposed two implementation errors: generated Rust
  encoder errors needed explicit `io::Error` conversion, and Swift encoder checks
  attempted to throw through a non-throwing API. Both were corrected before final
  validation.
- Standalone probes against the final generated runtimes passed with Dart AOT
  (assertions verified disabled), Swift `-O`, and Python `-O`: all five invalid
  index shapes were rejected, including both materializing and count-only paths
  where available. Swift's executable was launched inside the project's FHS
  toolchain environment, avoiding the host/toolchain libc mismatch.
- Checked-in focused tests also passed with Rust `--release` (2 tests), Scala
  `-Xdisable-assertions` (4), Java `-DenableAssertions=false` (4), Python `-O` (3),
  and Kotlin JVM/KMP's Gradle `enableAssertions=false` (2 each). The four TypeScript
  regressions passed against the final generated code.
- The fresh serialization acceptance run passed all 200 combinations. The
  subsequent Kotlin JVM regular compile saturated its default 512 MiB heap:
  `jstat` measured over 1,500 full collections and roughly 200 seconds in full
  GC, with old-generation occupancy above 99%. That invocation was stopped;
  the Kotlin fixture project now explicitly allows a 2 GiB compiler heap.
  The same configuration is copied into the branch-ordinal regression project.
- Kotlin regular/wrapped then passed in 41/40 seconds, and both manual lanes
  passed. The first integrated run of the new ordinal harness caught a fatal
  Scala discarded-value warning in its test helper; the helper now explicitly
  discards the assertion result instead of relying on implicit Unit conversion.

### Final CI result

`mdl --par --continue --keep-run-dir --simple-log :build :test :ci` completed
successfully in run `20260917-225441-561283027`: **206 actions passed**, with 181
executed in that run and 25 restored from the preparatory runs
`20260917-223329-892845374` and `20260917-225126-144050939`. No production source
changed between these runs. The only intervening implementation changes were the
explicit Kotlin fixture heap and the new ordinal test's discarded-value fix;
the affected Kotlin suites and ordinal harness were rerun successfully.

- JVM/Scala.js build and **860 compiler tests in 139 suites** passed, with no
  failed, canceled or aborted compiler tests/suites.
- All backend regular/wrapped, manual, wiring and MCP lanes passed, as did native
  smoke and editor-grammar checks. C# Release ran 594 tests in each ADT mode;
  Scala ran 674 regular tests and 683 wrapped tests.
- Serialization acceptance: **200/200**. Service acceptance: **162/162**.
- The integrated ordinal harness passed on Scala and Kotlin.
- Focused release/optimized checks, Scala formatting checks, shell syntax checks
  and `git diff --check` passed.
