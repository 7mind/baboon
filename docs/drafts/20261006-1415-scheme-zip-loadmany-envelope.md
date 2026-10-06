# Scheme ZIP export, JS `loadMany`, envelope-aware JSON↔UEBA conversion

Status: implemented (2026-10-06); the plan below is annotated with what the evidence showed. No `AGENTS.md` exists in this repository;
`CLAUDE.md` is the governing guide.

## 1. CLI: `:scheme` modes

Two mutually exclusive modes, validated before any model is loaded:

| Mode    | Flags                                         | Output                                   |
|---------|-----------------------------------------------|------------------------------------------|
| single  | `--domain D --version V [--target F]`         | `F`, or stdout (pure DSL) when absent    |
| archive | `--domains SELECTORS --zip-output F`          | ZIP archive `F`                          |

Rejected: any single-mode flag combined with any archive-mode flag; `--domain` without
`--version` (and vice versa); `--domains` without `--zip-output` (and vice versa); no flags.

Selectors: comma-separated `domain@version` items; each item is trimmed; each component is
either the whole-component wildcard `*` or an exact value (dotted identifier / Baboon
version). Malformed items (empty, no `@`, several `@`, partial wildcards such as `my.*`,
internal whitespace, unparseable versions) are rejected. Matches are unioned and
deduplicated; a selector matching nothing is an error naming the selector and the
available domains/versions.

### Cross-version references in rendered schemas

`BaboonSchemeRenderer` renders one version self-contained (imports flattened), but keeps
evolution metadata that is relative to the *immediately preceding* version of the lineage:
type renames `: was[Old]`, field renames `f: T was old`, enum member renames. On reload the
comparator diffs consecutive *loaded* versions.

Consequence: a selection with a gap (e.g. `1.0.0,3.0.0` of `1,2,3`) cannot be reloaded
faithfully. Observed (`SchemeArchiveTest`, fixture `scheme-zip-ok`):

- with a type rename aimed at the skipped version (`New : was[Mid]`, `Mid` only in 2.0.0),
  the reload fails: `Evolution(InvalidTypeRename(zipdemo.evo/:#New, zipdemo.evo/:#Mid))`
  (the planning hypothesis that the rename would be dropped silently was wrong);
- with a type that changed in 2.0.0 and reverted in 3.0.0 (`zipdemo.revert/Flip`), the reload
  succeeds but fabricates a `1.0.0->3.0.0` step and claims `sameIn(3.0.0) = [1.0.0, 3.0.0]`
  (truth: `[3.0.0]`): a byte-identity bound a real 2.0.0 reader would trust and misread.

Policy: a selection must be **contiguous per domain** (any run `v_i..v_j` of the lineage's
sorted versions). Gaps are rejected with an error listing the omitted versions. Nothing is
added silently and no evolution metadata is discarded. Contiguous runs that start after the
lineage's first version reload correctly: the first selected version keeps its `was`
metadata in `Domain.renames`, no comparison is made against the absent predecessor, and
compatibility bounds computed from the run are conservative (never wider than the truth).

### Archive contract (written by the CLI, read by `loadMany`)

- Entries: one UTF-8 file per selected domain/version at `schemas/<domain>/<version>.baboon`
  (domain is dotted, e.g. `schemas/my.domain/1.0.0.baboon`). No directory entries.
- Compression method 0 (stored), sizes and CRC-32 in the local header (no data descriptor),
  DOS timestamp fixed at 1980-01-01 00:00:00, no extra fields, no comments, entries sorted by
  name. Identical inputs produce identical bytes.
- Written by the shared `StoredZipWriter`, not `java.util.zip`: on JDK 25, `ZipEntry.setTimeLocal`
  with 1980-01-01T00:00:00 hits the `DOSTIME_BEFORE_1980` sentinel, so `ZipOutputStream` adds an
  extended-timestamp (`UT`) extra field computed through the default time zone, and the bytes
  change with `TZ` (caught by the time-zone determinism test).
- Written to a sibling temporary file and atomically moved into place; nothing is published
  when selection, rendering or writing fails.

Reader (`loadMany`) accepts any archive within these rules:
- Method 0 only; other methods, encryption, ZIP64 and multi-disk archives are rejected.
- Paths must be relative, `/`-separated, without empty, `.` or `..` segments, backslashes,
  drive prefixes or NUL; names are UTF-8 (flag bit 11) or ASCII. Duplicate paths are rejected.
- Directory entries (`…/`) are ignored. `*.baboon` entries are models; `*.bmo` entries are
  include files only. Any other file entry is rejected.
- `include "p"` resolves `p` against the archive root (the archive plays the role of a
  `--model-dir`).
- Content must be valid UTF-8; at least one `*.baboon` entry is required. CRC-32 is verified.

## 2. JavaScript API

```ts
export type BaboonArchiveInput =
  | { bytes: Uint8Array; base64?: never }
  | { base64: string; bytes?: never };

loadMany(archive: BaboonArchiveInput): Promise<BaboonLoadedModel>;

export interface BaboonEnvelopeEncodeOptions {
  envelopeVersion: 1 | 2;   // binary BaboonTypeMeta layout (facade default: 1)
  indexed: boolean;         // UEBA index mode (facade default: compact = false)
}

encodedEnvelopeLoaded(model, jsonEnvelope: string, options: BaboonEnvelopeEncodeOptions)
  : Promise<BaboonEncodeResult>;          // {success, data?, error?}
decodeEnvelopeLoaded(model, uebaEnvelope: Uint8Array)
  : Promise<BaboonDecodeResult>;          // {success, json?, error?}
```

`loadMany` validates exactly one own key (`bytes` is a `Uint8Array`, or `base64` is a
string in the standard alphabet — not a data URL), reads entries in memory and loads every
schema together through the existing loader. It rejects like `load`: an `Error` whose
message starts with `Loading failed:`; model issues are listed with their archive paths.

`decodeEnvelopeLoaded` takes no options: the binary envelope self-describes its layout and
the payload its index mode, and the JSON envelope has a single layout.

## 3. Envelope conversion semantics

Wire codec: the runtime's `object BaboonTypeMetaCodec` is mirrored byte-for-byte into the
compile-side `baboon.runtime.shared` sources (the established `IdentifierRepr` pattern),
pinned by a parity test. Payloads go through the interpreted `BaboonRuntimeCodec`.

- Identity (`$d`, `$v`, `$t`) is copied from input to output unchanged.
- Read version: the writer's exact version when loaded. Otherwise only a writer *newer*
  than every loaded version is accepted, and only losslessly (`ForwardReadPolicy.Lossless`):
  its byte-identical bound (`$uv`; v2 `minCompat`) must reach a loaded version; the newest
  loaded codec is then used. Binary v1 envelopes from unknown versions are rejected: the
  v1 slot may carry a Tolerant writer's prefix bound and cannot prove byte-identity.
- Bounds are recomputed from the loaded model at the read version: `minCompat` =
  `sameIn.head`; JSON `$rv` = `json-additive`; v2 `readableMin` = `prefix-compact` /
  `prefix-any-mode` by index mode; v1 uses `ForwardWritePolicy.Strict`.
- Strictness: unknown envelope keys, unknown DTO JSON fields, trailing payload bytes,
  invalid `$mv`/flags/versions/bounds ordering, truncated input, unknown domain/version/type,
  and non-envelope types (foreign, contract, service) are explicit errors. `any` fields and
  no-rt foreign values keep the interpreter's existing rejection; no-rt foreign map keys
  stay verbatim strings.

## 4. Defects found on the way

- `main` printed the `Baboon <version>` banner to stdout in `:scheme` stdout mode, so
  `:scheme ... > file.baboon` (documented) produced an invalid file; `SchemeStdoutPurityTest`
  drives the seam, not `main`. Fixed: the banner goes to stderr for `:scheme` (as for `:lsp`).
- `index.d.ts` did not declare the exported `listTypes` and `generateRandom`. Fixed.
- The runtime codec rejected `u32` values >= 2^31 on encode although its decoder produces
  them, silently wrapped out-of-range `i08`/`u08`/`i16`/`u16`/`i64`/`u64` JSON values, and threw
  `NumberFormatException` for valid integer map keys above the signed range (`u08` `"255"`,
  `u32`, `u64`) — https://github.com/7mind/baboon/issues/97. Fixed: one range-checked path for
  values and map keys; each type accepts exactly its range and rejects the rest with
  `ExpectedJsonNumber` (`RuntimeCodecIntegerRangeTest`). This changes `encode`/`encodeLoaded` on
  invalid input from wrapping to an error.
- `test-manual-python` read every language's compat output but depended only on
  `test-gen-compat-python`; `test-manual-scala` lacked `python` and `kotlin-kmp`. Under
  parallel `mdl` the lanes raced the generators — https://github.com/7mind/baboon/issues/98.
  Fixed: both lanes depend on every generator they read.

## 5. Verification

1. JVM: selector/mode parsing, contiguity, archive determinism (incl. timezone), atomic
   write, reload of every selection (multi-domain, rename), reader rejections, envelope
   goldens (v1/v2, compact/indexed), strictness and lossless-forward cases.
2. Scala.js: `+compile`; npm `test.js` against `fastLinkJS` output, using a ZIP produced by
   the native CLI (bytes + base64).
3. `mdl :build :test` with a new lane wiring (2) into CI.
