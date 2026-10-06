# @7mind.io/baboon

JavaScript bindings for the Baboon compiler and runtime codecs.

Baboon lets you define data models in a compact DSL, generate source code for multiple languages, and encode/decode payloads with version-aware codecs.

## Installation

```bash
npm install @7mind.io/baboon
```

## Usage

```javascript
import { BaboonCompiler } from "@7mind.io/baboon";

const model = `
model example.npm
version "1.0.0"

root data User {
  name: str
  age: i32
}
`;

const inputs = [{ path: "model.baboon", content: model }];

const result = await BaboonCompiler.compile({
  inputs,
  targets: [
    {
      language: "cs",
      generic: {
        generateTests: false,
        generateFixtures: false
      },
      cs: {
        generateJsonCodecs: true,
        generateUebaCodecs: true,
        deduplicate: true
      }
    }
  ],
  debug: false
});

if (!result.success) {
  throw new Error(result.errors?.join("\n") ?? "Compilation failed");
}

const files = { "model.baboon": model };

// Type identifiers follow `<pkg>/<owner>#<name>`, with `:` for top-level types.
const typeId = "example.npm/:#User";

const encoded = await BaboonCompiler.encode(
  files,
  "example.npm",
  "1.0.0",
  typeId,
  JSON.stringify({ name: "Ada", age: 42 }),
  false
);

if (!encoded.success || !encoded.data) {
  throw new Error(encoded.error ?? "Encoding failed");
}

const decoded = await BaboonCompiler.decode(
  files,
  "example.npm",
  "1.0.0",
  typeId,
  encoded.data
);

if (!decoded.success || !decoded.json) {
  throw new Error(decoded.error ?? "Decoding failed");
}

console.log(JSON.parse(decoded.json));
```

## Environment

The entrypoint installs a small SHA-256 shim using Node's built-in `crypto` module. For browser or sandboxed runtimes, provide a `globalThis.sha256` constructor with `update` and `digest` methods before importing `@7mind.io/baboon`.

## API surface

- `BaboonCompiler.compile(options)` → `Promise<{ success, files?, errors? }>`
- `BaboonCompiler.load(files)` → `Promise<BaboonLoadedModel>`
- `BaboonCompiler.loadMany({ bytes } | { base64 })` → `Promise<BaboonLoadedModel>`
- `BaboonCompiler.listTypes(model)` → `BaboonTypeInfo[]`
- `BaboonCompiler.generateRandom(model, pkg, version, idString)` → `{ success, json?, error? }`
- `BaboonCompiler.encode(files, pkg, version, idString, json, indexed)` → `Promise<{ success, data?, error? }>`
- `BaboonCompiler.encodeLoaded(model, pkg, version, idString, json, indexed)` → `Promise<{ success, data?, error? }>`
- `BaboonCompiler.decode(files, pkg, version, idString, data)` → `Promise<{ success, json?, error? }>`
- `BaboonCompiler.decodeLoaded(model, pkg, version, idString, data)` → `Promise<{ success, json?, error? }>`
- `BaboonCompiler.encodedEnvelopeLoaded(model, jsonEnvelope, { envelopeVersion, indexed })` → `Promise<{ success, data?, error? }>`
- `BaboonCompiler.decodeEnvelopeLoaded(model, uebaEnvelope)` → `Promise<{ success, json?, error? }>`
- `BaboonCompiler.cleanupScheme(files, domain, version)` / `cleanupSchemeLoaded(model, domain, version)` → `Promise<{ success, content?, error? }>`

`load` and `loadMany` reject with an `Error` whose message starts with `Loading failed:`;
model issues are listed with the paths of the files (or archive entries) they come from.

### Loading a schema archive

`loadMany` loads every schema of a ZIP archive held in memory, together, through the
same pipeline as `load`, and returns the same handle:

```javascript
// e.g. written by: baboon --model-dir ./models :scheme --domains="*@*" --zip-output=schemas.zip
const model = await BaboonCompiler.loadMany({ bytes: new Uint8Array(await response.arrayBuffer()) });
const same = await BaboonCompiler.loadMany({ base64: "UEsDBAoAAAg..." }); // plain base64, not a data: URL
```

Pass exactly one of `bytes` (a `Uint8Array`, Node's `Buffer` included) or `base64` (the
standard alphabet; no `data:` prefix, no whitespace). Archive contract:

- entries must be stored (compression method 0) — what `baboon :scheme --zip-output`
  writes and `zip -0` produces; deflated entries, encryption and ZIP64 are rejected;
- entry paths must be relative `/`-separated paths without empty, `.` or `..` segments,
  backslashes or drive prefixes, and unique;
- directory entries are ignored; `*.baboon` entries are the schemas (UTF-8, at least one);
  `*.bmo` entries are only available to `include`, whose paths resolve against the
  archive root; any other entry is rejected;
- CRC-32 checksums are verified.

Archives are read in memory with no extra dependency, in Node and in browsers alike.

### Converting envelopes

The facades of the generated runtimes wrap values in a top-level envelope that names
the domain, version and type (`docs/spec/codec-envelope.md`). `encodedEnvelopeLoaded` turns
a JSON envelope into a binary UEBA one; `decodeEnvelopeLoaded` does the reverse:

```javascript
const json = JSON.stringify({ $mv: 1, $d: "example.npm", $v: "1.0.0", $t: typeId, $c: { name: "Ada", age: 42 } });
const bin = await BaboonCompiler.encodedEnvelopeLoaded(model, json, { envelopeVersion: 1, indexed: false });
const back = await BaboonCompiler.decodeEnvelopeLoaded(model, bin.data); // back.json === json
```

- Both options are required: `envelopeVersion` is the binary layout (`1`, the facades'
  default, carries one compatibility bound; `2` carries both and is only readable by
  current runtimes), `indexed` the UEBA index mode (facades default to compact, `false`).
  Binary input is self-describing (v1 or v2, compact or indexed), so decoding takes no options.
- This is format conversion, not migration: the domain, version and full type identifier
  (`pkg/:#Name`, `pkg/ns#Name`, `pkg/[pkg/:#Adt]#Branch`) are taken from the input and kept.
  The payload is read with that exact version. A writer newer than every loaded version is
  converted only when its envelope proves the payload byte-identical to a loaded version
  (`$uv`, or binary v2 `minCompat`); binary v1 envelopes cannot prove that and are refused.
- Compatibility bounds are recomputed from the loaded model for the output format: JSON
  `$rv` (json-additive) and binary `readableMin` (prefix-compact / prefix-any-mode) promise
  different things and are never copied across. Bounds computed from a model that holds
  only part of a domain's lineage are conservative.
- Errors are reported in `error`: malformed or truncated envelopes, unsupported
  `metaVersion` or flags, bounds out of order, unknown domains, versions or types, JSON
  fields the type does not declare, trailing bytes, and values the runtime codec cannot
  convert (`any` fields, foreign types without an `rt` mapping). JSON `$mv` may be the
  number `1` or the legacy string `"1"`, or absent.

### Performance Optimization

For repeated operations, load the model once and reuse it. This skips parsing and validation steps on each call:

```javascript
// 1. Load model once
const model = await BaboonCompiler.load(files);

// 2. Encode/Decode multiple times efficiently
const encoded = await BaboonCompiler.encodeLoaded(
  model,
  "example.npm",
  "1.0.0",
  typeId,
  jsonPayload,
  false
);

const decoded = await BaboonCompiler.decodeLoaded(
  model,
  "example.npm",
  "1.0.0",
  typeId,
  encoded.data
);
```

## License

BSD-2-Clause
