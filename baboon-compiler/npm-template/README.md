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

The package is plain ES modules with no runtime dependencies: the compiler is a single
`main.js`, and it needs no host-provided globals, so the same files run in Node, in
browsers and in Web Workers.

## Running in a Web Worker

Loading and type-checking a model is synchronous and CPU-bound: a large multi-version
archive can keep a thread busy for seconds. To keep a page responsive, run the compiler
in a Web Worker. `worker.js` is a ready-made module-worker entry, and `connectBaboonWorker`
gives the main thread the same methods as `BaboonCompiler`, each returning a `Promise`;
the main thread never downloads or parses the compiler bundle:

```javascript
// baboon.worker.js — the worker's entry, one line:
import "@7mind.io/baboon/worker.js";
```

```javascript
import { connectBaboonWorker } from "@7mind.io/baboon/worker-client.js";

// `new Worker(new URL("./x.js", import.meta.url), { type: "module" })` is the form bundlers
// document for emitting a module worker with its dependencies.
const worker = new Worker(new URL("./baboon.worker.js", import.meta.url), { type: "module" });
const baboon = connectBaboonWorker(worker);

const model = await baboon.loadMany({ bytes });                 // runs in the worker
const result = await baboon.decodeEnvelopeLoaded(model, envelope);
await baboon.release(model);                                    // frees it in the worker
```

Without a bundler, point the worker at the package file itself
(`new Worker("/node_modules/@7mind.io/baboon/worker.js", { type: "module" })`, or wherever it
is served).

- Models stay in the worker; the main thread holds opaque handles, usable only through
  the connection that loaded them, until `release(model)`.
- Arguments and results are copied with the structured-clone algorithm (no transfer):
  the caller keeps its buffers.
- A worker-side failure rejects with the same message the in-thread call would; if the
  worker script fails to load or crashes, pending and later calls reject.
- To serve on another port (a `MessagePort`, Node's `worker_threads`), call
  `serveBaboon(port)` from `@7mind.io/baboon/worker-server.js` in the worker and
  `connectBaboonWorker(otherPort)` on the other side.

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

`loadMany` loads every schema of a schema archive (a ZIP file, conventionally `*.bsm`)
held in memory, together, through the same pipeline as `load`, and returns the same handle:

```javascript
// e.g. written by: baboon --model-dir ./models :scheme --domains="*@*" --zip-output=schemas.bsm
const model = await BaboonCompiler.loadMany({ bytes: new Uint8Array(await response.arrayBuffer()) });
const same = await BaboonCompiler.loadMany({ base64: "UEsDBAoAAAg..." }); // plain base64, not a data: URL
```

Pass exactly one of `bytes` (a `Uint8Array`, Node's `Buffer` included) or `base64` (the
standard alphabet; no `data:` prefix, no whitespace). Archive contract:

- entries must be stored (method 0, what `baboon :scheme --zip-output` writes) or
  deflated (method 8, what zip tools write by default); other methods, encryption and
  ZIP64 are rejected;
- entry paths must be relative `/`-separated paths without empty, `.` or `..` segments,
  backslashes or drive prefixes, and unique;
- directory entries are ignored; `*.baboon` entries are the schemas (UTF-8, at least one);
  `*.bmo` entries are only available to `include`, whose paths resolve next to the
  including entry first, then against the archive root; any other entry is rejected;
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
