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
- `BaboonCompiler.encode(files, pkg, version, idString, json, indexed)` → `Promise<{ success, data?, error? }>`
- `BaboonCompiler.decode(files, pkg, version, idString, data)` → `Promise<{ success, json?, error? }>`

## License

BSD-2-Clause
