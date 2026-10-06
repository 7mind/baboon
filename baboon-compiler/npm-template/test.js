import test from "ava";
import { readFileSync } from "fs";

import { BaboonCompiler } from "./index.js";

const MODEL = `
model example.npm
version "1.0.0"

root data User {
  name: str
  age: i32
}
`;

const MODEL_PATH = "model.baboon";
const TYPE_ID = "example.npm/:#User";
const FILES_MAP = { [MODEL_PATH]: MODEL };
const FILES_LIST = [{ path: MODEL_PATH, content: MODEL }];

const TARGETS = [
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
];

test("BaboonCompiler exports", t => {
  t.is(typeof BaboonCompiler.compile, "function");
  t.is(typeof BaboonCompiler.encode, "function");
  t.is(typeof BaboonCompiler.decode, "function");
});

test("Compiles a simple model", async t => {
  const result = await BaboonCompiler.compile({
    inputs: FILES_LIST,
    targets: TARGETS,
    debug: false
  });

  if (!result.success) {
    t.fail(result.errors?.join("\n") ?? "Compilation failed");
    return;
  }

  t.true(Array.isArray(result.files));
  t.true((result.files?.length ?? 0) > 0);
});

test("Encodes and decodes data", async t => {
  const encoded = await BaboonCompiler.encode(
    FILES_MAP,
    "example.npm",
    "1.0.0",
    TYPE_ID,
    JSON.stringify({ name: "Ada", age: 42 }),
    false
  );

  if (!encoded.success || !encoded.data) {
    t.fail(encoded.error ?? "Encoding failed");
    return;
  }

  const decoded = await BaboonCompiler.decode(
    FILES_MAP,
    "example.npm",
    "1.0.0",
    TYPE_ID,
    encoded.data
  );

  if (!decoded.success || !decoded.json) {
    t.fail(decoded.error ?? "Decoding failed");
    return;
  }

  const payload = JSON.parse(decoded.json);
  t.deepEqual(payload, { name: "Ada", age: 42 });
});

test("Encodes and decodes data using loaded model", async t => {
  const model = await BaboonCompiler.load(FILES_MAP);
  t.truthy(model, "Model should be loaded");

  const encoded = await BaboonCompiler.encodeLoaded(
    model,
    "example.npm",
    "1.0.0",
    TYPE_ID,
    JSON.stringify({ name: "Ada", age: 42 }),
    false
  );

  if (!encoded.success || !encoded.data) {
    t.fail(encoded.error ?? "Encoding failed");
    return;
  }

  const decoded = await BaboonCompiler.decodeLoaded(
    model,
    "example.npm",
    "1.0.0",
    TYPE_ID,
    encoded.data
  );

  if (!decoded.success || !decoded.json) {
    t.fail(decoded.error ?? "Decoding failed");
    return;
  }

  const payload = JSON.parse(decoded.json);
  t.deepEqual(payload, { name: "Ada", age: 42 });
});

const API_DECLARATION = /export interface BaboonCompilerAPI \{([\s\S]*?)\n\}/;

function declaredMethods() {
  const source = readFileSync(new URL("./index.d.ts", import.meta.url), "utf8");
  const body = API_DECLARATION.exec(source);
  if (!body) {
    throw new Error("index.d.ts declares no BaboonCompilerAPI interface");
  }
  return [...body[1].matchAll(/^\s{2}(\w+)\(/gm)].map(m => m[1]).sort();
}

// The converse (nothing exported is undeclared) needs unminified member names; it is checked
// against the fastLinkJS bundle by declarations-acceptance.mjs (mdl action test-js-npm).
test("every method index.d.ts declares is exported", t => {
  const declared = declaredMethods();
  t.true(declared.length > 0);
  for (const name of declared) {
    t.is(typeof BaboonCompiler[name], "function", `${name} is declared but not exported`);
  }
});

test("loadMany validates its argument like a strict discriminated union", async t => {
  const cases = [
    [undefined, /exactly one of 'bytes'/],
    [null, /exactly one of 'bytes'/],
    ["UEsFBg==", /exactly one of 'bytes'/],
    [new Uint8Array(0), /exactly one of 'bytes'/],
    [{}, /got keys: $/],
    [{ bytes: new Uint8Array(0), base64: "" }, /got keys: bytes, base64/],
    [{ bytes: [80, 75] }, /'bytes' must be a Uint8Array/],
    [{ base64: 42 }, /'base64' must be a string/],
    [{ base64: "data:application/zip;base64,UEsFBg==" }, /not valid base64/],
    [{ base64: "UEsF Bg==" }, /not valid base64/],
    [{ base64: "" }, /not a ZIP archive/],
    [{ bytes: new TextEncoder().encode("not a zip") }, /not a ZIP archive/],
    [{ archive: new Uint8Array(0) }, /got keys: archive/]
  ];
  for (const [input, message] of cases) {
    await t.throwsAsync(BaboonCompiler.loadMany(input), { message: /^Loading failed: / }, String(input));
    await t.throwsAsync(BaboonCompiler.loadMany(input), { message }, JSON.stringify(input));
  }
  // an undefined-valued key counts as absent, as the declared type allows
  await t.throwsAsync(BaboonCompiler.loadMany({ bytes: new Uint8Array(0), base64: undefined }), { message: /not a ZIP archive/ });
});

test("load and loadMany reject invalid models with the issues and their paths", async t => {
  const broken = { "broken.baboon": "model example.npm\nversion \"1.0.0\"\nroot data User { name: nosuchtype }\n" };
  const error = await t.throwsAsync(BaboonCompiler.load(broken));
  t.regex(error.message, /^Loading failed: /);
  t.regex(error.message, /nosuchtype/);
});

const ENVELOPE = JSON.stringify({
  $mv: 1,
  $d: "example.npm",
  $v: "1.0.0",
  $t: TYPE_ID,
  $c: { name: "Ada", age: 42 }
});

test("Converts envelopes between JSON and UEBA, preserving identity", async t => {
  const model = await BaboonCompiler.load(FILES_MAP);
  for (const options of [
    { envelopeVersion: 1, indexed: false },
    { envelopeVersion: 1, indexed: true },
    { envelopeVersion: 2, indexed: false },
    { envelopeVersion: 2, indexed: true }
  ]) {
    const encoded = await BaboonCompiler.encodedEnvelopeLoaded(model, ENVELOPE, options);
    t.true(encoded.success, encoded.error);
    t.true(encoded.data instanceof Uint8Array);
    t.is(encoded.data[0], options.envelopeVersion);
    const decoded = await BaboonCompiler.decodeEnvelopeLoaded(model, encoded.data);
    t.true(decoded.success, decoded.error);
    t.is(decoded.json, ENVELOPE);
  }
});

test("Envelope conversion reports invalid arguments as failed results", async t => {
  const model = await BaboonCompiler.load(FILES_MAP);
  const invalidOptions = [
    undefined,
    {},
    { envelopeVersion: 3, indexed: false },
    { envelopeVersion: "1", indexed: false },
    { envelopeVersion: 1, indexed: "no" },
    { envelopeVersion: 1, indexed: false, extra: true }
  ];
  for (const options of invalidOptions) {
    const result = await BaboonCompiler.encodedEnvelopeLoaded(model, ENVELOPE, options);
    t.false(result.success, JSON.stringify(options));
    t.regex(result.error, /options must be \{ envelopeVersion: 1 \| 2, indexed: boolean \}/);
  }
  const notJson = await BaboonCompiler.encodedEnvelopeLoaded(model, "{", { envelopeVersion: 1, indexed: false });
  t.regex(notJson.error, /Invalid JSON/);
  const notModel = await BaboonCompiler.encodedEnvelopeLoaded({}, ENVELOPE, { envelopeVersion: 1, indexed: false });
  t.regex(notModel.error, /model is not a handle returned by load or loadMany/);
  const notBytes = await BaboonCompiler.decodeEnvelopeLoaded(model, [1, 2, 3]);
  t.regex(notBytes.error, /data must be a Uint8Array/);
  const unknownMeta = await BaboonCompiler.decodeEnvelopeLoaded(model, new Uint8Array([3]));
  t.regex(unknownMeta.error, /unsupported binary envelope metaVersion 3/);
});
