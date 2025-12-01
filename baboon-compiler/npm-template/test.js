import test from "ava";

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
