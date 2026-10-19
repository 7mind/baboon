// connectBaboonWorker <-> serveBaboon through node:worker_threads (mdl action `test-js-npm`; not
// published). Needs the CLI archive (BABOON_SCHEMA_ZIP). browser-worker-acceptance.mjs covers the
// browser-style entry, worker.js, under Deno.
import test from "ava";
import { readFileSync } from "fs";
import { MessageChannel, Worker } from "node:worker_threads";

import { BaboonCompiler } from "./index.js";
import { connectBaboonWorker } from "./worker-client.js";

const archivePath = process.env.BABOON_SCHEMA_ZIP;
if (!archivePath) {
  throw new Error("BABOON_SCHEMA_ZIP must name an archive (see .mdl/defs/tests.md, action test-js-npm)");
}
const archive = new Uint8Array(readFileSync(archivePath));

const STABLE = '{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdStable","$uv":"1.0.0","$c":{"s":"s"}}';
const V2C = { envelopeVersion: 2, indexed: false };

function connect(t) {
  const { port1, port2 } = new MessageChannel();
  const worker = new Worker(new URL("./worker-node-host.mjs", import.meta.url), { workerData: { port: port2 }, transferList: [port2] });
  t.teardown(async () => {
    port1.close();
    await worker.terminate();
  });
  return connectBaboonWorker(port1);
}

test("the client does not load the compiler bundle", t => {
  const source = readFileSync(new URL("./worker-client.js", import.meta.url), "utf8");
  t.deepEqual([...source.matchAll(/^import .* from "(.*)";$/gm)].map(m => m[1]), ["./worker-protocol.js"]);
});

test("the compiler needs no host-provided sha256", async t => {
  t.is(globalThis.sha256, undefined);
  const model = await BaboonCompiler.loadMany({ bytes: archive });
  t.true(BaboonCompiler.listTypes(model).length > 0);
});

test("a worker answers like the compiler it runs", async t => {
  const baboon = connect(t);
  const remote = await baboon.loadMany({ bytes: archive });
  const local = await BaboonCompiler.loadMany({ bytes: archive });
  t.deepEqual(await baboon.listTypes(remote), JSON.parse(JSON.stringify(BaboonCompiler.listTypes(local))));

  const encoded = await baboon.encodedEnvelopeLoaded(remote, STABLE, V2C);
  t.true(encoded.success, encoded.error);
  t.deepEqual(encoded.data, (await BaboonCompiler.encodedEnvelopeLoaded(local, STABLE, V2C)).data);
  t.deepEqual(await baboon.decodeEnvelopeLoaded(remote, encoded.data), { success: true, json: STABLE });

  const failed = await baboon.decodeEnvelopeLoaded(remote, new Uint8Array(0));
  t.false(failed.success);
  t.regex(failed.error, /empty input/);
});

test("worker-side rejections keep their message", async t => {
  const baboon = connect(t);
  await t.throwsAsync(baboon.loadMany({ bytes: archive.slice(0, archive.length - 1) }), { message: /^Loading failed: not a ZIP archive/ });
  await t.throwsAsync(baboon.load({ "bad.baboon": "model x" }), { message: /^Loading failed: / });
});

test("a released model is gone, and a model stays with its connection", async t => {
  const first = connect(t);
  const second = connect(t);
  const model = await first.loadMany({ bytes: archive });
  await t.throwsAsync(second.listTypes(model), { message: /another Baboon worker connection/ });
  await first.release(model);
  await t.throwsAsync(first.listTypes(model), { message: /is not loaded in this worker/ });
  await t.throwsAsync(first.release(model), { message: /is not loaded in this worker/ });
});

test("the client offers exactly the compiler's methods, plus release", t => {
  const { port1 } = new MessageChannel();
  t.teardown(() => port1.close());
  const exported = Object.getOwnPropertyNames(Object.getPrototypeOf(BaboonCompiler))
    .filter(name => name !== "constructor" && /^[a-z][A-Za-z0-9]*$/.test(name) && typeof BaboonCompiler[name] === "function");
  t.deepEqual(Object.keys(connectBaboonWorker(port1)).sort(), [...exported, "release"].sort());
});
