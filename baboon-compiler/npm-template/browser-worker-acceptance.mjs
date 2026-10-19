// worker.js, the browser entry, in a real module Worker (Deno implements the Web Worker API); run by
// the mdl action `test-js-npm` with `deno test`, not published. Needs the CLI archive (BABOON_SCHEMA_ZIP).
import { connectBaboonWorker } from "./worker-client.js";

const archive = await Deno.readFile(Deno.env.get("BABOON_SCHEMA_ZIP"));
const STABLE = '{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdStable","$uv":"1.0.0","$c":{"s":"s"}}';

function check(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

Deno.test("worker.js serves the compiler to a module Worker", async () => {
  const worker = new Worker(new URL("./worker.js", import.meta.url).href, { type: "module" });
  try {
    const baboon = connectBaboonWorker(worker);
    const model = await baboon.loadMany({ bytes: archive });
    const types = await baboon.listTypes(model);
    check(types.some(ty => ty.id === "fwde2e.fwd/:#FwdStable"), `unexpected types: ${JSON.stringify(types)}`);
    const encoded = await baboon.encodedEnvelopeLoaded(model, STABLE, { envelopeVersion: 2, indexed: false });
    check(encoded.success && encoded.data instanceof Uint8Array, `encoding failed: ${encoded.error}`);
    const decoded = await baboon.decodeEnvelopeLoaded(model, encoded.data);
    check(decoded.success && decoded.json === STABLE, `decoding failed: ${JSON.stringify(decoded)}`);
  } finally {
    worker.terminate();
  }
});

Deno.test("calls reject when the worker script cannot load", async () => {
  const worker = new Worker(new URL("./no-such-worker.js", import.meta.url).href, { type: "module" });
  // Deno ends the process on an unhandled worker error; browsers only log it
  worker.addEventListener("error", event => event.preventDefault());
  try {
    const baboon = connectBaboonWorker(worker);
    const outcome = await baboon.load({}).then(() => "resolved", error => error.message);
    check(/the Baboon worker failed/.test(outcome), `unexpected outcome: ${outcome}`);
  } finally {
    worker.terminate();
  }
});
