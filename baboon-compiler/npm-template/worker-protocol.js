// Messages between worker-client.js and worker-server.js; internal, not a public API.
//
// request:  { id: number, method: string, args: Array<{ model: number } | { value: unknown }> }
// response: { id: number, ok: true, value: unknown } | { id: number, ok: false, error: string }
//
// Models never cross the boundary: the worker keeps them and answers `load`/`loadMany` with a
// numeric handle, which `{ model: handle }` arguments refer to until `release`.

/** The BaboonCompiler methods a worker serves, in index.d.ts order. */
export const BABOON_METHODS = Object.freeze([
  "compile",
  "load",
  "loadMany",
  "listTypes",
  "generateRandom",
  "encode",
  "encodeLoaded",
  "decode",
  "decodeLoaded",
  "encodedEnvelopeLoaded",
  "decodeEnvelopeLoaded",
  "cleanupScheme",
  "cleanupSchemeLoaded",
]);

/** Methods whose result is a model, answered with its handle. */
export const MODEL_METHODS = Object.freeze(["load", "loadMany"]);

/** Drops a model held by the worker. */
export const RELEASE_METHOD = "release";
