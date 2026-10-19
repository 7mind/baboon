// Main-thread side of a Baboon worker. It does not import the compiler bundle (main.js), so the page
// never downloads or parses it; only the worker does (see worker.js and worker-server.js).
import { BABOON_METHODS, MODEL_METHODS, RELEASE_METHOD } from "./worker-protocol.js";

const WORKER_FAILED = "the Baboon worker failed: its script did not load, or it crashed";

/** A model held by a worker; only the client that loaded it can use it. */
class BaboonWorkerModel {}

const handles = new WeakMap();

/**
 * Connects to a worker that serves Baboon (worker.js, or `serveBaboon`), through a `Worker`, a
 * `MessagePort`, or anything with `postMessage` and `addEventListener("message")`. Returns the
 * BaboonCompiler methods, each answering with a Promise, plus `release(model)`.
 */
export function connectBaboonWorker(port) {
  const pending = new Map();
  let nextId = 0;
  let failure = null;

  const failAll = message => {
    failure = new Error(message);
    for (const call of pending.values()) {
      call.reject(failure);
    }
    pending.clear();
  };

  port.addEventListener("message", event => {
    const { id, ok, value, error } = event.data;
    const call = pending.get(id);
    if (call === undefined) {
      throw new Error(`the Baboon worker answered request ${id}, which this client did not send or already settled`);
    }
    pending.delete(id);
    if (ok) {
      call.resolve(value);
    } else {
      call.reject(new Error(error));
    }
  });
  // `error` comes from a Worker whose script failed to load or threw outside a request
  port.addEventListener("error", event => failAll(event.message ? `${WORKER_FAILED}: ${event.message}` : WORKER_FAILED));
  port.addEventListener("messageerror", () => failAll("a message from the Baboon worker could not be deserialized"));
  if (typeof port.start === "function") {
    port.start();
  }

  const client = {};

  const argument = arg => {
    if (!(arg instanceof BaboonWorkerModel)) {
      return { value: arg };
    }
    const ref = handles.get(arg);
    if (ref.client !== client) {
      throw new Error("this model was loaded through another Baboon worker connection");
    }
    return { model: ref.handle };
  };

  const request = (method, args) => {
    if (failure !== null) {
      return Promise.reject(failure);
    }
    return new Promise((resolve, reject) => {
      const id = nextId++;
      try {
        const message = { id, method, args: args.map(argument) };
        pending.set(id, { resolve, reject });
        port.postMessage(message);
      } catch (error) {
        pending.delete(id);
        reject(error);
      }
    });
  };

  for (const method of BABOON_METHODS) {
    client[method] = MODEL_METHODS.includes(method)
      ? (...args) =>
          request(method, args).then(handle => {
            const model = new BaboonWorkerModel();
            handles.set(model, { client, handle });
            return model;
          })
      : (...args) => request(method, args);
  }
  client[RELEASE_METHOD] = model => request(RELEASE_METHOD, [model]).then(() => undefined);

  return Object.freeze(client);
}
