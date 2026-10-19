// Worker side: answers worker-client.js requests with BaboonCompiler, which only this side loads.
import { BaboonCompiler } from "./main.js";
import { BABOON_METHODS, MODEL_METHODS, RELEASE_METHOD } from "./worker-protocol.js";

/**
 * Serves BaboonCompiler over `port`: a dedicated worker's global scope (see worker.js), a
 * MessagePort, or anything with `postMessage` and `addEventListener("message")`.
 */
export function serveBaboon(port) {
  const models = new Map();
  let nextHandle = 0;

  const model = handle => {
    const found = models.get(handle);
    if (found === undefined) {
      throw new Error(`model ${handle} is not loaded in this worker (released, or from another worker)`);
    }
    return found;
  };

  const invoke = async (method, args) => {
    if (method === RELEASE_METHOD) {
      model(args[0].model);
      models.delete(args[0].model);
      return undefined;
    }
    if (!BABOON_METHODS.includes(method)) {
      throw new Error(`unknown method '${method}'`);
    }
    const value = await BaboonCompiler[method](...args.map(arg => ("model" in arg ? model(arg.model) : arg.value)));
    if (MODEL_METHODS.includes(method)) {
      const handle = nextHandle++;
      models.set(handle, value);
      return handle;
    }
    return value;
  };

  port.addEventListener("message", async event => {
    const { id, method, args } = event.data;
    let response;
    try {
      response = { id, ok: true, value: await invoke(method, args) };
    } catch (error) {
      response = { id, ok: false, error: messageOf(error) };
    }
    try {
      port.postMessage(response);
    } catch (error) {
      port.postMessage({ id, ok: false, error: `the result of ${method} cannot be sent from the worker: ${messageOf(error)}` });
    }
  });
  if (typeof port.start === "function") {
    port.start();
  }
}

function messageOf(error) {
  return error instanceof Error ? error.message : String(error);
}
