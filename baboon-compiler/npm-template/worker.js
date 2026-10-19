// Dedicated module-worker entry (a side effect: it serves on the worker scope it is evaluated in):
// new Worker(new URL("./worker.js", import.meta.url), { type: "module" }), or `import` it from your own entry
import { serveBaboon } from "./worker-server.js";

serveBaboon(globalThis);
