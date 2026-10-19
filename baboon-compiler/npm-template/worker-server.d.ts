import type { BaboonWorkerPort } from "./worker-client.js";

/** Answers `connectBaboonWorker` requests arriving on `port` with this thread's BaboonCompiler. */
export function serveBaboon(port: BaboonWorkerPort): void;
