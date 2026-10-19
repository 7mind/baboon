import type {
  BaboonArchiveInput,
  BaboonCompilationResult,
  BaboonCompilerOptions,
  BaboonDecodeResult,
  BaboonEncodeResult,
  BaboonEnvelopeEncodeOptions,
  BaboonGenerateResult,
  BaboonSchemeResult,
  BaboonTypeInfo,
} from "./index.js";

/** What a worker connection talks through: a `Worker`, a `MessagePort`, or a worker's global scope. */
export interface BaboonWorkerPort {
  postMessage(message: unknown): void;
  addEventListener(type: "message", listener: (event: MessageEvent) => void): void;
  addEventListener(type: "error" | "messageerror", listener: (event: Event & { message?: string }) => void): void;
  start?(): void;
}

declare const workerModel: unique symbol;

/** A model held by the worker; usable only through the connection that loaded it, until released. */
export interface BaboonWorkerModel {
  readonly [workerModel]: true;
}

/** BaboonCompilerAPI run in a worker: the same methods, each answering with a Promise. */
export interface BaboonWorkerAPI {
  compile(options: BaboonCompilerOptions): Promise<BaboonCompilationResult>;

  load(files: Record<string, string>): Promise<BaboonWorkerModel>;

  loadMany(archive: BaboonArchiveInput): Promise<BaboonWorkerModel>;

  listTypes(model: BaboonWorkerModel): Promise<BaboonTypeInfo[]>;

  generateRandom(model: BaboonWorkerModel, pkg: string, version: string, idString: string): Promise<BaboonGenerateResult>;

  encode(files: Record<string, string>, pkg: string, version: string, idString: string, json: string, indexed: boolean): Promise<BaboonEncodeResult>;

  encodeLoaded(model: BaboonWorkerModel, pkg: string, version: string, idString: string, json: string, indexed: boolean): Promise<BaboonEncodeResult>;

  decode(files: Record<string, string>, pkg: string, version: string, idString: string, data: Uint8Array): Promise<BaboonDecodeResult>;

  decodeLoaded(model: BaboonWorkerModel, pkg: string, version: string, idString: string, data: Uint8Array): Promise<BaboonDecodeResult>;

  encodedEnvelopeLoaded(model: BaboonWorkerModel, json: string, options: BaboonEnvelopeEncodeOptions): Promise<BaboonEncodeResult>;

  decodeEnvelopeLoaded(model: BaboonWorkerModel, data: Uint8Array): Promise<BaboonDecodeResult>;

  cleanupScheme(files: Record<string, string>, domain: string, version: string): Promise<BaboonSchemeResult>;

  cleanupSchemeLoaded(model: BaboonWorkerModel, domain: string, version: string): Promise<BaboonSchemeResult>;

  /** Frees the model in the worker; later calls with it reject. */
  release(model: BaboonWorkerModel): Promise<void>;
}

/**
 * Talks to a worker running `@7mind.io/baboon/worker.js` (or `serveBaboon`). Rejections carry the
 * worker-side error message; if the worker fails to load or crashes, pending and later calls reject.
 */
export function connectBaboonWorker(port: BaboonWorkerPort): BaboonWorkerAPI;
