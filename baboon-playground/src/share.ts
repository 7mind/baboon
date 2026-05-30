import { deflateSync, inflateSync, strToU8, strFromU8 } from "fflate";
import { type CompilerOptions, mergeWithDefaults } from "./options.ts";

// Editor state (files + compiler options) is serialized as JSON, deflated, and
// base64url-encoded into the URL *fragment* (`#shared=…`) so a playground URL
// is self-contained. The fragment is deliberately used instead of a query
// parameter: it never reaches the server, so GitHub Pages' Fastly CDN never
// applies its ~8 KB request-line limit (no HTTP 414). The only remaining hard
// bound is the browser's maximum URL length — Chrome's ~2 MiB navigation cap
// is the tightest among modern browsers — so MAX_SHARED_URL_LENGTH caps the
// whole share URL there; beyond it the UI asks the user to Export instead.
export const SHARED_PARAM = "shared";
export const MAX_SHARED_URL_LENGTH = 2 * 1024 * 1024;

// Versioned payload wrapper. Legacy links (the first Share release) were a flat
// `{ path: content }` map with no version marker; decodeShare falls back to
// treating any unversioned object as that map.
interface SharePayloadV1 {
  v: 1;
  files: Record<string, string>;
  options?: CompilerOptions;
}

export interface SharedState {
  files: Map<string, string>;
  options: CompilerOptions | null;
}

function bytesToBase64Url(bytes: Uint8Array): string {
  let bin = "";
  const chunk = 0x8000;
  for (let i = 0; i < bytes.length; i += chunk) {
    bin += String.fromCharCode(...bytes.subarray(i, i + chunk));
  }
  return btoa(bin).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

function base64UrlToBytes(value: string): Uint8Array {
  const padded = value.replace(/-/g, "+").replace(/_/g, "/");
  const bin = atob(padded);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  return bytes;
}

/** Compress + base64url-encode the editor files and compiler options. */
export function encodeShare(files: Map<string, string>, options: CompilerOptions): string {
  const fileObj: Record<string, string> = {};
  for (const [path, content] of files) fileObj[path] = content;
  const payload: SharePayloadV1 = { v: 1, files: fileObj, options };
  const compressed = deflateSync(strToU8(JSON.stringify(payload)), { level: 9 });
  return bytesToBase64Url(compressed);
}

/** Inverse of `encodeShare`. Throws if `encoded` is not a valid payload.
  * Accepts both the v1 wrapper and the legacy flat `{ path: content }` map. */
export function decodeShare(encoded: string): SharedState {
  const json = strFromU8(inflateSync(base64UrlToBytes(encoded)));
  const obj = JSON.parse(json) as unknown;
  if (obj === null || typeof obj !== "object" || Array.isArray(obj)) {
    throw new Error("shared payload is not an object");
  }
  const rec = obj as Record<string, unknown>;
  const isV1 = rec.v === 1 && typeof rec.files === "object" && rec.files !== null;
  const filesObj = (isV1 ? rec.files : rec) as Record<string, unknown>;

  const files = new Map<string, string>();
  for (const [path, content] of Object.entries(filesObj)) {
    if (typeof content === "string") files.set(path, content);
  }

  const options = isV1 && rec.options && typeof rec.options === "object"
    ? mergeWithDefaults(rec.options as Partial<CompilerOptions>)
    : null;

  return { files, options };
}

export function readSharedParam(): string | null {
  // Primary location is the fragment; fall back to the query string so older
  // `?shared=` links (if any) keep working.
  const fromHash = new URLSearchParams(window.location.hash.replace(/^#/, "")).get(SHARED_PARAM);
  if (fromHash) return fromHash;
  return new URLSearchParams(window.location.search).get(SHARED_PARAM);
}

export function buildShareUrl(encoded: string): string {
  return `${window.location.origin}${window.location.pathname}#${SHARED_PARAM}=${encoded}`;
}
