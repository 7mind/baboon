import { deflateSync, inflateSync, strToU8, strFromU8 } from "fflate";

// Editor state is serialized as a JSON `{ path: content }` map, deflated, and
// base64url-encoded into the URL *fragment* (`#shared=…`) so a playground URL
// is self-contained. The fragment is deliberately used instead of a query
// parameter: it never reaches the server, so GitHub Pages' Fastly CDN never
// applies its ~8 KB request-line limit (no HTTP 414). The only remaining hard
// bound is the browser's maximum URL length — Chrome's ~2 MiB navigation cap
// is the tightest among modern browsers — so MAX_SHARED_URL_LENGTH caps the
// whole share URL there; beyond it the UI asks the user to Export instead.
export const SHARED_PARAM = "shared";
export const MAX_SHARED_URL_LENGTH = 2 * 1024 * 1024;

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

/** Compress + base64url-encode the editor files for a `?shared=` link. */
export function encodeFiles(files: Map<string, string>): string {
  const obj: Record<string, string> = {};
  for (const [path, content] of files) obj[path] = content;
  const compressed = deflateSync(strToU8(JSON.stringify(obj)), { level: 9 });
  return bytesToBase64Url(compressed);
}

/** Inverse of `encodeFiles`. Throws if `encoded` is not a valid payload. */
export function decodeFiles(encoded: string): Map<string, string> {
  const json = strFromU8(inflateSync(base64UrlToBytes(encoded)));
  const obj = JSON.parse(json) as Record<string, unknown>;
  if (obj === null || typeof obj !== "object" || Array.isArray(obj)) {
    throw new Error("shared payload is not a file map");
  }
  const files = new Map<string, string>();
  for (const [path, content] of Object.entries(obj)) {
    if (typeof content === "string") files.set(path, content);
  }
  return files;
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
