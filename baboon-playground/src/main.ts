import "./style.css";
import { registerBaboonLanguage } from "./baboon-language.ts";
import { BaboonEditor } from "./editor.ts";
import { Preview } from "./preview.ts";
import { compile, initCompiler } from "./compiler.ts";
import { OptionsPanel, DEFAULT_OPTIONS } from "./options.ts";
import type { CompilerOptions } from "./options.ts";
import { CodecToolsPanel } from "./codec-tools.ts";
import { zipSync, unzipSync, strToU8, strFromU8 } from "fflate";
import { encodeShare, decodeShare, readSharedParam, buildShareUrl, MAX_SHARED_URL_LENGTH } from "./share.ts";

import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import jsonWorker from "monaco-editor/esm/vs/language/json/json.worker?worker";
import cssWorker from "monaco-editor/esm/vs/language/css/css.worker?worker";
import htmlWorker from "monaco-editor/esm/vs/language/html/html.worker?worker";
import tsWorker from "monaco-editor/esm/vs/language/typescript/ts.worker?worker";

self.MonacoEnvironment = {
  getWorker(_: unknown, label: string) {
    if (label === "json") return new jsonWorker();
    if (label === "css" || label === "scss" || label === "less") return new cssWorker();
    if (label === "html" || label === "handlebars" || label === "razor") return new htmlWorker();
    if (label === "typescript" || label === "javascript") return new tsWorker();
    return new editorWorker();
  },
};

registerBaboonLanguage();

const app = document.getElementById("app")!;

const header = document.createElement("div");
header.className = "app-header";
app.appendChild(header);

const title = document.createElement("h1");
title.className = "app-title";
title.textContent = "Baboon Playground";
header.appendChild(title);

const headerStatus = document.createElement("span");
headerStatus.className = "header-status";
headerStatus.textContent = "Loading compiler\u2026";
header.appendChild(headerStatus);

const headerActions = document.createElement("div");
headerActions.className = "header-actions";
header.appendChild(headerActions);

const importBtn = document.createElement("button");
importBtn.className = "header-btn";
importBtn.textContent = "Import";
importBtn.title = "Import .baboon files from a ZIP archive";
headerActions.appendChild(importBtn);

const exportBtn = document.createElement("button");
exportBtn.className = "header-btn";
exportBtn.textContent = "Export";
exportBtn.title = "Export editor files as a ZIP archive";
headerActions.appendChild(exportBtn);

const shareBtn = document.createElement("button");
shareBtn.className = "header-btn";
shareBtn.textContent = "Share";
shareBtn.title = "Copy a shareable URL that embeds the current editor files";
headerActions.appendChild(shareBtn);

const optionsBtn = document.createElement("button");
optionsBtn.className = "header-btn";
optionsBtn.textContent = "Options";
headerActions.appendChild(optionsBtn);

const codecsBtn = document.createElement("button");
codecsBtn.className = "header-btn";
codecsBtn.textContent = "Codecs";
codecsBtn.title = "JSON/UEBA codec tools";
headerActions.appendChild(codecsBtn);

const compileBtn = document.createElement("button");
compileBtn.className = "compile-btn";
compileBtn.textContent = "Compile";
headerActions.appendChild(compileBtn);

const mainContent = document.createElement("div");
mainContent.className = "main-content";
app.appendChild(mainContent);

const baboonEditor = new BaboonEditor(mainContent);
const preview = new Preview(mainContent);

preview.setNavigateToErrorCallback((file, line, column) => {
  baboonEditor.focusLocation(file, line, column);
});

let toastTimer: number | undefined;
function showToast(message: string, isError = false): void {
  let toast = document.getElementById("toast");
  if (!toast) {
    toast = document.createElement("div");
    toast.id = "toast";
    document.body.appendChild(toast);
  }
  toast.textContent = message;
  toast.className = isError ? "toast toast-error toast-visible" : "toast toast-visible";
  if (toastTimer !== undefined) window.clearTimeout(toastTimer);
  toastTimer = window.setTimeout(() => {
    toast!.classList.remove("toast-visible");
  }, isError ? 6000 : 3000);
}

let currentOptions: CompilerOptions = structuredClone(DEFAULT_OPTIONS);

const optionsPanel = new OptionsPanel((options) => {
  currentOptions = options;
});

// Restore editor state (files + compiler options) from a `#shared=<base64>`
// link, if present. Runs after the options panel exists so options apply.
const sharedParam = readSharedParam();
if (sharedParam) {
  try {
    const shared = decodeShare(sharedParam);
    if (shared.files.size > 0) baboonEditor.setFiles(shared.files);
    if (shared.options) optionsPanel.setOptions(shared.options);
  } catch (e) {
    showToast(`Could not load shared content: ${e instanceof Error ? e.message : String(e)}`, true);
  }
}

optionsBtn.addEventListener("click", () => {
  optionsPanel.open();
});

const codecToolsPanel = new CodecToolsPanel();

codecsBtn.addEventListener("click", () => {
  const files = baboonEditor.getFiles();
  codecToolsPanel.open(files);
});

exportBtn.addEventListener("click", () => {
  const files = baboonEditor.getFiles();
  const zipData: Record<string, Uint8Array> = {};
  for (const [path, content] of files) {
    zipData[path] = strToU8(content);
  }
  const zipped = zipSync(zipData);
  const blob = new Blob([zipped.buffer as ArrayBuffer], { type: "application/zip" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = "baboon-playground.zip";
  a.click();
  URL.revokeObjectURL(url);
});

shareBtn.addEventListener("click", async () => {
  let shareUrl: string;
  try {
    shareUrl = buildShareUrl(encodeShare(baboonEditor.getFiles(), optionsPanel.getOptions()));
  } catch (e) {
    showToast(`Could not encode editor state: ${e instanceof Error ? e.message : String(e)}`, true);
    return;
  }
  if (shareUrl.length > MAX_SHARED_URL_LENGTH) {
    showToast(
      `Editor content is too large to share as a link (${shareUrl.length} of ${MAX_SHARED_URL_LENGTH} chars). Use Export instead.`,
      true,
    );
    return;
  }
  try {
    await navigator.clipboard.writeText(shareUrl);
    window.history.replaceState(null, "", shareUrl);
    showToast("Share link copied to clipboard");
  } catch {
    window.history.replaceState(null, "", shareUrl);
    window.prompt("Copy this share link:", shareUrl);
  }
});

const fileInput = document.createElement("input");
fileInput.type = "file";
fileInput.accept = ".zip";
fileInput.style.display = "none";
document.body.appendChild(fileInput);

importBtn.addEventListener("click", () => {
  fileInput.click();
});

fileInput.addEventListener("change", () => {
  const file = fileInput.files?.[0];
  if (!file) return;
  fileInput.value = "";

  const reader = new FileReader();
  reader.onload = () => {
    const buffer = reader.result as ArrayBuffer;
    const unzipped = unzipSync(new Uint8Array(buffer));
    const files = new Map<string, string>();
    for (const [path, data] of Object.entries(unzipped)) {
      if (path.startsWith("__MACOSX/")) continue;
      if (path.endsWith("/")) continue;
      const basename = path.split("/").pop()!;
      if (basename.startsWith(".")) continue;
      if (!path.endsWith(".baboon") && !path.endsWith(".bmo")) continue;
      files.set(path, strFromU8(data));
    }
    if (files.size === 0) return;
    baboonEditor.setFiles(files);
  };
  reader.readAsArrayBuffer(file);
});

let compiling = false;

compileBtn.addEventListener("click", async () => {
  if (compiling) return;
  compiling = true;
  compileBtn.disabled = true;
  compileBtn.textContent = "Compiling...";

  try {
    const files = baboonEditor.getFiles();
    const result = await compile(files, currentOptions);
    preview.setResult(result);
  } catch (e) {
    preview.setResult({
      success: false,
      filesByLanguage: new Map(),
      errors: [{
        message: e instanceof Error ? e.message : String(e),
        file: null,
        line: null,
        column: null,
      }],
    });
  } finally {
    compiling = false;
    compileBtn.disabled = false;
    compileBtn.textContent = "Compile";
  }
});

window.addEventListener("resize", () => {
  baboonEditor.layout();
  preview.layout();
});

compileBtn.disabled = true;
codecsBtn.disabled = true;

initCompiler().then(() => {
  headerStatus.textContent = "";
  headerStatus.classList.add("header-status-hidden");
  compileBtn.disabled = false;
  codecsBtn.disabled = false;
}).catch((e) => {
  headerStatus.textContent = `Compiler load failed: ${e instanceof Error ? e.message : String(e)}`;
  headerStatus.classList.add("header-status-error");
});
