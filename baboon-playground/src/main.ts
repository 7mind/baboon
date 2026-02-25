import "./style.css";
import { registerBaboonLanguage } from "./baboon-language.ts";
import { BaboonEditor } from "./editor.ts";
import { Preview } from "./preview.ts";
import { compile } from "./compiler.ts";
import { OptionsPanel, DEFAULT_OPTIONS } from "./options.ts";
import type { CompilerOptions } from "./options.ts";

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

const headerActions = document.createElement("div");
headerActions.className = "header-actions";
header.appendChild(headerActions);

const optionsBtn = document.createElement("button");
optionsBtn.className = "options-btn";
optionsBtn.textContent = "Options";
headerActions.appendChild(optionsBtn);

const compileBtn = document.createElement("button");
compileBtn.className = "compile-btn";
compileBtn.textContent = "Compile";
headerActions.appendChild(compileBtn);

const mainContent = document.createElement("div");
mainContent.className = "main-content";
app.appendChild(mainContent);

const baboonEditor = new BaboonEditor(mainContent);
const preview = new Preview(mainContent);

let currentOptions: CompilerOptions = structuredClone(DEFAULT_OPTIONS);

const optionsPanel = new OptionsPanel((options) => {
  currentOptions = options;
});

optionsBtn.addEventListener("click", () => {
  optionsPanel.open();
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
      errors: [e instanceof Error ? e.message : String(e)],
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
