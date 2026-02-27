import { sha256 as jsSha256 } from "js-sha256";
import type { CompilerOptions } from "./options.ts";

interface JSInputFile {
  path: string;
  content: string;
}

interface JSOutputFile {
  path: string;
  content: string;
  product: string;
}

interface JSCompilationError {
  message: string;
  file?: string;
  line?: number;
  column?: number;
}

interface JSCompilationResult {
  success: boolean;
  files?: JSOutputFile[];
  errors?: JSCompilationError[];
}

interface JSLangOptions {
  wrappedAdtBranchCodecs?: boolean;
  generateJsonCodecs?: boolean;
  generateUebaCodecs?: boolean;
  generateJsonCodecsByDefault?: boolean;
  generateUebaCodecsByDefault?: boolean;
  writeEvolutionDict?: boolean;
}

interface JSGenericOptions {
  disableConversions?: boolean;
}

interface JSCompilerTarget {
  language: string;
  generic?: JSGenericOptions;
  [langKey: string]: unknown;
}

interface JSCompilerAPIOptions {
  inputs: JSInputFile[];
  targets: JSCompilerTarget[];
  debug?: boolean;
}

interface JSTypeInfo {
  pkg: string;
  version: string;
  id: string;
  name: string;
  kind: string;
}

interface JSGenerateResult {
  success: boolean;
  json?: string;
  error?: string;
}

interface JSEncodeResult {
  success: boolean;
  data?: Uint8Array;
  error?: string;
}

interface JSDecodeResult {
  success: boolean;
  json?: string;
  error?: string;
}

interface BaboonLoadedModel {
  __brand: "BaboonLoadedModel";
}

interface BaboonCompilerAPI {
  compile(options: JSCompilerAPIOptions): Promise<JSCompilationResult>;
  load(files: Record<string, string>): Promise<BaboonLoadedModel>;
  listTypes(model: BaboonLoadedModel): JSTypeInfo[];
  generateRandom(model: BaboonLoadedModel, pkg: string, version: string, typeId: string): JSGenerateResult;
  encodeLoaded(
    model: BaboonLoadedModel,
    pkg: string,
    version: string,
    typeId: string,
    json: string,
    indexed: boolean,
  ): Promise<JSEncodeResult>;
  decodeLoaded(
    model: BaboonLoadedModel,
    pkg: string,
    version: string,
    typeId: string,
    data: Uint8Array,
  ): Promise<JSDecodeResult>;
}

export interface OutputFile {
  path: string;
  content: string;
  product: string;
}

export interface CompilationError {
  message: string;
  file: string | null;
  line: number | null;
  column: number | null;
}

export interface CompilationResult {
  success: boolean;
  filesByLanguage: Map<string, OutputFile[]>;
  errors: CompilationError[];
}

const ALL_LANGUAGES = [
  "cs",
  "scala",
  "python",
  "rust",
  "typescript",
  "kotlin",
  "java",
  "dart",
  "swift",
] as const;

export type BaboonTargetLanguage = (typeof ALL_LANGUAGES)[number];

export const TARGET_LANGUAGES: readonly BaboonTargetLanguage[] = ALL_LANGUAGES;

export const LANGUAGE_DISPLAY_NAMES: Record<BaboonTargetLanguage, string> = {
  cs: "C#",
  scala: "Scala",
  python: "Python",
  rust: "Rust",
  typescript: "TypeScript",
  kotlin: "Kotlin",
  java: "Java",
  dart: "Dart",
  swift: "Swift",
};

export const LANGUAGE_TO_MONACO: Record<BaboonTargetLanguage, string> = {
  cs: "csharp",
  scala: "scala",
  python: "python",
  rust: "rust",
  typescript: "typescript",
  kotlin: "kotlin",
  java: "java",
  dart: "dart",
  swift: "swift",
};

const EXTENSION_TO_LANGUAGE: Record<string, BaboonTargetLanguage> = {
  ".cs": "cs",
  ".scala": "scala",
  ".py": "python",
  ".rs": "rust",
  ".ts": "typescript",
  ".kt": "kotlin",
  ".java": "java",
  ".dart": "dart",
  ".swift": "swift",
};

function detectLanguageByExtension(path: string): BaboonTargetLanguage | null {
  for (const [ext, lang] of Object.entries(EXTENSION_TO_LANGUAGE)) {
    if (path.endsWith(ext)) {
      return lang;
    }
  }
  return null;
}

class BrowserSha256 {
  private hash: ReturnType<typeof jsSha256.create>;

  constructor() {
    this.hash = jsSha256.create();
  }

  update(data: number[]): this {
    this.hash.update(data);
    return this;
  }

  digest(encoding: string): string {
    if (encoding === "hex") {
      return this.hash.hex();
    }
    throw new Error(`Unsupported digest encoding: ${encoding}`);
  }
}

function installSha256Polyfill(): void {
  if (typeof (globalThis as Record<string, unknown>).sha256 === "undefined") {
    (globalThis as Record<string, unknown>).sha256 = BrowserSha256;
  }
}

let compilerInstance: BaboonCompilerAPI | null = null;

async function getCompiler(): Promise<BaboonCompilerAPI> {
  if (compilerInstance) {
    return compilerInstance;
  }

  installSha256Polyfill();

  const compilerUrl = new URL("./compiler/main.js", document.baseURI).href;
  const module = await import(/* @vite-ignore */ compilerUrl) as {
    BaboonCompiler: BaboonCompilerAPI;
  };
  compilerInstance = module.BaboonCompiler;
  return compilerInstance;
}

// The Scala.js compiler API passes all inputs both to the top-level parser
// AND to the include resolver. On JVM, only .baboon files are parsed as
// top-level; .bmo fragments are resolved on-demand from the filesystem.
// To replicate this in the browser, we resolve includes client-side: inline
// .bmo content into .baboon files, then pass only .baboon files to compile().

const INCLUDE_LINE_REGEX = /^(\s*)include\s+"([^"]+)"\s*$/;

function collectDirectoryInputs(paths: Iterable<string>): Set<string> {
  const dirs = new Set<string>();
  for (const p of paths) {
    const lastSlash = p.lastIndexOf("/");
    if (lastSlash > 0) {
      dirs.add(p.substring(0, lastSlash));
    }
  }
  return dirs;
}

function resolveIncludePath(
  directoryInputs: Set<string>,
  includePath: string,
  allFiles: Map<string, string>,
): string | null {
  for (const dir of directoryInputs) {
    const resolved = dir + "/" + includePath;
    if (allFiles.has(resolved)) {
      return resolved;
    }
  }
  if (allFiles.has(includePath)) {
    return includePath;
  }
  return null;
}

interface SourceLocation {
  file: string;
  line: number;
}

interface ResolveResult {
  content: string;
  sourceMap: SourceLocation[];
}

function resolveIncludesWithMap(
  content: string,
  sourceFile: string,
  allFiles: Map<string, string>,
  directoryInputs: Set<string>,
  visited: Set<string>,
): ResolveResult {
  const inputLines = content.split("\n");
  const outputLines: string[] = [];
  const sourceMap: SourceLocation[] = [];

  for (let i = 0; i < inputLines.length; i++) {
    const line = inputLines[i];
    const match = line.match(INCLUDE_LINE_REGEX);

    if (!match) {
      outputLines.push(line);
      sourceMap.push({ file: sourceFile, line: i + 1 });
      continue;
    }

    const [, indent, includePath] = match;
    const resolved = resolveIncludePath(directoryInputs, includePath, allFiles);

    if (resolved === null) {
      outputLines.push(`${indent}// [playground] include not found: ${includePath}`);
      sourceMap.push({ file: sourceFile, line: i + 1 });
    } else if (visited.has(resolved)) {
      outputLines.push(`${indent}// [playground] circular include skipped: ${includePath}`);
      sourceMap.push({ file: sourceFile, line: i + 1 });
    } else {
      visited.add(resolved);
      const included = allFiles.get(resolved)!;
      const nested = resolveIncludesWithMap(included, resolved, allFiles, directoryInputs, visited);
      const nestedLines = nested.content.split("\n");
      for (let j = 0; j < nestedLines.length; j++) {
        outputLines.push(nestedLines[j]);
        sourceMap.push(nested.sourceMap[j] ?? { file: resolved, line: j + 1 });
      }
    }
  }

  return { content: outputLines.join("\n"), sourceMap };
}

interface PreparedInputs {
  inputs: JSInputFile[];
  sourceMaps: Map<string, SourceLocation[]>;
}

function prepareInputs(files: Map<string, string>): PreparedInputs {
  const directoryInputs = collectDirectoryInputs(files.keys());
  const inputs: JSInputFile[] = [];
  const sourceMaps = new Map<string, SourceLocation[]>();
  for (const [path, content] of files) {
    if (!path.endsWith(".baboon")) {
      continue;
    }
    const result = resolveIncludesWithMap(content, path, files, directoryInputs, new Set([path]));
    inputs.push({ path, content: result.content });
    sourceMaps.set(path, result.sourceMap);
  }
  return { inputs, sourceMaps };
}

function remapErrorLocation(
  error: CompilationError,
  sourceMaps: Map<string, SourceLocation[]>,
): CompilationError {
  if (error.file === null || error.line === null) return error;
  const map = sourceMaps.get(error.file);
  if (!map) return error;
  const idx = error.line - 1;
  if (idx < 0 || idx >= map.length) return error;
  const loc = map[idx];
  return { ...error, file: loc.file, line: loc.line };
}

function buildTargets(options: CompilerOptions): JSCompilerTarget[] {
  const generic: JSGenericOptions = {
    disableConversions: options.generic.disableConversions,
  };
  return ALL_LANGUAGES.map((language) => {
    const langOpts: JSLangOptions = options.languages[language];
    const target: JSCompilerTarget = { language, generic };
    target[language] = langOpts;
    return target;
  });
}

export async function compile(
  files: Map<string, string>,
  options: CompilerOptions,
): Promise<CompilationResult> {
  const compiler = await getCompiler();

  const { inputs, sourceMaps } = prepareInputs(files);
  const targets = buildTargets(options);

  const result = await compiler.compile({
    inputs,
    targets,
  });

  if (!result.success) {
    const errors: CompilationError[] = result.errors
      ? result.errors.map((e) =>
          remapErrorLocation(
            {
              message: e.message,
              file: e.file ?? null,
              line: e.line ?? null,
              column: e.column ?? null,
            },
            sourceMaps,
          ),
        )
      : [{ message: "Unknown compilation error", file: null, line: null, column: null }];
    return {
      success: false,
      filesByLanguage: new Map(),
      errors,
    };
  }

  const filesByLanguage = new Map<string, OutputFile[]>();
  for (const lang of ALL_LANGUAGES) {
    filesByLanguage.set(lang, []);
  }

  for (const file of result.files ?? []) {
    const lang = detectLanguageByExtension(file.path);
    if (lang) {
      filesByLanguage.get(lang)!.push({
        path: file.path,
        content: file.content,
        product: file.product,
      });
    }
  }

  return {
    success: true,
    filesByLanguage,
    errors: [],
  };
}

export { type BaboonLoadedModel };

export interface TypeInfo {
  pkg: string;
  version: string;
  id: string;
  name: string;
  kind: string;
}

export async function loadModel(files: Map<string, string>): Promise<BaboonLoadedModel> {
  const compiler = await getCompiler();
  const dict: Record<string, string> = {};
  const { inputs } = prepareInputs(files);
  for (const input of inputs) {
    dict[input.path] = input.content;
  }
  return compiler.load(dict);
}

export function listTypes(model: BaboonLoadedModel): TypeInfo[] {
  if (!compilerInstance) {
    throw new Error("Compiler not loaded");
  }
  return compilerInstance.listTypes(model);
}

export function generateRandom(
  model: BaboonLoadedModel,
  pkg: string,
  version: string,
  typeId: string,
): { success: boolean; json?: string; error?: string } {
  if (!compilerInstance) {
    throw new Error("Compiler not loaded");
  }
  return compilerInstance.generateRandom(model, pkg, version, typeId);
}

export async function encodeToUeba(
  model: BaboonLoadedModel,
  pkg: string,
  version: string,
  typeId: string,
  json: string,
  indexed: boolean,
): Promise<{ success: boolean; data?: Uint8Array; error?: string }> {
  const compiler = await getCompiler();
  return compiler.encodeLoaded(model, pkg, version, typeId, json, indexed);
}

export async function decodeFromUeba(
  model: BaboonLoadedModel,
  pkg: string,
  version: string,
  typeId: string,
  data: Uint8Array,
): Promise<{ success: boolean; json?: string; error?: string }> {
  const compiler = await getCompiler();
  return compiler.decodeLoaded(model, pkg, version, typeId, data);
}
