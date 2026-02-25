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

interface JSCompilationResult {
  success: boolean;
  files?: JSOutputFile[];
  errors?: string[];
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

interface BaboonCompilerAPI {
  compile(options: JSCompilerAPIOptions): Promise<JSCompilationResult>;
}

export interface OutputFile {
  path: string;
  content: string;
  product: string;
}

export interface CompilationResult {
  success: boolean;
  filesByLanguage: Map<string, OutputFile[]>;
  errors: string[];
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

const INCLUDE_REGEX = /^(\s*)include\s+"([^"]+)"\s*$/gm;

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

function resolveIncludes(
  content: string,
  allFiles: Map<string, string>,
  directoryInputs: Set<string>,
  visited: Set<string>,
): string {
  return content.replace(INCLUDE_REGEX, (_match, indent: string, includePath: string) => {
    const resolved = resolveIncludePath(directoryInputs, includePath, allFiles);
    if (resolved === null) {
      return `${indent}// [playground] include not found: ${includePath}`;
    }
    if (visited.has(resolved)) {
      return `${indent}// [playground] circular include skipped: ${includePath}`;
    }
    visited.add(resolved);
    const included = allFiles.get(resolved)!;
    return resolveIncludes(included, allFiles, directoryInputs, visited);
  });
}

function prepareInputs(files: Map<string, string>): JSInputFile[] {
  const directoryInputs = collectDirectoryInputs(files.keys());
  const inputs: JSInputFile[] = [];
  for (const [path, content] of files) {
    if (!path.endsWith(".baboon")) {
      continue;
    }
    const resolved = resolveIncludes(content, files, directoryInputs, new Set([path]));
    inputs.push({ path, content: resolved });
  }
  return inputs;
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

  const inputs = prepareInputs(files);
  const targets = buildTargets(options);

  const result = await compiler.compile({
    inputs,
    targets,
  });

  if (!result.success) {
    return {
      success: false,
      filesByLanguage: new Map(),
      errors: result.errors ?? ["Unknown compilation error"],
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
