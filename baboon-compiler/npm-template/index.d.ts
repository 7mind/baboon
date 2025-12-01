export type BaboonLanguage = "cs" | "scala";

export interface BaboonInputFile {
  path: string;
  content: string;
}

export interface BaboonOutputFile {
  path: string;
  content: string;
  product: string;
}

export interface BaboonCompilationResult {
  success: boolean;
  files?: BaboonOutputFile[];
  errors?: string[];
}

export interface BaboonEncodeResult {
  success: boolean;
  data?: Uint8Array;
  error?: string;
}

export interface BaboonDecodeResult {
  success: boolean;
  json?: string;
  error?: string;
}

export interface BaboonGenericOptions {
  codecTestIterations?: number;
  omitMostRecentVersionSuffixFromPaths?: boolean;
  omitMostRecentVersionSuffixFromNamespaces?: boolean;
  runtime?: "with" | "only" | "without";
  disableConversions?: boolean;
  generateTests?: boolean;
  generateFixtures?: boolean;
}

export interface BaboonCSOptions {
  obsoleteErrors?: boolean;
  omitMostRecentVersionSuffixFromPaths?: boolean;
  omitMostRecentVersionSuffixFromNamespaces?: boolean;
  wrappedAdtBranchCodecs?: boolean;
  writeEvolutionDict?: boolean;
  disregardImplicitUsings?: boolean;
  enableDeprecatedEncoders?: boolean;
  generateIndexWriters?: boolean;
  generateJsonCodecs?: boolean;
  generateUebaCodecs?: boolean;
  generateUebaCodecsByDefault?: boolean;
  generateJsonCodecsByDefault?: boolean;
  deduplicate?: boolean;
}

export interface BaboonScalaOptions {
  writeEvolutionDict?: boolean;
  wrappedAdtBranchCodecs?: boolean;
}

export interface BaboonCompilerTarget {
  language: BaboonLanguage;
  generic?: BaboonGenericOptions;
  cs?: BaboonCSOptions;
  scala?: BaboonScalaOptions;
}

export interface BaboonCompilerOptions {
  inputs: BaboonInputFile[];
  targets: BaboonCompilerTarget[];
  debug?: boolean;
}

export interface BaboonCompilerAPI {
  compile(options: BaboonCompilerOptions): Promise<BaboonCompilationResult>;
  encode(
    files: Record<string, string>,
    pkg: string,
    version: string,
    idString: string,
    json: string,
    indexed: boolean
  ): Promise<BaboonEncodeResult>;
  decode(
    files: Record<string, string>,
    pkg: string,
    version: string,
    idString: string,
    data: Uint8Array
  ): Promise<BaboonDecodeResult>;
}

export const BaboonCompiler: BaboonCompilerAPI;
