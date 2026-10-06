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

export interface BaboonTypeInfo {
  pkg: string;
  version: string;
  /** Full type identifier, e.g. `my.pkg/:#Name`; `type:<Name>` for aliases. */
  id: string;
  name: string;
  kind: "dto" | "adt" | "enum" | "foreign" | "contract" | "service" | "alias";
}

export interface BaboonGenerateResult {
  success: boolean;
  json?: string;
  error?: string;
}

export interface BaboonSchemeResult {
  success: boolean;
  content?: string;
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

export interface BaboonLoadedModel {
  // Opaque handle
}

/**
 * A ZIP archive of schemas held in memory, e.g. one written by
 * `baboon :scheme --domains=... --zip-output=...`. Exactly one form.
 * `base64` is the standard alphabet with padding optional; data URLs are rejected.
 */
export type BaboonArchiveInput =
  | { bytes: Uint8Array; base64?: never }
  | { base64: string; bytes?: never };

export interface BaboonEnvelopeEncodeOptions {
  /** Binary BaboonTypeMeta layout: 1 (single bound, ForwardWritePolicy.Strict) or 2 (both bounds). */
  envelopeVersion: 1 | 2;
  /** UEBA index mode of the payload. */
  indexed: boolean;
}

export interface BaboonCompilerAPI {
  compile(options: BaboonCompilerOptions): Promise<BaboonCompilationResult>;
  
  load(files: Record<string, string>): Promise<BaboonLoadedModel>;

  loadMany(archive: BaboonArchiveInput): Promise<BaboonLoadedModel>;

  listTypes(model: BaboonLoadedModel): BaboonTypeInfo[];

  generateRandom(
    model: BaboonLoadedModel,
    pkg: string,
    version: string,
    idString: string
  ): BaboonGenerateResult;

  encode(
    files: Record<string, string>,
    pkg: string,
    version: string,
    idString: string,
    json: string,
    indexed: boolean
  ): Promise<BaboonEncodeResult>;

  encodeLoaded(
    model: BaboonLoadedModel,
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

  decodeLoaded(
    model: BaboonLoadedModel,
    pkg: string,
    version: string,
    idString: string,
    data: Uint8Array
  ): Promise<BaboonDecodeResult>;

  /** JSON top-level envelope -> binary UEBA envelope; domain, version and type come from the envelope. */
  encodedEnvelopeLoaded(
    model: BaboonLoadedModel,
    json: string,
    options: BaboonEnvelopeEncodeOptions
  ): Promise<BaboonEncodeResult>;

  /** Binary UEBA top-level envelope (v1 or v2) -> JSON envelope string. */
  decodeEnvelopeLoaded(
    model: BaboonLoadedModel,
    data: Uint8Array
  ): Promise<BaboonDecodeResult>;

  cleanupScheme(
    files: Record<string, string>,
    domain: string,
    version: string
  ): Promise<BaboonSchemeResult>;

  cleanupSchemeLoaded(
    model: BaboonLoadedModel,
    domain: string,
    version: string
  ): Promise<BaboonSchemeResult>;
}

export const BaboonCompiler: BaboonCompilerAPI;
