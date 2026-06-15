import {
  type BaboonTargetLanguage,
  TARGET_LANGUAGES,
  LANGUAGE_DISPLAY_NAMES,
} from "./compiler.ts";

// Schema-only backends don't have codec/evolution options
const SCHEMA_ONLY_LANGUAGES: ReadonlySet<string> = new Set(["graphql", "openapi"]);
const CODEC_LANGUAGES = TARGET_LANGUAGES.filter((l) => !SCHEMA_ONLY_LANGUAGES.has(l));

export type RuntimeMode = "with" | "only" | "without";

export interface GenericOptions {
  disableConversions: boolean;
  runtime: RuntimeMode;
  omitVersionSuffixFromPaths: boolean;
  omitVersionSuffixFromNamespaces: boolean;
  generateTests: boolean;
  generateFixtures: boolean;
  codecTestIterations: number;
}

export interface LanguageOptions {
  // common to every codec backend
  generateJsonCodecs: boolean;
  generateUebaCodecs: boolean;
  generateJsonCodecsByDefault: boolean;
  generateUebaCodecsByDefault: boolean;
  wrappedAdtBranchCodecs: boolean;
  writeEvolutionDict: boolean;
  enableDeprecatedEncoders: boolean;
  generateDomainFacade: boolean;
  // service result wrapping
  serviceResultNoErrors: boolean;
  serviceResultType: string;
  serviceResultPattern: string;
  // service context parameter
  serviceContextMode: string; // none | abstract | type
  serviceContextType: string;
  serviceContextParameterName: string;
  // pragmas: one "key=value" per line
  pragma: string;
  // C#-only
  obsoleteErrors: boolean;
  disregardImplicitUsings: boolean;
  generateIndexWriters: boolean;
  deduplicate: boolean;
  // Rust / TypeScript
  asyncServices: boolean;
  // TypeScript
  importSuffix: string;
  // Kotlin
  multiplatform: boolean;
  // Scala service-result HKT
  serviceResultHkt: boolean;
  serviceResultHktName: string;
  serviceResultHktSignature: string;
  // MCP server generation
  generateMcpServer: boolean;
}

export interface CompilerOptions {
  generic: GenericOptions;
  languages: Record<BaboonTargetLanguage, LanguageOptions>;
}

export const DEFAULT_GENERIC_OPTIONS: GenericOptions = {
  disableConversions: false,
  runtime: "with",
  // Mirror the compiler's defaults (BaboonJS reads these with getOrElse(true)).
  omitVersionSuffixFromPaths: true,
  omitVersionSuffixFromNamespaces: true,
  generateTests: false,
  generateFixtures: false,
  codecTestIterations: 500,
};

// Per-language service-result defaults, mirroring ServiceResultConfig.*Default
// in the compiler so the controls show (and round-trip) the true defaults.
const SERVICE_RESULT_DEFAULTS: Record<BaboonTargetLanguage, { noErrors: boolean; type: string; pattern: string }> = {
  cs: { noErrors: true, type: "", pattern: "" },
  scala: { noErrors: false, type: "scala.util.Either", pattern: "[$error, $success]" },
  python: { noErrors: true, type: "", pattern: "" },
  rust: { noErrors: false, type: "Result", pattern: "<$success, $error>" },
  typescript: { noErrors: true, type: "", pattern: "" },
  kotlin: { noErrors: false, type: "Either", pattern: "<$error, $success>" },
  java: { noErrors: true, type: "", pattern: "" },
  dart: { noErrors: true, type: "", pattern: "" },
  swift: { noErrors: true, type: "", pattern: "" },
  graphql: { noErrors: true, type: "", pattern: "" },
  openapi: { noErrors: true, type: "", pattern: "" },
};

export const DEFAULT_LANGUAGE_OPTIONS: LanguageOptions = {
  generateJsonCodecs: true,
  generateUebaCodecs: true,
  generateJsonCodecsByDefault: false,
  generateUebaCodecsByDefault: false,
  wrappedAdtBranchCodecs: false,
  writeEvolutionDict: false,
  enableDeprecatedEncoders: false,
  generateDomainFacade: true,
  serviceResultNoErrors: false,
  serviceResultType: "",
  serviceResultPattern: "",
  serviceContextMode: "none",
  serviceContextType: "Ctx",
  serviceContextParameterName: "ctx",
  pragma: "",
  obsoleteErrors: false,
  disregardImplicitUsings: false,
  // BaboonJS defaults these to true.
  generateIndexWriters: true,
  deduplicate: true,
  asyncServices: false,
  importSuffix: "",
  multiplatform: false,
  serviceResultHkt: false,
  serviceResultHktName: "F",
  serviceResultHktSignature: "[+_, +_]",
  generateMcpServer: false,
};

function defaultsForLanguage(lang: BaboonTargetLanguage): LanguageOptions {
  const sr = SERVICE_RESULT_DEFAULTS[lang];
  return {
    ...DEFAULT_LANGUAGE_OPTIONS,
    serviceResultNoErrors: sr.noErrors,
    serviceResultType: sr.type,
    serviceResultPattern: sr.pattern,
  };
}

function buildDefaultLanguages(): Record<BaboonTargetLanguage, LanguageOptions> {
  const result = {} as Record<BaboonTargetLanguage, LanguageOptions>;
  for (const lang of TARGET_LANGUAGES) {
    result[lang] = defaultsForLanguage(lang);
  }
  return result;
}

export const DEFAULT_OPTIONS: CompilerOptions = {
  generic: { ...DEFAULT_GENERIC_OPTIONS },
  languages: buildDefaultLanguages(),
};

type BoolGenericKey = {
  [K in keyof GenericOptions]: GenericOptions[K] extends boolean ? K : never;
}[keyof GenericOptions];

type BoolLangKey = {
  [K in keyof LanguageOptions]: LanguageOptions[K] extends boolean ? K : never;
}[keyof LanguageOptions];

type StrLangKey = {
  [K in keyof LanguageOptions]: LanguageOptions[K] extends string ? K : never;
}[keyof LanguageOptions];

type GenericOptionDef =
  | { kind: "checkbox"; key: BoolGenericKey; label: string; description: string }
  | { kind: "number"; key: "codecTestIterations"; label: string; description: string; min: number; max: number }
  | { kind: "select"; key: "runtime"; label: string; description: string; choices: { value: RuntimeMode; label: string }[] };

type LanguageOptionDef =
  | { kind: "checkbox"; key: BoolLangKey; label: string; description: string; langs?: readonly BaboonTargetLanguage[]; aggregate?: boolean }
  | { kind: "text" | "textarea"; key: StrLangKey; label: string; description: string; langs?: readonly BaboonTargetLanguage[]; placeholder?: string }
  | { kind: "select"; key: StrLangKey; label: string; description: string; langs?: readonly BaboonTargetLanguage[]; choices: { value: string; label: string }[] };

const GENERIC_OPTION_DEFS: GenericOptionDef[] = [
  { kind: "checkbox", key: "disableConversions", label: "Disable conversions", description: "Skip version-to-version conversion generation" },
  {
    kind: "select",
    key: "runtime",
    label: "Runtime classes",
    description: "Emit shared runtime classes / evolution registrations",
    choices: [
      { value: "with", label: "with (code + runtime)" },
      { value: "only", label: "only (runtime only)" },
      { value: "without", label: "without (code only)" },
    ],
  },
  { kind: "checkbox", key: "omitVersionSuffixFromPaths", label: "Omit version suffix from paths", description: "Latest version has no version segment in file paths" },
  { kind: "checkbox", key: "omitVersionSuffixFromNamespaces", label: "Omit version suffix from namespaces", description: "Latest version has no version segment in its namespace" },
  { kind: "checkbox", key: "generateTests", label: "Generate tests", description: "Emit generated codec round-trip tests" },
  { kind: "checkbox", key: "generateFixtures", label: "Generate fixtures", description: "Emit generated random-value fixtures" },
  { kind: "number", key: "codecTestIterations", label: "Codec test iterations", description: "Iterations performed by generated codec tests", min: 0, max: 100000 },
];

const SERVICE_CONTEXT_MODES = [
  { value: "none", label: "none" },
  { value: "abstract", label: "abstract" },
  { value: "type", label: "type" },
];

const LANGUAGE_OPTION_DEFS: LanguageOptionDef[] = [
  { kind: "checkbox", key: "generateJsonCodecs", label: "Generate JSON codecs", description: "Generate JSON codecs for types with derived[json]", aggregate: true },
  { kind: "checkbox", key: "generateUebaCodecs", label: "Generate UEBA codecs", description: "Generate UEBA binary codecs for types with derived[ueba]", aggregate: true },
  { kind: "checkbox", key: "generateJsonCodecsByDefault", label: "JSON codecs by default", description: "Generate JSON codecs even for types without derived[json]", aggregate: true },
  { kind: "checkbox", key: "generateUebaCodecsByDefault", label: "UEBA codecs by default", description: "Generate UEBA codecs even for types without derived[ueba]", aggregate: true },
  { kind: "checkbox", key: "wrappedAdtBranchCodecs", label: "Wrapped ADT branches", description: "ADT branches encode their own type metadata", aggregate: true },
  { kind: "checkbox", key: "writeEvolutionDict", label: "Evolution dictionary", description: "Include schema evolution metadata dictionary", aggregate: true },
  { kind: "checkbox", key: "enableDeprecatedEncoders", label: "Encoders for deprecated versions", description: "Generate encoders for deprecated (non-latest) versions", aggregate: true },
  { kind: "checkbox", key: "generateDomainFacade", label: "Domain facade", description: "Emit a per-domain Domain<Id>Facade that auto-registers all versions" },
  // Service result wrapping
  { kind: "checkbox", key: "serviceResultNoErrors", label: "Service result: no errors", description: "Service methods return only the success type (no error wrapping)" },
  { kind: "text", key: "serviceResultType", label: "Service result type", description: "Wrapper type for service results (e.g. Either) — blank uses the language default", placeholder: "(language default)" },
  { kind: "text", key: "serviceResultPattern", label: "Service result pattern", description: "Pattern for the result type, e.g. <$error, $success>", placeholder: "(language default)" },
  // Service context parameter
  { kind: "select", key: "serviceContextMode", label: "Service context mode", description: "Context parameter mode for service methods", choices: SERVICE_CONTEXT_MODES },
  { kind: "text", key: "serviceContextType", label: "Service context type", description: "Context type name", placeholder: "Ctx" },
  { kind: "text", key: "serviceContextParameterName", label: "Service context parameter", description: "Context parameter name", placeholder: "ctx" },
  // Pragmas
  { kind: "textarea", key: "pragma", label: "Pragmas", description: "One key=value per line", placeholder: "key=value" },
  // C#
  { kind: "checkbox", key: "obsoleteErrors", label: "Obsolete as errors", description: "Emit [Obsolete] as errors instead of warnings", langs: ["cs"] },
  {
    // BaboonJS reads this field as the CLI `--cs-exclude-global-usings` input
    // (it internally negates it). true ⇒ emit explicit usings instead of
    // relying on C# ImplicitUsings.
    kind: "checkbox",
    key: "disregardImplicitUsings",
    label: "Exclude global usings",
    description: "Emit explicit using directives instead of relying on C# ImplicitUsings",
    langs: ["cs"],
  },
  { kind: "checkbox", key: "generateIndexWriters", label: "Generate index writers", description: "Emit UEBA index writers", langs: ["cs"] },
  { kind: "checkbox", key: "deduplicate", label: "Deduplicate", description: "Apply code deduplication", langs: ["cs"] },
  // Rust / TypeScript
  { kind: "checkbox", key: "asyncServices", label: "Async services", description: "Generate async service signatures", langs: ["rust", "typescript", "cs", "python", "java", "kotlin", "dart", "swift"] },
  // TypeScript
  { kind: "text", key: "importSuffix", label: "Import suffix", description: "Suffix appended to generated import paths (e.g. .ts/.js)", langs: ["typescript"], placeholder: "(none)" },
  // Kotlin
  { kind: "checkbox", key: "multiplatform", label: "Kotlin Multiplatform", description: "Generate Kotlin Multiplatform (KMP) sources", langs: ["kotlin"] },
  // Scala service-result HKT
  { kind: "checkbox", key: "serviceResultHkt", label: "Service result HKT", description: "Use a higher-kinded type parameter for service results", langs: ["scala"] },
  { kind: "text", key: "serviceResultHktName", label: "HKT name", description: "HKT type parameter name (e.g. F)", langs: ["scala"], placeholder: "F" },
  { kind: "text", key: "serviceResultHktSignature", label: "HKT signature", description: "HKT type parameter signature (e.g. [+_, +_])", langs: ["scala"], placeholder: "[+_, +_]" },
  // MCP server
  { kind: "checkbox", key: "generateMcpServer", label: "Generate MCP server", description: "Emit a Model Context Protocol server implementation", langs: ["typescript", "cs", "scala", "rust", "kotlin", "java", "dart", "swift", "python"] },
];

// Aggregated in the "All Languages" section: only uniform-default common checkboxes.
const AGGREGATE_OPTION_DEFS = LANGUAGE_OPTION_DEFS.filter(
  (d): d is Extract<LanguageOptionDef, { kind: "checkbox" }> => d.kind === "checkbox" && d.langs === undefined && d.aggregate === true,
);

function defsForLanguage(lang: BaboonTargetLanguage): LanguageOptionDef[] {
  return LANGUAGE_OPTION_DEFS.filter((d) => d.langs === undefined || d.langs.includes(lang));
}

export class OptionsPanel {
  private overlay: HTMLElement;
  private panel: HTMLElement;
  private scrollBody: HTMLElement;
  private options: CompilerOptions;
  private expandedLanguages: Set<BaboonTargetLanguage> = new Set();
  private onChange: (options: CompilerOptions) => void;

  constructor(onChange: (options: CompilerOptions) => void) {
    this.options = structuredClone(DEFAULT_OPTIONS);
    this.onChange = onChange;

    this.overlay = document.createElement("div");
    this.overlay.className = "options-overlay";
    this.overlay.addEventListener("click", (e) => {
      if (e.target === this.overlay) this.close();
    });

    this.panel = document.createElement("div");
    this.panel.className = "options-panel";
    this.overlay.appendChild(this.panel);

    this.scrollBody = document.createElement("div");
    this.scrollBody.className = "options-scroll-body";
    this.panel.appendChild(this.scrollBody);

    document.body.appendChild(this.overlay);
    this.render();
  }

  private render(): void {
    const header = this.panel.querySelector(".options-header");
    if (!header) {
      this.panel.insertBefore(this.renderHeader(), this.scrollBody);
    }

    this.scrollBody.innerHTML = "";
    this.scrollBody.appendChild(this.renderGenericSection());
    this.scrollBody.appendChild(this.renderGlobalLanguageSection());
    for (const lang of CODEC_LANGUAGES) {
      this.scrollBody.appendChild(this.renderLanguageSection(lang));
    }
  }

  private renderHeader(): HTMLElement {
    const header = document.createElement("div");
    header.className = "options-header";

    const title = document.createElement("span");
    title.className = "options-title";
    title.textContent = "Compiler Options";
    header.appendChild(title);

    const closeBtn = document.createElement("button");
    closeBtn.className = "options-close-btn";
    closeBtn.textContent = "✕";
    closeBtn.addEventListener("click", () => this.close());
    header.appendChild(closeBtn);

    return header;
  }

  private renderGenericSection(): HTMLElement {
    const section = document.createElement("div");
    section.className = "options-section";

    section.appendChild(this.renderSectionTitle("Generic"));

    for (const def of GENERIC_OPTION_DEFS) {
      if (def.kind === "checkbox") {
        section.appendChild(this.renderCheckbox(def.label, def.description, this.options.generic[def.key], (checked) => {
          this.options.generic[def.key] = checked;
          this.onChange(this.options);
        }));
      } else if (def.kind === "number") {
        section.appendChild(this.renderNumber(def.label, def.description, this.options.generic[def.key], def.min, def.max, (value) => {
          this.options.generic[def.key] = value;
          this.onChange(this.options);
        }));
      } else {
        section.appendChild(this.renderSelect(def.label, def.description, this.options.generic[def.key], def.choices, (value) => {
          this.options.generic[def.key] = value;
          this.onChange(this.options);
        }));
      }
    }

    return section;
  }

  private computeGlobalState(key: BoolLangKey): boolean | null {
    let allTrue = true;
    let allFalse = true;
    for (const lang of CODEC_LANGUAGES) {
      if (this.options.languages[lang][key]) {
        allFalse = false;
      } else {
        allTrue = false;
      }
    }
    if (allTrue) return true;
    if (allFalse) return false;
    return null;
  }

  private renderGlobalLanguageSection(): HTMLElement {
    const section = document.createElement("div");
    section.className = "options-section";

    section.appendChild(this.renderSectionTitle("All Languages"));

    for (const def of AGGREGATE_OPTION_DEFS) {
      const state = this.computeGlobalState(def.key);
      section.appendChild(this.renderCheckbox(def.label, def.description, state === true, (checked) => {
        for (const lang of CODEC_LANGUAGES) {
          this.options.languages[lang][def.key] = checked;
        }
        this.onChange(this.options);
        this.render();
      }, state === null));
    }

    return section;
  }

  private renderLanguageSection(lang: BaboonTargetLanguage): HTMLElement {
    const section = document.createElement("div");
    section.className = "options-section";

    const expanded = this.expandedLanguages.has(lang);

    const titleEl = this.renderCollapsibleTitle(LANGUAGE_DISPLAY_NAMES[lang], expanded, () => {
      if (expanded) {
        this.expandedLanguages.delete(lang);
      } else {
        this.expandedLanguages.add(lang);
      }
      this.render();
    });
    section.appendChild(titleEl);

    if (expanded) {
      const langOpts = this.options.languages[lang];
      for (const def of defsForLanguage(lang)) {
        if (def.kind === "checkbox") {
          section.appendChild(this.renderCheckbox(def.label, def.description, langOpts[def.key], (checked) => {
            langOpts[def.key] = checked;
            this.onChange(this.options);
            this.render();
          }));
        } else if (def.kind === "select") {
          section.appendChild(this.renderSelect(def.label, def.description, langOpts[def.key], def.choices, (value) => {
            langOpts[def.key] = value;
            this.onChange(this.options);
            this.render();
          }));
        } else {
          section.appendChild(this.renderTextInput(def.kind, def.label, def.description, langOpts[def.key], def.placeholder ?? "", (value) => {
            langOpts[def.key] = value;
            this.onChange(this.options);
          }));
        }
      }
    }

    return section;
  }

  private renderSectionTitle(text: string): HTMLElement {
    const title = document.createElement("div");
    title.className = "options-section-title";
    title.textContent = text;
    return title;
  }

  private renderCollapsibleTitle(text: string, expanded: boolean, onToggle: () => void): HTMLElement {
    const title = document.createElement("div");
    title.className = "options-section-title collapsible";
    title.addEventListener("click", onToggle);

    const arrow = document.createElement("span");
    arrow.className = "options-collapse-arrow";
    arrow.textContent = expanded ? "▾" : "▸";
    title.appendChild(arrow);

    title.appendChild(document.createTextNode(text));
    return title;
  }

  private renderCheckbox(
    label: string,
    description: string,
    checked: boolean,
    onToggle: (checked: boolean) => void,
    indeterminate: boolean = false,
  ): HTMLElement {
    const row = document.createElement("label");
    row.className = "options-row";

    const checkbox = document.createElement("input");
    checkbox.type = "checkbox";
    checkbox.className = "options-checkbox";
    checkbox.checked = checked;
    checkbox.indeterminate = indeterminate;
    checkbox.addEventListener("change", () => onToggle(checkbox.checked));
    row.appendChild(checkbox);

    row.appendChild(this.renderLabelText(label, description));
    return row;
  }

  private renderNumber(
    label: string,
    description: string,
    value: number,
    min: number,
    max: number,
    onInput: (value: number) => void,
  ): HTMLElement {
    const row = document.createElement("label");
    row.className = "options-row options-row-control";
    row.appendChild(this.renderLabelText(label, description));

    const input = document.createElement("input");
    input.type = "number";
    input.className = "options-number";
    input.min = String(min);
    input.max = String(max);
    input.value = String(value);
    input.addEventListener("change", () => {
      let v = Number.parseInt(input.value, 10);
      if (Number.isNaN(v)) v = value;
      v = Math.min(max, Math.max(min, v));
      input.value = String(v);
      onInput(v);
    });
    row.appendChild(input);
    return row;
  }

  private renderSelect<T extends string>(
    label: string,
    description: string,
    value: T,
    choices: { value: T; label: string }[],
    onSelect: (value: T) => void,
  ): HTMLElement {
    const row = document.createElement("label");
    row.className = "options-row options-row-control";
    row.appendChild(this.renderLabelText(label, description));

    const select = document.createElement("select");
    select.className = "options-select";
    for (const choice of choices) {
      const opt = document.createElement("option");
      opt.value = choice.value;
      opt.textContent = choice.label;
      if (choice.value === value) opt.selected = true;
      select.appendChild(opt);
    }
    select.addEventListener("change", () => onSelect(select.value as T));
    row.appendChild(select);
    return row;
  }

  private renderTextInput(
    kind: "text" | "textarea",
    label: string,
    description: string,
    value: string,
    placeholder: string,
    onInput: (value: string) => void,
  ): HTMLElement {
    const row = document.createElement("div");
    row.className = "options-row options-row-stacked";
    row.appendChild(this.renderLabelText(label, description));

    const input = kind === "textarea" ? document.createElement("textarea") : document.createElement("input");
    if (input instanceof HTMLInputElement) input.type = "text";
    input.className = kind === "textarea" ? "options-textarea" : "options-text";
    input.placeholder = placeholder;
    input.value = value;
    input.addEventListener("change", () => onInput(input.value));
    row.appendChild(input);
    return row;
  }

  private renderLabelText(label: string, description: string): HTMLElement {
    const text = document.createElement("span");
    text.className = "options-label-text";

    const labelSpan = document.createElement("span");
    labelSpan.className = "options-label-name";
    labelSpan.textContent = label;
    text.appendChild(labelSpan);

    const desc = document.createElement("span");
    desc.className = "options-label-desc";
    desc.textContent = description;
    text.appendChild(desc);

    return text;
  }

  open(): void {
    this.overlay.classList.add("visible");
  }

  close(): void {
    this.overlay.classList.remove("visible");
  }

  getOptions(): CompilerOptions {
    return structuredClone(this.options);
  }

  /** Replace the current options (e.g. when restoring from a shared link) and
    * re-render. Missing/unknown keys fall back to the defaults. */
  setOptions(options: CompilerOptions): void {
    this.options = mergeWithDefaults(options);
    this.onChange(this.options);
    this.render();
  }
}

/** Coerce an arbitrary (possibly partial / shared) options object into a fully
  * populated CompilerOptions, filling gaps from the per-language defaults so
  * older or hand-edited links keep working. */
export function mergeWithDefaults(options: Partial<CompilerOptions> | undefined): CompilerOptions {
  const generic: GenericOptions = { ...DEFAULT_GENERIC_OPTIONS, ...(options?.generic ?? {}) };
  if (generic.runtime !== "with" && generic.runtime !== "only" && generic.runtime !== "without") {
    generic.runtime = DEFAULT_GENERIC_OPTIONS.runtime;
  }
  if (typeof generic.codecTestIterations !== "number" || Number.isNaN(generic.codecTestIterations)) {
    generic.codecTestIterations = DEFAULT_GENERIC_OPTIONS.codecTestIterations;
  }
  const languages = buildDefaultLanguages();
  for (const lang of TARGET_LANGUAGES) {
    const provided = options?.languages?.[lang];
    if (provided) languages[lang] = { ...languages[lang], ...provided };
  }
  return { generic, languages };
}
