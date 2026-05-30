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
  // C#-only
  obsoleteErrors: boolean;
  disregardImplicitUsings: boolean;
  generateIndexWriters: boolean;
  deduplicate: boolean;
  // Rust / TypeScript
  asyncServices: boolean;
  // Kotlin
  multiplatform: boolean;
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

export const DEFAULT_LANGUAGE_OPTIONS: LanguageOptions = {
  generateJsonCodecs: true,
  generateUebaCodecs: true,
  generateJsonCodecsByDefault: false,
  generateUebaCodecsByDefault: false,
  wrappedAdtBranchCodecs: false,
  writeEvolutionDict: false,
  enableDeprecatedEncoders: false,
  obsoleteErrors: false,
  disregardImplicitUsings: false,
  // BaboonJS defaults these to true.
  generateIndexWriters: true,
  deduplicate: true,
  asyncServices: false,
  multiplatform: false,
};

function buildDefaultLanguages(): Record<BaboonTargetLanguage, LanguageOptions> {
  const result = {} as Record<BaboonTargetLanguage, LanguageOptions>;
  for (const lang of TARGET_LANGUAGES) {
    result[lang] = { ...DEFAULT_LANGUAGE_OPTIONS };
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

type GenericOptionDef =
  | { kind: "checkbox"; key: BoolGenericKey; label: string; description: string }
  | { kind: "number"; key: "codecTestIterations"; label: string; description: string; min: number; max: number }
  | { kind: "select"; key: "runtime"; label: string; description: string; choices: { value: RuntimeMode; label: string }[] };

interface LanguageOptionDef {
  key: keyof LanguageOptions;
  label: string;
  description: string;
  /** Languages this option applies to. Undefined ⇒ common to all codec backends. */
  langs?: readonly BaboonTargetLanguage[];
}

const GENERIC_OPTION_DEFS: GenericOptionDef[] = [
  {
    kind: "checkbox",
    key: "disableConversions",
    label: "Disable conversions",
    description: "Skip version-to-version conversion generation",
  },
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
  {
    kind: "checkbox",
    key: "omitVersionSuffixFromPaths",
    label: "Omit version suffix from paths",
    description: "Latest version has no version segment in file paths",
  },
  {
    kind: "checkbox",
    key: "omitVersionSuffixFromNamespaces",
    label: "Omit version suffix from namespaces",
    description: "Latest version has no version segment in its namespace",
  },
  {
    kind: "checkbox",
    key: "generateTests",
    label: "Generate tests",
    description: "Emit generated codec round-trip tests",
  },
  {
    kind: "checkbox",
    key: "generateFixtures",
    label: "Generate fixtures",
    description: "Emit generated random-value fixtures",
  },
  {
    kind: "number",
    key: "codecTestIterations",
    label: "Codec test iterations",
    description: "Iterations performed by generated codec tests",
    min: 0,
    max: 100000,
  },
];

const LANGUAGE_OPTION_DEFS: LanguageOptionDef[] = [
  {
    key: "generateJsonCodecs",
    label: "Generate JSON codecs",
    description: "Generate JSON codecs for types with derived[json]",
  },
  {
    key: "generateUebaCodecs",
    label: "Generate UEBA codecs",
    description: "Generate UEBA binary codecs for types with derived[ueba]",
  },
  {
    key: "generateJsonCodecsByDefault",
    label: "JSON codecs by default",
    description: "Generate JSON codecs even for types without derived[json]",
  },
  {
    key: "generateUebaCodecsByDefault",
    label: "UEBA codecs by default",
    description: "Generate UEBA codecs even for types without derived[ueba]",
  },
  {
    key: "wrappedAdtBranchCodecs",
    label: "Wrapped ADT branches",
    description: "ADT branches encode their own type metadata",
  },
  {
    key: "writeEvolutionDict",
    label: "Evolution dictionary",
    description: "Include schema evolution metadata dictionary",
  },
  {
    key: "enableDeprecatedEncoders",
    label: "Encoders for deprecated versions",
    description: "Generate encoders for deprecated (non-latest) versions",
  },
  {
    key: "obsoleteErrors",
    label: "Obsolete as errors",
    description: "Emit [Obsolete] as errors instead of warnings",
    langs: ["cs"],
  },
  {
    // BaboonJS reads this field as the CLI `--cs-exclude-global-usings` input
    // (it internally negates it). true ⇒ emit explicit usings instead of
    // relying on C# ImplicitUsings.
    key: "disregardImplicitUsings",
    label: "Exclude global usings",
    description: "Emit explicit using directives instead of relying on C# ImplicitUsings",
    langs: ["cs"],
  },
  {
    key: "generateIndexWriters",
    label: "Generate index writers",
    description: "Emit UEBA index writers",
    langs: ["cs"],
  },
  {
    key: "deduplicate",
    label: "Deduplicate",
    description: "Apply code deduplication",
    langs: ["cs"],
  },
  {
    key: "asyncServices",
    label: "Async services",
    description: "Generate async service signatures",
    langs: ["rust", "typescript"],
  },
  {
    key: "multiplatform",
    label: "Kotlin Multiplatform",
    description: "Generate Kotlin Multiplatform (KMP) sources",
    langs: ["kotlin"],
  },
];

// Defs common to every codec backend (shown in the per-language sections and
// aggregated in the "All Languages" section).
const COMMON_LANGUAGE_OPTION_DEFS = LANGUAGE_OPTION_DEFS.filter((d) => d.langs === undefined);

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
        section.appendChild(this.renderCheckbox(
          def.label,
          def.description,
          this.options.generic[def.key],
          (checked) => {
            this.options.generic[def.key] = checked;
            this.onChange(this.options);
          },
        ));
      } else if (def.kind === "number") {
        section.appendChild(this.renderNumber(
          def.label,
          def.description,
          this.options.generic[def.key],
          def.min,
          def.max,
          (value) => {
            this.options.generic[def.key] = value;
            this.onChange(this.options);
          },
        ));
      } else {
        section.appendChild(this.renderSelect(
          def.label,
          def.description,
          this.options.generic[def.key],
          def.choices,
          (value) => {
            this.options.generic[def.key] = value;
            this.onChange(this.options);
          },
        ));
      }
    }

    return section;
  }

  private computeGlobalState(key: keyof LanguageOptions): boolean | null {
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

    for (const def of COMMON_LANGUAGE_OPTION_DEFS) {
      const state = this.computeGlobalState(def.key);
      section.appendChild(this.renderCheckbox(
        def.label,
        def.description,
        state === true,
        (checked) => {
          for (const lang of CODEC_LANGUAGES) {
            this.options.languages[lang][def.key] = checked;
          }
          this.onChange(this.options);
          this.render();
        },
        state === null,
      ));
    }

    return section;
  }

  private renderLanguageSection(lang: BaboonTargetLanguage): HTMLElement {
    const section = document.createElement("div");
    section.className = "options-section";

    const expanded = this.expandedLanguages.has(lang);

    const titleEl = this.renderCollapsibleTitle(
      LANGUAGE_DISPLAY_NAMES[lang],
      expanded,
      () => {
        if (expanded) {
          this.expandedLanguages.delete(lang);
        } else {
          this.expandedLanguages.add(lang);
        }
        this.render();
      },
    );
    section.appendChild(titleEl);

    if (expanded) {
      const langOpts = this.options.languages[lang];
      for (const def of defsForLanguage(lang)) {
        section.appendChild(this.renderCheckbox(
          def.label,
          def.description,
          langOpts[def.key],
          (checked) => {
            langOpts[def.key] = checked;
            this.onChange(this.options);
            this.render();
          },
        ));
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

  private renderCollapsibleTitle(
    text: string,
    expanded: boolean,
    onToggle: () => void,
  ): HTMLElement {
    const title = document.createElement("div");
    title.className = "options-section-title collapsible";
    title.addEventListener("click", onToggle);

    const arrow = document.createElement("span");
    arrow.className = "options-collapse-arrow";
    arrow.textContent = expanded ? "▾" : "▸";
    title.appendChild(arrow);

    const label = document.createTextNode(text);
    title.appendChild(label);

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
    checkbox.addEventListener("change", () => {
      onToggle(checkbox.checked);
    });
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
    select.addEventListener("change", () => {
      onSelect(select.value as T);
    });
    row.appendChild(select);
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
  * populated CompilerOptions, filling gaps from the defaults so older or
  * hand-edited links keep working. */
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
    if (provided) languages[lang] = { ...DEFAULT_LANGUAGE_OPTIONS, ...provided };
  }
  return { generic, languages };
}
