import {
  type BaboonTargetLanguage,
  TARGET_LANGUAGES,
  LANGUAGE_DISPLAY_NAMES,
} from "./compiler.ts";

export interface GenericOptions {
  disableConversions: boolean;
}

export interface LanguageOptions {
  wrappedAdtBranchCodecs: boolean;
  generateJsonCodecs: boolean;
  generateUebaCodecs: boolean;
  generateJsonCodecsByDefault: boolean;
  generateUebaCodecsByDefault: boolean;
  writeEvolutionDict: boolean;
}

export interface CompilerOptions {
  generic: GenericOptions;
  languages: Record<BaboonTargetLanguage, LanguageOptions>;
}

export const DEFAULT_GENERIC_OPTIONS: GenericOptions = {
  disableConversions: false,
};

export const DEFAULT_LANGUAGE_OPTIONS: LanguageOptions = {
  wrappedAdtBranchCodecs: false,
  generateJsonCodecs: true,
  generateUebaCodecs: true,
  generateJsonCodecsByDefault: false,
  generateUebaCodecsByDefault: false,
  writeEvolutionDict: false,
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

interface GenericOptionDef {
  key: keyof GenericOptions;
  label: string;
  description: string;
}

interface LanguageOptionDef {
  key: keyof LanguageOptions;
  label: string;
  description: string;
}

const GENERIC_OPTION_DEFS: GenericOptionDef[] = [
  {
    key: "disableConversions",
    label: "Disable conversions",
    description: "Skip version-to-version conversion generation",
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
];

export class OptionsPanel {
  private overlay: HTMLElement;
  private panel: HTMLElement;
  private scrollBody: HTMLElement;
  private options: CompilerOptions;
  private expandedLanguages: Set<BaboonTargetLanguage> = new Set();
  private onChange: (options: CompilerOptions) => void;

  constructor(onChange: (options: CompilerOptions) => void) {
    this.options = {
      generic: { ...DEFAULT_GENERIC_OPTIONS },
      languages: buildDefaultLanguages(),
    };
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
    for (const lang of TARGET_LANGUAGES) {
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
    closeBtn.textContent = "\u2715";
    closeBtn.addEventListener("click", () => this.close());
    header.appendChild(closeBtn);

    return header;
  }

  private renderGenericSection(): HTMLElement {
    const section = document.createElement("div");
    section.className = "options-section";

    section.appendChild(this.renderSectionTitle("Generic"));

    for (const def of GENERIC_OPTION_DEFS) {
      section.appendChild(this.renderCheckbox(
        def.label,
        def.description,
        this.options.generic[def.key],
        (checked) => {
          this.options.generic[def.key] = checked;
          this.onChange(this.options);
        },
      ));
    }

    return section;
  }

  private computeGlobalState(key: keyof LanguageOptions): boolean | null {
    let allTrue = true;
    let allFalse = true;
    for (const lang of TARGET_LANGUAGES) {
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

    for (const def of LANGUAGE_OPTION_DEFS) {
      const state = this.computeGlobalState(def.key);
      section.appendChild(this.renderCheckbox(
        def.label,
        def.description,
        state === true,
        (checked) => {
          for (const lang of TARGET_LANGUAGES) {
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
      for (const def of LANGUAGE_OPTION_DEFS) {
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
    arrow.textContent = expanded ? "\u25BE" : "\u25B8";
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

    row.appendChild(text);
    return row;
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
}
