import * as monaco from "monaco-editor";
import {
  type BaboonLoadedModel,
  type TypeInfo,
  loadModel,
  listTypes,
  generateRandom,
  encodeToUeba,
  decodeFromUeba,
} from "./compiler.ts";

export class CodecToolsPanel {
  private overlay: HTMLElement;
  private model: BaboonLoadedModel | null = null;
  private types: TypeInfo[] = [];
  private selectedType: TypeInfo | null = null;

  private typeSelect: HTMLSelectElement;
  private generateBtn: HTMLButtonElement;
  private encodeBtn: HTMLButtonElement;
  private decodeBtn: HTMLButtonElement;
  private indexedCheckbox: HTMLInputElement;
  private jsonEditorContainer: HTMLElement;
  private jsonEditor: monaco.editor.IStandaloneCodeEditor;
  private uebaHexArea: HTMLTextAreaElement;
  private statusEl: HTMLElement;

  constructor() {
    this.overlay = document.createElement("div");
    this.overlay.className = "codec-overlay";
    document.body.appendChild(this.overlay);

    const panel = document.createElement("div");
    panel.className = "codec-panel";
    this.overlay.appendChild(panel);

    // Header
    const header = document.createElement("div");
    header.className = "codec-header";
    panel.appendChild(header);

    const title = document.createElement("span");
    title.className = "codec-title";
    title.textContent = "Codec Tools";
    header.appendChild(title);

    const closeBtn = document.createElement("button");
    closeBtn.className = "options-close-btn";
    closeBtn.textContent = "\u00D7";
    closeBtn.addEventListener("click", () => this.close());
    header.appendChild(closeBtn);

    const body = document.createElement("div");
    body.className = "codec-body";
    panel.appendChild(body);

    // Type selector row
    const typeRow = document.createElement("div");
    typeRow.className = "codec-row";
    body.appendChild(typeRow);

    const typeLabel = document.createElement("label");
    typeLabel.className = "codec-label";
    typeLabel.textContent = "Type:";
    typeRow.appendChild(typeLabel);

    this.typeSelect = document.createElement("select");
    this.typeSelect.className = "codec-select";
    this.typeSelect.addEventListener("change", () => this.onTypeSelected());
    typeRow.appendChild(this.typeSelect);

    // Action buttons row
    const actionsRow = document.createElement("div");
    actionsRow.className = "codec-row codec-actions";
    body.appendChild(actionsRow);

    this.generateBtn = document.createElement("button");
    this.generateBtn.className = "codec-btn codec-btn-primary";
    this.generateBtn.textContent = "Generate Random";
    this.generateBtn.addEventListener("click", () => this.onGenerate());
    actionsRow.appendChild(this.generateBtn);

    this.encodeBtn = document.createElement("button");
    this.encodeBtn.className = "codec-btn";
    this.encodeBtn.textContent = "Encode to UEBA";
    this.encodeBtn.addEventListener("click", () => this.onEncode());
    actionsRow.appendChild(this.encodeBtn);

    this.decodeBtn = document.createElement("button");
    this.decodeBtn.className = "codec-btn";
    this.decodeBtn.textContent = "Decode UEBA";
    this.decodeBtn.addEventListener("click", () => this.onDecode());
    actionsRow.appendChild(this.decodeBtn);

    const indexedLabel = document.createElement("label");
    indexedLabel.className = "codec-indexed-label";
    this.indexedCheckbox = document.createElement("input");
    this.indexedCheckbox.type = "checkbox";
    this.indexedCheckbox.className = "codec-checkbox";
    indexedLabel.appendChild(this.indexedCheckbox);
    const indexedText = document.createElement("span");
    indexedText.textContent = "Indexed";
    indexedLabel.appendChild(indexedText);
    actionsRow.appendChild(indexedLabel);

    // JSON section
    const jsonSection = document.createElement("div");
    jsonSection.className = "codec-section";
    body.appendChild(jsonSection);

    const jsonLabel = document.createElement("div");
    jsonLabel.className = "codec-section-label";
    jsonLabel.textContent = "JSON";
    jsonSection.appendChild(jsonLabel);

    this.jsonEditorContainer = document.createElement("div");
    this.jsonEditorContainer.className = "codec-json-editor";
    jsonSection.appendChild(this.jsonEditorContainer);

    this.jsonEditor = monaco.editor.create(this.jsonEditorContainer, {
      language: "json",
      theme: "vs-dark",
      automaticLayout: true,
      minimap: { enabled: false },
      fontSize: 13,
      tabSize: 2,
      scrollBeyondLastLine: false,
      wordWrap: "on",
      lineNumbers: "off",
    });

    // UEBA section
    const uebaSection = document.createElement("div");
    uebaSection.className = "codec-section";
    body.appendChild(uebaSection);

    const uebaLabel = document.createElement("div");
    uebaLabel.className = "codec-section-label";
    uebaLabel.textContent = "UEBA (hex)";
    uebaSection.appendChild(uebaLabel);

    this.uebaHexArea = document.createElement("textarea");
    this.uebaHexArea.className = "codec-ueba-hex";
    this.uebaHexArea.placeholder = "UEBA hex bytes will appear here...";
    uebaSection.appendChild(this.uebaHexArea);

    // Status bar
    this.statusEl = document.createElement("div");
    this.statusEl.className = "codec-status";
    body.appendChild(this.statusEl);

    this.overlay.addEventListener("click", (e) => {
      if (e.target === this.overlay) {
        this.close();
      }
    });
  }

  async open(files: Map<string, string>): Promise<void> {
    this.overlay.classList.add("visible");
    this.setStatus("Loading model...");

    try {
      this.model = await loadModel(files);
      this.types = listTypes(this.model);
      this.populateTypeSelector();
      this.setStatus(`Loaded ${this.types.length} types`);
    } catch (e) {
      this.setStatus(`Load failed: ${e instanceof Error ? e.message : String(e)}`, true);
    }

    this.jsonEditor.layout();
  }

  close(): void {
    this.overlay.classList.remove("visible");
  }

  private populateTypeSelector(): void {
    this.typeSelect.innerHTML = "";

    const grouped = new Map<string, TypeInfo[]>();
    for (const t of this.types) {
      const key = `${t.pkg} @ ${t.version}`;
      let group = grouped.get(key);
      if (!group) {
        group = [];
        grouped.set(key, group);
      }
      group.push(t);
    }

    for (const [groupLabel, groupTypes] of grouped) {
      const optGroup = document.createElement("optgroup");
      optGroup.label = groupLabel;
      for (const t of groupTypes) {
        const option = document.createElement("option");
        option.value = `${t.pkg}|${t.version}|${t.id}`;
        option.textContent = `${t.name} (${t.kind})`;
        optGroup.appendChild(option);
      }
      this.typeSelect.appendChild(optGroup);
    }

    this.onTypeSelected();
  }

  private onTypeSelected(): void {
    const val = this.typeSelect.value;
    if (!val) {
      this.selectedType = null;
      return;
    }
    const [pkg, version, id] = val.split("|");
    this.selectedType = this.types.find(
      (t) => t.pkg === pkg && t.version === version && t.id === id,
    ) ?? null;
  }

  private onGenerate(): void {
    if (!this.model || !this.selectedType) {
      this.setStatus("No type selected", true);
      return;
    }

    const result = generateRandom(
      this.model,
      this.selectedType.pkg,
      this.selectedType.version,
      this.selectedType.id,
    );

    if (result.success && result.json) {
      const model = this.jsonEditor.getModel();
      if (model) {
        model.setValue(result.json);
      }
      this.setStatus("Random instance generated");
      this.onEncode();
    } else {
      this.setStatus(`Generation failed: ${result.error}`, true);
    }
  }

  private async onEncode(): Promise<void> {
    if (!this.model || !this.selectedType) {
      this.setStatus("No type selected", true);
      return;
    }

    const jsonText = this.jsonEditor.getModel()?.getValue() ?? "";
    if (!jsonText.trim()) {
      this.setStatus("No JSON to encode", true);
      return;
    }

    try {
      const result = await encodeToUeba(
        this.model,
        this.selectedType.pkg,
        this.selectedType.version,
        this.selectedType.id,
        jsonText,
        this.indexedCheckbox.checked,
      );

      if (result.success && result.data) {
        this.uebaHexArea.value = bytesToHex(result.data);
        this.setStatus(`Encoded: ${result.data.length} bytes`);
      } else {
        this.setStatus(`Encode failed: ${result.error}`, true);
      }
    } catch (e) {
      this.setStatus(`Encode error: ${e instanceof Error ? e.message : String(e)}`, true);
    }
  }

  private async onDecode(): Promise<void> {
    if (!this.model || !this.selectedType) {
      this.setStatus("No type selected", true);
      return;
    }

    const hex = this.uebaHexArea.value.trim();
    if (!hex) {
      this.setStatus("No UEBA hex to decode", true);
      return;
    }

    try {
      const data = hexToBytes(hex);
      const result = await decodeFromUeba(
        this.model,
        this.selectedType.pkg,
        this.selectedType.version,
        this.selectedType.id,
        data,
      );

      if (result.success && result.json) {
        const formatted = JSON.stringify(JSON.parse(result.json), null, 2);
        const model = this.jsonEditor.getModel();
        if (model) {
          model.setValue(formatted);
        }
        this.setStatus("Decoded successfully");
      } else {
        this.setStatus(`Decode failed: ${result.error}`, true);
      }
    } catch (e) {
      this.setStatus(`Decode error: ${e instanceof Error ? e.message : String(e)}`, true);
    }
  }

  private setStatus(message: string, isError: boolean = false): void {
    this.statusEl.textContent = message;
    this.statusEl.classList.toggle("codec-status-error", isError);
  }
}

function bytesToHex(bytes: Uint8Array): string {
  return Array.from(bytes)
    .map((b) => b.toString(16).padStart(2, "0"))
    .join(" ");
}

function hexToBytes(hex: string): Uint8Array {
  const clean = hex.replace(/\s+/g, "");
  const bytes = new Uint8Array(clean.length / 2);
  for (let i = 0; i < bytes.length; i++) {
    bytes[i] = parseInt(clean.substring(i * 2, i * 2 + 2), 16);
  }
  return bytes;
}
