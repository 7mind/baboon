import * as monaco from "monaco-editor";
import { FileTree } from "./file-tree.ts";
import {
  type BaboonTargetLanguage,
  type CompilationResult,
  type OutputFile,
  TARGET_LANGUAGES,
  LANGUAGE_DISPLAY_NAMES,
  LANGUAGE_TO_MONACO,
} from "./compiler.ts";

export class Preview {
  private container: HTMLElement;
  private toolbar: HTMLElement;
  private languageSelect: HTMLSelectElement;
  private contentPanel: HTMLElement;
  private fileTree: FileTree;
  private editorContainer: HTMLElement;
  private editor: monaco.editor.IStandaloneCodeEditor;
  private errorPanel: HTMLElement;

  private result: CompilationResult | null = null;
  private selectedLanguage: BaboonTargetLanguage = "typescript";
  private filesByPath: Map<string, OutputFile> = new Map();

  constructor(parent: HTMLElement) {
    this.container = document.createElement("div");
    this.container.className = "preview-panel";
    parent.appendChild(this.container);

    this.toolbar = document.createElement("div");
    this.toolbar.className = "preview-toolbar";
    this.container.appendChild(this.toolbar);

    const langLabel = document.createElement("label");
    langLabel.textContent = "Language: ";
    langLabel.className = "preview-label";
    this.toolbar.appendChild(langLabel);

    this.languageSelect = document.createElement("select");
    this.languageSelect.className = "language-select";
    for (const lang of TARGET_LANGUAGES) {
      const option = document.createElement("option");
      option.value = lang;
      option.textContent = LANGUAGE_DISPLAY_NAMES[lang];
      if (lang === this.selectedLanguage) {
        option.selected = true;
      }
      this.languageSelect.appendChild(option);
    }
    this.languageSelect.addEventListener("change", () => {
      this.selectedLanguage = this.languageSelect.value as BaboonTargetLanguage;
      this.renderCurrentLanguage();
    });
    this.toolbar.appendChild(this.languageSelect);

    this.errorPanel = document.createElement("div");
    this.errorPanel.className = "error-panel";
    this.errorPanel.style.display = "none";
    this.container.appendChild(this.errorPanel);

    this.contentPanel = document.createElement("div");
    this.contentPanel.className = "preview-content";
    this.container.appendChild(this.contentPanel);

    const treeContainer = document.createElement("div");
    treeContainer.className = "preview-tree-container";
    this.contentPanel.appendChild(treeContainer);

    this.fileTree = new FileTree(treeContainer, (path) => {
      this.showFile(path);
    });

    this.editorContainer = document.createElement("div");
    this.editorContainer.className = "preview-editor-container";
    this.contentPanel.appendChild(this.editorContainer);

    this.editor = monaco.editor.create(this.editorContainer, {
      readOnly: true,
      theme: "vs-dark",
      automaticLayout: true,
      minimap: { enabled: false },
      fontSize: 14,
      tabSize: 2,
      scrollBeyondLastLine: false,
      wordWrap: "on",
    });

    this.showPlaceholder();
  }

  setResult(result: CompilationResult): void {
    this.result = result;

    if (!result.success) {
      this.showErrors(result.errors);
      return;
    }

    this.errorPanel.style.display = "none";
    this.contentPanel.style.display = "flex";
    this.renderCurrentLanguage();
  }

  private renderCurrentLanguage(): void {
    if (!this.result) return;

    const files = this.result.filesByLanguage.get(this.selectedLanguage) ?? [];
    this.filesByPath = new Map();
    for (const file of files) {
      this.filesByPath.set(file.path, file);
    }

    const paths = files.map((f) => f.path);
    this.fileTree.setFiles(paths);

    if (paths.length === 0) {
      this.showPlaceholder();
    }
  }

  private showFile(path: string): void {
    const file = this.filesByPath.get(path);
    if (!file) return;

    const monacoLang = LANGUAGE_TO_MONACO[this.selectedLanguage];
    const model = this.editor.getModel();
    if (model) {
      monaco.editor.setModelLanguage(model, monacoLang);
      model.setValue(file.content);
    } else {
      const newModel = monaco.editor.createModel(file.content, monacoLang);
      this.editor.setModel(newModel);
    }
  }

  private showErrors(errors: string[]): void {
    this.contentPanel.style.display = "none";
    this.errorPanel.style.display = "block";
    this.errorPanel.innerHTML = "";

    const heading = document.createElement("div");
    heading.className = "error-heading";
    heading.textContent = "Compilation Errors";
    this.errorPanel.appendChild(heading);

    for (const error of errors) {
      const errorEl = document.createElement("pre");
      errorEl.className = "error-message";
      errorEl.textContent = error;
      this.errorPanel.appendChild(errorEl);
    }
  }

  private showPlaceholder(): void {
    const model = this.editor.getModel();
    if (model) {
      monaco.editor.setModelLanguage(model, "plaintext");
      model.setValue("// Click 'Compile' to generate output");
    }
  }

  layout(): void {
    this.editor.layout();
  }

  dispose(): void {
    this.editor.dispose();
  }
}
