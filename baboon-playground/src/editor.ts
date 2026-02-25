import * as monaco from "monaco-editor";
import { BABOON_LANGUAGE_ID } from "./baboon-language.ts";

const DEFAULT_FILES: Record<string, string> = {
  "models/pkg/shared.bmo": `data Address {
  street: str
  city: str
  zip: str
  country: str
}

data Timestamp {
  created: tsu
  updated: tsu
}
`,

  "models/pkg/v1.baboon": `model example.playground
version "1.0.0"

include "shared.bmo"

enum Status {
  Active = 1
  Inactive = 2
}

root data User : derived[json] {
  id: uid
  name: str
  email: str
  address: Address
  timestamps: Timestamp
  status: Status
  tags: lst[str]
}

root adt PaymentMethod {
  data CreditCard {
    pan: str
    holder: str
    expiry: str
  }
  data BankTransfer {
    iban: str
    bic: str
  }
}

root data AppConfig {
  appName: str
  debug: bit
}
`,

  "models/pkg/v2.baboon": `model example.playground
version "2.0.0"

include "shared.bmo"

// Import all types from v1; redefined types below override imported ones,
// types not redefined (e.g. AppConfig) are preserved unmodified.
import "1.0.0" { * }

// Evolved types are just redefined in the new version
enum Status {
  Active = 1
  Inactive = 2
  Suspended = 3
}

root data User : derived[json] {
  id: uid
  name: str
  email: str
  address: Address
  timestamps: Timestamp
  status: Status
  phone: opt[str]
  labels: set[str]
}

root adt PaymentMethod {
  data CreditCard {
    pan: str
    holder: str
    expiry: str
  }
  data BankTransfer {
    iban: str
    bic: str
  }
  data Wallet {
    provider: str
    token: str
  }
}
`,
};

interface SourceFile {
  path: string;
  model: monaco.editor.ITextModel;
}

interface TreeNode {
  name: string;
  fullPath: string;
  children: Map<string, TreeNode>;
  isFile: boolean;
}

export class BaboonEditor {
  private container: HTMLElement;
  private sidebar: HTMLElement;
  private treeContainer: HTMLElement;
  private editorContainer: HTMLElement;
  private editor: monaco.editor.IStandaloneCodeEditor;
  private files: Map<string, SourceFile> = new Map();
  private selectedPath: string | null = null;
  private expandedPaths: Set<string> = new Set();

  constructor(parent: HTMLElement) {
    this.container = document.createElement("div");
    this.container.className = "editor-panel";
    parent.appendChild(this.container);

    this.sidebar = document.createElement("div");
    this.sidebar.className = "source-sidebar";
    this.container.appendChild(this.sidebar);

    const toolbarEl = document.createElement("div");
    toolbarEl.className = "source-toolbar";
    this.sidebar.appendChild(toolbarEl);

    const addFileBtn = document.createElement("button");
    addFileBtn.className = "source-toolbar-btn";
    addFileBtn.textContent = "+ File";
    addFileBtn.title = "New file";
    addFileBtn.addEventListener("click", () => this.promptNewFile());
    toolbarEl.appendChild(addFileBtn);

    const addDirBtn = document.createElement("button");
    addDirBtn.className = "source-toolbar-btn";
    addDirBtn.title = "New folder";
    addDirBtn.textContent = "+ Folder";
    addDirBtn.addEventListener("click", () => this.promptNewFolder());
    toolbarEl.appendChild(addDirBtn);

    this.treeContainer = document.createElement("div");
    this.treeContainer.className = "source-tree";
    this.sidebar.appendChild(this.treeContainer);

    this.editorContainer = document.createElement("div");
    this.editorContainer.className = "editor-container";
    this.container.appendChild(this.editorContainer);

    this.editor = monaco.editor.create(this.editorContainer, {
      language: BABOON_LANGUAGE_ID,
      theme: "vs-dark",
      automaticLayout: true,
      minimap: { enabled: false },
      fontSize: 14,
      tabSize: 2,
      insertSpaces: true,
      scrollBeyondLastLine: false,
    });

    for (const [path, content] of Object.entries(DEFAULT_FILES)) {
      this.addFile(path, content);
    }

    const firstPath = Object.keys(DEFAULT_FILES)[0];
    if (firstPath) {
      this.selectFile(firstPath);
    }
  }

  private addFile(path: string, content: string): void {
    const model = monaco.editor.createModel(content, BABOON_LANGUAGE_ID);
    this.files.set(path, { path, model });
    this.expandParents(path);
    this.renderTree();
  }

  private expandParents(path: string): void {
    const parts = path.split("/");
    for (let i = 1; i < parts.length; i++) {
      this.expandedPaths.add(parts.slice(0, i).join("/"));
    }
  }

  private selectFile(path: string): void {
    const file = this.files.get(path);
    if (!file) return;
    this.selectedPath = path;
    this.editor.setModel(file.model);
    this.renderTree();
  }

  private removeFile(path: string): void {
    const file = this.files.get(path);
    if (!file) return;
    file.model.dispose();
    this.files.delete(path);

    if (this.selectedPath === path) {
      const remaining = [...this.files.keys()];
      if (remaining.length > 0) {
        this.selectFile(remaining[0]);
      } else {
        this.selectedPath = null;
        this.editor.setModel(null);
      }
    }
    this.renderTree();
  }

  private removeFolder(folderPath: string): void {
    const prefix = folderPath + "/";
    const toRemove = [...this.files.keys()].filter(
      (p) => p.startsWith(prefix) || p === folderPath
    );
    for (const p of toRemove) {
      const file = this.files.get(p);
      if (file) {
        file.model.dispose();
        this.files.delete(p);
      }
    }
    this.expandedPaths.delete(folderPath);

    if (this.selectedPath && toRemove.includes(this.selectedPath)) {
      const remaining = [...this.files.keys()];
      if (remaining.length > 0) {
        this.selectFile(remaining[0]);
      } else {
        this.selectedPath = null;
        this.editor.setModel(null);
      }
    }
    this.renderTree();
  }

  private renameFile(oldPath: string, newPath: string): void {
    const file = this.files.get(oldPath);
    if (!file || this.files.has(newPath)) return;
    this.files.delete(oldPath);
    file.path = newPath;
    this.files.set(newPath, file);
    if (this.selectedPath === oldPath) {
      this.selectedPath = newPath;
    }
    this.expandParents(newPath);
    this.renderTree();
  }

  private promptNewFile(): void {
    const base = this.selectedPath
      ? this.selectedPath.includes("/")
        ? this.selectedPath.substring(0, this.selectedPath.lastIndexOf("/"))
        : ""
      : "models";
    const path = prompt("File path:", base ? `${base}/new.baboon` : "new.baboon");
    if (!path || this.files.has(path)) return;
    this.addFile(path, "");
    this.selectFile(path);
  }

  private promptNewFolder(): void {
    const base = this.selectedPath
      ? this.selectedPath.includes("/")
        ? this.selectedPath.substring(0, this.selectedPath.lastIndexOf("/"))
        : ""
      : "models";
    const folderPath = prompt("Folder path:", base ? `${base}/new` : "new");
    if (!folderPath) return;
    const placeholder = `${folderPath}/.gitkeep`;
    if (!this.files.has(placeholder)) {
      this.addFile(placeholder, "");
      this.expandedPaths.add(folderPath);
      this.renderTree();
    }
  }

  private buildTree(): TreeNode {
    const root: TreeNode = {
      name: "",
      fullPath: "",
      children: new Map(),
      isFile: false,
    };

    for (const path of [...this.files.keys()].sort()) {
      const parts = path.split("/");
      let current = root;
      for (let i = 0; i < parts.length; i++) {
        const part = parts[i];
        const isLast = i === parts.length - 1;
        const fullPath = parts.slice(0, i + 1).join("/");
        if (!current.children.has(part)) {
          current.children.set(part, {
            name: part,
            fullPath,
            children: new Map(),
            isFile: isLast,
          });
        }
        current = current.children.get(part)!;
      }
    }

    return root;
  }

  private renderTree(): void {
    this.treeContainer.innerHTML = "";
    const root = this.buildTree();
    this.renderNode(root, this.treeContainer, 0);
  }

  private renderNode(
    node: TreeNode,
    parent: HTMLElement,
    depth: number
  ): void {
    const sorted = [...node.children.values()].sort((a, b) => {
      if (a.isFile === b.isFile) return a.name.localeCompare(b.name);
      return a.isFile ? 1 : -1;
    });

    for (const child of sorted) {
      if (child.name === ".gitkeep") continue;

      const item = document.createElement("div");
      item.className = "file-tree-item";
      if (child.isFile && child.fullPath === this.selectedPath) {
        item.classList.add("selected");
      }
      item.style.paddingLeft = `${depth * 16 + 8}px`;

      if (child.isFile) {
        const icon = document.createElement("span");
        icon.className = "file-tree-icon file-icon";
        icon.textContent = child.name.endsWith(".bmo") ? "B" : "b";
        item.appendChild(icon);

        const label = document.createElement("span");
        label.className = "source-tree-label";
        label.textContent = child.name;
        item.appendChild(label);

        const actions = document.createElement("span");
        actions.className = "source-tree-actions";

        const renameBtn = document.createElement("span");
        renameBtn.className = "source-tree-action";
        renameBtn.textContent = "\u270E";
        renameBtn.title = "Rename";
        renameBtn.addEventListener("click", (e) => {
          e.stopPropagation();
          const newPath = prompt("New path:", child.fullPath);
          if (newPath && newPath !== child.fullPath) {
            this.renameFile(child.fullPath, newPath);
          }
        });
        actions.appendChild(renameBtn);

        const deleteBtn = document.createElement("span");
        deleteBtn.className = "source-tree-action";
        deleteBtn.textContent = "\u2715";
        deleteBtn.title = "Delete";
        deleteBtn.addEventListener("click", (e) => {
          e.stopPropagation();
          this.removeFile(child.fullPath);
        });
        actions.appendChild(deleteBtn);

        item.appendChild(actions);
        item.addEventListener("click", () => this.selectFile(child.fullPath));
      } else {
        const expanded = this.expandedPaths.has(child.fullPath);

        const icon = document.createElement("span");
        icon.className = "file-tree-icon dir-icon";
        icon.textContent = expanded ? "\u25BE" : "\u25B8";
        item.appendChild(icon);

        const label = document.createElement("span");
        label.className = "source-tree-label";
        label.textContent = child.name;
        item.appendChild(label);

        const actions = document.createElement("span");
        actions.className = "source-tree-actions";

        const deleteBtn = document.createElement("span");
        deleteBtn.className = "source-tree-action";
        deleteBtn.textContent = "\u2715";
        deleteBtn.title = "Delete folder";
        deleteBtn.addEventListener("click", (e) => {
          e.stopPropagation();
          this.removeFolder(child.fullPath);
        });
        actions.appendChild(deleteBtn);

        item.appendChild(actions);

        item.addEventListener("click", () => {
          if (expanded) {
            this.expandedPaths.delete(child.fullPath);
          } else {
            this.expandedPaths.add(child.fullPath);
          }
          this.renderTree();
        });
      }

      parent.appendChild(item);

      if (!child.isFile && this.expandedPaths.has(child.fullPath)) {
        this.renderNode(child, parent, depth + 1);
      }
    }
  }

  setFiles(files: Map<string, string>): void {
    for (const file of this.files.values()) {
      file.model.dispose();
    }
    this.files.clear();
    this.expandedPaths.clear();
    this.selectedPath = null;
    this.editor.setModel(null);

    for (const [path, content] of files) {
      this.addFile(path, content);
    }

    const firstPath = [...files.keys()][0];
    if (firstPath) {
      this.selectFile(firstPath);
    }
  }

  getFiles(): Map<string, string> {
    const result = new Map<string, string>();
    for (const [path, file] of this.files) {
      if (path.endsWith(".gitkeep")) continue;
      result.set(path, file.model.getValue());
    }
    return result;
  }

  layout(): void {
    this.editor.layout();
  }

  dispose(): void {
    for (const file of this.files.values()) {
      file.model.dispose();
    }
    this.editor.dispose();
  }
}
