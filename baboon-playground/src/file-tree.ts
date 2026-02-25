interface TreeNode {
  name: string;
  fullPath: string;
  children: Map<string, TreeNode>;
  isFile: boolean;
}

export class FileTree {
  private container: HTMLElement;
  private root: TreeNode;
  private selectedPath: string | null = null;
  private onSelect: (path: string) => void;
  private expandedPaths: Set<string> = new Set();

  constructor(parent: HTMLElement, onSelect: (path: string) => void) {
    this.container = document.createElement("div");
    this.container.className = "file-tree";
    parent.appendChild(this.container);
    this.onSelect = onSelect;
    this.root = this.createNode("", "", false);
  }

  private createNode(name: string, fullPath: string, isFile: boolean): TreeNode {
    return { name, fullPath, children: new Map(), isFile };
  }

  setFiles(paths: string[]): void {
    this.root = this.createNode("", "", false);

    for (const path of paths.sort()) {
      const parts = path.split("/");
      let current = this.root;

      for (let i = 0; i < parts.length; i++) {
        const part = parts[i];
        const isLast = i === parts.length - 1;
        const fullPath = parts.slice(0, i + 1).join("/");

        if (!current.children.has(part)) {
          current.children.set(
            part,
            this.createNode(part, fullPath, isLast)
          );
          if (!isLast) {
            this.expandedPaths.add(fullPath);
          }
        }
        current = current.children.get(part)!;
      }
    }

    this.selectedPath = null;
    this.render();

    if (paths.length > 0) {
      const firstFile = this.findFirstFile(this.root);
      if (firstFile) {
        this.select(firstFile);
      }
    }
  }

  private findFirstFile(node: TreeNode): string | null {
    for (const child of node.children.values()) {
      if (child.isFile) return child.fullPath;
      const found = this.findFirstFile(child);
      if (found) return found;
    }
    return null;
  }

  select(path: string): void {
    this.selectedPath = path;
    this.render();
    this.onSelect(path);
  }

  private render(): void {
    this.container.innerHTML = "";
    this.renderNode(this.root, this.container, 0);
  }

  private renderNode(node: TreeNode, parent: HTMLElement, depth: number): void {
    const sortedChildren = [...node.children.values()].sort((a, b) => {
      if (a.isFile === b.isFile) return a.name.localeCompare(b.name);
      return a.isFile ? 1 : -1;
    });

    for (const child of sortedChildren) {
      const item = document.createElement("div");
      item.className = "file-tree-item";
      if (child.isFile && child.fullPath === this.selectedPath) {
        item.classList.add("selected");
      }
      item.style.paddingLeft = `${depth * 16 + 8}px`;

      if (child.isFile) {
        const icon = document.createElement("span");
        icon.className = "file-tree-icon file-icon";
        icon.textContent = "\u{1F4C4}";
        item.appendChild(icon);

        const label = document.createElement("span");
        label.textContent = child.name;
        item.appendChild(label);

        item.addEventListener("click", () => this.select(child.fullPath));
      } else {
        const expanded = this.expandedPaths.has(child.fullPath);

        const icon = document.createElement("span");
        icon.className = "file-tree-icon dir-icon";
        icon.textContent = expanded ? "\u25BE" : "\u25B8";
        item.appendChild(icon);

        const label = document.createElement("span");
        label.textContent = child.name;
        item.appendChild(label);

        item.addEventListener("click", () => {
          if (expanded) {
            this.expandedPaths.delete(child.fullPath);
          } else {
            this.expandedPaths.add(child.fullPath);
          }
          this.render();
        });
      }

      parent.appendChild(item);

      if (!child.isFile && this.expandedPaths.has(child.fullPath)) {
        this.renderNode(child, parent, depth + 1);
      }
    }
  }

  getElement(): HTMLElement {
    return this.container;
  }
}
