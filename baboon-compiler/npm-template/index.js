import { createHash } from "crypto";

import { BaboonCompiler } from "./main.js";

class NodeSha256 {
  constructor() {
    if (typeof Buffer === "undefined") {
      throw new Error("Buffer is not available; provide a global sha256 implementation.");
    }
    this.hash = createHash("sha256");
  }

  update(data) {
    this.hash.update(Buffer.from(data));
    return this;
  }

  digest(encoding) {
    return this.hash.digest(encoding);
  }
}

if (typeof globalThis.sha256 === "undefined") {
  globalThis.sha256 = NodeSha256;
}

export { BaboonCompiler };
export default BaboonCompiler;
