// index.d.ts declares exactly the methods BaboonCompiler exports (mdl action `test-js-npm`; not
// published). Run against the fastLinkJS bundle: there, Scala.js keeps internal members under
// mangled names (`name__Signature`), whereas fullLinkJS minifies them into names indistinguishable
// from exports.
import test from "ava";
import { readFileSync } from "fs";

import { BaboonCompiler } from "./index.js";

test("index.d.ts declares exactly the methods BaboonCompiler exports", t => {
  const source = readFileSync(new URL("./index.d.ts", import.meta.url), "utf8");
  const body = /export interface BaboonCompilerAPI \{([\s\S]*?)\n\}/.exec(source);
  t.truthy(body, "index.d.ts declares no BaboonCompilerAPI interface");
  const declared = [...body[1].matchAll(/^\s{2}(\w+)\(/gm)].map(m => m[1]).sort();
  const exported = Object.getOwnPropertyNames(Object.getPrototypeOf(BaboonCompiler))
    .filter(name => name !== "constructor" && /^[a-z][A-Za-z0-9]*$/.test(name) && typeof BaboonCompiler[name] === "function")
    .sort();
  t.deepEqual(exported, declared);
});
