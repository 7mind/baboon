import { describe, test, expect } from "vitest";
import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "url";

import { decode_AllBasicTypes_json, encode_AllBasicTypes_json } from "../src/generated/convtest/testpkg/all-basic-types.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const compatDir = path.resolve(__dirname, "../../../target/compat-test");

const languages = ["scala", "cs", "rust", "python", "typescript", "kotlin", "java", "dart"];

describe("Cross-language JSON compatibility", () => {
    for (const lang of languages) {
        const jsonPath = path.join(compatDir, `${lang}-json`, "all-basic-types.json");

        test(`decode ${lang} JSON`, () => {
            if (!fs.existsSync(jsonPath)) {
                console.warn(`Skipping ${lang}: ${jsonPath} not found`);
                return;
            }

            const data = fs.readFileSync(jsonPath, "utf-8");
            const json = JSON.parse(data);
            const decoded = decode_AllBasicTypes_json(json);
            const reEncoded = encode_AllBasicTypes_json(decoded);
            const reDecoded = decode_AllBasicTypes_json(reEncoded);
            expect(reDecoded).toStrictEqual(decoded);
        });
    }
});
