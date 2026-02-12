import {describe, test, expect} from "vitest";
import * as fs from "fs";
import * as path from "path";
import {fileURLToPath} from "url";
import {AllBasicTypes} from "../src/generated/convtest/testpkg/AllBasicTypes";
import {BaboonCodecContext} from "../src/generated/BaboonSharedRuntime";


const __dirname = path.dirname(fileURLToPath(import.meta.url));
const compatDir = path.resolve(__dirname, "../../../target/compat-test");

const languages = ["scala", "cs", "rust", "python", "typescript", "kotlin", "java", "dart", "swift"];

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
            const decoded = AllBasicTypes.jsonCodec().decode(BaboonCodecContext.Default, json);
            const reEncoded = AllBasicTypes.jsonCodec().encode(BaboonCodecContext.Default, decoded);
            const reDecoded = AllBasicTypes.jsonCodec().decode(BaboonCodecContext.Default, reEncoded);
            expect(reDecoded).toStrictEqual(decoded);
        });
    }
});
