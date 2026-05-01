import {describe, test, expect} from "vitest";
import * as fs from "fs";
import * as path from "path";
import {fileURLToPath} from "url";
import {ForeignKeyHolder} from "../src/generated/convtest/m24foreign/ForeignKeyHolder";
import {ItemKey} from "../src/generated/convtest/m24foreign/ItemKey";
import {BaboonCodecContext} from "../src/generated/BaboonSharedRuntime";

// -----------------------------------------------------------------------------
// PR-I.1d (M24 Phase 3.1) — Custom-foreign `<Foreign>_KeyCodec` extension hook
// (TypeScript mirror).
//
// Mirrors the Scala reference (Test_CrossLanguageCompat.scala), Java mirror
// (CrossLanguageTest.java), C# mirror (Test_CrossLanguageCompat.cs), and Dart
// mirror (m24_foreign_keycodec_test.dart): round-trip the TS-emitted
// m24-foreign-keycodec.json through ForeignKeyHolder_JsonCodec and assert
// byte-identity (PR-I-D02 pattern guidance) of the encoded JSON string against
// the canonical wire form `{"m":{"alpha":"v1","beta":"v2"}}`.
// -----------------------------------------------------------------------------

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const compatDir = path.resolve(__dirname, "../../../target/compat-test");
const ctx = BaboonCodecContext.Default;

describe("PR-I.1d Custom-foreign KeyCodec hook (TypeScript)", () => {
    test("m24ForeignKeyCodecRoundTripTypeScript", () => {
        const file = path.join(compatDir, "typescript-json", "m24-foreign-keycodec.json");
        expect(fs.existsSync(file)).toBe(true);
        const jsonStr = fs.readFileSync(file, "utf-8");
        const json = JSON.parse(jsonStr);
        const decoded = ForeignKeyHolder.jsonCodec().decode(ctx, json);
        // Map equality requires same key references — so compare entries reflectively.
        const decodedEntries = Array.from(decoded.m.entries()).map(([k, v]) => [k.v, v]);
        expect(decodedEntries).toEqual([
            ["alpha", "v1"],
            ["beta", "v2"],
        ]);
    });

    test("m24ForeignKeyCodecCanonicalWireForm", () => {
        const sample = new ForeignKeyHolder(new Map<ItemKey, string>([
            [new ItemKey("alpha"), "v1"],
            [new ItemKey("beta"), "v2"],
        ]));
        const encoded = ForeignKeyHolder.jsonCodec().encode(ctx, sample);
        const actual = JSON.stringify(encoded);
        const expected = '{"m":{"alpha":"v1","beta":"v2"}}';
        expect(actual).toBe(expected);
    });
});
