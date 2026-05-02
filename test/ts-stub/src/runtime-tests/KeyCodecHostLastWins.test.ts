// PR-26.2 (M26) — KeyCodec Host last-wins concurrency contract regression test.
//
// Asserts FStr_KeyCodecHost.register(impl) overwrites the previously registered
// impl (last-wins). TypeScript already used a module-level `let` mutable
// singleton pre-PR-26.2; this test pins that behavior across future refactors.
//
// Generated symbols are produced by mdl :test-gen-regular-adt under
// target/test-regular/ts-stub/.

import { afterEach, describe, expect, test } from "vitest";

import { BaboonCodecContext } from "../generated/BaboonSharedRuntime";
import { FStr_KeyCodecHost, type FStr_KeyCodec } from "../generated/my/ok/m19/foreign/FStr";
import { Holder, Holder_JsonCodec } from "../generated/my/ok/m19/foreign/Holder";
import { ItemKey } from "../generated/my/ok/m19/foreign/ItemKey";

const prefixCodec = (tag: string): FStr_KeyCodec => ({
    encodeKey: (v: string) => `${tag}:${v}`,
    decodeKey: (s: string) => s.startsWith(`${tag}:`) ? s.substring(tag.length + 1) : s,
});

const identityCodec: FStr_KeyCodec = {
    encodeKey: (v: string) => v,
    decodeKey: (s: string) => s,
};

describe("PR-26.2 — KeyCodec Host last-wins", () => {
    // PR-26.2-D01: restore identity impl after each test so the module-level
    // FStr_KeyCodecHost singleton does not leak a PrefixCodec into sibling
    // tests sharing the worker. Runs even on assertion failure.
    afterEach(() => {
        FStr_KeyCodecHost.register(identityCodec);
    });

    test("register(B) after register(A) → encode observes B (NOT A)", () => {
        const ctx = BaboonCodecContext.Compact;
        const original = new Holder(new Map<ItemKey, string>([[new ItemKey("k"), "v"]]));

        FStr_KeyCodecHost.register(prefixCodec("A"));
        const encodedA = JSON.stringify(Holder_JsonCodec.instance.encode(ctx, original));
        expect(encodedA).toContain("A:k");

        FStr_KeyCodecHost.register(prefixCodec("B"));
        const encodedB = JSON.stringify(Holder_JsonCodec.instance.encode(ctx, original));
        expect(encodedB).toContain("B:k");
        expect(encodedB).not.toContain("A:k");
    });
});
