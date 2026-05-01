// PR-F (M24) — cross-language malformed map-key error consistency.
//
// Verifies that decoding a JSON object whose map-key cannot be parsed back into the
// id type throws BaboonDecoderFailure with a message containing "malformed key".
// Replaces the prior unchecked `as unknown as { tag: "Right" }` cast.
//
// Uses the my.ok.m19.singleid fixture (id ItemId { v: uid }; root data Holder { m: map[ItemId, str] }).
// Generated symbols are produced by mdl :test-gen-regular-adt under target/test-regular/ts-stub/.

import { describe, expect, test } from "vitest";

import { BaboonCodecContext, BaboonDecoderFailure } from "../generated/BaboonSharedRuntime";
import { Holder_JsonCodec } from "../generated/my/ok/m19/singleid/Holder";

describe("PR-F (M24) — malformed map-key throws BaboonDecoderFailure", () => {
    test("Holder JSON decode throws BaboonDecoderFailure for malformed map key", () => {
        const badJson = { m: { not_a_valid_id: "v" } };
        const ctx = BaboonCodecContext.Compact;
        expect(() => Holder_JsonCodec.instance.decode(ctx, badJson))
            .toThrow(BaboonDecoderFailure);
        try {
            Holder_JsonCodec.instance.decode(ctx, badJson);
        } catch (e) {
            expect((e as Error).message).toContain("malformed key");
        }
    });
});
