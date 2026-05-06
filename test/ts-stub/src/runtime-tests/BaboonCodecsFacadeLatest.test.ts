// Tests for the three new BaboonCodecsFacade methods:
//   preload(), decodeFromBinLatest(), decodeFromJsonLatest()
//
// Lives under src/runtime-tests/ so codegen (which writes into src/generated/ and
// src/baboontests/) does not overwrite it. Imports resolve against generated symbols written
// by the codegen step; run from target/test-regular/ts-stub/ or target/test-wrapped/ts-stub/.

import { describe, expect, test } from "vitest";

import {
    AbstractBaboonConversions,
    AbstractBaboonJsonCodecs,
    AbstractBaboonUebaCodecs,
    BaboonBinCodec,
    BaboonBinReader,
    BaboonBinWriter,
    BaboonCodecContext,
    BaboonDomainVersion,
    BaboonGenerated,
    BaboonGeneratedLatest,
    BaboonJsonCodec,
    BaboonMeta,
    BaboonConverterFailure,
    Lazy,
} from "../generated/BaboonSharedRuntime";

import { BaboonCodecsFacade } from "../generated/BaboonCodecsFacade";
import { Inner, Inner_JsonCodec, Inner_UEBACodec } from "../generated/my/ok/Inner";

// ===== Test scaffolding ================================================================

const DOMAIN_ID = "my.ok";
const VERSION_STR = "1.0.0";
const INNER_TYPE = Inner.BaboonTypeIdentifier;

class MyOkUebaCodecs extends AbstractBaboonUebaCodecs {
    constructor() {
        super();
        this.register(
            INNER_TYPE,
            new Lazy<BaboonBinCodec<unknown>>(() => Inner_UEBACodec.instance as BaboonBinCodec<unknown>),
        );
    }
}

class MyOkJsonCodecs extends AbstractBaboonJsonCodecs {
    constructor() {
        super();
        this.register(
            INNER_TYPE,
            new Lazy<BaboonJsonCodec<unknown>>(() => Inner_JsonCodec.instance as BaboonJsonCodec<unknown>),
        );
    }
}

class MyOkConversions implements AbstractBaboonConversions {
    public versionsFrom(): string[] { return []; }
    public versionTo(): string { return VERSION_STR; }
}

class MyOkMeta implements BaboonMeta {
    public sameInVersions(_typeId: string): string[] { return [VERSION_STR]; }
}

function freshFacade(): BaboonCodecsFacade {
    const facade = new BaboonCodecsFacade();
    const dv = new BaboonDomainVersion(DOMAIN_ID, VERSION_STR);
    facade.register(
        dv,
        () => new MyOkJsonCodecs(),
        () => new MyOkUebaCodecs(),
        () => new MyOkConversions(),
        () => new MyOkMeta(),
    );
    return facade;
}

const SAMPLE_INNER = new Inner(42);

function encodeInnerViaBin(facade: BaboonCodecsFacade): Uint8Array {
    const result = facade.encodeToBin(BaboonCodecContext.Compact, SAMPLE_INNER);
    if (result.tag !== "Right") throw new Error("encodeToBin failed: " + JSON.stringify(result));
    return result.value;
}

function encodeInnerViaJson(facade: BaboonCodecsFacade): Record<string, unknown> {
    const result = facade.encodeToJson(SAMPLE_INNER);
    if (result.tag !== "Right") throw new Error("encodeToJson failed: " + JSON.stringify(result));
    return result.value;
}

// ===== preload =========================================================================

describe("preload", () => {
    test("returns without throwing and does not block the caller", () => {
        const facade = freshFacade();
        // preload() must return synchronously; any errors inside the async thunk are swallowed.
        expect(() => facade.preload()).not.toThrow();
    });

    test("subsequent codec operations still work after preload", async () => {
        const facade = freshFacade();
        facade.preload();
        // Yield one microtask so the Promise.resolve().then() inside preload has run.
        await Promise.resolve();
        // The facade must remain usable after preload.
        const bytes = encodeInnerViaBin(facade);
        expect(bytes.length).toBeGreaterThan(0);
    });
});

// ===== decodeFromBinLatest =============================================================

describe("decodeFromBinLatest", () => {
    test("composition with convert propagates BaboonConverterFailure (convert is a stub)", () => {
        // TS convert() is an unimplemented stub that returns BaboonConverterFailure.
        // decodeFromBinLatest must compose cleanly: decode succeeds, convert fails, the
        // failure propagates as Left.
        const facade = freshFacade();
        const bytes = encodeInnerViaBin(facade);
        const result = facade.decodeFromBinLatest<BaboonGeneratedLatest>(bytes, INNER_TYPE);
        expect(result.tag).toBe("Left");
        if (result.tag !== "Left") return;
        expect(result.value).toBeInstanceOf(BaboonConverterFailure);
    });

    test("returns Left for truncated bytes (decode step fails before convert)", () => {
        const facade = freshFacade();
        const bytes = encodeInnerViaBin(facade);
        // Truncate to 2 bytes — meta decode will fail.
        const truncated = bytes.slice(0, 2);
        const result = facade.decodeFromBinLatest<BaboonGeneratedLatest>(truncated, INNER_TYPE);
        expect(result.tag).toBe("Left");
    });

    test("returns Left for empty bytes", () => {
        const facade = freshFacade();
        const result = facade.decodeFromBinLatest<BaboonGeneratedLatest>(
            new Uint8Array(0),
            INNER_TYPE,
        );
        expect(result.tag).toBe("Left");
    });
});

// ===== decodeFromJsonLatest ============================================================

describe("decodeFromJsonLatest", () => {
    test("composition with convert propagates BaboonConverterFailure (convert is a stub)", () => {
        // Same reasoning as decodeFromBinLatest: JSON decode succeeds, convert stub fails.
        const facade = freshFacade();
        const envelope = encodeInnerViaJson(facade);
        const result = facade.decodeFromJsonLatest<BaboonGeneratedLatest>(envelope, INNER_TYPE);
        expect(result.tag).toBe("Left");
        if (result.tag !== "Left") return;
        expect(result.value).toBeInstanceOf(BaboonConverterFailure);
    });

    test("returns Left for a non-envelope object (not a Baboon meta object)", () => {
        const facade = freshFacade();
        const result = facade.decodeFromJsonLatest<BaboonGeneratedLatest>(
            { some: "object" },
            INNER_TYPE,
        );
        // decodeFromJson returns Left for unrecognised envelopes; decodeFromJsonLatest propagates.
        expect(result.tag).toBe("Left");
    });

    test("returns Left for a JSON string that parses but is not a Baboon envelope", () => {
        const facade = freshFacade();
        const result = facade.decodeFromJsonLatest<BaboonGeneratedLatest>(
            '{"foo":"bar"}',
            INNER_TYPE,
        );
        expect(result.tag).toBe("Left");
    });
});
