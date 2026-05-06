// Tests for the BaboonExt helper functions (domainVersion, baboonUnmodifiedSinceVersion,
// unmodifiedSinceVersion) exported from BaboonSharedRuntime.
//
// Mirrors C# `BaboonExt` / Rust `BaboonExt` trait helpers. Lives under `src/runtime-tests/`
// so codegen does not overwrite it (codegen writes into `src/generated/` and `src/baboontests/`).

import { describe, expect, test } from "vitest";

import {
    BaboonDomainVersion,
    BaboonException,
    BaboonGenerated,
    BaboonMeta,
    baboonUnmodifiedSinceVersion,
    domainVersion,
    unmodifiedSinceVersion,
} from "../generated/BaboonSharedRuntime";

// ---------------------------------------------------------------------------
// Fixture: minimal BaboonGenerated implementation
// ---------------------------------------------------------------------------

class FakeInner implements BaboonGenerated {
    baboonDomainVersion(): string { return "1.0.0"; }
    baboonDomainIdentifier(): string { return "my.ok"; }
    baboonSameInVersions(): string[] { return ["1.0.0"]; }
    baboonTypeIdentifier(): string { return "my.ok/:#Inner"; }
}

class FakeEvolved implements BaboonGenerated {
    baboonDomainVersion(): string { return "3.0.0"; }
    baboonDomainIdentifier(): string { return "my.ok"; }
    // Stable since 1.0.0 despite domain now being at 3.0.0.
    baboonSameInVersions(): string[] { return ["1.0.0", "2.0.0", "3.0.0"]; }
    baboonTypeIdentifier(): string { return "my.ok/:#Evolved"; }
}

class FakeEmptySameIn implements BaboonGenerated {
    baboonDomainVersion(): string { return "1.0.0"; }
    baboonDomainIdentifier(): string { return "my.ok"; }
    baboonSameInVersions(): string[] { return []; }
    baboonTypeIdentifier(): string { return "my.ok/:#Empty"; }
}

// ---------------------------------------------------------------------------
// Fixture: minimal BaboonMeta implementation
// ---------------------------------------------------------------------------

class FakeMeta implements BaboonMeta {
    sameInVersions(typeId: string): string[] {
        const table: Record<string, string[]> = {
            "my.ok/:#Inner": ["1.0.0"],
            "my.ok/:#Evolved": ["1.0.0", "2.0.0", "3.0.0"],
        };
        return table[typeId] ?? [];
    }
}

// ---------------------------------------------------------------------------
// domainVersion
// ---------------------------------------------------------------------------

describe("domainVersion", () => {
    test("returns BaboonDomainVersion with correct identifier and version", () => {
        const dv: BaboonDomainVersion = domainVersion(new FakeInner());
        expect(dv.domainIdentifier).toBe("my.ok");
        expect(dv.domainVersion).toBe("1.0.0");
    });

    test("returns current domain version for evolved type", () => {
        const dv: BaboonDomainVersion = domainVersion(new FakeEvolved());
        expect(dv.domainIdentifier).toBe("my.ok");
        expect(dv.domainVersion).toBe("3.0.0");
    });
});

// ---------------------------------------------------------------------------
// baboonUnmodifiedSinceVersion
// ---------------------------------------------------------------------------

describe("baboonUnmodifiedSinceVersion", () => {
    test("returns first entry of baboonSameInVersions", () => {
        expect(baboonUnmodifiedSinceVersion(new FakeInner())).toBe("1.0.0");
    });

    test("returns oldest stable version for evolved type", () => {
        expect(baboonUnmodifiedSinceVersion(new FakeEvolved())).toBe("1.0.0");
    });

    test("throws BaboonException when baboonSameInVersions is empty", () => {
        expect(() => baboonUnmodifiedSinceVersion(new FakeEmptySameIn())).toThrow(BaboonException);
    });
});

// ---------------------------------------------------------------------------
// unmodifiedSinceVersion
// ---------------------------------------------------------------------------

describe("unmodifiedSinceVersion", () => {
    test("looks up first same-in-versions entry for given typeId", () => {
        expect(unmodifiedSinceVersion(new FakeMeta(), "my.ok/:#Inner")).toBe("1.0.0");
    });

    test("returns oldest stable version for evolved type by typeId", () => {
        expect(unmodifiedSinceVersion(new FakeMeta(), "my.ok/:#Evolved")).toBe("1.0.0");
    });

    test("throws BaboonException for unknown typeId with empty result", () => {
        expect(() => unmodifiedSinceVersion(new FakeMeta(), "my.ok/:#Unknown")).toThrow(BaboonException);
    });
});
