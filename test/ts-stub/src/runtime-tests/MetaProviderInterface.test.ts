// Regression for PR-25.8 / PR-22-D02. The TS runtime declares two structural surfaces consumed
// by `BaboonTypeMeta.from`:
//   - `BaboonGenerated`     — every generated DTO/codec target (4 meta methods)
//   - `BaboonAdtMemberMeta` — additional method on ADT branches (`baboonAdtTypeIdentifier()`)
//
// Pre-PR-25.8, `TsDefnTranslator` did NOT add `implements BaboonAdtMemberMeta` to ADT-branch DTO
// emission, AND the runtime interface declared the field as a `string` property whereas codegen
// emits a method. `BaboonTypeMeta.from(value, useAdtIdentifier=true)` therefore failed silently
// (typeguard rejected every generated branch) and fell back to the concrete-branch type
// identifier — wrong wire envelope, no compile error.
//
// The two assertions below are structural: assigning a generated value to a variable of the
// runtime interface type forces TypeScript to verify the implements relationship at compile time.
// Pre-fix: the ADT-branch assignment fails to compile (`baboonAdtTypeIdentifier` either missing
// or shape-mismatched); the property-style runtime declaration also rejects the method emission.

import { describe, expect, test } from "vitest";

import {
    BaboonAdtMemberMeta,
    BaboonGenerated,
    BaboonTypeMeta,
} from "../generated/BaboonSharedRuntime";

import { Inner } from "../generated/my/ok/Inner";
// `T4_A1` is the ADT defined in baboon-fixtures `testpkg.pkg0` v3 (latest). Its branches are
// emitted inline inside the ADT's TS module file rather than in separate files.
import { B1 } from "../generated/testpkg/pkg0/T4_A1";

describe("PR-25.8 generated DTO/ADT branch structural meta surface", () => {
    test("generated DTO satisfies BaboonGenerated structurally", () => {
        const dto = new Inner(42);

        // Direct interface assignment — TS verifies the implements relationship at compile time.
        const provider: BaboonGenerated = dto;

        expect(provider.baboonTypeIdentifier()).toEqual(Inner.BaboonTypeIdentifier);
        expect(provider.baboonDomainIdentifier()).toEqual(Inner.BaboonDomainIdentifier);
        expect(provider.baboonDomainVersion()).toEqual(Inner.BaboonDomainVersion);
        expect(Array.isArray(provider.baboonSameInVersions())).toBe(true);
        expect(provider.baboonSameInVersions().length).toBeGreaterThan(0);
    });

    test("generated ADT branch satisfies BaboonAdtMemberMeta structurally", () => {
        // T4_A1 has data branches B1/B2/B3. B1 has a single field of DTO type T3_D1; build a
        // smoke-only branch by passing a stub through `as unknown as` — we exercise meta only,
        // never serialise. (Generated DTOs accept their typed args at the constructor; we
        // reflectively construct just enough to reach the meta methods.)
        const branch = Object.create(B1.prototype) as B1;

        const member: BaboonAdtMemberMeta = branch;

        expect(typeof member.baboonAdtTypeIdentifier).toBe("function");
        expect(member.baboonAdtTypeIdentifier()).toEqual(B1.BaboonAdtTypeIdentifier);
        // BaboonAdtMemberMeta extends BaboonGenerated — same four methods are exposed.
        expect(typeof member.baboonTypeIdentifier).toBe("function");
        expect(member.baboonTypeIdentifier()).toEqual(B1.BaboonTypeIdentifier);
    });

    test("BaboonTypeMeta.from picks ADT identifier when useAdtIdentifier=true", () => {
        // The codegen-runtime contract: when the caller declares an ADT-typed reference (by
        // passing useAdtIdentifier=true), the envelope's typeIdentifier must come from
        // `baboonAdtTypeIdentifier()`, not from the concrete branch's `baboonTypeIdentifier()`.
        // This was the silent-correctness defect noted in PR-22-D02.
        const branch = Object.create(B1.prototype) as B1;

        const concreteMeta = BaboonTypeMeta.from(branch, false);
        const adtMeta = BaboonTypeMeta.from(branch, true);

        expect(concreteMeta.typeIdentifier).toEqual(B1.BaboonTypeIdentifier);
        expect(adtMeta.typeIdentifier).toEqual(B1.BaboonAdtTypeIdentifier);
        expect(adtMeta.typeIdentifier).not.toEqual(concreteMeta.typeIdentifier);
    });
});
