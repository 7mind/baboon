// Regression test for the `EnumVariant : was[OldName]` rename conversion.
//
// A latent codegen defect in `TsConversionTranslator.CopyEnumByName`
// previously emitted switch arms missing the `case` keyword:
//
//     "OldValue": return "NewValue" as testpkg_EnumMemberRename;
//
// Without the `case` keyword each line parses as a labelled statement
// followed by an unconditional return. Combined with the generated
// `// @ts-nocheck` header this passes the TypeScript checker silently,
// and every input falls through to the `default` arm — silently dropping
// the rename. This test invokes the generated converter and asserts:
//
//   1. a renamed source variant (`OldValue` -> `NewValue`) maps correctly;
//   2. a non-renamed variant (`KeepValue`) passes through unchanged via
//      the `default` arm.

import {describe, test, expect} from "vitest";
import {convert__enum_member_rename__from__1_0_0}
    from "../src/generated/convtest/testpkg/from_1_0_0_enum-member-rename";
import {EnumMemberRename as NewEnum}
    from "../src/generated/convtest/testpkg/EnumMemberRename";
import {EnumMemberRename as OldEnum}
    from "../src/generated/convtest/testpkg/v1_0_0/EnumMemberRename";

describe("EnumMemberRename rename conversion (1.0.0 -> current)", () => {
    test("renamed variant OldValue maps to NewValue", () => {
        const converted = convert__enum_member_rename__from__1_0_0(OldEnum.OldValue);
        expect(converted).toBe(NewEnum.NewValue);
    });

    test("non-renamed variant KeepValue passes through", () => {
        const converted = convert__enum_member_rename__from__1_0_0(OldEnum.KeepValue);
        expect(converted).toBe(NewEnum.KeepValue);
    });
});
