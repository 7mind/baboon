import { describe, expect, test } from "vitest";
import { Child as OldChild } from "../generated-review/review/evolution/v1_0_0/Child";
import { NestedHolder as OldNestedHolder } from "../generated-review/review/evolution/v1_0_0/NestedHolder";
import { CollectionHolder as OldCollectionHolder } from "../generated-review/review/evolution/v1_0_0/CollectionHolder";
import { Child } from "../generated-review/review/evolution/Child";
import { convert__nested_holder__from__1_0_0 } from "../generated-review/review/evolution/from_1_0_0_nested-holder";
import { convert__collection_holder__from__1_0_0 } from "../generated-review/review/evolution/from_1_0_0_collection-holder";
import { Shape_Circle as OldCircle } from "../generated-review/review/evolution/v1_0_0/Shape";
import { Shape_Circle } from "../generated-review/review/evolution/Shape";
import { Leaf as OldLeaf } from "../generated-review/review/evolution/v1_0_0/Leaf";
import { Leaf } from "../generated-review/review/evolution/Leaf";
import { Containers as OldContainers } from "../generated-review/review/evolution/v1_0_0/Containers";
import { Color as OldColor } from "../generated-review/review/evolution/v1_0_0/Color";
import { Color } from "../generated-review/review/evolution/Color";
import { Tone as OldTone } from "../generated-review/review/evolution/v1_0_0/Tone";
import { Tone } from "../generated-review/review/evolution/Tone";
import { convert__tone__from__1_0_0 } from "../generated-review/review/evolution/from_1_0_0_tone";
import { NativeHolder as OldNativeHolder } from "../generated-review/review/evolution/v1_0_0/NativeHolder";
import { Manual as OldManual } from "../generated-review/review/evolution/v1_0_0/Manual";
import { ManualHolder as OldManualHolder } from "../generated-review/review/evolution/v1_0_0/ManualHolder";
import { convert__shape__from__1_0_0 } from "../generated-review/review/evolution/from_1_0_0_shape";
import { convert__containers__from__1_0_0 } from "../generated-review/review/evolution/from_1_0_0_containers";
import { convert__native_holder__from__1_0_0 } from "../generated-review/review/evolution/from_1_0_0_native-holder";
import { convert__manual_holder__from__1_0_0 } from "../generated-review/review/evolution/from_1_0_0_manual-holder";
import { OldRenamedShape_Circle } from "../generated-review/review/evolution/v1_0_0/OldRenamedShape";
import { RenamedShape_Circle } from "../generated-review/review/evolution/RenamedShape";
import { convert__old_renamed_shape__from__1_0_0 } from "../generated-review/review/evolution/from_1_0_0_old-renamed-shape";

describe("typed schema evolution", () => {
    test("renamed ADTs convert branch payload changes with the new constructor", () => {
        const result = convert__old_renamed_shape__from__1_0_0(new OldRenamedShape_Circle(7));
        expect(result).toBeInstanceOf(RenamedShape_Circle);
        expect(result.radius).toBe(7n);
        expect(result.label).toBeUndefined();
    });
    test("renames enum values using the configured representation", () => {
        expect(convert__tone__from__1_0_0(OldTone.lightBlue)).toBe(Tone.deepBlue);
    });
    test("retains ADT branch prototypes", () => {
        const result = convert__shape__from__1_0_0(new OldCircle(7));
        expect(result).toBeInstanceOf(Shape_Circle);
        expect(result.radius).toBe(7);
    });

    test("recurses into lists and both map key representations", () => {
        const result = convert__containers__from__1_0_0(new OldContainers(
            [new OldLeaf(1)], new Map([["x", new OldLeaf(2)]]), new Map([[3, new OldLeaf(4)]]), OldColor.red));
        expect(result.leaves[0]).toBeInstanceOf(Leaf);
        expect(result.named.get("x")).toBeInstanceOf(Leaf);
        expect(result.numbered.get(3)).toBeInstanceOf(Leaf);
        expect(result.color).toBe(Color.red);
    });

    test("preserves unchanged foreign values without serializing", () => {
        const value = new Date("2020-01-01T00:00:00Z");
        expect(convert__native_holder__from__1_0_0(new OldNativeHolder(value)).value).toBe(value);
    });

    test("fails explicitly when a nested conversion requires implementation", () => {
        expect(() => convert__manual_holder__from__1_0_0(new OldManualHolder(new OldManual(7))))
            .toThrow("Custom conversion required: Manual -> Manual");
    });
    test("retains nested prototypes, bigint, sets and maps", () => {
        const source = new OldNestedHolder(new OldChild(9007199254740993n, new Set(["a", "b"]), new Map([["x", 42n]])));
        const result = convert__nested_holder__from__1_0_0(source);
        expect(result.child).toBeInstanceOf(Child);
        expect(result.child.number).toBe(9007199254740993n);
        expect(result.child.tags).toEqual(new Set(["a", "b"]));
        expect(result.child.amounts).toEqual(new Map([["x", 42n]]));
        expect(result.child.baboonDomainVersion()).toBe("2.0.0");
    });

    test("turns a present optional into a singleton list", () => {
        expect(convert__collection_holder__from__1_0_0(new OldCollectionHolder(42)).values).toEqual([42]);
    });

    test("turns an absent optional into an empty list", () => {
        expect(convert__collection_holder__from__1_0_0(new OldCollectionHolder(undefined)).values).toEqual([]);
    });
});
