import { expect, test } from "vitest";
import { Leaf as OldLeaf } from "../generated-review-records/review/evolution/v1_0_0/Leaf";
import { Leaf } from "../generated-review-records/review/evolution/Leaf";
import { Containers as OldContainers } from "../generated-review-records/review/evolution/v1_0_0/Containers";
import { Color as OldColor } from "../generated-review-records/review/evolution/v1_0_0/Color";
import { convert__containers__from__1_0_0 } from "../generated-review-records/review/evolution/from_1_0_0_containers";

test("record mode recursively converts string keys while retaining numeric Maps", () => {
    const result = convert__containers__from__1_0_0(new OldContainers(
        [], { x: new OldLeaf(2) }, new Map([[3, new OldLeaf(4)]]), OldColor.red));
    expect(result.named.x).toBeInstanceOf(Leaf);
    expect(result.named.x.value).toBe(2);
    expect(result.numbered.get(3)).toBeInstanceOf(Leaf);
    expect(result.numbered.get(3)?.value).toBe(4);
});
