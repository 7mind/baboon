# D44 — C# ADT-branch capture-collision (repro-first, T199)

This directory is an **isolated** model-dir (mirroring the D9 `mcp-stub-ok`
precedent) holding the regression fixture for defect **D44**. It is
deliberately NOT placed in the shared `baboon-compiler/src/test/resources/baboon/`
model-dir, which is scanned by all 9 backends' regular/wrapped lanes: D44 is a
**C#-only** collision, so forcing this pathological ADT onto the other 8
backends would add risk with zero benefit (the D3/D5/D7/D11 failure mode).

## The defect

For an ADT, the C# JSON and UEBA encoders take the value as a method parameter
literally named `value`:

```csharp
public override JToken Encode(BaboonCodecContext ctx, A value)
```

and then emit one pattern-match capture per branch, using the **branch type
name lowercased** as the capture identifier. A branch named `Value` lowercases
to `value`, colliding with the enclosing parameter:

```csharp
if (value is A.Value value)   // CS0136 + CS0841
```

## Reproduction (this is the T199 deliverable — EXPECTED to fail)

Fixture: `capture.baboon` — `root adt A { data Value {str:str} data Custom {int:i32} }`.

Command (native binary; equivalent to the `sbt baboonJVM/runMain
io.septimalmind.baboon.Baboon` form used during authoring):

```
baboon --model-dir <this-dir>/ :cs --output <out>/cs \
  --generate-json-codecs-by-default=true --generate-ueba-codecs-by-default=true
```

Emitted `<out>/cs/Csharp-Adtcapture/A.cs` contains `if (value is A.Value value)`
at both the JSON codec (line 444) and the UEBA codec (line 506). Compiling that
output (`dotnet build`, net9.0) FAILS with the following — the pre-fix failing
state that the T200 fix must flip green:

```
A.cs(441,55): error CS0841: Cannot use local variable 'value' before it is declared
A.cs(444,17): error CS0841: Cannot use local variable 'value' before it is declared
A.cs(444,34): error CS0136: A local or parameter named 'value' cannot be declared in this scope because that name is used in an enclosing local scope to define a local or parameter
A.cs(448,26): error CS8121: An expression of type 'A.Value' cannot be handled by a pattern of type 'A.Custom'.
A.cs(502,56): error CS0841: Cannot use local variable 'value' before it is declared
A.cs(506,17): error CS0841: Cannot use local variable 'value' before it is declared
A.cs(506,34): error CS0136: A local or parameter named 'value' cannot be declared in this scope because that name is used in an enclosing local scope to define a local or parameter
A.cs(512,26): error CS8121: An expression of type 'A.Value' cannot be handled by a pattern of type 'A.Custom'.
```

The `CS0136`/`CS0841` pair on the branch-`Value` capture is the D44 defect
(line 444 = JSON codec, line 506 = UEBA codec). The `CS8121` entries are a
downstream consequence of the mis-scoped `value` and disappear once the capture
is renamed.

## Constructibility gate (criticism 2) — outcome

The secondary "case-only-differing sibling" shape (e.g. `Foo`/`FOO`, both
lowercasing to `foo`) was theorized in H41/D44 but never reproduced. Before
baking it into a committed compile fixture, both preconditions were probed:

**(a) Does the typer/validator accept two ADT branch names differing only by
case?** — **NO. REJECTED.** A probe model `root adt B { data Foo {a:str} data
FOO {b:i32} }` fails to load:

```
In ADT: B
Conflicting branches have been found: Foo, FOO
(runner exit code 4)
```

This **refutes** the criticism's stated assumption that case-insensitive
duplicate-name validation "is currently unchecked": the loader already enforces
case-insensitive branch-name uniqueness. Consequently a case-only-differing
branch pair is **unreachable** — no such model loads, so the C# emitter never
runs on it.

**(b) Is C# per-type file emission safe on a case-insensitive FS (macOS CI)?**
— **Not applicable / safe.** The C# backend emits ADT branches as **nested
classes inside a single file** (`A.cs`), NOT one file per branch. The primary
fixture emits exactly `A.cs`, `BaboonRuntime.cs`, `DomainCsharpAdtcaptureFacade.cs`.
There is no per-branch file, hence no case-insensitive-FS filename collision.
For the sibling pair specifically, no files are emitted at all (blocked by (a)).

**Decision — path taken: `Value`-only compile fixture; sibling deferred, NOT
added as a codegen-shape assertion.** Because (a) fails, the sibling pair is not
part of the compile fixture. The specified fallback — a `.jvm/src/test`
codegen-shape assertion that "the two generated branch pattern-captures for a
case-only-differing pair are distinct identifiers" — is **not constructible**:
the loader rejects any model with case-only-differing branch names, so no C# is
ever generated for such a pair to assert against. The sibling-uniqueness
requirement it was meant to guard is **already guaranteed** by the loader's
case-insensitive branch-name conflict check demonstrated in (a). Locking that
loader-rejection invariant with its own dedicated validator test would be a
separate (green, non-repro) task, out of scope for this repro-first T199.

## mdl lanes

- `test-gen-cs-adt-capture` — generates C# from this isolated model-dir with
  both codec flags and drops a minimal net9.0 csproj over the output.
- `test-cs-adt-capture` — `dotnet build`s that output. **REPRO-FIRST: this
  build is EXPECTED to FAIL with CS0136/CS0841 until the T200 generator fix.**

These lanes are intentionally NOT wired into the `test`/`ci` aggregate while
red, to keep the aggregate green between T199 and T200. T200 flips the build
green and adds the aggregate dependency.
