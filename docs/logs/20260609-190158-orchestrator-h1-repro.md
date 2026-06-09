# Investigate session log — D1 / H1 (C# root-cause reproduction, orchestrator-side)

- Defect: D1
- Hypothesis: H1 (C# codec lowercases ADT branch name into an unescaped pattern-capture identifier)
- Role: orchestrator (direct reproduction)
- Verdict: confirmed

### Summary
- **Mechanism (validated against source):** `CSJsonCodecGenerator.scala:158` `val branchNameRef = q"${branchName.toLowerCase}"` used at `:177` as `if (value is $fqBranch $branchNameRef)` and `:162` `Encode(ctx, $branchNameRef)`. Same in `CSUEBACodecGenerator.scala:196` (`val castedName = branchName.toLowerCase`, used `:199`). No reserved-word escaping exists anywhere in the C# identifier path (no `keyword`/`reserved`/`escape`/`@`-prefix logic).
- **Reproduction:** model `adt AvatarItem { data Default {} data BuiltIn { id: str } }` (made reachable via `root data Holder { item: AvatarItem }`), compiled with the JVM compiler (`baboonJVM/runMain io.septimalmind.baboon.Baboon ... :cs --generate-json-codecs-by-default=true --generate-ueba-codecs-by-default=true`). Generated `Avatar/AvatarItem.cs:437`:
  ```csharp
  if (value is AvatarItem.Default default)
      return new JObject(new JProperty("Default", ...Default_JsonCodec.Instance.Encode(ctx, default)));
  ```
  and `:499-502` (UEBA) identically. NOTE: codecs are only emitted for `derived[...]`-annotated types unless the `--generate-*-codecs-by-default=true` flags are passed — a bare unannotated ADT emits no codec, hence no collision.
- **Compile confirmation:** isolated the snippet into a `net9.0` project; `dotnet build` → `error CS1026: ) expected` at the column of `default` (the `builtin` branch compiles fine). Replacing `default` with the C#-verbatim identifier `@default` → `Build succeeded`. Confirms both the failure and the native-fix direction.
