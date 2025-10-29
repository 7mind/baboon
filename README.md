# baboon

*Let the Baboon do the monkey job*.

Stripped-down experimental Data Modeling Language and its compiler, with schema evolution support and garbage collection.

Te compiler design is an unconventional, based on DAG manipulations and multiple phases. That gives it efficiency and extreme simplicity.

#### Editor Support:

* [Intellij Idea Plugin](https://plugins.jetbrains.com/plugin/28203-baboon-support) ([baboon-intellij](https://github.com/7mind/baboon-intellij))
* [VSCode Extension](https://marketplace.visualstudio.com/items?itemName=SeptimalMind.baboon-vscode)
* [VSCodium Extension](https://open-vsx.org/extension/SeptimalMind/baboon-vscode)

## Features

1. Set-based structural inheritance with subtraction and intersections
2. Automatic JSON codec derivation
3. Automatic UEBA (Ultra-Efficient Binary Aggregate, a custom tagless binary format) codec derivation
4. Automatic evolution derivation where possible, stubs where manual conversion is required
5. Structural *and* nominal inheritance
6. Namespaces
7. Inclusions (at syntax tree level)
8. Lists, sets, dictionaries and optional types.
9. Advanced deduplication for generated C# code.   
10. Codegen targets: C#, Scala

## Limitations

1. No templates
2. Only Enums, DTOs and ADTs
3. Nominal inheritance support is limited to trait model
4. Generic/type constructor support is limited to builtin collections
5. (*) This is a DML, not an IDL, service/interface definitions support is extremely limited at the moment
7. (*) Comments are not preserved in the cogen output
8. (*) No structural inheritance information is preserved in the transpiler output
9. (*) Only integer constants may be associated with enum members
10. (*) No newtypes/type aliases
11. (*) No inheritance-based lenses/projections/conversions

Points marked with (*) will/may be improved in the future.

## CLI

See [tests](https://github.com/7mind/baboon/blob/main/.mobala/steps/run-test.sh).

## Notes

1. All the types which are not transitively referenced by `root` types will be eliminated from the compiler output.
2. Usages in structural inheritance are not considered references, so structural parents which are not directly referenced as fields and not marked as `root`s will be eliminated 

## Foreign types

Be very careful about foreign types. It's your responsibility to make sure everything is set properly.

At the bare minimum you will have to do the following for each foreign type you use:

1) Create a custom codec for your FT
2) Override generated dummy codec instance with `BaboonCodecs#Register`
3) Override generated dummy codec instance using setter on `${Foreign_Type_Name}_UEBACodec#Instance` field
4) Override generated dummy codec instance using setter on `${Foreign_Type_Name}_JsonCodec#Instance` field

Make sure your foreign types are NOT primitive types or other generated types. It's a funny idea, but it will explode in runtime.

Foreign types may hold any position in generics but it's up to you to ensure correctness.
