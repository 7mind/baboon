# baboon

*Let the Baboon do the monkey job*.

Stripped-down experimental DML with schema evolution support and garbage collection.

## Features

1. Set-based structural inheritance with subtraction and intersections
2. Automatic codecs derivation (JSON and tagless custom binary format)
3. Automatic evolution derivation where possible, stubs where manual convertion is required
4. Codegen targets: C#

## Limitations

1. (*) This is a DML, not an IDL, it does not support service/interface definitions
2. (*) Currently only C# cogen target is supported
3. (*) Comments are not preserved in the cogen output
4. No nominal inheritance
5. Only structural inheritance is supported
6. (*) No inheritance information is preserved in the transpiler output
7. No templates
8. Only Enums, DTOs and ADTs
9. ADT members can only reference top-level type definitions
10. (*) Only integer constants may be associated with enum members
11. (*) No newtypes/type aliases
12. (*) No inheritance-based lenses/projections/conversions

Points marked with (*) will/may be improved in future.

## CLI

```bash
baboon \
    --model-dir ./src/test/resources/baboon/ \
    --model ./src/test/resources/baboon/pkg0/pkg01.baboon \
    --model ./src/test/resources/baboon/pkg0/pkg02.baboon \
    --output /path/to/directory
```

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