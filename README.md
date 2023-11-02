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
6. No inheritance information is preserved in the transpiler output
7. No templates
8. Only Enums, DTOs and ADTs
9. ADT members can only reference top-level type definitions
10. No constants associated with enum members
11. (*) No newtypes/type aliases

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
