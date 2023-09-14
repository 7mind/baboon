# baboon

*Let the Baboon do the monkey job*.

Stripped-down experimental DML with schema evolution support and garbage collection.

## Limitations

1. This is a DML, not an IDL, it does not support service/interface definitions
2. No nominal inheritance
3. Only the simplest form of structural inheritance is supported
4. No inheritance information is preserved in the transpiler output
5. No templates
6. Only Enums, DTOs and ADTs
7. ADT members can only reference top-level type definitions
8. No values in Enums
9. No newtypes/type aliases
10. Currently only C# cogen target is supported


## CLI

```bash
baboon \
    --model-dir ./src/test/resources/baboon/ \
    --model ./src/test/resources/baboon/pkg0/pkg01.baboon \
    --model ./src/test/resources/baboon/pkg0/pkg02.baboon \
    --output /path/to/directory
```

## Notes

All the types which are not transitively referenced by `root` types will be eliminated from the compiler output.
