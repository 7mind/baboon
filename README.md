# baboon
Stripped-down experimental DML with schema evolution support and garbage collection

## Limitations

1. This is a DML, not an IDL, it does not support service/interface definitions
2. No inheritance
3. No templates
4. Only Enums, DTOs and ADTs
5. ADT members can only reference top-level type definitions
6. No values in Enums
7. No newtypes/type aliases
