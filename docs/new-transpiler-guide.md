# Implementing a New Baboon Backend: Step-by-Step Guide

This guide walks you through implementing a full-featured code generation backend for the Baboon compiler. It assumes you want to add support for a new target language (referred to as `$LANG` throughout). Use the existing C#, Scala, Rust, TypeScript, Python, Kotlin, Java, and Dart translators as reference implementations.

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [Prerequisites and Planning](#2-prerequisites-and-planning)
3. [Step 1: Define the Value Type and Type System](#step-1-define-the-value-type-and-type-system)
4. [Step 2: Implement the Type Translator](#step-2-implement-the-type-translator)
5. [Step 3: Implement File and Tree Utilities](#step-3-implement-file-and-tree-utilities)
6. [Step 4: Implement Definition Translation](#step-4-implement-definition-translation)
7. [Step 5: Implement JSON Codec Generation](#step-5-implement-json-codec-generation)
8. [Step 6: Implement UEBA Binary Codec Generation](#step-6-implement-ueba-binary-codec-generation)
9. [Step 7: Implement Fixture Generation](#step-7-implement-fixture-generation)
10. [Step 8: Implement Test Generation](#step-8-implement-test-generation)
11. [Step 9: Implement Conversion Generation](#step-9-implement-conversion-generation)
12. [Step 10: Implement the Main Translator](#step-10-implement-the-main-translator)
13. [Step 11: Wire into the Compiler](#step-11-wire-into-the-compiler)
14. [Step 12: Create Runtime Libraries](#step-12-create-runtime-libraries)
15. [Step 13: Create Stub Test Project](#step-13-create-stub-test-project)
16. [Step 14: Create Cross-Language Compatibility Tests](#step-14-create-cross-language-compatibility-tests)
17. [Step 15: Update Build Infrastructure](#step-15-update-build-infrastructure)
18. [Type Mapping Reference](#type-mapping-reference)
19. [UEBA Binary Protocol Reference](#ueba-binary-protocol-reference)
20. [TextTree Usage Guide](#texttree-usage-guide)
21. [Service and Pragma Support](#service-and-pragma-support)
22. [Transport Plumbing (separate doc)](transport-plumbing.md)
23. [Common Pitfalls](#common-pitfalls)

---

## 1. Architecture Overview

### Compilation Pipeline

```
.baboon files
    |
    v
BaboonParser --> Raw AST
    |
    v
BaboonTyper --> Typed AST (Domain, DomainMember, Typedef)
    |
    v
BaboonValidator --> Validated Model
    |
    v
BaboonFamily (organized by pkg/version/lineage)
    |
    v
$LANGBaboonTranslator.translate()
    |--- $LANGDefnTranslator     --> Type definitions (DTOs, ADTs, enums, contracts, services)
    |--- $LANGJsonCodecGenerator --> JSON codecs
    |--- $LANGUEBACodecGenerator --> UEBA binary codecs
    |--- $LANGCodecFixtureTranslator --> Random fixture generators
    |--- $LANGCodecTestsTranslator   --> Codec round-trip tests
    |--- $LANGConversionTranslator   --> Schema evolution conversions
    |--- Runtime files               --> Embedded runtime libraries
    |
    v
Sources(Map[Path -> OutputFile])
```

### Key Abstraction: TextTree

All code generation MUST use `TextTree[V]` where `V` is your language-specific value type. Never use raw string concatenation or interpolation for generated code.

TextTree provides:
- **Quote interpolation**: `q"code with $variable"` creates `TextTree[V]`
- **Joining**: `trees.join(", ")` joins a sequence with separator
- **Indentation**: `tree.shift(N)` indents by N spaces
- **Strip margin**: `tree.stripMargin` removes leading `|` characters
- **Rendering**: `tree.mapRender { case MyType(name) => name }` converts to string

### Product Types

The compiler generates several product categories, controlled by CLI flags:

| Product | CLI Flag | Description |
|---------|----------|-------------|
| `Definition` | `--output` (required) | Type definitions + codecs |
| `Runtime` | `--runtime` | Runtime support libraries |
| `Fixture` | `--fixture-output` | Random data generators |
| `FixtureRuntime` | `--test-output` | Shared fixture utilities |
| `Test` | `--test-output` | Codec round-trip tests |
| `Conversion` | `--disable-conversions` | Schema migration code |

---

## 2. Prerequisites and Planning

### Type Mapping Decisions

Before writing code, decide how each Baboon builtin type maps to your target language. Some types may not have direct native equivalents.

**Scalar types to map:**

| Baboon | Description | Notes |
|--------|-------------|-------|
| `bit` | Boolean | Usually trivial |
| `i08`, `i16`, `i32`, `i64` | Signed integers | Most languages have these |
| `u08`, `u16`, `u32`, `u64` | Unsigned integers | Some languages lack unsigned types; use next-wider signed or special types |
| `f32`, `f64` | IEEE 754 floats | Usually trivial |
| `f128` | Decimal | Use language's decimal/BigDecimal. Serialize as string in JSON |
| `str` | UTF-8 string | Usually trivial |
| `bytes` | Byte array | Use language's byte array. Serialize as hex or base64 in JSON |
| `uid` | UUID | Use native UUID type or string wrapper. C# uses mixed-endian GUID format in UEBA |
| `tsu` | UTC timestamp | Use native datetime. If no UTC type, use struct with millis + kind byte |
| `tso` | Offset timestamp | Use native datetime with offset. If no offset type, use struct with millis + offset millis + kind byte |

**Collection types to map:**

| Baboon | Description | Target Recommendations |
|--------|-------------|----------------------|
| `opt[T]` | Optional | `Option[T]`, `T?`, `Optional<T>`, etc. |
| `lst[T]` | Ordered list | Prefer immutable: `List[T]`, `IReadOnlyList<T>`, `Vec<T>` |
| `set[T]` | Unordered set | Prefer ordered/immutable: `Set[T]`, `ImmutableHashSet<T>`, `BTreeSet<T>` |
| `map[K,V]` | Key-value map | Prefer ordered/immutable: `Map[K,V]`, `IReadOnlyDictionary<K,V>`, `BTreeMap<K,V>` |

**Types without direct native equivalents:**

If your language lacks a native type for a Baboon builtin, use a structured representation:
- **UUID**: Can be stored as a string if no native UUID type exists
- **Timestamps (`tsu`/`tso`)**: Can be stored as a struct with `epochMillis: i64` and `offsetMillis: i64` (for `tso`) and `kind: u8`. Or use string representation (ISO 8601)
- **Decimal (`f128`)**: Use a string representation if no native arbitrary-precision decimal. Always serialize as string in JSON to preserve precision
- **Unsigned integers**: If no native unsigned types, use the next larger signed type (e.g., `u32` -> `i64`) or dedicated wrapper types

### File Organization Decision

Choose how generated files map to your language's module system:
- **One file per type** (like Rust, TypeScript): Each DTO/ADT/enum gets its own file
- **Namespace-grouped files** (like C#): Types grouped by namespace into files
- **Package-based** (like Scala, Python): Types organized into package directories

---

## Step 1: Define the Value Type and Type System

Create the package `io.septimalmind.baboon.translator.$lang/` and add two files.

### $LANGValue.scala

Define your language's TextTree value type. This type is used throughout code generation to represent references to types, enabling automatic import resolution.

```scala
package io.septimalmind.baboon.translator.$lang

import izumi.fundamentals.collections.nonempty.NEList

sealed trait $LANGValue

object $LANGValue {
  // Represents a module/package/namespace identifier
  final case class $LANGModuleId(parts: NEList[String]) {
    val module: String = parts.last
  }

  // Represents a type reference with its module location
  final case class $LANGType(
    module: $LANGModuleId,
    name: String,
    fq: Boolean = false,      // render fully qualified?
    predef: Boolean = false,   // built-in type (no import needed)?
  ) extends $LANGValue

  // Represents a type name without module context (no import generated)
  final case class $LANGTypeName(name: String) extends $LANGValue
}
```

Key design decisions:
- `$LANGType` nodes in a `TextTree[$LANGValue]` are scanned by the main translator to generate imports
- `$LANGTypeName` is for cases where you want to emit a name without triggering import resolution
- The `fq` flag controls whether the type renders with full qualification
- The `predef` flag prevents import generation for built-in types (e.g., `int`, `string`, `bool`)

### $LANGTypes.scala

Define constants for all predefined types your generator will reference:

```scala
package io.septimalmind.baboon.translator.$lang

import $LANGValue.*

object $LANGTypes {
  // Runtime package
  val runtimeModule: $LANGModuleId = $LANGModuleId(NEList("baboon", "runtime"))
  val fixtureModule: $LANGModuleId = $LANGModuleId(NEList("baboon", "fixture"))

  // Builtin scalar types (predef = true means no import needed)
  val tBool: $LANGType   = $LANGType(predefModule, "bool", predef = true)
  val tI8: $LANGType     = $LANGType(predefModule, "i8", predef = true)
  val tI16: $LANGType    = $LANGType(predefModule, "i16", predef = true)
  val tI32: $LANGType    = $LANGType(predefModule, "i32", predef = true)
  val tI64: $LANGType    = $LANGType(predefModule, "i64", predef = true)
  val tU8: $LANGType     = $LANGType(predefModule, "u8", predef = true)
  // ... and so on for all scalar types

  // External library types (these will trigger imports)
  val tUuid: $LANGType    = $LANGType(uuidModule, "Uuid")
  val tDecimal: $LANGType = $LANGType(decimalModule, "Decimal")
  val tDateTimeUtc: $LANGType = $LANGType(timeModule, "DateTime")
  val tDateTimeOffset: $LANGType = $LANGType(timeModule, "DateTimeOffset")

  // Collection types
  val tList: $LANGType = $LANGType(predefModule, "List", predef = true)
  val tSet: $LANGType  = $LANGType(predefModule, "Set", predef = true)
  val tMap: $LANGType  = $LANGType(predefModule, "Map", predef = true)
  val tOpt: $LANGType  = $LANGType(predefModule, "Option", predef = true)

  // Runtime types (for codecs, fixtures, etc.)
  val tBaboonJsonCodec: $LANGType = $LANGType(runtimeModule, "BaboonJsonCodec")
  val tBaboonBinCodec: $LANGType  = $LANGType(runtimeModule, "BaboonBinCodec")
  val tBaboonRandom: $LANGType    = $LANGType(fixtureModule, "BaboonRandom")
  val tBaboonCodecContext: $LANGType = $LANGType(runtimeModule, "BaboonCodecContext")
}
```

---

## Step 2: Implement the Type Translator

### $LANGTypeTranslator.scala

This class converts Baboon types (`TypeId`, `TypeRef`) into target language type references (`TextTree[$LANGValue]`).

**Core responsibilities:**
1. Map `TypeId.Builtin` (scalars and collections) to native types
2. Map `TypeId.User` (DTOs, ADTs, enums) to fully qualified type references
3. Handle generic type parameters for collections
4. Handle foreign type bindings
5. Generate versioned module paths (latest version omits suffix)

```scala
class $LANGTypeTranslator(target: $LANGTarget) {
  import $LANGValue.*

  // Convert a TypeRef to a TextTree with proper generic syntax
  def asRef(tpe: TypeRef, domain: Domain, evolution: BaboonEvolution): TextTree[$LANGValue] = {
    tpe match {
      case TypeRef.Scalar(id) => q"${asType(id, domain, evolution)}"
      case TypeRef.Constructor(id, args) =>
        id.name.name match {
          case "opt" => q"Option<${asRef(args.head, domain, evolution)}>"
          case "lst" => q"Vec<${asRef(args.head, domain, evolution)}>"
          case "set" => q"Set<${asRef(args.head, domain, evolution)}>"
          case "map" => q"Map<${asRef(args.head, domain, evolution)}, ${asRef(args(1), domain, evolution)}>"
        }
    }
  }

  // Convert a TypeId to a native type
  def asType(tpe: TypeId, domain: Domain, evolution: BaboonEvolution): $LANGType = {
    tpe match {
      case b: TypeId.Builtin => builtinType(b)
      case u: TypeId.User    => userType(u, domain, evolution)
    }
  }

  // Map builtin type IDs to native types
  private def builtinType(id: TypeId.Builtin): $LANGType = {
    id.name.name match {
      case "bit"   => $LANGTypes.tBool
      case "i08"   => $LANGTypes.tI8
      case "i16"   => $LANGTypes.tI16
      case "i32"   => $LANGTypes.tI32
      case "i64"   => $LANGTypes.tI64
      case "u08"   => $LANGTypes.tU8
      // ... all scalar types
      case "f128"  => $LANGTypes.tDecimal
      case "str"   => $LANGTypes.tString
      case "uid"   => $LANGTypes.tUuid
      case "tsu"   => $LANGTypes.tDateTimeUtc
      case "tso"   => $LANGTypes.tDateTimeOffset
      case "bytes" => $LANGTypes.tBytes
    }
  }

  // Convert user-defined type to target type with versioned module path
  private def userType(id: TypeId.User, domain: Domain, evolution: BaboonEvolution): $LANGType = {
    val modulePath = toModulePath(id.pkg, domain.version, evolution)
    $LANGType(modulePath, id.name.name)
  }

  // Generate versioned module path
  // Latest version: pkg.subpkg.typename
  // Older version:  pkg.subpkg.v1_0.typename
  def toModulePath(pkg: Pkg, version: Version, evolution: BaboonEvolution): $LANGModuleId = {
    val isLatest = evolution.latest.version == version
    val baseParts = pkg.path.map(_.toLowerCase)
    val versionedParts = if (isLatest) baseParts else baseParts :+ version.format("v", "_")
    $LANGModuleId(NEList.unsafeFrom(versionedParts))
  }
}
```

---

## Step 3: Implement File and Tree Utilities

### $LANGFileTools.scala

Handles file path generation for output files:

```scala
class $LANGFileTools(target: $LANGTarget) {
  // Generate the base file name for a domain version
  def basename(domain: Domain, evolution: BaboonEvolution): String = {
    val isLatest = evolution.latest.version == domain.version
    val pkgPath = domain.id.path.mkString("-")
    if (isLatest) pkgPath
    else s"$pkgPath-${domain.version.format("", "-")}"
  }

  // Generate output path for a type definition
  def outputPath(typeId: TypeId.User, domain: Domain, evolution: BaboonEvolution, suffix: String): String = {
    // Convert package + owner + name to file path using language conventions
    // e.g., "pkg/subpkg/my_type.rs" or "Pkg/SubPkg/MyType.cs"
    ???
  }
}
```

### $LANGTreeTools.scala

Handles wrapping generated code in language-specific module/namespace/package declarations:

```scala
class $LANGTreeTools {
  // Wrap code in namespace/module/package declaration
  def inModule(modulePath: Seq[String], tree: TextTree[$LANGValue]): TextTree[$LANGValue] = {
    // Language-specific: e.g., "namespace Foo.Bar { ... }" or "package foo.bar\n..."
    ???
  }
}
```

### $LANGDomainTreeTools.scala

Generates metadata methods/properties for types (version info, type identifiers):

```scala
class $LANGDomainTreeTools(domain: Domain, evolution: BaboonEvolution) {
  // Generate metadata for a type definition
  def metaFields(typeId: TypeId.User): TextTree[$LANGValue] = {
    q"""baboon_domain_version = "${domain.version}"
       |baboon_domain_identifier = "${domain.id}"
       |baboon_type_identifier = "${typeId.render}"
       |baboon_same_in_versions = [${sameInVersions(typeId).map(v => q""""$v"""").join(", ")}]""".stripMargin
  }
}
```

---

## Step 4: Implement Definition Translation

### $LANGDefnTranslator.scala

This is the largest component. It translates each `DomainMember.User` into target language type definitions.

**Must handle all typedef kinds:**

| Kind | Description | Example Output |
|------|-------------|----------------|
| `Typedef.Dto` | Data transfer object | Class/struct/record with fields |
| `Typedef.Adt` | Algebraic data type | Sealed class hierarchy / tagged union / enum with data |
| `Typedef.Enum` | Enumeration | Enum type with named members |
| `Typedef.Contract` | Interface/trait | Abstract interface with field declarations |
| `Typedef.Service` | RPC service | Interface with method signatures |
| `Typedef.Foreign` | External type | No code generated (mapped via bindings) |

**Important patterns for each kind:**

### DTO Generation

Generate a proper data class with:
- All fields from `dto.fields` with mapped types
- Contract implementations if `dto.contracts` is non-empty
- Equality and hashing (especially for collections and nested types)
- Codec metadata (version, type identifier, same-in-versions list)
- Codec registration (JSON + UEBA codec instances)

```
// Pseudocode for DTO output
class MyDto {
  field1: Type1
  field2: Type2

  // metadata
  static baboonTypeIdentifier = "pkg.MyDto"
  static baboonDomainVersion = "1.0"
}
```

### ADT Generation

Generate a sealed type hierarchy:
- A base type (sealed trait/abstract class/tagged union)
- One branch type per member in `adt.members` (each is a DTO)
- Shared fields from `adt.fields` on the base type
- Branch discrimination logic for serialization

The ADT branches are separate `DomainMember.User` entries with `Owner.Adt` as their owner. They will have their `translate()` called independently, but you must skip them at the top level (they are rendered as part of the parent ADT).

### Enum Generation

Generate an enumeration with:
- Named members from `enum.members`
- `parse(string) -> Enum` method
- `all() -> List[Enum]` method
- String conversion support

### Contract Generation

Generate an interface/trait with:
- Abstract field/property declarations from `contract.fields`
- Parent contract references from `contract.contracts`

### Service Generation

Generate a service interface with methods. Each method has:
- `sig`: Input type (the argument type)
- `out`: Optional output type (if `None`, use unit/void)
- `err`: Optional error type

Service rendering depends on pragmas:
- **Service result configuration**: Controls whether methods return bare types or wrapped results (e.g., `Either[Error, Success]`, `Result<Success, Error>`)
- **Service context configuration**: Controls whether methods take a context parameter

In addition to the service interface, the compiler generates **transport wiring** — a static dispatcher class that marshals between wire formats (JSON, UEBA) and domain types, and optionally a **runtime typeclass interface** (`IBaboonServiceRt`) that abstracts over the error container shape.

See [Service and Pragma Support](#service-and-pragma-support) for pragma configuration and [Transport Plumbing](transport-plumbing.md) for the full wiring architecture, container shape abstraction, and implementation guide.

### Foreign Type Handling

For `Typedef.Foreign`, check if a binding exists for your language key (e.g., `"$lang"`). If so, use the declared type directly. Do not generate any code - foreign types must have manual codec implementations.

### Output Structure

```scala
case class Output(
  path: String,                          // File path relative to output directory
  tree: Option[TextTree[$LANGValue]],   // Generated code (None = skip)
  module: $LANGModuleId,                // Module/package for import resolution
  product: CompilerProduct,              // Definition, Fixture, Test, etc.
  codecReg: List[CodecRegistration],    // Codec registrations for this type
)
```

---

## Step 5: Implement JSON Codec Generation

### $LANGJsonCodecGenerator.scala

Implements `$LANGCodecTranslator` trait. Generates JSON serialization/deserialization code.

**Encoding rules by type:**

| Baboon Type | JSON Representation |
|-------------|-------------------|
| `bit` | `true` / `false` |
| `i08`..`i32`, `u08`..`u16` | JSON number |
| `i64`, `u32`, `u64` | JSON number (be careful about precision in JS-based languages; `i64`/`u64` should be strings in TypeScript) |
| `f32`, `f64` | JSON number |
| `f128` | **JSON string** (preserves decimal precision) |
| `str` | JSON string |
| `bytes` | **JSON string** (hex-encoded or base64-encoded - must match other backends; current convention is hex) |
| `uid` | **JSON string** (UUID string format: `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`) |
| `tsu` | **JSON string** (ISO 8601 UTC: `yyyy-MM-ddTHH:mm:ss.SSSZ`) |
| `tso` | **JSON string** (ISO 8601 with offset: `yyyy-MM-ddTHH:mm:ss.SSS+HH:MM`) |
| `opt[T]` | `T` value or `null` |
| `lst[T]` | JSON array |
| `set[T]` | JSON array |
| `map[K,V]` | JSON object (keys are always strings; non-string keys must be stringified) |
| Enum | **JSON string** (enum member name) |
| DTO | JSON object with field names as keys |
| ADT (non-wrapped) | JSON object: field data directly |
| ADT (wrapped) | JSON object: `{"BranchName": { ...field data... }}` |

**Key considerations:**
- **Map keys**: Always serialized as strings. For non-string key types, convert to string representation (e.g., `"123"` for integer keys)
- **ADT branch discrimination**: In wrapped mode, the outer key identifies the branch. In non-wrapped mode, the object structure determines the branch
- **Deprecated versions**: If `enableDeprecatedEncoders` is false, non-latest versions should throw on encode but still support decode

---

## Step 6: Implement UEBA Binary Codec Generation

### $LANGUEBACodecGenerator.scala

UEBA (Universal Efficient Binary Abstraction) is a custom binary protocol. Every backend **must** produce identical binary output for the same data to ensure cross-language compatibility.

### Protocol Structure

**Header byte:**
```
Bit 0: Index flag (1 = indexed mode, 0 = compact mode)
Bits 1-7: Reserved (must be 0)
```

**DTO encoding (compact mode):**
```
[header: u8 = 0x00]
[field_1 encoded]
[field_2 encoded]
...
[field_N encoded]
```

**DTO encoding (indexed mode):**
```
[header: u8 = 0x01]
[index_entry_count: u16]
For each variable-length field:
  [offset: u32] [length: u32]
[field_1 encoded]
[field_2 encoded]
...
[field_N encoded]
```

Variable-length fields are those whose binary size isn't known at compile time: strings, bytes, collections, options, nested DTOs, and any type containing them.

**ADT encoding:**
```
[branch_index: u8]  // 0-based index of the branch
[branch DTO encoded]
```

In wrapped mode, the branch index is written by the ADT codec, and the branch DTO omits the index. In non-wrapped mode, each branch writes its own complete encoding.

**Enum encoding:**
```
[variant_index: u8]  // 0-based index of the enum member
```

### Scalar Binary Encodings

All multi-byte values use **little-endian** byte order:

| Baboon | Binary Size | Encoding |
|--------|-------------|----------|
| `bit` | 1 byte | `0x00` = false, `0x01` = true |
| `i08` | 1 byte | Signed byte |
| `i16` | 2 bytes | Little-endian signed 16-bit |
| `i32` | 4 bytes | Little-endian signed 32-bit |
| `i64` | 8 bytes | Little-endian signed 64-bit |
| `u08` | 1 byte | Unsigned byte |
| `u16` | 2 bytes | Little-endian unsigned 16-bit |
| `u32` | 4 bytes | Little-endian unsigned 32-bit |
| `u64` | 8 bytes | Little-endian unsigned 64-bit |
| `f32` | 4 bytes | Little-endian IEEE 754 single |
| `f64` | 8 bytes | Little-endian IEEE 754 double |
| `f128` | Variable | `[length: i32][utf8_string_bytes]` (decimal as string) |
| `str` | Variable | `[length: i32][utf8_bytes]` |
| `bytes` | Variable | `[length: i32][raw_bytes]` |
| `uid` | 16 bytes | .NET GUID mixed-endian format (see below) |
| `tsu` | 17 bytes | `[epoch_millis: i64][offset_millis: i64][kind: u8 = 0]` |
| `tso` | 17 bytes | `[epoch_millis: i64][offset_millis: i64][kind: u8 = 1]` |
| `opt[T]` | 1 + sizeof(T) | `[0x00]` if None, `[0x01][T encoded]` if Some |
| `lst[T]` | Variable | `[count: i32][T_1 encoded]...[T_N encoded]` |
| `set[T]` | Variable | `[count: i32][T_1 encoded]...[T_N encoded]` |
| `map[K,V]` | Variable | `[count: i32][K_1 encoded][V_1 encoded]...[K_N encoded][V_N encoded]` |

### UUID Binary Format (.NET GUID)

UUIDs are encoded in .NET GUID mixed-endian format for compatibility with the C# reference implementation:
- Bytes 0-3: `Data1` (4 bytes, **little-endian**)
- Bytes 4-5: `Data2` (2 bytes, **little-endian**)
- Bytes 6-7: `Data3` (2 bytes, **little-endian**)
- Bytes 8-15: `Data4` (8 bytes, **big-endian**)

To convert from standard UUID (big-endian) to .NET GUID format:
1. Reverse bytes 0-3
2. Reverse bytes 4-5
3. Reverse bytes 6-7
4. Bytes 8-15 remain unchanged

### Index Support

When `BaboonCodecContext.Indexed` is used, the encoder must:
1. Write header byte with bit 0 set (`0x01`)
2. Count variable-length fields and write count as `u16`
3. For each variable-length field, write `[offset: u32][length: u32]` entries
4. Write all field data

The decoder reads the header, and if indexed mode is detected, reads the index entries. This enables random-access decoding of specific fields.

---

## Step 7: Implement Fixture Generation

### $LANGCodecFixtureTranslator.scala

Generates random data constructors for each type. These are used by tests to create random instances for codec round-trip testing.

**Generator mapping:**

| Baboon Type | Generator |
|-------------|-----------|
| `bit` | `rnd.nextBool()` |
| `i08`..`i64` | `rnd.nextI08()`..`rnd.nextI64()` |
| `u08`..`u64` | `rnd.nextU08()`..`rnd.nextU64()` |
| `f32` | `rnd.nextF32()` |
| `f64` | `rnd.nextF64()` |
| `f128` | `rnd.nextDecimal()` (generate as integer to avoid precision issues) |
| `str` | `rnd.nextString()` |
| `bytes` | `rnd.nextBytes()` |
| `uid` | `rnd.nextUuid()` |
| `tsu` | `rnd.nextTimestampUtc()` |
| `tso` | `rnd.nextTimestampOffset()` |
| `opt[T]` | `rnd.mkOption(() => genT())` |
| `lst[T]` | `rnd.mkList(() => genT())` |
| `set[T]` | `rnd.mkSet(() => genT())` |
| `map[K,V]` | `rnd.mkMap(() => genK(), () => genV())` |
| Enum | `rnd.randomEnum(EnumType.all())` |
| DTO | `MyDto(gen_field1(), gen_field2(), ...)` |
| ADT | `rnd.oneOf(randomAll())` where `randomAll()` generates one of each branch |

**Skip fixture generation for:**
- Foreign types (no automatic constructor available)
- Recursive types (would cause infinite loops)
- Contracts and services (not data types)

---

## Step 8: Implement Test Generation

### $LANGCodecTestsTranslator.scala

Generates codec round-trip tests using the target language's test framework.

**Each type gets tests for:**

1. **JSON codec round-trip** (N iterations):
   ```
   fixture = random()
   encoded = jsonEncode(fixture)
   decoded = jsonDecode(encoded)
   assert(fixture == decoded)
   ```

2. **UEBA codec round-trip - compact mode** (N iterations):
   ```
   fixture = random()
   encoded = uebaEncode(CompactContext, fixture)
   decoded = uebaDecode(CompactContext, encoded)
   assert(fixture == decoded)
   ```

3. **UEBA codec round-trip - indexed mode** (N iterations):
   ```
   fixture = random()
   encoded = uebaEncode(IndexedContext, fixture)
   decoded = uebaDecode(IndexedContext, encoded)
   assert(fixture == decoded)
   ```

4. **Cross-language JSON reading** (reads files produced by C# tests):
   ```
   jsonFile = "../../../../../target/cs/json-default/{typeId}.json"
   if (file.exists) {
     decoded = jsonDecode(readFile(jsonFile))
     assert(decoded != null)  // Successful decode
   }
   ```

5. **Cross-language UEBA reading** (reads files produced by C# tests):
   ```
   uebaFile = "../../../../../target/cs/ueba-compact/{typeId}.uebin"
   if (file.exists) {
     decoded = uebaDecode(readFile(uebaFile))
     assert(decoded != null)
   }
   ```

The iteration count comes from `target.generic.codecTestIterations` (default: 500).

**Additionally**, each test writes its own encoded output for other languages to read:
```
// After encoding, write to file for cross-language testing
writeFile("../../../../../target/$lang/json-default/{typeId}.json", encodedJson)
writeFile("../../../../../target/$lang/ueba-compact/{typeId}.uebin", encodedUeba)
writeFile("../../../../../target/$lang/ueba-indexed/{typeId}.uebin", encodedUebaIndexed)
```

---

## Step 9: Implement Conversion Generation

### $LANGConversionTranslator.scala

Generates schema evolution conversions between domain versions. For each type that changed between versions, a conversion function is generated.

**Conversion types:**

1. **CustomConversionRequired**: The type changed in a way that can't be auto-converted. Generate an abstract method the user must implement.

2. **CopyEnumByName**: Enum members may have been renamed. Generate string-based mapping:
   ```
   convert(from) {
     name = from.toString()
     // Apply renames
     if (name == "OldValue") name = "NewValue"
     return parse(name)
   }
   ```

3. **CopyAdtBranchByName**: ADT branches may have been added/removed/renamed. Generate branch-by-branch conversion.

4. **DtoConversion**: Field-by-field conversion with these operations:
   - **Transfer**: Copy field directly (with recursive conversion if types differ)
   - **InitializeWithDefault**: New field gets default value (`None`, empty collection, etc.)
   - **WrapIntoCollection**: Scalar wrapped into `Some(x)`, `[x]`, `Set(x)`, etc.
   - **ExpandPrecision**: Type widened (e.g., `i32` -> `i64`), do cast
   - **SwapCollectionType**: `lst` <-> `set` conversion
   - **Rename**: Field renamed (copy from old field name)

**Registration**: All conversions are collected into a `BaboonConversions` class with a `RequiredConversions` interface that users implement for custom conversions.

---

## Step 10: Implement the Main Translator

### $LANGBaboonTranslator.scala

This is the entry point that orchestrates all generation. It must implement `BaboonAbstractTranslator[F]`.

**Main flow:**

```scala
class $LANGBaboonTranslator[F[+_, +_]: Error2](
  typeTranslator: $LANGTypeTranslator,
  conversionTranslatorFactory: $LANGConversionTranslator.Factory[F],
  target: $LANGTarget,
  treeTools: $LANGTreeTools,
  fileTools: $LANGFileTools,
  defnTranslator: Subcontext[$LANGDefnTranslator[F]],
) extends BaboonAbstractTranslator[F] {

  override def translate(family: BaboonFamily): Out[Sources] = {
    for {
      translated <- translateFamily(family)      // Generate all definitions
      runtime    <- sharedRuntime()              // Embed runtime libraries
      fixture    <- sharedFixture()              // Embed fixture utilities
      allOutputs = translated ++ runtime ++ fixture
      rendered   = allOutputs.map(o => (o.path, OutputFile(renderTree(o, family), o.product)))
      unique     <- rendered.toUniqueMap(...)
    } yield Sources(unique)
  }
}
```

**`renderTree` must:**
1. Scan the `TextTree[$LANGValue]` for all `$LANGType` nodes
2. Filter out predef types and self-references
3. Generate import statements for remaining types
4. Wrap in module/namespace/package declaration
5. Call `.mapRender` to convert to final string

**`sharedRuntime` must:**
1. Read embedded resource files from `baboon-runtime/$lang/`
2. Return them as `Output` entries with `CompilerProduct.Runtime`

---

## Step 11: Wire into the Compiler

### CompilerOptions.scala

Add your language's options case class:

```scala
final case class $LANGOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateJsonCodecsByDefault: Boolean,
  generateUebaCodecsByDefault: Boolean,
  serviceResult: ServiceResultConfig,
  serviceContext: ServiceContextConfig,
  pragmas: Map[String, String],
)
```

Add a new `CompilerTarget` variant:

```scala
case class $LANGTarget(
  id: String,
  output: OutputOptions,
  generic: GenericOptions,
  language: $LANGOptions,
) extends CompilerTarget
```

Add default service result config:

```scala
val $langDefault: ServiceResultConfig = ServiceResultConfig(
  noErrors   = true,   // or false, depending on language conventions
  resultType = None,   // or Some("Result") for languages with result types
  pattern    = None,   // or Some("<$success, $error>")
  hkt        = None,
)
```

### CLIOptions.scala

Add a CLI options case class:

```scala
case class $LANGCLIOptions(
  @Recurse
  generic: GenericTranspilerCLIOptions,
  @HelpMessage("Allow to erase target directory even if files with these extensions exist there")
  extAllowCleanup: List[String],
  @HelpMessage("Adds evolution metadata dictionary")
  $langWriteEvolutionDict: Option[Boolean],
  @HelpMessage("Every ADT branch will encode ADT metadata and expect it in the decoder")
  $langWrappedAdtBranchCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs")
  generateJsonCodecs: Option[Boolean],
  @HelpMessage("Generate UEBA codecs")
  generateUebaCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs even for types without derived[json]")
  generateJsonCodecsByDefault: Option[Boolean],
  @HelpMessage("Generate UEBA codecs even for types without derived[ueba]")
  generateUebaCodecsByDefault: Option[Boolean],
  // ... service options (same as other languages)
  serviceResultNoErrors: Option[Boolean],
  serviceResultType: Option[String],
  serviceResultPattern: Option[String],
  serviceContextMode: Option[String],
  serviceContextType: Option[String],
  serviceContextParameterName: Option[String],
  pragma: List[String],
) extends SharedCLIOptions
```

### Baboon.scala

Add a new case in the modality match:

```scala
case "$lang" =>
  CaseApp.parse[$LANGCLIOptions](roleArgs).leftMap(e => s"Can't parse $lang CLI: $e").map {
    case (opts, _) =>
      val shopts = mkGenericOpts(opts)
      CompilerTarget.$LANGTarget(
        id       = "$LANG",
        output   = shopts.outOpts,
        generic  = shopts.genericOpts,
        language = $LANGOptions(
          writeEvolutionDict          = opts.$langWriteEvolutionDict.getOrElse(false),
          wrappedAdtBranchCodecs      = opts.$langWrappedAdtBranchCodecs.getOrElse(false),
          generateJsonCodecs          = opts.generateJsonCodecs.getOrElse(true),
          generateUebaCodecs          = opts.generateUebaCodecs.getOrElse(true),
          generateJsonCodecsByDefault = opts.generateJsonCodecsByDefault.getOrElse(false),
          generateUebaCodecsByDefault = opts.generateUebaCodecsByDefault.getOrElse(false),
          serviceResult               = mkServiceResult(opts, ServiceResultConfig.$langDefault),
          serviceContext              = mkServiceContext(opts),
          pragmas                     = parsePragmas(opts.pragma),
        ),
      )
  }
```

Add a new case in `processTarget`:

```scala
case t: CompilerTarget.$LANGTarget =>
  new BaboonJvm$LANGModule[F](t)
```

Update the help text to include the new language.

### BaboonModule.scala

Add a DI module:

```scala
class BaboonCommon$LANGModule[F[+_, +_]: Error2: TagKK] extends ModuleDef {
  include(new SharedTranspilerModule[F])

  makeSubcontext[$LANGDefnTranslator[F]]
    .localDependencies(List(DIKey[Domain], DIKey[BaboonEvolution]))
    .withSubmodule(new ModuleDef {
      make[$LANGDefnTranslator[F]].from[$LANGDefnTranslator.$LANGDefnTranslatorImpl[F]]
      make[$LANGCodecFixtureTranslator].from[$LANGCodecFixtureTranslator.Impl]
      make[$LANGCodecTestsTranslator].from[$LANGCodecTestsTranslator.Impl]
      many[$LANGCodecTranslator]
        .add[$LANGJsonCodecGenerator]
        .add[$LANGUEBACodecGenerator]
    })

  make[$LANGFileTools].from[$LANGFileTools.Impl]
  make[$LANGTreeTools].from[$LANGTreeTools.Impl]

  make[$LANGTypeTranslator]
  makeFactory[$LANGConversionTranslator.Factory[F]]

  make[$LANGBaboonTranslator[F]].aliased[BaboonAbstractTranslator[F]]
  many[BaboonAbstractTranslator[F]]
    .ref[$LANGBaboonTranslator[F]]
}
```

### BaboonModuleJvm.scala / BaboonJvm$LANGModule.scala

Create a JVM-specific module that takes the target configuration:

```scala
class BaboonJvm$LANGModule[F[+_, +_]: Error2: TagKK](target: CompilerTarget.$LANGTarget) extends ModuleDef {
  include(new BaboonCommon$LANGModule[F])
  make[CompilerTarget.$LANGTarget].fromValue(target)
}
```

### ServiceContextResolver.scala and ServiceResultResolver.scala

Add your language key to the `languages` list:

```scala
private val languages = Seq("cs", "python", "rust", "scala", "typescript", "kotlin", "java", "dart", "$lang")
```

---

## Step 12: Create Runtime Libraries

Create `baboon-compiler/src/main/resources/baboon-runtime/$lang/` with the runtime support files your generated code depends on.

**Minimum required runtime components:**

### Codec Infrastructure
- `BaboonCodecContext` - enum/class with `Default`, `Indexed`, `Compact` modes
- `BaboonJsonCodec<T>` - interface/trait for JSON encode/decode
- `BaboonBinCodec<T>` - interface/trait for UEBA encode/decode
- `BaboonBinCodecIndexed` - interface for index element counting
- `BaboonIndexEntry` - struct with `offset: u32, length: u32`
- Base classes for codecs (with/without encoder for deprecated versions)

### Binary I/O
- Little-endian binary reader/writer
- Methods for all scalar types (readI32/writeI32, readI64/writeI64, etc.)
- String encoding: `[length: i32][utf8_bytes]`
- UUID encoding: .NET GUID mixed-endian format
- Timestamp encoding: `[epoch_millis: i64][offset_millis: i64][kind: u8]`
- Decimal encoding: `[length: i32][string_representation]`

### Metadata
- `BaboonGenerated` - marker interface/trait for all generated types
- `BaboonGeneratedLatest` - marker for latest version types
- `BaboonAdtMemberMeta` - interface for ADT branch identification
- Version tracking structures

### Conversion Infrastructure
- `AbstractConversion<From, To>` - base conversion class
- `AbstractBaboonConversions` - conversion registry and dispatcher

### Time Formatting
- ISO 8601 timestamp formatting with millisecond precision
- Format: `yyyy-MM-ddTHH:mm:ss.SSSZ` for UTC
- Format: `yyyy-MM-ddTHH:mm:ss.SSS+HH:MM` for offset timestamps

### Fixture Runtime (separate file)
- `BaboonRandom` - random data generator
- Methods for all scalar types
- Collection generators (`mkList`, `mkSet`, `mkMap`, `mkOption`)
- Enum random selection

**Important**: Resource files are embedded into the GraalVM native image. After modifying resources, always run `sbt clean compile` followed by `mdl :build` to ensure changes are picked up.

---

## Step 13: Create Stub Test Project

Create `test/$lang-stub/` with a minimal project that can compile and run the generated code.

### Project Setup

The stub project must:
1. Declare dependencies on any libraries your generated code uses (JSON, UUID, datetime, etc.)
2. Have a source directory where generated code will be placed
3. Have a test configuration that can discover and run generated tests
4. Have a `.gitignore` that excludes generated files and build artifacts

### Example Structure

```
test/$lang-stub/
  .gitignore            # Exclude: generated*, target/, build artifacts
  $build_file           # Language build file (package.json, Cargo.toml, etc.)
  src/
    (empty - generated code goes here)
```

### Dependencies

Based on existing backends, you'll typically need:
- JSON serialization library
- UUID library
- Date/time library
- Decimal/BigDecimal library
- Test framework
- Binary I/O (may be part of standard library)

---

## Step 14: Create Cross-Language Compatibility Tests

### test/conv-test-$lang/

This project verifies that your backend can produce JSON and UEBA that other backends can read, and vice versa.

**Two components:**

### 1. Compatibility Data Generator

Create a program (`compat_main.$ext`) that:
1. Constructs an `AllBasicTypes` instance with known values
2. Encodes it to JSON and writes to `target/compat-test/$lang-json/all-basic-types.json`
3. Encodes it to UEBA and writes to `target/compat-test/$lang-ueba/all-basic-types.ueba`

The `AllBasicTypes` type is defined in `test/conv-test/pkg02.baboon` with `derived[json], derived[ueba]` annotations. It includes all scalar types, optional, list, set, map, and nested collections.

**The test data must use specific known values** so all languages produce the same logical data:
```
vi8 = some_value, vi16 = some_value, ..., vstr = "Hello, Baboon!", vi32 = 123456, vbit = true, ...
```

Look at existing compat generators (e.g., `test/conv-test-cs/ConvTest/CompatMain.cs` or `test/conv-test-rs/src/main.rs`) for the exact values to use.

### 2. Cross-Language Decode Tests

Create tests that:
1. Read JSON files produced by all other backends from `target/compat-test/{other-lang}-json/`
2. Decode them and verify basic field values
3. Read UEBA files produced by all other backends from `target/compat-test/{other-lang}-ueba/`
4. Decode them and verify basic field values
5. Optionally re-encode and compare

### Project Structure

```
test/conv-test-$lang/
  .gitignore
  $build_file
  src/
    generated/           # Generated from test/conv-test/ models
  tests/
    cross_language_test.$ext
  compat_main.$ext       # or appropriate location for a runnable entry point
```

---

## Step 15: Update Build Infrastructure

### flake.nix

Add your language's toolchain to `devShells.default.nativeBuildInputs`:

```nix
devShells.default = pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [
    # ... existing tools ...
    $lang_package  # e.g., go, swift, kotlin, etc.
  ];
};
```

### .gitignore

Add language-specific entries to the root `.gitignore`:

```
# $LANG
$build_artifacts/
$cache_dirs/
```

Update `safeToRemove` in `Baboon.scala` `mkGenericOpts` to include your file extension:

```scala
val safeToRemove = ... Set("meta", "cs", "json", "scala", "py", "pyc", "rs", "ts", "$ext")
```

### .mdl/defs/tests.md

Add test actions following the existing pattern:

#### test-gen-regular-adt / test-gen-wrapped-adt

Add rsync and compiler invocation for your language:

```bash
# In the rsync section:
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='$build_artifacts' \
  ./test/$lang-stub/ "$TEST_DIR/$lang-stub/"

# In the compiler invocation, add a new target:
  :$lang \
  --output "$TEST_DIR/$lang-stub/src" \
  --test-output "$TEST_DIR/$lang-stub/src" \
  --fixture-output "$TEST_DIR/$lang-stub/src" \
  --$lang-write-evolution-dict=true \
  --$lang-wrapped-adt-branch-codecs={true|false} \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true
```

#### test-$lang-regular / test-$lang-wrapped

```markdown
# action: test-$lang-regular

Run $LANG tests with regular ADT codecs.

\`\`\`bash
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/$lang-stub"
$build_and_test_command
popd

ret success:bool=true
\`\`\`
```

(Same pattern for wrapped variant.)

#### test-gen-manual

Add your language to the compiler invocation:

```bash
  :$lang \
  --output ./test/conv-test-$lang/src/generated
```

#### test-gen-compat-$lang

```markdown
# action: test-gen-compat-$lang

Generate compatibility test files using $LANG.

\`\`\`bash
dep action.test-gen-manual

pushd ./test/conv-test-$lang
$run_compat_main_command
popd

ret success:bool=true
\`\`\`
```

#### test-manual-$lang

```markdown
# action: test-manual-$lang

Run $LANG cross-language compatibility tests.

\`\`\`bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-gen-compat-python
dep action.test-gen-compat-rust
dep action.test-gen-compat-typescript
dep action.test-gen-compat-$lang

pushd ./test/conv-test-$lang
$run_tests_command
popd

ret success:bool=true
\`\`\`
```

#### test (orchestrator)

Add dependencies:
```bash
dep action.test-$lang-regular
dep action.test-$lang-wrapped
dep action.test-manual-$lang
```

### Update existing test-manual-* actions

Each existing `test-manual-*` action must add a dependency on `test-gen-compat-$lang` so they can read your backend's output:

```bash
dep action.test-gen-compat-$lang
```

---

## Type Mapping Reference

Complete type mapping table across all existing backends:

| Baboon | C# | Scala | Rust | TypeScript | Python | Kotlin | Java | Dart |
|--------|-----|-------|------|------------|--------|--------|------|------|
| `bit` | `Boolean` | `Boolean` | `bool` | `boolean` | `bool` | `Boolean` | `boolean` | `bool` |
| `i08` | `SByte` | `Byte` | `i8` | `number` | `int` | `Byte` | `byte` | `int` |
| `i16` | `Int16` | `Short` | `i16` | `number` | `int` | `Short` | `short` | `int` |
| `i32` | `Int32` | `Int` | `i32` | `number` | `int` | `Int` | `int` | `int` |
| `i64` | `Int64` | `Long` | `i64` | `bigint` | `int` | `Long` | `long` | `int` |
| `u08` | `Byte` | `Byte` | `u8` | `number` | `int` | `UByte` | `short` | `int` |
| `u16` | `UInt16` | `Short` | `u16` | `number` | `int` | `UShort` | `int` | `int` |
| `u32` | `UInt32` | `Int` | `u32` | `number` | `int` | `UInt` | `long` | `int` |
| `u64` | `UInt64` | `Long` | `u64` | `bigint` | `int` | `ULong` | `long` | `int` |
| `f32` | `Single` | `Float` | `f32` | `number` | `float` | `Float` | `float` | `double` |
| `f64` | `Double` | `Double` | `f64` | `number` | `float` | `Double` | `double` | `double` |
| `f128` | `Decimal` | `BigDecimal` | `Decimal` | `string` | `Decimal` | `BigDecimal` | `BigDecimal` | `BaboonDecimal` |
| `str` | `String` | `String` | `String` | `string` | `str` | `String` | `String` | `String` |
| `bytes` | `ByteString` | `ByteString` | `Vec<u8>` | `Uint8Array` | `bytes` | `ByteString` | `ByteString` | `Uint8List` |
| `uid` | `Guid` | `UUID` | `Uuid` | `string` | `UUID` | `UUID` | `UUID` | `String` |
| `tsu` | `RpDateTime` | `OffsetDateTime` | `DateTime<Utc>` | `BaboonDateTime` | `datetime` | `OffsetDateTime` | `OffsetDateTime` | `DateTime` |
| `tso` | `RpDateTime` | `OffsetDateTime` | `DateTime<FixedOffset>` | `BaboonDateTime` | `datetime` | `OffsetDateTime` | `OffsetDateTime` | `BaboonDateTimeOffset` |
| `opt[T]` | `T?` / `Nullable<T>` | `Option[T]` | `Option<T>` | `T \| null` | `Optional[T]` | `T?` | `Optional<T>` | `T?` |
| `lst[T]` | `IReadOnlyList<T>` | `List[T]` | `Vec<T>` | `T[]` | `list[T]` | `List<T>` | `List<T>` | `List<T>` |
| `set[T]` | `ImmutableHashSet<T>` | `Set[T]` | `BTreeSet<T>` | `Set<T>` | `set[T]` | `Set<T>` | `Set<T>` | `Set<T>` |
| `map[K,V]` | `IReadOnlyDictionary<K,V>` | `Map[K,V]` | `BTreeMap<K,V>` | `Map<K,V>` | `dict[K,V]` | `Map<K,V>` | `Map<K,V>` | `Map<K,V>` |

---

## UEBA Binary Protocol Reference

### Index Structure

When indexed mode is active (header bit 0 = 1):

```
[header: u8 = 0x01]
[var_field_count: u16]
[index_entry_0: offset(u32) + length(u32)]
[index_entry_1: offset(u32) + length(u32)]
...
[index_entry_N: offset(u32) + length(u32)]
[field_0_data]
[field_1_data]
...
```

Only variable-length fields get index entries. Fixed-size fields (bool, integers, floats) do not.

### Variable-Length Field Detection

A field is variable-length if its type is one of:
- `str`, `bytes`, `f128` (string-encoded)
- `opt[T]` (1 byte + optional payload)
- `lst[T]`, `set[T]`, `map[K,V]` (4-byte count + variable items)
- Any user-defined type (DTO, ADT)
- Any type containing a variable-length type

Fixed-size fields: `bit` (1), `i08`/`u08` (1), `i16`/`u16` (2), `i32`/`u32` (4), `i64`/`u64` (8), `f32` (4), `f64` (8), `uid` (16), `tsu`/`tso` (17).

---

## TextTree Usage Guide

### Basic Usage

```scala
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

// Simple interpolation
val name: TextTree[$LANGValue] = q"MyClass"
val code = q"class $name { }"

// Multi-line with stripMargin
val body = q"""class $name {
              |    val x: Int = 1
              |}""".stripMargin

// Joining sequences
val fields: Seq[TextTree[$LANGValue]] = ???
val joined = fields.join(",\n")
```

### Critical Pitfall: stripMargin and Extracted Variables

`TextTree.stripMargin` only processes `|` characters at **that level** - NOT from interpolated sub-trees.

**Wrong:**
```scala
val innerCode = q"""val x = 1
                   |val y = 2"""  // stripMargin NOT called

val outerCode = q"""class Foo {
                   |    $innerCode
                   |}""".stripMargin
// Result: inner `|` characters are NOT stripped
```

**Correct:**
```scala
val innerCode = q"""val x = 1
                   |val y = 2""".stripMargin

val outerCode = q"""class Foo {
                   |    ${innerCode.shift(4).trim}
                   |}""".stripMargin
```

**Or, extract without stripMargin and use shift/trim:**
```scala
val innerCode = q"""val x = 1
                   |val y = 2"""

val outerCode = q"""class Foo {
                   |    ${innerCode.shift(4).trim}
                   |}""".stripMargin
```

### Rendering

The main translator's `renderTree` method converts TextTree to string:

```scala
def renderTree(output: Output): String = {
  val tree = output.tree
  tree.mapRender {
    case $LANGType(module, name, fq, _) =>
      if (fq) s"${module.parts.mkString(".")}.$name" else name
    case $LANGTypeName(name) => name
  }
}
```

### Type References

Use `$LANGType` (not `$LANGTypeName`) when you want imports to be generated:

```scala
val typeRef: $LANGType = $LANGTypes.tUuid
val code = q"val id: $typeRef = ..."  // Will generate import for Uuid
```

Use `$LANGTypeName` when you don't want an import:

```scala
val name = $LANGTypeName("myVariable")
val code = q"val $name = 42"  // No import generated
```

---

## Service and Pragma Support

For the full wiring architecture — including the container shape abstraction, `IBaboonServiceRt` typeclass generation, the three-step wiring pipeline, error types, and testing strategy with overlay stubs — see [Transport Plumbing](transport-plumbing.md).

### Service Result Configuration

Controls how service method return types are rendered:

```scala
ServiceResultConfig(
  noErrors: Boolean,      // true = bare return type, false = wrapped in result type
  resultType: Option[String],  // e.g., "Either", "Result"
  pattern: Option[String],     // e.g., "[$error, $success]", "<$success, $error>"
  hkt: Option[HktConfig],     // For Scala-like HKT support
)
```

**Rendering:**
- `noErrors = true`: Method returns `OutType` directly (or `Unit` if no output)
- `noErrors = false`: Method returns `ResultType[ErrType, OutType]` using the pattern

Pragmas can override per-domain:
```
pragma $lang.service.result.no-errors = "false"
pragma $lang.service.result.type = "Result"
pragma $lang.service.result.pattern = "<$success, $error>"
```

### Service Context Configuration

Controls whether service methods take a context parameter:

```scala
ServiceContextConfig(
  mode: String,           // "none", "abstract", "type"
  typeName: String,       // e.g., "Ctx" (default)
  parameterName: String,  // e.g., "ctx" (default)
)
```

**Modes:**
- `"none"`: No context parameter on methods
- `"abstract"`: Context is a type parameter on the service trait/interface (e.g., `trait MyService[Ctx]`)
- `"type"`: Context is a concrete type (e.g., methods take `ctx: MyApp.Context`)

Pragmas can override per-domain:
```
pragma $lang.service.context = "abstract"
pragma $lang.service.context.type = "RequestContext"
pragma $lang.service.context.parameter.name = "context"
```

### Pragma Resolution Order

1. CLI `--pragma key=value` flags (highest priority)
2. Domain-level `pragma key = "value"` in `.baboon` files
3. CLI option defaults (lowest priority)

### Registering Known Pragma Keys

Add your language key to `ServiceContextResolver.languages` and `ServiceResultResolver.languages`:

```scala
private val languages = Seq("cs", "python", "rust", "scala", "typescript", "kotlin", "java", "dart", "$lang")
```

This registers all `$lang.service.context.*` and `$lang.service.result.*` pragma keys as known.

---

## Common Pitfalls

### 1. TextTree stripMargin Only Works at Current Level
See the [TextTree Usage Guide](#texttree-usage-guide) section. This is the most common source of formatting bugs.

### 2. CLI Boolean Flags Use Equals Syntax
`--flag false` (space) does NOT work with decline CLI parser. Always use `--flag=false` (equals syntax) for `Option[Boolean]` CLI flags in tests and build scripts.

### 3. UEBA UUID Format is .NET GUID
UUIDs in UEBA binary format use .NET's mixed-endian GUID layout, not standard RFC 4122 big-endian. You must swap bytes in the first three fields.

### 4. sbt May Not Detect Resource Changes
After modifying runtime resource files in `baboon-compiler/src/main/resources/`, run `sbt clean compile` instead of just `sbt compile`. Then rebuild the native image with `mdl :build`.

### 5. f128 JSON Precision
Always serialize `f128`/Decimal as strings in JSON. JavaScript `number` type loses precision beyond 2^53. Python's `float` also loses precision. String representation preserves exact decimal values.

### 6. i64/u64 in JavaScript-Based Languages
JavaScript `number` cannot represent the full `i64`/`u64` range. TypeScript uses `bigint` for these. Consider your language's numeric precision limits.

### 7. f32 Fixture Precision
When generating `f32` fixtures, the value must round-trip through the target language's single-precision float representation. Generate fixtures that survive this round-trip.

### 8. Timestamp Millisecond Precision
All timestamps must be formatted with exactly 3 decimal places for milliseconds: `2024-01-15T10:30:00.000Z`, not `2024-01-15T10:30:00Z`.

### 9. ADT Branch Ownership
ADT branch types have `Owner.Adt` as their owner. When iterating domain members, skip these at the top level - they are generated as part of their parent ADT.

### 10. Recursive Types
Some types may be recursive (e.g., a DTO with a field of its own type). Skip fixture generation for recursive types to avoid infinite loops. The compiler tracks cycles in `Domain.loops`.

### 11. Foreign Type Codecs
Foreign types require manual codec implementations in the target language. Do not generate codecs for them. The generated code must compile without foreign type codec stubs - users provide the implementations.

### 12. Cross-Language Test File Paths
Test file paths for cross-language testing use relative paths like `../../../../../target/cs/json-default/`. The exact depth depends on your stub project's directory structure relative to the repository root. Count the directory levels carefully.

### 13. Wrapped vs Non-Wrapped ADT Branch Codecs
In wrapped mode:
- JSON: `{"BranchName": {"field": "value"}}`
- UEBA: ADT writes branch index, branch writes fields without index

In non-wrapped mode:
- JSON: `{"field": "value"}` (branch determined by structure)
- UEBA: Each branch writes its complete encoding including any branch identification

Both modes must be supported and toggled via `--$lang-wrapped-adt-branch-codecs={true|false}`.

---

## Checklist

Before considering your backend complete, verify:

- [ ] All Baboon scalar types map to native (or well-typed wrapper) types
- [ ] Collections use immutable/ordered types where possible
- [ ] DTOs generate proper data classes with equality and hashing
- [ ] ADTs generate sealed type hierarchies with proper discrimination
- [ ] Enums generate enumeration types with parse/all methods
- [ ] Contracts generate abstract interfaces
- [ ] Services generate interfaces with configurable return types and contexts
- [ ] Foreign types are handled via language-specific bindings
- [ ] JSON codecs produce output compatible with all other backends
- [ ] UEBA codecs produce output compatible with all other backends (especially UUID format)
- [ ] Both compact and indexed UEBA modes work
- [ ] Both wrapped and non-wrapped ADT branch codecs work
- [ ] Fixtures generate valid random data for all non-recursive types
- [ ] Tests verify JSON round-trip, UEBA round-trip (both modes), and cross-language reading
- [ ] Conversions handle all field operations (transfer, default, wrap, expand, swap, rename)
- [ ] Custom conversions generate abstract methods for user implementation
- [ ] Runtime libraries are embedded as JVM resources
- [ ] CLI options are registered and parsed correctly
- [ ] DI module is wired correctly
- [ ] Stub test project compiles and tests pass
- [ ] Cross-language compat tests pass (read/write JSON and UEBA from/to all other backends)
- [ ] Build actions added to `.mdl/defs/tests.md`
- [ ] `flake.nix` updated with language toolchain
- [ ] `.gitignore` updated
- [ ] Existing `test-manual-*` actions depend on `test-gen-compat-$lang`
