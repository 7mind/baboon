# Enum Wire-Format Spec (PR-35, 2026-04-28)

## Decision

**Pascal-case canonical wire format for enum values across all 9 backends.**

For an enum member with source name `m`, the JSON wire form is `m.capitalize`
(first character uppercased, rest of the string unchanged). Decoders must
accept exact match against the emitted form — no case-insensitive
permissiveness.

UEBA encodes enum values as ordinal bytes; this spec governs only the JSON
wire and the in-memory enum identifier in target languages. The two are
deliberately aligned so that string-based encoders (e.g. Dart's `value.name`,
Swift's `rawValue`) yield the same form as Pascal-emitting languages
(Scala's `toString`, Java's `name()`, Kotlin's `.name`, C#'s `ToString()`,
Rust's serde-default, Python's pydantic enum value).

## Reasoning

Six of nine backends already emit Pascal naturally. The cheapest path to
cross-language interop is to align the remaining three (TypeScript, Dart,
Swift) with the dominant convention, rather than force the six to override
their language defaults.

Cross-language interop is the critical correctness invariant: a fixture
written by Scala must round-trip through Dart, Swift, and TypeScript decoders
unchanged. Before this spec, JSON enum values diverged for any non-Pascal
source name (e.g. `cafe` produced by source code became `Cafe` from one set
of backends and `cafe` from another).

## Migration Implications

Existing user models with non-Pascal-case enum members will see a wire-format
change in the three previously-raw backends (TS, Dart, Swift). This is
consistent with what the other six backends were already producing, so any
model that successfully round-tripped between (Scala, C#, Java, Kotlin,
Python, Rust) and was *not* tested with TS/Dart/Swift would be unaffected.
Models that included TS/Dart/Swift on the wire-producer side and relied on
the raw form must regenerate consumers.

**C# and JVM runtime codec wire-format break:** Pre-PR-35, C# decoders accepted case-insensitive matches (`Enum.TryParse(s, true, ...)`) and the JVM runtime codec normalised inputs to lowercase before lookup. Both now require exact Pascal-case matches. Clients sending non-Pascal-case enum strings (e.g. `"cafe"`, `"CAFE"`, `"CaFe"`) to C# or JVM-runtime servers will be rejected post-upgrade.

The repository's only cross-language fixture using non-Pascal-case enum
members is `T_NsPascal { cafe; bar_pub }` in
`baboon-compiler/src/test/resources/baboon/pkg0/pkg03.baboon`. It was
removed in PR-29 because of the divergence; PR-35 reinstates it as a
regression lock.

## Per-Backend Impact Summary

| Backend | JSON encode site | Decoder site | Change |
|---|---|---|---|
| Scala | `value.toString` (case-object name from `m.name.capitalize`) | companion `parse(...)` exact match | none |
| Kotlin | `instance.name` (Pascal enum class case) | companion `parse(s)` map exact match | none |
| C# | `value.ToString()` (Pascal enum) | `Enum.TryParse(asStr, true, ...)` | tighten — drop `true`, drop `?.ToLower()` |
| Java | `value.name()` (Pascal enum) | static `parse(s)` switch exact match | none |
| Python | `value.value` (Pascal pydantic enum value) | `T(json.loads(wire))` | none |
| Rust | serde derive default (Pascal variant ident) | serde derive default | none |
| TypeScript | `value` (string-typed enum) | `T_parse(s)` | DefnTranslator must emit Pascal string values; UEBA arms re-aligned |
| Dart | `value.name` (Dart enum case ident) | `T.parse(s)` | DefnTranslator must emit capitalized cases; UEBA arms follow |
| Swift | `$ref.rawValue` (raw-string-backed Swift enum) | `T(rawValue: s)` | DefnTranslator must emit capitalized cases + capitalized rawValues |

## Shared Helper API

```scala
package io.septimalmind.baboon.typer

object EnumWireStyle {
  /** Pascal-case wire-format string for an enum member name.
   *  First character uppercased, remainder unchanged.
   */
  def wireName(memberName: String): String = memberName.capitalize
}
```

A function and not a configurable strategy: the spec mandates Pascal as the
sole canonical form. The `TsOptions.enumLowercaseValues` flag is retained
as a backward-compat escape hatch for already-deployed TypeScript clients;
when set, both UEBA codec arms and the DefnTranslator use lowercase. It is
not the canonical form and is not honored by other backends.

If a future migration ever wants a different style, the helper grows a
`Style` parameter; today, simplicity wins.
