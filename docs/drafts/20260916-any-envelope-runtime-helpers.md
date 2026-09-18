# Scala and Kotlin any-envelope runtime helpers

Generated Scala and Kotlin codecs delegate any-field framing and JSON envelopes to `BaboonAnyBinCodec` and `BaboonAnyJsonCodec`. These runtime APIs are additive; existing generated code can continue using its private inline helpers with the updated runtime.

When runtime generation is disabled, update the separately maintained runtime distribution together with the compiler. New generated codecs require these helpers and are not supported with an older runtime lacking them. Runtime-disabled generation still emits no runtime files. No reflection or silent fallback to an older runtime is introduced.

Kotlin JVM and KMP share the JSON helper source. Their binary helpers retain distinct stream APIs and metadata buffering. JSON-disabled Kotlin output omits the JSON helper and strips the JSON-to-binary conversion branch from the binary helper, matching the existing runtime JSON-section mechanism. Scala retains its existing Circe runtime dependency even when JSON codec generation is disabled.

The helper move preserves the six meta-kind variants, static fallback precedence, cross-format facade requirements, metadata-extension skipping and existing error behavior. It does not standardize malformed-input handling between Scala and Kotlin. Public runtime-envelope tests cover exact binary bytes, JSON content, extension skipping and errors; existing generated `AnyRoundTrip` suites remain required for all six variants, nested containers and registered-facade conversions.
