# test/services/rs-async

Minimal Cargo project for the Rust ASYNC regular-service wiring lane
(D28/T96/T97, no-errors flavour).

## Purpose

This project is used by the `test-gen-rs-wiring-async` mdl action as the base into
which the baboon compiler generates a Rust async petstore service
(`--rs-async-services=true`, `--service-result-no-errors=true`) from
`test/services/petstore.baboon`.

`test-rs-wiring-async` then runs `RUSTFLAGS="-D warnings" cargo build` over the
generated code and asserts it exits 0.

## Status: GREEN (fix landed — T97/D28)

T97 fixed the Rust async service wiring generator
(`RsServiceWiringTranslator.scala`): Clone bounds are now emitted on muxer generic
parameters (`Impl: Clone`, `Rt: Clone`), and the async client error type is now
`Box<dyn std::error::Error + Send + Sync>` (which `?` can propagate without the
`Sized` constraint issue). `cargo build` exits 0 under `-D warnings`.

### Historical RED (pre-T97)

Before the fix this lane was an EXPECTED RED reproduction gating D28. Two families
of errors appeared:

1. `error[E0599]: no method named 'clone' found for type parameter 'Impl'`
   — muxer wrappers cloned generics into `Box::pin(async move { … })` without
   `Clone` bounds.

2. `error[E0277]: '?' couldn't convert the error: 'dyn StdError + Send + Sync: Sized'
   is not satisfied` — async client applied `?` on an unsized boxed-dyn error.
