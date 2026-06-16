# test/services/rs-async-errors

Minimal Cargo project for the Rust ASYNC regular-service wiring lane
(D28/T96/T97, errors flavour).

## Purpose

This project is used by the `test-gen-rs-wiring-async-errors` mdl action as the base
into which the baboon compiler generates a Rust async petstore service
(`--rs-async-services=true`, `--service-result-no-errors=false`,
`--service-result-type=Result`) from `test/services/petstore-errors.baboon`.

`test-rs-wiring-async-errors` then runs `RUSTFLAGS="-D warnings" cargo build` over
the generated code and asserts it exits 0.

## Status: GREEN (fix landed — T97/D28)

T97 fixed the Rust async errors-mode service wiring generator
(`RsServiceWiringTranslator.scala`): the errors-mode async invoke body now uses
`async move` closures (resolving E0728), Clone bounds are emitted on muxer generics
(resolving E0599), and the async client error type is correctly sized (resolving
E0277). `cargo build` exits 0 under `-D warnings`.

### Historical RED (pre-T97)

Before the fix this lane was an EXPECTED RED reproduction gating D28. Three families
of errors appeared:

1. `error[E0728]: 'await' is only allowed inside 'async' functions and blocks`
   — the errors-mode async invoke body used `.await` inside a sync `FnOnce` closure
   passed to `IBaboonServiceRt::flat_map`.

2. `error[E0599]: no method named 'clone' found for type parameter 'Impl'/'Rt'`
   — muxer wrappers cloned generics into `Box::pin(async move { … })` without
   `Clone` bounds.

3. `error[E0277]: '?' couldn't convert the error: 'dyn StdError + Send + Sync: Sized'
   is not satisfied` — async client applied `?` on an unsized boxed-dyn error.
