# test/services/rs-async-errors

Minimal Cargo project for the Rust ASYNC regular-service wiring reproduction lane
(D28/T96, errors flavour).

## Purpose

This project is used by the `test-gen-rs-wiring-async-errors` mdl action as the base
into which the baboon compiler generates a Rust async petstore service
(`--rs-async-services=true`, `--service-result-no-errors=false`,
`--service-result-type=Result`) from `test/services/petstore-errors.baboon`.

`test-rs-wiring-async-errors` then runs `cargo build` over the generated code. The
build MUST fail with exit 101.

## Status: EXPECTED RED (T96 — gates the fix D28)

The RED was reproduced via the native baboon binary + `cargo 1.91`.

### Captured rustc errors (all in GENERATED `src/`, none in this directory)

Three families of errors appear in the errors-mode async path:

1. `error[E0728]: 'await' is only allowed inside 'async' functions and blocks`
   — `src/petstore/api/pet_store_wiring.rs` × 4 (one per method × json/ueba). The
   errors-mode async invoke body emits
   `rt.flat_map(input, |v| { … impl_.method(v).await … })`, but
   `IBaboonServiceRt::flat_map` takes a SYNC closure returning a `Result`, not a
   future. `.await` inside that sync closure is illegal.
   (RsServiceWiringTranslator.scala:734/747/767 errors path.)

2. `error[E0599]: no method named 'clone' found for type parameter 'Impl'/'Rt'`
   — `src/petstore/api/pet_store_wiring.rs` muxer wrappers (`PetStoreJsonService` /
   `PetStoreUebaService`): the async `invoke` clones `impl_`/`rt` into a
   `Box::pin(async move { … })` but the generic bounds omit `Clone`.
   (RsServiceWiringTranslator.scala:203–215 async muxer wrapper path.)

3. `error[E0277]: '?' couldn't convert the error: 'dyn StdError + Send + Sync: Sized'
   is not satisfied`
   — `src/petstore/api/pet_store_client.rs`: the async client applies `?` on a
   `Result<_, Box<dyn StdError + Send + Sync>>` but the trait bound is unsatisfied
   for the unsized `dyn` type.
   (RsServiceWiringTranslator.scala async client path.)
