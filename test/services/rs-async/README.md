# test/services/rs-async

Minimal Cargo project for the Rust ASYNC regular-service wiring reproduction lane
(D28/T96, no-errors flavour).

## Purpose

This project is used by the `test-gen-rs-wiring-async` mdl action as the base into
which the baboon compiler generates a Rust async petstore service
(`--rs-async-services=true`, `--service-result-no-errors=true`) from
`test/services/petstore.baboon`.

`test-rs-wiring-async` then runs `cargo build` over the generated code. The build
MUST fail with exit 101.

## Status: EXPECTED RED (T96 — gates the fix D28)

The RED was reproduced via the native baboon binary + `cargo 1.91`.

### Captured rustc errors (all in GENERATED `src/`, none in this directory)

Two families of errors appear in the no-errors async path:

1. `error[E0599]: no method named 'clone' found for type parameter 'Impl'`
   — `src/petstore/api/pet_store_wiring.rs` muxer wrappers (`PetStoreJsonService` /
   `PetStoreUebaService`): the async `invoke` clones `impl_` into a
   `Box::pin(async move { … })` but the generic bound omits `Clone`.
   (RsServiceWiringTranslator.scala:203–215 async muxer wrapper path.)

2. `error[E0277]: '?' couldn't convert the error: 'dyn StdError + Send + Sync: Sized'
   is not satisfied`
   — `src/petstore/api/pet_store_client.rs`: the async client applies `?` on a
   `Result<_, Box<dyn StdError + Send + Sync>>` but `Box<dyn StdError + Send + Sync>`
   does not implement `From<Box<dyn StdError + Send + Sync>>` because
   `dyn StdError + Send + Sync` is not `Sized`.
   (RsServiceWiringTranslator.scala async client path.)
