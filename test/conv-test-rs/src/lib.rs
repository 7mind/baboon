#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

#[path = "generated/baboon_runtime.rs"]
pub mod baboon_runtime;

// Required by generated `any_showcase.rs`, which references `crate::any_opaque::AnyOpaque` and the
// facade. These modules are emitted into ./generated/ but were not previously routed into the
// crate root because no conv-test type referenced `any` until M13 (PR 13.1).
#[path = "generated/any_opaque.rs"]
pub mod any_opaque;

#[path = "generated/baboon_codecs_facade.rs"]
pub mod baboon_codecs_facade;

#[path = "generated/convtest/mod.rs"]
pub mod convtest;
