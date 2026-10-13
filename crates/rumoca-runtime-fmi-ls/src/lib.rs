//! Fixed WIT adapter over the privately received, checked native component.

#[cfg(all(target_arch = "wasm32", target_os = "wasi", target_env = "p2"))]
mod adapter;
#[cfg(all(target_arch = "wasm32", target_os = "wasi", target_env = "p2"))]
mod bootstrap;
#[cfg(all(target_arch = "wasm32", target_os = "wasi", target_env = "p2"))]
mod wit_exports;

mod resource;

pub use resource::{FmiLsResource, FmiLsResourceError, FmiLsStepResult};

#[cfg(all(target_arch = "wasm32", target_os = "wasi", target_env = "p2"))]
pub mod bindings {
    wit_bindgen::generate!({
        world: "co-simulation-fmu",
        path: "../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit",
    });
}
