//! Fixed WIT adapter over the privately received, checked native component.

#[cfg(all(target_arch = "wasm32", target_os = "wasi", target_env = "p2"))]
mod adapter;
#[cfg(all(target_arch = "wasm32", target_os = "wasi", target_env = "p2"))]
mod bootstrap;
#[cfg(all(target_arch = "wasm32", target_os = "wasi", target_env = "p2"))]
mod wit_exports;

mod resource;

pub use resource::{FmiLsResource, FmiLsResourceError, FmiLsStepResult};

#[cfg(all(
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2",
    feature = "preinit",
    feature = "certify-linked"
))]
compile_error!("the snapshot and the certification build worlds are exclusive");

#[cfg(all(
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2",
    not(feature = "preinit"),
    not(feature = "certify-linked")
))]
pub mod bindings {
    wit_bindgen::generate!({
        world: "co-simulation-fmu",
        path: "../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit",
    });
}

#[cfg(all(
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2",
    feature = "preinit"
))]
pub mod bindings {
    wit_bindgen::generate!({
        world: "rumoca:preinit/co-simulation-fmu-preinit",
        path: [
            "../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit",
            "src/preinit",
        ],
    });
}

#[cfg(all(
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2",
    feature = "certify-linked"
))]
pub mod bindings {
    wit_bindgen::generate!({
        world: "rumoca:certify/co-simulation-fmu-certify",
        path: [
            "../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit",
            "src/certify",
        ],
    });
}
