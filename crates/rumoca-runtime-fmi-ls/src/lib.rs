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
        // The world lives in `rumoca:certify` and includes the `fmi:fmi3`
        // world, so its interfaces belong to another package: generate them
        // here, at the same paths the published bindings use.
        generate_all,
    });
}

/// A published product loads without re-deriving its kernels because the
/// certifying core re-derived them at build. That certifies the published
/// core only while `certify-linked` changes nothing else either core builds.
#[cfg(test)]
#[test]
fn the_certify_feature_enables_no_other_feature() {
    let manifest = include_str!("../Cargo.toml");
    assert!(
        manifest.lines().any(|line| line.trim() == "certify-linked = []"),
        "certify-linked must stay an empty feature"
    );
}
