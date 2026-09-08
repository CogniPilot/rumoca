//! Umbrella binary for the suites gated behind `template-runtime-tests`, which
//! render and execute generated target code. See `suite_core/main.rs` for the
//! grouping rules.
//!
//! `xtask verify template-runtimes` drives this target per backend and selects
//! each member with a module-prefixed libtest filter, so the external-toolchain
//! groups stay separable.

mod artifact_session;
mod backend_template_runtime_regression;
#[cfg(feature = "fmu-packaging")]
mod cli_target_fmi;
mod codegen_example_regression;
#[cfg(feature = "fmu-packaging")]
mod fmi_ls_dae_contract;
#[cfg(feature = "fmu-packaging")]
mod fmi_ls_wasm_runtime;
mod modelica_interchange_runtime;
mod template_runtime_policy;
mod template_target_ci;
