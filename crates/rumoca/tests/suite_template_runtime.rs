//! Umbrella binary for the suites gated behind `template-runtime-tests`, which
//! render and execute generated target code. See `suite_core.rs` for the
//! grouping rules.
//!
//! `xtask verify template-runtimes` drives this target per backend and selects
//! each member with a module-prefixed libtest filter, so the external-toolchain
//! groups stay separable.

#[path = "backend_template_runtime_regression.rs"]
mod backend_template_runtime_regression;
#[path = "codegen_example_regression.rs"]
mod codegen_example_regression;
#[path = "template_target_ci.rs"]
mod template_target_ci;
