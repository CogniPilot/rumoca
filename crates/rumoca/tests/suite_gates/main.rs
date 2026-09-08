//! Umbrella binary for the repo-inspection policy and budget gates.
//!
//! These members link no `rumoca` library, so this binary stays a few MB and
//! `cargo xtask verify quick` can run the fast gates without paying for a
//! whole-compiler link. Keep it that way: a member that needs the compiler
//! belongs in `suite_core/main.rs`, which also documents the grouping rules.
//!
//! The `rdd2_perf_guard` member is behind the `rdd2-metric-gates` feature: it
//! drives the RELEASE `rumoca` binary as a subprocess, which needs a release
//! build plus the out-of-tree `modelica_models` library. A feature is how this
//! repo gates heavy suites, and it also keeps the guard out of the default
//! binary entirely. It still links no `rumoca` library, so it belongs here
//! rather than in `suite_core`. Run it with:
//!
//! ```text
//! cargo build --release -p rumoca
//! cargo test -p rumoca --features rdd2-metric-gates --test suite_gates
//! ```

mod code_size_budget_test;
mod dae_loc_trigger_test;
// Pure-text metrics over emitted C, plus their unit tests. Ungated on purpose:
// they need no corpus and no release binary, so gating them would mean the
// detectors' own correctness tests compiled only under a feature nothing in CI
// runs. See the module docs.
mod rdd2_text_metrics;

#[cfg(feature = "rdd2-metric-gates")]
mod rdd2_gate_environment;

#[cfg(feature = "rdd2-metric-gates")]
mod rdd2_perf_guard;
mod spec_budget_test;
