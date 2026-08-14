//! Umbrella binary for the repo-inspection policy and budget gates.
//!
//! These members link no `rumoca` library, so this binary stays a few MB and
//! `cargo xtask verify quick` can run the fast gates without paying for a
//! whole-compiler link. Keep it that way: a member that needs the compiler
//! belongs in `suite_core.rs`, which also documents the grouping rules.
//!
//! Two members (`galec_review_surface`, `rdd2_perf_guard`) are RDD2 metric
//! gates behind the `rdd2-metric-gates` feature: they drive the RELEASE
//! `rumoca` binary as a subprocess and measure what it produced, which needs a
//! release build plus the out-of-tree `modelica_models` library. A feature is
//! how this repo gates heavy suites, and it also keeps them out of the default
//! binary entirely. They still link no `rumoca` library, so they belong here
//! rather than in `suite_core`. Run them with:
//!
//! ```text
//! cargo build --release -p rumoca
//! cargo test -p rumoca --features rdd2-metric-gates --test suite_gates
//! ```

#[path = "code_size_budget_test.rs"]
mod code_size_budget_test;
#[path = "dae_loc_trigger_test.rs"]
mod dae_loc_trigger_test;
// Pure-text metrics over emitted C, plus their unit tests. Ungated on purpose:
// they need no corpus and no release binary, so gating them would mean the
// detectors' own correctness tests compiled only under a feature nothing in CI
// runs. See the module docs.
#[path = "rdd2_metric_support/text_metrics.rs"]
mod rdd2_text_metrics;

#[cfg(feature = "rdd2-metric-gates")]
#[path = "rdd2_metric_support/environment.rs"]
mod rdd2_gate_environment;

#[cfg(feature = "rdd2-metric-gates")]
#[path = "galec_review_surface.rs"]
mod galec_review_surface;
#[path = "history_policy_test.rs"]
mod history_policy_test;
#[path = "mls_formalization_manifest_test.rs"]
mod mls_formalization_manifest_test;
#[cfg(feature = "rdd2-metric-gates")]
#[path = "rdd2_perf_guard.rs"]
mod rdd2_perf_guard;
#[path = "spec_budget_test.rs"]
mod spec_budget_test;
