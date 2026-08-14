//! Umbrella binary for the suites gated behind `msl-sim-tests`, which simulate
//! the pinned Modelica Standard Library tree. See `suite_core.rs` for the
//! grouping rules.

#[path = "msl_sim_regression.rs"]
mod msl_sim_regression;
