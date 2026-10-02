//! Umbrella binary for the suites gated behind `msl-sim-tests`, which simulate
//! the pinned Modelica Standard Library tree. See `suite_core/main.rs` for the
//! grouping rules.

mod fourbar_connect_order;
mod move_supplied_derivative;
mod msl_sim_regression;
mod selected_medium_through_alias_chain;
mod state_jacobian_oracle;
