//! Heavy Solve-lowering regressions, gated behind `heavy-solve-tests`.
//!
//! These guards exercise models whose checked DAEs currently scalarize into
//! very large Solve IR (one `log_map` call produces ~139 MB) and therefore
//! run for minutes rather than failing fast. They stay out of the default
//! matrix until the compact Solve function-fold owner from the SPEC 0036
//! checklist lands, at which point they should return to `suite_core` as
//! fast-failing budget assertions.
//!
//! Run with: `cargo test -p rumoca --features heavy-solve-tests --test suite_heavy_solve`

#[path = "quadrotor_se23_regression_test.rs"]
mod quadrotor_se23_regression_test;
