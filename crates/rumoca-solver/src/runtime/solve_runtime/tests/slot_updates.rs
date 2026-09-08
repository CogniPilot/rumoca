//! Discrete storage update regressions.

use super::*;

#[test]
fn apply_discrete_slot_value_reports_out_of_bounds_target() {
    let mut y = [0.0];
    let mut p = [];

    let err = crate::apply_discrete_slot_value(
        solve::ScalarSlot::Y { index: 2 },
        1.0,
        &mut y,
        &mut p,
        1e-12,
    )
    .expect_err("out-of-bounds discrete target must be reported");

    assert_eq!(
        err,
        EvalSolveError::MissingInput {
            vector: "y",
            index: 2,
            len: 1,
            span: None,
        }
    );
}
