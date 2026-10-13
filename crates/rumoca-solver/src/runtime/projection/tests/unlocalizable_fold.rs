//! SPEC_0044 ME-EVENT-008: an unsettled row of a block that a `noEvent`
//! relation switches on its own unknowns is the typed ES016 fold, and only
//! such a block's row is.

use super::*;

fn plan() -> solve::AlgebraicProjectionPlan {
    solve::AlgebraicProjectionPlan {
        blocks: (vec![
            solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![2],
                ..Default::default()
            },
            solve::AlgebraicProjectionBlock {
                rows: vec![1, 2],
                y_indices: vec![3, 4],
                ..Default::default()
            },
        ])
        .into(),
    }
}

fn guards() -> Vec<solve::UnlocalizableGuard> {
    vec![solve::UnlocalizableGuard {
        y_indices: vec![3, 4],
        relation: "v_in < 0".to_string(),
        unknown_names: "`v_in`, `v_out`".to_string(),
    }]
}

fn fold_at(guards: &[solve::UnlocalizableGuard], residual: &[f64]) -> Option<RuntimeSolveError> {
    let args = AlgebraicProjectionArgs {
        parameters: &[],
        time: 0.25,
        state_count: 2,
        tolerance: 1.0e-9,
    };
    unlocalizable_fold(guards, &plan(), &[0, 1, 2], (residual, &[1.0; 3]), args)
}

#[test]
fn an_unsettled_row_of_a_switched_block_is_the_typed_fold() {
    let Some(RuntimeSolveError::UnlocalizableFold { fold, time }) =
        fold_at(&guards(), &[0.0, 0.0, 1.0])
    else {
        panic!("the switched block's unsettled row is the ES016 fold");
    };
    assert_eq!(time, 0.25);
    assert!(fold.contains("`v_in < 0`"), "{fold}");
    assert!(fold_at(&guards(), &[0.0, f64::NAN, 0.0]).is_some());
}

#[test]
fn other_blocks_and_settled_rows_are_not_a_fold() {
    assert!(fold_at(&guards(), &[1.0, 0.0, 0.0]).is_none());
    assert!(fold_at(&guards(), &[0.0; 3]).is_none());
    assert!(fold_at(&[], &[0.0, 0.0, 1.0]).is_none());
}
