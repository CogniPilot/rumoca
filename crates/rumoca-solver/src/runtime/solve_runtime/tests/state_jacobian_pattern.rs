//! Issue #365: the Solve IR state-Jacobian relation is the derivative
//! JVP's proven pattern closed through the algebraic projection, so a banded
//! model colors as banded instead of as a dense matrix.

use super::*;

fn load_seed(dst: u32, index: usize) -> solve::LinearOp {
    solve::LinearOp::LoadSeed { dst, index }
}

fn difference(lhs: usize, rhs: usize) -> Vec<solve::LinearOp> {
    vec![
        load_seed(0, lhs),
        load_seed(1, rhs),
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn seed(index: usize) -> Vec<solve::LinearOp> {
    vec![load_seed(0, index), solve::LinearOp::StoreOutput { src: 0 }]
}

/// `der(x0) = x1`, `der(x1) = z`, `der(x2) = x2 - x1`, `z = x0` with states
/// `x0..x2` (solver 0..2) and the algebraic `z` (solver 3).
fn chained_model() -> solve::SolveModel {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: solve::VarLayout::from_parts(IndexMap::new(), 4, 0),
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: ["x0", "x1", "x2", "z"].map(str::to_string).to_vec(),
                    ..Default::default()
                },
                state_scalar_count: 3,
                algebraic_scalar_count: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        derivative_placeholder_row(0),
                        derivative_placeholder_row(1),
                        derivative_placeholder_row(2),
                        shifted_variable_residual_row(3, 0.0),
                    ],
                    "state_jacobian_implicit.mo",
                )),
                implicit_row_targets: (0..4)
                    .map(|index| Some(solve::scalar_slot_y(index)))
                    .collect(),
                derivative_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        derivative_placeholder_row(1),
                        derivative_placeholder_row(3),
                        derivative_placeholder_row(2),
                    ],
                    "state_jacobian_derivative.mo",
                )),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![3],
                        y_indices: vec![3],
                        tearing: None,
                        alternate_charts: Vec::new(),
                    }],
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0; 4],
        ..Default::default()
    };
    set_test_implicit_jvp(
        &mut model,
        vec![seed(0), seed(1), seed(2), difference(3, 0)],
        "state_jacobian_implicit_jvp.mo",
    );
    model.artifacts.continuous.full_jacobian_v = spanned_block(
        vec![seed(1), seed(3), difference(2, 1)],
        "state_jacobian_derivative_jvp.mo",
    );
    derive_test_structural_artifacts(&mut model);
    model
}

/// Rows per state column of the Solve IR's certified state-Jacobian relation.
fn state_jacobian_columns(model: &solve::SolveModel) -> Vec<Vec<usize>> {
    model
        .artifacts
        .continuous
        .structural
        .state_jacobian()
        .expect("the derivative JVP derives a state-Jacobian relation")
        .column_rows()
}

#[test]
fn state_jacobian_columns_close_algebraic_columns_over_their_projection_block() {
    let columns = state_jacobian_columns(&chained_model());
    // d(der)/dx0 reaches der(x1) only through z; dx1 reaches der(x0) and
    // der(x2); dx2 reaches der(x2).
    assert_eq!(&*columns, &[vec![1], vec![0, 2], vec![2]]);
}

#[test]
fn an_algebraic_no_projection_block_solves_is_refused_at_construction() {
    let mut model = chained_model();
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan::default();
    let error =
        match solve_eval::derive_solve_structural_artifacts(&model.problem, &model.artifacts) {
            Ok(_) => panic!("an unsolved algebraic column has no certified state dependency"),
            Err(error) => error.to_string(),
        };
    assert!(
        error.contains("solver column 3 is an algebraic no projection block solves"),
        "{error}"
    );
}
