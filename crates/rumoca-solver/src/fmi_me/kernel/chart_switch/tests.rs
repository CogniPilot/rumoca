//! The Event-Mode basis change on a synthetic two-chart plan (SPEC_0040
//! STRUCT-T07 constraint-fold chart rows): a failed transfer changes nothing,
//! a saved FMU state restores with an alternate that was never built, and a
//! state that integrates another source reports its nominal change.
//!
//! The fixture has one generated state `x` and algebraic coordinates `q` and
//! `a` with `a = q + 1`. The primary chart integrates `q` (`x - q = 0`) and
//! reconstructs `a`; the alternate integrates `a` (`x - a = 0`) and
//! reconstructs `q` from its second row.

use super::*;
use crate::fmi_me::{MeInstanceConfig, MeModelSource};
use rumoca_ir_solve as solve;
use solve::BinaryOp::{Add, Mul, Sub};
use solve::LinearOp::{Binary, Const, LoadSeed, LoadY, StoreOutput};

fn block(rows: Vec<Vec<solve::LinearOp>>, name: &'static str) -> solve::ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(name), 1, 2);
    solve::ScalarProgramBlock::with_source_span(
        rows,
        span.require_provenance("chart switch fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

/// `y[lhs] - y[rhs]`, and its tangent `seed[lhs] - seed[rhs]`.
fn difference(lhs: usize, rhs: usize, tangent: bool) -> Vec<solve::LinearOp> {
    let load = |dst, index| match tangent {
        true => LoadSeed { dst, index },
        false => LoadY { dst, index },
    };
    vec![
        load(0, lhs),
        load(1, rhs),
        Binary {
            dst: 2,
            op: Sub,
            lhs: 0,
            rhs: 1,
        },
        StoreOutput { src: 2 },
    ]
}

/// `a - q - 1`, whose tangent is `difference(2, 1)`.
fn offset_row() -> Vec<solve::LinearOp> {
    let mut row = difference(2, 1, false);
    row.pop();
    row.extend([
        Const { dst: 3, value: 1.0 },
        Binary {
            dst: 4,
            op: Sub,
            lhs: 2,
            rhs: 3,
        },
        StoreOutput { src: 4 },
    ]);
    row
}

/// `q*q + 1`, which no real `q` solves, and its tangent `2*q*dq`.
fn unsolvable_rows() -> (Vec<solve::LinearOp>, Vec<solve::LinearOp>) {
    let row = vec![
        LoadY { dst: 0, index: 1 },
        Binary {
            dst: 1,
            op: Mul,
            lhs: 0,
            rhs: 0,
        },
        Const { dst: 2, value: 1.0 },
        Binary {
            dst: 3,
            op: Add,
            lhs: 1,
            rhs: 2,
        },
        StoreOutput { src: 3 },
    ];
    let tangent = vec![
        LoadY { dst: 0, index: 1 },
        LoadSeed { dst: 1, index: 1 },
        Binary {
            dst: 2,
            op: Mul,
            lhs: 0,
            rhs: 1,
        },
        Const { dst: 3, value: 2.0 },
        Binary {
            dst: 4,
            op: Mul,
            lhs: 2,
            rhs: 3,
        },
        StoreOutput { src: 4 },
    ];
    (row, tangent)
}

/// One basis: its residual rows, their tangents, row targets, and blocks.
struct Basis {
    rows: Vec<Vec<solve::LinearOp>>,
    tangents: Vec<Vec<solve::LinearOp>>,
    targets: [usize; 2],
    blocks: [(usize, usize); 2],
}

fn primary_basis() -> Basis {
    Basis {
        rows: vec![difference(0, 1, false), offset_row()],
        tangents: vec![difference(0, 1, true), difference(2, 1, true)],
        targets: [1, 2],
        blocks: [(0, 1), (1, 2)],
    }
}

fn alternate_basis(solvable: bool) -> Basis {
    let (row, tangent) = match solvable {
        true => (offset_row(), difference(2, 1, true)),
        false => unsolvable_rows(),
    };
    Basis {
        rows: vec![difference(0, 2, false), row],
        tangents: vec![difference(0, 2, true), tangent],
        targets: [2, 1],
        blocks: [(0, 2), (1, 1)],
    }
}

/// `problem` executing `basis`, with its artifacts and refresh owners.
fn lowered(
    problem: &solve::SolveProblem,
    basis: &Basis,
) -> (solve::SolveProblem, solve::ContinuousSolveArtifacts) {
    let mut problem = problem.clone();
    let continuous = &mut problem.continuous;
    continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(block(basis.rows.clone(), "chart.mo"));
    continuous.implicit_row_targets = basis
        .targets
        .iter()
        .map(|&index| Some(solve::scalar_slot_y(index)))
        .collect();
    continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: basis
            .blocks
            .iter()
            .map(|&(row, y)| solve::AlgebraicProjectionBlock {
                rows: vec![row],
                y_indices: vec![y],
                tearing: None,
                alternate_charts: Vec::new(),
            })
            .collect(),
    };
    let tangents = block(basis.tangents.clone(), "chart_jvp.mo");
    let mut artifacts = solve::SolveArtifacts {
        continuous: solve::ContinuousSolveArtifacts {
            implicit_jacobian_v: solve::ComputeBlock::from_scalar_program_block(tangents.clone()),
            implicit_jacobian_v_scalar: tangents,
            full_jacobian_v: block(
                vec![vec![Const { dst: 0, value: 0.0 }, StoreOutput { src: 0 }]],
                "chart_der_jvp.mo",
            ),
            ..Default::default()
        },
        ..Default::default()
    };
    let (structural, _) =
        rumoca_eval_solve::derive_solve_structural_artifacts(&problem, &artifacts)
            .expect("fixture structural artifacts derive");
    artifacts.continuous.structural = structural;
    problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut problem)
            .expect("fixture refresh owners construct");
    (problem, artifacts.continuous)
}

/// The two-chart model; `a` carries a nominal ten times `q`'s.
fn two_chart_model(solvable_alternate: bool) -> solve::SolveModel {
    let skeleton = solve::SolveProblem {
        solve_layout: solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                names: vec!["x".to_string(), "q".to_string(), "a".to_string()],
                ..Default::default()
            },
            state_scalar_count: 1,
            algebraic_scalar_count: 2,
            ..Default::default()
        },
        continuous: solve::ContinuousSolveSystem {
            derivative_rhs: solve::ComputeBlock::from_scalar_program_block(block(
                vec![vec![Const { dst: 0, value: 1.0 }, StoreOutput { src: 0 }]],
                "chart_der.mo",
            )),
            ..Default::default()
        },
        ..Default::default()
    };
    let (mut problem, artifacts) = lowered(&skeleton, &primary_basis());
    let (alternate, alternate_artifacts) = lowered(&skeleton, &alternate_basis(solvable_alternate));
    let chart = |independent: usize, dependent: usize, plan| solve::ReducedChart {
        independent_y_indices: vec![independent],
        dependent_y_indices: vec![dependent],
        trial_rcond: 1.0,
        trial_singular_threshold: 1.0e-15,
        plan,
    };
    let continuous = alternate.continuous;
    problem.continuous.reduced_chart_set.charts = vec![
        chart(1, 2, None),
        chart(
            2,
            1,
            Some(solve::ReducedChartPlan {
                residual: continuous.residual,
                implicit_rhs: continuous.implicit_rhs,
                implicit_row_targets: continuous.implicit_row_targets,
                algebraic_projection_plan: continuous.algebraic_projection_plan,
                derivative_rhs: continuous.derivative_rhs,
                refresh_owners: continuous.refresh_owners,
                artifacts: alternate_artifacts,
                delta: None,
            }),
        ),
    ];
    solve::SolveModel {
        problem,
        artifacts: solve::SolveArtifacts {
            continuous: artifacts,
            ..Default::default()
        },
        initial_y: vec![0.5, 0.5, 1.5],
        solver_nominals: vec![1.0, 1.0, 10.0],
        visible_names: vec!["x".to_string(), "q".to_string(), "a".to_string()],
        ..Default::default()
    }
}

fn instantiate(model: &solve::SolveModel) -> SolveMeKernel {
    SolveMeKernel::instantiate(
        MeModelSource::fixture(model),
        &MeInstanceConfig::new("chart-switch", 1.0e-6, 0.0, 1.0)
            .expect("chart switch configuration constructs"),
    )
    .expect("two-chart fixture instantiates")
}

/// Latch a change to the alternate from the current physical coordinate.
fn latch_alternate(kernel: &mut SolveMeKernel) {
    let physical = kernel.solver_y_guess.borrow().clone();
    kernel.pending_basis_change = Some(dynamic_chart::PendingBasisChange {
        target: 1,
        physical_solver_y: physical,
        target_reference: 1.0,
    });
}

fn nominal(kernel: &SolveMeKernel) -> f64 {
    let mut nominals = [f64::NAN];
    kernel
        .get_nominals_of_continuous_states(&mut nominals)
        .expect("one nominal per state");
    nominals[0]
}

#[test]
fn a_failed_transfer_leaves_the_active_chart_states_and_caches_unchanged() {
    let mut kernel = instantiate(&two_chart_model(false));
    let primary = Rc::clone(&kernel.runtime);
    let (states, guess) = (
        kernel.states.clone(),
        kernel.solver_y_guess.borrow().clone(),
    );
    latch_alternate(&mut kernel);
    let error = kernel
        .run_basis_change_boundary()
        .expect_err("the alternate cannot reconstruct q, so the transfer fails");
    assert_eq!(error.stage(), Some(MeStage::EventIteration), "{error}");
    assert_eq!(kernel.active_chart, 0);
    assert_eq!(kernel.active_reference, None);
    assert!(
        Rc::ptr_eq(&kernel.runtime, &primary),
        "the primary basis stays bound"
    );
    assert_eq!(kernel.states, states);
    assert_eq!(*kernel.solver_y_guess.borrow(), guess);
    assert!(
        kernel.pending_basis_change.is_none(),
        "the request is consumed"
    );
    assert_eq!(nominal(&kernel), 1.0);
}

#[test]
fn a_saved_state_restores_with_an_alternate_that_was_never_built() {
    let mut kernel = instantiate(&two_chart_model(true));
    let primary = Rc::clone(&kernel.runtime);
    let saved = kernel.fmu_state();
    assert!(
        saved.component.runtimes[1].is_none(),
        "an alternate is built only on first use"
    );
    assert_eq!(nominal(&kernel), 1.0);

    latch_alternate(&mut kernel);
    let switched = kernel
        .run_basis_change_boundary()
        .expect("the transfer succeeds");
    assert_eq!(kernel.active_chart, 1);
    assert_eq!(kernel.states, [1.5], "the state now integrates a");
    assert!(switched.nominals_of_continuous_states_changed);
    assert_eq!(nominal(&kernel), 10.0, "the state carries a's nominal");

    kernel
        .reset_to_fmu_state(&saved)
        .expect("the saved state restores");
    assert_eq!(kernel.active_chart, 0);
    assert_eq!(kernel.active_reference, None);
    assert!(Rc::ptr_eq(&kernel.runtime, &primary));
    assert_eq!(kernel.states, [0.5]);
    assert_eq!(nominal(&kernel), 1.0);

    // The alternate built after the save has no saved state; the next transfer
    // re-establishes its reconstruction.
    latch_alternate(&mut kernel);
    kernel
        .run_basis_change_boundary()
        .expect("the transfer succeeds again");
    assert_eq!(kernel.active_chart, 1);
    assert_eq!(kernel.states, [1.5]);
}
