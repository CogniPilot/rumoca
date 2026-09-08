//! Coverage for the derivative, algebraic-residual, and residual-JVP exports
//! emitted from one MLIR module.

use rumoca_core::{SourceId, Span};
use rumoca_exec_mlir::{MlirError, compile_derivative_rhs as exec_compile_derivative_rhs};
use rumoca_ir_solve::{
    AlgebraicProjectionBlock, AlgebraicProjectionPlan, BinaryOp, ComputeBlock, DiscreteSolveSystem,
    InitializationSolveSystem, LinearOp, ScalarProgramBlock, SolveClockPartition,
    SolveEventPartition, scalar_slot_y,
};

use super::support::{continuous, derivative, fixture, missing_cpu_tool};

fn spb(rows: Vec<Vec<LinearOp>>, label: &str) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        rows,
        Span::from_offsets(SourceId::from_source_name(label), 0, label.len())
            .require_provenance("MLIR multi-function fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

fn multi_function_problem() -> fixture::ContinuousProblem {
    let derivative_rhs = ComputeBlock::from_scalar_program_block(spb(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        "multi_fn_derivative.mo",
    ));
    let implicit_rhs = ComputeBlock::from_scalar_program_block(spb(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::LoadY { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        "multi_fn_algebraic_residual.mo",
    ));
    let inventory = fixture::ContinuousInventory::new(vec![
        fixture::FixtureScalar::state("x", 0.0, 1.0),
        fixture::FixtureScalar::algebraic("rhs", 0.0, 1.0),
    ]);
    let initialization = InitializationSolveSystem::empty();
    let discrete = DiscreteSolveSystem::default();
    let events = SolveEventPartition::default();
    let clocks = SolveClockPartition::default();
    let continuous = continuous::checked_continuous_system(
        inventory.solve_layout(),
        continuous::ContinuousFixtureParts {
            implicit_rhs,
            implicit_row_targets: vec![Some(scalar_slot_y(1))],
            algebraic_projection_plan: AlgebraicProjectionPlan {
                blocks: vec![AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![1],
                    tearing: None,
                }],
            },
            residual: ComputeBlock::default(),
            manifold_residual: ComputeBlock::default(),
            manifold_projection_plan: AlgebraicProjectionPlan::default(),
            derivative_rhs,
        },
        &discrete,
        &events,
        &clocks,
    );
    inventory.seal(continuous, initialization, discrete, events, clocks)
}

fn compile_or_skip(
    solve: fixture::ContinuousProblem,
    name: &str,
) -> Option<rumoca_exec_mlir::CompiledMlirResidual> {
    match exec_compile_derivative_rhs(fixture::complete_model(solve), name) {
        Ok(compiled) => Some(compiled),
        Err(MlirError::ToolNotFound { tool, .. }) => {
            missing_cpu_tool::missing_cpu_tool(tool);
            None
        }
        Err(error) => panic!("compile failed: {error}"),
    }
}

#[test]
fn multi_fn_implicit_rhs_numerics() {
    let Some(compiled) = compile_or_skip(multi_function_problem(), "multi_implicit") else {
        return;
    };
    assert_eq!(compiled.implicit_rows(), 1);
    for &(x, rhs) in &[(0.5, 1.0), (2.0, -0.5), (-1.0, 3.0)] {
        let y = [x, rhs];
        let mut out = [0.0];
        compiled
            .call_implicit_rhs(&y, &[], 0.0, &mut out)
            .expect("implicit residual is present")
            .expect("implicit residual evaluates");
        assert!((out[0] - (rhs + x)).abs() < 1e-12);
    }
}

#[test]
fn multi_fn_jacobian_v_includes_state_and_algebraic_lanes() {
    let Some(compiled) = compile_or_skip(multi_function_problem(), "multi_jvp") else {
        return;
    };
    for &(state_seed, algebraic_seed) in &[(1.0, 0.0), (0.0, 1.0), (2.5, -0.75)] {
        let y = [3.0, -3.0];
        let seed = [state_seed, algebraic_seed];
        let mut out = [0.0];
        compiled
            .call_jacobian_v(&y, &[], &seed, 0.0, &mut out)
            .expect("implicit JVP is present")
            .expect("implicit JVP evaluates");
        assert!((out[0] - (state_seed + algebraic_seed)).abs() < 1e-12);
    }
}

#[test]
fn multi_fn_derivative_reads_the_algebraic_rhs() {
    let Some(compiled) = compile_or_skip(multi_function_problem(), "multi_derivative") else {
        return;
    };
    let y = [7.0, -2.25];
    let mut out = [0.0];
    compiled
        .call(&y, &[], 0.0, &mut out)
        .expect("derivative evaluates");
    assert!((out[0] + 2.25).abs() < 1e-12);
}

#[test]
fn multi_fn_empty_implicit_has_no_residual_or_jvp_symbol() {
    let derivative_rhs = ComputeBlock::from_scalar_program_block(spb(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        "multi_fn_empty_implicit.mo",
    ));
    let solve = derivative::derivative_problem(
        fixture::ContinuousInventory::new(vec![fixture::FixtureScalar::state("x", 0.0, 1.0)]),
        derivative_rhs,
    );
    let Some(compiled) = compile_or_skip(solve, "multi_empty_implicit") else {
        return;
    };
    assert!(compiled.call_implicit_rhs(&[], &[], 0.0, &mut []).is_none());
    assert!(
        compiled
            .call_jacobian_v(&[], &[], &[], 0.0, &mut [])
            .is_none()
    );
}
