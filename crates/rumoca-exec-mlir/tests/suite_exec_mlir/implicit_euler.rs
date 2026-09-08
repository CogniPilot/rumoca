//! Algebraic Newton coverage for the MLIR implicit-residual and JVP exports.
//!
//! Both fixtures are honest semi-explicit systems with `Y = [x, rhs]` and
//! `xdot = rhs`. Their algebraic rows are `rhs + x = 0` for decay and
//! `rhs - x(1 - x) = 0` for logistic growth. Implicit Euler adds the host
//! kinematic row `x_new - x_old - h*rhs_new = 0`; it never treats the
//! algebraic residual as an ODE right-hand side.

use rumoca_core::{SourceId, Span};
use rumoca_exec_mlir::{
    CompiledMlirResidual, MlirError, compile_derivative_rhs as exec_compile_derivative_rhs,
};
use rumoca_ir_solve::{
    AlgebraicProjectionBlock, AlgebraicProjectionPlan, BinaryOp, ComputeBlock, DiscreteSolveSystem,
    InitializationSolveSystem, LinearOp, ScalarProgramBlock, SolveClockPartition,
    SolveEventPartition, scalar_slot_y,
};

use super::support::{continuous, fixture, missing_cpu_tool};

fn spb(rows: Vec<Vec<LinearOp>>, label: &str) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        rows,
        Span::from_offsets(SourceId::from_source_name(label), 0, label.len())
            .require_provenance("MLIR algebraic-Newton fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

fn semi_explicit_problem(
    implicit_rhs: ComputeBlock,
    x_start: f64,
    rhs_start: f64,
) -> fixture::ContinuousProblem {
    let derivative_rhs = ComputeBlock::from_scalar_program_block(spb(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        "implicit_euler_derivative.mo",
    ));
    let inventory = fixture::ContinuousInventory::new(vec![
        fixture::FixtureScalar::state("x", x_start, 1.0),
        fixture::FixtureScalar::algebraic("rhs", rhs_start, 1.0),
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

fn algebraic_decay_problem() -> fixture::ContinuousProblem {
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
        "implicit_euler_algebraic_residual.mo",
    ));
    semi_explicit_problem(implicit_rhs, 1.0, -1.0)
}

fn algebraic_logistic_problem() -> fixture::ContinuousProblem {
    let implicit_rhs = ComputeBlock::from_scalar_program_block(spb(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::LoadY { dst: 1, index: 0 },
            LinearOp::Const { dst: 2, value: 1.0 },
            LinearOp::Binary {
                dst: 3,
                op: BinaryOp::Sub,
                lhs: 2,
                rhs: 1,
            },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Mul,
                lhs: 1,
                rhs: 3,
            },
            LinearOp::Binary {
                dst: 5,
                op: BinaryOp::Sub,
                lhs: 0,
                rhs: 4,
            },
            LinearOp::StoreOutput { src: 5 },
        ]],
        "implicit_euler_logistic_residual.mo",
    ));
    semi_explicit_problem(implicit_rhs, 0.5, 0.25)
}

fn compile_or_skip(
    problem: fixture::ContinuousProblem,
    name: &str,
) -> Option<CompiledMlirResidual> {
    match exec_compile_derivative_rhs(fixture::complete_model(problem), name) {
        Ok(compiled) => Some(compiled),
        Err(MlirError::ToolNotFound { tool, .. }) => {
            missing_cpu_tool::missing_cpu_tool(tool);
            None
        }
        Err(error) => panic!("compile failed: {error}"),
    }
}

struct StepResult {
    y: [f64; 2],
    iterations: usize,
}

fn implicit_euler_step(
    compiled: &CompiledMlirResidual,
    current: [f64; 2],
    time: f64,
    step_size: f64,
    iteration_limit: usize,
) -> StepResult {
    let mut derivative = [0.0];
    compiled
        .call(&current, &[], time, &mut derivative)
        .expect("semi-explicit derivative evaluates");
    let mut trial = [current[0] + step_size * derivative[0], current[1]];
    let state_seed = [1.0, 0.0];
    let algebraic_seed = [0.0, 1.0];
    for iteration in 1..=iteration_limit {
        let mut algebraic_residual = [0.0];
        compiled
            .call_implicit_rhs(&trial, &[], time + step_size, &mut algebraic_residual)
            .expect("implicit residual is present")
            .expect("implicit residual evaluates");
        let kinematic_residual = trial[0] - current[0] - step_size * trial[1];
        if kinematic_residual.abs().max(algebraic_residual[0].abs()) < 1e-13 {
            return StepResult {
                y: trial,
                iterations: iteration,
            };
        }
        let dg_dx = residual_jvp(compiled, &trial, &state_seed, time + step_size);
        let dg_drhs = residual_jvp(compiled, &trial, &algebraic_seed, time + step_size);
        let determinant = dg_drhs + step_size * dg_dx;
        let neg_kinematic = -kinematic_residual;
        let neg_algebraic = -algebraic_residual[0];
        let delta_x = (neg_kinematic * dg_drhs + step_size * neg_algebraic) / determinant;
        let delta_rhs = (neg_algebraic - neg_kinematic * dg_dx) / determinant;
        trial[0] += delta_x;
        trial[1] += delta_rhs;
    }
    panic!("semi-explicit Newton exceeded {iteration_limit} iterations")
}

fn residual_jvp(compiled: &CompiledMlirResidual, y: &[f64; 2], seed: &[f64; 2], time: f64) -> f64 {
    let mut product = [0.0];
    compiled
        .call_jacobian_v(y, &[], seed, time, &mut product)
        .expect("implicit JVP is present")
        .expect("implicit JVP evaluates");
    product[0]
}

fn integrate(
    compiled: &CompiledMlirResidual,
    initial: [f64; 2],
    step_size: f64,
    steps: usize,
    iteration_limit: usize,
) -> ([f64; 2], usize) {
    let mut y = initial;
    let mut max_iterations = 0;
    for step in 0..steps {
        let result = implicit_euler_step(
            compiled,
            y,
            step as f64 * step_size,
            step_size,
            iteration_limit,
        );
        y = result.y;
        max_iterations = max_iterations.max(result.iterations);
    }
    (y, max_iterations)
}

#[test]
fn algebraic_residual_targets_rhs() {
    let Some(compiled) = compile_or_skip(algebraic_decay_problem(), "algebraic_residual") else {
        return;
    };
    let y = [2.5, -0.75];
    let mut residual = [0.0];
    compiled
        .call_implicit_rhs(&y, &[], 0.0, &mut residual)
        .expect("implicit residual is present")
        .expect("implicit residual evaluates");
    assert!((residual[0] - 1.75).abs() < 1e-12);
}

#[test]
fn algebraic_residual_jvp_uses_both_y_lanes() {
    let Some(compiled) = compile_or_skip(algebraic_decay_problem(), "algebraic_jvp") else {
        return;
    };
    let y = [4.0, -4.0];
    let seed = [2.0, 3.0];
    let mut product = [0.0];
    compiled
        .call_jacobian_v(&y, &[], &seed, 0.0, &mut product)
        .expect("implicit JVP is present")
        .expect("implicit JVP evaluates");
    assert!((product[0] - 5.0).abs() < 1e-12);
}

#[test]
fn derivative_semantics_are_distinct_from_the_residual() {
    let Some(compiled) = compile_or_skip(algebraic_decay_problem(), "distinct_derivative") else {
        return;
    };
    let y = [2.0, -2.0];
    let mut derivative = [0.0];
    let mut residual = [0.0];
    compiled
        .call(&y, &[], 0.0, &mut derivative)
        .expect("derivative evaluates");
    compiled
        .call_implicit_rhs(&y, &[], 0.0, &mut residual)
        .expect("implicit residual is present")
        .expect("implicit residual evaluates");
    assert!((derivative[0] + 2.0).abs() < 1e-12);
    assert!(residual[0].abs() < 1e-12);
}

#[test]
fn algebraic_newton_solves_the_rhs_coordinate() {
    let Some(compiled) = compile_or_skip(algebraic_decay_problem(), "algebraic_newton") else {
        return;
    };
    let x = 3.25;
    let mut y = [x, 0.5];
    let seed = [0.0, 1.0];
    let mut residual = [0.0];
    let mut derivative = [0.0];
    for _ in 0..2 {
        compiled
            .call_implicit_rhs(&y, &[], 0.0, &mut residual)
            .expect("implicit residual is present")
            .expect("implicit residual evaluates");
        compiled
            .call_jacobian_v(&y, &[], &seed, 0.0, &mut derivative)
            .expect("implicit JVP is present")
            .expect("implicit JVP evaluates");
        y[1] -= residual[0] / derivative[0];
    }
    assert!((y[1] + x).abs() < 1e-12);
}

#[test]
fn semi_explicit_decay_integrates_to_t_one() {
    let Some(compiled) = compile_or_skip(algebraic_decay_problem(), "implicit_decay") else {
        return;
    };
    let step_size = 0.01;
    let steps = 100;
    let (result, max_iterations) = integrate(&compiled, [1.0, -1.0], step_size, steps, 4);
    let discrete_expected = (1.0 + step_size).powi(-(steps as i32));
    let analytical = (-1.0f64).exp();
    let error = (result[0] - analytical).abs();
    assert!((result[0] - discrete_expected).abs() < 1e-12);
    assert!(error < 2e-3, "decay error {error:e}");
    assert!(
        max_iterations <= 2,
        "decay used {max_iterations} iterations"
    );
}

#[test]
fn semi_explicit_logistic_large_steps_converge_to_t_one() {
    let Some(compiled) = compile_or_skip(algebraic_logistic_problem(), "implicit_logistic") else {
        return;
    };
    let step_size = 0.25;
    let steps = 4;
    let (result, max_iterations) = integrate(&compiled, [0.5, 0.25], step_size, steps, 8);
    let analytical = 1.0 / (1.0 + (-1.0f64).exp());
    let error = (result[0] - analytical).abs();
    assert!(error < 6.5e-3, "logistic error {error:e}");
    assert!(
        max_iterations <= 5,
        "large-step logistic Newton used {max_iterations} iterations"
    );
}
