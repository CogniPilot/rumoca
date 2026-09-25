//! The linked kernel's block residual split (SPEC_0043 §6a) against the
//! unsplit evaluation: every split program's dependent part over its
//! invariant values equals the whole program bit for bit at points that move
//! only the block's unknowns, interpreter trajectories are byte-identical with
//! the split on or off, each block projection call evaluates each invariant
//! part once, and a failing invariant call fails as the unsplit programs do.

use rumoca_eval_solve::projection_policy::with_block_residual_split;
use rumoca_eval_solve::{PreparedBlockResidualSplit, PreparedScalarProgramBlock, RowEvalContext};
use rumoca_ir_solve::{self as solve, BlockResidualSplit};
use rumoca_solver::{block_residual_split_counts, reset_block_residual_split_counts};

use super::super::entry::lower_dae_for_simulation;
use super::tangent_jacobian::{LOOPS, affine_chain, compile_with_roots, msl_root};
use crate::{SimOptions, SimResult, simulate_dae};
use rumoca_solver::SimExecutionPolicy;

fn lower(dae: &rumoca_ir_dae::Dae) -> solve::SolveModel {
    lower_dae_for_simulation(dae, &SimOptions::default()).expect("the fixture lowers")
}

/// Deterministic values in `[-2, 2)`.
fn sample(state: &mut u64) -> f64 {
    *state = state
        .wrapping_mul(6_364_136_223_846_793_005)
        .wrapping_add(1_442_695_040_888_963_407);
    (*state >> 11) as f64 / (1_u64 << 53) as f64 * 4.0 - 2.0
}

/// Check every split program of every block the runtime splits; returns the
/// programs checked.
fn check_splits(label: &str, model: &solve::SolveModel) -> usize {
    let implicit =
        rumoca_eval_solve::to_scalar_program_block(&model.problem.continuous.implicit_rhs)
            .expect("scalar residual programs");
    let prepared = PreparedScalarProgramBlock::new(implicit.clone()).expect("prepared programs");
    let structures = model.artifacts.continuous.structural.algebraic_projection();
    let blocks = &model.problem.continuous.algebraic_projection_plan.blocks;
    let mut state = 0x9e37_79b9_u64;
    let mut checked = 0;
    for (index, (block, structure)) in blocks.iter().zip(structures).enumerate() {
        if block.rows.len() < 2 {
            continue;
        }
        // The runtime's program choice: the residual selection's programs,
        // or the rows' programs when the block has none.
        let mut sources = Vec::new();
        match structure.residual_output_evaluation() {
            Some(selection) => {
                for program in selection.programs() {
                    sources.push(program.program());
                }
            }
            None => {
                for &row in &block.rows {
                    sources.extend(
                        prepared
                            .row_output_position(row)
                            .map(|(program, _)| program),
                    );
                }
            }
        }
        for program in sources {
            let label = format!("{label} block {index} program {program}");
            if check_split(&label, model, &prepared, (block, program), &mut state) {
                checked += 1;
            }
        }
    }
    checked
}

/// Check one program's split, if it has one, at three points of one call.
fn check_split(
    label: &str,
    model: &solve::SolveModel,
    prepared: &PreparedScalarProgramBlock,
    (block, program): (&solve::AlgebraicProjectionBlock, usize),
    state: &mut u64,
) -> bool {
    let ops = &prepared.block().programs()[program];
    let Some(split) = BlockResidualSplit::derive(ops, &block.y_indices) else {
        return false;
    };
    if let Err(error) = split.check(ops, &block.y_indices) {
        panic!("{label}: {error:?}");
    }
    let outputs = solve::ScalarProgramBlock::program_output_count(ops);
    let split = PreparedBlockResidualSplit::new(split, outputs, None);
    let p = &model.parameters;
    let mut y: Vec<f64> = (0..model.solver_scalar_count())
        .map(|_| sample(state))
        .collect();
    let mut values = Vec::new();
    split
        .eval_invariant((&y, p, 0.3), context(model), &mut values)
        .expect("the invariant part evaluates");
    for _ in 0..3 {
        // A pass of one call moves only the block's unknowns.
        block
            .y_indices
            .iter()
            .for_each(|&unknown| y[unknown] = sample(state));
        let (mut split_out, mut whole_out) = (Vec::new(), Vec::new());
        split
            .eval_dependent(&values, (&y, p, 0.3), context(model), &mut split_out)
            .expect("the dependent part evaluates");
        prepared
            .eval_row_outputs_unchecked_with_context(
                program,
                &y,
                p,
                0.3,
                context(model),
                &mut whole_out,
            )
            .expect("the whole program evaluates");
        assert!(
            split_out
                .iter()
                .map(|v| v.to_bits())
                .eq(whole_out.iter().map(|v| v.to_bits())),
            "{label}"
        );
    }
    true
}

fn context(model: &solve::SolveModel) -> RowEvalContext<'_> {
    RowEvalContext {
        pure_calls: Some(&model.pure_calls),
        external_tables: Some(model.external_tables.as_slice()),
        ..RowEvalContext::default()
    }
}

fn options(policy: SimExecutionPolicy) -> SimOptions {
    SimOptions {
        t_end: 1.0,
        dt: Some(0.01),
        execution_policy: policy,
        ..SimOptions::default()
    }
}

fn simulate(
    dae: &rumoca_ir_dae::Dae,
    split: bool,
    policy: SimExecutionPolicy,
) -> Result<SimResult, String> {
    with_block_residual_split(split, || simulate_dae(dae, &options(policy)))
        .map_err(|error| format!("{error:?}"))
}

fn bits(result: &SimResult) -> Vec<Vec<u64>> {
    result
        .data
        .iter()
        .map(|series| series.iter().map(|value| value.to_bits()).collect())
        .collect()
}

/// [`assert_trajectory_exact_under`] for native execution, which splits the
/// compiled residual programs, and for the interpreter, which splits the
/// prepared ones; returns the interpreter's counts.
fn assert_trajectory_exact(
    label: &str,
    dae: &rumoca_ir_dae::Dae,
) -> rumoca_solver::BlockResidualSplitCounts {
    let native = assert_trajectory_exact_under(label, dae, SimExecutionPolicy::Auto);
    assert!(
        native.calls > 0 && native.fallbacks == 0,
        "{label} native: {native:?}"
    );
    assert_trajectory_exact_under(label, dae, SimExecutionPolicy::Interpreter)
}

/// Split and unsplit trajectories under `policy` agree bit for bit; returns
/// the split counts of the split run.
fn assert_trajectory_exact_under(
    label: &str,
    dae: &rumoca_ir_dae::Dae,
    policy: SimExecutionPolicy,
) -> rumoca_solver::BlockResidualSplitCounts {
    let unsplit = simulate(dae, false, policy).expect("the unsplit run");
    reset_block_residual_split_counts();
    let split = simulate(dae, true, policy).expect("the split run");
    let counts = block_residual_split_counts();
    assert_eq!(split.times, unsplit.times, "{label}: output times");
    assert!(
        bits(&split) == bits(&unsplit),
        "{label}: the split changes the trajectory"
    );
    counts
}

#[test]
fn fixture_splits_are_exact_and_evaluate_each_invariant_part_once_per_call() {
    let chain = compile_with_roots(&affine_chain(), "TangentChain", &[]);
    let loops = compile_with_roots(LOOPS, "TangentLoops", &[]);
    let chain_programs = check_splits("TangentChain", &lower(&chain));
    assert!(chain_programs >= 1, "a chain program splits");
    assert!(check_splits("TangentLoops", &lower(&loops)) >= 1);
    let counts = assert_trajectory_exact("TangentChain", &chain);
    assert!(counts.calls > 0, "the chain block splits: {counts:?}");
    assert_eq!(counts.fallbacks, 0);
    // The chain's one split block: exactly one invariant evaluation per split
    // program per block projection call.
    assert_eq!(
        counts.invariant_evaluations,
        counts.calls * chain_programs as u64,
        "{counts:?}"
    );
    assert!(
        counts.dependent_evaluations >= 2 * counts.calls,
        "{counts:?}"
    );
    // The loops are not affine: their blocks keep their torn and Newton
    // programs, and the split leaves their trajectories unchanged.
    for policy in [SimExecutionPolicy::Auto, SimExecutionPolicy::Interpreter] {
        let counts = assert_trajectory_exact_under("TangentLoops", &loops, policy);
        assert_eq!(counts.calls, 0, "{counts:?}");
    }
}

/// `f` converts `1e300*max(u - 0.5, 0)` to an Integer, out of range once its
/// argument, a state, passes 0.5; it feeds an affine loop, so its call is
/// invariant in the loop's block and its evaluation fails there once the
/// state crosses.
const FAILING: &str = "model SplitFailing
  function f
    input Real u;
    output Real v;
  protected
    Integer k;
  algorithm
    k := integer(1e300*max(u - 0.5, 0));
    v := u + k;
  end f;
  Real x(start=0, fixed=true);
  Real a;
  Real b;
equation
  der(x) = 1;
  a + 0.5*b = f(x);
  b - 0.2*a = 1 + x;
end SplitFailing;";

#[test]
fn a_failing_invariant_call_fails_as_the_unsplit_programs_do() {
    let dae = compile_with_roots(FAILING, "SplitFailing", &[]);
    let unsplit = simulate(&dae, false, SimExecutionPolicy::Interpreter);
    reset_block_residual_split_counts();
    let split = simulate(&dae, true, SimExecutionPolicy::Interpreter);
    let counts = block_residual_split_counts();
    let unsplit = unsplit.expect_err("the unsplit run fails at the assertion");
    let split = split.expect_err("the split run fails at the assertion");
    assert_eq!(split, unsplit, "the same error at the same point");
    assert!(
        counts.fallbacks >= 1,
        "the invariant call failed and fell back: {counts:?}"
    );
}

#[test]
fn fourbar1_splits_are_exact() {
    let Some(root) = msl_root() else {
        return;
    };
    let dae = compile_with_roots(
        "model SplitFourbar1\n  extends Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1;\nend SplitFourbar1;\n",
        "SplitFourbar1",
        &[root],
    );
    assert!(check_splits("Fourbar1", &lower(&dae)) >= 1);
    let counts = assert_trajectory_exact("Fourbar1", &dae);
    assert!(counts.calls > 0 && counts.fallbacks == 0, "{counts:?}");
}
