//! Interpreter trajectories under each projection Jacobian source.
//!
//! The colored tangent lanes equal the one-direction colored calls bit for
//! bit, and a block whose program does not widen keeps the one-direction
//! calls, so a trajectory is bit-identical with the lanes on or off.
//! The torn tear Jacobian always comes from the tangent plan.

use rumoca_eval_solve::projection_policy::{JacobianSources, with_jacobian_sources};
use rumoca_ir_solve::{self as solve, ColoredTangentPlan};

use super::super::entry::lower_dae_for_simulation;
use super::tangent_jacobian::{LOOPS, affine_chain, compile_with_roots, msl_root};
use crate::{SimOptions, SimResult, simulate_dae};

fn options(t_end: f64) -> SimOptions {
    SimOptions {
        t_end,
        dt: Some(0.01),
        execution_policy: rumoca_solver::SimExecutionPolicy::Interpreter,
        ..SimOptions::default()
    }
}

fn simulate(dae: &rumoca_ir_dae::Dae, t_end: f64, sources: JacobianSources) -> SimResult {
    match with_jacobian_sources(sources, || simulate_dae(dae, &options(t_end))) {
        Ok(result) => result,
        Err(error) => panic!("simulate under {sources:?}: {error:?}"),
    }
}

fn bits(result: &SimResult) -> Vec<Vec<u64>> {
    result
        .data
        .iter()
        .map(|series| series.iter().map(|value| value.to_bits()).collect())
        .collect()
}

/// Blocks whose colored application has a program that does not widen, and
/// blocks whose application widens.
fn colored_plan_counts(dae: &rumoca_ir_dae::Dae) -> (usize, usize) {
    let model = lower_dae_for_simulation(dae, &SimOptions::default()).expect("lowers");
    let structures = model.artifacts.continuous.structural.algebraic_projection();
    let applications = structures
        .iter()
        .filter_map(solve::JacobianStructure::jacobian_application);
    applications.fold(
        (0, 0),
        |(refused, widened), application| match ColoredTangentPlan::derive(application) {
            Ok(_) => (refused, widened + 1),
            Err(_) => (refused + 1, widened),
        },
    )
}

/// Colored lanes on and off give the same trajectory bit for bit; returns
/// the refused and widened block counts.
fn assert_colored_lanes_exact(label: &str, dae: &rumoca_ir_dae::Dae, t_end: f64) -> (usize, usize) {
    let lanes = simulate(dae, t_end, JacobianSources::POLICY);
    let one_direction = simulate(
        dae,
        t_end,
        JacobianSources {
            colored_lanes: false,
        },
    );
    assert_eq!(lanes.times, one_direction.times, "{label}: output times");
    assert!(
        bits(&lanes) == bits(&one_direction),
        "{label}: the colored lanes change the trajectory"
    );
    colored_plan_counts(dae)
}

#[test]
fn fixture_trajectories_under_each_jacobian_source() {
    let loops = compile_with_roots(LOOPS, "TangentLoops", &[]);
    let chain = compile_with_roots(&affine_chain(), "TangentChain", &[]);
    let (_, widened) = assert_colored_lanes_exact("TangentChain", &chain, 1.0);
    assert!(widened >= 1, "the chain block widens");
    assert_colored_lanes_exact("TangentLoops", &loops, 1.0);
}

/// End time of the MSL runs, long enough to cross many refreshes of every block.
const MSL_END: f64 = 0.5;

/// (label, qualified model name) of each MSL model.
const MSL_MODELS: [(&str, &str); 4] = [
    (
        "Fourbar1",
        "Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1",
    ),
    (
        "RevoluteConstraint",
        "Modelica.Mechanics.MultiBody.Examples.Constraints.RevoluteConstraint",
    ),
    (
        "LineForceWithTwoMasses",
        "Modelica.Mechanics.MultiBody.Examples.Elementary.LineForceWithTwoMasses",
    ),
    (
        "RollingWheel",
        "Modelica.Mechanics.MultiBody.Examples.Elementary.RollingWheel",
    ),
];

/// The models whose colored applications refuse to widen keep the exact
/// one-direction Jacobian, so their interpreter trajectories match the
/// one-direction build bit for bit.
#[test]
fn msl_trajectories_under_each_jacobian_source() {
    let Some(root) = msl_root() else {
        return;
    };
    let mut refused_blocks = 0;
    for (short, name) in MSL_MODELS {
        let wrapper = format!("JacobianSources{short}");
        let source = format!("model {wrapper}\n  extends {name};\nend {wrapper};\n");
        let dae = compile_with_roots(&source, &wrapper, std::slice::from_ref(&root));
        let (refused, _) = assert_colored_lanes_exact(short, &dae, MSL_END);
        refused_blocks += refused;
    }
    assert!(
        refused_blocks > 0,
        "some MSL block keeps its one-direction Jacobian"
    );
}
