//! Every causal step of an issued tearing carries a construction proof that
//! its isolated coefficient is bounded away from zero, and the proof a checker
//! recomputes from the step's row agrees with it (SPEC_0043 §4). Where the
//! structural tearing isolates through a coefficient computed from solver
//! values, an alternative causal step replaces it.

use rumoca::Compiler;
use rumoca_eval_solve::PreparedScalarProgramBlock;
use rumoca_eval_solve::refresh_plan::causal_step_coefficient_proof;
use rumoca_ir_solve as solve;
use rumoca_sim::{
    SimOptions, SimSolverMode, lower_dae_for_simulation, simulate_dae_with_diagnostics,
};

/// `x` appears in the first row only through the solver value `y`, and with a
/// unit coefficient in the second, so only the second may isolate it.
const PRODUCT_LOOP: &str = "
model ProductLoop
  Real x(start = 0.5);
  Real y(start = 2);
  Real w(start = 0, fixed = true);
equation
  y * x = 1 + 0.1 * w;
  x + 2 * y = 5;
  der(w) = 0.5;
end ProductLoop;";

fn lowered(model: &str, source: &str) -> solve::SolveModel {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("compile {model}: {error:#}"));
    lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .unwrap_or_else(|error| panic!("lower {model}: {error:#}"))
}

/// The checker: recompute every causal step's proof from its row.
pub(crate) fn assert_every_step_is_proven(model: &solve::SolveModel) -> usize {
    let prepared = PreparedScalarProgramBlock::new(
        rumoca_eval_solve::to_scalar_program_projection(&model.problem.continuous.implicit_rhs)
            .expect("the residual programs scalarize")
            .into_block(),
    )
    .expect("the residual programs prepare");
    let mut steps = 0;
    for block in &model.problem.continuous.algebraic_projection_plan.blocks {
        for step in block
            .tearing
            .iter()
            .flat_map(|tearing| &tearing.causal_steps)
        {
            assert_ne!(
                step.coefficient,
                solve::CausalCoefficient::Unproven,
                "{step:?}"
            );
            assert_eq!(
                causal_step_coefficient_proof(&prepared, step.row, step.y_index),
                step.coefficient,
                "the recorded proof is the one its row gives"
            );
            steps += 1;
        }
    }
    steps
}

#[test]
fn a_causal_step_isolates_only_through_a_proven_coefficient() {
    let model = lowered("ProductLoop", PRODUCT_LOOP);
    let names = &model.problem.solve_layout.solver_maps.names;
    let x = names
        .iter()
        .position(|name| name == "x")
        .expect("x has a slot");
    let block = model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .iter()
        .find(|block| block.y_indices.contains(&x))
        .expect("x is projected");
    if let Some(tearing) = &block.tearing {
        for step in tearing.causal_steps.iter().filter(|step| step.y_index == x) {
            assert_ne!(
                step.coefficient,
                solve::CausalCoefficient::Unproven,
                "x is isolated through its constant coefficient"
            );
        }
    }
    assert_every_step_is_proven(&model);
    let compiled = Compiler::new()
        .model("ProductLoop")
        .compile_str(PRODUCT_LOOP, "ProductLoop.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("the product loop simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).unwrap();
        &result.data[index]
    };
    for step in 0..result.times.len() {
        let (x, y, w) = (column("x")[step], column("y")[step], column("w")[step]);
        assert!((y * x - 1.0 - 0.1 * w).abs() < 1e-6, "row 1 at step {step}");
        assert!((x + 2.0 * y - 5.0).abs() < 1e-6, "row 2 at step {step}");
    }
}
