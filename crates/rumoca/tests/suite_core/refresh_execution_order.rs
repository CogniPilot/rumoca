//! SPEC_0007: the algebraic projection is ordered by what executing a block
//! reads, so a tensor row that loads a whole array does not run ahead of the
//! block that produces one of its elements.
use rumoca::Compiler;
use rumoca_sim::{SimOptions, lower_dae_for_simulation};

/// The chain of `Modelica.Blocks.Continuous.Filter` behind a controller that
/// differentiates its output: one tensor row computes every `der(x[i])` and
/// loads `uu` whole, while `uu[i + 1] = x[i]` reads only `x`. By value
/// `der(x[1])` needs `uu[1]` alone, so structural analysis may order it ahead
/// of `uu[2]`.
const CHAIN: &str = r#"
model Chain
  parameter Real r[2](each fixed = false);
  Real x[2](each start = 0, each fixed = true);
  Real uu[3];
  Real y;
  Real e(start = 0, fixed = true);
initial equation
  r = {-1, -2};
equation
  y = uu[3];
  der(e) = 1 - y + 0.1*der(y);
  uu[1] = sin(time);
  for i in 1:2 loop
    der(x[i]) = r[i]*(x[i] - uu[i]);
  end for;
  for i in 1:2 loop
    uu[i + 1] = x[i];
  end for;
end Chain;
"#;

#[test]
fn a_tensor_row_runs_after_the_rows_that_produce_what_it_loads() {
    let compiled = Compiler::new()
        .model("Chain")
        .compile_str(CHAIN, "Chain.mo")
        .unwrap();
    let model = lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let (structural, _) =
        rumoca_eval_solve::derive_solve_structural_artifacts(&model.problem, &model.artifacts)
            .unwrap();
    let owners = &model.problem.continuous.refresh_owners;
    for (name, plan) in [
        ("algebraic", owners.algebraic()),
        ("derivative", owners.derivative()),
    ] {
        assert_eq!(
            plan.execution_order_refusal, None,
            "the {name} refresh executes its rows in an order that produces every read"
        );
        assert!(
            plan.causal_solution_certified || plan.value_stage_schedule_is_certified(&structural),
            "the {name} refresh keeps its staged schedule"
        );
    }
}
