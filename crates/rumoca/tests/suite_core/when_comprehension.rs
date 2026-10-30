//! MLS §10.4.1 array constructors inside `when` owners. A comprehension in a
//! `when` equation or in a `when` statement of a model algorithm lowers through
//! the model-scope comprehension plans like one in an ordinary equation. Before
//! the fix those plans were folded only from equation owners, so DAE
//! construction panicked ("analysis proves the exact comprehension occurrence")
//! on the first such comprehension; a domain the analysis cannot settle is now a
//! typed refusal at the comprehension instead.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package WhenComprehension
  model AlgorithmWhen "comprehension in a when statement of a model algorithm"
    parameter Real P[3, 3] = diagonal({1, 2, 3});
    discrete Real d[3](each start = 0, each fixed = true);
  algorithm
    when sample(0, 0.1) then
      d := {P[i, i] * time for i in 1:3};
    end when;
  end AlgorithmWhen;

  model EquationWhen "comprehension in a when equation"
    parameter Real P[3, 3] = diagonal({1, 2, 3});
    discrete Real d[3](each start = 0, each fixed = true);
  equation
    when sample(0, 0.1) then
      d = {P[i, i] * time for i in 1:3};
    end when;
  end EquationWhen;

  model ParameterDomain "domain bound by a structural parameter"
    parameter Integer n = 3;
    parameter Real P[n, n] = diagonal({1, 2, 3});
    discrete Real d[n](each start = 0, each fixed = true);
  algorithm
    when sample(0, 0.1) then
      d := {P[i, i] * time for i in 1:n};
    end when;
  end ParameterDomain;

  model TriangularDomain "an inner bound that is an outer index is refused"
    parameter Real P[3, 3] = diagonal({1, 2, 3});
    discrete Real d[3](each start = 0, each fixed = true);
  algorithm
    when sample(0, 0.1) then
      d := {sum({P[i, j] for j in 1:i}) for i in 1:3};
    end when;
  end TriangularDomain;
end WhenComprehension;
"#;

fn compile(model: &str) -> Result<rumoca::CompilationResult, String> {
    Compiler::new()
        .model(model)
        .compile_str(SOURCE, "when_comprehension.mo")
        .map_err(|error| format!("{error:?}"))
}

/// The final value of every named column after simulating `model` to `t_end`.
fn final_values(model: &str, t_end: f64, names: &[&str]) -> Vec<f64> {
    let compiled = compile(model).unwrap_or_else(|error| panic!("{model}: {error}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model}: {error}"));
    names
        .iter()
        .map(|name| {
            let column = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap_or_else(|| panic!("{model} has no column {name}"));
            *result.data[column].last().unwrap()
        })
        .collect()
}

/// The last tick before 0.25 is at 0.2, where `d[i] = i * 0.2`.
fn assert_diagonal_at_last_tick(model: &str) {
    let values = final_values(model, 0.25, &["d[1]", "d[2]", "d[3]"]);
    for (index, (actual, expected)) in values.iter().zip([0.2, 0.4, 0.6]).enumerate() {
        assert!(
            (actual - expected).abs() <= 1e-12,
            "{model} d[{}]: {actual}, expected {expected}",
            index + 1
        );
    }
}

#[test]
fn a_comprehension_in_a_when_statement_is_lowered() {
    assert_diagonal_at_last_tick("WhenComprehension.AlgorithmWhen");
}

#[test]
fn a_comprehension_in_a_when_equation_is_lowered() {
    assert_diagonal_at_last_tick("WhenComprehension.EquationWhen");
}

#[test]
fn a_when_comprehension_over_a_structural_parameter_is_lowered() {
    assert_diagonal_at_last_tick("WhenComprehension.ParameterDomain");
}

#[test]
fn an_unsettled_when_comprehension_domain_is_refused_not_a_panic() {
    let Err(error) = compile("WhenComprehension.TriangularDomain") else {
        panic!("a triangular comprehension domain is refused");
    };
    assert!(error.contains("comprehension"), "{error}");
}
