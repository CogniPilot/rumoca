//! An assert condition that holds for every value of its time-varying
//! operands once its `Evaluate = true` parameters are fixed (MLS 3.7 §18.6)
//! is the literal `true`: it owns no relation and so no event. A condition
//! that reads an ordinary parameter keeps its relation, because that
//! parameter stays tunable.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, lower_dae_for_simulation};

const SOURCE: &str = r#"
model Guarded
  parameter Boolean allowReversal = true annotation(Evaluate = true);
  parameter Real m_small = 0.01;
  Real m(start = 1, fixed = true);
equation
  der(m) = -1;
  assert(m > -m_small or allowReversal, "reverse flow");
end Guarded;

model Tunable
  parameter Boolean allowReversal = true;
  parameter Real m_small = 0.01;
  Real m(start = 1, fixed = true);
equation
  der(m) = -1;
  assert(m > -m_small or allowReversal, "reverse flow");
end Tunable;

model Forbidden
  parameter Boolean allowReversal = false annotation(Evaluate = true);
  parameter Real m_small = 0.01;
  Real m(start = 1, fixed = true);
equation
  der(m) = -1;
  assert(m > -m_small or allowReversal, "reverse flow");
end Forbidden;
"#;

fn root_count(model: &str) -> usize {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "evaluate_assert_fold.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .unwrap_or_else(|error| panic!("{model} lowers: {error}"))
        .problem
        .events
        .root_conditions
        .output_count()
}

#[test]
fn an_assert_true_under_an_evaluate_parameter_owns_no_event() {
    assert_eq!(root_count("Guarded"), 0);
}

#[test]
fn an_assert_reading_a_tunable_parameter_keeps_its_relation() {
    assert!(root_count("Tunable") > 0);
}

#[test]
fn an_assert_false_under_an_evaluate_parameter_keeps_its_relation() {
    assert!(root_count("Forbidden") > 0);
}
