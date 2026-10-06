//! MLS 3.6 §11.2.1.1: a multiple-output call statement reads its arguments
//! and then assigns its outputs. Inside a loop, scalar locals that such a call
//! assigns before every read are defined afresh in each iteration, so the
//! compact loop owns them per iteration instead of carrying a value that
//! nothing defines on entry.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function pair
  input Integer i;
  output Integer a;
  output Integer b;
algorithm
  a := i;
  b := 2 * i;
end pair;

function sumPairs
  output Real s;
protected
  Integer a;
  Integer b;
algorithm
  s := 0;
  for i in 1:3 loop
    (a, b) := pair(i);
    s := s + a + b;
  end for;
end sumPairs;

model SumPairs
  output Real s;
equation
  s = sumPairs();
end SumPairs;

function sumThenReuse
  output Real s;
protected
  Integer a;
  Integer b;
algorithm
  s := 0;
  for i in 1:3 loop
    (a, b) := pair(i);
    s := s + a + b;
  end for;
  (a, b) := pair(5);
  s := s + a;
end sumThenReuse;

model SumThenReuse
  output Real s;
equation
  s = sumThenReuse();
end SumThenReuse;
"#;

fn value(model: &str) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "LoopCallOutputs.mo")
        .unwrap_or_else(|error| panic!("{model} should compile: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("{model} should evaluate: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "s")
        .unwrap_or_else(|| panic!("{model} has s"))
        .value
}

#[test]
fn locals_a_call_assigns_in_each_iteration_are_iteration_local() {
    assert_eq!(value("SumPairs"), 18.0);
}

/// A local the loop's calls define and code after the loop reads again is
/// carried; it enters the loop from a dead seed, as an assigned one does.
#[test]
fn a_call_output_read_after_the_loop_is_carried_from_a_seed() {
    assert_eq!(value("SumThenReuse"), 23.0);
}
