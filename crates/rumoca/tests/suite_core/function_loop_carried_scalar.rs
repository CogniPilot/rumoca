//! Regression for scalar values a function loop carries across its back edge.
//!
//! Loop compaction proves a scalar local dead by looking at what follows its
//! definition. Inside a `for` body that proof is incomplete: MLS §11.2.2
//! re-executes the body from its first statement, so every earlier statement is
//! also a later reader. Deleting a definition that is live on that edge is
//! silent wrong code — every iteration then reads the value the loop was
//! entered with — so each shape below is pinned against the value OpenModelica
//! computes for the same source.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

/// `x4` is written from a multi-output call's result and read back by that same
/// call on the next iteration. OpenModelica evaluates `runTuple()` to
/// `{1.0, 2.0, 3.0}`.
const TUPLE_COPY_BACK_MODEL: &str = r#"
within;

function pair
  input Real a;
  output Real b;
  output Real c;
algorithm
  b := a + 1.0;
  c := a;
end pair;

function runTuple
  output Real diag[3];
protected
  Real x4;
  Real x4n;
  Real x3;
algorithm
  x4 := 0.0;
  x4n := 0.0;
  x3 := 0.0;
  diag := zeros(3);
  for tick in 1:3 loop
    (x4n, x3) := pair(x4);
    x4 := x4n;
    diag[tick] := x4;
  end for;
end runTuple;

model ObserveTupleCopyBack
  Real state[3](each start = 0.0, each fixed = true);
equation
  der(state) = runTuple();
end ObserveTupleCopyBack;
"#;

/// `a` reads the value `s` held when the iteration began, and `s` is rewritten
/// after that read. Substituting either definition forward moves a read past a
/// write. OpenModelica evaluates `runShadow()` to `{0.0, 2.0, 4.0}`.
const SHADOWED_CARRY_MODEL: &str = r#"
within;

function runShadow
  output Real diag[3];
protected
  Real s;
  Real a;
algorithm
  s := 0.0;
  a := 0.0;
  diag := zeros(3);
  for tick in 1:3 loop
    a := s;
    s := 2.0 * tick;
    diag[tick] := a;
  end for;
end runShadow;

model ObserveShadowedCarry
  Real state[3](each start = 0.0, each fixed = true);
equation
  der(state) = runShadow();
end ObserveShadowedCarry;
"#;

/// The straight-line fusion `xn := x + 1.0; x := xn` must keep working: it is
/// the same recurrence written without a shadowing read, and OpenModelica also
/// evaluates it to `{1.0, 2.0, 3.0}`.
const FUSED_CARRY_MODEL: &str = r#"
within;

function runScalar
  output Real diag[3];
protected
  Real x;
  Real xn;
algorithm
  x := 0.0;
  xn := 0.0;
  diag := zeros(3);
  for tick in 1:3 loop
    xn := x + 1.0;
    x := xn;
    diag[tick] := x;
  end for;
end runScalar;

model ObserveFusedCarry
  Real state[3](each start = 0.0, each fixed = true);
equation
  der(state) = runScalar();
end ObserveFusedCarry;
"#;

/// `acc` is overwritten outright on every iteration and read only after the
/// loop. Nothing inside the body observes it, so the definition looks dead to
/// a proof that only asks what the *body* reads; the value that leaves the
/// loop is the one the last iteration wrote. OpenModelica evaluates
/// `runOverwrite()` to `{4.0, 5.0, 6.0}`.
const OVERWRITTEN_CARRY_MODEL: &str = r#"
within;

function runOverwrite
  output Real diag[3];
protected
  Real acc;
  Real r[4];
algorithm
  r := {1.0, 2.0, 3.0, 4.0};
  acc := -1.0;
  for i in 1:4 loop
    acc := r[i];
  end for;
  diag := {acc, acc + 1.0, acc + 2.0};
end runOverwrite;

model ObserveOverwrittenCarry
  Real state[3](each start = 0.0, each fixed = true);
equation
  der(state) = runOverwrite();
end ObserveOverwrittenCarry;
"#;

/// The same carry written as a self-dependent accumulation. This shape was
/// never at risk (the body reads `acc` before writing it) and must stay
/// correct. OpenModelica evaluates `runAccumulate()` to `{10.0, 11.0, 12.0}`.
const ACCUMULATED_CARRY_MODEL: &str = r#"
within;

function runAccumulate
  output Real diag[3];
protected
  Real acc;
  Real r[4];
algorithm
  r := {1.0, 2.0, 3.0, 4.0};
  acc := 0.0;
  for i in 1:4 loop
    acc := acc + r[i];
  end for;
  diag := {acc, acc + 1.0, acc + 2.0};
end runAccumulate;

model ObserveAccumulatedCarry
  Real state[3](each start = 0.0, each fixed = true);
equation
  der(state) = runAccumulate();
end ObserveAccumulatedCarry;
"#;

/// The same carry written under a guard: only the iterations whose element
/// passes the test write `acc`, and the value read after the loop is the last
/// one that did. OpenModelica evaluates `runConditional()` to
/// `{4.0, 5.0, 6.0}`.
const CONDITIONAL_CARRY_MODEL: &str = r#"
within;

function runConditional
  output Real diag[3];
protected
  Real acc;
  Real r[4];
algorithm
  r := {1.0, 2.0, 3.0, 4.0};
  acc := -1.0;
  for i in 1:4 loop
    if r[i] > 2.5 then
      acc := r[i];
    end if;
  end for;
  diag := {acc, acc + 1.0, acc + 2.0};
end runConditional;

model ObserveConditionalCarry
  Real state[3](each start = 0.0, each fixed = true);
equation
  der(state) = runConditional();
end ObserveConditionalCarry;
"#;

fn loop_derivatives(source: &str, model: &str, file: &str) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, file)
        .expect("a loop-carried scalar must compile");
    let probe = eval_dae_at(compiled.dae(), &SimOptions::default(), &[], 0.0)
        .expect("a loop-carried scalar must evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    probe
        .report
        .derivatives
        .iter()
        .map(|slot| slot.value)
        .collect()
}

fn assert_iteration_values(actual: &[f64], expected: [f64; 3]) {
    assert_eq!(
        actual.len(),
        expected.len(),
        "expected one derivative per iteration, got {actual:?}"
    );
    for (actual, expected) in actual.iter().zip(expected) {
        assert!(
            (actual - expected).abs() < 1.0e-12,
            "expected {expected}, got {actual}; all values: {actual:?}"
        );
    }
}

#[test]
fn multi_output_call_copy_back_survives_the_loop_back_edge() {
    let values = loop_derivatives(
        TUPLE_COPY_BACK_MODEL,
        "ObserveTupleCopyBack",
        "ObserveTupleCopyBack.mo",
    );
    assert_iteration_values(&values, [1.0, 2.0, 3.0]);
}

#[test]
fn a_carried_scalar_read_before_it_is_rewritten_keeps_its_entry_value() {
    let values = loop_derivatives(
        SHADOWED_CARRY_MODEL,
        "ObserveShadowedCarry",
        "ObserveShadowedCarry.mo",
    );
    assert_iteration_values(&values, [0.0, 2.0, 4.0]);
}

#[test]
fn an_overwritten_carry_read_after_the_loop_keeps_its_last_write() {
    let values = loop_derivatives(
        OVERWRITTEN_CARRY_MODEL,
        "ObserveOverwrittenCarry",
        "ObserveOverwrittenCarry.mo",
    );
    assert_iteration_values(&values, [4.0, 5.0, 6.0]);
}

#[test]
fn an_accumulated_carry_read_after_the_loop_keeps_its_last_write() {
    let values = loop_derivatives(
        ACCUMULATED_CARRY_MODEL,
        "ObserveAccumulatedCarry",
        "ObserveAccumulatedCarry.mo",
    );
    assert_iteration_values(&values, [10.0, 11.0, 12.0]);
}

#[test]
fn a_conditionally_written_carry_read_after_the_loop_keeps_its_last_write() {
    let values = loop_derivatives(
        CONDITIONAL_CARRY_MODEL,
        "ObserveConditionalCarry",
        "ObserveConditionalCarry.mo",
    );
    assert_iteration_values(&values, [4.0, 5.0, 6.0]);
}

#[test]
fn a_dominated_scratch_definition_still_fuses_into_its_recurrence() {
    let values = loop_derivatives(
        FUSED_CARRY_MODEL,
        "ObserveFusedCarry",
        "ObserveFusedCarry.mo",
    );
    assert_iteration_values(&values, [1.0, 2.0, 3.0]);
}
