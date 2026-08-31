//! Regression for record values a function loop carries across its back edge.
//!
//! A record-typed local rewritten on every iteration and read after the loop
//! leaves the loop as a record-typed carried value. A record has no scalar view
//! of its own, so every consumer reaches it one field at a time: dependency
//! projection walks the carry per field, and Solve lowering lays the carried
//! tuple out in the record's packed field lanes. Each shape below is pinned
//! against the value OpenModelica computes for the same source.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, eval_dae_at, simulate_dae};

/// `p` is overwritten outright on every iteration and read only after the
/// loop, so the value that leaves the loop is the one the last iteration
/// wrote. With `x = 1` the last iteration stores `P(4, 5)` and OpenModelica
/// evaluates `y` to `9.0`.
const OVERWRITTEN_RECORD_CARRY: &str = r#"
model ObserveRecordCarry
  record P
    Real a;
    Real b;
  end P;

  function acc
    input Real r[4];
    output Real y;
  protected
    P p;
  algorithm
    p := P(-1, -1);
    for i in 1:4 loop
      p := P(r[i], r[i] + 1);
    end for;
    y := p.a + p.b;
  end acc;

  Real x(start = 1.0, fixed = true);
  Real y;
equation
  der(x) = 0;
  y = acc({x, x + 1, x + 2, x + 3});
end ObserveRecordCarry;
"#;

/// The same carry with a constant argument, which the DAE evaluator folds
/// without reaching Solve lowering. OpenModelica evaluates `der(state)` to
/// `9.0`.
const CONSTANT_RECORD_CARRY: &str = r#"
model ObserveConstantRecordCarry
  record P
    Real a;
    Real b;
  end P;

  function acc
    input Real r[4];
    output Real y;
  protected
    P p;
  algorithm
    p := P(-1, -1);
    for i in 1:4 loop
      p := P(r[i], r[i] + 1);
    end for;
    y := p.a + p.b;
  end acc;

  Real state(start = 0.0, fixed = true);
equation
  der(state) = acc({1, 2, 3, 4});
end ObserveConstantRecordCarry;
"#;

/// A record whose fields have different widths, so a field's packed lanes are
/// neither the first nor a whole element of the carried tuple. With `x = 1`
/// the last iteration stores `v = {4, 5}` and `w = 6`, and OpenModelica
/// evaluates `y` to `4 + 5 + 6 = 15.0`.
const ARRAY_FIELD_RECORD_CARRY: &str = r#"
model ObserveArrayFieldRecordCarry
  record Q
    Real v[2];
    Real w;
  end Q;

  function acc
    input Real r[4];
    output Real y;
  protected
    Q q;
  algorithm
    q := Q({-1, -1}, -1);
    for i in 1:4 loop
      q := Q({r[i], r[i] + 1}, r[i] + 2);
    end for;
    y := q.v[1] + q.v[2] + q.w;
  end acc;

  Real x(start = 1.0, fixed = true);
  Real y;
equation
  der(x) = 0;
  y = acc({x, x + 1, x + 2, x + 3});
end ObserveArrayFieldRecordCarry;
"#;

/// A record whose own field is a record is still outside per-field
/// projection. The rejection is the acceptance contract of the carry above:
/// what stays rejected must say which field it is, which construct it could
/// not look through, and under which stable code, instead of reading like an
/// internal assertion.
const NESTED_RECORD_CARRY: &str = r#"
model ObserveNestedRecordCarry
  record Inner
    Real u;
  end Inner;

  record Outer
    Inner i;
    Real w;
  end Outer;

  function acc
    input Real r[4];
    output Real y;
  protected
    Outer o;
  algorithm
    o := Outer(Inner(-1), -1);
    for k in 1:4 loop
      o := Outer(Inner(r[k]), r[k] + 1);
    end for;
    y := o.i.u + o.w;
  end acc;

  Real x(start = 1.0, fixed = true);
  Real y;
equation
  der(x) = 0;
  y = acc({x, x + 1, x + 2, x + 3});
end ObserveNestedRecordCarry;
"#;

fn series<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("simulation result missing column {name}"));
    result.data[index].as_slice()
}

fn simulated_output(source: &str, model: &str, file: &str) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, file)
        .expect("a loop-carried record must compile");
    let result = simulate_dae(
        compiled.dae(),
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("a loop-carried record must simulate");
    *series(&result, "y")
        .first()
        .expect("the simulation reports at least one sample")
}

#[test]
fn an_overwritten_record_carry_read_after_the_loop_keeps_its_last_write() {
    let y = simulated_output(
        OVERWRITTEN_RECORD_CARRY,
        "ObserveRecordCarry",
        "ObserveRecordCarry.mo",
    );
    assert!(
        (y - 9.0).abs() < 1.0e-12,
        "expected 9.0 from the last iteration's record, got {y}"
    );
}

#[test]
fn a_constant_record_carry_folds_to_its_last_write() {
    let compiled = Compiler::new()
        .model("ObserveConstantRecordCarry")
        .compile_str(CONSTANT_RECORD_CARRY, "ObserveConstantRecordCarry.mo")
        .expect("a constant loop-carried record must compile");
    let probe = eval_dae_at(compiled.dae(), &SimOptions::default(), &[], 0.0)
        .expect("a constant loop-carried record must evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    let derivatives = probe
        .report
        .derivatives
        .iter()
        .map(|slot| slot.value)
        .collect::<Vec<_>>();
    assert_eq!(derivatives.len(), 1, "expected one state derivative");
    assert!(
        (derivatives[0] - 9.0).abs() < 1.0e-12,
        "expected 9.0 from the last iteration's record, got {derivatives:?}"
    );
}

#[test]
fn a_record_carry_with_an_array_field_keeps_its_last_write() {
    let y = simulated_output(
        ARRAY_FIELD_RECORD_CARRY,
        "ObserveArrayFieldRecordCarry",
        "ObserveArrayFieldRecordCarry.mo",
    );
    assert!(
        (y - 15.0).abs() < 1.0e-12,
        "expected 15.0 from the last iteration's record, got {y}"
    );
}

#[test]
fn a_nested_record_carry_is_rejected_by_a_coded_and_spanned_diagnostic() {
    let compiled = Compiler::new()
        .model("ObserveNestedRecordCarry")
        .compile_str(NESTED_RECORD_CARRY, "ObserveNestedRecordCarry.mo")
        .expect("a nested loop-carried record must reach Solve lowering");
    let error = simulate_dae(
        compiled.dae(),
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect_err("a nested loop-carried record is outside per-field projection");
    assert_eq!(
        error.diagnostic_code(),
        "EL005",
        "the rejection must carry its stable SPEC_0008 code: {error}"
    );
    let message = error.to_string();
    assert!(
        message.contains("Inner.u"),
        "the rejection must name the field it could not read: {message}"
    );
    assert!(
        message.contains("a field of an enclosing record"),
        "the rejection must name the construct it could not look through: {message}"
    );
    let span = error
        .source_span()
        .expect("the rejection must point at source, not at compiler internals");
    assert!(
        !span.is_dummy(),
        "the rejection must carry an honest source span"
    );
}
