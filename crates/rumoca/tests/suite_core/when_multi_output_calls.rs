//! Multi-output function calls in when-equations (MLS 3.7 §8.3.5,
//! §11.2.1.2): `(a, b) = f(x)` inside a when-equation assigns both outputs at
//! each activation and holds them otherwise, exactly like `(a, b) := f(x)` in
//! the equivalent when-statement.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const FUNCTION: &str = r#"
function sumAndProduct
  input Real u;
  input Real v;
  output Real s;
  output Real p;
algorithm
  s := u + v;
  p := u * v;
end sumAndProduct;

function divMod
  input Integer a;
  input Integer b;
  output Integer q;
  output Integer r;
algorithm
  q := div(a, b);
  r := mod(a, b);
end divMod;
"#;

#[test]
fn component_instances_have_distinct_when_call_owners() {
    let result = simulate(
        "SharedSourceCalls",
        r#"
model Cell
  parameter Real offset = 1;
  discrete Real s(start = 0, fixed = true);
  discrete Real p(start = 0, fixed = true);
equation
  when sample(0, 0.1) then
    (s, p) = sumAndProduct(offset, time);
  end when;
end Cell;
model SharedSourceCalls
  Cell first(offset = 1);
  Cell second(offset = 2);
end SharedSourceCalls;
"#,
    );
    for (name, expected) in [
        ("first.s", 1.2),
        ("first.p", 0.2),
        ("second.s", 2.2),
        ("second.p", 0.4),
    ] {
        assert!(
            (value_at(&result, name, 0.25) - expected).abs() < 1e-9,
            "{name} retains its own call outputs"
        );
    }
}

fn simulate(model: &str, body: &str) -> SimResult {
    let source = format!("{FUNCTION}\n{body}");
    let compiled = Compiler::new()
        .model(model)
        .compile_str(&source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"))
}

/// The value of `name` at the last sample at or before `time`.
fn value_at(result: &SimResult, name: &str, time: f64) -> f64 {
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is recorded"));
    let sample = result
        .times
        .iter()
        .rposition(|sample| *sample <= time)
        .expect("a sample precedes the probe");
    result.data[column][sample]
}

#[test]
fn a_multi_output_call_in_a_when_equation_assigns_both_outputs_per_tick() {
    let equation = simulate(
        "EquationForm",
        r#"
model EquationForm
  discrete Real k(start = 0, fixed = true);
  discrete Real s(start = 0, fixed = true);
  discrete Real p(start = 0, fixed = true);
equation
  when sample(0.25, 0.25) then
    k = pre(k) + 1;
    (s, p) = sumAndProduct(pre(k), 2.0);
  end when;
end EquationForm;
"#,
    );
    let statement = simulate(
        "StatementForm",
        r#"
model StatementForm
  discrete Real k(start = 0, fixed = true);
  discrete Real s(start = 0, fixed = true);
  discrete Real p(start = 0, fixed = true);
algorithm
  when sample(0.25, 0.25) then
    k := pre(k) + 1;
    (s, p) := sumAndProduct(pre(k), 2.0);
  end when;
end StatementForm;
"#,
    );
    // Ticks at 0.25, 0.5, 0.75: after the third, pre(k) = 2.
    for (time, s, p) in [
        (0.1, 0.0, 0.0),
        (0.3, 2.0, 0.0),
        (0.6, 3.0, 2.0),
        (0.9, 4.0, 4.0),
    ] {
        for result in [&equation, &statement] {
            assert_eq!(value_at(result, "s", time), s, "s at {time}");
            assert_eq!(value_at(result, "p", time), p, "p at {time}");
        }
    }
}

#[test]
fn a_multi_output_call_keeps_elsewhen_priority() {
    let result = simulate(
        "Priority",
        r#"
model Priority
  discrete Real s(start = 0, fixed = true);
  discrete Real p(start = 0, fixed = true);
equation
  when time > 0.3 then
    (s, p) = sumAndProduct(1.0, 2.0);
  elsewhen time > 0.6 then
    (s, p) = sumAndProduct(3.0, 4.0);
  end when;
end Priority;
"#,
    );
    assert_eq!(value_at(&result, "s", 0.2), 0.0);
    assert_eq!(value_at(&result, "s", 0.5), 3.0);
    assert_eq!(value_at(&result, "p", 0.5), 2.0);
    // `time > 0.6` rises while `time > 0.3` stays true: the elsewhen fires.
    assert_eq!(value_at(&result, "s", 0.9), 7.0);
    assert_eq!(value_at(&result, "p", 0.9), 12.0);
}

#[test]
fn a_multi_output_call_assigns_integer_receivers() {
    let result = simulate(
        "IntegerOutputs",
        r#"
model IntegerOutputs
  discrete Integer n(start = 0, fixed = true);
  discrete Integer q(start = 0, fixed = true);
  discrete Integer r(start = 0, fixed = true);
equation
  when sample(0.25, 0.25) then
    n = pre(n) + 5;
    (q, r) = divMod(pre(n) + 5, 3);
  end when;
end IntegerOutputs;
"#,
    );
    for (time, q, r) in [
        (0.1, 0.0, 0.0),
        (0.3, 1.0, 2.0),
        (0.6, 3.0, 1.0),
        (0.9, 5.0, 0.0),
    ] {
        assert_eq!(value_at(&result, "q", time), q, "q at {time}");
        assert_eq!(value_at(&result, "r", time), r, "r at {time}");
    }
}

/// MLS §16.5.1: in a clocked when body the receivers join the clock's
/// partition and the call reads the partition's `previous` values.
#[test]
fn a_multi_output_call_in_a_clocked_when_joins_the_partition() {
    let result = simulate(
        "ClockedOutputs",
        r#"
model ClockedOutputs
  Clock c = Clock(1, 4);
  Real k(start = 0);
  Real s(start = 0);
  Real p(start = 0);
equation
  when c then
    k = previous(k) + 1;
    (s, p) = sumAndProduct(previous(k), 2.0);
  end when;
end ClockedOutputs;
"#,
    );
    // Ticks at 0, 0.25, 0.5, 0.75: the tick at 0.5 reads previous(k) = 2.
    for (time, s, p) in [(0.1, 2.0, 0.0), (0.3, 3.0, 2.0), (0.6, 4.0, 4.0)] {
        assert_eq!(value_at(&result, "s", time), s, "s at {time}");
        assert_eq!(value_at(&result, "p", time), p, "p at {time}");
    }
}
