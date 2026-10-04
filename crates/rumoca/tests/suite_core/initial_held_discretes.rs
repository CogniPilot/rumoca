//! An initialization row that reads a discrete coordinate determined by the
//! continuous solution (MLS 3.7 §8.6 mixed system). The projection holds the
//! discrete at its current value and the runtime alternates the projection with
//! the discrete assignments until the discrete stops changing.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = "
model HeldDiscreteInit
  Real x;
  Real y;
  Boolean high;
initial equation
  x = if high then 2 else 1;
equation
  y = x + 1;
  high = y > 1.5;
  der(x) = -0.1;
end HeldDiscreteInit;";

#[test]
fn an_initial_row_reading_a_relation_defined_discrete_reaches_the_fixed_point() {
    let dae = Compiler::new()
        .model("HeldDiscreteInit")
        .compile_str(SOURCE, "initial_held_discretes.mo")
        .unwrap()
        .dae;
    let result = simulate_dae_with_diagnostics(
        &dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..Default::default()
        },
    )
    .unwrap();
    let value = |name: &str| {
        let index = result.names.iter().position(|n| n == name).unwrap();
        result.data[index][0]
    };
    // `high` starts false, which gives x = 1, y = 2 > 1.5, so `high` becomes
    // true and x = 2, y = 3: the only consistent initial point.
    assert_eq!(value("high"), 1.0);
    assert!((value("x") - 2.0).abs() < 1e-9, "x = {}", value("x"));
    assert!((value("y") - 3.0).abs() < 1e-9, "y = {}", value("y"));
}

const DISCRETE_OFFSET: &str = "
model HeldDiscreteOffset
  Real x(start = 0, fixed = false);
  Real q;
  discrete Real d(start = 0, fixed = true);
equation
  der(x) = 0;
  x + q = 5;
  when time > 0.5 then
    d = 1;
  end when;
initial equation
  x = d + 2;
end HeldDiscreteOffset;";

/// `x + q = 5; x = d + 2;` with `d(start = 0, fixed = true)` is solvable at
/// x = 2, q = 3, which is what OpenModelica returns.
#[test]
fn an_initial_row_reading_a_fixed_discrete_is_solved_with_it() {
    let dae = Compiler::new()
        .model("HeldDiscreteOffset")
        .compile_str(DISCRETE_OFFSET, "initial_held_discretes.mo")
        .unwrap()
        .dae;
    let result = simulate_dae_with_diagnostics(
        &dae,
        &SimOptions {
            t_end: 0.25,
            dt: Some(0.25),
            ..Default::default()
        },
    )
    .unwrap();
    let value = |name: &str| {
        let index = result.names.iter().position(|n| n == name).unwrap();
        result.data[index][0]
    };
    assert!((value("x") - 2.0).abs() < 1e-9, "x = {}", value("x"));
    assert!((value("q") - 3.0).abs() < 1e-9, "q = {}", value("q"));
}
