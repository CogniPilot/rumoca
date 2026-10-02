//! MLS §8.3.7 assertion levels.
//!
//! "If the level is AssertionLevel.warning, the current evaluation is not
//! aborted", and "the assert(..) statement shall have no influence on the
//! behavior of the model". A violated warning-level assertion, in a function
//! body (the pump characteristics of `Modelica.Fluid.Machines`) or in an
//! equation section, never fails the simulation; an explicit
//! `AssertionLevel.error` is the default level and fails it.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const MODEL: &str = r#"
model Levels
  function head
    input Real q;
    output Real h;
  algorithm
    assert(q < 0.5, "flow beyond the nominal curve", level = LEVEL);
    h := 2 - q;
  end head;
  Real h = head(time);
  Real x(start = 0, fixed = true);
equation
  der(x) = h;
  assert(x < 0.5, "x beyond its nominal range", level = LEVEL);
end Levels;
"#;

fn simulate(level: &str) -> Result<SimResult, String> {
    let source = MODEL.replace("LEVEL", level);
    let compiled = Compiler::new()
        .model("Levels")
        .compile_str(&source, "Levels.mo")
        .unwrap_or_else(|error| panic!("Levels compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error:?}"))
}

#[test]
fn violated_warning_assertions_never_abort_the_simulation() {
    let result = simulate("AssertionLevel.warning").expect("warnings never abort");
    let x = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("x is recorded");
    let final_x = result.data[x].last().copied().expect("x has samples");
    // der(x) = 2 - t, so x(1) = 1.5: both assertions are violated before t = 1.
    assert!((final_x - 1.5).abs() < 1e-6, "x(1) = {final_x}");
}

#[test]
fn an_explicit_error_level_fails_like_the_default() {
    let error = simulate("AssertionLevel.error").expect_err("error-level assertions abort");
    assert!(
        error.contains("x beyond its nominal range") || error.contains("flow beyond"),
        "{error}"
    );
}
