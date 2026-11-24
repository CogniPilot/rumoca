//! MLS §12.3 zero-output call statements inside function bodies whose only
//! effect is checking (#687). MSL 4.1.0's `Modelica.Utilities.Streams.error`
//! is `assert(false, message, AssertionLevel.error)`; a function that calls it
//! as a statement raises that assertion where the call stands.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package Checked
  pure function error
    input String string;
  algorithm
    assert(false, string, AssertionLevel.error);
  end error;
  function checked
    input Real x;
    output Real y;
  algorithm
    if x > 1 then
      error("x exceeds one");
    end if;
    y := 2*x;
  end checked;
  model Within
    Real y = checked(0.5*time);
  end Within;
  model Beyond
    Real y = checked(2*time);
  end Beyond;
end Checked;
"#;

fn simulate(model: &str) -> Result<rumoca_sim::SimResult, String> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "Checked.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:#}"));
    simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default())
        .map_err(|error| format!("{error:#}"))
}

#[test]
fn a_checking_call_in_a_function_body_computes_while_its_check_holds() {
    let result = simulate("Checked.Within").expect("the check holds for the whole run");
    let index = result.names.iter().position(|name| name == "y").unwrap();
    let y = *result.data[index].last().unwrap();
    assert!((y - 1.0).abs() < 1e-12, "y(1) = {y}");
}

#[test]
fn a_checking_call_in_a_function_body_fails_with_its_message() {
    let error = simulate("Checked.Beyond").expect_err("the check fails from t = 0.5");
    assert!(error.contains("x exceeds one"), "{error}");
}

/// A callee outside the checking contract (its body loops) is left as the
/// call statement it is, refused by name as before.
#[test]
fn a_call_to_a_function_that_does_more_than_check_keeps_its_refusal() {
    let source = r#"
package Busy
  pure function spin
    input Real x;
  protected
    Real y = x;
  algorithm
    while y > 1 loop
      y := y / 2;
    end while;
  end spin;
  function caller
    input Real x;
    output Real z;
  algorithm
    spin(x);
    z := x;
  end caller;
  model M
    Real z = caller(time);
  end M;
end Busy;
"#;
    let error = Compiler::new()
        .model("Busy.M")
        .compile_str(source, "Busy.mo")
        .expect_err("a call statement that loops has no owner");
    let message = format!("{error:#}");
    assert!(
        message.contains("calls `spin` as a statement without reading a result"),
        "{message}"
    );
}
