//! MLS §8.3.7 assertions inside MLS §11.5 runtime function conditionals.
//!
//! A branch assertion belongs to the call-scoped action sequence guarded by
//! its branch selection: it fails exactly when the executed path fails it,
//! reads the definitions the branch has made before it, and never fires on a
//! path that does not execute it. The body mirrors the argument checks of
//! `Modelica.Fluid.Utilities.regFun3`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

fn source(model: &str, call: &str) -> String {
    format!(
        r#"
model {model}
  function f
    input Real x;
    input Real a;
    input Real b;
    output Real y;
  protected
    Real d;
  algorithm
    if a*b >= 0 then
    else
      assert(abs(a) < 1e-10 or abs(b) < 1e-10, "opposite signs");
    end if;
    d := b - a;
    if abs(d) <= 0 then
      y := a;
    elseif x > 0 then
      d := d*2;
      assert(d > -100, "d too negative");
      y := a + d*x;
    else
      y := b;
    end if;
  end f;
  Real z(start = 0, fixed = true);
equation
  der(z) = {call};
end {model};
"#
    )
}

fn simulate(model: &str, call: &str) -> Result<SimResult, String> {
    let source = source(model, call);
    let compiled = Compiler::new()
        .model(model)
        .compile_str(&source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .map_err(|error| error.to_string())
}

#[test]
fn branch_assertions_that_hold_leave_the_function_value_unchanged() {
    let result = simulate("BranchAssertionsHold", "f(time, 1, 2 + time)")
        .expect("no executed branch assertion fails");
    let column = result
        .names
        .iter()
        .position(|name| name == "z")
        .expect("z is recorded");
    let z = result.data[column].last().copied().expect("z has samples");
    // der(z) = 1 + 2(1 + t)t, so z(1) = 1 + 1 + 2/3.
    assert!((z - 8.0 / 3.0).abs() < 1e-5, "z(1) = {z}");
}

#[test]
fn an_else_branch_assertion_fails_when_its_branch_executes() {
    let error = simulate("ElseBranchAssertionFails", "f(time, 1, -2 - time)")
        .expect_err("the else branch runs with opposite-sign derivatives");
    assert!(error.contains("opposite signs"), "{error}");
}

#[test]
fn a_branch_assertion_reads_the_definitions_its_branch_made_before_it() {
    // d = 2(-1 - 200t) in the executed branch crosses -100 at t = 0.245; the
    // incoming d = -1 - 200t would not cross it before t = 0.495.
    let error = simulate("BranchLocalAssertionFails", "f(time, -1, -2 - 200*time)")
        .expect_err("the doubled branch-local d violates the assertion");
    assert!(error.contains("d too negative"), "{error}");
    assert!(error.contains("t=0.245"), "{error}");
}
