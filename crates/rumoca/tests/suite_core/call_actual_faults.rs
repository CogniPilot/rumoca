//! Faults in actual arguments (SPEC_0040 DAE-C18, MLS 3.7 §12.4).
//!
//! Every actual is evaluated before the call, so an out-of-bounds address in
//! an actual is an error when the call executes, whether or not the callee
//! reads that formal. A literal address is refused at translation; a
//! run-time address is a checked fault of the Solve program that evaluates
//! the actual. Dependency projection is not an owner of either.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
function selectValue
  input Boolean first;
  input Real value;
  input Real unused;
  output Real y;
algorithm
  y := if first then value else 0;
end selectValue;

model UnusedFormal
  Real s[1] = {time};
  discrete Integer k(start = 1, fixed = true);
  Real y;
equation
  when time > 0.5 then
    k = 2;
  end when;
  y = selectValue(true, time, s[k]);
end UnusedFormal;

model ConditionalFormal
  Real s[1] = {time};
  discrete Integer k(start = 1, fixed = true);
  Real y;
equation
  when time > 0.5 then
    k = 2;
  end when;
  y = selectValue(false, s[k], time);
end ConditionalFormal;

model LiteralAddress
  Real s[1] = {time};
  Real y;
equation
  y = selectValue(false, time, s[2]);
end LiteralAddress;

model InBounds
  Real s[2] = {time, 2*time};
  discrete Integer k(start = 1, fixed = true);
  Real y;
equation
  when time > 0.5 then
    k = 2;
  end when;
  y = selectValue(true, s[k], s[k]);
end InBounds;
"#;

fn simulate(model: &str) -> Result<rumoca_sim::SimResult, String> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "CallActualFaults.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())
}

#[test]
fn a_run_time_address_fault_in_an_unread_actual_fails_the_call() {
    for model in ["UnusedFormal", "ConditionalFormal"] {
        let error = simulate(model).expect_err(model);
        assert!(
            error.contains("index 2") && error.contains("outside 1..=1"),
            "{model}: {error}"
        );
    }
}

#[test]
fn a_literal_address_fault_in_an_unread_actual_is_refused_at_translation() {
    let error = Compiler::new()
        .model("LiteralAddress")
        .compile_str(SOURCE, "CallActualFaults.mo")
        .expect_err("an out-of-bounds literal address is refused");
    assert!(format!("{error:?}").contains("out of bounds"), "{error:?}");
}

#[test]
fn in_bounds_run_time_addresses_simulate() {
    let result = simulate("InBounds").expect("in-bounds addresses simulate");
    assert!(result.times.last().is_some_and(|t| *t >= 1.0 - 1e-9));
}
