//! MLS 3.7 §3.7.1: `sign(v)` "expands into noEvent(if v > 0 then 1 else if
//! v < 0 then -1 else 0)", so its value is an Integer for a Real operand too,
//! and may be assigned to an Integer (as `Modelica.Math.Nonlinear` does with
//! `s := sign(is)`).

use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
model SignInteger
  function signedCount
    input Real v;
    input Integer n;
    output Integer count;
  protected
    Integer s;
  algorithm
    s := sign(v);
    count := 0;
    for i in 1:n loop
      count := count + s;
    end for;
  end signedCount;
  parameter Real p = -2.5;
  Integer k = signedCount(p, 3);
  Integer direct = sign(p);
  Real x(start = 0, fixed = true);
equation
  der(x) = sign(p)*p + k;
end SignInteger;
"#;

fn last(sim: &rumoca_sim::SimResult, name: &str) -> f64 {
    let column = sim
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("trace contains `{name}`; names={:?}", sim.names));
    *sim.data[column].last().expect("trace is nonempty")
}

#[test]
fn sign_of_a_real_is_an_integer_in_functions_and_equations() {
    let compiled = rumoca::Compiler::new()
        .model("SignInteger")
        .compile_str(SOURCE, "sign_integer.mo")
        .expect("an Integer receives sign of a Real");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .expect("model simulates");
    assert_eq!(last(&sim, "k"), -3.0);
    assert_eq!(last(&sim, "direct"), -1.0);
    // der(x) = (-1)(-2.5) + (-3) = -0.5
    assert!((last(&sim, "x") + 0.5).abs() < 1.0e-6);
}
