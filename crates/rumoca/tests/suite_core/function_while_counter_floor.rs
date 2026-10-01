//! `while` loops whose counter starts at a literal below 1 (MLS 3.7 §11.2.3).
//!
//! The `Modelica.Media` inversions (`BaseIF97.Inverses.dofpt3` and its
//! siblings) iterate `while i < IMAX and not found loop ... i := i + 1;` with
//! `Integer i = 0` and `Boolean found = false`, and define their result only
//! inside the loop. The counter rises from 0, so `IMAX + 1` iterations bound
//! the loop; the condition holds on entry, so the first iteration runs on
//! every path and defines the result there.

use rumoca::Compiler;

const SOURCE: &str = r#"
record Derivs
  Real p;
  Real pd;
end Derivs;

function eval
  input Real d;
  input Real T;
  output Derivs n;
algorithm
  n.p := d*d*T;
  n.pd := 2*d*T;
end eval;

function invert "Newton inversion of p = d^2*T for d"
  input Real p;
  input Real T;
  input Real dguess0;
  output Real d;
protected
  Integer i = 0;
  Boolean found = false;
  Real dguess = dguess0;
  Real dp;
  Derivs n;
algorithm
  while i < 50 and not found loop
    d := dguess;
    n := eval(d, T);
    dp := n.p - p;
    if abs(dp/p) <= 1e-12 then
      found := true;
    end if;
    d := d - dp/n.pd;
    if d > 0 then
      dguess := d;
    else
      dguess := 1e-3;
    end if;
    i := i + 1;
  end while;
end invert;

model Inversion
  Real x(start = 1, fixed = true);
  Real d;
equation
  der(x) = 1;
  d = invert(2*x*x, 2, 0.5);
end Inversion;
"#;

#[test]
fn a_zero_started_counter_bounds_the_loop_and_its_first_iteration_defines_the_result() {
    let compiled = Compiler::new()
        .model("Inversion")
        .compile_str(SOURCE, "FunctionWhileCounterFloor.mo")
        .unwrap_or_else(|error| panic!("Inversion compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .expect("Inversion simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    for (index, _) in result.times.iter().enumerate() {
        let x = column("x")[index];
        let d = column("d")[index];
        assert!((d - x).abs() < 1e-8 * x, "d = {d}, expected {x}");
    }
}
