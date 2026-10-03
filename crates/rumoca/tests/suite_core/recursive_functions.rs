//! MLS 3.7 §12.2: "A function can be recursive." A recursive call SCC lowers
//! to one SOLVE-C62 recursive owner group bounded by the execution profile's
//! declared depth limit (SPEC_0022 FUNC-043).

use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
package Recursion
  partial function Integrand
    input Real u;
    output Real y;
  end Integrand;
  function wave
    extends Integrand;
    input Real w;
  algorithm
    y := sin(w*u);
  end wave;
  function simpson
    "Adaptive Simpson step: recurses on both halves until the estimate settles"
    input Integrand f;
    input Real a;
    input Real b;
    input Real whole;
    input Real tolerance;
    output Real value;
  protected
    Real m;
    Real left;
    Real right;
  algorithm
    m := (a + b)/2;
    left := (m - a)/6*(f(a) + 4*f((a + m)/2) + f(m));
    right := (b - m)/6*(f(m) + 4*f((m + b)/2) + f(b));
    if abs(left + right - whole) <= 15*tolerance or b - a < 1e-6 then
      value := left + right + (left + right - whole)/15;
    else
      value := simpson(f, a, m, left, tolerance/2) + simpson(f, m, b, right, tolerance/2);
    end if;
  end simpson;
  function integrate
    input Integrand f;
    input Real a;
    input Real b;
    output Real value;
  algorithm
    value := simpson(f, a, b, (b - a)/6*(f(a) + 4*f((a + b)/2) + f(b)), 1e-12);
  end integrate;
  function isEven
    input Integer n;
    output Boolean even;
  algorithm
    even := if n == 0 then true else isOdd(n - 1);
  end isEven;
  function isOdd
    input Integer n;
    output Boolean odd;
  algorithm
    odd := if n == 0 then false else isEven(n - 1);
  end isOdd;
  function power
    input Real x;
    input Integer n;
    output Real y;
  algorithm
    y := if n == 0 then 1 else x*power(x, n - 1);
  end power;
  model Model
    parameter Real w = 2;
    parameter Integer n = 7;
    final parameter Real s = integrate(function wave(w = w), 0, 1);
    parameter Boolean odd = isOdd(n);
    Real cube = power(time, 3);
    Real q(start = 1, fixed = true);
  equation
    der(q) = if odd then -s*q else s*q;
  end Model;
end Recursion;
"#;

fn compile(source: &str) -> Result<rumoca::CompilationResult, String> {
    rumoca::Compiler::new()
        .model("Recursion.Model")
        .compile_str(source, "recursion.mo")
        .map_err(|error| format!("{error:#}"))
}

fn last(sim: &rumoca_sim::SimResult, name: &str) -> f64 {
    let column = sim
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("trace contains `{name}`; names={:?}", sim.names));
    *sim.data[column].last().expect("trace is nonempty")
}

fn simulate(source: &str) -> Result<rumoca_sim::SimResult, String> {
    let compiled = compile(source)?;
    simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .map_err(|error| error.to_string())
}

#[test]
fn recursive_and_mutually_recursive_functions_simulate() {
    let sim = simulate(SOURCE).expect("recursive functions simulate");
    // s = (1 - cos 2)/2 and 7 is odd, so q(1) = exp(-s).
    let s = (1.0 - 2.0f64.cos()) / 2.0;
    assert!(
        (last(&sim, "q") - (-s).exp()).abs() < 1e-6,
        "q={}",
        last(&sim, "q")
    );
    assert!((last(&sim, "cube") - 1.0).abs() < 1e-12);
}

#[test]
fn recursion_deeper_than_the_profile_limit_fails_with_a_typed_error() {
    let source = SOURCE.replace("parameter Integer n = 7;", "parameter Integer n = 101;");
    let error = simulate(&source).expect_err("101 nested calls exceed the depth limit");
    assert!(error.contains("depth limit"), "{error}");
}

#[test]
fn recursive_function_with_an_assertion_is_refused() {
    let source = SOURCE.replace(
        "    even := if n == 0 then true else isOdd(n - 1);",
        "    assert(n >= 0, \"n is a count\");\n    even := if n == 0 then true else isOdd(n - 1);",
    );
    let error = simulate(&source).expect_err("SOLVE-C62 admits no call-scoped assertion");
    assert!(
        error.contains("recursive function groups cannot carry call-scoped assertions"),
        "{error}"
    );
}

#[test]
fn fmi_c_profile_refuses_recursive_calls() {
    let compiled = compile(SOURCE).expect("recursive functions compile");
    for target in ["fmi3", "fmi2"] {
        let error = match rumoca::render_target_files(&compiled, "Model", target, None) {
            Ok(_) => panic!("{target}: the C profile must refuse a recursive owner group"),
            Err(error) => format!("{error:#}"),
        };
        assert!(
            error.contains("recursive function calls"),
            "{target}: {error}"
        );
    }
}

#[test]
fn derivative_through_a_recursive_call_is_refused() {
    let source = SOURCE.replace(
        "    der(q) = if odd then -s*q else s*q;",
        "    der(q) = -power(q, 2);",
    );
    let error = simulate(&source).expect_err("SOLVE-C62 members have no directional relation");
    assert!(error.contains("directional"), "{error}");
}
