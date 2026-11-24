//! A derivative's coefficient that is a parameter given by a function call.
//!
//! Solve lowering probes a matched derivative's coefficient for a declared
//! zero. The probe folds arithmetic over declarations, not a function call; a
//! coefficient it cannot fold is unknown, and the program checks it where it
//! runs. Refusing the model there ("affine derivative coefficient is not
//! compile-time numeric") stopped every discretized volume whose cell mass is
//! `V/N*Medium.density_pTX(..)`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const FUNCTION: &str = r#"
function twice
  input Real u;
  output Real y;
algorithm
  y := 2*u;
end twice;
"#;

fn last(model: &str, body: &str, name: &str) -> f64 {
    let source = format!("{FUNCTION}\nmodel {model}\n{body}\nend {model};\n");
    let compiled = Compiler::new()
        .model(model)
        .compile_str(&source, "coefficient.mo")
        .unwrap_or_else(|error| panic!("{model}: {error:#}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 2.0,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model}: {error}"));
    let column = result
        .names
        .iter()
        .position(|n| n == name)
        .unwrap_or_else(|| panic!("{model}: no {name}"));
    *result.data[column].last().expect("a sample")
}

/// `m = twice(1) = 2`: `h(2) = 1 - e^-1`.
#[test]
fn a_scalar_derivative_scaled_by_a_function_valued_parameter_runs() {
    let h = last(
        "Scalar",
        "  parameter Real V = 1;\n  parameter Real m = twice(V);\n  Real h(start = 0, fixed = true);\nequation\n  m*der(h) = 1 - h;",
        "h",
    );
    assert!((h - (1.0 - (-1.0f64).exp())).abs() < 1e-4, "h(2) = {h}");
}

/// The reporter's R6 (#745): `m = twice(1)/3`, so each `h[i](2) = 1 - e^-3 =
/// 0.9502`. Ran until #733 made the scaled derivative of a loop's lane an
/// explicit owner, which reaches the coefficient probe.
#[test]
fn a_loop_s_derivatives_scaled_by_a_function_valued_parameter_run() {
    let h = last(
        "R6",
        "  parameter Real V = 1;\n  parameter Integer N = 3;\n  parameter Real m = twice(V)/N;\n  Real h[N](each start = 0, each fixed = true);\nequation\n  for i in 1:N loop\n    m*der(h[i]) = 1 - h[i];\n  end for;",
        "h[1]",
    );
    assert!((h - 0.9502).abs() < 1e-4, "h[1](2) = {h}");
}

/// A coefficient the probe does fold to zero is still refused by name.
#[test]
fn a_declared_zero_coefficient_is_still_refused() {
    let source = "model Zero\n  parameter Real m = 0;\n  Real h(start = 0, fixed = true);\nequation\n  m*der(h) = 1 - h;\nend Zero;\n";
    let outcome = Compiler::new()
        .model("Zero")
        .compile_str(source, "zero.mo")
        .map_err(|error| format!("{error:#}"))
        .and_then(|compiled| {
            simulate_dae_with_diagnostics(
                &compiled.dae,
                &SimOptions {
                    t_end: 1.0,
                    ..Default::default()
                },
            )
            .map(|_| ())
            .map_err(|error| error.to_string())
        });
    let error = outcome.expect_err("a zero coefficient leaves no derivative");
    assert!(error.contains("zero"), "{error}");
}

/// A function-valued coefficient that is zero passes the probe as unknown and
/// is rejected where the program runs, never silently a derivative. (`twice(0)`
/// itself folds before the probe and is refused as a declared zero; the
/// parameter argument is what the probe cannot fold.)
#[test]
fn a_function_valued_zero_coefficient_is_rejected_at_run_time() {
    let source = format!(
        "{FUNCTION}\nmodel ZeroCall\n  parameter Real V = 0;\n  parameter Real m = twice(V);\n  Real h(start = 0, fixed = true);\nequation\n  m*der(h) = 1 - h;\nend ZeroCall;\n"
    );
    let compiled = Compiler::new()
        .model("ZeroCall")
        .compile_str(&source, "zero_call.mo")
        .unwrap_or_else(|error| panic!("the probe cannot fold twice(V): {error:#}"));
    let error = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .map(|_| ())
    .expect_err("a zero coefficient leaves no derivative");
    // The quotient divides by 0 where the coefficient is not finite and
    // nonzero; the evaluator rejects the non-finite derivative by its state.
    let message = error.to_string();
    assert!(
        message.contains("non-finite derivative evaluation for state 'h'"),
        "{message}"
    );
}
