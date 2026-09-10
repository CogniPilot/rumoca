//! Function-body quotient lowering (MLS 3.7 §3.7.2 Operators 3.4/3.5/3.6).
//!
//! A pure Modelica function containing `div`/`mod`/`rem` lowers to the solve
//! IR as the definitionally exact arithmetic composition — `mod` floored,
//! `div`/`rem` truncated — because §3.7.2 function bodies are event-free.
//! These pins value-check each operator against its MLS definition computed
//! independently in Rust, straddling a wrap boundary and a sign change, pin
//! the mixed Integer-operand promotion, and pin that a checked Integer
//! result keeps the typed pure-call rejection instead of riding Binary64.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

fn series<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("simulation result missing column {name}"));
    result.data[index].as_slice()
}

const WRAP_MOD: &str = r#"
model WrapMod
  function wrap
    input Real u;
    output Real y;
  algorithm
    y := mod(u, 2.5);
  end wrap;
  Real x(start = 1.0, fixed = true);
  Real y;
equation
  der(x) = 2;
  y = wrap(x);
end WrapMod;
"#;

#[test]
fn function_body_mod_matches_the_floored_definition_across_a_wrap() {
    let compiled = Compiler::new()
        .model("WrapMod")
        .compile_str(WRAP_MOD, "WrapMod.mo")
        .expect("function-body mod compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("function-body mod simulates");
    let xs = series(&result, "x");
    let ys = series(&result, "y");
    // x runs 1.0 → 3.0, crossing the 2.5 wrap boundary mid-run.
    assert!(xs.first().is_some_and(|x| *x < 2.5));
    assert!(xs.last().is_some_and(|x| *x > 2.5));
    for (x, y) in xs.iter().zip(ys) {
        let expected = x - (x / 2.5).floor() * 2.5;
        assert!(
            (y - expected).abs() < 1e-9,
            "mod({x}, 2.5): lowered {y}, MLS floored definition {expected}"
        );
    }
}

// The divisor stays a literal inside the function: the checked DAE
// admission requires a translation-frozen divisor as seen from the body (a
// function input is call-time varying), which is exactly the wrapAngle
// shape this wave targets.
const TRUNCATED_FORMS: &str = r#"
model TruncatedForms
  function quotients
    input Real u;
    output Real d;
    output Real r;
  algorithm
    d := div(u, 1.5);
    r := rem(u, 1.5);
  end quotients;
  Real x(start = 0.0, fixed = true);
  Real u;
  Real d;
  Real r;
equation
  der(x) = 1;
  u = x - 2;
  (d, r) = quotients(u);
end TruncatedForms;
"#;

#[test]
fn function_body_div_and_rem_match_the_truncated_definitions_across_a_sign_change() {
    let compiled = Compiler::new()
        .model("TruncatedForms")
        .compile_str(TRUNCATED_FORMS, "TruncatedForms.mo")
        .expect("function-body div/rem compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 4.0,
            ..SimOptions::default()
        },
    )
    .expect("function-body div/rem simulates");
    let us = series(&result, "u");
    let ds = series(&result, "d");
    let rs = series(&result, "r");
    // u runs -2.0 → 2.0: negative dividends make truncation differ from
    // flooring, so a floored implementation fails these samples.
    assert!(us.first().is_some_and(|u| *u < 0.0));
    assert!(us.last().is_some_and(|u| *u > 0.0));
    for ((u, d), r) in us.iter().zip(ds).zip(rs) {
        let expected_div = (u / 1.5).trunc();
        let expected_rem = u - expected_div * 1.5;
        assert!(
            (d - expected_div).abs() < 1e-9,
            "div({u}, 1.5): lowered {d}, MLS truncated definition {expected_div}"
        );
        assert!(
            (r - expected_rem).abs() < 1e-9,
            "rem({u}, 1.5): lowered {r}, MLS definition {expected_rem}"
        );
    }
}

const MIXED_INTEGER_DIVISOR: &str = r#"
model MixedIntegerDivisor
  function wrapN
    input Real u;
    output Real y;
  algorithm
    y := mod(u, 3);
  end wrapN;
  Real x(start = 0.5, fixed = true);
  Real y;
equation
  der(x) = 3;
  y = wrapN(x);
end MixedIntegerDivisor;
"#;

#[test]
fn mixed_integer_divisor_promotes_and_matches_the_floored_definition() {
    let compiled = Compiler::new()
        .model("MixedIntegerDivisor")
        .compile_str(MIXED_INTEGER_DIVISOR, "MixedIntegerDivisor.mo")
        .expect("mixed Integer divisor compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 2.0,
            ..SimOptions::default()
        },
    )
    .expect("mixed Integer divisor simulates");
    let xs = series(&result, "x");
    let ys = series(&result, "y");
    assert!(xs.last().is_some_and(|x| *x > 3.0), "run crosses the wrap");
    for (x, y) in xs.iter().zip(ys) {
        let expected = x - (x / 3.0).floor() * 3.0;
        assert!(
            (y - expected).abs() < 1e-9,
            "mod({x}, 3): lowered {y}, MLS floored definition {expected}"
        );
    }
}

const SIGNED_MLS_EXAMPLES: &str = r#"
model SignedMlsExamples
  function modNegDivisor
    input Real u;
    output Real y;
  algorithm
    y := mod(u, -1.4);
  end modNegDivisor;
  function remPosDivisor
    input Real u;
    output Real y;
  algorithm
    y := rem(u, 1.4);
  end remPosDivisor;
  Real x(start = 0.0, fixed = true);
  Real m;
  Real r;
equation
  der(x) = 1;
  m = modNegDivisor(2.0 + x);
  r = remPosDivisor(-4.0 + x);
end SignedMlsExamples;
"#;

#[test]
fn signed_mls_examples_match_the_normative_values() {
    // MLS 3.7 §3.7.2 Operators 3.5/3.6 normative examples:
    // mod(3, -1.4) = -1.2 and rem(-3, 1.4) = -0.2, covering the negative
    // divisor the dynamic fixtures do not reach.
    let compiled = Compiler::new()
        .model("SignedMlsExamples")
        .compile_str(SIGNED_MLS_EXAMPLES, "SignedMlsExamples.mo")
        .expect("signed example fixture compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("signed example fixture simulates");
    // At the final sample x = 1, so the arguments are exactly 3 and -3.
    let m = *series(&result, "m").last().expect("final mod sample");
    let r = *series(&result, "r").last().expect("final rem sample");
    assert!(
        (m - (-1.2)).abs() < 1e-9,
        "mod(3, -1.4): lowered {m}, MLS example -1.2"
    );
    assert!(
        (r - (-0.2)).abs() < 1e-9,
        "rem(-3, 1.4): lowered {r}, MLS example -0.2"
    );
}

const INTEGER_RESULT: &str = r#"
model IntegerResult
  function idiv
    input Integer i;
    output Integer k;
  algorithm
    k := div(i, 2);
  end idiv;
  parameter Integer i0 = 7;
  Real x(start = 0.0, fixed = true);
  Real y;
equation
  der(x) = 1;
  y = idiv(i0)*1.0 + x;
end IntegerResult;
"#;

#[test]
fn integer_result_quotient_keeps_the_typed_pure_call_rejection() {
    // The [148] boundary: an Integer-result quotient must not ride Binary64
    // (exact above 2^53 is unrepresentable), so preparation fails typed
    // until a dedicated typed integer quotient operation exists.
    let compiled = Compiler::new()
        .model("IntegerResult")
        .compile_str(INTEGER_RESULT, "IntegerResult.mo")
        .expect("the Integer-result fixture still constructs its DAE");
    let error = simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default())
        .expect_err("an Integer-result function quotient must fail typed");
    let message = error.to_string();
    assert!(
        message.contains("pure-call argument or slot interface is invalid"),
        "expected the typed pure-call rejection, got: {message}"
    );
}

const NO_EVENT_IN_FUNCTION: &str = r#"
model NoEventInFunction
  function guarded
    input Real u;
    output Real y;
  algorithm
    y := noEvent(abs(u));
  end guarded;
  Real x(start = 1.0, fixed = true);
  Real y;
equation
  der(x) = -1;
  y = guarded(x);
end NoEventInFunction;
"#;

const SMOOTH_IN_FUNCTION: &str = r#"
model SmoothInFunction
  function smoothed
    input Real u;
    output Real y;
  algorithm
    y := smooth(1, u*u);
  end smoothed;
  Real x(start = 1.0, fixed = true);
  Real y;
equation
  der(x) = -1;
  y = smoothed(x);
end SmoothInFunction;
"#;

const HOMOTOPY_IN_FUNCTION: &str = r#"
model HomotopyInFunction
  function blended
    input Real u;
    output Real y;
  algorithm
    y := homotopy(u*u, u);
  end blended;
  Real x(start = 1.0, fixed = true);
  Real y;
equation
  der(x) = -1;
  y = blended(x);
end HomotopyInFunction;
"#;

fn simulate_function_builtin(source: &str, model: &str) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .expect("the function-body builtin fixture constructs its DAE");
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 2.0,
            ..SimOptions::default()
        },
    )
    .expect("the admitted function-body builtin simulates")
}

#[test]
fn no_event_in_function_body_is_value_transparent() {
    let result = simulate_function_builtin(NO_EVENT_IN_FUNCTION, "NoEventInFunction");
    for (x, y) in series(&result, "x").iter().zip(series(&result, "y")) {
        assert!((y - x.abs()).abs() < 1e-9, "noEvent(abs({x})) produced {y}");
    }
}

#[test]
fn smooth_in_function_body_is_value_transparent() {
    let result = simulate_function_builtin(SMOOTH_IN_FUNCTION, "SmoothInFunction");
    for (x, y) in series(&result, "x").iter().zip(series(&result, "y")) {
        assert!((y - x * x).abs() < 1e-9, "smooth(1, {x}*{x}) produced {y}");
    }
}

#[test]
fn homotopy_in_function_body_keeps_its_typed_rejection() {
    // Model-level homotopy owns a checked continuation parameter. Function
    // programs do not yet carry that owner, so accepting this call would
    // silently choose one branch and create a wrong-code path.
    let compiled = Compiler::new()
        .model("HomotopyInFunction")
        .compile_str(HOMOTOPY_IN_FUNCTION, "HomotopyInFunction.mo")
        .expect("the homotopy fixture constructs its DAE");
    let error = simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default())
        .expect_err("function-body homotopy must fail without a continuation owner");
    assert!(
        error
            .to_string()
            .contains("pure-call argument or slot interface is invalid"),
        "expected the typed pure-call rejection, got: {error}"
    );
}
