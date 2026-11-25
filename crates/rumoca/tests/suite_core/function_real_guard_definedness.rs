//! MLS 3.7 §12.4.4 definedness of a value a conditional over a Real leaves
//! undefined on some paths, read later under a condition that excludes them.
//!
//! `Modelica.Media.Air.MoistAir.Utilities.spliceFunction_der` assigns
//! `scaledXt` in the else arm of
//! `if scaledX1 <= -lim ... elseif scaledX1 >= lim ... else`, and reads it
//! under `abs(scaledX1) < lim`. The read is valid because that condition
//! holds on no path that skipped the assignment. A condition that leaves one
//! of those paths possible proves nothing, and the function keeps its refusal.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

/// `t` is assigned in one arm of `guards` and read under `condition`.
fn source(guards: &str, condition: &str) -> String {
    format!(
        r#"
model Splice
  function f
    input Real x;
    output Real out;
  protected
    constant Real lim = 1.0;
    Real s;
    Real t;
    Real y;
  algorithm
    s := x/2;
{guards}
    out := y;
    if {condition} then
      out := out + t;
    end if;
  end f;
  Real r = f(time);
end Splice;
"#
    )
}

/// One guard: `t` is undefined where `s >= lim`.
const ABOVE: &str = "    if s >= lim then
      y := 1.0;
    else
      t := tan(s);
      y := 0.5;
    end if;";

/// The last value of `r`, or the refusal.
fn last(guards: &str, condition: &str) -> Result<f64, String> {
    let compiled = Compiler::new()
        .model("Splice")
        .compile_str(&source(guards, condition), "Splice.mo")
        .map_err(|error| format!("{error:?}"))?;
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error:?}"))?;
    let column = result
        .names
        .iter()
        .position(|name| name == "r")
        .expect("the result records r");
    Ok(*result.data[column].last().expect("a sample"))
}

/// At time 1 `s = 0.5`, inside every accepted condition here.
fn assert_defined(guards: &str, condition: &str) {
    let value = last(guards, condition).unwrap_or_else(|error| panic!("{condition}: {error}"));
    assert!(
        (value - (0.5 + 0.5f64.tan())).abs() < 1e-9,
        "{condition}: r(1) = {value}"
    );
}

fn assert_refused(guards: &str, condition: &str) {
    let error = last(guards, condition).expect_err(condition);
    assert!(
        error.contains("reads `t`, which only some branches of the conditional"),
        "{condition}: {error}"
    );
}

#[test]
fn a_strict_comparison_excludes_the_path_its_endpoint_begins() {
    // `s < lim` and the undefined path `s >= lim` share no value.
    assert_defined(ABOVE, "s < lim");
    assert_defined(ABOVE, "lim > s");
    assert_defined(ABOVE, "s <= 0.5");
}

#[test]
fn a_comparison_that_admits_the_endpoint_keeps_the_refusal() {
    // `s <= lim` holds at `s = lim`, where `t` is undefined.
    assert_refused(ABOVE, "s <= lim");
    // A failed `>=` also holds for a NaN, which no path here excludes.
    assert_refused(ABOVE, "not (s >= lim)");
}

/// Two guards, as in `spliceFunction_der`: `t` is undefined where
/// `s <= -lim` and where `s >= lim`.
const OUTSIDE: &str = "    if s <= -lim then
      y := 0.0;
    elseif s >= lim then
      y := 1.0;
    else
      t := tan(s);
      y := 0.5;
    end if;";

#[test]
fn a_condition_between_two_undefined_ranges_excludes_both() {
    assert_defined(OUTSIDE, "s >= -0.5 and s <= 0.5");
    assert_defined(OUTSIDE, "s > -lim and s < lim");
}

#[test]
fn a_condition_that_reaches_one_undefined_range_keeps_the_refusal() {
    assert_refused(OUTSIDE, "s >= -0.5 and s <= 1.5");
    assert_refused(OUTSIDE, "s < lim");
}

#[test]
fn a_bounded_magnitude_excludes_both_undefined_ranges() {
    // The shape of `spliceFunction_der`.
    assert_defined(OUTSIDE, "abs(s) < lim");
    assert_defined(OUTSIDE, "lim > abs(s)");
    assert_defined(ABOVE, "abs(s) <= 0.5");
}

#[test]
fn a_magnitude_that_reaches_an_undefined_range_keeps_the_refusal() {
    // `abs(s) <= lim` holds at both endpoints, where `t` is undefined.
    assert_refused(OUTSIDE, "abs(s) <= lim");
    // A magnitude above a bound is two ranges, not one.
    assert_refused(ABOVE, "abs(s) > 0.5");
    // A failed `abs(s) >= lim` also holds for a NaN.
    assert_refused(OUTSIDE, "not (abs(s) >= lim)");
}
