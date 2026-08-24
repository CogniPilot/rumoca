//! The call boundary: the rank an actual carries into a differentiated call.
//!
//! A Jacobian wrapper reads its own shape out of declarations, so a call whose
//! actual has more dimensions than its formal, which Modelica evaluates once
//! per element (MLS 12.4.6), has a result no wrapper can name. These checks
//! pin that the boundary is refused where the author wrote it, and that the
//! shapes the wrapper does state are the ones the call really produces.

use crate::tests::{expand, refuse};
use crate::{Refusal, Rule, expand_source};

/// A scalar formal reached by a `Real[3]` actual. `sq(x)` is ordinary
/// Modelica, and the Jacobian of it is `Real[3, 3]`; the wrapper's own rule
/// would state `Real[1, 1]`, which is the shape of neither.
const VECTORIZED: &str = r"
function sq
  input Real a;
  output Real b;
algorithm
  b := a*a*a;
end sq;

model Refused
  parameter Real x[3] = {0.44, -0.55, 0.66};
  Real J[3, 3] = jacobian(sq(x), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end Refused;
";

/// The same call under a declaration the collapsed wrapper happens to fit.
/// Nothing downstream can catch this one: `Real[3, 1, 1]` is exactly what the
/// vectorized wrapper returns, so the artifact runs here and states a Jacobian
/// that is not the `Real[m, n]` JAC-S3 promises.
const VECTORIZED_AND_FITTING: &str = r"
function sq
  input Real a;
  output Real b;
algorithm
  b := a*a*a;
end sq;

model Refused
  parameter Real x[3] = {0.44, -0.55, 0.66};
  Real J[3, 1, 1] = jacobian(sq(x), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end Refused;
";

#[test]
fn a_vectorized_call_refuses_at_the_call_site() {
    let refusal = refuse(VECTORIZED);
    assert_eq!(refusal.rule, Rule::ActualShape);
    assert_eq!(refusal.site.line, 11);
    assert!(
        refusal.detail.contains("vectorizes"),
        "the refusal must say why: {refusal}"
    );
}

/// The declaration downstream must not decide the verdict: the refusal is a
/// fact about the call, and the model's own `Real[3, 1, 1]` never reaches it.
#[test]
fn a_vectorized_call_refuses_even_where_the_collapsed_shape_fits() {
    let refusal = refuse(VECTORIZED_AND_FITTING);
    assert_eq!(refusal.rule, Rule::ActualShape);
    assert_eq!(refusal.site.line, 11);
}

/// A non-differentiated actual can vectorize a call just as a differentiated
/// one can, and the wrapper states the same wrong shape when it does.
#[test]
fn a_vectorized_companion_argument_refuses() {
    let refusal = refuse(
        r"
function scaled
  input Real a[3];
  input Real k;
  output Real y[3];
algorithm
  y := k*a;
end scaled;

model Refused
  parameter Real x[3] = {0.3, -0.4, 0.5};
  parameter Real k[3] = {1.0, 2.0, 3.0};
  Real J[3, 3] = jacobian(scaled(x, k), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::ActualShape);
}

/// A rank this pass cannot read is refused, not assumed. The expansion runs on
/// document text before resolution (JAC-E1), so a call whose actual is not a
/// declaration it can read has no rank to compare against the formal's.
#[test]
fn an_actual_whose_rank_is_not_stated_refuses() {
    let refusal = refuse(
        r"
function fscale
  input Real x[3];
  input Real k;
  output Real y[3];
algorithm
  y := k*x;
end fscale;

model Refused
  parameter Real x[3] = {0.3, -0.4, 0.5};
  parameter Real k = 2.5;
  Real J[3, 3] = jacobian(fscale(x, 2.0*k), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::ActualShape);
    assert!(
        refusal.detail.contains("cannot read"),
        "the refusal must say the rank was unreadable: {refusal}"
    );
}

/// A subscript that selects one element lowers the rank the actual carries,
/// and the formal it reaches is the scalar one.
#[test]
fn a_subscripted_actual_carries_the_rank_its_subscripts_leave() {
    let expanded = expand(
        r"
function sq
  input Real a;
  output Real b;
algorithm
  b := a*a*a;
end sq;

model Probe
  parameter Real x[3] = {0.44, -0.55, 0.66};
  Real J[1, 1] = jacobian(sq(x[2]), x[2]);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end Probe;
",
    );
    assert!(
        expanded.contains("output Real J_ad[1, 1];"),
        "the wrapper must state the scalar shape:\n{expanded}"
    );
}

/// A matching call keeps its wrapper, and the wrapper states the shape the
/// call really produces: three columns from the input, two rows from the
/// output.
#[test]
fn a_call_whose_actuals_match_its_formals_is_admitted() {
    let expanded = expand(
        r"
function fscale
  input Real x[3];
  input Real k;
  output Real y[2];
algorithm
  y[1] := k*x[1] + x[2];
  y[2] := x[3]*x[3];
end fscale;

model Probe
  parameter Real x[3] = {0.3, -1.2, 0.7};
  parameter Real k = 2.5;
  Real J[2, 3] = jacobian(fscale(x, k), x);
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end Probe;
",
    );
    assert!(
        expanded.contains("output Real J_ad[2, 3];"),
        "the wrapper must state the call's own shape:\n{expanded}"
    );
}

/// The refusal reaches both artifacts, because both reach the expansion
/// through one `expand_source`.
#[test]
fn a_vectorized_call_refuses_in_the_portable_writer_too() {
    let error = expand_source(VECTORIZED, "Refused.mo").expect_err("the expansion refuses");
    let rendered = format!("{error}");
    assert!(
        rendered.contains("JAC-R10") && rendered.contains("Refused.mo:11:"),
        "the refusal must carry its rule and its site: {rendered}"
    );
}

/// A refusal is a fact about one construct, so nothing else about the call is
/// read before it fires.
#[test]
fn the_vectorized_refusal_mints_nothing() {
    let Refusal { rule, .. } = refuse(VECTORIZED);
    assert_eq!(rule, Rule::ActualShape);
    assert!(
        expand_source(VECTORIZED, "Refused.mo").is_err(),
        "a refused call must produce no expansion at all"
    );
}
