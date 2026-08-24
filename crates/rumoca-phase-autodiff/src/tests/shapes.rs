//! The tangent's shape must be the primal's shape.
//!
//! Modelica spells two different multiplications with adjacent syntax: `*`
//! between two vectors is the scalar product (MLS 10.6.3), `.*` is the
//! elementwise one. A chain rule written with the scalar product where the
//! primal was elementwise stays well typed whenever the surrounding expression
//! absorbs the collapsed rank, so it elaborates and simulates in every tool and
//! answers a different question than the one asked. These checks pin which
//! multiplication each rule writes, and pin the refusals that stand where no
//! rule of the right shape exists.

use crate::Rule;
use crate::tests::{expand, refuse};

#[test]
fn an_elementwise_power_stays_elementwise() {
    let expanded = expand(
        r"
function powelem
  input Real x[3];
  input Real k[3];
  output Real y[3];
algorithm
  y := k .* (x .^ 2);
end powelem;

model Probe
  parameter Real k[3] = {1.0, 3.0, 7.0};
  parameter Real x[3] = {0.5, 1.25, 2.0};
  Real J[3, 3] = jacobian(powelem(x, k), x);
end Probe;
",
    );
    assert!(
        expanded.contains("(2) .* ((x) .^ ((2) - 1)) .* (x_ad)"),
        "an elementwise power must carry elementwise factors, or the tangent collapses to a \
         scalar product:\n{expanded}"
    );
}

#[test]
fn a_scalar_power_keeps_the_scalar_rule() {
    let expanded = expand(
        r"
function cubed
  input Real x;
  output Real y;
algorithm
  y := x^3;
end cubed;

model Probe
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(cubed(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("(3) * ((x) ^ ((3) - 1)) * (x_ad)"),
        "a rank-zero base keeps the scalar chain rule:\n{expanded}"
    );
}

#[test]
fn a_subscripted_array_port_is_a_scalar_base() {
    let expanded = expand(
        r"
function element
  input Real x[3];
  output Real y;
algorithm
  y := x[2]^3;
end element;

model Probe
  parameter Real x[3] = {0.5, 1.25, 2.0};
  Real J[1, 3] = jacobian(element(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("(3) * ((x[2]) ^ ((3) - 1)) * (x_ad[2])"),
        "fully subscripting an array port yields a rank-zero base:\n{expanded}"
    );
}

#[test]
fn a_matrix_power_refuses() {
    let refusal = refuse(
        r"
function squared
  input Real x[2];
  output Real y[2];
protected
  Real a[2, 2];
  Real b[2, 2];
algorithm
  a := {{x[1], 0.5}, {0.25, x[2]}};
  b := a^2;
  y := b*{1.0, 1.0};
end squared;

model Refused
  parameter Real x[2] = {1.5, 0.8};
  Real J[2, 2] = jacobian(squared(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::ExpressionForm);
    assert!(refusal.to_string().contains("scalar"), "{refusal}");
}

#[test]
fn a_power_whose_base_rank_is_unknown_refuses() {
    let refusal = refuse(
        r"
function through
  input Real x;
  output Real y;
protected
  Real t;
algorithm
  t := scaled(x);
  y := (cat(1, {t}, {t}))^2;
end through;

function scaled
  input Real x;
  output Real y;
algorithm
  y := 2.0*x;
end scaled;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(through(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::ExpressionForm);
    assert!(refusal.to_string().contains("scalar"), "{refusal}");
}

#[test]
fn a_non_scalar_exponent_refuses() {
    let refusal = refuse(
        r"
function arrexp
  input Real x[3];
  output Real y[3];
algorithm
  y := x .^ {2, 3, 2};
end arrexp;

model Refused
  parameter Real x[3] = {0.5, 1.25, 2.0};
  Real J[3, 3] = jacobian(arrexp(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::ExpressionForm);
    assert!(refusal.to_string().contains("exponent"), "{refusal}");
}

#[test]
fn a_vectorized_elementary_call_stays_elementwise() {
    let expanded = expand(
        r"
function bent
  input Real x[3];
  output Real y[3];
algorithm
  y := atan(x);
end bent;

model Probe
  parameter Real x[3] = {0.5, 1.25, -2.0};
  Real J[3, 3] = jacobian(bent(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("(x_ad) .* cos(atan(x)) .* cos(atan(x))"),
        "a vectorized call maps its rule over the array, so the rule's own products and \
         quotients have to be elementwise:\n{expanded}"
    );
}

#[test]
fn a_vectorized_absolute_value_refuses() {
    let refusal = refuse(
        r"
function magnitudes
  input Real x[3];
  output Real y[3];
algorithm
  y := abs(x);
end magnitudes;

model Refused
  parameter Real x[3] = {1.5, 0.8, -0.4};
  Real J[3, 3] = jacobian(magnitudes(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::ExpressionForm);
    assert!(refusal.to_string().contains("abs"), "{refusal}");
}

#[test]
fn a_scalar_absolute_value_is_admitted() {
    let expanded = expand(
        r"
function magnitude
  input Real x;
  output Real y;
algorithm
  y := abs(x);
end magnitude;

model Probe
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(magnitude(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("if (x) >= 0 then"),
        "a rank-zero argument keeps the taken-branch convention:\n{expanded}"
    );
}

#[test]
fn an_array_valued_for_index_refuses() {
    let refusal = refuse(
        r"
function fsq
  input Real x[3];
  output Real y;
algorithm
  y := 0.0;
  for e in x loop
    y := y + e*e;
  end for;
end fsq;

model Refused
  parameter Real x[3] = {1.5, 0.8, -0.4};
  Real J[1, 3] = jacobian(fsq(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::StatementForm);
    assert!(refusal.to_string().contains("for"), "{refusal}");
}

#[test]
fn a_for_range_whose_bounds_move_refuses() {
    let refusal = refuse(
        r"
function ranged
  input Real x;
  output Real y;
algorithm
  y := 0.0;
  for t in 0.0:0.5:x loop
    y := y + t;
  end for;
end ranged;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(ranged(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::StatementForm);
    assert!(refusal.to_string().contains("range"), "{refusal}");
}

#[test]
fn an_integer_range_for_index_is_admitted() {
    let expanded = expand(
        r"
function accumulate
  input Real x[3];
  output Real y;
algorithm
  y := 0.0;
  for i in 1:3 loop
    y := y + x[i]*x[i];
  end for;
end accumulate;

model Probe
  parameter Real x[3] = {1.5, 0.8, -0.4};
  Real J[1, 3] = jacobian(accumulate(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("for i in 1:3 loop"),
        "an integer range keeps its loop:\n{expanded}"
    );
    assert!(
        expanded.contains("y_ad := (y_ad) + ((x_ad[i]) * (x[i]) + (x[i]) * (x_ad[i]));"),
        "the loop body must carry the accumulated tangent:\n{expanded}"
    );
}

#[test]
fn a_declared_function_shadowing_a_builtin_refuses() {
    let refusal = refuse(
        r"
function tanh
  input Real a;
  output Real b;
algorithm
  b := a*a*a;
end tanh;

function shaped
  input Real x;
  output Real y;
algorithm
  y := tanh(x) + x;
end shaped;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(shaped(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::CalleeLookup);
    assert!(refusal.to_string().contains("tanh"), "{refusal}");
}

#[test]
fn an_atan2_with_a_scalar_second_operand_keeps_the_array_shape() {
    let expanded = expand(
        r"
function bearing
  input Real x[3];
  output Real y[3];
algorithm
  y := atan2(x, 2.0);
end bearing;

model Probe
  parameter Real x[3] = {0.37, -0.51, 0.83};
  Real J[3, 3] = jacobian(bearing(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("((x) .* (x) .+ (2.0) .* (2.0))"),
        "the atan2 denominator sums two terms whose ranks the call does not have to match, so \
         the sum has to be elementwise:\n{expanded}"
    );
}

#[test]
fn an_atan2_with_a_scalar_first_operand_keeps_the_array_shape() {
    let expanded = expand(
        r"
function bearing
  input Real x[3];
  output Real y[3];
algorithm
  y := atan2(2.0, x);
end bearing;

model Probe
  parameter Real x[3] = {0.37, -0.51, 0.83};
  Real J[3, 3] = jacobian(bearing(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("((2.0) .* (2.0) .+ (x) .* (x))"),
        "the mirror case has the same shape obligation:\n{expanded}"
    );
}
