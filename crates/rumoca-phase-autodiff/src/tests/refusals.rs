//! One test per entry of the stated refusal set.

use crate::Rule;
use crate::tests::{expand, refuse};

#[test]
fn a_recursive_function_refuses() {
    let refusal = refuse(
        r"
function descend
  input Real x;
  input Integer n;
  output Real y;
algorithm
  if n <= 0 then
    y := x;
  else
    y := x*descend(x, n - 1);
  end if;
end descend;

model Refused
  parameter Real x = 1.5;
  parameter Integer n = 3;
  Real J[1, 1] = jacobian(descend(x, n), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::CalleeTangent);
    assert!(refusal.to_string().contains("recursive"), "{refusal}");
}

#[test]
fn a_multiple_output_function_refuses() {
    let refusal = refuse(
        r"
function halves
  input Real x;
  output Real a;
  output Real b;
algorithm
  a := x*x;
  b := 2.0*x;
end halves;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(halves(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::Signature);
}

#[test]
fn a_tangent_name_collision_refuses() {
    let refusal = refuse(
        r"
function shadowed
  input Real x;
  input Real x_ad;
  output Real y;
algorithm
  y := x*x_ad;
end shadowed;

model Refused
  parameter Real x = 1.5;
  parameter Real x_ad = 0.5;
  Real J[1, 1] = jacobian(shadowed(x, x_ad), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::NameCollision);
}

#[test]
fn a_break_statement_refuses() {
    let refusal = refuse(
        r"
function capped
  input Real x;
  output Real y;
algorithm
  y := 0.0;
  for i in 1:4 loop
    if i > 2 then
      break;
    end if;
    y := y + x*i;
  end for;
end capped;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(capped(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::StatementForm);
    assert!(refusal.to_string().contains("break"), "{refusal}");
}

#[test]
fn a_return_statement_refuses() {
    let refusal = refuse(
        r"
function early
  input Real x;
  output Real y;
algorithm
  y := x*x;
  return;
end early;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(early(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::StatementForm);
}

#[test]
fn an_integer_formal_refuses() {
    let refusal = refuse(
        r"
function counted
  input Integer n;
  input Real x;
  output Real y;
algorithm
  y := n*x;
end counted;

model Refused
  parameter Integer n = 2;
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(counted(n, x), n);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::DifferentiableType);
}

#[test]
fn a_minimum_builtin_refuses() {
    let refusal = refuse(
        r"
function clamped
  input Real x;
  output Real y;
algorithm
  y := min(x, 1.0);
end clamped;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(clamped(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::ExpressionForm);
    assert!(refusal.to_string().contains("min"), "{refusal}");
}

#[test]
fn a_matrix_valued_differentiated_input_refuses() {
    let refusal = refuse(
        r"
function trace_of
  input Real m[2, 2];
  output Real y;
algorithm
  y := m[1, 1] + m[2, 2];
end trace_of;

model Refused
  parameter Real m[2, 2] = {{1.0, 0.0}, {0.0, 2.0}};
  Real J[1, 4] = jacobian(trace_of(m), m);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::Signature);
}

#[test]
fn a_call_statement_refuses() {
    let refusal = refuse(
        r"
function noisy
  input Real x;
  output Real y;
algorithm
  Modelica.Utilities.Streams.print(String(x));
  y := x*x;
end noisy;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(noisy(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::StatementForm);
}

#[test]
fn the_taken_branch_convention_differentiates_the_branch_that_runs() {
    let expanded = expand(
        r"
function branched
  input Real x;
  output Real y;
algorithm
  if x > 0.0 then
    y := 2.0*x;
  else
    y := -3.0*x;
  end if;
end branched;

model Probe
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(branched(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("if x > 0.0 then"),
        "the tangent must keep the condition it was taken under:\n{expanded}"
    );
    assert!(
        expanded.contains("y_ad := (2.0) * (x_ad);"),
        "the tangent must follow the taken branch:\n{expanded}"
    );
}
