//! A declaration binding is an assignment, and it carries a tangent.
//!
//! `Real t = 2.0*u[1];` in a function body is an assignment Modelica performs
//! before the algorithm runs. A tangent function that redeclares `t` without
//! its binding computes a different primal, and one that mints `t_ad` without
//! assigning it reads whatever memory holds: both produce a Jacobian that is
//! wrong while every tool involved reports success. These checks pin the
//! binding onto the primal declaration, pin the statement that carries its
//! tangent, and pin the refusals where no such statement can be stated.

use crate::Rule;
use crate::tests::{expand, refuse};

#[test]
fn a_bound_local_keeps_its_binding_and_carries_its_tangent() {
    let expanded = expand(
        r"
function bound
  input Real u[3];
  output Real y[3];
protected
  Real t = 2.0*u[1];
algorithm
  y[1] := t*u[2];
  y[2] := t*u[3];
  y[3] := t;
end bound;

model Probe
  parameter Real x[3] = {0.37, -0.51, 0.83};
  Real J[3, 3] = jacobian(bound(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("Real t = 2.0 * u[1];"),
        "the tangent function must redeclare the local with its binding:\n{expanded}"
    );
    assert!(
        expanded.contains("t_ad := (2.0) * (u_ad[1]);"),
        "the binding's own tangent must be stated before the body reads it:\n{expanded}"
    );
}

#[test]
fn a_bound_output_defines_its_companion_before_the_body_reads_it() {
    let expanded = expand(
        r"
function seeded
  input Real u[3];
  output Real y[3] = {5.0, 1.0, 1.0};
algorithm
  y[1] := y[1]*u[1];
  y[2] := y[2]*u[2];
  y[3] := y[3]*u[3];
end seeded;

model Probe
  parameter Real x[3] = {0.37, -0.51, 0.83};
  Real J[3, 3] = jacobian(seeded(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("Real y[3] = {5.0, 1.0, 1.0};"),
        "the tangent function must redeclare the output with its binding:\n{expanded}"
    );
    assert!(
        expanded.contains("y_ad := 0.0*({5.0, 1.0, 1.0});"),
        "a binding with no tangent of its own still has to define the companion, or the \
         generated body reads whatever memory holds:\n{expanded}"
    );
}

#[test]
fn a_constant_local_keeps_its_variability_and_mints_no_companion() {
    let expanded = expand(
        r"
function guarded
  input Real u[3];
  output Real y[3];
protected
  constant Real eps = 4.0;
algorithm
  y[1] := eps*u[1];
  y[2] := u[2]*u[2];
  y[3] := u[3];
end guarded;

model Probe
  parameter Real x[3] = {0.37, -0.51, 0.83};
  Real J[3, 3] = jacobian(guarded(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("constant Real eps = 4.0;"),
        "a constant keeps its variability and its binding, or the differentiated body reads a \
         different constant than the primal did:\n{expanded}"
    );
    assert!(
        !expanded.contains("eps_ad"),
        "a constant cannot move, so no companion is minted for it:\n{expanded}"
    );
}

#[test]
fn a_parameter_local_is_constant_under_differentiation() {
    let expanded = expand(
        r"
function scaled
  input Real u[3];
  output Real y[3];
protected
  parameter Real gain = 2.5;
algorithm
  y[1] := gain*u[1];
  y[2] := gain*u[2];
  y[3] := gain*u[3];
end scaled;

model Probe
  parameter Real x[3] = {0.37, -0.51, 0.83};
  Real J[3, 3] = jacobian(scaled(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("parameter Real gain = 2.5;"),
        "a parameter keeps its variability and its binding:\n{expanded}"
    );
    assert!(
        !expanded.contains("gain_ad"),
        "a parameter cannot move, so no companion is minted for it:\n{expanded}"
    );
}

#[test]
fn a_binding_reading_a_later_declaration_refuses() {
    let refusal = refuse(
        r"
function forward
  input Real u[3];
  output Real y[3];
protected
  Real early = late*u[1];
  Real late = 2.0*u[2];
algorithm
  y[1] := early;
  y[2] := late;
  y[3] := u[3];
end forward;

model Refused
  parameter Real x[3] = {0.37, -0.51, 0.83};
  Real J[3, 3] = jacobian(forward(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::DeclarationBinding);
    assert!(refusal.to_string().contains("JAC-R9"), "{refusal}");
    assert!(refusal.to_string().contains("late"), "{refusal}");
}

#[test]
fn a_constant_binding_that_moves_refuses() {
    let refusal = refuse(
        r"
function leaking
  input Real u[3];
  output Real y[3];
protected
  constant Real held = u[1];
algorithm
  y[1] := held*u[1];
  y[2] := u[2];
  y[3] := u[3];
end leaking;

model Refused
  parameter Real x[3] = {0.37, -0.51, 0.83};
  Real J[3, 3] = jacobian(leaking(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::DeclarationBinding);
    assert!(refusal.to_string().contains("JAC-R9"), "{refusal}");
}

#[test]
fn a_bound_local_whose_binding_reads_an_earlier_local_is_admitted() {
    let expanded = expand(
        r"
function chained
  input Real u[3];
  output Real y[3];
protected
  Real first = 2.0*u[1];
  Real second = first*u[2];
algorithm
  y[1] := second;
  y[2] := first;
  y[3] := u[3];
end chained;

model Probe
  parameter Real x[3] = {0.37, -0.51, 0.83};
  Real J[3, 3] = jacobian(chained(x), x);
end Probe;
",
    );
    assert!(
        expanded.contains("first_ad := (2.0) * (u_ad[1]);"),
        "the earlier binding's tangent comes first:\n{expanded}"
    );
    assert!(
        expanded.contains("second_ad := (first_ad) * (u[2]) + (first) * (u_ad[2]);"),
        "a binding may read what is declared before it:\n{expanded}"
    );
}
