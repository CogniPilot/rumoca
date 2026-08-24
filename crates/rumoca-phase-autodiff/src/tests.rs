//! Unit checks of the surface, the engine, and the refusal boundary.

mod admission;
mod bindings;
mod calls;
mod refusals;
mod shapes;

use crate::{Refusal, Rule, expand_source, may_expand};

const SCALED: &str = r"
function fscale
  input Real x[3];
  input Real k;
  output Real y[2];
protected
  Real t;
algorithm
  t := k*x[1];
  y[1] := t*x[2] + sin(x[3]);
  y[2] := exp(x[1])*x[2];
end fscale;

model Probe
  parameter Real x[3] = {0.3, -1.2, 0.7};
  parameter Real k = 2.5;
  Real J[2, 3] = jacobian(fscale(x, k), x);
end Probe;
";

fn refuse(source: &str) -> Refusal {
    match expand_source(source, "Refused.mo") {
        Err(crate::ExpansionError::Refused(refusal)) => refusal,
        Err(other) => panic!("expected a refusal, got {other}"),
        Ok(text) => panic!("expected a refusal, expansion produced:\n{text}"),
    }
}

fn expand(source: &str) -> String {
    expand_source(source, "Probe.mo").expect("expansion runs")
}

#[test]
fn a_source_without_the_surface_is_returned_unchanged() {
    let source = "model M\n  Real x;\nequation\n  der(x) = -x;\nend M;\n";
    assert!(!may_expand(source));
    assert_eq!(
        expand_source(source, "M.mo").expect("expansion runs"),
        source
    );
}

#[test]
fn expansion_mints_the_wrapper_and_rewrites_the_call() {
    let expanded = expand_source(SCALED, "Probe.mo").expect("expansion runs");
    assert!(
        expanded.contains("function fscale_jacobian_x"),
        "missing wrapper:\n{expanded}"
    );
    assert!(
        expanded.contains("function fscale_ad_tangent"),
        "missing tangent:\n{expanded}"
    );
    assert!(
        expanded.contains("Real J[2, 3] = fscale_jacobian_x(x, k);"),
        "call not rewritten:\n{expanded}"
    );
    // The only `jacobian(` left is inside the provenance description string
    // the generated functions carry back to the minting site.
    assert!(
        !expanded.contains("= jacobian("),
        "surface call survived:\n{expanded}"
    );
}

#[test]
fn the_expansion_carries_its_minting_site() {
    let expanded = expand_source(SCALED, "Probe.mo").expect("expansion runs");
    assert!(
        expanded.contains("\"jacobian(fscale(x, k), x) at Probe.mo:"),
        "wrapper lost its provenance:\n{expanded}"
    );
}

#[test]
fn a_name_that_merely_ends_in_the_surface_is_not_a_call_site() {
    for source in [
        "  J := LieGroups.SO3.Quat.right_jacobian(tangent);",
        "  J := Lib.jacobian(a, b);",
        "  // jacobian of the measurement model",
        "  Real jacobianRow[3];",
    ] {
        assert!(
            !may_expand(source),
            "the scan must not wake the expander for: {source}"
        );
    }
    assert!(may_expand("  Real J[1, 1] = jacobian (f(x), x);"));
}

#[test]
fn a_declared_jacobian_wins_over_the_construct() {
    let source = r"
function jacobian
  input Real a[2, 2];
  input Real b[2];
  output Real y[2];
algorithm
  y := a*b;
end jacobian;

model Uses
  parameter Real a[2, 2] = {{1.0, 0.0}, {0.0, 1.0}};
  parameter Real b[2] = {2.0, 3.0};
  Real y[2] = jacobian(a, b);
end Uses;
";
    assert_eq!(
        expand_source(source, "Uses.mo").expect("a declared name is left alone"),
        source
    );
}

#[test]
fn the_generated_text_reparses() {
    let expanded = expand_source(SCALED, "Probe.mo").expect("expansion runs");
    rumoca_phase_parse::parse_to_ast(&expanded, "Expanded.mo").expect("expansion reparses");
}

#[test]
fn a_product_takes_the_product_rule() {
    let expanded = expand_source(SCALED, "Probe.mo").expect("expansion runs");
    assert!(
        expanded.contains("(k_ad) * (x[1]) + (k) * (x_ad[1])"),
        "product rule not emitted:\n{expanded}"
    );
}

#[test]
fn a_while_loop_refuses() {
    let refusal = refuse(
        r"
function walk
  input Real x;
  output Real y;
protected
  Real step;
algorithm
  y := x;
  step := 0;
  while step < 3 loop
    y := y*x;
    step := step + 1;
  end while;
end walk;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(walk(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::StatementForm);
    assert!(refusal.to_string().contains("JAC-R4"), "{refusal}");
}

#[test]
fn a_non_differentiable_builtin_refuses() {
    let refusal = refuse(
        r"
function stepper
  input Real x;
  output Real y;
algorithm
  y := sign(x)*x;
end stepper;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(stepper(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::ExpressionForm);
    assert!(refusal.to_string().contains("sign"), "{refusal}");
}

#[test]
fn an_external_function_refuses() {
    let refusal = refuse(
        r#"
function outside
  input Real x;
  output Real y;
  external "C" y = outside(x);
end outside;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(outside(x), x);
end Refused;
"#,
    );
    assert_eq!(refusal.rule, Rule::CalleeTangent);
    assert!(refusal.to_string().contains("external"), "{refusal}");
}

#[test]
fn an_unknown_function_refuses() {
    let refusal = refuse(
        r"
model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(missing(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::CalleeLookup);
}

#[test]
fn an_argument_that_is_not_a_reference_refuses() {
    let refusal = refuse(
        r"
function twice
  input Real x;
  output Real y;
algorithm
  y := 2*x;
end twice;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(twice(2*x), 2*x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::CallForm);
}

#[test]
fn a_repeated_argument_refuses() {
    let refusal = refuse(
        r"
function pair
  input Real a;
  input Real b;
  output Real y;
algorithm
  y := a*b;
end pair;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(pair(x, x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::CallForm);
}

#[test]
fn a_symbolic_input_dimension_refuses() {
    let refusal = refuse(
        r"
function total
  input Integer n;
  input Real x[n];
  output Real y;
algorithm
  y := sum(x);
end total;

model Refused
  parameter Integer n = 3;
  parameter Real x[3] = {1.0, 2.0, 3.0};
  Real J[1, 3] = jacobian(total(n, x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::Signature);
}

#[test]
fn a_record_typed_port_refuses() {
    let refusal = refuse(
        r"
record Pose
  Real p[3];
end Pose;

function height
  input Pose pose;
  output Real y;
algorithm
  y := pose.p[3];
end height;

model Refused
  Pose pose;
  Real J[1, 1] = jacobian(height(pose), pose);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::DifferentiableType);
}
