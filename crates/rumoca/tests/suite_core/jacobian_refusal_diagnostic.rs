//! A refused expansion has to reach the person who wrote the model.
//!
//! The engine's own tests pin which constructs refuse; this one pins that a
//! refusal survives the trip through the compiler as a diagnostic naming the
//! rule and the line, instead of a downstream error about a function that was
//! never generated.

use rumoca::Compiler;

const DATA_DEPENDENT_LOOP: &str = r"
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
  Real clock(start = 0, fixed = true);
equation
  der(clock) = 0;
end Refused;
";

#[test]
fn a_refused_expansion_reports_its_rule_and_line() {
    let error = Compiler::new()
        .model("Refused")
        .compile_str(DATA_DEPENDENT_LOOP, "Refused.mo")
        .expect_err("a data-dependent while loop must refuse");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("JAC-R4"),
        "the refusal must name its rule: {rendered}"
    );
    assert!(
        rendered.contains("while loop"),
        "the refusal must say what it refused: {rendered}"
    );
    assert!(
        rendered.contains("Refused.mo:10"),
        "the refusal must point at the refused statement: {rendered}"
    );
}

#[test]
fn a_model_without_the_surface_is_unaffected() {
    let source = r"
model Plain
  Real x(start = 1, fixed = true);
equation
  der(x) = -x;
end Plain;
";
    Compiler::new()
        .model("Plain")
        .compile_str(source, "Plain.mo")
        .expect("a model that never writes the surface compiles unchanged");
}
