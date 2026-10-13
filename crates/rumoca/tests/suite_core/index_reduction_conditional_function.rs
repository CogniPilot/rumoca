//! A constraint written through a function whose body is one `if` expression
//! is differentiated by substituting that body, so the structural stage
//! (index reduction) accepts the model.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const SOURCE: &str = r#"
model ConditionalConstraintPendulum
  function G
    input Integer a;
    input Real x;
    input Real y;
    output Real g;
  algorithm
    g := if a == 1 then x*x + y*y - 1 else 0;
  end G;
  Real x(start = 1, fixed = true);
  Real y(start = 0, fixed = true);
  Real vx(start = 0, fixed = true);
  Real vy(start = 0, fixed = true);
  Real lam;
equation
  der(x) = vx;
  der(y) = vy;
  der(vx) = lam * x;
  der(vy) = lam * y - 9.81;
  0 = G(1, x, y);
end ConditionalConstraintPendulum;
"#;

#[test]
fn constraint_through_an_if_expression_function_passes_the_structural_stage() {
    let compiled = Compiler::new()
        .model("ConditionalConstraintPendulum")
        .compile_str(SOURCE, "conditional_constraint.mo")
        .unwrap();
    eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("structural stage rejected the model: {error}"));
}
