//! Regression: a function input parameter whose name shadows a model
//! variable must resolve against the local (function-scope) binding when the
//! function is inlined during solve lowering — not against the global model
//! layout.
//!
//! Before the fix, the subscripted array-like lowering path consulted the
//! global layout shape for the shadowed name. A `Real[4]` function input `p`
//! inlined inside a `der()` RHS was mis-resolved against a `Real[3]` model
//! state `p`, producing a phantom `p[4]` and failing solve lowering with
//! `missing variable binding p[4]`.

use rumoca::Compiler;
use rumoca_phase_solve::lower_dae_to_solve_model;

const SHADOW_MODEL: &str = r#"
within;
function pfn
  input Real p[4];
  output Real r[4];
algorithm
  r[1] := p[1];
  r[2] := p[2];
  r[3] := p[3];
  r[4] := p[4];
end pfn;

model Shadow
  Real p[3](start = {1, -1, -0.5}, each fixed = true);
  Real v[3](start = {0, 0, 0}, each fixed = true);
  Real w[4];
equation
  w = pfn({v[1], v[2], v[3], v[1]});
  der(p) = v;
  der(v) = {0, 0, w[4]};
end Shadow;
"#;

const SHADOW_MISSING_LOCAL_MODEL: &str = r#"
within;
function pfn_missing
  input Real u[3];
  output Real r[4];
protected
  Real p[4];
algorithm
  p[1] := u[1];
  p[2] := u[2];
  p[3] := u[3];
  r := p[:];
end pfn_missing;

model ShadowMissingLocal
  parameter Real p[4] = {10, 20, 30, 40};
  Real y[4];
equation
  y = pfn_missing({1, 2, 3});
end ShadowMissingLocal;
"#;

const SHADOW_UNASSIGNED_LOCAL_MODEL: &str = r#"
within;
function pfn_unassigned
  output Real r;
protected
  Real p[1];
algorithm
  r := p[1];
end pfn_unassigned;

model ShadowUnassignedLocal
  parameter Real p[1] = {42};
  Real y;
equation
  y = pfn_unassigned();
end ShadowUnassignedLocal;
"#;

#[test]
fn function_input_shadowing_model_state_lowers_to_solve() {
    let compiled = Compiler::new()
        .model("Shadow")
        .compile_str(SHADOW_MODEL, "Shadow.mo")
        .expect("compile to DAE should succeed");

    // The function input `p[4]` must resolve to the inlined argument
    // (`v[1]`), not the model state `p` (which is only `Real[3]`).
    lower_dae_to_solve_model(&compiled.dae)
        .expect("solve lowering must resolve the shadowed function input, not the model state");
}

#[test]
fn incomplete_local_array_shadow_reports_error_instead_of_global_fallback() {
    let compiled = Compiler::new()
        .model("ShadowMissingLocal")
        .compile_str(SHADOW_MISSING_LOCAL_MODEL, "ShadowMissingLocal.mo")
        .expect("compile to DAE should succeed");

    let err = lower_dae_to_solve_model(&compiled.dae)
        .expect_err("solve lowering must not fall back to the shadowed model state");
    let err_text = err.to_string();
    assert!(
        err_text.contains("p[4]"),
        "error should identify the missing local component, got: {err_text}"
    );
}

#[test]
fn declared_unassigned_local_array_shadow_reports_error_instead_of_global_fallback() {
    let compiled = Compiler::new()
        .model("ShadowUnassignedLocal")
        .compile_str(SHADOW_UNASSIGNED_LOCAL_MODEL, "ShadowUnassignedLocal.mo")
        .expect("compile to DAE should succeed");

    let err = lower_dae_to_solve_model(&compiled.dae)
        .expect_err("solve lowering must not fall back to the shadowed model parameter");
    let err_text = err.to_string();
    assert!(
        err_text.contains("p[1]"),
        "error should identify the unassigned local component, got: {err_text}"
    );
}
