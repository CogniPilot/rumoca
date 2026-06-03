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
