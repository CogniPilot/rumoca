//! Regression: scalar `der(p[i]) = v[i]` rows of an array ODE must each match
//! 1:1 with their own `der` unknown, not collapse into a coupled SCC.
//!
//! Before the fix, the structural incidence collected the *un-subscripted* base
//! name of a `der(...)` operand (`der(p[1])` -> `p`). The array fallback in the
//! unknown resolver then expanded `p` to every `der(p[i])`, so each of the three
//! trivially explicit `der(p[i]) = v[i]` rows was marked incident to all three
//! `der(p)` unknowns. Matching grouped them into a spurious 3x3 coupled SCC,
//! and tearing turned that block into a structurally singular linear system
//! (`SymbolicSingular`) — even though the equations are a plain explicit ODE.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at, structural_report_for_dae};

// A 9-state explicit ODE whose `der` depends on a matrix-valued function output
// fed through a matrix product (`der(s) = M*s`, `M = mat9(s)`). This is the
// minimal shape of the kinematic-quadrotor chain that exposed the bug.
const ARRAY_DER_MODEL: &str = r#"
within;
function helper
  input Real a; input Real b; output Real c;
algorithm
  c := a * b + sin(a) - cos(b);
end helper;
function mat9
  input Real x[9]; output Real M[9,9];
algorithm
  for i in 1:9 loop
    for j in 1:9 loop
      M[i,j] := helper(x[i], x[j]) + (if i == j then 1.0 else 0.0);
    end for;
  end for;
end mat9;
model ArrayDer
  Real s[9](each start = 0.1, each fixed = true);
  Real M[9,9];
  Real y[9];
equation
  M = mat9(s);
  y = M * s;
  der(s) = y;
end ArrayDer;
"#;

#[test]
fn array_der_rows_do_not_form_spurious_coupled_scc() {
    let compiled = Compiler::new()
        .model("ArrayDer")
        .compile_str(ARRAY_DER_MODEL, "ArrayDer.mo")
        .expect("compile to DAE should succeed");

    let report = structural_report_for_dae(&compiled.dae, &SimOptions::default())
        .expect("structural analysis should succeed");

    // The explicit ODE has no algebraic loop: every block must be scalar.
    assert_eq!(
        report.coupled_block_count(),
        0,
        "der(s[i]) = y[i] must not be grouped into a coupled SCC; largest coupled block = {}",
        report.largest_coupled_block()
    );

    // And it must actually evaluate to a finite, correct derivative.
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("explicit ODE should lower and evaluate");
    assert!(probe.report.error.is_none(), "eval error: {:?}", probe.report.error);
    assert!(!probe.report.has_nonfinite(), "derivatives must be finite");
}
