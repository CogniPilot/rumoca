//! Regression: a function *output* (or local) array whose name shadows a
//! caller variable of the same name must start fresh when the function is
//! inlined during solve lowering — it must not inherit the caller's bindings.
//!
//! A library `product` whose output is `X[10]`, called from a model whose
//! state is also `X[13]`, must read its own `X[1..10]`. Before the fix, the
//! inlined output `X` inherited the caller's stale `X[11..13]` bindings, so a
//! read of the function result resolved to the caller's `X` at a `+10` offset
//! (`X[1]` -> `X[11]`). This silently corrupted the SE_2(3) quadrotor control
//! (13-state square trajectory diverged) while same-size collisions (the
//! 10-state hover) happened to coincide and stayed correct.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const OUTPUT_SHADOW_MODEL: &str = r#"
within;
function helper
  input Real q[4];
  output Real r[3];
algorithm
  r := {q[2], q[3], q[4]};
end helper;

function prod
  input Real A[10];
  output Real X[10];
protected
  Real pr[3];
algorithm
  pr := helper(A[7:10]);
  X[1] := pr[1];
  X[2] := pr[2];
  X[3] := pr[3];
  for j in 4:10 loop
    X[j] := A[j];
  end for;
end prod;

function wrapper
  input Real X[13];
  output Real y[10];
algorithm
  y := prod(X[1:10]);
end wrapper;

model OutputShadow
  Real s[10](each start = 0, each fixed = true);
  parameter Real X[13] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 999, 0, 0};
equation
  der(s) = wrapper(X) - s;
end OutputShadow;
"#;

fn der_value(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .derivatives
        .iter()
        .find(|slot| slot.name == name)
        .unwrap_or_else(|| {
            panic!(
                "missing derivative {name}; have: {:?}",
                report
                    .derivatives
                    .iter()
                    .map(|s| s.name.clone())
                    .collect::<Vec<_>>()
            )
        })
        .value
}

#[test]
fn inlined_function_output_shadow_does_not_inherit_caller_bindings() {
    let compiled = Compiler::new()
        .model("OutputShadow")
        .compile_str(OUTPUT_SHADOW_MODEL, "OutputShadow.mo")
        .expect("compile to DAE should succeed");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("output-shadow model should lower and evaluate");
    let report = &probe.report;
    assert!(report.error.is_none(), "eval error: {:?}", report.error);

    // At s = 0, der(s) = wrapper(X) = prod(X[1:10]).
    // prod's output X[1..3] = helper(A[7:10]) = {A[8], A[9], A[10]} = {8, 9, 10};
    // X[4..10] = A[4..10] = {4, 5, 6, 7, 8, 9, 10}.
    // The leak bug made der(s[1]) read the caller's X[11] = 999 instead of 8.
    let expected = [8.0, 9.0, 10.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
    for (i, want) in expected.iter().enumerate() {
        assert_eq!(
            der_value(report, &format!("der(s[{}])", i + 1)),
            *want,
            "inlined output X[{}] inherited a caller binding (X[11]=999 leak)",
            i + 1
        );
    }
}
