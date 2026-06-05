//! Regression: an `if`-statement with no `else` branch inside a function body
//! must keep its guard when the function is inlined during solve lowering.
//!
//! Before the fix, branch bodies mutated builder-level caches
//! (`local_indexed_bindings` / `local_const_bindings`) in place while lowering
//! a conditional. Those caches are consulted before the merged register
//! `scope`, so a value written only inside the `then` block of a no-`else`
//! `if` leaked out unconditionally: `x := q; if x[1] < 0 then x := -x; end if;`
//! negated `x` regardless of the condition. The merged `select` register was
//! built correctly but shadowed by the stale branch binding, so `der()` came
//! out negated (silent wrong answer, no error).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

// `qp[1] = +0.9 > 0`, so none of the four equivalent "negate iff first < 0"
// forms should change the value. der(a)/der(b)/der(c)/der(d) must all equal the
// input. der(a) (no-else array) and der(b) (no-else scalar) are the forms that
// regressed.
const NOELSE_MODEL: &str = r#"
within;
function f_noelse_arr
  input Real q[4]; output Real o[4]; protected Real x[4];
algorithm x := q; if x[1] < 0 then x := -x; end if; o := x;
end f_noelse_arr;
function f_noelse_scalar
  input Real a; output Real o; protected Real x;
algorithm x := a; if x < 0 then x := -x; end if; o := x;
end f_noelse_scalar;
function f_withelse_arr
  input Real q[4]; output Real o[4]; protected Real x[4];
algorithm if q[1] < 0 then x := -q; else x := q; end if; o := x;
end f_withelse_arr;
function f_ifexpr_arr
  input Real q[4]; output Real o[4];
algorithm o := if q[1] < 0 then -q else q;
end f_ifexpr_arr;
model NoElseIf
  parameter Real qp[4] = {0.9, 0.1, -0.2, 0.3};
  Real a[4](each start = 0, each fixed = true);
  Real b(start = 0, fixed = true);
  Real c[4](each start = 0, each fixed = true);
  Real d[4](each start = 0, each fixed = true);
equation
  der(a) = f_noelse_arr(qp);
  der(b) = f_noelse_scalar(0.7);
  der(c) = f_withelse_arr(qp);
  der(d) = f_ifexpr_arr(qp);
end NoElseIf;
"#;

// First element negative: every form must now negate.
const NOELSE_MODEL_NEG: &str = r#"
within;
function f_noelse_arr
  input Real q[4]; output Real o[4]; protected Real x[4];
algorithm x := q; if x[1] < 0 then x := -x; end if; o := x;
end f_noelse_arr;
function f_noelse_scalar
  input Real a; output Real o; protected Real x;
algorithm x := a; if x < 0 then x := -x; end if; o := x;
end f_noelse_scalar;
model NoElseIfNeg
  parameter Real qp[4] = {-0.9, 0.1, -0.2, 0.3};
  Real a[4](each start = 0, each fixed = true);
  Real b(start = 0, fixed = true);
equation
  der(a) = f_noelse_arr(qp);
  der(b) = f_noelse_scalar(-0.7);
end NoElseIfNeg;
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
fn noelse_if_keeps_guard_when_condition_false() {
    let compiled = Compiler::new()
        .model("NoElseIf")
        .compile_str(NOELSE_MODEL, "NoElseIf.mo")
        .expect("compile to DAE should succeed");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("no-else-if model should lower and evaluate");
    let report = &probe.report;
    assert!(report.error.is_none(), "eval error: {:?}", report.error);

    // qp[1] = +0.9, so the guard is false and nothing should be negated.
    let qp = [0.9, 0.1, -0.2, 0.3];
    for (i, expected) in qp.iter().enumerate() {
        // no-else array form (the high-value regression)
        assert_eq!(
            der_value(report, &format!("der(a[{}])", i + 1)),
            *expected,
            "no-else array if ran unconditionally on der(a[{}])",
            i + 1
        );
        // with-else array form (was already correct)
        assert_eq!(der_value(report, &format!("der(c[{}])", i + 1)), *expected);
        // if-expression form (was already correct)
        assert_eq!(der_value(report, &format!("der(d[{}])", i + 1)), *expected);
    }
    // no-else scalar form
    assert_eq!(
        der_value(report, "der(b)"),
        0.7,
        "no-else scalar if ran unconditionally on der(b)"
    );
}

#[test]
fn noelse_if_branch_still_fires_when_condition_true() {
    let compiled = Compiler::new()
        .model("NoElseIfNeg")
        .compile_str(NOELSE_MODEL_NEG, "NoElseIfNeg.mo")
        .expect("compile to DAE should succeed");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("no-else-if model should lower and evaluate");
    let report = &probe.report;
    assert!(report.error.is_none(), "eval error: {:?}", report.error);

    // qp[1] = -0.9 < 0, so the guard is true and the value must be negated.
    let neg_qp = [0.9, -0.1, 0.2, -0.3];
    for (i, expected) in neg_qp.iter().enumerate() {
        assert_eq!(
            der_value(report, &format!("der(a[{}])", i + 1)),
            *expected,
            "no-else array if should have negated der(a[{}])",
            i + 1
        );
    }
    // f_noelse_scalar(-0.7): -0.7 < 0 -> negate -> 0.7
    assert_eq!(der_value(report, "der(b)"), 0.7);
}
