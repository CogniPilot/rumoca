//! Regression: a `for`-equation whose body is `der(x[i]) = u` must keep one
//! equation per element.
//!
//! `eliminate_derivative_aliases` recognizes rows of the shape
//! `alias == der(state)` and rewrites `alias` into `der(state)` everywhere,
//! deleting the alias row. It used to accept the `der` argument by *base name*,
//! so with the array state `x` every element row `der(x[i]) - u` looked like the
//! claim `u == der(x)`. For `for i in 1:3 loop der(x[i]) = u` two element rows
//! then both "aliased" `u` and both were deleted, leaving a 3-element state with
//! one derivative row and stale compact-family row metadata: the compiler either
//! aborted with an internal contract violation ("compact family row range 1..4
//! exceeds 2 continuous equations") or reported the balanced model as
//! over-determined.
//!
//! MLS §10.5 makes `x[1]` one element of `x`, so the legal for-equation of MLS
//! §8.3.2 below constrains exactly one element per row; `u` aliases `der(x[1])`
//! at most, never the whole derivative array.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at, structural_report_for_dae};

/// Three states driven by one shared algebraic. Three elements is the smallest
/// size that leaves an interior cell once the corner cells are materialized, so
/// it is also the smallest model that reaches the compact-family bookkeeping.
const SHARED_ALGEBRAIC_FOR_LOOP: &str = r#"
model ForLoopSharedAlgebraic
  Real x[3](each start = 0.0, each fixed = true);
  Real u;
equation
  u = 3.0 + 2.0*x[1];
  for i in 1:3 loop
    der(x[i]) = u;
  end for;
end ForLoopSharedAlgebraic;
"#;

#[test]
fn for_loop_element_derivative_rows_are_not_whole_array_aliases() {
    let compiled = Compiler::new()
        .model("ForLoopSharedAlgebraic")
        .compile_str(SHARED_ALGEBRAIC_FOR_LOOP, "ForLoopSharedAlgebraic.mo")
        .expect("compile to DAE should succeed");

    let report = structural_report_for_dae(&compiled.dae, &SimOptions::default())
        .expect("structural analysis should succeed");

    // 3 element rows + the algebraic definition, against der(x[1..3]) and u.
    assert_eq!(
        (report.n_equations, report.n_unknowns),
        (4, 4),
        "balanced model must stay balanced: {} equations vs {} unknowns",
        report.n_equations,
        report.n_unknowns
    );

    let probe = eval_dae_at(
        &compiled.dae,
        &SimOptions::default(),
        &[("x[1]".to_string(), 1.0)],
        0.0,
    )
    .expect("shared-algebraic for-equation should lower and evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );

    // u = 3 + 2*x[1] = 5, and every element takes that same derivative.
    let derivatives: Vec<(String, f64)> = probe
        .report
        .derivatives
        .iter()
        .map(|slot| (slot.name.clone(), slot.value))
        .collect();
    assert_eq!(
        derivatives.len(),
        3,
        "every element keeps its own derivative row: {derivatives:?}"
    );
    for (name, value) in &derivatives {
        assert!(
            (value - 5.0).abs() < 1e-12,
            "{name} = {value}, expected u = 3 + 2*x[1] = 5 (all: {derivatives:?})"
        );
    }
}
