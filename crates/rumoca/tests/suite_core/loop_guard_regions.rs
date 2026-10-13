//! MLS 3.7 section 11.2.3: a `while` loop runs its body only while its
//! condition holds. The loop lowers to a fold whose early-exit predicate is
//! that condition, so a pass that runs already knows the condition is true and
//! its body is lowered without a region guarded by the same condition.

use rumoca::Compiler;
use rumoca_ir_solve::{SolveOperation, TypedProgram};
use rumoca_sim::{SimOptions, eval_dae_at};

/// Conditional regions that run inside the passes of a fold of `program`.
fn conditionals_in_fold_passes(program: &TypedProgram, in_fold: bool) -> usize {
    program
        .operations()
        .iter()
        .map(|spanned| match spanned.operation() {
            SolveOperation::Fold { transition, .. } => {
                conditionals_in_fold_passes(transition.body(), true)
            }
            SolveOperation::Map { body, .. } => conditionals_in_fold_passes(body.body(), in_fold),
            SolveOperation::AssignedConditional { program } => {
                usize::from(in_fold)
                    + conditionals_in_fold_passes(program.if_true().body(), in_fold)
                    + conditionals_in_fold_passes(program.if_false().body(), in_fold)
            }
            SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                usize::from(in_fold)
                    + conditionals_in_fold_passes(if_true.body(), in_fold)
                    + conditionals_in_fold_passes(if_false.body(), in_fold)
            }
            _ => 0,
        })
        .sum()
}

fn probe(source: &str) -> (usize, f64) {
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(source, "Probe.mo")
        .unwrap_or_else(|error| panic!("the loop compiles: {error}"));
    let lowered = rumoca_phase_solve::lower_solve_model(
        &compiled.dae,
        &std::collections::HashMap::new(),
        |_| {},
    )
    .expect("the loop lowers through the checked phase owner");
    let regions = lowered
        .model()
        .pure_calls
        .owners()
        .iter()
        .map(|owner| conditionals_in_fold_passes(owner.body(), false))
        .sum::<usize>();
    drop(lowered);
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("the loop evaluates: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let value = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "r")
        .expect("the loop result is a solver output")
        .value;
    (regions, value)
}

#[test]
fn a_while_condition_the_exit_test_names_guards_no_region_in_the_pass() {
    let source = r#"
function Accumulate
  input Real a[8];
  input Real limit;
  output Real y;
protected
  Integer k;
  Real total;
algorithm
  total := 0.0;
  k := 1;
  while k <= 8 and total < limit loop
    total := total + a[k];
    k := k + 1;
  end while;
  y := total;
end Accumulate;
model Probe
  input Real limit = 10.0;
  output Real r;
equation
  r = Accumulate({1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0}, limit);
end Probe;
"#;
    let (regions, value) = probe(source);
    assert_eq!(
        regions, 0,
        "the pass body needs no region guarded by the loop condition"
    );
    // 1 + 2 + 3 + 4 reaches the limit, so the fifth element is never added.
    assert_eq!(value, 10.0);
}

#[test]
fn a_guard_the_exit_test_does_not_name_keeps_its_region() {
    let source = r#"
function Accumulate
  input Real a[8];
  input Real limit;
  output Real y;
protected
  Integer k;
  Real total;
algorithm
  total := 0.0;
  k := 1;
  while k <= 8 and total < limit loop
    if a[k] > 2.5 then
      total := total + a[k];
    end if;
    k := k + 1;
  end while;
  y := total;
end Accumulate;
model Probe
  input Real limit = 10.0;
  output Real r;
equation
  r = Accumulate({1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0}, limit);
end Probe;
"#;
    let (regions, value) = probe(source);
    assert!(regions > 0, "the inner test stays a region");
    // 3 + 4 + 5 = 12 reaches the limit at k = 5.
    assert_eq!(value, 12.0);
}
