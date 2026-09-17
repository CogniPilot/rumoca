//! MLS §3.6.5 / §11.5 folding of a proven-constant conditional at model scope.
//!
//! `Modelica.Mechanics.MultiBody.Examples.Elementary.RollingWheelSetPulling`
//! and the reduced `DeadArmReduction` share one shape: a model-scope conditional
//! whose guard is a compile-time constant selects an arm that calls a function
//! the other arm's specialization does not need. The MultiBody case is the
//! `CombiTimeTable` binding `isCsvExt = if tableOnFile then Strings.findLast(...)
//! else false` with `tableOnFile` proven false; the reduction is a `while`-loop
//! search this compiler does not reduce, called only under a proven-false guard.
//!
//! MLS §11.5 evaluates the branch conditions in order and yields the value of
//! the first `true` one, or the else value when none is. Function-shape
//! discovery mints no certificate for a proven-dead arm's call, so canonical DAE
//! construction must fold the same arm away rather than build a call with no
//! certificate. An unproven guard keeps both arms, so a runtime branch that
//! calls an unreducible function still fails, exactly as it does at top level.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

/// The reduced shape: an unreducible `while`-loop search reached only through a
/// proven-false guard, its else arm the value the program actually takes.
const DEAD_ARM_MODEL: &str = r#"
within;
model DeadArmModelConditional
  function searchLike
    input Integer n;
    output Integer p;
  protected
    Integer i;
  algorithm
    p := 0;
    i := n;
    while i >= 1 loop
      if i == 3 then p := i; end if;
      i := i - 1;
    end while;
  end searchLike;
  parameter Boolean useIt = false;
  Real w;
equation
  w = (if useIt then searchLike(10) else 7.0);
end DeadArmModelConditional;
"#;

fn algebraic(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| {
            panic!(
                "missing solver value {name}; have: {:?}",
                report
                    .solver_y
                    .iter()
                    .map(|slot| slot.name.clone())
                    .collect::<Vec<_>>()
            )
        })
        .value
}

/// A dead-arm call to an unreducible-loop function compiles and the taken (else)
/// branch is what the model evaluates. Before the fold this was ED020: discovery
/// pruned the call's certificate while construction still built the dead arm.
#[test]
fn a_dead_arm_call_compiles_and_evaluates_the_taken_branch() {
    let compiled = Compiler::new()
        .model("DeadArmModelConditional")
        .compile_str(DEAD_ARM_MODEL, "DeadArmModelConditional.mo")
        .expect("a proven-false guard folds away the unreducible call arm");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the folded DAE evaluates");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    assert!(
        (algebraic(&probe.report, "w") - 7.0).abs() < 1e-9,
        "the else arm is the taken value"
    );
}

/// The same call under a runtime guard is unproven, so both arms are built and
/// the unreducible loop is still rejected. This is the property the fold must
/// not break: only a proven-constant guard prunes, never a runtime one.
#[test]
fn an_unproven_model_conditional_still_builds_the_unreducible_arm() {
    let source = DEAD_ARM_MODEL
        .replace("if useIt", "if time > 0.5")
        .replace("DeadArmModelConditional", "UnprovenModelConditional");
    let error = Compiler::new()
        .model("UnprovenModelConditional")
        .compile_str(&source, "UnprovenModelConditional.mo")
        .expect_err("a runtime guard keeps both arms, so the unreducible loop still fails");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("loop reduction"),
        "the runtime arm must still reach the unreducible call: {rendered}"
    );
}
