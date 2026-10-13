//! MLS §11.2.2/§11.2.6 and SPEC_0043 §4: a reached scalar definition
//! survives a conditional update and is carried to the next iteration.

use super::{assert_runtime_call_retained, solver_value};
use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const SOURCE: &str = r#"
package Scan
  function Count
    input Real scores[3];
    input Real validity;
    input Boolean enabled;
    input Integer trials;
    output Real count;
  protected
    Real valid;
    Boolean running;
  algorithm
    count := 0;
    valid := validity;
    if valid > 0 and enabled then
      running := true;
      for candidate in 1:trials loop
        if scores[candidate] < 1.5 then
          running := false;
        end if;
        if running then
          count := count + 1;
        end if;
      end for;
    end if;
  end Count;
end Scan;
model GuardedIncomingBoolean
  input Real scores[3] = {3,2,1};
  input Real validity = 1;
  input Boolean enabled = true;
  input Integer trials = 3;
  Real y = Scan.Count(scores,validity,enabled,min(3,max(0,trials)));
end GuardedIncomingBoolean;
"#;

#[test]
fn a_guarded_incoming_boolean_survives_a_conditional_false_update() {
    let compiled = Compiler::new()
        .model("GuardedIncomingBoolean")
        .compile_str(SOURCE, "GuardedIncomingBoolean.mo")
        .unwrap_or_else(|error| panic!("the reached incoming Boolean must stay defined: {error}"));
    assert_runtime_call_retained(&compiled.dae);
    for (enabled, validity, first, trials, expected) in [
        (1.0, 1.0, 3.0, 3.0, 2.0),
        (1.0, 1.0, 1.0, 3.0, 0.0),
        (0.0, 1.0, 3.0, 3.0, 0.0),
        (1.0, 0.0, 3.0, 3.0, 0.0),
        (1.0, 1.0, 3.0, 0.0, 0.0),
        (1.0, 1.0, 3.0, 3.0, 2.0),
    ] {
        let probe = eval_dae_at(
            &compiled.dae,
            &SimOptions {
                initial_inputs: vec![
                    ("enabled".into(), enabled),
                    ("validity".into(), validity),
                    ("scores[1]".into(), first),
                    ("trials".into(), trials),
                ],
                ..SimOptions::default()
            },
            &[],
            0.0,
        )
        .unwrap();
        assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
        assert_eq!(solver_value(&probe.report, "y"), expected);
    }
}

/// Preserve the enclosing snapshot context of the raster selector: a mutable
/// validity value, a preceding validating loop, a grid/else selection, and a
/// second captured validity selection enclosing the incoming Boolean's loop.
#[test]
fn nested_captured_validity_keeps_the_incoming_boolean_defined() {
    let source = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/function_guarded_loops/NestedCapturedValidity.mo"
    ));
    let compiled = Compiler::new()
        .model("GuardedIncomingBoolean")
        .compile_str(source, "NestedCapturedValidity.mo")
        .unwrap_or_else(|error| {
            panic!("the nested selected incoming Boolean stays defined: {error}")
        });
    assert_runtime_call_retained(&compiled.dae);
    for (enabled, validity, grid, first, trials, expected) in [
        (1.0, 1.0, 0.0, 3.0, 3.0, 2.0),
        (1.0, 1.0, 0.0, -100.0, 3.0, 0.0),
        (1.0, 1.0, 0.0, 1.0e16, 3.0, 0.0),
        (1.0, 1.0, 1.0, 3.0, 3.0, -1.0),
        (0.0, 1.0, 0.0, 3.0, 3.0, 0.0),
        (1.0, 0.0, 0.0, 3.0, 3.0, 0.0),
        (1.0, 1.0, 0.0, 3.0, 0.0, 0.0),
        (1.0, 1.0, 0.0, 3.0, 1.0, 1.0),
        (1.0, 1.0, 0.0, 3.0, 3.0, 2.0),
    ] {
        let probe = eval_dae_at(
            &compiled.dae,
            &SimOptions {
                initial_inputs: vec![
                    ("enabled".into(), enabled),
                    ("validity".into(), validity),
                    ("grid".into(), grid),
                    ("scores[1]".into(), first),
                    ("trials".into(), trials),
                ],
                ..SimOptions::default()
            },
            &[],
            0.0,
        )
        .unwrap();
        assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
        assert_eq!(solver_value(&probe.report, "y"), expected);
    }
}

fn captured_loop_source(whole_definition: bool) -> String {
    let source = SOURCE.replace("      running := true;\n", "").replace(
        "          count := count + 1;",
        "          for observed in 1:1 loop\n            count := count + 1;\n          end for;",
    );
    assert_ne!(source, SOURCE);
    if whole_definition {
        let defined = source.replace(
            "      for candidate in 1:trials loop",
            "      for candidate in 1:trials loop\n        running := enabled and scores[candidate] >= 1.5;",
        );
        assert_ne!(defined, source);
        defined
    } else {
        source
    }
}

#[test]
fn a_whole_local_definition_before_its_capture_needs_no_entry_value() {
    let source = captured_loop_source(true);
    let compiled = Compiler::new()
        .model("GuardedIncomingBoolean")
        .compile_str(&source, "DominatedCapturedBoolean.mo")
        .unwrap_or_else(|error| panic!("the source defines its local before the capture: {error}"));
    assert_runtime_call_retained(&compiled.dae);
    for (enabled, first, trials, expected) in [
        (1.0, 3.0, 3.0, 2.0),
        (1.0, -100.0, 3.0, 1.0),
        (0.0, 3.0, 3.0, 0.0),
        (1.0, 3.0, 0.0, 0.0),
        (1.0, 3.0, 1.0, 1.0),
        (1.0, 3.0, 3.0, 2.0),
    ] {
        let probe = eval_dae_at(
            &compiled.dae,
            &SimOptions {
                initial_inputs: vec![
                    ("enabled".into(), enabled),
                    ("scores[1]".into(), first),
                    ("trials".into(), trials),
                ],
                ..SimOptions::default()
            },
            &[],
            0.0,
        )
        .unwrap();
        assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
        assert_eq!(solver_value(&probe.report, "y"), expected);
    }
}

#[test]
fn a_captured_boolean_read_without_an_incoming_definition_is_refused() {
    let source = captured_loop_source(false);
    let error = Compiler::new()
        .model("GuardedIncomingBoolean")
        .compile_str(&source, "UndefinedCapturedBoolean.mo")
        .expect_err("the capture cannot supply its missing incoming Boolean");
    let conditional = source
        .find("scores[candidate] < 1.5")
        .expect("the one-sided false update owns the partial definition");
    super::undefined_source::assert_refusal(
        &error,
        &source,
        "GuardedIncomingBoolean",
        "function conditional",
        &format!(
            "`Scan.Count` reads `running`, which only some branches of the conditional at byte {conditional} define"
        ),
    );
}
