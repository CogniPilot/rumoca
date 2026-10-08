//! MLS 3.6 §12.4.4: a function value is defined on the paths that wrote it.
//! A record result that a call writes on the path under a request guard, and
//! whose later read sits under validity guards that imply that path, is
//! defined where it is read. The same read outside the guard is a read of a
//! value only some paths define and stays refused.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const GUARDED: &str = r#"
package GuardedProblem
  constant Integer capacity = 3;

  record Problem
    Boolean accepted;
    Integer nodeCount;
    Real positions[capacity];
  end Problem;

  function Prepare
    input Integer count;
    output Problem problem;
  algorithm
    problem.accepted := count >= 0 and count <= capacity;
    problem.nodeCount := count;
    problem.positions := {1,2,3};
  end Prepare;

  function Correct
    input Integer count;
    input Boolean requested;
    output Real result;
  protected
    Problem problem;
    Boolean valid;
  algorithm
    result := 0;
    if requested then
      valid := count >= 0;
      if valid then
        valid := count <= capacity;
      end if;
      if valid then
        problem := Prepare(count);
        valid := problem.accepted;
      end if;
      if valid then
        for node in 1:capacity loop
          if node <= problem.nodeCount then
            problem.positions[node] := 2*problem.positions[node];
            result := result+problem.positions[node];
          end if;
        end for;
      end if;
    end if;
  end Correct;
end GuardedProblem;

model GuardedProblemProbe
  input Integer count = 2;
  input Boolean requested = true;
  output Real result;
equation
  result = GuardedProblem.Correct(count,requested);
end GuardedProblemProbe;
"#;

/// The fixture with the closing loop moved under `condition`.
fn guarded_by(condition: &str, before: &str) -> String {
    let closing = "      if valid then\n        for node";
    assert!(
        GUARDED.contains(closing),
        "the fixture keeps its closing guard"
    );
    GUARDED.replacen(
        closing,
        &format!("{before}      if {condition} then\n        for node"),
        1,
    )
}

fn compile(model: &str, source: &str) -> Result<rumoca::CompilationResult, String> {
    Compiler::new()
        .model(model)
        .compile_str(source, "GuardedProblem.mo")
        .map_err(|error| error.to_string())
}

fn result_value(compiled: &rumoca::CompilationResult) -> f64 {
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("GuardedProblemProbe should evaluate: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "result")
        .expect("result is a solver output")
        .value
}

#[test]
fn a_record_defined_under_validity_guards_is_defined_under_the_guard_that_implies_them() {
    let compiled = compile("GuardedProblemProbe", GUARDED)
        .unwrap_or_else(|error| panic!("GuardedProblemProbe should compile: {error}"));
    // count = 2: positions {2, 4} are doubled and summed.
    assert_eq!(result_value(&compiled), 2.0 + 4.0);
}

#[test]
fn a_conjunction_with_the_validity_is_defined_where_the_record_is() {
    let source = guarded_by("valid and count > 0", "");
    let compiled = compile("GuardedProblemProbe", &source)
        .unwrap_or_else(|error| panic!("a conjunction with `valid` should compile: {error}"));
    assert_eq!(result_value(&compiled), 2.0 + 4.0);
}

#[test]
fn a_read_under_a_guard_that_does_not_imply_the_validity_is_refused() {
    let source = guarded_by("count <= capacity", "");
    let error = compile("GuardedProblemProbe", &source)
        .map(|_| ())
        .expect_err("the record is undefined when count is negative");
    assert!(error.contains("only some branches"), "{error}");
}

#[test]
fn a_read_after_the_guard_value_was_written_again_is_refused() {
    let source = guarded_by("valid", "      valid := true;\n");
    let error = compile("GuardedProblemProbe", &source)
        .map(|_| ())
        .expect_err("the record is undefined once `valid` no longer tells whether it was written");
    assert!(error.contains("only some branches"), "{error}");
}

#[test]
fn an_if_expression_arm_selected_by_the_validity_reads_the_record() {
    let source = guarded_by(
        "valid",
        "      result := if valid then problem.nodeCount else 0;\n",
    );
    let compiled = compile("GuardedProblemProbe", &source)
        .unwrap_or_else(|error| panic!("the arm is selected by `valid`: {error}"));
    // nodeCount = 2 seeds the sum of the doubled positions {2, 4}.
    assert_eq!(result_value(&compiled), 2.0 + 2.0 + 4.0);
}

#[test]
fn an_if_expression_arm_selected_by_another_guard_is_refused() {
    let source = guarded_by(
        "valid",
        "      result := if count <= capacity then problem.nodeCount else 0;\n",
    );
    let error = compile("GuardedProblemProbe", &source)
        .map(|_| ())
        .expect_err("the record is undefined when count is negative");
    assert!(error.contains("only some branches"), "{error}");
}

const LOOP_VALIDITY: &str = r#"
package LoopValidity
  constant Integer capacity = 3;

  record Problem
    Boolean accepted;
    Integer nodeCount;
    Real positions[capacity];
  end Problem;

  function Prepare
    input Integer count;
    output Problem problem;
  algorithm
    problem.accepted := count >= 0 and count <= capacity;
    problem.nodeCount := count;
    problem.positions := {1,2,3};
  end Prepare;

  function Correct
    input Integer count;
    input Boolean requested;
    output Real result;
  protected
    Problem problem;
    Problem proposal;
    Boolean valid;
  algorithm
    result := 0;
    if requested then
      valid := count >= 0;
      if valid then
        problem := Prepare(count);
        valid := problem.accepted;
      end if;
      if valid then
        proposal := problem;
        for node in 1:capacity loop
          if node <= problem.nodeCount then
            valid := valid and problem.positions[node] <= 1e6;
            proposal.positions[node] := 2*problem.positions[node];
          end if;
        end for;
      end if;
      if valid then
        result := proposal.positions[1] + proposal.positions[2];
      end if;
    end if;
  end Correct;
end LoopValidity;

model LoopValidityProbe
  input Integer count = 2;
  input Boolean requested = true;
  output Real result;
equation
  result = LoopValidity.Correct(count,requested);
end LoopValidityProbe;
"#;

fn loop_validity(read_guard: &str) -> String {
    let source = LOOP_VALIDITY.replacen(
        "      if valid then\n        result :=",
        &format!("      if {read_guard} then\n        result :="),
        1,
    );
    assert!(
        source.contains(read_guard),
        "the fixture keeps its closing read"
    );
    source
}

/// A Boolean captured as `requested and valid` proves `valid` false when it
/// is false on a path where `requested` holds, so the path that skipped the
/// loop block cannot reach the later read under `valid`.
#[test]
fn a_validity_correlated_through_a_loop_block_defines_the_read_after_it() {
    let compiled = compile("LoopValidityProbe", &loop_validity("valid"))
        .unwrap_or_else(|error| panic!("the read is under the block's guard: {error}"));
    // count = 2: positions {1, 2, 3} become {2, 4, 3}; 2 + 4.
    assert_eq!(result_value(&compiled), 2.0 + 4.0);
}

#[test]
fn a_read_whose_guard_does_not_imply_the_block_guard_after_a_loop_block_is_refused() {
    let error = compile("LoopValidityProbe", &loop_validity("count >= 0"))
        .map(|_| ())
        .expect_err("the block is skipped when the problem is not accepted");
    assert!(error.contains("only some branches"), "{error}");
}
