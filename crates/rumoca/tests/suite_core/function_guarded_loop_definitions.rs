//! MLS 3.7 sections 11.2.2 and 11.2.6: a loop selected by one loop-invariant
//! runtime condition that writes several arrays element by element defines
//! each of them where it runs, so a read after the loop in the same selected
//! sequence sees every element.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

fn source(write_range: &str) -> String {
    format!(
        r#"
package V
  constant Integer n = 4;
  function Total
    input Real enabled[n];
    input Boolean requested;
    output Real y;
  protected
    Real mask[n]; Real scaled[n];
  algorithm
    y := 0;
    if requested then
      for slot in {write_range} loop
        mask[slot] := if enabled[slot] > 0.5 then 1.0 else 0.0;
        scaled[slot] := 2.0 * mask[slot];
      end for;
      y := sum(mask) + sum(scaled);
    end if;
  end Total;
end V;
model Guarded
  Real y = V.Total({{1, 0, 1, 1}}, true);
end Guarded;
"#
    )
}

#[test]
fn a_guarded_loop_with_several_array_targets_defines_each_of_them() {
    let compiled = Compiler::new()
        .model("Guarded")
        .compile_str(&source("1:n"), "Guarded.mo")
        .expect("the guarded loop defines both arrays before they are read");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the guarded loop DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let y = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == "y")
        .expect("solver value y")
        .value;
    assert_eq!(y, 3.0 + 6.0);
}

#[test]
fn a_guarded_loop_that_leaves_elements_undefined_is_refused() {
    let error = Compiler::new()
        .model("Guarded")
        .compile_str(&source("1:n-1"), "Guarded.mo")
        .expect_err("an element the guarded loop never writes is read");
    assert!(
        error.to_string().contains("do not all have a definition"),
        "{error}"
    );
}

fn nested_source(guard: &str) -> String {
    format!(
        r#"
package N
  constant Integer d = 3;
  function Fit
    input Real s[d,d];
    input Real t[d,d];
    output Real y;
  algorithm
    y := sum(s) + sum(t);
  end Fit;
  function Search
    input Real src[d,d];
    input Real dst[d,d];
    input Boolean requested;
    input Integer trials;
    output Real best;
  protected
    Real sampleSource[d,d]; Real sampleTarget[d,d];
    Integer chosen[d];
  algorithm
    best := 0;
    if requested then
      for hypothesis in 1:trials loop
        chosen := {{1, 2, 3}};
        {guard}
        for point in 1:d loop
          sampleSource[point,:] := src[chosen[point],:];
          sampleTarget[point,:] := dst[chosen[point],:];
        end for;
        best := best + Fit(sampleSource, sampleTarget);
        {end_guard}
      end for;
    end if;
  end Search;
end N;
model Nested
  Real y = N.Search({{{{1,2,3}},{{4,5,6}},{{7,8,9}}}}, {{{{1,2,3}},{{4,5,6}},{{7,8,10}}}}, true, 2);
end Nested;
"#,
        end_guard = if guard.is_empty() { "" } else { "end if;" },
    )
}

/// An iteration-local array a loop writes row by row is defined for every
/// element before its read, also inside a selected sequence that owns the
/// outer loop, whose inner loop is a fold nested in the outer transition.
#[test]
fn a_nested_loop_defines_iteration_scratch_inside_a_selected_sequence() {
    let compiled = Compiler::new()
        .model("Nested")
        .compile_str(&nested_source(""), "Nested.mo")
        .expect("the inner loop defines both sample arrays on every iteration");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the nested loop DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let y = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == "y")
        .expect("solver value y")
        .value;
    // Two hypotheses over the rows of both matrices: 2 * (45 + 46).
    assert_eq!(y, 182.0);
}

/// A guard that reads a value the loop carries selects per iteration, so the
/// arrays it guards are not defined on every iteration.
#[test]
fn a_per_iteration_guard_leaves_the_scratch_undefined() {
    let error = Compiler::new()
        .model("Nested")
        .compile_str(&nested_source("if best >= 0 then"), "Nested.mo")
        .expect_err("the sample arrays are defined only when the guard holds");
    assert!(
        error.to_string().contains("do not all have a definition"),
        "{error}"
    );
}

const SPLIT_BRANCHES: &str = r#"
package U
  constant Integer cap = 3;
  record State
    Integer generation;
    Real total;
    Real weights[cap];
  end State;
  function Empty
    input Integer generation = 1;
    output State result;
  algorithm
    result.generation := generation;
    result.total := 0;
    result.weights := zeros(cap);
  end Empty;
  function Unrelated
    input State previous;
    input Boolean reset;
    input Boolean other;
    output Real y;
  protected
    State working;
  algorithm
    if reset then
      working := Empty(5);
    end if;
    if other then
      working := previous;
      for i in 1:cap loop
        working.total := working.total + previous.weights[i];
      end for;
    end if;
    y := 0;
    if reset or other then
      y := working.generation;
    end if;
  end Unrelated;
  function Score
    input State previous;
    input Boolean reset;
    input Boolean requested;
    output Real y;
  protected
    State working;
    Boolean valid;
  algorithm
    y := -1;
    if requested then
      valid := true;
      if reset then
        working := Empty(5);
      else
        working := previous;
        for i in 1:cap loop
          valid := valid and previous.weights[i] >= 0;
        end for;
      end if;
      if valid then
        working.total := working.generation + sum(working.weights);
        y := working.total;
      end if;
    end if;
  end Score;
end U;
model Split
  parameter Real w = 2;
  Real kept = U.Score(U.State(2, 0, {w, 3, 4}), false, true);
  Real reset = U.Score(U.State(2, 0, {w, 3, 4}), true, true);
  Real idle = U.Score(U.State(2, 0, {w, 3, 4}), false, false);
  Real bad = U.Score(U.State(2, 0, {w, -3, 4}), false, true);
end Split;
model Unrelated
  parameter Real w = 2;
  Real y = U.Unrelated(U.State(2, 0, {w, 3, 4}), false, true);
end Unrelated;
"#;

/// MLS 3.7 section 11.2.6: a conditional whose branches both define a record
/// value, one of them through a loop, defines it on every path; the
/// compiler splits the conditional into guarded sequences and joins the
/// guard and its complement.
#[test]
fn both_sides_of_a_split_conditional_define_a_record() {
    let compiled = Compiler::new()
        .model("Split")
        .compile_str(SPLIT_BRANCHES, "Split.mo")
        .expect("both branches define every field of the record");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the split conditional DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(solver_value(&probe.report, "kept"), 11.0);
    assert_eq!(solver_value(&probe.report, "reset"), 5.0);
    assert_eq!(solver_value(&probe.report, "idle"), -1.0);
    assert_eq!(solver_value(&probe.report, "bad"), -1.0);
}

/// Two conditionals over unrelated conditions do not define a value together:
/// neither condition is the complement of the other.
#[test]
fn unrelated_conditionals_do_not_define_a_record_together() {
    let error = Compiler::new()
        .model("Unrelated")
        .compile_str(SPLIT_BRANCHES, "Unrelated.mo")
        .expect_err("the record is defined only when one of two unrelated conditions holds");
    assert!(error.to_string().contains("only some branches"), "{error}");
}

fn solver_value(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| panic!("missing solver value {name}"))
        .value
}

const ASSERTED: &str = r#"
package W
  constant Integer n = 4;
  function Scores
    input Real rgb[:];
    input Boolean enabled;
    output Real scores[size(rgb, 1)];
  protected
    Real gray[size(rgb, 1)];
  algorithm
    scores := zeros(size(rgb, 1));
    if enabled then
      assert(size(rgb, 1) >= 2, "needs two samples");
      for slot in 1:size(rgb, 1) loop
        gray[slot] := 2.0 * rgb[slot];
      end for;
      for slot in 2:size(rgb, 1) - 1 loop
        scores[slot] := gray[slot];
      end for;
    end if;
  end Scores;
end W;
model Asserted
  Real scores[W.n] = W.Scores({1, 2, 3, 4}, time < 0.5);
end Asserted;
"#;

/// A proven assertion at the head of a guarded branch that also holds loops
/// leaves a conditional with no value to define; the loops that follow it keep
/// their guard.
#[test]
fn a_proven_assertion_before_guarded_loops_leaves_their_definitions_intact() {
    let compiled = Compiler::new()
        .model("Asserted")
        .compile_str(ASSERTED, "Asserted.mo")
        .expect("the proven assertion does not hide the guarded loops");
    for (time, expected) in [(0.0, [0.0, 4.0, 6.0, 0.0]), (1.0, [0.0; 4])] {
        let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], time)
            .expect("the guarded DAE evaluates");
        assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
        for (index, want) in expected.iter().enumerate() {
            let name = format!("scores[{}]", index + 1);
            let got = probe
                .report
                .solver_y
                .iter()
                .find(|slot| slot.name.replace(' ', "") == name)
                .unwrap_or_else(|| panic!("solver value {name}"))
                .value;
            assert_eq!(got, *want, "{name} at t={time}");
        }
    }
}
