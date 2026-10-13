//! MLS 3.7 sections 11.2.2 and 11.2.6: a loop selected by one loop-invariant
//! runtime condition that writes several arrays element by element defines
//! each of them where it runs, so a read after the loop in the same selected
//! sequence sees every element.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

mod asserted_values;
mod assigned_loop_exit_scale;
mod exact_raster;
mod exhaustive_flow;
mod generated_captures;
mod lazy_assignments;
mod mixed_conditional_types;
mod original_build_pairs;
mod partial_base_reads;
mod record_gated_flow;
mod record_loop_entry;
mod record_root_reads;
mod saturate3;
mod selected_nearest;
mod slot_subscripts;
mod split_runtime;
mod threshold_scan;
mod tuple_argument_reads;
pub(super) mod undefined_source;
mod whole_defined_slices;

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

/// The capture selects both the complete child writes and their read. Its
/// value is fixed for that selected sequence, even when the read updates best.
#[test]
fn a_per_iteration_guard_defines_scratch_where_the_same_capture_reads_it() {
    let compiled = Compiler::new()
        .model("Nested")
        .compile_str(&nested_source("if best >= 0 then"), "Nested.mo")
        .unwrap_or_else(|error| panic!("the selected source read has complete coverage: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0).unwrap();
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(solver_value(&probe.report, "y"), 182.0);
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
    if reset or not other then
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

/// Two conditionals over unrelated conditions leave the value undefined where
/// neither holds, and a later read under `reset or not other` is reached there.
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

fn canonical_static_call_values(source: &str, model: &str) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "GuardedNumericCause.mo")
        .expect("the original guarded function control compiles to canonical DAE");
    compiled.dae.inspect(|view| {
        let calls = (0..view.expression_count())
            .map(|index| view.expression_id(index).unwrap())
            .filter(|id| {
                let expression = view.expression(*id).unwrap();
                expression.function_scope().is_none()
                    && matches!(
                        expression.operation(),
                        rumoca_ir_dae::ExpressionOperation::Call { .. }
                    )
            })
            .collect::<Vec<_>>();
        assert_eq!(calls.len(), 1, "one original model-level checked call");
        rumoca_eval_dae::NumericEvaluator::new(view)
            .expression(calls[0])
            .expect("the complete literal call inputs require no runtime substitution")
    })
}

#[test]
fn guarded_array_writes_have_correct_values_in_canonical_dae() {
    assert_eq!(
        canonical_static_call_values(&source("1:n"), "Guarded"),
        vec![9.0]
    );
}

#[test]
fn nested_iteration_scratch_has_correct_values_in_canonical_dae() {
    assert_eq!(
        canonical_static_call_values(&nested_source(""), "Nested"),
        vec![182.0]
    );
}

#[test]
fn ordered_numeric_fold_outputs_preserve_disabled_and_empty_domains() {
    let disabled = source("1:n").replace(", true);", ", false);");
    assert_ne!(disabled, source("1:n"));
    assert_eq!(
        canonical_static_call_values(&disabled, "Guarded"),
        vec![0.0]
    );
    let empty = nested_source("").replace(", true, 2);", ", true, 0);");
    assert_ne!(empty, nested_source(""));
    assert_eq!(canonical_static_call_values(&empty, "Nested"), vec![0.0]);
    let disabled_nested = nested_source("").replace(", true, 2);", ", false, 2);");
    assert_ne!(disabled_nested, nested_source(""));
    assert_eq!(
        canonical_static_call_values(&disabled_nested, "Nested"),
        vec![0.0]
    );
}

fn assert_runtime_call_retained(dae: &rumoca_ir_dae::Dae) {
    let solve = rumoca_sim::lower_dae_for_simulation(dae, &SimOptions::default())
        .expect("external input function lowers without compile-time evaluation");
    assert!(
        !solve.pure_calls.owners().is_empty(),
        "checked function owner remains"
    );
    assert!(
        solve
            .problem
            .continuous
            .implicit_rhs
            .nodes
            .iter()
            .filter_map(|node| match node {
                rumoca_ir_solve::ComputeNode::ScalarPrograms(block) => Some(block),
                _ => None,
            })
            .flat_map(|block| block.programs())
            .flatten()
            .any(|operation| matches!(operation, rumoca_ir_solve::LinearOp::PureCall { .. })),
        "the algebraic runtime executes a checked call"
    );
}

#[test]
fn guarded_array_writes_execute_with_nonconstant_external_inputs() {
    let original = source("1:n");
    let runtime = original.replace(
        "  Real y = V.Total({1, 0, 1, 1}, true);",
        "  input Real enabled[4] = {1,0,1,1};\n  input Boolean requested = true;\n  Real y = V.Total(enabled, requested);",
    );
    assert_ne!(runtime, original);
    let compiled = Compiler::new()
        .model("Guarded")
        .compile_str(&runtime, "RuntimeGuarded.mo")
        .unwrap();
    assert_runtime_call_retained(&compiled.dae);
    for (requested, last, expected) in [
        (1.0, 1.0, 9.0),
        (0.0, 1.0, 0.0),
        (1.0, 0.0, 6.0),
        (1.0, 1.0, 9.0),
    ] {
        let probe = eval_dae_at(
            &compiled.dae,
            &SimOptions {
                initial_inputs: vec![("requested".into(), requested), ("enabled[4]".into(), last)],
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
fn nested_iteration_scratch_executes_with_nonconstant_external_inputs() {
    let runtime = runtime_nested_coverage_source();
    let compiled = Compiler::new()
        .model("Nested")
        .compile_str(&runtime, "RuntimeNested.mo")
        .unwrap();
    assert_runtime_call_retained(&compiled.dae);
    for (requested, trials, first, expected) in [
        (1.0, 2.0, 1.0, 182.0),
        (1.0, 1.0, 1.0, 91.0),
        (1.0, 0.0, 1.0, 0.0),
        (0.0, 2.0, 1.0, 0.0),
        (1.0, 2.0, 2.0, 184.0),
        (1.0, 2.0, 1.0, 182.0),
    ] {
        let probe = eval_dae_at(
            &compiled.dae,
            &SimOptions {
                initial_inputs: vec![
                    ("requested".into(), requested),
                    ("trials".into(), trials),
                    ("src[1,1]".into(), first),
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

fn runtime_nested_coverage_source() -> String {
    runtime_nested_source("")
}

fn runtime_nested_source(guard: &str) -> String {
    let original = nested_source(guard);
    let runtime = original.replace(
        "  Real y = N.Search({{1,2,3},{4,5,6},{7,8,9}}, {{1,2,3},{4,5,6},{7,8,10}}, true, 2);",
        "  input Real src[3,3] = {{1,2,3},{4,5,6},{7,8,9}};\n  input Real dst[3,3] = {{1,2,3},{4,5,6},{7,8,10}};\n  input Boolean requested = true;\n  input Integer trials = 2;\n  Real y = N.Search(src,dst,requested,min(2,max(0,trials)));",
    );
    assert_ne!(runtime, original);
    runtime
}

fn assert_nested_coverage_refused(
    original: &str,
    source: &str,
    conditional_byte: Option<usize>,
) -> std::ops::Range<usize> {
    assert_ne!(
        source, original,
        "the negative control must change the complete fixture"
    );
    let error = Compiler::new()
        .model("Nested")
        .compile_str(source, "PartialNested.mo")
        .expect_err("a reached whole-array read has incomplete source coverage");
    let detail = if let Some(byte) = conditional_byte {
        format!(
            "`N.Search` reads `sampleSource`, which only some branches of the conditional at byte {byte} define"
        )
    } else {
        "`N.Search` reads elements of `sampleSource` that do not all have a definition".into()
    };
    let owner = if conditional_byte.is_some() {
        "function conditional"
    } else {
        "function element assignment"
    };
    undefined_source::assert_refusal(&error, source, "Nested", owner, &detail);
    let rumoca::CompilerError::CompileDiagnosticsError { failures, .. } = &error else {
        unreachable!("the shared checker already required typed source diagnostics")
    };
    let label = failures
        .iter()
        .find(|failure| failure.model_name == "Nested")
        .unwrap()
        .primary_label
        .as_ref()
        .unwrap();
    label.span.start.0..label.span.end.0
}

#[test]
fn a_parent_varying_capture_defines_scratch_on_its_selected_child_path() {
    let original = runtime_nested_coverage_source();
    let source = original
        .replace(
            "        for point in",
            "        if hypothesis == 1 then\n        for point in",
        )
        .replace(
            "        best := best + Fit(sampleSource, sampleTarget);",
            "        best := best + Fit(sampleSource, sampleTarget);\n        end if;",
        );
    assert_ne!(source, original);
    let compiled = Compiler::new()
        .model("Nested")
        .compile_str(&source, "SelectedParentChild.mo")
        .expect("every row is defined on the same captured parent path as its read");
    assert_runtime_call_retained(&compiled.dae);
    for (requested, trials, first, expected) in [
        (1.0, 2.0, 1.0, 91.0),
        (1.0, 1.0, 1.0, 91.0),
        (1.0, 0.0, 1.0, 0.0),
        (0.0, 2.0, 1.0, 0.0),
        (1.0, 2.0, 2.0, 92.0),
    ] {
        let probe = eval_dae_at(
            &compiled.dae,
            &SimOptions {
                initial_inputs: vec![
                    ("requested".into(), requested),
                    ("trials".into(), trials),
                    ("src[1,1]".into(), first),
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
fn unconditional_elements_and_complete_guarded_coverage_remain_readable() {
    let source = r#"
package Partial
  function Sum
    input Boolean requested;
    output Real y;
  protected
    Real values[3];
  algorithm
    values[1] := 1;
    if requested then
      for row in 1:3 loop
        values[row] := row;
      end for;
    end if;
    y := if requested then sum(values) else values[1];
  end Sum;
end Partial;
model PartialCoverage
  input Boolean requested = false;
  Real y = Partial.Sum(requested);
end PartialCoverage;
"#;
    let compiled = Compiler::new()
        .model("PartialCoverage")
        .compile_str(source, "PartialCoverage.mo")
        .expect("the selected whole value and unconditional first element have source owners");
    assert_runtime_call_retained(&compiled.dae);
    for (requested, expected) in [(1.0, 6.0), (0.0, 1.0), (1.0, 6.0)] {
        let probe = eval_dae_at(
            &compiled.dae,
            &SimOptions {
                initial_inputs: vec![("requested".into(), requested)],
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
fn a_per_iteration_capture_keeps_its_selected_read_with_changing_inputs() {
    let source = runtime_nested_source("if best >= 0 then");
    let compiled = Compiler::new()
        .model("Nested")
        .compile_str(&source, "SelectedReadInputs.mo")
        .unwrap_or_else(|error| {
            panic!("the reached read retains its selected source owner: {error}")
        });
    assert_runtime_call_retained(&compiled.dae);
    for (requested, trials, first, expected) in [
        (1.0, 2.0, 1.0, 182.0),
        (1.0, 2.0, 0.0, 180.0),
        (1.0, 2.0, -100.0, -10.0),
        (0.0, 2.0, -100.0, 0.0),
        (1.0, 0.0, -100.0, 0.0),
        (1.0, 1.0, -100.0, -10.0),
        (1.0, 2.0, 1.0, 182.0),
    ] {
        let probe = eval_dae_at(
            &compiled.dae,
            &SimOptions {
                initial_inputs: vec![
                    ("requested".into(), requested),
                    ("trials".into(), trials),
                    ("src[1,1]".into(), first),
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
fn a_skipped_per_iteration_capture_cannot_define_an_unguarded_read() {
    let original = runtime_nested_source("if best >= 0 then");
    let source = original.replace("best := 0;", "best := -1;").replace(
        "        best := best + Fit(sampleSource, sampleTarget);\n        end if;",
        "        end if;\n        best := best + Fit(sampleSource, sampleTarget);",
    );
    assert_ne!(source, original);
    let child_range = source
        .find("1:d loop")
        .expect("the same child loop owns its guarded coverage");
    assert_nested_coverage_refused(&original, &source, Some(child_range));
}

#[test]
fn a_nested_partial_point_domain_does_not_define_the_whole_matrix() {
    let original = runtime_nested_coverage_source();
    let source = original.replace("point in 1:d loop", "point in 1:d-1 loop");
    assert_nested_coverage_refused(&original, &source, None);
}

#[test]
fn parent_indexed_child_writes_do_not_define_the_whole_matrix() {
    let original = runtime_nested_coverage_source();
    let source = original
        .replace("sampleSource[point,:]", "sampleSource[hypothesis,:]")
        .replace("sampleTarget[point,:]", "sampleTarget[hypothesis,:]");
    assert_nested_coverage_refused(&original, &source, None);
}

#[test]
fn a_child_parent_predicate_does_not_cover_every_parent_entry() {
    let original = runtime_nested_coverage_source();
    let source = original
        .replace(
            "        for point in",
            "        if hypothesis == 1 then\n        for point in",
        )
        .replace(
            "        best := best + Fit",
            "        end if;\n        best := best + Fit",
        );
    assert_nested_coverage_refused(&original, &source, Some(576));
}

#[test]
fn a_child_guard_that_skips_a_row_does_not_define_the_whole_matrix() {
    let original = runtime_nested_coverage_source();
    let source = original
        .replace(
            "          sampleSource[point,:]",
            "          if point < d then\n          sampleSource[point,:]",
        )
        .replace(
            "          sampleTarget[point,:] := dst[chosen[point],:];",
            "          sampleTarget[point,:] := dst[chosen[point],:];\n          end if;",
        );
    assert_nested_coverage_refused(&original, &source, None);
}

#[test]
fn child_coverage_does_not_define_a_value_after_a_possibly_empty_parent() {
    let original = runtime_nested_coverage_source();
    let source = original
        .replace(
            "        best := best + Fit(sampleSource, sampleTarget);",
            "",
        )
        .replace(
            "  end Search;",
            "    best := Fit(sampleSource, sampleTarget);\n  end Search;",
        );
    assert!(source[471..].starts_with("1:trials loop"));
    assert!(source[544..].starts_with("1:d loop"));
    let read = assert_nested_coverage_refused(&original, &source, Some(471));
    let suffix = "best := Fit(sampleSource, sampleTarget);";
    let suffix_start = source.rfind(suffix).unwrap();
    assert!(read.start >= suffix_start && read.end <= suffix_start + suffix.len());
}

#[test]
fn an_empty_loop_does_not_define_arrays_read_after_it() {
    let error = Compiler::new()
        .model("Guarded")
        .compile_str(&source("1:0"), "EmptyUndefinedArrays.mo")
        .expect_err("no iteration defines mask/scaled before their reached sums");
    assert!(
        error.to_string().contains("do not all have a definition"),
        "{error}"
    );
}

#[test]
fn a_zero_extent_local_remains_defined_after_an_empty_loop() {
    let source = r#"
function EmptyTotal
  output Real y;
protected
  Real values[0,2];
algorithm
  for row in 1:size(values,1) loop
    values[row,:] := {1,2};
  end for;
  y := sum(values);
end EmptyTotal;
model EmptyLocal
  Real y = EmptyTotal();
end EmptyLocal;
"#;
    let compiled = Compiler::new()
        .model("EmptyLocal")
        .compile_str(source, "EmptyLocal.mo")
        .expect("a zero-element local has no undefined element");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0).unwrap();
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(solver_value(&probe.report, "y"), 0.0);
}

mod serial_guarded_state;
