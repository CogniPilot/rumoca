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
