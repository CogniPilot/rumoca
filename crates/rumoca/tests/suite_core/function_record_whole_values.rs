//! Record values a function writes whole, updates field by field inside
//! branches, and reads whole (MLS §12.4.4, §12.6): the record is split into
//! field locals, a whole write is projected (or held once when its value is a
//! call), and a whole read is reassembled by the record constructor from the
//! field locals current at the read.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const WHOLE_VALUES: &str = r#"
within;
package W
  record Frame
    Integer generation;
    Real x[2];
    Real histogram[2];
  end Frame;
  function Empty
    output Frame f;
  algorithm
    f.generation := 0;
    f.x := zeros(2);
    f.histogram := {7, 8};
  end Empty;
  function Build
    input Boolean valid;
    input Integer g;
    input Real v;
    output Frame frame;
  protected
    Frame candidate;
  algorithm
    frame := Empty();
    if valid then
      candidate := Empty();
      candidate.x[1] := v;
      if v > 0 then
        candidate.generation := g;
        for k in 1:2 loop
          candidate.x[2] := candidate.x[2] + k * v;
        end for;
        frame := candidate;
      end if;
    end if;
  end Build;
  function Total
    input Frame f;
    output Real y;
  algorithm
    y := f.generation + 10 * f.x[1] + 100 * f.x[2] + 1000 * f.histogram[1];
  end Total;
  function Carry
    input Frame previous;
    input Real v;
    output Real y;
  protected
    Frame next;
  algorithm
    next := previous;
    if v > 0 then
      next.generation := next.generation + 1;
      next.x[2] := v;
    end if;
    y := Total(next);
  end Carry;
end W;

model ObserveWholeValues
  Real accepted = W.Total(W.Build(true, 3, 2));
  Real rejected = W.Total(W.Build(true, 3, -1));
  Real idle = W.Total(W.Build(false, 3, 2));
  Real carried = W.Carry(W.Frame(4, {1, 2}, {5, 6}), 9);
  Real kept = W.Carry(W.Frame(4, {1, 2}, {5, 6}), -9);
end ObserveWholeValues;
"#;

fn value(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| panic!("missing solver value {name}"))
        .value
}

#[test]
fn a_record_written_whole_updated_in_branches_and_read_whole_is_split() {
    let compiled = Compiler::new()
        .model("ObserveWholeValues")
        .compile_str(WHOLE_VALUES, "ObserveWholeValues.mo")
        .expect("whole writes, branch updates and whole reads of a record compile");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the split record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    // `Total` weighs generation, x[1], x[2] and histogram[1] by 1, 10, 100 and
    // 1000; the histogram keeps the value `Empty()` held.
    assert_eq!(
        value(&probe.report, "accepted"),
        3.0 + 20.0 + 600.0 + 7000.0
    );
    assert_eq!(value(&probe.report, "rejected"), 7000.0);
    assert_eq!(value(&probe.report, "idle"), 7000.0);
    assert_eq!(value(&probe.report, "carried"), 5.0 + 10.0 + 900.0 + 5000.0);
    assert_eq!(value(&probe.report, "kept"), 4.0 + 10.0 + 200.0 + 5000.0);
}

const ARRAY_FIELDS: &str = r#"
within;
package A
  record Edge
    Integer id;
    Real r;
  end Edge;
  record State
    Edge edges[2];
  end State;
  function Count
    input Edge edges[:];
    output Real y;
  algorithm
    y := sum(edges.r);
  end Count;
  function CountState
    input State s;
    output Real y;
  algorithm
    y := Count(s.edges);
  end CountState;
  function Edges
    input Real u;
    output Real y;
  protected
    State s;
  algorithm
    s.edges[1].id := 1;
    s.edges[1].r := 1;
    s.edges[2].id := 2;
    s.edges[2].r := 2;
    if u > 0 then
      s.edges[2].r := u;
    end if;
    y := Count(s.edges);
  end Edges;
  function Whole
    input Real u;
    output Real y;
  protected
    State s;
  algorithm
    s.edges[1].id := 1;
    s.edges[1].r := 1;
    s.edges[2].id := 2;
    s.edges[2].r := 2;
    if u > 0 then
      s.edges[2].r := u;
    end if;
    y := CountState(s);
  end Whole;
end A;
model ObserveEdges
  Real y = A.Edges(3);
end ObserveEdges;
model ObserveWhole
  Real y = A.Whole(3);
end ObserveWhole;
"#;

/// An array-of-records field read whole is not split, so it keeps one
/// record-array local whose element fields the branch writes.
#[test]
fn an_array_of_records_read_whole_keeps_its_local() {
    let compiled = Compiler::new()
        .model("ObserveEdges")
        .compile_str(ARRAY_FIELDS, "ObserveEdges.mo")
        .expect("an array of records read whole compiles from its record-array local");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the record-array DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(value(&probe.report, "y"), 4.0);
}

/// A record read whole whose array-of-records field is written below keeps
/// its source form (no constructor call reassembles struct-of-arrays
/// columns), and the existing record-array element owners evaluate it.
#[test]
fn a_record_with_a_written_record_array_field_keeps_its_source_form() {
    let compiled = Compiler::new()
        .model("ObserveWhole")
        .compile_str(ARRAY_FIELDS, "ObserveWhole.mo")
        .expect("a record read whole over a written record-array field compiles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(value(&probe.report, "y"), 4.0);
}

/// MLS 3.7 §12.4.4: a split record whose only whole write is conditional
/// leaves its other fields unassigned on the untaken path, so reading it
/// whole is refused rather than reassembled from undefined field locals.
#[test]
fn a_conditionally_defined_split_record_read_whole_is_refused() {
    let source = r#"
within;
package U
  record Frame
    Integer generation;
    Real x[2];
  end Frame;
  function Empty
    output Frame f;
  algorithm
    f.generation := 0;
    f.x := zeros(2);
  end Empty;
  function Build
    input Real v;
    output Frame frame;
  protected
    Frame candidate;
  algorithm
    if v > 0 then
      candidate := Empty();
    end if;
    candidate.x[1] := v;
    if v > 1 then
      candidate.generation := 1;
    end if;
    frame := candidate;
  end Build;
end U;
model ObserveUndefined
  U.Frame f = U.Build(time + 2);
end ObserveUndefined;
"#;
    let error = Compiler::new()
        .model("ObserveUndefined")
        .compile_str(source, "ObserveUndefined.mo")
        .expect_err("a field unassigned on one path is not read");
    let message = error.to_string();
    assert!(
        message.contains("do not all have a definition"),
        "unexpected diagnostic: {message}"
    );
}
