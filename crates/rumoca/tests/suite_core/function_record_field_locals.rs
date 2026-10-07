//! Record results whose record-typed fields are written inside loops and
//! conditionals (MLS §12.4.4): each such field is held in its own local and
//! the result is assembled from the field locals.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const RECORD_FIELD_LOCALS: &str = r#"
within;
package P
  record Edge
    Boolean enabled;
    Integer id;
    Real r[2];
  end Edge;
  record Birth
    Integer generation;
    Integer epoch;
  end Birth;
  record State
    Integer generation;
    Edge edges[3];
    Birth birth;
  end State;
  function EmptyEdge
    input Integer id;
    output Edge result;
  algorithm
    result.enabled := false;
    result.id := id;
    result.r := {id, 2 * id};
  end EmptyEdge;
  function Empty
    input Integer generation;
    input Real u;
    output State result;
  algorithm
    result.generation := generation;
    for slot in 1:3 loop
      result.edges[slot] := EmptyEdge(slot);
    end for;
    result.birth := Birth(generation, -1);
    if u > 0 then
      result.birth := Birth(generation + 1, 7);
    end if;
  end Empty;
  function Observe
    input Integer g;
    input Real u;
    output Real y[5];
  protected
    State s;
  algorithm
    s := Empty(g, u);
    y := {s.generation, s.edges[2].r[2], s.edges[3].id, s.birth.generation, s.birth.epoch};
  end Observe;
end P;

model ObserveRecordFieldLocals
  Real positive[5] = P.Observe(2, 1.0);
  Real negative[5] = P.Observe(2, -1.0);
end ObserveRecordFieldLocals;
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
fn record_fields_written_in_loops_and_branches_assemble() {
    let compiled = Compiler::new()
        .model("ObserveRecordFieldLocals")
        .compile_str(RECORD_FIELD_LOCALS, "ObserveRecordFieldLocals.mo")
        .expect("record fields written in loops and branches assemble from field locals");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the assembled record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    for (prefix, expected) in [
        ("positive", [2.0, 4.0, 3.0, 3.0, 7.0]),
        ("negative", [2.0, 4.0, 3.0, 2.0, -1.0]),
    ] {
        for (index, expected) in expected.into_iter().enumerate() {
            let name = format!("{prefix}[{}]", index + 1);
            assert_eq!(value(&probe.report, &name), expected, "{name}");
        }
    }
}

/// Nested record fields written in branches, element fields of a record
/// array written in loops, and record fields assigned whole and then updated
/// field by field inside branches, from a constructor, a decomposed input, or
/// a function result.
const NESTED_FIELD_LOCALS: &str = r#"
within;
package Q
  record Birth
    Integer generation;
    Integer epoch;
  end Birth;
  record Edge
    Integer id;
    Real r[2];
  end Edge;
  record Estimator
    Real position[2];
    Birth birth;
  end Estimator;
  record State
    Estimator estimator;
    Edge edges[3];
    Integer steps;
  end State;
  record Result
    State next;
    Boolean accepted;
  end Result;
  function Same
    input Estimator e;
    output Estimator m;
  algorithm
    m := e;
  end Same;
  function Publish
    input State previous;
    input Estimator proposed;
    input Real u;
    input Boolean through;
    output Result result;
  algorithm
    result.next := previous;
    result.accepted := false;
    if u > 0 then
      if through then
        result.next.estimator := Same(proposed);
      else
        result.next.estimator := proposed;
      end if;
      if u > 1 then
        result.next.estimator.birth.epoch := previous.estimator.birth.epoch;
        result.next.estimator.position[2] := previous.estimator.position[2];
      end if;
      for slot in 2:3 loop
        result.next.edges[slot].id := 10 * slot;
      end for;
      result.next.steps := previous.steps + 1;
      result.accepted := true;
    end if;
  end Publish;
  function Observe
    input Real u;
    input Boolean through;
    output Real y[9];
  protected
    Result r;
  algorithm
    r := Publish(
      State(Estimator({1, 2}, Birth(3, 4)), {Edge(k, {k, 2 * k}) for k in 1:3}, 5),
      Estimator({10, 20}, Birth(30, 40)), u, through);
    y := {r.next.estimator.position[1], r.next.estimator.position[2],
      r.next.estimator.birth.generation, r.next.estimator.birth.epoch,
      r.next.edges[1].id, r.next.edges[3].id, r.next.edges[3].r[2], r.next.steps,
      if r.accepted then 1 else 0};
  end Observe;
  function Straight
    input Integer g;
    output Integer b[2];
  protected
    Estimator e;
  algorithm
    e.position := {g, g};
    e.birth := Birth(g, 1);
    e.birth.epoch := 2 * g;
    b := {e.birth.generation, e.birth.epoch};
  end Straight;
end Q;

model ObserveNestedFieldLocals
  Real kept[9] = Q.Observe(2.0, false);
  Real replaced[9] = Q.Observe(0.5, true);
  Real untouched[9] = Q.Observe(-1.0, true);
  Real straight[2] = Q.Straight(3);
end ObserveNestedFieldLocals;
"#;

#[test]
fn nested_record_fields_written_in_control_flow_assemble() {
    let compiled = Compiler::new()
        .model("ObserveNestedFieldLocals")
        .compile_str(NESTED_FIELD_LOCALS, "ObserveNestedFieldLocals.mo")
        .expect("nested record fields written in control flow assemble from field locals");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the assembled record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    for (prefix, expected) in [
        ("kept", [10.0, 2.0, 30.0, 4.0, 1.0, 30.0, 6.0, 6.0, 1.0]),
        (
            "replaced",
            [10.0, 20.0, 30.0, 40.0, 1.0, 30.0, 6.0, 6.0, 1.0],
        ),
        ("untouched", [1.0, 2.0, 3.0, 4.0, 1.0, 3.0, 6.0, 5.0, 0.0]),
    ] {
        for (index, expected) in expected.into_iter().enumerate() {
            let name = format!("{prefix}[{}]", index + 1);
            assert_eq!(value(&probe.report, &name), expected, "{name}");
        }
    }
    assert_eq!(value(&probe.report, "straight[1]"), 3.0);
    assert_eq!(value(&probe.report, "straight[2]"), 6.0);
}

/// A nested record read whole and written field by field inside a branch is
/// split, and the whole read is reassembled by its constructor from the field
/// locals current at the read (MLS §12.6).
#[test]
fn nested_record_read_whole_is_reassembled_from_its_field_locals() {
    let source = r#"
within;
package R
  record Birth
    Integer generation;
    Integer epoch;
  end Birth;
  record State
    Birth birth;
    Birth copy;
  end State;
  function F
    input Real u;
    output State r;
  algorithm
    r.birth.generation := 1;
    r.birth.epoch := 2;
    if u > 0 then
      r.birth.epoch := 5;
    end if;
    r.copy := r.birth;
  end F;
  function Observe
    input Real u;
    output Real y;
  protected
    State s;
  algorithm
    s := F(u);
    y := s.copy.epoch;
  end Observe;
end R;
model ObserveWholeRead
  Real y = R.Observe(time + 1);
end ObserveWholeRead;
"#;
    let compiled = Compiler::new()
        .model("ObserveWholeRead")
        .compile_str(source, "ObserveWholeRead.mo")
        .expect("a record field read whole and written in a branch is reassembled");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the reassembled record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(value(&probe.report, "y"), 5.0);
}
