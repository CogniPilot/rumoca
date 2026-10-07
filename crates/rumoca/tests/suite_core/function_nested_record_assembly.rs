//! A function result or local record assembled through nested field paths
//! (MLS §12.4.4): `r.inner.x := ...` writes build `r.inner` field by field.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const NESTED_RECORD_ASSIGNMENT: &str = r#"
within;
package NativeNestedRecordAssignment
  record Identity
    Integer sequence;
  end Identity;
  record State
    Identity identity;
  end State;
  function Seed
    output State state;
  algorithm
    state.identity.sequence := 7;
  end Seed;
end NativeNestedRecordAssignment;

model NativeNestedRecordAssignmentProbe
  output NativeNestedRecordAssignment.State next;
equation
  next = NativeNestedRecordAssignment.Seed();
end NativeNestedRecordAssignmentProbe;
"#;

const NESTED_FIELD_PATHS: &str = r#"
within;
package Nested
  record Birth
    Integer generation;
    Integer epoch;
  end Birth;
  record Estimator
    Real position[3];
  end Estimator;
  record Deep
    Birth birth;
    Real scale;
  end Deep;
  record State
    Estimator estimator;
    Birth birth;
    Deep deep;
    Real time;
  end State;
  function Empty
    input Estimator estimator;
    input Integer generation = 1;
    output State result;
  protected
    Real offset;
  algorithm
    result.estimator := estimator;
    result.time := 0.5;
    result.birth.generation := generation;
    result.birth.epoch := -1;
    offset := 2.0 * generation;
    result.deep.scale := offset;
    result.deep.birth.epoch := generation + 10;
    result.deep.birth.generation := generation + 20;
  end Empty;
  function Observe
    input Real p[3];
    input Integer generation;
    output Real values[9];
  protected
    State state;
  algorithm
    state := Empty(Estimator(p), generation);
    values := {state.estimator.position[1], state.estimator.position[2],
      state.estimator.position[3], state.time, state.birth.generation,
      state.birth.epoch, state.deep.scale, state.deep.birth.generation,
      state.deep.birth.epoch};
  end Observe;
  function ObserveSeed
    output Real sequence;
  protected
    NativeNestedRecordAssignment.State state;
  algorithm
    state := NativeNestedRecordAssignment.Seed();
    sequence := state.identity.sequence;
  end ObserveSeed;
end Nested;

package NativeNestedRecordAssignment
  record Identity
    Integer sequence;
  end Identity;
  record State
    Identity identity;
  end State;
  function Seed
    output State state;
  algorithm
    state.identity.sequence := 7;
  end Seed;
end NativeNestedRecordAssignment;

model ObserveNested
  Real values[9];
  Real sequence;
equation
  values = Nested.Observe({1.0, 2.0, 3.0}, 2);
  sequence = Nested.ObserveSeed();
end ObserveNested;
"#;

fn nested_function(algorithm: &str) -> String {
    format!(
        r#"
within;
record Birth
  Integer generation;
  Integer epoch;
end Birth;
record State
  Birth birth;
  Real time;
end State;
function Build
  input Integer g;
  output State result;
protected
  Integer local;
algorithm
{algorithm}
end Build;
model ObserveBuild
  State state;
equation
  state = Build(3);
end ObserveBuild;
"#
    )
}

fn value(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| panic!("missing solver value {name}"))
        .value
}

#[test]
fn nested_record_result_field_compiles() {
    Compiler::new()
        .model("NativeNestedRecordAssignmentProbe")
        .compile_str(NESTED_RECORD_ASSIGNMENT, "NativeNestedRecordAssignment.mo")
        .expect("a nested record result field is assembled field by field");
}

#[test]
fn nested_field_paths_assemble_every_level() {
    let compiled = Compiler::new()
        .model("ObserveNested")
        .compile_str(NESTED_FIELD_PATHS, "ObserveNested.mo")
        .expect("nested record field writes assemble their records");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the assembled record DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let expected = [1.0, 2.0, 3.0, 0.5, 2.0, -1.0, 4.0, 22.0, 12.0];
    for (index, expected) in expected.into_iter().enumerate() {
        assert_eq!(
            value(&probe.report, &format!("values[{}]", index + 1)),
            expected,
            "values[{}]",
            index + 1
        );
    }
    assert_eq!(value(&probe.report, "sequence"), 7.0);
}

#[test]
fn staged_nested_field_paths_assemble() {
    let source = nested_function(
        "  result.birth.generation := g;\n  result.birth.epoch := -g;\n  local := 2 * g;\n  result.time := local;",
    );
    Compiler::new()
        .model("ObserveBuild")
        .compile_str(&source, "ObserveBuild.mo")
        .expect("a staged record field assembled through nested paths compiles");
}

#[test]
fn record_field_written_whole_and_by_field_is_refused() {
    let source = nested_function(
        "  result.birth := Birth(g, 1);\n  result.birth.epoch := 2;\n  result.time := 0.0;",
    );
    let error = Compiler::new()
        .model("ObserveBuild")
        .compile_str(&source, "ObserveBuild.mo")
        .expect_err("a nested field update of a whole-assigned record is not assembled");
    assert!(
        error
            .to_string()
            .contains("is assigned both whole and field by field"),
        "unexpected diagnostic: {error}"
    );
}

#[test]
fn unwritten_nested_field_is_uninitialized() {
    let source = nested_function("  result.birth.generation := g;\n  result.time := 0.0;");
    let error = Compiler::new()
        .model("ObserveBuild")
        .compile_str(&source, "ObserveBuild.mo")
        .expect_err("a nested field no statement writes is returned uninitialized");
    assert!(
        error.to_string().contains("uninitialized")
            && error.to_string().contains("result.birth.epoch"),
        "unexpected diagnostic: {error}"
    );
}

const RECORD_ARRAY_COLUMNS: &str = r#"
within;
package P
  record Edge
    Boolean enabled;
    Integer id;
    Real r[2];
  end Edge;
  record Graph
    Integer generation;
    Edge edges[3];
  end Graph;
  record State
    Graph graph;
    Real time;
  end State;
  function Seed
    input Integer g;
    output Graph result;
  algorithm
    result.generation := g;
    for slot in 1:3 loop
      result.edges[slot] := Edge(slot > 1, slot, {slot, 2 * slot});
    end for;
  end Seed;
  function Carry
    input Graph graph;
    input Real t;
    output State result;
  algorithm
    result.graph := graph;
    result.time := t;
  end Carry;
  function Observe
    input Integer g;
    output Real y[5];
  protected
    State s;
  algorithm
    s := Carry(Seed(g), 0.5);
    y := {s.graph.generation, s.graph.edges[2].r[2], s.graph.edges[3].id,
      if s.graph.edges[1].enabled then 1 else 0, s.time};
  end Observe;
end P;

model ObserveRecordArrayColumns
  Real y[5] = P.Observe(2);
end ObserveRecordArrayColumns;
"#;

/// A decomposed record input copied into a result field writes an array of
/// records as one column per element field; the assembly rebuilds the array
/// of records element by element from those columns.
#[test]
fn record_array_field_assembles_from_columns() {
    let compiled = Compiler::new()
        .model("ObserveRecordArrayColumns")
        .compile_str(RECORD_ARRAY_COLUMNS, "ObserveRecordArrayColumns.mo")
        .expect("an array of records written as columns assembles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the assembled record array should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    for (index, expected) in [2.0, 4.0, 3.0, 0.0, 0.5].into_iter().enumerate() {
        let name = format!("y[{}]", index + 1);
        assert_eq!(value(&probe.report, &name), expected, "{name}");
    }
}
