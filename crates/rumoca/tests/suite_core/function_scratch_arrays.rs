//! MLS 3.7 sections 11.2.2, 11.2.6 and 12.4.4: a protected scratch array that
//! a loop fills element by element is defined for the reads that follow it,
//! on each outer iteration and under the selection that guards it.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const SOURCE: &str = r#"
function RepeatedScratch
  input Real values[:];
  output Real result[size(values,1)];
protected
  Real scratch[size(values,1)];
algorithm
  for i in 1:size(values,1) loop
    for j in 1:size(values,1) loop
      scratch[j] := values[j]-values[i];
    end for;
    result[i] := sum(scratch);
  end for;
end RepeatedScratch;

function ConditionalRepeatedScratch
  input Real values[:];
  input Boolean enabled;
  output Real result[size(values,1)];
protected
  Real scratch[size(values,1)];
algorithm
  result := zeros(size(values,1));
  if enabled then
    for i in 1:size(values,1) loop
      for j in 1:size(values,1) loop
        scratch[j] := values[j]-values[i];
      end for;
      if values[i] > 0 then
        result[i] := sum(scratch);
      end if;
    end for;
  end if;
end ConditionalRepeatedScratch;

function ConditionalScratch
  input Real values[:];
  input Boolean enabled;
  output Real result[size(values,1)];
protected
  Real scratch[size(values,1)];
  Real center;
algorithm
  result := zeros(size(values,1));
  if enabled then
    for i in 1:size(values,1) loop
      scratch[i] := 2*values[i];
    end for;
    for i in 1:size(values,1) loop
      center := scratch[i];
      if center > 0 then
        result[i] := center;
      end if;
    end for;
  end if;
end ConditionalScratch;

model Repeated
  Real result[3] = RepeatedScratch({-1, 2, 3});
end Repeated;

model ConditionalRepeated
  Real result[3] = ConditionalRepeatedScratch({-1, 2, 3}, time < 0.5);
end ConditionalRepeated;

model Conditional
  Real result[3] = ConditionalScratch({-1, 2, 3}, time < 0.5);
end Conditional;
"#;

fn result_at(model: &str, time: f64) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], time)
        .unwrap_or_else(|error| panic!("{model} evaluates: {error:?}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    (1..=3)
        .map(|index| {
            let name = format!("result[{index}]");
            probe
                .report
                .solver_y
                .iter()
                .find(|slot| slot.name.replace(' ', "") == name)
                .unwrap_or_else(|| panic!("solver value {name}"))
                .value
        })
        .collect()
}

#[test]
fn an_inner_loop_redefines_the_scratch_on_each_outer_iteration() {
    assert_eq!(result_at("Repeated", 0.0), [7.0, -2.0, -5.0]);
}

#[test]
fn a_selected_outer_loop_keeps_the_inner_scratch_definitions() {
    assert_eq!(result_at("ConditionalRepeated", 0.0), [0.0, -2.0, -5.0]);
    assert_eq!(result_at("ConditionalRepeated", 1.0), [0.0; 3]);
}

#[test]
fn a_selected_sequence_reads_the_scratch_its_earlier_loop_filled() {
    assert_eq!(result_at("Conditional", 0.0), [0.0, 4.0, 6.0]);
    assert_eq!(result_at("Conditional", 1.0), [0.0; 3]);
}

const COLUMN_COPY: &str = r#"
package C
  record Edge
    Boolean enabled;
    Real w;
  end Edge;
  record State
    Integer generation;
    Edge edges[2];
  end State;
  function F
    input Real u;
    input Boolean flag;
    output Real y;
  protected
    State a;
    State b;
  algorithm
    a.generation := 1;
    for i in 1:2 loop
      a.edges[i].enabled := i == 1;
      a.edges[i].w := u * i;
    end for;
    b.generation := 2;
    b.edges := a.edges;
    if flag then
      b.edges[2].w := 100;
    end if;
    y := b.edges[1].w + b.edges[2].w + b.generation;
  end F;
end C;
model Copy
  parameter Real u = 3;
  Real y = C.F(u, time < 0.5);
  Real z = C.F(u, false);
end Copy;
"#;

/// A record local whose array-of-records field is copied whole into another
/// record is one copy per leaf column; later element writes update the copy
/// only.
#[test]
fn a_record_array_field_is_copied_by_column() {
    let compiled = Compiler::new()
        .model("Copy")
        .compile_str(COLUMN_COPY, "Copy.mo")
        .expect("the record-array copy compiles");
    for (time, y) in [(0.0, 105.0), (1.0, 11.0)] {
        let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], time)
            .expect("the copy evaluates");
        let value = |name: &str| {
            probe
                .report
                .solver_y
                .iter()
                .find(|slot| slot.name == name)
                .unwrap_or_else(|| panic!("solver value {name}"))
                .value
        };
        assert_eq!(value("y"), y, "y at t={time}");
        assert_eq!(value("z"), 11.0, "z at t={time}");
    }
}

const FILLED_EDGES: &str = r#"
package G
  constant Integer cap = 3;
  record Edge
    Integer id;
    Real rotation[2, 2];
    Real translation[2];
  end Edge;
  record State
    Integer generation;
    Edge edges[cap];
  end State;
  function EmptyEdge
    input Real scale;
    output Edge result;
  algorithm
    result.id := 0;
    result.rotation := scale * identity(2);
    result.translation := {scale, 2 * scale};
  end EmptyEdge;
  function Empty
    input Real scale;
    output State result;
  protected
    Edge empty;
  algorithm
    result.generation := 1;
    empty := EmptyEdge(scale);
    for slot in 1:cap loop result.edges[slot] := empty; end for;
  end Empty;
end G;
model Filled
  parameter Real scale = 3;
  output G.State s = G.Empty(scale);
end Filled;
"#;

/// A record with array fields written whole into every element of a
/// record-array field repeats each field over the elements.
#[test]
fn a_record_with_array_fields_fills_a_record_array_field() {
    let compiled = Compiler::new()
        .model("Filled")
        .compile_str(FILLED_EDGES, "Filled.mo")
        .expect("the filled record array compiles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the filled record array evaluates");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let reals = probe
        .report
        .solver_y
        .iter()
        .map(|slot| slot.value)
        .collect::<Vec<_>>();
    // Per element: rotation (2 x 2) and translation (2); the layout orders the
    // columns by field, each over the three elements.
    assert_eq!(reals.len(), 3 * 4 + 3 * 2);
    assert_eq!(
        reals.iter().filter(|value| **value == 3.0).count(),
        3 * 2 + 3
    );
    assert_eq!(reals.iter().filter(|value| **value == 6.0).count(), 3);
    assert_eq!(reals.iter().filter(|value| **value == 0.0).count(), 3 * 2);
}
