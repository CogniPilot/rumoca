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
