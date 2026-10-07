//! MLS 3.7 sections 12.4.3 and 8.3.1: a whole record may receive one
//! record-valued result of a multi-result equation, each leaf coordinate of the
//! record reading its field projection of that result, whatever system (continuous
//! or discrete) owns the leaf.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const RECORD_RECEIVER: &str = r#"
package P
  record Pose
    Real p[2];
    Integer id;
  end Pose;
  record State
    Integer generation;
    Real x[2];
    Pose pose;
  end State;
  function F
    input Real u;
    output State next;
    output Boolean accepted;
    output Real score;
  algorithm
    next.generation := 3;
    next.x := {u, 2 * u};
    next.pose.p := {u + 1, 4};
    next.pose.id := 9;
    accepted := u > 0;
    score := u + 1;
  end F;
end P;
model Receives
  parameter Real u = 1;
  P.State next;
  Boolean accepted;
  Real score;
  Real y = next.x[2] + next.pose.p[1] + score;
equation
  (next, accepted, score) = P.F(u);
end Receives;
model Mismatched
  parameter Real u = 1;
  P.Pose next;
  Boolean accepted;
  Real score;
equation
  (next, accepted, score) = P.F(u);
end Mismatched;
"#;

#[test]
fn a_whole_record_receives_a_record_valued_result() {
    let compiled = Compiler::new()
        .model("Receives")
        .compile_str(RECORD_RECEIVER, "Receives.mo")
        .expect("a whole record receives one result of a multi-result equation");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the record receiver DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let y = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == "y")
        .expect("solver value y")
        .value;
    // next.x[2] = 2, next.pose.p[1] = 2, score = 2.
    assert_eq!(y, 6.0);
}

#[test]
fn a_record_of_another_type_cannot_receive_the_result() {
    let error = Compiler::new()
        .model("Mismatched")
        .compile_str(RECORD_RECEIVER, "Mismatched.mo")
        .expect_err("the receiving record has another type than the result");
    assert!(
        error
            .to_string()
            .contains("distinct resolved type identities"),
        "{error}"
    );
}

const RECORD_ARRAY_RECEIVER: &str = r#"
package Q
  record Edge
    Boolean enabled;
    Real w[2];
  end Edge;
  record State
    Integer generation;
    Edge edges[2];
    Edge grid[2, 2];
  end State;
  function F
    input Real u;
    output State next;
    output Real score;
  algorithm
    next.generation := 3;
    for i in 1:2 loop
      next.edges[i].enabled := i == 1;
      next.edges[i].w := {u * i, 10 * i};
      for j in 1:2 loop
        next.grid[i, j].enabled := j == 2;
        next.grid[i, j].w := {100 * i + j, u};
      end for;
    end for;
    score := u + 1;
  end F;
end Q;
model ReceivesArray
  parameter Real u = 1;
  Q.State next;
  Real score;
  Real y = next.edges[1].w[1] + next.edges[2].w[2] + next.grid[2, 1].w[1] + score;
equation
  (next, score) = Q.F(u);
end ReceivesArray;
"#;

/// A record whose field is an array of records receives a record-valued result
/// element by element: Flat holds `next.edges[1]` and `next.edges[2]` as
/// separate instances, so the projection indexes the field, then selects.
#[test]
fn a_record_with_a_record_array_field_receives_a_record_valued_result() {
    let compiled = Compiler::new()
        .model("ReceivesArray")
        .compile_str(RECORD_ARRAY_RECEIVER, "ReceivesArray.mo")
        .expect("a record with an array-of-records field receives the result");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the record receiver DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let value = |name: &str| {
        probe
            .report
            .solver_y
            .iter()
            .find(|slot| slot.name.replace(' ', "") == name)
            .unwrap_or_else(|| panic!("solver value {name}"))
            .value
    };
    assert_eq!(value("next.edges[2].w[1]"), 2.0);
    assert_eq!(value("next.edges[2].w[2]"), 20.0);
    assert_eq!(value("next.grid[2,1].w[1]"), 201.0);
    // 1 + 20 + 201 + 2
    assert_eq!(value("y"), 224.0);
}

/// MLS 10.5.3: an update with fewer subscripts than axes replaces the
/// sub-array the leading indices select.
#[test]
fn a_partial_index_update_replaces_the_selected_sub_array() {
    let source = r#"
function Rows
  input Real u;
  output Real a[2, 2];
algorithm
  a := zeros(2, 2);
  a[1] := {u, 1};
  a[2] := {2 * u, 2};
end Rows;
model Partial
  parameter Real u = 3;
  Real a[2, 2] = Rows(u);
end Partial;
"#;
    let compiled = Compiler::new()
        .model("Partial")
        .compile_str(source, "Partial.mo")
        .expect("a partial index update compiles");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the partial update evaluates");
    let values = probe
        .report
        .solver_y
        .iter()
        .map(|slot| slot.value)
        .collect::<Vec<_>>();
    assert_eq!(values, [3.0, 1.0, 6.0, 2.0]);
}
