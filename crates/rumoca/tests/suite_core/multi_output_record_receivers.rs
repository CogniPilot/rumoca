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
