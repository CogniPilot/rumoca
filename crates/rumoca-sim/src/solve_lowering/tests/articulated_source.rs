//! Source-to-owner proof for the same shared factor used by actual Revolute components.
use super::{SimOptions, SimSolverMode, compile, lower_dae_for_simulation};
const SOURCE: &str = r#"package K
    function articulatedJointReduction "One shared original articulated joint factor; the separate increment belongs to a numerical application"
      input Real IAc[6,6]; input Real S[6];
      input Real armature; input Real factorIncrement;
      input Real shaftEffort; input Real pAc[6]; input Real c[6];
      output Real U[6]; output Real D; output Real u;
      output Real Ia[6,6]; output Real pa[6];
    algorithm
      U := IAc*S;
      D := (S*U+armature)+factorIncrement;
      u := shaftEffort-S*pAc;
      Ia := IAc-{{U[i]*U[j] for j in 1:6} for i in 1:6}/D;
      pa := pAc+Ia*c+U*(u/D);
    end articulatedJointReduction;
end K;
model Case
 Real signal(start=0.25,fixed=true);
 output Real U[6]; output Real D; output Real u; output Real Ia[6,6]; output Real pa[6];
equation
 der(signal)=-signal;
 (U,D,u,Ia,pa)=K.articulatedJointReduction(identity(6),{1,0,0,0,0,0},1,0.005,2,{signal,0,0,0,0,0},zeros(6));
end Case;
"#;
fn calls(source: &str) -> rumoca_ir_solve::SolvePureCallTable {
    lower_dae_for_simulation(
        &compile(source, "Case"),
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .unwrap()
    .pure_calls
}
#[test]
fn original_factor_body_issues_and_current_wire_rederives_the_fact() {
    let table = calls(SOURCE);
    let count = table
        .owners()
        .iter()
        .filter(|o| o.articulated_joint_reduction().is_some())
        .count();
    assert!(count > 0);
    let wire = serde_json::to_value(&table).unwrap();
    for owner in wire["owners"].as_array().unwrap() {
        assert!(owner.get("articulated").is_none());
    }
    let replay: rumoca_ir_solve::SolvePureCallTable = serde_json::from_value(wire).unwrap();
    assert_eq!(replay, table);
    assert_eq!(
        count,
        replay
            .owners()
            .iter()
            .filter(|o| o.articulated_joint_reduction().is_some())
            .count()
    );
}
#[test]
fn changed_increment_inertia_or_outer_product_refuses_optional_fact_only() {
    for source in [
        SOURCE.replace("+factorIncrement", "-factorIncrement"),
        SOURCE.replace("U := IAc*S", "U := transpose(IAc)*S"),
        SOURCE.replace("U[i]*U[j]", "U[i]*U[i]"),
        SOURCE.replace("pa := pAc+Ia*c+U*(u/D)", "pa := pAc+Ia*c"),
    ] {
        assert_ne!(source, SOURCE);
        let table = calls(&source);
        assert!(
            table
                .owners()
                .iter()
                .all(|o| o.articulated_joint_reduction().is_none())
        );
    }
}
#[test]
fn spellings_do_not_establish_the_mechanical_relation() {
    let source = SOURCE.replace("articulatedJointReduction", "ordinaryFunction");
    assert!(
        calls(&source)
            .owners()
            .iter()
            .any(|o| o.articulated_joint_reduction().is_some())
    );
}
#[test]
fn a_wire_flag_cannot_admit_a_different_factor_body() {
    let table = calls(&SOURCE.replace("+factorIncrement", "-factorIncrement"));
    let mut wire = serde_json::to_value(&table).unwrap();
    for owner in wire["owners"].as_array_mut().unwrap() {
        owner["articulated"] = true.into();
    }
    if let Ok(replay) = serde_json::from_value::<rumoca_ir_solve::SolvePureCallTable>(wire) {
        assert!(
            replay
                .owners()
                .iter()
                .all(|o| o.articulated_joint_reduction().is_none())
        );
    }
}
