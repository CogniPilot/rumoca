//! Product frontend admission; no private component numerical operation.
use super::{SimOptions, compile, lower_dae_for_simulation};

fn lowered(dynamics: &str) -> rumoca_ir_solve::SolveModel {
    let source = format!(
        "model Case parameter Real M[2,2]={{{{2,0.5}},{{0.5,3}}}}; parameter Real d[2]={{0.3,0.7}}; parameter Real f[2]={{4,-2}}; Real q[2](each start=0,each fixed=true); Real v[2](start={{2,-1}},each fixed=true); equation der(q)=v; {dynamics} end Case;"
    );
    lower_dae_for_simulation(&compile(&source, "Case"), &SimOptions::default()).unwrap()
}

#[test]
fn real_affine_map_and_coupled_source_issue_one_replay_checked_relation() {
    let model = lowered("M*der(v)=f-d.*v;");
    let system = &model.problem.continuous;
    let plan = model
        .problem
        .mechanical_step
        .as_ref()
        .expect("actual source must issue a relation");
    assert_eq!(plan.cartesian().pairs(), [[0, 2], [1, 3]]);
    assert_eq!(plan.dynamics_node(), 1);
    assert!(matches!(
        system.derivative_rhs.nodes[0],
        rumoca_ir_solve::ComputeNode::Map { .. }
    ));
    assert_eq!(
        plan.coefficient_program(&system.derivative_rhs)
            .unwrap()
            .stored_output_count(),
        8
    );
    let wire = serde_json::to_vec(&model.problem).unwrap();
    let replay: rumoca_ir_solve::SolveProblem = serde_json::from_slice(&wire).unwrap();
    assert_eq!(replay.mechanical_step, model.problem.mechanical_step);
}

#[test]
fn unspecified_or_wrong_velocity_damping_declines_only_the_optional_relation() {
    for dynamics in ["M*der(v)=f;", "M*der(v)=f-d.*q;", "der(v)=f-d.*v;"] {
        assert!(
            lowered(dynamics).problem.mechanical_step.is_none(),
            "{dynamics}"
        );
    }
}
