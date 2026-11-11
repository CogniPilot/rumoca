//! Real source-to-typed-owner and complete geometric state classification controls.
use super::{SimOptions, SimSolverMode, compile, lower_dae_for_simulation};

const SOURCE: &str = r#"package K
      function rhs
        input Real q[4]; input Real w[3]; output Real dq[4];
      algorithm
        dq[1] := 0.5 * (-q[2]*w[1]-q[3]*w[2]-q[4]*w[3]);
        dq[2] := 0.5 * ( q[1]*w[1]-q[4]*w[2]+q[3]*w[3]);
        dq[3] := 0.5 * ( q[4]*w[1]+q[1]*w[2]-q[2]*w[3]);
        dq[4] := 0.5 * (-q[3]*w[1]+q[2]*w[2]+q[1]*w[3]);
      end rhs;
    end K;
    model Case
      Real q[4](start={1,0,0,0}, each fixed=true);
      Real w[3](start={0.1,0.2,0.3}, each fixed=true);
      Real r[3](each start=0,each fixed=true);
      Real v[3](each start=0,each fixed=true);
      Real auxiliary[17](each start=300,each fixed=true);
    equation
      der(q)=K.rhs(q,w); der(w)=zeros(3);
      der(r)=v; der(v)={0,0,-9.81}; der(auxiliary)=-0.01*auxiliary;
    end Case;"#;

#[test]
fn quaternion_source_has_exact_owner_and_complete_state_roles() {
    let source = SOURCE;
    let model = lower_dae_for_simulation(
        &compile(source, "Case"),
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .unwrap();
    let calls = &model.pure_calls;
    let owner = &calls.owners()[0];
    assert!(owner.body_quaternion_kinematics().is_some());
    let replay: rumoca_ir_solve::SolvePureCallTable =
        serde_json::from_str(&serde_json::to_string(calls).unwrap()).unwrap();
    assert!(replay.owners()[0].body_quaternion_kinematics().is_some());
    assert!(replay.matches_site(&owner.call_site()));
    let block = &model.problem.continuous.derivative_rhs;
    let plan = rumoca_ir_solve::GeometricStepPlan::from_derivative(block, 30, calls).unwrap();
    assert!(model.problem.continuous.cartesian_pairs.is_none());
    assert!(model.problem.continuous.mechanical_step.is_none());
    assert_eq!(plan.quaternions().len(), 1);
    assert_eq!(plan.cartesian_pairs().len(), 3);
    assert_eq!(plan.auxiliary().len(), 17);
    let mut coverage: Vec<usize> = Vec::new();
    coverage.extend(plan.quaternions()[0].quaternion());
    coverage.extend(plan.quaternions()[0].angular_velocity());
    for pair in plan.cartesian_pairs() {
        coverage.extend(pair);
    }
    coverage.extend(plan.auxiliary());
    coverage.sort();
    assert_eq!(coverage, (0..30).collect::<Vec<usize>>());
}

#[test]
fn wrong_hamilton_scale_and_state_bindings_never_issue_the_profile() {
    for source in [
        SOURCE.replace("0.5 *", "0.25 *"),
        SOURCE.replace(
            "q[1]*w[1]-q[4]*w[2]+q[3]*w[3]",
            "q[1]*w[1]+q[4]*w[2]-q[3]*w[3]",
        ),
        SOURCE.replace(
            "0.5 * (-q[2]*w[1]-q[3]*w[2]-q[4]*w[3])",
            "(0.5 * (-q[2]*w[1]-q[3]*w[2]-q[4]*w[3])+1e20)-1e20",
        ),
        SOURCE.replace("der(q)=K.rhs(q,w)", "der(q)=K.rhs({q[2],q[1],q[3],q[4]},w)"),
        SOURCE.replace("der(q)=K.rhs(q,w)", "der(q)=K.rhs(2*q,w)"),
        SOURCE.replace("der(q)=K.rhs(q,w)", "der(q)=K.rhs(q,{q[1],q[2],q[3]})"),
    ] {
        let model = lower_dae_for_simulation(
            &compile(&source, "Case"),
            &SimOptions {
                solver_mode: SimSolverMode::RkLike,
                ..Default::default()
            },
        )
        .unwrap();
        assert!(
            rumoca_ir_solve::GeometricStepPlan::from_derivative(
                &model.problem.continuous.derivative_rhs,
                30,
                &model.pure_calls
            )
            .is_none()
        );
        assert_eq!(
            model.initial_y.len(),
            30,
            "ordinary RK model remains usable"
        );
    }
}

#[test]
fn current_wire_rederives_the_body_fact_instead_of_replaying_a_flag() {
    let model = lower_dae_for_simulation(
        &compile(SOURCE, "Case"),
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .unwrap();
    let wire = serde_json::to_string(&model.pure_calls).unwrap();
    assert!(!wire.contains("quaternion"));
    let half = 0.5f64.to_bits().to_string();
    assert!(wire.contains(&half));
    let wrong = wire.replace(&half, &0.25f64.to_bits().to_string());
    let replay: rumoca_ir_solve::SolvePureCallTable = serde_json::from_str(&wrong).unwrap();
    assert!(replay.owners()[0].body_quaternion_kinematics().is_none());
    assert!(!replay.matches_site(&model.pure_calls.owners()[0].call_site()));
    assert!(
        rumoca_ir_solve::GeometricStepPlan::from_derivative(
            &model.problem.continuous.derivative_rhs,
            30,
            &replay
        )
        .is_none()
    );
    assert!(
        rumoca_ir_solve::GeometricStepPlan::from_derivative(
            &model.problem.continuous.derivative_rhs,
            30,
            &Default::default()
        )
        .is_none()
    );
}
