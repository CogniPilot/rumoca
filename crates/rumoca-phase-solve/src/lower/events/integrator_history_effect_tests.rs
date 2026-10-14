use super::*;

fn provenance() -> rumoca_core::ProvenanceSpan {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("integrator_history_effect.mo"),
        0,
        1,
    )
    .require_provenance("integrator history effect fixture")
    .unwrap()
}

fn load_program(slot: solve::ScalarSlot) -> Vec<solve::LinearOp> {
    let load = match slot {
        solve::ScalarSlot::Y { index, .. } => solve::LinearOp::LoadY { dst: 0, index },
        solve::ScalarSlot::P { index, .. } => solve::LinearOp::LoadP { dst: 0, index },
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => unreachable!(),
    };
    vec![load, solve::LinearOp::StoreOutput { src: 0 }]
}

fn scalar_block(programs: Vec<Vec<solve::LinearOp>>) -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_source_span(programs, provenance()).unwrap()
}

fn compute_block(programs: Vec<Vec<solve::LinearOp>>) -> solve::ComputeBlock {
    solve::ComputeBlock::from_scalar_program_block(scalar_block(programs))
}

fn discrete_with_targets(targets: Vec<solve::ScalarSlot>) -> solve::DiscreteSolveSystem {
    let row_count = targets.len();
    solve::DiscreteSolveSystem {
        rhs: scalar_block(
            targets
                .iter()
                .map(|_| {
                    vec![
                        solve::LinearOp::Const { dst: 0, value: 0.0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]
                })
                .collect(),
        ),
        update_targets: targets,
        row_roles: vec![solve::DiscreteRowRole::Equation; row_count],
        pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent; row_count],
        observation_refresh: vec![false; row_count],
        integrator_history_effects: vec![solve::IntegratorHistoryEffect::Restart; row_count],
        clock_owners: vec![None; row_count],
        ..solve::DiscreteSolveSystem::default()
    }
}

fn derive_integrator_history_effects(
    discrete: &mut solve::DiscreteSolveSystem,
    continuous: &solve::ContinuousSolveSystem,
    state_scalar_count: usize,
) {
    let Some(sensitive) = integrator_history_sensitive_slots(
        continuous,
        &discrete.runtime_assignment_rhs,
        &discrete.runtime_assignment_targets,
        state_scalar_count,
    ) else {
        return;
    };
    apply_integrator_history_effects(discrete, &sensitive, state_scalar_count);
}

#[test]
fn direct_continuous_dependencies_and_state_targets_restart() {
    let continuous = solve::ContinuousSolveSystem {
        derivative_rhs: compute_block(vec![load_program(solve::scalar_slot_p(0))]),
        ..Default::default()
    };
    let mut discrete = discrete_with_targets(vec![
        solve::scalar_slot_p(0),
        solve::scalar_slot_p(1),
        solve::scalar_slot_y(0),
    ]);

    derive_integrator_history_effects(&mut discrete, &continuous, 1);

    assert_eq!(
        discrete.integrator_history_effects,
        [
            solve::IntegratorHistoryEffect::Restart,
            solve::IntegratorHistoryEffect::Preserve,
            solve::IntegratorHistoryEffect::Restart,
        ]
    );
}

#[test]
fn runtime_assignment_dependencies_propagate_transitively() {
    let continuous = solve::ContinuousSolveSystem {
        derivative_rhs: compute_block(vec![load_program(solve::scalar_slot_p(0))]),
        ..Default::default()
    };
    let mut discrete = discrete_with_targets(vec![solve::scalar_slot_p(1)]);
    discrete.runtime_assignment_rhs = scalar_block(vec![load_program(solve::scalar_slot_p(1))]);
    discrete.runtime_assignment_targets = vec![solve::scalar_slot_p(0)];

    derive_integrator_history_effects(&mut discrete, &continuous, 0);

    assert_eq!(
        discrete.integrator_history_effects,
        [solve::IntegratorHistoryEffect::Restart]
    );
}

#[test]
fn disconnected_runtime_assignment_cycles_fail_closed() {
    let continuous = solve::ContinuousSolveSystem::default();
    let mut discrete =
        discrete_with_targets(vec![solve::scalar_slot_p(2), solve::scalar_slot_p(4)]);
    discrete.runtime_assignment_rhs = scalar_block(vec![
        load_program(solve::scalar_slot_p(3)),
        load_program(solve::scalar_slot_p(2)),
    ]);
    discrete.runtime_assignment_targets = vec![solve::scalar_slot_p(2), solve::scalar_slot_p(3)];

    derive_integrator_history_effects(&mut discrete, &continuous, 0);

    assert_eq!(
        discrete.integrator_history_effects,
        [
            solve::IntegratorHistoryEffect::Restart,
            solve::IntegratorHistoryEffect::Preserve,
        ]
    );
}
