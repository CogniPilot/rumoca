use super::*;

fn two_ranges() -> solve::SolveModel {
    let mut model = solve::SolveModel::default();
    for (current, destination) in [(0, 8), (4, 6)] {
        let variable = model.problem.solve_layout.variable_storage_runs.len();
        let pre_binding_start = model.problem.solve_layout.pre_param_bindings.len();
        model
            .problem
            .solve_layout
            .variable_storage_runs
            .push(solve::SolveVariableStorageRun {
                base: solve::scalar_slot_p(current),
                scalar_count: 2,
                role: solve::SolveVariableStorageRole::DiscreteReal,
                value_kind: solve::SolveVariableValueKind::Real,
            });
        for offset in 0..2 {
            model
                .problem
                .solve_layout
                .pre_param_bindings
                .push(solve::PreParamBinding {
                    dest_p_index: destination + offset,
                    source: solve::PreParamSource::P {
                        index: current + offset,
                    },
                    clock_schedule: None,
                });
        }
        model.problem.discrete.clock_owners.push(None);
        model
            .problem
            .discrete
            .event_iteration_plan
            .runs
            .push(solve::EventIterationRun {
                variable,
                pre_binding_start,
                owner: solve::EventIterationOwner::ScalarRows {
                    start_row: variable,
                },
            });
    }
    model
}

#[test]
fn source_order_can_differ_from_disjoint_destination_order() {
    let model = two_ranges();
    let source = [2.0, 3.0, 0.0, 0.0, 4.0, 5.0];
    let mut params = [1.0; 10];
    assert!(advance(&model, &[], &source, &mut params).unwrap());
    assert_eq!(params, [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 4.0, 5.0, 2.0, 3.0]);
    assert!(!advance(&model, &[], &source, &mut params).unwrap());
}

#[test]
fn invalid_tail_and_short_source_reject_without_partial_publication() {
    let model = two_ranges();
    for source in [&[2.0, 3.0, 0.0, 0.0, 4.0, f64::NAN][..], &[2.0, 3.0][..]] {
        let mut params = [1.0; 10];
        assert!(advance(&model, &[], source, &mut params).is_err());
        assert_eq!(params, [1.0; 10]);
    }
}

#[test]
fn entry_preflight_rejects_invalid_tail_before_copying() {
    let model = two_ranges();
    let mut params = [1.0; 10];
    assert!(seed_event_entry_pre_params(&model, &[], &[2.0, 3.0], &mut params).is_err());
    assert_eq!(params, [1.0; 10]);
}
