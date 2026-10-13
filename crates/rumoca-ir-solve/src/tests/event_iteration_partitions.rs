use super::*;

fn fixture(guarded: bool) -> SolveProblem {
    let mut problem = event_iteration_contract_fixture();
    problem.layout = VarLayout::from_parts(IndexMap::new(), 0, 6);
    problem.solve_layout.variable_storage_runs[0].scalar_count = 3;
    problem.solve_layout.compiled_parameter_len = 6;
    problem.solve_layout.discrete_real_scalar_names =
        vec!["z[1]".into(), "z[2]".into(), "z[3]".into()];
    problem.solve_layout.pre_param_bindings = (0..3)
        .map(|index| PreParamBinding {
            dest_p_index: 3 + index,
            source: PreParamSource::P { index },
            clock_schedule: None,
        })
        .collect();
    let scalar_rows = if guarded { 1 } else { 3 };
    problem.discrete.rhs = ScalarProgramBlock::with_source_span(
        vec![guarded_range_program(scalar_rows)],
        fixture_provenance(),
    )
    .unwrap();
    problem.discrete.update_targets = (0..scalar_rows).map(scalar_slot_p).collect();
    problem.discrete.row_roles = vec![DiscreteRowRole::Equation; scalar_rows];
    problem.discrete.pre_modes = vec![DiscreteEventPreMode::FollowCurrent; scalar_rows];
    problem.discrete.observation_refresh = vec![false; scalar_rows];
    problem.discrete.integrator_history_effects =
        vec![IntegratorHistoryEffect::Preserve; scalar_rows];
    problem.discrete.clock_owners = vec![None; scalar_rows];
    let second_owner = if guarded {
        problem.discrete.guarded_assignments = vec![
            GuardedAssignmentProgram::checked(
                guarded_range_program(2),
                fixture_provenance(),
                [(scalar_slot_p(1), 2)],
                DiscreteRowRole::Equation,
                DiscreteEventPreMode::Fixed,
                false,
                IntegratorHistoryEffect::Preserve,
                None,
            )
            .unwrap(),
        ];
        EventIterationOwner::GuardedAssignment {
            program_index: 0,
            target_range_index: 0,
        }
    } else {
        EventIterationOwner::ScalarRows { start_row: 1 }
    };
    problem.discrete.event_iteration_plan = EventIterationPlan::new_partitioned(
        vec![EventIterationRun {
            variable: 0,
            pre_binding_start: 0,
            owner: EventIterationOwner::Partitioned {
                first_partition: 0,
                partition_count: 2,
            },
        }],
        vec![
            EventIterationPartition {
                scalar_start: 0,
                scalar_count: 1,
                owner: EventIterationOwner::ScalarRows { start_row: 0 },
            },
            EventIterationPartition {
                scalar_start: 1,
                scalar_count: 2,
                owner: second_owner,
            },
        ],
    );
    problem
}

fn boolean_fixture() -> SolveProblem {
    let mut problem = fixture(true);
    let storage = &mut problem.solve_layout.variable_storage_runs[0];
    storage.role = SolveVariableStorageRole::DiscreteValue;
    storage.value_kind = SolveVariableValueKind::Boolean;
    problem.solve_layout.variable_declarations[0] = SolveVariableDeclaration::new(
        SolveVariableStorageRole::DiscreteValue,
        SolveVariableValueKind::Boolean,
    );
    problem.solve_layout.discrete_valued_scalar_names =
        std::mem::take(&mut problem.solve_layout.discrete_real_scalar_names);
    problem
}

#[test]
fn one_declaration_admits_exact_mixed_scalar_and_guarded_partitions() {
    for problem in [fixture(false), fixture(true), boolean_fixture()] {
        problem.validate_shape_contract().unwrap();
        assert_eq!(problem.solve_layout.variable_storage_runs.len(), 1);
        assert_eq!(
            problem
                .discrete
                .event_iteration_run_clock(&problem.discrete.event_iteration_plan.runs[0]),
            Ok(None)
        );
        let json = serde_json::to_string(&problem).unwrap();
        let decoded: SolveProblem = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
    }
}

#[test]
fn a_boolean_partition_cannot_replace_its_producer_with_generic_hold() {
    let mut problem = boolean_fixture();
    problem.discrete.event_iteration_plan.partitions[0].owner = EventIterationOwner::Hold;
    let error = problem.validate_shape_contract().unwrap_err();
    assert!(
        error
            .to_string()
            .contains("only ordinary discrete Real runs may hold"),
        "{error}"
    );
}

#[test]
fn current_solve_wire_requires_the_partition_table() {
    let mut wire = serde_json::to_value(boolean_fixture()).unwrap();
    wire["discrete"]["event_iteration_plan"]
        .as_object_mut()
        .unwrap()
        .remove("partitions");
    assert!(serde_json::from_value::<SolveProblem>(wire).is_err());
}

#[derive(Clone, Copy)]
enum InvalidPartition {
    EmptyOwner,
    EmptySlice,
    OutOfBoundsTable,
    OverflowTable,
    InitialHole,
    Overlap,
    TrailingHole,
    OutOfBoundsSlice,
    OverflowSlice,
    Nested,
    Unclaimed,
    WrongRows,
    Reordered,
}

fn forge_partition(problem: &mut SolveProblem, invalid: InvalidPartition) {
    let plan = &mut problem.discrete.event_iteration_plan;
    match invalid {
        InvalidPartition::EmptyOwner => {
            plan.runs[0].owner = EventIterationOwner::Partitioned {
                first_partition: 0,
                partition_count: 0,
            }
        }
        InvalidPartition::EmptySlice => plan.partitions[0].scalar_count = 0,
        InvalidPartition::OutOfBoundsTable => {
            plan.runs[0].owner = EventIterationOwner::Partitioned {
                first_partition: 1,
                partition_count: 2,
            }
        }
        InvalidPartition::OverflowTable => {
            plan.runs[0].owner = EventIterationOwner::Partitioned {
                first_partition: usize::MAX,
                partition_count: 2,
            }
        }
        InvalidPartition::InitialHole => plan.partitions[0].scalar_start = 1,
        InvalidPartition::Overlap => plan.partitions[1].scalar_start = 0,
        InvalidPartition::TrailingHole => plan.partitions[1].scalar_count = 1,
        InvalidPartition::OutOfBoundsSlice => plan.partitions[1].scalar_count = 3,
        InvalidPartition::OverflowSlice => plan.partitions[1].scalar_count = usize::MAX,
        InvalidPartition::Nested => plan.partitions[0].owner = plan.runs[0].owner,
        InvalidPartition::Unclaimed => plan.partitions.push(plan.partitions[0]),
        InvalidPartition::WrongRows => {
            plan.partitions[1].owner = EventIterationOwner::ScalarRows { start_row: 0 }
        }
        InvalidPartition::Reordered => plan.partitions.swap(0, 1),
    }
}

#[test]
fn malformed_partitions_fail_construction_and_checked_wire_replay() {
    for invalid in [
        InvalidPartition::EmptyOwner,
        InvalidPartition::EmptySlice,
        InvalidPartition::OutOfBoundsTable,
        InvalidPartition::OverflowTable,
        InvalidPartition::InitialHole,
        InvalidPartition::Overlap,
        InvalidPartition::TrailingHole,
        InvalidPartition::OutOfBoundsSlice,
        InvalidPartition::OverflowSlice,
        InvalidPartition::Nested,
        InvalidPartition::Unclaimed,
        InvalidPartition::WrongRows,
        InvalidPartition::Reordered,
    ] {
        let mut problem = fixture(false);
        forge_partition(&mut problem, invalid);
        assert!(problem.validate_shape_contract().is_err());
        let forged = serde_json::to_value(&problem).unwrap();
        assert!(serde_json::from_value::<SolveProblem>(forged).is_err());
    }
}

#[test]
fn guarded_partition_must_match_the_exact_sliced_storage_range() {
    let mut problem = fixture(true);
    problem.discrete.guarded_assignments[0] = GuardedAssignmentProgram::checked(
        guarded_range_program(2),
        fixture_provenance(),
        [(scalar_slot_p(0), 2)],
        DiscreteRowRole::Equation,
        DiscreteEventPreMode::Fixed,
        false,
        IntegratorHistoryEffect::Preserve,
        None,
    )
    .unwrap();
    let error = problem.validate_shape_contract().unwrap_err();
    assert!(
        error
            .to_string()
            .contains("guarded target range does not define the run"),
        "{error}"
    );
}

#[test]
fn partition_clock_query_checks_every_leaf_and_rejects_nesting() {
    let mut problem = fixture(false);
    problem
        .clocks
        .periodic_event_schedules
        .push(PeriodicEventSchedule::default());
    problem.discrete.clock_owners[1] = problem.clocks.periodic_clock_id(0);
    problem.discrete.clock_owners[2] = problem.clocks.periodic_clock_id(0);
    let run = problem.discrete.event_iteration_plan.runs[0];
    assert!(problem.discrete.event_iteration_run_clock(&run).is_err());
    let error = validate_event_iteration_plan(&problem).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("partition owners disagree on their typed clock"),
        "{error}"
    );
    problem.discrete.clock_owners[1] = None;
    problem.discrete.clock_owners[2] = None;
    problem.discrete.event_iteration_plan.partitions[1].owner = run.owner;
    assert!(problem.discrete.event_iteration_run_clock(&run).is_err());
}

#[test]
fn stateless_native_schedule_retains_every_scalar_partition_output() {
    let mut problem = fixture(false);
    let storage = &mut problem.solve_layout.variable_storage_runs[0];
    storage.role = SolveVariableStorageRole::DiscreteValue;
    storage.value_kind = SolveVariableValueKind::Boolean;
    problem.solve_layout.variable_declarations[0] = SolveVariableDeclaration::new(
        SolveVariableStorageRole::DiscreteValue,
        SolveVariableValueKind::Boolean,
    );
    problem.solve_layout.discrete_valued_scalar_names =
        std::mem::take(&mut problem.solve_layout.discrete_real_scalar_names);
    problem.discrete.rhs = ScalarProgramBlock::with_source_span(
        [1.0, 0.0, 1.0]
            .into_iter()
            .map(|value| {
                vec![
                    LinearOp::Const { dst: 0, value },
                    LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect(),
        fixture_provenance(),
    )
    .unwrap();
    problem.validate_shape_contract().unwrap();
    issue_native_assignment_schedule(&mut problem);
    let schedule = problem
        .continuous
        .refresh_owners
        .native_assignment_schedule()
        .expect("partitioned scalar ownership preserves the stateless native profile");
    assert_eq!(schedule.derived_outputs().len(), 3);
    assert_eq!(
        schedule
            .derived_outputs()
            .iter()
            .map(NativeDerivedOutput::p_index)
            .collect::<Vec<_>>(),
        [0, 1, 2]
    );
    assert!(
        schedule
            .derived_outputs()
            .iter()
            .all(|output| output.lane() == NativeOutputLane::Boolean)
    );
}
