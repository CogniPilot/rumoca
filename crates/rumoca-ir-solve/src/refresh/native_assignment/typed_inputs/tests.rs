use super::*;

fn inputs(count: usize) -> TypedInputs {
    let problem = SolveProblem {
        layout: crate::VarLayout::from_parts(Default::default(), 0, count),
        solve_layout: crate::SolveLayout {
            variable_storage_runs: vec![crate::SolveVariableStorageRun {
                base: crate::scalar_slot_p(0),
                scalar_count: count,
                role: SolveVariableStorageRole::ExternalInput,
                value_kind: SolveVariableValueKind::Integer,
            }],
            ..Default::default()
        },
        ..Default::default()
    };
    TypedInputs::classify(&problem).unwrap()
}

/// Preserve the original scalar checker's Integer/Real sibling control.
#[test]
fn range_binding_keeps_unbound_real_views_checked() {
    for (integer_outputs, expected) in [(1, true), (2, false)] {
        let mut inputs = inputs(1);
        let mut flow = StageFlow {
            inputs: &mut inputs,
            sinks: (0..integer_outputs)
                .map(|ordinal| (ordinal, NativeIntegerSource::Input { lane_offset: 0 }))
                .collect(),
            stored_output: 0,
            bindings: Default::default(),
        };
        flow.program(&[
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ])
        .unwrap();
        assert_eq!(inputs.lane_at(0).unwrap().checked(), expected);
    }
}

#[test]
fn source_storage_and_sparse_real_reads_stay_compact() {
    for count in [8, 65536] {
        let mut inputs = inputs(count);
        assert_eq!(inputs.runs.len(), 1);
        assert_eq!(inputs.lane_bytes, count * 8);
        let mut flow = StageFlow {
            inputs: &mut inputs,
            sinks: BTreeMap::new(),
            stored_output: 0,
            bindings: Default::default(),
        };
        flow.program(&[
            LinearOp::TensorLoad {
                dst_start: 0,
                input: crate::TensorInputKind::P,
                input_start: 0,
                count,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count: count / 2,
                stride: 2,
            },
        ])
        .unwrap();
        assert_eq!(inputs.checks.len(), 1);
        assert_eq!(inputs.checks[0].selected().stride(), 2);
        for p in [0, count / 2, count - 2] {
            let lane = inputs.lane_at(p).unwrap();
            assert_eq!(
                (lane.p_index(), lane.lane_offset(), lane.checked()),
                (p, p * 8, true)
            );
            assert!(!inputs.lane_at(p + 1).unwrap().checked());
        }
        assert!(inputs.lane_at(count).is_none());
    }
}

#[test]
fn overwritten_input_view_splits_only_its_original_source_pieces() {
    for count in [8, 65536] {
        let mut inputs = inputs(count);
        let middle = count / 2;
        let mut flow = StageFlow {
            inputs: &mut inputs,
            sinks: BTreeMap::new(),
            stored_output: 0,
            bindings: Default::default(),
        };
        flow.program(&[
            LinearOp::TensorLoad {
                dst_start: 0,
                input: crate::TensorInputKind::P,
                input_start: 0,
                count,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::Const {
                dst: middle as Reg,
                value: 0.,
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count,
                stride: 1,
            },
        ])
        .unwrap();
        assert_eq!(inputs.checks.len(), 2);
        assert!(!inputs.lane_at(middle).unwrap().checked());
        for p in [0, middle - 1, middle + 1, count - 1] {
            assert!(inputs.lane_at(p).unwrap().checked());
        }
    }
}

#[test]
fn whole_integer_call_argument_is_one_original_source_binding() {
    for count in [8, 65536] {
        let profile = crate::SolveArithmeticProfile::construct(
            crate::SolveRealFormat::Binary64,
            crate::SolveIntegerDomain::FULL,
        );
        let value = crate::SolveValueType::tensor(
            crate::SolveScalarType::integer(profile),
            vec![count as u32],
        )
        .unwrap();
        let mut table = crate::SolvePureCallTable::builder(profile);
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("TypedInputRun.mo"),
            1,
            2,
        );
        let owner = table
            .add_owner(
                crate::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
                vec![value.clone()],
                vec![crate::SolvePureCallOutput::result(value)],
                span,
                |builder, inputs, outputs| {
                    let value = builder.load(inputs[0], span)?;
                    builder.store(outputs[0], value, span)
                },
            )
            .unwrap();
        let site = table.call_site(owner).unwrap();
        let mut inputs = inputs(count);
        let mut flow = StageFlow {
            inputs: &mut inputs,
            sinks: BTreeMap::new(),
            stored_output: 0,
            bindings: Default::default(),
        };
        flow.program(&[
            LinearOp::TensorLoad {
                dst_start: 0,
                input: crate::TensorInputKind::P,
                input_start: 0,
                count,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::PureCall {
                dst_start: count as Reg,
                input_starts: vec![0].into(),
                site,
            },
        ])
        .unwrap();
        assert_eq!(flow.bindings.inputs.len(), 1);
        assert_eq!(flow.bindings.inputs[0].cells(), 0..count as u32);
        assert_eq!(flow.bindings.results.len(), 1);
        assert!(flow.bindings.read_results.is_empty());
        for p in [0, count / 2, count - 1] {
            assert!(!inputs.lane_at(p).unwrap().checked());
        }
    }
}

fn run(
    start: usize,
    count: usize,
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
) -> crate::SolveVariableStorageRun {
    crate::SolveVariableStorageRun {
        base: crate::scalar_slot_p(start),
        scalar_count: count,
        role,
        value_kind,
    }
}

#[test]
fn integer_and_boolean_parameters_and_constants_are_typed_lanes() {
    use SolveVariableStorageRole as Role;
    use SolveVariableValueKind as Kind;
    let problem = SolveProblem {
        layout: crate::VarLayout::from_parts(Default::default(), 0, 9),
        solve_layout: crate::SolveLayout {
            variable_storage_runs: vec![
                run(0, 2, Role::Parameter, Kind::Integer),
                run(2, 1, Role::Constant, Kind::Integer),
                run(3, 1, Role::Parameter, Kind::Boolean),
                run(4, 2, Role::Parameter, Kind::Real),
                run(6, 1, Role::ExternalInput, Kind::Integer),
                run(7, 2, Role::DiscreteValue, Kind::Integer),
            ],
            ..Default::default()
        },
        ..Default::default()
    };
    let inventory = TypedInputs::classify(&problem).unwrap();
    let placed: Vec<_> = inventory
        .runs
        .iter()
        .map(|run| (run.p_range(), run.role(), run.lane()))
        .collect();
    assert_eq!(
        placed,
        vec![
            (0..2, Role::Parameter, NativeOutputLane::Integer),
            (2..3, Role::Constant, NativeOutputLane::Integer),
            (3..4, Role::Parameter, NativeOutputLane::Boolean),
            (6..7, Role::ExternalInput, NativeOutputLane::Integer),
        ]
    );
    assert_eq!(inventory.integer_lane(1), Some(8));
    assert!(inventory.lane_at(4).is_none());
    assert!(inventory.lane_at(7).is_none());
}

#[test]
fn a_parameter_assigned_by_initialization_is_not_a_typed_lane() {
    use SolveVariableStorageRole as Role;
    use SolveVariableValueKind as Kind;
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("typed_parameter_lanes.mo"),
        1,
        2,
    )
    .require_provenance("typed parameter lane fixture")
    .unwrap();
    let rhs = crate::ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const { dst: 0, value: 3.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        span,
    )
    .unwrap();
    let initialization =
        crate::InitializationSolveSystem::construct(crate::InitializationSystemInput {
            update_rhs: rhs,
            update_targets: vec![crate::scalar_slot_p(1)],
            ..Default::default()
        })
        .unwrap();
    let problem = SolveProblem {
        layout: crate::VarLayout::from_parts(Default::default(), 0, 3),
        solve_layout: crate::SolveLayout {
            variable_storage_runs: vec![
                run(0, 1, Role::Parameter, Kind::Integer),
                run(1, 1, Role::Parameter, Kind::Integer),
                run(2, 1, Role::Constant, Kind::Boolean),
            ],
            ..Default::default()
        },
        initialization,
        ..Default::default()
    };
    let inventory = TypedInputs::classify(&problem).unwrap();
    let covered: Vec<_> = inventory.runs.iter().map(|run| run.p_range()).collect();
    assert_eq!(covered, vec![0..1, 2..3]);
}

fn boolean_inputs(count: usize) -> TypedInputs {
    let problem = SolveProblem {
        layout: crate::VarLayout::from_parts(Default::default(), 0, count),
        solve_layout: crate::SolveLayout {
            variable_storage_runs: vec![crate::SolveVariableStorageRun {
                base: crate::scalar_slot_p(0),
                scalar_count: count,
                role: SolveVariableStorageRole::ExternalInput,
                value_kind: SolveVariableValueKind::Boolean,
            }],
            ..Default::default()
        },
        ..Default::default()
    };
    TypedInputs::classify(&problem).unwrap()
}

/// A Real view is written only for a run some read does not bind to the lane.
#[test]
fn an_integer_run_read_only_by_bound_sinks_has_no_real_view() {
    for (integer_outputs, viewed) in [(1, true), (2, false)] {
        let mut inputs = inputs(1);
        let mut flow = StageFlow {
            inputs: &mut inputs,
            sinks: (0..integer_outputs)
                .map(|ordinal| (ordinal, NativeIntegerSource::Input { lane_offset: 0 }))
                .collect(),
            stored_output: 0,
            bindings: Default::default(),
        };
        flow.program(&[
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ])
        .unwrap();
        assert!(inputs.runs[0].view_is_read());
        inputs.settle_views();
        assert_eq!(inputs.runs[0].view_is_read(), viewed);
    }
}

#[test]
fn a_boolean_run_has_a_view_exactly_when_a_register_read_uses_it() {
    for (loads, viewed) in [(true, true), (false, false)] {
        let mut inputs = boolean_inputs(4);
        let mut flow = StageFlow {
            inputs: &mut inputs,
            sinks: BTreeMap::new(),
            stored_output: 0,
            bindings: Default::default(),
        };
        let mut program = vec![LinearOp::Const { dst: 0, value: 1. }];
        if loads {
            program.push(LinearOp::TensorLoad {
                dst_start: 1,
                input: crate::TensorInputKind::P,
                input_start: 0,
                count: 4,
                seed_start: None,
                lanes: 1,
            });
            program.push(LinearOp::StoreOutputRange {
                start: 1,
                count: 4,
                stride: 1,
            });
        }
        flow.program(&program).unwrap();
        inputs.settle_views();
        assert_eq!(inputs.runs[0].view_is_read(), viewed);
    }
}

fn flow_over(operations: &[LinearOp]) -> Result<(), NativeEvaluationRefusal> {
    let mut inputs = inputs(1);
    StageFlow {
        inputs: &mut inputs,
        sinks: BTreeMap::new(),
        stored_output: 0,
        bindings: Default::default(),
    }
    .program(operations)
}

/// A fold-carried load outside a fold body has no register-read proof, so the
/// stage stays admissible only while no live register holds an Integer source.
#[test]
fn an_unprovable_read_is_admitted_only_while_no_register_holds_an_integer_source() {
    let carried = LinearOp::LoadFoldCarried { dst: 1, index: 0 };
    assert!(flow_over(std::slice::from_ref(&carried)).is_ok());
    assert!(
        flow_over(&[LinearOp::Const { dst: 0, value: 1.0 }, carried.clone()]).is_ok(),
        "a constant is not an Integer source"
    );
    assert!(matches!(
        flow_over(&[LinearOp::LoadP { dst: 0, index: 0 }, carried]),
        Err(NativeEvaluationRefusal::UnresolvedTypedRead)
    ));
}
