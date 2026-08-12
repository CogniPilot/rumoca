use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};

fn checked(program: Vec<solve::LinearOp>) -> Vec<solve::LinearOp> {
    solve::ScalarProgramRegisterFlow::derive(&program)
        .expect("parameter-static fixture must be a checked register program");
    program
}

fn certifies(program: &[solve::LinearOp]) -> bool {
    parameter_static_refresh_program(program, 10, 2, &BTreeSet::from([11, 12]))
}

#[test]
fn compact_tensor_inputs_preserve_the_parameter_static_certificate() {
    let parameter_tensor = checked(vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::P,
            input_start: 4,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::TensorFill {
            dst_start: 3,
            value_start: 0,
            count: 3,
            lanes: 1,
        },
        solve::LinearOp::StoreOutputRange {
            start: 3,
            count: 3,
            stride: 1,
        },
    ]);
    assert!(certifies(&parameter_tensor));

    let certified_y_tensor = checked(vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: 10,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]);
    assert!(certifies(&certified_y_tensor));

    let dynamic_y_tensor = checked(vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: 10,
            count: 4,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]);
    assert!(!certifies(&dynamic_y_tensor));
}

#[test]
fn seed_dependent_tensor_load_is_never_parameter_static() {
    let seeded_parameter_tensor = checked(vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::P,
            input_start: 4,
            count: 1,
            seed_start: Some(0),
            lanes: 2,
        },
        solve::LinearOp::StoreOutput { src: 1 },
    ]);
    assert!(!certifies(&seeded_parameter_tensor));
}

#[test]
fn compact_fold_owner_is_certified_without_domain_expansion() {
    let fold = solve::FunctionFoldProgram::checked(
        StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 3,
                step: 1,
            }],
        },
        1,
        1,
        vec![
            solve::LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            solve::LinearOp::LoadFoldCapture { dst: 1, index: 0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("compact fold fixture has a checked carried/capture ABI");
    let program = checked(vec![
        solve::LinearOp::LoadP { dst: 0, index: 4 },
        solve::LinearOp::Const { dst: 1, value: 0.0 },
        solve::LinearOp::FunctionFold {
            dst_start: 2,
            initial_start: 1,
            capture_start: 0,
            program: Arc::new(fold),
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]);
    assert!(certifies(&program));
}

#[test]
fn lazy_conditional_regions_are_recursively_fail_closed() {
    let conditional = solve::FunctionConditionalProgram::checked(
        0,
        [1],
        [(
            vec![
                solve::LinearOp::LoadTime { dst: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        )],
        vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("lazy conditional fixture has checked correlated regions");
    let program = checked(vec![
        solve::LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: Arc::new(conditional),
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]);
    assert!(!certifies(&program));
}

#[test]
fn affine_compute_dependencies_include_every_shifted_solver_y_coordinate() {
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    };
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::AffineStencil {
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                .expect("fixture has a valid dense output map"),
            domain,
            base_ops: checked(vec![
                solve::LinearOp::LoadY { dst: 0, index: 10 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            load_strides: vec![solve::AffineStencilLoadStride {
                op_position: 0,
                terms: vec![solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 2,
                }],
            }],
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let dependencies = compute_block_dependencies(&block, 10)
        .expect("checked affine metadata has a finite dependency image");

    assert_eq!(
        dependencies
            .into_seed_stack(9..16)
            .expect("issued refresh candidates fit compact storage"),
        vec![10, 12, 14]
    );
}

#[test]
fn map_dependencies_accumulate_negative_stride_terms_without_expansion() {
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    };
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                .expect("fixture has a valid dense output map"),
            domain,
            base_ops: checked(vec![
                solve::LinearOp::LoadY { dst: 0, index: 14 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            load_strides: vec![
                solve::AffineStencilLoadStride {
                    op_position: 0,
                    terms: vec![solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: -1,
                    }],
                },
                solve::AffineStencilLoadStride {
                    op_position: 0,
                    terms: vec![solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: -1,
                    }],
                },
            ],
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let dependencies = compute_block_dependencies(&block, 10)
        .expect("checked affine metadata has a finite dependency image");

    assert_eq!(
        dependencies
            .into_seed_stack(9..16)
            .expect("issued refresh candidates fit compact storage"),
        vec![10, 12, 14]
    );
}

#[test]
fn affine_dependency_storage_is_independent_of_domain_cardinality() {
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 1_000_000,
            step: 1,
        }],
    };
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::AffineStencil {
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                .expect("fixture has a valid dense output map"),
            domain,
            base_ops: checked(vec![
                solve::LinearOp::LoadY { dst: 0, index: 10 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            load_strides: vec![solve::AffineStencilLoadStride {
                op_position: 0,
                terms: vec![solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 2,
                }],
            }],
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let dependencies = compute_block_dependencies(&block, 10)
        .expect("million-point affine metadata remains compact");

    assert!(dependencies.may_contain(10));
    assert!(dependencies.may_contain(2_000_008));
    assert!(!dependencies.may_contain(2_000_007));
}

#[test]
fn empty_affine_domain_has_no_runtime_y_dependencies() {
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 0,
            step: 1,
        }],
    };
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                .expect("empty domain still has a checked output map"),
            domain,
            base_ops: checked(vec![
                solve::LinearOp::LoadY { dst: 0, index: 10 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let dependencies = compute_block_dependencies(&block, 10)
        .expect("empty affine domains are valid and execute no body");

    assert!(
        dependencies
            .into_seed_stack([10])
            .expect("issued refresh candidates fit compact storage")
            .is_empty()
    );
}

#[test]
fn clocked_and_unclocked_outputs_get_distinct_refresh_dependencies() {
    let consumer = solve::ScalarProgramBlock::with_output_indices(
        vec![checked(vec![
            solve::LinearOp::LoadY { dst: 0, index: 10 },
            solve::LinearOp::StoreOutput { src: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 11 },
            solve::LinearOp::StoreOutput { src: 1 },
        ])],
        vec![solve::source_span_from_offsets(1, 0, 1)],
        vec![0, 1],
    )
    .expect("two correlated outputs have a checked scalar-program owner");
    let mut problem = solve::SolveProblem::default();
    problem.clocks.periodic_event_schedules = vec![
        solve::PeriodicEventSchedule::from_seconds(0.01, 0.0)
            .expect("fixture period is an exact positive schedule"),
    ];
    problem.clocks.activation_parameter_indices = vec![0];
    let clock = problem
        .clocks
        .periodic_clock_id(0)
        .expect("fixture clock identity is issued by its partition");
    problem.discrete.rhs = consumer;
    problem.discrete.clock_owners = vec![None, Some(clock)];

    let unclocked = event_consumer_dependencies(&problem, 10, None)
        .expect("unclocked dependency projection is checked");
    let clocked = event_consumer_dependencies(&problem, 10, Some(clock))
        .expect("clock dependency projection is checked");

    assert_eq!(
        unclocked
            .into_seed_stack(10..12)
            .expect("fixture candidate range is finite"),
        vec![10]
    );
    assert_eq!(
        clocked
            .into_seed_stack(10..12)
            .expect("fixture candidate range is finite"),
        vec![11]
    );
}

#[test]
fn construction_issues_event_base_and_clock_remainder_plans() {
    let span = solve::source_span_from_offsets(2, 0, 1);
    let implicit = solve::ScalarProgramBlock::with_output_indices(
        vec![
            checked(vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            checked(vec![
                solve::LinearOp::Const { dst: 0, value: 2.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
        ],
        vec![span, span],
        vec![0, 1],
    )
    .expect("fixture algebraics have checked independent owners");
    let consumer = solve::ScalarProgramBlock::with_output_indices(
        vec![checked(vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 1 },
            solve::LinearOp::StoreOutput { src: 1 },
        ])],
        vec![span],
        vec![0, 1],
    )
    .expect("fixture event consumer has checked correlated outputs");
    let mut problem = solve::SolveProblem::default();
    problem.solve_layout.state_scalar_count = 0;
    problem.solve_layout.algebraic_scalar_count = 2;
    problem.solve_layout.solver_maps.names = vec!["a".to_string(), "b".to_string()];
    problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(implicit);
    problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    problem.clocks.periodic_event_schedules = vec![
        solve::PeriodicEventSchedule::from_seconds(0.01, 0.0)
            .expect("fixture period is an exact positive schedule"),
    ];
    problem.clocks.activation_parameter_indices = vec![0];
    let clock = problem
        .clocks
        .periodic_clock_id(0)
        .expect("fixture clock identity is issued by its partition");
    problem.discrete.rhs = consumer;
    problem.discrete.clock_owners = vec![None, Some(clock)];

    let owners = build_continuous_refresh_owners(&problem)
        .expect("construction can partition the checked event dependencies");

    assert_eq!(
        owners
            .event()
            .rows
            .iter()
            .map(solve::AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        vec![0]
    );
    assert_eq!(
        owners.clock_events()[0]
            .rows
            .iter()
            .map(solve::AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        vec![1]
    );
    assert_eq!(
        owners.clock_events_after_event()[0]
            .remainder()
            .rows
            .iter()
            .map(solve::AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        vec![1]
    );
}
