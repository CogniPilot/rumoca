use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};

fn checked(program: Vec<solve::LinearOp>) -> Vec<solve::LinearOp> {
    solve::ScalarProgramRegisterFlow::derive(&program)
        .expect("parameter-static fixture must be a checked register program");
    program
}

#[test]
fn conditional_regions_contribute_every_solver_y_input_range() {
    let load = |index| {
        vec![
            solve::LinearOp::LoadY { dst: 0, index },
            solve::LinearOp::StoreOutput { src: 0 },
        ]
    };
    let conditional =
        solve::FunctionConditionalProgram::checked(0, [1], [(load(52), load(54))], load(56))
            .expect("conditional dependency fixture has checked regions");
    let program = checked(vec![
        solve::LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: Arc::new(conditional),
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]);

    assert_eq!(row_y_input_ranges(&program), [52..53, 54..55, 56..57]);
}

#[test]
fn affine_compute_dependencies_include_every_shifted_solver_y_coordinate() {
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
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
            id: rumoca_core::StructuredIndexBinderId::new(0),
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
            id: rumoca_core::StructuredIndexBinderId::new(0),
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
            id: rumoca_core::StructuredIndexBinderId::new(0),
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
    let solve_layout = solve::SolveLayout::default();
    let implicit_rhs = solve::ComputeBlock::default();
    let implicit_row_targets = Vec::new();
    let algebraic_projection_plan = solve::AlgebraicProjectionPlan::default();
    let derivative_rhs = solve::ComputeBlock::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition {
        periodic_event_schedules: vec![
            solve::PeriodicEventSchedule::from_seconds(0.01, 0.0)
                .expect("fixture period is an exact positive schedule"),
        ],
        activation_parameter_indices: vec![0],
    };
    let clock = clocks
        .periodic_clock_id(0)
        .expect("fixture clock identity is issued by its partition");
    let discrete = solve::DiscreteSolveSystem {
        rhs: consumer,
        clock_owners: vec![None, Some(clock)],
        ..solve::DiscreteSolveSystem::default()
    };
    let source = ContinuousRefreshSource {
        solve_layout: &solve_layout,
        implicit_rhs: &implicit_rhs,
        implicit_row_targets: &implicit_row_targets,
        algebraic_projection_plan: &algebraic_projection_plan,
        derivative_rhs: &derivative_rhs,
        discrete: &discrete,
        events: &events,
        clocks: &clocks,
    };

    let unclocked = event_consumer_dependencies(&source, 10, None)
        .expect("unclocked dependency projection is checked");
    let clocked = event_consumer_dependencies(&source, 10, Some(clock))
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
    let mut solve_layout = solve::SolveLayout {
        state_scalar_count: 0,
        algebraic_scalar_count: 2,
        ..solve::SolveLayout::default()
    };
    solve_layout.solver_maps.names = vec!["a".to_string(), "b".to_string()];
    let implicit_rhs = solve::ComputeBlock::from_scalar_program_block(implicit);
    let implicit_row_targets = vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    let mut algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![
            solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
                tearing: None,
            },
            solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
                tearing: None,
            },
        ],
    };
    let derivative_rhs = solve::ComputeBlock::default();
    let clocks = solve::SolveClockPartition {
        periodic_event_schedules: vec![
            solve::PeriodicEventSchedule::from_seconds(0.01, 0.0)
                .expect("fixture period is an exact positive schedule"),
        ],
        activation_parameter_indices: vec![0],
    };
    let clock = clocks
        .periodic_clock_id(0)
        .expect("fixture clock identity is issued by its partition");
    let discrete = solve::DiscreteSolveSystem {
        rhs: consumer,
        clock_owners: vec![None, Some(clock)],
        ..solve::DiscreteSolveSystem::default()
    };
    let events = solve::SolveEventPartition::default();

    let refresh_plans = build_continuous_refresh_plans(
        &solve_layout,
        (
            &implicit_rhs,
            &implicit_row_targets,
            &mut algebraic_projection_plan,
            &derivative_rhs,
        ),
        &discrete,
        &events,
        &clocks,
    )
    .expect("construction can partition the checked event dependencies");
    let continuous = solve::ContinuousSolveSystem::construct(
        &solve_layout,
        solve::ContinuousSolveSystemInputs::new(
            implicit_rhs.clone(),
            implicit_row_targets,
            algebraic_projection_plan,
            implicit_rhs,
            (
                solve::ComputeBlock::default(),
                solve::AlgebraicProjectionPlan::default(),
            ),
            derivative_rhs,
            refresh_plans,
        ),
    )
    .expect("fixture plans correlate with their exact continuous source");
    assert_event_and_clock_refresh_owners(continuous.refresh_owners());
}

fn assert_event_and_clock_refresh_owners(owners: &solve::ContinuousRefreshOwners) {
    assert_eq!(
        owners
            .event()
            .rows()
            .iter()
            .map(solve::AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        vec![0]
    );
    assert_eq!(
        owners.clock_events()[0]
            .rows()
            .iter()
            .map(solve::AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        vec![1]
    );
    assert_eq!(
        owners.clock_events_after_event()[0]
            .remainder()
            .rows()
            .iter()
            .map(solve::AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        vec![1]
    );
}

#[test]
fn tearing_normalization_promotes_only_the_inexact_causal_step() {
    use solve::BinaryOp;
    use solve::LinearOp::{Binary, LoadY, StoreOutput};

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("tearing-normalization-fixture"),
        1,
        2,
    )
    .require_provenance("tearing normalization fixture")
    .expect("fixture span is source-backed");

    // Output row 0: `y0*y1 - y2`, an exact isolator for solver-Y index 1.
    let exact_row = vec![
        LoadY { dst: 0, index: 0 },
        LoadY { dst: 1, index: 1 },
        LoadY { dst: 2, index: 2 },
        Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        StoreOutput { src: 4 },
    ];
    // Output row 1: `y3*y3 - y4`, quadratic in y3 and therefore not an exact
    // explicit assignment for solver-Y index 3.
    let inexact_row = vec![
        LoadY { dst: 0, index: 3 },
        LoadY { dst: 1, index: 4 },
        Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        Binary {
            dst: 3,
            op: BinaryOp::Sub,
            lhs: 2,
            rhs: 1,
        },
        StoreOutput { src: 3 },
    ];
    let block = solve::ScalarProgramBlock::with_source_span(vec![exact_row, inexact_row], span)
        .expect("scalar fixture is computable");
    let implicit_scalar_rhs = PreparedScalarProgramBlock::new(block).expect("fixture prepares");

    // Ground truth: confirm the fixture rows carry the intended exactness before
    // asserting the promotion respects it.
    assert!(
        causal_step_certifies_exact_assignment(&implicit_scalar_rhs, 0, 1),
        "row 0 isolates solver-Y index 1 exactly"
    );
    assert!(
        !causal_step_certifies_exact_assignment(&implicit_scalar_rhs, 1, 3),
        "row 1 is quadratic in solver-Y index 3 and is not an exact assignment"
    );

    let mut tearing = solve::BlockTearing {
        tear_y_indices: vec![7],
        residual_rows: vec![9],
        causal_steps: vec![
            solve::CausalStep { row: 0, y_index: 1 },
            solve::CausalStep { row: 1, y_index: 3 },
        ],
    };
    promote_inexact_causal_steps(&mut tearing, &implicit_scalar_rhs);

    // The exact step is retained; the inexact step is promoted into the reduced
    // Newton, keeping `tear_y_indices.len() == residual_rows.len()`.
    assert_eq!(
        tearing.causal_steps,
        vec![solve::CausalStep { row: 0, y_index: 1 }],
        "the exact causal step stays a back-substitution step"
    );
    assert_eq!(
        tearing.tear_y_indices,
        vec![7, 3],
        "the inexact step's unknown becomes a tear variable"
    );
    assert_eq!(
        tearing.residual_rows,
        vec![9, 1],
        "the inexact step's row becomes a reduced residual"
    );
    assert_eq!(tearing.tear_y_indices.len(), tearing.residual_rows.len());
}

#[test]
fn tearing_normalization_promotes_every_step_when_none_are_exact() {
    use solve::BinaryOp;
    use solve::LinearOp::{Binary, LoadY, StoreOutput};

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("all-implicit-fixture"),
        1,
        2,
    )
    .require_provenance("all-implicit tearing fixture")
    .expect("fixture span is source-backed");

    // A single quadratic residual that is not an exact assignment for its target.
    let row = vec![
        LoadY { dst: 0, index: 0 },
        LoadY { dst: 1, index: 1 },
        Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        Binary {
            dst: 3,
            op: BinaryOp::Sub,
            lhs: 2,
            rhs: 1,
        },
        StoreOutput { src: 3 },
    ];
    let block = solve::ScalarProgramBlock::with_source_span(vec![row], span)
        .expect("scalar fixture is computable");
    let implicit_scalar_rhs = PreparedScalarProgramBlock::new(block).expect("fixture prepares");

    let mut tearing = solve::BlockTearing {
        tear_y_indices: vec![5],
        residual_rows: vec![6],
        causal_steps: vec![solve::CausalStep { row: 0, y_index: 0 }],
    };
    promote_inexact_causal_steps(&mut tearing, &implicit_scalar_rhs);

    // With no exact step to retain, back-substitution degenerates to a no-op and
    // the reduced Newton solves every unknown of the block.
    assert!(tearing.causal_steps.is_empty());
    assert_eq!(tearing.tear_y_indices, vec![5, 0]);
    assert_eq!(tearing.residual_rows, vec![6, 0]);
}
