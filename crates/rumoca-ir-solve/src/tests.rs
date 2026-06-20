use super::*;
use indexmap::IndexMap;
use rumoca_core::{SourceId, StructuredIndexBinder};

const REPRESENTATIVE_SOLVE_PROBLEM_GOLDEN: &str =
    include_str!("../tests/golden/representative_solve_problem.solve.json");

fn test_tensor_domain(count: usize) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    }
}

fn fixture_span() -> Span {
    Span::from_offsets(SourceId(44), 0, 1)
}

#[test]
fn scalar_program_block_with_source_span_preserves_explicit_fixture_span() {
    let block = ScalarProgramBlock::with_source_span(vec![vec![]], fixture_span());
    assert_eq!(block.program_spans, vec![fixture_span()]);
}

fn make_layout(y_shapes: &[(&str, Vec<usize>)], p_shapes: &[(&str, Vec<usize>)]) -> VarLayout {
    let mut bindings = IndexMap::new();
    let mut shapes = IndexMap::new();
    let mut y_offset = 0usize;
    let mut p_offset = 0usize;
    for (name, shape) in y_shapes {
        let size: usize = shape.iter().product();
        bindings.insert(name.to_string(), scalar_slot_y(y_offset));
        shapes.insert(name.to_string(), shape.clone());
        y_offset += size;
    }
    for (name, shape) in p_shapes {
        let size: usize = shape.iter().product();
        bindings.insert(name.to_string(), scalar_slot_p(p_offset));
        shapes.insert(name.to_string(), shape.clone());
        p_offset += size;
    }
    VarLayout::from_parts_with_shapes(bindings, shapes, y_offset, p_offset)
        .expect("representative Solve fixture layout should satisfy shape contract")
}

fn representative_solve_problem_fixture() -> SolveProblem {
    SolveProblem {
        schema_version: SOLVE_SCHEMA_VERSION,
        layout: make_layout(
            &[("x", vec![1]), ("y", vec![1]), ("hold.y", vec![1])],
            &[("p", vec![1]), ("__pre__.hold.y", vec![1])],
        ),
        solve_layout: representative_solve_layout(),
        continuous: representative_continuous_system(),
        initialization: representative_initialization_system(),
        discrete: representative_discrete_system(),
        events: representative_event_partition(),
        clocks: representative_clock_partition(),
    }
}

fn representative_solver_maps() -> SolverNameIndexMaps {
    let mut name_to_idx = IndexMap::new();
    name_to_idx.insert("x".to_string(), 0);
    name_to_idx.insert("y".to_string(), 1);
    name_to_idx.insert("hold.y".to_string(), 2);

    let mut base_to_indices = IndexMap::new();
    base_to_indices.insert("x".to_string(), vec![0]);
    base_to_indices.insert("y".to_string(), vec![1]);
    base_to_indices.insert("hold.y".to_string(), vec![2]);

    SolverNameIndexMaps {
        names: vec!["x".to_string(), "y".to_string(), "hold.y".to_string()],
        name_to_idx,
        base_to_indices,
    }
}

fn representative_solve_layout() -> SolveLayout {
    SolveLayout {
        solver_maps: representative_solver_maps(),
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        output_scalar_count: 1,
        parameter_count: 1,
        compiled_parameter_len: 2,
        discrete_real_scalar_names: vec!["hold.y".to_string()],
        relation_memory_parameter_indices: vec![1],
        initial_event_parameter_index: Some(1),
        pre_param_bindings: vec![PreParamBinding {
            dest_p_index: 1,
            source: PreParamSource::Y { index: 2 },
        }],
        ..SolveLayout::default()
    }
}

fn representative_continuous_system() -> ContinuousSolveSystem {
    ContinuousSolveSystem {
        implicit_rhs: ComputeBlock {
            nodes: vec![ComputeNode::ScalarPrograms(
                ScalarProgramBlock::with_source_span(
                    vec![vec![
                        LinearOp::LoadY { dst: 0, index: 0 },
                        LinearOp::LoadP { dst: 1, index: 0 },
                        LinearOp::Binary {
                            dst: 2,
                            op: BinaryOp::Sub,
                            lhs: 0,
                            rhs: 1,
                        },
                        LinearOp::StoreOutput { src: 2 },
                    ]],
                    fixture_span(),
                ),
            )],
        },
        implicit_row_targets: vec![Some(scalar_slot_y(1))],
        algebraic_projection_plan: AlgebraicProjectionPlan {
            blocks: vec![AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
                causal_steps: Vec::new(),
            }],
        },
        residual: ComputeBlock::from_scalar_program_block(ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span(),
        )),
        derivative_rhs: representative_derivative_rhs(),
    }
}

fn representative_derivative_rhs() -> ComputeBlock {
    ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: vec![LinearOp::LoadP { dst: 0, index: 0 }],
            lhs_start: 0,
            rhs_ops: vec![LinearOp::LoadY { dst: 1, index: 0 }],
            rhs_start: 1,
            m: 1,
            k: 1,
            n: 1,
            lhs_sparsity: SparsityPattern::Diagonal,
            rhs_sparsity: SparsityPattern::Dense,
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        }],
    }
}

fn representative_initialization_system() -> InitializationSolveSystem {
    InitializationSolveSystem {
        row_targets: vec![Some(scalar_slot_y(1))],
        residual: ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span(),
        ),
        projection_indices: Vec::new(),
        projection_plan: AlgebraicProjectionPlan::default(),
        update_rhs: ScalarProgramBlock::default(),
        update_targets: Vec::new(),
    }
}

fn representative_discrete_system() -> DiscreteSolveSystem {
    DiscreteSolveSystem {
        runtime_assignment_rhs: ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 2 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span(),
        ),
        runtime_assignment_targets: vec![scalar_slot_p(1)],
        rhs: ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::Compare {
                    dst: 2,
                    op: CompareOp::Gt,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            fixture_span(),
        ),
        update_targets: vec![scalar_slot_y(2)],
        pre_modes: vec![DiscreteEventPreMode::Fixed],
        observation_refresh: vec![true],
    }
}

fn representative_event_partition() -> SolveEventPartition {
    SolveEventPartition {
        root_conditions: ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadTime { dst: 0 },
                LinearOp::LoadP { dst: 1, index: 0 },
                LinearOp::Compare {
                    dst: 2,
                    op: CompareOp::Ge,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            fixture_span(),
        ),
        scheduled_time_events: vec![0.1],
        ..SolveEventPartition::default()
    }
}

fn representative_clock_partition() -> SolveClockPartition {
    SolveClockPartition {
        periodic_event_schedules: vec![PeriodicEventSchedule {
            period_seconds: 0.1,
            phase_seconds: 0.0,
        }],
    }
}

fn assert_same_json_shape<T: serde::Serialize>(actual: &T, expected: &T) {
    assert_eq!(
        serde_json::to_value(actual).expect("serialize actual"),
        serde_json::to_value(expected).expect("serialize expected")
    );
}

#[test]
fn y_slice_returns_some_for_y_array_variable() {
    let layout = make_layout(&[("x", vec![3, 3])], &[]);
    let src = layout
        .y_slice("x")
        .expect("3×3 Y-slot variable should yield YSlice");
    assert!(matches!(src, TensorSource::YSlice { start: 0, shape } if shape == [3, 3]));
}

#[test]
fn p_slice_returns_some_for_p_array_variable() {
    let layout = make_layout(&[], &[("A", vec![2, 4])]);
    let src = layout
        .p_slice("A")
        .expect("2×4 P-slot variable should yield PSlice");
    assert!(matches!(src, TensorSource::PSlice { start: 0, shape } if shape == [2, 4]));
}

#[test]
fn indexed_bindings_are_derived_from_shape_metadata() {
    let layout = make_layout(&[("body.frame.R.T", vec![3, 3])], &[]);
    let entries = layout
        .indexed_bindings()
        .get(&ComponentReferenceKey::generated("body.frame.R.T"))
        .expect("array layout should expose structured scalar slots");

    assert_eq!(entries.len(), 9);
    assert_eq!(entries[0].indices, vec![1, 1]);
    assert!(matches!(entries[0].slot, ScalarSlot::Y { index: 0, .. }));
    assert_eq!(entries[8].indices, vec![3, 3]);
    assert!(matches!(entries[8].slot, ScalarSlot::Y { index: 8, .. }));
}

#[test]
fn scalar_program_block_rejects_span_count_mismatch_with_span() {
    let span = Span::from_offsets(SourceId::from_source_name("bad_scalar_spans.mo"), 2, 5);

    let err = ScalarProgramBlock::with_program_spans(
        vec![vec![LinearOp::StoreOutput { src: 0 }]],
        vec![span, span],
    )
    .expect_err("explicit scalar row spans must match row count");

    assert!(matches!(
        err,
        SolveProblemShapeContractError::ScalarProgramSpanMismatch {
            programs: 1,
            spans: 2,
            span: actual,
            ..
        } if actual == Some(span)
    ));
}

#[test]
fn scalar_program_block_rejects_output_index_count_mismatch_with_span() {
    let span = Span::from_offsets(SourceId::from_source_name("bad_scalar_outputs.mo"), 7, 11);

    let err = ScalarProgramBlock::with_output_indices(
        vec![vec![LinearOp::StoreOutput { src: 0 }]],
        vec![span],
        vec![0, 1],
    )
    .expect_err("explicit scalar output indices must match row count");

    assert!(matches!(
        err,
        SolveProblemShapeContractError::ScalarProgramOutputIndexMismatch {
            programs: 1,
            output_indices: 2,
            span: actual,
            ..
        } if actual == Some(span)
    ));
}

#[test]
fn scalar_program_block_first_source_span_skips_dummy_rows() {
    let span = Span::from_offsets(SourceId::from_source_name("scalar_source.mo"), 13, 21);
    let block = ScalarProgramBlock::with_program_spans(
        vec![
            vec![LinearOp::StoreOutput { src: 0 }],
            vec![LinearOp::StoreOutput { src: 1 }],
        ],
        vec![Span::DUMMY, span],
    )
    .expect("scalar span fixture metadata should match row count");

    assert_eq!(block.program_span(0), None);
    assert_eq!(block.program_span(1), Some(span));
    assert_eq!(block.first_source_span(), Some(span));
}

#[test]
fn y_slice_returns_none_for_p_slot_variable() {
    let layout = make_layout(&[], &[("p", vec![2])]);
    assert!(
        layout.y_slice("p").is_none(),
        "P-slot variable must not yield YSlice"
    );
}

#[test]
fn p_slice_returns_none_for_y_slot_variable() {
    let layout = make_layout(&[("x", vec![2])], &[]);
    assert!(
        layout.p_slice("x").is_none(),
        "Y-slot variable must not yield PSlice"
    );
}

#[test]
fn y_slice_returns_none_for_scalar_variable_without_shape() {
    let mut bindings = IndexMap::new();
    bindings.insert("s".to_string(), scalar_slot_y(0));
    let layout = VarLayout::from_parts_with_shapes(bindings, IndexMap::new(), 1, 0)
        .expect("scalar variable fixture layout should satisfy shape contract");
    assert!(
        layout.y_slice("s").is_none(),
        "scalar variable with no recorded shape must not yield YSlice"
    );
}

#[test]
fn y_slice_returns_none_for_unknown_variable() {
    let layout = make_layout(&[("x", vec![2])], &[]);
    assert!(layout.y_slice("unknown").is_none());
}

fn serde_roundtrip_tensor_block_fixture() -> ComputeBlock {
    ComputeBlock {
        nodes: vec![
            serde_roundtrip_scalar_node(),
            serde_roundtrip_matmul_node(),
            serde_roundtrip_linsolve_node(),
            serde_roundtrip_map_node(),
            serde_roundtrip_affine_stencil_node(),
        ],
    }
}

fn serde_roundtrip_scalar_node() -> ComputeNode {
    ComputeNode::ScalarPrograms(ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span(),
    ))
}

fn serde_roundtrip_matmul_node() -> ComputeNode {
    ComputeNode::MatMul {
        lhs_ops: vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::Move { dst: 1, src: 0 },
        ],
        lhs_start: 1,
        rhs_ops: vec![
            LinearOp::LoadSeed { dst: 2, index: 0 },
            LinearOp::Move { dst: 3, src: 2 },
        ],
        rhs_start: 3,
        m: 1,
        k: 1,
        n: 1,
        lhs_sparsity: SparsityPattern::Diagonal,
        rhs_sparsity: SparsityPattern::Dense,
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn serde_roundtrip_linsolve_node() -> ComputeNode {
    ComputeNode::LinSolve {
        setup_ops: vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 1, index: 1 },
            LinearOp::LoadP { dst: 2, index: 2 },
            LinearOp::LoadY { dst: 3, index: 0 },
        ],
        matrix_start: 0,
        rhs_start: 3,
        n: 2,
        next_reg: 4,
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn serde_roundtrip_map_node() -> ComputeNode {
    ComputeNode::Map {
        domain: test_tensor_domain(3),
        output_map: TensorOutputMap::dense_contiguous(0, &test_tensor_domain(3))
            .expect("valid dense output map"),
        base_ops: vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_strides: Vec::new(),
        const_strides: vec![AffineStencilConstStride {
            op_position: 0,
            terms: vec![AffineStencilConstStrideTerm {
                dimension: 0,
                stride: 1.0,
            }],
        }],
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn serde_roundtrip_affine_stencil_node() -> ComputeNode {
    ComputeNode::AffineStencil {
        domain: test_tensor_domain(8),
        output_map: TensorOutputMap::dense_contiguous(0, &test_tensor_domain(8))
            .expect("valid dense output map"),
        base_ops: vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_strides: vec![AffineStencilLoadStride {
            op_position: 0,
            terms: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }],
        const_strides: Vec::new(),
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn assert_tensor_node_tags_survive_json(json: &str) {
    for tag in [
        "MatMul",
        "LinSolve",
        "Map",
        "AffineStencil",
        "lhs_sparsity",
        "metadata",
    ] {
        assert!(json.contains(tag), "{tag} must appear in JSON: {json}");
    }
}

fn assert_tensor_nodes_survive_roundtrip(back: &ComputeBlock) {
    assert_eq!(
        back.nodes.len(),
        5,
        "all five compute nodes must survive round-trip"
    );
    assert!(matches!(&back.nodes[0], ComputeNode::ScalarPrograms(_)));
    assert!(matches!(&back.nodes[2], ComputeNode::LinSolve { n: 2, .. }));
    assert!(matches!(&back.nodes[3], ComputeNode::Map { .. }));
    assert!(matches!(
        &back.nodes[4],
        ComputeNode::AffineStencil { domain, .. }
            if domain
                .scalar_count()
                .expect("fixture domain should have a valid scalar count")
                == 8
    ));
    assert_roundtrip_matmul_shape(&back.nodes[1]);
}

fn assert_roundtrip_matmul_shape(node: &ComputeNode) {
    assert!(matches!(
        node,
        ComputeNode::MatMul {
            m: 1,
            k: 1,
            n: 1,
            lhs_sparsity: SparsityPattern::Diagonal,
            metadata: TensorNodeMetadata {
                element_type: TensorElementType::Real64,
                layout: TensorLayout::RowMajorDense,
                scalar_fallback: ScalarFallback::Exact,
            },
            ..
        }
    ));
}

#[test]
fn compute_block_tensor_nodes_survive_serde_roundtrip() {
    let block = serde_roundtrip_tensor_block_fixture();
    let json = serde_json::to_string(&block).expect("serialize ComputeBlock");
    assert_tensor_node_tags_survive_json(&json);

    let back: ComputeBlock = serde_json::from_str(&json).expect("deserialize ComputeBlock");
    assert_tensor_nodes_survive_roundtrip(&back);
}

#[test]
fn solve_problem_json_has_supported_schema_version() {
    let value = serde_json::to_value(SolveProblem::default()).expect("serialize SolveProblem");
    assert_eq!(
        value
            .get("schema_version")
            .and_then(serde_json::Value::as_u64),
        Some(u64::from(SOLVE_SCHEMA_VERSION))
    );

    let mut missing = value.clone();
    missing
        .as_object_mut()
        .expect("SolveProblem JSON should be object")
        .remove("schema_version");
    assert!(
        serde_json::from_value::<SolveProblem>(missing).is_err(),
        "SolveProblem JSON must carry an explicit schema_version"
    );

    let mut unsupported = value;
    unsupported["schema_version"] = serde_json::json!(SOLVE_SCHEMA_VERSION + 1);
    let err = serde_json::from_value::<SolveProblem>(unsupported)
        .expect_err("unsupported SolveProblem schema version must fail");
    assert!(err.to_string().contains("unsupported Solve schema_version"));
}

#[test]
fn representative_solve_problem_json_roundtrip_preserves_schema_shape() {
    let problem = representative_solve_problem_fixture();
    let json = serde_json::to_string_pretty(&problem).expect("serialize SolveProblem");
    let decoded: SolveProblem = serde_json::from_str(&json).expect("deserialize SolveProblem");
    assert_same_json_shape(&decoded, &problem);
}

#[test]
fn representative_solve_problem_json_matches_committed_golden() {
    let problem = representative_solve_problem_fixture();
    let actual = serde_json::to_value(&problem).expect("serialize representative SolveProblem");
    let expected: serde_json::Value = serde_json::from_str(REPRESENTATIVE_SOLVE_PROBLEM_GOLDEN)
        .expect("valid SolveProblem golden JSON");

    serde_json::from_value::<SolveProblem>(expected.clone())
        .expect("golden uses supported Solve schema");
    assert_eq!(actual, expected);
}

#[test]
fn representative_solve_problem_bincode_roundtrip_preserves_schema_shape() {
    let problem = representative_solve_problem_fixture();
    let bytes = bincode::serialize(&problem).expect("serialize SolveProblem as bincode");
    let decoded: SolveProblem =
        bincode::deserialize(&bytes).expect("deserialize SolveProblem from bincode");
    assert_same_json_shape(&decoded, &problem);
}

#[test]
fn solve_problem_shape_contract_rejects_bad_schema_version() {
    let mut problem = representative_solve_problem_fixture();
    problem.schema_version = SOLVE_SCHEMA_VERSION + 1;

    assert_eq!(
        problem.validate_shape_contract(),
        Err(SolveProblemShapeContractError::SchemaVersion {
            actual: SOLVE_SCHEMA_VERSION + 1,
            expected: SOLVE_SCHEMA_VERSION,
        })
    );
}

#[test]
fn solve_problem_shape_contract_rejects_zero_tensor_dimension() {
    let mut problem = representative_solve_problem_fixture();
    problem.continuous.derivative_rhs = ComputeBlock {
        nodes: vec![ComputeNode::LinSolve {
            setup_ops: Vec::new(),
            matrix_start: 0,
            rhs_start: 0,
            n: 0,
            next_reg: 0,
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        }],
    };

    assert_eq!(
        problem.validate_shape_contract(),
        Err(SolveProblemShapeContractError::ZeroTensorDimension {
            context: "continuous.derivative_rhs".to_string(),
            node_index: 0,
            dimension: "LinSolve",
            span: Span::DUMMY,
        })
    );
}

#[test]
fn solve_problem_shape_contract_rejects_zero_step_tensor_domain() {
    let mut problem = representative_solve_problem_fixture();
    problem.continuous.derivative_rhs = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 0,
                }],
            },
            output_map: TensorOutputMap {
                start: 0,
                strides: Vec::new(),
            },
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        }],
    };

    assert_eq!(
        problem.validate_shape_contract(),
        Err(SolveProblemShapeContractError::StructuredIndexDomain {
            context: "continuous.derivative_rhs".to_string(),
            node_index: 0,
            dimension: "Map",
            error: StructuredIndexDomainError::ZeroStep {
                binder_id: 0,
                display_name: "i".to_string(),
            },
            span: Span::DUMMY,
        })
    );
}
