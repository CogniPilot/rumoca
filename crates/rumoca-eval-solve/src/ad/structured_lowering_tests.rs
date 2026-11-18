//! Forward-mode products of folds, runtime-indexed loads, random streams,
//! tables, dot products, identities and block matrix products, checked by
//! evaluating the derived programs against hand-derived directional values.

use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    BinaryOp, ComputeBlock, ComputeNode, FoldInitialSource, FoldTensorNode, FoldTensorUpdate,
    FunctionFoldProgram, LinearOp, RandomGenerator, ScalarProgramBlock, TensorIndex,
    TensorNodeMetadata, TensorUpdateSubscript,
};
use std::sync::Arc;

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1)
}

fn block(rows: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
    let provenance = span()
        .require_provenance("structured lowering fixture")
        .expect("fixture span is source-backed");
    ScalarProgramBlock::with_source_span(rows, provenance).expect("fixture block is valid")
}

fn domain(upper: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper,
            step: 1,
        }],
    }
}

fn evaluate(
    block: &ScalarProgramBlock,
    y: &[f64],
    p: &[f64],
    context: crate::RowEvalContext<'_>,
) -> Vec<f64> {
    let mut out = vec![f64::NAN; block.output_count()];
    crate::eval_scalar_program_block_with_context(block, y, p, 0.0, context, &mut out)
        .expect("fixture evaluates");
    out
}

/// The solver-y directional derivative of every row at `y` along `seed`.
fn directional(rows: &[Vec<LinearOp>], y: &[f64], seed: &[f64]) -> Vec<f64> {
    let derived =
        block(crate::ad::lower_scalar_program_block_ad(rows).expect("forward lowering succeeds"));
    evaluate(
        &derived,
        y,
        &[],
        crate::RowEvalContext {
            seed: Some(seed),
            ..Default::default()
        },
    )
}

fn primal(rows: &[Vec<LinearOp>], y: &[f64]) -> Vec<f64> {
    evaluate(&block(rows.to_vec()), y, &[], Default::default())
}

fn mul(dst: u32, lhs: u32, rhs: u32) -> LinearOp {
    LinearOp::Binary {
        dst,
        op: BinaryOp::Mul,
        lhs,
        rhs,
    }
}

fn add(dst: u32, lhs: u32, rhs: u32) -> LinearOp {
    LinearOp::Binary {
        dst,
        op: BinaryOp::Add,
        lhs,
        rhs,
    }
}

/// `acc += capture * i` over `i in 1..=3`.
fn weighted_sum() -> Arc<FunctionFoldProgram> {
    Arc::new(
        FunctionFoldProgram::checked(
            domain(3),
            1,
            1,
            vec![
                LinearOp::LoadFoldCarried { dst: 0, index: 0 },
                LinearOp::LoadFoldCapture { dst: 1, index: 0 },
                LinearOp::LoadFoldIndex {
                    dst: 2,
                    dimension: 0,
                },
                mul(3, 1, 2),
                add(4, 0, 3),
                LinearOp::StoreOutput { src: 4 },
            ],
        )
        .expect("a checked fold"),
    )
}

#[test]
fn fold_tangent_flows_through_carried_capture_and_binder_lanes() {
    // y0 is the capture and y1 the initial accumulator: 6*y0 + y1.
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::FunctionFold {
            dst_start: 2,
            initial_start: 1,
            capture_start: 0,
            program: weighted_sum(),
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let rows = [row];
    assert_eq!(primal(&rows, &[2.0, 5.0]), [17.0]);
    for (seed, expected) in [([1.0, 0.0], 6.0), ([0.0, 1.0], 1.0), ([1.0, 1.0], 7.0)] {
        assert_eq!(
            directional(&rows, &[2.0, 5.0], &seed),
            [expected],
            "{seed:?}"
        );
    }
}

#[test]
fn guarded_fold_tangent_follows_the_activation() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 2 },
        LinearOp::GuardedFunctionFold {
            dst_start: 3,
            initial_start: 1,
            capture_start: 0,
            activation: 2,
            program: weighted_sum(),
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    let rows = [row];
    for (activation, expected) in [(1.0, 6.0 * 1.0 + 1.0), (0.0, 1.0)] {
        assert_eq!(
            directional(&rows, &[2.0, 5.0, activation], &[1.0, 1.0, 0.0]),
            [expected],
            "activation {activation}"
        );
    }
}

#[test]
fn runtime_indexed_fold_reads_carry_dual_lanes() {
    let dimensions = || vec![2].into_boxed_slice();
    let indices = || vec![TensorIndex::Runtime(0)].into_boxed_slice();
    let carried = LinearOp::LoadIndexedFoldCarried {
        dst: 1,
        base: 0,
        stride: 1,
        dimensions: dimensions(),
        indices: indices(),
    };
    let capture = LinearOp::LoadIndexedFoldCapture {
        dst: 2,
        base: 0,
        stride: 1,
        dimensions: dimensions(),
        indices: indices(),
    };
    // Per pass over i in 1..=2: [c*c, carried[2] + c] where c = capture[2].
    let program = Arc::new(
        FunctionFoldProgram::checked(
            domain(2),
            2,
            2,
            vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                carried,
                capture,
                mul(3, 2, 2),
                add(4, 1, 2),
                LinearOp::StoreOutput { src: 3 },
                LinearOp::StoreOutput { src: 4 },
            ],
        )
        .expect("a checked fold"),
    );
    let rows = [4, 5].map(|src| {
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadY { dst: 1, index: 1 },
            LinearOp::Const { dst: 2, value: 0.0 },
            LinearOp::Const { dst: 3, value: 0.0 },
            LinearOp::FunctionFold {
                dst_start: 4,
                initial_start: 2,
                capture_start: 0,
                program: program.clone(),
            },
            LinearOp::StoreOutput { src },
        ]
    });
    let y = [3.0, 5.0];
    assert_eq!(primal(&rows, &y), [25.0, 10.0]);
    assert_eq!(directional(&rows, &y, &[7.0, 11.0]), [110.0, 22.0]);
}

fn nested_fold_ops() -> Vec<LinearOp> {
    let inner = Arc::new(
        FunctionFoldProgram::checked(
            domain(2),
            2,
            1,
            vec![
                LinearOp::LoadFoldCarried { dst: 0, index: 0 },
                LinearOp::LoadFoldCarried { dst: 1, index: 1 },
                LinearOp::LoadFoldIndex {
                    dst: 2,
                    dimension: 0,
                },
                LinearOp::LoadFoldCapture { dst: 3, index: 0 },
                mul(4, 2, 3),
                add(5, 0, 4),
                LinearOp::StoreOutput { src: 5 },
                LinearOp::StoreOutput { src: 1 },
            ],
        )
        .expect("a checked inner fold"),
    );
    vec![
        LinearOp::LoadFoldCapture { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::StoreOutputFunctionFold {
            initial: vec![
                FoldInitialSource::ParentCarried { base: 0, count: 1 },
                FoldInitialSource::Registers { start: 1, count: 1 },
            ]
            .into(),
            capture_start: 0,
            program: inner,
            result_base: 0,
            count: 2,
            condition: Some(1),
            nested_when_true: true,
        },
    ]
}

fn derived_fold_body(update: Vec<LinearOp>, carried: usize, captures: usize) -> Vec<LinearOp> {
    let program = Arc::new(
        FunctionFoldProgram::checked(domain(2), carried, captures, update)
            .expect("a checked outer fold"),
    );
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Const { dst: 2, value: 0.0 },
        LinearOp::FunctionFold {
            dst_start: 3,
            initial_start: 1,
            capture_start: 0,
            program,
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    let derived = crate::ad::lower_scalar_program_block_ad(&[row]).expect("forward lowering");
    derived[0]
        .iter()
        .find_map(|op| match op {
            LinearOp::FunctionFold { program, .. } => Some(program.update.clone()),
            _ => None,
        })
        .expect("the derived row keeps one fold owner")
}

#[test]
fn nested_fold_output_doubles_initial_capture_and_result_lanes() {
    let derived = derived_fold_body(nested_fold_ops(), 2, 1);
    let nested = derived
        .iter()
        .find_map(|op| match op {
            LinearOp::StoreOutputFunctionFold {
                initial,
                result_base,
                count,
                condition,
                nested_when_true,
                ..
            } => Some((initial, *result_base, *count, *condition, *nested_when_true)),
            _ => None,
        })
        .expect("the nested fold output survives differentiation");
    let (initial, result_base, count, condition, when_true) = nested;
    assert_eq!(
        initial[0],
        FoldInitialSource::ParentCarried { base: 0, count: 2 }
    );
    assert!(matches!(
        initial[1],
        FoldInitialSource::Registers { count: 2, .. }
    ));
    assert_eq!((result_base, count, when_true), (0, 4, true));
    assert!(condition.is_some());
}

#[test]
fn tensor_update_in_a_fold_carries_interleaved_primal_and_tangent_lanes() {
    let update = vec![
        LinearOp::LoadFoldIndex {
            dst: 0,
            dimension: 0,
        },
        LinearOp::LoadFoldCapture { dst: 1, index: 0 },
        mul(2, 0, 1),
        LinearOp::Const { dst: 3, value: 1.0 },
        LinearOp::StoreOutputFoldTensorUpdate {
            source_base: 0,
            source_stride: 1,
            dimensions: vec![2].into(),
            updates: vec![FoldTensorUpdate {
                subscripts: vec![TensorUpdateSubscript::Index(TensorIndex::Runtime(0))].into(),
                condition: Some(3),
                value_start: 2,
                value_stride: 1,
            }]
            .into(),
            nodes: vec![
                FoldTensorNode::Update { base: 0, update: 0 },
                FoldTensorNode::Select {
                    condition: 3,
                    if_true: 1,
                    if_false: 0,
                },
            ]
            .into(),
            result: 1,
            lanes: 1,
        },
    ];
    let derived = derived_fold_body(update, 2, 1);
    let store = derived
        .iter()
        .find_map(|op| match op {
            LinearOp::StoreOutputFoldTensorUpdate {
                source_stride,
                updates,
                nodes,
                lanes,
                ..
            } => Some((*source_stride, updates.clone(), nodes.clone(), *lanes)),
            _ => None,
        })
        .expect("the tensor update survives differentiation");
    let (source_stride, updates, nodes, lanes) = store;
    assert_eq!((source_stride, lanes), (2, 2));
    assert_eq!(updates[0].value_stride, 2);
    assert!(updates[0].condition.is_some());
    assert!(matches!(
        nodes[1],
        FoldTensorNode::Select {
            if_true: 1,
            if_false: 0,
            ..
        }
    ));
}

#[test]
fn runtime_indexed_parameter_tangent_is_the_seed_at_the_same_offset() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::LoadIndexedP {
            dst: 1,
            base: 0,
            count: 3,
            index: 0,
        },
        LinearOp::LoadY { dst: 2, index: 0 },
        mul(3, 1, 2),
        LinearOp::StoreOutput { src: 3 },
    ];
    // p[1] = 4 and y0 = 3: d(p[1] * y0) = 4 * dy0 + 3 * dp[1].
    let p = [2.0, 4.0, 6.0];
    let seed = [10.0, 1.0, 100.0, 1000.0];
    let primal_block = block(vec![row.clone()]);
    let full = crate::ad::lower_scalar_program_block_full_jvp(&primal_block, 1).unwrap();
    let context = crate::RowEvalContext {
        seed: Some(&seed),
        ..Default::default()
    };
    assert_eq!(
        evaluate(&full, &[3.0], &p, context),
        [4.0 * 10.0 + 3.0 * 100.0]
    );

    let y_only = block(crate::ad::lower_scalar_program_block_ad(&[row]).unwrap());
    assert_eq!(evaluate(&y_only, &[3.0], &p, context), [4.0 * 10.0]);
}

#[test]
fn random_streams_are_constant_in_the_derivative_and_scale_the_neighbouring_tangent() {
    let stream = |extra: Vec<LinearOp>, store: u32| {
        let mut row = vec![
            LinearOp::Const {
                dst: 0,
                value: 11.0,
            },
            LinearOp::Const {
                dst: 1,
                value: 17.0,
            },
            LinearOp::RandomInitialState {
                dst: 2,
                generator: RandomGenerator::Xorshift64Star,
                local_seed: 0,
                global_seed: 1,
                state_len: 2,
                state_index: 0,
            },
            LinearOp::RandomInitialState {
                dst: 3,
                generator: RandomGenerator::Xorshift64Star,
                local_seed: 0,
                global_seed: 1,
                state_len: 2,
                state_index: 1,
            },
            LinearOp::LoadY { dst: 7, index: 0 },
        ];
        row.extend(extra);
        row.extend([mul(8, store, 7), LinearOp::StoreOutput { src: 8 }]);
        row
    };
    let rows = [
        stream(vec![], 2),
        stream(
            vec![LinearOp::RandomResult {
                dst: 4,
                generator: RandomGenerator::Xorshift64Star,
                state_start: 2,
                state_len: 2,
            }],
            4,
        ),
        stream(
            vec![LinearOp::RandomState {
                dst: 5,
                generator: RandomGenerator::Xorshift64Star,
                state_start: 2,
                state_len: 2,
                state_index: 1,
            }],
            5,
        ),
    ];
    // The tangent of `value * y0` is `value * dy0` because the random value has none.
    let y = [1.0];
    let values = primal(&rows, &y);
    let tangents = directional(&rows, &y, &[1.0]);
    assert!(
        values
            .iter()
            .all(|value| value.is_finite() && *value != 0.0)
    );
    assert_eq!(tangents, values);
}

#[test]
fn table_lookup_tangent_is_the_slope_times_the_input_tangent() {
    let tables = [rumoca_core::ExternalTableData {
        id: 1,
        data: vec![vec![0.0, 10.0], vec![2.0, 14.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }];
    let context = |seed| crate::RowEvalContext {
        seed,
        external_tables: Some(&tables),
        ..Default::default()
    };
    let rows = vec![vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::LoadY { dst: 2, index: 0 },
        LinearOp::TableLookup {
            dst: 3,
            table_id: 0,
            column: 1,
            input: 2,
        },
        LinearOp::TableBounds {
            dst: 4,
            table_id: 0,
            max: true,
        },
        LinearOp::TableNextEvent {
            dst: 5,
            table_id: 0,
            time: 2,
        },
        add(6, 4, 5),
        mul(7, 6, 2),
        add(8, 3, 7),
        LinearOp::StoreOutput { src: 8 },
    ]];
    let y = [0.5];
    let bounds_and_event = {
        let table_only = block(vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const { dst: 1, value: 0.5 },
            LinearOp::TableBounds {
                dst: 2,
                table_id: 0,
                max: true,
            },
            LinearOp::TableNextEvent {
                dst: 3,
                table_id: 0,
                time: 1,
            },
            add(4, 2, 3),
            LinearOp::StoreOutput { src: 4 },
        ]]);
        evaluate(&table_only, &[], &[], context(None))[0]
    };
    let derived = block(crate::ad::lower_scalar_program_block_ad(&rows).unwrap());
    // d/dy0 [lookup(y0) + (bounds + next) * y0] = slope + (bounds + next).
    let tangent = evaluate(&derived, &y, &[], context(Some(&[1.0])))[0];
    assert!(
        (tangent - (2.0 + bounds_and_event)).abs() <= 1.0e-12,
        "{tangent}"
    );
}

#[test]
fn dot_product_and_identity_tangents_follow_the_product_rule() {
    let dot = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 2 },
        LinearOp::LoadY { dst: 3, index: 3 },
        LinearOp::DotProduct {
            dst: 4,
            lhs_start: 0,
            rhs_start: 2,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let empty_dot = vec![
        LinearOp::DotProduct {
            dst: 0,
            lhs_start: 0,
            rhs_start: 0,
            count: 0,
            lhs_stride: 1,
            rhs_stride: 1,
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    let identity = |element: u32| {
        vec![
            LinearOp::TensorIdentity {
                dst_start: 0,
                size: 2,
                lanes: 1,
            },
            LinearOp::LoadY { dst: 4, index: 0 },
            mul(5, element, 4),
            LinearOp::StoreOutput { src: 5 },
        ]
    };
    let rows = [dot, empty_dot, identity(0), identity(1)];
    let y = [5.0, 6.0, 7.0, 8.0];
    assert_eq!(
        directional(&rows, &y, &[1.0, 2.0, 3.0, 4.0]),
        [62.0, 0.0, 1.0, 0.0]
    );
}

#[test]
fn matrix_product_with_seeded_factors_uses_one_block_product() {
    let loads = |start: u32, first: usize| -> Vec<LinearOp> {
        (0..2)
            .map(|offset| LinearOp::LoadY {
                dst: start + offset as u32,
                index: first + offset,
            })
            .collect()
    };
    let node = ComputeNode::MatMul {
        lhs_ops: loads(0, 0),
        lhs_start: 0,
        rhs_ops: loads(2, 2),
        rhs_start: 2,
        m: 1,
        k: 2,
        n: 1,
        lhs_pattern: crate::fixture_pattern(1, 2, false),
        rhs_pattern: crate::fixture_pattern(2, 1, false),
        metadata: TensorNodeMetadata::default(),
        span: span(),
    };
    let derived = crate::ad::lower_compute_block_jvp(&ComputeBlock { nodes: vec![node] })
        .expect("a seeded product lowers");
    let ComputeNode::MatMul { k, .. } = &derived.nodes[0] else {
        panic!("a matrix product stays a matrix product");
    };
    assert_eq!(*k, 4, "the inner dimension doubles for [dA | A] [B; dB]");
    let scalar = crate::to_scalar_program_block(&derived).expect("the product scalarizes");
    let context = crate::RowEvalContext {
        seed: Some(&[1.0, 2.0, 3.0, 4.0]),
        ..Default::default()
    };
    assert_eq!(
        evaluate(&scalar, &[5.0, 6.0, 7.0, 8.0], &[], context),
        [62.0]
    );
}

fn map_node(
    base_ops: Vec<LinearOp>,
    load_strides: Vec<rumoca_ir_solve::AffineStencilLoadStride>,
    const_strides: Vec<rumoca_ir_solve::AffineStencilConstStride>,
) -> ComputeBlock {
    let domain = domain(3);
    ComputeBlock {
        nodes: vec![ComputeNode::Map {
            output_map: rumoca_ir_solve::TensorOutputMap::dense_contiguous(0, &domain)
                .expect("valid dense output map"),
            domain,
            base_ops,
            load_strides,
            const_strides,
            metadata: TensorNodeMetadata::default(),
            span: span(),
        }],
    }
}

#[test]
fn affine_strides_must_target_a_load_or_a_constant_of_the_base_program() {
    let term = rumoca_ir_solve::AffineStencilIndexStrideTerm {
        dimension: 0,
        stride: 1,
    };
    let const_term = rumoca_ir_solve::AffineStencilConstStrideTerm {
        dimension: 0,
        stride: 1.0,
    };
    let loads_a_constant = map_node(
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        vec![rumoca_ir_solve::AffineStencilLoadStride {
            op_position: 0,
            terms: vec![term],
        }],
        Vec::new(),
    );
    let error = crate::ad::lower_compute_block_jvp(&loads_a_constant).unwrap_err();
    assert_eq!(error.source_span(), Some(span()));
    assert!(
        error
            .to_string()
            .contains("load stride targets invalid op position 0"),
        "{error}"
    );

    let strides_a_load = map_node(
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        Vec::new(),
        vec![rumoca_ir_solve::AffineStencilConstStride {
            op_position: 0,
            terms: vec![const_term],
        }],
    );
    let error = crate::ad::lower_compute_block_jvp(&strides_a_load).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("const stride targets invalid op position 0"),
        "{error}"
    );
}
