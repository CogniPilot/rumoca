//! Compact derivative nodes: the constructed product is bitwise equal to the
//! scalarized source's, and its finite checks report exactly the scalarized
//! source's first failing identity.

use super::*;
use crate::compute_block_scalarize::to_scalar_program_block;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};

const STATES: usize = 4;

/// `dx[k] = y[k]·y[k+1] + exp(y[k])` for `k` in `0..3`, compact; `dx[3] =
/// y[3]·y[0]` as one scalar program issued before it, so the node's points
/// are programs 1, 2 and 3.
fn compact_block() -> ComputeBlock {
    compact_block_with(
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadY { dst: 1, index: 1 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::Unary {
                dst: 3,
                op: UnaryOp::Exp,
                arg: 0,
            },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Add,
                lhs: 2,
                rhs: 3,
            },
            LinearOp::StoreOutput { src: 4 },
        ],
        true,
    )
}

/// The same layout with `base_ops` as the compact node's body; `strided`
/// adds the state-load strides of operations 0 and 1.
fn compact_block_with(base_ops: Vec<LinearOp>, strided: bool) -> ComputeBlock {
    let scalar = ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 3 },
            LinearOp::LoadY { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        vec![span()],
        vec![3],
    )
    .unwrap();
    let stride = |op_position| AffineStencilLoadStride {
        op_position,
        terms: vec![AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: 1,
        }],
    };
    let map = ComputeNode::Map {
        domain: StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id: 0,
                display_name: "k".into(),
                lower: 1,
                upper: 3,
                step: 1,
            }],
        },
        output_map: TensorOutputMap {
            start: 0,
            strides: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        },
        base_ops,
        load_strides: if strided {
            vec![stride(0), stride(1)]
        } else {
            vec![]
        },
        const_strides: vec![],
        metadata: TensorNodeMetadata::default(),
        span: span(),
    };
    let mut block = ComputeBlock::from_scalar_program_block(scalar);
    block.nodes.push(map);
    block
}

fn four_state_model(derivative: ComputeBlock) -> SolveModel {
    n_state_model(STATES, derivative)
}

fn n_state_model(states: usize, derivative: ComputeBlock) -> SolveModel {
    let mut source = model(vec![0, 1]);
    source.problem.layout = VarLayout::from_parts_with_shapes_and_spans(
        [("x".into(), scalar_slot_y(0))].into(),
        [("x".into(), vec![states])].into(),
        [("x".into(), span())].into(),
        states,
        0,
    )
    .unwrap();
    source.problem.solve_layout.state_scalar_count = states;
    source.problem.solve_layout.variable_storage_runs[0].scalar_count = states;
    source.problem.continuous.derivative_rhs = derivative;
    source
}

fn scalarized(block: &ComputeBlock) -> ComputeBlock {
    ComputeBlock::from_scalar_program_block(to_scalar_program_block(block).unwrap())
}

fn run(
    product: &ModelStateJacobian<'_>,
    y: [f64; STATES],
) -> Result<Vec<u64>, crate::TypedProgramEvalError> {
    run_at(product, y, 0.0)
}

fn run_at(
    product: &ModelStateJacobian<'_>,
    y: [f64; STATES],
    time: f64,
) -> Result<Vec<u64>, crate::TypedProgramEvalError> {
    let real = SolveValueType::scalar(SolveScalarType::real(product.table().arithmetic()));
    let arguments = y
        .into_iter()
        .chain([time])
        .map(|x| {
            TypedValue::construct(real.clone(), vec![SolveValueKind::Real64(x.to_bits())]).unwrap()
        })
        .collect::<Vec<_>>();
    let values = eval_pure_call_checked(product.table(), product.call_site().owner(), &arguments)?;
    Ok(values[0]
        .elements()
        .iter()
        .map(|value| match value {
            SolveValueKind::Real64(bits) => *bits,
            _ => panic!("Binary64 matrix expected"),
        })
        .collect())
}

/// The checked default profile with nested map frames admitted, so the
/// compact map itself is issued and evaluated by the interpreter.
fn map_profile() -> SolveTargetProfile {
    let mut declared = serde_json::to_value(SolveTargetProfile::current_dense_host()).unwrap();
    declared["admits_nested_map_frames"] = serde_json::Value::Bool(true);
    declared["name"] = serde_json::Value::String("compact-map-test".into());
    let profile: SolveTargetProfile = serde_json::from_value(declared).unwrap();
    assert!(profile.select().admits_nested_map_frames());
    profile
}

fn compact_product(model: &SolveModel) -> ModelStateJacobian<'_> {
    ModelStateJacobian::construct_for_target(model, &map_profile()).unwrap()
}

fn has_map(program: &TypedProgram) -> bool {
    program
        .operations()
        .iter()
        .any(|operation| matches!(operation.operation(), SolveOperation::Map { .. }))
}

#[test]
fn compact_node_issues_one_map_and_matches_the_scalarized_product_bitwise() {
    let compact = four_state_model(compact_block());
    let reference = four_state_model(scalarized(&compact_block()));
    let compact = compact_product(&compact);
    let reference = ModelStateJacobian::construct(&reference).unwrap();
    let body = |product: &ModelStateJacobian<'_>| product.table().owners()[0].body().clone();
    assert!(has_map(&body(&compact)), "compact node must stay one map");
    assert!(!has_map(&body(&reference)));
    for y in [
        [0.5, -1.25, 2.0, 0.75],
        [-3.0, 0.0, 1e-3, 7.5],
        [1.0, 1.0, 1.0, 1.0],
    ] {
        let got = run(&compact, y).unwrap();
        assert_eq!(got, run(&reference, y).unwrap(), "y = {y:?}");
        // Row k: d/dy[k] = y[k+1] + exp(y[k]), d/dy[k+1] = y[k].
        let at = |row: usize, column: usize| f64::from_bits(got[row * STATES + column]);
        for k in 0..3 {
            assert_eq!(at(k, k), y[k + 1] + y[k].exp());
            assert_eq!(at(k, k + 1), y[k]);
        }
        assert_eq!(at(3, 3), y[0]);
        assert_eq!(at(3, 0), y[3]);
    }
}

#[test]
fn injected_overflow_at_point_k_reports_the_scalar_identity_base_plus_k() {
    let compact = four_state_model(compact_block());
    let reference = four_state_model(scalarized(&compact_block()));
    let compact = compact_product(&compact);
    let reference = ModelStateJacobian::construct(&reference).unwrap();
    for k in 0..3 {
        let mut y = [0.5; STATES];
        y[k] = 800.0;
        let error = run(&compact, y).unwrap_err();
        assert_eq!(error, run(&reference, y).unwrap_err(), "point {k}");
        assert!(matches!(
            error,
            crate::TypedProgramEvalError::NonFinite {
                check: SolveFiniteCheck::Primal { program, operation: 3 },
                element: 0,
                ..
            } if program == 1 + k as u32
        ));
    }
}

#[test]
fn first_error_follows_scalar_program_order_not_operation_order() {
    // Point 0 fails late (exp at operation 3); point 1 fails early (the
    // product at operation 2). The scalarized source reports point 0's.
    let y = [800.0, 1e200, 1e200, 0.5];
    let compact = four_state_model(compact_block());
    let reference = four_state_model(scalarized(&compact_block()));
    let compact = compact_product(&compact);
    let reference = ModelStateJacobian::construct(&reference).unwrap();
    let error = run(&compact, y).unwrap_err();
    assert_eq!(error, run(&reference, y).unwrap_err());
    assert!(matches!(
        error,
        crate::TypedProgramEvalError::NonFinite {
            check: SolveFiniteCheck::Primal {
                program: 1,
                operation: 3
            },
            ..
        }
    ));
}

#[test]
fn compact_node_with_strided_constants_refuses_by_name() {
    // A per-point constant (op 0 is a `Const`) is not constructed yet.
    let mut block = compact_block_with(
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        false,
    );
    if let ComputeNode::Map { const_strides, .. } = &mut block.nodes[1] {
        const_strides.push(AffineStencilConstStride {
            op_position: 0,
            terms: vec![AffineStencilConstStrideTerm {
                dimension: 0,
                stride: 1.0,
            }],
        });
    }
    assert!(matches!(
        ModelStateJacobian::construct(&four_state_model(block)),
        Err(ModelStateJacobianError::Unsupported {
            reason: "compact strided constants are not issued yet",
            ..
        })
    ));
}

#[test]
fn compact_node_reading_no_state_keeps_its_map_and_primal_checks() {
    // `dx[k] = exp(t)`: no partials, so rows 0..3 are zero, yet the map
    // still runs and exp(800) reports the scalarized source's identity.
    let ops = || {
        vec![
            LinearOp::LoadTime { dst: 0 },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Exp,
                arg: 0,
            },
            LinearOp::StoreOutput { src: 1 },
        ]
    };
    let compact = four_state_model(compact_block_with(ops(), false));
    let reference = four_state_model(scalarized(&compact_block_with(ops(), false)));
    let compact = compact_product(&compact);
    let reference = ModelStateJacobian::construct(&reference).unwrap();
    assert!(has_map(compact.table().owners()[0].body()));
    let y = [0.5, -1.0, 2.0, 0.25];
    let got = run_at(&compact, y, 1.5).unwrap();
    assert_eq!(got, run_at(&reference, y, 1.5).unwrap());
    assert!(
        got[..3 * STATES]
            .iter()
            .all(|bits| *bits == 0.0f64.to_bits())
    );
    let error = run_at(&compact, y, 800.0).unwrap_err();
    assert_eq!(error, run_at(&reference, y, 800.0).unwrap_err());
    assert!(matches!(
        error,
        crate::TypedProgramEvalError::NonFinite {
            check: SolveFiniteCheck::Primal {
                program: 1,
                operation: 1
            },
            ..
        }
    ));
}

#[test]
fn constant_compact_node_yields_zero_rows_like_the_scalarized_source() {
    let ops = || {
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]
    };
    let compact = four_state_model(compact_block_with(ops(), false));
    let reference = four_state_model(scalarized(&compact_block_with(ops(), false)));
    let compact = compact_product(&compact);
    let reference = ModelStateJacobian::construct(&reference).unwrap();
    let y = [0.5, -1.0, 2.0, 0.25];
    assert_eq!(run(&compact, y).unwrap(), run(&reference, y).unwrap());
}

#[test]
fn default_target_issues_the_scalarized_program_without_a_map() {
    // Today's dense host (and Cranelift) profile admits no nested map frame,
    // so the compact node is issued as its scalarized programs: the same
    // program the scalar reference builds, under the same identities.
    assert!(
        !SolveTargetProfile::current_dense_host()
            .select()
            .admits_nested_map_frames()
    );
    let compact = four_state_model(compact_block());
    let reference = four_state_model(scalarized(&compact_block()));
    let compact = ModelStateJacobian::construct(&compact).unwrap();
    let reference = ModelStateJacobian::construct(&reference).unwrap();
    let body = |product: &ModelStateJacobian<'_>| product.table().owners()[0].body().clone();
    assert!(!has_map(&body(&compact)));
    assert_eq!(body(&compact), body(&reference));
    let mut y = [0.5; STATES];
    assert_eq!(run(&compact, y).unwrap(), run(&reference, y).unwrap());
    y[1] = 800.0;
    assert_eq!(
        run(&compact, y).unwrap_err(),
        run(&reference, y).unwrap_err()
    );
}

#[test]
fn scalar_expansion_beyond_the_target_bound_refuses_as_unsupported() {
    // 300 points x 4 operations exceed the default profile's bound, so a
    // maps-disabled selection refuses before expanding a row; directional AD
    // stays the consumer. A map-admitting target still issues the compact map.
    let points = 300;
    let block = || {
        let mut block = ComputeBlock { nodes: vec![] };
        block.nodes.push(ComputeNode::Map {
            domain: StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    provenance: rumoca_core::IndexBinderProvenance::Generated,
                    id: 0,
                    display_name: "k".into(),
                    lower: 1,
                    upper: points,
                    step: 1,
                }],
            },
            output_map: TensorOutputMap {
                start: 0,
                strides: vec![AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            },
            base_ops: vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Binary {
                    dst: 1,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 0,
                },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Mul,
                    lhs: 1,
                    rhs: 0,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
            load_strides: vec![AffineStencilLoadStride {
                op_position: 0,
                terms: vec![AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            }],
            const_strides: vec![],
            metadata: TensorNodeMetadata::default(),
            span: span(),
        });
        block
    };
    let states = usize::try_from(points).unwrap();
    let model = n_state_model(states, block());
    assert!(matches!(
        ModelStateJacobian::construct(&model),
        Err(ModelStateJacobianError::Unsupported {
            reason: "compact nodes exceed the target's scalar expansion bound",
            ..
        })
    ));
    let compact = compact_product(&model);
    assert!(has_map(compact.table().owners()[0].body()));
}

/// `dx[start + k - 1] = y[start + k - 1]^3` over `k = 1..=points`: one
/// compact node of `points` points and 4 base operations.
fn cube_node(points: i64, start: usize) -> ComputeNode {
    ComputeNode::Map {
        domain: StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id: 0,
                display_name: "k".into(),
                lower: 1,
                upper: points,
                step: 1,
            }],
        },
        output_map: TensorOutputMap {
            start,
            strides: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        },
        base_ops: vec![
            LinearOp::LoadY {
                dst: 0,
                index: start,
            },
            LinearOp::Binary {
                dst: 1,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 0,
            },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 1,
                rhs: 0,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
        load_strides: vec![AffineStencilLoadStride {
            op_position: 0,
            terms: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }],
        const_strides: vec![],
        metadata: TensorNodeMetadata::default(),
        span: span(),
    }
}

#[test]
fn expansion_bound_applies_to_the_whole_product_not_each_node() {
    // Two nodes of 150 points x 4 operations: each alone (600) fits the
    // default bound, together (1200) they do not, so the product refuses.
    let bound = SolveTargetProfile::current_dense_host()
        .select()
        .max_unrolled_operations();
    assert!((150 * 4..2 * 150 * 4).contains(&bound));
    let model = n_state_model(
        300,
        ComputeBlock {
            nodes: vec![cube_node(150, 0), cube_node(150, 150)],
        },
    );
    assert!(matches!(
        ModelStateJacobian::construct(&model),
        Err(ModelStateJacobianError::Unsupported {
            reason: "compact nodes exceed the target's scalar expansion bound",
            ..
        })
    ));
    // One such node alone is admitted and expanded under the same profile.
    let single = n_state_model(
        150,
        ComputeBlock {
            nodes: vec![cube_node(150, 0)],
        },
    );
    let product = ModelStateJacobian::construct(&single).unwrap();
    assert!(!has_map(product.table().owners()[0].body()));
    // A map-admitting target issues both compact maps.
    assert!(has_map(compact_product(&model).table().owners()[0].body()));
}

#[test]
fn compact_node_without_binders_refuses_by_name_on_every_profile() {
    // A rank-zero domain is a single point with no ordinal; it refuses at
    // admission, so neither profile reaches a construction error.
    let mut node = cube_node(1, 0);
    if let ComputeNode::Map {
        domain,
        output_map,
        load_strides,
        ..
    } = &mut node
    {
        domain.binders.clear();
        output_map.strides.clear();
        load_strides.clear();
    }
    let model = n_state_model(1, ComputeBlock { nodes: vec![node] });
    for result in [
        ModelStateJacobian::construct(&model),
        ModelStateJacobian::construct_for_target(&model, &map_profile()),
    ] {
        assert!(matches!(
            result,
            Err(ModelStateJacobianError::Unsupported {
                reason: "compact domain has no binders",
                ..
            })
        ));
    }
}
