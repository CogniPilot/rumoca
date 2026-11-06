//! Compact derivative nodes through the actual native consumer: the selected
//! Cranelift profile admits no nested map frames, so preparation compiles the
//! scalarized form. It must equal the scalarized model's native product bit
//! for bit, or decline as unsupported; it never fails preparation.

use super::*;
use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    AffineStencilIndexStrideTerm, AffineStencilLoadStride, BinaryOp, ComputeBlock, ComputeNode,
    LinearOp, ScalarProgramBlock, SolveModel, TensorNodeMetadata, TensorOutputMap, UnaryOp,
};

fn states_model(states: usize) -> SolveModel {
    let text = format!(
        "model CompactStates\n  Real x[{states}](each start=0.5, each fixed=true);\nequation\n  der(x) = -x;\nend CompactStates;"
    );
    let mut session = Session::new(SessionConfig::default());
    session.add_document("compact_states.mo", &text).unwrap();
    let compiled = session.compile_model("CompactStates").unwrap();
    let model =
        crate::lower_dae_for_simulation(&compiled.dae, &crate::SimOptions::default()).unwrap();
    assert_eq!(model.state_scalar_count(), states);
    model
}

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("compact.mo"), 1, 2)
}

fn ordinal(upper: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "k".into(),
            lower: 1,
            upper,
            step: 1,
        }],
    }
}

fn unit_stride(op_position: usize) -> AffineStencilLoadStride {
    AffineStencilLoadStride {
        op_position,
        terms: vec![AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: 1,
        }],
    }
}

fn map(
    points: i64,
    start: usize,
    base_ops: Vec<LinearOp>,
    load_strides: Vec<AffineStencilLoadStride>,
) -> ComputeNode {
    ComputeNode::Map {
        domain: ordinal(points),
        output_map: TensorOutputMap {
            start,
            strides: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        },
        base_ops,
        load_strides,
        const_strides: vec![],
        metadata: TensorNodeMetadata::default(),
        span: span(),
    }
}

/// `dx[3] = y[3]·y[0]` as one scalar program, then the compact node
/// `dx[k] = y[k]·y[k+1] + exp(y[k])` for `k` in `0..3`.
fn neighbour_block() -> ComputeBlock {
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
    let mut block = ComputeBlock::from_scalar_program_block(scalar);
    block.nodes.push(map(
        3,
        0,
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
        vec![unit_stride(0), unit_stride(1)],
    ));
    block
}

fn with_derivative(mut model: SolveModel, block: ComputeBlock) -> SolveModel {
    model.problem.continuous.derivative_rhs = block;
    model
}

fn has_compact_node(model: &SolveModel) -> bool {
    model
        .problem
        .continuous
        .derivative_rhs
        .nodes
        .iter()
        .any(|node| matches!(node, ComputeNode::Map { .. }))
}

#[test]
fn compact_node_compiles_natively_and_matches_the_scalarized_model_bitwise() {
    let base = states_model(4);
    let compact = with_derivative(base.clone(), neighbour_block());
    let reference = with_derivative(
        base,
        ComputeBlock::from_scalar_program_block(
            rumoca_eval_solve::to_scalar_program_block(&neighbour_block()).unwrap(),
        ),
    );
    assert!(has_compact_node(&compact) && !has_compact_node(&reference));
    let parameters = compact.parameters.clone();
    let compact = match prepare(SolveExecutionSource::new(compact)).unwrap() {
        ModelStateJacobianPreparation::Compiled(code) => code,
        ModelStateJacobianPreparation::Unsupported { reason } => {
            panic!("an in-bound compact node must compile natively: {reason}")
        }
    };
    let reference = match prepare(SolveExecutionSource::new(reference)).unwrap() {
        ModelStateJacobianPreparation::Compiled(code) => code,
        ModelStateJacobianPreparation::Unsupported { reason } => panic!("{reason}"),
    };
    for state in [[0.5, -1.25, 2.0, 0.75], [-3.0, 0.0, 1e-3, 7.5], [1.0; 4]] {
        let mut got = [f64::NAN; 16];
        let mut want = [f64::NAN; 16];
        compact.call(&state, &parameters, 0.25, &mut got).unwrap();
        reference
            .call(&state, &parameters, 0.25, &mut want)
            .unwrap();
        assert_eq!(
            got.map(f64::to_bits),
            want.map(f64::to_bits),
            "state {state:?}"
        );
    }
    // An overflow at a point fails both products' finite guards alike and
    // publishes nothing: the caller's output is left exactly as it was. A
    // following good call publishes normally.
    let sentinel = [41.0; 16];
    let mut got = sentinel;
    let mut want = sentinel;
    // Point 0 overflows at its third operation: both products report the
    // scalar identity of program 1, operation 3, element 0, not some other
    // native failure.
    let poisoned = [800.0, 0.5, 0.5, 0.5];
    let failure = compact
        .call(&poisoned, &parameters, 0.0, &mut got)
        .unwrap_err()
        .to_string();
    let expected = reference
        .call(&poisoned, &parameters, 0.0, &mut want)
        .unwrap_err()
        .to_string();
    assert_eq!(failure, expected);
    assert!(
        failure.contains("nonfinite Primal { program: 1, operation: 3 } element 0"),
        "{failure}"
    );
    assert_eq!(got.map(f64::to_bits), sentinel.map(f64::to_bits));
    assert_eq!(want.map(f64::to_bits), sentinel.map(f64::to_bits));
    let good = [0.5, -1.25, 2.0, 0.75];
    compact.call(&good, &parameters, 0.0, &mut got).unwrap();
    reference.call(&good, &parameters, 0.0, &mut want).unwrap();
    assert_eq!(got.map(f64::to_bits), want.map(f64::to_bits));
}

#[test]
fn compact_nodes_over_the_native_bound_decline_without_failing_preparation() {
    // Two 150-point nodes of four operations exceed the Cranelift profile's
    // unrolled-code budget together, so preparation declines by name and the
    // directional derivative path stays in use.
    let cube = |start: usize| {
        map(
            150,
            start,
            vec![
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
            vec![unit_stride(0)],
        )
    };
    let model = with_derivative(
        states_model(300),
        ComputeBlock {
            nodes: vec![cube(0), cube(150)],
        },
    );
    match prepare(SolveExecutionSource::new(model)) {
        Ok(ModelStateJacobianPreparation::Unsupported { reason }) => {
            assert!(reason.contains("expansion bound"), "{reason}");
        }
        Ok(ModelStateJacobianPreparation::Compiled(_)) => {
            panic!("an over-bound compact product must not compile")
        }
        Err(error) => panic!("preparation must decline, not fail: {error}"),
    }
}
