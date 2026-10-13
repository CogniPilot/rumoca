use super::*;
use crate::{CompareOp, FunctionConditionalProgram, TensorInputKind};

fn load(index: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadP { dst: 0, index },
        LinearOp::StoreOutput { src: 0 },
    ]
}

fn relation() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 0.5 },
        LinearOp::Compare {
            dst: 2,
            op: CompareOp::Lt,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn conditional(guard: Vec<LinearOp>, raw: Vec<LinearOp>, fallback: Vec<LinearOp>) -> LinearOp {
    LinearOp::FunctionConditional {
        dst_start: 0,
        capture_start: 0,
        program: std::sync::Arc::new(
            FunctionConditionalProgram::checked(0, vec![1], vec![(guard, raw)], fallback).unwrap(),
        ),
    }
}

fn admitted(ops: &[LinearOp]) -> bool {
    let mut reads = ModeReads {
        flag: 7,
        memory: &[8],
        seed_offset: 0,
        total: 0,
        guards: 0,
        unknown: false,
    };
    reads
        .visit_linear_op_slice(
            LinearOpSliceKind::ScalarProgram {
                program_index: 0,
                span: None,
            },
            ops,
        )
        .unwrap();
    !reads.unknown && reads.total == reads.guards
}

#[test]
fn only_relation_wrappers_own_allowed_mode_reads() {
    let wrapper = conditional(load(7), relation(), load(8));
    assert!(admitted(std::slice::from_ref(&wrapper)));
    let mut tangent_guard = load(7);
    tangent_guard.insert(1, LinearOp::LoadSeed { dst: 1, index: 7 });
    assert!(admitted(&[conditional(
        tangent_guard.clone(),
        relation(),
        load(8)
    )]));
    tangent_guard[1] = LinearOp::LoadSeed { dst: 0, index: 7 };
    assert!(!admitted(&[conditional(
        tangent_guard,
        relation(),
        load(8)
    )]));
    for extra in [
        LinearOp::LoadP { dst: 1, index: 7 },
        LinearOp::LoadIndexedP {
            dst: 1,
            base: 6,
            count: 3,
            index: 0,
        },
        LinearOp::TensorLoad {
            dst_start: 1,
            input: TensorInputKind::P,
            input_start: 6,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        conditional(load(7), relation(), load(6)),
        conditional(relation(), load(7), load(8)),
        conditional(load(7), load(8), load(8)),
    ] {
        assert!(!admitted(&[wrapper.clone(), extra]));
    }
    let nested = conditional(
        relation(),
        vec![wrapper, LinearOp::StoreOutput { src: 0 }],
        load(8),
    );
    assert!(admitted(&[nested]));
    let nested_authored = conditional(relation(), load(7), load(8));
    let extra = conditional(
        load(7),
        vec![nested_authored, LinearOp::StoreOutput { src: 0 }],
        load(8),
    );
    assert!(!admitted(&[extra]));
}

#[test]
fn compact_parameter_strides_cannot_hide_a_mode_read() {
    use crate::{
        AffineStencilIndexStrideTerm, AffineStencilLoadStride, ComputeNode, TensorNodeMetadata,
        TensorOutputMap,
    };
    use rumoca_core::{IndexBinderProvenance, Span, StructuredIndexBinder, StructuredIndexDomain};
    for (base, count, strides, expected) in [
        (6, 2, vec![1], false),
        (8, 2, vec![-1], false),
        (6, 1_000_000_000, vec![1, -1], true),
    ] {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: IndexBinderProvenance::Generated,
                id: 0,
                display_name: "i".into(),
                lower: 1,
                upper: count,
                step: 1,
            }],
        };
        let node = ComputeNode::Map {
            output_map: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
            domain,
            base_ops: load(base),
            load_strides: strides
                .into_iter()
                .map(|stride| AffineStencilLoadStride {
                    op_position: 0,
                    terms: vec![AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride,
                    }],
                })
                .collect(),
            const_strides: vec![],
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        };
        let mut reads = ModeReads {
            flag: 7,
            memory: &[8],
            seed_offset: 0,
            total: 0,
            guards: 0,
            unknown: false,
        };
        reads.visit_compute_node(0, &node).unwrap();
        assert_eq!(!reads.unknown && reads.total == reads.guards, expected);
    }
}

#[test]
fn dual_relation_outputs_retain_exact_primal_and_tangent_writers() {
    let guard = vec![
        LinearOp::LoadP { dst: 0, index: 7 },
        LinearOp::LoadSeed { dst: 1, index: 7 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let raw = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadSeed { dst: 1, index: 0 },
        LinearOp::Const { dst: 2, value: 0.5 },
        LinearOp::Const { dst: 3, value: 0.0 },
        LinearOp::Compare {
            dst: 4,
            op: CompareOp::Lt,
            lhs: 0,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::StoreOutput { src: 3 },
    ];
    let fallback = vec![
        LinearOp::LoadP { dst: 0, index: 8 },
        LinearOp::LoadSeed { dst: 1, index: 8 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];
    let check = |guard: Vec<LinearOp>, raw: Vec<LinearOp>, fallback: Vec<LinearOp>| {
        let program =
            FunctionConditionalProgram::checked(0, vec![2], vec![(guard, raw)], fallback).unwrap();
        admitted(&[LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        }])
    };
    assert!(check(guard.clone(), raw.clone(), fallback.clone()));
    let mut constant_guard = guard.clone();
    constant_guard[1] = LinearOp::Const { dst: 1, value: 0.0 };
    let mut constant_fallback = fallback.clone();
    constant_fallback[1] = LinearOp::Const { dst: 1, value: 0.0 };
    assert!(check(
        constant_guard.clone(),
        raw.clone(),
        constant_fallback.clone()
    ));
    for value in [1.0, -0.0] {
        let mut forged = constant_guard.clone();
        forged[1] = LinearOp::Const { dst: 1, value };
        assert!(!check(forged, raw.clone(), constant_fallback.clone()));
        let mut forged = constant_fallback.clone();
        forged[1] = LinearOp::Const { dst: 1, value };
        assert!(!check(constant_guard.clone(), raw.clone(), forged));
    }
    constant_guard[1] = LinearOp::Const { dst: 0, value: 0.0 };
    assert!(!check(
        constant_guard,
        raw.clone(),
        constant_fallback.clone()
    ));
    constant_fallback[1] = LinearOp::Const { dst: 0, value: 0.0 };
    constant_fallback[3] = LinearOp::StoreOutput { src: 0 };
    assert!(!check(guard.clone(), raw.clone(), constant_fallback));
    for overwrite in [
        LinearOp::Const { dst: 3, value: 1.0 },
        LinearOp::Const {
            dst: 3,
            value: -0.0,
        },
        LinearOp::TensorFill {
            dst_start: 2,
            value_start: 0,
            count: 2,
            lanes: 1,
        },
        LinearOp::TensorFill {
            dst_start: 3,
            value_start: 0,
            count: 2,
            lanes: 1,
        },
        LinearOp::LoadP { dst: 5, index: 7 },
    ] {
        let mut forged = raw.clone();
        forged.insert(5, overwrite);
        assert!(!check(guard.clone(), forged, fallback.clone()));
    }
    let mut wrong_seed = fallback.clone();
    wrong_seed[1] = LinearOp::LoadSeed { dst: 1, index: 9 };
    assert!(!check(guard.clone(), raw.clone(), wrong_seed));
    let mut aliased_seed = fallback.clone();
    aliased_seed[1] = LinearOp::LoadSeed { dst: 0, index: 8 };
    aliased_seed[3] = LinearOp::StoreOutput { src: 0 };
    assert!(!check(guard, raw, aliased_seed));
}
