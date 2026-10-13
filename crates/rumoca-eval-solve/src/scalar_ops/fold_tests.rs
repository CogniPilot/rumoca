//! Counted folds unrolled into straight-line scalar ops keep the fold's value.

use super::*;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    FunctionFoldProgram, ScalarProgramBlock, TensorConcatenateSource, TensorIndex,
};
use std::sync::Arc;

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("scalar_fold.mo"),
        0,
        1,
    )
}

/// Sums `capture * i` over `i in 1..=3`. The body also holds a dead constant,
/// a binder copy, a concatenation and a runtime-subscripted projection of it.
fn weighted_sum_fold() -> Arc<FunctionFoldProgram> {
    let update = vec![
        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
        LinearOp::LoadFoldCapture { dst: 1, index: 0 },
        LinearOp::LoadFoldIndex {
            dst: 2,
            dimension: 0,
        },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::Const {
            dst: 5,
            value: 99.0,
        },
        LinearOp::Move { dst: 6, src: 2 },
        LinearOp::TensorConcatenate {
            dst_start: 7,
            sources: vec![
                TensorConcatenateSource {
                    start: 1,
                    dimensions: vec![1].into_boxed_slice(),
                },
                TensorConcatenateSource {
                    start: 4,
                    dimensions: vec![1].into_boxed_slice(),
                },
            ]
            .into_boxed_slice(),
            dimensions: vec![2].into_boxed_slice(),
            axis: 0,
            lanes: 1,
        },
        LinearOp::Const { dst: 9, value: 2.0 },
        LinearOp::LoadIndexedRegister {
            dst: 10,
            base: 7,
            stride: 1,
            dimensions: vec![2].into_boxed_slice(),
            indices: vec![TensorIndex::Runtime(9)].into_boxed_slice(),
        },
        LinearOp::StoreOutput { src: 10 },
    ];
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    };
    Arc::new(FunctionFoldProgram::checked(domain, 1, 1, update).expect("a checked fold"))
}

fn run(ops: &[LinearOp], parameters: &[f64]) -> f64 {
    let block = ScalarProgramBlock::with_output_indices(vec![ops.to_vec()], vec![span()], vec![0])
        .expect("a checked scalar program");
    let mut out = [f64::NAN];
    crate::eval_scalar_program_block(&block, &[], parameters, 0.0, None, &mut out).unwrap();
    out[0]
}

fn program(guarded: bool) -> Vec<LinearOp> {
    let program = weighted_sum_fold();
    let fold = if guarded {
        LinearOp::GuardedFunctionFold {
            dst_start: 3,
            initial_start: 0,
            capture_start: 1,
            activation: 2,
            program,
        }
    } else {
        LinearOp::FunctionFold {
            dst_start: 3,
            initial_start: 0,
            capture_start: 1,
            program,
        }
    };
    vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::LoadP { dst: 2, index: 1 },
        fold,
        LinearOp::StoreOutput { src: 3 },
    ]
}

#[test]
fn unrolled_fold_matches_the_fold_op_and_leaves_no_fold_behind() {
    let canonical = program(false);
    let lowered = lower_scalar_ops(&canonical, None).expect("a counted fold unrolls");
    assert!(
        lowered
            .iter()
            .all(|op| !matches!(op, LinearOp::FunctionFold { .. })),
        "{lowered:?}"
    );
    assert!(
        !lowered
            .iter()
            .any(|op| matches!(op, LinearOp::Const { value, .. } if *value == 99.0)),
        "the unread constant of the body is dropped"
    );
    for capture in [2.0, -0.5] {
        let expected = 1.0 + capture * 6.0;
        assert_eq!(run(&canonical, &[capture, 0.0]), expected);
        assert_eq!(run(&lowered, &[capture, 0.0]), expected);
    }
}

#[test]
fn unrolled_guarded_fold_keeps_the_initial_tuple_while_inactive() {
    let canonical = program(true);
    let lowered = lower_scalar_ops(&canonical, None).expect("a guarded fold unrolls");
    for (activation, expected) in [(1.0, 13.0), (0.0, 1.0)] {
        assert_eq!(run(&canonical, &[2.0, activation]), expected);
        assert_eq!(run(&lowered, &[2.0, activation]), expected);
    }
}

#[test]
fn a_fold_with_a_continuation_predicate_is_not_unrolled() {
    let base = weighted_sum_fold();
    let predicate = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let looping = Arc::new((*base).clone().with_continuation(predicate).unwrap());
    let ops = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::FunctionFold {
            dst_start: 2,
            initial_start: 0,
            capture_start: 1,
            program: looping,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    assert!(lower_scalar_ops(&ops, None).is_none());
}
