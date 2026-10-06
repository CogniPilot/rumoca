use super::*;

fn domain(lower: i64, upper: i64, step: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".into(),
            lower,
            upper,
            step,
        }],
    }
}

fn fold(lower: i64, upper: i64, step: i64) -> LinearOp {
    let update = vec![
        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
        LinearOp::LoadFoldCapture { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let program =
        crate::FunctionFoldProgram::checked(domain(lower, upper, step), 1, 1, update).unwrap();
    LinearOp::FunctionFold {
        dst_start: 2,
        initial_start: 0,
        capture_start: 1,
        program: Arc::new(program),
    }
}

#[test]
fn full_empty_descending_fold_fixed_points_equal_legacy() {
    for (lower, upper, step) in [(1, 14400, 1), (1, 0, 1), (9, 1, -2)] {
        compare_program(&[
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadSeed { dst: 1, index: 7 },
            fold(lower, upper, step),
            LinearOp::StoreOutput { src: 2 },
        ]);
    }
}

fn capture(index: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadFunctionConditionalCapture { dst: 0, index },
        LinearOp::StoreOutput { src: 0 },
    ]
}

#[test]
fn conditional_condition_arms_fallback_correlations_equal_legacy() {
    let program = crate::FunctionConditionalProgram::checked(
        2,
        vec![1].into_boxed_slice(),
        [(capture(0), capture(1))],
        capture(0),
    )
    .unwrap();
    compare_program(&[
        LinearOp::LoadP { dst: 0, index: 3 },
        LinearOp::LoadY { dst: 1, index: 9 },
        LinearOp::FunctionConditional {
            dst_start: 2,
            capture_start: 0,
            program: Arc::new(program),
        },
        LinearOp::StoreOutput { src: 2 },
    ]);
}
