use super::nested_guard::call_native;
use super::*;

fn indexed_row(index: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::Const {
            dst: 0,
            value: 10.0,
        },
        LinearOp::Const {
            dst: 1,
            value: index,
        },
        LinearOp::LoadIndexedRegister {
            dst: 2,
            base: 0,
            stride: 1,
            dimensions: vec![1].into_boxed_slice(),
            indices: vec![rumoca_ir_solve::TensorIndex::Runtime(1)].into_boxed_slice(),
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn protected_row() -> Vec<LinearOp> {
    let nested = rumoca_ir_solve::FunctionFoldProgram::checked(
        super::nested_guard::domain(0, "inner"),
        1,
        0,
        indexed_row(2.0),
    )
    .expect("checked inner fold retains a known invalid runtime index");
    let outer = rumoca_ir_solve::FunctionFoldProgram::checked(
        super::nested_guard::domain(1, "outer"),
        1,
        1,
        vec![
            LinearOp::LoadFoldCapture { dst: 0, index: 0 },
            LinearOp::StoreOutputFunctionFold {
                initial: Box::new([rumoca_ir_solve::FoldInitialSource::ParentCarried {
                    base: 0,
                    count: 1,
                }]),
                capture_start: 0,
                program: std::sync::Arc::new(nested),
                result_base: 0,
                count: 1,
                condition: Some(0),
                nested_when_true: true,
            },
        ],
    )
    .expect("checked outer fold retains a live true-arm condition");
    vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 3.0 },
        LinearOp::FunctionFold {
            dst_start: 2,
            initial_start: 1,
            capture_start: 0,
            program: std::sync::Arc::new(outer),
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn assert_reference_fault(error: rumoca_eval_solve::EvalSolveError) {
    assert!(
        matches!(
            error,
            rumoca_eval_solve::EvalSolveError::TensorIndexOutOfBounds {
                axis: 1,
                index: 2,
                extent: 1,
                ..
            }
        ),
        "{error:?}"
    );
}

#[test]
fn known_runtime_index_preserves_selected_fault_and_inactive_protection() {
    let valid = indexed_row(1.0);
    let native = compile_residual_rows(std::slice::from_ref(&valid)).unwrap();
    assert_eq!(
        rumoca_eval_solve::eval_row(&valid, &[], &[], 0.0, None).unwrap(),
        10.0
    );
    let mut out = [-123.0];
    call_native(&native, 0.0, &mut out).unwrap();
    assert_eq!(out, [10.0]);
    assert_eq!(native.jit_call_count(), 1);

    let protected = protected_row();
    let native = compile_residual_rows(std::slice::from_ref(&protected)).unwrap();
    assert_eq!(
        rumoca_eval_solve::eval_row(&protected, &[], &[0.0], 0.0, None).unwrap(),
        3.0
    );
    call_native(&native, 0.0, &mut out).unwrap();
    assert_eq!(out, [3.0], "inactive known-invalid access stays protected");
    assert_eq!(native.jit_call_count(), 1);
    assert_reference_fault(
        rumoca_eval_solve::eval_row(&protected, &[], &[1.0], 0.0, None).unwrap_err(),
    );

    let invalid = indexed_row(2.0);
    assert_reference_fault(rumoca_eval_solve::eval_row(&invalid, &[], &[], 0.0, None).unwrap_err());
    let direct = compile_residual_rows(std::slice::from_ref(&invalid)).unwrap();
    let error = call_native(&direct, 0.0, &mut out)
        .expect_err("a known runtime index must retain its selected fault");
    assert!(
        matches!(&error, CompileError::SourceOperation {
            kind: crate::NativeSourceFault::TensorIndex,
            message,
            span: None,
        } if message == "index 2 is outside 1..=1"),
        "{error:?}"
    );
    assert_eq!(direct.jit_call_count(), 1);
    let error = call_native(&native, 1.0, &mut out)
        .expect_err("a guarded known runtime index faults when selected");
    assert!(
        matches!(&error, CompileError::SourceOperation {
            kind: crate::NativeSourceFault::TensorIndex,
            message,
            span: None,
        } if message == "index 2 is outside 1..=1"),
        "{error:?}"
    );
    assert_eq!(native.jit_call_count(), 2);
}
