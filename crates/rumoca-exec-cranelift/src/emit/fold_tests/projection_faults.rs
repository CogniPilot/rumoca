use super::nested_guard::{call_native_with_index, domain};
use super::*;

#[derive(Clone, Copy, Debug)]
enum Storage {
    Register,
    Carried,
    Capture,
}

fn row(storage: Storage, known_index: Option<f64>) -> Vec<LinearOp> {
    let dimensions = Box::new([1]);
    let indices = Box::new([rumoca_ir_solve::TensorIndex::Runtime(1)]);
    let projection = match storage {
        Storage::Register => LinearOp::LoadIndexedRegister {
            dst: 2,
            base: 0,
            stride: 1,
            dimensions,
            indices,
        },
        Storage::Carried => LinearOp::LoadIndexedFoldCarried {
            dst: 2,
            base: 0,
            stride: 1,
            dimensions,
            indices,
        },
        Storage::Capture => LinearOp::LoadIndexedFoldCapture {
            dst: 2,
            base: 1,
            stride: 1,
            dimensions,
            indices,
        },
    };
    let index = match known_index {
        Some(value) => LinearOp::Const { dst: 1, value },
        None => LinearOp::LoadFoldCapture { dst: 1, index: 0 },
    };
    let program = rumoca_ir_solve::FunctionFoldProgram::checked(
        domain(0, "projection"),
        1,
        2,
        vec![
            LinearOp::Const {
                dst: 0,
                value: 10.0,
            },
            index,
            projection,
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("checked packed, carried, and captured aggregates retain runtime indexing");
    vec![
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::Const {
            dst: 1,
            value: 10.0,
        },
        LinearOp::FunctionFold {
            dst_start: 2,
            initial_start: 1,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn check_call(row: &[LinearOp], native: &CompiledResidualRows, index: f64) {
    let reference = rumoca_eval_solve::eval_row(row, &[], &[0.0, index], 0.0, None);
    let mut output = [-123.0];
    let actual = call_native_with_index(native, 0.0, index, &mut output);
    if index == 1.0 {
        assert_eq!(reference.unwrap(), 10.0);
        actual.unwrap();
        assert_eq!(output, [10.0]);
        return;
    }
    let error = reference.unwrap_err();
    if index == 0.0 || index == 2.0 {
        assert!(
            matches!(error, rumoca_eval_solve::EvalSolveError::TensorIndexOutOfBounds {
            axis: 1, extent: 1, index: actual, ..
        } if actual as f64 == index),
            "{error:?}"
        );
    } else {
        assert!(
            matches!(error, rumoca_eval_solve::EvalSolveError::InvalidTensorIndex {
            axis: 1, value, ..
        } if value == index || (value.is_nan() && index.is_nan())),
            "{error:?}"
        );
    }
    assert!(
        matches!(actual, Err(CompileError::Input(ref message))
        if message == "native tensor index is out of bounds"),
        "{actual:?}"
    );
    assert_eq!(output, [-123.0], "fault returns before scalar publication");
}

#[test]
fn known_indices_preserve_faults_for_every_fold_projection_storage() {
    for storage in [Storage::Register, Storage::Carried, Storage::Capture] {
        for index in [1.0, 0.0, 2.0, 1.5, f64::NAN, f64::INFINITY] {
            let row = row(storage, Some(index));
            let native = compile_residual_rows(std::slice::from_ref(&row)).unwrap();
            check_call(&row, &native, index);
            assert_eq!(native.jit_call_count(), 1, "{storage:?}, {index}");
        }
    }
}

#[test]
fn live_indices_preserve_faults_for_every_fold_projection_storage() {
    for storage in [Storage::Register, Storage::Carried, Storage::Capture] {
        let row = row(storage, None);
        let native = compile_residual_rows(std::slice::from_ref(&row)).unwrap();
        for index in [1.0, 0.0, 2.0, 1.5, f64::NAN, f64::INFINITY] {
            check_call(&row, &native, index);
        }
        assert_eq!(native.jit_call_count(), 6, "{storage:?}");
    }
}
