use super::*;
use rumoca_ir_solve::TensorInputKind;

fn load(input: TensorInputKind, lanes: usize) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start: 0,
        input,
        input_start: 2,
        count: 3,
        seed_start: Some(4),
        lanes,
    }
}

// Before-fix proof reads metadata only: never call an undersized native pointer.
#[test]
fn native_tensor_load_contract_covers_primal_and_dual_input_windows() {
    for (input, y_len, p_len) in [(TensorInputKind::Y, 5, 0), (TensorInputKind::P, 0, 5)] {
        for (lanes, seed_len) in [(1, 0), (2, 7)] {
            let requirements = input_requirements_for_linear_ops(&[load(input, lanes)]).unwrap();
            assert_eq!(
                requirements,
                InputRequirements {
                    y_len,
                    p_len,
                    seed_len
                }
            );
        }
    }
}

#[test]
fn native_tensor_load_contract_rejects_overflow_before_code_emission() {
    let operation = LinearOp::TensorLoad {
        dst_start: 0,
        input: TensorInputKind::Y,
        input_start: usize::MAX,
        count: 2,
        seed_start: None,
        lanes: 1,
    };
    let error = input_requirements_for_linear_ops(&[operation]).unwrap_err();
    assert!(
        matches!(error, CompileError::Backend(ref message)
        if message.contains("y indexed input requirement overflow")),
        "{error}"
    );
}

#[test]
fn native_tensor_load_plan_publishes_the_guard_before_any_runtime_call() {
    let row = vec![
        load(TensorInputKind::P, 2),
        LinearOp::StoreOutputRange {
            start: 0,
            count: 6,
            stride: 1,
        },
    ];
    let plan = super::super::plan_row(&row).unwrap();
    assert_eq!(
        row_input_requirements(&plan),
        InputRequirements {
            y_len: 0,
            p_len: 5,
            seed_len: 7,
        }
    );
    let compiled = super::super::compile_jacobian_rows(&[row]).unwrap();
    assert_eq!(
        compiled.input_requirements(),
        InputRequirements {
            y_len: 0,
            p_len: 5,
            seed_len: 7,
        }
    );
}
