use super::*;

fn domain(id: u32, name: &str) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id,
            display_name: name.into(),
            lower: 1,
            upper: 1,
            step: 1,
        }],
    }
}

fn guarded_row(nested_when_true: bool) -> Vec<LinearOp> {
    let nested = rumoca_ir_solve::FunctionFoldProgram::checked(
        domain(0, "inner"),
        1,
        0,
        vec![
            LinearOp::Const {
                dst: 0,
                value: 10.0,
            },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::LoadIndexedRegister {
                dst: 2,
                base: 0,
                stride: 1,
                dimensions: vec![1].into_boxed_slice(),
                indices: vec![rumoca_ir_solve::TensorIndex::Runtime(1)].into_boxed_slice(),
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("checked inner fold retains the invalid access as a runtime fault");
    let outer = rumoca_ir_solve::FunctionFoldProgram::checked(
        domain(1, "outer"),
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
                nested_when_true,
            },
        ],
    )
    .expect("checked outer fold stores exactly its carried tuple");
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

#[test]
fn nested_fold_guard_preserves_both_polarities_and_selected_access_faults() {
    for nested_when_true in [true, false] {
        let row = guarded_row(nested_when_true);
        let compiled = compile_residual_rows(std::slice::from_ref(&row))
            .expect("compile the original checked compact fold witness");
        let inactive = f64::from(!nested_when_true);
        let active = f64::from(nested_when_true);
        assert_eq!(
            rumoca_eval_solve::eval_row(&row, &[], &[inactive], 0.0, None).unwrap(),
            3.0,
            "canonical inactive arm preserves its initial tuple"
        );
        let reference = rumoca_eval_solve::eval_row(&row, &[], &[active], 0.0, None)
            .expect_err("canonical selected access faults");
        assert!(
            matches!(
                reference,
                rumoca_eval_solve::EvalSolveError::TensorIndexOutOfBounds {
                    axis: 1,
                    index: 2,
                    extent: 1,
                    ..
                }
            ),
            "{reference:?}"
        );
        let mut out = [-123.0];
        let error = compiled
            .call(&[], &[active], 0.0, &mut out)
            .expect_err("native selected access preserves its fault");
        assert!(
            error.to_string().contains("index is out of bounds"),
            "{error:?}"
        );
        compiled
            .call(&[], &[inactive], 0.0, &mut out)
            .expect("native inactive arm must never access the invalid element");
        assert_eq!(out, [3.0], "nested_when_true={nested_when_true}");
        assert_eq!(compiled.jit_call_count(), 2);
    }
}
