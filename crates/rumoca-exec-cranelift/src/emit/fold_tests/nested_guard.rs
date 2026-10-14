use super::*;

pub(super) fn domain(id: usize, name: &str) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
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
        1,
        vec![
            LinearOp::Const {
                dst: 0,
                value: 10.0,
            },
            LinearOp::LoadFoldCapture { dst: 1, index: 0 },
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
    wrap_nested(nested, nested_when_true)
}

fn valid_guarded_row(nested_when_true: bool) -> Vec<LinearOp> {
    let nested = rumoca_ir_solve::FunctionFoldProgram::checked(
        domain(0, "inner"),
        1,
        1,
        vec![
            LinearOp::Const {
                dst: 0,
                value: 10.0,
            },
            LinearOp::LoadFoldCapture { dst: 1, index: 0 },
            LinearOp::LoadIndexedRegister {
                dst: 2,
                base: 0,
                stride: 1,
                dimensions: vec![1].into_boxed_slice(),
                indices: vec![rumoca_ir_solve::TensorIndex::Runtime(1)].into_boxed_slice(),
            },
            LinearOp::LoadFoldCarried { dst: 3, index: 0 },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Add,
                lhs: 3,
                rhs: 2,
            },
            LinearOp::StoreOutput { src: 4 },
        ],
    )
    .expect("checked valid-index branch adds ten to its initial carried value");
    wrap_nested(nested, nested_when_true)
}

fn wrap_nested(
    nested: rumoca_ir_solve::FunctionFoldProgram,
    nested_when_true: bool,
) -> Vec<LinearOp> {
    let outer = rumoca_ir_solve::FunctionFoldProgram::checked(
        domain(1, "outer"),
        1,
        2,
        vec![
            LinearOp::LoadFoldCapture { dst: 0, index: 0 },
            LinearOp::LoadFoldCapture { dst: 1, index: 1 },
            LinearOp::StoreOutputFunctionFold {
                initial: Box::new([rumoca_ir_solve::FoldInitialSource::ParentCarried {
                    base: 0,
                    count: 1,
                }]),
                capture_start: 1,
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
        LinearOp::LoadP { dst: 1, index: 1 },
        LinearOp::Const { dst: 2, value: 3.0 },
        LinearOp::FunctionFold {
            dst_start: 3,
            initial_start: 2,
            capture_start: 0,
            program: std::sync::Arc::new(outer),
        },
        LinearOp::StoreOutput { src: 3 },
    ]
}

// The normal test call runs an interpreter precheck, which can refuse an active
// access before reaching native code. Invoke the issued native kernel directly
// after the same input/output admission, keeping the independent reference above.
pub(super) fn call_native(
    compiled: &CompiledResidualRows,
    condition: f64,
    out: &mut [f64],
) -> Result<(), CompileError> {
    call_native_with_index(compiled, condition, 2.0, out)
}

pub(super) fn call_native_with_index(
    compiled: &CompiledResidualRows,
    condition: f64,
    index: f64,
    out: &mut [f64],
) -> Result<(), CompileError> {
    let p = [condition, index];
    call_native_with_parameters(compiled, &p, out)
}

pub(super) fn call_native_with_parameters(
    compiled: &CompiledResidualRows,
    p: &[f64],
    out: &mut [f64],
) -> Result<(), CompileError> {
    validate_input_requirements(compiled.input_requirements, &[], p, None)?;
    assert_eq!(
        compiled.jits.len(),
        1,
        "one compact source row has one issued native kernel"
    );
    let jit = compiled
        .jits
        .first()
        .expect("the issued row has its native kernel");
    validate_output_len(out, jit.output_count)?;
    assert_eq!(jit.output_start, 0);
    let mut registers = Vec::new();
    // SAFETY: the original checked row was compiled by the production issuer;
    // its exact input and output slices were admitted above. The retained module
    // owns this kernel, and the production wrapper allocates its register tape.
    let result = unsafe { call_residual_jit(jit, &mut registers, &[], p, 0.0, out) };
    compiled.record_jit_call();
    status::check(result)
}

#[test]
fn nested_fold_guard_preserves_both_polarities_and_selected_access_faults() {
    for (nested_when_true, expected) in [(true, [3.0, 13.0]), (false, [13.0, 3.0])] {
        let row = valid_guarded_row(nested_when_true);
        let compiled = compile_residual_rows(std::slice::from_ref(&row)).unwrap();
        for (condition, expected) in [0.0, 1.0].into_iter().zip(expected) {
            assert_eq!(
                rumoca_eval_solve::eval_row(&row, &[], &[condition, 1.0], 0.0, None).unwrap(),
                expected,
                "canonical selected branch: polarity={nested_when_true}, condition={condition}"
            );
            let mut out = [-123.0];
            call_native_with_index(&compiled, condition, 1.0, &mut out).unwrap();
            assert_eq!(
                out,
                [expected],
                "native selected branch: polarity={nested_when_true}, condition={condition}"
            );
        }
        assert_eq!(compiled.jit_call_count(), 2);
    }
    for nested_when_true in [true, false] {
        let row = guarded_row(nested_when_true);
        let compiled = compile_residual_rows(std::slice::from_ref(&row))
            .expect("compile the original checked compact fold witness");
        let inactive = f64::from(!nested_when_true);
        let active = f64::from(nested_when_true);
        assert_eq!(
            rumoca_eval_solve::eval_row(&row, &[], &[inactive, 2.0], 0.0, None).unwrap(),
            3.0,
            "canonical inactive arm preserves its initial tuple"
        );
        let reference = rumoca_eval_solve::eval_row(&row, &[], &[active, 2.0], 0.0, None)
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
        let error = call_native(&compiled, active, &mut out)
            .expect_err("native selected access preserves its fault");
        assert!(
            error.to_string().contains("index is out of bounds"),
            "{error:?}"
        );
        call_native(&compiled, inactive, &mut out)
            .expect("native inactive arm must never access the invalid element");
        assert_eq!(out, [3.0], "nested_when_true={nested_when_true}");
        assert_eq!(compiled.jit_call_count(), 2);
    }
}
