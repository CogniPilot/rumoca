//! Preserve constructor-issued source status categories before ME recovery.
use crate::RuntimeSolveError;
use rumoca_exec_wasm::{TypedCallFaultKind as Kind, WasmCompileError};

pub fn execution_error(error: WasmCompileError) -> RuntimeSolveError {
    match error {
        WasmCompileError::LinearSolveSource(fault) => {
            rumoca_eval_solve::EvalSolveError::LinearSolve {
                size: fault.size,
                component: Some(fault.component),
                reason: "singular matrix",
                span: Some(fault.provenance),
            }
            .into()
        }
        WasmCompileError::TypedSource(fault) => {
            let message = format!("WASM source operation fault: {fault:?}");
            match fault.kind {
                Kind::InvalidBuffer => message.into(),
                Kind::AssertionFailed | Kind::ObservationCapacity | Kind::ObservationIntegrity => {
                    RuntimeSolveError::CompiledExecution {
                        context: "WASM checked effect",
                        message,
                        source_spans: Box::new([fault.provenance]),
                    }
                }
                Kind::IndexBounds => RuntimeSolveError::SourceFault {
                    message,
                    span: Some(fault.provenance),
                },
                Kind::InvalidInput | Kind::IntegerArithmetic | Kind::IntegerConversion => {
                    RuntimeSolveError::solve_ir_with_span(message, Some(fault.provenance))
                }
            }
        }
        WasmCompileError::GatherSource(fault) => {
            let message = format!("WASM source gather fault: {fault:?}");
            match fault.kind {
                Kind::IndexBounds | Kind::IntegerConversion => RuntimeSolveError::SourceFault {
                    message,
                    span: Some(fault.provenance),
                },
                Kind::InvalidBuffer => message.into(),
                Kind::AssertionFailed | Kind::ObservationCapacity | Kind::ObservationIntegrity => {
                    RuntimeSolveError::CompiledExecution {
                        context: "WASM checked effect",
                        message,
                        source_spans: Box::new([fault.provenance]),
                    }
                }
                Kind::InvalidInput | Kind::IntegerArithmetic => {
                    RuntimeSolveError::solve_ir_with_span(message, Some(fault.provenance))
                }
            }
        }
        error => error.to_string().into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{SourceId, Span};

    #[test]
    fn issued_gather_fault_retains_source_category_and_exact_provenance() {
        let span = Span::from_offsets(SourceId::from_source_name("status-source.mo"), 11, 17);
        for kind in [Kind::IndexBounds, Kind::IntegerConversion] {
            let fault = rumoca_exec_wasm::NativeGatherFault {
                status: 19,
                kind,
                kernel: 2,
                program: 3,
                operation: 4,
                region_path: vec![(5, 6)],
                provenance: span,
            };
            let error = execution_error(WasmCompileError::GatherSource(fault));
            assert!(matches!(&error, RuntimeSolveError::SourceFault { .. }));
            assert_eq!(error.source_span(), Some(span));
            assert!(error.to_string().contains("operation: 4"));
        }
        assert!(matches!(
            execution_error(WasmCompileError::Backend("actual engine trap".into())),
            RuntimeSolveError::CompiledExecution { .. }
        ));
    }
}
