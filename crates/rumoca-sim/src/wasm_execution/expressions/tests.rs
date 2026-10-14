mod authored_models;
mod conditional_ranges;
mod linear_solve;
mod observations;
mod outer_scalar_fold;
mod tuples;
use super::*;
use rumoca_core::{SourceId, Span};
use rumoca_ir_solve::LinearOp;
use rumoca_solver::SolveExecutionBackend;

#[test]
fn wasm_backend_rejects_a_foreign_call_table_during_context_admission() {
    let model = rumoca_ir_solve::SolveModel::default();
    let backend = WasmExecutionBackend {
        layout: model.problem.layout.clone(),
        calls: model.pure_calls.clone(),
    };
    assert!(backend.validate_model_context(&model).is_ok());
    assert!(backend.validate_model_context(&model.clone()).is_ok());
    let foreign = rumoca_ir_solve::SolveModel::default();
    assert_eq!(foreign.pure_calls, model.pure_calls);
    assert!(!foreign.pure_calls.shares_table_owner(&model.pure_calls));
    assert!(
        backend.validate_model_context(&foreign).is_err(),
        "portable code must remain bound to its original call-table owner"
    );
    let error =
        rumoca_solver::SolveRuntime::new_with_execution_backend(&foreign, Some(Rc::new(backend)))
            .err()
            .expect("foreign portable context must reject runtime construction");
    assert!(matches!(
        error,
        rumoca_eval_solve::EvalSolveError::CompiledExecution { .. }
    ));
}

#[test]
fn wasm_backend_rejects_a_changed_layout_with_the_original_call_owner() {
    let model = rumoca_ir_solve::SolveModel::default();
    let backend = WasmExecutionBackend {
        layout: model.problem.layout.clone(),
        calls: model.pure_calls.clone(),
    };
    let mut changed = model.clone();
    changed.problem.layout = VarLayout::from_parts(Default::default(), 1, 0);
    assert!(changed.pure_calls.shares_table_owner(&model.pure_calls));
    assert!(backend.validate_model_context(&changed).is_err());
}

#[test]
fn wasm_backend_selection_retains_the_declared_engine_for_a_supported_model() {
    let mut compiler = rumoca_compile::Session::new(Default::default());
    compiler.update_document(
        "input.mo",
        "model Supported Real x(start=1, fixed=true); equation der(x)=-x; end Supported;",
    );
    let compiled = compiler.compile_model("Supported").unwrap();
    let options = rumoca_solver::SimOptions::default();
    let lowered =
        crate::lower_correlated_for_simulation_with_overrides(&compiled.dae, &options).unwrap();
    assert_eq!(
        execution_receipt(&options, lowered.model()).engine,
        rumoca_solver::SimExecutionEngine::WasmProgram
    );
    assert!(
        admitted_native_execution_backend(&options, lowered.model())
            .unwrap()
            .is_some()
    );
    let backend = Rc::new(WasmExecutionBackend {
        layout: lowered.model().problem.layout.clone(),
        calls: lowered.model().pure_calls.clone(),
    });
    rumoca_solver::SolveRuntime::new_with_execution_backend(lowered.model(), Some(backend))
        .expect("the portable backend must prepare the entire supported model");
}

#[test]
fn selectable_expression_refuses_an_unlowered_program_before_any_execution() {
    let span = Span::from_offsets(SourceId::from_source_name("cold_program.mo"), 3, 17);
    let source = ScalarProgramBlock::with_program_spans(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 4.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::LoadIndexedP {
                    dst: 1,
                    base: 0,
                    count: 1,
                    index: 0,
                },
                LinearOp::StoreOutput { src: 1 },
            ],
        ],
        vec![span; 2],
    )
    .unwrap();
    let layout = VarLayout::from_parts(Default::default(), 2, 1);
    assert!(
        WasmExpression::compile(
            &source,
            &layout,
            &rumoca_ir_solve::SolvePureCallTable::default()
        )
        .is_err(),
        "a runnable expression must not retain unlowered cold programs"
    );
}

#[test]
fn complete_expression_owns_code_for_every_original_program() {
    let span = Span::from_offsets(SourceId::from_source_name("supported_programs.mo"), 3, 17);
    let source = ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                LinearOp::Const {
                    dst: 0,
                    value: -0.0,
                },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 9.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        vec![span; 2],
        vec![7, 2],
    )
    .unwrap();
    let layout = VarLayout::from_parts(Default::default(), 2, 1);
    let compiled = WasmExpression::compile(
        &source,
        &layout,
        &rumoca_ir_solve::SolvePureCallTable::default(),
    )
    .unwrap();
    assert_eq!(compiled.selected.len(), source.row_count());
    assert_eq!(compiled.source.output_indices(), source.output_indices());
    assert!(
        compiled
            .selected
            .iter()
            .all(|entry| !entry.0.module_bytes().is_empty())
    );
}
