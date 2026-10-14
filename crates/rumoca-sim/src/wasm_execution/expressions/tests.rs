use super::*;
use rumoca_core::{SourceId, Span};
use rumoca_ir_solve::LinearOp;

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
        WasmExpression::compile(&source, &layout).is_err(),
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
    let compiled = WasmExpression::compile(&source, &layout).unwrap();
    assert_eq!(compiled.selected.len(), source.row_count());
    assert_eq!(compiled.full.rows(), source.row_count());
    assert_eq!(compiled.source.output_indices(), source.output_indices());
    assert!(
        compiled
            .selected
            .iter()
            .all(|code| !code.module_bytes().is_empty())
    );
}
