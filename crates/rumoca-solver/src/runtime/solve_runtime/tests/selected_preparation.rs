use super::*;

struct Backend {
    refused: Option<rumoca_core::Span>,
    failed: Option<rumoca_core::Span>,
    calls: Rc<RefCell<Vec<rumoca_core::Span>>>,
}

struct Kernel {
    inner: Rc<dyn CompiledSolveExpression>,
    spans: Vec<rumoca_core::Span>,
    failed: Option<rumoca_core::Span>,
    calls: Rc<RefCell<Vec<rumoca_core::Span>>>,
}

impl CompiledSolveExpression for Kernel {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.borrow_mut().extend(&self.spans);
        self.inner.call(y, p, t, tables, out)?;
        if self.failed.is_some_and(|span| self.spans.contains(&span)) {
            return Err(RuntimeSolveError::SourceFault {
                message: "cold selected source fault".into(),
                span: self.failed,
            });
        }
        Ok(())
    }
}

impl SolveExecutionBackend for Backend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        if block.row_count() == 1
            && self
                .refused
                .is_some_and(|span| block.program_spans().contains(&span))
        {
            return Err("selected cold program is unsupported".into());
        }
        Ok(Rc::new(Kernel {
            inner: super::super::test_kernels::expression(block)?,
            spans: block.program_spans().to_vec(),
            failed: self.failed,
            calls: self.calls.clone(),
        }))
    }
}

fn source() -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_program_spans(
        vec![
            vec![
                solve::LinearOp::Const { dst: 0, value: 3.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::Const { dst: 0, value: 7.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        vec![
            test_span("active_selected.mo"),
            test_span("cold_selected.mo"),
        ],
    )
    .unwrap()
}

#[test]
fn selected_preparation_rejects_an_unsupported_cold_program_without_execution() {
    let source = source();
    let span = source.program_span(1).unwrap();
    let calls = Rc::default();
    let backend: Rc<dyn SolveExecutionBackend> = Rc::new(Backend {
        refused: Some(span),
        failed: None,
        calls: Rc::clone(&calls),
    });
    let error = compiled_expression(Some(&backend), "cold", &source)
        .err()
        .expect("missing required selected code must reject preparation");
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { span: Some(issued), .. } if issued == span)
    );
    assert!(calls.borrow().is_empty());
}

#[test]
fn selecting_one_program_never_runs_an_unrelated_faulting_program() {
    let source = source();
    let calls = Rc::default();
    let backend: Rc<dyn SolveExecutionBackend> = Rc::new(Backend {
        refused: None,
        failed: source.program_span(1),
        calls: Rc::clone(&calls),
    });
    let code = compiled_expression(Some(&backend), "cold", &source)
        .unwrap()
        .unwrap();
    assert_eq!(
        code.call_program_output((0, 0), &[], &[], 0.0, &[])
            .unwrap(),
        3.0
    );
    assert_eq!(*calls.borrow(), [source.program_span(0).unwrap()]);
    assert!(
        code.call_program_output((2, 0), &[], &[], 0.0, &[])
            .is_err()
    );
    assert!(
        code.call_program_output((0, 1), &[], &[], 0.0, &[])
            .is_err()
    );
    assert_eq!(
        calls.borrow().len(),
        1,
        "bad coordinates must fail before execution"
    );
}

#[test]
fn selected_bulk_fault_preserves_outputs_and_success_reuses_each_program_once() {
    let source = source();
    let calls = Rc::default();
    let backend: Rc<dyn SolveExecutionBackend> = Rc::new(Backend {
        refused: None,
        failed: source.program_span(1),
        calls: Rc::clone(&calls),
    });
    let code = compiled_expression(Some(&backend), "cold", &source)
        .unwrap()
        .unwrap();
    let mut out = [17.0, 19.0];
    assert!(
        code.call_program_outputs_at(&[(0, 0), (1, 0)], (&[], &[], 0.0), &[], &mut out)
            .is_err()
    );
    assert_eq!(out, [17.0, 19.0]);
    assert_eq!(*calls.borrow(), source.program_spans());
    calls.borrow_mut().clear();
    code.call_program_outputs_at(&[(0, 0), (0, 0)], (&[], &[], 0.0), &[], &mut out)
        .unwrap();
    assert_eq!(out, [3.0, 3.0]);
    assert_eq!(*calls.borrow(), [source.program_span(0).unwrap()]);
}

#[test]
fn a_whole_only_backend_still_prepares_required_selected_expression_code() {
    let source = spanned_block(
        vec![
            vec![
                solve::LinearOp::Const { dst: 0, value: 3.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::Const { dst: 0, value: 7.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        "selected_code.mo",
    );
    let backend: Rc<dyn SolveExecutionBackend> =
        Rc::new(super::super::test_kernels::ReferenceBackend);
    let code = compiled_expression(Some(&backend), "selected", &source)
        .unwrap()
        .unwrap();
    assert_eq!(
        code.call_program_output((1, 0), &[], &[], 0.0, &[])
            .unwrap(),
        7.0
    );
}

#[test]
fn a_whole_only_backend_still_prepares_required_selected_directional_code() {
    let source = spanned_block(
        vec![
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        "selected_jvp.mo",
    );
    let backend: Rc<dyn SolveExecutionBackend> =
        Rc::new(super::super::test_kernels::ReferenceBackend);
    let code = compiled_jacobian(Some(&backend), "selected", &source)
        .unwrap()
        .unwrap();
    assert_eq!(
        code.call_program_output((1, 0), &[], &[], 0.0, &[3.0, 7.0], &[])
            .unwrap(),
        7.0
    );
}
