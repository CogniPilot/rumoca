use super::*;
use std::cell::Cell;

#[derive(Clone, Copy)]
enum FamilyMode {
    Bind,
    Decline,
    DeclineEntries,
    BindingError,
}

struct Backend {
    mode: FamilyMode,
    refused: Option<rumoca_core::Span>,
    fault: Option<rumoca_core::Span>,
    families: Cell<usize>,
    individuals: Cell<usize>,
    executed: Rc<RefCell<Vec<rumoca_core::Span>>>,
}

struct Family {
    source: solve::ScalarProgramBlock,
    mode: FamilyMode,
    fault: Option<rumoca_core::Span>,
    executed: Rc<RefCell<Vec<rumoca_core::Span>>>,
}

struct Entry {
    inner: Rc<dyn CompiledSolveExpression>,
    span: rumoca_core::Span,
    fault: Option<rumoca_core::Span>,
    executed: Rc<RefCell<Vec<rumoca_core::Span>>>,
}

impl CompiledSolveExpression for Family {
    fn bind_program(
        &self,
        source: &solve::ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        assert!(self.source.shares_program_owner(source));
        match self.mode {
            FamilyMode::DeclineEntries => return Ok(None),
            FamilyMode::BindingError => return Err("wrong source owner".into()),
            _ => {}
        }
        let span = source.program_span(program).unwrap();
        let block = solve::ScalarProgramBlock::with_program_spans(
            vec![source.programs()[program].clone()],
            vec![span],
        )
        .unwrap();
        Ok(Some(Rc::new(Entry {
            inner: super::super::test_kernels::expression(&block)?,
            span,
            fault: self.fault,
            executed: self.executed.clone(),
        })))
    }
    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
        _: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        panic!("ordinary lifecycle must call only its bound original program")
    }
}

impl CompiledSolveExpression for Entry {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.executed.borrow_mut().push(self.span);
        if self.fault == Some(self.span) {
            return Err(RuntimeSolveError::SourceFault {
                message: "owned operation".into(),
                span: Some(self.span),
            });
        }
        self.inner.call(y, p, t, tables, out)
    }
}

impl SolveExecutionBackend for Backend {
    fn compile_selectable_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.families.set(self.families.get() + 1);
        if matches!(self.mode, FamilyMode::Decline) {
            return Err("optional family unavailable".into());
        }
        Ok(Rc::new(Family {
            source: source.clone(),
            mode: self.mode,
            fault: self.fault,
            executed: self.executed.clone(),
        }))
    }
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.individuals.set(self.individuals.get() + 1);
        assert_eq!(source.row_count(), 1);
        if self.refused == source.program_span(0) {
            return Err("mandatory base unavailable".into());
        }
        super::super::test_kernels::expression(source)
    }
    fn compile_jacobian_expression(
        &self,
        _: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        panic!("unused")
    }
    fn compile_assignment_schedule(
        &self,
        _: &solve::ComputeBlock,
        _: &solve::ContinuousRefreshOwners,
        _: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        panic!("unused")
    }
    fn compile_event_transaction(
        &self,
        _: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        panic!("unused")
    }
}

fn fixture(mode: FamilyMode) -> (solve::ScalarProgramBlock, Backend) {
    let spans = ["family_first.mo", "family_second.mo"].map(|name| {
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(name), 1, 2)
    });
    let source = solve::ScalarProgramBlock::with_program_spans(
        vec![
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
                solve::LinearOp::LoadTime { dst: 1 },
                solve::LinearOp::StoreOutput { src: 1 },
            ],
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        spans.to_vec(),
    )
    .unwrap();
    (
        source,
        Backend {
            mode,
            refused: None,
            fault: None,
            families: Cell::new(0),
            individuals: Cell::new(0),
            executed: Rc::new(RefCell::new(Vec::new())),
        },
    )
}

#[test]
fn bound_family_keeps_original_program_scope_outputs_and_fault_commit() {
    let (source, mut backend) = fixture(FamilyMode::Bind);
    backend.fault = source.program_span(1);
    let programs = CompiledPrograms::new("family", &source, &backend).unwrap();
    assert_eq!((backend.families.get(), backend.individuals.get()), (1, 0));
    let mut out = vec![-99.0];
    let point = RowEvalPoint {
        y: &[2.0, 7.0],
        p: &[],
        t: 3.0,
    };
    programs.call(0, point, &[], &mut out).unwrap();
    assert_eq!(out, vec![2.0, 3.0]);
    assert_eq!(
        *backend.executed.borrow(),
        vec![source.program_span(0).unwrap()]
    );
    let error = programs.call(1, point, &[], &mut out).unwrap_err();
    assert!(
        matches!(error, RuntimeSolveError::SourceFault { span, .. } if span == source.program_span(1))
    );
    assert_eq!(out, vec![2.0, 3.0]);
}

#[test]
fn declined_family_still_requires_each_original_compiled_base_cold() {
    let (source, mut backend) = fixture(FamilyMode::Decline);
    backend.refused = source.program_span(1);
    let error = CompiledPrograms::new("family", &source, &backend)
        .err()
        .unwrap();
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { message, span }
        if span == source.program_span(1) && message.contains("family[1]")
            && message.contains("mandatory base unavailable"))
    );
    assert_eq!((backend.families.get(), backend.individuals.get()), (1, 2));
    assert!(backend.executed.borrow().is_empty());
}

#[test]
fn declined_family_entries_prepare_original_programs_before_admission() {
    let (source, backend) = fixture(FamilyMode::DeclineEntries);
    let programs = CompiledPrograms::new("family", &source, &backend).unwrap();
    assert_eq!((backend.families.get(), backend.individuals.get()), (1, 2));
    assert!(backend.executed.borrow().is_empty());
    let mut out = vec![-99.0];
    programs
        .call(
            1,
            RowEvalPoint {
                y: &[2.0, 7.0],
                p: &[],
                t: 3.0,
            },
            &[],
            &mut out,
        )
        .unwrap();
    assert_eq!(out, vec![7.0]);
    assert_eq!(backend.individuals.get(), 2);
}

#[test]
fn family_binding_error_preserves_original_span_and_is_fatal_cold() {
    let (source, backend) = fixture(FamilyMode::BindingError);
    let error = CompiledPrograms::new("family", &source, &backend)
        .err()
        .unwrap();
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { message, span }
        if span == source.program_span(0) && message.contains("wrong source owner"))
    );
    assert_eq!(backend.individuals.get(), 0);
    assert!(backend.executed.borrow().is_empty());
}
