//! Missing capabilities refuse complete preparation before source execution.
use super::super::test_kernels::recording::RecordingBackend;
use super::*;
use std::cell::Cell;

#[derive(Default)]
struct NumericOnly(Rc<Cell<usize>>);

struct NumericKernel {
    inner: Rc<dyn CompiledSolveExpression>,
    calls: Rc<Cell<usize>>,
}

impl CompiledSolveExpression for NumericKernel {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        self.inner.call(y, p, t, tables, out)
    }
}

impl SolveExecutionBackend for NumericOnly {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Ok(Rc::new(NumericKernel {
            inner: super::super::test_kernels::expression(source)?,
            calls: self.0.clone(),
        }))
    }
}

#[derive(Default)]
struct WithJacobian(NumericOnly);

impl SolveExecutionBackend for WithJacobian {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.0.compile_expression(source)
    }
    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        super::super::test_kernels::jacobian(source)
    }
}

#[test]
fn missing_jacobian_refuses_the_required_owner_before_any_source_call() {
    let backend = Rc::new(NumericOnly::default());
    let source = spanned_block(vec![affine_seed_row(&[(0, 1.0)])], "missing-jacobian.mo");
    let recorded = Rc::new(RecordingBackend::new(backend.clone()));
    let erased: Rc<dyn SolveExecutionBackend> = recorded.clone();
    let error = DirectionalProgram::prepare("required JVP", source, Some(&erased), None)
        .err()
        .unwrap();
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { span: Some(span), message }
        if span == test_span("missing-jacobian.mo") && message.contains("Jacobian expressions are not compiled"))
    );
    assert_eq!(backend.0.get(), 0);
    assert_eq!(recorded.requests(), (1, 0, 0));
    let primal = spanned_block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "accepted-primal.mo",
    );
    let owner = PrimalProgram::prepare("numeric capability", primal, Some(&erased), None).unwrap();
    let mut output = [91.0];
    owner
        .eval(
            RowEvalPoint {
                y: &[-0.0],
                p: &[],
                t: 0.0,
            },
            RowEvalContext::default(),
            &mut output,
        )
        .unwrap();
    assert_eq!(output[0].to_bits(), (-0.0f64).to_bits());
    assert_eq!(backend.0.get(), 1);
}

#[test]
fn missing_assignment_and_transaction_capabilities_refuse_runtime_admission() {
    let models = [
        (
            warm_start_test_model(),
            "assignment schedules are not compiled",
        ),
        (
            super::super::event_transactions::tests::transaction_model(),
            "event transactions are not compiled",
        ),
    ];
    for (model, expected) in models {
        let backend = Rc::new(WithJacobian::default());
        let recorded = Rc::new(RecordingBackend::new(backend.clone()));
        let error = SolveRuntime::new_fixture_with_backend(&model, recorded.clone())
            .err()
            .unwrap();
        assert!(
            matches!(error, EvalSolveError::CompiledExecution { .. }),
            "{error}"
        );
        assert!(error.to_string().contains(expected), "{error}");
        assert_eq!(backend.0.0.get(), 0);
        let (_, assignments, transactions) = recorded.requests();
        if expected.starts_with("assignment") {
            assert!(assignments > 0);
            assert_eq!(transactions, 0);
        } else {
            assert!(transactions > 0);
        }
        SolveRuntime::new_fixture(&model).unwrap();
    }
}

fn observation_model() -> solve::SolveModel {
    let span = test_span("required-observation.mo");
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![],
            vec![solve::SolvePureCallOutput::assertion_predicate()],
            span,
            |body, _, outputs| {
                let condition = body.constant(solve::SolveValue::boolean(false), span)?;
                let assertion = body.assertion_output(0, span)?;
                body.check_assertion(assertion, condition, &[], span, |_, _, _| Ok(()))?;
                body.store(outputs[0], condition, span)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    let mut model = solve::SolveModel {
        pure_calls: table,
        ..Default::default()
    };
    model.problem.events.action_conditions = spanned_block(
        vec![vec![
            solve::LinearOp::PureCallObservation {
                dst_start: 0,
                input_starts: vec![].into(),
                site: solve::SolveAssertionObservationSite::new(site.clone()).unwrap(),
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "required-observation.mo",
    );
    model.problem.events.actions = vec![solve::SolveEventAction {
        kind: solve::SolveEventActionKind::Assert,
        message: solve::SolveEventMessage {
            parts: vec![solve::SolveEventMessagePart::Text(
                "original assertion".into(),
            )],
        },
        span,
        origin: "source assertion".into(),
        clock_owner: None,
        assertion_projection: solve::SolveAssertionActionProjection::new(site, 0),
    }];
    model
}

#[test]
fn numeric_compilation_cannot_substitute_for_required_observation_capture() {
    let model = observation_model();
    let backend = Rc::new(WithJacobian::default());
    let error = SolveRuntime::new_fixture_with_backend(&model, backend.clone())
        .err()
        .unwrap();
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { span: Some(span), message }
        if span == test_span("required-observation.mo") && message.contains("checked event observations are not compiled"))
    );
    assert_eq!(backend.0.0.get(), 0);
    SolveRuntime::new_fixture(&model).unwrap();
}
