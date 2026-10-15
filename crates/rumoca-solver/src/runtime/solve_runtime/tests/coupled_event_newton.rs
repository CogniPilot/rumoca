//! The coupled event Newton solves an algebraic row and a discrete Real row
//! together with the event rows' exact directional derivative.
//!
//! The algebraic `y` solves `y = 2 - z` and the discrete `z` is assigned
//! `z := 0.25*y*y + 0.5`, so the coupled event system has the root
//! `z = 4 - sqrt(10)`, `y = 2 - z` near the start. The Jacobian comes from the
//! implicit row's JVP over solver and parameter seeds and the discrete row's
//! JVP; without the discrete row's JVP the solve reports the missing
//! derivative.

use super::*;

/// The implicit row `y - (2 - z)` and the discrete row `0.25*y*y + 0.5`.
fn coupled_event_rows() -> (solve::ScalarProgramBlock, solve::ScalarProgramBlock) {
    let implicit = spanned_block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::LoadP { dst: 2, index: 0 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Sub,
                lhs: 1,
                rhs: 2,
            },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ]],
        "coupled_event_implicit.mo",
    );
    let discrete = spanned_block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Binary {
                dst: 1,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 0,
            },
            solve::LinearOp::Const {
                dst: 2,
                value: 0.25,
            },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Mul,
                lhs: 2,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 4, value: 0.5 },
            solve::LinearOp::Binary {
                dst: 5,
                op: solve::BinaryOp::Add,
                lhs: 3,
                rhs: 4,
            },
            solve::LinearOp::StoreOutput { src: 5 },
        ]],
        "coupled_event_discrete.mo",
    );
    (implicit, discrete)
}

/// d(implicit)/d(y, z) = seed[y] + seed[z] and d(discrete)/dy = 0.5*y*seed[y].
fn coupled_event_jvp_rows() -> (Vec<Vec<solve::LinearOp>>, Vec<Vec<solve::LinearOp>>) {
    let implicit_jvp = vec![vec![
        solve::LinearOp::LoadSeed { dst: 0, index: 0 },
        solve::LinearOp::LoadSeed { dst: 1, index: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]];
    let discrete_jvp_rows = vec![vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::Const { dst: 1, value: 0.5 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Mul,
            lhs: 1,
            rhs: 0,
        },
        solve::LinearOp::LoadSeed { dst: 3, index: 0 },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Mul,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ]];
    (implicit_jvp, discrete_jvp_rows)
}

fn coupled_event_model(discrete_jvp: bool) -> solve::SolveModel {
    let (implicit, discrete) = coupled_event_rows();
    let (implicit_jvp, discrete_jvp_rows) = coupled_event_jvp_rows();
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: crate::fixture_layout::named_y_layout(
                rumoca_ir_solve::VarLayout::from_parts(Default::default(), 0, 1),
                vec!["y".to_string()],
            ),
            solve_layout: solve::SolveLayout {
                algebraic_scalar_count: 1,
                compiled_parameter_len: 1,
                discrete_real_scalar_names: vec!["z".to_string()],
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(implicit),
                implicit_row_targets: (vec![Some(solve::scalar_slot_y(0))]).into(),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: (vec![solve::AlgebraicProjectionBlock {
                        rows: vec![0],
                        y_indices: vec![0],
                        tearing: None,
                        alternate_charts: Vec::new(),
                    }])
                    .into(),
                },
                ..Default::default()
            },
            discrete: solve::DiscreteSolveSystem {
                rhs: discrete,
                update_targets: vec![solve::scalar_slot_p(0)],
                row_roles: vec![solve::DiscreteRowRole::Equation],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
                observation_refresh: vec![false],
                integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
                clock_owners: vec![None],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0].into(),
        parameters: vec![0.0].into(),
        ..Default::default()
    };
    set_test_implicit_jvp(&mut model, implicit_jvp, "coupled_event_implicit_jvp.mo");
    if discrete_jvp {
        model.artifacts.discrete.rhs_jacobian_v = Some(spanned_block(
            discrete_jvp_rows,
            "coupled_event_discrete_jvp.mo",
        ));
    }
    model
}

fn solve_coupled(model: &solve::SolveModel) -> Result<(Vec<f64>, Vec<f64>), RuntimeSolveError> {
    let runtime = SolveRuntime::new_fixture(model).expect("the coupled event fixture prepares");
    solve_prepared_coupled(&runtime)
}

fn solve_prepared_coupled(
    runtime: &SolveRuntime,
) -> Result<(Vec<f64>, Vec<f64>), RuntimeSolveError> {
    let mut y = vec![1.0];
    let mut p = vec![1.0];
    let snapshot = DiscretePreSnapshot {
        row_filter: EventUpdateRowFilter::All,
        root_relation_overrides: &[],
        event_iteration: 0,
    };
    runtime
        .scalar_execution()
        .unwrap()
        .solve_coupled_event_rows(
            &snapshot,
            &mut DiscreteRowsSettleInput {
                y: &mut y,
                p: &mut p,
                t: 0.0,
                tol: 1.0e-12,
                max_iters: 20,
            },
        )?;
    Ok((y, p))
}

#[test]
fn coupled_event_newton_solves_with_the_event_rows_directional_derivative() {
    let (y, p) = solve_coupled(&coupled_event_model(true)).expect("the coupled event solves");
    let z = 4.0 - 10.0_f64.sqrt();
    assert!((p[0] - z).abs() <= 1.0e-10, "z = {}", p[0]);
    assert!((y[0] - (2.0 - z)).abs() <= 1.0e-10, "y = {}", y[0]);
}

#[test]
fn coupled_event_newton_reports_a_missing_directional_derivative() {
    let error = solve_coupled(&coupled_event_model(false))
        .expect_err("a discrete row without a derivative cannot be linearized");
    assert!(
        error.to_string().contains("no directional derivative"),
        "{error}"
    );
}

struct EventTangentBackend {
    refused: Option<rumoca_core::Span>,
    discrete_calls: Rc<std::cell::Cell<usize>>,
}

struct EventTangentKernel {
    inner: Rc<dyn CompiledSolveJacobianExpression>,
    tracked: bool,
    calls: Rc<std::cell::Cell<usize>>,
}

impl CompiledSolveJacobianExpression for EventTangentKernel {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if self.tracked {
            self.calls.set(self.calls.get() + 1);
        }
        self.inner.call(y, p, t, seed, tables, out)
    }
}

impl SolveExecutionBackend for EventTangentBackend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        super::super::test_kernels::expression(block)
    }
    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        if self
            .refused
            .is_some_and(|span| block.program_spans().contains(&span))
        {
            return Err("unsupported cold event derivative".into());
        }
        Ok(Rc::new(EventTangentKernel {
            inner: super::super::test_kernels::jacobian(block)?,
            tracked: block
                .program_spans()
                .contains(&test_span("coupled_event_discrete_jvp.mo")),
            calls: self.discrete_calls.clone(),
        }))
    }
    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        super::super::test_kernels::assignment(source, owners, schedule)
    }
}

#[test]
fn coupled_event_newton_executes_the_prepared_discrete_derivative() {
    let model = coupled_event_model(true);
    let calls = Rc::new(std::cell::Cell::new(0));
    let backend = Rc::new(EventTangentBackend {
        refused: None,
        discrete_calls: calls.clone(),
    });
    let runtime =
        SolveRuntime::new_fixture_with_backend(&model, backend).expect("complete backend prepares");
    assert_eq!(calls.get(), 0, "preparation does not execute derivatives");
    let (y, p) = solve_prepared_coupled(&runtime).expect("compiled coupled derivative solves");
    let z = 4.0 - 10.0_f64.sqrt();
    assert!((p[0] - z).abs() <= 1.0e-10);
    assert!((y[0] - (2.0 - z)).abs() <= 1.0e-10);
    assert!(
        calls.get() > 0,
        "the discrete derivative must execute its prepared backend handle"
    );
}

#[test]
fn every_cold_event_derivative_family_is_required_before_runtime_execution() {
    for family in ["discrete", "runtime_assignment", "guarded", "structured"] {
        let mut model = coupled_event_model(true);
        let cold = spanned_block(coupled_event_jvp_rows().1, "cold_event_derivative.mo");
        match family {
            "discrete" => model.artifacts.discrete.rhs_jacobian_v = Some(cold),
            "runtime_assignment" => {
                model.artifacts.discrete.runtime_assignment_jacobian_v = Some(cold)
            }
            "guarded" => model.artifacts.discrete.guarded_jacobian_v = Some(cold),
            "structured" => model.artifacts.discrete.structured_jacobian_v = Some(cold),
            _ => unreachable!(),
        }
        let calls = Rc::new(std::cell::Cell::new(0));
        let backend = Rc::new(EventTangentBackend {
            refused: Some(test_span("cold_event_derivative.mo")),
            discrete_calls: calls.clone(),
        });
        let error = SolveRuntime::new_fixture_with_backend(&model, backend)
            .err()
            .expect("unsupported required derivative rejects preparation");
        assert!(
            error.to_string().contains(&format!("event_{family}_jvp")),
            "{family}: {error}"
        );
        assert_eq!(calls.get(), 0, "a refused inventory never executes");
    }
}

struct JvpBackend {
    failed: Option<rumoca_core::Span>,
    refused: Option<rumoca_core::Span>,
    calls: Rc<RefCell<Vec<rumoca_core::Span>>>,
}

struct JvpKernel {
    failed: Option<rumoca_core::Span>,
    inner: Rc<dyn CompiledSolveJacobianExpression>,
    spans: Vec<rumoca_core::Span>,
    calls: Rc<RefCell<Vec<rumoca_core::Span>>>,
}

impl CompiledSolveJacobianExpression for JvpKernel {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.borrow_mut().extend(&self.spans);
        if self.failed.is_some_and(|span| self.spans.contains(&span)) {
            return Err(RuntimeSolveError::SourceFault {
                message: "coupled derivative source fault".into(),
                span: self.failed,
            });
        }
        self.inner.call(y, p, t, seed, tables, out)
    }
}

struct PrimalKernel {
    inner: Rc<dyn CompiledSolveExpression>,
    spans: Vec<rumoca_core::Span>,
    failed: Option<rumoca_core::Span>,
    calls: Rc<RefCell<Vec<rumoca_core::Span>>>,
}

impl CompiledSolveExpression for PrimalKernel {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.borrow_mut().extend(&self.spans);
        if self.failed.is_some_and(|span| self.spans.contains(&span)) {
            return Err(RuntimeSolveError::SourceFault {
                message: "coupled primal source fault".into(),
                span: self.failed,
            });
        }
        self.inner.call(y, p, t, tables, out)
    }
}

impl SolveExecutionBackend for JvpBackend {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Ok(Rc::new(PrimalKernel {
            inner: super::super::test_kernels::expression(source)?,
            spans: source.program_spans().to_vec(),
            failed: self.failed,
            calls: self.calls.clone(),
        }))
    }

    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        if self
            .refused
            .is_some_and(|span| source.program_spans().contains(&span))
        {
            return Err("cold event JVP is unsupported".into());
        }
        Ok(Rc::new(JvpKernel {
            failed: self.failed,
            inner: super::super::test_kernels::jacobian(source)?,
            spans: source.program_spans().to_vec(),
            calls: self.calls.clone(),
        }))
    }

    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        super::super::test_kernels::assignment(source, owners, schedule)
    }
}

#[test]
fn coupled_event_native_recovery_executes_prepared_discrete_and_implicit_jvps() {
    let model = coupled_event_model(true);
    let calls = Rc::new(RefCell::new(Vec::new()));
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(JvpBackend {
            failed: None,
            refused: None,
            calls: calls.clone(),
        }),
    )
    .unwrap();
    assert!(
        calls.borrow().is_empty(),
        "preparation must not execute any JVP"
    );
    let native = solve_prepared_coupled(&runtime).unwrap();
    assert_eq!(native, solve_coupled(&model).unwrap());
    let calls = calls.borrow();
    for source in [
        "coupled_event_discrete_jvp.mo",
        "coupled_event_implicit_jvp.mo",
        "coupled_event_implicit.mo",
    ] {
        assert!(
            calls.contains(&test_span(source)),
            "compiled {source} was bypassed"
        );
    }
}

#[test]
fn cold_coupled_event_jvps_are_rejected_during_preparation() {
    for role in 0..4 {
        let mut model = coupled_event_model(true);
        let span = test_span("cold_coupled_event_jvp.mo");
        let block = spanned_block(coupled_event_jvp_rows().1, "cold_coupled_event_jvp.mo");
        let artifacts = &mut model.artifacts.discrete;
        match role {
            0 => artifacts.rhs_jacobian_v = Some(block),
            1 => artifacts.runtime_assignment_jacobian_v = Some(block),
            2 => artifacts.guarded_jacobian_v = Some(block),
            3 => artifacts.structured_jacobian_v = Some(block),
            _ => unreachable!(),
        }
        let calls = Rc::new(RefCell::new(Vec::new()));
        let error = SolveRuntime::new_fixture_with_backend(
            &model,
            Rc::new(JvpBackend {
                failed: None,
                refused: Some(span),
                calls: calls.clone(),
            }),
        )
        .err()
        .expect("a present unsupported JVP must fail before event recovery is used");
        assert!(
            matches!(error, EvalSolveError::CompiledExecution { span: Some(actual), .. } if actual == span),
            "{error}"
        );
        assert!(calls.borrow().is_empty());
        SolveRuntime::new_fixture(&model)
            .expect("explicit reference preparation still accepts the source");
    }
}

#[test]
fn cold_jvp_error_does_not_blame_one_of_multiple_source_spans() {
    let spans = [test_span("first_jvp.mo"), test_span("second_jvp.mo")];
    let program = vec![
        solve::LinearOp::Const { dst: 0, value: 0.0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ];
    let source = solve::ScalarProgramBlock::with_program_spans(
        vec![program.clone(), program],
        spans.to_vec(),
    )
    .unwrap();
    let calls = Rc::new(RefCell::new(Vec::new()));
    let backend: Rc<dyn SolveExecutionBackend> = Rc::new(JvpBackend {
        failed: None,
        refused: Some(spans[1]),
        calls: calls.clone(),
    });
    let error = super::super::compiled_jacobian(Some(&backend), "mixed_jvp", &source)
        .err()
        .expect("the backend refuses this source during preparation");
    assert!(matches!(
        error,
        EvalSolveError::CompiledExecution { span: None, .. }
    ));
    assert!(error.to_string().contains("cold event JVP is unsupported"));
    assert!(calls.borrow().is_empty());
}

#[test]
fn native_coupled_event_without_derivative_reports_unavailable() {
    let calls = Rc::new(RefCell::new(Vec::new()));
    let runtime = SolveRuntime::new_fixture_with_backend(
        &coupled_event_model(false),
        Rc::new(JvpBackend {
            failed: None,
            refused: None,
            calls,
        }),
    )
    .expect("a derivative unavailable for an unused event path does not invent an artifact");
    let error = solve_prepared_coupled(&runtime).unwrap_err();
    assert!(
        matches!(
            error,
            RuntimeSolveError::DirectionalDerivativeUnavailable { .. }
        ),
        "{error}"
    );
}

#[test]
fn coupled_native_jvp_fault_preserves_category_span_and_input_transaction() {
    check_coupled_fault_transaction("coupled_event_discrete_jvp.mo");
}

#[test]
fn coupled_native_primal_fault_preserves_category_span_and_input_transaction() {
    check_coupled_fault_transaction("coupled_event_implicit.mo");
}

fn check_coupled_fault_transaction(source: &'static str) {
    let span = test_span(source);
    let runtime = SolveRuntime::new_fixture_with_backend(
        &coupled_event_model(true),
        Rc::new(JvpBackend {
            failed: Some(span),
            refused: None,
            calls: Rc::default(),
        }),
    )
    .unwrap();
    let mut y = vec![1.0];
    let mut p = vec![1.0];
    let snapshot = DiscretePreSnapshot {
        row_filter: EventUpdateRowFilter::All,
        root_relation_overrides: &[],
        event_iteration: 0,
    };
    let error = runtime
        .scalar_execution()
        .unwrap()
        .solve_coupled_event_rows(
            &snapshot,
            &mut DiscreteRowsSettleInput {
                y: &mut y,
                p: &mut p,
                t: 0.0,
                tol: 1e-12,
                max_iters: 20,
            },
        )
        .expect_err("a prepared source fault must not be bypassed by interpretation");
    assert!(
        matches!(error, RuntimeSolveError::SourceFault { span: Some(actual), .. } if actual == span),
        "{error}"
    );
    assert_eq!(y, [1.0]);
    assert_eq!(p, [1.0]);
}
