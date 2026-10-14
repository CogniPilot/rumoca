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
    fn compile_event_transaction(
        &self,
        _: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("fixture has no transactions".into())
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
