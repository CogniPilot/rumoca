//! Admitted backend first-fault and output rollback controls.
use super::native_projection_jvp::selected_projection;
use super::*;
use crate::runtime::projection::ImplicitProjectionModel;
use std::cell::Cell;

struct FaultingExpression {
    calls: Rc<Cell<usize>>,
    fail: bool,
    write_before_failure: bool,
}

impl CompiledSolveExpression for FaultingExpression {
    fn call(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        if self.write_before_failure || !self.fail {
            out[0] = 777.0;
        }
        if self.fail {
            Err("first admitted test fault".into())
        } else {
            Ok(())
        }
    }
}

fn variants(first: Rc<Cell<usize>>, later: Rc<Cell<usize>>) -> Vec<CompiledDiscreteSpecialization> {
    [(later, false), (first, true)]
        .into_iter()
        .map(|(calls, fail)| CompiledDiscreteSpecialization {
            source_span: test_span("AdmittedSpecialization.mo"),
            expression: Rc::new(FaultingExpression {
                calls,
                fail,
                write_before_failure: true,
            }),
            output_count: 1,
            guard_expectations: Box::new([]),
        })
        .collect()
}

#[test]
fn specialization_fault_never_retries_or_publishes_partial_output() {
    let runtime = SolveRuntime::new_fixture(&warm_start_test_model()).unwrap();
    let point = native_specialization::RowEvalPoint {
        y: &[1.0, 2.0],
        p: &[],
        t: 0.0,
    };
    for owner in 0..3 {
        let first = Rc::new(Cell::new(0));
        let later = Rc::new(Cell::new(0));
        let entries = variants(first.clone(), later.clone());
        let mut out = vec![-0.0, f64::from_bits(0x7ff8_0000_0000_0123)];
        let before = out.iter().map(|x| x.to_bits()).collect::<Vec<_>>();
        let result = match owner {
            0 => {
                let cache = RefCell::new(FxHashMap::from_iter([(0, entries)]));
                runtime.try_compiled_program_outputs(&cache, 0, &[(0, 0)], point, &mut out)
            }
            1 => {
                let cache = RefCell::new(FxHashMap::from_iter([(0, entries)]));
                runtime.try_compiled_single_output(&cache, 0, point, &mut out)
            }
            _ => runtime.try_cached_discrete_program(
                &mut entries.clone(),
                point.y,
                point.p,
                point.t,
                &mut out,
            ),
        };
        let error = result.expect_err("an admitted fault must stop dispatch");
        assert!(error.to_string().contains("first admitted test fault"));
        assert_eq!(first.get(), 1);
        assert_eq!(later.get(), 0, "an older variant ran after the first fault");
        assert_eq!(out.iter().map(|x| x.to_bits()).collect::<Vec<_>>(), before);
    }
}

#[test]
fn full_refresh_residual_does_not_overwrite_fault_with_interpreted_success() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let calls = Rc::new(Cell::new(0));
    runtime.compiled_implicit_rhs = Some(Rc::new(FaultingExpression {
        calls: calls.clone(),
        fail: true,
        write_before_failure: true,
    }));
    let mut out = vec![-0.0_f64; runtime.implicit_rhs.len()];
    let before = out.iter().map(|x| x.to_bits()).collect::<Vec<_>>();
    let _error = selected_projection(&runtime, false)
        .eval_residual(&[1.0, 2.0], &[], 0.0, &mut out)
        .expect_err("an admitted fault must not evaluate a reference replacement");
    assert_eq!(calls.get(), 1);
    assert_eq!(out.iter().map(|x| x.to_bits()).collect::<Vec<_>>(), before);
}

#[test]
fn guarded_assignment_fault_preserves_output_length_and_exact_bits() {
    let span = test_span("AdmittedGuardedAssignment.mo");
    let owner = solve::GuardedAssignmentProgram::checked(
        vec![
            solve::LinearOp::Const {
                dst: 0,
                value: 42.0,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        span.require_provenance("guarded assignment fault fixture")
            .unwrap(),
        [(solve::scalar_slot_p(0), 1)],
        solve::DiscreteRowRole::EventAction,
        solve::DiscreteEventPreMode::FollowCurrent,
        false,
        solve::IntegratorHistoryEffect::Preserve,
        None,
    )
    .unwrap();
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: solve::VarLayout::from_parts(IndexMap::new(), 0, 1),
            solve_layout: solve::SolveLayout {
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            discrete: solve::DiscreteSolveSystem {
                guarded_assignments: vec![owner],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![0.0].into(),
        ..Default::default()
    };
    let runtime = SolveRuntime::new_fixture(&model).unwrap();
    let calls = Rc::new(Cell::new(0));
    runtime.compiled_guarded_assignments.borrow_mut().insert(
        0,
        Rc::new(FaultingExpression {
            calls: calls.clone(),
            fail: true,
            write_before_failure: true,
        }),
    );
    let mut out = vec![-0.0, f64::from_bits(0x7ff8_0000_0000_0456)];
    let before = out.iter().map(|x| x.to_bits()).collect::<Vec<_>>();
    let _error = runtime
        .eval_guarded_assignment_outputs(0, &[], &[3.0], 0.0, &mut out)
        .expect_err("an admitted guarded fault must not resize or publish outputs");
    assert_eq!(calls.get(), 1);
    assert_eq!(out.iter().map(|x| x.to_bits()).collect::<Vec<_>>(), before);
}

impl CompiledSolveJacobianExpression for FaultingExpression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        _: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        CompiledSolveExpression::call(self, y, p, t, tables, out)
    }
}

#[test]
fn admitted_refresh_jvp_preserves_outputs_in_both_seed_spaces() {
    for full_seed in [false, true] {
        let mut runtime = SolveRuntime::new_fixture(&warm_start_test_model()).unwrap();
        let calls = Rc::new(Cell::new(0));
        let hook = Rc::new(FaultingExpression {
            calls: calls.clone(),
            fail: true,
            write_before_failure: true,
        });
        if full_seed {
            runtime.compiled_implicit_full_jacobian_v = Some(hook);
        } else {
            runtime.compiled_implicit_projection_jacobian_v = Some(hook);
        }
        let mut out = [-0.0_f64, f64::from_bits(0x7ff8_0000_0000_0134)];
        let before = out.map(f64::to_bits);
        let error = selected_projection(&runtime, full_seed)
            .eval_jacobian_v(&[1.0, 2.0], &[], 0.0, &[1.0, 1.0], &mut out)
            .unwrap_err();
        assert!(matches!(error, RuntimeSolveError::CompiledExecution { .. }));
        assert_eq!(calls.get(), 1);
        assert_eq!(out.map(f64::to_bits), before);
    }
}

#[test]
fn absent_full_refresh_hook_and_successful_hook_remain_accepted() {
    let mut runtime = SolveRuntime::new_fixture(&warm_start_test_model()).unwrap();
    let mut out = [0.0; 2];
    selected_projection(&runtime, false)
        .eval_residual(&[1.0, 2.0], &[], 0.0, &mut out)
        .unwrap();
    let calls = Rc::new(Cell::new(0));
    runtime.compiled_implicit_rhs = Some(Rc::new(FaultingExpression {
        calls: calls.clone(),
        fail: false,
        write_before_failure: false,
    }));
    selected_projection(&runtime, false)
        .eval_residual(&[1.0, 2.0], &[], 0.0, &mut out)
        .unwrap();
    assert_eq!(calls.get(), 1);
    assert_eq!(out[0], 777.0);
}

struct EventSourceFault {
    calls: Rc<Cell<usize>>,
    span: rumoca_core::Span,
    backend_fault: bool,
}
impl CompiledSolveExpression for EventSourceFault {
    fn call(
        &self,
        _: &[f64],
        p: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls.set(self.calls.get() + 1);
        if p[1] == 0.0 {
            if self.backend_fault {
                return Err(solve_eval::EvalSolveError::CompiledExecution {
                    message: "backend event fault".into(),
                    span: Some(self.span),
                }
                .into());
            }
            return Err(solve_eval::EvalSolveError::NativeSourceIndexFault {
                message: "issued source index fault".into(),
                span: Some(self.span),
            }
            .into());
        }
        out[0] = 42.0;
        Ok(())
    }
}

fn event_source_fault_model() -> solve::SolveModel {
    solve::SolveModel {
        problem: solve::SolveProblem {
            layout: solve::VarLayout::from_parts(IndexMap::new(), 0, 2),
            solve_layout: solve::SolveLayout {
                parameter_count: 2,
                compiled_parameter_len: 2,
                ..Default::default()
            },
            discrete: solve::DiscreteSolveSystem {
                rhs: spanned_block(
                    vec![
                        vec![
                            solve::LinearOp::Const {
                                dst: 0,
                                value: 42.0,
                            },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ],
                        vec![
                            solve::LinearOp::Const { dst: 0, value: 1.0 },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ],
                    ],
                    "EventSourceFault.mo",
                ),
                update_targets: vec![solve::scalar_slot_p(0), solve::scalar_slot_p(1)],
                row_roles: vec![solve::DiscreteRowRole::Equation; 2],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent; 2],
                observation_refresh: vec![false; 2],
                integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve; 2],
                clock_owners: vec![None; 2],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![0.0; 2].into(),
        ..Default::default()
    }
}

#[test]
fn typed_event_source_fault_is_deferred_only_while_the_iterate_changes() {
    use super::super::event_update::{DiscretePreSnapshot, EventUpdateRowFilter};
    let span = test_span("EventSourceFault.mo");
    let model = event_source_fault_model();
    for backend_fault in [false, true] {
        let runtime = SolveRuntime::new_fixture(&model).unwrap();
        let calls = Rc::new(Cell::new(0));
        runtime.compiled_discrete_rows.borrow_mut().insert(
            0,
            vec![CompiledDiscreteSpecialization {
                source_span: span,
                expression: Rc::new(EventSourceFault {
                    calls: calls.clone(),
                    span,
                    backend_fault,
                }),
                output_count: 1,
                guard_expectations: Box::new([]),
            }],
        );
        let snapshot = DiscretePreSnapshot {
            row_filter: EventUpdateRowFilter::All,
            root_relation_overrides: &[],
            event_iteration: 0,
        };
        let mut p = [0.0, 0.0];
        let first = runtime.apply_constant_discrete_rows_for_pre_snapshot(
            &snapshot,
            &mut [],
            &mut p,
            0.0,
            1e-10,
        );
        if backend_fault {
            assert!(matches!(
                first.unwrap_err(),
                RuntimeSolveError::CompiledExecution { .. }
            ));
            assert_eq!(calls.get(), 1);
            assert_eq!(p, [0.0, 0.0]);
        } else {
            assert!(first.unwrap());
            assert_eq!(p, [0.0, 1.0]);
            assert!(
                runtime
                    .apply_constant_discrete_rows_for_pre_snapshot(
                        &snapshot,
                        &mut [],
                        &mut p,
                        0.0,
                        1e-10
                    )
                    .unwrap()
            );
            assert!(
                !runtime
                    .apply_constant_discrete_rows_for_pre_snapshot(
                        &snapshot,
                        &mut [],
                        &mut p,
                        0.0,
                        1e-10
                    )
                    .unwrap()
            );
            assert_eq!(p, [42.0, 1.0]);
            assert_eq!(calls.get(), 3);
        }
    }
}
