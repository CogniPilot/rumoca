//! Routing/cache witnesses with explicit test kernels, not code-generation proof.
use super::*;
use std::cell::Cell;

struct Entry {
    calls: Rc<Cell<usize>>,
    fail: bool,
}
impl CompiledSolveDerivativeOperation for Entry {
    fn call(
        &self,
        y: &mut [f64],
        _: &[f64],
        _: f64,
        _: &[rumoca_core::ExternalTableData],
        output: &mut [f64],
        cache: SolveDerivativeCacheView<'_>,
    ) -> Result<(), SolveDerivativeOperationFailure> {
        self.calls.set(self.calls.get() + 1);
        if self.fail {
            output[0] = f64::NAN;
            return Err(SolveDerivativeOperationFailure {
                phase: Some(solve::fmi::FmiDerivativeStatus::Values),
                error: RuntimeSolveError::solve_ir("complete value failure"),
            });
        }
        y[1] = 88.;
        output[0] = 99.;
        *cache.valid = true;
        cache.values[1] = 7.;
        cache.present[1] = 1;
        Ok(())
    }
}

#[test]
fn complete_callback_runs_once_without_the_old_refresh_and_shares_its_cache() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let calls = Rc::new(Cell::new(0));
    let scalar = runtime.scalar_fixture_mut();
    scalar
        .static_refresh_cache
        .get_mut()
        .prepare_operation_storage(2, 0)
        .unwrap();
    scalar.compiled_derivative_operation = Some(Rc::new(Entry {
        calls: calls.clone(),
        fail: false,
    }));
    let mut y = vec![10., 42.];
    let mut output = [-1.];
    let view = runtime.scalar_execution().unwrap();
    view.eval_state_derivatives_at_solver_y(0., &[], 1e-8, 10, &mut y, &mut output)
        .unwrap();
    assert_eq!(calls.get(), 1);
    assert_eq!(output, [99.]);
    assert_eq!(y[1], 88.);
    assert_eq!(
        view.scalar.static_refresh_cache.borrow().values.get(1),
        Some(7.)
    );
    view.prepare_static_refresh_cache(&[], 2);
    assert_eq!(
        view.scalar.static_refresh_cache.borrow().values.get(1),
        Some(7.)
    );
}

#[test]
fn complete_value_failure_is_retained_without_retrying_the_old_expression() {
    let model = warm_start_test_model();
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    let calls = Rc::new(Cell::new(0));
    runtime.scalar_fixture_mut().compiled_derivative_operation = Some(Rc::new(Entry {
        calls: calls.clone(),
        fail: true,
    }));
    let mut y = vec![10., 42.];
    let mut output = [-1.];
    let error = runtime
        .scalar_execution()
        .unwrap()
        .eval_state_derivatives_at_solver_y(0., &[], 1e-8, 10, &mut y, &mut output)
        .unwrap_err();
    assert!(error.to_string().contains("complete value failure"));
    assert!(output[0].is_nan());
    assert_eq!(calls.get(), 1);
}

struct SourceBackend {
    expected: solve::RetainedAssertionRoot,
    calls: Rc<Cell<usize>>,
}

#[test]
fn with_guess_callback_uses_complete_entry_once_and_preserves_its_failure() {
    for fail in [false, true] {
        let model = warm_start_test_model();
        let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
        let calls = Rc::new(Cell::new(0));
        let scalar = runtime.scalar_fixture_mut();
        scalar
            .static_refresh_cache
            .get_mut()
            .prepare_operation_storage(2, 0)
            .unwrap();
        scalar.compiled_derivative_operation = Some(Rc::new(Entry {
            calls: calls.clone(),
            fail,
        }));
        let mut guess = vec![10., 42.];
        let mut output = [-1.];
        let result = runtime
            .scalar_execution()
            .unwrap()
            .eval_state_derivatives_with_guess_into(
                0.,
                &[10.],
                &[],
                &mut guess,
                1e-8,
                10,
                &mut output,
            );
        assert_eq!(calls.get(), 1);
        if fail {
            assert!(
                result
                    .unwrap_err()
                    .to_string()
                    .contains("complete value failure")
            );
            assert!(output[0].is_nan());
        } else {
            result.unwrap();
            assert_eq!(output, [99.]);
            assert_eq!(guess[1], 88.);
        }
    }
}
impl SolveExecutionBackend for SourceBackend {
    fn compile_fmi_derivative_operation(
        &self,
        root: solve::RetainedAssertionRoot,
    ) -> Result<Option<Rc<dyn CompiledSolveDerivativeOperation>>, String> {
        assert!(root.shares_root_owner(&self.expected));
        assert!(std::ptr::eq(root.metadata(), self.expected.metadata()));
        self.calls.set(self.calls.get() + 1);
        Ok(None)
    }
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
        test_kernels::expression(source).map_err(Into::into)
    }
    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, crate::CompileRefusal> {
        test_kernels::jacobian(source).map_err(Into::into)
    }
}

#[test]
fn complete_preparation_receives_the_original_correlated_root() {
    let mut model = solve::SolveModel::default();
    model.problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
            .unwrap();
    let component = solve::fmi::FmiComponent::construct(model, vec![]).unwrap();
    let root = component.runtime_view().retain_assertion_root();
    let calls = Rc::new(Cell::new(0));
    let backend = Rc::new(SourceBackend {
        expected: root.clone(),
        calls: calls.clone(),
    });
    let backend = crate::fmi_me::MeExecutionBackend::new(backend).into_runtime_backend();
    let _runtime = SolveRuntime::construct(
        SolveRuntimeModel::retained(root),
        Some(backend),
        ModelStateJacobianRequest::NotRequested,
        None,
    )
    .unwrap();
    assert_eq!(calls.get(), 1);
}

struct BindingBackend {
    bind: bool,
}
impl SolveExecutionBackend for BindingBackend {
    fn compile_fmi_derivative_operation(
        &self,
        _: solve::RetainedAssertionRoot,
    ) -> Result<Option<Rc<dyn CompiledSolveDerivativeOperation>>, String> {
        Ok(self.bind.then(|| {
            Rc::new(Entry {
                calls: Rc::default(),
                fail: false,
            }) as Rc<_>
        }))
    }
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
        test_kernels::expression(source).map_err(Into::into)
    }
    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, crate::CompileRefusal> {
        test_kernels::jacobian(source).map_err(Into::into)
    }
}

#[test]
fn the_receipt_reports_whether_the_complete_and_kinematic_entries_are_bound() {
    let adoption = |bind: bool| {
        let mut model = solve::SolveModel::default();
        model.problem.continuous.refresh_owners =
            rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
                .unwrap();
        let component = solve::fmi::FmiComponent::construct(model, vec![]).unwrap();
        let root = component.runtime_view().retain_assertion_root();
        let handle = crate::fmi_me::MeExecutionBackend::new(Rc::new(BindingBackend { bind }));
        let _runtime = SolveRuntime::construct(
            SolveRuntimeModel::retained(root),
            Some(handle.clone().into_runtime_backend()),
            ModelStateJacobianRequest::NotRequested,
            None,
        )
        .unwrap();
        handle
            .programs()
            .into_iter()
            .map(|entry| (entry.program, entry.bound, entry.reason))
            .collect::<Vec<_>>()
    };
    assert_eq!(
        adoption(true),
        [
            ("fmi_derivative_operation".to_owned(), true, None),
            (
                "fmi_kinematic_derivative".to_owned(),
                false,
                Some("the compiled operation binds no kinematic entry".to_owned())
            ),
        ]
    );
    let unbound = Some("the execution backend binds no complete operation".to_owned());
    assert_eq!(
        adoption(false),
        [
            (
                "fmi_derivative_operation".to_owned(),
                false,
                unbound.clone()
            ),
            ("fmi_kinematic_derivative".to_owned(), false, unbound),
        ]
    );
}
