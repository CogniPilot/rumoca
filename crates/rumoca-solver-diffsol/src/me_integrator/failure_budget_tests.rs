use diffsol::nonlinear_solver::convergence::{Convergence, ConvergenceStatus};
use diffsol::{
    Bdf, DiffsolError, FaerLU, FaerMat, FaerVec, LineSearch, NewtonNonlinearSolver, NoLineSearch,
    OdeBuilder, OdeSolverMethod,
};

/// Inject a recoverable first attempt, or an unresolved step, into the same
/// exact constant-derivative problem. No model or event handling participates.
#[derive(Default)]
struct FailingLineSearch<const ALWAYS: bool> {
    attempts: usize,
    inner: NoLineSearch,
}

impl<const ALWAYS: bool> LineSearch<FaerVec<f64>> for FailingLineSearch<ALWAYS> {
    fn reset(&mut self) {
        self.attempts += 1;
    }

    fn take_optimal_step(
        &mut self,
        x: &mut FaerVec<f64>,
        delta: &mut FaerVec<f64>,
        error_y: &FaerVec<f64>,
        fun: &impl Fn(&FaerVec<f64>, &mut FaerVec<f64>),
        linear_solver: &impl Fn(&mut FaerVec<f64>) -> Result<(), DiffsolError>,
        convergence: &mut Convergence<FaerVec<f64>>,
    ) -> Result<ConvergenceStatus, DiffsolError> {
        if ALWAYS || self.attempts % 2 == 1 {
            return Ok(ConvergenceStatus::Diverged);
        }
        self.inner
            .take_optimal_step(x, delta, error_y, fun, linear_solver, convergence)
    }
}

fn check_failure_budget<const ALWAYS: bool>() {
    let mut problem = OdeBuilder::<FaerMat<f64>>::new()
        .rtol(1e-6)
        .atol([1e-6])
        .rhs_implicit(
            |_x, _p, _t, out| out[0] = 1.0,
            |_x, _p, _t, _v, out| out[0] = 0.0,
        )
        .init(|_p, _t, out| out[0] = 0.0, 1)
        .build()
        .unwrap();
    problem.ode_options.max_nonlinear_solver_failures = 2;
    let state = problem.bdf_state::<FaerLU<f64>>().unwrap();
    let newton = NewtonNonlinearSolver::new(
        FaerLU::<f64>::default(),
        FailingLineSearch::<ALWAYS>::default(),
    );
    let mut method: Bdf<'_, _, _, FaerMat<f64>> = Bdf::new(&problem, state, newton).unwrap();
    if ALWAYS {
        let error = method.step().unwrap_err();
        assert!(matches!(
            error,
            DiffsolError::OdeSolverError(
                diffsol::error::OdeSolverError::TooManyNonlinearSolverFailures {
                    time: 0.0,
                    num_failures: 3,
                }
            )
        ));
        assert_eq!(method.state().t, 0.0);
        assert_eq!(method.state().y[0], 0.0);
        return;
    }
    for _ in 0..8 {
        let old_time = method.state().t;
        method
            .step()
            .expect("an earlier recovered step must not exhaust this step's budget");
        let state = method.state();
        assert!(state.t > old_time);
        assert!((state.y[0] - state.t).abs() < 1e-12);
    }
    assert_eq!(method.get_statistics().number_of_nonlinear_solver_fails, 8);
}

#[test]
fn bdf_nonlinear_failure_budget_excludes_previously_accepted_steps() {
    check_failure_budget::<false>();
}

#[test]
fn bdf_nonlinear_failure_budget_rejects_an_unresolved_step() {
    check_failure_budget::<true>();
}
