use crate::{
    error::{DiffsolError, NonLinearSolverError},
    non_linear_solver_error,
    nonlinear_solver::convergence::ConvergenceStatus,
    Convergence, InitialConditionSolverOptions, Scalar, Vector,
};
use log::warn;
use num_traits::{FromPrimitive, One, Pow};

/// Line search trait for nonlinear solvers
/// The line search is used to find an optimal step size for the Newton iteration.
/// The line search modifies the delta vector in place to scale it by the optimal step size
/// The line search returns the norm of the modified delta vector
/// The x vector is also modified in place to take the optimal step
pub trait LineSearch<V: Vector>: Default {
    /// Take the optimal step for the current iteration
    ///
    /// # Arguments
    /// - `x`: current solution vector, modified in place to take the optimal step
    /// - `delta`: current Newton step vector, modified in place to scale by the optimal step size (previous value is overwritten)
    /// - `error_y`: current error estimate vector, used to compute the norm
    /// - `fun`: function to compute the residual F(x), takes x and modifies delta in place
    /// - `linear_solver`: function to solve the linear system J delta = -F(x), takes delta and modifies it in place
    /// - `convergence`: convergence object to compute norms and check convergence
    ///
    /// # Returns
    /// - `ConvergenceStatus`: status of the convergence after taking the optimal step
    /// - `DiffsolError`: error if the line search fails
    fn take_optimal_step(
        &mut self,
        x: &mut V,
        delta: &mut V,
        error_y: &V,
        fun: &impl Fn(&V, &mut V),
        linear_solver: &impl Fn(&mut V) -> Result<(), DiffsolError>,
        convergence: &mut Convergence<V>,
    ) -> Result<ConvergenceStatus, DiffsolError>;

    /// Reset the line search state
    fn reset(&mut self);

    /// Rebind cached line-search state to a freshly solved correction after a
    /// supplemental observer rejects an otherwise state-converged iterate.
    /// Stateless line searches do not need to do anything here.
    fn refresh_pending_correction(
        &mut self,
        _delta: &V,
        _error_y: &V,
        _convergence: &Convergence<V>,
    ) {
    }
}

#[derive(Default)]
pub struct NoLineSearch;

impl<V: Vector> LineSearch<V> for NoLineSearch {
    /// No line search implementation, simply takes the full step
    fn take_optimal_step(
        &mut self,
        x: &mut V,
        delta: &mut V,
        error_y: &V,
        fun: &impl Fn(&V, &mut V),
        linear_solver: &impl Fn(&mut V) -> Result<(), DiffsolError>,
        convergence: &mut Convergence<V>,
    ) -> Result<ConvergenceStatus, DiffsolError> {
        //delta = f_at_n
        fun(x, delta);

        //delta = -delta_n
        linear_solver(delta)?;

        // xn = xn + delta_n
        x.sub_assign(&*delta);

        // norm
        let norm = convergence.norm(delta, error_y);
        Ok(convergence.check_new_iteration(norm))
    }

    fn reset(&mut self) {}
}

/// Backtracking line search implementation, derived from backtracking line search algorithm
/// in Sundials IDA solver (<https://github.com/LLNL/sundials/blob/main/src/ida/ida_ic.c#L480>)
///
/// Parameters:
/// - tau: step size reduction factor (0 < tau < 1), default 0.5
/// - c: Armijo condition constant (0 < c < 1), default 1e-4
/// - steptol: minimum step size, default eps^(2/3)
/// - max_iter: maximum number of line search iterations, default 100
/// - n_iters: number of line search iterations performed during last call
///
pub struct BacktrackingLineSearch<V: Vector> {
    pub tau: V::T,
    pub c: V::T,
    pub steptol: V::T,
    pub max_iter: usize,
    pub n_iters: usize,
    delta0: V,
    x0: V,
    norm: V::T,
}

impl<V: Vector> Default for BacktrackingLineSearch<V> {
    fn default() -> Self {
        let ic_options = InitialConditionSolverOptions::<V::T>::default();
        Self {
            tau: ic_options.step_reduction_factor,
            c: ic_options.armijo_constant,
            steptol: V::T::EPSILON.pow(V::T::from_f64(2.0 / 3.0).unwrap()),
            max_iter: ic_options.max_linesearch_iterations,
            n_iters: 0,
            delta0: V::zeros(0, Default::default()),
            x0: V::zeros(0, Default::default()),
            norm: V::T::one(),
        }
    }
}

impl<V: Vector> LineSearch<V> for BacktrackingLineSearch<V> {
    fn reset(&mut self) {
        self.n_iters = 0;
    }

    fn refresh_pending_correction(&mut self, delta: &V, error_y: &V, convergence: &Convergence<V>) {
        self.norm = convergence.norm(delta, error_y);
    }
    fn take_optimal_step(
        &mut self,
        x: &mut V,
        delta: &mut V,
        error_y: &V,
        fun: &impl Fn(&V, &mut V),
        linear_solver: &impl Fn(&mut V) -> Result<(), DiffsolError>,
        convergence: &mut Convergence<V>,
    ) -> Result<ConvergenceStatus, DiffsolError> {
        // on the first iteration, we need to init delta and norm
        if convergence.niter() == 0 {
            //delta = f_at_n
            fun(x, delta);

            //delta = -delta_n
            linear_solver(delta)?;

            self.norm = convergence.norm(delta, error_y);

            if self.norm.is_nan() {
                warn!("Linesearch: Convergence norm is NaN on first iteration. Check model for NaN in residual computation.");
            }
        }

        // The contraction estimate bounds error after this correction. A
        // correction computed by the preceding line search is still pending;
        // apply it before returning success, and record its norm only once.
        match convergence.check_new_iteration(self.norm) {
            ConvergenceStatus::Converged => {
                x.sub_assign(&*delta);
                return Ok(ConvergenceStatus::Converged);
            }
            ConvergenceStatus::Diverged => return Ok(ConvergenceStatus::Diverged),
            ConvergenceStatus::Continue => {}
        }

        if self.x0.len() == 0 {
            self.x0 = V::zeros(x.len(), x.context().clone());
            self.delta0 = V::zeros(delta.len(), delta.context().clone());
        }
        self.x0.copy_from(x);
        self.delta0.copy_from(delta);
        let half = V::T::from_f64(0.5).unwrap();

        // backtracking line search on phi = 0.5 ||F||^2
        let norm = self.norm;
        let phi0 = norm * norm * half;
        let two_phi0 = norm * norm;
        let min_alpha = self.steptol / norm;
        let mut alpha = V::T::one();

        for i in 0..self.max_iter {
            // take the step and recompute the norm
            x.axpy(-alpha, &self.delta0, V::T::one());
            // xi = x0 + alpha * delta_n

            fun(x, delta);
            //delta_p = f_at_n

            linear_solver(delta)?;
            //delta_p = -delta_n

            let new_norm = convergence.norm(delta, error_y);
            self.n_iters = i;

            // directional derivative for phi along p is: grad_phi^T p = F^T J p = -||F||^2
            // so the Armijo condition reduces to phi(u+α p) <= phi0 - c * α * ||F||^2``
            let phi1 = new_norm * new_norm * half;

            if phi1 <= phi0 - self.c * alpha * two_phi0 {
                self.norm = new_norm;
                // delta now belongs to the accepted trial point. Its next
                // outer iteration consumes both the correction and its norm.
                return Ok(ConvergenceStatus::Continue);
            }
            if alpha < min_alpha {
                warn!(
                    "Linesearch: Step size fell below minimum threshold. This usually indicates: \
                    (1) model produces NaN/Inf, (2) very ill-conditioned Jacobian, or (3) incorrect initial conditions."
                );
                return Err(non_linear_solver_error!(LinesearchFailedMinStep));
            }

            alpha *= self.tau;

            // reset x
            x.copy_from(&self.x0);
        }
        warn!(
            "Linesearch: Failed to find acceptable step after {} iterations. \
            This usually indicates a stiff problem or model producing NaN/Inf values.",
            self.max_iter
        );
        Err(non_linear_solver_error!(LinesearchFailedMaxIterations))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ConvergenceStatus, NalgebraVec};

    fn run_history_fixture(refresh: bool) -> (ConvergenceStatus, f64) {
        let mut line_search = BacktrackingLineSearch::<NalgebraVec<f64>>::default();
        let error_y = NalgebraVec::from_vec(vec![1.0], Default::default());
        let mut convergence = Convergence::with_tolerance(0.0, &error_y, 0.02);
        convergence.set_max_iter(3);
        let fun = |x: &NalgebraVec<f64>, residual: &mut NalgebraVec<f64>| {
            residual.set_index(0, 9.0 * x.get_index(0) - 8.2);
        };
        let chord_solve = |residual: &mut NalgebraVec<f64>| -> Result<(), DiffsolError> {
            residual.set_index(0, residual.get_index(0) / 10.0);
            Ok(())
        };
        let mut x = NalgebraVec::from_vec(vec![1.8], Default::default());
        let mut delta = NalgebraVec::zeros(1, Default::default());

        let status = line_search
            .take_optimal_step(
                &mut x,
                &mut delta,
                &error_y,
                &fun,
                &chord_solve,
                &mut convergence,
            )
            .unwrap();
        assert!(matches!(status, ConvergenceStatus::Continue));
        assert!((x.get_index(0) - 1.0).abs() < 1e-12);
        assert!((delta.get_index(0) - 0.08).abs() < 1e-12);

        let status = line_search
            .take_optimal_step(
                &mut x,
                &mut delta,
                &error_y,
                &fun,
                &chord_solve,
                &mut convergence,
            )
            .unwrap();
        assert!(matches!(status, ConvergenceStatus::Converged));
        assert!((x.get_index(0) - 0.92).abs() < 1e-12);

        fun(&x, &mut delta);
        chord_solve(&mut delta).unwrap();
        assert!((delta.get_index(0) - 0.008).abs() < 1e-12);
        if refresh {
            line_search.refresh_pending_correction(&delta, &error_y, &convergence);
        }

        let status = line_search
            .take_optimal_step(
                &mut x,
                &mut delta,
                &error_y,
                &fun,
                &chord_solve,
                &mut convergence,
            )
            .unwrap();
        (status, x.get_index(0))
    }

    #[test]
    fn observer_refresh_rebinds_backtracking_norm_history() {
        let (refreshed_status, refreshed_x) = run_history_fixture(true);
        assert!(matches!(refreshed_status, ConvergenceStatus::Converged));
        assert!((refreshed_x - 0.912).abs() < 1e-12);

        let (stale_status, stale_x) = run_history_fixture(false);
        assert!(matches!(stale_status, ConvergenceStatus::Diverged));
        assert!((stale_x - 0.92).abs() < 1e-12);
    }
}
