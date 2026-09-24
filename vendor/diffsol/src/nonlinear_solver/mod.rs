use crate::{error::DiffsolError, Matrix, NonLinearOp, NonLinearOpJacobian};
use convergence::Convergence;

/// An optional, host-supplied error check for a returned nonlinear iterate.
///
/// The callback is deliberately expressed only in terms of the vendor's
/// scalar/vector types.  A host may use it to evaluate an observable at the
/// returned trial state and its remaining correction without exposing any
/// model, IR, or FMI types to the numerical backend.
pub trait SupplementalErrorNorm<V: crate::Vector> {
    /// Return a normalized error score. The numerical method applies its
    /// existing state-specific acceptance budget to this score.
    /// `estimated_delta` is added to `actual_trial_state`, so it represents
    /// a signed correction or displacement to an embedded alternative.
    fn supplemental_error(
        &mut self,
        time: V::T,
        actual_trial_state: &V,
        estimated_delta: &V,
    ) -> Result<V::T, DiffsolError>;
}

pub struct NonLinearSolveSolution<V> {
    pub x0: V,
    pub x: V,
}

impl<V> NonLinearSolveSolution<V> {
    pub fn new(x0: V, x: V) -> Self {
        Self { x0, x }
    }
}

/// A solver for the nonlinear problem `F(x) = 0`.
pub trait NonLinearSolver<M: Matrix>: Default {
    /// Set the problem to be solved, any previous problem is discarded.
    fn set_problem<C: NonLinearOpJacobian<V = M::V, T = M::T, M = M, C = M::C>>(&mut self, op: &C);

    fn is_jacobian_set(&self) -> bool;

    /// Reset the approximation of the Jacobian matrix.
    fn reset_jacobian<C: NonLinearOpJacobian<V = M::V, T = M::T, M = M, C = M::C>>(
        &mut self,
        op: &C,
        x: &M::V,
        t: M::T,
    );

    /// Clear the approximation of the Jacobian matrix.
    fn clear_jacobian(&mut self);

    // Solve the problem `F(x, t) = 0` for fixed t, and return the solution `x`.
    fn solve<C: NonLinearOp<V = M::V, T = M::T, M = M>>(
        &mut self,
        op: &C,
        x: &M::V,
        t: M::T,
        error_y: &M::V,
        convergence: &mut Convergence<'_, M::V>,
    ) -> Result<M::V, DiffsolError> {
        let mut x = x.clone();
        self.solve_in_place(op, &mut x, t, error_y, convergence)?;
        Ok(x)
    }

    /// Solve the problem `F(x) = 0` in place.
    fn solve_in_place<C: NonLinearOp<V = M::V, T = M::T, M = M>>(
        &mut self,
        op: &C,
        x: &mut C::V,
        t: C::T,
        error_y: &C::V,
        convergence: &mut Convergence<'_, M::V>,
    ) -> Result<(), DiffsolError>;

    /// Solve in place with a supplemental check on the returned iterate.
    /// Backends must override this method when they can honor the observer.
    fn solve_in_place_with_observer<C: NonLinearOp<V = M::V, T = M::T, M = M>>(
        &mut self,
        _op: &C,
        _x: &mut C::V,
        _t: C::T,
        _error_y: &C::V,
        _convergence: &mut Convergence<'_, M::V>,
        _observer: &mut dyn SupplementalErrorNorm<C::V>,
    ) -> Result<(), DiffsolError> {
        Err(DiffsolError::from(
            crate::error::NonLinearSolverError::SupplementalErrorUnsupported,
        ))
    }

    /// Solve the linearised problem `J * x = b`, where `J` was calculated using [Self::reset_jacobian].
    /// The input `b` is provided in `x`, and the solution is returned in `x`.
    fn solve_linearised_in_place(&self, x: &mut M::V) -> Result<(), DiffsolError>;
}

pub mod convergence;
pub mod line_search;
pub mod newton;
pub mod root;

//tests
#[cfg(test)]
pub mod tests {
    use self::newton::NewtonNonlinearSolver;
    use crate::{
        linear_solver::nalgebra::lu::LU,
        matrix::{dense_nalgebra_serial::NalgebraMat, MatrixCommon},
        op::{closure::Closure, ParameterisedOp},
        scale, BacktrackingLineSearch, DenseMatrix, LineSearch, NalgebraVec, NoLineSearch, Op,
        Vector,
    };

    use super::*;
    use num_traits::{FromPrimitive, One, Zero};
    use std::marker::PhantomData;

    struct AnalyticSupplementalCheck {
        calls: usize,
        first_state: Option<NalgebraVec<f64>>,
        first_delta: Option<NalgebraVec<f64>>,
        lambda: f64,
        rtol: f64,
        atol: f64,
    }

    impl SupplementalErrorNorm<NalgebraVec<f64>> for AnalyticSupplementalCheck {
        fn supplemental_error(
            &mut self,
            _time: f64,
            actual_trial_state: &NalgebraVec<f64>,
            estimated_delta: &NalgebraVec<f64>,
        ) -> Result<f64, DiffsolError> {
            self.calls += 1;
            if self.calls == 1 {
                self.first_state = Some(actual_trial_state.clone());
                self.first_delta = Some(estimated_delta.clone());
            }
            let actual_q = self.lambda * (2.0 - actual_trial_state.get_index(0));
            let corrected_q = self.lambda
                * (2.0 - (actual_trial_state.get_index(0) + estimated_delta.get_index(0)));
            let q_error = (corrected_q - actual_q).abs();
            let q_budget = self.atol + self.rtol * actual_q.abs();
            Ok(q_error / q_budget)
        }
    }

    struct DefaultObserverSolver<M: Matrix>(PhantomData<M>);

    impl<M: Matrix> Default for DefaultObserverSolver<M> {
        fn default() -> Self {
            Self(PhantomData)
        }
    }

    impl<M: Matrix> NonLinearSolver<M> for DefaultObserverSolver<M> {
        fn set_problem<C: NonLinearOpJacobian<V = M::V, T = M::T, M = M, C = M::C>>(
            &mut self,
            _op: &C,
        ) {
        }

        fn is_jacobian_set(&self) -> bool {
            false
        }

        fn reset_jacobian<C: NonLinearOpJacobian<V = M::V, T = M::T, M = M, C = M::C>>(
            &mut self,
            _op: &C,
            _x: &M::V,
            _t: M::T,
        ) {
        }

        fn clear_jacobian(&mut self) {}

        fn solve_in_place<C: NonLinearOp<V = M::V, T = M::T, M = M>>(
            &mut self,
            _op: &C,
            _x: &mut C::V,
            _t: C::T,
            _error_y: &C::V,
            _convergence: &mut Convergence<'_, M::V>,
        ) -> Result<(), DiffsolError> {
            panic!("the default observer path must reject before solving")
        }

        fn solve_linearised_in_place(&self, _x: &mut M::V) -> Result<(), DiffsolError> {
            panic!("the test solver has no linear solve")
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn get_square_problem<M>() -> (
        Closure<
            M,
            impl Fn(&M::V, &M::V, M::T, &mut M::V),
            impl Fn(&M::V, &M::V, M::T, &M::V, &mut M::V),
        >,
        M::T,
        M::V,
        Vec<NonLinearSolveSolution<M::V>>,
    )
    where
        M: DenseMatrix + 'static,
    {
        let jac1 = M::from_diagonal(&M::V::from_vec(
            vec![M::T::from_f64(2.0).unwrap(), M::T::from_f64(2.0).unwrap()],
            Default::default(),
        ));
        let jac2 = jac1.clone();
        let p = M::V::zeros(0, jac1.context().clone());
        let eights = M::V::from_vec(
            vec![M::T::from_f64(8.0).unwrap(), M::T::from_f64(8.0).unwrap()],
            jac1.context().clone(),
        );
        let op = Closure::new(
            // 0 = J * x * x - 8
            move |x: &<M as MatrixCommon>::V, _p: &<M as MatrixCommon>::V, _t, y| {
                jac1.gemv(M::T::one(), x, M::T::zero(), y); // y = J * x
                y.component_mul_assign(x);
                y.axpy(-M::T::one(), &eights, M::T::one());
            },
            // J = 2 * J * x * dx
            move |x: &<M as MatrixCommon>::V, _p: &<M as MatrixCommon>::V, _t, v, y| {
                jac2.gemv(M::T::from_f64(2.0).unwrap(), x, M::T::zero(), y); // y = 2 * J * x
                y.component_mul_assign(v);
            },
            2,
            2,
            p.len(),
            p.context().clone(),
        );
        let rtol = M::T::from_f64(1e-6).unwrap();
        let atol = M::V::from_vec(
            vec![M::T::from_f64(1e-6).unwrap(), M::T::from_f64(1e-6).unwrap()],
            p.context().clone(),
        );
        let solns = vec![NonLinearSolveSolution::new(
            M::V::from_vec(
                vec![M::T::from_f64(2.1).unwrap(), M::T::from_f64(2.1).unwrap()],
                p.context().clone(),
            ),
            M::V::from_vec(
                vec![M::T::from_f64(2.0).unwrap(), M::T::from_f64(2.0).unwrap()],
                p.context().clone(),
            ),
        )];
        (op, rtol, atol, solns)
    }

    pub fn test_nonlinear_solver<C>(
        mut solver: impl NonLinearSolver<C::M>,
        op: C,
        rtol: C::T,
        atol: &C::V,
        solns: Vec<NonLinearSolveSolution<C::V>>,
    ) where
        C: NonLinearOpJacobian,
    {
        solver.set_problem(&op);
        let mut convergence = Convergence::new(rtol, atol);
        let t = C::T::zero();
        solver.reset_jacobian(&op, &solns[0].x0, t);
        for soln in solns {
            let x = solver
                .solve(&op, &soln.x0, t, &soln.x0, &mut convergence)
                .unwrap();
            let tol = x.clone() * scale(rtol) + atol;
            x.assert_eq(&soln.x, &tol);
        }
    }

    type MCpu = NalgebraMat<f64>;

    #[test]
    fn test_newton_cpu_square() {
        let lu = LU::default();
        let (op, rtol, atol, soln) = get_square_problem::<MCpu>();
        let p = NalgebraVec::zeros(0, *op.context());
        let op = ParameterisedOp::new(&op, &p);
        let nls = NoLineSearch;
        let s = NewtonNonlinearSolver::new(lu, nls);
        test_nonlinear_solver(s, op, rtol, &atol, soln);
    }

    #[test]
    fn test_newton_cpu_square_backtrack() {
        let lu = LU::default();
        let (op, rtol, atol, soln) = get_square_problem::<MCpu>();
        let p = NalgebraVec::zeros(0, *op.context());
        let op = ParameterisedOp::new(&op, &p);
        let ls = BacktrackingLineSearch::default();
        let s = NewtonNonlinearSolver::new(lu, ls);
        test_nonlinear_solver(s, op, rtol, &atol, soln);
    }

    #[test]
    fn default_observer_path_returns_typed_unsupported_error() {
        let (op, rtol, atol, solns) = get_square_problem::<MCpu>();
        let p = NalgebraVec::zeros(0, *op.context());
        let op = ParameterisedOp::new(&op, &p);
        let mut solver = DefaultObserverSolver::<MCpu>::default();
        let mut convergence = Convergence::new(rtol, &atol);
        let mut observer = AnalyticSupplementalCheck {
            calls: 0,
            first_state: None,
            first_delta: None,
            lambda: 1.0e6,
            rtol,
            atol: atol.get_index(0),
        };
        let mut x = solns[0].x0.clone();
        let error = solver
            .solve_in_place_with_observer(
                &op,
                &mut x,
                0.0,
                &solns[0].x0,
                &mut convergence,
                &mut observer,
            )
            .unwrap_err();
        assert!(matches!(
            error,
            DiffsolError::NonLinearSolverError(
                crate::error::NonLinearSolverError::SupplementalErrorUnsupported
            )
        ));
        assert_eq!(observer.calls, 0);
    }

    fn run_newton_supplemental_analytic_check<Lsearch>(line_search: Lsearch)
    where
        Lsearch: LineSearch<NalgebraVec<f64>>,
    {
        let lu = LU::default();
        let (op, rtol, atol, solns) = get_square_problem::<MCpu>();
        let p = NalgebraVec::zeros(0, *op.context());
        let op = ParameterisedOp::new(&op, &p);
        let mut solver = NewtonNonlinearSolver::new(lu, line_search);
        solver.set_problem(&op);
        let mut convergence = Convergence::new(rtol, &atol);
        solver.reset_jacobian(&op, &solns[0].x0, 0.0);
        let lambda = 1.0e6;
        let mut observer = AnalyticSupplementalCheck {
            calls: 0,
            first_state: None,
            first_delta: None,
            lambda,
            rtol: rtol,
            atol: atol.get_index(0),
        };

        let mut x = solns[0].x0.clone();
        solver
            .solve_in_place_with_observer(
                &op,
                &mut x,
                0.0,
                &solns[0].x0,
                &mut convergence,
                &mut observer,
            )
            .unwrap();

        if let (Some(first_state), Some(first_delta)) = (observer.first_state, observer.first_delta)
        {
            assert!(observer.calls >= 2);
            assert!(first_state.get_index(0) > 1.9);
            assert!(first_delta.norm(2) < 1e-3);
        }
        let tol = x.clone() * scale(rtol) + &atol;
        x.assert_eq(&solns[0].x, &tol);
        let q = lambda * (2.0 - x.get_index(0));
        let q_budget = atol.get_index(0) + rtol * q.abs();
        assert!(
            q.abs() <= q_budget,
            "analytic observable q={q:.3e} exceeds declared budget {q_budget:.3e}"
        );
    }

    #[test]
    fn test_newton_supplemental_check_uses_returned_state_and_remaining_delta() {
        run_newton_supplemental_analytic_check(NoLineSearch);
    }

    #[test]
    fn test_newton_supplemental_check_refreshes_backtracking_correction() {
        run_newton_supplemental_analytic_check(BacktrackingLineSearch::default());
    }
}
