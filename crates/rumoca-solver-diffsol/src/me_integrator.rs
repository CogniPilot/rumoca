//! Diffsol BDF behind the solver-neutral FMI ME plugin contract.
//!
//! Only numerical-method state lives here: the BDF problem, nonlinear solver,
//! multistep history, and the most recently accepted native continuous
//! extension. FMI lifecycle, roots, events, output cadence, and trace policy
//! remain in the common host.

use std::{cell::Cell, rc::Rc};

use diffsol::{
    BacktrackingLineSearch, BdfState, Closure, ConstantClosure, DefaultDenseMatrix, DiffsolError,
    NewtonNonlinearSolver, OdeBuilder, OdeSolverMethod, OdeSolverProblem, OdeSolverState,
    OdeSolverStopReason, UnitCallable, VectorHost, error::OdeSolverError,
};
use rumoca_solver::fmi_me::{
    MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeIntegrationError,
    MeIntegratorBackend, MeNumericalFailure, MeNumericalSetup, MeStepCandidate,
    accepted_interval_contains,
};
use self_cell::self_cell;

use crate::{LinearSolver, Matrix, Scalar, Vector};

const METHOD: &str = "diffsol-bdf";

type RhsFn = Box<dyn Fn(&Vector, &Vector, Scalar, &mut Vector)>;
type JacobianFn = Box<dyn Fn(&Vector, &Vector, Scalar, &Vector, &mut Vector)>;
type InitialFn = Box<dyn Fn(&Vector, Scalar, &mut Vector)>;
type BdfRhs = Closure<Matrix, RhsFn, JacobianFn>;
type BdfInitial = ConstantClosure<Matrix, InitialFn>;
type EmptyOperation = UnitCallable<Matrix>;
type BdfEquations = diffsol::OdeSolverEquations<
    Matrix,
    BdfRhs,
    BdfInitial,
    EmptyOperation,
    EmptyOperation,
    EmptyOperation,
    EmptyOperation,
>;
type BdfProblem = OdeSolverProblem<BdfEquations>;
type BdfNonlinear = NewtonNonlinearSolver<Matrix, LinearSolver, BacktrackingLineSearch<Vector>>;
type BdfDenseMatrix = <Vector as DefaultDenseMatrix>::M;
type BdfSolver<'problem> = diffsol::Bdf<
    'problem,
    BdfEquations,
    BdfNonlinear,
    BdfDenseMatrix,
    diffsol::NoAug<BdfEquations>,
>;

self_cell!(
    /// One owned Diffsol problem and the BDF method that borrows it.
    ///
    /// Diffsol intentionally lends a problem to its method. Keeping the pair in
    /// a safe self-referential cell preserves multistep history across host
    /// calls without leaking the problem or manufacturing a `'static` borrow.
    struct BdfCell {
        owner: BdfProblem,

        #[not_covariant]
        dependent: BdfSolver,
    }
);

/// Build the Diffsol numerical plugin accepted by the common FMI ME host.
///
/// No Diffsol-specific type crosses the returned trait-object boundary.
#[must_use]
pub fn model_exchange_integrator(
    setup: MeNumericalSetup,
) -> Box<dyn MeIntegratorBackend + 'static> {
    Box::new(DiffsolBdfIntegrator::new(setup))
}

struct DiffsolBdfIntegrator {
    setup: MeNumericalSetup,
    derivatives: Option<Rc<MeDerivativeHandle>>,
    solver: Option<BdfCell>,
    accepted_interval: Option<AcceptedInterval>,
}

impl DiffsolBdfIntegrator {
    fn new(setup: MeNumericalSetup) -> Self {
        Self {
            setup,
            derivatives: None,
            solver: None,
            accepted_interval: None,
        }
    }

    fn rebuild(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: Rc<MeDerivativeHandle>,
        failure: MeNumericalFailure,
    ) -> Result<(), MeIntegrationError> {
        self.require_width(point, derivatives.as_ref(), failure)?;
        let absolute_tolerances = scaled_absolute_tolerances(&self.setup)?;
        let problem = build_problem(
            point,
            Rc::clone(&derivatives),
            self.setup.relative_tolerance(),
            absolute_tolerances,
            self.setup.initial_step_hint().unwrap_or(1.0e-3),
        )?;
        let initial_derivatives = derivatives
            .derivatives(point.time(), point.states())
            .map_err(MeIntegrationError::from)?;
        let state = initial_state(&problem, point, &initial_derivatives, failure)?;
        let solver = BdfCell::try_new(problem, move |owned| {
            diffsol::Bdf::new(
                owned,
                state,
                NewtonNonlinearSolver::new(
                    LinearSolver::default(),
                    BacktrackingLineSearch::default(),
                ),
            )
            .map_err(|error| numerical(failure, error))
        })?;
        if derivatives.has_failed() {
            return Err(MeIntegrationError::DerivativeRefused);
        }
        self.derivatives = Some(derivatives);
        self.solver = Some(solver);
        self.accepted_interval = None;
        Ok(())
    }

    fn require_width(
        &self,
        point: &MeContinuousPoint,
        derivatives: &MeDerivativeHandle,
        failure: MeNumericalFailure,
    ) -> Result<(), MeIntegrationError> {
        let width = self.setup.state_nominals().len();
        if point.width() == width && derivatives.state_count() == width {
            return Ok(());
        }
        Err(MeIntegrationError::numerical(
            METHOD,
            failure,
            format!(
                "numerical setup has width {width}, point has width {}, and derivative source has width {}",
                point.width(),
                derivatives.state_count()
            ),
        ))
    }

    fn require_solver(&self) -> Result<&BdfCell, MeIntegrationError> {
        self.solver.as_ref().ok_or_else(|| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Construction,
                "the host has not initialized the BDF problem",
            )
        })
    }

    fn require_solver_mut(&mut self) -> Result<&mut BdfCell, MeIntegrationError> {
        self.solver.as_mut().ok_or_else(|| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Construction,
                "the host has not initialized the BDF problem",
            )
        })
    }

    fn require_current_point(&self, request: &MeAdvanceRequest) -> Result<(), MeIntegrationError> {
        let solver = self.require_solver()?;
        solver.with_dependent(|_, method| {
            let state = method.state();
            if state.t.to_bits() != request.current().time().to_bits()
                || state.y.as_slice() != request.current().states()
            {
                return Err(MeIntegrationError::numerical(
                    METHOD,
                    MeNumericalFailure::AdvanceExhausted,
                    "the retained BDF history does not start at the host-issued current point",
                ));
            }
            Ok(())
        })
    }

    fn reset_in_place(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: Rc<MeDerivativeHandle>,
    ) -> Result<(), MeIntegrationError> {
        self.require_width(point, derivatives.as_ref(), MeNumericalFailure::Reset)?;
        let values = derivatives
            .derivatives(point.time(), point.states())
            .map_err(MeIntegrationError::from)?;
        self.require_solver_mut()?
            .with_dependent_mut(|problem, method| {
                let mut fresh = BdfState::<Vector>::new_without_initialise(problem)
                    .map_err(|error| numerical(MeNumericalFailure::Reset, error))?;
                {
                    let state = fresh.as_mut();
                    state.y.as_mut_slice().copy_from_slice(point.states());
                    state.dy.as_mut_slice().copy_from_slice(&values);
                    *state.t = point.time();
                }
                fresh.set_step_size(problem.h0, &problem.atol, problem.rtol, &problem.eqn, 1);
                fresh
                    .set_problem(problem)
                    .map_err(|error| numerical(MeNumericalFailure::Reset, error))?;
                method.set_state(fresh);
                match method.set_stop_time(point.time()) {
                    Ok(())
                    | Err(DiffsolError::OdeSolverError(OdeSolverError::StopTimeAtCurrentTime)) => {
                        Ok(())
                    }
                    Err(error) => Err(numerical(MeNumericalFailure::Reset, error)),
                }
            })?;
        if derivatives.has_failed() {
            return Err(MeIntegrationError::DerivativeRefused);
        }
        self.derivatives = Some(derivatives);
        self.accepted_interval = None;
        Ok(())
    }
}

impl MeIntegratorBackend for DiffsolBdfIntegrator {
    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        self.rebuild(
            point,
            Rc::new(derivatives),
            MeNumericalFailure::Construction,
        )
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        self.require_current_point(request)?;
        let latest = request.latest_accepted_time();
        let candidate = self.require_solver_mut()?.with_dependent_mut(|_, method| {
            method
                .set_stop_time(latest)
                .map_err(|error| numerical(MeNumericalFailure::AdvanceExhausted, error))?;
            let stop = method
                .step()
                .map_err(|error| numerical(MeNumericalFailure::AdvanceExhausted, error))?;
            if matches!(stop, OdeSolverStopReason::RootFound(..)) {
                return Err(MeIntegrationError::numerical(
                    METHOD,
                    MeNumericalFailure::AdvanceExhausted,
                    "Diffsol reported a root even though the numerical plugin owns no root function",
                ));
            }
            let state = method.state();
            let states = try_copy(state.y.as_slice(), "BDF accepted endpoint")?;
            let order = u32::try_from(method.order()).map_err(|_| {
                MeIntegrationError::numerical(
                    METHOD,
                    MeNumericalFailure::AdvanceExhausted,
                    "Diffsol reported an unrepresentable continuous-extension order",
                )
            })?;
            Ok(MeStepCandidate::new(state.t, states, order))
        })?;
        if self
            .derivatives
            .as_ref()
            .is_some_and(|derivatives| derivatives.has_failed())
        {
            return Err(MeIntegrationError::DerivativeRefused);
        }
        self.accepted_interval = Some(AcceptedInterval {
            start_time: request.current().time(),
            start_states: try_copy(request.current().states(), "BDF interval start")?,
            end_time: candidate.accepted_time(),
            end_states: try_copy(candidate.accepted_states(), "BDF interval end")?,
        });
        Ok(candidate)
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        let interval = self.accepted_interval.as_ref().ok_or_else(|| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                "no accepted BDF interval is available",
            )
        })?;
        if states.len() != self.setup.state_nominals().len() {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                "the sampler output width differs from the initialized component width",
            ));
        }
        if !accepted_interval_contains(interval.start_time, interval.end_time, time) {
            return Err(MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Interpolation,
                format!(
                    "sample time {time} lies outside [{}, {}]",
                    interval.start_time, interval.end_time
                ),
            ));
        }
        // The BDF state vectors are the native extension's endpoint values.
        // Diffsol's polynomial evaluator is intended for the open interval and
        // can accumulate a different endpoint rounding path, so preserve the
        // exact accepted points the host is validating.
        if time.to_bits() == interval.start_time.to_bits() || time < interval.start_time {
            states.copy_from_slice(&interval.start_states);
            return Ok(());
        }
        if time.to_bits() == interval.end_time.to_bits() || time > interval.end_time {
            states.copy_from_slice(&interval.end_states);
            return Ok(());
        }
        self.require_solver()?.with_dependent(|_, method| {
            let sampled = method
                .interpolate(time)
                .map_err(|error| numerical(MeNumericalFailure::Interpolation, error))?;
            states.copy_from_slice(sampled.as_slice());
            Ok(())
        })
    }

    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        let derivatives = self.derivatives.as_ref().cloned().ok_or_else(|| {
            MeIntegrationError::numerical(
                METHOD,
                MeNumericalFailure::Reset,
                "the host has not initialized the derivative capability",
            )
        })?;
        self.reset_in_place(point, derivatives)
    }
}

struct AcceptedInterval {
    start_time: f64,
    start_states: Vec<f64>,
    end_time: f64,
    end_states: Vec<f64>,
}

fn build_problem(
    point: &MeContinuousPoint,
    derivatives: Rc<MeDerivativeHandle>,
    relative_tolerance: f64,
    absolute_tolerances: Vec<f64>,
    initial_step: f64,
) -> Result<BdfProblem, MeIntegrationError> {
    let initial = try_copy(point.states(), "BDF initial point")?;
    let rhs_derivatives = Rc::clone(&derivatives);
    let rhs: RhsFn = Box::new(move |state, _parameters, time, output| {
        rhs_derivatives.derivatives_into(time, state.as_slice(), output.as_mut_slice());
    });
    let jacobian_derivatives = derivatives;
    let probing = Rc::new(Cell::new(true));
    let jacobian_probe = Rc::clone(&probing);
    let jacobian: JacobianFn = Box::new(move |state, _parameters, time, seed, output| {
        if jacobian_probe.get() {
            let magnitude = seed.as_slice().iter().copied().map(f64::abs).sum();
            output.as_mut_slice().fill(magnitude);
            return;
        }
        jacobian_derivatives.directional_derivative_into(
            time,
            state.as_slice(),
            seed.as_slice(),
            output.as_mut_slice(),
        );
    });
    let initialize: InitialFn = Box::new(move |_parameters, _time, output| {
        output.as_mut_slice().copy_from_slice(&initial);
    });
    let problem = OdeBuilder::<Matrix>::new()
        .t0(point.time())
        .h0(initial_step)
        .rtol(relative_tolerance)
        .atol(absolute_tolerances)
        .p(Vec::new())
        .rhs_implicit(rhs, jacobian)
        .init(initialize, point.width())
        .build()
        .map_err(|error| numerical(MeNumericalFailure::Construction, error));
    probing.set(false);
    problem
}

fn initial_state(
    problem: &BdfProblem,
    point: &MeContinuousPoint,
    derivatives: &[f64],
    failure: MeNumericalFailure,
) -> Result<BdfState<Vector>, MeIntegrationError> {
    let mut state = BdfState::<Vector>::new_without_initialise(problem)
        .map_err(|error| numerical(failure, error))?;
    {
        let values = state.as_mut();
        values.y.as_mut_slice().copy_from_slice(point.states());
        values.dy.as_mut_slice().copy_from_slice(derivatives);
        *values.t = point.time();
    }
    state.set_step_size(problem.h0, &problem.atol, problem.rtol, &problem.eqn, 1);
    Ok(state)
}

fn try_copy(source: &[f64], context: &'static str) -> Result<Vec<f64>, MeIntegrationError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(source.len())
        .map_err(|_| MeIntegrationError::Allocation {
            context,
            entries: source.len(),
        })?;
    values.extend_from_slice(source);
    Ok(values)
}

fn scaled_absolute_tolerances(setup: &MeNumericalSetup) -> Result<Vec<f64>, MeIntegrationError> {
    let nominals = setup.state_nominals();
    let mut tolerances = Vec::new();
    tolerances
        .try_reserve_exact(nominals.len())
        .map_err(|_| MeIntegrationError::Allocation {
            context: "BDF absolute tolerances",
            entries: nominals.len(),
        })?;
    let absolute_tolerance = setup.absolute_tolerance();
    tolerances.extend(
        nominals
            .iter()
            .map(|nominal| (absolute_tolerance * nominal).clamp(f64::MIN_POSITIVE, f64::MAX)),
    );
    Ok(tolerances)
}

fn numerical(category: MeNumericalFailure, error: impl std::fmt::Display) -> MeIntegrationError {
    MeIntegrationError::numerical(METHOD, category, error.to_string())
}
