// Diffsol problem closures are single-threaded here but require cloneable shared
// handles that live with the leaked solver problem.
#![allow(clippy::arc_with_non_send_sync)]

use diffsol::{OdeSolverMethod, VectorHost};
use indexmap::IndexMap;
use rumoca_eval_solve::{self as solve_eval, SolveRuntime};
use rumoca_ir_solve as solve;
use rumoca_solver::{
    SimOptions, event_solver_step_cap, runtime_root_event_application_time, time_match_with_tol,
};
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use crate::runtime::{
    NoStateRuntime, advance_no_state_runtime_to, apply_no_state_deadline_tick,
    initialize_no_state_runtime,
};
use crate::{
    LinearSolver, OdeModel, RuntimeParameters, SimError, apply_event_updates, bdf_derivative_guess,
    build_ode_problem_with_runtime_params_and_initial, initial_bdf_state, reset_solver_state,
    settle_algebraics_and_relation_memory, solver_call, validate_model, write_state_to_solver,
};

type StepFn = Box<dyn FnMut(f64) -> Result<StepAdvance, SimError>>;
type ResetFn = Box<dyn FnMut(f64, &BdfResetSnapshot) -> Result<(), SimError>>;
const SESSION_ADVANCE_EVENT_LIMIT: usize = 256;

pub struct SimulationSession {
    inner: SimulationSessionInner,
    t_end: f64,
}

enum SimulationSessionInner {
    Bdf(Box<BdfSession>),
    RuntimeOnly(Box<RuntimeOnlyDriver>),
}

struct BdfSession {
    _runtime_context: solve_eval::SimulationContext,
    step_fn: StepFn,
    time_fn: Box<dyn Fn() -> f64>,
    y_fn: Box<dyn Fn() -> Vec<f64>>,
    event_reset_fn: Box<dyn FnMut(f64) -> Result<(), SimError>>,
    reset_fn: ResetFn,
    refresh_input_fn: Box<dyn FnMut() -> Result<(), SimError>>,
    project_fn: Box<dyn FnMut() -> Result<(), SimError>>,
    runtime: SolveRuntime,
    runtime_params: RuntimeParameters,
    reset_snapshot: BdfResetSnapshot,
    input_values: IndexMap<String, f64>,
    inputs_dirty: bool,
}

#[derive(Clone)]
struct BdfResetSnapshot {
    y: Vec<f64>,
    dy: Vec<f64>,
    params: Vec<f64>,
}

#[derive(Debug, Clone, Copy, Default)]
struct StepAdvance {
    hit_root: bool,
}

struct RuntimeOnlyDriver {
    _runtime_context: solve_eval::SimulationContext,
    model: solve::SolveModel,
    opts: SimOptions,
    runtime: NoStateRuntime,
    input_values: IndexMap<String, f64>,
}

impl SimulationSession {
    pub fn new(model: &solve::SolveModel, opts: SimOptions) -> Result<Self, SimError> {
        let t_end = opts.t_end;
        let inner = if model.state_scalar_count() == 0 {
            SimulationSessionInner::RuntimeOnly(Box::new(RuntimeOnlyDriver::new(model, opts)?))
        } else {
            SimulationSessionInner::Bdf(Box::new(BdfSession::new(model, opts)?))
        };
        Ok(Self { inner, t_end })
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        match &mut self.inner {
            SimulationSessionInner::Bdf(session) => session.set_input(name, value),
            SimulationSessionInner::RuntimeOnly(session) => session.set_input(name, value),
        }
    }

    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        match &mut self.inner {
            SimulationSessionInner::Bdf(session) => session.set_inputs(inputs),
            SimulationSessionInner::RuntimeOnly(session) => session.set_inputs(inputs),
        }
    }

    pub fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        let target_time = target_time.min(self.t_end);
        match &mut self.inner {
            SimulationSessionInner::Bdf(session) => session.advance_to(target_time),
            SimulationSessionInner::RuntimeOnly(session) => session.advance_to(target_time),
        }
    }

    pub fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        match &mut self.inner {
            SimulationSessionInner::Bdf(session) => session.reset(t_start),
            SimulationSessionInner::RuntimeOnly(session) => session.reset(t_start),
        }
    }

    pub fn time(&self) -> f64 {
        match &self.inner {
            SimulationSessionInner::Bdf(session) => session.time(),
            SimulationSessionInner::RuntimeOnly(session) => session.time(),
        }
    }

    pub fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        match &self.inner {
            SimulationSessionInner::Bdf(session) => session.get(name),
            SimulationSessionInner::RuntimeOnly(session) => session.get(name),
        }
    }

    pub fn state(&self) -> Result<SessionState, SimError> {
        match &self.inner {
            SimulationSessionInner::Bdf(session) => session.state(),
            SimulationSessionInner::RuntimeOnly(session) => session.state(),
        }
    }

    pub fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        match &self.inner {
            SimulationSessionInner::Bdf(session) => session.values_for(names),
            SimulationSessionInner::RuntimeOnly(session) => session.values_for(names),
        }
    }

    pub fn input_names(&self) -> &[String] {
        match &self.inner {
            SimulationSessionInner::Bdf(session) => session.input_names(),
            SimulationSessionInner::RuntimeOnly(session) => session.input_names(),
        }
    }

    pub fn variable_names(&self) -> &[String] {
        match &self.inner {
            SimulationSessionInner::Bdf(session) => session.variable_names(),
            SimulationSessionInner::RuntimeOnly(session) => session.variable_names(),
        }
    }

    pub fn max_runner_advance_dt(&self) -> Option<f64> {
        match &self.inner {
            SimulationSessionInner::Bdf(_) => Some(0.002),
            SimulationSessionInner::RuntimeOnly(_) => None,
        }
    }
}

impl RuntimeOnlyDriver {
    fn new(model: &solve::SolveModel, opts: SimOptions) -> Result<Self, SimError> {
        if model.state_scalar_count() != 0 {
            return Err(SimError::SolverError(
                "no-state session requires a model with zero continuous states".to_string(),
            ));
        }
        let runtime_context = solve_eval::SimulationContext::new();
        runtime_context.hydrate_solve_model(model);
        validate_model(model)?;
        let runtime = initialize_no_state_runtime(model, &opts, 1, false)?;
        Ok(Self {
            _runtime_context: runtime_context,
            model: model.clone(),
            opts,
            runtime,
            input_values: IndexMap::new(),
        })
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        let Some(param_idx) = self.model.problem.solve_layout.input_parameter_index(name) else {
            return Err(SimError::SolverError(format!("unknown input '{name}'")));
        };
        self.input_values.insert(name.to_string(), value);
        if let Some(slot) = self.runtime.params.get_mut(param_idx) {
            *slot = value;
        }
        Ok(())
    }

    fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        for (name, value) in inputs {
            self.set_input(name, *value)?;
        }
        Ok(())
    }

    fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        if target_time <= self.runtime.current_t {
            return Ok(());
        }
        let tol = self.opts.atol.max(1.0e-10);
        advance_no_state_runtime_to(&self.model, &self.opts, &mut self.runtime, target_time, tol)?;
        let can_tick_at_target = self.runtime.current_t < target_time
            || time_match_with_tol(self.runtime.current_t, target_time);
        let event_at_target = self
            .runtime
            .last_event_t
            .is_some_and(|event_t| time_match_with_tol(event_t, target_time));
        if can_tick_at_target && !event_at_target {
            apply_no_state_deadline_tick(&self.model, &mut self.runtime, target_time, tol)?;
        }
        Ok(())
    }

    fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.input_values.clear();
        let mut opts = self.opts.clone();
        opts.t_start = t_start;
        self.runtime = initialize_no_state_runtime(&self.model, &opts, 1, false)?;
        self.opts = opts;
        Ok(())
    }

    fn time(&self) -> f64 {
        self.runtime.current_t
    }

    fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        if let Some(value) = self.input_values.get(name).copied() {
            return Ok(Some(value));
        }
        let Some(idx) = self
            .model
            .visible_names
            .iter()
            .position(|visible| visible == name)
        else {
            return Ok(None);
        };
        let values = self.runtime.runtime.visible_values(
            &self.runtime.current_y,
            &self.runtime.params,
            self.runtime.current_t,
        )?;
        values.get(idx).copied().map(Some).ok_or_else(|| {
            SimError::RuntimeContract {
                reason: format!(
                    "visible value '{name}' resolved to index {idx}, but runtime returned {} values",
                    values.len()
                ),
            }
        })
    }

    fn state(&self) -> Result<SessionState, SimError> {
        Ok(SessionState {
            time: self.time(),
            values: self.session_visible_values()?,
        })
    }

    fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        let visible_values = self.session_visible_values()?;
        let mut values = IndexMap::with_capacity(names.len());
        for name in names {
            if let Some(value) = visible_values.get(name).copied() {
                values.insert(name.clone(), value);
            }
        }
        Ok(values)
    }

    fn input_names(&self) -> &[String] {
        self.model.problem.solve_layout.input_scalar_names()
    }

    fn variable_names(&self) -> &[String] {
        &self.model.visible_names
    }

    fn session_visible_values(&self) -> Result<IndexMap<String, f64>, SimError> {
        let visible_values = self.runtime.runtime.visible_values(
            &self.runtime.current_y,
            &self.runtime.params,
            self.runtime.current_t,
        )?;
        let mut values = collect_visible_values(&self.model.visible_names, visible_values)?;
        values.extend(
            self.input_values
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        Ok(values)
    }
}

impl BdfSession {
    // SPEC_0021: Exception - constructor wires Diffsol problem lifetime,
    // closures, reset behavior, and runtime input state as one owned session.
    #[allow(clippy::too_many_lines)]
    fn new(model: &solve::SolveModel, opts: SimOptions) -> Result<Self, SimError> {
        let runtime_context = solve_eval::SimulationContext::new();
        runtime_context.hydrate_solve_model(model);
        let runtime = SolveRuntime::new(model)?;
        let root_runtime = Arc::new(runtime.clone());
        let ode_model = OdeModel::new(model)?;
        let runtime_params = Rc::new(RefCell::new(model.parameters.clone()));
        let initial_y = settled_initial_y(model, &runtime, &ode_model, &opts, &runtime_params)?;
        let ode_model = Arc::new(ode_model);
        let problem = build_ode_problem_with_runtime_params_and_initial(
            model,
            &opts,
            runtime_params.clone(),
            opts.t_start,
            initial_y.clone(),
            ode_model.clone(),
            root_runtime,
        )?;
        let problem_ref = Box::leak(Box::new(problem));
        let newton = || {
            diffsol::NewtonNonlinearSolver::new(
                LinearSolver::default(),
                diffsol::BacktrackingLineSearch::default(),
            )
        };
        let state = {
            let params = runtime_params.borrow();
            initial_bdf_state(
                model,
                &ode_model,
                problem_ref,
                &initial_y,
                params.as_slice(),
            )?
        };
        let solver = solver_call("BDF new", || {
            diffsol::Bdf::<_, _, _, diffsol::NoAug<_>>::new(problem_ref, state, newton())
        })?;
        let reset_snapshot = BdfResetSnapshot {
            y: solver.state().y.as_slice().to_vec(),
            dy: solver.state().dy.as_slice().to_vec(),
            params: runtime_params.borrow().to_vec(),
        };
        let solver = Rc::new(RefCell::new(solver));

        let step_fn = make_step_fn(Rc::clone(&solver), model, runtime_params.clone(), &opts)?;
        let time_solver = Rc::clone(&solver);
        let time_fn = Box::new(move || time_solver.borrow().state().t);
        let y_solver = Rc::clone(&solver);
        let y_fn = Box::new(move || y_solver.borrow().state().y.as_slice().to_vec());
        let reset_solver = Rc::clone(&solver);
        let event_reset_solver = Rc::clone(&solver);
        let event_reset_model = model.clone();
        let event_reset_opts = opts.clone();
        let event_reset_params = runtime_params.clone();
        let event_reset_fn = Box::new(move |t_start: f64| {
            let initial_y = {
                let solver = event_reset_solver.borrow();
                solver.state().y.as_slice().to_vec()
            };
            let ode_model = Arc::new(OdeModel::new(&event_reset_model)?);
            let reset_runtime = SolveRuntime::new(&event_reset_model)?;
            let root_runtime = Arc::new(reset_runtime.clone());
            let initial_y = settled_problem_y(
                &event_reset_model,
                &reset_runtime,
                &ode_model,
                &event_reset_opts,
                &event_reset_params,
                t_start,
                initial_y,
            )?;
            let problem = build_ode_problem_with_runtime_params_and_initial(
                &event_reset_model,
                &event_reset_opts,
                event_reset_params.clone(),
                t_start,
                initial_y.clone(),
                ode_model.clone(),
                root_runtime,
            )?;
            let problem_ref = Box::leak(Box::new(problem));
            let state = {
                let params = event_reset_params.borrow();
                initial_bdf_state(
                    &event_reset_model,
                    &ode_model,
                    problem_ref,
                    &initial_y,
                    params.as_slice(),
                )?
            };
            let rebuilt = solver_call("BDF new", || {
                diffsol::Bdf::<_, _, _, diffsol::NoAug<_>>::new(problem_ref, state, newton())
            })?;
            *event_reset_solver.borrow_mut() = rebuilt;
            Ok(())
        });
        let reset_opts = opts.clone();
        let reset_params = runtime_params.clone();
        let reset_fn = Box::new(move |t_start: f64, snapshot: &BdfResetSnapshot| {
            reset_solver_state(
                &mut *reset_solver.borrow_mut(),
                &reset_params,
                &snapshot.y,
                &snapshot.dy,
                &snapshot.params,
                t_start,
                event_solver_step_cap(reset_opts.dt),
            )
        });
        let project_fn = make_project_fn(
            Rc::clone(&solver),
            model,
            runtime.clone(),
            runtime_params.clone(),
            &opts,
        )?;
        let refresh_input_fn = make_input_refresh_fn(
            Rc::clone(&solver),
            model,
            runtime.clone(),
            runtime_params.clone(),
            &opts,
        )?;

        Ok(Self {
            _runtime_context: runtime_context,
            step_fn,
            time_fn,
            y_fn,
            event_reset_fn,
            reset_fn,
            refresh_input_fn,
            project_fn,
            runtime,
            runtime_params,
            reset_snapshot,
            input_values: IndexMap::new(),
            inputs_dirty: false,
        })
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        let Some(param_idx) = self
            .runtime
            .model
            .problem
            .solve_layout
            .input_parameter_index(name)
        else {
            return Err(SimError::SolverError(format!("unknown input '{name}'")));
        };
        let old = self.input_values.insert(name.to_string(), value);
        if old != Some(value) {
            self.inputs_dirty = true;
        }
        if let Some(slot) = self.runtime_params.borrow_mut().get_mut(param_idx) {
            *slot = value;
        }
        Ok(())
    }

    fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        for (name, value) in inputs {
            self.set_input(name, *value)?;
        }
        Ok(())
    }

    fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        for _ in 0..SESSION_ADVANCE_EVENT_LIMIT {
            let current_time = self.time();
            if target_time <= current_time {
                return Ok(());
            }
            if self.inputs_dirty {
                (self.refresh_input_fn)()?;
                self.inputs_dirty = false;
            }
            let advance = (self.step_fn)(target_time - current_time)?;
            if !advance.hit_root {
                return Ok(());
            }
            (self.project_fn)()?;
            let reset_time = runtime_root_event_application_time(self.time(), target_time);
            (self.event_reset_fn)(reset_time)?;
        }
        Err(SimError::SolverError(format!(
            "event processing did not settle before t={target_time}"
        )))
    }

    fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.input_values.clear();
        self.inputs_dirty = false;
        (self.reset_fn)(t_start, &self.reset_snapshot)
    }

    fn time(&self) -> f64 {
        (self.time_fn)()
    }

    fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        if let Some(value) = self.input_values.get(name).copied() {
            return Ok(Some(value));
        }
        let y = (self.y_fn)();
        let params = self.runtime_params.borrow();
        let Some(idx) = self
            .runtime
            .model
            .visible_names
            .iter()
            .position(|visible| visible == name)
        else {
            return Ok(None);
        };
        let values = self
            .runtime
            .visible_values(&y, params.as_slice(), self.time())?;
        values.get(idx).copied().map(Some).ok_or_else(|| {
            SimError::RuntimeContract {
                reason: format!(
                    "visible value '{name}' resolved to index {idx}, but runtime returned {} values",
                    values.len()
                ),
            }
        })
    }

    fn state(&self) -> Result<SessionState, SimError> {
        let y = (self.y_fn)();
        let params = self.runtime_params.borrow();
        Ok(SessionState {
            time: self.time(),
            values: self.session_visible_values(&y, params.as_slice())?,
        })
    }

    fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        let y = (self.y_fn)();
        let params = self.runtime_params.borrow();
        let visible_values = self.session_visible_values(&y, params.as_slice())?;
        let mut values = IndexMap::with_capacity(names.len());
        for name in names {
            if let Some(value) = visible_values.get(name).copied() {
                values.insert(name.clone(), value);
            }
        }
        Ok(values)
    }

    fn input_names(&self) -> &[String] {
        self.runtime.model.problem.solve_layout.input_scalar_names()
    }

    fn variable_names(&self) -> &[String] {
        &self.runtime.model.visible_names
    }

    fn session_visible_values(
        &self,
        y: &[f64],
        params: &[f64],
    ) -> Result<IndexMap<String, f64>, SimError> {
        let visible_values = self.runtime.visible_values(y, params, self.time())?;
        let mut values = collect_visible_values(&self.runtime.model.visible_names, visible_values)?;
        values.extend(
            self.input_values
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        Ok(values)
    }
}

fn make_input_refresh_fn<Eqn, S>(
    solver: Rc<RefCell<S>>,
    model: &solve::SolveModel,
    runtime: SolveRuntime,
    params: RuntimeParameters,
    opts: &SimOptions,
) -> Result<Box<dyn FnMut() -> Result<(), SimError>>, SimError>
where
    Eqn: diffsol::OdeEquations<T = f64> + 'static,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'static, Eqn> + 'static,
{
    let refresh_model = OdeModel::new(model)?;
    let state_count = model.state_scalar_count();
    let solve_model = model.clone();
    let tol = opts.atol.max(1.0e-10);
    Ok(Box::new(move || {
        let mut solver = solver.borrow_mut();
        let t = solver.state().t;
        let mut y = solver.state().y.as_slice().to_vec();
        let mut p = params.borrow().to_vec();
        settle_algebraics_and_relation_memory(
            &runtime,
            &refresh_model,
            &mut y,
            &mut p,
            t,
            state_count,
            tol,
        )?;
        let dy = bdf_derivative_guess(&solve_model, &refresh_model, &y, &p, t)?;
        write_state_to_solver(&mut *solver, &params, &y, Some(&dy), &p, t);
        Ok(())
    }))
}

fn make_step_fn<Eqn, S>(
    solver: Rc<RefCell<S>>,
    model: &solve::SolveModel,
    params: RuntimeParameters,
    opts: &SimOptions,
) -> Result<StepFn, SimError>
where
    Eqn: diffsol::OdeEquations<T = f64> + 'static,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'static, Eqn> + 'static,
{
    let step_model = OdeModel::new(model)?;
    let step_opts = opts.clone();
    Ok(Box::new(move |dt: f64| {
        step_solver_by(&solver, &step_model, &params, &step_opts, dt)
    }))
}

#[derive(Debug, Clone)]
pub struct SessionState {
    pub time: f64,
    pub values: IndexMap<String, f64>,
}

fn collect_visible_values(
    names: &[String],
    values: Vec<f64>,
) -> Result<IndexMap<String, f64>, SimError> {
    if names.len() != values.len() {
        return Err(SimError::RuntimeContract {
            reason: format!(
                "runtime returned {} visible values for {} visible names",
                values.len(),
                names.len()
            ),
        });
    }
    Ok(names.iter().cloned().zip(values).collect())
}

fn settled_initial_y(
    model: &solve::SolveModel,
    runtime: &SolveRuntime,
    ode_model: &OdeModel,
    opts: &rumoca_solver::SimOptions,
    runtime_params: &RuntimeParameters,
) -> Result<Vec<f64>, SimError> {
    settled_problem_y(
        model,
        runtime,
        ode_model,
        opts,
        runtime_params,
        opts.t_start,
        model.initial_y.clone(),
    )
}

fn settled_problem_y(
    model: &solve::SolveModel,
    runtime: &SolveRuntime,
    ode_model: &OdeModel,
    opts: &rumoca_solver::SimOptions,
    runtime_params: &RuntimeParameters,
    t_start: f64,
    mut y: Vec<f64>,
) -> Result<Vec<f64>, SimError> {
    let mut params = runtime_params.borrow_mut();
    settle_algebraics_and_relation_memory(
        runtime,
        ode_model,
        &mut y,
        params.as_mut_slice(),
        t_start,
        model.state_scalar_count(),
        opts.atol.max(1.0e-10),
    )?;
    Ok(y)
}

fn make_project_fn<Eqn, S>(
    solver: Rc<RefCell<S>>,
    model: &solve::SolveModel,
    runtime: SolveRuntime,
    params: RuntimeParameters,
    opts: &SimOptions,
) -> Result<Box<dyn FnMut() -> Result<(), SimError>>, SimError>
where
    Eqn: diffsol::OdeEquations<T = f64> + 'static,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'static, Eqn> + 'static,
{
    let project_model = OdeModel::new(model)?;
    let event_model = model.clone();
    let state_count = model.state_scalar_count();
    let tol = opts.atol.max(1.0e-10);
    Ok(Box::new(move || {
        project_session_algebraics(
            &solver,
            &event_model,
            &runtime,
            &project_model,
            &params,
            state_count,
            tol,
        )
    }))
}

fn project_session_algebraics<Eqn, S>(
    solver: &Rc<RefCell<S>>,
    _solve_model: &solve::SolveModel,
    runtime: &SolveRuntime,
    model: &OdeModel,
    params: &RuntimeParameters,
    state_count: usize,
    tol: f64,
) -> Result<(), SimError>
where
    Eqn: diffsol::OdeEquations<T = f64> + 'static,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'static, Eqn>,
{
    let mut solver = solver.borrow_mut();
    let t = solver.state().t;
    let mut y = solver.state().y.as_slice().to_vec();
    {
        let mut params = params.borrow_mut();
        settle_algebraics_and_relation_memory(
            runtime,
            model,
            &mut y,
            params.as_mut_slice(),
            t,
            state_count,
            tol,
        )?;
        apply_event_updates(runtime, model, &mut y, params.as_mut_slice(), t, tol)?;
    }
    solver.state_mut().y.as_mut_slice().copy_from_slice(&y);
    let state = solver.state_clone();
    solver.set_state(state);
    Ok(())
}

fn step_solver_by<Eqn, S>(
    solver: &Rc<RefCell<S>>,
    _model: &OdeModel,
    _params: &RuntimeParameters,
    _opts: &SimOptions,
    dt: f64,
) -> Result<StepAdvance, SimError>
where
    Eqn: diffsol::OdeEquations<T = f64> + 'static,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'static, Eqn>,
{
    if dt <= 0.0 {
        return Ok(StepAdvance::default());
    }
    let current_t = {
        let solver = solver.borrow();
        solver.state().t
    };
    let target = current_t + dt;
    // NOTE: deliberately no `implicit_residual_is_zero_through_interval`
    // fast-path here. Bumping `state_mut().t = target` to skip a "steady"
    // interval jumps the solver clock forward while leaving the BDF multistep
    // history at the old time, so the history times go inconsistent the moment
    // the model leaves equilibrium — exactly the kind of corruption that makes
    // a stiff long-horizon solve collapse at the next transition. The batch
    // dense-output path (`advance_output_interval` in `lib.rs`) has no such
    // shortcut and completes the full horizon; mirror it.
    let mut solver = solver.borrow_mut();
    // Advance with the solver's own adaptive steps and land on `target` via
    // dense output (`state_mut_back`), rather than pinning a stop time at every
    // output sample. `set_stop_time(target)` forces a shortened, awkward final
    // step onto each output instant; on stiff models (e.g. the rover thermal
    // system at its lunar louver/thermostat crossings) that constrained step
    // repeatedly fails the Newton iteration and the solve collapses
    // (~"nonlinear solver failures" at the same instant every run). Free-running
    // the step and rewinding into the just-completed step keeps the natural step
    // size, which is exactly what the batch dense-output path does
    // (`advance_output_interval` / `state_mut_back` in `lib.rs`) and why the
    // batch path completes the full horizon where the simulation session used
    // to collapse.
    loop {
        if solver.state().t >= target {
            solver
                .state_mut_back(target)
                .map_err(|err| SimError::SolverError(format!("state_mut_back: {err}")))?;
            return Ok(StepAdvance::default());
        }
        match solver_call("BDF step", || solver.step()) {
            Ok(
                diffsol::OdeSolverStopReason::TstopReached
                | diffsol::OdeSolverStopReason::InternalTimestep,
            ) => continue,
            Ok(diffsol::OdeSolverStopReason::RootFound(t_root, _)) => {
                // The free-running step overshoots the root (the solver state
                // sits at the natural step end, past `t_root`). The caller's
                // event handling assumes the solver is *at* the event instant,
                // so rewind onto it via dense output before signalling — exactly
                // what the batch path does in `handle_root_crossing`. Without
                // this the projection and event reset run against a post-root
                // state at the wrong time and the next solve diverges to NaN.
                if t_root <= target || time_match_with_tol(t_root, target) {
                    let event_t = root_event_time_for_target(t_root, target);
                    solver
                        .state_mut_back(event_t)
                        .map_err(|err| SimError::SolverError(format!("state_mut_back: {err}")))?;
                    return Ok(StepAdvance { hit_root: true });
                }
                // Root lies beyond the requested interval: land on `target` and
                // defer the crossing to the next step, which resumes from
                // `target` and re-detects it.
                solver
                    .state_mut_back(target)
                    .map_err(|err| SimError::SolverError(format!("state_mut_back: {err}")))?;
                return Ok(StepAdvance::default());
            }
            Err(err) => return Err(err),
        }
    }
}

fn root_event_time_for_target(root_t: f64, target_t: f64) -> f64 {
    if time_match_with_tol(root_t, target_t) {
        target_t
    } else {
        root_t
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;
    use rumoca_ir_solve::{
        ComputeBlock, LinearOp, ScalarProgramBlock, SolveLayout, SolveProblem, SolverNameIndexMaps,
    };

    macro_rules! fixture_span {
        () => {
            rumoca_ir_solve::source_span_from_offsets(50, 0, 1)
        };
    }

    fn advance_by(session: &mut SimulationSession, dt: f64, context: &str) {
        let target = session.time() + dt;
        session.advance_to(target).expect(context);
    }

    #[test]
    fn set_input_updates_solve_ir_parameter_tail() {
        let model = single_input_integrator();
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(1.0e-3),
                ..Default::default()
            },
        )
        .expect("session should build");

        session.set_input("u", 2.0).expect("input should exist");
        advance_by(&mut session, 0.05, "session should integrate");

        let x = session
            .get("x")
            .expect("session read should succeed")
            .expect("x should be visible");
        assert!((x - 0.1).abs() <= 1.0e-4, "x={x}");
    }

    #[test]
    fn changed_input_refreshes_bdf_history() {
        let model = single_input_integrator();
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(1.0e-3),
                ..Default::default()
            },
        )
        .expect("session should build");

        session.set_input("u", 1.0).expect("input should exist");
        advance_by(&mut session, 0.05, "first input segment should advance");
        session.set_input("u", 2.0).expect("input should update");
        advance_by(&mut session, 0.05, "changed input should advance");

        let x = session
            .get("x")
            .expect("session read should succeed")
            .expect("x should be visible");
        assert!((x - 0.15).abs() <= 1.0e-4, "x={x}");
    }

    #[test]
    fn zero_input_equilibrium_advances_without_bdf_underflow() {
        let model = single_input_integrator();
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(2.0e-3),
                ..Default::default()
            },
        )
        .expect("session should build");

        for _ in 0..16 {
            advance_by(
                &mut session,
                2.0e-3,
                "zero-input equilibrium should advance",
            );
        }

        assert!((session.time() - 0.032).abs() <= 1.0e-12);
        let x = session
            .get("x")
            .expect("session read should succeed")
            .expect("x should be visible");
        assert!(x.abs() <= 1.0e-12, "x={x}");
    }

    #[test]
    fn advance_to_clamps_to_sim_options_end_time() {
        let model = single_input_integrator();
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                t_end: 0.05,
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(1.0e-3),
                ..Default::default()
            },
        )
        .expect("session should build");

        session.set_input("u", 1.0).expect("input should exist");
        session
            .advance_to(0.1)
            .expect("advance past final time should clamp");

        assert!(
            (session.time() - 0.05).abs() <= 1.0e-12,
            "session should stop at t_end, got t={}",
            session.time()
        );
    }

    #[test]
    fn bdf_session_reset_restores_cached_initial_state() {
        let model = single_input_integrator();
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(1.0e-3),
                ..Default::default()
            },
        )
        .expect("session should build");

        session.set_input("u", 2.0).expect("input should exist");
        advance_by(&mut session, 0.05, "session should advance");
        let advanced_x = session
            .get("x")
            .expect("session read should succeed")
            .expect("x should be visible");
        assert!(advanced_x > 0.05);

        session
            .reset(7.25)
            .expect("reset should restore cached initial state");

        assert!((session.time() - 7.25).abs() <= 1.0e-12);
        let reset_x = session
            .get("x")
            .expect("session read should succeed")
            .expect("x should be visible");
        assert!(reset_x.abs() <= 1.0e-12);
        assert_eq!(
            session.get("u").expect("session read should succeed"),
            None,
            "reset should clear stale input overrides"
        );
    }

    #[test]
    fn advance_updates_relation_memory_before_projecting_algebraics() {
        let model = falling_contact_probe();
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(1.0e-3),
                ..Default::default()
            },
        )
        .expect("session should build");

        advance_by(&mut session, 0.2, "first advance should reach relation");
        advance_by(&mut session, 0.05, "second advance should settle relation");

        let z = session
            .get("z")
            .expect("session read should succeed")
            .expect("z should be visible");
        let force = session
            .get("force")
            .expect("session read should succeed")
            .expect("force should be visible");
        assert!(z < 0.0, "z={z}");
        assert!((force - 42.0).abs() <= 1.0e-8, "force={force}");
    }

    #[test]
    fn root_at_advance_deadline_does_not_overshoot_target() {
        let model = falling_contact_probe();
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(1.0e-3),
                ..Default::default()
            },
        )
        .expect("session should build");

        session
            .advance_to(0.1)
            .expect("root exactly at requested target should advance");

        assert!(
            (session.time() - 0.1).abs() <= 1.0e-12,
            "advance_to must stop at the requested deadline, got t={}",
            session.time()
        );
        assert_eq!(session.get("force").unwrap(), Some(0.0));

        session
            .advance_to(0.101)
            .expect("next advance should process the event right-limit");

        assert_eq!(session.get("force").unwrap(), Some(42.0));
    }

    #[test]
    fn simulation_session_advances_zero_state_model_through_solver_runtime() {
        let mut model = solve::SolveModel::default();
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
        model.parameters = vec![7.0];
        model.visible_names = vec!["m".to_string()];

        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                t_start: 0.0,
                t_end: 1.0,
                dt: Some(0.1),
                ..Default::default()
            },
        )
        .expect("zero-state session should build");

        assert_eq!(session.variable_names(), ["m"]);
        assert_eq!(session.get("m").unwrap(), Some(7.0));

        advance_by(&mut session, 0.1, "advance should update no-state runtime");

        assert!((session.time() - 0.1).abs() <= 1.0e-12);
        assert_eq!(session.state().unwrap().values["m"], 7.0);

        session
            .reset(2.5)
            .expect("reset should rebuild no-state runtime");

        assert!((session.time() - 2.5).abs() <= 1.0e-12);
        assert_eq!(session.get("m").unwrap(), Some(7.0));
    }

    fn single_input_integrator() -> solve::SolveModel {
        let rhs = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        );
        let zero = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        );
        solve::SolveModel {
            problem: SolveProblem {
                schema_version: solve::SOLVE_SCHEMA_VERSION,
                layout: solve::VarLayout::from_parts(Default::default(), 1, 1),
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: ComputeBlock::from_scalar_program_block(rhs.clone()),
                    implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                    algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
                    residual: ComputeBlock::from_scalar_program_block(rhs.clone()),
                    derivative_rhs: ComputeBlock::from_scalar_program_block(rhs.clone()),
                },
                initialization: solve::InitializationSolveSystem {
                    residual: ComputeBlock::from_scalar_program_block(zero.clone()),
                    row_targets: Vec::new(),
                    projection_indices: Vec::new(),
                    projection_plan: solve::AlgebraicProjectionPlan::default(),
                    update_rhs: solve::ScalarProgramBlock::default(),
                    update_targets: Vec::new(),
                },
                discrete: solve::DiscreteSolveSystem::default(),
                events: solve::SolveEventPartition::default(),
                clocks: solve::SolveClockPartition::default(),
                solve_layout: SolveLayout {
                    solver_maps: SolverNameIndexMaps {
                        names: vec!["x".to_string()],
                        name_to_idx: IndexMap::from([("x".to_string(), 0)]),
                        base_to_indices: IndexMap::from([("x".to_string(), vec![0])]),
                    },
                    state_scalar_count: 1,
                    algebraic_scalar_count: 0,
                    output_scalar_count: 0,
                    parameter_count: 0,
                    compiled_parameter_len: 1,
                    input_scalar_names: vec!["u".to_string()],
                    discrete_real_scalar_names: Vec::new(),
                    discrete_valued_scalar_names: Vec::new(),
                    relation_memory_parameter_indices: Vec::new(),
                    initial_event_parameter_index: None,
                    pre_param_bindings: Vec::new(),
                },
            },
            artifacts: solve::SolveArtifacts {
                continuous: solve::ContinuousSolveArtifacts {
                    mass_matrix: vec![vec![1.0]],
                    implicit_jacobian_v: ComputeBlock::from_scalar_program_block(zero.clone()),
                    implicit_jacobian_v_scalar: zero.clone(),
                    full_jacobian_v: zero.clone(),
                },
            },
            initial_y: vec![0.0],
            parameters: vec![0.0],
            external_tables: solve::ExternalTables::default(),
            visible_names: vec!["x".to_string()],
            visible_value_rows: solve::ScalarProgramBlock::default(),
            variable_meta: Vec::new(),
        }
    }

    fn falling_contact_probe() -> solve::SolveModel {
        let derivative = scalar_block(vec![
            vec![
                LinearOp::Const {
                    dst: 0,
                    value: -1.0,
                },
                LinearOp::StoreOutput { src: 0 },
            ],
            algebraic_contact_residual_row(),
        ]);
        let jacobian_v = scalar_block(vec![
            zero_row(),
            vec![
                LinearOp::LoadSeed { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ]);
        solve::SolveModel {
            problem: SolveProblem {
                schema_version: solve::SOLVE_SCHEMA_VERSION,
                layout: solve::VarLayout::from_parts(Default::default(), 2, 1),
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: ComputeBlock::from_scalar_program_block(derivative.clone()),
                    implicit_row_targets: vec![
                        Some(solve::scalar_slot_y(0)),
                        Some(solve::scalar_slot_y(1)),
                    ],
                    algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                        blocks: vec![solve::AlgebraicProjectionBlock {
                            rows: vec![1],
                            y_indices: vec![1],
                            causal_steps: Vec::new(),
                        }],
                    },
                    residual: ComputeBlock::from_scalar_program_block(derivative.clone()),
                    derivative_rhs: ComputeBlock::from_scalar_program_block(derivative.clone()),
                },
                initialization: solve::InitializationSolveSystem {
                    residual: ComputeBlock::from_scalar_program_block(scalar_block(vec![
                        zero_row(),
                        zero_row(),
                    ])),
                    row_targets: Vec::new(),
                    projection_indices: Vec::new(),
                    projection_plan: solve::AlgebraicProjectionPlan::default(),
                    update_rhs: solve::ScalarProgramBlock::default(),
                    update_targets: Vec::new(),
                },
                discrete: solve::DiscreteSolveSystem::default(),
                events: solve::SolveEventPartition {
                    root_conditions: scalar_block(vec![vec![
                        LinearOp::LoadY { dst: 0, index: 0 },
                        LinearOp::StoreOutput { src: 0 },
                    ]]),
                    ..Default::default()
                },
                clocks: solve::SolveClockPartition::default(),
                solve_layout: SolveLayout {
                    solver_maps: SolverNameIndexMaps {
                        names: vec!["z".to_string(), "force".to_string()],
                        name_to_idx: IndexMap::from([
                            ("z".to_string(), 0),
                            ("force".to_string(), 1),
                        ]),
                        base_to_indices: IndexMap::from([
                            ("z".to_string(), vec![0]),
                            ("force".to_string(), vec![1]),
                        ]),
                    },
                    state_scalar_count: 1,
                    algebraic_scalar_count: 1,
                    output_scalar_count: 0,
                    parameter_count: 0,
                    compiled_parameter_len: 1,
                    input_scalar_names: Vec::new(),
                    discrete_real_scalar_names: Vec::new(),
                    discrete_valued_scalar_names: vec!["c".to_string()],
                    relation_memory_parameter_indices: vec![0],
                    initial_event_parameter_index: None,
                    pre_param_bindings: Vec::new(),
                },
            },
            artifacts: solve::SolveArtifacts {
                continuous: solve::ContinuousSolveArtifacts {
                    mass_matrix: vec![vec![1.0]],
                    implicit_jacobian_v: ComputeBlock::from_scalar_program_block(
                        jacobian_v.clone(),
                    ),
                    implicit_jacobian_v_scalar: jacobian_v.clone(),
                    full_jacobian_v: jacobian_v.clone(),
                },
            },
            initial_y: vec![0.1, 0.0],
            parameters: vec![0.0],
            external_tables: solve::ExternalTables::default(),
            visible_names: vec!["z".to_string(), "force".to_string()],
            visible_value_rows: solve::ScalarProgramBlock::default(),
            variable_meta: Vec::new(),
        }
    }

    fn scalar_block(rows: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
        ScalarProgramBlock::with_source_span(rows, fixture_span!())
    }

    fn algebraic_contact_residual_row() -> Vec<LinearOp> {
        vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::Const {
                dst: 2,
                value: 42.0,
            },
            LinearOp::Const { dst: 3, value: 0.0 },
            LinearOp::Select {
                dst: 4,
                cond: 1,
                if_true: 2,
                if_false: 3,
            },
            LinearOp::Binary {
                dst: 5,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 4,
            },
            LinearOp::StoreOutput { src: 5 },
        ]
    }

    fn zero_row() -> Vec<LinearOp> {
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]
    }
}
