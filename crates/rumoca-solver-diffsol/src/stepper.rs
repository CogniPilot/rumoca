// Diffsol problem closures are single-threaded here but require cloneable shared
// handles that live with the leaked solver problem.
#![allow(clippy::arc_with_non_send_sync)]

use diffsol::{OdeSolverMethod, VectorHost};
use rumoca_eval_solve::{self as solve_eval, SolveRuntime};
use rumoca_ir_solve as solve;
use rumoca_solver::{
    SimOptions, event_solver_step_cap, implicit_residual_is_zero_through_interval,
    runtime_root_event_application_time,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::{
    LinearSolver, OdeModel, RuntimeParameters, SimError, apply_event_updates, bdf_derivative_guess,
    build_ode_problem_with_runtime_params_and_initial, initial_bdf_state, reset_solver_state,
    settle_algebraics_and_relation_memory, solver_call, write_state_to_solver,
};

type StepFn = Box<dyn FnMut(f64) -> Result<StepAdvance, SimError>>;
type ResetFn = Box<dyn FnMut(f64, &BdfResetSnapshot) -> Result<(), SimError>>;

pub struct SimStepper {
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
    input_values: HashMap<String, f64>,
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

impl SimStepper {
    // SPEC_0021: Exception - constructor wires Diffsol problem lifetime,
    // closures, reset behavior, and runtime input state as one owned stepper.
    #[allow(clippy::too_many_lines)]
    pub fn new(model: &solve::SolveModel, opts: SimOptions) -> Result<Self, SimError> {
        let runtime_context = solve_eval::SimulationContext::new();
        runtime_context.hydrate_solve_model(model);
        let runtime = SolveRuntime::new(model);
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
            let reset_runtime = SolveRuntime::new(&event_reset_model);
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
            input_values: HashMap::new(),
            inputs_dirty: false,
        })
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
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

    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        for (name, value) in inputs {
            self.set_input(name, *value)?;
        }
        Ok(())
    }

    pub fn step(&mut self, dt: f64) -> Result<(), SimError> {
        if self.inputs_dirty {
            (self.refresh_input_fn)()?;
            self.inputs_dirty = false;
        }
        let advance = (self.step_fn)(dt)?;
        if advance.hit_root {
            (self.project_fn)()?;
            let reset_time = runtime_root_event_application_time(self.time(), f64::INFINITY);
            (self.event_reset_fn)(reset_time)?;
        }
        Ok(())
    }

    pub fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.input_values.clear();
        self.inputs_dirty = false;
        (self.reset_fn)(t_start, &self.reset_snapshot)
    }

    pub fn time(&self) -> f64 {
        (self.time_fn)()
    }

    pub fn get(&self, name: &str) -> Option<f64> {
        if let Some(value) = self.input_values.get(name).copied() {
            return Some(value);
        }
        let y = (self.y_fn)();
        let params = self.runtime_params.borrow();
        self.runtime
            .visible_values(&y, params.as_slice(), self.time())
            .ok()
            .and_then(|values| {
                self.runtime
                    .model
                    .visible_names
                    .iter()
                    .position(|visible| visible == name)
                    .and_then(|idx| values.get(idx).copied())
            })
    }

    pub fn state(&self) -> StepperState {
        let y = (self.y_fn)();
        let params = self.runtime_params.borrow();
        let values = self.stepper_visible_values(&y, params.as_slice());
        StepperState {
            time: self.time(),
            values,
        }
    }

    pub fn values_for(&self, names: &[String]) -> HashMap<String, f64> {
        let y = (self.y_fn)();
        let params = self.runtime_params.borrow();
        let mut values = self
            .runtime
            .visible_values_for_names(&y, params.as_slice(), self.time(), names)
            .unwrap_or_default();
        for name in names {
            if let Some(value) = self.input_values.get(name).copied() {
                values.insert(name.clone(), value);
            }
        }
        values
    }

    pub fn input_names(&self) -> &[String] {
        self.runtime.model.problem.solve_layout.input_scalar_names()
    }

    pub fn variable_names(&self) -> &[String] {
        &self.runtime.model.visible_names
    }

    fn stepper_visible_values(&self, y: &[f64], params: &[f64]) -> HashMap<String, f64> {
        let mut values: HashMap<String, f64> = self
            .runtime
            .visible_values(y, params, self.time())
            .ok()
            .map(|visible_values| {
                self.runtime
                    .model
                    .visible_names
                    .iter()
                    .cloned()
                    .zip(visible_values)
                    .collect()
            })
            .unwrap_or_default();
        values.extend(
            self.input_values
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        values
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
pub struct StepperState {
    pub time: f64,
    pub values: HashMap<String, f64>,
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
        project_stepper_algebraics(
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

fn project_stepper_algebraics<Eqn, S>(
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
    model: &OdeModel,
    params: &RuntimeParameters,
    opts: &SimOptions,
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
    {
        let solver_ref = solver.borrow();
        let params_ref = params.borrow();
        if implicit_residual_is_zero_through_interval(
            model,
            solver_ref.state().y.as_slice(),
            params_ref.as_slice(),
            current_t,
            target,
            opts.atol.max(1.0e-12),
        )? {
            drop(solver_ref);
            *solver.borrow_mut().state_mut().t = target;
            return Ok(StepAdvance::default());
        }
    }
    let mut solver = solver.borrow_mut();
    solver
        .set_stop_time(target)
        .map_err(|err| SimError::SolverError(format!("Failed to set stop time: {err}")))?;
    loop {
        match solver_call("BDF step", || solver.step()) {
            Ok(diffsol::OdeSolverStopReason::TstopReached) => return Ok(StepAdvance::default()),
            Ok(diffsol::OdeSolverStopReason::InternalTimestep) => continue,
            Ok(diffsol::OdeSolverStopReason::RootFound(_, _)) => {
                return Ok(StepAdvance { hit_root: true });
            }
            Err(err) => return Err(err),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;
    use rumoca_ir_solve::{
        ComputeBlock, LinearOp, ScalarProgramBlock, SolveLayout, SolveProblem, SolverNameIndexMaps,
    };

    #[test]
    fn set_input_updates_solve_ir_parameter_tail() {
        let model = single_input_integrator();
        let mut stepper = SimStepper::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(1.0e-3),
                ..Default::default()
            },
        )
        .expect("stepper should build");

        stepper.set_input("u", 2.0).expect("input should exist");
        stepper.step(0.05).expect("step should integrate");

        let x = stepper.get("x").expect("x should be visible");
        assert!((x - 0.1).abs() <= 1.0e-4, "x={x}");
    }

    #[test]
    fn changed_input_refreshes_bdf_history() {
        let model = single_input_integrator();
        let mut stepper = SimStepper::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(1.0e-3),
                ..Default::default()
            },
        )
        .expect("stepper should build");

        stepper.set_input("u", 1.0).expect("input should exist");
        stepper.step(0.05).expect("first input segment should step");
        stepper.set_input("u", 2.0).expect("input should update");
        stepper.step(0.05).expect("changed input should step");

        let x = stepper.get("x").expect("x should be visible");
        assert!((x - 0.15).abs() <= 1.0e-4, "x={x}");
    }

    #[test]
    fn zero_input_equilibrium_advances_without_bdf_underflow() {
        let model = single_input_integrator();
        let mut stepper = SimStepper::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(2.0e-3),
                ..Default::default()
            },
        )
        .expect("stepper should build");

        for _ in 0..16 {
            stepper
                .step(2.0e-3)
                .expect("zero-input equilibrium should advance");
        }

        assert!((stepper.time() - 0.032).abs() <= 1.0e-12);
        let x = stepper.get("x").expect("x should be visible");
        assert!(x.abs() <= 1.0e-12, "x={x}");
    }

    #[test]
    fn bdf_stepper_reset_restores_cached_initial_state() {
        let model = single_input_integrator();
        let mut stepper = SimStepper::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(1.0e-3),
                ..Default::default()
            },
        )
        .expect("stepper should build");

        stepper.set_input("u", 2.0).expect("input should exist");
        stepper.step(0.05).expect("stepper should advance");
        assert!(stepper.get("x").unwrap_or_default() > 0.05);

        stepper
            .reset(7.25)
            .expect("reset should restore cached initial state");

        assert!((stepper.time() - 7.25).abs() <= 1.0e-12);
        assert!(stepper.get("x").unwrap_or_default().abs() <= 1.0e-12);
        assert_eq!(
            stepper.get("u"),
            None,
            "reset should clear stale input overrides"
        );
    }

    #[test]
    fn step_updates_relation_memory_before_projecting_algebraics() {
        let model = falling_contact_probe();
        let mut stepper = SimStepper::new(
            &model,
            SimOptions {
                rtol: 1.0e-8,
                atol: 1.0e-10,
                dt: Some(1.0e-3),
                ..Default::default()
            },
        )
        .expect("stepper should build");

        stepper.step(0.2).expect("first step should reach relation");
        stepper
            .step(0.05)
            .expect("second step should settle relation");

        let z = stepper.get("z").expect("z should be visible");
        let force = stepper.get("force").expect("force should be visible");
        assert!(z < 0.0, "z={z}");
        assert!((force - 42.0).abs() <= 1.0e-8, "force={force}");
    }

    fn single_input_integrator() -> solve::SolveModel {
        let rhs = ScalarProgramBlock::new(vec![vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        let zero = ScalarProgramBlock::new(vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        solve::SolveModel {
            problem: SolveProblem {
                schema_version: solve::SOLVE_SCHEMA_VERSION,
                layout: solve::VarLayout::from_parts(Default::default(), 1, 1),
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: ComputeBlock::from_scalar_program_block(rhs.clone()),
                    implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                    algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
                    residual: rhs.clone(),
                    derivative_rhs: ComputeBlock::from_scalar_program_block(rhs.clone()),
                },
                initialization: solve::InitializationSolveSystem {
                    residual: zero.clone(),
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
        let derivative = ScalarProgramBlock::new(vec![
            vec![
                LinearOp::Const {
                    dst: 0,
                    value: -1.0,
                },
                LinearOp::StoreOutput { src: 0 },
            ],
            algebraic_contact_residual_row(),
        ]);
        let jacobian_v = ScalarProgramBlock::new(vec![
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
                    residual: derivative.clone(),
                    derivative_rhs: ComputeBlock::from_scalar_program_block(derivative.clone()),
                },
                initialization: solve::InitializationSolveSystem {
                    residual: ScalarProgramBlock::new(vec![zero_row(), zero_row()]),
                    row_targets: Vec::new(),
                    projection_indices: Vec::new(),
                    projection_plan: solve::AlgebraicProjectionPlan::default(),
                    update_rhs: solve::ScalarProgramBlock::default(),
                    update_targets: Vec::new(),
                },
                discrete: solve::DiscreteSolveSystem::default(),
                events: solve::SolveEventPartition {
                    root_conditions: ScalarProgramBlock::new(vec![vec![
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
