use indexmap::IndexMap;
use rumoca_eval_solve::{SolveRuntime, apply_discrete_slot_values};
use rumoca_ir_solve as solve;
use rumoca_solver::{RuntimeSolveError, commit_pre_params_after_event};

use crate::solve_lowering::SimulationDiagnosticError;

const UPDATE_TOL: f64 = 1.0e-9;
const UPDATE_MAX_ITERS: usize = 64;

#[derive(Debug, Clone)]
pub(crate) struct StepperState {
    pub(crate) time: f64,
    pub(crate) values: IndexMap<String, f64>,
}

pub(crate) struct SimStepper {
    runtime: &'static SolveRuntime,
    reset_y: Vec<f64>,
    reset_params: Vec<f64>,
    y: Vec<f64>,
    params: Vec<f64>,
    input_values: IndexMap<String, f64>,
    time: f64,
}

impl SimStepper {
    /// Build a pure-discrete stepper from an already-lowered solve model. The
    /// caller (the auto/rk-like dispatch) lowers the model once and routes it
    /// here only when it has zero continuous states, so nothing is lowered
    /// twice.
    pub(crate) fn from_solve_model(
        solve_model: solve::SolveModel,
    ) -> Result<Self, SimulationDiagnosticError> {
        let runtime = Box::leak(Box::new(
            SolveRuntime::new(&solve_model)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?,
        ));
        let mut y = runtime.model.initial_y.clone();
        let mut params = runtime.model.parameters.clone();
        runtime
            .apply_initialization_updates(&mut y, &mut params, 0.0, UPDATE_TOL, UPDATE_MAX_ITERS)
            .map_err(runtime_error)?;
        runtime
            .update_relation_memory_from_state(0.0, &[], &mut params, UPDATE_TOL, UPDATE_MAX_ITERS)
            .map_err(runtime_error)?;
        runtime
            .refresh_algebraic_and_output_slots(0.0, &mut y, &params, UPDATE_TOL, UPDATE_MAX_ITERS)
            .map_err(runtime_error)?;
        commit_pre_params_after_event(&runtime.model, &y, &mut params, UPDATE_TOL);
        Ok(Self {
            runtime,
            reset_y: y.clone(),
            reset_params: params.clone(),
            y,
            params,
            input_values: IndexMap::new(),
            time: 0.0,
        })
    }

    pub(crate) fn set_input(
        &mut self,
        name: &str,
        value: f64,
    ) -> Result<(), SimulationDiagnosticError> {
        let Some(param_idx) = self
            .runtime
            .model
            .problem
            .solve_layout
            .input_parameter_index(name)
        else {
            return Err(SimulationDiagnosticError::Solver(format!(
                "unknown input '{name}'"
            )));
        };
        self.input_values.insert(name.to_string(), value);
        if let Some(slot) = self.params.get_mut(param_idx) {
            *slot = value;
        }
        Ok(())
    }

    pub(crate) fn reset(&mut self, t_start: f64) -> Result<(), SimulationDiagnosticError> {
        self.y.clone_from(&self.reset_y);
        self.params.clone_from(&self.reset_params);
        self.input_values.clear();
        self.time = t_start;
        Ok(())
    }

    pub(crate) fn step(&mut self, dt: f64) -> Result<(), SimulationDiagnosticError> {
        if dt <= 0.0 {
            return Ok(());
        }
        self.time += dt;
        let values = self
            .runtime
            .eval_scalar_program_block(
                &self.runtime.model.problem.discrete.rhs,
                &self.y,
                &self.params,
                self.time,
            )
            .map_err(runtime_error)?;
        apply_discrete_slot_values(
            &self.runtime.model.problem.discrete.update_targets,
            &values,
            &mut self.y,
            &mut self.params,
            UPDATE_TOL,
        )
        .map_err(runtime_error)?;
        self.runtime
            .apply_runtime_assignments_once(&mut self.y, &mut self.params, self.time)
            .map_err(runtime_error)?;
        commit_pre_params_after_event(&self.runtime.model, &self.y, &mut self.params, UPDATE_TOL);
        Ok(())
    }

    pub(crate) fn time(&self) -> f64 {
        self.time
    }

    pub(crate) fn get(&self, name: &str) -> Result<Option<f64>, SimulationDiagnosticError> {
        if let Some(value) = self.input_values.get(name).copied() {
            return Ok(Some(value));
        }
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
            .visible_values(&self.y, &self.params, self.time)
            .map_err(runtime_error)?;
        values.get(idx).copied().map(Some).ok_or_else(|| {
            SimulationDiagnosticError::Solver(format!(
                "visible value '{name}' resolved to index {idx}, but runtime returned {} values",
                values.len()
            ))
        })
    }

    pub(crate) fn state(&self) -> Result<StepperState, SimulationDiagnosticError> {
        Ok(StepperState {
            time: self.time(),
            values: self.stepper_visible_values()?,
        })
    }

    pub(crate) fn values_for(
        &self,
        names: &[String],
    ) -> Result<IndexMap<String, f64>, SimulationDiagnosticError> {
        self.runtime
            .visible_values_for_names(&self.y, &self.params, self.time, names)
            .map_err(runtime_error)
    }

    pub(crate) fn input_names(&self) -> &[String] {
        self.runtime.model.problem.solve_layout.input_scalar_names()
    }

    pub(crate) fn variable_names(&self) -> &[String] {
        &self.runtime.model.visible_names
    }

    fn stepper_visible_values(&self) -> Result<IndexMap<String, f64>, SimulationDiagnosticError> {
        let visible_values = self
            .runtime
            .visible_values(&self.y, &self.params, self.time)
            .map_err(runtime_error)?;
        if self.runtime.model.visible_names.len() != visible_values.len() {
            return Err(SimulationDiagnosticError::Solver(format!(
                "runtime returned {} visible values for {} visible names",
                visible_values.len(),
                self.runtime.model.visible_names.len()
            )));
        }
        let mut values: IndexMap<String, f64> = self
            .runtime
            .model
            .visible_names
            .iter()
            .cloned()
            .zip(visible_values)
            .collect();
        values.extend(
            self.input_values
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        Ok(values)
    }
}

fn runtime_error(error: RuntimeSolveError) -> SimulationDiagnosticError {
    SimulationDiagnosticError::Solver(error.to_string())
}
