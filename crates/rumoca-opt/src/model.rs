use crate::OptError;
use indexmap::IndexSet;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

/// Runtime and numerical options used by optimization APIs.
#[derive(Debug, Clone, Copy)]
pub struct OptOptions {
    /// Algebraic projection tolerance used while evaluating gradients.
    pub settle_tol: f64,
    /// Maximum algebraic projection iterations used while evaluating gradients.
    pub settle_max_iters: usize,
}

impl Default for OptOptions {
    fn default() -> Self {
        Self {
            settle_tol: 1.0e-10,
            settle_max_iters: 64,
        }
    }
}

impl OptOptions {
    pub(crate) fn settle(self) -> rumoca_eval_solve::AlgebraicSettle {
        rumoca_eval_solve::AlgebraicSettle {
            tol: self.settle_tol,
            max_iters: self.settle_max_iters,
        }
    }
}

/// One scalar trainable parameter in the lowered Solve parameter vector.
#[derive(Debug, Clone, PartialEq)]
pub struct TrainableParameter {
    /// Human-readable parameter name.
    pub name: String,
    /// Slot in the runtime `p[]` vector.
    pub slot: usize,
}

/// Selected trainable parameters for an optimization pass.
#[derive(Debug, Clone, PartialEq)]
pub struct TrainableSet {
    entries: Vec<TrainableParameter>,
}

impl TrainableSet {
    /// Select every model parameter exposed by the differentiable model.
    pub fn all(model: &DifferentiableModel) -> Result<Self, OptError> {
        Self::from_entries(model.parameter_slots().to_vec())
    }

    /// Select trainables by exact lowered parameter names.
    pub fn by_names(model: &DifferentiableModel, names: &[&str]) -> Result<Self, OptError> {
        let entries = names
            .iter()
            .map(|name| model.parameter_by_name(name))
            .collect::<Result<Vec<_>, _>>()?;
        Self::from_entries(entries)
    }

    /// Selected scalar parameters in deterministic slot order.
    pub fn entries(&self) -> &[TrainableParameter] {
        &self.entries
    }

    /// Number of selected scalar parameters.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// True when no trainables are selected.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    fn from_entries(mut entries: Vec<TrainableParameter>) -> Result<Self, OptError> {
        entries.sort_by_key(|entry| entry.slot);
        entries.dedup_by_key(|entry| entry.slot);
        if entries.is_empty() {
            return Err(OptError::EmptyTrainableSet);
        }
        Ok(Self { entries })
    }
}

/// A compiled, lowered, differentiable model with mutable parameter values.
pub struct DifferentiableModel {
    runtime: rumoca_eval_solve::SolveRuntime,
    state: Vec<f64>,
    params: Vec<f64>,
    parameters: Vec<TrainableParameter>,
    options: OptOptions,
}

impl DifferentiableModel {
    /// Lower a DAE model once and prepare the differentiable runtime.
    pub fn from_dae(
        dae_model: &dae::Dae,
        sim_options: &SimOptions,
        opt_options: OptOptions,
    ) -> Result<Self, OptError> {
        let solve_model =
            rumoca_sim::lower_for_differentiation_with_overrides(dae_model, sim_options)?;
        validate_sensitivity_artifacts(&solve_model)?;
        let state = solve_model.initial_y[..solve_model.state_scalar_count()].to_vec();
        let params = solve_model.parameters.clone();
        let parameters = collect_model_parameter_slots(dae_model, &solve_model);
        let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model)?;
        Ok(Self {
            runtime,
            state,
            params,
            parameters,
            options: opt_options,
        })
    }

    /// Lower a DAE model with default optimization options.
    pub fn from_dae_default(
        dae_model: &dae::Dae,
        sim_options: &SimOptions,
    ) -> Result<Self, OptError> {
        Self::from_dae(dae_model, sim_options, OptOptions::default())
    }

    /// State names in solver state order.
    pub fn state_names(&self) -> &[String] {
        &self.runtime.model.problem.solve_layout.solver_maps.names[..self.runtime.state_count]
    }

    /// Current state vector used for objective evaluation.
    pub fn state(&self) -> &[f64] {
        &self.state
    }

    /// Replace the current state vector.
    pub fn set_state(&mut self, state: &[f64]) -> Result<(), OptError> {
        if state.len() != self.state.len() {
            return Err(OptError::LengthMismatch {
                what: "state",
                got: state.len(),
                expected: self.state.len(),
            });
        }
        self.state.copy_from_slice(state);
        Ok(())
    }

    /// Exposed model-parameter slots.
    pub fn parameter_slots(&self) -> &[TrainableParameter] {
        &self.parameters
    }

    /// Current runtime parameter vector.
    pub fn parameters(&self) -> &[f64] {
        &self.params
    }

    /// Set one model parameter by exact lowered name.
    pub fn set_parameter_value(&mut self, name: &str, value: f64) -> Result<(), OptError> {
        let parameter = self.parameter_by_name(name)?;
        self.params[parameter.slot] = value;
        Ok(())
    }

    /// Current value of one model parameter by exact lowered name.
    pub fn parameter_value(&self, name: &str) -> Result<f64, OptError> {
        let parameter = self.parameter_by_name(name)?;
        Ok(self.params[parameter.slot])
    }

    /// Evaluate `der(state)` at the model's current state and parameters.
    pub fn eval_rhs(&self, t: f64) -> Result<Vec<f64>, OptError> {
        self.runtime
            .eval_state_derivatives(
                t,
                &self.state,
                &self.params,
                self.options.settle_tol,
                self.options.settle_max_iters,
            )
            .map_err(Into::into)
    }

    /// True when reverse-mode derivative VJP is exact for this model.
    pub fn supports_rhs_reverse_vjp(&self) -> bool {
        self.runtime.solver_count == self.runtime.state_count
    }

    pub(crate) fn runtime(&self) -> &rumoca_eval_solve::SolveRuntime {
        &self.runtime
    }

    pub(crate) fn params_mut(&mut self) -> &mut [f64] {
        &mut self.params
    }

    pub(crate) fn linearization(&self, t: f64) -> rumoca_eval_solve::AlgebraicLinearization<'_> {
        rumoca_eval_solve::AlgebraicLinearization {
            t,
            params: &self.params,
            settle: self.options.settle(),
        }
    }

    pub(crate) fn parameter_by_name(&self, name: &str) -> Result<TrainableParameter, OptError> {
        self.parameters
            .iter()
            .find(|parameter| parameter.name == name)
            .cloned()
            .ok_or_else(|| OptError::UnknownTrainable {
                name: name.to_string(),
                available: available_trainables(&self.parameters),
            })
    }
}

fn validate_sensitivity_artifacts(model: &solve::SolveModel) -> Result<(), OptError> {
    if model.state_scalar_count() == 0 {
        return Ok(());
    }
    if model
        .artifacts
        .continuous
        .full_jacobian_v
        .programs
        .is_empty()
    {
        return Err(OptError::Lowering(
            "differentiable lowering did not produce derivative sensitivity artifacts".to_string(),
        ));
    }
    if model.solver_scalar_count() > model.state_scalar_count()
        && model
            .artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .programs
            .is_empty()
    {
        return Err(OptError::Lowering(
            "differentiable lowering did not produce algebraic projection sensitivity artifacts"
                .to_string(),
        ));
    }
    Ok(())
}

fn available_trainables(parameters: &[TrainableParameter]) -> String {
    parameters
        .iter()
        .map(|parameter| parameter.name.as_str())
        .collect::<Vec<_>>()
        .join(", ")
}

fn collect_model_parameter_slots(
    dae_model: &dae::Dae,
    model: &solve::SolveModel,
) -> Vec<TrainableParameter> {
    let excluded = parameter_dependency_participants(dae_model);
    let mut seen_slots = IndexSet::new();
    let mut parameters = Vec::new();
    for (name, var) in &dae_model.variables.parameters {
        if independent_trainable_parameter(name.as_str(), var, &excluded) {
            write_trainable_parameter_slots(
                name.as_str(),
                var,
                model,
                &mut seen_slots,
                &mut parameters,
            );
        }
    }
    parameters.sort_by_key(|parameter| parameter.slot);
    parameters
}

fn independent_trainable_parameter(
    name: &str,
    var: &dae::Variable,
    excluded: &IndexSet<String>,
) -> bool {
    var.is_tunable
        && var.origin == dae::VariableOrigin::Source
        && var.causality == dae::VariableCausality::Parameter
        && !excluded.contains(name)
}

fn parameter_dependency_participants(dae_model: &dae::Dae) -> IndexSet<String> {
    let parameter_names = dae_model
        .variables
        .parameters
        .keys()
        .map(|name| name.as_str().to_string())
        .collect::<IndexSet<_>>();
    let mut participants = IndexSet::new();
    for (name, var) in &dae_model.variables.parameters {
        let refs = parameter_start_refs(var, &parameter_names);
        if refs.is_empty() {
            continue;
        }
        participants.insert(name.as_str().to_string());
        participants.extend(refs);
    }
    participants
}

fn parameter_start_refs(
    var: &dae::Variable,
    parameter_names: &IndexSet<String>,
) -> IndexSet<String> {
    let Some(start) = var.start.as_ref() else {
        return IndexSet::new();
    };
    let mut refs = Vec::new();
    start.collect_var_refs(&mut refs);
    refs.into_iter()
        .filter_map(|reference| {
            let name = reference.as_str();
            parameter_names.contains(name).then(|| name.to_string())
        })
        .collect()
}

fn write_trainable_parameter_slots(
    name: &str,
    var: &dae::Variable,
    model: &solve::SolveModel,
    seen_slots: &mut IndexSet<usize>,
    parameters: &mut Vec<TrainableParameter>,
) {
    let Ok(size) = var.try_size() else {
        return;
    };
    for scalar_name in trainable_scalar_names(name, var, size) {
        if let Some(solve::ScalarSlot::P { index, .. }) = model.problem.layout.binding(&scalar_name)
            && !scalar_name.starts_with("__")
            && seen_slots.insert(index)
        {
            parameters.push(TrainableParameter {
                name: scalar_name,
                slot: index,
            });
        }
    }
}

fn trainable_scalar_names(name: &str, var: &dae::Variable, size: usize) -> Vec<String> {
    if size <= 1 && var.dims.is_empty() {
        return vec![name.to_string()];
    }
    (0..size)
        .map(|index| dae::scalar_name_text_for_flat_index(name, &var.dims, index))
        .collect()
}
