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
    pub(crate) fn settle(self) -> rumoca_solver::AlgebraicSettle {
        rumoca_solver::AlgebraicSettle {
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
    runtime: rumoca_solver::SolveRuntime,
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
        let runtime = rumoca_solver::SolveRuntime::new(&solve_model)?;
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

    pub(crate) fn runtime(&self) -> &rumoca_solver::SolveRuntime {
        &self.runtime
    }

    pub(crate) fn params_mut(&mut self) -> &mut [f64] {
        &mut self.params
    }

    pub(crate) fn linearization(&self, t: f64) -> rumoca_solver::AlgebraicLinearization<'_> {
        rumoca_solver::AlgebraicLinearization {
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
        .programs()
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
            .programs()
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
    dae_model.inspect(|view| {
        let excluded = parameter_dependency_participants(view);
        let mut seen_slots = IndexSet::new();
        let mut parameters = Vec::new();
        for (id, variable) in view
            .variables()
            .filter(|(_, variable)| variable.role() == dae::VariableRole::Parameter)
        {
            if independent_trainable_parameter(id, variable, &excluded) {
                write_trainable_parameter_slots(variable, model, &mut seen_slots, &mut parameters);
            }
        }
        parameters.sort_by_key(|parameter| parameter.slot);
        parameters
    })
}

fn independent_trainable_parameter<'dae>(
    id: dae::VariableId<'dae>,
    variable: dae::VariableView<'dae>,
    excluded: &IndexSet<u32>,
) -> bool {
    // Only `Real` parameters are trainable. MLS §3.8.3 makes `Integer`,
    // `Boolean`, `String`, and enumeration parameters discrete-valued, so a
    // gradient with respect to them is not defined; the optimizer would perturb
    // them by fractional steps and hand the runtime a value the source model
    // cannot take. `parameter Integer n` used as an array dimension is the
    // concrete case: it reaches the lowered parameter vector like any other
    // declared parameter, and training it would also invalidate the shapes the
    // model was lowered with.
    variable.value_type().scalar_type() == dae::ScalarType::Real
        && variable.is_tunable()
        && variable.origin() == dae::VariableOrigin::Source
        && variable.causality() == dae::VariableCausality::Parameter
        && !excluded.contains(&id.index())
}

fn parameter_dependency_participants(view: dae::DaeView<'_>) -> IndexSet<u32> {
    let mut participants = IndexSet::new();
    for (id, variable) in view
        .variables()
        .filter(|(_, variable)| variable.role() == dae::VariableRole::Parameter)
    {
        let refs = parameter_start_refs(view, variable);
        if refs.is_empty() {
            continue;
        }
        participants.insert(id.index());
        participants.extend(refs);
    }
    participants
}

fn parameter_start_refs<'dae>(
    view: dae::DaeView<'dae>,
    variable: dae::VariableView<'dae>,
) -> IndexSet<u32> {
    let Some(start) = variable.start() else {
        return IndexSet::new();
    };
    let mut references = IndexSet::new();
    dae::for_each_expression(view, start, |_, expression| {
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id)) =
            expression.operation()
        {
            references.insert(id.index());
        }
    });
    references
}

fn write_trainable_parameter_slots(
    variable: dae::VariableView<'_>,
    model: &solve::SolveModel,
    seen_slots: &mut IndexSet<usize>,
    parameters: &mut Vec<TrainableParameter>,
) {
    for scalar in 0..variable.scalar_count() {
        let scalar_name = variable
            .scalar_name(scalar)
            .expect("checked parameter scalar has a name");
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
