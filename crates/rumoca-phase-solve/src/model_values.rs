use std::collections::HashMap;

use rumoca_core::Span;
use rumoca_eval_dae::{NumericEvaluationError, NumericEvaluationErrorKind, NumericEvaluator};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::{LowerError, lower_prepared_solve_package, lower_solve_artifacts};

/// Failure while constructing the complete executable Solve root from one DAE.
#[derive(Debug)]
pub enum SolveModelLoweringError {
    Lower(LowerError),
    RuntimeValues { message: String, span: Option<Span> },
    InvalidOverride { message: String },
}

impl SolveModelLoweringError {
    #[must_use]
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::Lower(error) => error.source_span(),
            Self::RuntimeValues { span, .. } => *span,
            Self::InvalidOverride { .. } => None,
        }
    }
}

impl std::fmt::Display for SolveModelLoweringError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lower(error) => write!(formatter, "{error}"),
            Self::RuntimeValues { message, .. } | Self::InvalidOverride { message } => {
                write!(formatter, "{message}")
            }
        }
    }
}

impl std::error::Error for SolveModelLoweringError {}

impl From<LowerError> for SolveModelLoweringError {
    fn from(error: LowerError) -> Self {
        Self::Lower(error)
    }
}

/// Complete checked Solve construction plus its phase-owned timing split.
pub struct LoweredSolveModel<'source> {
    model: solve::SolveModel,
    prepared: rumoca_phase_structural::PreparedDae<'source>,
    program_seconds: f64,
    runtime_value_seconds: f64,
}

/// Phase-owned milestones in complete Solve-model construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveModelLoweringStage {
    Programs,
    RuntimeValues,
}

impl LoweredSolveModel<'_> {
    /// The exact structurally prepared DAE this executable root was lowered
    /// from. FMI metadata must inspect this value, never the pre-transform
    /// source root beside it.
    #[must_use]
    pub fn prepared_dae(&self) -> &dae::Dae {
        self.prepared.as_dae()
    }

    #[must_use]
    pub fn program_seconds(&self) -> f64 {
        self.program_seconds
    }

    #[must_use]
    pub fn runtime_value_seconds(&self) -> f64 {
        self.runtime_value_seconds
    }

    /// Borrow the executable root while it remains correlated with the exact
    /// prepared DAE that produced it.
    ///
    /// This supports observers and execution-backend preparation before the
    /// pair is consumed into an FMI component; it provides no mutable or owned
    /// escape.
    #[must_use]
    pub fn model(&self) -> &solve::SolveModel {
        &self.model
    }

    /// Apply one checked experiment-level initial-state override without
    /// exposing mutable Solve IR.
    pub fn set_initial_state(
        &mut self,
        name: &str,
        value: f64,
    ) -> Result<(), SolveModelLoweringError> {
        if !value.is_finite() {
            return Err(SolveModelLoweringError::InvalidOverride {
                message: format!("start override for `{name}` must be finite"),
            });
        }
        let state_count = self.model.state_scalar_count();
        let state_names = self
            .model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(..state_count)
            .ok_or_else(|| SolveModelLoweringError::InvalidOverride {
                message: "state count exceeds the checked Solve name layout".to_owned(),
            })?;
        let index = state_names
            .iter()
            .position(|candidate| candidate == name)
            .ok_or_else(|| SolveModelLoweringError::InvalidOverride {
                message: format!("`{name}` is not a state of this model"),
            })?;
        let target = self.model.initial_y.get_mut(index).ok_or_else(|| {
            SolveModelLoweringError::InvalidOverride {
                message: format!(
                    "state `{name}` has no initial-value slot in the checked Solve model"
                ),
            }
        })?;
        *target = value;
        Ok(())
    }

    #[must_use]
    pub fn into_model(self) -> solve::SolveModel {
        self.model
    }
}

/// Construct one complete executable Solve root from one checked DAE.
pub fn lower_solve_model<'source>(
    model: &'source dae::Dae,
    overrides: &HashMap<String, f64>,
    mut begin_stage: impl FnMut(SolveModelLoweringStage),
) -> Result<LoweredSolveModel<'source>, SolveModelLoweringError> {
    begin_stage(SolveModelLoweringStage::Programs);
    let program_start = rumoca_core::maybe_start_timer();
    let prepared = rumoca_phase_structural::prepare_for_solve(model).map_err(|error| {
        LowerError::Structural {
            reason: error.to_string(),
            span: error.source_span(),
        }
    })?;
    let package = lower_prepared_solve_package(&prepared)?;
    let problem = package.problem;
    let artifacts = lower_solve_artifacts(&problem)?;
    let program_seconds = rumoca_core::maybe_elapsed_seconds(program_start);

    begin_stage(SolveModelLoweringStage::RuntimeValues);
    let runtime_value_start = rumoca_core::maybe_start_timer();
    let vectors = runtime_vectors(prepared.as_dae(), &problem, overrides)?;
    let solve_model = solve::SolveModel {
        problem,
        pure_calls: package.pure_calls,
        artifacts,
        initial_y: vectors.initial_y,
        solver_nominals: vectors.solver_nominals,
        parameters: vectors.parameters,
        external_tables: solve::ExternalTables::default(),
        visible_names: vectors.visible_names,
        visible_value_rows: vectors.visible_value_rows,
        variable_meta: vectors.variable_meta,
    };
    solve_model.validate().map_err(LowerError::from)?;
    let runtime_value_seconds = rumoca_core::maybe_elapsed_seconds(runtime_value_start);
    Ok(LoweredSolveModel {
        model: solve_model,
        prepared,
        program_seconds,
        runtime_value_seconds,
    })
}

struct RuntimeVectors {
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
    parameters: Vec<f64>,
    visible_names: Vec<String>,
    visible_value_rows: solve::ScalarProgramBlock,
    variable_meta: Vec<solve::SolveVariableMeta>,
}

fn runtime_vectors(
    model: &dae::Dae,
    problem: &solve::SolveProblem,
    overrides: &HashMap<String, f64>,
) -> Result<RuntimeVectors, SolveModelLoweringError> {
    model.inspect(|view| {
        let evaluator = NumericEvaluator::with_overrides(view, |variable, scalar| {
            variable
                .scalar_name(scalar)
                .and_then(|name| overrides.get(&name).copied())
        });
        RuntimeVectorBuilder {
            model,
            view,
            problem,
            evaluator,
        }
        .build()
    })
}

struct RuntimeVectorBuilder<'model, 'dae, F> {
    model: &'model dae::Dae,
    view: dae::DaeView<'dae>,
    problem: &'model solve::SolveProblem,
    evaluator: NumericEvaluator<'dae, F>,
}

impl<'dae, F> RuntimeVectorBuilder<'_, 'dae, F>
where
    F: FnMut(dae::VariableView<'dae>, usize) -> Option<f64>,
{
    fn build(mut self) -> Result<RuntimeVectors, SolveModelLoweringError> {
        let mut columns = RuntimeColumns {
            initial_y: vec![0.0; self.problem.layout.y_scalars()],
            solver_nominals: vec![1.0; self.problem.layout.y_scalars()],
            parameters: vec![0.0; self.problem.layout.p_scalars()],
        };
        self.seed_homotopy_continuation(&mut columns)?;

        for (id, variable) in self.view.variables() {
            if is_non_numeric(variable) {
                continue;
            }
            let values = self.evaluator.initial_value(id).map_err(evaluation_error)?;
            let nominals = self.variable_nominals(variable)?;
            self.write_variable(variable, &values, &nominals, &mut columns)?;
        }
        let (visible_names, visible_value_rows, variable_meta) = self.visible_projections()?;
        Ok(RuntimeVectors {
            initial_y: columns.initial_y,
            solver_nominals: columns.solver_nominals,
            parameters: columns.parameters,
            visible_names,
            visible_value_rows,
            variable_meta,
        })
    }

    /// Seed the hidden homotopy continuation slot (λ) to `1.0`.
    ///
    /// MLS 3.6 §3.7.4.3 defines `homotopy(actual, simplified)` through the blend
    /// `lambda*actual + (1 - lambda)*simplified` and explicitly permits the
    /// trivial implementation `homotopy(actual, simplified) = actual`. λ = 1
    /// *is* that trivial implementation for the blend the Solve lowering emits,
    /// so seeding `1.0` makes `actual` the default reading of every homotopy
    /// expression: simulation-time rows, rows in systems the initialization
    /// continuation does not steer, and every backend that has no continuation
    /// at all agree on it. Seeding `0.0` would instead make `simplified` the
    /// default — a system the model author supplied only as a starting guess —
    /// for anything the continuation failed to reach.
    ///
    /// `SolveRuntime::project_initial_variables` is the only writer that moves
    /// λ: it drives `0 -> 1` around the solves its coverage certificate names
    /// and leaves `1.0` behind.
    fn seed_homotopy_continuation(
        &self,
        columns: &mut RuntimeColumns,
    ) -> Result<(), SolveModelLoweringError> {
        let Some(index) = self.problem.solve_layout.initial_homotopy_parameter_index else {
            return Ok(());
        };
        let len = columns.parameters.len();
        let slot = columns.parameters.get_mut(index).ok_or_else(|| {
            runtime_error(
                format!(
                    "initial homotopy parameter index {index} is outside the {len} runtime \
                     parameters"
                ),
                first_span(self.view),
            )
        })?;
        *slot = 1.0;
        Ok(())
    }

    fn visible_projections(
        &self,
    ) -> Result<
        (
            Vec<String>,
            solve::ScalarProgramBlock,
            Vec<solve::SolveVariableMeta>,
        ),
        SolveModelLoweringError,
    > {
        let mut names = Vec::new();
        let mut programs = Vec::new();
        let mut spans = Vec::new();
        let mut metadata = Vec::new();
        for (id, variable) in self
            .view
            .variables()
            .filter(|(_, variable)| is_visible_role(variable.role()) && !is_non_numeric(*variable))
        {
            for scalar in 0..variable.scalar_count() {
                let name = scalar_name(variable, scalar)?;
                let slot = visible_variable_slot(self.problem, id, variable, scalar, &name)?;
                programs.push(slot_projection(slot, variable.declaration().span())?);
                spans.push(variable.declaration().span());
                metadata.push(self.variable_meta(id, variable, name.clone()));
                names.push(name);
            }
        }
        let rows = solve::ScalarProgramBlock::with_program_spans(programs, spans)
            .map_err(|error| runtime_error(error.to_string(), first_span(self.view)))?;
        Ok((names, rows, metadata))
    }

    fn write_variable(
        &self,
        variable: dae::VariableView<'dae>,
        values: &[f64],
        nominals: &[f64],
        columns: &mut RuntimeColumns,
    ) -> Result<(), SolveModelLoweringError> {
        for scalar in 0..variable.scalar_count() {
            let name = scalar_name(variable, scalar)?;
            let slot = variable_slot(self.problem, variable, &name)?;
            match slot {
                solve::ScalarSlot::Y { index, .. } => {
                    columns.initial_y[index] = values[scalar];
                    columns.solver_nominals[index] = nominals[scalar];
                }
                solve::ScalarSlot::P { index, .. } => columns.parameters[index] = values[scalar],
                solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
                    return Err(runtime_error(
                        format!("checked variable `{name}` was assigned a non-storage slot"),
                        variable.declaration().span(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn variable_nominals(
        &mut self,
        variable: dae::VariableView<'dae>,
    ) -> Result<Vec<f64>, SolveModelLoweringError> {
        let Some(expression) = variable.nominal() else {
            return Ok(vec![1.0; variable.scalar_count()]);
        };
        let mut values = self
            .evaluator
            .expression(expression)
            .map_err(evaluation_error)?;
        if values.len() == 1 && variable.scalar_count() > 1 {
            values.resize(variable.scalar_count(), values[0]);
        }
        if values.len() != variable.scalar_count()
            || values
                .iter()
                .any(|value| !value.is_finite() || *value <= 0.0)
        {
            return Err(runtime_error(
                format!(
                    "nominal for `{}` must contain {} finite positive values",
                    variable.name(),
                    variable.scalar_count()
                ),
                expression_span(self.view, expression),
            ));
        }
        Ok(values)
    }

    fn variable_meta(
        &self,
        id: dae::VariableId<'dae>,
        variable: dae::VariableView<'dae>,
        name: String,
    ) -> solve::SolveVariableMeta {
        let time_domain = self
            .problem
            .solve_layout
            .variable_declarations
            .get(id.index() as usize)
            .expect("checked Solve declaration catalog follows dense DAE identity")
            .time_domain();
        solve::SolveVariableMeta {
            name,
            source_span: variable.declaration().span(),
            role: role_name(variable.role()).to_string(),
            is_state: variable.role() == dae::VariableRole::State,
            value_type: Some(format!("{:?}", variable.value_type().scalar_type())),
            variability: Some(format!("{:?}", variable.variability())),
            time_domain: Some(time_domain.as_str().to_string()),
            unit: variable.unit().map(str::to_string),
            start: variable
                .start()
                .and_then(|expression| self.expression_source(expression)),
            min: variable
                .minimum()
                .and_then(|expression| self.expression_source(expression)),
            max: variable
                .maximum()
                .and_then(|expression| self.expression_source(expression)),
            nominal: variable
                .nominal()
                .and_then(|expression| self.expression_source(expression)),
            fixed: variable.fixed(),
            description: variable.description().map(str::to_string),
        }
    }

    fn expression_source(&self, expression: dae::ExprId<'dae>) -> Option<String> {
        let provenance = self.view.expression(expression)?.provenance();
        self.model.source_text(provenance).map(str::to_string)
    }
}

fn visible_variable_slot(
    problem: &solve::SolveProblem,
    id: dae::VariableId<'_>,
    variable: dae::VariableView<'_>,
    scalar: usize,
    name: &str,
) -> Result<solve::ScalarSlot, SolveModelLoweringError> {
    problem
        .solve_layout
        .variable_scalar_slot(id.index() as usize, scalar)
        .ok_or_else(|| {
            runtime_error(
                format!("checked variable `{name}` has no typed Solve slot"),
                variable.declaration().span(),
            )
        })
}

struct RuntimeColumns {
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
    parameters: Vec<f64>,
}

/// True when a checked declaration carries no numeric value at all.
///
/// The runtime vectors are `f64` columns; MLS §3.8.4 gives `String` no numeric value, so
/// a `String` declaration (for example `Modelica.Clocked.Types.SolverMethod solverMethod`
/// on every clocked partition) has nothing to initialize and nothing to trace. Skipping
/// it substitutes no stand-in value: the checked DAE rejects a `String` operand in any
/// numeric expression or residual at construction, so no lowered program can read the
/// storage column the layout reserves for it.
fn is_non_numeric(variable: dae::VariableView<'_>) -> bool {
    variable.value_type().scalar_type() == dae::ScalarType::String
}

fn is_visible_role(role: dae::VariableRole) -> bool {
    matches!(
        role,
        dae::VariableRole::State
            | dae::VariableRole::Algebraic
            | dae::VariableRole::Output
            | dae::VariableRole::Input
            | dae::VariableRole::DiscreteReal
            | dae::VariableRole::DiscreteValue
    )
}

fn slot_projection(
    slot: solve::ScalarSlot,
    span: Span,
) -> Result<Vec<solve::LinearOp>, SolveModelLoweringError> {
    let load = match slot {
        solve::ScalarSlot::Y { index, .. } => solve::LinearOp::LoadY { dst: 0, index },
        solve::ScalarSlot::P { index, .. } => solve::LinearOp::LoadP { dst: 0, index },
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
            return Err(runtime_error(
                "visible DAE coordinate does not map to writable Solve storage",
                span,
            ));
        }
    };
    Ok(vec![load, solve::LinearOp::StoreOutput { src: 0 }])
}

fn scalar_name(
    variable: dae::VariableView<'_>,
    scalar: usize,
) -> Result<String, SolveModelLoweringError> {
    variable.scalar_name(scalar).ok_or_else(|| {
        runtime_error(
            format!(
                "checked variable `{}` has no scalar name at ordinal {scalar}",
                variable.name()
            ),
            variable.declaration().span(),
        )
    })
}

fn variable_slot(
    problem: &solve::SolveProblem,
    variable: dae::VariableView<'_>,
    name: &str,
) -> Result<solve::ScalarSlot, SolveModelLoweringError> {
    problem.layout.binding(name).ok_or_else(|| {
        runtime_error(
            format!("checked variable `{name}` has no Solve storage slot"),
            variable.declaration().span(),
        )
    })
}

fn evaluation_error(error: NumericEvaluationError) -> SolveModelLoweringError {
    if error.kind() == NumericEvaluationErrorKind::InvalidOverride {
        SolveModelLoweringError::InvalidOverride {
            message: error.to_string(),
        }
    } else {
        runtime_error(error.to_string(), error.span())
    }
}

fn expression_span<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> Span {
    view.expression(expression)
        .expect("finalized expression identity resolves")
        .provenance()
        .span()
}

const fn role_name(role: dae::VariableRole) -> &'static str {
    match role {
        dae::VariableRole::Parameter => "parameter",
        dae::VariableRole::Constant => "constant",
        dae::VariableRole::Input => "input",
        dae::VariableRole::State => "state",
        dae::VariableRole::Algebraic => "algebraic",
        dae::VariableRole::Output => "output",
        dae::VariableRole::DiscreteReal => "discrete-real",
        dae::VariableRole::DiscreteValue => "discrete-valued",
    }
}

fn first_span(view: dae::DaeView<'_>) -> Span {
    view.responsible_span()
        .expect("runtime layout with visible variables has responsible provenance")
}

fn runtime_error(message: impl Into<String>, span: Span) -> SolveModelLoweringError {
    SolveModelLoweringError::RuntimeValues {
        message: message.into(),
        span: (!span.is_dummy()).then_some(span),
    }
}
