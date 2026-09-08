use std::collections::HashMap;

use rumoca_core::Span;
use rumoca_eval_dae::{NumericEvaluationError, NumericEvaluationErrorKind, NumericEvaluator};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::scalar_constant_derivative_refinement::{
    AdmittedScalarConstantDerivativeProfile, CheckedDaeSolveScalarConstantDerivativeRefinement,
    ScalarConstantDerivativeMismatch, ScalarConstantDerivativeUnsupported,
    admit_scalar_constant_derivative_profile, check_scalar_constant_derivative_facts,
};
use crate::{LowerError, lower_prepared_solve_package, lower_solve_artifacts};
use crate::{
    VariableCatalogRefinementError,
    variable_catalog_refinement::{
        CheckedDaeSolveVariableCatalogRefinement, VariableCatalogTransferMap,
        check_variable_catalog_refinement,
    },
};

type CatalogEntry = (
    solve::SolveVariableSource,
    solve::SolveVariableSourceAttributes,
    solve::SolveVariableEvaluatedValues,
);

/// Failure while constructing the complete executable Solve root from one DAE.
#[derive(Debug)]
pub enum SolveModelLoweringError {
    Lower(LowerError),
    RuntimeValues {
        message: String,
        span: Option<Span>,
    },
    InvalidOverride {
        message: String,
    },
    VariableCatalogRefinement {
        error: VariableCatalogRefinementError,
        span: Option<Span>,
    },
    /// The DAE was admitted to the scalar constant-derivative profile, but
    /// the Solve root built from it does not refine the admitted facts.
    ScalarConstantDerivativeRefinement {
        error: ScalarConstantDerivativeMismatch,
        span: Option<Span>,
    },
}

impl SolveModelLoweringError {
    #[must_use]
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::Lower(error) => error.source_span(),
            Self::RuntimeValues { span, .. } => *span,
            Self::InvalidOverride { .. } => None,
            Self::VariableCatalogRefinement { span, .. }
            | Self::ScalarConstantDerivativeRefinement { span, .. } => *span,
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
            Self::VariableCatalogRefinement { error, .. } => write!(formatter, "{error}"),
            Self::ScalarConstantDerivativeRefinement { error, .. } => {
                write!(formatter, "{error}")
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

/// The one prepared input context carried from profile admission through C60.
struct PreparedSolveContext<'source> {
    prepared: rumoca_phase_structural::PreparedDae<'source>,
    scalar_constant_derivative_profile:
        Result<AdmittedScalarConstantDerivativeProfile, ScalarConstantDerivativeUnsupported>,
}

/// A freshly constructed Solve root that has passed SOLVE-C60.
///
/// C61 consumes this entire carrier, so it cannot be asked to authenticate an
/// independently supplied root or profile with a detached C60 receipt.
struct C60CheckedSolveRoot<'source> {
    model: solve::SolveModel,
    context: PreparedSolveContext<'source>,
    variable_catalog_refinement: CheckedDaeSolveVariableCatalogRefinement,
}

/// The final live checked root. Proof fields remain co-owned until terminal
/// consumption deliberately erases them into a bare Solve model.
struct CheckedSolveRoot<'source> {
    model: solve::SolveModel,
    _prepared: rumoca_phase_structural::PreparedDae<'source>,
    _variable_catalog_refinement: CheckedDaeSolveVariableCatalogRefinement,
    /// The C61 equation-refinement receipt for a root inside the scalar
    /// constant-derivative profile, or the typed reason the exact prepared
    /// DAE is outside that profile and the root is unclaimed by it.
    scalar_constant_derivative_refinement: Result<
        CheckedDaeSolveScalarConstantDerivativeRefinement,
        ScalarConstantDerivativeUnsupported,
    >,
}

/// Complete checked Solve construction plus its phase-owned timing split.
pub struct LoweredSolveModel<'source> {
    checked_root: CheckedSolveRoot<'source>,
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
        &self.checked_root.model
    }

    /// Borrow the C61 receipt proving this root's derivative kernel, tangent
    /// program, visible row, start transfer, and owner census refine its DAE,
    /// or the typed profile refusal for a root the receipt does not cover.
    ///
    /// The receipt can only have been minted by the checker over this exact
    /// root during `lower_solve_model`; a read-only borrow cannot detach it.
    pub const fn scalar_constant_derivative_refinement(
        &self,
    ) -> Result<
        &CheckedDaeSolveScalarConstantDerivativeRefinement,
        &ScalarConstantDerivativeUnsupported,
    > {
        self.checked_root
            .scalar_constant_derivative_refinement
            .as_ref()
    }

    #[must_use]
    pub fn into_model(self) -> solve::SolveModel {
        self.checked_root.model
    }
}

impl<'source> PreparedSolveContext<'source> {
    fn new(
        prepared: rumoca_phase_structural::PreparedDae<'source>,
        overrides: &HashMap<String, f64>,
    ) -> Self {
        let scalar_constant_derivative_profile = prepared
            .as_dae()
            .inspect(|view| admit_scalar_constant_derivative_profile(view, overrides));
        Self {
            prepared,
            scalar_constant_derivative_profile,
        }
    }

    fn prepared(&self) -> &rumoca_phase_structural::PreparedDae<'source> {
        &self.prepared
    }
}

impl<'source> C60CheckedSolveRoot<'source> {
    fn construct(
        context: PreparedSolveContext<'source>,
        model: solve::SolveModel,
        mapping: VariableCatalogTransferMap,
    ) -> Result<Self, SolveModelLoweringError> {
        let variable_catalog_refinement = context
            .prepared
            .as_dae()
            .inspect(|view| {
                check_variable_catalog_refinement(
                    view.variable_refinement(),
                    model.variable_refinement(),
                    mapping,
                )
            })
            .map_err(|error| variable_catalog_refinement_error(context.prepared.as_dae(), error))?;
        Ok(Self {
            model,
            context,
            variable_catalog_refinement,
        })
    }

    fn into_equation_refined(self) -> Result<CheckedSolveRoot<'source>, SolveModelLoweringError> {
        let Self {
            model,
            context,
            variable_catalog_refinement,
        } = self;
        let PreparedSolveContext {
            prepared,
            scalar_constant_derivative_profile,
        } = context;
        let scalar_constant_derivative_refinement = match scalar_constant_derivative_profile {
            Ok(profile) => Ok(
                check_scalar_constant_derivative_facts(&profile, &model).map_err(|error| {
                    scalar_constant_derivative_refinement_error(prepared.as_dae(), error)
                })?,
            ),
            Err(unsupported) => Err(unsupported),
        };
        Ok(CheckedSolveRoot {
            model,
            _prepared: prepared,
            _variable_catalog_refinement: variable_catalog_refinement,
            scalar_constant_derivative_refinement,
        })
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
    // Admission is retained inside the one prepared context before Solve
    // construction. That context is consumed by C60 and then C61, so neither
    // transition can be paired with a second prepared root.
    let context = PreparedSolveContext::new(prepared, overrides);
    let package = lower_prepared_solve_package(context.prepared())?;
    let problem = package.problem;
    let artifacts = lower_solve_artifacts(&problem)?;
    let program_seconds = rumoca_core::maybe_elapsed_seconds(program_start);

    begin_stage(SolveModelLoweringStage::RuntimeValues);
    let runtime_value_start = rumoca_core::maybe_start_timer();
    let vectors = runtime_vectors(context.prepared().as_dae(), &problem, overrides)?;
    let solve_model = solve::SolveModel::construct(
        problem,
        package.pure_calls,
        artifacts,
        solve::SolveModelRuntimeInputs {
            initial_y: vectors.initial_y,
            solver_nominals: vectors.solver_nominals,
            parameters: vectors.parameters,
        },
        vectors.visible_value_rows,
        vectors.catalog_entries,
    )
    .map_err(|error| solve_model_construction_error(context.prepared().as_dae(), error))?;
    let checked_root =
        C60CheckedSolveRoot::construct(context, solve_model, vectors.catalog_transfer_map)?
            .into_equation_refined()?;
    let runtime_value_seconds = rumoca_core::maybe_elapsed_seconds(runtime_value_start);
    Ok(LoweredSolveModel {
        checked_root,
        program_seconds,
        runtime_value_seconds,
    })
}

/// Locate a C61 mismatch at the admitted profile's single continuous
/// equation: it is the derivative meaning the Solve root failed to preserve.
fn scalar_constant_derivative_refinement_error(
    model: &dae::Dae,
    error: ScalarConstantDerivativeMismatch,
) -> SolveModelLoweringError {
    let span = model.inspect(|view| match view.continuous_owner(0) {
        Some(dae::ContinuousOwnerView::Residual { equation, .. }) => {
            Some(equation.provenance().span())
        }
        Some(dae::ContinuousOwnerView::Structured { .. }) | None => view.responsible_span(),
    });
    SolveModelLoweringError::ScalarConstantDerivativeRefinement { error, span }
}

pub(super) fn variable_catalog_refinement_error(
    model: &dae::Dae,
    error: VariableCatalogRefinementError,
) -> SolveModelLoweringError {
    let span = model.inspect(|view| {
        error
            .source_occurrence()
            .and_then(|occurrence| {
                view.variables()
                    .find(|(_, variable)| variable.source_occurrence() == occurrence)
                    .map(|(_, variable)| variable.declaration().span())
            })
            .or_else(|| {
                error
                    .dae_ordinal()
                    .and_then(|ordinal| view.variables().nth(ordinal))
                    .map(|(_, variable)| variable.declaration().span())
            })
    });
    SolveModelLoweringError::VariableCatalogRefinement { error, span }
}

struct RuntimeVectors {
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
    parameters: Vec<f64>,
    visible_value_rows: solve::ScalarProgramBlock,
    catalog_entries: Vec<CatalogEntry>,
    catalog_transfer_map: VariableCatalogTransferMap,
}

fn runtime_vectors(
    model: &dae::Dae,
    problem: &solve::SolveProblem,
    overrides: &HashMap<String, f64>,
) -> Result<RuntimeVectors, SolveModelLoweringError> {
    model.inspect(|view| {
        validate_runtime_overrides(view, overrides)?;
        let evaluator = NumericEvaluator::with_overrides(view, |variable, scalar| {
            (variable.role() == dae::VariableRole::Input || variable.is_tunable())
                .then(|| variable.scalar_name(scalar))
                .flatten()
                .and_then(|name| overrides.get(&name).copied())
        });
        RuntimeVectorBuilder {
            view,
            problem,
            overrides,
            evaluator,
        }
        .build()
    })
}

/// Evaluate the declared pre-write value of one host-driven input.
///
/// This is the only pre-construction evaluation used by a host that promises
/// to write the input at runtime. The resulting exact scalar overrides enter
/// the ordinary `lower_solve_model` construction, so the start expression is
/// not evaluated a second time and the sealed catalog retains the values that
/// actually occupy the final runtime slots.
pub fn host_driven_input_start_values<'dae>(
    view: dae::DaeView<'dae>,
    variable: dae::VariableView<'dae>,
) -> Result<Option<Vec<f64>>, SolveModelLoweringError> {
    if variable.role() != dae::VariableRole::Input {
        return Err(runtime_error(
            format!(
                "host-driven start requested for non-input `{}`",
                variable.name()
            ),
            variable.declaration().span(),
        ));
    }
    let Some(expression) = variable.start() else {
        return Ok(None);
    };
    if variable.scalar_count() == 0 {
        return Ok(Some(Vec::new()));
    }
    if !matches!(
        variable.value_type().scalar_type(),
        dae::ScalarType::Real | dae::ScalarType::Integer
    ) {
        return Err(runtime_error(
            format!(
                "numeric host input cannot represent {:?} `{}`",
                variable.value_type().scalar_type(),
                variable.name()
            ),
            variable.declaration().span(),
        ));
    }
    let mut evaluator = NumericEvaluator::new(view);
    let mut values = evaluator.expression(expression).map_err(evaluation_error)?;
    if values.len() == 1 && variable.scalar_count() > 1 {
        values.resize(variable.scalar_count(), values[0]);
    }
    if values.len() != variable.scalar_count() {
        return Err(runtime_error(
            format!(
                "start for `{}` contains {} scalars; expected {}",
                variable.name(),
                values.len(),
                variable.scalar_count()
            ),
            expression_span(view, expression, variable)?,
        ));
    }
    Ok(Some(values))
}

fn validate_runtime_overrides(
    view: dae::DaeView<'_>,
    overrides: &HashMap<String, f64>,
) -> Result<(), SolveModelLoweringError> {
    let mut scalar_types = HashMap::new();
    for (_, variable) in view.variables() {
        for scalar in 0..variable.scalar_count() {
            let name = variable.scalar_name(scalar).ok_or_else(|| {
                invalid_override(format!(
                    "checked variable `{}` has no scalar name at ordinal {scalar}",
                    variable.name()
                ))
            })?;
            if scalar_types
                .insert(
                    name.clone(),
                    (
                        variable.value_type().scalar_type(),
                        variable.role(),
                        variable.is_tunable(),
                    ),
                )
                .is_some()
            {
                return Err(invalid_override(format!(
                    "runtime override identity `{name}` is not unique"
                )));
            }
        }
    }
    for (name, value) in overrides {
        if !value.is_finite() {
            return Err(invalid_override(format!(
                "runtime override for `{name}` must be finite"
            )));
        }
        let (kind, role, tunable) = scalar_types.get(name).copied().ok_or_else(|| {
            invalid_override(format!(
                "`{name}` is not an exact scalar identity of this model"
            ))
        })?;
        if role != dae::VariableRole::State && role != dae::VariableRole::Input && !tunable {
            return Err(invalid_override(format!(
                "`{name}` is not a tunable parameter, input, or state"
            )));
        }
        match kind {
            dae::ScalarType::Real => {}
            dae::ScalarType::Integer if value.fract() == 0.0 => {}
            dae::ScalarType::Integer => {
                return Err(invalid_override(format!(
                    "Integer override for `{name}` must be integral"
                )));
            }
            dae::ScalarType::Boolean
            | dae::ScalarType::String
            | dae::ScalarType::Enumeration
            | dae::ScalarType::Record => {
                return Err(invalid_override(format!(
                    "numeric runtime override cannot represent {kind:?} scalar `{name}`"
                )));
            }
        }
    }
    Ok(())
}

fn invalid_override(message: String) -> SolveModelLoweringError {
    SolveModelLoweringError::InvalidOverride { message }
}

struct RuntimeVectorBuilder<'model, 'dae, F> {
    view: dae::DaeView<'dae>,
    problem: &'model solve::SolveProblem,
    overrides: &'model HashMap<String, f64>,
    evaluator: NumericEvaluator<'dae, F>,
}

impl<'dae, F> RuntimeVectorBuilder<'_, 'dae, F>
where
    F: FnMut(dae::VariableView<'dae>, usize) -> Option<f64>,
{
    fn build(mut self) -> Result<RuntimeVectors, SolveModelLoweringError> {
        let mut columns = RuntimeColumns {
            initial_y: vec![0.0; self.problem.layout().y_scalars()],
            solver_nominals: vec![1.0; self.problem.layout().y_scalars()],
            parameters: vec![0.0; self.problem.layout().p_scalars()],
        };
        self.seed_homotopy_continuation(&mut columns)?;
        let mut catalog_entries = Vec::with_capacity(self.view.variables().count());
        let mut catalog_transfer_map =
            VariableCatalogTransferMap::new(self.view.variables().count());

        for (id, variable) in self.view.variables() {
            catalog_transfer_map.record(variable.source_occurrence(), catalog_entries.len());
            if is_non_numeric(variable) {
                catalog_entries.push(self.non_numeric_catalog_entry(variable)?);
                continue;
            }
            let mut values = self.evaluator.initial_value(id).map_err(evaluation_error)?;
            self.apply_state_overrides(variable, &mut values)?;
            let nominals = self.variable_nominals(variable)?;
            let minimum = self.numeric_attribute(variable, variable.minimum())?;
            let maximum = self.numeric_attribute(variable, variable.maximum())?;
            self.write_variable(id, variable, &values, &nominals, &mut columns)?;
            let start = Some(self.final_runtime_values(id, variable, &columns)?);
            let nominal = variable.nominal().map(|_| nominals);
            catalog_entries.push(self.catalog_entry(variable, start, minimum, maximum, nominal)?);
        }
        let visible_value_rows = self.visible_projections()?;
        Ok(RuntimeVectors {
            initial_y: columns.initial_y,
            solver_nominals: columns.solver_nominals,
            parameters: columns.parameters,
            visible_value_rows,
            catalog_entries,
            catalog_transfer_map,
        })
    }

    fn non_numeric_catalog_entry(
        &self,
        variable: dae::VariableView<'dae>,
    ) -> Result<CatalogEntry, SolveModelLoweringError> {
        if is_visible_role(variable.role()) {
            return Err(runtime_error(
                format!(
                    "Solve lowering cannot represent runtime {:?} variable `{}`",
                    variable.value_type().scalar_type(),
                    variable.name()
                ),
                variable.declaration().span(),
            ));
        }
        self.catalog_entry(variable, None, None, None, None)
    }

    fn apply_state_overrides(
        &self,
        variable: dae::VariableView<'dae>,
        values: &mut [f64],
    ) -> Result<(), SolveModelLoweringError> {
        if variable.role() != dae::VariableRole::State {
            return Ok(());
        }
        for (scalar, value) in values.iter_mut().enumerate() {
            let name = variable.scalar_name(scalar).ok_or_else(|| {
                invalid_override(format!(
                    "checked state `{}` has no scalar name at ordinal {scalar}",
                    variable.name()
                ))
            })?;
            if let Some(overridden) = self.overrides.get(&name) {
                *value = *overridden;
            }
        }
        Ok(())
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
        let Some(index) = self.problem.solve_layout().initial_homotopy_parameter_index else {
            return Ok(());
        };
        let len = columns.parameters.len();
        let slot = columns.parameters.get_mut(index).ok_or_else(|| {
            SolveModelLoweringError::RuntimeValues {
                message: format!(
                    "initial homotopy parameter index {index} is outside the {len} runtime \
                     parameters"
                ),
                span: self.view.responsible_span(),
            }
        })?;
        *slot = 1.0;
        Ok(())
    }

    fn visible_projections(&self) -> Result<solve::ScalarProgramBlock, SolveModelLoweringError> {
        let mut programs = Vec::new();
        let mut spans = Vec::new();
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
            }
        }
        let rows =
            solve::ScalarProgramBlock::with_program_spans(programs, spans).map_err(|error| {
                SolveModelLoweringError::RuntimeValues {
                    message: error.to_string(),
                    span: self.view.responsible_span(),
                }
            })?;
        Ok(rows)
    }

    fn catalog_entry(
        &self,
        variable: dae::VariableView<'dae>,
        start: Option<Vec<f64>>,
        minimum: Option<Vec<f64>>,
        maximum: Option<Vec<f64>>,
        nominal: Option<Vec<f64>>,
    ) -> Result<
        (
            solve::SolveVariableSource,
            solve::SolveVariableSourceAttributes,
            solve::SolveVariableEvaluatedValues,
        ),
        SolveModelLoweringError,
    > {
        let scalar_names = (0..variable.scalar_count())
            .map(|scalar| scalar_name(variable, scalar))
            .collect::<Result<Vec<_>, _>>()?;
        let source = solve::SolveVariableSource::new(
            variable.source_occurrence(),
            variable.name().to_string(),
            variable.value_type().dimensions().to_vec(),
            scalar_names,
            variable.declaration().span(),
        );
        let attributes = solve::SolveVariableSourceAttributes::new(
            solve_causality(variable.causality()),
            solve_variability(variable),
            variable.is_tunable(),
            variable.unit().map(str::to_string),
            variable.description().map(str::to_string),
            variable.fixed(),
        );
        let values = solve::SolveVariableEvaluatedValues::new(start, minimum, maximum, nominal);
        Ok((source, attributes, values))
    }

    fn final_runtime_values(
        &self,
        id: dae::VariableId<'dae>,
        variable: dae::VariableView<'dae>,
        columns: &RuntimeColumns,
    ) -> Result<Vec<f64>, SolveModelLoweringError> {
        (0..variable.scalar_count())
            .map(|scalar| {
                let name = scalar_name(variable, scalar)?;
                let slot = visible_variable_slot(self.problem, id, variable, scalar, &name)?;
                match slot {
                    solve::ScalarSlot::Y { index, .. } => columns
                        .initial_y
                        .get(index)
                        .copied()
                        .ok_or_else(|| missing_runtime_slot(variable, &name)),
                    solve::ScalarSlot::P { index, .. } => columns
                        .parameters
                        .get(index)
                        .copied()
                        .ok_or_else(|| missing_runtime_slot(variable, &name)),
                    solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
                        Err(missing_runtime_slot(variable, &name))
                    }
                }
            })
            .collect()
    }

    fn numeric_attribute(
        &mut self,
        variable: dae::VariableView<'dae>,
        expression: Option<dae::ExprId<'dae>>,
    ) -> Result<Option<Vec<f64>>, SolveModelLoweringError> {
        let Some(expression) = expression else {
            return Ok(None);
        };
        let mut values = self
            .evaluator
            .expression(expression)
            .map_err(evaluation_error)?;
        if values.len() == 1 && variable.scalar_count() > 1 {
            values.resize(variable.scalar_count(), values[0]);
        }
        if values.len() != variable.scalar_count() {
            return Err(runtime_error(
                format!(
                    "attribute for `{}` contains {} scalars; expected {}",
                    variable.name(),
                    values.len(),
                    variable.scalar_count()
                ),
                expression_span(self.view, expression, variable)?,
            ));
        }
        Ok(Some(values))
    }

    fn write_variable(
        &self,
        id: dae::VariableId<'dae>,
        variable: dae::VariableView<'dae>,
        values: &[f64],
        nominals: &[f64],
        columns: &mut RuntimeColumns,
    ) -> Result<(), SolveModelLoweringError> {
        for scalar in 0..variable.scalar_count() {
            let name = scalar_name(variable, scalar)?;
            let slot = visible_variable_slot(self.problem, id, variable, scalar, &name)?;
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
                expression_span(self.view, expression, variable)?,
            ));
        }
        Ok(values)
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
        .solve_layout()
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

fn evaluation_error(error: NumericEvaluationError) -> SolveModelLoweringError {
    if error.kind() == NumericEvaluationErrorKind::InvalidOverride {
        SolveModelLoweringError::InvalidOverride {
            message: error.to_string(),
        }
    } else {
        runtime_error(error.to_string(), error.span())
    }
}

fn expression_span<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    variable: dae::VariableView<'dae>,
) -> Result<Span, SolveModelLoweringError> {
    view.expression(expression)
        .map(|expression| expression.provenance().span())
        .ok_or_else(|| {
            runtime_error(
                format!(
                    "checked expression for `{}` has no finalized identity",
                    variable.name()
                ),
                variable.declaration().span(),
            )
        })
}

const fn solve_causality(causality: dae::VariableCausality) -> solve::SolveVariableCausality {
    match causality {
        dae::VariableCausality::Input => solve::SolveVariableCausality::Input,
        dae::VariableCausality::Output => solve::SolveVariableCausality::Output,
        dae::VariableCausality::Parameter => solve::SolveVariableCausality::Parameter,
        dae::VariableCausality::CalculatedParameter => {
            solve::SolveVariableCausality::CalculatedParameter
        }
        dae::VariableCausality::Independent => solve::SolveVariableCausality::Independent,
        dae::VariableCausality::Local => solve::SolveVariableCausality::Local,
    }
}

fn solve_variability(variable: dae::VariableView<'_>) -> solve::SolveVariableVariability {
    match variable.variability() {
        dae::ExpressionVariability::Constant => solve::SolveVariableVariability::Constant,
        dae::ExpressionVariability::Parameter if variable.is_tunable() => {
            solve::SolveVariableVariability::Tunable
        }
        dae::ExpressionVariability::Parameter => solve::SolveVariableVariability::Fixed,
        dae::ExpressionVariability::Discrete => solve::SolveVariableVariability::Discrete,
        dae::ExpressionVariability::Continuous => solve::SolveVariableVariability::Continuous,
    }
}

fn missing_runtime_slot(variable: dae::VariableView<'_>, name: &str) -> SolveModelLoweringError {
    runtime_error(
        format!("checked variable `{name}` has no final Solve runtime slot"),
        variable.declaration().span(),
    )
}

fn solve_model_construction_error(
    model: &dae::Dae,
    error: solve::SolveModelConstructionError,
) -> SolveModelLoweringError {
    let span = match &error {
        solve::SolveModelConstructionError::VariableCatalog(error) => error.span(),
        solve::SolveModelConstructionError::Shape(error) => error.source_span(),
        _ => None,
    };
    SolveModelLoweringError::RuntimeValues {
        message: error.to_string(),
        span: span.or_else(|| model.inspect(|view| view.responsible_span())),
    }
}

fn runtime_error(message: impl Into<String>, span: Span) -> SolveModelLoweringError {
    SolveModelLoweringError::RuntimeValues {
        message: message.into(),
        span: (!span.is_dummy()).then_some(span),
    }
}

mod ownership_trait_assertions;
