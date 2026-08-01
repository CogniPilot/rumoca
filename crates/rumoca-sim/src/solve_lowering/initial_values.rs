use std::collections::HashMap;

use rumoca_core::Span;
use rumoca_eval_dae::{NumericEvaluationError, NumericEvaluationErrorKind, NumericEvaluator};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::diagnostics::SimulationDiagnosticError;

pub(super) struct RuntimeVectors {
    pub(super) initial_y: Vec<f64>,
    pub(super) solver_nominals: Vec<f64>,
    pub(super) parameters: Vec<f64>,
    pub(super) visible_names: Vec<String>,
    pub(super) visible_value_rows: solve::ScalarProgramBlock,
    pub(super) variable_meta: Vec<solve::SolveVariableMeta>,
}

pub(super) fn runtime_vectors(
    model: &dae::Dae,
    problem: &solve::SolveProblem,
    overrides: &HashMap<String, f64>,
) -> Result<RuntimeVectors, SimulationDiagnosticError> {
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
    fn build(mut self) -> Result<RuntimeVectors, SimulationDiagnosticError> {
        let mut columns = RuntimeColumns {
            initial_y: vec![0.0; self.problem.layout.y_scalars()],
            solver_nominals: vec![1.0; self.problem.layout.y_scalars()],
            parameters: vec![0.0; self.problem.layout.p_scalars()],
        };
        self.seed_homotopy_continuation(&mut columns)?;

        for index in 0..self.view.variable_count() {
            let id = self
                .view
                .variable_id(index)
                .expect("finalized dense variable has an identity");
            let variable = self
                .view
                .variable(id)
                .expect("finalized variable identity resolves");
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
    ) -> Result<(), SimulationDiagnosticError> {
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
        SimulationDiagnosticError,
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
                metadata.push(self.variable_meta(variable, name.clone()));
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
    ) -> Result<(), SimulationDiagnosticError> {
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
    ) -> Result<Vec<f64>, SimulationDiagnosticError> {
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
        variable: dae::VariableView<'dae>,
        name: String,
    ) -> solve::SolveVariableMeta {
        solve::SolveVariableMeta {
            name,
            source_span: variable.declaration().span(),
            role: role_name(variable.role()).to_string(),
            is_state: variable.role() == dae::VariableRole::State,
            value_type: Some(format!("{:?}", variable.value_type().scalar_type())),
            variability: Some(format!("{:?}", variable.variability())),
            time_domain: Some(time_domain(variable.role()).to_string()),
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
) -> Result<solve::ScalarSlot, SimulationDiagnosticError> {
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
) -> Result<Vec<solve::LinearOp>, SimulationDiagnosticError> {
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
) -> Result<String, SimulationDiagnosticError> {
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
) -> Result<solve::ScalarSlot, SimulationDiagnosticError> {
    problem.layout.binding(name).ok_or_else(|| {
        runtime_error(
            format!("checked variable `{name}` has no Solve storage slot"),
            variable.declaration().span(),
        )
    })
}

pub(super) fn evaluation_error(error: NumericEvaluationError) -> SimulationDiagnosticError {
    if error.kind() == NumericEvaluationErrorKind::InvalidOverride {
        SimulationDiagnosticError::InvalidOverride {
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

const fn time_domain(role: dae::VariableRole) -> &'static str {
    match role {
        dae::VariableRole::Parameter | dae::VariableRole::Constant => "static",
        dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue => "event-discrete",
        dae::VariableRole::Input
        | dae::VariableRole::State
        | dae::VariableRole::Algebraic
        | dae::VariableRole::Output => "continuous-time",
    }
}

fn first_span(view: dae::DaeView<'_>) -> Span {
    view.responsible_span()
        .expect("runtime layout with visible variables has responsible provenance")
}

fn runtime_error(message: impl Into<String>, span: Span) -> SimulationDiagnosticError {
    SimulationDiagnosticError::RuntimePreparation {
        message: message.into(),
        span: (!span.is_dummy()).then_some(span),
    }
}
