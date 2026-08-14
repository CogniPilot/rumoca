//! DAE + Solve lowering into the checked FMI component projection.

use rumoca_ir_dae as dae;
use rumoca_ir_fmi::{
    FmiCausality, FmiComponent, FmiComponentError, FmiVariability, FmiVariableInput,
};
use rumoca_ir_solve::{SolveProblem, SolveVariableStorageRole, SolveVariableValueKind};
use std::collections::BTreeSet;

#[derive(Debug, thiserror::Error)]
pub enum FmiLoweringError {
    #[error("unsupported FMI scalar type `{kind:?}` for `{variable}`")]
    UnsupportedScalarType {
        variable: String,
        kind: dae::ScalarType,
    },
    #[error("numeric FMI metadata for `{variable}` is invalid: {message}")]
    NumericMetadata { variable: String, message: String },
    #[error("FMI assertion-only event profile is invalid: {0}")]
    UnsupportedAssertionProfile(&'static str),
    #[error(transparent)]
    Component(#[from] FmiComponentError),
}

pub fn lower_to_fmi_component(
    dae: &dae::Dae,
    solve: SolveProblem,
    pure_calls: rumoca_ir_solve::SolvePureCallTable,
) -> Result<FmiComponent, FmiLoweringError> {
    dae.inspect(validate_dae_assertion_profile)?;
    let inputs = dae.inspect(lower_variables)?;
    FmiComponent::construct(solve, pure_calls, inputs).map_err(Into::into)
}

fn validate_dae_assertion_profile(view: dae::DaeView<'_>) -> Result<(), FmiLoweringError> {
    if view.time_event_count() != 0
        || view.structured_root_count() != 0
        || view.discrete_real_equation_count() != 0
    {
        return Err(FmiLoweringError::UnsupportedAssertionProfile(
            "time, structured-root, or discrete-Real events remain",
        ));
    }
    let mut conditions = BTreeSet::new();
    let mut relations = BTreeSet::new();
    for index in 0..view.event_action_count() {
        let action = view
            .event_action(view.event_action_id(index).expect("dense action identity"))
            .expect("checked action resolves");
        if !matches!(action.operation(), dae::EventActionOperation::Assert { .. }) {
            return Err(FmiLoweringError::UnsupportedAssertionProfile(
                "a non-assertion action remains",
            ));
        }
        collect_assertion_condition(view, action.trigger(), &mut conditions, &mut relations)?;
        collect_assertion_condition(view, action.guard(), &mut conditions, &mut relations)?;
    }
    if conditions.len() != view.condition_count() || relations.len() != view.relation_count() {
        return Err(FmiLoweringError::UnsupportedAssertionProfile(
            "a condition or relation is not owned by an assertion",
        ));
    }
    for index in 0..view.root_count() {
        let root = view
            .root(view.root_id(index).expect("dense root identity"))
            .expect("checked root resolves");
        if !relations.contains(&root.relation().index())
            || !conditions.contains(&root.activation().index())
        {
            return Err(FmiLoweringError::UnsupportedAssertionProfile(
                "a root is not owned by an assertion condition",
            ));
        }
    }
    Ok(())
}

fn collect_assertion_condition<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
    conditions: &mut BTreeSet<u32>,
    relations: &mut BTreeSet<u32>,
) -> Result<(), FmiLoweringError> {
    if !conditions.insert(condition.index()) {
        return Ok(());
    }
    let condition = view
        .condition(condition)
        .expect("checked assertion condition resolves");
    match condition.operation() {
        dae::ConditionOperation::Relation(relation) => {
            require_parameter_assertion_expression(
                view,
                view.relation(relation)
                    .expect("checked assertion relation resolves")
                    .expression(),
            )?;
            relations.insert(relation.index());
        }
        dae::ConditionOperation::Not(operand) => {
            collect_assertion_condition(view, operand, conditions, relations)?;
        }
        dae::ConditionOperation::And(lhs, rhs) | dae::ConditionOperation::Or(lhs, rhs) => {
            collect_assertion_condition(view, lhs, conditions, relations)?;
            collect_assertion_condition(view, rhs, conditions, relations)?;
        }
        dae::ConditionOperation::AnyRise(_, _) | dae::ConditionOperation::Clock(_) => {
            return Err(FmiLoweringError::UnsupportedAssertionProfile(
                "an edge- or clock-owned assertion remains",
            ));
        }
        dae::ConditionOperation::Discrete(expression) => {
            require_parameter_assertion_expression(view, expression)?;
        }
        dae::ConditionOperation::Initial | dae::ConditionOperation::Always => {}
    }
    Ok(())
}

fn require_parameter_assertion_expression<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Result<(), FmiLoweringError> {
    let mut runtime_coordinate = false;
    dae::for_each_expression(view, expression, |_, expression| {
        if let dae::ExpressionOperation::Coordinate(coordinate) = expression.operation()
            && !matches!(
                coordinate,
                dae::CoordinateView::Parameter(_)
                    | dae::CoordinateView::Algebraic(_)
                    | dae::CoordinateView::Binder(_)
                    | dae::CoordinateView::FunctionParameter(_)
            )
        {
            runtime_coordinate = true;
        }
    });
    if runtime_coordinate {
        return Err(FmiLoweringError::UnsupportedAssertionProfile(
            "an assertion directly depends on a runtime coordinate",
        ));
    }
    Ok(())
}

fn lower_variables(view: dae::DaeView<'_>) -> Result<Vec<FmiVariableInput>, FmiLoweringError> {
    let mut numeric = rumoca_phase_dae::numeric::NumericDaeContext::new(view);
    view.variables()
        .map(|(_, variable)| lower_variable(&mut numeric, variable))
        .collect()
}

fn lower_variable<'dae>(
    numeric: &mut rumoca_phase_dae::numeric::NumericDaeContext<'dae>,
    variable: dae::VariableView<'dae>,
) -> Result<FmiVariableInput, FmiLoweringError> {
    let scalar_count = variable.scalar_count();
    let role = solve_role(variable.role());
    let value_kind = solve_value_kind(variable)?;
    let (primary_start, fallback_start) = match variable.role() {
        dae::VariableRole::Parameter | dae::VariableRole::Constant => {
            (variable.binding(), variable.start())
        }
        _ => (variable.start(), variable.binding()),
    };
    let start = match numeric_attribute(numeric, variable, primary_start)? {
        Some(values) => values,
        None => numeric_attribute(numeric, variable, fallback_start)?
            .unwrap_or_else(|| vec![default_scalar_value(value_kind); scalar_count]),
    };
    Ok(FmiVariableInput {
        name: variable.name().to_string(),
        scalar_names: (0..scalar_count)
            .filter_map(|index| variable.scalar_name(index))
            .collect(),
        role,
        value_kind,
        dimensions: variable.value_type().dimensions().to_vec(),
        start,
        minimum: numeric_attribute(numeric, variable, variable.minimum())?,
        maximum: numeric_attribute(numeric, variable, variable.maximum())?,
        nominal: numeric_attribute(numeric, variable, variable.nominal())?,
        unit: variable.unit().map(str::to_owned),
        description: variable.description().map(str::to_owned),
        causality: fmi_causality(variable.causality()),
        variability: fmi_variability(variable),
        tunable: variable.is_tunable(),
        declaration: variable.declaration().span(),
    })
}

fn numeric_attribute<'dae>(
    numeric: &mut rumoca_phase_dae::numeric::NumericDaeContext<'dae>,
    variable: dae::VariableView<'dae>,
    expression: Option<dae::ExprId<'dae>>,
) -> Result<Option<Vec<f64>>, FmiLoweringError> {
    let Some(expression) = expression else {
        return Ok(None);
    };
    if !matches!(
        variable.value_type().scalar_type(),
        dae::ScalarType::Real | dae::ScalarType::Integer | dae::ScalarType::Boolean
    ) {
        return Ok(None);
    }
    let mut values =
        numeric
            .expression(expression)
            .map_err(|error| FmiLoweringError::NumericMetadata {
                variable: variable.name().to_string(),
                message: error.to_string(),
            })?;
    if values.len() == 1 && variable.scalar_count() > 1 {
        values.resize(variable.scalar_count(), values[0]);
    }
    if values.len() != variable.scalar_count() {
        return Err(FmiLoweringError::NumericMetadata {
            variable: variable.name().to_string(),
            message: format!(
                "attribute has {} scalars; expected {}",
                values.len(),
                variable.scalar_count()
            ),
        });
    }
    Ok(Some(values))
}

const fn solve_role(role: dae::VariableRole) -> SolveVariableStorageRole {
    match role {
        dae::VariableRole::Parameter => SolveVariableStorageRole::Parameter,
        dae::VariableRole::Constant => SolveVariableStorageRole::Constant,
        dae::VariableRole::Input => SolveVariableStorageRole::ExternalInput,
        dae::VariableRole::State => SolveVariableStorageRole::State,
        dae::VariableRole::Algebraic => SolveVariableStorageRole::Algebraic,
        dae::VariableRole::Output => SolveVariableStorageRole::Output,
        dae::VariableRole::DiscreteReal => SolveVariableStorageRole::DiscreteReal,
        dae::VariableRole::DiscreteValue => SolveVariableStorageRole::DiscreteValue,
    }
}

fn solve_value_kind(
    variable: dae::VariableView<'_>,
) -> Result<SolveVariableValueKind, FmiLoweringError> {
    match variable.value_type().scalar_type() {
        dae::ScalarType::Real => Ok(SolveVariableValueKind::Real),
        dae::ScalarType::Boolean => Ok(SolveVariableValueKind::Boolean),
        kind @ (dae::ScalarType::Integer
        | dae::ScalarType::Enumeration
        | dae::ScalarType::String
        | dae::ScalarType::Record) => Err(FmiLoweringError::UnsupportedScalarType {
            variable: variable.name().to_string(),
            kind,
        }),
    }
}

const fn default_scalar_value(_kind: SolveVariableValueKind) -> f64 {
    0.0
}

const fn fmi_causality(causality: dae::VariableCausality) -> FmiCausality {
    match causality {
        dae::VariableCausality::Input => FmiCausality::Input,
        dae::VariableCausality::Output => FmiCausality::Output,
        dae::VariableCausality::Parameter => FmiCausality::Parameter,
        dae::VariableCausality::CalculatedParameter => FmiCausality::CalculatedParameter,
        dae::VariableCausality::Independent => FmiCausality::Independent,
        dae::VariableCausality::Local => FmiCausality::Local,
    }
}

fn fmi_variability(variable: dae::VariableView<'_>) -> FmiVariability {
    match variable.variability() {
        dae::ExpressionVariability::Constant => FmiVariability::Constant,
        dae::ExpressionVariability::Parameter if variable.is_tunable() => FmiVariability::Tunable,
        dae::ExpressionVariability::Parameter => FmiVariability::Fixed,
        dae::ExpressionVariability::Discrete => FmiVariability::Discrete,
        dae::ExpressionVariability::Continuous => FmiVariability::Continuous,
    }
}
