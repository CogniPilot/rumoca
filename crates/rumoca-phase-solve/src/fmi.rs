//! DAE + Solve lowering into the checked FMI description of the problem.

use std::collections::HashMap;

use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::fmi::{
    FmiCausality, FmiComponent, FmiComponentError, FmiVariability, FmiVariableInput,
};
use rumoca_ir_solve::{SolveVariableStorageRole, SolveVariableValueKind};
use serde::{Deserialize, Serialize};

use crate::{SolveModelLoweringError, lower_solve_model};

#[derive(Debug, thiserror::Error)]
pub enum FmiLoweringError {
    #[error("unsupported FMI scalar type `{kind:?}` for `{variable}`")]
    UnsupportedScalarType {
        variable: String,
        kind: dae::ScalarType,
        span: Span,
    },
    #[error("numeric FMI metadata for `{variable}` is invalid: {message}")]
    NumericMetadata {
        variable: String,
        message: String,
        span: Span,
    },
    #[error(transparent)]
    SolveModel(#[from] SolveModelLoweringError),
    #[error(transparent)]
    Component(#[from] FmiComponentError),
    #[error("FMI component wire is invalid: {0}")]
    Wire(String),
}

impl FmiLoweringError {
    #[must_use]
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::UnsupportedScalarType { span, .. } | Self::NumericMetadata { span, .. } => {
                Some(*span)
            }
            Self::SolveModel(error) => error.source_span(),
            Self::Component(error) => error.span(),
            Self::Wire(_) => None,
        }
    }
}

pub const FMI_COMPONENT_SCHEMA_VERSION: u16 = 1;

/// Borrowed, canonical construction inputs for one correlated FMI component.
#[derive(Serialize)]
pub struct FmiComponentWireRef<'model> {
    schema_version: u16,
    solve_model: crate::SolveModelWireRef<'model>,
    variables: Vec<FmiVariableInput>,
}

/// Encode the still-correlated phase result without exposing either half as
/// an independently selectable runtime input.
pub fn fmi_component_wire<'wire>(
    lowered: &'wire crate::LoweredSolveModel<'_>,
) -> Result<FmiComponentWireRef<'wire>, FmiLoweringError> {
    let variables = lowered.prepared_dae().inspect(lower_variables)?;
    let solve_model = crate::solve_model_wire(lowered.model())
        .map_err(|error| FmiLoweringError::Wire(error.to_string()))?;
    Ok(FmiComponentWireRef {
        schema_version: FMI_COMPONENT_SCHEMA_VERSION,
        solve_model,
        variables,
    })
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FmiComponentWire {
    schema_version: u16,
    #[serde(deserialize_with = "crate::deserialize_solve_model")]
    solve_model: rumoca_ir_solve::SolveModel,
    variables: Vec<FmiVariableInput>,
}

/// Replay the canonical FMI wire through the same checked component
/// construction used by fresh lowering.
pub fn deserialize_fmi_component<'de, D>(deserializer: D) -> Result<FmiComponent, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let wire = FmiComponentWire::deserialize(deserializer)?;
    if wire.schema_version != FMI_COMPONENT_SCHEMA_VERSION {
        return Err(serde::de::Error::custom(format!(
            "unsupported FMI component schema {}; expected {}",
            wire.schema_version, FMI_COMPONENT_SCHEMA_VERSION
        )));
    }
    FmiComponent::construct(wire.solve_model, wire.variables).map_err(serde::de::Error::custom)
}

/// Project one checked DAE into one checked executable FMI component.
///
/// Every input here is a fact the Modelica declaration owns. The FMI-only
/// maximum-step-duration local of SPEC_0044 §8 is not one of them: the checked
/// component derives it from the delay partition of the kernel below, so this
/// lowering neither names it nor decides whether it exists.
pub fn lower_to_fmi_component(
    dae: &dae::Dae,
    overrides: &HashMap<String, f64>,
) -> Result<FmiComponent, FmiLoweringError> {
    let lowered = lower_solve_model(dae, overrides, |_| {})?;
    finish_fmi_component(lowered)
}

/// Consume one still-correlated complete Solve lowering into its checked FMI
/// component.
///
/// The prepared DAE and executable root cannot be supplied independently: the
/// only input is the phase-owned aggregate returned by [`lower_solve_model`].
pub fn finish_fmi_component(
    lowered: crate::LoweredSolveModel<'_>,
) -> Result<FmiComponent, FmiLoweringError> {
    let inputs = lowered.prepared_dae().inspect(lower_variables)?;
    let solve = lowered.into_model();
    FmiComponent::construct(solve, inputs).map_err(Into::into)
}

fn lower_variables(view: dae::DaeView<'_>) -> Result<Vec<FmiVariableInput>, FmiLoweringError> {
    let mut numeric = rumoca_eval_dae::NumericEvaluator::new(view);
    view.variables()
        .map(|(_, variable)| lower_variable(&mut numeric, variable))
        .collect()
}

fn lower_variable<'dae>(
    numeric: &mut rumoca_eval_dae::NumericEvaluator<'dae>,
    variable: dae::VariableView<'dae>,
) -> Result<FmiVariableInput, FmiLoweringError> {
    let scalar_count = variable.scalar_count();
    let role = solve_role(variable.role());
    let value_kind = solve_value_kind(variable)?;
    let start = match numeric_attribute(numeric, variable, variable.start())? {
        Some(values) => values,
        None => numeric_attribute(numeric, variable, variable.binding())?
            .unwrap_or_else(|| vec![0.0; scalar_count]),
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
    numeric: &mut rumoca_eval_dae::NumericEvaluator<'dae>,
    variable: dae::VariableView<'dae>,
    expression: Option<dae::ExprId<'dae>>,
) -> Result<Option<Vec<f64>>, FmiLoweringError> {
    let Some(expression) = expression else {
        return Ok(None);
    };
    if !matches!(
        variable.value_type().scalar_type(),
        dae::ScalarType::Real | dae::ScalarType::Integer
    ) {
        return Ok(None);
    }
    let mut values =
        numeric
            .expression(expression)
            .map_err(|error| FmiLoweringError::NumericMetadata {
                variable: variable.name().to_string(),
                message: error.to_string(),
                span: error.span(),
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
            span: variable.declaration().span(),
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
        kind @ (dae::ScalarType::Integer
        | dae::ScalarType::Boolean
        | dae::ScalarType::Enumeration
        | dae::ScalarType::String
        | dae::ScalarType::Record) => Err(FmiLoweringError::UnsupportedScalarType {
            variable: variable.name().to_string(),
            kind,
            span: variable.declaration().span(),
        }),
    }
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
