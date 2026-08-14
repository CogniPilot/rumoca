//! DAE + Solve lowering into the checked FMI component projection.

use rumoca_ir_dae as dae;
use rumoca_ir_fmi::{
    FmiCausality, FmiComponent, FmiComponentError, FmiVariability, FmiVariableInput,
};
use rumoca_ir_solve::{SolveProblem, SolveVariableStorageRole, SolveVariableValueKind};

#[derive(Debug, thiserror::Error)]
pub enum FmiLoweringError {
    #[error("unsupported FMI scalar type `{kind:?}` for `{variable}`")]
    UnsupportedScalarType {
        variable: String,
        kind: dae::ScalarType,
    },
    #[error("numeric FMI metadata for `{variable}` is invalid: {message}")]
    NumericMetadata { variable: String, message: String },
    #[error(transparent)]
    Component(#[from] FmiComponentError),
}

pub fn lower_to_fmi_component(
    dae: &dae::Dae,
    solve: SolveProblem,
) -> Result<FmiComponent, FmiLoweringError> {
    let inputs = dae.inspect(lower_variables)?;
    FmiComponent::construct(solve, inputs).map_err(Into::into)
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
    let start = match numeric_attribute(numeric, variable, variable.start())? {
        Some(values) => values,
        None => numeric_attribute(numeric, variable, variable.binding())?
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
        kind @ (dae::ScalarType::Integer
        | dae::ScalarType::Boolean
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
