//! DAE + Solve lowering into the checked FMI description of the problem.

use std::collections::HashMap;

use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::fmi::{
    FmiCausality, FmiComponent, FmiComponentError, FmiDeclaredCausality, FmiVariability,
    FmiVariableInput,
};
use rumoca_ir_solve::{SolveVariableStorageRole, SolveVariableValueKind};

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
    #[error("exact FMI initialization for `{variable}` is invalid: {error}")]
    ExactInitialization {
        variable: String,
        #[source]
        error: rumoca_eval_dae::ExactInitialError,
    },
    #[error(transparent)]
    SolveModel(#[from] SolveModelLoweringError),
    #[error(transparent)]
    Component(#[from] FmiComponentError),
}

impl FmiLoweringError {
    #[must_use]
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::UnsupportedScalarType { span, .. } | Self::NumericMetadata { span, .. } => {
                Some(*span)
            }
            Self::SolveModel(error) => error.source_span(),
            Self::ExactInitialization { error, .. } => Some(error.span),
            Self::Component(error) => error.span(),
        }
    }
}

/// Project one checked DAE into one checked executable FMI component.
///
/// Source declarations and explicit export-default ingredients remain distinct.
/// The FMI-only maximum-step-duration local of SPEC_0044 §8 is derived: the checked
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
    let inputs = lowered
        .prepared_dae()
        .inspect(|view| lower_variables(view, &lowered.input_initializations))?;
    let solve = lowered.into_model();
    FmiComponent::construct(solve, inputs).map_err(Into::into)
}

fn lower_variables(
    view: dae::DaeView<'_>,
    issued: &[crate::model_values::InputInitializationFact],
) -> Result<Vec<FmiVariableInput>, FmiLoweringError> {
    let mut numeric = rumoca_eval_dae::NumericEvaluator::new(view);
    view.variables()
        .map(|(id, variable)| {
            lower_variable(
                view,
                &mut numeric,
                variable,
                issued
                    .binary_search_by_key(&(id.index() as usize), |fact| fact.index)
                    .ok()
                    .map(|index| &issued[index]),
            )
        })
        .collect()
}

fn lower_variable<'dae>(
    view: dae::DaeView<'dae>,
    numeric: &mut rumoca_eval_dae::NumericEvaluator<'dae>,
    variable: dae::VariableView<'dae>,
    issued: Option<&crate::model_values::InputInitializationFact>,
) -> Result<FmiVariableInput, FmiLoweringError> {
    let role = solve_role(variable.role());
    let causality = fmi_causality(variable.causality());
    let value_kind = solve_value_kind(variable)?;
    let start = match issued {
        Some(fact) => fact.source.clone(),
        None => declaration_start(view, numeric, variable, value_kind)?,
    };
    Ok(FmiVariableInput {
        name: variable.name().to_string(),
        role,
        value_kind,
        dimensions: variable.value_type().dimensions().to_vec(),
        start,
        effective_input_default: issued.map(|fact| fact.effective.clone()),
        minimum: match issued {
            Some(fact) => fact.minimum.clone(),
            None => numeric_attribute(numeric, variable, variable.minimum())?,
        },
        maximum: match issued {
            Some(fact) => fact.maximum.clone(),
            None => numeric_attribute(numeric, variable, variable.maximum())?,
        },
        nominal: numeric_attribute(numeric, variable, variable.nominal())?,
        unit: variable.unit().map(str::to_owned),
        description: variable.description().map(str::to_owned),
        causality,
        declared_causality: declared_causality(variable.declared_causality())
            .filter(|declared| declared.exported() != causality),
        variability: fmi_variability(variable),
        tunable: variable.is_tunable(),
        evaluable: variable.is_evaluable(),
        declaration: variable.declaration().span(),
        text_start: text_start(view, variable),
        enumeration: variable.enumeration().cloned(),
    })
}

/// The per-scalar literal initialization of a `String` declaration. Inputs
/// select their binding before their start; other roles retain start-first
/// selection. A scalar literal broadcasts over an array declaration.
/// Any other expression leaves the text start absent, and a consumer that
/// needs one refuses the declaration rather than inventing a value.
fn text_start<'dae>(
    view: dae::DaeView<'dae>,
    variable: dae::VariableView<'dae>,
) -> Option<Vec<String>> {
    if variable.value_type().scalar_type() != dae::ScalarType::String {
        return None;
    }
    let expression = if variable.role() == dae::VariableRole::Input {
        rumoca_eval_dae::declaration_initial_expression(
            variable,
            dae::InputInitializationPolicy::HostDrivenStart,
        )
    } else {
        variable.start().or_else(|| variable.binding())
    }?;
    let mut values = Vec::new();
    collect_string_literals(view, expression, &mut values)?;
    let count = variable.scalar_count();
    dae::broadcast_scalar_values(&mut values, count);
    (values.len() == count).then_some(values)
}

fn collect_string_literals<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    values: &mut Vec<String>,
) -> Option<()> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::String(text)) => {
            values.push(text.clone());
            Some(())
        }
        dae::ExpressionOperation::Array(elements) => {
            let mut index = 0;
            while let Some(element) = elements.get(index) {
                collect_string_literals(view, element, values)?;
                index += 1;
            }
            Some(())
        }
        // A literal filled over the declaration broadcasts like a scalar
        // binding: `fill(text, n)` or an iterator-free comprehension.
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Fill,
            arguments,
        } => collect_string_literals(view, arguments.get(0)?, values),
        dae::ExpressionOperation::Comprehension { body, .. } => {
            let start = values.len();
            collect_string_literals(view, body, values)?;
            (values.len() == start + 1).then_some(())
        }
        _ => None,
    }
}

fn declaration_start<'dae>(
    view: dae::DaeView<'dae>,
    numeric: &mut rumoca_eval_dae::NumericEvaluator<'dae>,
    variable: dae::VariableView<'dae>,
    value_kind: SolveVariableValueKind,
) -> Result<rumoca_ir_solve::SolveDeclarationStart, FmiLoweringError> {
    use rumoca_ir_solve::SolveDeclarationStart as Start;
    let expression = rumoca_eval_dae::declaration_initial_expression(
        variable,
        dae::InputInitializationPolicy::HostDrivenStart,
    );
    let Some(expression) = expression else {
        return Ok(Start::Missing {
            value_kind,
            count: variable.scalar_count(),
        });
    };
    let error = |message: String, span| FmiLoweringError::NumericMetadata {
        variable: variable.name().to_string(),
        message,
        span,
    };
    match value_kind {
        SolveVariableValueKind::Real => numeric_attribute(numeric, variable, Some(expression))?
            .map(Start::Real)
            .ok_or_else(|| {
                error(
                    "Real source initialization is absent".into(),
                    variable.declaration().span(),
                )
            }),
        SolveVariableValueKind::Integer
        | SolveVariableValueKind::Boolean
        | SolveVariableValueKind::Enumeration => {
            let exact =
                rumoca_eval_dae::exact_initial_values(view, expression, variable.scalar_count())
                    .map_err(|error| FmiLoweringError::ExactInitialization {
                        variable: variable.name().to_string(),
                        error,
                    })?;
            let converted = match exact {
                rumoca_eval_dae::ExactDeclarationValues::Integer(values) => {
                    crate::numeric_values::from_exact(&values).map(Start::Integer)
                }
                rumoca_eval_dae::ExactDeclarationValues::Enumeration(values) => {
                    crate::numeric_values::from_exact(&values).map(Start::Enumeration)
                }
                rumoca_eval_dae::ExactDeclarationValues::Boolean(values) => {
                    crate::numeric_values::from_exact(&values).map(Start::Boolean)
                }
            };
            converted.map_err(|failure| error(failure.to_string(), variable.declaration().span()))
        }
        SolveVariableValueKind::String => Ok(Start::Missing {
            value_kind,
            count: variable.scalar_count(),
        }),
    }
}

fn numeric_attribute<'dae>(
    numeric: &mut rumoca_eval_dae::NumericEvaluator<'dae>,
    variable: dae::VariableView<'dae>,
    expression: Option<dae::ExprId<'dae>>,
) -> Result<Option<rumoca_ir_solve::SolveInitialValues>, FmiLoweringError> {
    let Some(expression) = expression else {
        return Ok(None);
    };
    if variable.scalar_count() == 0 {
        return Ok(Some(rumoca_ir_solve::SolveInitialValues::default()));
    }
    if !matches!(
        variable.value_type().scalar_type(),
        dae::ScalarType::Real
            | dae::ScalarType::Integer
            | dae::ScalarType::Boolean
            | dae::ScalarType::Enumeration
    ) {
        return Ok(None);
    }
    let evaluated = if matches!(
        variable.value_type().scalar_type(),
        dae::ScalarType::Integer | dae::ScalarType::Enumeration
    ) {
        numeric.integer_attribute_values(expression)
    } else {
        numeric.expression_values(expression)
    };
    let mut values = evaluated.map_err(|error| FmiLoweringError::NumericMetadata {
        variable: variable.name().to_string(),
        message: error.to_string(),
        span: error.span(),
    })?;
    values.broadcast(variable.scalar_count());
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
    crate::numeric_values::from_source(&values)
        .map(Some)
        .map_err(|error| FmiLoweringError::NumericMetadata {
            variable: variable.name().to_string(),
            message: error.to_string(),
            span: variable.declaration().span(),
        })
}

/// Evaluate one numeric declaration attribute through the phase-owned DAE
/// evaluator and apply the declaration's scalar-broadcast rule.
pub fn numeric_attribute_values<'dae>(
    view: dae::DaeView<'dae>,
    variable: dae::VariableView<'dae>,
    expression: Option<dae::ExprId<'dae>>,
) -> Result<Option<rumoca_ir_solve::SolveInitialValues>, FmiLoweringError> {
    numeric_attribute(
        &mut rumoca_eval_dae::NumericEvaluator::new(view),
        variable,
        expression,
    )
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
        dae::ScalarType::Integer => Ok(SolveVariableValueKind::Integer),
        dae::ScalarType::Boolean => Ok(SolveVariableValueKind::Boolean),
        dae::ScalarType::Enumeration => Ok(SolveVariableValueKind::Enumeration),
        dae::ScalarType::String => Ok(SolveVariableValueKind::String),
        kind @ dae::ScalarType::Record => Err(FmiLoweringError::UnsupportedScalarType {
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

const fn declared_causality(causality: dae::DeclaredCausality) -> Option<FmiDeclaredCausality> {
    match causality {
        dae::DeclaredCausality::None => None,
        dae::DeclaredCausality::Input => Some(FmiDeclaredCausality::Input),
        dae::DeclaredCausality::Output => Some(FmiDeclaredCausality::Output),
    }
}

#[cfg(test)]
mod declaration_start_tests;

fn fmi_variability(variable: dae::VariableView<'_>) -> FmiVariability {
    match variable.variability() {
        dae::ExpressionVariability::Constant => FmiVariability::Constant,
        dae::ExpressionVariability::Parameter if variable.is_tunable() => FmiVariability::Tunable,
        dae::ExpressionVariability::Parameter => FmiVariability::Fixed,
        dae::ExpressionVariability::Discrete => FmiVariability::Discrete,
        dae::ExpressionVariability::Continuous => FmiVariability::Continuous,
    }
}
