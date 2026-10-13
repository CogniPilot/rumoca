//! Cross-boundary binding of exact source-issued input defaults.

use super::*;
use solve::fmi::{FmiInputDefaultInput, FmiInputDefaultOrigin, FmiInputDefaultOriginRun};

pub(crate) struct InputInitializationFact {
    pub(crate) index: usize,
    pub(crate) source: solve::SolveDeclarationStart,
    pub(crate) effective: FmiInputDefaultInput,
    pub(crate) minimum: Option<solve::SolveInitialValues>,
    pub(crate) maximum: Option<solve::SolveInitialValues>,
}

pub(super) fn convert(
    id: dae::VariableId<'_>,
    variable: dae::VariableView<'_>,
    issued: &rumoca_eval_dae::InputInitialization,
    kind: solve::SolveVariableValueKind,
) -> Result<InputInitializationFact, SolveModelLoweringError> {
    let span = variable.declaration().span();
    let convert = |values| {
        crate::numeric_values::from_typed_source(values, variable.value_type().scalar_type(), span)
            .map_err(|e| runtime_error(e, span))
    };
    let source = issued.source().map(convert).transpose()?.unwrap_or(
        solve::SolveDeclarationStart::Missing {
            value_kind: kind,
            count: variable.scalar_count(),
        },
    );
    let origins = issued
        .origins()
        .iter()
        .map(|run| FmiInputDefaultOriginRun {
            start: run.start(),
            count: run.count(),
            span: run.span(),
            origin: match run.origin() {
                rumoca_eval_dae::InputOrigin::Binding => FmiInputDefaultOrigin::Binding,
                rumoca_eval_dae::InputOrigin::Start => FmiInputDefaultOrigin::Start,
                rumoca_eval_dae::InputOrigin::Supplied => FmiInputDefaultOrigin::Supplied,
                rumoca_eval_dae::InputOrigin::MlsFallback => FmiInputDefaultOrigin::MlsFallback,
            },
        })
        .collect();
    let attribute = |values: &rumoca_eval_dae::SourceValues, attribute_span| {
        crate::numeric_values::from_source(
            &values
                .numerical_attribute_values(variable.value_type().scalar_type(), attribute_span)
                .map_err(evaluation_error)?,
        )
        .map_err(|e| runtime_error(e.to_string(), attribute_span))
    };
    Ok(InputInitializationFact {
        index: id.index() as usize,
        source,
        minimum: issued
            .fallback()
            .and_then(|b| b.minimum())
            .map(|v| {
                attribute(
                    v,
                    issued
                        .fallback()
                        .and_then(|b| b.minimum_span())
                        .unwrap_or(span),
                )
            })
            .transpose()?,
        maximum: issued
            .fallback()
            .and_then(|b| b.maximum())
            .map(|v| {
                attribute(
                    v,
                    issued
                        .fallback()
                        .and_then(|b| b.maximum_span())
                        .unwrap_or(span),
                )
            })
            .transpose()?,
        effective: FmiInputDefaultInput {
            declaration: id.index() as usize,
            values: convert(issued.effective())?,
            origins,
            fallback_minimum: issued
                .fallback()
                .and_then(|b| b.minimum())
                .map(convert)
                .transpose()?,
            fallback_maximum: issued
                .fallback()
                .and_then(|b| b.maximum())
                .map(convert)
                .transpose()?,
            fallback_minimum_span: issued.fallback().and_then(|b| b.minimum_span()),
            fallback_maximum_span: issued.fallback().and_then(|b| b.maximum_span()),
        },
    })
}
