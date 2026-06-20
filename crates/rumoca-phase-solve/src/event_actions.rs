use rumoca_core::{Expression, Literal, OpBinary, PredefinedComponentType, Span};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::lower::{self, LowerError};

pub(crate) fn lower_event_action_conditions(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<Vec<Vec<solve::LinearOp>>, LowerError> {
    let span = event_action_context_span(dae_model);
    let mut conditions = event_vec_with_capacity(
        dae_model.events.event_actions.len(),
        "event action condition count",
        span,
    )?;
    for action in &dae_model.events.event_actions {
        conditions.push(action.condition.clone());
    }
    lower::lower_expression_rows_from_expressions(&conditions, layout, &dae_model.symbols.functions)
}

pub(crate) fn lower_event_actions(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<Vec<solve::SolveEventAction>, LowerError> {
    let span = event_action_context_span(dae_model);
    let mut actions = event_vec_with_capacity(
        dae_model.events.event_actions.len(),
        "event action count",
        span,
    )?;
    for action in &dae_model.events.event_actions {
        actions.push(lower_event_action(action, dae_model, layout)?);
    }
    Ok(actions)
}

fn event_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: Option<Span>,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    values.try_reserve_exact(capacity).map_err(|_| {
        event_action_contract_error(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

fn event_action_contract_error(reason: String, span: Option<Span>) -> LowerError {
    match span {
        Some(span) if !span.is_dummy() => LowerError::ContractViolation { reason, span },
        Some(_) | None => LowerError::UnspannedContractViolation { reason },
    }
}

fn event_action_context_span(dae_model: &dae::Dae) -> Option<Span> {
    dae_model
        .events
        .event_actions
        .iter()
        .find_map(|action| (!action.span.is_dummy()).then_some(action.span))
}

fn lower_event_action(
    action: &dae::DaeEventAction,
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<solve::SolveEventAction, LowerError> {
    let span = required_event_action_span(action)?;
    let (kind, message) = match &action.kind {
        dae::DaeEventActionKind::Assert { message } => (
            solve::SolveEventActionKind::Assert,
            lower_event_action_message(message, span, dae_model, layout)?,
        ),
        dae::DaeEventActionKind::Terminate { message } => (
            solve::SolveEventActionKind::Terminate,
            lower_event_action_message(message, span, dae_model, layout)?,
        ),
    };
    Ok(solve::SolveEventAction {
        kind,
        message,
        span,
        origin: action.origin.clone(),
    })
}

fn required_event_action_span(action: &dae::DaeEventAction) -> Result<Span, LowerError> {
    if action.span.is_dummy() {
        return Err(LowerError::UnspannedContractViolation {
            reason: format!(
                "event action `{}` is missing source span metadata",
                action.origin
            ),
        });
    }
    Ok(action.span)
}

fn lower_event_action_message(
    message: &Expression,
    span: Span,
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<solve::SolveEventMessage, LowerError> {
    let mut parts = Vec::new();
    lower_event_action_message_parts(message, span, dae_model, layout, &mut parts)?;
    Ok(solve::SolveEventMessage { parts })
}

fn lower_event_action_message_parts(
    message: &Expression,
    span: Span,
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    parts: &mut Vec<solve::SolveEventMessagePart>,
) -> Result<(), LowerError> {
    match message {
        Expression::Literal {
            value: Literal::String(value),
            ..
        } => {
            parts.push(solve::SolveEventMessagePart::Text(value.clone()));
            Ok(())
        }
        Expression::Binary {
            op: OpBinary::Add,
            lhs,
            rhs,
            ..
        } => {
            lower_event_action_message_parts(lhs, span, dae_model, layout, parts)?;
            lower_event_action_message_parts(rhs, span, dae_model, layout, parts)
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } if rumoca_core::predefined_component_type(name.last_segment())
            == Some(PredefinedComponentType::String) =>
        {
            lower_string_conversion_message_part(args, span, dae_model, layout, parts)
        }
        _ => Err(LowerError::UnsupportedAt {
            reason: "unsupported assert/terminate message expression for Solve IR".to_string(),
            contexts: Vec::new(),
            span,
        }),
    }
}

fn lower_string_conversion_message_part(
    args: &[Expression],
    span: Span,
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    parts: &mut Vec<solve::SolveEventMessagePart>,
) -> Result<(), LowerError> {
    let [arg] = args else {
        return Err(LowerError::UnsupportedAt {
            reason: format!(
                "String() in assert/terminate message requires exactly one argument, got {}",
                args.len()
            ),
            contexts: Vec::new(),
            span,
        });
    };
    let rows = lower::lower_expression_rows_from_expressions(
        std::slice::from_ref(arg),
        layout,
        &dae_model.symbols.functions,
    )?;
    let [row] = rows.as_slice() else {
        return Err(LowerError::ContractViolation {
            reason: "String() message expression did not lower to one scalar row".to_string(),
            span,
        });
    };
    parts.push(solve::SolveEventMessagePart::Number(row.clone()));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn string_literal(value: &str, span: Span) -> Expression {
        Expression::Literal {
            value: Literal::String(value.to_string()),
            span,
        }
    }

    #[test]
    fn lower_event_action_rejects_missing_source_span() {
        let action = dae::DaeEventAction {
            condition: Expression::Literal {
                value: Literal::Boolean(true),
                span: Span::DUMMY,
            },
            kind: dae::DaeEventActionKind::Assert {
                message: string_literal("failed", Span::DUMMY),
            },
            span: Span::DUMMY,
            origin: "assert action".to_string(),
        };
        let err = lower_event_action(&action, &dae::Dae::default(), &solve::VarLayout::default())
            .expect_err("unspanned event actions should fail before Solve IR lowering");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason().contains("event action `assert action`"),
            "error should name the unspanned event action: {err}"
        );
    }

    #[test]
    fn event_vec_with_capacity_does_not_fabricate_dummy_span() {
        let err = event_vec_with_capacity::<u8>(
            usize::MAX,
            "event action test vector",
            Some(Span::DUMMY),
        )
        .expect_err("oversized unspanned event action vector should fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("event action test vector capacity exceeds host memory limits"),
            "error should explain event action capacity overflow: {err}"
        );
    }
}
