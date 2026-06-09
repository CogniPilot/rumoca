use rumoca_core::{Expression, Literal, OpBinary, PredefinedComponentType, Span};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::lower::{self, LowerError};

pub(crate) fn lower_event_action_conditions(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<Vec<Vec<solve::LinearOp>>, LowerError> {
    let conditions = dae_model
        .events
        .event_actions
        .iter()
        .map(|action| action.condition.clone())
        .collect::<Vec<_>>();
    lower::lower_expression_rows_from_expressions(&conditions, layout, &dae_model.symbols.functions)
}

pub(crate) fn lower_event_actions(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<Vec<solve::SolveEventAction>, LowerError> {
    dae_model
        .events
        .event_actions
        .iter()
        .map(|action| lower_event_action(action, dae_model, layout))
        .collect()
}

fn lower_event_action(
    action: &dae::DaeEventAction,
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<solve::SolveEventAction, LowerError> {
    let (kind, message) = match &action.kind {
        dae::DaeEventActionKind::Assert { message } => (
            solve::SolveEventActionKind::Assert,
            lower_event_action_message(message, action.span, dae_model, layout)?,
        ),
        dae::DaeEventActionKind::Terminate { message } => (
            solve::SolveEventActionKind::Terminate,
            lower_event_action_message(message, action.span, dae_model, layout)?,
        ),
    };
    Ok(solve::SolveEventAction {
        kind,
        message,
        span: action.span,
        origin: action.origin.clone(),
    })
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
