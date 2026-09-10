//! Solve lowering of event-action messages (`assert`/`terminate` text).
//!
//! Message lowering is string-shaped work with no bearing on discrete row
//! ownership or on the clock partition, so it lives beside the event lowering
//! rather than inside it.

use super::*;

pub(super) struct MessageActionContext<'borrow, 'dae> {
    pub(super) view: dae::DaeView<'dae>,
    pub(super) layout: &'borrow LoweredLayout<'dae>,
    pub(super) clocks: &'borrow LoweredClocks<'dae>,
}

pub(super) fn push_message_action<'dae>(
    context: MessageActionContext<'_, 'dae>,
    action: dae::EventActionView<'dae>,
    message: dae::ExprId<'dae>,
    kind: solve::SolveEventActionKind,
    actions: &mut Vec<solve::SolveEventAction>,
    conditions: &mut ScalarRows,
) -> Result<(), LowerError> {
    let MessageActionContext {
        view,
        layout,
        clocks,
    } = context;
    let span = action.provenance().span();
    let message = lower_message(view, layout, message)?;
    let compiler = ScalarCompiler::new(view, layout, None);
    let clock = condition_clock_owner(view, action.guard());
    let program = match clock {
        Some(clock) => compiler.clocked_action_condition_program(clock, action.guard(), span)?,
        None => {
            let trigger_memory = condition_memory(layout, action.trigger(), span)?;
            compiler.edge_condition_program(
                action.trigger(),
                action.guard(),
                trigger_memory,
                span,
            )?
        }
    };
    conditions.push(program, span, actions.len());
    actions.push(solve::SolveEventAction {
        kind,
        message,
        span,
        origin: action.provenance().origin().to_string(),
        clock_owner: clock.map(|clock| clocks.clock(clock)).transpose()?,
    });
    Ok(())
}

fn lower_message<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    message: dae::ExprId<'dae>,
) -> Result<solve::SolveEventMessage, LowerError> {
    let mut parts = Vec::new();
    lower_message_parts(view, layout, message, &mut parts)?;
    Ok(solve::SolveEventMessage { parts })
}

fn lower_message_parts<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    message: dae::ExprId<'dae>,
    parts: &mut Vec<solve::SolveEventMessagePart>,
) -> Result<(), LowerError> {
    let expression = view
        .expression(message)
        .expect("checked event message expression resolves");
    match expression.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::String(message)) => {
            parts.push(solve::SolveEventMessagePart::Text(message.clone()));
            Ok(())
        }
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Add,
            lhs,
            rhs,
        } if expression.value_type().scalar_type() == dae::ScalarType::String => {
            lower_message_parts(view, layout, lhs, parts)?;
            lower_message_parts(view, layout, rhs, parts)
        }
        dae::ExpressionOperation::StringConversion { value, format, .. } => {
            let source = match view
                .expression(value)
                .expect("checked String conversion value resolves")
                .value_type()
                .scalar_type()
            {
                dae::ScalarType::Real => solve::SolveStringConversionSource::Real,
                dae::ScalarType::Integer => solve::SolveStringConversionSource::Integer,
                dae::ScalarType::Boolean => solve::SolveStringConversionSource::Boolean,
                dae::ScalarType::Enumeration
                | dae::ScalarType::String
                | dae::ScalarType::Record => {
                    unreachable!("checked String conversion has a supported scalar source")
                }
            };
            let value = ScalarCompiler::new(view, layout, None).program(value, 0)?;
            let format = lower_message_format(view, layout, format)?;
            parts.push(solve::SolveEventMessagePart::Conversion {
                value,
                source,
                format,
            });
            Ok(())
        }
        _ => Err(LowerError::unsupported(
            "Solve event messages require String literals, concatenation, or checked String conversions",
            expression.provenance().span(),
        )),
    }
}

fn lower_message_format<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    format: dae::StringConversionFormatView<'dae>,
) -> Result<solve::SolveStringConversionFormat, LowerError> {
    Ok(match format {
        dae::StringConversionFormatView::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } => solve::SolveStringConversionFormat::Options {
            minimum_length: lower_message_option(view, layout, minimum_length)?,
            left_justified: lower_message_option(view, layout, left_justified)?,
            significant_digits: lower_message_option(view, layout, significant_digits)?,
        },
        dae::StringConversionFormatView::Format { value } => {
            let expression = view
                .expression(value)
                .expect("checked String format expression resolves");
            return Err(LowerError::unsupported(
                "explicit String format is not representable in checked Solve event messages",
                expression.provenance().span(),
            ));
        }
    })
}

fn lower_message_option<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    value: Option<dae::ExprId<'dae>>,
) -> Result<Option<Vec<solve::LinearOp>>, LowerError> {
    value
        .map(|value| ScalarCompiler::new(view, layout, None).program(value, 0))
        .transpose()
}
