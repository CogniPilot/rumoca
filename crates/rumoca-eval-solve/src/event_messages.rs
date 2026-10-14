//! Shared event-message formatting with an explicit numeric execution owner.

use crate::{EvalSolveError, LinearOp, RowEvalContext, eval_row_with_context, invalid_row};
use rumoca_ir_solve::{
    SolveEventAction, SolveEventMessagePart, SolveStringConversionFormat,
    SolveStringConversionSource,
};

/// Numeric field of a message conversion, independent of its target backend.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum EventMessageProgramKind {
    Value,
    MinimumLength,
    LeftJustified,
    SignificantDigits,
}

/// Exact source action/conversion and numerical point requested by the formatter.
#[derive(Clone, Copy)]
pub struct EventMessageProgramInput<'a> {
    pub action: &'a SolveEventAction,
    pub part: usize,
    pub kind: EventMessageProgramKind,
    pub y: &'a [f64],
    pub p: &'a [f64],
    pub t: f64,
}

/// An execution owner installed only after its complete message inventory is prepared.
pub type EventMessageProgramExecution =
    dyn for<'a> Fn(EventMessageProgramInput<'a>) -> Result<f64, EvalSolveError>;

/// The message of one event action, rendered at one point.
pub fn eval_event_action_message(
    action: &rumoca_ir_solve::SolveEventAction,
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
) -> Result<String, EvalSolveError> {
    if action.assertion_projection.is_some() {
        return context
            .event_observations
            .and_then(|observed| observed.message_for_action(action))
            .ok_or_else(|| invalid_row("failed action has no exact invocation observation"));
    }
    let mut message = String::new();
    let eval = MessageEvalContext {
        y,
        p,
        t,
        row: context,
        span: action.span,
        action,
        part: 0,
    };
    for (part_index, part) in action.message.parts.iter().enumerate() {
        match part {
            SolveEventMessagePart::Text(text) => {
                append_event_message_part(&mut message, text, action.span)?;
            }
            SolveEventMessagePart::Conversion {
                value,
                source,
                format,
            } => {
                let rendered = eval_event_message_conversion(
                    value,
                    *source,
                    format,
                    MessageEvalContext {
                        part: part_index,
                        ..eval
                    },
                )?;
                append_event_message_part(&mut message, &rendered, action.span)?;
            }
        }
    }
    Ok(message)
}

#[derive(Clone, Copy)]
struct MessageEvalContext<'a> {
    y: &'a [f64],
    p: &'a [f64],
    t: f64,
    row: RowEvalContext<'a>,
    span: rumoca_core::Span,
    action: &'a SolveEventAction,
    part: usize,
}

pub(crate) const MAX_EVENT_MESSAGE_BYTES: usize = 1_048_576;
pub(crate) const MAX_STRING_SIGNIFICANT_DIGITS: i64 = 1_024;

fn eval_event_message_conversion(
    row: &[LinearOp],
    source: SolveStringConversionSource,
    format: &SolveStringConversionFormat,
    eval: MessageEvalContext<'_>,
) -> Result<String, EvalSolveError> {
    let value = eval.numeric(row, EventMessageProgramKind::Value)?;
    let SolveStringConversionFormat::Options {
        minimum_length,
        left_justified,
        significant_digits,
    } = format;
    let minimum_length = eval_message_integer_option(
        minimum_length.as_deref(),
        0,
        "minimumLength",
        EventMessageProgramKind::MinimumLength,
        eval,
    )?;
    let left_justified =
        eval_message_boolean_option(left_justified.as_deref(), true, "leftJustified", eval)?;
    let significant_digits = eval_message_integer_option(
        significant_digits.as_deref(),
        6,
        "significantDigits",
        EventMessageProgramKind::SignificantDigits,
        eval,
    )?;
    if !(1..=MAX_STRING_SIGNIFICANT_DIGITS).contains(&significant_digits) {
        return Err(invalid_message_option(
            format!("significantDigits must be in 1..={MAX_STRING_SIGNIFICANT_DIGITS}"),
            eval.span,
        ));
    }
    let converted = match source {
        SolveStringConversionSource::Real => {
            format_significant_digits(value, significant_digits as usize)
        }
        SolveStringConversionSource::Integer => {
            if !value.is_finite() || value.fract() != 0.0 {
                return Err(invalid_message_option(
                    "Integer String conversion received a non-integer runtime value",
                    eval.span,
                ));
            }
            format!("{value:.0}")
        }
        SolveStringConversionSource::Boolean => {
            if value == 0.0 {
                "false".to_string()
            } else if value == 1.0 {
                "true".to_string()
            } else {
                return Err(invalid_message_option(
                    "Boolean String conversion received a value outside {0,1}",
                    eval.span,
                ));
            }
        }
    };
    pad_event_message_conversion(converted, minimum_length, left_justified, eval.span)
}

pub(crate) fn pad_event_message_conversion(
    converted: String,
    minimum_length: i64,
    left_justified: bool,
    span: rumoca_core::Span,
) -> Result<String, EvalSolveError> {
    let width = usize::try_from(minimum_length)
        .map_err(|_| invalid_message_option("minimumLength must be nonnegative", span))?;
    if width > MAX_EVENT_MESSAGE_BYTES {
        return Err(invalid_message_option(
            format!("minimumLength exceeds the {MAX_EVENT_MESSAGE_BYTES}-byte message limit"),
            span,
        ));
    }
    if converted.len() >= width {
        return Ok(converted);
    }
    let padding_len = width - converted.len();
    let mut padded = String::new();
    padded
        .try_reserve_exact(width)
        .map_err(|_| invalid_message_option("event message allocation failed", span))?;
    if left_justified {
        padded.push_str(&converted);
        padded.extend(std::iter::repeat_n(' ', padding_len));
    } else {
        padded.extend(std::iter::repeat_n(' ', padding_len));
        padded.push_str(&converted);
    }
    Ok(padded)
}

fn eval_message_integer_option(
    row: Option<&[LinearOp]>,
    default: i64,
    name: &'static str,
    kind: EventMessageProgramKind,
    eval: MessageEvalContext<'_>,
) -> Result<i64, EvalSolveError> {
    let Some(row) = row else {
        return Ok(default);
    };
    let value = eval.numeric(row, kind)?;
    if !value.is_finite()
        || value.fract() != 0.0
        || value < i64::MIN as f64
        || value >= 9_223_372_036_854_775_808.0
    {
        return Err(invalid_message_option(
            format!("{name} must evaluate to an Integer"),
            eval.span,
        ));
    }
    Ok(value as i64)
}

fn eval_message_boolean_option(
    row: Option<&[LinearOp]>,
    default: bool,
    name: &'static str,
    eval: MessageEvalContext<'_>,
) -> Result<bool, EvalSolveError> {
    let Some(row) = row else {
        return Ok(default);
    };
    match eval.numeric(row, EventMessageProgramKind::LeftJustified)? {
        0.0 => Ok(false),
        1.0 => Ok(true),
        _ => Err(invalid_message_option(
            format!("{name} must evaluate to a Boolean"),
            eval.span,
        )),
    }
}

pub(crate) fn append_event_message_part(
    message: &mut String,
    part: &str,
    span: rumoca_core::Span,
) -> Result<(), EvalSolveError> {
    let total = message
        .len()
        .checked_add(part.len())
        .filter(|total| *total <= MAX_EVENT_MESSAGE_BYTES)
        .ok_or_else(|| {
            invalid_message_option(
                format!("event message exceeds the {MAX_EVENT_MESSAGE_BYTES}-byte limit"),
                span,
            )
        })?;
    message
        .try_reserve_exact(total - message.len())
        .map_err(|_| invalid_message_option("event message allocation failed", span))?;
    message.push_str(part);
    Ok(())
}

pub(crate) fn invalid_message_option(
    message: impl Into<String>,
    span: rumoca_core::Span,
) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: message.into(),
        span: Some(span),
    }
}

pub(crate) fn format_significant_digits(value: f64, digits: usize) -> String {
    if !value.is_finite() || value == 0.0 {
        return value.to_string();
    }
    let exponent = value.abs().log10().floor() as i32;
    if exponent < -4 || exponent >= digits as i32 {
        let mut formatted = format!("{:.*e}", digits.saturating_sub(1), value);
        trim_fraction_zeros(&mut formatted, 'e');
        return formatted;
    }
    let fractional = (digits as i32 - exponent - 1).max(0) as usize;
    let mut formatted = format!("{value:.fractional$}");
    trim_fraction_zeros(&mut formatted, '\0');
    formatted
}

fn trim_fraction_zeros(value: &mut String, exponent_marker: char) {
    let exponent = (exponent_marker != '\0')
        .then(|| value.find(exponent_marker))
        .flatten()
        .unwrap_or(value.len());
    if !value[..exponent].contains('.') {
        return;
    }
    let mut end = exponent;
    while end > 0 && value.as_bytes()[end - 1] == b'0' {
        end -= 1;
    }
    if end > 0 && value.as_bytes()[end - 1] == b'.' {
        end -= 1;
    }
    if end != exponent {
        value.replace_range(end..exponent, "");
    }
}

impl MessageEvalContext<'_> {
    fn numeric(
        self,
        row: &[LinearOp],
        kind: EventMessageProgramKind,
    ) -> Result<f64, EvalSolveError> {
        match self.row.message_program_execution {
            Some(execute) => execute(EventMessageProgramInput {
                action: self.action,
                part: self.part,
                kind,
                y: self.y,
                p: self.p,
                t: self.t,
            }),
            None => eval_row_with_context(row, self.y, self.p, self.t, self.row),
        }
    }
}
