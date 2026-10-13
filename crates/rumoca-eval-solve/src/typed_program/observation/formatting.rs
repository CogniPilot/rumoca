//! Render only the initialized scalar captures of a checked observation.

use super::*;
use rumoca_ir_solve::{
    AssertionCaptureSelector, CheckedAssertionAction, CheckedAssertionConversion,
    CheckedAssertionMessagePart, SolveStringConversionSource,
};

pub(super) fn render_message(
    action: &CheckedAssertionAction<'_>,
    observation: &assertions::AssertionObservation,
) -> Result<String, EvalSolveError> {
    let mut message = String::new();
    for part in action.message() {
        let rendered;
        let text: &str = match part {
            CheckedAssertionMessagePart::Text(text) => text,
            CheckedAssertionMessagePart::Conversion(conversion) => {
                rendered = render_conversion(conversion, observation)?;
                &rendered
            }
        };
        crate::append_event_message_part(&mut message, text, observation.provenance)?;
    }
    Ok(message)
}

fn selected(
    selector: AssertionCaptureSelector,
    observation: &assertions::AssertionObservation,
) -> Result<SolveValueKind, EvalSolveError> {
    let value = observation
        .captures
        .iter()
        .find_map(|(index, value)| (*index == selector.output()).then_some(value))
        .ok_or_else(|| binding_error("message capture was not initialized by this invocation"))?;
    let [scalar] = value.elements() else {
        return Err(binding_error("assertion message capture is not a scalar"));
    };
    Ok(*scalar)
}

fn integer(value: SolveValueKind, span: Span) -> Result<i64, EvalSolveError> {
    if let SolveValueKind::Integer(value) = value {
        return Ok(value);
    }
    let value = crate::typed_kind_to_scalar(value);
    if !value.is_finite()
        || value.fract() != 0.0
        || value < i64::MIN as f64
        || value >= 9_223_372_036_854_775_808.0
    {
        return Err(crate::invalid_message_option(
            "message option must evaluate to an Integer",
            span,
        ));
    }
    Ok(value as i64)
}

fn boolean(value: SolveValueKind, span: Span) -> Result<bool, EvalSolveError> {
    match crate::typed_kind_to_scalar(value) {
        0.0 => Ok(false),
        1.0 => Ok(true),
        _ => Err(crate::invalid_message_option(
            "message option must evaluate to a Boolean",
            span,
        )),
    }
}

fn integer_option(
    selector: Option<AssertionCaptureSelector>,
    default: i64,
    observation: &assertions::AssertionObservation,
) -> Result<i64, EvalSolveError> {
    match selector {
        Some(selector) => integer(selected(selector, observation)?, observation.provenance),
        None => Ok(default),
    }
}

fn render_conversion(
    conversion: &CheckedAssertionConversion,
    observation: &assertions::AssertionObservation,
) -> Result<String, EvalSolveError> {
    let span = observation.provenance;
    let value = selected(conversion.value, observation)?;
    let width = integer_option(conversion.minimum_length, 0, observation)?;
    let digits = integer_option(conversion.significant_digits, 6, observation)?;
    let left = match conversion.left_justified {
        Some(selector) => boolean(selected(selector, observation)?, span)?,
        None => true,
    };
    if !(1..=crate::MAX_STRING_SIGNIFICANT_DIGITS).contains(&digits) {
        return Err(crate::invalid_message_option(
            "significantDigits is outside the checked message range",
            span,
        ));
    }
    let rendered = match conversion.source {
        SolveStringConversionSource::Real => {
            crate::format_significant_digits(crate::typed_kind_to_scalar(value), digits as usize)
        }
        SolveStringConversionSource::Integer => integer(value, span)?.to_string(),
        SolveStringConversionSource::Boolean => boolean(value, span)?.to_string(),
    };
    crate::pad_event_message_conversion(rendered, width, left, span)
}
