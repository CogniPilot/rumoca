//! Assertion messages for passive templates: literal segments as exact UTF-8
//! bytes, and each `String(...)` conversion as rows the component evaluates
//! when the assertion fails, formatted as the linked runtime formats them
//! (rumoca-eval-solve `eval_event_message_conversion`).

use std::sync::Arc;

use crate::errors::CodegenError;
use minijinja::Value;
use rumoca_ir_solve::{
    LinearOp, ScalarProgramBlock, SolveEventMessagePart, SolveProblem, SolveStringConversionFormat,
    SolveStringConversionSource,
};
use serde::Serialize;

mod observations;
pub(super) use observations::observations;

/// One segment of an assertion message.
#[derive(Debug, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(super) enum MessagePart {
    Text(Vec<u8>),
    Conversion(Conversion),
}

/// A conversion's value and option rows, as indices into the message rows.
#[derive(Debug, PartialEq, Serialize)]
pub(super) struct Conversion {
    /// `real`, `integer`, or `boolean`.
    source: &'static str,
    value: usize,
    minimum_length: Option<usize>,
    left_justified: Option<usize>,
    significant_digits: Option<usize>,
}

/// Every action's message, and the rows its conversions read.
pub(super) struct AssertionMessages {
    pub(super) parts: Vec<Vec<MessagePart>>,
    rows: Vec<Vec<LinearOp>>,
    spans: Vec<rumoca_core::Span>,
}

impl AssertionMessages {
    /// The conversion rows as a renderable plan, one output per row.
    pub(super) fn rows_value(&self) -> Result<Value, CodegenError> {
        let block = ScalarProgramBlock::with_program_spans(self.rows.clone(), self.spans.clone())
            .map_err(|error| CodegenError::template(error.to_string()))?;
        Ok(Value::from_object(
            super::scalar_program_plan::ScalarProgramPlan::new(Arc::new(block))?,
        ))
    }
}

pub(super) fn messages(problem: &SolveProblem) -> Result<AssertionMessages, CodegenError> {
    let mut out = AssertionMessages {
        parts: Vec::new(),
        rows: Vec::new(),
        spans: Vec::new(),
    };
    for action in &problem.events.actions {
        if action.assertion_projection.is_some() {
            out.parts.push(Vec::new());
            continue;
        }
        let mut parts = Vec::new();
        for part in &action.message.parts {
            parts.push(match part {
                SolveEventMessagePart::Text(text) => MessagePart::Text(text.as_bytes().to_vec()),
                SolveEventMessagePart::Conversion {
                    value,
                    source,
                    format,
                } => MessagePart::Conversion(conversion(
                    &mut out,
                    action.span,
                    value,
                    *source,
                    format,
                )),
            });
        }
        out.parts.push(parts);
    }
    Ok(out)
}

fn conversion(
    out: &mut AssertionMessages,
    span: rumoca_core::Span,
    value: &[LinearOp],
    source: SolveStringConversionSource,
    format: &SolveStringConversionFormat,
) -> Conversion {
    let mut row = |ops: &[LinearOp]| {
        out.rows.push(ops.to_vec());
        out.spans.push(span);
        out.rows.len() - 1
    };
    let SolveStringConversionFormat::Options {
        minimum_length,
        left_justified,
        significant_digits,
    } = format;
    Conversion {
        source: conversion_source(source),
        value: row(value),
        minimum_length: minimum_length.as_deref().map(&mut row),
        left_justified: left_justified.as_deref().map(&mut row),
        significant_digits: significant_digits.as_deref().map(&mut row),
    }
}

fn conversion_source(source: SolveStringConversionSource) -> &'static str {
    match source {
        SolveStringConversionSource::Real => "real",
        SolveStringConversionSource::Integer => "integer",
        SolveStringConversionSource::Boolean => "boolean",
    }
}

/// The literal start text of every `String` scalar of the checked FMI
/// inventory, in inventory order, as exact UTF-8 bytes. The C profile keeps
/// these texts beside the numeric storage, which no program reads for them.
pub(super) fn text_starts(fmi: &minijinja::Value) -> Result<Vec<Vec<u8>>, CodegenError> {
    let mut texts = Vec::new();
    for variable in fmi.get_attr("variables")?.try_iter()? {
        if variable.get_attr("value_kind")?.as_str() != Some("String") {
            continue;
        }
        let starts = variable.get_attr("text_start")?;
        if starts.is_none() {
            return Err(CodegenError::template(
                "unsupported-feature:fmi.c.string: a String variable has no literal start",
            ));
        }
        for start in starts.try_iter()? {
            let text = start
                .as_str()
                .ok_or_else(|| CodegenError::template("a String start is not text"))?;
            texts.push(text.as_bytes().to_vec());
        }
    }
    Ok(texts)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::{
        SolveEventAction, SolveEventActionKind, SolveEventMessage, SolveStringConversionFormat,
        SolveStringConversionSource,
    };

    fn assertion(parts: Vec<SolveEventMessagePart>) -> SolveEventAction {
        SolveEventAction {
            kind: SolveEventActionKind::Assert,
            message: SolveEventMessage { parts },
            span: rumoca_ir_solve::source_span_from_offsets(1, 0, 1),
            origin: "message encoding fixture".into(),
            clock_owner: None,
            assertion_projection: None,
        }
    }

    #[test]
    fn literal_segments_keep_their_utf8_bytes_and_action_order() {
        let mut problem = SolveProblem::default();
        problem.events.actions = vec![
            assertion(vec![
                SolveEventMessagePart::Text("Mass μ must be ".into()),
                SolveEventMessagePart::Text("positive: \"m\"\n".into()),
            ]),
            assertion(Vec::new()),
            assertion(vec![SolveEventMessagePart::Text("inertia".into())]),
        ];
        let rendered = messages(&problem).unwrap();
        assert_eq!(
            rendered.parts,
            vec![
                vec![
                    MessagePart::Text("Mass μ must be ".as_bytes().to_vec()),
                    MessagePart::Text("positive: \"m\"\n".as_bytes().to_vec()),
                ],
                Vec::new(),
                vec![MessagePart::Text(b"inertia".to_vec())],
            ]
        );
        assert!(rendered.rows.is_empty());
    }

    #[test]
    fn a_conversion_keeps_its_value_and_option_rows_in_order() {
        let row = |value| {
            vec![
                LinearOp::Const { dst: 0, value },
                LinearOp::StoreOutput { src: 0 },
            ]
        };
        let mut problem = SolveProblem::default();
        problem.events.actions = vec![assertion(vec![
            SolveEventMessagePart::Text("mass = ".into()),
            SolveEventMessagePart::Conversion {
                value: row(2.5),
                source: SolveStringConversionSource::Real,
                format: SolveStringConversionFormat::Options {
                    minimum_length: Some(row(8.0)),
                    left_justified: None,
                    significant_digits: Some(row(3.0)),
                },
            },
        ])];
        let rendered = messages(&problem).unwrap();
        assert_eq!(
            rendered.parts[0][1],
            MessagePart::Conversion(Conversion {
                source: "real",
                value: 0,
                minimum_length: Some(1),
                left_justified: None,
                significant_digits: Some(2),
            })
        );
        assert_eq!(rendered.rows, vec![row(2.5), row(8.0), row(3.0)]);
        assert!(rendered.rows_value().is_ok());
    }
}
