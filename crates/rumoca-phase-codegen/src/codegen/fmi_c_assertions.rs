//! Constant assertion messages as exact UTF-8 bytes for passive templates.

use crate::errors::CodegenError;
use rumoca_ir_solve::{SolveEventMessagePart, SolveProblem};

pub(super) fn messages(problem: &SolveProblem) -> Result<Vec<Vec<u8>>, CodegenError> {
    problem
        .events
        .actions
        .iter()
        .map(|action| {
            let mut message = String::new();
            for part in &action.message.parts {
                let SolveEventMessagePart::Text(text) = part else {
                    return Err(CodegenError::template(
                        "unsupported-feature:fmi.c.assertion-message: dynamic conversion",
                    ));
                };
                message.push_str(text);
            }
            Ok(message.into_bytes())
        })
        .collect()
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
            span: rumoca_core::Span::DUMMY,
            origin: "message encoding fixture".into(),
            clock_owner: None,
        }
    }

    #[test]
    fn constant_messages_preserve_utf8_segments_and_action_order() {
        let mut problem = SolveProblem::default();
        problem.events.actions = vec![
            assertion(vec![
                SolveEventMessagePart::Text("Mass μ must be ".into()),
                SolveEventMessagePart::Text("positive: \"m\"\n".into()),
            ]),
            assertion(Vec::new()),
            assertion(vec![SolveEventMessagePart::Text("inertia".into())]),
        ];
        assert_eq!(
            messages(&problem).unwrap(),
            vec![
                "Mass μ must be positive: \"m\"\n".as_bytes().to_vec(),
                Vec::new(),
                b"inertia".to_vec(),
            ]
        );
    }

    #[test]
    fn an_unsupported_conversion_cannot_disappear_from_a_message() {
        let mut problem = SolveProblem::default();
        problem.events.actions = vec![assertion(vec![
            SolveEventMessagePart::Text("mass = ".into()),
            SolveEventMessagePart::Conversion {
                value: Vec::new(),
                source: SolveStringConversionSource::Real,
                format: SolveStringConversionFormat::Options {
                    minimum_length: None,
                    left_justified: None,
                    significant_digits: None,
                },
            },
        ])];
        let error = messages(&problem).expect_err("FMI C cannot render a dynamic message");
        assert!(
            error
                .to_string()
                .contains("unsupported-feature:fmi.c.assertion-message: dynamic conversion"),
            "{error}"
        );
    }
}
