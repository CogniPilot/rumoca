//! Message conversions read their options from the invocation's captures.

use super::*;
use crate::{AssertionInvocationEvaluation, EvalSolveError, eval_assertion_invocation};
use rumoca_ir_solve::{
    CheckedAssertionInvocation, SolveAssertionActionProjection, SolveAssertionLevel,
    SolveEventAction, SolveEventActionKind, SolveEventMessage, SolveEventMessagePart,
    SolveStringConversionFormat, SolveStringConversionSource,
};

/// A failing assertion that captures the Integer 17 and a Boolean `left`.
fn table(left: bool) -> SolvePureCallTable {
    let p = profile(SolveRealFormat::Binary64);
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    let mut table = SolvePureCallTable::builder(p);
    table
        .add_owner(
            identity(970),
            vec![boolean.clone()],
            vec![
                SolvePureCallOutput::result(integer.clone()),
                SolvePureCallOutput::assertion_predicate_at_level(SolveAssertionLevel::Error),
                SolvePureCallOutput::assertion_message_value(integer, 1),
                SolvePureCallOutput::assertion_message_value(boolean, 2),
            ],
            span(1),
            |b, inputs, outputs| {
                let predicate = b.load(inputs[0], span(2))?;
                let assertion = b.assertion_output(1, span(3))?;
                let messages =
                    b.check_assertion(assertion, predicate, &[], span(4), |region, _, outs| {
                        let number =
                            region.constant(SolveValue::integer(p, 17).unwrap(), span(5))?;
                        region.store(outs[0], number, span(6))?;
                        let flag = region.constant(SolveValue::boolean(left), span(7))?;
                        region.store(outs[1], flag, span(8))
                    })?;
                let one = b.constant(SolveValue::integer(p, 1).unwrap(), span(9))?;
                b.store(outputs[0], one, span(10))?;
                b.store(outputs[1], predicate, span(11))?;
                b.store(outputs[2], messages[0], span(12))?;
                b.store(outputs[3], messages[1], span(13))
            },
        )
        .unwrap();
    table.finish()
}

fn program(site: rumoca_ir_solve::SolvePureCallSite, output: u32) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::PureCall {
            dst_start: 1,
            input_starts: vec![0].into(),
            site,
        },
        LinearOp::StoreOutput { src: output },
    ]
}

const INTEGER_CAPTURE: u32 = 3;
const BOOLEAN_CAPTURE: u32 = 4;

fn action(
    site: &rumoca_ir_solve::SolvePureCallSite,
    source: SolveStringConversionSource,
    value: u32,
    left_justified: Option<u32>,
    action_span: Span,
) -> SolveEventAction {
    SolveEventAction {
        kind: SolveEventActionKind::Assert,
        message: SolveEventMessage {
            parts: vec![SolveEventMessagePart::Conversion {
                value: program(site.clone(), value),
                source,
                format: SolveStringConversionFormat::Options {
                    minimum_length: Some(program(site.clone(), INTEGER_CAPTURE)),
                    left_justified: left_justified.map(|output| program(site.clone(), output)),
                    significant_digits: None,
                },
            }],
        },
        span: action_span,
        origin: "authored assertion".to_owned(),
        clock_owner: None,
        assertion_projection: SolveAssertionActionProjection::new(site.clone(), 1),
    }
}

fn evaluate(
    table: &SolvePureCallTable,
    action: SolveEventAction,
) -> Result<AssertionInvocationEvaluation, EvalSolveError> {
    let site = table.owners()[0].call_site();
    let invocation = program(site, 1);
    let actions = [action];
    let binding = CheckedAssertionInvocation::new(table, &invocation, 1, &actions).unwrap();
    eval_assertion_invocation(&binding, &[TypedValue::scalar(&SolveValue::boolean(false))])
}

fn message(evaluation: Result<AssertionInvocationEvaluation, EvalSolveError>) -> String {
    let Ok(AssertionInvocationEvaluation::Failed { reports }) = evaluation else {
        panic!("the failed assertion publishes its report");
    };
    reports[0].message.clone()
}

#[test]
fn left_justified_option_selects_the_padding_side_from_a_boolean_capture() {
    for (left, expected) in [
        (true, format!("17{}", " ".repeat(15))),
        (false, format!("{}17", " ".repeat(15))),
    ] {
        let table = table(left);
        let site = table.owners()[0].call_site();
        let action = action(
            &site,
            SolveStringConversionSource::Integer,
            INTEGER_CAPTURE,
            Some(BOOLEAN_CAPTURE),
            span(4),
        );
        assert_eq!(message(evaluate(&table, action)), expected);
    }
}

#[test]
fn boolean_conversion_renders_a_boolean_capture() {
    let table = table(true);
    let site = table.owners()[0].call_site();
    let action = action(
        &site,
        SolveStringConversionSource::Boolean,
        BOOLEAN_CAPTURE,
        None,
        span(4),
    );
    assert_eq!(message(evaluate(&table, action)).trim(), "true");
}

#[test]
fn a_non_boolean_left_justified_option_is_refused_when_the_projection_is_checked() {
    let table = table(true);
    let site = table.owners()[0].call_site();
    let action = action(
        &site,
        SolveStringConversionSource::Integer,
        INTEGER_CAPTURE,
        Some(INTEGER_CAPTURE),
        span(4),
    );
    let invocation = program(site, 1);
    assert!(CheckedAssertionInvocation::new(&table, &invocation, 1, &[action]).is_none());
}

#[test]
fn an_action_that_moved_off_its_assertion_source_is_a_binding_error() {
    let table = table(true);
    let site = table.owners()[0].call_site();
    let action = action(
        &site,
        SolveStringConversionSource::Integer,
        INTEGER_CAPTURE,
        None,
        span(40),
    );
    let error = evaluate(&table, action).map(|_| ()).unwrap_err();
    assert!(
        matches!(
            &error,
            EvalSolveError::ShapeContract { message, span: None }
                if message == "assertion action changed its source provenance"
        ),
        "{error:?}"
    );
}
