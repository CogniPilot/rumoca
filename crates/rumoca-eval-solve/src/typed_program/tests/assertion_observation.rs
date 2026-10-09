use super::*;
use crate::{AssertionInvocationEvaluation, eval_assertion_invocation};
use rumoca_ir_solve::{
    CheckedAssertionInvocation, SolveAssertionActionProjection, SolveEventAction,
    SolveEventActionKind, SolveEventMessage, SolveEventMessagePart, SolveStringConversionFormat,
    SolveStringConversionSource,
};

fn invocation_program(site: rumoca_ir_solve::SolvePureCallSite, output: u32) -> Vec<LinearOp> {
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

fn action(site: rumoca_ir_solve::SolvePureCallSite) -> SolveEventAction {
    SolveEventAction {
        kind: SolveEventActionKind::Assert,
        message: SolveEventMessage {
            parts: vec![
                SolveEventMessagePart::Text("captured ".to_owned()),
                SolveEventMessagePart::Conversion {
                    value: invocation_program(site.clone(), 3),
                    source: SolveStringConversionSource::Integer,
                    format: SolveStringConversionFormat::Options {
                        minimum_length: None,
                        left_justified: None,
                        significant_digits: None,
                    },
                },
            ],
        },
        span: span(4),
        origin: "authored assertion".to_owned(),
        clock_owner: None,
        assertion_projection: SolveAssertionActionProjection::new(site, 1),
    }
}

#[test]
fn same_invocation_projection_formats_captures_without_demanding_ordinary_results() {
    let table = super::assertion_forwarding::forwarded_table();
    let site = table.owners()[1].call_site();
    let program = invocation_program(site.clone(), 1);
    let actions = [action(site)];
    let binding = CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let false_argument = [TypedValue::scalar(&SolveValue::boolean(false))];
    let AssertionInvocationEvaluation::Failed { reports } =
        eval_assertion_invocation(&binding, &false_argument).unwrap()
    else {
        panic!("a source stop was published as a result tuple");
    };
    assert_eq!(reports.len(), 1);
    assert_eq!(reports[0].message, "captured 17");
    assert_eq!(reports[0].span, span(4));
    assert_eq!(reports[0].action_index, 0);
    let true_argument = [TypedValue::scalar(&SolveValue::boolean(true))];
    let AssertionInvocationEvaluation::Fault { error, reports } =
        eval_assertion_invocation(&binding, &true_argument).unwrap()
    else {
        panic!("passed assertion hid the later numerical fault");
    };
    assert!(reports.is_empty());
    assert_eq!(error.source_span(), Some(span(11)));
}

#[test]
fn warning_observation_precedes_the_later_numerical_fault() {
    let (table, _) = super::assertions::assertion_then_fault_builder(
        rumoca_ir_solve::SolveAssertionLevel::Warning,
        false,
    );
    let table = table.finish();
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone(), 1);
    let mut actions = [action(site)];
    actions[0].kind = SolveEventActionKind::Warning;
    let binding = CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let argument = [TypedValue::scalar(&SolveValue::boolean(false))];
    let AssertionInvocationEvaluation::Fault { reports, error } =
        eval_assertion_invocation(&binding, &argument).unwrap()
    else {
        panic!("a warning changed the later numerical failure into success");
    };
    assert_eq!(reports.len(), 1);
    assert_eq!(reports[0].kind, SolveEventActionKind::Warning);
    assert_eq!(reports[0].message, "captured 17");
    assert_eq!(reports[0].span, span(4));
    assert_eq!(error.source_span(), Some(span(11)));
}

#[test]
fn checked_projection_rejects_result_selectors_wrong_severity_and_foreign_sites() {
    let table = super::assertion_forwarding::forwarded_table();
    let site = table.owners()[1].call_site();
    let program = invocation_program(site.clone(), 1);
    let mut actions = [action(site.clone())];
    let SolveEventMessagePart::Conversion { value, .. } = &mut actions[0].message.parts[1] else {
        unreachable!();
    };
    *value = invocation_program(site.clone(), 1);
    assert!(CheckedAssertionInvocation::new(&table, &program, 1, &actions).is_none());
    actions[0] = action(site.clone());
    actions[0].kind = SolveEventActionKind::Warning;
    assert!(CheckedAssertionInvocation::new(&table, &program, 1, &actions).is_none());
    actions[0] = action(table.owners()[0].call_site());
    assert!(CheckedAssertionInvocation::new(&table, &program, 1, &actions).is_none());
    actions[0] = action(site);
    assert!(CheckedAssertionInvocation::new(&table, &program, 0, &actions).is_none());
}

#[test]
fn event_context_consumes_only_its_exact_predicate_program_and_capture_frame() {
    let table = super::assertion_forwarding::forwarded_table();
    let site = table.owners()[1].call_site();
    let actions = [action(site.clone())];
    let program = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::PureCallObservation {
            dst_start: 1,
            input_starts: Box::new([0]),
            site: rumoca_ir_solve::SolveAssertionObservationSite::new(site).unwrap(),
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let block =
        rumoca_ir_solve::ScalarProgramBlock::with_program_spans(vec![program], vec![span(4)])
            .unwrap();
    let observations =
        crate::CheckedEventObservationContext::construct(&table, &block, &actions).unwrap();
    let context = crate::RowEvalContext {
        pure_calls: Some(&table),
        event_observations: Some(&observations),
        ..Default::default()
    };
    let mut output = [91.0];
    crate::eval_scalar_program_block_with_context(&block, &[], &[], 0.0, context, &mut output)
        .unwrap();
    assert_eq!(output, [0.0]);
    let reports = observations.take_reports();
    assert_eq!(reports.len(), 1);
    assert_eq!(reports[0].message, "captured 17");
    let replaced = rumoca_ir_solve::ScalarProgramBlock::with_program_spans(
        block
            .programs()
            .iter()
            .map(|program| program.to_vec())
            .collect(),
        vec![span(4)],
    )
    .unwrap();
    let error = crate::eval_scalar_program_block_with_context(
        &replaced,
        &[],
        &[],
        0.0,
        context,
        &mut output,
    )
    .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("does not own this exact program")
    );
    assert!(observations.take_reports().is_empty());
    let generic = crate::RowEvalContext {
        pure_calls: Some(&table),
        ..Default::default()
    };
    let error =
        crate::eval_scalar_program_block_with_context(&block, &[], &[], 0.0, generic, &mut output)
            .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("requires its checked event-action adapter")
    );
}
