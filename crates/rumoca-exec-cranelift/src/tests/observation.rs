//! Assertion observations execute in the native expression program: the
//! predicates they publish match the typed interpreter's checked event-action
//! observation, on a passing and on a failing assertion.

use super::*;
use rumoca_eval_solve::{CheckedEventObservationContext, RowEvalContext};
use rumoca_ir_solve::{
    SolveArithmeticProfile, SolveAssertionActionProjection, SolveAssertionLevel,
    SolveAssertionObservationSite, SolveBinaryOperator, SolveEventAction, SolveEventActionKind,
    SolveEventMessage, SolveEventMessagePart, SolveIntegerDomain, SolvePureCallIdentity,
    SolvePureCallOutput, SolvePureCallSite, SolvePureCallTable, SolveRealFormat, SolveScalarType,
    SolveStringConversionFormat, SolveStringConversionSource, SolveValue, SolveValueType,
};

fn arithmetic() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL)
}

fn real_type() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(arithmetic()))
}

fn boolean_type() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::Boolean)
}

/// One owner: result `2 x`, an assertion on `ok` capturing `x`, the predicate
/// and the failed-only message value, in that output order.
fn observed_table() -> SolvePureCallTable {
    let span = fixture_span();
    SolvePureCallTable::construct(arithmetic(), |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![real_type(), boolean_type()],
            vec![
                SolvePureCallOutput::result(real_type()),
                SolvePureCallOutput::assertion_predicate_at_level(SolveAssertionLevel::Error),
                SolvePureCallOutput::assertion_message_value(real_type(), 1),
            ],
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let ok = b.load(inputs[1], span)?;
                let predicate = b.assertion_output(1, span)?;
                let message =
                    b.check_assertion(predicate, ok, &[x], span, |m, captures, out| {
                        let value = m.load(captures[0], span)?;
                        m.store(out[0], value, span)
                    })?;
                let two = b.constant(SolveValue::real(arithmetic(), 2.0), span)?;
                let doubled = b.binary(SolveBinaryOperator::Multiply, x, two, span)?;
                b.store(outputs[0], doubled, span)?;
                b.store(outputs[1], ok, span)?;
                b.store(outputs[2], message[0], span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

/// Loads `x` into register 0 and `ok` from parameter 0 into register 1, runs
/// `call`, and stores register `output`.
fn invocation(call: LinearOp, output: u32) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value: 3.5 },
        LinearOp::LoadP { dst: 1, index: 0 },
        call,
        LinearOp::StoreOutput { src: output },
    ]
}

fn pure_call(site: &SolvePureCallSite) -> LinearOp {
    LinearOp::PureCall {
        dst_start: 2,
        input_starts: Box::new([0, 1]),
        site: site.clone(),
    }
}

fn observation(site: &SolvePureCallSite) -> LinearOp {
    LinearOp::PureCallObservation {
        dst_start: 2,
        input_starts: Box::new([0, 1]),
        site: SolveAssertionObservationSite::new(site.clone()).unwrap(),
    }
}

/// The scheduled assertion: its message formats the captured `x`.
fn action(site: &SolvePureCallSite) -> SolveEventAction {
    SolveEventAction {
        kind: SolveEventActionKind::Assert,
        message: SolveEventMessage {
            parts: vec![
                SolveEventMessagePart::Text("captured ".to_owned()),
                SolveEventMessagePart::Conversion {
                    value: invocation(pure_call(site), 4),
                    source: SolveStringConversionSource::Real,
                    format: SolveStringConversionFormat::Options {
                        minimum_length: None,
                        left_justified: None,
                        significant_digits: None,
                    },
                },
            ],
        },
        span: fixture_span(),
        origin: "authored assertion".to_owned(),
        clock_owner: None,
        assertion_projection: SolveAssertionActionProjection::new(site.clone(), 1),
    }
}

fn interpreted(
    table: &SolvePureCallTable,
    block: &ScalarProgramBlock,
    actions: &[SolveEventAction],
    ok: f64,
) -> (f64, Vec<String>) {
    let observations = CheckedEventObservationContext::construct(table, block, actions).unwrap();
    let context = RowEvalContext {
        pure_calls: Some(table),
        event_observations: Some(&observations),
        ..Default::default()
    };
    let mut out = [f64::NAN];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        block,
        &[],
        &[ok],
        0.0,
        context,
        &mut out,
    )
    .unwrap();
    let messages = observations
        .take_reports()
        .into_iter()
        .map(|report| report.message)
        .collect();
    (out[0], messages)
}

#[test]
fn native_observation_publishes_the_interpreter_predicates_on_pass_and_fail() {
    let table = observed_table();
    let site = table.owners()[0].call_site();
    let actions = [action(&site)];
    let block = ScalarProgramBlock::with_program_spans(
        vec![invocation(observation(&site), 2)],
        vec![fixture_span()],
    )
    .unwrap();
    let pure_calls = compile_pure_call_table(&table).unwrap();
    assert!(
        pure_calls
            .owner_support(site.owner())
            .unwrap()
            .primal()
            .is_native()
    );
    let compiled =
        compile_expression_scalar_program_block_with_pure_calls(&block, &pure_calls).unwrap();
    for ok in [1.0, 0.0] {
        let (expected, messages) = interpreted(&table, &block, &actions, ok);
        let mut native = [f64::NAN];
        compiled.call(&[], &[ok], 0.0, &mut native).unwrap();
        assert_eq!(native, [expected], "ok {ok}");
        // The interpreter reports the authored message exactly when the
        // predicate is zero; the native row owns the predicate alone.
        assert_eq!(messages.len(), usize::from(ok == 0.0), "ok {ok}");
        if ok == 0.0 {
            assert_eq!(messages, ["captured 3.5"]);
        }
    }
}

#[test]
fn native_observation_and_an_ordinary_call_of_one_owner_publish_independently() {
    let table = observed_table();
    let site = table.owners()[0].call_site();
    let program = vec![
        LinearOp::Const { dst: 0, value: 3.5 },
        LinearOp::LoadP { dst: 1, index: 0 },
        pure_call(&site),
        LinearOp::PureCallObservation {
            dst_start: 5,
            input_starts: Box::new([0, 1]),
            site: SolveAssertionObservationSite::new(site.clone()).unwrap(),
        },
        LinearOp::Binary {
            dst: 6,
            op: rumoca_ir_solve::BinaryOp::Add,
            lhs: 2,
            rhs: 5,
        },
        LinearOp::StoreOutput { src: 6 },
    ];
    let block =
        ScalarProgramBlock::with_program_spans(vec![program], vec![fixture_span()]).unwrap();
    let pure_calls = compile_pure_call_table(&table).unwrap();
    let compiled =
        compile_expression_scalar_program_block_with_pure_calls(&block, &pure_calls).unwrap();
    for (ok, expected) in [(1.0, 8.0), (0.0, 7.0)] {
        let mut native = [f64::NAN];
        compiled.call(&[], &[ok], 0.0, &mut native).unwrap();
        assert_eq!(native, [expected], "ok {ok}");
    }
}

/// Two Error-level assertions on Boolean inputs `a` and `b`, then an Integer
/// quotient by zero: a value row faults after either assertion fails, while an
/// observation stops at the first failed one and evaluates nothing after it.
/// Outputs: result, predicate `a` (1), its message (2), predicate `b` (3), its
/// message (4).
fn two_assertions_then_fault_table() -> SolvePureCallTable {
    let span = fixture_span();
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic()));
    SolvePureCallTable::construct(arithmetic(), |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![boolean_type(), boolean_type()],
            vec![
                SolvePureCallOutput::result(integer.clone()),
                SolvePureCallOutput::assertion_predicate_at_level(SolveAssertionLevel::Error),
                SolvePureCallOutput::assertion_message_value(integer.clone(), 1),
                SolvePureCallOutput::assertion_predicate_at_level(SolveAssertionLevel::Error),
                SolvePureCallOutput::assertion_message_value(integer.clone(), 1),
            ],
            span,
            |b, inputs, outputs| {
                let a = b.load(inputs[0], span)?;
                let second = b.load(inputs[1], span)?;
                let mut messages = Vec::new();
                for (predicate, condition) in [(1, a), (3, second)] {
                    messages.push(seventeen_on_failure(b, predicate, condition)?);
                }
                let one = b.constant(SolveValue::integer(arithmetic(), 1).unwrap(), span)?;
                let zero = b.constant(SolveValue::integer(arithmetic(), 0).unwrap(), span)?;
                let quotient = b.binary(SolveBinaryOperator::IntegerQuotient, one, zero, span)?;
                b.store(outputs[0], quotient, span)?;
                b.store(outputs[1], a, span)?;
                b.store(outputs[2], messages[0], span)?;
                b.store(outputs[3], second, span)?;
                b.store(outputs[4], messages[1], span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn integer_message_action(site: &SolvePureCallSite, predicate: usize) -> SolveEventAction {
    let message = LinearOp::StoreOutput {
        src: 2 + predicate as u32 + 1,
    };
    SolveEventAction {
        kind: SolveEventActionKind::Assert,
        message: SolveEventMessage {
            parts: vec![SolveEventMessagePart::Conversion {
                value: vec![
                    LinearOp::LoadP { dst: 0, index: 0 },
                    LinearOp::LoadP { dst: 1, index: 1 },
                    LinearOp::PureCall {
                        dst_start: 2,
                        input_starts: Box::new([0, 1]),
                        site: site.clone(),
                    },
                    message,
                ],
                source: SolveStringConversionSource::Integer,
                format: SolveStringConversionFormat::Options {
                    minimum_length: None,
                    left_justified: None,
                    significant_digits: None,
                },
            }],
        },
        span: fixture_span(),
        origin: "authored assertion".to_owned(),
        clock_owner: None,
        assertion_projection: SolveAssertionActionProjection::new(site.clone(), predicate),
    }
}

/// The observation program of one call over parameters `a` and `b`, storing
/// each predicate in order.
fn two_predicate_block(site: &SolvePureCallSite) -> ScalarProgramBlock {
    let program = |which: u32| {
        vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 1, index: 1 },
            LinearOp::PureCallObservation {
                dst_start: 2,
                input_starts: Box::new([0, 1]),
                site: SolveAssertionObservationSite::new(site.clone()).unwrap(),
            },
            LinearOp::StoreOutput { src: 2 + which },
        ]
    };
    ScalarProgramBlock::with_program_spans(
        vec![program(0), program(1)],
        vec![fixture_span(), fixture_span()],
    )
    .unwrap()
}

#[test]
fn a_native_observation_stops_at_the_first_failed_error_assertion_like_the_interpreter() {
    let table = two_assertions_then_fault_table();
    let site = table.owners()[0].call_site();
    let actions = [
        integer_message_action(&site, 1),
        integer_message_action(&site, 3),
    ];
    let block = two_predicate_block(&site);
    let observations = CheckedEventObservationContext::construct(&table, &block, &actions).unwrap();
    let context = RowEvalContext {
        pure_calls: Some(&table),
        event_observations: Some(&observations),
        ..Default::default()
    };
    let pure_calls = compile_pure_call_table(&table).unwrap();
    assert!(
        pure_calls
            .owner_support(site.owner())
            .unwrap()
            .observation()
            .is_native()
    );
    let compiled =
        compile_expression_scalar_program_block_with_pure_calls(&block, &pure_calls).unwrap();
    // Both assertions failing publishes only the first: the second is never
    // evaluated. Whatever the inputs, the Integer quotient after them is never
    // evaluated when an assertion fails, and faults when none does.
    for (a, b) in [(0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0)] {
        let mut interpreted = [f64::NAN; 2];
        let expected = rumoca_eval_solve::eval_scalar_program_block_with_context(
            &block,
            &[],
            &[a, b],
            0.0,
            context,
            &mut interpreted,
        );
        let _ = observations.take_reports();
        let mut native = [f64::NAN; 2];
        let outcome = compiled.call(&[], &[a, b], 0.0, &mut native);
        assert_eq!(outcome.is_ok(), expected.is_ok(), "a {a} b {b}");
        if expected.is_ok() {
            assert_eq!(native, interpreted, "a {a} b {b}");
        }
    }
    let mut first_only = [f64::NAN; 2];
    compiled
        .call(&[], &[0.0, 0.0], 0.0, &mut first_only)
        .unwrap();
    assert_eq!(first_only, [0.0, 1.0]);
}

#[test]
fn a_native_observation_cannot_have_its_outcome_replaced_by_a_later_fault() {
    let table = two_assertions_then_fault_table();
    let site = table.owners()[0].call_site();
    let block = two_predicate_block(&site);
    let pure_calls = compile_pure_call_table(&table).unwrap();
    let compiled =
        compile_expression_scalar_program_block_with_pure_calls(&block, &pure_calls).unwrap();
    let mut out = [f64::NAN; 2];
    compiled
        .call(&[], &[1.0, 0.0], 0.0, &mut out)
        .expect("the failed assertion is the outcome; the later quotient never runs");
    assert_eq!(out, [1.0, 0.0]);
    compiled
        .call(&[], &[1.0, 1.0], 0.0, &mut out)
        .expect_err("with every assertion holding the later fault is the outcome");
}

/// An Error-level check of `condition` on predicate output `predicate`, whose
/// failed message is the Integer 17.
fn seventeen_on_failure<'program>(
    b: &mut rumoca_ir_solve::TypedProgramBuilder<'program>,
    predicate: usize,
    condition: rumoca_ir_solve::ProgramRegister<'program>,
) -> Result<
    rumoca_ir_solve::ProgramRegister<'program>,
    rumoca_ir_solve::SolveProgramConstructionError,
> {
    let span = fixture_span();
    let predicate = b.assertion_output(predicate, span)?;
    let message = b.check_assertion(predicate, condition, &[], span, |m, _, out| {
        let value = m.constant(SolveValue::integer(arithmetic(), 17).unwrap(), span)?;
        m.store(out[0], value, span)
    })?;
    Ok(message[0])
}
