//! Execute original retained assertion owners through the admitted host engine.
mod authored_source;
#[cfg(feature = "precompiled-programs")]
mod precompiled;
use super::*;
use crate::TypedCallFaultKind;
use rumoca_core::{SourceId, Span};
use rumoca_ir_solve as s;
use std::num::NonZeroU64;

fn span(at: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("retained-event-engine.mo"),
        at,
        at + 1,
    )
}
fn source() -> s::SolveExecutionSource {
    source_reading(s::LinearOp::LoadY { dst: 1, index: 1 }, 0)
}

/// The fixture with its divisor loaded by `divisor` and `p_cells` parameters.
fn source_reading(divisor: s::LinearOp, p_cells: usize) -> s::SolveExecutionSource {
    let p = s::SolveArithmeticProfile::construct(
        s::SolveRealFormat::Binary64,
        s::SolveIntegerDomain::FULL,
    );
    let integer = s::SolveValueType::scalar(s::SolveScalarType::integer(p));
    let mut builder = s::SolvePureCallTable::builder(p);
    builder
        .add_owner(
            s::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![
                s::SolveValueType::scalar(s::SolveScalarType::Boolean),
                integer.clone(),
            ],
            vec![
                s::SolvePureCallOutput::result(integer.clone()),
                s::SolvePureCallOutput::assertion_predicate_at_level(
                    s::SolveAssertionLevel::Warning,
                ),
                s::SolvePureCallOutput::assertion_message_value(integer, 1),
            ],
            span(1),
            |b, input, output| {
                let condition = b.load(input[0], span(2))?;
                let check = b.assertion_output(1, span(3))?;
                let message =
                    b.check_assertion(check, condition, &[], span(4), |m, _, outputs| {
                        let value = m.constant(
                            s::SolveValue::integer(p, 9_007_199_254_740_993).unwrap(),
                            span(5),
                        )?;
                        m.store(outputs[0], value, span(6))
                    })?;
                let numerator = b.constant(s::SolveValue::integer(p, 42).unwrap(), span(7))?;
                let denominator = b.load(input[1], span(8))?;
                let value = b.binary(
                    s::SolveBinaryOperator::IntegerQuotient,
                    numerator,
                    denominator,
                    span(9),
                )?;
                b.store(output[0], value, span(10))?;
                b.store(output[1], condition, span(11))?;
                b.store(output[2], message[0], span(12))
            },
        )
        .unwrap();
    let calls = builder.finish();
    let site = calls.owners()[0].call_site();
    let program = vec![
        s::LinearOp::LoadY { dst: 0, index: 0 },
        divisor,
        s::LinearOp::PureCallObservation {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: s::SolveAssertionObservationSite::new(site.clone()).unwrap(),
        },
        s::LinearOp::StoreOutput { src: 2 },
    ];
    let mut model = s::SolveModel {
        pure_calls: calls,
        ..Default::default()
    };
    model.problem.layout = s::VarLayout::from_parts(Default::default(), 2, p_cells);
    model.problem.events.action_conditions =
        s::ScalarProgramBlock::with_program_spans(vec![program], vec![span(100)]).unwrap();
    model.problem.events.actions = vec![s::SolveEventAction {
        kind: s::SolveEventActionKind::Warning,
        span: span(4),
        origin: "authored warning".into(),
        clock_owner: None,
        assertion_projection: s::SolveAssertionActionProjection::new(site, 1),
        message: s::SolveEventMessage {
            parts: vec![s::SolveEventMessagePart::Text("authored warning".into())],
        },
    }];
    s::SolveExecutionSource::new(model)
}

fn compile() -> CompiledRetainedEventWasm {
    compile_source(source())
}

/// The fixture reading its condition from Y and its divisor from P.
fn compile_reading_p() -> CompiledRetainedEventWasm {
    compile_source(source_reading(s::LinearOp::LoadP { dst: 1, index: 0 }, 2))
}

fn compile_source(source: s::SolveExecutionSource) -> CompiledRetainedEventWasm {
    let program = source
        .event_program(s::RetainedAssertionSource::ActionConditions, 0)
        .unwrap();
    CompiledRetainedEventWasm::compile(program).unwrap()
}

#[test]
fn retained_event_host_correlates_original_action_and_exact_integer_capture() {
    let code = compile();
    let source = code.source();
    let model = source.source().model();
    let original = &model.problem.events.actions[0];
    let owner = &model.pure_calls.owners()[0];
    assert_eq!(source.span(), span(100));
    assert_eq!(model.problem.layout.y_scalars(), 2);
    assert_eq!(model.problem.layout.p_scalars(), 0);
    let mut out = vec![37.25; s::ScalarProgramBlock::program_output_count(source.program())];
    let mut messages = Vec::new();
    code.call(
        &[0.0, 1.0],
        &[],
        0.0,
        &mut out,
        |action, captures, at, actual_owner| {
            assert!(std::ptr::eq(action.action(), original));
            assert!(std::ptr::eq(actual_owner, owner));
            assert_eq!(action.action_index(), 0);
            assert_eq!(at, span(4));
            assert_eq!(captures.len(), 1);
            assert_eq!(captures[0].0, 2);
            assert_eq!(captures[0].1.bits(), &[9_007_199_254_740_993]);
            assert!(matches!(
                captures[0].1.value_type().element_type(),
                s::SolveScalarType::Integer(_)
            ));
            let s::SolveEventMessagePart::Text(message) = &action.action().message.parts[0] else {
                panic!("the checked original action must retain its authored message");
            };
            messages.push(message.clone());
            Ok(())
        },
    )
    .unwrap();
    assert_eq!(out, [0.0]);
    assert_eq!(messages, ["authored warning"]);
    let mut calls = 0;
    code.call(&[1.0, 1.0], &[], 0.0, &mut out, |_, _, _, _| {
        calls += 1;
        Ok(())
    })
    .unwrap();
    assert_eq!(out, [1.0]);
    assert_eq!(
        calls, 0,
        "a passing invocation must not replay the earlier warning"
    );
}

#[test]
fn retained_event_host_fault_keeps_source_and_output_atomic_then_recovers() {
    let code = compile();
    let owner = code.source().table().owners()[0].id();
    let mut out = [-0.0];
    let mut calls = 0;
    let error = code
        .call(&[0.0, 0.0], &[], 0.0, &mut out, |_, _, _, _| {
            calls += 1;
            Ok(())
        })
        .unwrap_err();
    let WasmCompileError::TypedSource(fault) = error else {
        panic!("the original integer operation must retain its source-fault category");
    };
    assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
    assert_eq!(fault.provenance, span(9));
    assert_eq!(fault.owner, owner);
    assert_eq!(out[0].to_bits(), (-0.0f64).to_bits());
    assert_eq!(
        calls, 0,
        "a failed invocation must not publish its earlier warning"
    );
    code.call(&[1.0, 1.0], &[], 0.0, &mut out, |_, _, _, _| {
        calls += 1;
        Ok(())
    })
    .unwrap();
    assert_eq!(out, [1.0]);
    assert_eq!(
        calls, 0,
        "failed invocation records must not survive recovery"
    );
    code.call(&[0.0, 1.0], &[], 0.0, &mut out, |_, captures, at, _| {
        assert_eq!(captures[0].1.bits(), &[9_007_199_254_740_993]);
        assert_eq!(at, span(4));
        calls += 1;
        Ok(())
    })
    .unwrap();
    assert_eq!(out, [0.0]);
    assert_eq!(calls, 1);
}

#[test]
fn retained_event_host_rejects_footprints_and_report_failure_before_publication() {
    let code = compile();
    let mut out = [37.25];
    for (y, p) in [(&[0.0][..], &[][..]), (&[0.0, 1.0][..], &[1.0][..])] {
        let mut calls = 0;
        assert!(matches!(
            code.call(y, p, 0.0, &mut out, |_, _, _, _| {
                calls += 1;
                Ok(())
            }),
            Err(WasmCompileError::Input(_))
        ));
        assert_eq!(out, [37.25]);
        assert_eq!(calls, 0);
    }
    let error = code
        .call(&[0.0, 1.0], &[], 0.0, &mut out, |_, _, _, _| {
            Err(WasmCompileError::Backend(
                "report publication failed".into(),
            ))
        })
        .unwrap_err();
    assert!(
        matches!(error, WasmCompileError::Backend(message) if message == "report publication failed")
    );
    assert_eq!(out, [37.25]);
    let mut calls = 0;
    code.call(&[1.0, 1.0], &[], 0.0, &mut out, |_, _, _, _| {
        calls += 1;
        Ok(())
    })
    .unwrap();
    assert_eq!(out, [1.0]);
    assert_eq!(calls, 0);
}

/// The event module's emission issues the Y certificate, and the call on the
/// caller's Y returns the status and scratch bits of the full-copy call,
/// faulting or not, and leaves the caller's Y bitwise unchanged.
#[test]
fn a_certified_event_runs_on_the_callers_y_with_the_full_copy_results() {
    let code = compile();
    assert!(code.read_only_y.is_some());
    let cells = code.scratch.borrow().len();
    let mut statuses = Vec::new();
    for y in [[0.0, 1.0], [1.0, 1.0], [0.0, 0.0], [-0.0, f64::NAN]] {
        let bits = y.map(f64::to_bits);
        let mut shared = vec![u64::MAX; cells];
        let mut copied = shared.clone();
        let status = code
            .runtime
            .call_native_read_only(&y, &[], 0.0, &mut shared, &mut [])
            .unwrap();
        let copied_status = code
            .runtime
            .call_native(&mut y.to_vec(), &[], 0.0, &mut copied, &mut [])
            .unwrap();
        assert_eq!((status, &shared), (copied_status, &copied));
        assert_eq!(y.map(f64::to_bits), bits);
        statuses.push(status);
    }
    assert_eq!(statuses[..2], [0, 0]);
    assert_ne!(statuses[2], 0, "a zero divisor must fault");
}

/// Y and P are shared slices and may overlap. A certified module still runs
/// on a copy of Y whenever the two share a cell; adjacent slices and empty
/// slices share none, and an uncertified module always runs on a copy.
#[test]
fn a_certified_event_keeps_the_copy_when_y_and_p_share_a_cell() {
    let code = compile_reading_p();
    let certificate = code.read_only_y.as_ref();
    assert!(certificate.is_some());
    let cells = [1.0, 2.0, 5.0];
    let apart = [2.0, 5.0];
    for (y, p, expected) in [
        (&cells[..2], &cells[..2], YPresentation::Copied),
        (&cells[..2], &cells[1..], YPresentation::Copied),
        (&cells[1..], &cells[..2], YPresentation::Copied),
        (&cells[..2], &apart[..], YPresentation::InPlace),
        (&cells[..1], &cells[1..], YPresentation::InPlace),
        (&cells[1..1], &cells[..], YPresentation::InPlace),
        (&cells[..], &cells[1..1], YPresentation::InPlace),
    ] {
        assert_eq!(y_presentation(certificate, y, p), expected);
        assert_eq!(y_presentation(None, y, p), YPresentation::Copied);
    }
}

/// A certified module reading Y and P returns the status and scratch bits of
/// the copied call whether its Y and P are one slice, overlapping windows or
/// apart, faulting or not, and leaves the caller's cells bitwise unchanged.
#[test]
fn a_certified_event_reading_y_and_p_matches_the_copied_call_for_every_overlap() {
    let code = compile_reading_p();
    assert!(code.read_only_y.is_some());
    let cells = code.scratch.borrow().len();
    let mut statuses = Vec::new();
    for values in [
        [1.0, 2.0, 5.0],
        [1.0, 0.0, 5.0],
        [0.0, 3.0, 5.0],
        [0.0, 0.0, 5.0],
    ] {
        let bits = values.map(f64::to_bits);
        let apart = [values[1], values[2]];
        for (y, p) in [
            (&values[..2], &values[..2]),
            (&values[..2], &values[1..]),
            (&values[..2], &apart[..]),
        ] {
            let mut selected = vec![u64::MAX; cells];
            let mut copied = selected.clone();
            let status = code.execute(y, p, 0.0, &mut selected).unwrap();
            let copied_status = code
                .runtime
                .call_native(&mut y.to_vec(), p, 0.0, &mut copied, &mut [])
                .unwrap();
            assert_eq!((status, &selected), (copied_status, &copied));
            assert_eq!(values.map(f64::to_bits), bits);
            statuses.push(status);
        }
    }
    // One slice divides by its own condition cell; the windows and the
    // disjoint pair divide by the second cell.
    assert_eq!(statuses[..3], [0, 0, 0]);
    assert_eq!(statuses[3], 0);
    assert_ne!(statuses[4], 0, "a zero divisor must fault");
    assert_ne!(statuses[5], 0, "a zero divisor must fault");
    assert_ne!(statuses[6], 0, "a zero divisor must fault");
}

/// A fault on overlapping Y and P leaves the output and the caller's cells
/// untouched and calls no observer, as on the copied path.
#[test]
fn a_certified_event_fault_on_overlapping_y_and_p_keeps_output_and_observers_untouched() {
    let code = compile_reading_p();
    let owner = code.source().table().owners()[0].id();
    let one = [0.0, 0.0];
    let windows = [1.0, 0.0, 5.0];
    for (y, p) in [(&one[..], &one[..]), (&windows[..2], &windows[1..])] {
        let mut out = [-0.0];
        let mut calls = 0;
        let error = code
            .call(y, p, 0.0, &mut out, |_, _, _, _| {
                calls += 1;
                Ok(())
            })
            .unwrap_err();
        let WasmCompileError::TypedSource(fault) = error else {
            panic!("the original integer operation must retain its source-fault category");
        };
        assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
        assert_eq!(fault.owner, owner);
        assert_eq!(out[0].to_bits(), (-0.0f64).to_bits());
        assert_eq!(calls, 0);
    }
    assert_eq!(one.map(f64::to_bits), [0, 0]);
    assert_eq!(windows.map(f64::to_bits), [1.0, 0.0, 5.0].map(f64::to_bits));
}
