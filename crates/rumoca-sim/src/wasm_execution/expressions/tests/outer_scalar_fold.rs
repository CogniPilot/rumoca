//! Distinguish repeated scalar invocations from loops inside one typed owner.
use super::*;
use rumoca_ir_solve as solve;

fn outer_span() -> Span {
    Span::from_offsets(
        SourceId::from_source_name("outer-scalar-observation.mo"),
        101,
        113,
    )
}

fn outer_table() -> solve::SolvePureCallTable {
    let span = outer_span();
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real.clone(), boolean],
            vec![
                solve::SolvePureCallOutput::result(real.clone()),
                solve::SolvePureCallOutput::assertion_predicate_at_level(
                    solve::SolveAssertionLevel::Warning,
                ),
                solve::SolvePureCallOutput::assertion_message_value(real, 1),
            ],
            span,
            |body, inputs, outputs| {
                let value = body.load(inputs[0], span)?;
                let holds = body.load(inputs[1], span)?;
                let assertion = body.assertion_output(1, span)?;
                let message = body.check_assertion(
                    assertion,
                    holds,
                    &[value],
                    span,
                    |region, captures, outputs| {
                        let value = region.load(captures[0], span)?;
                        region.store(outputs[0], value, span)
                    },
                )?;
                body.store(outputs[0], value, span)?;
                body.store(outputs[1], holds, span)?;
                body.store(outputs[2], message[0], span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn outer_fold(site: &solve::SolvePureCallSite, second_passes: bool) -> Vec<LinearOp> {
    let update = vec![
        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
        LinearOp::LoadFoldIndex {
            dst: 1,
            dimension: 0,
        },
        LinearOp::Const {
            dst: 2,
            value: if second_passes { 1.5 } else { 3.0 },
        },
        LinearOp::Compare {
            dst: 3,
            op: solve::CompareOp::Gt,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::PureCallObservation {
            dst_start: 4,
            input_starts: vec![1, 3].into(),
            site: solve::SolveAssertionObservationSite::new(site.clone()).unwrap(),
        },
        LinearOp::Const { dst: 5, value: 1.0 },
        LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Sub,
            lhs: 5,
            rhs: 4,
        },
        LinearOp::Binary {
            dst: 7,
            op: solve::BinaryOp::Max,
            lhs: 0,
            rhs: 6,
        },
        LinearOp::StoreOutput { src: 7 },
    ];
    let fold = solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id: 0,
                display_name: "i".into(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        },
        1,
        0,
        update,
    )
    .unwrap();
    vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::FunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(fold),
        },
        LinearOp::StoreOutput { src: 1 },
    ]
}

fn outer_action(site: solve::SolvePureCallSite) -> solve::SolveEventAction {
    let span = outer_span();
    // This checked recipe selects capture output 2; it is never evaluated to
    // obtain the dynamic invocation's message value.
    let capture = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: site.clone(),
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    solve::SolveEventAction {
        kind: solve::SolveEventActionKind::Warning,
        span,
        origin: "outer scalar warning".into(),
        clock_owner: None,
        assertion_projection: solve::SolveAssertionActionProjection::new(site, 1),
        message: solve::SolveEventMessage {
            parts: vec![
                solve::SolveEventMessagePart::Text("captured ".into()),
                solve::SolveEventMessagePart::Conversion {
                    value: capture,
                    source: solve::SolveStringConversionSource::Real,
                    format: solve::SolveStringConversionFormat::Options {
                        minimum_length: None,
                        left_justified: None,
                        significant_digits: None,
                    },
                },
            ],
        },
    }
}

fn outer_source(second_passes: bool) -> solve::SolveExecutionSource {
    let table = outer_table();
    let site = table.owners()[0].call_site();
    let program = outer_fold(&site, second_passes);
    let mut model = solve::SolveModel {
        pure_calls: table,
        ..Default::default()
    };
    model.problem.events.action_conditions =
        solve::ScalarProgramBlock::with_program_spans(vec![program], vec![outer_span()]).unwrap();
    model.problem.events.actions = vec![outer_action(site)];
    solve::SolveExecutionSource::new(model)
}

/// Reference execution uses the unchanged checked typed semantic owner.
/// The complete scalar reference event adapter currently cannot derive an
/// isolated fold update's enclosing register scope; this does not claim it can.
fn reference_outer_messages(
    source: &solve::SolveExecutionSource,
    second_passes: bool,
) -> Vec<String> {
    let program = source
        .event_program(solve::RetainedAssertionSource::ActionConditions, 0)
        .unwrap();
    let invocation = program.invocation_at(&[(1, 0)], 4).unwrap();
    let input_types = invocation.owner().inputs();
    let mut messages = Vec::new();
    let LinearOp::FunctionFold { program: fold, .. } = &program.program()[1] else {
        unreachable!()
    };
    for ordinal in 0..fold.domain.scalar_count().unwrap() {
        let i = fold.domain.index_tuple_at(ordinal).unwrap().unwrap()[0] as f64;
        let values = vec![
            rumoca_eval_solve::TypedValue::construct(
                input_types[0].clone(),
                vec![solve::SolveValueKind::Real64(i.to_bits())],
            )
            .unwrap(),
            rumoca_eval_solve::TypedValue::construct(
                input_types[1].clone(),
                vec![solve::SolveValueKind::Boolean(second_passes && i == 2.0)],
            )
            .unwrap(),
        ];
        let rumoca_eval_solve::AssertionInvocationEvaluation::Complete { reports, .. } =
            rumoca_eval_solve::eval_assertion_invocation(invocation.checked(), &values).unwrap()
        else {
            panic!("warning owner must complete")
        };
        messages.extend(reports.into_iter().map(|report| report.message));
    }
    messages
}

#[test]
fn outer_scalar_fold_wasm_refuses_unsupported_profile_before_execution() {
    for second_passes in [true, false] {
        let source = outer_source(second_passes);
        let expected = reference_outer_messages(&source, second_passes);
        assert_eq!(
            expected,
            if second_passes {
                vec!["captured 1"]
            } else {
                vec!["captured 1", "captured 2"]
            }
        );
        let program = source
            .event_program(solve::RetainedAssertionSource::ActionConditions, 0)
            .unwrap();
        assert_eq!(program.span(), outer_span());
        assert!(std::ptr::eq(
            program.program(),
            source.model().problem.events.action_conditions.programs()[0].as_slice()
        ));
        assert!(program.has_observations());
        let error = rumoca_exec_wasm::CompiledRetainedEventWasm::compile(program)
            .err()
            .expect("unsupported scalar fold must reject before an executable artifact exists");
        assert!(
            matches!(&error, rumoca_exec_wasm::WasmCompileError::Backend(message) if message.contains("does not yet support compact function-fold")),
            "{error}"
        );
    }
}

#[test]
#[cfg(not(target_arch = "wasm32"))]
fn outer_scalar_fold_native_carrier_control_keeps_first_actual_warning() {
    for second_passes in [true, false] {
        let source = outer_source(second_passes);
        let expected = reference_outer_messages(&source, second_passes);
        let calls =
            rumoca_exec_cranelift::compile_pure_call_table(&source.model().pure_calls).unwrap();
        let code = rumoca_exec_cranelift::compile_event_observation(
            source
                .event_program(solve::RetainedAssertionSource::ActionConditions, 0)
                .unwrap(),
            &calls,
        )
        .unwrap();
        let mut out = [123.0];
        code.call(&[], &[], 0.0, &[], &mut out).unwrap();
        assert_eq!(out, [1.0]);
        assert_eq!(
            code.message_for_action(&source.model().problem.events.actions[0]),
            Some(expected[0].clone())
        );
    }
}
