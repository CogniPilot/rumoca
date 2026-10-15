//! Ordinary value execution and observed diagnostics are distinct source forms.
use super::*;
use rumoca_eval_solve::{PureCallInvocation, TypedValue, eval_pure_call};

fn reference_cells(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
    input: &[u64],
) -> Vec<u64> {
    let arguments = site
        .inputs()
        .iter()
        .zip(input)
        .map(|(ty, &bits)| {
            let kind = match ty.element_type() {
                solve::SolveScalarType::Boolean => solve::SolveValueKind::Boolean(bits != 0),
                solve::SolveScalarType::Integer(_) => solve::SolveValueKind::Integer(bits as i64),
                _ => panic!("fixture uses only Boolean/Integer inputs"),
            };
            TypedValue::construct(ty.clone(), vec![kind]).unwrap()
        })
        .collect::<Vec<_>>();
    eval_pure_call(table, site.owner(), &arguments)
        .unwrap()
        .iter()
        .flat_map(|value| {
            value.elements().iter().map(|kind| match kind {
                solve::SolveValueKind::Boolean(v) => u64::from(*v),
                solve::SolveValueKind::Integer(v) => *v as u64,
                _ => panic!("fixture uses only Boolean/Integer outputs"),
            })
        })
        .collect()
}

fn native_control(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
    input: &[u64],
    expected: &[u64],
) {
    let native = rumoca_exec_cranelift::compile_pure_call_table(table).unwrap();
    let input = site
        .inputs()
        .iter()
        .zip(input)
        .map(|(ty, &bits)| match ty.element_type() {
            solve::SolveScalarType::Boolean => bits as f64,
            solve::SolveScalarType::Integer(_) => bits as i64 as f64,
            _ => unreachable!(),
        })
        .collect::<Vec<_>>();
    let mut output = vec![0.0; expected.len()];
    assert!(native.executes(PureCallInvocation::Primal(site)));
    native
        .call_scalar_payload(
            PureCallInvocation::Primal(site),
            &input,
            &mut output,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    let expected = site
        .outputs()
        .iter()
        .zip(expected)
        .map(|(slot, &bits)| match slot.value_type().element_type() {
            solve::SolveScalarType::Boolean => bits as f64,
            solve::SolveScalarType::Integer(_) => bits as i64 as f64,
            _ => unreachable!(),
        })
        .collect::<Vec<_>>();
    assert_eq!(output, expected);
}

fn no_capture_loop_table(level: solve::SolveAssertionLevel) -> solve::SolvePureCallTable {
    let p = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let outputs = || {
        vec![
            solve::SolvePureCallOutput::result(integer.clone()),
            solve::SolvePureCallOutput::assertion_predicate_at_level(level),
        ]
    };
    let mut table = solve::SolvePureCallTable::builder(p);
    let child = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(501).unwrap()),
            vec![
                integer.clone(),
                solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
            ],
            outputs(),
            span(1),
            |b, input, out| {
                let value = b.load(input[0], span(2))?;
                let condition = b.load(input[1], span(3))?;
                let assertion = b.assertion_output(1, span(4))?;
                b.check_assertion(assertion, condition, &[], span(4), |_, _, outputs| {
                    assert!(outputs.is_empty());
                    Ok(())
                })?;
                b.store(out[0], value, span(5))?;
                b.store(out[1], condition, span(6))
            },
        )
        .unwrap();
    table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(502).unwrap()),
            vec![solve::SolveValueType::scalar(
                solve::SolveScalarType::Boolean,
            )],
            outputs(),
            span(30),
            |b, inputs, out| {
                let holds = b.load(inputs[0], span(30))?;
                let zero = b.constant(solve::SolveValue::integer(p, 0).unwrap(), span(31))?;
                let result = b.fold(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            provenance: rumoca_core::IndexBinderProvenance::Generated,
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper: 3,
                            step: 1,
                        }],
                    },
                    &[zero],
                    &[holds],
                    span(32),
                    |r, _, captures, binders, outputs| {
                        let i = r.load(binders[0], span(33))?;
                        let holds = r.load(captures[0], span(33))?;
                        let call = r.emit_call(child, &[i, holds], span(34))?;
                        let predicate = r.assertion_output(1, span(35))?;
                        r.forward_assertion(&call, 1, predicate, span(36))?;
                        r.store(outputs[0], call.registers()[0], span(37))
                    },
                )?;
                let inactive = b.constant(solve::SolveValue::boolean(true), span(38))?;
                b.store(out[0], result[0], span(39))?;
                b.store(out[1], inactive, span(40))
            },
        )
        .unwrap();
    table.finish()
}

fn numeric_program(site: solve::SolvePureCallSite) -> solve::ScalarProgramBlock {
    let count = site.output_scalar_count().unwrap();
    solve::ScalarProgramBlock::with_program_spans(
        vec![vec![
            solve::LinearOp::Const {
                dst: 0,
                value: -0.0,
            },
            solve::LinearOp::StoreOutput { src: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::PureCall {
                dst_start: 2,
                input_starts: vec![1].into(),
                site,
            },
            solve::LinearOp::StoreOutputRange {
                start: 2,
                count,
                stride: 1,
            },
        ]],
        vec![span(70)],
    )
    .unwrap()
}

#[test]
fn private_numeric_no_capture_fold_keeps_scalar_boolean_conversion_and_input_atomicity() {
    let table = no_capture_loop_table(solve::SolveAssertionLevel::Error);
    let site = table.owners()[1].call_site();
    let source = numeric_program(site.clone());
    let reference = rumoca_eval_solve::PreparedScalarProgramBlock::new(source.clone()).unwrap();
    for input in [0, 1, 2] {
        let expected = reference_cells(&table, &site, &[input]);
        // Nested diagnostic slots stay inactive in the ordinary value tuple.
        assert_eq!(expected, [3, 1]);
        native_control(&table, &site, &[input], &expected);
        let mut values = vec![19.0; source.output_count()];
        reference
            .eval_with_context(
                &[input as f64],
                &[],
                0.0,
                rumoca_eval_solve::RowEvalContext {
                    pure_calls: Some(&table),
                    ..Default::default()
                },
                &mut values,
            )
            .unwrap();
        assert_eq!(
            values.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            [-0.0f64, 3.0, 1.0].map(f64::to_bits)
        );
    }
    let layout = solve::VarLayout::from_parts(Default::default(), 1, 0);
    let code = crate::compile_private_program_wasm(&source, &layout, &table)
        .expect("the canonical private numeric value form must lower");
    let mut scratch = vec![19.0; code.scratch_bytes() as usize / 8];
    for input in [0.0, 1.0, 2.0, 0.0] {
        code.call(&[input], &[], 0.0, &mut scratch).unwrap();
        assert_eq!(
            scratch[..3].iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            [-0.0f64, 3.0, 1.0].map(f64::to_bits)
        );
    }
    scratch.fill(19.0);
    let before = scratch.iter().map(|v| v.to_bits()).collect::<Vec<_>>();
    let error = code.call(&[], &[], 0.0, &mut scratch).unwrap_err();
    assert!(
        matches!(error, crate::WasmCompileError::Input(_)),
        "{error:?}"
    );
    assert_eq!(
        scratch.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
        before
    );
    code.call(&[1.0], &[], 0.0, &mut scratch).unwrap();
    assert_eq!(
        scratch[..3].iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
        [-0.0f64, 3.0, 1.0].map(f64::to_bits)
    );
}

#[test]
fn original_no_capture_loop_observation_keeps_fatal_stop_and_repeated_warnings() {
    for level in [
        solve::SolveAssertionLevel::Error,
        solve::SolveAssertionLevel::Warning,
    ] {
        let table = no_capture_loop_table(level);
        let site = table.owners()[1].call_site();
        let program = invocation_program(site.clone());
        let actions = [action(site, level)];
        let checked =
            solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
        let compiled = compile_checked_assertion_wasm(checked).unwrap();
        let mut execution = IssuedExecution::new(&compiled);
        let observations = match execution.call(&compiled, &[0]) {
            CheckedAssertionWasmOutcome::Stopped { observations }
                if level == solve::SolveAssertionLevel::Error =>
            {
                observations
            }
            CheckedAssertionWasmOutcome::Complete { observations, .. }
                if level == solve::SolveAssertionLevel::Warning =>
            {
                observations
            }
            _ => panic!("observation mode changed its source stop policy"),
        };
        assert_eq!(
            observations.len(),
            if level == solve::SolveAssertionLevel::Error {
                1
            } else {
                3
            }
        );
        for observation in observations {
            assert!(std::ptr::eq(observation.action().action(), &actions[0]));
            assert_eq!(observation.source_span(), span(4));
            assert!(observation.captures().is_empty());
        }
        let CheckedAssertionWasmOutcome::Fault {
            fault,
            observations,
        } = execution.call(&compiled, &[2])
        else {
            panic!("invalid packed Boolean bits must not execute the assertion body");
        };
        assert_eq!(fault.kind, crate::TypedCallFaultKind::InvalidInput);
        assert!(observations.is_empty());
        assert!(matches!(
            execution.call(&compiled, &[1]),
            CheckedAssertionWasmOutcome::Complete { observations, .. } if observations.is_empty()
        ));
    }
}
