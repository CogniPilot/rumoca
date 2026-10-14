//! Value products and observed invocations execute different issued contracts.
mod fixtures;
use super::*;
use crate::TypedCallFaultKind;
use fixtures::*;
use solve::LinearOp;

fn block(site: solve::SolvePureCallSite) -> solve::ScalarProgramBlock {
    let count = site.output_scalar_count().unwrap();
    let mut operations = vec![
        LinearOp::Const {
            dst: 0,
            value: -0.0,
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    operations.extend((0..site.inputs().len()).map(|index| LinearOp::LoadY {
        dst: 1 + index as u32,
        index,
    }));
    operations.push(LinearOp::PureCall {
        dst_start: 1 + site.inputs().len() as u32,
        input_starts: (1..=site.inputs().len() as u32).collect::<Vec<_>>().into(),
        site: site.clone(),
    });
    operations.push(LinearOp::StoreOutputRange {
        start: 1 + site.inputs().len() as u32,
        count,
        stride: 1,
    });
    solve::ScalarProgramBlock::with_program_spans(vec![operations], vec![span(70)]).unwrap()
}

fn compiled(
    source: &solve::ScalarProgramBlock,
    table: &solve::SolvePureCallTable,
    inputs: usize,
    directional: bool,
) -> crate::CompiledPrivateProgramWasm {
    let layout = solve::VarLayout::from_parts(Default::default(), inputs, 0);
    if directional {
        crate::compile_private_directional_program_wasm(source, &layout, table, 1).unwrap()
    } else {
        crate::compile_private_program_wasm(source, &layout, table).unwrap()
    }
}

fn reference(
    source: &solve::ScalarProgramBlock,
    table: &solve::SolvePureCallTable,
    y: &[f64],
    seed: Option<&[f64]>,
) -> Vec<f64> {
    let mut expected = vec![0.0; source.output_count()];
    rumoca_eval_solve::PreparedScalarProgramBlock::new(source.clone())
        .unwrap()
        .eval_with_context(
            y,
            &[],
            0.0,
            rumoca_eval_solve::RowEvalContext {
                seed,
                pure_calls: Some(table),
                ..Default::default()
            },
            &mut expected,
        )
        .unwrap();
    expected
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

#[test]
fn value_rows_publish_failed_only_messages_at_both_levels_and_keep_standalone_refusal() {
    for level in [
        solve::SolveAssertionLevel::Error,
        solve::SolveAssertionLevel::Warning,
    ] {
        let table = real_table(level);
        let site = table.owners()[0].call_site();
        assert!(matches!(
            crate::compile_pure_call_wasm(&table, &site),
            Err(TypedCallCompileError::UnsupportedOutputKind { .. })
        ));
        let source = block(site.clone());
        let code = compiled(&source, &table, 2, false);
        let native = rumoca_exec_cranelift::compile_pure_call_table(&table).unwrap();
        let invocation = rumoca_eval_solve::PureCallInvocation::Primal(&site);
        assert!(native.executes(invocation));
        let mut cells = vec![19.0; code.scratch_bytes() as usize / 8];
        for condition in [0.0, 1.0, 0.0] {
            let y = [1.25, condition];
            let expected = reference(&source, &table, &y, None);
            code.call(&y, &[], 0.0, &mut cells).unwrap();
            assert_eq!(bits(&cells[..expected.len()]), bits(&expected));
            let mut actual = vec![0.0; 3];
            native
                .call_scalar_payload(
                    invocation,
                    &y,
                    &mut actual,
                    &mut Vec::new(),
                    &mut Vec::new(),
                )
                .unwrap();
            assert_eq!(bits(&actual), bits(&expected[1..]));
        }
    }
}

#[test]
fn nonrepresentable_integer_message_refuses_scalar_projection_without_rounding_or_commit() {
    let table = simple_table(solve::SolveAssertionLevel::Error, false, false);
    let source = block(table.owners()[0].call_site());
    let code = compiled(&source, &table, 1, false);
    let mut expected = vec![0.0; source.output_count()];
    let error = rumoca_eval_solve::PreparedScalarProgramBlock::new(source.clone())
        .unwrap()
        .eval_with_context(
            &[0.0],
            &[],
            0.0,
            rumoca_eval_solve::RowEvalContext {
                pure_calls: Some(&table),
                ..Default::default()
            },
            &mut expected,
        )
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("Integer result is not exactly representable in the scalar ABI")
    );
    let mut cells = vec![19.0; code.scratch_bytes() as usize / 8];
    assert!(code.call(&[0.0], &[], 0.0, &mut cells).is_err());
    assert_eq!(
        cells[..source.output_count()],
        vec![19.0; source.output_count()]
    );
    let native = rumoca_exec_cranelift::compile_pure_call_table(&table).unwrap();
    let site = table.owners()[0].call_site();
    let invocation = rumoca_eval_solve::PureCallInvocation::Primal(&site);
    assert!(native.executes(invocation));
    let mut native_output = [19.0; 3];
    assert!(
        native
            .call_scalar_payload(
                invocation,
                &[0.0],
                &mut native_output,
                &mut Vec::new(),
                &mut Vec::new()
            )
            .is_err()
    );
    assert_eq!(native_output, [19.0; 3]);
    code.call(&[1.0], &[], 0.0, &mut cells).unwrap();
    assert_eq!(
        bits(&cells[..source.output_count()]),
        bits(&reference(&source, &table, &[1.0], None))
    );
}

#[test]
fn value_message_fault_is_lazy_source_bound_and_publishes_no_partial_tuple() {
    let table = simple_table(solve::SolveAssertionLevel::Error, true, false);
    let source = block(table.owners()[0].call_site());
    let code = compiled(&source, &table, 1, false);
    let mut cells = vec![19.0; code.scratch_bytes() as usize / 8];
    code.call(&[1.0], &[], 0.0, &mut cells).unwrap();
    assert_eq!(
        bits(&cells[..source.output_count()]),
        bits(&reference(&source, &table, &[1.0], None))
    );
    let before = bits(&cells[..source.output_count()]);
    let error = code.call(&[0.0], &[], 0.0, &mut cells).unwrap_err();
    assert!(
        matches!(error, WasmCompileError::TypedSource(ref fault)
        if fault.kind == TypedCallFaultKind::IntegerArithmetic && fault.provenance == span(6)),
        "{error}"
    );
    assert_eq!(bits(&cells[..source.output_count()]), before);
}

#[test]
fn ordinary_value_continues_to_later_fault_but_checked_observation_stops_first() {
    let table = simple_table(solve::SolveAssertionLevel::Error, false, true);
    let site = table.owners()[0].call_site();
    let source = block(site.clone());
    let code = compiled(&source, &table, 1, false);
    let mut cells = vec![19.0; code.scratch_bytes() as usize / 8];
    let error = code.call(&[0.0], &[], 0.0, &mut cells).unwrap_err();
    assert!(
        matches!(error, WasmCompileError::TypedSource(ref fault)
        if fault.kind == TypedCallFaultKind::IntegerArithmetic && fault.provenance == span(10)),
        "{error}"
    );
    assert_eq!(
        cells[..source.output_count()],
        vec![19.0; source.output_count()]
    );
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let observed = compile_checked_assertion_wasm(checked).unwrap();
    let mut runner = IssuedExecution::new(&observed);
    let CheckedAssertionWasmOutcome::Stopped { observations } = runner.call(&observed, &[0]) else {
        panic!("checked error assertion must stop before the later integer fault");
    };
    assert_eq!(observations.len(), 1);
    assert_eq!(observations[0].source_span(), span(4));
}

#[test]
fn absent_assigned_value_reads_publish_neutral_payload_without_reading_old_scratch() {
    let table = absent_table();
    let owner = &table.owners()[0];
    assert_eq!(
        owner.assertion_flow().unwrap().assigned_read_predicates(),
        [1]
    );
    let source = block(owner.call_site());
    let code = compiled(&source, &table, 0, false);
    let expected = reference(&source, &table, &[], None);
    let mut cells = vec![19.0; code.scratch_bytes() as usize / 8];
    for _ in 0..2 {
        code.call(&[], &[], 0.0, &mut cells).unwrap();
        assert_eq!(bits(&cells[..expected.len()]), bits(&expected));
        assert_eq!(cells[1].to_bits(), 0.0f64.to_bits());
    }
}

#[test]
fn nested_directional_value_assertions_keep_source_output_roles_and_changed_seeds() {
    let table = real_table(solve::SolveAssertionLevel::Error);
    let source = directional_block(table.owners()[1].call_site());
    let code = compiled(&source, &table, 2, true);
    let native_calls = rumoca_exec_cranelift::compile_pure_call_table(&table).unwrap();
    let site = table.owners()[1].call_site();
    assert!(native_calls.executes(rumoca_eval_solve::PureCallInvocation::Primal(&site)));
    assert!(
        native_calls.executes(rumoca_eval_solve::PureCallInvocation::Directional(
            site.directional().unwrap(),
        ))
    );
    let native = rumoca_exec_cranelift::compile_jacobian_scalar_program_block_with_pure_calls(
        &source,
        &native_calls,
    )
    .unwrap();
    let mut cells = vec![19.0; code.scratch_bytes() as usize / 8];
    for y in [[-3.5, 0.0], [2.75, 1.0], [-0.0, 0.0]] {
        for seed in [-2.5, 0.0, -0.0, 7.25] {
            let seed = [seed];
            let expected = reference(&source, &table, &y, Some(&seed));
            code.call_directional(&y, &[], 0.0, &seed, &mut cells)
                .unwrap();
            assert_eq!(bits(&cells[..expected.len()]), bits(&expected));
            let mut actual = vec![19.0; expected.len()];
            native.call(&y, &[], 0.0, &seed, &mut actual).unwrap();
            assert_eq!(bits(&actual), bits(&expected));
        }
    }
}
