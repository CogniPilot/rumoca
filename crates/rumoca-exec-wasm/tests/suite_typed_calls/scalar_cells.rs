//! Single-cell typed-call ABI and raw payload regression coverage.
use super::*;

fn identity_call(
    scalar: solve::SolveScalarType,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let value_type = solve::SolveValueType::scalar(scalar);
    let mut table = solve::SolvePureCallTable::builder(profile());
    let owner = table
        .add_owner(
            identity(9001),
            vec![value_type.clone()],
            vec![solve::SolvePureCallOutput::result(value_type)],
            span(9001),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(9002))?;
                b.store(outputs[0], value, span(9003))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn scalar_identity_preserves_real_payloads_integer_extremes_and_boolean_cells() {
    let p = profile();
    for (scalar, patterns) in [
        (
            solve::SolveScalarType::real(p),
            vec![
                0,
                0x8000_0000_0000_0000,
                0x7ff8_dead_beef_1234,
                0xfff8_dead_beef_1234,
                0x7ff0_0000_0000_0001,
                0x7ff0_0000_0000_0000,
            ],
        ),
        (
            solve::SolveScalarType::integer(p),
            vec![0, 1, i64::MIN as u64, i64::MAX as u64, u64::MAX],
        ),
        (solve::SolveScalarType::Boolean, vec![0, 1]),
    ] {
        let (table, site) = identity_call(scalar);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        assert_eq!(compiled.layout().input_bytes, 8);
        assert_eq!(compiled.layout().output_bytes, 8);
        for payload in wasmparser::Parser::new(0).parse_all(compiled.module_bytes()) {
            let wasmparser::Payload::CodeSectionEntry(body) = payload.unwrap() else {
                continue;
            };
            for op in body.get_operators_reader().unwrap() {
                assert!(
                    !matches!(
                        op.unwrap(),
                        wasmparser::Operator::MemoryCopy { .. } | wasmparser::Operator::Loop { .. }
                    ),
                    "scalar identity must use straight-line cells"
                );
            }
        }
        let mut runner = Runner::new(&compiled);
        for bits in patterns {
            let input = bits.to_le_bytes();
            let (status, output) = runner.run(&input);
            assert_eq!(status, 0);
            assert_eq!(output, input);
        }
    }
}

#[test]
fn scalar_boolean_fault_and_buffer_guards_preserve_success_only_output_publication() {
    let (table, site) = identity_call(solve::SolveScalarType::Boolean);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let (status, output) = runner.run(&2u64.to_le_bytes());
    assert!(compiled.faults().iter().any(
        |fault| fault.status == status as u32 && fault.kind == TypedCallFaultKind::InvalidInput
    ));
    assert_eq!(output, vec![0xa5; 8]);
    assert_eq!(runner.run(&1u64.to_le_bytes()).0, 0);
    let before = runner.memory.data(&runner.store).to_vec();
    let end = before.len() as i32;
    for pointers in [
        (1, runner.output as i32, runner.scratch as i32),
        (0, 0, runner.scratch as i32),
        (0, runner.output as i32, runner.output as i32),
        (-8, runner.output as i32, runner.scratch as i32),
        (0, end, runner.scratch as i32),
        (0, runner.output as i32, end),
    ] {
        let status = runner.call.call(&mut runner.store, pointers).unwrap();
        assert!(
            compiled
                .faults()
                .iter()
                .any(|fault| fault.status == status as u32
                    && fault.kind == TypedCallFaultKind::InvalidBuffer)
        );
        assert_eq!(runner.memory.data(&runner.store), before);
    }
}
