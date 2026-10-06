use super::*;

fn fault_artifact(storage: arena::ArenaStorage) -> CompiledNativeCallProgramWasm {
    let span = solve::source_span_from_offsets(11, 44, 50);
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let mut calls = solve::SolvePureCallTable::builder(arithmetic);
    let owner = calls
        .add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span,
            |body, inputs, outputs| {
                let input = body.load(inputs[0], span)?;
                let integer = body.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    input,
                    span,
                )?;
                let output =
                    body.convert(solve::SolveConversionOperator::IntegerToReal, integer, span)?;
                body.store(outputs[0], output, span)
            },
        )
        .unwrap();
    let site = calls.call_site(owner).unwrap();
    let calls = calls.finish();
    let block = solve::ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::PureCall {
                dst_start: 2,
                input_starts: vec![1].into_boxed_slice(),
                site,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        vec![span],
    )
    .unwrap();
    let layout = VarLayout::from_parts(Default::default(), 1, 1);
    emit_module_with_storage(
        Ready::private(&block, &layout, &calls).unwrap(),
        &layout,
        true,
        storage,
    )
    .unwrap()
}

#[test]
fn pooled_prefix_faults_keep_exact_status_span_inputs_and_recovery_vs_defined_memory() {
    let pooled = fault_artifact(arena::ArenaStorage::Pooled);
    let defined = fault_artifact(arena::ArenaStorage::Defined);
    let fault = pooled
        .faults()
        .iter()
        .find(|fault| fault.kind == crate::TypedCallFaultKind::IntegerConversion)
        .unwrap();
    assert_eq!(
        fault.provenance,
        solve::source_span_from_offsets(11, 44, 50)
    );
    let mut runner = Runner::new();
    let pooled_call = runner.instance(&pooled, Some(65536));
    let defined_call = runner.instance(&defined, None);
    for value in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY, 1e100, -3.75] {
        let inputs = [-0., value];
        let expected = runner.run(defined_call, inputs);
        let actual = runner.run(pooled_call, inputs);
        assert_eq!(actual, expected);
        assert_eq!(actual.0 != 0, !value.is_finite() || value.abs() > 1e50);
        if actual.0 != 0 {
            assert_eq!(actual.0 as u32, fault.status);
        }
        assert_eq!(runner.inputs(), inputs.map(f64::to_bits));
    }
}
