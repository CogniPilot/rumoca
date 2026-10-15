use super::*;
use rumoca_ir_solve as solve;
use rumoca_solver::CompiledSolveEventTransaction;

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("mixed_event.mo"),
        17,
        29,
    )
}

fn transaction() -> (EventTransactionProgram, SolvePureCallTable) {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let types = vec![
        solve::SolveValueType::scalar(SolveScalarType::real(arithmetic)),
        solve::SolveValueType::scalar(SolveScalarType::Integer(arithmetic.integer_domain())),
        solve::SolveValueType::scalar(SolveScalarType::Boolean),
    ];
    let mut table = SolvePureCallTable::builder(arithmetic);
    let owner = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            types.clone(),
            types
                .iter()
                .cloned()
                .map(solve::SolvePureCallOutput::result)
                .collect(),
            span(),
            |body, inputs, outputs| {
                let real = body.load(inputs[0], span())?;
                body.store(outputs[0], real, span())?;
                let integer = body.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    real,
                    span(),
                )?;
                let input_integer = body.load(inputs[1], span())?;
                let sum = body.binary(
                    solve::SolveBinaryOperator::Add,
                    integer,
                    input_integer,
                    span(),
                )?;
                body.store(outputs[1], sum, span())?;
                let boolean = body.load(inputs[2], span())?;
                let negated = body.unary(solve::SolveUnaryOperator::Not, boolean, span())?;
                body.store(outputs[2], negated, span())
            },
        )
        .unwrap();
    let program = EventTransactionProgram::checked(
        solve::EventTransactionConstruction {
            site: table.call_site(owner).unwrap(),
            inputs: types
                .iter()
                .cloned()
                .enumerate()
                .map(|(i, kind)| (solve::scalar_slot_p(i + 3), kind))
                .collect(),
            targets: types
                .into_iter()
                .enumerate()
                .map(|(i, kind)| (solve::scalar_slot_p(i), kind, None))
                .collect(),
            producer_owners: (0..3)
                .map(|start_row| solve::EventTransactionProducerOwner::ScalarRows { start_row })
                .collect(),
            assertions: Vec::new(),
            assertion_action_indices: Vec::new(),
            statement_count: 3,
            clock_owners: Vec::new(),
        },
        span().require_provenance("mixed event fixture").unwrap(),
    )
    .unwrap();
    (program, table.finish())
}

#[test]
fn compiled_mixed_event_transaction_preserves_typed_values_fault_atomicity_and_recovery() {
    let (program, table) = transaction();
    let code = WasmEventTransaction::compile(&program, &table).unwrap();
    let mut out = [19.0; 3];
    code.call(&[2.75, 5.0, 1.0], &mut out).unwrap();
    assert_eq!(out, [2.75, 7.0, 0.0]);
    for input in [[f64::NAN, 5.0, 0.0], [f64::INFINITY, 5.0, 0.0]] {
        out.fill(19.0);
        let error = code.call(&input, &mut out).unwrap_err();
        assert_eq!(error.source_span(), Some(span()));
        assert_eq!(out, [19.0; 3]);
    }
    assert!(code.call(&[2.75, 5.5, 1.0], &mut out).is_err());
    assert_eq!(out, [19.0; 3]);
    // Scalar solver coordinates encode truth by zero/nonzero; this is not
    // the typed Boolean-cell ABI, which admits only the exact words 0 and 1.
    code.call(&[2.75, 5.0, 2.0], &mut out).unwrap();
    assert_eq!(out, [2.75, 7.0, 0.0]);
    let mut words = [19u64; 3];
    let mut scratch = vec![0; code.compiled.artifact().layout().scratch_bytes as usize / 8];
    let invalid = code
        .compiled
        .call(&[2.75f64.to_bits(), 5, 2], &mut words, &mut scratch)
        .unwrap_err();
    assert!(
        matches!(invalid, rumoca_exec_wasm::WasmCompileError::TypedSource(fault)
        if fault.kind == rumoca_exec_wasm::TypedCallFaultKind::InvalidInput)
    );
    assert_eq!(words, [19; 3]);
    assert!(code.call(&[2.75, 5.0], &mut out).is_err());
    let mut short = [19.0; 2];
    assert!(code.call(&[2.75, 5.0, 1.0], &mut short).is_err());
    assert_eq!(short, [19.0; 2]);
    code.call(&[-0.0, 5.0, 0.0], &mut out).unwrap();
    assert_eq!(
        out.map(f64::to_bits),
        [(-0.0_f64).to_bits(), 5.0_f64.to_bits(), 1.0_f64.to_bits()]
    );
    let foreign = SolvePureCallTable::default();
    assert!(WasmEventTransaction::compile(&program, &foreign).is_err());
}
