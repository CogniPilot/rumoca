use super::*;
use rumoca_eval_solve::{TypedValue, eval_pure_call};

/// One owner wrapping the xorshift128+ row with the Modelica output order
/// `(result, stateOut)`, which differs from the catalog's interface order.
fn table() -> solve::SolvePureCallTable {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let state = solve::SolveValueType::tensor(solve::SolveScalarType::integer(arithmetic), vec![4])
        .unwrap();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_body.mo"),
        1,
        2,
    );
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(902).unwrap()),
            vec![state.clone()],
            vec![
                solve::SolvePureCallOutput::result(real),
                solve::SolvePureCallOutput::result(state),
            ],
            at,
            |builder, inputs, outputs| {
                let state_in = builder.load(inputs[0], at)?;
                let values = builder.native(NativeBody::Xorshift128Plus, &[state_in], at)?;
                builder.store(outputs[0], values[1], at)?;
                builder.store(outputs[1], values[0], at)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

/// The compiled host call and the typed evaluator compute the catalog row's
/// value bit for bit, including a state element outside the C `int` range.
#[test]
fn native_body_matches_the_definitional_evaluator() {
    let table = table();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    let state = [17_i64, -3, 1 << 40, i64::from(i32::MIN)];
    let mut outputs = [0_u64; 5];
    compiled
        .call_cells(
            rumoca_eval_solve::PureCallInvocation::Primal(&site),
            &state.map(|value| value as u64),
            &mut outputs,
        )
        .unwrap();
    let operands = state.map(NativeScalar::Integer);
    let expected = NativeBody::Xorshift128Plus.evaluate(&[&operands]).unwrap();
    let [NativeScalar::Real(result)] = expected[1].as_slice() else {
        panic!("one Real result");
    };
    assert_eq!(outputs[0], result.to_bits());
    let state_out = expected[0]
        .iter()
        .map(|value| match value {
            NativeScalar::Integer(value) => *value,
            NativeScalar::Real(_) => panic!("Integer state"),
        })
        .collect::<Vec<_>>();
    let cells = state_out
        .iter()
        .map(|value| *value as u64)
        .collect::<Vec<_>>();
    assert_eq!(&outputs[1..], cells.as_slice());

    let owner = &table.owners()[0];
    let argument = TypedValue::construct(
        owner.inputs()[0].clone(),
        state.map(solve::SolveValueKind::Integer).to_vec(),
    )
    .unwrap();
    let evaluated = eval_pure_call(&table, owner.id(), &[argument]).unwrap();
    assert_eq!(
        evaluated[0].elements(),
        &[solve::SolveValueKind::Real64(result.to_bits())]
    );
    let integers = state_out
        .into_iter()
        .map(solve::SolveValueKind::Integer)
        .collect::<Vec<_>>();
    assert_eq!(evaluated[1].elements(), integers.as_slice());
}

#[test]
fn host_entry_refuses_an_unknown_body_code() {
    let input = [0_u64; 4];
    let mut output = [0_u64; 5];
    // SAFETY: both tapes are large enough for every catalog row used here.
    let status = unsafe { rumoca_host_native(-1, input.as_ptr(), output.as_mut_ptr()) };
    assert_eq!(status, status::NATIVE_BODY_FAILURE);
    assert_eq!(native_body_code(NativeBody::Xorshift1024Star), 2);
}

/// An owner drawing from the threaded `ModelicaRandom` cell.
fn draw_table() -> solve::SolvePureCallTable {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let integer = solve::SolveScalarType::integer(arithmetic);
    let cell = solve::SolveValueType::tensor(integer, vec![34]).unwrap();
    let id = solve::SolveValueType::scalar(integer);
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_draw.mo"),
        1,
        2,
    );
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(903).unwrap()),
            vec![id, cell.clone()],
            vec![
                solve::SolvePureCallOutput::result(real),
                solve::SolvePureCallOutput::result(cell),
            ],
            at,
            |builder, inputs, outputs| {
                let id = builder.load(inputs[0], at)?;
                let cell = builder.load(inputs[1], at)?;
                let values =
                    builder.native(NativeBody::Xorshift1024StarImpureDraw, &[id, cell], at)?;
                builder.store(outputs[0], values[0], at)?;
                builder.store(outputs[1], values[1], at)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

/// The foreign body's error is a typed failure in both the compiled host call
/// and the typed evaluator, never a value.
#[test]
fn a_foreign_error_fails_the_call_in_every_evaluator() {
    let table = draw_table();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    let mut inputs = vec![7_u64];
    inputs.extend([0_u64; 34]);
    let mut outputs = [0_u64; 35];
    let error = compiled
        .call_cells(
            rumoca_eval_solve::PureCallInvocation::Primal(&site),
            &inputs,
            &mut outputs,
        )
        .unwrap_err();
    assert!(error.to_string().contains("foreign body"), "{error}");

    let owner = &table.owners()[0];
    let arguments = [
        TypedValue::construct(
            owner.inputs()[0].clone(),
            vec![solve::SolveValueKind::Integer(7)],
        )
        .unwrap(),
        TypedValue::construct(
            owner.inputs()[1].clone(),
            vec![solve::SolveValueKind::Integer(0); 34],
        )
        .unwrap(),
    ];
    let error = eval_pure_call(&table, owner.id(), &arguments).unwrap_err();
    assert!(error.to_string().contains("not initialized"), "{error}");
    assert!(error.source_span().is_some());
}
