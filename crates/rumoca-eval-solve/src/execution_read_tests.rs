//! Checked-Solve fixtures: explicit finite guards are introduced below DAE.
use rumoca_ir_solve as solve;

pub(crate) fn guarded_table() -> solve::SolvePureCallTable {
    let profile = solve::SolvePureCallTable::default().arithmetic();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let span = solve::source_span_from_offsets(813, 1, 2);
    solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(813).unwrap()),
            vec![real.clone(), real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let q = b.load(inputs[1], span)?;
                b.check_finite(
                    q,
                    solve::SolveFiniteCheck::Primal {
                        program: 7,
                        operation: 3,
                    },
                    span,
                )?;
                b.store(outputs[0], x, span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

pub(crate) fn guarded_program(q: solve::LinearOp) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 1.0 },
        q,
        solve::LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: guarded_table().owners()[0].call_site(),
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

#[test]
fn a_guard_only_input_changes_the_checked_execution_outcome() {
    let table = guarded_table();
    let owner = &table.owners()[0];
    let input = |index: usize, value: f64| {
        crate::TypedValue::construct(
            owner.inputs()[index].clone(),
            vec![solve::SolveValueKind::Real64(value.to_bits())],
        )
        .unwrap()
    };
    let result =
        crate::eval_pure_call_checked(&table, owner.id(), &[input(0, 2.0), input(1, 1.0)]).unwrap();
    assert_eq!(
        result[0].elements(),
        &[solve::SolveValueKind::Real64(2.0f64.to_bits())]
    );
    let error =
        crate::eval_pure_call_checked(&table, owner.id(), &[input(0, 2.0), input(1, f64::NAN)])
            .unwrap_err();
    assert!(matches!(
        error,
        crate::TypedProgramEvalError::NonFinite {
            check: solve::SolveFiniteCheck::Primal {
                program: 7,
                operation: 3
            },
            element: 0,
            ..
        }
    ));
    assert_eq!(
        error.source_span(),
        Some(solve::source_span_from_offsets(813, 1, 2))
    );
}
