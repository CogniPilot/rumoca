//! Canonical broadcast operators share scalar numeric semantics and fault guards.
mod fixtures;
use super::*;
use fixtures::*;

#[test]
pub(crate) fn broadcast_real_operators_preserve_direct_quotients_and_declared_imports() {
    use solve::SolveBinaryOperator as B;
    for operator in [
        B::Add,
        B::Subtract,
        B::Multiply,
        B::Divide,
        B::Min,
        B::Max,
        B::Power,
        B::Atan2,
    ] {
        for scalar_left in [false, true] {
            let (table, site) = broadcast_table(
                profile(),
                solve::SolveScalarType::real(profile()),
                vec![2, 2],
                operator,
                scalar_left,
            );
            let compiled = compile_pure_call_wasm(&table, &site).unwrap();
            let imports: &[&str] = match operator {
                B::Power => &["pow"],
                B::Atan2 => &["atan2"],
                _ => &[],
            };
            assert_eq!(compiled.math_imports(), imports);
            let mut runner = Runner::new(&compiled);
            check_real_cases(&mut runner, operator, scalar_left);
        }
    }
}

fn ordered<T>(scalar_left: bool, scalar: T, element: T) -> (T, T) {
    if scalar_left {
        (scalar, element)
    } else {
        (element, scalar)
    }
}

fn check_real_cases(runner: &mut Runner, operator: solve::SolveBinaryOperator, scalar_left: bool) {
    for (aggregate, scalar) in real_cases() {
        let (status, actual) = runner.run(&cells(aggregate.into_iter().chain([scalar]).map(real)));
        assert_eq!(status, 0);
        for (&element, actual) in aggregate.iter().zip(actual.chunks_exact(8)) {
            let (lhs, rhs) = ordered(scalar_left, scalar, element);
            check_real(operator, lhs, rhs, actual);
        }
    }
}

#[test]
pub(crate) fn broadcast_division_full_tensor_keeps_every_direct_binary64_cell() {
    for scalar_left in [false, true] {
        let (table, site) = broadcast_table(
            profile(),
            solve::SolveScalarType::real(profile()),
            vec![90, 160],
            solve::SolveBinaryOperator::Divide,
            scalar_left,
        );
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let values = (0..14400)
            .map(|i| [347.0, 394.0, 439.0, 484.0][i % 4])
            .collect::<Vec<_>>();
        let (status, actual) =
            Runner::new(&compiled).run(&cells(values.iter().copied().chain([3.0]).map(real)));
        assert_eq!(status, 0);
        for (&value, cell) in values.iter().zip(actual.chunks_exact(8)) {
            let expected: f64 = if scalar_left {
                3.0 / value
            } else {
                value / 3.0
            };
            assert_eq!(cell, expected.to_bits().to_le_bytes());
        }
    }
}

#[test]
pub(crate) fn broadcast_integer_operators_match_independent_exact_cells() {
    use solve::SolveBinaryOperator as B;
    for operator in [
        B::Add,
        B::Subtract,
        B::Multiply,
        B::Min,
        B::Max,
        B::IntegerQuotient,
        B::IntegerModulo,
        B::IntegerRemainder,
    ] {
        for scalar_left in [false, true] {
            let (table, site) = broadcast_table(
                profile(),
                solve::SolveScalarType::integer(profile()),
                vec![4],
                operator,
                scalar_left,
            );
            let compiled = compile_pure_call_wasm(&table, &site).unwrap();
            let aggregate = [-7, -1, 4, 7];
            let inputs = [integer_values(&aggregate), integer_values(&[3])];
            let expected = cells(aggregate.map(|value| {
                let (lhs, rhs) = ordered(scalar_left, 3, value);
                solve::SolveValueKind::Integer(independent_integer(operator, lhs, rhs).unwrap())
            }));
            assert_eq!(oracle(&table, &site, &inputs).unwrap(), expected);
            assert_eq!(
                Runner::new(&compiled).run(&cells(inputs.into_iter().flatten())),
                (0, expected)
            );
        }
    }
}

#[test]
pub(crate) fn broadcast_integer_late_faults_are_atomic_source_mapped_and_recover() {
    use solve::SolveBinaryOperator as B;
    for (operator, values, scalar) in [
        (B::Add, [2, 4, 6, i64::MAX], 1),
        (B::Subtract, [2, 4, 6, i64::MIN], 1),
        (B::Multiply, [2, 4, 6, i64::MAX], 2),
        (B::IntegerQuotient, [2, 4, 6, i64::MIN], -1),
        (B::IntegerQuotient, [2, 4, 6, 7], 0),
        (B::IntegerModulo, [2, 4, 6, 7], 0),
        (B::IntegerRemainder, [2, 4, 6, 7], 0),
    ] {
        check_integer_failure(profile(), operator, &values, scalar);
    }
    let bounded = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(-10, 10).unwrap(),
    );
    check_integer_failure(bounded, B::Add, &[2, 4, 6, 10], 1);
}

#[test]
pub(crate) fn broadcast_boolean_operators_and_unadmitted_combinations_are_explicit() {
    use solve::SolveBinaryOperator as B;
    for operator in [B::And, B::Or] {
        for scalar_left in [false, true] {
            let (table, site) = broadcast_table(
                profile(),
                solve::SolveScalarType::Boolean,
                vec![4],
                operator,
                scalar_left,
            );
            let compiled = compile_pure_call_wasm(&table, &site).unwrap();
            let mut runner = Runner::new(&compiled);
            for scalar in [false, true] {
                let values = [false, true, true, false];
                let expected = boolean_cells(operator, scalar, values);
                let inputs = values
                    .into_iter()
                    .chain([scalar])
                    .map(solve::SolveValueKind::Boolean);
                assert_eq!(runner.run(&cells(inputs)), (0, expected));
            }
            let (status, output) = runner.run(&cells(
                [0u64, 1, 1, 0, 2].map(|v| solve::SolveValueKind::Integer(v as i64)),
            ));
            assert!(
                compiled.faults().iter().any(
                    |f| f.status == status as u32 && f.kind == TypedCallFaultKind::InvalidInput
                )
            );
            assert_eq!(output, vec![0xa5; 32]);
        }
    }
    for operator in [B::Min, B::Max] {
        let (table, site) = broadcast_table(
            profile(),
            solve::SolveScalarType::Boolean,
            vec![4],
            operator,
            false,
        );
        assert!(matches!(
            compile_pure_call_wasm(&table, &site),
            Err(TypedCallCompileError::UnsupportedOperation {
                opcode: "broadcast binary",
                operation: 2,
                ..
            })
        ));
    }
    let p32 = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary32,
        solve::SolveIntegerDomain::FULL,
    );
    let (table, site) = broadcast_table(
        p32,
        solve::SolveScalarType::real(p32),
        vec![4],
        B::Divide,
        false,
    );
    assert!(matches!(
        compile_pure_call_wasm(&table, &site),
        Err(TypedCallCompileError::UnsupportedArithmetic)
    ));
}

fn boolean_cells(operator: solve::SolveBinaryOperator, scalar: bool, values: [bool; 4]) -> Vec<u8> {
    cells(values.map(|value| {
        solve::SolveValueKind::Boolean(if operator == solve::SolveBinaryOperator::And {
            value && scalar
        } else {
            value || scalar
        })
    }))
}

#[test]
pub(crate) fn broadcast_scalar_slot_snapshot_survives_write_and_buffer_refusals() {
    let (table, site) = scalar_snapshot_table();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let values = [347.0, 394.0, 439.0, 484.0];
    let input = cells(values.into_iter().chain([3.0]).map(real));
    let expected = cells(values.map(|v| real(v / 3.0)));
    assert_eq!(runner.run(&input), (0, expected.clone()));
    for output_pointer in [0, runner.output as i32 + 1] {
        let before = runner.memory.data(&runner.store).to_vec();
        let status = runner
            .call
            .call(
                &mut runner.store,
                (0, output_pointer, runner.scratch as i32),
            )
            .unwrap();
        assert!(
            compiled
                .faults()
                .iter()
                .any(|f| f.status == status as u32 && f.kind == TypedCallFaultKind::InvalidBuffer)
        );
        assert_eq!(runner.memory.data(&runner.store), before);
    }
    assert_eq!(runner.run(&input), (0, expected));
}

#[test]
pub(crate) fn broadcast_nested_lazy_region_retains_exact_fault_path_and_recovery() {
    let (table, site) = conditional_broadcast_table();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let values = [2, 4, 6, 8];
    let input = |active| {
        cells(integer_values(&values).into_iter().chain([
            solve::SolveValueKind::Integer(0),
            solve::SolveValueKind::Boolean(active),
        ]))
    };
    assert_eq!(
        runner.run(&input(false)),
        (0, cells(integer_values(&values)))
    );
    let (status, output) = runner.run(&input(true));
    let fault = compiled
        .faults()
        .iter()
        .find(|f| f.status == status as u32)
        .unwrap();
    assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
    assert_eq!(fault.owner, site.owner());
    assert_eq!(fault.operation, Some(2));
    assert_eq!(fault.region_path, [(3, 0)]);
    assert_eq!(fault.provenance, span(48103));
    assert_eq!(output, vec![0xa5; 32]);
    assert_eq!(
        runner.run(&input(false)),
        (0, cells(integer_values(&values)))
    );
}
