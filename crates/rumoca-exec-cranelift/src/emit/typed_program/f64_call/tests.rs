use super::*;
use std::num::NonZeroU64;

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}
fn at() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("borrowed_f64.mo"),
        1,
        2,
    )
}
fn real() -> solve::SolveValueType {
    solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()))
}
fn identity(types: Vec<solve::SolveValueType>) -> solve::SolvePureCallTable {
    identity_with_profile(profile(), types)
}

fn identity_with_profile(
    arithmetic: solve::SolveArithmeticProfile,
    types: Vec<solve::SolveValueType>,
) -> solve::SolvePureCallTable {
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            types.clone(),
            types
                .into_iter()
                .map(solve::SolvePureCallOutput::result)
                .collect(),
            at(),
            |b, inputs, outputs| {
                for (input, output) in inputs.iter().zip(outputs) {
                    let value = b.load(*input, at())?;
                    b.store(*output, value, at())?;
                }
                Ok(())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn borrowed_f64_tensor_preserves_all_ieee_bits_and_input_lifetimes() {
    let tensor = solve::SolveValueType::tensor(real().element_type(), vec![2, 3]).unwrap();
    let table = identity(vec![tensor]);
    let mut call = CompiledF64Call::compile(&table, &table.owners()[0].call_site(), &[6]).unwrap();
    drop(table); // The prepared entry retains its source and executable owner.
    let bits = [
        0,
        (-0.0_f64).to_bits(),
        f64::INFINITY.to_bits(),
        f64::NEG_INFINITY.to_bits(),
        0x7ff8_1234_5678_9abc,
        1,
    ];
    let input = bits.map(f64::from_bits);
    let mut output = [42.0; 6];
    for _ in 0..2 {
        call.call(&[&input], &mut output).unwrap();
        assert_eq!(output.map(f64::to_bits), bits);
        assert_eq!(input.map(f64::to_bits), bits);
    }
}

#[test]
fn borrowed_f64_regions_cover_scalars_and_tensors_without_gathering() {
    let tensor = solve::SolveValueType::tensor(real().element_type(), vec![3]).unwrap();
    let table = identity(vec![real(), real(), real(), tensor, real()]);
    let mut call =
        CompiledF64Call::compile(&table, &table.owners()[0].call_site(), &[3, 0, 3, 1]).unwrap();
    let mut output = [0.0; 7];
    call.call(
        &[&[1.0, 2.0, 3.0], &[], &[4.0, 5.0, 6.0], &[7.0]],
        &mut output,
    )
    .unwrap();
    assert_eq!(output, [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]);
    call.call(
        &[&[-1.0, -2.0, -3.0], &[], &[-4.0, -5.0, -6.0], &[-7.0]],
        &mut output,
    )
    .unwrap();
    assert_eq!(output, [-1.0, -2.0, -3.0, -4.0, -5.0, -6.0, -7.0]);
}

#[test]
fn borrowed_f64_bad_shapes_and_partitions_refuse_before_output() {
    let tensor = solve::SolveValueType::tensor(real().element_type(), vec![4]).unwrap();
    let table = identity(vec![tensor]);
    let site = table.owners()[0].call_site();
    for regions in [&[][..], &[2, 2], &[3], &[5], &[4, 1], &[usize::MAX]] {
        assert!(CompiledF64Call::compile(&table, &site, regions).is_err());
    }
    let mut call = CompiledF64Call::compile(&table, &site, &[4]).unwrap();
    let mut output = [73.0; 4];
    assert!(call.call(&[&[1.0, 2.0, 3.0]], &mut output).is_err());
    assert!(call.call(&[], &mut output).is_err());
    assert!(call.call(&[&[1.0; 4], &[]], &mut output).is_err());
    assert!(call.call(&[&[1.0; 4]], &mut output[..3]).is_err());
    assert_eq!(output, [73.0; 4]);
}

#[test]
fn borrowed_f64_empty_regions_and_zero_input_owner_are_well_defined() {
    assert_eq!(
        solve::SolveValueType::tensor(real().element_type(), vec![0]),
        Err(solve::SolveTypeConstructionError::ZeroTensorExtent),
    );
    let table = identity(vec![real()]);
    let mut call =
        CompiledF64Call::compile(&table, &table.owners()[0].call_site(), &[0, 1, 0]).unwrap();
    let mut scalar = [0.0];
    call.call(&[&[], &[7.0], &[]], &mut scalar).unwrap();
    assert_eq!(scalar, [7.0]);
    let table = solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![],
            vec![solve::SolvePureCallOutput::result(real())],
            at(),
            |b, _, outputs| {
                let value = b.constant(solve::SolveValue::real(profile(), 7.0), at())?;
                b.store(outputs[0], value, at())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let mut call =
        CompiledF64Call::compile(&table, &table.owners()[0].call_site(), &[0, 0]).unwrap();
    let mut output = [0.0];
    call.call(&[&[], &[]], &mut output).unwrap();
    assert_eq!(output, [7.0]);
}

#[test]
fn borrowed_f64_mixed_types_and_foreign_interface_keep_typed_conversion() {
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(profile()));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let narrow = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary32,
        solve::SolveIntegerDomain::FULL,
    );
    let binary32 = solve::SolveValueType::scalar(solve::SolveScalarType::real(narrow));
    for ty in [integer, boolean] {
        let table = identity(vec![real(), ty]);
        let site = table.owners()[0].call_site();
        assert!(CompiledF64Call::compile(&table, &site, &[2]).is_err());
    }
    let narrow_table = identity_with_profile(narrow, vec![binary32]);
    assert!(
        CompiledF64Call::compile(&narrow_table, &narrow_table.owners()[0].call_site(), &[1],)
            .is_err()
    );
    let table = identity(vec![real()]);
    let foreign = identity(vec![real(), real()]);
    assert!(CompiledF64Call::compile(&table, &foreign.owners()[0].call_site(), &[1]).is_err());
    // The existing Integer scalar ABI still rejects inexact input and retains
    // the caller's output on error; direct-entry admission does not replace it.
    let table = identity(vec![solve::SolveValueType::scalar(
        solve::SolveScalarType::integer(profile()),
    )]);
    let code = crate::compile_pure_call_table(&table).unwrap();
    let site = table.owners()[0].call_site();
    let mut out = [17.0];
    assert!(
        code.call_scalar_payload(
            rumoca_eval_solve::PureCallInvocation::Primal(&site),
            &[0.5],
            &mut out,
            &mut Vec::new(),
            &mut Vec::new()
        )
        .is_err()
    );
    assert_eq!(out, [17.0]);
}

#[test]
fn borrowed_f64_fallible_owner_declines_and_old_call_keeps_output_on_fault() {
    let matrix = solve::SolveValueType::tensor(real().element_type(), vec![2, 2]).unwrap();
    let vector = solve::SolveValueType::tensor(real().element_type(), vec![2]).unwrap();
    let table = solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![matrix, vector.clone()],
            vec![solve::SolvePureCallOutput::result(vector)],
            at(),
            |b, inputs, outputs| {
                let matrix = b.load(inputs[0], at())?;
                let rhs = b.load(inputs[1], at())?;
                let solution = b.linear_solve(matrix, rhs, at())?;
                b.store(outputs[0], solution, at())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    let error = CompiledF64Call::compile(&table, &site, &[4, 2])
        .err()
        .unwrap();
    assert!(error.to_string().contains("total single-frame"));
    let code = crate::compile_pure_call_table(&table).unwrap();
    let mut out = [17.0, 19.0];
    assert!(
        code.call_scalar_payload(
            rumoca_eval_solve::PureCallInvocation::Primal(&site),
            &[1.0, 2.0, 2.0, 4.0, 1.0, 1.0],
            &mut out,
            &mut Vec::new(),
            &mut Vec::new()
        )
        .is_err()
    );
    assert_eq!(out, [17.0, 19.0]);
}

fn checked_log_after_store() -> solve::SolvePureCallTable {
    solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![real()],
            vec![solve::SolvePureCallOutput::result(real())],
            at(),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], at())?;
                b.check_finite(
                    value,
                    solve::SolveFiniteCheck::Input { coordinate: 0 },
                    at(),
                )?;
                b.store(outputs[0], value, at())?;
                let log = b.unary(solve::SolveUnaryOperator::Log, value, at())?;
                b.check_finite(
                    log,
                    solve::SolveFiniteCheck::Primal {
                        program: 4,
                        operation: 7,
                    },
                    at(),
                )
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn checked_f64_late_domain_failure_does_not_publish_and_keeps_source_alive() {
    let table = checked_log_after_store();
    let site = table.owners()[0].call_site();
    assert!(CompiledF64Call::compile(&table, &site, &[1]).is_err());
    let mut call = CompiledCheckedF64Call::compile(&table, &site, &[1]).unwrap();
    drop(table);
    let sentinel = f64::from_bits(0x7ff8_1234_5678_9876);
    let mut output = [sentinel];
    let error = call.call(&[&[-1.0]], &mut output).unwrap_err();
    assert!(
        matches!(error, CompileError::NonFinite { check: solve::SolveFiniteCheck::Primal { program: 4, operation: 7 }, element: 0, span: Some(span) } if span == at())
    );
    assert_eq!(output[0].to_bits(), sentinel.to_bits());
    call.call(&[&[2.0]], &mut output).unwrap();
    assert_eq!(output, [2.0]);
    assert!(call.call(&[&[1.0, 2.0]], &mut output).is_err());
    assert_eq!(output, [2.0]);
}

#[test]
fn checked_f64_tensor_guard_reports_first_element_and_commits_whole_values() {
    let tensor = solve::SolveValueType::tensor(real().element_type(), vec![2, 2]).unwrap();
    let table = solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor)],
            at(),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], at())?;
                b.store(outputs[0], value, at())?;
                b.check_finite(value, solve::SolveFiniteCheck::Derivative, at())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let mut call =
        CompiledCheckedF64Call::compile(&table, &table.owners()[0].call_site(), &[4]).unwrap();
    let mut output = [11.0, -0.0, 13.0, 14.0];
    let bits = output.map(f64::to_bits);
    let error = call
        .call(&[&[1.0, 2.0, f64::NAN, f64::INFINITY]], &mut output)
        .unwrap_err();
    assert!(matches!(
        error,
        CompileError::NonFinite {
            check: solve::SolveFiniteCheck::Derivative,
            element: 2,
            ..
        }
    ));
    assert_eq!(output.map(f64::to_bits), bits);
    call.call(&[&[-0.0, 5.0, 6.0, 7.0]], &mut output).unwrap();
    assert_eq!(
        output.map(f64::to_bits),
        [-0.0_f64, 5.0, 6.0, 7.0].map(f64::to_bits)
    );
}

#[test]
fn checked_f64_scalar_tape_reuse_preserves_guards_and_new_call_values() {
    let table = solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![real(), real()],
            vec![solve::SolvePureCallOutput::result(real())],
            at(),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], at())?;
                let y = b.load(inputs[1], at())?;
                let product = b.binary(solve::SolveBinaryOperator::Multiply, x, y, at())?;
                b.check_finite(
                    product,
                    solve::SolveFiniteCheck::Primal {
                        program: 0,
                        operation: 2,
                    },
                    at(),
                )?;
                let sum = b.binary(solve::SolveBinaryOperator::Add, product, x, at())?;
                b.check_finite(
                    sum,
                    solve::SolveFiniteCheck::Primal {
                        program: 0,
                        operation: 4,
                    },
                    at(),
                )?;
                let result = b.binary(solve::SolveBinaryOperator::Subtract, sum, y, at())?;
                b.store(outputs[0], result, at())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let mut call =
        CompiledCheckedF64Call::compile(&table, &table.owners()[0].call_site(), &[2]).unwrap();
    let mut output = [42.0_f64];
    for input in [
        [2.0, 3.0],
        [-5.0, 4.0],
        [-0.0, 0.0],
        [f64::MAX, 2.0],
        [7.0, -2.0],
    ] {
        let before = output[0].to_bits();
        let result = call.call(&[&input], &mut output);
        let product = input[0] * input[1];
        if product.is_finite() {
            result.unwrap();
            assert_eq!(
                output[0].to_bits(),
                ((product + input[0]) - input[1]).to_bits()
            );
        } else {
            assert!(matches!(
                result,
                Err(CompileError::NonFinite {
                    check: solve::SolveFiniteCheck::Primal {
                        program: 0,
                        operation: 2
                    },
                    element: 0,
                    ..
                })
            ));
            assert_eq!(output[0].to_bits(), before);
        }
    }
}

#[test]
fn checked_f64_scalar_forwarding_ends_at_aggregate_and_projection() {
    for construct in [false, true] {
        let table = solve::SolvePureCallTable::construct(profile(), |table| {
            table.add_owner(
                solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
                vec![real(), real()],
                vec![solve::SolvePureCallOutput::result(real())],
                at(),
                |b, inputs, outputs| {
                    let x = b.load(inputs[0], at())?;
                    let y = b.load(inputs[1], at())?;
                    let dead_sum = b.binary(solve::SolveBinaryOperator::Add, x, y, at())?;
                    let dead_product =
                        b.binary(solve::SolveBinaryOperator::Multiply, x, y, at())?;
                    b.check_finite(dead_sum, solve::SolveFiniteCheck::Derivative, at())?;
                    b.check_finite(dead_product, solve::SolveFiniteCheck::Derivative, at())?;
                    // The dead scalar cells can be reused by the aggregate and
                    // projection. Neither write may leave a forwarded old value.
                    let aggregate = if construct {
                        b.construct_aggregate(&[x], vec![1], at())?
                    } else {
                        b.fill(x, vec![1], at())?
                    };
                    let selected = b.project_element(aggregate, vec![0], at())?;
                    let result = b.binary(solve::SolveBinaryOperator::Add, selected, y, at())?;
                    b.store(outputs[0], result, at())
                },
            )?;
            Ok(())
        })
        .unwrap();
        let mut call =
            CompiledCheckedF64Call::compile(&table, &table.owners()[0].call_site(), &[2]).unwrap();
        let mut output = [0.0_f64];
        for input in [[2.0, 3.0], [-7.0, 4.0], [5.0, -3.0]] {
            call.call(&[&input], &mut output).unwrap();
            assert_eq!(output[0].to_bits(), (input[0] + input[1]).to_bits());
        }
    }
}

#[test]
fn checked_f64_large_aggregate_guard_preserves_first_fault_and_publication() {
    let tensor = solve::SolveValueType::tensor(real().element_type(), vec![20]).unwrap();
    let table = solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor)],
            at(),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], at())?;
                b.store(outputs[0], value, at())?;
                b.check_finite(value, solve::SolveFiniteCheck::Derivative, at())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let mut call =
        CompiledCheckedF64Call::compile(&table, &table.owners()[0].call_site(), &[20]).unwrap();
    let mut input = [1.0_f64; 20];
    input[17] = f64::NAN;
    input[19] = f64::INFINITY;
    let sentinel = f64::from_bits(0x7ff8_1234_5678_9abc);
    let mut output = [sentinel; 20];
    assert!(matches!(
        call.call(&[&input], &mut output),
        Err(CompileError::NonFinite { element: 17, .. })
    ));
    assert_eq!(output.map(f64::to_bits), [sentinel.to_bits(); 20]);
    input[17] = -0.0;
    input[19] = 2.0;
    call.call(&[&input], &mut output).unwrap();
    assert_eq!(output.map(f64::to_bits), input.map(f64::to_bits));
}
