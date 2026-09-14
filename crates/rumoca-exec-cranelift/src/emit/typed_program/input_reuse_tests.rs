use super::*;
use crate::emit::host_runtime::take_sin_calls;

fn sine_owner() -> solve::SolvePureCallTable {
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("pure_input_reuse.mo"),
        0,
        10,
    );
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            at,
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], at)?;
                let sine = builder.unary(solve::SolveUnaryOperator::Sin, x, at)?;
                builder.store(outputs[0], sine, at)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn native_pure_call_reuses_only_identical_complete_input_bits() {
    let table = sine_owner();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    take_sin_calls();
    for x in [0.7_f64, -0.0, 0.0, -0.7] {
        for _ in 0..3 {
            let mut output = [0_u64];
            compiled
                .call_cells(
                    rumoca_eval_solve::PureCallInvocation::Primal(&site),
                    &[x.to_bits()],
                    &mut output,
                )
                .unwrap();
            assert_eq!(output, [x.sin().to_bits()]);
        }
        assert_eq!(
            take_sin_calls(),
            1,
            "one native evaluation per exact input tuple"
        );
    }
}

#[test]
fn directional_input_reuse_includes_seed_and_is_separate_from_primal() {
    let table = sine_owner();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    let directional = site.directional().unwrap();
    take_sin_calls();
    let mut primal_output = [0];
    compiled
        .call_cells(
            rumoca_eval_solve::PureCallInvocation::Primal(&site),
            &[0.7_f64.to_bits()],
            &mut primal_output,
        )
        .unwrap();
    assert_eq!(take_sin_calls(), 1);
    for (x, seed) in [(0.7_f64, 1.0_f64), (0.7, 2.0), (-0.0, 0.0), (0.0, -0.0)] {
        for _ in 0..3 {
            let mut output = [0_u64; 2];
            compiled
                .call_cells(
                    rumoca_eval_solve::PureCallInvocation::Directional(directional),
                    &[x.to_bits(), seed.to_bits()],
                    &mut output,
                )
                .unwrap();
            assert_eq!(output, [x.sin().to_bits(), (x.cos() * seed).to_bits()]);
        }
        assert_eq!(
            take_sin_calls(),
            1,
            "changing the seed must invalidate the directional result"
        );
    }
}

fn checked_linear_owner() -> solve::SolvePureCallTable {
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("pure_linear_input_reuse.mo"),
        0,
        10,
    );
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let matrix = solve::SolveValueType::tensor(element, vec![2, 2]).unwrap();
    let vector = solve::SolveValueType::tensor(element, vec![2]).unwrap();
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(2).unwrap()),
            vec![matrix, vector.clone()],
            vec![
                solve::SolvePureCallOutput::result(vector),
                solve::SolvePureCallOutput::assertion_predicate(),
            ],
            at,
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], at)?;
                let rhs = builder.load(inputs[1], at)?;
                let rhs = builder.unary(solve::SolveUnaryOperator::Sin, rhs, at)?;
                let solution = builder.linear_solve(matrix, rhs, at)?;
                builder.store(outputs[0], solution, at)?;
                let entry = builder.project_element(matrix, vec![0, 0], at)?;
                let zero = builder.constant(solve::SolveValue::real(arithmetic, 0.0), at)?;
                let predicate =
                    builder.compare(solve::SolveCompareOperator::Greater, entry, zero, at)?;
                builder.store(outputs[1], predicate, at)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn native_input_reuse_covers_whole_tensors_and_retains_assertion_predicates() {
    let table = checked_linear_owner();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    take_sin_calls();
    for (a, last) in [(2.0_f64, 0.2_f64), (2.0, 0.3), (-2.0, 0.3)] {
        let input = [a, 0.0, 0.0, 3.0, 0.1, last].map(f64::to_bits);
        for _ in 0..3 {
            let mut output = [0; 3];
            compiled
                .call_cells(
                    rumoca_eval_solve::PureCallInvocation::Primal(&site),
                    &input,
                    &mut output,
                )
                .unwrap();
            assert_eq!(
                output,
                [
                    (0.1_f64.sin() / a).to_bits(),
                    (last.sin() / 3.0).to_bits(),
                    u64::from(a > 0.0)
                ]
            );
        }
        assert_eq!(
            take_sin_calls(),
            2,
            "all tensor cells belong to the exact input coordinate"
        );
    }
}

#[test]
fn failed_native_evaluation_never_publishes_a_result() {
    let table = checked_linear_owner();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    take_sin_calls();
    for a in [2.0_f64, 0.0, 0.0, 2.0, 2.0] {
        let input = [a, 0.0, 0.0, 3.0, 0.1, 0.2].map(f64::to_bits);
        let mut output = [0; 3];
        let result = compiled.call_cells(
            rumoca_eval_solve::PureCallInvocation::Primal(&site),
            &input,
            &mut output,
        );
        if a == 0.0 {
            assert!(result.unwrap_err().to_string().contains("linear solve"));
        } else {
            result.unwrap();
            assert_eq!(output[0], (0.1_f64.sin() / a).to_bits());
        }
    }
    assert_eq!(
        take_sin_calls(),
        8,
        "two failed calls rerun, and success after failure recomputes"
    );
}

#[test]
fn compiled_tables_have_independent_result_storage() {
    let table = sine_owner();
    let first = CompiledPureCallTable::compile(&table).unwrap();
    let second = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    take_sin_calls();
    for compiled in [&first, &second, &first, &second] {
        let mut output = [0];
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Primal(&site),
                &[0.7_f64.to_bits()],
                &mut output,
            )
            .unwrap();
        assert_eq!(output, [0.7_f64.sin().to_bits()]);
    }
    assert_eq!(take_sin_calls(), 2);
}

#[test]
fn nested_calls_compare_arguments_even_within_one_caller_invocation() {
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("nested_input_coordinates.mo"),
        0,
        10,
    );
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        let inner = table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real.clone())],
            at,
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], at)?;
                let result = builder.unary(solve::SolveUnaryOperator::Sin, x, at)?;
                builder.store(outputs[0], result, at)
            },
        )?;
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(2).unwrap()),
            vec![real.clone(), real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            at,
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], at)?;
                let z = builder.load(inputs[1], at)?;
                let a = builder.call(inner, &[x], at)?[0];
                let b = builder.call(inner, &[z], at)?[0];
                let sum = builder.binary(solve::SolveBinaryOperator::Add, a, b, at)?;
                builder.store(outputs[0], sum, at)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[1].call_site();
    take_sin_calls();
    for _ in 0..3 {
        let mut output = [0];
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Primal(&site),
                &[0.7_f64.to_bits(), 1.2_f64.to_bits()],
                &mut output,
            )
            .unwrap();
        assert_eq!(output, [(0.7_f64.sin() + 1.2_f64.sin()).to_bits()]);
    }
    assert_eq!(take_sin_calls(), 2);
}
