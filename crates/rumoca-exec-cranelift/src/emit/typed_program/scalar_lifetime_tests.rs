use super::*;
use std::num::NonZeroU64;

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}

fn real() -> solve::SolveValueType {
    solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()))
}

fn at() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("scalar_lifetime.mo"),
        1,
        2,
    )
}

fn table(finite: bool, inline: bool) -> solve::SolvePureCallTable {
    solve::SolvePureCallTable::construct(profile(), |t| {
        let inner = t.add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(801).unwrap()),
            vec![real()],
            vec![solve::SolvePureCallOutput::result(real())],
            at(),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], at())?;
                let temporary = b.unary(solve::SolveUnaryOperator::Exp, x, at())?;
                if finite {
                    b.check_finite(temporary, solve::SolveFiniteCheck::Derivative, at())?;
                }
                let result = b.unary(solve::SolveUnaryOperator::Sin, temporary, at())?;
                b.store(outputs[0], result, at())
            },
        )?;
        if inline {
            t.add_owner(
                solve::SolvePureCallIdentity::issued(NonZeroU64::new(802).unwrap()),
                vec![real()],
                vec![solve::SolvePureCallOutput::result(real())],
                at(),
                |b, inputs, outputs| {
                    let x = b.load(inputs[0], at())?;
                    let called = b.call(inner, &[x], at())?;
                    b.store(outputs[0], called[0], at())
                },
            )?;
        }
        Ok(())
    })
    .unwrap()
}

#[test]
fn scalar_homes_preserve_bits_in_direct_generic_and_inline_entries() {
    for inline in [false, true] {
        let table = table(false, inline);
        let owner = table.owners().last().unwrap();
        let site = owner.call_site();
        let code = crate::compile_pure_call_table(&table).unwrap();
        let mut direct = (!inline).then(|| CompiledF64Call::compile(&table, &site, &[1]).unwrap());
        let body = table.owners()[0].body();
        assert!(matches!(
            body.storage_plan().aliases().registers()[1],
            solve::SolveRegisterHome::Scalar { .. }
        ));
        assert_eq!(body.storage_plan().cells().offsets()[1], None);
        for bits in [
            0,
            (-0.0_f64).to_bits(),
            1,
            0.5_f64.to_bits(),
            (-2.0_f64).to_bits(),
            0x7ff8_1234_5678_9abc,
        ] {
            let x = f64::from_bits(bits);
            let mut output = [42.0];
            code.call_scalar_payload(
                rumoca_eval_solve::PureCallInvocation::Primal(&site),
                &[x],
                &mut output,
                &mut Vec::new(),
                &mut Vec::new(),
            )
            .unwrap();
            let expected = x.exp().sin().to_bits();
            assert_eq!(output[0].to_bits(), expected);
            if let Some(direct) = &mut direct {
                direct.call(&[&[x]], &mut output).unwrap();
                assert_eq!(output[0].to_bits(), expected);
            }
        }
    }
}

#[test]
fn a_finite_failure_reads_the_scalar_value_and_preserves_output_bits() {
    let table = table(true, false);
    let owner = &table.owners()[0];
    assert!(matches!(
        owner.body().storage_plan().aliases().registers()[1],
        solve::SolveRegisterHome::Scalar { .. }
    ));
    let mut call = CompiledCheckedF64Call::compile(&table, &owner.call_site(), &[1]).unwrap();
    let sentinel = 0x7ff8_1234_abcd_5678;
    let mut output = [f64::from_bits(sentinel)];
    assert!(call.call(&[&[1000.0]], &mut output).is_err());
    assert_eq!(output[0].to_bits(), sentinel);
    call.call(&[&[0.5]], &mut output).unwrap();
    assert_eq!(output[0].to_bits(), 0.5_f64.exp().sin().to_bits());
}
