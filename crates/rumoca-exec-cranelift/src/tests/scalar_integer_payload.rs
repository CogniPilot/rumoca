use super::*;
use rumoca_ir_solve::{
    SolveArithmeticProfile, SolveBinaryOperator, SolveIntegerDomain, SolvePureCallIdentity,
    SolvePureCallOutput, SolvePureCallTable, SolveRealFormat, SolveScalarType, SolveValue,
    SolveValueType,
};

fn owner(domain: SolveIntegerDomain, wide: bool) -> SolvePureCallTable {
    let profile = SolveArithmeticProfile::construct(SolveRealFormat::Binary64, domain);
    let integer = SolveValueType::scalar(SolveScalarType::integer(profile));
    let span = fixture_span();
    SolvePureCallTable::construct(profile, |table| {
        let outputs = if wide {
            vec![SolvePureCallOutput::result(integer.clone()); 2]
        } else {
            vec![SolvePureCallOutput::result(integer.clone())]
        };
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![integer],
            outputs,
            span,
            |program, inputs, outputs| {
                let input = program.load(inputs[0], span)?;
                program.store(outputs[0], input, span)?;
                if wide {
                    let literal = program.constant(
                        SolveValue::integer(profile, 9_007_199_254_740_993).unwrap(),
                        span,
                    )?;
                    let sum = program.binary(SolveBinaryOperator::Add, literal, input, span)?;
                    program.store(outputs[1], sum, span)?;
                }
                Ok(())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn rows(site: rumoca_ir_solve::SolvePureCallSite, count: u32) -> ScalarProgramBlock {
    ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::PureCall {
                dst_start: 1,
                input_starts: vec![0].into_boxed_slice(),
                site,
            },
            LinearOp::StoreOutputRange {
                start: 1,
                count: usize::try_from(count).unwrap(),
                stride: 1,
            },
        ]],
        vec![fixture_span()],
        (0..count as usize).collect(),
    )
    .unwrap()
}

#[test]
fn scalar_integer_payload_native_host_keeps_raw_int64_and_refuses_rounded_transport() {
    let table = owner(SolveIntegerDomain::FULL, true);
    let site = table.owners()[0].call_site();
    let compiled = compile_pure_call_table(&table).unwrap();
    let invocation = rumoca_eval_solve::PureCallInvocation::Primal(&site);
    let mut raw = [0; 2];
    compiled.jit.call_cells(invocation, &[0], &mut raw).unwrap();
    assert_eq!(raw, [0, 9_007_199_254_740_993]);
    let (mut inputs, mut outputs) = (Vec::new(), Vec::new());
    let mut transported = [-123.0; 2];
    let error = compiled
        .call_scalar_payload(
            invocation,
            &[0.0],
            &mut transported,
            &mut inputs,
            &mut outputs,
        )
        .unwrap_err();
    assert!(error.to_string().contains("scalar ABI"), "{error:?}");
    assert_eq!(
        transported, [-123.0; 2],
        "a rejected payload publishes no output"
    );
    compiled
        .call_scalar_payload(
            invocation,
            &[1.0],
            &mut transported,
            &mut inputs,
            &mut outputs,
        )
        .unwrap();
    assert_eq!(transported, [1.0, 9_007_199_254_740_994.0]);
    assert!(
        compiled
            .call_scalar_payload(
                invocation,
                &[9_223_372_036_854_775_808.0],
                &mut transported,
                &mut inputs,
                &mut outputs
            )
            .is_err()
    );
}

#[test]
fn scalar_integer_payload_native_row_refuses_inexact_inputs_and_outputs() {
    let table = owner(SolveIntegerDomain::FULL, true);
    let site = table.owners()[0].call_site();
    let compiled = compile_pure_call_table(&table).unwrap();
    let source = rows(site, 2);
    let compiled =
        compile_expression_scalar_program_block_with_pure_calls(&source, &compiled).unwrap();
    let mut out = [0.0; 2];
    compiled.call(&[], &[1.0], 0.0, &mut out).unwrap();
    assert_eq!(out, [1.0, 9_007_199_254_740_994.0]);
    for input in [0.0, 0.5, f64::NAN, 9_223_372_036_854_775_808.0] {
        let error = compiled.call(&[], &[input], 0.0, &mut out).unwrap_err();
        assert!(
            error.to_string().contains("scalar ABI"),
            "{input}: {error:?}"
        );
    }
}

#[test]
fn scalar_integer_payload_native_row_enforces_the_declared_integer_domain() {
    let table = owner(SolveIntegerDomain::construct(-10, 10).unwrap(), false);
    let site = table.owners()[0].call_site();
    let compiled = compile_pure_call_table(&table).unwrap();
    let source = rows(site, 1);
    let compiled =
        compile_expression_scalar_program_block_with_pure_calls(&source, &compiled).unwrap();
    let mut out = [0.0];
    for input in [-10.0, 10.0] {
        compiled.call(&[], &[input], 0.0, &mut out).unwrap();
        assert_eq!(out, [input]);
    }
    for input in [-11.0, 11.0] {
        assert!(compiled.call(&[], &[input], 0.0, &mut out).is_err());
    }
}

#[derive(Clone, Copy)]
enum Arithmetic {
    Binary(SolveBinaryOperator),
    Unary(rumoca_ir_solve::SolveUnaryOperator),
}

fn arithmetic_table(domain: SolveIntegerDomain, operation: Arithmetic) -> SolvePureCallTable {
    let profile = SolveArithmeticProfile::construct(SolveRealFormat::Binary64, domain);
    let integer = SolveValueType::scalar(SolveScalarType::integer(profile));
    let arity = match operation {
        Arithmetic::Binary(_) => 2,
        Arithmetic::Unary(_) => 1,
    };
    let span = fixture_span();
    SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![integer.clone(); arity],
            vec![SolvePureCallOutput::result(integer)],
            span,
            |program, inputs, outputs| {
                let lhs = program.load(inputs[0], span)?;
                let result = match operation {
                    Arithmetic::Binary(operator) => {
                        let rhs = program.load(inputs[1], span)?;
                        program.binary(operator, lhs, rhs, span)?
                    }
                    Arithmetic::Unary(operator) => program.unary(operator, lhs, span)?,
                };
                program.store(outputs[0], result, span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn arithmetic_cases(
    domain: SolveIntegerDomain,
    operator: Arithmetic,
    cases: &[(&[i64], Option<i64>)],
) {
    let table = arithmetic_table(domain, operator);
    let site = table.owners()[0].call_site();
    let compiled = compile_pure_call_table(&table).unwrap();
    for (input, expected) in cases {
        let arguments: Vec<_> = input
            .iter()
            .zip(site.inputs())
            .map(|(&value, ty)| {
                rumoca_eval_solve::TypedValue::construct(
                    ty.clone(),
                    vec![rumoca_ir_solve::SolveValueKind::Integer(value)],
                )
                .unwrap()
            })
            .collect();
        let reference = rumoca_eval_solve::eval_pure_call(&table, site.owner(), &arguments);
        let input: Vec<_> = input.iter().map(|&value| value as u64).collect();
        let mut out = [0];
        let native = compiled.jit.call_cells(
            rumoca_eval_solve::PureCallInvocation::Primal(&site),
            &input,
            &mut out,
        );
        if let Some(expected) = expected {
            assert_eq!(
                reference.unwrap()[0].elements(),
                &[rumoca_ir_solve::SolveValueKind::Integer(*expected)]
            );
            native.unwrap();
            assert_eq!(out[0] as i64, *expected);
        } else {
            assert!(matches!(
                reference,
                Err(rumoca_eval_solve::TypedProgramEvalError::IntegerArithmetic { .. })
            ));
            let error = native.unwrap_err();
            assert!(
                error
                    .to_string()
                    .contains("arithmetic left the checked target domain"),
                "{error:?}"
            );
        }
    }
}

#[test]
fn scalar_integer_payload_native_arithmetic_checks_overflow_and_result_domains() {
    use SolveBinaryOperator::{Add, Multiply, Subtract};
    let full = SolveIntegerDomain::FULL;
    arithmetic_cases(
        full,
        Arithmetic::Binary(Add),
        &[
            (&[i64::MAX, 0], Some(i64::MAX)),
            (&[i64::MAX, 1], None),
            (&[i64::MIN, -1], None),
        ],
    );
    arithmetic_cases(
        full,
        Arithmetic::Binary(Subtract),
        &[
            (&[i64::MIN, 0], Some(i64::MIN)),
            (&[i64::MIN, 1], None),
            (&[i64::MAX, -1], None),
        ],
    );
    arithmetic_cases(
        full,
        Arithmetic::Binary(Multiply),
        &[
            (&[i64::MAX, 1], Some(i64::MAX)),
            (&[i64::MAX, 2], None),
            (&[i64::MIN, -1], None),
            (&[3037000499, 3037000499], Some(9223372030926249001)),
            (&[3037000500, 3037000500], None),
        ],
    );
    let narrow = SolveIntegerDomain::construct(-10, 10).unwrap();
    arithmetic_cases(
        narrow,
        Arithmetic::Binary(Add),
        &[(&[8, 2], Some(10)), (&[8, 3], None)],
    );
    arithmetic_cases(
        narrow,
        Arithmetic::Binary(Subtract),
        &[(&[-8, 2], Some(-10)), (&[-8, 3], None)],
    );
    arithmetic_cases(
        narrow,
        Arithmetic::Binary(Multiply),
        &[(&[5, 2], Some(10)), (&[5, 3], None)],
    );
}

#[test]
fn scalar_integer_payload_native_unary_checks_minimum_and_asymmetric_result_domains() {
    use rumoca_ir_solve::SolveUnaryOperator::{Abs, Negate, Sign};
    arithmetic_cases(
        SolveIntegerDomain::FULL,
        Arithmetic::Unary(Negate),
        &[(&[i64::MIN + 1], Some(i64::MAX)), (&[i64::MIN], None)],
    );
    arithmetic_cases(
        SolveIntegerDomain::FULL,
        Arithmetic::Unary(Abs),
        &[(&[i64::MIN + 1], Some(i64::MAX)), (&[i64::MIN], None)],
    );
    arithmetic_cases(
        SolveIntegerDomain::construct(0, 10).unwrap(),
        Arithmetic::Unary(Abs),
        &[(&[10], Some(10))],
    );
    arithmetic_cases(
        SolveIntegerDomain::construct(2, 10).unwrap(),
        Arithmetic::Unary(Sign),
        &[(&[2], None)],
    );
    arithmetic_cases(
        SolveIntegerDomain::construct(2, 10).unwrap(),
        Arithmetic::Unary(Negate),
        &[(&[2], None)],
    );
}

#[test]
fn scalar_integer_payload_native_modulo_checks_signed_extremes_and_domains() {
    arithmetic_cases(
        SolveIntegerDomain::FULL,
        Arithmetic::Binary(SolveBinaryOperator::IntegerModulo),
        &[
            (&[8, 3], Some(2)),
            (&[-8, 3], Some(1)),
            (&[8, -3], Some(-1)),
            (&[-8, -3], Some(-2)),
            (&[0, -3], Some(0)),
            (&[9_007_199_254_740_993, 7], Some(5)),
            (&[i64::MIN, -1], Some(0)),
            (&[i64::MIN, 7], Some(6)),
            (&[i64::MIN, i64::MAX], Some(i64::MAX - 1)),
            (&[i64::MAX, i64::MIN], Some(-1)),
            (&[i64::MIN, i64::MIN], Some(0)),
            (&[i64::MAX, 0], None),
            (&[i64::MIN, 0], None),
        ],
    );
    arithmetic_cases(
        SolveIntegerDomain::construct(-10, -1).unwrap(),
        Arithmetic::Binary(SolveBinaryOperator::IntegerModulo),
        &[(&[-8, -5], Some(-3)), (&[-10, -5], None)],
    );
}

#[test]
fn scalar_integer_payload_native_remainder_checks_signed_extremes_and_domains() {
    arithmetic_cases(
        SolveIntegerDomain::FULL,
        Arithmetic::Binary(SolveBinaryOperator::IntegerRemainder),
        &[
            (&[8, 3], Some(2)),
            (&[-8, 3], Some(-2)),
            (&[8, -3], Some(2)),
            (&[-8, -3], Some(-2)),
            (&[0, -3], Some(0)),
            (&[9_007_199_254_740_993, 7], Some(5)),
            (&[i64::MIN, -1], Some(0)),
            (&[i64::MIN, 7], Some(-1)),
            (&[i64::MIN, i64::MAX], Some(-1)),
            (&[i64::MAX, i64::MIN], Some(i64::MAX)),
            (&[i64::MIN, i64::MIN], Some(0)),
            (&[i64::MAX, 0], None),
            (&[i64::MIN, 0], None),
        ],
    );
    arithmetic_cases(
        SolveIntegerDomain::construct(-10, -1).unwrap(),
        Arithmetic::Binary(SolveBinaryOperator::IntegerRemainder),
        &[(&[-8, -5], Some(-3)), (&[-10, -5], None)],
    );
}
