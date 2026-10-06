use super::*;

pub(super) fn broadcast_table(
    p: solve::SolveArithmeticProfile,
    scalar: solve::SolveScalarType,
    shape: Vec<u32>,
    operator: solve::SolveBinaryOperator,
    scalar_left: bool,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let tensor = solve::SolveValueType::tensor(scalar, shape).unwrap();
    let scalar = solve::SolveValueType::scalar(scalar);
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(48000),
            vec![tensor.clone(), scalar],
            vec![solve::SolvePureCallOutput::result(tensor)],
            span(48000),
            |b, inputs, outputs| {
                let a = b.load(inputs[0], span(48001))?;
                let s = b.load(inputs[1], span(48002))?;
                let value = b.broadcast_binary(operator, a, s, scalar_left, span(48003))?;
                b.store(outputs[0], value, span(48004))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

pub(super) fn real_cases() -> Vec<([f64; 4], f64)> {
    let tiny = f64::from_bits(1);
    let nan = f64::from_bits(0x7ff8_0000_0000_0042);
    vec![
        ([347.0, 394.0, 439.0, 484.0], 3.0),
        ([-347.0, -394.0, -439.0, -484.0], 3.0),
        ([tiny, -tiny, 0.0, -0.0], tiny),
        ([1e-300, -1e-300, 1e-309, -1e-309], 1e-309),
        ([f64::MAX, -f64::MAX, tiny, -0.0], f64::MAX),
        ([f64::MAX, -f64::MAX, tiny, -tiny], f64::MIN_POSITIVE),
        ([1.0, -1.0, 0.0, -0.0], 0.0),
        ([1.0, -1.0, 0.0, -0.0], -0.0),
        (
            [f64::MAX, tiny, f64::INFINITY, f64::NEG_INFINITY],
            f64::INFINITY,
        ),
        (
            [f64::MAX, tiny, f64::INFINITY, f64::NEG_INFINITY],
            f64::NEG_INFINITY,
        ),
        ([1.0, 0.0, -0.0, f64::INFINITY], nan),
        ([nan, f64::NEG_INFINITY, f64::INFINITY, -0.0], 1.0),
    ]
}

pub(super) fn check_real(operator: solve::SolveBinaryOperator, lhs: f64, rhs: f64, actual: &[u8]) {
    use solve::SolveBinaryOperator as B;
    let expected = match operator {
        B::Add => lhs + rhs,
        B::Subtract => lhs - rhs,
        B::Multiply => lhs * rhs,
        B::Divide => lhs / rhs,
        B::Min => lhs.min(rhs),
        B::Max => lhs.max(rhs),
        B::Power => lhs.powf(rhs),
        B::Atan2 => lhs.atan2(rhs),
        _ => unreachable!("Real fixture"),
    };
    let actual = f64::from_le_bytes(actual.try_into().unwrap());
    if expected.is_nan() {
        assert!(actual.is_nan());
    } else if matches!(operator, B::Min | B::Max) && lhs == rhs {
        assert!(actual.to_bits() == lhs.to_bits() || actual.to_bits() == rhs.to_bits());
    } else {
        assert_eq!(
            actual.to_bits(),
            expected.to_bits(),
            "{operator:?}({lhs:?}, {rhs:?})"
        );
    }
}

pub(super) fn integer_values(values: &[i64]) -> Vec<solve::SolveValueKind> {
    values
        .iter()
        .copied()
        .map(solve::SolveValueKind::Integer)
        .collect()
}

pub(super) fn independent_integer(
    operator: solve::SolveBinaryOperator,
    lhs: i64,
    rhs: i64,
) -> Option<i64> {
    use solve::SolveBinaryOperator as B;
    let (lhs, rhs) = (i128::from(lhs), i128::from(rhs));
    let result = match operator {
        B::Add => lhs + rhs,
        B::Subtract => lhs - rhs,
        B::Multiply => lhs * rhs,
        B::Min => lhs.min(rhs),
        B::Max => lhs.max(rhs),
        B::IntegerQuotient => lhs.checked_div(rhs)?,
        B::IntegerRemainder => lhs.checked_rem(rhs)?,
        B::IntegerModulo => {
            let r = lhs.checked_rem(rhs)?;
            if r != 0 && (r < 0) != (rhs < 0) {
                r + rhs
            } else {
                r
            }
        }
        _ => unreachable!("Integer fixture"),
    };
    i64::try_from(result).ok()
}

pub(super) fn check_integer_failure(
    p: solve::SolveArithmeticProfile,
    operator: solve::SolveBinaryOperator,
    values: &[i64],
    scalar: i64,
) {
    let (table, site) = broadcast_table(
        p,
        solve::SolveScalarType::integer(p),
        vec![values.len() as u32],
        operator,
        false,
    );
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let input = [integer_values(values), integer_values(&[scalar])];
    assert!(matches!(
        oracle(&table, &site, &input),
        Err(rumoca_eval_solve::TypedProgramEvalError::IntegerArithmetic { .. })
    ));
    let (status, actual) = runner.run(&cells(input.into_iter().flatten()));
    let fault = compiled
        .faults()
        .iter()
        .find(|f| f.status == status as u32)
        .unwrap();
    assert_eq!(fault.owner, site.owner());
    assert_eq!(fault.operation, Some(2));
    assert_eq!(fault.region_path, []);
    assert_eq!(fault.opcode, "broadcast binary");
    assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
    assert_eq!(fault.provenance, span(48003));
    assert_eq!(actual, vec![0xa5; values.len() * 8]);
    let valid = [integer_values(&vec![2; values.len()]), integer_values(&[3])];
    assert_eq!(
        runner.run(&cells(valid.iter().flatten().copied())),
        (0, oracle(&table, &site, &valid).unwrap())
    );
}

pub(super) fn scalar_snapshot_table() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let tensor = solve::SolveValueType::tensor(scalar.element_type(), vec![4]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(48050),
            vec![tensor.clone(), scalar.clone()],
            vec![solve::SolvePureCallOutput::result(tensor)],
            span(48050),
            |b, inputs, outputs| {
                let local = b.declare_slot(
                    scalar,
                    solve::SolveStorageClass::MethodLocal,
                    solve::SolveSlotAccess::ReadWrite,
                    span(48051),
                )?;
                let a = b.load(inputs[0], span(48052))?;
                let s = b.load(inputs[1], span(48053))?;
                b.store(local, s, span(48054))?;
                let old = b.load(local, span(48055))?;
                let zero = b.constant(solve::SolveValue::real(p, 0.0), span(48056))?;
                b.store(local, zero, span(48057))?;
                let quotient = b.broadcast_binary(
                    solve::SolveBinaryOperator::Divide,
                    a,
                    old,
                    false,
                    span(48058),
                )?;
                b.store(outputs[0], quotient, span(48059))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

pub(super) fn conditional_broadcast_table() -> (solve::SolvePureCallTable, solve::SolvePureCallSite)
{
    let p = profile();
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let tensor = solve::SolveValueType::tensor(scalar.element_type(), vec![4]).unwrap();
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(48100),
            vec![tensor.clone(), scalar, boolean],
            vec![solve::SolvePureCallOutput::result(tensor.clone())],
            span(48100),
            |b, inputs, outputs| {
                let a = b.load(inputs[0], span(48101))?;
                let s = b.load(inputs[1], span(48102))?;
                let active = b.load(inputs[2], span(48103))?;
                let values = b.conditional(
                    active,
                    &[a, s],
                    vec![tensor],
                    span(48104),
                    |r, captures, outputs| {
                        let a = r.load(captures[0], span(48101))?;
                        let s = r.load(captures[1], span(48102))?;
                        let value = r.broadcast_binary(
                            solve::SolveBinaryOperator::IntegerQuotient,
                            a,
                            s,
                            false,
                            span(48103),
                        )?;
                        r.store(outputs[0], value, span(48104))
                    },
                    |r, captures, outputs| {
                        let a = r.load(captures[0], span(48105))?;
                        r.store(outputs[0], a, span(48106))
                    },
                )?;
                b.store(outputs[0], values[0], span(48107))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}
