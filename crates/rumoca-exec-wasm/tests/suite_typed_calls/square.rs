//! Power retains the target math intrinsic rather than a multiplication rewrite.
use super::*;

pub(crate) fn square_table(
    exponent: Option<f64>,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(110),
            vec![real_type.clone(); if exponent.is_some() { 1 } else { 2 }],
            vec![solve::SolvePureCallOutput::result(real_type)],
            span(650),
            |b, inputs, outputs| {
                let base = b.load(inputs[0], span(651))?;
                let exponent = if let Some(exponent) = exponent {
                    b.constant(solve::SolveValue::real(p, exponent), span(652))?
                } else {
                    b.load(inputs[1], span(653))?
                };
                let result =
                    b.binary(solve::SolveBinaryOperator::Power, base, exponent, span(654))?;
                b.store(outputs[0], result, span(655))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

fn check_square(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
    runner: &mut Runner,
    value: f64,
) {
    let (status, actual) = runner.run(&cells([real(value)]));
    assert_eq!(status, 0);
    let expected = oracle(table, site, &[vec![real(value)]]).unwrap();
    if value.is_nan() {
        let actual = f64::from_bits(u64::from_le_bytes(actual.try_into().unwrap()));
        assert!(actual.is_nan());
    } else {
        assert_eq!(
            actual, expected,
            "canonical square disagreement for {value:?}"
        );
    }
}

#[test]
fn power_intrinsic_matches_canonical_ieee_corners_and_exponent_samples() {
    let (table, site) = square_table(Some(2.0));
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for value in [
        0.0,
        -0.0,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::MAX,
        f64::MIN_POSITIVE,
        f64::from_bits(1),
        f64::NAN,
        f64::from_bits(0x7ff0_0000_0000_0001),
        // The discarded multiply rewrite differs by one ULP on the recorded
        // host. Exact canonical comparison retains that regression here.
        3.8836460454820846e-137,
    ] {
        check_square(&table, &site, &mut runner, value);
    }
    let mut fraction = 1_u64;
    for exponent in 0..2047_u64 {
        fraction = fraction.wrapping_mul(6364136223846793005).wrapping_add(1);
        let bits = exponent << 52 | fraction & 0x000f_ffff_ffff_ffff;
        check_square(&table, &site, &mut runner, f64::from_bits(bits));
        check_square(&table, &site, &mut runner, -f64::from_bits(bits));
    }
}

#[test]
fn power_intrinsic_preserves_variable_and_other_literal_exponents() {
    for exponent in [None, Some(0.0), Some(3.0), Some(f64::NAN)] {
        let (table, site) = square_table(exponent);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let mut runner = Runner::new(&compiled);
        for base in [-2.0, -0.0, 0.0, 2.0, f64::INFINITY] {
            let inputs = if exponent.is_some() {
                vec![vec![real(base)]]
            } else {
                vec![vec![real(base)], vec![real(-1.0)]]
            };
            let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
            assert_eq!(status, 0);
            assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
        }
    }
}
