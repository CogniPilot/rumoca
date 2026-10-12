use super::*;

#[test]
fn scalar_integer_payload_accepts_exact_large_values_without_a_53_bit_ceiling() {
    for value in [
        i64::MIN,
        -18_014_398_509_481_984,
        0,
        9_007_199_254_740_994,
        18_014_398_509_481_984,
    ] {
        assert_eq!(
            checked_scalar_payload_value(SolveValueKind::Integer(value)).unwrap(),
            value as f64
        );
    }
}

#[test]
fn scalar_integer_payload_refuses_rounded_outputs() {
    for value in [i64::MAX, 9_007_199_254_740_993, -9_007_199_254_740_993] {
        let error = checked_scalar_payload_value(SolveValueKind::Integer(value)).unwrap_err();
        assert!(error.to_string().contains("scalar ABI"), "{error:?}");
    }
}

#[test]
fn scalar_integer_payload_checks_inputs_before_saturating_casts() {
    let full = SolveValueType::scalar(SolveScalarType::Integer(
        rumoca_ir_solve::SolveIntegerDomain::FULL,
    ));
    for value in [i64::MIN as f64, 0.0, 18_014_398_509_481_984.0] {
        assert_eq!(
            typed_kind_from_scalar(value, &full).unwrap(),
            SolveValueKind::Integer(value as i64)
        );
    }
    for value in [
        9_223_372_036_854_775_808.0,
        -9_223_372_036_854_777_856.0,
        0.5,
        f64::NAN,
        f64::INFINITY,
    ] {
        assert!(typed_kind_from_scalar(value, &full).is_err(), "{value}");
    }
    let narrow = SolveValueType::scalar(SolveScalarType::Integer(
        rumoca_ir_solve::SolveIntegerDomain::construct(-10, 10).unwrap(),
    ));
    for value in [-10.0, 10.0] {
        assert!(typed_kind_from_scalar(value, &narrow).is_ok());
    }
    for value in [-11.0, 11.0] {
        assert!(typed_kind_from_scalar(value, &narrow).is_err());
    }
}
