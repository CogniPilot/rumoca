//! Trap T7: GALEC Real literals require decimal places and a signed
//! lowercase exponent. Accept/reject table plus formatter conformance.

use rumoca_galec::{GalecError, format_real_literal, is_conformant_real_literal};

#[test]
fn conformant_literals_accepted() {
    let accepted = [
        "1.0",
        "0.0",
        "-1.5",
        "3.14159",
        "0.5",
        "1.0e+5",
        "2.5e-3",
        "-0.0",
        "123.456e+10",
        "-7.25e-30",
        "0.000001",
    ];
    for text in accepted {
        assert!(is_conformant_real_literal(text), "expected accept: {text}");
    }
}

#[test]
fn nonconformant_literals_rejected() {
    let rejected = [
        "1e5",      // no decimal places
        "1.",       // decimal places need at least one digit
        ".5",       // integer places mandatory
        "1.0e5",    // exponent sign mandatory
        "1",        // Integer, not Real
        "01.0",     // leading zero in integer places
        "1.0E+5",   // uppercase exponent
        "+1.0",     // no leading plus
        "1.0e+",    // exponent needs digits
        "1.0e",     // exponent needs sign and digits
        "-.5",      // integer places mandatory
        "1..0",     // double dot
        "1.0e+5.0", // exponent is an integer
        "",         // empty
        "nan",      // no textual specials
    ];
    for text in rejected {
        assert!(!is_conformant_real_literal(text), "expected reject: {text}");
    }
}

#[test]
fn formatter_produces_expected_spellings() {
    let cases = [
        (0.0, "0.0"),
        (-0.0, "-0.0"),
        (0.5, "0.5"),
        (-2.5, "-2.5"),
        (100_000.0, "100000.0"),
        (0.000_001, "0.000001"),
        (1.0e300, "1.0e+300"),
        (-1.5e300, "-1.5e+300"),
        (1.0e-300, "1.0e-300"),
        (1.0e21, "1.0e+21"),
    ];
    for (value, expected) in cases {
        let printed = format_real_literal(value).expect("finite value must format");
        assert_eq!(printed, expected, "for value {value}");
    }
}

#[test]
fn formatter_output_is_always_conformant() {
    let values = [
        0.0,
        -0.0,
        1.0,
        -1.0,
        0.1 + 0.2,
        std::f64::consts::PI,
        1.0e-42,
        -3.25e17,
        f64::MAX,
        f64::MIN_POSITIVE,
        5e-324, // smallest subnormal
    ];
    for value in values {
        let printed = format_real_literal(value).expect("finite value must format");
        assert!(
            is_conformant_real_literal(&printed),
            "nonconformant output {printed} for {value:e}"
        );
        let reparsed: f64 = printed.parse().expect("printed literal must reparse");
        assert_eq!(reparsed, value, "round-trip failed for {value:e}");
    }
}

#[test]
fn non_finite_values_rejected_with_stable_code() {
    for value in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        let error = format_real_literal(value).expect_err("non-finite must fail");
        assert!(matches!(error, GalecError::NonFiniteRealLiteral { .. }));
        assert_eq!(error.code(), "EG001");
    }
}
