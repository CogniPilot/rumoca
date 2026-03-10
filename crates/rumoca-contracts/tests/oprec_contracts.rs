//! Operator record contract tests - MLS §14

use rumoca_contracts::test_support::{expect_resolve_failure_with_code, expect_success};

#[test]
fn oprec_operator_record_structure_allows_record_fields_and_operator_declarations() {
    expect_success(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator '+'
                function add
                    input Complex a;
                    input Complex b;
                    output Complex c;
                algorithm
                    c := Complex(a.re + b.re, a.im + b.im);
                end add;
            end '+';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// OPREC-001: Encapsulated
// "Operator or operator function must be encapsulated"
// =============================================================================

#[test]
fn oprec_001_encapsulated_operator_ok() {
    expect_success(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator '+'
                function add
                    input Complex a;
                    input Complex b;
                    output Complex c;
                algorithm
                    c := Complex(a.re + b.re, a.im + b.im);
                end add;
            end '+';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn oprec_001_unencapsulated_operator_rejected() {
    expect_resolve_failure_with_code(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            operator '+'
                function add
                    input Complex a;
                    input Complex b;
                    output Complex c;
                algorithm
                    c := Complex(a.re + b.re, a.im + b.im);
                end add;
            end '+';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
        "ER071",
    );
}

// =============================================================================
// OPREC-002: Single output
// "All operator functions shall return exactly one output"
// =============================================================================

#[test]
fn oprec_002_single_output_ok() {
    expect_success(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator '+'
                function add
                    input Complex a;
                    input Complex b;
                    output Complex c;
                algorithm
                    c := Complex(a.re + b.re, a.im + b.im);
                end add;
            end '+';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn oprec_002_multiple_outputs_rejected() {
    expect_resolve_failure_with_code(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator '+'
                function add
                    input Complex a;
                    input Complex b;
                    output Complex c;
                    output Complex d;
                algorithm
                    c := Complex(a.re + b.re, a.im + b.im);
                    d := c;
                end add;
            end '+';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
        "ER055",
    );
}

// =============================================================================
// OPREC-003: Record input
// "Must have at least one component of record class as input (except constructor)"
// =============================================================================

#[test]
fn oprec_003_record_input_ok() {
    expect_success(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator '+'
                function add
                    input Complex a;
                    input Complex b;
                    output Complex c;
                algorithm
                    c := Complex(a.re + b.re, a.im + b.im);
                end add;
            end '+';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn oprec_003_missing_record_input_rejected() {
    expect_resolve_failure_with_code(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator '+'
                function add
                    input Real a;
                    input Real b;
                    output Complex c;
                algorithm
                    c := Complex(a + b, a - b);
                end add;
            end '+';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
        "ER056",
    );
}

// =============================================================================
// OPREC-004: Constructor output
// "Constructor shall return one component of the operator record class"
// =============================================================================

#[test]
fn oprec_004_constructor_output_ok() {
    expect_success(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator 'constructor'
                function from_real
                    input Real x;
                    output Complex c;
                algorithm
                    c := Complex(x, x);
                end from_real;
            end 'constructor';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn oprec_004_constructor_output_rejected() {
    expect_resolve_failure_with_code(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator 'constructor'
                function from_real
                    input Real x;
                    output Real y;
                algorithm
                    y := x;
                end from_real;
            end 'constructor';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
        "ER057",
    );
}

// =============================================================================
// OPREC-008: Zero operator single
// "'0' operator can only contain one function with zero inputs"
// =============================================================================

#[test]
fn oprec_008_zero_operator_single_zero_input_ok() {
    expect_success(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator '0'
                function zero
                    output Complex c;
                algorithm
                    c := Complex(0, 0);
                end zero;
            end '0';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn oprec_008_zero_operator_multiple_functions_rejected() {
    expect_resolve_failure_with_code(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator '0'
                function zero
                    output Complex c;
                algorithm
                    c := Complex(0, 0);
                end zero;

                function zero2
                    output Complex c;
                algorithm
                    c := Complex(0, 0);
                end zero2;
            end '0';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
        "ER072",
    );
}

#[test]
fn oprec_008_zero_operator_with_input_rejected() {
    expect_resolve_failure_with_code(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator '0'
                function zero
                    input Complex a;
                    output Complex c;
                algorithm
                    c := a;
                end zero;
            end '0';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
        "ER072",
    );
}

// =============================================================================
// OPREC-010: String operator output
// "operator A.'String' shall only contain functions declaring one output of String type"
// =============================================================================

#[test]
fn oprec_010_string_output_ok() {
    expect_success(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator 'String'
                function to_string
                    input Complex a;
                    output String s;
                algorithm
                    s := "complex";
                end to_string;
            end 'String';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn oprec_010_non_string_output_rejected() {
    expect_resolve_failure_with_code(
        r#"
        encapsulated operator record Complex
            Real re;
            Real im;

            encapsulated operator 'String'
                function to_string
                    input Complex a;
                    output Real y;
                algorithm
                    y := a.re;
                end to_string;
            end 'String';
        end Complex;

        model Test
            Complex c;
        equation
            c = Complex(1, 2);
        end Test;
    "#,
        "Test",
        "ER058",
    );
}
