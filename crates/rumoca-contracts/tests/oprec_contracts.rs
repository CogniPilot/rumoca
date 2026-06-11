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

// =============================================================================
// OPREC-006: First two inputs no default values, rest must have defaults
// =============================================================================

#[test]
fn oprec_006_operator_input_default_rejected() {
    rumoca_contracts::test_support::expect_resolve_failure_with_code(
        r#"
        package P
            operator record OR
                Real re;
                encapsulated operator function '+'
                    import P.OR;
                    input OR a;
                    input OR b = OR(0);
                    output OR c = OR(a.re + b.re);
                end '+';
            end OR;
            model M
                OR x = OR(1);
                OR y = OR(2);
                OR z = x + y;
            end M;
        end P;
    "#,
        "P.M",
        "ER110",
    );
}

// =============================================================================
// OPREC-005: For potential call, shall not exist multiple matches
// =============================================================================

#[test]
fn oprec_005_identical_overload_signatures_rejected() {
    rumoca_contracts::test_support::expect_resolve_failure_with_code(
        r#"
        package P
            operator record OR
                Real re;
                encapsulated operator '*'
                    function mul1
                        import P.OR;
                        input OR a;
                        input OR b;
                        output OR c = OR(a.re * b.re);
                    end mul1;
                    function mul2
                        import P.OR;
                        input OR a;
                        input OR b;
                        output OR c = OR(a.re * b.re * 2);
                    end mul2;
                end '*';
            end OR;
            model M
                OR x = OR(1);
                OR y = OR(2);
                OR z = x * y;
            end M;
        end P;
    "#,
        "P.M",
        "ER122",
    );
}

// =============================================================================
// OPREC-007: Error if multiple functions match a binary operation
// =============================================================================

#[test]
fn oprec_007_ambiguous_binary_operator_match_rejected() {
    rumoca_contracts::test_support::expect_resolve_failure_with_code(
        r#"
        package P
            operator record OR
                Real re;
                encapsulated operator '+'
                    function add1
                        import P.OR;
                        input OR a;
                        input OR b;
                        output OR c = OR(a.re + b.re);
                    end add1;
                    function add2
                        import P.OR;
                        input OR a;
                        input OR b;
                        output OR c = OR(a.re - b.re);
                    end add2;
                end '+';
            end OR;
            model M
                OR x = OR(1);
                OR y = OR(2);
                OR z = x + y;
            end M;
        end P;
    "#,
        "P.M",
        "ER122",
    );
}

// =============================================================================
// OPREC-009: For pair of operator record classes C and D, at most one of
// C.'constructor'(d) and D.'constructor'(c) shall be legal
// =============================================================================

#[test]
fn oprec_009_cross_constructor_pair_rejected() {
    rumoca_contracts::test_support::expect_resolve_failure_with_code(
        r#"
        package P
            operator record C
                Real re;
                encapsulated operator 'constructor'
                    function fromD
                        import P.D;
                        import P.C;
                        input D d;
                        output C c = C(d.v);
                    end fromD;
                end 'constructor';
            end C;
            operator record D
                Real v;
                encapsulated operator 'constructor'
                    function fromC
                        import P.C;
                        import P.D;
                        input C c;
                        output D d = D(c.re);
                    end fromC;
                end 'constructor';
            end D;
            model M
                C x = C(1.0);
            end M;
        end P;
    "#,
        "P.M",
        "ER125",
    );
}

// =============================================================================
// OPREC-011: If inner dimension is zero for matrix*vector/matrix, uses '0'
// operator; error if '0' not defined. Operator-record arrays with zero inner
// dimensions are rejected (no '0' operator support yet), pinned here.
// =============================================================================

#[test]
fn oprec_011_zero_inner_dimension_product_rejected() {
    rumoca_contracts::test_support::expect_resolve_failure_with_code(
        r#"
        package P
            operator record OR
                Real re;
            end OR;
            model M
                OR a[2, 0];
                OR b[0];
                OR c[2];
            equation
                c = a * b;
            end M;
        end P;
    "#,
        "P.M",
        "ER129",
    );
}
