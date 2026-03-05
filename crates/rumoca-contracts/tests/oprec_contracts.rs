//! OPREC (Operator Record) contract tests - MLS §14
//!
//! Tests for the 11 operator-record contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{expect_failure_in_phase_with_code, expect_success};
use rumoca_session::FailedPhase;

// =============================================================================
// OPREC-001: Encapsulated
// "Operator or operator function must be encapsulated"
// =============================================================================

#[test]
fn oprec_001_operator_record_encapsulated() {
    expect_success(
        r#"
        encapsulated operator record OR
            Real x;
        end OR;

        model Test
            OR a;
        equation
            a = OR(1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// OPREC-002: Single output
// "All operator functions shall return exactly one output"
// =============================================================================

#[test]
#[ignore = "TODO(OPREC-002): enforce single-output restriction for operator functions"]
fn oprec_002_operator_functions_single_output() {
    expect_failure_in_phase_with_code(
        r#"
        operator record OR
            Real x;

            operator function 'constructor'
                input Real x;
                output OR a;
                output OR b;
            algorithm
                a := OR(x);
                b := OR(x);
            end 'constructor';
        end OR;

        model Test
            OR a;
        equation
            a = OR(1.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// OPREC-003: Record input
// "Must have at least one component of record class as input (except constructor)"
// =============================================================================

#[test]
#[ignore = "TODO(OPREC-003): enforce operator-function record-input requirement"]
fn oprec_003_operator_function_requires_record_input() {
    expect_failure_in_phase_with_code(
        r#"
        operator record OR
            Real x;

            operator function '+'
                input Real a;
                input Real b;
                output OR c;
            algorithm
                c := OR(a + b);
            end '+';
        end OR;

        model Test
            OR a;
        equation
            a = OR(1.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// OPREC-004: Constructor output
// "Constructor shall return one component of the operator record class"
// =============================================================================

#[test]
fn oprec_004_constructor_output_is_operator_record() {
    expect_success(
        r#"
        operator record OR
            Real x;
        end OR;

        model Test
            OR a;
            OR b;
        equation
            a = OR(1.0);
            b = OR(a.x + 1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// OPREC-005: No multiple matches
// "For potential call, shall not exist multiple matches"
// =============================================================================

#[test]
#[ignore = "TODO(OPREC-005): enforce unique operator-function match resolution"]
fn oprec_005_operator_call_no_multiple_matches() {
    expect_failure_in_phase_with_code(
        r#"
        operator record OR
            Real x;

            operator function '+'
                input OR a;
                input OR b;
                output OR c;
            algorithm
                c := OR(a.x + b.x);
            end '+';

            operator function '+'
                input OR a;
                input OR b;
                output OR c;
            algorithm
                c := OR(a.x + b.x + 1.0);
            end '+';
        end OR;

        model Test
            OR a;
            OR b;
            OR c;
        equation
            a = OR(1.0);
            b = OR(2.0);
            c = a + b;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// OPREC-006: Binary defaults
// "First two inputs no default values, rest must have defaults"
// =============================================================================

#[test]
#[ignore = "TODO(OPREC-006): enforce binary-operator default argument rules"]
fn oprec_006_binary_defaults() {
    expect_failure_in_phase_with_code(
        r#"
        operator record OR
            Real x;

            operator function '+'
                input OR a = OR(1.0);
                input OR b;
                output OR c;
            algorithm
                c := OR(a.x + b.x);
            end '+';
        end OR;

        model Test
            OR a;
        equation
            a = OR(1.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// OPREC-007: Binary no ambiguity
// "Error if multiple functions match a binary operation"
// =============================================================================

#[test]
#[ignore = "TODO(OPREC-007): reject ambiguous binary operator overloads"]
fn oprec_007_binary_no_ambiguity() {
    expect_failure_in_phase_with_code(
        r#"
        operator record OR
            Real x;

            operator function '+'
                input OR a;
                input OR b;
                output OR c;
            algorithm
                c := OR(a.x + b.x);
            end '+';

            operator function '+'
                input OR a;
                input OR b;
                input Real k = 0.0;
                output OR c;
            algorithm
                c := OR(a.x + b.x + k);
            end '+';
        end OR;

        model Test
            OR a;
            OR b;
            OR c;
        equation
            a = OR(1.0);
            b = OR(2.0);
            c = a + b;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// OPREC-008: Zero operator single
// "'0' operator can only contain one function with zero inputs"
// =============================================================================

#[test]
#[ignore = "TODO(OPREC-008): enforce singleton zero-operator definition"]
fn oprec_008_zero_operator_single() {
    expect_failure_in_phase_with_code(
        r#"
        operator record OR
            Real x;

            operator function '0'
                output OR c;
            algorithm
                c := OR(0.0);
            end '0';

            operator function '0'
                output OR c;
            algorithm
                c := OR(1.0);
            end '0';
        end OR;

        model Test
            OR a;
        equation
            a = OR(1.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// OPREC-009: Constructor mutual exclusion
// "At most one of C.'constructor'(D) and D.'constructor'(C) shall be legal"
// =============================================================================

#[test]
#[ignore = "TODO(OPREC-009): enforce constructor mutual-exclusion across operator-record pairs"]
fn oprec_009_constructor_mutual_exclusion() {
    expect_failure_in_phase_with_code(
        r#"
        operator record C
            Real x;
        end C;

        operator record D
            Real x;
        end D;

        model Test
            C c;
            D d;
        equation
            c = C(1.0);
            d = D(c.x);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// OPREC-010: String operator output
// "operator A.'String' shall only contain functions declaring one output of String type"
// =============================================================================

#[test]
#[ignore = "TODO(OPREC-010): enforce String-operator output typing and arity"]
fn oprec_010_string_operator_output() {
    expect_failure_in_phase_with_code(
        r#"
        operator record OR
            Real x;

            operator function 'String'
                input OR a;
                output OR bad;
            algorithm
                bad := OR(a.x);
            end 'String';
        end OR;

        model Test
            OR a;
        equation
            a = OR(1.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// OPREC-011: Zero inner dimension
// "Zero inner dimension in matrix*vector/matrix uses '0' operator; error if missing"
// =============================================================================

#[test]
#[ignore = "TODO(OPREC-011): enforce zero-inner-dimension operator-record multiplication rule"]
fn oprec_011_zero_inner_dimension_requires_zero_operator() {
    expect_failure_in_phase_with_code(
        r#"
        operator record OR
            Real x;
        end OR;

        model Test
            OR a[0, 2];
            OR b[2];
        equation
            b = a * b;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}
