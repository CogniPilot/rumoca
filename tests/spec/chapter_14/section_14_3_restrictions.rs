//! MLS §14.3: Operator Restrictions
//!
//! Tests for operator record restrictions including:
//! - Encapsulation requirements
//! - No extends for operator records
//! - Operator function requirements
//! - Operator naming restrictions
//!
//! Reference: https://specification.modelica.org/master/overloaded-operators.html

use crate::spec::{expect_failure, expect_parse_success};

// ============================================================================
// §14.3.1 ENCAPSULATION REQUIREMENTS
// ============================================================================

/// MLS §14.3: Encapsulation requirements for operators
mod encapsulation_requirements {
    use super::*;

    /// MLS: Operators must be encapsulated
    #[test]
    fn mls_14_3_operator_encapsulated() {
        expect_parse_success(
            r#"
            operator record Number
                Real value;

                encapsulated operator '+'
                    function add
                        input Number a;
                        input Number b;
                        output Number result;
                    algorithm
                        result.value := a.value + b.value;
                    end add;
                end '+';
            end Number;
            "#,
        );
    }

    /// MLS: Constructor must be encapsulated
    #[test]
    fn mls_14_3_constructor_encapsulated() {
        expect_parse_success(
            r#"
            operator record Point
                Real x;
                Real y;

                encapsulated operator 'constructor'
                    function create
                        input Real x;
                        input Real y;
                        output Point result;
                    algorithm
                        result.x := x;
                        result.y := y;
                    end create;
                end 'constructor';
            end Point;
            "#,
        );
    }

    /// MLS: Zero operator must be encapsulated
    #[test]
    fn mls_14_3_zero_encapsulated() {
        expect_parse_success(
            r#"
            operator record Number
                Real value;

                encapsulated operator '0'
                    function zero
                        output Number result;
                    algorithm
                        result.value := 0;
                    end zero;
                end '0';
            end Number;
            "#,
        );
    }
}

// ============================================================================
// §14.3.2 EXTENDS RESTRICTIONS
// ============================================================================

/// MLS §14.3: Extends restrictions for operator records
mod extends_restrictions {
    use super::*;

    /// MLS: Operator record without extends (valid)
    #[test]
    fn mls_14_3_no_extends_valid() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;
            end Complex;
            "#,
        );
    }

    /// MLS: Operator record cannot extend other classes
    #[test]
    #[ignore = "Operator record extends restriction not yet implemented"]
    fn mls_14_3_no_extends_error() {
        expect_failure(
            r#"
            record Base
                Real x;
            end Base;

            operator record Extended
                extends Base;
                Real y;
            end Extended;
            "#,
            "Extended",
        );
    }

    /// MLS: Operator record cannot be extended
    #[test]
    #[ignore = "Operator record extension restriction not yet implemented"]
    fn mls_14_3_cannot_be_extended() {
        expect_failure(
            r#"
            operator record Base
                Real x;
            end Base;

            operator record Extended
                extends Base;
                Real y;
            end Extended;
            "#,
            "Extended",
        );
    }
}

// ============================================================================
// §14.3.3 OPERATOR FUNCTION REQUIREMENTS
// ============================================================================

/// MLS §14.3: Operator function requirements
mod function_requirements {
    use super::*;

    /// MLS: Operator function must have correct signature
    #[test]
    fn mls_14_3_binary_signature() {
        expect_parse_success(
            r#"
            operator record Value
                Real x;

                encapsulated operator '+'
                    function add
                        input Value a;
                        input Value b;
                        output Value result;
                    algorithm
                        result.x := a.x + b.x;
                    end add;
                end '+';
            end Value;
            "#,
        );
    }

    /// MLS: Unary operator single input
    #[test]
    fn mls_14_3_unary_signature() {
        expect_parse_success(
            r#"
            operator record Value
                Real x;

                encapsulated operator '-'
                    function negate
                        input Value v;
                        output Value result;
                    algorithm
                        result.x := -v.x;
                    end negate;
                end '-';
            end Value;
            "#,
        );
    }

    /// MLS: Comparison operator returns Boolean
    #[test]
    fn mls_14_3_comparison_returns_boolean() {
        expect_parse_success(
            r#"
            operator record Value
                Real x;

                encapsulated operator '=='
                    function equal
                        input Value a;
                        input Value b;
                        output Boolean result;
                    algorithm
                        result := a.x == b.x;
                    end equal;
                end '==';
            end Value;
            "#,
        );
    }

    /// MLS: Constructor returns operator record type
    #[test]
    fn mls_14_3_constructor_returns_type() {
        expect_parse_success(
            r#"
            operator record Point
                Real x;
                Real y;

                encapsulated operator 'constructor'
                    function create
                        input Real x;
                        input Real y;
                        output Point result;
                    algorithm
                        result.x := x;
                        result.y := y;
                    end create;
                end 'constructor';
            end Point;
            "#,
        );
    }

    /// MLS: Zero operator returns operator record type
    #[test]
    fn mls_14_3_zero_returns_type() {
        expect_parse_success(
            r#"
            operator record Vector
                Real x;
                Real y;
                Real z;

                encapsulated operator '0'
                    function zero
                        output Vector result;
                    algorithm
                        result.x := 0;
                        result.y := 0;
                        result.z := 0;
                    end zero;
                end '0';
            end Vector;
            "#,
        );
    }
}

// ============================================================================
// §14.3.4 OPERATOR NAMING
// ============================================================================

/// MLS §14.3: Operator naming restrictions
mod naming_restrictions {
    use super::*;

    /// MLS: Valid operator names
    #[test]
    fn mls_14_3_valid_operator_names() {
        expect_parse_success(
            r#"
            operator record Full
                Real value;

                encapsulated operator 'constructor'
                    function make
                        output Full result;
                    algorithm
                        result.value := 0;
                    end make;
                end 'constructor';

                encapsulated operator '0'
                    function zero
                        output Full result;
                    algorithm
                        result.value := 0;
                    end zero;
                end '0';

                encapsulated operator '+'
                    function add
                        input Full a;
                        input Full b;
                        output Full result;
                    algorithm
                        result.value := a.value + b.value;
                    end add;
                end '+';

                encapsulated operator '-'
                    function subtract
                        input Full a;
                        input Full b;
                        output Full result;
                    algorithm
                        result.value := a.value - b.value;
                    end subtract;
                end '-';

                encapsulated operator '*'
                    function multiply
                        input Full a;
                        input Full b;
                        output Full result;
                    algorithm
                        result.value := a.value * b.value;
                    end multiply;
                end '*';

                encapsulated operator '/'
                    function divide
                        input Full a;
                        input Full b;
                        output Full result;
                    algorithm
                        result.value := a.value / b.value;
                    end divide;
                end '/';

                encapsulated operator '^'
                    function power
                        input Full base;
                        input Real exp;
                        output Full result;
                    algorithm
                        result.value := base.value ^ exp;
                    end power;
                end '^';

                encapsulated operator '=='
                    function equal
                        input Full a;
                        input Full b;
                        output Boolean result;
                    algorithm
                        result := a.value == b.value;
                    end equal;
                end '==';

                encapsulated operator 'String'
                    function convert
                        input Full f;
                        output String s;
                    algorithm
                        s := String(f.value);
                    end convert;
                end 'String';
            end Full;
            "#,
        );
    }
}

// ============================================================================
// §14.3.5 PARTIAL RESTRICTIONS
// ============================================================================

/// MLS §14.3: Partial operator record restrictions
mod partial_restrictions {
    use super::*;

    /// MLS: Operator record cannot be partial
    #[test]
    #[ignore = "Partial operator record restriction not yet implemented"]
    fn mls_14_3_no_partial_error() {
        expect_failure(
            r#"
            partial operator record Incomplete
                Real value;
            end Incomplete;
            "#,
            "Incomplete",
        );
    }
}
