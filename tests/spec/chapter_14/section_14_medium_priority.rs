//! MLS §14: Medium Priority Operator Record Tests
//!
//! This module tests medium priority normative requirements:
//! - §14.1: Operator record syntax and semantics
//! - §14.2: Operator definitions (constructor, binary, unary, '0')
//! - §14.3: Operator restrictions (encapsulation, no extends)
//! - §14.4: Numeric operations
//! - §14.5: Relational operator overloading
//!
//! Reference: https://specification.modelica.org/master/overloaded-operators.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §14.1 OPERATOR RECORD SYNTAX
// ============================================================================

/// MLS §14.1: Operator record basic syntax
mod operator_record_syntax {
    use super::*;

    /// MLS: "operator record declaration"
    #[test]
    fn mls_14_1_operator_record_basic() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;
            end Complex;
            "#,
        );
    }

    /// MLS: "operator record with constructor"
    #[test]
    fn mls_14_1_operator_record_constructor() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator 'constructor'
                    function fromReal
                        input Real re;
                        input Real im = 0;
                        output Complex result;
                    algorithm
                        result.re := re;
                        result.im := im;
                    end fromReal;
                end 'constructor';
            end Complex;
            "#,
        );
    }

    /// Valid: Regular record (for comparison)
    #[test]
    fn mls_14_1_regular_record() {
        expect_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p(x = 1, y = 2);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §14.2 OPERATOR DEFINITIONS
// ============================================================================

/// MLS §14.2: Operator function definitions
mod operator_definitions {
    use super::*;

    /// MLS: "Binary '+' operator"
    #[test]
    fn mls_14_2_operator_add() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator '+'
                    function add
                        input Complex c1;
                        input Complex c2;
                        output Complex result;
                    algorithm
                        result.re := c1.re + c2.re;
                        result.im := c1.im + c2.im;
                    end add;
                end '+';
            end Complex;
            "#,
        );
    }

    /// MLS: "Unary '-' operator"
    #[test]
    fn mls_14_2_operator_unary_minus() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator '-'
                    function negate
                        input Complex c;
                        output Complex result;
                    algorithm
                        result.re := -c.re;
                        result.im := -c.im;
                    end negate;
                end '-';
            end Complex;
            "#,
        );
    }

    /// MLS: "Binary '*' operator"
    #[test]
    fn mls_14_2_operator_multiply() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator '*'
                    function multiply
                        input Complex c1;
                        input Complex c2;
                        output Complex result;
                    algorithm
                        result.re := c1.re * c2.re - c1.im * c2.im;
                        result.im := c1.re * c2.im + c1.im * c2.re;
                    end multiply;
                end '*';
            end Complex;
            "#,
        );
    }

    /// MLS: "'0' operator for zero element"
    #[test]
    fn mls_14_2_operator_zero() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator '0'
                    function zero
                        output Complex result;
                    algorithm
                        result.re := 0;
                        result.im := 0;
                    end zero;
                end '0';
            end Complex;
            "#,
        );
    }

    /// MLS: "'String' operator for conversion"
    #[test]
    fn mls_14_2_operator_string() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator 'String'
                    function toString
                        input Complex c;
                        output String s;
                    algorithm
                        s := String(c.re) + " + " + String(c.im) + "i";
                    end toString;
                end 'String';
            end Complex;
            "#,
        );
    }
}

// ============================================================================
// §14.3 OPERATOR RESTRICTIONS
// ============================================================================

/// MLS §14.3: Operator record restrictions
mod operator_restrictions {
    use super::*;

    /// MLS: "Operator functions must be encapsulated"
    #[test]
    fn mls_14_3_operator_must_be_encapsulated() {
        expect_failure(
            r#"
            operator record Complex
                Real re;
                Real im;

                operator '+'
                    function add
                        input Complex c1;
                        input Complex c2;
                        output Complex result;
                    algorithm
                        result.re := c1.re + c2.re;
                        result.im := c1.im + c2.im;
                    end add;
                end '+';
            end Complex;
            "#,
            "Complex",
        );
    }

    /// MLS: "Operator record cannot extend another operator record"
    #[test]
    #[ignore = "operator record extends restriction not yet implemented"]
    fn mls_14_3_operator_record_no_extends() {
        expect_failure(
            r#"
            operator record Base
                Real x;
            end Base;

            operator record Derived
                extends Base;
                Real y;
            end Derived;
            "#,
            "Derived",
        );
    }

    /// MLS: "Operator record cannot be partial"
    #[test]
    #[ignore = "operator record partial restriction not yet implemented"]
    fn mls_14_3_operator_record_no_partial() {
        expect_failure(
            r#"
            partial operator record BadComplex
                Real re;
                Real im;
            end BadComplex;
            "#,
            "BadComplex",
        );
    }
}

// ============================================================================
// §14.5 RELATIONAL OPERATOR OVERLOADING
// ============================================================================

/// MLS §14.5: Relational operator overloading
mod relational_operators {
    use super::*;

    /// MLS: "'==' operator for equality"
    #[test]
    fn mls_14_5_operator_equal() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator '=='
                    function isEqual
                        input Complex c1;
                        input Complex c2;
                        output Boolean result;
                    algorithm
                        result := c1.re == c2.re and c1.im == c2.im;
                    end isEqual;
                end '==';
            end Complex;
            "#,
        );
    }

    /// MLS: "'<>' operator for inequality"
    #[test]
    fn mls_14_5_operator_not_equal() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator '<>'
                    function isNotEqual
                        input Complex c1;
                        input Complex c2;
                        output Boolean result;
                    algorithm
                        result := c1.re <> c2.re or c1.im <> c2.im;
                    end isNotEqual;
                end '<>';
            end Complex;
            "#,
        );
    }
}

// ============================================================================
// §14.4 NUMERIC OPERATIONS
// ============================================================================

/// MLS §14.4: Numeric operation overloading
mod numeric_operations {
    use super::*;

    /// MLS: "'/' operator for division"
    #[test]
    fn mls_14_4_operator_divide() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator '/'
                    function divide
                        input Complex c1;
                        input Complex c2;
                        output Complex result;
                    protected
                        Real denom;
                    algorithm
                        denom := c2.re^2 + c2.im^2;
                        result.re := (c1.re*c2.re + c1.im*c2.im) / denom;
                        result.im := (c1.im*c2.re - c1.re*c2.im) / denom;
                    end divide;
                end '/';
            end Complex;
            "#,
        );
    }

    /// MLS: "'^' operator for power"
    #[test]
    fn mls_14_4_operator_power() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator '^'
                    function power
                        input Complex c;
                        input Integer n;
                        output Complex result;
                    algorithm
                        result.re := c.re;
                        result.im := c.im;
                    end power;
                end '^';
            end Complex;
            "#,
        );
    }
}
