//! MLS §14.2: Operator Definitions
//!
//! Tests for operator definitions including:
//! - Binary operators (+, -, *, /, ^)
//! - Unary operators (-, not)
//! - Comparison operators (==, <>, <, >, <=, >=)
//! - String operator
//!
//! Reference: https://specification.modelica.org/master/overloaded-operators.html

use crate::spec::expect_parse_success;

// ============================================================================
// §14.2.1 BINARY OPERATORS
// ============================================================================

/// MLS §14.2: Binary operator definitions
mod binary_operators {
    use super::*;

    /// MLS: Addition operator '+'
    #[test]
    fn mls_14_2_operator_add() {
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

    /// MLS: Subtraction operator '-' (binary)
    #[test]
    fn mls_14_2_operator_subtract() {
        expect_parse_success(
            r#"
            operator record Number
                Real value;

                encapsulated operator '-'
                    function subtract
                        input Number a;
                        input Number b;
                        output Number result;
                    algorithm
                        result.value := a.value - b.value;
                    end subtract;
                end '-';
            end Number;
            "#,
        );
    }

    /// MLS: Multiplication operator '*'
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

    /// MLS: Division operator '/'
    #[test]
    fn mls_14_2_operator_divide() {
        expect_parse_success(
            r#"
            operator record Number
                Real value;

                encapsulated operator '/'
                    function divide
                        input Number a;
                        input Number b;
                        output Number result;
                    algorithm
                        result.value := a.value / b.value;
                    end divide;
                end '/';
            end Number;
            "#,
        );
    }

    /// MLS: Power operator '^'
    #[test]
    fn mls_14_2_operator_power() {
        expect_parse_success(
            r#"
            operator record Number
                Real value;

                encapsulated operator '^'
                    function power
                        input Number base;
                        input Real exponent;
                        output Number result;
                    algorithm
                        result.value := base.value ^ exponent;
                    end power;
                end '^';
            end Number;
            "#,
        );
    }

    /// MLS: Element-wise operators '.*', './', '.^'
    #[test]
    fn mls_14_2_elementwise_operators() {
        expect_parse_success(
            r#"
            operator record Vector
                Real x[3];

                encapsulated operator '.*'
                    function elementMultiply
                        input Vector a;
                        input Vector b;
                        output Vector result;
                    algorithm
                        for i in 1:3 loop
                            result.x[i] := a.x[i] * b.x[i];
                        end for;
                    end elementMultiply;
                end '.*';
            end Vector;
            "#,
        );
    }
}

// ============================================================================
// §14.2.2 UNARY OPERATORS
// ============================================================================

/// MLS §14.2: Unary operator definitions
mod unary_operators {
    use super::*;

    /// MLS: Unary minus operator '-'
    #[test]
    fn mls_14_2_unary_minus() {
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

    /// MLS: Unary plus operator '+'
    #[test]
    fn mls_14_2_unary_plus() {
        expect_parse_success(
            r#"
            operator record Number
                Real value;

                encapsulated operator '+'
                    function positive
                        input Number n;
                        output Number result;
                    algorithm
                        result.value := n.value;
                    end positive;
                end '+';
            end Number;
            "#,
        );
    }

    /// MLS: Not operator 'not'
    #[test]
    fn mls_14_2_operator_not() {
        expect_parse_success(
            r#"
            operator record LogicValue
                Boolean value;

                encapsulated operator 'not'
                    function negate
                        input LogicValue v;
                        output LogicValue result;
                    algorithm
                        result.value := not v.value;
                    end negate;
                end 'not';
            end LogicValue;
            "#,
        );
    }
}

// ============================================================================
// §14.2.3 COMPARISON OPERATORS
// ============================================================================

/// MLS §14.2: Comparison operator definitions
mod comparison_operators {
    use super::*;

    /// MLS: Equality operator '=='
    #[test]
    fn mls_14_2_operator_equal() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator '=='
                    function equal
                        input Complex c1;
                        input Complex c2;
                        output Boolean result;
                    algorithm
                        result := c1.re == c2.re and c1.im == c2.im;
                    end equal;
                end '==';
            end Complex;
            "#,
        );
    }

    /// MLS: Inequality operator '<>'
    #[test]
    fn mls_14_2_operator_notequal() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator '<>'
                    function notEqual
                        input Complex c1;
                        input Complex c2;
                        output Boolean result;
                    algorithm
                        result := c1.re <> c2.re or c1.im <> c2.im;
                    end notEqual;
                end '<>';
            end Complex;
            "#,
        );
    }

    /// MLS: Less than operator '<'
    #[test]
    fn mls_14_2_operator_less() {
        expect_parse_success(
            r#"
            operator record Magnitude
                Real value;

                encapsulated operator '<'
                    function lessThan
                        input Magnitude a;
                        input Magnitude b;
                        output Boolean result;
                    algorithm
                        result := a.value < b.value;
                    end lessThan;
                end '<';
            end Magnitude;
            "#,
        );
    }

    /// MLS: Greater than operator '>'
    #[test]
    fn mls_14_2_operator_greater() {
        expect_parse_success(
            r#"
            operator record Magnitude
                Real value;

                encapsulated operator '>'
                    function greaterThan
                        input Magnitude a;
                        input Magnitude b;
                        output Boolean result;
                    algorithm
                        result := a.value > b.value;
                    end greaterThan;
                end '>';
            end Magnitude;
            "#,
        );
    }

    /// MLS: Less than or equal operator '<='
    #[test]
    fn mls_14_2_operator_lessequal() {
        expect_parse_success(
            r#"
            operator record Magnitude
                Real value;

                encapsulated operator '<='
                    function lessThanOrEqual
                        input Magnitude a;
                        input Magnitude b;
                        output Boolean result;
                    algorithm
                        result := a.value <= b.value;
                    end lessThanOrEqual;
                end '<=';
            end Magnitude;
            "#,
        );
    }

    /// MLS: Greater than or equal operator '>='
    #[test]
    fn mls_14_2_operator_greaterequal() {
        expect_parse_success(
            r#"
            operator record Magnitude
                Real value;

                encapsulated operator '>='
                    function greaterThanOrEqual
                        input Magnitude a;
                        input Magnitude b;
                        output Boolean result;
                    algorithm
                        result := a.value >= b.value;
                    end greaterThanOrEqual;
                end '>=';
            end Magnitude;
            "#,
        );
    }
}

// ============================================================================
// §14.2.4 LOGICAL OPERATORS
// ============================================================================

/// MLS §14.2: Logical operator definitions
mod logical_operators {
    use super::*;

    /// MLS: And operator 'and'
    #[test]
    fn mls_14_2_operator_and() {
        expect_parse_success(
            r#"
            operator record Logic
                Boolean value;

                encapsulated operator 'and'
                    function logicAnd
                        input Logic a;
                        input Logic b;
                        output Logic result;
                    algorithm
                        result.value := a.value and b.value;
                    end logicAnd;
                end 'and';
            end Logic;
            "#,
        );
    }

    /// MLS: Or operator 'or'
    #[test]
    fn mls_14_2_operator_or() {
        expect_parse_success(
            r#"
            operator record Logic
                Boolean value;

                encapsulated operator 'or'
                    function logicOr
                        input Logic a;
                        input Logic b;
                        output Logic result;
                    algorithm
                        result.value := a.value or b.value;
                    end logicOr;
                end 'or';
            end Logic;
            "#,
        );
    }
}

// ============================================================================
// §14.2.5 STRING OPERATOR
// ============================================================================

/// MLS §14.2: String operator
mod string_operator {
    use super::*;

    /// MLS: String conversion operator 'String'
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
// MIXED OPERATORS
// ============================================================================

/// MLS §14.2: Mixed type operators
mod mixed_operators {
    use super::*;

    /// MLS: Operator with scalar multiplication
    #[test]
    fn mls_14_2_scalar_multiply() {
        expect_parse_success(
            r#"
            operator record Vector
                Real x[3];

                encapsulated operator '*'
                    function scaleLeft
                        input Real s;
                        input Vector v;
                        output Vector result;
                    algorithm
                        for i in 1:3 loop
                            result.x[i] := s * v.x[i];
                        end for;
                    end scaleLeft;

                    function scaleRight
                        input Vector v;
                        input Real s;
                        output Vector result;
                    algorithm
                        for i in 1:3 loop
                            result.x[i] := v.x[i] * s;
                        end for;
                    end scaleRight;
                end '*';
            end Vector;
            "#,
        );
    }

    /// MLS: Operator with different return types
    #[test]
    fn mls_14_2_cross_type_operator() {
        expect_parse_success(
            r#"
            operator record Vector3
                Real x;
                Real y;
                Real z;

                encapsulated operator '*'
                    function dot
                        input Vector3 a;
                        input Vector3 b;
                        output Real result;
                    algorithm
                        result := a.x * b.x + a.y * b.y + a.z * b.z;
                    end dot;
                end '*';
            end Vector3;
            "#,
        );
    }
}
