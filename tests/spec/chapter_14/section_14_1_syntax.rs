//! MLS §14.1: Operator Record Syntax
//!
//! Tests for operator record syntax including:
//! - Operator record declaration
//! - Constructor definitions
//! - Field declarations
//! - Encapsulation requirements
//!
//! Reference: https://specification.modelica.org/master/overloaded-operators.html

use crate::spec::expect_parse_success;

// ============================================================================
// §14.1.1 OPERATOR RECORD DECLARATION
// ============================================================================

/// MLS §14.1: Operator record declaration syntax
mod declaration_syntax {
    use super::*;

    /// MLS: Basic operator record
    #[test]
    fn mls_14_1_basic_declaration() {
        expect_parse_success(
            r#"
            operator record Simple
                Real value;
            end Simple;
            "#,
        );
    }

    /// MLS: Operator record with multiple fields
    #[test]
    fn mls_14_1_multiple_fields() {
        expect_parse_success(
            r#"
            operator record Vector3
                Real x;
                Real y;
                Real z;
            end Vector3;
            "#,
        );
    }

    /// MLS: Operator record with default values
    #[test]
    fn mls_14_1_field_defaults() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re = 0;
                Real im = 0;
            end Complex;
            "#,
        );
    }

    /// MLS: Nested operator record fields
    #[test]
    fn mls_14_1_nested_fields() {
        expect_parse_success(
            r#"
            operator record Point
                Real x;
                Real y;
            end Point;

            operator record Line
                Point start;
                Point finish;
            end Line;
            "#,
        );
    }

    /// MLS: Operator record with array fields
    #[test]
    fn mls_14_1_array_fields() {
        expect_parse_success(
            r#"
            operator record Matrix2x2
                Real m[2, 2];
            end Matrix2x2;
            "#,
        );
    }
}

// ============================================================================
// §14.1.2 CONSTRUCTOR SYNTAX
// ============================================================================

/// MLS §14.1: Constructor syntax for operator records
mod constructor_syntax {
    use super::*;

    /// MLS: Basic constructor
    #[test]
    fn mls_14_1_basic_constructor() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator 'constructor'
                    function fromReals
                        input Real re;
                        input Real im;
                        output Complex result;
                    algorithm
                        result.re := re;
                        result.im := im;
                    end fromReals;
                end 'constructor';
            end Complex;
            "#,
        );
    }

    /// MLS: Constructor with default arguments
    #[test]
    fn mls_14_1_constructor_defaults() {
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

    /// MLS: Multiple constructors
    #[test]
    fn mls_14_1_multiple_constructors() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator 'constructor'
                    function fromReals
                        input Real re;
                        input Real im;
                        output Complex result;
                    algorithm
                        result.re := re;
                        result.im := im;
                    end fromReals;

                    function fromPolar
                        input Real r;
                        input Real phi;
                        output Complex result;
                    algorithm
                        result.re := r * cos(phi);
                        result.im := r * sin(phi);
                    end fromPolar;
                end 'constructor';
            end Complex;
            "#,
        );
    }

    /// MLS: Constructor creating array
    #[test]
    fn mls_14_1_constructor_array() {
        expect_parse_success(
            r#"
            operator record Vector
                Real x[3];

                encapsulated operator 'constructor'
                    function fromValues
                        input Real a;
                        input Real b;
                        input Real c;
                        output Vector result;
                    algorithm
                        result.x := {a, b, c};
                    end fromValues;
                end 'constructor';
            end Vector;
            "#,
        );
    }
}

// ============================================================================
// §14.1.3 ZERO OPERATOR
// ============================================================================

/// MLS §14.1: Zero operator syntax
mod zero_operator {
    use super::*;

    /// MLS: Basic '0' operator
    #[test]
    fn mls_14_1_zero_operator() {
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

    /// MLS: Zero for matrix type
    #[test]
    fn mls_14_1_zero_matrix() {
        expect_parse_success(
            r#"
            operator record Matrix2x2
                Real m[2, 2];

                encapsulated operator '0'
                    function zero
                        output Matrix2x2 result;
                    algorithm
                        result.m := zeros(2, 2);
                    end zero;
                end '0';
            end Matrix2x2;
            "#,
        );
    }
}

// ============================================================================
// §14.1.4 ENCAPSULATION
// ============================================================================

/// MLS §14.1: Encapsulation requirements
mod encapsulation_syntax {
    use super::*;

    /// MLS: Encapsulated operator
    #[test]
    fn mls_14_1_encapsulated_operator() {
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

    /// MLS: Multiple encapsulated operators
    #[test]
    fn mls_14_1_multiple_encapsulated() {
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
}

// ============================================================================
// COMPLEX EXAMPLES
// ============================================================================

/// Complex operator record examples
mod complex_examples {
    use super::*;

    /// Complex number full definition
    #[test]
    fn example_complex_full() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re "Real part";
                Real im "Imaginary part";

                encapsulated operator 'constructor'
                    function fromReals
                        input Real re;
                        input Real im = 0;
                        output Complex result;
                    algorithm
                        result.re := re;
                        result.im := im;
                    end fromReals;
                end 'constructor';

                encapsulated operator '0'
                    function zero
                        output Complex result;
                    algorithm
                        result.re := 0;
                        result.im := 0;
                    end zero;
                end '0';

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

                encapsulated operator '-'
                    function negate
                        input Complex c;
                        output Complex result;
                    algorithm
                        result.re := -c.re;
                        result.im := -c.im;
                    end negate;

                    function subtract
                        input Complex c1;
                        input Complex c2;
                        output Complex result;
                    algorithm
                        result.re := c1.re - c2.re;
                        result.im := c1.im - c2.im;
                    end subtract;
                end '-';

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

    /// Quaternion definition
    #[test]
    fn example_quaternion() {
        expect_parse_success(
            r#"
            operator record Quaternion
                Real w;
                Real x;
                Real y;
                Real z;

                encapsulated operator 'constructor'
                    function fromComponents
                        input Real w;
                        input Real x;
                        input Real y;
                        input Real z;
                        output Quaternion result;
                    algorithm
                        result.w := w;
                        result.x := x;
                        result.y := y;
                        result.z := z;
                    end fromComponents;

                    function identity
                        output Quaternion result;
                    algorithm
                        result.w := 1;
                        result.x := 0;
                        result.y := 0;
                        result.z := 0;
                    end identity;
                end 'constructor';
            end Quaternion;
            "#,
        );
    }
}
