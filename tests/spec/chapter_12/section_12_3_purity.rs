//! MLS §12.3: Pure Modelica Functions
//!
//! This file tests the purity rules for Modelica functions.
//! Pure functions always produce the same outputs for the same inputs.
//!
//! Reference: https://specification.modelica.org/master/functions.html#pure-modelica-functions

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §12.3 PURE FUNCTION DECLARATIONS
// ============================================================================

/// MLS §12.3: Pure function declarations
mod section_12_3_pure_declaration {
    use super::*;

    /// MLS §12.3: Explicit pure keyword
    #[test]
    fn mls_12_3_explicit_pure() {
        expect_parse_success(
            r#"
            pure function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;
            "#,
        );
    }

    /// MLS §12.3: Pure function with multiple inputs
    #[test]
    fn mls_12_3_pure_multiple_inputs() {
        expect_parse_success(
            r#"
            pure function Add
                input Real a;
                input Real b;
                input Real c;
                output Real result;
            algorithm
                result := a + b + c;
            end Add;
            "#,
        );
    }

    /// MLS §12.3: Pure function with multiple outputs
    #[test]
    fn mls_12_3_pure_multiple_outputs() {
        expect_parse_success(
            r#"
            pure function MinMax
                input Real x;
                input Real y;
                output Real minVal;
                output Real maxVal;
            algorithm
                if x < y then
                    minVal := x;
                    maxVal := y;
                else
                    minVal := y;
                    maxVal := x;
                end if;
            end MinMax;
            "#,
        );
    }

    /// MLS §12.3: Pure function with protected variables
    #[test]
    fn mls_12_3_pure_with_protected() {
        expect_parse_success(
            r#"
            pure function Normalize
                input Real x;
                input Real y;
                output Real nx;
                output Real ny;
            protected
                Real len;
            algorithm
                len := sqrt(x*x + y*y);
                if len > 0 then
                    nx := x / len;
                    ny := y / len;
                else
                    nx := 0;
                    ny := 0;
                end if;
            end Normalize;
            "#,
        );
    }

    /// MLS §12.3: Pure function calling other pure functions
    #[test]
    fn mls_12_3_pure_calling_pure() {
        expect_parse_success(
            r#"
            pure function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            pure function SumOfSquares
                input Real a;
                input Real b;
                output Real result;
            algorithm
                result := Square(a) + Square(b);
            end SumOfSquares;
            "#,
        );
    }

    /// MLS §12.3: "Non-external Modelica functions are normally pure"
    #[test]
    fn mls_12_3_implicit_pure() {
        // Functions without explicit pure/impure are implicitly pure
        expect_parse_success(
            r#"
            function ImplicitPure
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end ImplicitPure;
            "#,
        );
    }
}

// ============================================================================
// §12.3 IMPURE FUNCTION DECLARATIONS
// ============================================================================

/// MLS §12.3: Impure function declarations
mod section_12_3_impure_declaration {
    use super::*;

    /// MLS §12.3: Explicit impure keyword
    #[test]
    fn mls_12_3_explicit_impure() {
        expect_parse_success(
            r#"
            impure function PrintAndReturn
                input Real x;
                output Real y;
            algorithm
                y := x;
            end PrintAndReturn;
            "#,
        );
    }

    /// MLS §12.3: Impure function with side effects (conceptual)
    #[test]
    fn mls_12_3_impure_side_effects() {
        expect_parse_success(
            r#"
            impure function Logger
                input String message;
                input Real value;
                output Real result;
            algorithm
                result := value;
            end Logger;
            "#,
        );
    }

    /// MLS §12.3: Impure function calling impure
    #[test]
    fn mls_12_3_impure_calling_impure() {
        expect_parse_success(
            r#"
            impure function Log
                input Real x;
                output Real y;
            algorithm
                y := x;
            end Log;

            impure function LogTwice
                input Real x;
                output Real y;
            algorithm
                y := Log(Log(x));
            end LogTwice;
            "#,
        );
    }
}

// ============================================================================
// §12.3 IMPURE FUNCTION USAGE CONTEXTS
// ============================================================================

/// MLS §12.3: Contexts where impure functions may be called
mod section_12_3_impure_contexts {
    use super::*;

    /// MLS §12.3: Impure function in when-equation
    #[test]
    fn mls_12_3_impure_in_when_equation() {
        expect_success(
            r#"
            impure function LogEvent
                input Real x;
                output Real y;
            algorithm
                y := x;
            end LogEvent;

            model Test
                Real x(start = 0);
                discrete Real logged(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    logged = LogEvent(x);
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §12.3: Impure function in when-statement
    #[test]
    fn mls_12_3_impure_in_when_statement() {
        expect_parse_success(
            r#"
            impure function LogEvent
                input Real x;
                output Real y;
            algorithm
                y := x;
            end LogEvent;

            model Test
                Real x(start = 0);
                discrete Real logged(start = 0);
            algorithm
                when x > 1 then
                    logged := LogEvent(x);
                end when;
            equation
                der(x) = 1;
            end Test;
            "#,
        );
    }

    /// MLS §12.3: Impure function in initial equation
    #[test]
    fn mls_12_3_impure_in_initial_equation() {
        expect_success(
            r#"
            impure function GetInitialValue
                output Real y;
            algorithm
                y := 1.0;
            end GetInitialValue;

            model Test
                Real x;
            initial equation
                x = GetInitialValue();
            equation
                der(x) = -x;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §12.3: Impure function in initial algorithm
    #[test]
    fn mls_12_3_impure_in_initial_algorithm() {
        expect_parse_success(
            r#"
            impure function GetInitialValue
                output Real y;
            algorithm
                y := 1.0;
            end GetInitialValue;

            model Test
                Real x(start = 0);
            initial algorithm
                x := GetInitialValue();
            equation
                der(x) = -x;
            end Test;
            "#,
        );
    }

    /// MLS §12.3: Impure function in parameter binding
    #[test]
    fn mls_12_3_impure_in_parameter_binding() {
        expect_parse_success(
            r#"
            impure function ReadParameter
                output Real y;
            algorithm
                y := 1.0;
            end ReadParameter;

            model Test
                parameter Real p = ReadParameter();
                Real x(start = p);
            equation
                der(x) = -x;
            end Test;
            "#,
        );
    }

    /// MLS §12.3: Impure function inside another impure function
    #[test]
    fn mls_12_3_impure_in_impure() {
        expect_parse_success(
            r#"
            impure function Inner
                input Real x;
                output Real y;
            algorithm
                y := x;
            end Inner;

            impure function Outer
                input Real x;
                output Real y;
            algorithm
                y := Inner(x) * 2;
            end Outer;
            "#,
        );
    }
}

// ============================================================================
// §12.3 PURE WRAPPER
// ============================================================================

/// MLS §12.3: pure() wrapper expression
mod section_12_3_pure_wrapper {
    use super::*;

    /// MLS §12.3: pure() wrapper for impure function
    #[test]
    #[ignore = "pure() wrapper not yet implemented"]
    fn mls_12_3_pure_wrapper_basic() {
        expect_success(
            r#"
            impure function GetValue
                output Real y;
            algorithm
                y := 1.0;
            end GetValue;

            model Test
                Real x = pure(GetValue());
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §12.3: pure() wrapper in expression
    #[test]
    #[ignore = "pure() wrapper not yet implemented"]
    fn mls_12_3_pure_wrapper_expression() {
        expect_success(
            r#"
            impure function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;

            model Test
                Real a = 5;
                Real b = pure(F(a)) + 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.3 EXTERNAL FUNCTION PURITY
// ============================================================================

/// MLS §12.3: External function purity
mod section_12_3_external_purity {
    use super::*;

    /// MLS §12.3: External function with explicit pure
    #[test]
    fn mls_12_3_external_pure() {
        expect_parse_success(
            r#"
            pure function ExternalSin
                input Real x;
                output Real y;
            external "C" y = sin(x);
            end ExternalSin;
            "#,
        );
    }

    /// MLS §12.3: External function with explicit impure
    #[test]
    fn mls_12_3_external_impure() {
        expect_parse_success(
            r#"
            impure function ExternalPrint
                input String message;
                output Integer status;
            external "C" status = printf(message);
            end ExternalPrint;
            "#,
        );
    }

    /// MLS §12.3: External function without purity (deprecated but allowed)
    #[test]
    fn mls_12_3_external_no_purity() {
        expect_parse_success(
            r#"
            function ExternalFunc
                input Real x;
                output Real y;
            external "C";
            end ExternalFunc;
            "#,
        );
    }
}
