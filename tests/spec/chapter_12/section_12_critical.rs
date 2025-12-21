//! MLS §12: Critical Function Body Restriction Tests
//!
//! This module tests critical normative requirements for functions:
//! - §12.2: Functions shall not have equation sections
//! - §12.2: Functions shall not have when-statements
//! - §12.2: Forbidden operators in functions (der, initial, pre, etc.)
//! - §12.2: Public components must be input or output
//! - §12.2: Input is read-only
//! - §12.2: Cannot contain model, block, operator, or connector
//!
//! Reference: https://specification.modelica.org/master/functions.html

use crate::spec::{expect_failure, expect_parse_failure, expect_success};

// ============================================================================
// §12.2 FUNCTION EQUATION RESTRICTION
// ============================================================================

/// MLS §12.2: Functions shall not have equation sections
mod function_equation_restriction {
    use super::*;

    /// MLS: "Functions shall not have equations"
    #[test]
    #[ignore = "Function equation restriction not yet enforced"]
    fn mls_12_2_function_with_equation_section() {
        expect_parse_failure(
            r#"
            function F
                input Real x;
                output Real y;
            equation
                y = x * 2;
            end F;
            "#,
        );
    }

    /// MLS: "Functions shall not have initial equations"
    #[test]
    #[ignore = "Function initial equation restriction not yet enforced"]
    fn mls_12_2_function_with_initial_equation() {
        expect_parse_failure(
            r#"
            function F
                input Real x;
                output Real y;
            initial equation
                y = 0;
            algorithm
                y := x;
            end F;
            "#,
        );
    }

    /// MLS: "Functions shall not have initial algorithms"
    #[test]
    #[ignore = "Function initial algorithm restriction not yet enforced"]
    fn mls_12_2_function_with_initial_algorithm() {
        expect_parse_failure(
            r#"
            function F
                input Real x;
                output Real y;
            initial algorithm
                y := 0;
            algorithm
                y := x;
            end F;
            "#,
        );
    }

    /// Valid: Function with algorithm section
    #[test]
    fn mls_12_2_function_with_algorithm_allowed() {
        expect_success(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            model Test
                Real y = Square(3);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.2 FUNCTION WHEN-STATEMENT RESTRICTION
// ============================================================================

/// MLS §12.2: Functions shall not have when-statements
mod function_when_restriction {
    use super::*;

    /// MLS: "When-statements are not allowed in functions"
    #[test]
    #[ignore = "Function when-statement restriction not yet enforced"]
    fn mls_12_2_function_with_when_statement() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x;
                when x > 0 then
                    y := 1;
                end when;
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "When-statements are not allowed in functions" (with elsewhen)
    #[test]
    #[ignore = "Function when-statement restriction not yet enforced"]
    fn mls_12_2_function_with_when_elsewhen() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                when x > 1 then
                    y := 1;
                elsewhen x > 0 then
                    y := 0;
                end when;
            end F;
            "#,
            "F",
        );
    }

    /// Valid: Function with if-statement (not when)
    #[test]
    fn mls_12_2_function_with_if_allowed() {
        expect_success(
            r#"
            function Abs
                input Real x;
                output Real y;
            algorithm
                if x >= 0 then
                    y := x;
                else
                    y := -x;
                end if;
            end Abs;

            model Test
                Real y = Abs(-5);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.2 FORBIDDEN OPERATORS IN FUNCTIONS
// ============================================================================

/// MLS §12.2: Forbidden built-in operators in functions
mod function_forbidden_operators {
    use super::*;

    /// MLS: "der is not allowed inside functions"
    #[test]
    #[ignore = "Function der() restriction not yet enforced"]
    fn mls_12_2_function_with_der() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := der(x);
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "initial is not allowed inside functions"
    #[test]
    #[ignore = "Function initial() restriction not yet enforced"]
    fn mls_12_2_function_with_initial() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                if initial() then
                    y := 0;
                else
                    y := x;
                end if;
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "terminal is not allowed inside functions"
    #[test]
    #[ignore = "Function terminal() restriction not yet enforced"]
    fn mls_12_2_function_with_terminal() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                if terminal() then
                    y := 0;
                else
                    y := x;
                end if;
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "pre is not allowed inside functions"
    #[test]
    #[ignore = "Function pre() restriction not yet enforced"]
    fn mls_12_2_function_with_pre() {
        expect_failure(
            r#"
            function F
                input Integer n;
                output Integer m;
            algorithm
                m := pre(n);
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "edge is not allowed inside functions"
    #[test]
    #[ignore = "Function edge() restriction not yet enforced"]
    fn mls_12_2_function_with_edge() {
        expect_failure(
            r#"
            function F
                input Boolean b;
                output Boolean result;
            algorithm
                result := edge(b);
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "change is not allowed inside functions"
    #[test]
    #[ignore = "Function change() restriction not yet enforced"]
    fn mls_12_2_function_with_change() {
        expect_failure(
            r#"
            function F
                input Integer n;
                output Boolean result;
            algorithm
                result := change(n);
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "reinit is not allowed inside functions"
    #[test]
    #[ignore = "Function reinit() restriction not yet enforced"]
    fn mls_12_2_function_with_reinit() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                Real temp;
            algorithm
                temp := x;
                reinit(temp, 0);
                y := temp;
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "sample is not allowed inside functions"
    #[test]
    #[ignore = "Function sample() restriction not yet enforced"]
    fn mls_12_2_function_with_sample() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Boolean result;
            algorithm
                result := sample(0, 0.1);
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "delay is not allowed inside functions"
    #[test]
    #[ignore = "Function delay() restriction not yet enforced"]
    fn mls_12_2_function_with_delay() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := delay(x, 0.1);
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "cardinality is not allowed inside functions"
    #[test]
    fn mls_12_2_function_with_cardinality() {
        expect_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            function F
                input C c;
                output Integer n;
            algorithm
                n := cardinality(c);
            end F;
            "#,
            "F",
        );
    }

    /// Valid: Mathematical functions allowed
    #[test]
    fn mls_12_2_math_functions_allowed() {
        expect_success(
            r#"
            function Compute
                input Real x;
                output Real y;
            algorithm
                y := sin(x) + cos(x) + sqrt(abs(x)) + exp(x);
            end Compute;

            model Test
                Real y = Compute(1.0);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.2 INPUT READ-ONLY RESTRICTION
// ============================================================================

/// MLS §12.2: Input formal parameters are read-only
mod function_input_readonly {
    use super::*;

    /// MLS: "Input formal parameters are read-only"
    #[test]
    fn mls_12_2_assign_to_input() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                x := x + 1;
                y := x;
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "Input formal parameters are read-only" (array element)
    #[test]
    fn mls_12_2_assign_to_input_array_element() {
        expect_failure(
            r#"
            function F
                input Real x[3];
                output Real y;
            algorithm
                x[1] := 0;
                y := x[1] + x[2] + x[3];
            end F;
            "#,
            "F",
        );
    }

    /// Valid: Read from input
    #[test]
    fn mls_12_2_read_from_input_allowed() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                Real temp;
            algorithm
                temp := x;
                y := temp * 2;
            end F;

            model Test
                Real y = F(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.2 PUBLIC COMPONENT RESTRICTIONS
// ============================================================================

/// MLS §12.2: Public components must be input or output
mod function_public_component_restriction {
    use super::*;

    /// MLS: "Each public component shall have prefix input or output"
    #[test]
    #[ignore = "Function public component restriction not yet enforced"]
    fn mls_12_2_public_without_causality() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
                Real temp;
            algorithm
                temp := x * 2;
                y := temp;
            end F;
            "#,
            "F",
        );
    }

    /// Valid: Protected local variables allowed
    #[test]
    fn mls_12_2_protected_local_allowed() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                Real temp;
            algorithm
                temp := x * 2;
                y := temp;
            end F;

            model Test
                Real y = F(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Multiple inputs and outputs
    #[test]
    fn mls_12_2_multiple_io_allowed() {
        expect_success(
            r#"
            function F
                input Real x;
                input Real y;
                output Real z;
                output Real w;
            algorithm
                z := x + y;
                w := x * y;
            end F;

            model Test
                Real a;
                Real b;
            equation
                (a, b) = F(1, 2);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.2 COMPONENT CLASS RESTRICTIONS
// ============================================================================

/// MLS §12.2: Functions cannot contain model, block, operator, or connector
mod function_component_class_restriction {
    use super::*;

    /// MLS: "Functions shall not contain components of class model"
    #[test]
    #[ignore = "Function model component restriction not yet enforced"]
    fn mls_12_2_function_with_model_component() {
        expect_failure(
            r#"
            model M
                Real x;
            equation
                x = 1;
            end M;

            function F
                input Real x;
                output Real y;
                M m;
            algorithm
                y := x;
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "Functions shall not contain components of class block"
    #[test]
    #[ignore = "Function block component restriction not yet enforced"]
    fn mls_12_2_function_with_block_component() {
        expect_failure(
            r#"
            block B
                input Real u;
                output Real y;
            equation
                y = u;
            end B;

            function F
                input Real x;
                output Real y;
                B b;
            algorithm
                y := x;
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "Functions shall not contain components of class connector"
    #[test]
    #[ignore = "Function connector component restriction not yet enforced"]
    fn mls_12_2_function_with_connector_component() {
        expect_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            function F
                input Real x;
                output Real y;
                C c;
            algorithm
                y := x;
            end F;
            "#,
            "F",
        );
    }

    /// Valid: Record component in function
    #[test]
    fn mls_12_2_function_with_record_allowed() {
        expect_success(
            r#"
            record R
                Real x;
                Real y;
            end R;

            function F
                input Real a;
                output Real b;
            protected
                R r;
            algorithm
                r.x := a;
                r.y := a * 2;
                b := r.x + r.y;
            end F;

            model Test
                Real y = F(3);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.2 SINGLE ALGORITHM OR EXTERNAL RESTRICTION
// ============================================================================

/// MLS §12.2: Functions have at most one algorithm or external
mod function_single_algorithm_restriction {
    use super::*;

    /// MLS: "A function shall have at most one algorithm section"
    #[test]
    #[ignore = "Function multiple algorithm detection not yet implemented"]
    fn mls_12_2_multiple_algorithm_sections() {
        expect_parse_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x;
            algorithm
                y := y * 2;
            end F;
            "#,
        );
    }

    /// Valid: Single algorithm section
    #[test]
    fn mls_12_2_single_algorithm_allowed() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;

            model Test
                Real y = F(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: External function
    #[test]
    fn mls_12_2_external_function_allowed() {
        expect_success(
            r#"
            function Sin
                input Real x;
                output Real y;
            external "C" y = sin(x);
            end Sin;

            model Test
                Real y = Sin(1.0);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.3 IMPURE FUNCTION RESTRICTIONS
// ============================================================================

/// MLS §12.3: Impure function call restrictions
mod impure_function_restriction {
    use super::*;

    /// Valid: Impure function called in when-equation
    #[test]
    fn mls_12_3_impure_in_when_allowed() {
        expect_success(
            r#"
            impure function Log
                input String msg;
            algorithm
                // Would log message to external system
            end Log;

            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    Log("x exceeded 1");
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Pure function called anywhere
    #[test]
    fn mls_12_3_pure_anywhere_allowed() {
        expect_success(
            r#"
            pure function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            model Test
                Real x = 3;
                Real y = Square(x);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.4 OUTPUT INITIALIZATION REQUIREMENT
// ============================================================================

/// MLS §12.4: Output variables must be assigned
mod output_initialization_requirement {
    use super::*;

    /// MLS: "Output variables must be assigned inside the function"
    #[test]
    #[ignore = "Function output initialization check not yet implemented"]
    fn mls_12_4_output_not_assigned() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
                output Real z;
            algorithm
                y := x * 2;
                // z is never assigned!
            end F;
            "#,
            "F",
        );
    }

    /// Valid: All outputs assigned
    #[test]
    fn mls_12_4_all_outputs_assigned() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
                output Real z;
            algorithm
                y := x * 2;
                z := x * 3;
            end F;

            model Test
                Real a;
                Real b;
            equation
                (a, b) = F(5);
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Output with default value
    #[test]
    fn mls_12_4_output_with_default() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y = 0;
            algorithm
                if x > 0 then
                    y := x;
                end if;
            end F;

            model Test
                Real y = F(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
