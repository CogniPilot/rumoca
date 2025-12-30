//! MLS §4: Classes, Predefined Types, and Declarations - Detailed Conformance Tests
//!
//! Comprehensive tests covering normative statements from MLS §4 including:
//! - §4.1: Access control (public/protected)
//! - §4.2: Double declaration restrictions
//! - §4.5: Variability semantics
//! - §4.7: Specialized class restrictions
//! - §4.8: Balanced model requirements
//!
//! Reference: https://specification.modelica.org/master/class-predefined-types-declarations.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §4.1 ACCESS CONTROL
// ============================================================================

/// MLS §4.1: Access control (public/protected)
mod access_control {
    use super::*;

    /// Public declarations are accessible
    #[test]
    fn mls_4_1_public_declaration() {
        expect_success(
            r#"
            model Test
            public
                Real x = 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Protected declarations
    #[test]
    fn mls_4_1_protected_declaration() {
        expect_success(
            r#"
            model Test
            protected
                Real internalVar = 1;
            public
                Real externalVar = internalVar;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Mixed public/protected sections
    #[test]
    fn mls_4_1_mixed_sections() {
        expect_success(
            r#"
            model Test
            public
                Real a = 1;
            protected
                Real b = 2;
            public
                Real c = 3;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Default visibility is public
    #[test]
    fn mls_4_1_default_public() {
        expect_success(
            r#"
            model Test
                Real x = 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Protected in function
    #[test]
    fn mls_4_1_protected_in_function() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                Real temp;
            algorithm
                temp := x * 2;
                y := temp + 1;
            end F;

            model Test
                Real y = F(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Protected elements are not accessible from outside"
    #[test]
    #[ignore = "Protected access restriction not yet enforced"]
    fn error_access_protected_element() {
        expect_failure(
            r#"
            model Inner
            protected
                Real secret = 42;
            end Inner;

            model Outer
                Inner i;
                Real x = i.secret;
            equation
            end Outer;
            "#,
            "Outer",
        );
    }
}

// ============================================================================
// §4.2 DOUBLE DECLARATION RESTRICTIONS
// ============================================================================

/// MLS §4.2: Double declaration restrictions
mod double_declaration {
    use super::*;

    /// MLS: "Each component name must be unique within a class"
    #[test]
    fn error_duplicate_component() {
        expect_failure(
            r#"
            model Test
                Real x;
                Real x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Component and class names share the same namespace"
    #[test]
    fn error_component_class_collision() {
        expect_failure(
            r#"
            model Test
                Real Inner;
                model Inner
                end Inner;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: same name in different scopes
    #[test]
    fn mls_4_2_different_scopes_ok() {
        expect_success(
            r#"
            model Outer
                Real x = 1;
                model Inner
                    Real x = 2;
                equation
                end Inner;
                Inner i;
            equation
            end Outer;
            "#,
            "Outer",
        );
    }
}

// ============================================================================
// §4.5 VARIABILITY SEMANTICS
// ============================================================================

/// MLS §4.5: Variability semantic requirements
mod variability_semantics {
    use super::*;

    /// MLS: "A constant must have a binding equation"
    #[test]
    fn error_constant_without_binding() {
        expect_failure(
            r#"
            model Test
                constant Real c;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid constant with binding
    #[test]
    fn mls_4_5_constant_with_binding() {
        expect_success(
            r#"
            model Test
                constant Real c = 3.14159;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "A parameter has fixed=true by default"
    #[test]
    fn mls_4_5_parameter_fixed_default() {
        expect_success(
            r#"
            model Test
                parameter Real k = 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Parameter with fixed=false
    #[test]
    fn mls_4_5_parameter_fixed_false() {
        expect_success(
            r#"
            model Test
                parameter Real k(fixed = false);
                Real x;
            initial equation
                k = 1;
            equation
                der(x) = k;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Discrete variables only change at events"
    #[test]
    fn mls_4_5_discrete_at_events() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer count(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    count = pre(count) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Continuous variables cannot depend on discrete in continuous equations"
    #[test]
    #[ignore = "Continuous/discrete dependency check not yet implemented"]
    fn error_continuous_depends_on_discrete() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
                Real y;
            equation
                der(x) = 1;
                when x > 1 then
                    n = pre(n) + 1;
                end when;
                y = n;
            end Test;
            "#,
            "Test",
        );
    }

    /// Variability propagation: constant in parameter expression
    #[test]
    fn mls_4_5_constant_in_parameter() {
        expect_success(
            r#"
            model Test
                constant Real pi = 3.14159;
                parameter Real two_pi = 2 * pi;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Parameter cannot depend on variable"
    #[test]
    fn error_parameter_depends_on_variable() {
        expect_failure(
            r#"
            model Test
                Real x(start = 1);
                parameter Real k = x;
            equation
                der(x) = -x;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §4.7 SPECIALIZED CLASS RESTRICTIONS
// ============================================================================

/// MLS §4.7: Specialized class restrictions
mod specialized_class_restrictions {
    use super::*;

    /// Record cannot have equations
    #[test]
    fn error_record_with_equations() {
        expect_failure(
            r#"
            record Point
                Real x;
                Real y;
            equation
                x = 1;
            end Point;
            "#,
            "Point",
        );
    }

    /// Record cannot have algorithm sections
    #[test]
    fn error_record_with_algorithm() {
        expect_failure(
            r#"
            record Point
                Real x;
                Real y;
            algorithm
                x := 1;
            end Point;
            "#,
            "Point",
        );
    }

    /// Valid record
    #[test]
    fn mls_4_7_valid_record() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;
            "#,
        );
    }

    /// Type must be a type alias
    #[test]
    fn mls_4_7_type_alias() {
        expect_success(
            r#"
            type Voltage = Real(unit = "V");

            model Test
                Voltage v = 5;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Package cannot have equations
    #[test]
    fn error_package_with_equations() {
        expect_failure(
            r#"
            package P
                constant Real c = 1;
            equation
                c = 2;
            end P;
            "#,
            "P",
        );
    }

    /// Valid package
    #[test]
    fn mls_4_7_valid_package() {
        expect_parse_success(
            r#"
            package Constants
                constant Real pi = 3.14159;
                constant Real e = 2.71828;
            end Constants;
            "#,
        );
    }

    /// Block requires input/output
    #[test]
    fn mls_4_7_block_with_io() {
        expect_parse_success(
            r#"
            block Gain
                input Real u;
                output Real y;
                parameter Real k = 1;
            equation
                y = k * u;
            end Gain;
            "#,
        );
    }

    /// Connector shall have only public elements
    #[test]
    fn error_connector_with_protected() {
        expect_failure(
            r#"
            connector C
                Real v;
            protected
                Real secret;
            end C;
            "#,
            "C",
        );
    }
}

// ============================================================================
// §4.8 BALANCED MODEL REQUIREMENTS
// ============================================================================

/// MLS §4.8: Balanced model requirements
mod balanced_models {
    use super::*;

    /// Basic balanced model
    #[test]
    fn mls_4_8_basic_balanced() {
        expect_success(
            r#"
            model Test
                Real x;
            equation
                x = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// Balanced ODE system
    #[test]
    fn mls_4_8_ode_balanced() {
        expect_success(
            r#"
            model Test
                Real x(start = 1);
                Real y(start = 0);
            equation
                der(x) = -x;
                der(y) = x;
            end Test;
            "#,
            "Test",
        );
    }

    /// Balanced with for-loop
    #[test]
    fn mls_4_8_for_balanced() {
        expect_success(
            r#"
            model Test
                Real x[5];
            equation
                for i in 1:5 loop
                    x[i] = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// Balanced with connection
    #[test]
    fn mls_4_8_connection_balanced() {
        expect_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Component
                C a;
                C b;
            equation
                a.v = 1;
                a.i + b.i = 0;
                b.v = a.v;
            end Component;

            model Test
                Component c1;
                Component c2;
            equation
                connect(c1.b, c2.a);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §4.9 PREDEFINED TYPES
// ============================================================================

/// MLS §4.9: Predefined types and classes
mod predefined_types {
    use super::*;

    /// Real type with all attributes
    #[test]
    fn mls_4_9_real_attributes() {
        expect_success(
            r#"
            model Test
                Real x(
                    start = 0,
                    fixed = true,
                    min = -1,
                    max = 1,
                    nominal = 1,
                    unit = "m"
                );
            equation
                x = 0.5;
            end Test;
            "#,
            "Test",
        );
    }

    /// Integer type
    #[test]
    fn mls_4_9_integer_type() {
        expect_success(
            r#"
            model Test
                Integer n(min = 0, max = 100) = 50;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Boolean type
    #[test]
    fn mls_4_9_boolean_type() {
        expect_success(
            r#"
            model Test
                Boolean flag(start = false) = true;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// String type
    #[test]
    fn mls_4_9_string_type() {
        expect_success(
            r#"
            model Test
                String name = "test";
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// StateSelect enumeration
    #[test]
    fn mls_4_9_stateselect() {
        expect_parse_success(
            r#"
            model Test
                Real x(stateSelect = StateSelect.prefer);
            equation
                der(x) = 1;
            end Test;
            "#,
        );
    }

    /// AssertionLevel enumeration
    #[test]
    fn mls_4_9_assertionlevel() {
        expect_success(
            r#"
            model Test
                parameter Real x = 1;
            equation
                assert(x > 0, "x must be positive", AssertionLevel.warning);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// COMPLEX CLASS SCENARIOS
// ============================================================================

/// Complex class scenarios
mod complex_scenarios {
    use super::*;

    /// Nested classes
    #[test]
    fn complex_nested_classes() {
        expect_parse_success(
            r#"
            package Outer
                package Inner
                    model M
                        Real x = 1;
                    end M;
                end Inner;
            end Outer;
            "#,
        );
    }

    /// Class with multiple sections
    #[test]
    fn complex_multiple_sections() {
        expect_success(
            r#"
            model Test
            public
                parameter Real k = 1;
            protected
                Real internalVar(start = 0);
            public
                Real externalVar;
            equation
                der(internalVar) = k;
                externalVar = internalVar;
            end Test;
            "#,
            "Test",
        );
    }

    /// Model extending connector
    #[test]
    fn complex_model_with_connector() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model TwoPin
                Pin p;
                Pin n;
                Real v;
                Real i;
            equation
                v = p.v - n.v;
                i = p.i;
                0 = p.i + n.i;
            end TwoPin;

            model Resistor
                extends TwoPin;
                parameter Real R = 1;
            equation
                v = R * i;
            end Resistor;
            "#,
            "Resistor",
        );
    }
}
