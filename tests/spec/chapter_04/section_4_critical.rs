//! MLS §4: Critical Declaration and Class Restriction Tests
//!
//! This module tests critical normative requirements for declarations:
//! - §4.2: Double declaration restrictions
//! - §4.4: Constant binding requirement
//! - §4.5: Variability dependency rules
//! - §4.6: Specialized class restrictions (record, package, connector, function)
//!
//! These are semantic checks that prevent fundamental declaration errors.
//!
//! Reference: https://specification.modelica.org/master/class-predefined-types-declarations.html

use crate::spec::{expect_failure, expect_parse_failure, expect_parse_success, expect_success};

// ============================================================================
// §4.2 DOUBLE DECLARATION RESTRICTIONS
// ============================================================================

/// MLS §4.2: Each component name must be unique within a class
mod double_declaration_restriction {
    use super::*;

    /// MLS: "Each component name shall be distinct from other component names"
    #[test]
    fn mls_4_2_duplicate_real_variable() {
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

    /// MLS: "Each component name shall be distinct"
    #[test]
    fn mls_4_2_duplicate_integer_variable() {
        expect_failure(
            r#"
            model Test
                Integer n;
                Integer n;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Component and class names share the same namespace"
    #[test]
    fn mls_4_2_component_class_same_name() {
        expect_failure(
            r#"
            model Test
                Real Inner;
                model Inner
                    Real x;
                end Inner;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Duplicate parameter names forbidden
    #[test]
    fn mls_4_2_duplicate_parameter() {
        expect_failure(
            r#"
            model Test
                parameter Real k = 1;
                parameter Real k = 2;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Duplicate constant names forbidden
    #[test]
    fn mls_4_2_duplicate_constant() {
        expect_failure(
            r#"
            model Test
                constant Real pi = 3.14159;
                constant Real pi = 3.14;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Duplicate in different visibility sections still forbidden
    #[test]
    fn mls_4_2_duplicate_across_visibility() {
        expect_failure(
            r#"
            model Test
            public
                Real x;
            protected
                Real x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Same name in different scopes (allowed)
    #[test]
    fn mls_4_2_same_name_different_scope_allowed() {
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

    /// Valid: Unique names (allowed)
    #[test]
    fn mls_4_2_unique_names_allowed() {
        expect_success(
            r#"
            model Test
                Real x;
                Real y;
                Real z;
            equation
                x = 1;
                y = 2;
                z = 3;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §4.4 CONSTANT BINDING REQUIREMENT
// ============================================================================

/// MLS §4.4: Constants must have binding equations
mod constant_binding_requirement {
    use super::*;

    /// MLS: "A constant shall have a declaration equation"
    #[test]
    fn mls_4_4_constant_without_binding() {
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

    /// MLS: "A constant shall have a declaration equation"
    #[test]
    fn mls_4_4_constant_integer_without_binding() {
        expect_failure(
            r#"
            model Test
                constant Integer n;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "A constant shall have a declaration equation"
    #[test]
    fn mls_4_4_constant_string_without_binding() {
        expect_failure(
            r#"
            model Test
                constant String s;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "A constant shall have a declaration equation"
    #[test]
    fn mls_4_4_constant_boolean_without_binding() {
        expect_failure(
            r#"
            model Test
                constant Boolean b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "A constant shall have a declaration equation" - array case
    #[test]
    fn mls_4_4_constant_array_without_binding() {
        expect_failure(
            r#"
            model Test
                constant Real v[3];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Constant with binding
    #[test]
    fn mls_4_4_constant_with_binding_allowed() {
        expect_success(
            r#"
            model Test
                constant Real c = 3.14159;
                constant Integer n = 42;
                constant String s = "hello";
                constant Boolean b = true;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Constant array with binding
    #[test]
    fn mls_4_4_constant_array_with_binding_allowed() {
        expect_success(
            r#"
            model Test
                constant Real v[3] = {1, 2, 3};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Constant with expression binding
    #[test]
    fn mls_4_4_constant_expression_binding_allowed() {
        expect_success(
            r#"
            model Test
                constant Real pi = 3.14159;
                constant Real two_pi = 2 * pi;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §4.5 VARIABILITY DEPENDENCY RULES
// ============================================================================

/// MLS §4.5: Variability constraints on binding equations
mod variability_dependency {
    use super::*;

    /// MLS: "A parameter binding cannot depend on a variable"
    #[test]
    fn mls_4_5_parameter_depends_on_variable() {
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

    /// MLS: "A constant binding cannot depend on a parameter"
    #[test]
    fn mls_4_5_constant_depends_on_parameter() {
        expect_failure(
            r#"
            model Test
                parameter Real k = 1;
                constant Real c = k;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "A constant binding cannot depend on a variable"
    #[test]
    fn mls_4_5_constant_depends_on_variable() {
        expect_failure(
            r#"
            model Test
                Real x(start = 1);
                constant Real c = x;
            equation
                der(x) = -x;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Constant in parameter expression
    #[test]
    fn mls_4_5_constant_in_parameter_allowed() {
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

    /// Valid: Parameter in variable expression
    #[test]
    fn mls_4_5_parameter_in_variable_allowed() {
        expect_success(
            r#"
            model Test
                parameter Real k = 2;
                Real x(start = 0);
            equation
                der(x) = k;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §4.6 SPECIALIZED CLASS RESTRICTIONS
// ============================================================================

/// MLS §4.6: Record restrictions
mod record_restrictions {
    use super::*;

    /// MLS: "A record shall not have equation sections"
    #[test]
    fn mls_4_6_record_with_equation() {
        expect_failure(
            r#"
            record Point
                Real x;
                Real y;
            equation
                x = 0;
            end Point;
            "#,
            "Point",
        );
    }

    /// MLS: "A record shall not have algorithm sections"
    #[test]
    fn mls_4_6_record_with_algorithm() {
        expect_failure(
            r#"
            record Point
                Real x;
                Real y;
            algorithm
                x := 0;
            end Point;
            "#,
            "Point",
        );
    }

    /// MLS: "A record shall not have initial equation sections"
    #[test]
    fn mls_4_6_record_with_initial_equation() {
        expect_failure(
            r#"
            record Point
                Real x;
                Real y;
            initial equation
                x = 0;
            end Point;
            "#,
            "Point",
        );
    }

    /// Valid: Simple record
    #[test]
    fn mls_4_6_valid_record() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;
            "#,
        );
    }

    /// Valid: Record with default values
    #[test]
    fn mls_4_6_record_with_defaults() {
        expect_parse_success(
            r#"
            record Point
                Real x = 0;
                Real y = 0;
            end Point;
            "#,
        );
    }
}

/// MLS §4.6: Package restrictions
mod package_restrictions {
    use super::*;

    /// MLS: "A package shall not have equation sections"
    #[test]
    fn mls_4_6_package_with_equation() {
        expect_parse_failure(
            r#"
            package P
                constant Real c = 1;
            equation
                c = 2;
            end P;
            "#,
        );
    }

    /// MLS: "A package shall not have algorithm sections"
    #[test]
    fn mls_4_6_package_with_algorithm() {
        expect_parse_failure(
            r#"
            package P
                constant Real c = 1;
            algorithm
                c := 2;
            end P;
            "#,
        );
    }

    /// MLS: "A package shall contain only class definitions and constants"
    #[test]
    fn mls_4_6_package_with_variable() {
        expect_failure(
            r#"
            package P
                Real x;
            end P;
            "#,
            "P",
        );
    }

    /// MLS: "A package shall contain only class definitions and constants"
    #[test]
    fn mls_4_6_package_with_parameter() {
        expect_failure(
            r#"
            package P
                parameter Real k = 1;
            end P;
            "#,
            "P",
        );
    }

    /// Valid: Package with constants
    #[test]
    fn mls_4_6_package_with_constants() {
        expect_parse_success(
            r#"
            package Constants
                constant Real pi = 3.14159;
                constant Real e = 2.71828;
            end Constants;
            "#,
        );
    }

    /// Valid: Package with nested classes
    #[test]
    fn mls_4_6_package_with_classes() {
        expect_parse_success(
            r#"
            package MyLib
                model M
                    Real x;
                end M;

                function F
                    input Real x;
                    output Real y;
                algorithm
                    y := x * 2;
                end F;
            end MyLib;
            "#,
        );
    }
}

/// MLS §4.6: Connector restrictions
mod connector_restrictions {
    use super::*;

    /// MLS: "All elements of a connector shall be public"
    #[test]
    fn mls_4_6_connector_with_protected() {
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

    /// MLS: "A connector shall not contain equation sections"
    #[test]
    fn mls_4_6_connector_with_equation() {
        expect_parse_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            equation
                v = 0;
            end C;
            "#,
        );
    }

    /// MLS: "A connector shall not contain algorithm sections"
    #[test]
    fn mls_4_6_connector_with_algorithm() {
        expect_parse_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            algorithm
                v := 0;
            end C;
            "#,
        );
    }

    /// Valid: Simple connector
    #[test]
    fn mls_4_6_valid_connector() {
        expect_parse_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;
            "#,
        );
    }
}

/// MLS §4.6: Function restrictions
mod function_restrictions {
    use super::*;

    /// MLS: "A function shall not have equation sections"
    #[test]
    fn mls_4_6_function_with_equation() {
        expect_parse_failure(
            r#"
            function F
                input Real x;
                output Real y;
            equation
                y = x;
            end F;
            "#,
        );
    }

    /// MLS: "A function shall not have when-clauses in algorithms"
    #[test]
    fn mls_4_6_function_with_when_in_algorithm() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                when x > 0 then
                    y := x;
                end when;
            end F;
            "#,
            "F",
        );
    }

    /// Valid: Simple function
    #[test]
    fn mls_4_6_valid_function() {
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
// §4.5 PARTIAL CLASS RESTRICTIONS
// ============================================================================

/// MLS §4.5: Partial class instantiation restrictions
mod partial_class_restrictions {
    use super::*;

    /// MLS: "A partial class cannot be instantiated"
    #[test]
    fn mls_4_5_instantiate_partial_model() {
        expect_failure(
            r#"
            partial model Base
                Real x;
            equation
            end Base;

            model Test
                Base b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "A partial class cannot be instantiated"
    #[test]
    fn mls_4_5_instantiate_partial_block() {
        expect_failure(
            r#"
            partial block B
                input Real u;
                output Real y;
            equation
            end B;

            model Test
                B myBlock;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Extend partial class
    #[test]
    fn mls_4_5_extend_partial_allowed() {
        expect_success(
            r#"
            partial model Base
                Real x;
            equation
            end Base;

            model Derived
                extends Base;
            equation
                x = 1;
            end Derived;
            "#,
            "Derived",
        );
    }
}

// ============================================================================
// §4.8 PREDEFINED TYPE RESTRICTIONS
// ============================================================================

/// MLS §4.8: Predefined types cannot be redeclared
mod predefined_type_restrictions {
    use super::*;

    /// MLS: Cannot redeclare Real
    #[test]
    fn mls_4_8_cannot_redeclare_real() {
        expect_parse_failure(
            r#"
            type Real = Integer;
            "#,
        );
    }

    /// MLS: Cannot redeclare Integer
    #[test]
    fn mls_4_8_cannot_redeclare_integer() {
        expect_parse_failure(
            r#"
            type Integer = Real;
            "#,
        );
    }

    /// MLS: Cannot redeclare Boolean
    #[test]
    fn mls_4_8_cannot_redeclare_boolean() {
        expect_parse_failure(
            r#"
            type Boolean = Integer;
            "#,
        );
    }

    /// MLS: Cannot redeclare String
    #[test]
    fn mls_4_8_cannot_redeclare_string() {
        expect_parse_failure(
            r#"
            type String = Real;
            "#,
        );
    }

    /// MLS: Can create type alias with same name in different scope (package)
    #[test]
    fn mls_4_8_type_alias_in_package_valid() {
        expect_parse_success(
            r#"
            package MyTypes
                type Voltage = Real(unit = "V");
                type Current = Real(unit = "A");
            end MyTypes;
            "#,
        );
    }
}
