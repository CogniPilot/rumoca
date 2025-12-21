//! MLS §6.1: Interface Concepts
//!
//! Tests for interface extraction and interface concepts including:
//! - Interface definition
//! - Public vs protected elements
//! - Constants in interfaces
//! - Functions in interfaces
//! - Interface of composite types
//!
//! Reference: https://specification.modelica.org/master/interface-or-type.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §6.1.1 INTERFACE DEFINITION
// ============================================================================

/// MLS §6.1: Interface definition and extraction
mod interface_definition {
    use super::*;

    /// MLS: Interface includes all public components
    #[test]
    fn mls_6_1_public_components() {
        expect_success(
            r#"
            model PublicInterface
                Real x;
                Real y;
                Real z;
            equation
                x = 1;
                y = 2;
                z = 3;
            end PublicInterface;
            "#,
            "PublicInterface",
        );
    }

    /// MLS: Interface includes parameters
    #[test]
    fn mls_6_1_parameters_in_interface() {
        expect_success(
            r#"
            model ParamInterface
                parameter Real gain = 1.0;
                parameter Integer count = 10;
                Real x;
            equation
                x = gain * count;
            end ParamInterface;
            "#,
            "ParamInterface",
        );
    }

    /// MLS: Interface includes constants
    #[test]
    fn mls_6_1_constants_in_interface() {
        expect_success(
            r#"
            model ConstInterface
                constant Real pi = 3.14159;
                constant Real e = 2.71828;
                Real x;
            equation
                x = pi + e;
            end ConstInterface;
            "#,
            "ConstInterface",
        );
    }

    /// MLS: Interface includes connectors
    #[test]
    fn mls_6_1_connectors_in_interface() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model WithConnector
                Pin p;
                Pin n;
            equation
                p.v = n.v;
                p.i + n.i = 0;
            end WithConnector;
            "#,
            "WithConnector",
        );
    }

    /// MLS: Interface includes nested class access
    #[test]
    fn mls_6_1_nested_class_interface() {
        expect_parse_success(
            r#"
            package Container
                type Voltage = Real(unit = "V");
                type Current = Real(unit = "A");

                connector Pin
                    Voltage v;
                    flow Current i;
                end Pin;
            end Container;

            model UsesNested
                Container.Pin p;
            equation
                p.v = 1;
            end UsesNested;
            "#,
        );
    }
}

// ============================================================================
// §6.1.2 PUBLIC VS PROTECTED
// ============================================================================

/// MLS §6.1: Public and protected elements
mod public_protected {
    use super::*;

    /// MLS: Protected elements excluded from interface
    #[test]
    fn mls_6_1_protected_excluded() {
        expect_parse_success(
            r#"
            model WithProtected
                Real publicVar = 1;
            protected
                Real privateVar = 2;
            equation
            end WithProtected;
            "#,
        );
    }

    /// MLS: Mixed public and protected sections
    #[test]
    fn mls_6_1_mixed_visibility() {
        expect_parse_success(
            r#"
            model MixedVisibility
                Real a = 1;
            protected
                Real b = 2;
            public
                Real c = 3;
            protected
                Real d = 4;
            equation
            end MixedVisibility;
            "#,
        );
    }

    /// MLS: Protected parameters
    #[test]
    fn mls_6_1_protected_parameters() {
        expect_parse_success(
            r#"
            model ProtectedParams
                parameter Real publicParam = 1;
            protected
                parameter Real privateParam = 2;
            public
                Real x;
            equation
                x = publicParam + privateParam;
            end ProtectedParams;
            "#,
        );
    }

    /// MLS: Protected components with public connectors
    #[test]
    fn mls_6_1_protected_with_connectors() {
        expect_parse_success(
            r#"
            connector C
                Real x;
                flow Real f;
            end C;

            model ProtectedInternal
                C port_a;
                C port_b;
            protected
                Real internal_state;
            equation
                port_a.x = port_b.x;
                port_a.f + port_b.f = 0;
                internal_state = port_a.x;
            end ProtectedInternal;
            "#,
        );
    }
}

// ============================================================================
// §6.1.3 INTERFACE OF COMPOSITE TYPES
// ============================================================================

/// MLS §6.1: Interface of composite and hierarchical types
mod composite_interface {
    use super::*;

    /// MLS: Interface of model with subcomponents
    #[test]
    fn mls_6_1_subcomponent_interface() {
        expect_success(
            r#"
            model Inner
                Real x = 1;
            equation
            end Inner;

            model Outer
                Inner a;
                Inner b;
                Real sum;
            equation
                sum = a.x + b.x;
            end Outer;
            "#,
            "Outer",
        );
    }

    /// MLS: Interface propagates through hierarchy
    #[test]
    fn mls_6_1_hierarchical_interface() {
        expect_success(
            r#"
            model Level1
                Real x = 1;
            equation
            end Level1;

            model Level2
                Level1 sub;
            equation
            end Level2;

            model Level3
                Level2 sub;
                Real y;
            equation
                y = sub.sub.x;
            end Level3;
            "#,
            "Level3",
        );
    }

    /// MLS: Array component interface
    #[test]
    fn mls_6_1_array_component_interface() {
        expect_success(
            r#"
            model Element
                Real value = 0;
            equation
            end Element;

            model ArrayOfElements
                Element elements[5];
                Real total;
            equation
                total = elements[1].value + elements[2].value +
                        elements[3].value + elements[4].value + elements[5].value;
            end ArrayOfElements;
            "#,
            "ArrayOfElements",
        );
    }

    /// MLS: Record component interface
    #[test]
    fn mls_6_1_record_component_interface() {
        expect_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model WithRecord
                Point p;
                Real distance;
            equation
                p.x = 3;
                p.y = 4;
                distance = sqrt(p.x^2 + p.y^2);
            end WithRecord;
            "#,
            "WithRecord",
        );
    }
}

// ============================================================================
// §6.1.4 INTERFACE WITH EXTENDS
// ============================================================================

/// MLS §6.1: Interface modification through extends
mod interface_extends {
    use super::*;

    /// MLS: Extended class includes base interface
    #[test]
    fn mls_6_1_extended_interface() {
        expect_success(
            r#"
            model Base
                Real a;
            equation
            end Base;

            model Derived
                extends Base;
                Real b;
            equation
                a = 1;
                b = 2;
            end Derived;
            "#,
            "Derived",
        );
    }

    /// MLS: Multiple extends merge interfaces
    #[test]
    fn mls_6_1_multiple_extends_interface() {
        expect_success(
            r#"
            model A
                Real a;
            equation
            end A;

            model B
                Real b;
            equation
            end B;

            model C
                extends A;
                extends B;
                Real c;
            equation
                a = 1;
                b = 2;
                c = a + b;
            end C;
            "#,
            "C",
        );
    }

    /// MLS: Extends with modification in interface
    #[test]
    fn mls_6_1_extends_modification() {
        expect_success(
            r#"
            model Base
                parameter Real p = 1;
                Real x;
            equation
                x = p;
            end Base;

            model Modified
                extends Base(p = 10);
            equation
            end Modified;
            "#,
            "Modified",
        );
    }
}

// ============================================================================
// §6.1.5 INTERFACE RESTRICTIONS
// ============================================================================

/// MLS §6.1: Restrictions on class interfaces
mod interface_restrictions {
    use super::*;

    /// MLS: Block interface - fixed causality
    #[test]
    fn mls_6_1_block_interface() {
        expect_success(
            r#"
            block Controller
                input Real u;
                output Real y;
                parameter Real K = 1;
            equation
                y = K * u;
            end Controller;
            "#,
            "Controller",
        );
    }

    /// MLS: Connector interface restrictions
    #[test]
    fn mls_6_1_connector_interface() {
        expect_parse_success(
            r#"
            connector ElectricalPin
                Real v;
                flow Real i;
            end ElectricalPin;
            "#,
        );
    }

    /// MLS: Record interface - data only
    #[test]
    fn mls_6_1_record_interface() {
        expect_parse_success(
            r#"
            record DataRecord
                Real value;
                Integer count;
                Boolean flag;
                String name;
            end DataRecord;
            "#,
        );
    }

    /// MLS: Package interface - encapsulation
    #[test]
    fn mls_6_1_package_interface() {
        expect_parse_success(
            r#"
            package MyLib
                constant Real version = 1.0;

                function helper
                    input Real x;
                    output Real y;
                algorithm
                    y := x * 2;
                end helper;

                model Component
                    Real x = 1;
                equation
                end Component;
            end MyLib;
            "#,
        );
    }

    /// MLS: Function interface - input/output
    #[test]
    fn mls_6_1_function_interface() {
        expect_parse_success(
            r#"
            function Transform
                input Real x;
                input Real y;
                output Real result;
            protected
                Real temp;
            algorithm
                temp := x + y;
                result := temp * 2;
            end Transform;
            "#,
        );
    }
}

// ============================================================================
// NEGATIVE TESTS - Interface violations
// ============================================================================

/// Interface violation error cases
mod interface_errors {
    use super::*;

    /// Access to undefined component
    #[test]
    fn error_undefined_component_access() {
        expect_failure(
            r#"
            model Inner
                Real x = 1;
            equation
            end Inner;

            model Outer
                Inner sub;
                Real y;
            equation
                y = sub.undefined;
            end Outer;
            "#,
            "Outer",
        );
    }

    /// Invalid hierarchical access
    #[test]
    #[ignore = "Invalid hierarchical access validation not yet implemented"]
    fn error_invalid_hierarchical_access() {
        expect_failure(
            r#"
            model Base
                Real x = 1;
            equation
            end Base;

            model Test
                Base b;
                Real y;
            equation
                y = b.x.invalid;
            end Test;
            "#,
            "Test",
        );
    }
}
