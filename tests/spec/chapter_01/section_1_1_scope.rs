//! MLS §1.1: Scope of the Specification
//!
//! This module tests fundamental concepts defined in the MLS scope:
//! - Model hierarchy (model, block, connector, record, package, function)
//! - Equation-based modeling
//! - Component encapsulation
//!
//! Reference: https://specification.modelica.org/master/introduction.html#scope-of-the-specification

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §1.1.1 CLASS HIERARCHY
// ============================================================================

/// MLS §1.1: Specialized classes form a hierarchy
mod class_hierarchy {
    use super::*;

    /// MLS: "model" is the basic class for physical modeling
    #[test]
    fn mls_1_1_model_basic() {
        expect_success(
            r#"
            model SimpleModel
                Real x;
            equation
                x = 1;
            end SimpleModel;
            "#,
            "SimpleModel",
        );
    }

    /// MLS: "block" is a model with fixed causality (input/output)
    #[test]
    fn mls_1_1_block_causality() {
        expect_success(
            r#"
            block Gain
                input Real u;
                output Real y;
                parameter Real k = 1;
            equation
                y = k * u;
            end Gain;
            "#,
            "Gain",
        );
    }

    /// MLS: "connector" defines interface points for connections
    #[test]
    fn mls_1_1_connector_interface() {
        expect_parse_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;
            "#,
        );
    }

    /// MLS: "record" is a class with only public data
    #[test]
    fn mls_1_1_record_data_only() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;
            "#,
        );
    }

    /// MLS: "package" is a container for classes
    #[test]
    fn mls_1_1_package_container() {
        expect_parse_success(
            r#"
            package MyLibrary
                model A
                    Real x = 1;
                equation
                end A;

                model B
                    Real y = 2;
                equation
                end B;
            end MyLibrary;
            "#,
        );
    }

    /// MLS: "function" is a class for algorithmic computations
    #[test]
    fn mls_1_1_function_computation() {
        expect_parse_success(
            r#"
            function square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end square;
            "#,
        );
    }

    /// MLS: "type" is an alias for another type with optional modifications
    #[test]
    fn mls_1_1_type_alias() {
        expect_parse_success(
            r#"
            type Voltage = Real(unit = "V");
            "#,
        );
    }

    /// MLS: "operator record" is a record with operator overloading
    #[test]
    fn mls_1_1_operator_record() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;
            end Complex;
            "#,
        );
    }
}

// ============================================================================
// §1.1.2 EQUATION-BASED MODELING
// ============================================================================

/// MLS §1.1: Modelica uses equation-based (declarative) modeling
mod equation_based {
    use super::*;

    /// MLS: "Equations are not assignments" - both sides are symmetric
    #[test]
    fn mls_1_1_equation_symmetric() {
        expect_success(
            r#"
            model Symmetric
                Real x, y;
            equation
                x = y;  // This is symmetric: x = y is the same as y = x
                x = 1;
            end Symmetric;
            "#,
            "Symmetric",
        );
    }

    /// MLS: Multiple equations form a system to be solved
    #[test]
    fn mls_1_1_equation_system() {
        expect_success(
            r#"
            model System
                Real x, y, z;
            equation
                x + y = 10;
                y + z = 15;
                x + z = 12;
            end System;
            "#,
            "System",
        );
    }

    /// MLS: Differential equations using der()
    #[test]
    fn mls_1_1_differential_equations() {
        expect_success(
            r#"
            model FirstOrder
                Real x(start = 1);
            equation
                der(x) = -x;
            end FirstOrder;
            "#,
            "FirstOrder",
        );
    }

    /// MLS: Algebraic equations (no derivatives)
    #[test]
    fn mls_1_1_algebraic_equations() {
        expect_success(
            r#"
            model Algebraic
                Real x, y;
            equation
                x^2 + y^2 = 1;
                x = 0.6;
            end Algebraic;
            "#,
            "Algebraic",
        );
    }
}

// ============================================================================
// §1.1.3 COMPONENT ENCAPSULATION
// ============================================================================

/// MLS §1.1: Models encapsulate components
mod encapsulation {
    use super::*;

    /// MLS: Model can contain components of other types
    #[test]
    fn mls_1_1_component_composition() {
        expect_success(
            r#"
            model Inner
                Real x = 1;
            equation
            end Inner;

            model Outer
                Inner a;
                Inner b;
            equation
            end Outer;
            "#,
            "Outer",
        );
    }

    /// MLS: Hierarchical component access with dot notation
    #[test]
    fn mls_1_1_hierarchical_access() {
        expect_success(
            r#"
            model Child
                Real value = 0;
            equation
            end Child;

            model Parent
                Child c;
                Real total;
            equation
                total = c.value + 1;
            end Parent;
            "#,
            "Parent",
        );
    }

    /// MLS: Parameters allow configuration of components
    #[test]
    fn mls_1_1_parameterization() {
        expect_success(
            r#"
            model Configurable
                parameter Real gain = 1.0;
                Real x;
            equation
                x = gain * 2;
            end Configurable;
            "#,
            "Configurable",
        );
    }

    /// MLS: Component modification at instantiation
    #[test]
    fn mls_1_1_modification() {
        expect_success(
            r#"
            model Base
                parameter Real p = 1;
                Real x = p;
            equation
            end Base;

            model Modified
                Base b(p = 10);
            equation
            end Modified;
            "#,
            "Modified",
        );
    }
}

// ============================================================================
// §1.1.4 PHYSICAL MODELING CONCEPTS
// ============================================================================

/// MLS §1.1: Physical modeling with connectors and connections
mod physical_modeling {
    use super::*;

    /// MLS: Connectors define physical interfaces
    #[test]
    fn mls_1_1_physical_connector() {
        expect_parse_success(
            r#"
            connector ElectricalPin
                Real v "Voltage";
                flow Real i "Current";
            end ElectricalPin;
            "#,
        );
    }

    /// MLS: Flow variables sum to zero at connection points
    #[test]
    fn mls_1_1_flow_balance() {
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model TwoPort
                C p;
                C n;
            equation
                p.e = n.e;
                p.f + n.f = 0;
            end TwoPort;
            "#,
            "TwoPort",
        );
    }

    /// MLS: Potential variables are equal at connection points
    #[test]
    fn mls_1_1_potential_equality() {
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model Node
                C c1;
                C c2;
            equation
                connect(c1, c2);
            end Node;
            "#,
            "Node",
        );
    }
}
