//! MLS Chapter 5: Critical Edge Case Tests
//!
//! This module contains critical tests for:
//! - Circular dependency detection
//! - Import cycle detection
//! - Scoping edge cases
//! - Flattening edge cases
//!
//! Reference: https://specification.modelica.org/master/scoping-name-lookup-and-flattening.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// CRITICAL: CIRCULAR DEPENDENCY DETECTION
// ============================================================================

/// MLS §5.6: Circular class dependencies
mod circular_class_dependencies {
    use super::*;

    /// Critical: Direct circular extends
    #[test]
    #[ignore = "Circular dependency detection not yet implemented"]
    fn mls_5_6_circular_class_dependency() {
        expect_failure(
            r#"
            class A extends B; end A;
            class B extends A; end B;
            "#,
            "A",
        );
    }

    /// Critical: Self-extending class (simplest cycle)
    #[test]
    #[ignore = "Circular dependency detection not yet implemented"]
    fn mls_5_6_self_extends() {
        expect_failure(
            r#"
            class A extends A; end A;
            "#,
            "A",
        );
    }

    /// Critical: Three-way circular extends
    #[test]
    #[ignore = "Circular dependency detection not yet implemented"]
    fn mls_5_6_three_way_circular() {
        expect_failure(
            r#"
            class A extends B; end A;
            class B extends C; end B;
            class C extends A; end C;
            "#,
            "A",
        );
    }

    /// Critical: Circular through nested classes
    #[test]
    #[ignore = "Circular dependency detection not yet implemented"]
    fn mls_5_6_circular_through_nested() {
        expect_failure(
            r#"
            package P
                class A extends P.B; end A;
                class B extends P.A; end B;
            end P;
            "#,
            "P.A",
        );
    }

    /// Critical: Circular via component type
    #[test]
    #[ignore = "Circular dependency detection not yet implemented"]
    fn mls_5_6_circular_via_component() {
        // This might be valid in some interpretations (recursive data structures)
        // but for strict inheritance checking, it could be problematic
        expect_parse_success(
            r#"
            class Node
                Node next;
            end Node;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: IMPORT CYCLE DETECTION
// ============================================================================

/// MLS §5.3: Import cycle scenarios
mod import_cycles {
    use super::*;

    /// Critical: Import from enclosing scope (valid)
    #[test]
    fn mls_5_3_import_enclosing_valid() {
        expect_parse_success(
            r#"
            package Outer
                constant Real c = 1.0;
                package Inner
                    import Outer.c;
                    model M
                        Real x = c;
                    end M;
                end Inner;
            end Outer;
            "#,
        );
    }

    /// Critical: Complex import chain (valid)
    #[test]
    fn mls_5_3_import_chain_valid() {
        expect_parse_success(
            r#"
            package A
                constant Real x = 1.0;
            end A;

            package B
                import A.x;
                constant Real y = x;
            end B;

            package C
                import B.y;
                constant Real z = y;
            end C;
            "#,
        );
    }

    /// Critical: Forward reference in same scope
    #[test]
    fn mls_5_3_forward_reference() {
        expect_parse_success(
            r#"
            package P
                model User
                    Helper h;
                end User;
                model Helper
                    Real x;
                end Helper;
            end P;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: SCOPING EDGE CASES
// ============================================================================

/// MLS §5.3: Critical scoping scenarios
mod scoping_edge_cases {
    use super::*;

    /// Critical: Shadowing with same name at different levels
    #[test]
    fn mls_5_3_shadowing_levels() {
        expect_parse_success(
            r#"
            package P
                constant Real x = 1.0;
                model M
                    Real x = 2.0;
                    Real y = x;  // Should use local x
                end M;
            end P;
            "#,
        );
    }

    /// Critical: Access to enclosing scope
    #[test]
    fn mls_5_3_enclosing_access() {
        expect_success(
            r#"
            package P
                constant Real pi = 3.14159;
                model Circle
                    parameter Real r = 1.0;
                    Real area = pi * r^2;
                end Circle;
            end P;
            "#,
            "P.Circle",
        );
    }

    /// Critical: Qualified name lookup
    #[test]
    fn mls_5_3_qualified_lookup() {
        expect_success(
            r#"
            package Math
                constant Real e = 2.71828;
            end Math;

            model Test
                Real x = Math.e;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Deeply nested scope access
    #[test]
    fn mls_5_3_deep_nested_access() {
        expect_parse_success(
            r#"
            package L1
                constant Real a = 1.0;
                package L2
                    constant Real b = a + 1.0;
                    package L3
                        constant Real c = b + 1.0;
                        model M
                            Real x = c;
                        end M;
                    end L3;
                end L2;
            end L1;
            "#,
        );
    }

    /// Critical: Lookup with extends in scope
    #[test]
    fn mls_5_3_lookup_with_extends() {
        expect_parse_success(
            r#"
            class Base
                constant Real x = 1.0;
            end Base;

            class Derived
                extends Base;
                Real y = x;
            end Derived;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: FLATTENING EDGE CASES
// ============================================================================

/// MLS §5.6: Critical flattening scenarios
mod flattening_edge_cases {
    use super::*;

    /// Critical: Multiple levels of extends
    #[test]
    fn mls_5_6_multilevel_extends() {
        expect_success(
            r#"
            class A
                Real x;
            end A;

            class B
                extends A;
                Real y;
            end B;

            class C
                extends B;
                Real z;
            equation
                x = 1;
                y = 2;
                z = 3;
            end C;
            "#,
            "C",
        );
    }

    /// Critical: Extends with modifications at each level
    #[test]
    fn mls_5_6_multilevel_modifications() {
        expect_success(
            r#"
            class Base
                parameter Real a = 1.0;
            end Base;

            class Mid
                extends Base(a = 2.0);
            end Mid;

            class Top
                extends Mid(a = 3.0);
            equation
            end Top;
            "#,
            "Top",
        );
    }

    /// Critical: Component with class modification
    #[test]
    fn mls_5_6_component_class_mod() {
        expect_success(
            r#"
            class Inner
                parameter Real x = 1.0;
            end Inner;

            model Outer
                Inner i(x = 2.0);
            equation
            end Outer;
            "#,
            "Outer",
        );
    }

    /// Critical: Nested class flattening
    #[test]
    fn mls_5_6_nested_class_flattening() {
        expect_parse_success(
            r#"
            model Outer
                class Inner
                    Real x;
                end Inner;
                Inner i;
            equation
                i.x = 1;
            end Outer;
            "#,
        );
    }

    /// Critical: Array of components flattening
    #[test]
    fn mls_5_6_array_component_flattening() {
        expect_success(
            r#"
            class Element
                Real x;
            end Element;

            model Container
                parameter Integer n = 3;
                Element elements[n];
            equation
                for i in 1:n loop
                    elements[i].x = i;
                end for;
            end Container;
            "#,
            "Container",
        );
    }
}

// ============================================================================
// CRITICAL: INNER/OUTER EDGE CASES
// ============================================================================

/// MLS §5.4: Critical inner/outer scenarios
mod inner_outer_edge_cases {
    use super::*;

    /// Critical: Missing inner declaration
    #[test]
    #[ignore = "Missing inner detection not yet implemented"]
    fn mls_5_4_missing_inner() {
        expect_failure(
            r#"
            model NoInner
                model Child
                    outer Real x;
                end Child;
                Child c;
            equation
            end NoInner;
            "#,
            "NoInner",
        );
    }

    /// Critical: Inner without matching outer
    #[test]
    fn mls_5_4_inner_without_outer() {
        // This should be valid - inner provides default
        expect_success(
            r#"
            model OnlyInner
                inner Real world = 1.0;
            equation
            end OnlyInner;
            "#,
            "OnlyInner",
        );
    }

    /// Critical: Nested inner/outer
    #[test]
    fn mls_5_4_nested_inner_outer() {
        expect_parse_success(
            r#"
            model System
                inner Real gravity = 9.81;

                model Subsystem
                    outer Real gravity;
                    Real force = gravity * 10;
                end Subsystem;

                Subsystem sub;
            end System;
            "#,
        );
    }

    /// Critical: Simultaneous inner outer
    #[test]
    fn mls_5_5_simultaneous_inner_outer() {
        expect_parse_success(
            r#"
            model Middle
                inner outer Real shared = 1.0;
            end Middle;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: NAME CONFLICT EDGE CASES
// ============================================================================

/// MLS §5: Name conflict scenarios
mod name_conflicts {
    use super::*;

    /// Critical: Duplicate component names (should error)
    #[test]
    #[ignore = "Duplicate name detection not yet implemented"]
    fn mls_5_duplicate_component_names() {
        expect_failure(
            r#"
            model Duplicate
                Real x;
                Real x;
            end Duplicate;
            "#,
            "Duplicate",
        );
    }

    /// Critical: Component and class with same name
    #[test]
    #[ignore = "Name conflict detection not yet implemented"]
    fn mls_5_component_class_conflict() {
        expect_failure(
            r#"
            model Conflict
                class Foo end Foo;
                Real Foo;
            end Conflict;
            "#,
            "Conflict",
        );
    }

    /// Critical: Import conflicting with local
    #[test]
    #[ignore = "Import conflict detection not yet implemented"]
    fn mls_5_import_local_conflict() {
        expect_failure(
            r#"
            package P
                constant Real x = 1.0;
            end P;

            model Conflict
                import P.x;
                Real x = 2.0;
            end Conflict;
            "#,
            "Conflict",
        );
    }

    /// Critical: Multiple wildcard imports with overlap
    #[test]
    #[ignore = "Import ambiguity detection not yet implemented"]
    fn mls_5_wildcard_import_ambiguity() {
        expect_failure(
            r#"
            package A
                constant Real x = 1.0;
            end A;

            package B
                constant Real x = 2.0;
            end B;

            model Ambiguous
                import A.*;
                import B.*;
                Real y = x;  // Ambiguous reference
            end Ambiguous;
            "#,
            "Ambiguous",
        );
    }
}
