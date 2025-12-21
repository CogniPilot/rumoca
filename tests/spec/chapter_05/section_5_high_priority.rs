//! MLS §5: High Priority Scoping and Lookup Tests
//!
//! This module tests high priority normative requirements:
//! - §5.4: Inner/outer component matching
//! - §5.3: Name lookup rules
//! - §5.2: Encapsulated lookup restrictions
//! - §5.6: Circular dependency detection
//!
//! Reference: https://specification.modelica.org/master/scoping-name-lookup-and-flattening.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §5.4 INNER/OUTER COMPONENT MATCHING
// ============================================================================

/// MLS §5.4: Inner/outer component requirements
mod inner_outer_matching {
    use super::*;

    /// MLS: "An outer element shall have a matching inner element"
    #[test]
    #[ignore = "Missing inner detection not yet implemented"]
    fn mls_5_4_outer_without_inner() {
        expect_failure(
            r#"
            model Test
                outer Real world;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "An outer element shall have a matching inner element"
    #[test]
    #[ignore = "Missing inner detection not yet implemented"]
    fn mls_5_4_outer_in_subcomponent_without_inner() {
        expect_failure(
            r#"
            model Inner
                outer Real env;
            equation
            end Inner;

            model Test
                Inner i;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: outer with matching inner
    #[test]
    fn mls_5_4_outer_with_inner() {
        expect_success(
            r#"
            model Inner
                outer Real env;
            equation
            end Inner;

            model Test
                inner Real env = 1.0;
                Inner i;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: inner without outer (unused inner is ok)
    #[test]
    fn mls_5_4_inner_without_outer() {
        expect_success(
            r#"
            model Test
                inner Real env = 1.0;
                Real x = 2.0;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Inner/outer must have compatible types"
    #[test]
    #[ignore = "Inner/outer type compatibility check not yet implemented"]
    fn mls_5_4_inner_outer_type_mismatch() {
        expect_failure(
            r#"
            model Inner
                outer Integer env;
            equation
            end Inner;

            model Test
                inner Real env = 1.0;
                Inner i;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Nested inner/outer
    #[test]
    fn mls_5_4_nested_inner_outer() {
        expect_success(
            r#"
            model Level3
                outer Real world;
            equation
            end Level3;

            model Level2
                Level3 l3;
            equation
            end Level2;

            model Level1
                Level2 l2;
            equation
            end Level1;

            model Test
                inner Real world = 9.81;
                Level1 l1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "inner outer" simultaneous usage
    #[test]
    fn mls_5_4_inner_outer_simultaneous() {
        expect_success(
            r#"
            model Middle
                inner outer Real env;
            equation
            end Middle;

            model Leaf
                outer Real env;
            equation
            end Leaf;

            model Test
                inner Real env = 1.0;
                Middle mid(env = env);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §5.3 NAME LOOKUP RULES
// ============================================================================

/// MLS §5.3: Static name lookup rules
mod name_lookup_rules {
    use super::*;

    /// MLS: "Global name lookup starts from root"
    #[test]
    fn mls_5_3_global_lookup() {
        expect_success(
            r#"
            package Lib
                constant Real pi = 3.14159;
            end Lib;

            model Test
                Real x = .Lib.pi;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Relative lookup starts from enclosing class"
    #[test]
    fn mls_5_3_relative_lookup() {
        expect_success(
            r#"
            package Pkg
                constant Real c = 1.0;

                model Inner
                    Real x = c;
                equation
                end Inner;
            end Pkg;

            model Test
                Pkg.Inner i;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Local declarations shadow outer scope"
    #[test]
    fn mls_5_3_shadowing() {
        expect_success(
            r#"
            model Outer
                Real x = 1;
                model Inner
                    Real x = 2;
                    Real y = x;
                equation
                end Inner;
                Inner i;
            equation
            end Outer;
            "#,
            "Outer",
        );
    }

    /// MLS: "Name not found should error"
    #[test]
    fn mls_5_3_undefined_name() {
        expect_failure(
            r#"
            model Test
                Real x = undefinedVariable;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot reference class before it's defined in same scope"
    #[test]
    fn mls_5_3_forward_reference() {
        // In Modelica, order doesn't matter for declarations
        // This test checks if the compiler handles it correctly
        expect_success(
            r#"
            model Test
                Real y = x;
                Real x = 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §5.2 ENCAPSULATED LOOKUP RESTRICTIONS
// ============================================================================

/// MLS §5.2: Encapsulated class lookup restrictions
mod encapsulated_lookup {
    use super::*;

    /// MLS: "Encapsulated class cannot access enclosing scope directly"
    #[test]
    fn mls_5_2_encapsulated_no_enclosing_access() {
        expect_failure(
            r#"
            package Outer
                constant Real c = 1.0;

                encapsulated model Inner
                    Real x = c;
                equation
                end Inner;
            end Outer;
            "#,
            "Outer.Inner",
        );
    }

    /// MLS: "Encapsulated class can access via import"
    #[test]
    fn mls_5_2_encapsulated_with_import() {
        expect_parse_success(
            r#"
            package Outer
                constant Real c = 1.0;

                encapsulated model Inner
                    import Outer.c;
                    Real x = c;
                equation
                end Inner;
            end Outer;
            "#,
        );
    }

    /// Valid: Non-encapsulated can access enclosing
    #[test]
    fn mls_5_2_non_encapsulated_access() {
        expect_success(
            r#"
            package Outer
                constant Real c = 1.0;

                model Inner
                    Real x = c;
                equation
                end Inner;
            end Outer;

            model Test
                Outer.Inner i;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §5.6 CIRCULAR DEPENDENCY DETECTION
// ============================================================================

/// MLS §5.6: Circular dependency restrictions
mod circular_dependency {
    use super::*;

    /// MLS: "Circular inheritance is forbidden"
    #[test]
    #[ignore = "Circular inheritance detection not yet implemented"]
    fn mls_5_6_circular_extends() {
        expect_failure(
            r#"
            model A
                extends B;
            end A;

            model B
                extends A;
            end B;
            "#,
            "A",
        );
    }

    /// MLS: "Self-inheritance is forbidden"
    #[test]
    #[ignore = "Self-inheritance detection not yet implemented"]
    fn mls_5_6_self_extends() {
        expect_failure(
            r#"
            model A
                extends A;
            end A;
            "#,
            "A",
        );
    }

    /// Valid: Non-circular inheritance chain
    #[test]
    fn mls_5_6_linear_extends() {
        expect_success(
            r#"
            model Base
                Real x = 1;
            equation
            end Base;

            model Middle
                extends Base;
                Real y = 2;
            equation
            end Middle;

            model Derived
                extends Middle;
                Real z = 3;
            equation
            end Derived;
            "#,
            "Derived",
        );
    }
}

// ============================================================================
// §5.3 IMPORT PRIORITY RULES
// ============================================================================

/// MLS §5.3: Import statement lookup priority
mod import_priority {
    use super::*;

    /// MLS: "Qualified import brings specific name into scope"
    #[test]
    fn mls_5_3_qualified_import() {
        expect_success(
            r#"
            package Lib
                constant Real pi = 3.14159;
            end Lib;

            model Test
                import Lib.pi;
                Real x = pi;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Unqualified import brings all names into scope"
    #[test]
    #[ignore = "Unqualified import not fully implemented"]
    fn mls_5_3_unqualified_import() {
        expect_success(
            r#"
            package Lib
                constant Real pi = 3.14159;
                constant Real e = 2.71828;
            end Lib;

            model Test
                import Lib.*;
                Real x = pi;
                Real y = e;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Renaming import creates alias"
    #[test]
    fn mls_5_3_renaming_import() {
        expect_success(
            r#"
            package Lib
                constant Real pi = 3.14159;
            end Lib;

            model Test
                import P = Lib.pi;
                Real x = P;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Local declaration shadows import"
    #[test]
    fn mls_5_3_local_shadows_import() {
        expect_success(
            r#"
            package Lib
                constant Real c = 1.0;
            end Lib;

            model Test
                import Lib.c;
                Real c = 2.0;
                Real x = c;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Import conflict with duplicate names"
    #[test]
    #[ignore = "Import name conflict detection not yet implemented"]
    fn mls_5_3_import_conflict() {
        expect_failure(
            r#"
            package Lib1
                constant Real c = 1.0;
            end Lib1;

            package Lib2
                constant Real c = 2.0;
            end Lib2;

            model Test
                import Lib1.c;
                import Lib2.c;
                Real x = c;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
