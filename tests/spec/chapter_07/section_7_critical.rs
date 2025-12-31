//! MLS Chapter 7: Critical Edge Case Tests
//!
//! This module contains critical tests for:
//! - Diamond inheritance
//! - Multiple inheritance conflicts
//! - Modification ordering
//! - Redeclaration edge cases
//!
//! Reference: https://specification.modelica.org/master/inheritance-modification-redeclaration.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// CRITICAL: DIAMOND INHERITANCE
// ============================================================================

/// MLS §7.1: Diamond inheritance scenarios
mod diamond_inheritance {
    use super::*;

    /// Critical: Classic diamond inheritance
    #[test]
    fn mls_7_1_diamond_inheritance() {
        expect_success(
            r#"
            class Base
                Real x;
            end Base;

            class Left
                extends Base;
            end Left;

            class Right
                extends Base;
            end Right;

            class Diamond
                extends Left;
                extends Right;
            equation
                x = 1;
            end Diamond;
            "#,
            "Diamond",
        );
    }

    /// Critical: Diamond with different modifications
    #[test]
    #[ignore = "Diamond with conflicting modifications not yet implemented"]
    fn mls_7_1_diamond_conflicting_mods() {
        // This should error due to conflicting modifications
        expect_failure(
            r#"
            class Base
                parameter Real x = 1.0;
            end Base;

            class Left
                extends Base(x = 2.0);
            end Left;

            class Right
                extends Base(x = 3.0);
            end Right;

            class Diamond
                extends Left;
                extends Right;
            end Diamond;
            "#,
            "Diamond",
        );
    }

    /// Critical: Diamond where modifications agree
    #[test]
    fn mls_7_1_diamond_agreeing_mods() {
        expect_success(
            r#"
            class Base
                parameter Real x = 1.0;
            end Base;

            class Left
                extends Base(x = 2.0);
            end Left;

            class Right
                extends Base(x = 2.0);
            end Right;

            class Diamond
                extends Left;
                extends Right;
            equation
            end Diamond;
            "#,
            "Diamond",
        );
    }

    /// Critical: Deep diamond (multiple levels)
    #[test]
    fn mls_7_1_deep_diamond() {
        expect_success(
            r#"
            class A Real x; end A;
            class B extends A; end B;
            class C extends A; end C;
            class D extends B; extends C; end D;
            class E extends D;
            equation x = 1;
            end E;
            "#,
            "E",
        );
    }
}

// ============================================================================
// CRITICAL: MULTIPLE EXTENDS
// ============================================================================

/// MLS §7.1: Multiple extends scenarios
mod multiple_extends {
    use super::*;

    /// Critical: Multiple extends with disjoint components
    #[test]
    fn mls_7_1_multiple_extends_disjoint() {
        expect_success(
            r#"
            class HasX
                Real x;
            end HasX;

            class HasY
                Real y;
            end HasY;

            class HasBoth
                extends HasX;
                extends HasY;
            equation
                x = 1;
                y = 2;
            end HasBoth;
            "#,
            "HasBoth",
        );
    }

    /// Critical: Multiple extends with conflicting components
    #[test]
    #[ignore = "Conflicting inheritance detection not yet implemented"]
    fn mls_7_1_multiple_extends_conflict() {
        expect_failure(
            r#"
            class HasX1
                Real x = 1;
            end HasX1;

            class HasX2
                Real x = 2;
            end HasX2;

            class Conflict
                extends HasX1;
                extends HasX2;
            end Conflict;
            "#,
            "Conflict",
        );
    }

    /// Critical: Extends order matters for modifications
    #[test]
    fn mls_7_1_extends_order_modifications() {
        expect_parse_success(
            r#"
            class Base
                parameter Real a = 1.0;
                parameter Real b = 2.0;
            end Base;

            class Modified
                extends Base(a = 10.0);
                extends Base(b = 20.0);
            end Modified;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: MODIFICATION EDGE CASES
// ============================================================================

/// MLS §7.2: Modification edge cases
mod modification_edge_cases {
    use super::*;

    /// Critical: Modification of non-existent component
    #[test]
    fn mls_7_2_modify_nonexistent() {
        expect_failure(
            r#"
            class Base
                Real x;
            end Base;

            class Derived
                extends Base(y = 1.0);  // y doesn't exist
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Critical: Modification of final component
    #[test]
    #[ignore = "Final modification restriction not yet implemented"]
    fn mls_7_2_modify_final() {
        expect_failure(
            r#"
            class Base
                final parameter Real x = 1.0;
            end Base;

            class Derived
                extends Base(x = 2.0);
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Critical: Nested modification
    #[test]
    fn mls_7_2_nested_modification() {
        expect_success(
            r#"
            class Inner
                parameter Real a = 1.0;
                parameter Real b = 2.0;
            end Inner;

            class Outer
                Inner i;
            end Outer;

            class Modified
                extends Outer(i(a = 10.0, b = 20.0));
            equation
            end Modified;
            "#,
            "Modified",
        );
    }

    /// Critical: Array element modification
    #[test]
    fn mls_7_2_array_element_modification() {
        expect_success(
            r#"
            class Base
                parameter Real x[3] = {1, 2, 3};
            end Base;

            class Derived
                extends Base(x = {4, 5, 6});
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Critical: Modification with expression
    #[test]
    fn mls_7_2_modification_with_expression() {
        expect_success(
            r#"
            class Base
                parameter Real x = 1.0;
            end Base;

            class Derived
                parameter Real factor = 2.0;
                extends Base(x = factor * 3.14);
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Critical: Conditional modification
    #[test]
    fn mls_7_2_conditional_modification() {
        expect_success(
            r#"
            class Conditional
                parameter Boolean useHigh = true;
                parameter Real x = if useHigh then 100 else 10;
            equation
            end Conditional;
            "#,
            "Conditional",
        );
    }

    /// Critical: Dot-notation hierarchical modification
    /// Tests modifications like `sub.param = value` syntax (MLS §7.2)
    /// This is equivalent to `sub(param = value)` but uses hierarchical name syntax
    #[test]
    fn mls_7_2_dot_notation_modification() {
        use rumoca::Compiler;

        let source = r#"
            model Inner
                parameter Boolean flag = false;
                parameter Real value = 1.0;
            end Inner;

            model Outer
                Inner sub;
            end Outer;

            model Test
                // Dot-notation modification: sub.flag instead of sub(flag = true)
                Outer o(sub.flag = true, sub.value = 42.0);
            equation
            end Test;
        "#;

        let result = Compiler::new()
            .model("Test")
            .compile_str(source, "test.mo")
            .expect("Should compile successfully");

        // Verify the modifications propagated correctly
        let flag = result
            .dae
            .p
            .get("o.sub.flag")
            .expect("Should have o.sub.flag");
        let value = result
            .dae
            .p
            .get("o.sub.value")
            .expect("Should have o.sub.value");

        // Check start values were set from modifications
        assert!(
            flag.start_is_modification,
            "flag should be from modification"
        );
        assert!(
            value.start_is_modification,
            "value should be from modification"
        );
    }

    /// Critical: Deep dot-notation hierarchical modification
    /// Tests multi-level modifications like `a.b.c = value`
    #[test]
    fn mls_7_2_deep_dot_notation_modification() {
        use rumoca::Compiler;

        let source = r#"
            model Level1
                parameter Real x = 0.0;
            end Level1;

            model Level2
                Level1 l1;
            end Level2;

            model Level3
                Level2 l2;
            end Level3;

            model Test
                // Three-level deep modification
                Level3 l3(l2.l1.x = 99.0);
            equation
            end Test;
        "#;

        let result = Compiler::new()
            .model("Test")
            .compile_str(source, "test.mo")
            .expect("Should compile successfully");

        // Verify the 3-level deep modification propagated
        let x = result
            .dae
            .p
            .get("l3.l2.l1.x")
            .expect("Should have l3.l2.l1.x");
        assert!(x.start_is_modification, "x should be from modification");
    }
}

// ============================================================================
// CRITICAL: REDECLARATION EDGE CASES
// ============================================================================

/// MLS §7.3: Redeclaration edge cases
mod redeclaration_edge_cases {
    use super::*;

    /// Critical: Valid redeclaration of replaceable
    #[test]
    #[ignore = "Inline class redeclaration syntax not yet supported"]
    fn mls_7_3_valid_redeclaration() {
        expect_parse_success(
            r#"
            class Base
                replaceable class Inner
                    Real x;
                end Inner;
                Inner i;
            end Base;

            class Derived
                extends Base(redeclare class Inner
                    Real x;
                    Real y;
                end Inner);
            end Derived;
            "#,
        );
    }

    /// Critical: Simple redeclaration (component)
    #[test]
    fn mls_7_3_simple_redeclaration() {
        expect_parse_success(
            r#"
            class Base
                replaceable Real x = 1.0;
            end Base;

            class Derived
                extends Base(redeclare Real x = 2.0);
            end Derived;
            "#,
        );
    }

    /// Critical: Redeclare non-replaceable (error)
    #[test]
    fn mls_7_3_redeclare_non_replaceable() {
        expect_failure(
            r#"
            class Base
                class Inner  // Not replaceable
                    Real x;
                end Inner;
            end Base;

            class Derived
                extends Base(redeclare class Inner
                    Real y;
                end Inner);
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Critical: Constrainedby redeclaration
    #[test]
    fn mls_7_3_constrainedby() {
        expect_parse_success(
            r#"
            class Interface
                Real x;
            end Interface;

            class Implementation
                extends Interface;
                Real y;
            end Implementation;

            class Container
                replaceable Interface obj constrainedby Interface;
            end Container;

            class Specific
                extends Container(redeclare Implementation obj);
            end Specific;
            "#,
        );
    }

    /// Critical: Redeclaration with modification
    #[test]
    fn mls_7_3_redeclare_with_modification() {
        expect_parse_success(
            r#"
            class Base
                replaceable parameter Real x = 1.0;
            end Base;

            class Derived
                extends Base(redeclare parameter Real x = 2.0);
            end Derived;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: PROTECTED INHERITANCE
// ============================================================================

/// MLS §7.1: Protected inheritance
mod protected_inheritance {
    use super::*;

    /// Critical: Extends of protected class
    #[test]
    #[ignore = "Protected inheritance access check not yet implemented"]
    fn mls_7_1_extend_protected_class() {
        expect_failure(
            r#"
            package P
            protected
                class Hidden
                    Real x;
                end Hidden;
            end P;

            class External
                extends P.Hidden;  // Should fail - Hidden is protected
            end External;
            "#,
            "External",
        );
    }

    /// Critical: Access to protected inherited component
    #[test]
    #[ignore = "Protected member access check not yet implemented"]
    fn mls_7_1_protected_member_access() {
        expect_failure(
            r#"
            class Base
            protected
                Real secret = 42;
            end Base;

            class Derived
                extends Base;
            public
                Real exposed = secret;  // May expose protected
            end Derived;

            model Test
                Derived d;
                Real x = d.secret;  // Should fail - secret is protected
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// CRITICAL: PARTIAL CLASS INHERITANCE
// ============================================================================

/// MLS §7.1: Partial class inheritance
mod partial_inheritance {
    use super::*;

    /// Critical: Extend partial class
    #[test]
    fn mls_7_1_extend_partial() {
        expect_success(
            r#"
            partial class PartialBase
                Real x;
            end PartialBase;

            class Complete
                extends PartialBase;
            equation
                x = 1;
            end Complete;
            "#,
            "Complete",
        );
    }

    /// Critical: Cannot instantiate partial
    #[test]
    fn mls_7_1_instantiate_partial() {
        expect_failure(
            r#"
            partial class PartialClass
                Real x;
            end PartialClass;

            model Test
                PartialClass p;  // Should fail - cannot instantiate partial
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Partial extends partial
    #[test]
    fn mls_7_1_partial_extends_partial() {
        expect_parse_success(
            r#"
            partial class Base
                Real x;
            end Base;

            partial class StillPartial
                extends Base;
                Real y;
            end StillPartial;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: ENCAPSULATED EXTENDS
// ============================================================================

/// MLS §7.1: Encapsulated inheritance scenarios
mod encapsulated_inheritance {
    use super::*;

    /// Critical: Encapsulated class extends
    #[test]
    fn mls_7_1_encapsulated_extends() {
        expect_parse_success(
            r#"
            encapsulated class Encap
                import Modelica.Units.SI;
                extends SI.Voltage;
            end Encap;
            "#,
        );
    }

    /// Critical: Encapsulated blocks outer scope access
    #[test]
    #[ignore = "Encapsulated scope restriction not yet implemented"]
    fn mls_7_1_encapsulated_no_outer_access() {
        expect_failure(
            r#"
            package P
                constant Real pi = 3.14159;
                encapsulated class Encap
                    Real area = pi * 1.0;  // Should fail - no access to pi
                end Encap;
            end P;
            "#,
            "P.Encap",
        );
    }
}
