//! MLS §7: High Priority Inheritance Tests
//!
//! This module tests high priority normative requirements:
//! - §7.2.6: final modifier enforcement
//! - §7.3.2: constrainedby type checking
//! - §7.4: break modifier semantics
//! - §7.1: Extends restrictions
//!
//! Reference: https://specification.modelica.org/master/inheritance-modification-redeclaration.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §7.2.6 FINAL MODIFIER ENFORCEMENT
// ============================================================================

/// MLS §7.2.6: Final modifier prevents further modification
mod final_modifier {
    use super::*;

    /// MLS: "A final modification cannot be further modified"
    #[test]
    #[ignore = "Final modifier enforcement not yet implemented"]
    fn mls_7_2_6_final_cannot_be_modified() {
        expect_failure(
            r#"
            model Base
                final parameter Real k = 1;
            equation
            end Base;

            model Derived
                extends Base(k = 2);
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// MLS: "A final modification cannot be further modified" (nested)
    #[test]
    #[ignore = "Final modifier enforcement not yet implemented"]
    fn mls_7_2_6_final_nested_modification() {
        expect_failure(
            r#"
            model Inner
                parameter Real x = 1;
            equation
            end Inner;

            model Base
                final Inner i(x = 2);
            equation
            end Base;

            model Derived
                extends Base(i(x = 3));
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// MLS: "A final element cannot be redeclared"
    #[test]
    #[ignore = "Final redeclare restriction not yet implemented"]
    fn mls_7_2_6_final_cannot_redeclare() {
        expect_failure(
            r#"
            model Base
                final replaceable model M = Integer;
            equation
            end Base;

            model Derived
                extends Base(redeclare model M = Real);
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Valid: Non-final can be modified
    #[test]
    fn mls_7_2_6_non_final_modification() {
        expect_success(
            r#"
            model Base
                parameter Real k = 1;
            equation
            end Base;

            model Derived
                extends Base(k = 2);
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Valid: Using final in base class
    #[test]
    fn mls_7_2_6_final_declaration() {
        expect_success(
            r#"
            model Base
                final parameter Real k = 1;
            equation
            end Base;

            model Derived
                extends Base;
            equation
            end Derived;
            "#,
            "Derived",
        );
    }
}

// ============================================================================
// §7.3.2 CONSTRAINEDBY TYPE CHECKING
// ============================================================================

/// MLS §7.3.2: constrainedby restricts redeclaration types
mod constrainedby_checking {
    use super::*;

    /// MLS: "Redeclaration must be subtype of constraining type"
    #[test]
    #[ignore = "constrainedby type checking not yet implemented"]
    fn mls_7_3_2_constrainedby_violation() {
        expect_failure(
            r#"
            model Base
                Real x = 1;
            equation
            end Base;

            model Extended
                extends Base;
                Real y = 2;
            equation
            end Extended;

            model Container
                replaceable Base b constrainedby Base;
            equation
            end Container;

            connector C
                Real v;
            end C;

            model Test
                extends Container(redeclare C b);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Redeclaration compatible with constraining type
    #[test]
    fn mls_7_3_2_constrainedby_satisfied() {
        expect_success(
            r#"
            model Base
                Real x = 1;
            equation
            end Base;

            model Extended
                extends Base;
                Real y = 2;
            equation
            end Extended;

            model Container
                replaceable Base b constrainedby Base;
            equation
            end Container;

            model Test
                extends Container(redeclare Extended b);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Default constraining type is declared type"
    #[test]
    fn mls_7_3_2_default_constraining_type() {
        expect_success(
            r#"
            model Base
                Real x = 1;
            equation
            end Base;

            model Extended
                extends Base;
                Real y = 2;
            equation
            end Extended;

            model Container
                replaceable Base b;
            equation
            end Container;

            model Test
                extends Container(redeclare Extended b);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §7.4 BREAK MODIFIER
// ============================================================================

/// MLS §7.4: break modifier for selective model extension
mod break_modifier {
    use super::*;

    /// MLS: "break removes inherited element"
    #[test]
    fn mls_7_4_break_removes_element() {
        expect_success(
            r#"
            model Base
                Real x = 1;
                Real y = 2;
            equation
            end Base;

            model Derived
                extends Base(break x);
                Real x = 10;
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// MLS: "break can remove equations"
    #[test]
    #[ignore = "break equation syntax not yet supported"]
    fn mls_7_4_break_equation() {
        expect_parse_success(
            r#"
            model Base
                Real x;
            equation
                x = 1;
            end Base;

            model Derived
                extends Base(break x = 1);
            equation
                x = 2;
            end Derived;
            "#,
        );
    }

    /// MLS: "break cannot be used with non-inherited"
    #[test]
    #[ignore = "break on non-inherited element not yet checked"]
    fn mls_7_4_break_non_inherited() {
        expect_failure(
            r#"
            model Base
                Real x = 1;
            equation
            end Base;

            model Derived
                extends Base(break nonexistent);
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Valid: Multiple breaks
    #[test]
    fn mls_7_4_multiple_breaks() {
        expect_success(
            r#"
            model Base
                Real x = 1;
                Real y = 2;
                Real z = 3;
            equation
            end Base;

            model Derived
                extends Base(break x, break y);
                Real x = 10;
                Real y = 20;
            equation
            end Derived;
            "#,
            "Derived",
        );
    }
}

// ============================================================================
// §7.1 EXTENDS RESTRICTIONS
// ============================================================================

/// MLS §7.1: Restrictions on extends clause
mod extends_restrictions {
    use super::*;

    /// MLS: "Cannot extend a connector as a model"
    #[test]
    fn mls_7_1_model_extends_connector() {
        expect_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                extends C;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot extend a function as a model"
    #[test]
    fn mls_7_1_model_extends_function() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x;
            end F;

            model Test
                extends F;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Model extends model
    #[test]
    fn mls_7_1_model_extends_model() {
        expect_success(
            r#"
            model Base
                Real x = 1;
            equation
            end Base;

            model Derived
                extends Base;
                Real y = 2;
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Valid: Block extends block
    #[test]
    fn mls_7_1_block_extends_block() {
        expect_parse_success(
            r#"
            block Base
                input Real u;
                output Real y;
            equation
                y = u;
            end Base;

            block Derived
                extends Base;
            end Derived;
            "#,
        );
    }

    /// Valid: Connector extends connector
    #[test]
    fn mls_7_1_connector_extends_connector() {
        expect_parse_success(
            r#"
            connector Base
                Real v;
            end Base;

            connector Extended
                extends Base;
                flow Real i;
            end Extended;
            "#,
        );
    }

    /// Valid: Record extends record
    #[test]
    fn mls_7_1_record_extends_record() {
        expect_parse_success(
            r#"
            record Base
                Real x;
            end Base;

            record Extended
                extends Base;
                Real y;
            end Extended;
            "#,
        );
    }
}

// ============================================================================
// §7.2 MODIFICATION RESTRICTIONS
// ============================================================================

/// MLS §7.2: Modification restrictions
mod modification_restrictions {
    use super::*;

    /// MLS: "Modification must refer to existing element"
    #[test]
    fn mls_7_2_modify_nonexistent() {
        expect_failure(
            r#"
            model Base
                Real x = 1;
            equation
            end Base;

            model Derived
                extends Base(nonexistent = 2);
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// MLS: "each modifier requires array element"
    #[test]
    #[ignore = "each modifier validation not yet implemented"]
    fn mls_7_2_each_on_scalar() {
        expect_failure(
            r#"
            model Inner
                Real x = 1;
            equation
            end Inner;

            model Container
                Inner i;
            equation
            end Container;

            model Test
                extends Container(each i(x = 2));
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: each on array component
    #[test]
    fn mls_7_2_each_on_array() {
        expect_success(
            r#"
            model Container
                Real x[3](each start = 0);
            equation
                for i in 1:3 loop
                    x[i] = i;
                end for;
            end Container;
            "#,
            "Container",
        );
    }

    /// Valid: Nested modification
    #[test]
    fn mls_7_2_nested_modification() {
        expect_success(
            r#"
            model Inner
                parameter Real k = 1;
            equation
            end Inner;

            model Outer
                Inner i;
            equation
            end Outer;

            model Test
                extends Outer(i(k = 2));
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §7.3 REPLACEABLE AND REDECLARE
// ============================================================================

/// MLS §7.3: Replaceable and redeclare semantics
mod replaceable_redeclare {
    use super::*;

    /// MLS: "Cannot redeclare non-replaceable"
    #[test]
    #[ignore = "Non-replaceable redeclare check not yet implemented"]
    fn mls_7_3_redeclare_non_replaceable() {
        expect_failure(
            r#"
            model Base
                parameter Real k = 1;
            equation
            end Base;

            model Derived
                extends Base(redeclare parameter Integer k = 2);
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Valid: Redeclare replaceable
    #[test]
    fn mls_7_3_redeclare_replaceable() {
        expect_success(
            r#"
            model Base
                replaceable parameter Real k = 1;
            equation
            end Base;

            model Derived
                extends Base(redeclare parameter Real k = 2);
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Valid: Replaceable model component
    #[test]
    fn mls_7_3_replaceable_model() {
        expect_success(
            r#"
            model DefaultModel
                Real x = 1;
            equation
            end DefaultModel;

            model AltModel
                Real x = 2;
            equation
            end AltModel;

            model Container
                replaceable DefaultModel m;
            equation
            end Container;

            model Test
                extends Container(redeclare AltModel m);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
