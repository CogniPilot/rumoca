//! MLS §7.1-7.3: Inheritance, Modifications, Redeclaration
//!
//! Tests for:
//! - §7.1: Extends clause for inheritance
//! - §7.2: Modifications to inherited elements
//! - §7.2.4: Final modifier
//! - §7.2.5: Each modifier for arrays
//! - §7.3: Redeclaration of replaceable elements
//!
//! Reference: https://specification.modelica.org/master/inheritance-modification-redeclaration.html

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §7.1 INHERITANCE – EXTENDS CLAUSE
// ============================================================================

/// MLS §7.1: Extends clause for inheritance
mod section_7_1_extends {
    use super::*;

    #[test]
    fn mls_7_1_simple_extends() {
        expect_success(
            r#"
            model Base
                Real x;
            equation
                der(x) = 1;
            end Base;

            model Derived
                extends Base;
            end Derived;
            "#,
            "Derived",
        );
    }

    #[test]
    fn mls_7_1_extends_with_additions() {
        expect_success(
            r#"
            model Base
                Real x;
            equation
                der(x) = 1;
            end Base;

            model Derived
                extends Base;
                Real y;
            equation
                y = 2 * x;
            end Derived;
            "#,
            "Derived",
        );
    }

    #[test]
    fn mls_7_1_multiple_extends() {
        expect_success(
            r#"
            model A
                Real a = 1;
            equation
            end A;

            model B
                Real b = 2;
            equation
            end B;

            model C
                extends A;
                extends B;
            equation
            end C;
            "#,
            "C",
        );
    }

    #[test]
    fn mls_7_1_diamond_inheritance() {
        expect_success(
            r#"
            model Base
                Real x;
            equation
            end Base;

            model Left
                extends Base;
                Real l;
            equation
                l = x;
            end Left;

            model Right
                extends Base;
                Real r;
            equation
                r = x;
            end Right;

            model Diamond
                extends Left;
                extends Right;
            equation
                x = 1;
            end Diamond;
            "#,
            "Diamond",
        );
    }

    #[test]
    fn mls_7_1_extends_from_package() {
        expect_success(
            r#"
            package Lib
                model BaseModel
                    Real x;
                equation
                    der(x) = 1;
                end BaseModel;
            end Lib;

            model MyModel
                extends Lib.BaseModel;
            end MyModel;
            "#,
            "MyModel",
        );
    }

    #[test]
    fn mls_7_1_extends_chain() {
        expect_success(
            r#"
            model A Real a; equation a=1; end A;
            model B extends A; Real b; equation b=a; end B;
            model C extends B; Real c; equation c=b; end C;
            "#,
            "C",
        );
    }

    #[test]
    fn mls_7_1_extends_with_parameters() {
        expect_success(
            r#"
            model Base
                parameter Real k = 1;
                Real x(start = 0);
            equation
                der(x) = k;
            end Base;

            model Derived
                extends Base;
            end Derived;
            "#,
            "Derived",
        );
    }
}

// ============================================================================
// §7.2 MODIFICATIONS
// ============================================================================

/// MLS §7.2: Modifications to inherited elements
mod section_7_2_modifications {
    use super::*;

    #[test]
    fn mls_7_2_modify_parameter() {
        expect_success(
            r#"
            model Parameterized
                parameter Real k = 1;
                Real x;
            equation
                der(x) = -k * x;
            end Parameterized;

            model Modified
                extends Parameterized(k = 2);
            end Modified;
            "#,
            "Modified",
        );
    }

    #[test]
    fn mls_7_2_modify_start() {
        expect_success(
            r#"
            model Base
                Real x(start = 0);
            equation
                der(x) = 1;
            end Base;

            model Modified
                extends Base(x(start = 5));
            end Modified;
            "#,
            "Modified",
        );
    }

    #[test]
    fn mls_7_2_modify_multiple() {
        expect_success(
            r#"
            model Base
                parameter Real k = 1;
                Real x(start = 0);
            equation
                der(x) = k;
            end Base;

            model Modified
                extends Base(k = 2, x(start = 10));
            end Modified;
            "#,
            "Modified",
        );
    }

    #[test]
    fn mls_7_2_nested_modification() {
        expect_success(
            r#"
            model Inner
                parameter Real p = 1;
                Real x(start = 0);
            equation
                der(x) = p;
            end Inner;

            model Outer
                Inner sub;
            end Outer;

            model Modified
                extends Outer(sub(p = 2, x(start = 10)));
            end Modified;
            "#,
            "Modified",
        );
    }

    #[test]
    fn mls_7_2_component_modification() {
        expect_success(
            r#"
            model Inner Real x(start=0); equation der(x)=1; end Inner;
            model Outer Inner i(x(start=5)); end Outer;
            "#,
            "Outer",
        );
    }
}

/// MLS §7.2.4: Final modifier
mod section_7_2_4_final {
    use super::*;

    #[test]
    fn mls_7_2_4_final_parameter() {
        expect_parse_success(
            r#"
            model WithFinal
                final parameter Real k = 1;
                Real x;
            equation
                der(x) = -k * x;
            end WithFinal;
            "#,
        );
    }

    #[test]
    fn mls_7_2_4_final_in_extends() {
        expect_parse_success(
            r#"
            model Base
                parameter Real k = 1;
            end Base;

            model Derived
                extends Base(final k = 2);
            end Derived;
            "#,
        );
    }
}

/// MLS §7.2.5: Each modifier for arrays
mod section_7_2_5_each {
    use super::*;

    #[test]
    fn mls_7_2_5_each_basic() {
        expect_success(
            r#"
            model ArrayModel
                parameter Integer n = 3;
                Real x[n](each start = 0);
            equation
                for i in 1:n loop
                    der(x[i]) = i;
                end for;
            end ArrayModel;
            "#,
            "ArrayModel",
        );
    }

    #[test]
    fn mls_7_2_5_each_in_modification() {
        expect_success(
            r#"
            model Base
                Real x[3];
            equation
                for i in 1:3 loop der(x[i]) = 0; end for;
            end Base;

            model Derived
                extends Base(x(each start = 1));
            end Derived;
            "#,
            "Derived",
        );
    }

    #[test]
    fn mls_7_2_5_each_fixed() {
        expect_success(
            r#"
            model Test
                Real x[5](each start = 0, each fixed = true);
            equation
                for i in 1:5 loop der(x[i]) = i; end for;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §7.3 REDECLARATION
// ============================================================================

/// MLS §7.3: Redeclaration of replaceable elements
mod section_7_3_redeclaration {
    use super::*;

    #[test]
    fn mls_7_3_replaceable_component() {
        expect_parse_success(
            r#"
            model Base
                replaceable Real x = 1;
            equation
            end Base;
            "#,
        );
    }

    #[test]
    fn mls_7_3_redeclare() {
        expect_parse_success(
            r#"
            model Base
                replaceable parameter Real k = 1;
            equation
            end Base;

            model Derived
                extends Base(redeclare constant Real k = 2);
            equation
            end Derived;
            "#,
        );
    }

    #[test]
    fn mls_7_3_replaceable_type() {
        expect_parse_success(
            r#"
            model Generic
                replaceable type T = Real;
                T x;
            equation
                x = 1;
            end Generic;
            "#,
        );
    }
}

/// MLS §7.3.1: Constraining clause
mod section_7_3_1_constraining {
    use super::*;

    #[test]
    fn mls_7_3_1_constrainedby() {
        expect_parse_success(
            r#"
            partial model PartialResistor
                Real v;
                Real i;
            equation
                v = i;
            end PartialResistor;

            model LinearResistor
                extends PartialResistor;
                parameter Real R = 1;
            equation
                v = R * i;
            end LinearResistor;

            model Circuit
                replaceable LinearResistor r constrainedby PartialResistor;
            equation
            end Circuit;
            "#,
        );
    }
}

// ============================================================================
// INHERITANCE EDGE CASES
// ============================================================================

/// Edge cases for inheritance
mod inheritance_edge_cases {
    use super::*;

    #[test]
    fn edge_extends_with_equation_section() {
        expect_success(
            r#"
            model Base
                Real x;
            equation
                der(x) = 1;
            end Base;

            model Derived
                extends Base;
                Real y;
            equation
                y = x + 1;
            end Derived;
            "#,
            "Derived",
        );
    }

    #[test]
    fn edge_extends_partial() {
        expect_success(
            r#"
            partial model Partial
                Real x;
            end Partial;

            model Complete
                extends Partial;
            equation
                x = 1;
            end Complete;
            "#,
            "Complete",
        );
    }

    #[test]
    fn edge_multiple_modifications() {
        expect_success(
            r#"
            model Base
                parameter Real a = 1;
                parameter Real b = 2;
                parameter Real c = 3;
                Real x;
            equation
                x = a + b + c;
            end Base;

            model Derived
                extends Base(a = 10, b = 20, c = 30);
            end Derived;
            "#,
            "Derived",
        );
    }
}
