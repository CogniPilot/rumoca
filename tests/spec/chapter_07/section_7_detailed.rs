//! MLS §7: Inheritance, Modification, and Redeclaration - Detailed Conformance Tests
//!
//! Comprehensive tests covering normative statements from MLS §7 including:
//! - §7.1: Extends clause restrictions
//! - §7.2: Modification semantics
//! - §7.3: Redeclaration constraints
//! - §7.4: Selective model extension (break)
//!
//! Reference: https://specification.modelica.org/master/inheritance-modification-redeclaration.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §7.1 EXTENDS CLAUSE RESTRICTIONS
// ============================================================================

/// MLS §7.1: Extends clause normative requirements
mod extends_restrictions {
    use super::*;

    /// MLS: "The base class shall be transitively non-replaceable"
    #[test]
    #[ignore = "Transitively non-replaceable check not yet implemented"]
    fn error_extend_replaceable_class() {
        expect_failure(
            r#"
            model Container
                replaceable model Inner
                    Real x;
                end Inner;
            end Container;

            model Test
                extends Container.Inner;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid extends from non-replaceable class
    #[test]
    fn mls_7_1_extend_non_replaceable() {
        expect_success(
            r#"
            model Base
                Real x;
            equation
                x = 1;
            end Base;

            model Derived
                extends Base;
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Extends with visibility - basic test
    #[test]
    fn mls_7_1_extends_visibility() {
        expect_success(
            r#"
            model Base
                Real x;
            equation
                x = 1;
            end Base;

            model Derived
            protected
                extends Base;
            end Derived;
            "#,
            "Derived",
        );
    }

    /// MLS §7.1.4: "Protected extends makes all base elements protected"
    #[test]
    fn mls_7_1_4_protected_extends() {
        expect_parse_success(
            r#"
            model Base
                Real publicVar;
            end Base;

            model Derived
            protected
                extends Base;
            public
                Real y = publicVar;
            end Derived;
            "#,
        );
    }

    /// Extends from partial class
    #[test]
    fn mls_7_1_extends_partial() {
        expect_success(
            r#"
            partial model PartialBase
                Real x;
            end PartialBase;

            model Complete
                extends PartialBase;
            equation
                x = 1;
            end Complete;
            "#,
            "Complete",
        );
    }

    /// Extends with annotation
    #[test]
    fn mls_7_1_extends_with_annotation() {
        expect_parse_success(
            r#"
            model Base
                Real x;
            end Base;

            model Derived
                extends Base annotation(Documentation(info="Extended from Base"));
            equation
                x = 1;
            end Derived;
            "#,
        );
    }
}

// ============================================================================
// §7.2 MODIFICATION SEMANTICS
// ============================================================================

/// MLS §7.2: Modification normative requirements
mod modification_semantics {
    use super::*;

    /// MLS: "Outer modifications override inner modifications"
    #[test]
    fn mls_7_2_outer_overrides_inner() {
        expect_success(
            r#"
            model Base
                parameter Real x = 1;
            end Base;

            model Middle
                extends Base(x = 5);
            end Middle;

            model Derived
                extends Middle(x = 10);
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Modification with nested components
    #[test]
    fn mls_7_2_nested_modification() {
        expect_success(
            r#"
            model Inner
                parameter Real p = 1;
            end Inner;

            model Outer
                Inner i;
            end Outer;

            model Test
                Outer o(i(p = 5));
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Modification of array attributes
    #[test]
    fn mls_7_2_array_attribute_modification() {
        expect_success(
            r#"
            model Base
                Real x[3](each start = 0);
            equation
                for i in 1:3 loop der(x[i]) = i; end for;
            end Base;

            model Derived
                extends Base(x(each start = 1, each fixed = true));
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Modification of component type attributes
    #[test]
    fn mls_7_2_type_attribute_modification() {
        expect_success(
            r#"
            model Test
                Real x(min = -10, max = 10, start = 0);
            equation
                x = 5;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §7.2.4 FINAL MODIFIER
// ============================================================================

/// MLS §7.2.4: Final modifier restrictions
mod final_modifier {
    use super::*;

    /// Basic final declaration
    #[test]
    fn mls_7_2_4_basic_final() {
        expect_success(
            r#"
            model Test
                final parameter Real k = 1;
                Real x;
            equation
                x = k;
            end Test;
            "#,
            "Test",
        );
    }

    /// Final in extends modification
    #[test]
    fn mls_7_2_4_final_in_extends() {
        expect_success(
            r#"
            model Base
                parameter Real k = 1;
            equation
            end Base;

            model Derived
                extends Base(final k = 2);
            end Derived;
            "#,
            "Derived",
        );
    }

    /// MLS: "A final element shall not be modified"
    #[test]
    #[ignore = "Final modification restriction not yet enforced"]
    fn error_modify_final() {
        expect_failure(
            r#"
            model Base
                final parameter Real k = 1;
            end Base;

            model Derived
                extends Base(k = 2);
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Final propagates through inheritance
    #[test]
    #[ignore = "Final propagation through inheritance not yet checked"]
    fn error_modify_inherited_final() {
        expect_failure(
            r#"
            model A
                final parameter Real k = 1;
            end A;

            model B
                extends A;
            end B;

            model C
                extends B(k = 2);
            end C;
            "#,
            "C",
        );
    }
}

// ============================================================================
// §7.3 REDECLARATION RESTRICTIONS
// ============================================================================

/// MLS §7.3: Redeclaration normative requirements
mod redeclaration_restrictions {
    use super::*;

    /// Basic replaceable and redeclare
    #[test]
    fn mls_7_3_basic_redeclare() {
        expect_parse_success(
            r#"
            model Base
                replaceable parameter Real k = 1;
            end Base;

            model Derived
                extends Base(redeclare parameter Real k = 2);
            end Derived;
            "#,
        );
    }

    /// MLS: "A constant element shall not be redeclared"
    #[test]
    #[ignore = "Constant redeclaration restriction not yet enforced"]
    fn error_redeclare_constant() {
        expect_failure(
            r#"
            model Base
                replaceable constant Real c = 1;
            end Base;

            model Derived
                extends Base(redeclare constant Real c = 2);
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Redeclare with type change
    #[test]
    fn mls_7_3_redeclare_type() {
        expect_parse_success(
            r#"
            model Base
                replaceable Real x = 1;
            end Base;

            model Derived
                extends Base(redeclare Integer x = 2);
            end Derived;
            "#,
        );
    }

    /// Redeclare replaceable type
    #[test]
    fn mls_7_3_redeclare_replaceable_type() {
        expect_parse_success(
            r#"
            model Generic
                replaceable type ElementType = Real;
                ElementType x;
            end Generic;

            model Specific
                extends Generic(redeclare type ElementType = Integer);
            end Specific;
            "#,
        );
    }

    /// Redeclare model component
    #[test]
    fn mls_7_3_redeclare_model() {
        expect_parse_success(
            r#"
            partial model PartialSource
                Real y;
            end PartialSource;

            model ConstantSource
                extends PartialSource;
                parameter Real k = 1;
            equation
                y = k;
            end ConstantSource;

            model SineSource
                extends PartialSource;
                parameter Real amplitude = 1;
                parameter Real frequency = 1;
            equation
                y = amplitude * sin(2 * 3.14159 * frequency * time);
            end SineSource;

            model System
                replaceable ConstantSource source constrainedby PartialSource;
            end System;

            model SineSystem
                extends System(redeclare SineSource source);
            end SineSystem;
            "#,
        );
    }
}

// ============================================================================
// §7.3.1 CONSTRAINING CLAUSE
// ============================================================================

/// MLS §7.3.1: Constrainedby clause
mod constrainedby_clause {
    use super::*;

    /// Basic constrainedby
    #[test]
    fn mls_7_3_1_basic_constrainedby() {
        expect_parse_success(
            r#"
            partial model PartialResistor
                Real v;
                Real i;
            end PartialResistor;

            model LinearResistor
                extends PartialResistor;
                parameter Real R = 1;
            equation
                v = R * i;
            end LinearResistor;

            model Circuit
                replaceable LinearResistor r constrainedby PartialResistor;
            end Circuit;
            "#,
        );
    }

    /// MLS: "The redeclared element must be a subtype of the constraining type"
    #[test]
    #[ignore = "constrainedby subtype checking not yet implemented"]
    fn error_redeclare_violates_constraint() {
        expect_failure(
            r#"
            partial model PartialA
                Real x;
            end PartialA;

            partial model PartialB
                Real y;
            end PartialB;

            model ConcreteA
                extends PartialA;
            equation
                x = 1;
            end ConcreteA;

            model ConcreteB
                extends PartialB;
            equation
                y = 1;
            end ConcreteB;

            model Container
                replaceable ConcreteA component constrainedby PartialA;
            end Container;

            model BadRedeclare
                extends Container(redeclare ConcreteB component);
            end BadRedeclare;
            "#,
            "BadRedeclare",
        );
    }

    /// Constrainedby with modification
    #[test]
    fn mls_7_3_1_constrainedby_with_modification() {
        expect_parse_success(
            r#"
            partial model PartialComponent
                parameter Real k;
            end PartialComponent;

            model DefaultComponent
                extends PartialComponent;
                parameter Real extra = 0;
            equation
            end DefaultComponent;

            model Container
                replaceable DefaultComponent c(k = 1) constrainedby PartialComponent(k = 1);
            end Container;
            "#,
        );
    }
}

// ============================================================================
// §7.4 SELECTIVE MODEL EXTENSION (BREAK)
// ============================================================================

/// MLS §7.4: Selective model extension with break
mod selective_extension {
    use super::*;

    /// Basic break modifier
    #[test]
    fn mls_7_4_basic_break() {
        expect_parse_success(
            r#"
            model Base
                Real x;
            equation
                x = 1;
            end Base;

            model Modified
                extends Base(break x);
                Real x = 2;
            end Modified;
            "#,
        );
    }

    /// Break equation
    #[test]
    #[ignore = "Break equation not yet supported"]
    fn mls_7_4_break_equation() {
        expect_parse_success(
            r#"
            model Base
                Real x;
            equation
                x = 1;
            end Base;

            model Modified
                extends Base;
            break equation
                x = 1;
            equation
                x = 2;
            end Modified;
            "#,
        );
    }
}

// ============================================================================
// §7.5 SHORT CLASS DEFINITIONS
// ============================================================================

/// MLS §7.5: Short class definitions
mod short_class_definitions {
    use super::*;

    /// Basic short class definition (type alias)
    #[test]
    fn mls_7_5_type_alias() {
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

    /// Short class with array
    #[test]
    #[ignore = "Short class with array type not yet supported"]
    fn mls_7_5_array_type() {
        expect_success(
            r#"
            type Vector3 = Real[3];

            model Test
                Vector3 v = {1, 2, 3};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Short connector definition
    #[test]
    fn mls_7_5_short_connector() {
        expect_success(
            r#"
            connector RealInput = input Real;
            connector RealOutput = output Real;

            model Test
                RealInput u;
                RealOutput y;
            equation
                y = u;
            end Test;
            "#,
            "Test",
        );
    }

    /// Short class with modification
    #[test]
    fn mls_7_5_short_with_modification() {
        expect_success(
            r#"
            type PositiveReal = Real(min = 0);

            model Test
                PositiveReal x = 5;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Enumeration type
    #[test]
    fn mls_7_5_enumeration() {
        expect_parse_success(
            r#"
            type StateSelect = enumeration(
                never,
                avoid,
                default,
                prefer,
                always
            );

            model Test
                Real x(stateSelect = StateSelect.prefer);
            equation
                der(x) = 1;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// COMPLEX INHERITANCE SCENARIOS
// ============================================================================

/// Complex inheritance scenarios
mod complex_scenarios {
    use super::*;

    /// Multi-level inheritance with modifications at each level
    #[test]
    fn complex_multilevel_modification() {
        expect_success(
            r#"
            model Level1
                parameter Real a = 1;
            equation
            end Level1;

            model Level2
                extends Level1(a = 2);
                parameter Real b = a;
            equation
            end Level2;

            model Level3
                extends Level2(a = 3, b = 4);
                parameter Real c = a + b;
            equation
            end Level3;
            "#,
            "Level3",
        );
    }

    /// Inheritance with component arrays
    #[test]
    fn complex_array_component_inheritance() {
        expect_success(
            r#"
            model Base
                parameter Integer n = 3;
                Real x[n](each start = 0);
            equation
                for i in 1:n loop
                    der(x[i]) = i;
                end for;
            end Base;

            model Derived
                extends Base(n = 5, x(each start = 1));
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Diamond inheritance with modifications
    #[test]
    fn complex_diamond_with_modifications() {
        expect_success(
            r#"
            model A
                parameter Real p = 1;
            equation
            end A;

            model B
                extends A(p = 2);
            equation
            end B;

            model C
                extends A(p = 3);
            equation
            end C;

            model D
                extends B;
                extends C;
            equation
            end D;
            "#,
            "D",
        );
    }

    /// Nested replaceable components
    #[test]
    #[ignore = "Nested replaceable component parsing not yet supported"]
    fn complex_nested_replaceable() {
        expect_parse_success(
            r#"
            partial model PartialInner
                Real x;
            end PartialInner;

            model DefaultInner
                extends PartialInner;
            equation
                x = 1;
            end DefaultInner;

            model Outer
                replaceable DefaultInner inner constrainedby PartialInner;
            end Outer;

            model Container
                replaceable Outer outer;
            end Container;
            "#,
        );
    }
}
