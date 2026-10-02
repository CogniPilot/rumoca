//! INST (Instantiation) contract tests for lookup, inner/outer, final,
//! `break`, `each`, and per-occurrence class selection - MLS §5, §7
//!
//! The remaining instantiation contracts of SPEC_0022 are in
//! `inst_contracts.rs` and `inst_extends_contracts.rs`.

use rumoca_compile::compile::FailedPhase;
use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_parse_err_with_code,
    expect_resolve_failure_with_code, expect_success,
};

// =============================================================================
// INST-013: Constant-only references
// "Enclosing class variables accessible only if declared constant"
// (MLS §5.3.1)
// =============================================================================

#[test]
fn inst_013_enclosing_package_constant_accessible() {
    expect_success(
        r#"
        package P
            constant Real c = 2.5;
            model Inner
                Real x(start = 0, fixed = true);
            equation
                der(x) = c;
            end Inner;
        end P;
        model Test
            P.Inner inner_model;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_013_enclosing_non_constant_rejected() {
    expect_resolve_failure_with_code(
        r#"
        package P
            model Holder
                Real shared(start = 0);
                model Inner
                    Real x(start = 0, fixed = true);
                equation
                    der(x) = shared;
                end Inner;
                Inner inner_model;
            equation
                der(shared) = 1;
            end Holder;
        end P;
        model Test
            P.Holder h;
        end Test;
    "#,
        "Test",
        "ER130",
    );
}

#[test]
fn inst_013_enclosing_parameter_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            parameter Boolean enabled = true;
            model Inner
                Real x if enabled;
            end Inner;
            Inner inner_model;
        end M;
    "#,
        "M",
        "ER130",
    );
}

#[test]
fn inst_013_short_class_modifier_uses_enclosing_instance_scope() {
    expect_balanced(
        r#"
        model Resistor
            parameter Real R;
            Real v;
        equation
            v = R;
        end Resistor;

        model Test
            parameter Real R = 2;
            replaceable model Load = Resistor(R = R);
            Load load;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-018: Outer same class
// "Two outer declarations with same name but different classes is error"
// =============================================================================

#[test]
fn inst_018_outer_type_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model A
            Real v;
        end A;
        model B
            Real w;
        end B;
        model C
            outer A shared;
        end C;
        model D
            outer B shared;
        end D;
        model Test
            inner A shared;
            C c;
            D d;
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI009",
    );
}

// =============================================================================
// INST-019: Outer partial error
// "Outer of partial class is error"
// =============================================================================

#[test]
fn inst_019_inner_of_partial_class_rejected() {
    expect_resolve_failure_with_code(
        r#"
        partial model P
            Real v;
        end P;
        model C
            outer P shared;
        end C;
        model Test
            inner P shared;
            C c;
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        "ER005",
    );
}

// =============================================================================
// INST-021: Lookup non-partial
// "Class looked inside shall not be partial in simulation model"
// =============================================================================

#[test]
fn inst_021_partial_component_instantiation_rejected() {
    expect_resolve_failure_with_code(
        r#"
        partial model P
            Real v;
        end P;
        package Pkg
            model Use
                P p;
            end Use;
        end Pkg;
        model Test
            Pkg.Use u;
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        "ER005",
    );
}

// =============================================================================
// INST-023: Protection not changed
// "Protected element cannot be redeclared as public, or public as protected"
// (grammar-level: `public` is not a valid redeclare prefix)
// =============================================================================

#[test]
fn inst_023_redeclare_to_public_rejected() {
    expect_parse_err_with_code(
        r#"
        model A
        protected
            Real v = 1;
        end A;
        model Test
            extends A(redeclare public Real v = 2);
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// INST-033: Global scope error
// "If there does not exist a class A in global scope this is an error"
// =============================================================================

#[test]
fn inst_033_unknown_global_class_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            NoSuchClass n;
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        "ER002",
    );
}

// =============================================================================
// INST-030: Error if value for entire non-simple component overrides a final prefix
// =============================================================================

#[test]
fn inst_030_final_parameter_modification_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            model Base
                final parameter Real p = 1.0;
            end Base;
            Base b(p = 2.0);
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI028",
    );
}

// =============================================================================
// INST-009: All elements of a final element are also final
// =============================================================================

#[test]
fn inst_009_member_of_final_component_modification_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            model Sub
                parameter Real p = 1;
            end Sub;
            model Base
                final Sub s;
            end Base;
            Base b(s(p = 3));
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI028",
    );
}

// =============================================================================
// INST-017: Redeclaration shall not include a condition attribute
// =============================================================================

#[test]
fn inst_017_redeclare_with_condition_rejected() {
    expect_parse_err_with_code(
        r#"
        model M
            parameter Boolean has = true;
            model Base
                replaceable Real x;
            end Base;
            Base b(redeclare Real x if has);
        end M;
    "#,
        "EP001",
    );
}

// =============================================================================
// INST-020: Inner/outer component shall not have top-level public connectors containing inputs
// =============================================================================

#[test]
fn inst_020_inner_connector_with_input_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            connector C
                input Real u;
            end C;
            inner C c;
        end M;
    "#,
        "M",
        "ER095",
    );
}

// =============================================================================
// INST-024: Only specialized classes in some sense compatible can inherit from each other
// =============================================================================

#[test]
fn inst_024_function_extends_connector_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            connector C
                Real e;
                flow Real f;
            end C;
            function F
                extends C;
            end F;
            Real x = 1;
        end M;
    "#,
        "M",
        "ER091",
    );
}

// =============================================================================
// INST-040: Each keyword on modifier requires it is applied in array
// declaration/modification
// =============================================================================

#[test]
fn inst_040_each_on_scalar_component_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            model Sub
                parameter Real p = 1;
            end Sub;
            Sub s(each p = 2);
        end M;
    "#,
        "M",
        "ER104",
    );
}

// =============================================================================
// INST-041: Sizes must match without each prefix or it is an error
// =============================================================================

#[test]
fn inst_041_modification_size_mismatch_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            model Sub
                parameter Real p = 1;
            end Sub;
            Sub s[3](p = {1.0, 2.0});
        end M;
    "#,
        "M",
        "ER105",
    );
}

// =============================================================================
// INST-047: Matched components for deselection must be models, blocks, or
// connectors
// =============================================================================

#[test]
fn inst_047_break_on_parameter_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            model Base
                parameter Real p = 1;
                Real x = p;
            end Base;
            model Cut
                extends Base(break p);
            end Cut;
            Cut c;
        end M;
    "#,
        "M",
        "ER106",
    );
}

// =============================================================================
// INST-049: Deselected component may be of partial class even in simulation
// model
// =============================================================================

#[test]
fn inst_049_break_partial_component_accepted() {
    expect_success(
        r#"
        model M
            partial model Inner
                Real x;
            end Inner;
            partial model Base
                Inner sub;
                Real y = 1;
            end Base;
            model Cut
                extends Base(break sub);
            end Cut;
            Cut c;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// INST-025: Equations syntactically equivalent to equations in enclosing class
// are discarded
// =============================================================================

#[test]
fn inst_025_diamond_inherited_equations_deduplicated() {
    expect_balanced(
        r#"
        model M
            model Base
                Real x;
            equation
                x = time;
            end Base;
            model Mid1
                extends Base;
            end Mid1;
            model Mid2
                extends Base;
            end Mid2;
            model Leaf
                extends Mid1;
                extends Mid2;
            end Leaf;
            Leaf leaf;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// INST-050: Error if multiple unqualified imports match same name
// =============================================================================

#[test]
fn inst_050_ambiguous_unqualified_import_rejected() {
    expect_resolve_failure_with_code(
        r#"
        package Top
            package A
                constant Real k = 1;
            end A;
            package B
                constant Real k = 2;
            end B;
            model M
                import Top.A.*;
                import Top.B.*;
                Real x = k;
            end M;
        end Top;
    "#,
        "Top.M",
        "ER112",
    );
}

// =============================================================================
// INST-004: Preserve declaration order in inheritance
// =============================================================================

#[test]
fn inst_004_inherited_declaration_order_preserved() {
    let result = expect_success(
        r#"
        model M
            model Base
                Real a = 1;
                Real b = 2;
            end Base;
            model Derived
                extends Base;
                Real c = 3;
            end Derived;
            Derived d;
        end M;
    "#,
        "M",
    );
    let names: Vec<&str> = result
        .flat
        .variables
        .keys()
        .map(|name| name.as_str())
        .collect();
    let pos = |needle: &str| {
        names
            .iter()
            .position(|n| *n == needle)
            .unwrap_or_else(|| panic!("{needle} missing from {names:?}"))
    };
    assert!(
        pos("d.a") < pos("d.b") && pos("d.b") < pos("d.c"),
        "inherited declaration order must be preserved: {names:?}"
    );
}

// =============================================================================
// INST-035: Names of record classes and enumeration types are ignored during
// function name lookup
// =============================================================================

#[test]
fn inst_035_record_constructor_and_type_lookup_coexist() {
    expect_success(
        r#"
        model M
            record R
                Real x;
            end R;
            R r = R(1.0);
            Real y = r.x;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// INST-036: Implicitly defined names of record constructors and enum
// conversions ignored during type lookup
// =============================================================================

#[test]
fn inst_036_enum_conversion_and_type_lookup_coexist() {
    expect_success(
        r#"
        model M
            type E = enumeration(a, b);
            E e = E(2);
            Boolean is_b = e == E.b;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// INST-048: Conditionally declared components of B assumed declared for
// matching purposes
// =============================================================================

#[test]
fn inst_048_break_matches_conditional_component() {
    expect_success(
        r#"
        model M
            partial model Inner
                Real x;
            end Inner;
            partial model Base
                parameter Boolean has = true;
                Inner sub if has;
                Real y = 1;
            end Base;
            model Cut
                extends Base(break sub);
            end Cut;
            Cut c;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// INST-026: Original class B should be replaceable in class extends
// =============================================================================

#[test]
fn inst_026_class_extends_of_non_replaceable_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            model Base
                model Item
                    Real x = 1;
                end Item;
                Item item;
            end Base;
            model Derived
                extends Base;
                redeclare model extends Item
                    Real y = 2;
                end Item;
            end Derived;
            Derived d;
        end M;
    "#,
        "M",
        "ER123",
    );
}

// =============================================================================
// INST-028: Deselection break D is applied before any other non-selective
// modifications
// =============================================================================

#[test]
fn inst_028_break_composes_with_sibling_modifications() {
    expect_success(
        r#"
        model M
            partial model Inner
                Real x;
            end Inner;
            partial model Base
                Inner sub;
                parameter Real p = 1;
                Real y = p;
            end Base;
            model Cut
                extends Base(break sub, p = 2);
            end Cut;
            Cut c;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// INST-042: Remaining break modifications treated as if expression was missing
// =============================================================================

#[test]
fn inst_042_break_modification_removes_binding() {
    expect_success(
        r#"
        model M
            model Base
                parameter Real p = 1;
                Real x = p;
            end Base;
            model Cut
                extends Base(p = break);
            end Cut;
            Cut c(p = 3);
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// INST-031: Outer class declarations should be defined using short-class
// definitions without modifications
// =============================================================================

#[test]
fn inst_031_outer_class_short_definition_accepted() {
    expect_success(
        r#"
        model M
            model Inner2
                Real x = 1;
            end Inner2;
            model Sub
                outer model T = Inner2;
                T t;
            end Sub;
            inner model T = Inner2;
            Sub s;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// INST-032: If outer component declaration is disabled conditional, also
// ignored for automatic inner creation
// =============================================================================

#[test]
fn inst_032_disabled_conditional_outer_needs_no_inner() {
    expect_success(
        r#"
        model M
            connector C
                Real e;
                flow Real f;
            end C;
            model Sub
                parameter Boolean has = false;
                outer C shared if has;
                Real y = 1;
            end Sub;
            Sub s;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// INST-038: Modifications of elements declared with both inner and outer only
// applied to inner declaration
// =============================================================================

#[test]
fn inst_038_inner_outer_modification_accepted() {
    expect_success(
        r#"
        model M
            model Sub
                inner outer Real shared = 5;
                Real y = shared;
            end Sub;
            Sub s;
            inner Real shared = 1;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// INST-057 / INST-058: per-occurrence class selection through replaceable aliases
// =============================================================================

const INST_057_058_MODEL: &str = r#"
package Media
  partial package PartialMedium
    replaceable partial model BaseProperties
      Real T;
    end BaseProperties;
  end PartialMedium;
  package One
    extends PartialMedium;
    redeclare model extends BaseProperties
    equation
      T = 1;
    end BaseProperties;
  end One;
  package Two
    extends PartialMedium;
    redeclare model extends BaseProperties
    equation
      T = 2;
    end BaseProperties;
  end Two;
end Media;

model Tank
  replaceable package Medium = Media.One constrainedby Media.PartialMedium;
  Medium.BaseProperties medium;
end Tank;

model Sensor
  Real u;
end Sensor;

model Pair
  replaceable package MediumA = Media.One constrainedby Media.PartialMedium;
  Tank a(redeclare package Medium = MediumA);
  Tank b;
end Pair;

model Plant
  Pair pair(redeclare package MediumA = Media.Two);
  Sensor sensorA(u = pair.a.medium.T);
  Real tB;
equation
  tB = pair.b.medium.T;
end Plant;
"#;

#[test]
fn inst_057_member_tail_follows_each_occurrence() {
    let trace = rumoca_contracts::test_support::simulate_model(INST_057_058_MODEL, "Plant", 0.1);
    assert_eq!(trace.final_value("sensorA.u"), 2.0);
    assert_eq!(trace.final_value("tB"), 1.0);
}

#[test]
fn inst_058_redeclare_value_alias_selects_the_outer_redeclaration() {
    let trace = rumoca_contracts::test_support::simulate_model(INST_057_058_MODEL, "Plant", 0.1);
    assert_eq!(trace.final_value("pair.a.medium.T"), 2.0);
    assert_eq!(trace.final_value("pair.b.medium.T"), 1.0);
}
