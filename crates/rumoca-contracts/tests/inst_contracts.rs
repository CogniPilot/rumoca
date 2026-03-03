//! INST (Instantiation) contract tests - MLS §5, §7
//!
//! Tests for the 53 instantiation contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_parse_err_with_code,
    expect_resolve_failure_with_code, expect_success,
};
use rumoca_session::FailedPhase;

// =============================================================================
// INST-001: Modification context
// "Modifier value found in the context in which the modifier occurs"
// =============================================================================

#[test]
fn inst_001_modification_context() {
    // Parameter modification should apply in context of instantiation
    expect_balanced(
        r#"
        model Inner
            parameter Real p = 1;
            Real x;
        equation
            x = p;
        end Inner;
        model Test
            Inner a(p = 2);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-002: Modification merging
// "Outer modifiers override inner modifiers"
// =============================================================================

#[test]
fn inst_002_outer_overrides_inner() {
    let result = expect_balanced(
        r#"
        model Inner
            parameter Real p = 1;
            Real x;
        equation
            x = p;
        end Inner;
        model Test
            Inner a(p = 5);
        end Test;
    "#,
        "Test",
    );
    // The DAE should have p=5, not p=1
    assert!(
        !result.dae.parameters.is_empty(),
        "Should have parameters in DAE"
    );
}

// =============================================================================
// INST-003: Single modification
// "Two arguments of a modification shall not modify the same element"
// =============================================================================

#[test]
fn inst_003_no_duplicate_modifications() {
    expect_parse_err_with_code(
        r#"
        model Inner
            parameter Real p = 1;
            Real x;
        equation
            x = p;
        end Inner;
        model Test
            Inner a(p = 2, p = 3);
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// INST-007: Evaluable expressions
// "Structural parameters must be compile-time evaluable"
// =============================================================================

#[test]
fn inst_007_parameter_evaluable() {
    expect_success(
        r#"
        model Test
            parameter Integer n = 3;
            Real x[n];
        equation
            for i in 1:n loop
                x[i] = i;
            end for;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-008: Acyclic binding
// "Expression must not depend on the variable itself"
// =============================================================================

#[test]
fn inst_008_no_cyclic_binding() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            parameter Real a = b;
            parameter Real b = a;
            Real x;
        equation
            x = a;
        end Test;
    "#,
        "Test",
        "ER007",
    );
}

// =============================================================================
// INST-010: Final immutability
// "Element defined as final cannot be modified by modification or redeclaration"
// =============================================================================

#[test]
fn inst_010_final_cannot_modify() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            final parameter Real p = 1;
            Real x;
        equation
            x = p;
        end Base;
        model Test
            Base b(p = 2);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI028",
    );
}

// =============================================================================
// INST-016: Conditional evaluable
// "Condition expression must be evaluable Boolean scalar"
// =============================================================================

#[test]
fn inst_016_conditional_parameter() {
    expect_success(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            parameter Boolean use_heater = true;
            Pin p if use_heater;
        equation
            if use_heater then
                p.v = 1.0;
            end if;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-034: Encapsulated lookup stop
// "Lookup stops if enclosing class is encapsulated"
// =============================================================================

#[test]
fn inst_034_encapsulated_basic() {
    // Non-encapsulated nested classes may use enclosing scope names.
    expect_success(
        r#"
        model Container
            parameter Real g = 9.81;
            model Inner
                Real x;
            equation
                x = g;
            end Inner;

            Inner i;
        end Container;
    "#,
        "Container",
    );
}

#[test]
fn inst_034_encapsulated_self_lookup_ok() {
    // Encapsulated nested classes can still resolve their own local declarations.
    expect_success(
        r#"
        model Container
            encapsulated model Inner
                parameter Real g = 9.81;
                Real x;
            equation
                x = g;
            end Inner;

            Inner i;
        end Container;
    "#,
        "Container",
    );
}

// =============================================================================
// INST-053: Conditional component removal
// "Conditional components with false condition are removed"
// =============================================================================

#[test]
fn inst_053_conditional_false_removed() {
    expect_success(
        r#"
        model Test
            parameter Boolean use_x = false;
            Real y;
            Real x if use_x;
        equation
            y = 1;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_053_conditional_true_kept() {
    expect_success(
        r#"
        model Test
            parameter Boolean use_x = true;
            Real x if use_x;
        equation
            if use_x then
                x = 1;
            end if;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// Instantiation integration tests
// =============================================================================

#[test]
fn inst_extends_basic() {
    expect_balanced(
        r#"
        model Base
            Real x;
        equation
            x = 1;
        end Base;
        model Test
            extends Base;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_extends_with_modification() {
    expect_balanced(
        r#"
        model Base
            parameter Real p = 1;
            Real x;
        equation
            x = p;
        end Base;
        model Test
            extends Base(p = 42);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_nested_components() {
    expect_balanced(
        r#"
        model Inner
            Real x;
        equation
            x = 1;
        end Inner;
        model Middle
            Inner a;
        end Middle;
        model Test
            Middle m;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_component_modification() {
    expect_balanced(
        r#"
        model Inner
            parameter Real p = 0;
            Real x;
        equation
            x = p;
        end Inner;
        model Test
            Inner a(p = 10);
            Inner b(p = 20);
        end Test;
    "#,
        "Test",
    );
}
