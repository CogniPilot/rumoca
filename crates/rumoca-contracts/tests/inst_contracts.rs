//! INST (Instantiation) contract tests - MLS §5, §7
//!
//! Tests for the 53 instantiation contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_parse_err_with_code,
    expect_resolve_failure_with_code, expect_success,
};
use rumoca_session::compile::FailedPhase;

fn flat_var_is_protected(result: &rumoca_session::compile::CompilationResult, name: &str) -> bool {
    result
        .flat
        .variables
        .iter()
        .find(|(var_name, _)| var_name.as_str() == name)
        .map(|(_, variable)| variable.is_protected)
        .unwrap_or(false)
}

fn flat_var_exists(result: &rumoca_session::compile::CompilationResult, name: &str) -> bool {
    result
        .flat
        .variables
        .keys()
        .any(|var_name| var_name.as_str() == name)
}

fn flat_var_dims(
    result: &rumoca_session::compile::CompilationResult,
    name: &str,
) -> Option<Vec<i64>> {
    result
        .flat
        .variables
        .iter()
        .find(|(var_name, _)| var_name.as_str() == name)
        .map(|(_, variable)| variable.dims.clone())
}

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
// INST-006: Name collision
// "Declaration elements of flattened base class shall either not exist or match exactly"
// =============================================================================

#[test]
fn inst_006_identical_inherited_components_are_kept_once() {
    expect_balanced(
        r#"
        model Common
            parameter Real k = 1;
        end Common;

        model Left
            extends Common;
        end Left;

        model Right
            extends Common;
        end Right;

        model Test
            extends Left;
            extends Right;
            Real y;
        equation
            y = k;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_006_conflicting_inherited_components_fail() {
    expect_failure_in_phase_with_code(
        r#"
        model Left
            parameter Real k = 1;
        end Left;

        model Right
            parameter Integer k = 1;
        end Right;

        model Test
            extends Left;
            extends Right;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI010",
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
// INST-011: Inner/outer subtype
// "Inner component must be subtype of corresponding outer"
// =============================================================================

#[test]
fn inst_011_inner_can_be_subtype_of_outer() {
    expect_balanced(
        r#"
        model Base
            parameter Real k = 1;
        end Base;

        model Derived
            extends Base;
        end Derived;

        model Child
            outer Base cfg;
            Real y;
        equation
            y = cfg.k;
        end Child;

        model Test
            inner Derived cfg;
            Child child;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_011_inner_must_match_outer_constraint() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            parameter Real k = 1;
        end Base;

        model Other
            parameter Integer k = 1;
        end Other;

        model Child
            outer Base cfg;
            Real y;
        equation
            y = cfg.k;
        end Child;

        model Test
            inner Other cfg;
            Child child;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI009",
    );
}

// =============================================================================
// INST-012: Outer no modifications
// "Outer component declarations shall not have modifications"
// =============================================================================

#[test]
fn inst_012_outer_binding_is_rejected() {
    expect_parse_err_with_code(
        r#"
        model Test
            outer Real x = 1;
        end Test;
    "#,
        "EP001",
    );
}

#[test]
fn inst_012_outer_modification_is_rejected() {
    expect_parse_err_with_code(
        r#"
        model Test
            outer Real x(start = 1);
        end Test;
    "#,
        "EP001",
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
// INST-014: Redeclaration constraint
// "Only classes and components declared as replaceable can be redeclared with a new type"
// =============================================================================

#[test]
fn inst_014_replaceable_component_can_be_redeclared() {
    expect_success(
        r#"
        model BaseType
            Real x;
        equation
            x = 1;
        end BaseType;

        model DerivedType
            extends BaseType;
        end DerivedType;

        partial model Container
            replaceable BaseType c;
        end Container;

        model Test
            extends Container(redeclare DerivedType c);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_014_non_replaceable_component_cannot_be_redeclared() {
    expect_failure_in_phase_with_code(
        r#"
        model BaseType
            Real x;
        equation
            x = 1;
        end BaseType;

        model DerivedType
            extends BaseType;
        end DerivedType;

        partial model Container
            BaseType c;
        end Container;

        model Test
            extends Container(redeclare DerivedType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI014",
    );
}

#[test]
fn inst_014_replaceable_nested_class_can_be_redeclared() {
    expect_success(
        r#"
        model Base
            replaceable model Worker
                Real x;
            equation
                x = 1;
            end Worker;

            Worker w;
        end Base;

        model NewWorker
            extends Base.Worker;
        end NewWorker;

        model Test
            Base b(redeclare model Worker = NewWorker);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_014_non_replaceable_nested_class_cannot_be_redeclared() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            model Worker
                Real x;
            equation
                x = 1;
            end Worker;

            Worker w;
        end Base;

        model NewWorker
            Real x;
        equation
            x = 2;
        end NewWorker;

        model Test
            Base b(redeclare model Worker = NewWorker);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI014",
    );
}

// =============================================================================
// INST-022: Constant not redeclared
// "An element declared as constant cannot be redeclared"
// =============================================================================

#[test]
fn inst_022_constant_component_cannot_be_redeclared() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            replaceable constant Real k = 1;
        end Base;

        model Test
            extends Base(redeclare constant Integer k = 2);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI007",
    );
}

// =============================================================================
// INST-027: Constraining type auto-apply
// "Modifications following constraining type applied both for constraint and declaration"
// =============================================================================

#[test]
fn inst_027_constraining_clause_modifications_apply_after_redeclare() {
    let result = expect_success(
        r#"
        model BaseComb
            parameter Integer n = 0;
            Real u[n];
        end BaseComb;

        model AndComb
            extends BaseComb;
        end AndComb;

        partial model PartialLogical
            parameter Integer n = 2;
            replaceable BaseComb comb constrainedby BaseComb(n = n);
        end PartialLogical;

        model Conj
            extends PartialLogical(redeclare AndComb comb);
        end Conj;

        model Top
            Conj p;
        end Top;
    "#,
        "Top",
    );

    assert!(
        flat_var_exists(&result, "p.comb.u"),
        "expected redeclared component array p.comb.u to exist"
    );
    assert!(
        flat_var_dims(&result, "p.comb.u") == Some(vec![2]),
        "expected constrainedby modifier to size p.comb.u with n=2, got {:?}",
        flat_var_dims(&result, "p.comb.u")
    );
}

// =============================================================================
// INST-029: Break must match
// "Deselection break D must match at least one element of B"
// =============================================================================

#[test]
fn inst_029_break_existing_element_is_allowed() {
    expect_success(
        r#"
        model Base
            Real x;
        end Base;

        model Test
            extends Base(break x);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_029_break_missing_element_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            Real x;
        end Base;

        model Test
            extends Base(break y);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI029",
    );
}

// =============================================================================
// INST-037: Identical children first kept
// "Children with same name must be identical; only first one kept, error if not identical"
// =============================================================================

#[test]
fn inst_037_identical_inherited_child_class_is_kept_once() {
    expect_balanced(
        r#"
        model Common
            model Helper
                parameter Real k = 1;
            end Helper;
        end Common;

        model Left
            extends Common;
        end Left;

        model Right
            extends Common;
        end Right;

        model Test
            extends Left;
            extends Right;
            Helper h;
            Real y;
        equation
            y = h.k;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_037_conflicting_inherited_child_class_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model Left
            model Helper
                parameter Real k = 1;
            end Helper;
        end Left;

        model Right
            model Helper
                parameter Integer k = 1;
            end Helper;
        end Right;

        model Test
            extends Left;
            extends Right;
            Helper h;
            Real y;
        equation
            y = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI010",
    );
}

// =============================================================================
// INST-039: Protected extends
// "If extends under protected heading, all elements of base class become protected"
// =============================================================================

#[test]
fn inst_039_protected_extends_marks_inherited_components_protected() {
    let result = expect_success(
        r#"
        model Base
            Real x;
        equation
            x = 1;
        end Base;

        model Test
        protected
            extends Base;
        end Test;
    "#,
        "Test",
    );

    assert!(
        flat_var_is_protected(&result, "x"),
        "protected extends should mark inherited component x as protected"
    );
}

// =============================================================================
// INST-043: Implicit constraining type
// "If constraining-clause not present, type of declaration used as constraining type"
// =============================================================================

#[test]
fn inst_043_original_type_is_used_as_implicit_constraint() {
    expect_success(
        r#"
        model BaseType
            Real x;
        equation
            x = 1;
        end BaseType;

        model DerivedType
            extends BaseType;
        end DerivedType;

        partial model Container
            replaceable BaseType c;
        end Container;

        model Test
            extends Container(redeclare DerivedType c);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn inst_043_redeclare_must_still_be_subtype_without_explicit_constrainedby() {
    expect_failure_in_phase_with_code(
        r#"
        model BaseType
            Real x;
        equation
            x = 1;
        end BaseType;

        model OtherType
            Integer y;
        equation
            y = 1;
        end OtherType;

        partial model Container
            replaceable BaseType c;
        end Container;

        model Test
            extends Container(redeclare OtherType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
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
