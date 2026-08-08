//! INST (Instantiation) contract tests - MLS §5, §7
//!
//! Tests for the 53 instantiation contracts defined in SPEC_0022.

use rumoca_compile::compile::FailedPhase;
use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_parse_err_with_code,
    expect_resolve_failure_with_code, expect_success,
};

fn flat_var_is_protected(result: &rumoca_compile::compile::CompilationResult, name: &str) -> bool {
    result
        .flat
        .variables
        .iter()
        .find(|(var_name, _)| var_name.as_str() == name)
        .map(|(_, variable)| variable.is_protected)
        .unwrap_or(false)
}

fn flat_var_exists(result: &rumoca_compile::compile::CompilationResult, name: &str) -> bool {
    result
        .flat
        .variables
        .keys()
        .any(|var_name| var_name.as_str() == name)
}

/// Extent of a scalarized component array, counted from the flat variables.
///
/// A component array is flattened per element (`a[1].x`, `a[2].x`, …) rather
/// than kept as one variable with `dims`, so its extent is the number of
/// consecutive elements that carry `member`. Counting from 1 upwards also
/// proves the extent is not *larger* than expected, which is what the
/// dimension-replacing redeclarations below need to pin.
fn redeclared_array_extent(
    result: &rumoca_compile::compile::CompilationResult,
    array: &str,
    member: &str,
) -> usize {
    (1..)
        .take_while(|index| flat_var_exists(result, &format!("{array}[{index}].{member}")))
        .count()
}

fn flat_var_dims(
    result: &rumoca_compile::compile::CompilationResult,
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
        result.dae.inspect(|view| {
            view.variables().any(|(_, variable)| {
                variable.role() == rumoca_compile::compile::VariableRole::Parameter
            })
        }),
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

// -----------------------------------------------------------------------------
// MLS §7.3 / §A.2.5: an element-redeclaration is a whole component declaration
// (`component-clause1` -> `declaration` -> `IDENT [ array-subscripts ]`), so the
// subscripts it writes replace the replaced declaration's array dimensions, and
// a redeclaration that writes none leaves them standing. Each case below was
// checked against OpenModelica on the same source.
// -----------------------------------------------------------------------------

/// Shared fixture: `Holder` declares `a` with `holder_dims`, `Test` redeclares it.
fn redeclared_dims_source(holder_dims: &str, redeclare: &str, extra_decls: &str) -> String {
    format!(
        r#"
        model BaseType
            Real x;
        equation
            x = 1;
        end BaseType;

        model DerivedType
            extends BaseType;
            Real y;
        equation
            y = 2;
        end DerivedType;

        partial model Holder
            replaceable BaseType a{holder_dims} constrainedby BaseType;
        end Holder;

        model Test
            {extra_decls}
            extends Holder({redeclare});
        end Test;
    "#
    )
}

#[test]
fn redeclare_dimensions_replace_a_scalar_declaration() {
    // `replaceable BaseType a;` redeclared as `DerivedType a[3]`.
    // OMC on the same source: a[1..3], each with x and y.
    let result = expect_success(
        &redeclared_dims_source("", "redeclare DerivedType a[3]", ""),
        "Test",
    );
    assert_eq!(
        redeclared_array_extent(&result, "a", "x"),
        3,
        "a rank-raising redeclaration must reshape the component"
    );
    assert!(
        flat_var_exists(&result, "a[1].y"),
        "the redeclared type's own members must be instantiated"
    );
}

#[test]
fn redeclare_dimensions_replace_a_declared_extent() {
    // `replaceable BaseType a[2];` redeclared as `DerivedType a[4]`.
    // OMC: a[1..4]. The replaced extent must not survive.
    let result = expect_success(
        &redeclared_dims_source("[2]", "redeclare DerivedType a[4]", ""),
        "Test",
    );
    assert_eq!(
        redeclared_array_extent(&result, "a", "x"),
        4,
        "the redeclaration's extent must replace the declared one"
    );
}

#[test]
fn redeclare_dimensions_replace_a_declared_rank() {
    // `replaceable BaseType a[2,2];` redeclared as `DerivedType a[4]`.
    // OMC: a[1..4] — the rank drops from 2 to 1, and that is not an error.
    let result = expect_success(
        &redeclared_dims_source("[2,2]", "redeclare DerivedType a[4]", ""),
        "Test",
    );
    assert_eq!(
        redeclared_array_extent(&result, "a", "x"),
        4,
        "the redeclaration's rank must replace the declared one"
    );
}

#[test]
fn redeclare_without_dimensions_keeps_the_declared_dimensions() {
    // Ablation guard for the three cases above: propagating a redeclaration's
    // dimensions must not be read as "a redeclaration always clears them".
    // `replaceable BaseType a[3];` redeclared as `DerivedType a` (no
    // subscripts) stays a[1..3] — OMC agrees.
    let result = expect_success(
        &redeclared_dims_source("[3]", "redeclare DerivedType a", ""),
        "Test",
    );
    assert_eq!(
        redeclared_array_extent(&result, "a", "x"),
        3,
        "a redeclaration that states no dimensions must keep the declared ones"
    );
}

#[test]
fn redeclare_dimension_accepts_boolean_as_an_extent() {
    // MLS §10.5: the type name `Boolean` is a dimension of extent 2. A
    // redeclaration must read it exactly as a declaration does — OMC flattens
    // both to `a[false], a[true]`.
    //
    // This is the case a private copy of the literal-dimension helper got
    // wrong: without the `Boolean` arm the subscript decided nothing, the
    // component collapsed to a scalar, and the model compiled clean with no
    // diagnostic at all.
    let result = expect_success(
        &redeclared_dims_source("[3]", "redeclare DerivedType a[Boolean]", ""),
        "Test",
    );
    assert_eq!(
        redeclared_array_extent(&result, "a", "x"),
        2,
        "`Boolean` as a redeclared dimension must give extent 2, not a scalar"
    );
}

#[test]
fn declared_dimension_accepts_boolean_as_an_extent() {
    // Control for the case above: the declaration path reads `Boolean` as
    // extent 2 both before and after this change, which is what makes a
    // redeclaration that disagrees with it a defect rather than a policy.
    let result = expect_success(
        r#"
        model BaseType
            Real x;
        equation
            x = 1;
        end BaseType;

        model Test
            BaseType a[Boolean];
        end Test;
    "#,
        "Test",
    );
    assert_eq!(
        redeclared_array_extent(&result, "a", "x"),
        2,
        "`Boolean` as a declared dimension must give extent 2"
    );
}

#[test]
fn redeclare_dimension_expression_resolves_where_it_is_written() {
    // The redeclaration's dimension expression is evaluated in the class that
    // writes the redeclaration, not in the replaced declaration's own scope
    // (OMC: `Holder h(n = 5, redeclare B a[k])` with a local `k = 2` yields
    // a[1..2] while `h.n` stays 5).
    let result = expect_success(
        &redeclared_dims_source(
            "[2]",
            "redeclare DerivedType a[k]",
            "parameter Integer k = 3;",
        ),
        "Test",
    );
    assert_eq!(
        redeclared_array_extent(&result, "a", "x"),
        3,
        "a parameter-expression dimension must resolve against the redeclaring class"
    );
}

#[test]
fn redeclare_dimensions_apply_to_connector_arrays() {
    // The shape that used to reach typecheck as `has 0 dimension(s)`: a
    // connector array whose extent only the redeclaration states.
    // OMC: pin[1..3].
    let result = expect_success(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;

        connector PinAlt
            extends Pin;
        end PinAlt;

        partial model Plug
            replaceable Pin pin[2] constrainedby Pin;
        end Plug;

        model Test
            extends Plug(redeclare PinAlt pin[3]);
        equation
            for k in 1:3 loop
                pin[k].v = 0;
            end for;
        end Test;
    "#,
        "Test",
    );
    assert_eq!(
        redeclared_array_extent(&result, "pin", "v"),
        3,
        "a redeclared connector array must carry the redeclaration's extent"
    );
}

#[test]
fn redeclare_with_dimensions_still_requires_a_replaceable_element() {
    // MLS §7.3.3 still rejects a redeclaration of a non-replaceable element
    // when that redeclaration also restates dimensions.
    //
    // Dimension propagation cannot weaken this by itself — the replaceable /
    // final / constant / constrainedby check is dimension-blind, and
    // `collect_redeclarations` propagates its error with `?` before any
    // collected shape is applied. What this guards is that the new
    // dimension-carrying path did not swallow or bypass that propagation: a
    // dimensioned redeclaration must still surface `EI014`, not a reshaped
    // component.
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

        partial model Holder
            BaseType a;
        end Holder;

        model Test
            extends Holder(redeclare DerivedType a[3]);
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
        equation
            p.comb.u = zeros(2);
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
    // The deselected element is a model component: MLS §7.4 (INST-047) only
    // allows deselecting models, blocks, and connectors.
    expect_success(
        r#"
        model Base
            model Sub
                Real x = 1;
            end Sub;
            Sub sub;
        end Base;

        model Test
            extends Base(break sub);
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
            constant Real g = 9.81;
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
