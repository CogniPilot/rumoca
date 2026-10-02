//! INST (Instantiation) contract tests - MLS §5, §7
//!
//! Tests for the instantiation contracts defined in SPEC_0022; lookup,
//! inner/outer, final, `break`, `each`, and class-selection contracts are in
//! `inst_lookup_contracts.rs`, extends-clause contracts in
//! `inst_extends_contracts.rs`.

use rumoca_compile::compile::FailedPhase;
use rumoca_contracts::test_support::{
    expect_balanced, expect_compile_warning, expect_failure_in_phase_with_code,
    expect_parse_err_with_code, expect_resolve_failure_with_code, expect_success,
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

#[test]
fn inst_010_declaration_cannot_modify_final_type_attribute() {
    expect_failure_in_phase_with_code(
        r#"
        type Voltage = Real(final unit = "V");
        model Test
            Voltage v(unit = "kV");
        equation
            v = 1;
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
// INST-054: Automatic inner creation
// "An inner declaration of a unique non-partial class is automatically added
// for outer declarations lacking a matching inner, with a diagnostic"
// =============================================================================

#[test]
fn inst_054_outer_without_inner_synthesizes_default() {
    expect_compile_warning(
        r#"
        model World
            parameter Boolean enableAnimation = true;
            parameter Real nominalLength = 1;
            parameter Real defaultBodyDiameter = nominalLength/9;
            annotation(
                defaultComponentName="world",
                defaultComponentPrefixes="inner",
                missingInnerMessage="A default world component with the default
gravity field will be used.");
        end World;
        model Shape
            Real s;
        equation
            s = 1.0;
        end Shape;
        model Body
            outer World world;
            parameter Boolean animation = true;
            parameter Real sphereDiameter = world.defaultBodyDiameter;
            Shape sphere if world.enableAnimation and animation and sphereDiameter > 0;
        end Body;
        model Standalone
            Body body;
        end Standalone;
    "#,
        "Standalone",
        "WI013",
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
