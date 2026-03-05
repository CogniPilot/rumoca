//! INST (Instantiation) contract tests - MLS §5, §7
//!
//! Tests for the 53 instantiation contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_parse_err_with_code,
    expect_resolve_failure_with_code, expect_success,
};
use rumoca_session::{FailedPhase, PhaseResult, Session, SessionConfig};
use std::collections::BTreeSet;

fn expect_needs_inner(source: &str, model: &str, expected_missing: &[&str]) {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .unwrap_or_else(|e| panic!("Parse failed for {model}: {e}"));
    let result = session
        .compile_model_phases(model)
        .unwrap_or_else(|e| panic!("compile_model_phases failed for {model}: {e}"));
    match result {
        PhaseResult::NeedsInner { missing_inners } => {
            let actual: BTreeSet<String> = missing_inners.into_iter().collect();
            let expected: BTreeSet<String> = expected_missing
                .iter()
                .map(|name| name.to_string())
                .collect();
            assert_eq!(
                actual, expected,
                "Unexpected missing inner declarations for {model}"
            );
        }
        other => panic!("Expected NeedsInner for {model}, got {other:?}"),
    }
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
// INST-004: Unnamed extends nodes
// "Preserve declaration order in inheritance"
// =============================================================================

#[test]
fn inst_004_unnamed_extends_nodes() {
    expect_success(
        r#"
        model A
            parameter Real p = 2;
        end A;

        model B
            extends A(p = 2);
        end B;

        model C
            extends A(p = 2);
        end C;

        model Test
            extends B;
            extends C;
            Real x;
        equation
            x = p;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-005: Extends lookup stability
// "Extends classes looked up before and after handling; error if different"
// =============================================================================

#[test]
fn inst_005_extends_lookup_stability() {
    expect_success(
        r#"
        package Base
            replaceable record S
                Real x;
            end S;
        end Base;

        package Derived
            extends Base;
            redeclare record extends S
                Real y;
            end S;
        end Derived;

        model Test
            Derived.S s;
        equation
            s.x = 1;
            s.y = 2;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-006: Name collision
// "Flattened inherited declarations must match exactly"
// =============================================================================

#[test]
fn inst_006_name_collision_conflicting_inheritance_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model A
            Real x;
        equation
            x = 1;
        end A;

        model B
            Integer x;
        equation
            x = 1;
        end B;

        model Test
            extends A;
            extends B;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI010",
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
// INST-009: Final inheritance
// "All elements of a final element are also final"
// =============================================================================

#[test]
#[ignore = "TODO(INST-009): enforce transitive final inheritance on non-simple components"]
fn inst_009_final_inheritance_cannot_be_overridden() {
    expect_failure_in_phase_with_code(
        r#"
        record R
            parameter Real a = 1;
        end R;

        model Base
            final R r;
        end Base;

        model Test
            extends Base(r(a = 2));
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI028",
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
fn inst_011_inner_outer_subtype_mismatch_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model Top
            inner Real shared;

            model Child
                outer Integer shared;
                Integer y;
            equation
                y = shared;
            end Child;

            Child c;
        equation
            shared = 1;
        end Top;
    "#,
        "Top",
        FailedPhase::Instantiate,
        "EI009",
    );
}

// =============================================================================
// INST-012: Outer no modifications
// "Outer component declarations shall not have modifications"
// =============================================================================

#[test]
fn inst_012_outer_no_modifications() {
    expect_parse_err_with_code(
        r#"
        model Test
            outer Real shared = 1;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// INST-013: Constant-only references
// "Enclosing class variables accessible only if declared constant"
// =============================================================================

#[test]
#[ignore = "TODO(INST-013): reject non-constant enclosing refs during resolve/instantiate"]
fn inst_013_nonconstant_enclosing_reference_fails_lookup() {
    expect_resolve_failure_with_code(
        r#"
        model Outer
            Real x = 1;
            model Inner
                Real y = x;
            end Inner;
            Inner i;
        end Outer;
    "#,
        "Outer",
        "ER002",
    );
}

// =============================================================================
// INST-014: Redeclaration constraint
// "Only replaceable classes/components may be redeclared with a new type"
// =============================================================================

#[test]
fn inst_014_redeclare_non_replaceable_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model Inner
            Real x;
        equation
            x = 1;
        end Inner;

        model Other
            Real x;
        equation
            x = 2;
        end Other;

        model Base
            Inner c;
        end Base;

        model Test
            extends Base(redeclare Other c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI014",
    );
}

// =============================================================================
// INST-015: Transitively non-replaceable
// "Base classes in extends use transitively non-replaceable references"
// =============================================================================

#[test]
fn inst_015_transitively_non_replaceable_extends_ok() {
    expect_success(
        r#"
        model A
            Real x;
        equation
            x = 1;
        end A;

        model B
            extends A;
        end B;

        model Test
            extends B;
        end Test;
    "#,
        "Test",
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
// INST-017: Conditional no redeclare
// "Redeclaration shall not include a condition attribute"
// =============================================================================

#[test]
fn inst_017_conditional_no_redeclare() {
    expect_parse_err_with_code(
        r#"
        model Inner
            Real x;
        end Inner;

        model Base
            replaceable Inner c;
        end Base;

        model Test
            parameter Boolean use_c = true;
            extends Base(redeclare Inner c if use_c);
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// INST-018: Outer same class
// "Two outer declarations with same name but different classes is error"
// =============================================================================

#[test]
fn inst_018_outer_same_class_conflict() {
    expect_parse_err_with_code(
        r#"
        model Test
            outer Real shared;
            outer Integer shared;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// INST-019: Outer partial error
// "Outer of partial class is error"
// =============================================================================

#[test]
fn inst_019_outer_partial_error() {
    expect_resolve_failure_with_code(
        r#"
        partial model PartialT
            Real x;
        end PartialT;

        model Test
            outer PartialT p;
        equation
            p.x = 1;
        end Test;
    "#,
        "Test",
        "ER005",
    );
}

// =============================================================================
// INST-020: Inner/outer connector
// "Inner/outer connector shall not expose top-level input connectors"
// =============================================================================

#[test]
fn inst_020_inner_outer_connector_without_input_ok() {
    expect_success(
        r#"
        connector Bus
            Real v;
            flow Real i;
        end Bus;

        model Child
            outer Bus b;
        equation
            b.i = 0;
        end Child;

        model Test
            inner Bus b;
            Child c;
        equation
            b.v = 1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-021: Lookup non-partial
// "Class looked inside shall not be partial in simulation model"
// =============================================================================

#[test]
fn inst_021_lookup_non_partial() {
    expect_failure_in_phase_with_code(
        r#"
        package PartialMedium
          replaceable partial model BaseProperties
            Real p;
          end BaseProperties;
        end PartialMedium;

        model Test
          replaceable package Medium = PartialMedium;
          Medium.BaseProperties medium;
        equation
          medium.p = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI012",
    );
}

// =============================================================================
// INST-022: Constant not redeclared
// "An element declared as constant cannot be redeclared"
// =============================================================================

#[test]
fn inst_022_constant_not_redeclared() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            constant Real c = 1;
        end Base;

        model Test
            extends Base(redeclare Real c = 2);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI014",
    );
}

// =============================================================================
// INST-023: Protection not changed
// "Protected/public visibility shall not be changed by redeclaration"
// =============================================================================

#[test]
fn inst_023_protection_not_changed() {
    expect_success(
        r#"
        model Base
        protected
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

// =============================================================================
// INST-024: Extends kind compatibility
// "Only compatible specialized classes inherit from each other"
// =============================================================================

#[test]
fn inst_024_extends_kind_compatibility() {
    expect_success(
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

// =============================================================================
// INST-025: Equation syntactic equivalence
// "Syntactically equivalent inherited equations are discarded"
// =============================================================================

#[test]
fn inst_025_equation_syntactic_equiv() {
    expect_success(
        r#"
        model Base
            Real x;
        equation
            x = 1;
        end Base;

        model Test
            extends Base;
        equation
            x = 1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-026: Class extends replaceable
// "Original class should be replaceable in class extends"
// =============================================================================

#[test]
fn inst_026_class_extends_replaceable() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            Real x;
        equation
            x = 1;
        end M;

        model N
            Real x;
        equation
            x = 2;
        end N;

        model Base
            M m;
        end Base;

        model Test
            extends Base(redeclare N m);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI014",
    );
}

// =============================================================================
// INST-027: Constraining type auto-apply
// "Constraining type modifications apply to both declaration and constraint"
// =============================================================================

#[test]
fn inst_027_constraining_type_auto_apply() {
    expect_success(
        r#"
        partial model Interface
            parameter Real p = 1;
        end Interface;

        model Impl
            extends Interface;
        end Impl;

        model Test
            replaceable model M = Impl constrainedby Interface(p = 3);
            M m;
        equation
            m.p = 3;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-028: Break deselection order
// "Break is applied before non-selective modifications"
// =============================================================================

#[test]
fn inst_028_break_deselection_order() {
    expect_success(
        r#"
        model Base
            parameter Real x = 1;
            Real y;
        equation
            y = 2;
        end Base;

        model Test
            extends Base(break x, x = 10);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-029: Break must match
// "Break name must exist in base class"
// =============================================================================

#[test]
fn inst_029_break_must_match() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            Real x;
        equation
            x = 1;
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
// INST-030: Override final error
// "Overriding a final non-simple component is an error"
// =============================================================================

#[test]
#[ignore = "TODO(INST-030): reject overrides of final non-simple components"]
fn inst_030_override_final_error() {
    expect_failure_in_phase_with_code(
        r#"
        record R
            parameter Real a = 1;
        end R;

        model Base
            final R r;
        end Base;

        model Test
            extends Base(r(a = 5));
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI028",
    );
}

// =============================================================================
// INST-031: Outer class short
// "Outer class declarations should use short-class definitions without modifications"
// =============================================================================

#[test]
fn inst_031_outer_class_short() {
    expect_needs_inner(
        r#"
        model Test
            outer Real shared;
        equation
            shared = 1;
        end Test;
    "#,
        "Test",
        &["shared"],
    );
}

// =============================================================================
// INST-032: Outer disabled conditional
// "Disabled outer conditional components are ignored for automatic inner creation"
// =============================================================================

#[test]
fn inst_032_outer_disabled_conditional() {
    expect_success(
        r#"
        model Test
            parameter Boolean use_shared = false;
            Real x;
            outer Real shared if use_shared;
        equation
            x = 1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-033: Global scope error
// "Missing class in global scope is an error"
// =============================================================================

#[test]
fn inst_033_global_scope_error() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            extends MissingBase;
        end Test;
    "#,
        "Test",
        "ER003",
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
// INST-035: Record/enum lookup ignore
// "Record/enumeration names are ignored during function lookup"
// =============================================================================

#[test]
fn inst_035_record_enum_lookup_ignore() {
    expect_success(
        r#"
        record State
            Real x;
        end State;

        function StateFn
            input Real x;
            output Real y;
        algorithm
            y := x;
        end StateFn;

        model Test
            Real z;
        equation
            z = StateFn(1);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-036: Constructor lookup ignore
// "Implicit constructors/conversions ignored during type lookup"
// =============================================================================

#[test]
fn inst_036_constructor_lookup_ignore() {
    expect_success(
        r#"
        record R
            Real x;
        end R;

        type RAlias = R;

        model Test
            RAlias r;
        equation
            r = R(1);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-037: Identical children first kept
// "Same-name children must be identical; error if not identical"
// =============================================================================

#[test]
fn inst_037_identical_children_first_kept() {
    expect_failure_in_phase_with_code(
        r#"
        model A
            Real x;
        equation
            x = 1;
        end A;

        model B
            Integer x;
        equation
            x = 2;
        end B;

        model Test
            extends A;
            extends B;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI010",
    );
}

// =============================================================================
// INST-038: Inner/outer both modifies
// "Modification on inner outer only applies to inner declaration"
// =============================================================================

#[test]
fn inst_038_inner_outer_both_modifies() {
    expect_success(
        r#"
        model Child
            outer Real k;
            Real y;
        equation
            y = k;
        end Child;

        model InnerLevel
            inner outer Real k = 2;
            Child c;
        equation
            k = 2;
        end InnerLevel;

        model Test
            inner Real k = 2;
            InnerLevel lvl;
        equation
            k = 2;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-039: Protected extends
// "Elements inherited via protected extends become protected"
// =============================================================================

#[test]
#[ignore = "TODO(INST-039): propagate protected visibility from protected extends into flat vars"]
fn inst_039_protected_extends() {
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
    let x = result
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "x")
        .map(|(_, var)| var)
        .expect("expected inherited variable x");
    assert!(x.is_protected, "expected inherited x to be protected");
}

// =============================================================================
// INST-040: Each array required
// "each modifier use is valid for array declarations/modifications"
// =============================================================================

#[test]
fn inst_040_each_array_required() {
    expect_success(
        r#"
        model Cell
            parameter Real p = 0;
            Real x;
        equation
            x = p;
        end Cell;

        model Test
            Cell c[2](each p = 1);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-041: Each size match
// "Sizes must match without each prefix or it is an error"
// =============================================================================

#[test]
#[ignore = "TODO(INST-041): enforce non-each array modifier size mismatches"]
fn inst_041_each_size_match() {
    expect_failure_in_phase_with_code(
        r#"
        model Cell
            parameter Real p = 0;
            Real x;
        equation
            x = p;
        end Cell;

        model Test
            Cell c[2](p = {1, 2, 3});
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI005",
    );
}

// =============================================================================
// INST-042: Break removes expression
// "Breaked modifications are treated as missing expression"
// =============================================================================

#[test]
fn inst_042_break_removes_expression() {
    expect_success(
        r#"
        model Base
            parameter Real x = 1;
            Real y;
        equation
            y = 2;
        end Base;

        model Test
            extends Base(break x, x = 10);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-043: Implicit constraining type
// "If constraining clause is absent, declaration type is constraining type"
// =============================================================================

#[test]
fn inst_043_implicit_constraining_type() {
    expect_success(
        r#"
        model BaseT
            Real x;
        equation
            x = 1;
        end BaseT;

        model ChildT
            extends BaseT;
        end ChildT;

        model Container
            replaceable model M = BaseT;
            M m;
        end Container;

        model Test
            extends Container(redeclare model M = ChildT);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-044: Constraining subtype
// "Constraining type of redeclare must be subtype of original constraint"
// =============================================================================

#[test]
fn inst_044_constraining_subtype() {
    expect_failure_in_phase_with_code(
        r#"
        model Interface
            Real x;
        equation
            x = 1;
        end Interface;

        model Good
            extends Interface;
        end Good;

        model Bad
            Real y;
        equation
            y = 1;
        end Bad;

        model Base
            replaceable Good c constrainedby Interface;
        end Base;

        model Test
            extends Base(redeclare Bad c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// INST-045: Dimension correspondence
// "Constraining type dimensions should correspond to type-part dimensions"
// =============================================================================

#[test]
fn inst_045_dimension_correspondence() {
    expect_success(
        r#"
        model Elem
            Real x;
        equation
            x = 1;
        end Elem;

        model Base
            replaceable Elem e[2];
        end Base;

        model Test
            extends Base(redeclare Elem e[2]);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-046: Array dim redeclaration
// "Array dimensions may be redeclared if sub-typing rules are satisfied"
// =============================================================================

#[test]
fn inst_046_array_dim_redeclaration() {
    expect_success(
        r#"
        model Elem
            Real x;
        equation
            x = 1;
        end Elem;

        model Base
            replaceable Elem e[2];
        end Base;

        model Test
            extends Base(redeclare Elem e[2]);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-047: Component deselection types
// "Deselected components must be model/block/connector"
// =============================================================================

#[test]
fn inst_047_component_deselection_types() {
    expect_success(
        r#"
        model Child
            Real x;
        equation
            x = 1;
        end Child;

        model Base
            Child c;
            Real y;
        equation
            y = 1;
        end Base;

        model Test
            extends Base(break c);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-048: Conditional deselection
// "Conditional declarations are assumed present for deselection matching"
// =============================================================================

#[test]
fn inst_048_conditional_deselection() {
    expect_success(
        r#"
        model Base
            parameter Boolean use_x = false;
            Real x if use_x;
            Real y;
        equation
            y = 1;
        end Base;

        model Test
            extends Base(break x);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-049: Partial deselection allowed
// "Deselected component may be partial even in simulation model"
// =============================================================================

#[test]
#[ignore = "TODO(INST-049): allow deselecting partial components before partial-instantiation checks"]
fn inst_049_partial_deselection_allowed() {
    expect_success(
        r#"
        partial model P
            Real x;
        end P;

        partial model Base
            P p;
            Real y;
        equation
            y = 1;
        end Base;

        model Test
            extends Base(break p);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-050: Unqualified import conflict
// "Error if multiple unqualified imports match the same name"
// =============================================================================

#[test]
fn inst_050_unqualified_import_conflict() {
    expect_resolve_failure_with_code(
        r#"
        package P1
            model A
                Real x;
            equation
                x = 1;
            end A;
        end P1;

        package P2
            model A
                Real x;
            equation
                x = 2;
            end A;
        end P2;

        model Test
            import A = P1.A;
            import A = P2.A;
            A a;
        end Test;
    "#,
        "Test",
        "ER012",
    );
}

// =============================================================================
// INST-051: Constraining annotation conflict
// "Annotations on both declaration and constraining clause conflict"
// =============================================================================

#[test]
fn inst_051_constraining_annotation_conflict() {
    expect_success(
        r#"
        partial model Interface
            Real x;
        end Interface;

        model Impl
            extends Interface;
        equation
            x = 1;
        end Impl;

        model Test
            replaceable model M = Impl constrainedby Interface annotation(__RumocaTag = 1);
            M m;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// INST-052: Redeclaration dimension match
// "Redeclaration must keep the same number of dimensions"
// =============================================================================

#[test]
#[ignore = "TODO(INST-052): enforce redeclare dimension-count equality"]
fn inst_052_redeclaration_dimension_match() {
    expect_failure_in_phase_with_code(
        r#"
        model Elem
            Real x;
        equation
            x = 1;
        end Elem;

        model Base
            replaceable Elem e[2];
        end Base;

        model Test
            extends Base(redeclare Elem e[2, 2]);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI005",
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
