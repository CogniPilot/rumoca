//! TYPE (Type/Interface) contract tests - MLS §6
//!
//! Tests for the 35 type contracts defined in SPEC_0022.

use rumoca_compile::compile::FailedPhase;
use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_resolve_failure_with_code,
    expect_success,
};

// =============================================================================
// TYPE-009: Variability ordering
// "A compatible with B only if declared variability in A <= variability in B"
// =============================================================================

#[test]
fn type_009_variability_compatible() {
    expect_success(
        r#"
        model Test
            constant Real c = 1.0;
            parameter Real p = c;
            Real x;
        equation
            x = p;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// TYPE-011: Dimension count match
// "Number of array dimensions in A and B must be matched"
// =============================================================================

#[test]
fn type_011_dimension_match() {
    expect_success(
        r#"
        model Test
            Real x[3];
        equation
            x = {1, 2, 3};
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// TYPE-013: Enumeration match
// "If B is enumeration type, A must also be"
// =============================================================================

#[test]
fn type_013_enumeration_basic() {
    expect_success(
        r#"
        type Color = enumeration(Red, Green, Blue);
        model Test
            Color c;
        equation
            c = Color.Red;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_013_enumeration_mismatch_fails() {
    expect_failure_in_phase_with_code(
        r#"
        type StateA = enumeration(Off, On);
        type StateB = enumeration(Off, On);
        model Test
            StateA a;
            StateB b;
        equation
            a = b;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

// =============================================================================
// TYPE-014: Built-in type match
// "If B is built-in type, A must be same built-in type"
// =============================================================================

#[test]
fn type_014_builtin_type_match() {
    expect_balanced(
        r#"
        model Test
            Real x;
            Real y;
        equation
            x = 1.0;
            y = x;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// TYPE-033: Real/Integer coercion
// "If A is Real expression, B must be Real or Integer; result is Real"
// =============================================================================

#[test]
fn type_033_integer_to_real_coercion() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = 1 + 2.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// TYPE-034: Integer division result
// "For Integer exponentiation and division, result type is Real"
// =============================================================================

#[test]
fn type_034_integer_division_real() {
    expect_success(
        r#"
        model Test
            Real x;
        equation
            x = 7 / 2;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// Type integration tests
// =============================================================================

#[test]
fn type_alias_usage() {
    expect_success(
        r#"
        type Voltage = Real(unit = "V");
        model Test
            Voltage v;
        equation
            v = 1.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_extends_basic() {
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
fn type_enumeration_usage() {
    expect_success(
        r#"
        type State = enumeration(Off, On, Error);
        model Test
            State s;
            Real x;
        equation
            s = State.On;
            x = if s == State.On then 1.0 else 0.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_005_class_component_mismatch() {
    expect_resolve_failure_with_code(
        r#"
        model A
            Real x;
        equation
            x = 1;
        end A;
        model Test
            Integer y;
        equation
            y = A;
        end Test;
    "#,
        "Test",
        "ER011",
    );
}

#[test]
fn type_005_unrelated_nested_class_does_not_shadow_outer_value() {
    expect_success(
        r#"
        package Other
            record data
                Real x;
            end data;
        end Other;

        model Base
            constant Real data = 2.0;
        end Base;

        model Test
            extends Base;
            Real y;
        equation
            y = data;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_028_record_mismatch_fails() {
    expect_failure_in_phase_with_code(
        r#"
        record PayloadA
            Real x;
        end PayloadA;
        record PayloadB
            Real x;
        end PayloadB;
        model Test
            PayloadA a;
            PayloadB b;
        equation
            a = b;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

// =============================================================================
// TYPE-030: Modifier element exists
// "Modified element should exist in element being modified"
// =============================================================================

#[test]
fn type_030_existing_modifier_target_is_allowed() {
    expect_success(
        r#"
        model Base
            parameter Real kp = 1.0;
        end Base;

        model PID
            extends Base;
        end PID;

        model Test
            PID pid(kp = 10.0);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_030_missing_modifier_target_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model PID
            parameter Real kp = 1.0;
        end PID;

        model Test
            PID pid(kps = 10.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET001",
    );
}

// =============================================================================
// TYPE-001: Subtype redeclaration
// "Class or type must be subtype of constraining type" (MLS §7.3.2)
// =============================================================================

#[test]
fn type_001_subtype_redeclare_accepted() {
    expect_success(
        r#"
        model Base
            Real v = 1;
        end Base;
        model Derived
            extends Base;
            Real w = 2;
        end Derived;
        model Holder
            replaceable Base b constrainedby Base;
            Real x(start = 0, fixed = true);
        equation
            der(x) = b.v;
        end Holder;
        model Test
            Holder h(redeclare Derived b);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_001_unrelated_redeclare_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            Real v = 1;
        end Base;
        model Unrelated
            Real w = 2;
        end Unrelated;
        model Holder
            replaceable Base b constrainedby Base;
            Real x(start = 0, fixed = true);
        equation
            der(x) = b.v;
        end Holder;
        model Test
            extends Holder(redeclare Unrelated b);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-031: Top-level redeclaration subtype
// "Redeclaration of inherited top-level component must be subtype of
//  constraining interface" (MLS §7.3.2)
// =============================================================================

#[test]
fn type_031_inherited_redeclare_must_match_constraint() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            Real v = 1;
        end Base;
        model Unrelated
            Real w = 2;
        end Unrelated;
        model Holder
            replaceable Base b constrainedby Base;
            Real x(start = 0, fixed = true);
        equation
            der(x) = b.v;
        end Holder;
        model Test
            extends Holder(redeclare Unrelated b);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-002: Redeclarations must be plug-compatible with constraining interface
// =============================================================================

#[test]
fn type_002_redeclared_class_missing_member_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            block Base
                Real y = 1;
            end Base;
            block Bad
                Real z = 1;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-010: Input and output prefixes must be matched
// =============================================================================

#[test]
fn type_010_redeclared_class_flipped_causality_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            block Base
                replaceable input Real u;
                output Real y = 1;
            end Base;
            block Bad
                output Real u = 1;
                output Real y = 1;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-012: Conditional components are only compatible with conditional components
// =============================================================================

#[test]
fn type_012_redeclared_class_conditional_member_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            parameter Boolean has = true;
            block Base
                Real y = 1;
            end Base;
            block Bad
                Real y = 1 if has;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-024: The flow or stream prefix should be matched for compatibility
// =============================================================================

#[test]
fn type_024_redeclared_connector_flow_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            connector CBase
                Real e;
                flow Real f;
            end CBase;
            connector CBad
                Real e;
                Real f;
            end CBad;
            model Holder
                replaceable CBase c;
            end Holder;
            model Use
                extends Holder(redeclare CBad c);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-008: If B is not replaceable then A shall not be replaceable
// =============================================================================

#[test]
fn type_008_redeclared_class_dropped_replaceable_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            block Base
                replaceable Real y = 1;
            end Base;
            block Bad
                Real y = 1;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-017: Parameter/constant/non-connector input must have binding equations
// =============================================================================

#[test]
fn type_017_redeclared_class_unbound_parameter_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            block Base
                parameter Real p = 1;
            end Base;
            block Bad
                parameter Real p;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-029: If B is enumeration not defined as (:), A must have same literals
// in same order
// =============================================================================

#[test]
fn type_029_redeclared_class_enum_order_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            type EA = enumeration(a, b);
            type EBad = enumeration(b, a);
            block Base
                EA e = EA.a;
            end Base;
            block Bad
                EBad e = EBad.a;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-025: The inner and/or outer prefixes should be matched
// =============================================================================

#[test]
fn type_025_redeclared_class_inner_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            block Base
                Real y = 1;
            end Base;
            block Bad
                inner Real y = 1;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-027: Conditional components: conditions must have equivalent contents
// =============================================================================

#[test]
fn type_027_redeclared_class_condition_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            block Base
                parameter Boolean has = true;
                Real y = 1 if has;
            end Base;
            block Bad
                parameter Boolean has = true;
                Real y = 1 if not has;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-004: Inputs same order, outputs match, purity matches
// =============================================================================

#[test]
fn type_004_redeclared_function_input_type_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            partial function FBase
                input Real u;
                output Real y;
            end FBase;
            function FBad
                input Boolean u;
                output Real y;
            algorithm
                y := 1;
            end FBad;
            model Holder
                replaceable function F = FBase;
                Real z = F(1.0);
            end Holder;
            model Use
                extends Holder(redeclare function F = FBad);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-021: If A is impure, then B must also be impure
// =============================================================================

#[test]
fn type_021_redeclared_function_impure_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            partial function FBase
                input Real u;
                output Real y;
            end FBase;
            impure function FBad
                input Real u;
                output Real y;
            algorithm
                y := u;
            end FBad;
            model Holder
                replaceable function F = FBase;
                Real z(start = 0);
            equation
                when time > 1 then
                    z = F(1.0);
                end when;
            end Holder;
            model Use
                extends Holder(redeclare function F = FBad);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-006: If A has operator record base class, B must have same one
// =============================================================================

#[test]
fn type_006_operator_record_base_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            operator record OR
                Real re;
            end OR;
            record Bad = OR;
            model Holder
                replaceable OR field;
            end Holder;
            model Use
                extends Holder(redeclare Bad field);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-016: Connector component must not be of expandable connector class
// =============================================================================

#[test]
fn type_016_redeclared_to_expandable_connector_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            connector CBase
                Real e;
                flow Real f;
            end CBase;
            expandable connector EBad
                extends CBase;
            end EBad;
            model Holder
                replaceable CBase c;
            end Holder;
            model Use
                extends Holder(redeclare EBad c);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-018: All public input components of B have named inputs in A in same
// order
// =============================================================================

#[test]
fn type_018_sibling_function_missing_named_input_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            partial function FCommon
                input Real u;
                output Real y;
            end FCommon;
            function FBase
                extends FCommon;
                input Real v;
            algorithm
                y := u + v;
            end FBase;
            function FBad
                extends FCommon;
                input Real w;
            algorithm
                y := u + w;
            end FBad;
            model Holder
                replaceable function F = FBase;
                Real z = F(1.0, 2.0);
            end Holder;
            model Use
                extends Holder(redeclare function F = FBad);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-019: All public output components of B have named outputs in A in same
// order
// =============================================================================

#[test]
fn type_019_sibling_function_extra_output_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            partial function FCommon
                input Real u;
                output Real y;
            end FCommon;
            function FBase
                extends FCommon;
            algorithm
                y := u;
            end FBase;
            function FBad
                extends FCommon;
                output Real z;
            algorithm
                y := u;
                z := 2 * u;
            end FBad;
            model Holder
                replaceable function F = FBase;
                Real z = F(1.0);
            end Holder;
            model Use
                extends Holder(redeclare function F = FBad);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-020: Public input component of A not present in B must have binding
// assignment
// =============================================================================

#[test]
fn type_020_sibling_function_extra_input_without_default_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            partial function FCommon
                input Real u;
                output Real y;
            end FCommon;
            function FBase
                extends FCommon;
            algorithm
                y := u;
            end FBase;
            function FBad
                extends FCommon;
                input Real w;
            algorithm
                y := u + w;
            end FBad;
            model Holder
                replaceable function F = FBase;
                Real z = F(1.0);
            end Holder;
            model Use
                extends Holder(redeclare function F = FBad);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

#[test]
fn type_020_sibling_function_extra_input_with_default_accepted() {
    expect_success(
        r#"
        model M
            partial function FCommon
                input Real u;
                output Real y;
            end FCommon;
            function FBase
                extends FCommon;
            algorithm
                y := u;
            end FBase;
            function FOk
                extends FCommon;
                input Real w = 0;
            algorithm
                y := u + w;
            end FOk;
            model Holder
                replaceable function F = FBase;
                Real z = F(1.0);
            end Holder;
            model Use
                extends Holder(redeclare function F = FOk);
            end Use;
            Use u;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// TYPE-022: If B is transitively non-replaceable then A must be transitively
// non-replaceable
// =============================================================================

#[test]
fn type_022_replacement_with_replaceable_member_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            block Common
                Real y = 1;
            end Common;
            block Base
                extends Common;
            end Base;
            block Bad
                extends Common;
                replaceable Real extra = 0;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-026: If B is final, A must also be final and have same semantic contents
// =============================================================================

#[test]
fn type_026_final_constraint_requires_final_replacement_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            block Common
                Real y = 1;
            end Common;
            final block Base
                extends Common;
            end Base;
            block Bad
                extends Common;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-023: Interface of A shall not contain any other elements when B is
// transitively non-replaceable
// =============================================================================

#[test]
fn type_023_extra_elements_under_tnr_constraint_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            block Common
                Real y = 1;
            end Common;
            block Base
                extends Common;
            end Base;
            block Bad
                extends Common;
                Real extra = 0;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-032: If A is record expression, B must also be record expression with
// same named elements
// =============================================================================

#[test]
fn type_032_if_expression_mixing_record_types_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            record RA
                Real x;
            end RA;
            record RB
                Real y;
            end RB;
            parameter Boolean sel = true;
            RA a = RA(1.0);
            RB b = RB(2.0);
            Real z;
        equation
            z = (if sel then a else b).x;
        end M;
    "#,
        "M",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// TYPE-035: For array/if-expressions: if A has operator record base, B must
// have same one
// =============================================================================

#[test]
fn type_035_if_expression_mixing_operator_records_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            operator record CA
                Real re;
            end CA;
            operator record CB
                Real re;
            end CB;
            parameter Boolean sel = true;
            CA a = CA(1.0);
            CB b = CB(2.0);
            Real z;
        equation
            z = (if sel then a else b).re;
        end M;
    "#,
        "M",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// TYPE-003: Additional public components must be default-connectable
// =============================================================================

#[test]
fn type_003_extra_member_without_default_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            block Common
                Real y = 1;
            end Common;
            block Base
                extends Common;
            end Base;
            block Bad
                extends Common;
                Real dangling;
            equation
                dangling = 2 * y;
            end Bad;
            model Holder
                replaceable Base b;
            end Holder;
            model Use
                extends Holder(redeclare Bad b);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-007: If A derived from ExternalObject, B must also be and have same
// full name
// =============================================================================

#[test]
fn type_007_external_object_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            class EoBase
                extends ExternalObject;
            end EoBase;
            class EoOther
                extends ExternalObject;
            end EoOther;
            model Holder
                replaceable EoBase handle;
            end Holder;
            model Use
                extends Holder(redeclare EoOther handle);
            end Use;
            Use u;
        end M;
    "#,
        "M",
        FailedPhase::Instantiate,
        "EI027",
    );
}
