//! TYPE (Type/Interface) contract tests - MLS §6
//!
//! Tests for the 35 type contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_resolve_failure_with_code,
    expect_success, is_standalone_simulatable, unbound_fixed_parameter_names,
};
use rumoca_session::FailedPhase;

// =============================================================================
// TYPE-001: Subtype redeclaration
// "Class or type must be subtype of constraining type"
// =============================================================================

#[test]
#[ignore = "TODO(TYPE-001): enforce subtype constraints for replaceable model redeclarations"]
fn type_001_subtype_redeclaration_requires_subtype() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            replaceable model C
                Real x;
            end C;
            C c;
        end Base;

        model BadC
            Integer x;
        end BadC;

        model Test
            extends Base(redeclare model C = BadC);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-002: Plug-compatible modifier
// "Redeclarations must be plug-compatible with constraining interface"
// =============================================================================

#[test]
#[ignore = "TODO(TYPE-002): enforce plug-compatibility on incompatible model redeclarations"]
fn type_002_redeclaration_must_be_plug_compatible() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            replaceable model C
                Real x;
            end C;
            C c;
        end Base;

        model BadC
            Integer x;
        end BadC;

        model Test
            extends Base(redeclare model C = BadC);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-017: Binding required
// "Parameter/constant/non-connector input must have binding equations"
// =============================================================================

#[test]
fn type_017_unbound_fixed_parameter_not_standalone() {
    let result = expect_success(
        r#"
        model Test
            parameter Real p;
            Real x;
        equation
            x = p;
        end Test;
    "#,
        "Test",
    );
    assert!(
        !is_standalone_simulatable(&result),
        "model with unbound fixed parameter should not be standalone simulatable"
    );
    assert_eq!(
        unbound_fixed_parameter_names(&result),
        vec!["p".to_string()],
        "expected exactly one unbound fixed parameter"
    );
}

// =============================================================================
// TYPE-018: Func input order
// "Function interface uses declared input order"
// =============================================================================

#[test]
fn type_018_function_input_order_preserved() {
    expect_success(
        r#"
        function F
            input Real a;
            input Real b;
            output Real y;
        algorithm
            y := a - b;
        end F;

        model Test
            Real z;
        equation
            z = F(3.0, 1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// TYPE-020: Func input binding
// "Public input in A not present in B must have binding assignment"
// =============================================================================

#[test]
fn type_020_missing_function_input_binding_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real a;
            input Real b;
            output Real y;
        algorithm
            y := a + b;
        end F;

        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
        "ER034",
    );
}

// =============================================================================
// TYPE-021: Func impure propagate
// "If A is impure, then B must also be impure"
// =============================================================================

#[test]
fn type_021_impure_function_subtype_requires_impure_base() {
    expect_resolve_failure_with_code(
        r#"
        impure function BaseF
            input Real x;
            output Real y;
        algorithm
            y := x;
        end BaseF;

        function DerivedF
            extends BaseF;
        end DerivedF;

        model Test
            Real z;
        equation
            z = DerivedF(1.0);
        end Test;
    "#,
        "Test",
        "ER040",
    );
}

// =============================================================================
// TYPE-029: Enum literals order
// "Enumeration literals must match in content and order"
// =============================================================================

#[test]
fn type_029_enumeration_literal_order_mismatch_fails() {
    expect_failure_in_phase_with_code(
        r#"
        type E1 = enumeration(A, B, C);
        type E2 = enumeration(B, A, C);

        model Test
            E1 x;
            E2 y;
        equation
            x = y;
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
fn type_030_unknown_modifier_element_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x(foo = 1);
        equation
            x = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET001",
    );
}

// =============================================================================
// TYPE-031: Top-level redecl subtype
// "Inherited top-level redeclaration must be subtype of constraining interface"
// =============================================================================

#[test]
fn type_031_top_level_component_redeclaration_requires_subtype() {
    expect_failure_in_phase_with_code(
        r#"
        model BaseType
            Real x;
        end BaseType;

        model BadType
            Integer x;
        end BadType;

        model Base
            replaceable BaseType c;
        end Base;

        model Test
            extends Base(redeclare BadType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-032: Record expr compatible
// "Record expression compatibility uses same record interface"
// =============================================================================

#[test]
fn type_032_record_expression_compatible() {
    expect_success(
        r#"
        record R
            Real a;
            Real b;
        end R;

        model Test
            R r;
        equation
            r = R(1.0, 2.0);
        end Test;
    "#,
        "Test",
    );
}

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
// TYPE-003: Default-connectable
// "Additional public components must be default-connectable"
// =============================================================================

#[test]
fn type_003_additional_public_components_must_be_default_connectable() {
    expect_failure_in_phase_with_code(
        r#"
        model BaseType
            Real x;
        end BaseType;

        model BadType
            Real x;
            parameter Real p;
        end BadType;

        model Base
            replaceable BaseType c;
        end Base;

        model Test
            extends Base(redeclare BadType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-004: Function compatibility
// "Inputs same order, outputs match, purity matches"
// =============================================================================

#[test]
fn type_004_function_compatibility_requires_purity_match() {
    expect_resolve_failure_with_code(
        r#"
        impure function BaseF
            input Real x;
            output Real y;
        algorithm
            y := x;
        end BaseF;

        function DerivedF
            extends BaseF;
        end DerivedF;

        model Test
            Real z;
        equation
            z = DerivedF(1.0);
        end Test;
    "#,
        "Test",
        "ER040",
    );
}

// =============================================================================
// TYPE-006: Operator record base
// "If A has operator record base class, B must have same one"
// =============================================================================

#[test]
fn type_006_operator_record_base_must_match() {
    expect_failure_in_phase_with_code(
        r#"
        operator record OR1
            Real x;
        end OR1;

        operator record OR2
            Real x;
        end OR2;

        model Test
            OR1 a;
            OR2 b;
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
// TYPE-007: ExternalObject identity
// "ExternalObject-derived types must match identity"
// =============================================================================

#[test]
#[ignore = "TODO(TYPE-007): enforce ExternalObject identity compatibility"]
fn type_007_external_object_identity_must_match() {
    expect_failure_in_phase_with_code(
        r#"
        class EO1
            extends ExternalObject;
            impure function constructor
                output EO1 eo;
            external "C";
            end constructor;
            impure function destructor
                input EO1 eo;
            external "C";
            end destructor;
        end EO1;

        class EO2
            extends ExternalObject;
            impure function constructor
                output EO2 eo;
            external "C";
            end constructor;
            impure function destructor
                input EO2 eo;
            external "C";
            end destructor;
        end EO2;

        model Test
            EO1 a;
            EO2 b;
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
// TYPE-008: Replaceable constraint
// "If B is not replaceable then A shall not be replaceable"
// =============================================================================

#[test]
#[ignore = "TODO(TYPE-008): reject replaceable redeclarations against non-replaceable base"]
fn type_008_nonreplaceable_constraint_propagates() {
    expect_failure_in_phase_with_code(
        r#"
        model Base
            model C
                Real x;
            end C;
            C c;
        end Base;

        model C2
            Real x;
        end C2;

        model Test
            extends Base(redeclare replaceable model C = C2);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-010: Input/output match
// "Input and output prefixes must be matched"
// =============================================================================

#[test]
fn type_010_input_output_prefixes_must_match() {
    expect_failure_in_phase_with_code(
        r#"
        model BaseType
            input Real u;
            output Real y;
        equation
            y = u;
        end BaseType;

        model BadType
            Real u;
            Real y;
        equation
            y = u;
        end BadType;

        model Base
            replaceable BaseType c;
        end Base;

        model Test
            extends Base(redeclare BadType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-012: Conditional match
// "Conditional components are only compatible with conditional components"
// =============================================================================

#[test]
fn type_012_conditional_components_must_match() {
    expect_failure_in_phase_with_code(
        r#"
        model BaseType
            parameter Boolean use_x = true;
            Real x if use_x;
        end BaseType;

        model BadType
            Real x;
        end BadType;

        model Base
            replaceable BaseType c;
        end Base;

        model Test
            extends Base(redeclare BadType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-015: Connector not input
// "Connector component must not be an input"
// =============================================================================

#[test]
fn type_015_connector_component_must_not_be_input() {
    expect_failure_in_phase_with_code(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;

        model BaseType
            Pin p;
        end BaseType;

        model BadType
            input Pin p;
        end BadType;

        model Base
            replaceable BaseType c;
        end Base;

        model Test
            extends Base(redeclare BadType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-016: Not expandable
// "Connector component must not be of expandable connector class"
// =============================================================================

#[test]
fn type_016_connector_component_must_not_be_expandable() {
    expect_failure_in_phase_with_code(
        r#"
        connector Plain
            Real v;
            flow Real i;
        end Plain;

        expandable connector Bus
            Real v;
            flow Real i;
        end Bus;

        model BaseType
            Plain p;
        end BaseType;

        model BadType
            Bus p;
        end BadType;

        model Base
            replaceable BaseType c;
        end Base;

        model Test
            extends Base(redeclare BadType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-019: Func output order
// "All public outputs are compatible in declared order"
// =============================================================================

#[test]
fn type_019_function_output_order_preserved() {
    expect_success(
        r#"
        function Pair
            input Real x;
            output Real a;
            output Integer b;
        algorithm
            a := x;
            b := integer(x);
        end Pair;

        model Test
            Real a;
            Integer b;
        algorithm
            (a, b) := Pair(1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// TYPE-022: Transitively non-replaceable
// "If B is transitively non-replaceable then A must be transitively non-replaceable"
// =============================================================================

#[test]
fn type_022_transitively_non_replaceable_required() {
    expect_failure_in_phase_with_code(
        r#"
        model BaseType
            Real x;
        end BaseType;

        model BadType
            replaceable Real x;
        end BadType;

        model Base
            replaceable BaseType c;
        end Base;

        model Test
            extends Base(redeclare BadType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-023: No other elements
// "No extra interface elements when constraining type is transitively non-replaceable"
// =============================================================================

#[test]
fn type_023_nonreplaceable_interface_no_extra_elements() {
    expect_failure_in_phase_with_code(
        r#"
        model BaseType
            Real x;
        end BaseType;

        model BadType
            Real x;
            Real extra;
        end BadType;

        model Base
            replaceable BaseType c;
        end Base;

        model Test
            extends Base(redeclare BadType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-024: Flow/stream match
// "flow/stream prefixes should be matched for compatibility"
// =============================================================================

#[test]
fn type_024_flow_stream_prefixes_must_match() {
    expect_failure_in_phase_with_code(
        r#"
        connector C1
            Real v;
            flow Real i;
        end C1;

        connector C2
            Real v;
            stream Real i;
            flow Real j;
        end C2;

        model BaseType
            C1 p;
        end BaseType;

        model BadType
            C2 p;
        end BadType;

        model Base
            replaceable BaseType c;
        end Base;

        model Test
            extends Base(redeclare BadType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-025: Inner/outer match
// "inner/outer prefixes should be matched"
// =============================================================================

#[test]
fn type_025_inner_outer_prefixes_must_match() {
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
// TYPE-026: Final semantic match
// "If B is final, A must also be final and semantically equivalent"
// =============================================================================

#[test]
fn type_026_final_semantics_must_match() {
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
// TYPE-027: Conditional contents
// "Conditional components must have equivalent condition contents"
// =============================================================================

#[test]
fn type_027_conditional_component_conditions_must_match() {
    expect_failure_in_phase_with_code(
        r#"
        model BaseType
            parameter Boolean c = true;
            Real x if c;
        end BaseType;

        model BadType
            parameter Boolean d = true;
            Real x if d;
        end BadType;

        model Base
            replaceable BaseType c;
        end Base;

        model Test
            extends Base(redeclare BadType c);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI027",
    );
}

// =============================================================================
// TYPE-035: Operator record consistency
// "if/array expressions with operator records require same operator-record base"
// =============================================================================

#[test]
#[ignore = "TODO(TYPE-035): enforce operator-record compatibility in if/array expressions"]
fn type_035_operator_record_consistency_in_if_expression() {
    expect_failure_in_phase_with_code(
        r#"
        operator record OR1
            Real x;
        end OR1;

        operator record OR2
            Real x;
        end OR2;

        model Test
            OR1 a;
            OR2 b;
            OR1 z;
            Boolean c;
        equation
            c = true;
            z = if c then a else b;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
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
