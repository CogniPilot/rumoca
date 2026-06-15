//! DECL (Declaration) contract tests - MLS §4
//!
//! Tests for the 36 declaration contracts defined in SPEC_0022.

use rumoca_compile::compile::FailedPhase;
use rumoca_contracts::test_support::{
    expect_balanced, expect_compile_failure, expect_failure_in_phase_with_code,
    expect_parse_err_with_code, expect_parse_ok, expect_resolve_failure_with_code, expect_success,
};

// =============================================================================
// DECL-001: Name uniqueness
// "Name shall not have the same name as any other element"
// =============================================================================

#[test]
fn decl_001_rejects_duplicate_names() {
    expect_parse_err_with_code(
        r#"
        model Test
            Real x;
            Real x;
        equation
            x = 1;
        end Test;
    "#,
        "EP001",
    );
}

#[test]
fn decl_001_distinct_names_ok() {
    expect_success(
        r#"
        model Test
            Real x;
            Real y;
        equation
            x = 1;
            y = 2;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn decl_replaceable_argument_in_class_modification_parses() {
    expect_parse_ok(
        r#"
        model DefaultVariant
            Real x;
        end DefaultVariant;

        model Base
            replaceable model Variant = DefaultVariant;
        end Base;

        model Test
            Base base(replaceable model Variant = DefaultVariant);
        end Test;
    "#,
    );
}

// =============================================================================
// DECL-002: Block connector prefixes
// "Each public connector component of a block must have prefixes input and/or output"
// =============================================================================

#[test]
fn decl_002_block_connector_needs_io_prefix() {
    expect_resolve_failure_with_code(
        r#"
        connector C
            Real v;
            flow Real i;
        end C;
        block B
            C c;
        equation
        end B;
    "#,
        "B",
        "ER020",
    );
}

#[test]
fn decl_002_allows_block_connector_with_member_level_io() {
    expect_success(
        r#"
        connector C
            input Real u;
            output Real y;
        end C;
        block B
            C c;
        equation
            c.y = c.u;
        end B;
    "#,
        "B",
    );
}

// =============================================================================
// DECL-003: Record public only
// "Only public sections are allowed in record definition"
// =============================================================================

#[test]
fn decl_003_record_no_protected() {
    expect_resolve_failure_with_code(
        r#"
        record R
        protected
            Real x;
        end R;
    "#,
        "R",
        "ER021",
    );
}

#[test]
fn decl_003_record_public_ok() {
    expect_success(
        r#"
        record R
            Real x;
            Real y;
        end R;
        model Test
            R r;
        equation
            r.x = 1.0;
            r.y = 2.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// DECL-004: Record no prefixes
// "Elements of a record shall not have prefixes input, output, inner, outer, stream, or flow"
// =============================================================================

#[test]
fn decl_004_record_no_flow() {
    expect_resolve_failure_with_code(
        r#"
        record R
            flow Real x;
        end R;
    "#,
        "R",
        "ER022",
    );
}

// =============================================================================
// DECL-005: Record component types
// "Components in a record may only be of specialized class record or type"
// =============================================================================

#[test]
fn decl_005_record_no_model_component() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Real x;
        equation
            x = 1;
        end M;
        record R
            M m;
        end R;
    "#,
        "R",
        "ER023",
    );
}

// =============================================================================
// DECL-006: Connector public only
// "Only public sections are allowed in connector definition"
// =============================================================================

#[test]
fn decl_006_connector_no_protected() {
    expect_parse_err_with_code(
        r#"
        connector C
        protected
            Real v;
        end C;
    "#,
        "EP001",
    );
}

// =============================================================================
// DECL-007: Connector no inner/outer
// "Elements of a connector shall not have prefixes inner or outer"
// =============================================================================

#[test]
fn decl_007_connector_no_inner_outer() {
    expect_resolve_failure_with_code(
        r#"
        connector C
            inner Real v;
            flow Real i;
        end C;
    "#,
        "C",
        "ER024",
    );
}

// =============================================================================
// DECL-009: Protected access
// "A protected element shall not be accessed via dot notation"
// =============================================================================

#[test]
fn decl_009_no_protected_dot_access() {
    expect_resolve_failure_with_code(
        r#"
        model A
        protected
            Real x = 1;
        end A;
        model Test
            A a;
            Real y;
        equation
            y = a.x;
        end Test;
    "#,
        "Test",
        "ER025",
    );
}

// =============================================================================
// DECL-012: Input not parameter
// "Variables with input prefix must not also have prefix parameter or constant"
// =============================================================================

#[test]
fn decl_012_input_not_parameter() {
    expect_parse_err_with_code(
        r#"
        model Test
            input parameter Real x;
        equation
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// DECL-014: Partial class error
// "Error if the type is partial in a simulation model"
// =============================================================================

#[test]
fn decl_014_partial_class_error() {
    expect_resolve_failure_with_code(
        r#"
        partial model PM
            Real x;
        end PM;
        model Test
            PM pm;
        equation
        end Test;
    "#,
        "Test",
        "ER005",
    );
}

// =============================================================================
// DECL-015: Component/class namespace
// "Component cannot have the same name as its class"
// =============================================================================

#[test]
fn decl_015_component_not_same_name_as_class() {
    expect_parse_err_with_code(
        r#"
        model Real
            Real Real;
        equation
        end Real;
    "#,
        "EP001",
    );
}

// =============================================================================
// DECL-018: Array dim evaluable
// "Array dimensions shall be scalar non-negative evaluable expressions"
// =============================================================================

#[test]
fn decl_018_array_dim_constant() {
    expect_success(
        r#"
        model Test
            parameter Integer n = 3;
            Real x[n];
        equation
            x = {1, 2, 3};
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// DECL-019: Constant declaration eq
// "Constant variables shall have declaration equation"
// =============================================================================

#[test]
fn decl_019_constant_with_value() {
    expect_success(
        r#"
        model Test
            constant Real pi = 3.14159;
            Real x;
        equation
            x = pi;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// DECL-020: Discrete der forbidden
// "It is not allowed to apply der to discrete-time variables"
// =============================================================================

#[test]
fn decl_020_no_der_on_discrete() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            discrete Real x;
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        "ER026",
    );
}

// =============================================================================
// DECL-022: Non-Real always discrete
// "Default variability for Integer/String/Boolean/enum is discrete-time"
// =============================================================================

#[test]
fn decl_022_integer_discrete() {
    expect_success(
        r#"
        model Test
            Integer n;
            Boolean flag;
        equation
            n = 1;
            flag = true;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// DECL-024: Package contents
// "Package may only contain declarations of classes and constants"
// =============================================================================

#[test]
fn decl_024_package_with_constant() {
    expect_success(
        r#"
        package P
            constant Real pi = 3.14;
        end P;
        model Test
            Real x;
        equation
            x = P.pi;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn decl_024_package_no_variables() {
    expect_parse_err_with_code(
        r#"
        package P
            Real x;
        end P;
    "#,
        "EP001",
    );
}

// =============================================================================
// DECL-032: Globally balanced
// "Simulation models must be globally balanced"
// =============================================================================

#[test]
fn decl_032_balanced_model() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = 1;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn decl_032_balanced_ode() {
    expect_balanced(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn decl_032_unbalanced_model() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x;
            Real y;
        equation
            x = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED001",
    );
}

// =============================================================================
// DECL-036: Type class contents
// "type may only be predefined types, enumerations, array of type, or classes extending from type"
// =============================================================================

#[test]
fn decl_036_type_alias() {
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

// =============================================================================
// DECL-025: Operator contents
// "Operator class may only contain declarations of functions"
// (placement: operators may only live inside operator records)
// =============================================================================

#[test]
fn decl_025_top_level_operator_rejected() {
    expect_resolve_failure_with_code(
        r#"
        operator Op
            Real x;
        end Op;
        model Test
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        "ER048",
    );
}

// =============================================================================
// DECL-027: Connector component types
// "Connector may only contain components of specialized class connector,
//  record and type"
// =============================================================================

#[test]
fn decl_027_record_component_in_connector_accepted() {
    expect_success(
        r#"
        record R
            Real v;
        end R;
        connector C
            R r;
            flow Real f;
        end C;
        model Test
            C c;
            Real x(start = 0);
        equation
            c.r.v = 1;
            der(x) = 1;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn decl_027_model_component_in_connector_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Inner
            Real x(start = 0);
        equation
            der(x) = 1;
        end Inner;
        connector C
            Inner m;
        end C;
        model Test
            C c;
        end Test;
    "#,
        "Test",
        "ER049",
    );
}

// =============================================================================
// DECL-028: Operator record base
// "An operator record can only extend from an operator record"
// =============================================================================

#[test]
fn decl_028_operator_record_extending_plain_record_rejected() {
    expect_resolve_failure_with_code(
        r#"
        record R
            Real x;
        end R;
        operator record OR
            extends R;
        end OR;
        model Test
            OR o(x = 1);
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        "ER050",
    );
}

// =============================================================================
// DECL-030: Stream record members
// "Members of record may not have stream type prefix"
// =============================================================================

#[test]
fn decl_030_stream_prefix_in_record_rejected() {
    expect_resolve_failure_with_code(
        r#"
        record R
            stream Real s;
        end R;
        model Test
            R r(s = 1);
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        "ER064",
    );
}

// =============================================================================
// DECL-031: ExternalObject prefixes
// "ExternalObject instances may have type prefixes parameter and constant,
//  in functions also input and output"
// =============================================================================

#[test]
fn decl_031_external_object_as_model_input_rejected() {
    expect_compile_failure(
        r#"
        model EO
            extends ExternalObject;
            function constructor
                output EO eo;
                external "C";
            end constructor;
            function destructor
                input EO eo;
                external "C";
            end destructor;
        end EO;
        model Test
            input EO eo;
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// DECL-010: Type prefix scope
// "Type prefixes (flow, stream) only for connector-style declarations"
// (MLS §9.3)
// =============================================================================

#[test]
fn decl_010_flow_in_model_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            flow Real f;
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        "ER074",
    );
}

// =============================================================================
// DECL-011: Stream subtype
// "Variables with stream prefix shall be a subtype of Real" (MLS §15.1)
// =============================================================================

#[test]
fn decl_011_boolean_stream_rejected() {
    expect_resolve_failure_with_code(
        r#"
        connector C
            stream Boolean b;
            flow Real f;
            Real e;
        end C;
        model Test
            C c;
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        "ER075",
    );
}

// =============================================================================
// DECL-016: Flow primitive types
// "Primitive elements with flow prefix shall be subtype of Real or Integer"
// (MLS §9.3)
// =============================================================================

#[test]
fn decl_016_boolean_flow_rejected() {
    expect_resolve_failure_with_code(
        r#"
        connector C
            flow Boolean b;
            Real e;
        end C;
        model Test
            C c;
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
        "ER076",
    );
}

// =============================================================================
// DECL-013: Only type, record, operator record, connector, ExternalObject may
// have declaration equations
// =============================================================================

#[test]
fn decl_013_declaration_equation_on_model_component_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            model Sub
                Real x;
            equation
                x = 1;
            end Sub;
            Sub s = Sub();
        end M;
    "#,
        "M",
        "ER093",
    );
}

// =============================================================================
// DECL-017: When flow/input/output applied to structured component, no element
// may have these or stream prefix
// =============================================================================

#[test]
fn decl_017_record_element_with_input_prefix_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            record R
                input Real a;
            end R;
            input R r;
        end M;
    "#,
        "M",
        "ER022",
    );
}

// =============================================================================
// DECL-026: Operator may only be placed directly in an operator record
// =============================================================================

#[test]
fn decl_026_operator_function_outside_operator_record_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            function '+'
                input Real a;
                input Real b;
                output Real c;
            algorithm
                c := a + b;
            end '+';
            Real x = 1 + 2;
        end M;
    "#,
        "M",
        "ER092",
    );
}

// =============================================================================
// DECL-029: Operator record cannot extend from any of its enclosing scopes
// =============================================================================

#[test]
fn decl_029_operator_record_extends_enclosing_rejected() {
    expect_resolve_failure_with_code(
        r#"
        package P
            operator record Inner
                extends P;
                Real re;
            end Inner;
            model M
                Real x = 1;
            end M;
        end P;
    "#,
        "P.M",
        "ER092",
    );
}

// =============================================================================
// DECL-034: Not legal to combine equations/algorithms/components with extends
// from array class or simple type
// =============================================================================

#[test]
fn decl_034_extends_builtin_with_components_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            type T
                extends Real;
                constant Real extra = 1;
            end T;
            T t = 1.0;
        end M;
    "#,
        "M",
        "ER094",
    );
}

// =============================================================================
// DECL-008: Not legal to extend from an operator record except as short class
// definition
// =============================================================================

#[test]
fn decl_008_long_form_extends_operator_record_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            operator record OR
                Real re;
            end OR;
            record Bad
                extends OR;
            end Bad;
            Bad b(re = 1);
        end M;
    "#,
        "M",
        "ER091",
    );
}

#[test]
fn decl_008_short_form_extends_operator_record_accepted() {
    expect_success(
        r#"
        model M
            operator record OR
                Real re;
            end OR;
            record Ok = OR;
            Ok b(re = 1);
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// DECL-023: Short class definition inheriting from partial class will be
// partial regardless of prefix
// =============================================================================

#[test]
fn decl_023_short_class_of_partial_instantiation_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            partial model P
                Real x = 1;
            end P;
            model S = P;
            S s;
        end M;
    "#,
        "M",
        "ER005",
    );
}

// =============================================================================
// DECL-033: Variable assigned in when-clause shall not be defined in
// sub-component of model or block
// =============================================================================

#[test]
fn decl_033_when_assigns_subcomponent_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            model Sub
                discrete Real x(start = 0);
            end Sub;
            Sub s;
        equation
            when time > 1 then
                s.x = 2;
            end when;
        end M;
    "#,
        "M",
        "ER108",
    );
}
