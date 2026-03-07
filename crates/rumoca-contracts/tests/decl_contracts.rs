//! DECL (Declaration) contract tests - MLS §4
//!
//! Tests for the 36 declaration contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_parse_err_with_code,
    expect_parse_ok, expect_resolve_failure_with_code, expect_success,
};
use rumoca_session::compile::FailedPhase;

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
