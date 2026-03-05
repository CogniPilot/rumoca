//! FUNC (Function) contract tests - MLS §12
//!
//! Tests for the 35 function contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_parse_err_with_code, expect_parse_ok, expect_resolve_failure_with_code, expect_success,
};

// =============================================================================
// FUNC-001: Input/output only
// "Each public component must have prefix input or output"
// =============================================================================

#[test]
fn func_001_input_output_ok() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x * 2;
        end F;
        model Test
            Real z;
        equation
            z = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn func_001_public_no_prefix_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            Real x;
            output Real y;
        algorithm
            y := x;
        end F;
    "#,
        "F",
        "ER013",
    );
}

// =============================================================================
// FUNC-002: Input read-only
// "Input formal parameters are read-only after being bound"
// =============================================================================

#[test]
fn func_002_input_readonly() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            x := 5;
            y := x;
        end F;
    "#,
        "F",
        "ER014",
    );
}

// =============================================================================
// FUNC-006: No equations
// "Function shall not have equations, shall not have initial algorithms"
// =============================================================================

#[test]
fn func_006_no_equations_in_function() {
    expect_parse_err_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        equation
            y = x;
        end F;
    "#,
        "EP001",
    );
}

// =============================================================================
// FUNC-003: Output assignment
// "Output variables must be assigned inside the function or have external function interface"
// =============================================================================

#[test]
fn func_003_output_assigned_in_algorithm_ok() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x * 2.0;
        end F;
        model Test
            Real z;
        equation
            z = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn func_003_unassigned_output_without_external_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            // y is never assigned
            x := x;
        end F;
        model Test
            Real z;
        equation
            z = F(2.0);
        end Test;
    "#,
        "Test",
        "ER032",
    );
}

// =============================================================================
// FUNC-004: Impure restrictions
// "Error if impure function call is part of a system of equations"
// =============================================================================

#[test]
fn func_004_impure_call_in_equation_system_fails() {
    expect_resolve_failure_with_code(
        r#"
        impure function F
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F;

        model Test
            Real y;
        equation
            y = F(1.0);
        end Test;
    "#,
        "Test",
        "ER041",
    );
}

#[test]
fn func_004_pure_call_in_equation_system_ok() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F;

        model Test
            Real y;
        equation
            y = F(1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-007: No when-statements
// "Function body shall not contain when-statements"
// =============================================================================

#[test]
fn func_007_no_when_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            when x > 0 then
                y := 1;
            end when;
        end F;
    "#,
        "F",
        "ER015",
    );
}

// =============================================================================
// FUNC-010: Forbidden operators
// "der, initial, terminal, sample, pre, edge, change, reinit, delay, cardinality, inStream, actualStream"
// =============================================================================

#[test]
fn func_010_forbidden_sample_operator_in_function_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := sample(x, 1.0);
        end F;
        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
        "ER039",
    );
}

#[test]
fn func_010_allowed_operator_usage_in_function_ok() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := abs(x);
        end F;
        model Test
            Real z;
        equation
            z = F(-1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-011: No Clock components
// "Function may not contain components of type Clock"
// =============================================================================

#[test]
fn func_011_clock_component_in_function_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        protected
            Clock c;
        algorithm
            y := x;
        end F;
        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
        "ER036",
    );
}

#[test]
fn func_011_non_clock_components_ok() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        protected
            Real c;
        algorithm
            c := x;
            y := c;
        end F;
        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-012: No inner/outer
// "Function elements shall not have prefixes inner or outer"
// =============================================================================

#[test]
fn func_012_inner_component_in_function_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
            inner Real cache;
        algorithm
            cache := x;
            y := cache;
        end F;
        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
        "ER035",
    );
}

#[test]
fn func_012_no_inner_outer_prefixes_ok() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        protected
            Real cache;
        algorithm
            cache := x;
            y := cache;
        end F;
        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-013: Non-partial for simulation
// "For simulation, function shall not be partial"
// =============================================================================

#[test]
fn func_013_partial_function_call_fails_for_simulation() {
    expect_resolve_failure_with_code(
        r#"
        partial function F
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F;
        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
        "ER037",
    );
}

#[test]
fn func_013_non_partial_function_call_ok() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F;
        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-014: Single algorithm/external
// "Function can have at most one algorithm section or one external function interface"
// =============================================================================

#[test]
fn func_014_single_algorithm() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x * 2;
        end F;
        model Test
            Real z;
        equation
            z = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-015: Component types
// "Function must not contain model, block, operator, or connector components"
// =============================================================================

#[test]
fn func_015_function_component_cannot_be_connector_type() {
    expect_resolve_failure_with_code(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        function F
            input Real x;
            output Real y;
        protected
            Pin p;
        algorithm
            y := x;
        end F;
        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
        "ER038",
    );
}

#[test]
fn func_015_function_component_can_be_builtin_scalar_type() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        protected
            Integer k;
        algorithm
            k := integer(x);
            y := k;
        end F;
        model Test
            Real z;
        equation
            z = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-016: Not in connections
// "Functions shall not be used in connections"
// =============================================================================

#[test]
fn func_016_function_call_not_allowed_in_connect_endpoints() {
    expect_parse_err_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F;

        connector Pin
            Real v;
            flow Real i;
        end Pin;

        model Test
            Pin p;
        equation
            connect(F(1.0), p);
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// FUNC-017: Return in algorithm only
// "Return statement can only be used in an algorithm section of a function"
// =============================================================================

#[test]
fn func_017_return_in_function() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            if x > 0 then
                y := x;
                return;
            end if;
            y := -x;
        end F;
        model Test
            Real z;
        equation
            z = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn func_017_return_outside_function_algorithm_fails() {
    expect_parse_err_with_code(
        r#"
        model Test
            Real x;
        equation
            return;
            x = 1.0;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// FUNC-018: Input ordering significant
// "Relative ordering between input formal parameter declarations is significant"
// =============================================================================

#[test]
fn func_018_function_input_order_is_preserved() {
    let result = expect_success(
        r#"
        function F
            input Real first;
            input Real second;
            input Real third;
            output Real y;
        algorithm
            y := first + 10 * second + 100 * third;
        end F;
        model Test
            Real z;
        equation
            z = F(1.0, 2.0, 3.0);
        end Test;
    "#,
        "Test",
    );

    let func = result
        .flat
        .functions
        .values()
        .find(|f| f.name.as_str() == "F")
        .expect("flattened function F");
    let input_names: Vec<&str> = func.inputs.iter().map(|p| p.name.as_str()).collect();
    assert_eq!(input_names, vec!["first", "second", "third"]);
}

// =============================================================================
// FUNC-019: Named arg slot error
// "Error if named argument slot is already filled"
// =============================================================================

#[test]
fn func_019_named_argument_slot_filled_by_positional_fails() {
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
            z = F(1.0, b = 2.0, a = 3.0);
        end Test;
    "#,
        "Test",
        "ER033",
    );
}

#[test]
fn func_019_named_argument_slots_unique_ok() {
    expect_success(
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
            z = F(a = 1.0, b = 2.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-020: Unfilled slots error
// "Error if any unfilled slots remain after argument processing"
// =============================================================================

#[test]
fn func_020_missing_required_input_after_argument_processing_fails() {
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

#[test]
fn func_020_default_fills_remaining_input_slot_ok() {
    expect_success(
        r#"
        function F
            input Real a;
            input Real b = 2.0;
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
    );
}

// =============================================================================
// FUNC-021: Impure inheritance
// "If function declared impure, any extending function shall be declared impure"
// =============================================================================

#[test]
fn func_021_pure_function_extending_impure_fails() {
    expect_resolve_failure_with_code(
        r#"
        impure function Base
            input Real x;
            output Real y;
        algorithm
            y := x;
        end Base;

        function Derived
            extends Base;
        end Derived;

        model Test
            Real z;
        equation
            z = 0.0;
        end Test;
    "#,
        "Test",
        "ER040",
    );
}

#[test]
fn func_021_impure_function_extending_impure_ok() {
    expect_success(
        r#"
        impure function Base
            input Real x;
            output Real y;
        algorithm
            y := x;
        end Base;

        impure function Derived
            extends Base;
        end Derived;

        model Test
            Real z;
        equation
            z = 0.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-022: Impure call scope
// "Impure only allowed from: impure function, when-equation/statement, pure(), initial equations/algorithms"
// =============================================================================

#[test]
fn func_022_impure_call_in_regular_equation_fails() {
    expect_resolve_failure_with_code(
        r#"
        impure function F
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F;

        model Test
            Real y;
        equation
            y = F(1.0);
        end Test;
    "#,
        "Test",
        "ER041",
    );
}

#[test]
fn func_022_impure_call_in_initial_equation_ok() {
    expect_success(
        r#"
        impure function F
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F;

        model Test
            Real y(start = 0.0);
        initial equation
            y = F(1.0);
        equation
            der(y) = -y;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn func_022_impure_call_inside_impure_function_ok() {
    expect_success(
        r#"
        impure function Base
            input Real x;
            output Real y;
        algorithm
            y := x;
        end Base;

        impure function Wrapper
            input Real x;
            output Real y;
        algorithm
            y := Base(x);
        end Wrapper;

        model Test
            Real y;
        equation
            y = 1.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-023: Binding no cycles
// "Binding execution order must not have cycles"
// =============================================================================

#[test]
fn func_023_function_default_bindings_cycle_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real a = b;
            input Real b = a;
            output Real y;
        algorithm
            y := a + b;
        end F;

        model Test
            Real z;
        equation
            z = F();
        end Test;
    "#,
        "Test",
        "ER042",
    );
}

#[test]
fn func_023_function_default_bindings_acyclic_ok() {
    expect_success(
        r#"
        function F
            input Real a = 1.0;
            input Real b = a + 1.0;
            output Real y;
        algorithm
            y := a + b;
        end F;

        model Test
            Real z;
        equation
            z = F();
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-024: Uninitialized error
// "Error to use or return an uninitialized variable"
// =============================================================================

#[test]
fn func_024_uninitialized_local_use_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        protected
            Real temp;
        algorithm
            y := temp + x;
        end F;

        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
        "ER043",
    );
}

#[test]
fn func_024_initialized_local_use_ok() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        protected
            Real temp;
        algorithm
            temp := x;
            y := temp + 1.0;
        end F;

        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-005: Pure guarantee
// "Pure functions always give same output for same input"
// =============================================================================

#[test]
fn func_005_pure_function_cannot_call_impure_directly() {
    expect_resolve_failure_with_code(
        r#"
        impure function Imp
            input Real x;
            output Real y;
        algorithm
            y := x;
        end Imp;

        function PureWrap
            input Real x;
            output Real y;
        algorithm
            y := Imp(x);
        end PureWrap;

        model Test
            Real z;
        equation
            z = PureWrap(1.0);
        end Test;
    "#,
        "Test",
        "ER041",
    );
}

#[test]
fn func_005_pure_function_simple_ok() {
    expect_success(
        r#"
        function PureSquare
            input Real x;
            output Real y;
        algorithm
            y := x * x;
        end PureSquare;

        model Test
            Real z;
        equation
            z = PureSquare(2.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-008: Variability constraint
// "Higher variability cannot assign to lower variability"
// =============================================================================

#[test]
fn func_008_parameter_default_from_input_fails_variability() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        protected
            parameter Real p = x;
        algorithm
            y := p;
        end F;

        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
        "ER006",
    );
}

// =============================================================================
// FUNC-009: Array dimension
// "Array dimension sizes must be given by inputs, constants, or parameter expressions"
// =============================================================================

#[test]
fn func_009_function_array_dimension_from_non_input_local_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Integer n;
            output Real y;
        protected
            Integer m = n;
            Real a[m];
        algorithm
            y := n;
        end F;

        model Test
            Real z;
        equation
            z = F(2);
        end Test;
    "#,
        "Test",
        "ER044",
    );
}

#[test]
fn func_009_function_array_dimension_from_input_ok() {
    expect_success(
        r#"
        function F
            input Integer n;
            output Real y;
        protected
            Real a[n];
        algorithm
            y := n;
        end F;

        model Test
            Real z;
        equation
            z = F(2);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-025: LHS list output
// "Left-hand side references must agree with type of corresponding output component"
// =============================================================================

#[test]
fn func_025_statement_output_target_type_mismatch_fails() {
    expect_resolve_failure_with_code(
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
            Real b;
        algorithm
            (a, b) := Pair(1.0);
        end Test;
    "#,
        "Test",
        "ER047",
    );
}

#[test]
fn func_025_statement_output_target_type_match_ok() {
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
// FUNC-026: Vectorization non-replaceable
// "Only transitively non-replaceable functions support automatic vectorization"
// =============================================================================

#[test]
fn func_026_replaceable_function_vectorization_fails() {
    expect_resolve_failure_with_code(
        r#"
        package P
            replaceable function F
                input Real x;
                output Real y;
            algorithm
                y := x;
            end F;
        end P;

        model Test
            package Medium = P;
            Real z[2];
        equation
            z = Medium.F({1.0, 2.0});
        end Test;
    "#,
        "Test",
        "ER045",
    );
}

// =============================================================================
// FUNC-027: Vectorization size match
// "Array arguments have to be the same size"
// =============================================================================

#[test]
fn func_027_vectorization_array_size_mismatch_fails() {
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
            z = F({1.0, 2.0}, {1.0, 2.0, 3.0});
        end Test;
    "#,
        "Test",
        "ER046",
    );
}

// =============================================================================
// FUNC-028: Record constructor scope
// "Record constructor can only reference records found in global scope"
// =============================================================================

#[test]
fn func_028_nested_record_constructor_fails() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            record LocalRec
                Real x;
            end LocalRec;
            LocalRec r = LocalRec(1.0);
        equation
            r.x = 1.0;
        end Test;
    "#,
        "Test",
        "ER048",
    );
}

#[test]
fn func_028_top_level_record_constructor_ok() {
    expect_success(
        r#"
        record GlobalRec
            Real x;
        end GlobalRec;

        model Test
            GlobalRec r = GlobalRec(1.0);
        equation
            r.x = 1.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-029: Record cast conditional error
// "Conditional components in target record: it is an error"
// =============================================================================

#[test]
fn func_029_record_constructor_with_conditional_component_fails() {
    expect_resolve_failure_with_code(
        r#"
        record R
            parameter Boolean useX = true;
            Real x if useX;
        end R;

        model Test
            R r = R(useX = true, x = 1.0);
        equation
            if r.useX then
                r.x = 1.0;
            end if;
        end Test;
    "#,
        "Test",
        "ER049",
    );
}

// =============================================================================
// FUNC-030: Derivative outputs non-empty
// "Derivative output list shall not be empty"
// =============================================================================

#[test]
fn func_030_derivative_annotation_on_no_output_function_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
        algorithm
            x := x;
        annotation(derivative = F_der);
        end F;

        function F_der
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F_der;

        model Test
            Real z;
        equation
            z = 1.0;
        end Test;
    "#,
        "Test",
        "ER050",
    );
}

// =============================================================================
// FUNC-031: zeroDerivative condition
// "zeroDerivative applies only if inputVar is independent of differentiation variables"
// =============================================================================

#[test]
fn func_031_zero_derivative_must_reference_input_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x;
        annotation(derivative(zeroDerivative = y) = F_der);
        end F;

        function F_der
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F_der;

        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
        "ER051",
    );
}

// =============================================================================
// FUNC-032: External purity deprecated
// "External function without explicit pure/impure declaration is deprecated"
// =============================================================================

#[test]
fn func_032_external_function_default_pure_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        external "C" y = f(x);
        end F;

        model Test
            Real z;
        equation
            z = 0.0;
        end Test;
    "#,
        "Test",
        "ER052",
    );
}

#[test]
fn func_032_external_function_explicit_impure_ok() {
    expect_success(
        r#"
        impure function F
            input Real x;
            output Real y;
        external "C" y = f(x);
        end F;

        model Test
            Real z;
        equation
            z = 0.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-033: Functional param type
// "Function type parameter cannot be type-specifier of record or enumeration"
// =============================================================================

#[test]
fn func_033_functional_param_cannot_use_record_type_specifier() {
    expect_resolve_failure_with_code(
        r#"
        record R
            Real x;
        end R;

        partial function FR
            input R r;
            output Real y;
        algorithm
            y := r.x;
        end FR;

        function F
            input FR g;
            input Real x;
            output Real y;
            R r;
        algorithm
            r.x := x;
            y := g(r);
        end F;

        model Test
            Real z;
        equation
            z = 0.0;
        end Test;
    "#,
        "Test",
        "ER053",
    );
}

// =============================================================================
// FUNC-034: Input default independence
// "Default values for inputs shall not depend on non-input variables in the function"
// =============================================================================

#[test]
fn func_034_input_default_depends_on_non_input_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real a = b;
            output Real b;
        algorithm
            b := 1.0;
        end F;

        model Test
            Real z;
        equation
            z = F();
        end Test;
    "#,
        "Test",
        "ER054",
    );
}

#[test]
fn func_034_input_default_depends_on_other_input_ok() {
    expect_success(
        r#"
        function F
            input Real a = 1.0;
            input Real b = a;
            output Real y;
        algorithm
            y := a + b;
        end F;

        model Test
            Real z;
        equation
            z = F();
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-035: Derivative ordering
// "Most restrictive derivative annotations should be written first"
// =============================================================================

#[test]
fn func_035_derivative_annotations_must_be_most_restrictive_first() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x;
        annotation(
            derivative(order = 1) = F_der1,
            derivative(zeroDerivative = x) = F_der2
        );
        end F;

        function F_der1
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F_der1;

        function F_der2
            input Real x;
            output Real y;
        algorithm
            y := x;
        end F_der2;

        model Test
            Real z;
        equation
            z = F(1.0);
        end Test;
    "#,
        "Test",
        "ER055",
    );
}

// =============================================================================
// Function integration tests
// =============================================================================

#[test]
fn func_basic_call() {
    expect_success(
        r#"
        function Square
            input Real x;
            output Real y;
        algorithm
            y := x * x;
        end Square;
        model Test
            Real x;
            Real y;
        equation
            x = 3.0;
            y = Square(x);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn func_with_protected() {
    expect_parse_ok(
        r#"
        function F
            input Real x;
            output Real y;
        protected
            Real temp;
        algorithm
            temp := x * x;
            y := temp + 1;
        end F;
    "#,
    );
}

#[test]
fn func_multiple_outputs() {
    expect_parse_ok(
        r#"
        function SinCos
            input Real x;
            output Real s;
            output Real c;
        algorithm
            s := sin(x);
            c := cos(x);
        end SinCos;
    "#,
    );
}

#[test]
fn func_der_class_specifier_short_form_parses() {
    expect_parse_ok(
        r#"
        function f
            input Real x;
            output Real y;
        algorithm
            y := x;
        end f;

        function f_der = der(f, x);
    "#,
    );
}

#[test]
fn func_default_input() {
    expect_parse_ok(
        r#"
        function F
            input Real x;
            input Real scale = 1.0;
            output Real y;
        algorithm
            y := x * scale;
        end F;
    "#,
    );
}
