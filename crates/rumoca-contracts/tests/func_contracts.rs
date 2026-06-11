//! FUNC (Function) contract tests - MLS §12
//!
//! Tests for the 35 function contracts defined in SPEC_0022.

use rumoca_compile::compile::FailedPhase;
use rumoca_contracts::test_support::{
    expect_failure_in_phase_with_code, expect_parse_err_with_code, expect_parse_ok,
    expect_resolve_failure_with_code, expect_success,
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
// "der, initial, terminal, sample, pre, edge, change, reinit, delay,
//  cardinality, inStream, actualStream"
// =============================================================================

#[test]
fn func_010_builtin_math_call_ok() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := sin(x);
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
fn func_010_initial_forbidden_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            output Boolean y;
        algorithm
            y := initial();
        end F;
    "#,
        "F",
        "ER056",
    );
}

#[test]
fn func_010_sample_forbidden_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            output Boolean y;
        algorithm
            y := sample(0, 1);
        end F;
    "#,
        "F",
        "ER056",
    );
}

// =============================================================================
// FUNC-011: No Clock components
// "Function may not contain components of type Clock"
// =============================================================================

#[test]
fn func_011_non_clock_component_ok() {
    expect_success(
        r#"
        function F
            output Real y;
        protected
            Boolean tick;
        algorithm
            tick := true;
            y := if tick then 1.0 else 0.0;
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

#[test]
fn func_011_clock_component_fails() {
    expect_resolve_failure_with_code(
        r#"
        type MyClock = Clock;
        function F
            output Real y;
        protected
            MyClock clk;
        algorithm
            y := 0.0;
        end F;
    "#,
        "F",
        "ER061",
    );
}

// =============================================================================
// FUNC-012: No inner/outer
// "Function elements shall not have prefixes inner or outer"
// =============================================================================

#[test]
fn func_012_local_component_without_inner_outer_ok() {
    expect_success(
        r#"
        function F
            output Real y;
        protected
            Real local;
        algorithm
            local := 1.0;
            y := local;
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

#[test]
fn func_012_inner_prefix_in_function_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            output Real y;
        protected
            inner Real local;
        algorithm
            y := 0.0;
        end F;
    "#,
        "F",
        "ER060",
    );
}

#[test]
fn func_012_outer_prefix_in_function_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            output Real y;
        protected
            outer Real local;
        algorithm
            y := 0.0;
        end F;
    "#,
        "F",
        "ER060",
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
// FUNC-015: Component types
// "Function must not contain model, block, operator, or connector components"
// =============================================================================

#[test]
fn func_015_record_component_ok() {
    expect_success(
        r#"
        record R
            Real x;
        end R;
        function F
            input Real u;
            output Real y;
        protected
            R r;
        algorithm
            r.x := u;
            y := r.x;
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
fn func_015_connector_component_fails() {
    expect_resolve_failure_with_code(
        r#"
        connector Pin
            Real v;
        end Pin;
        function F
            output Real y;
        protected
            Pin p;
        algorithm
            y := 0.0;
        end F;
    "#,
        "F",
        "ER062",
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

// =============================================================================
// FUNC-019: Named arg slot error
// "Error if named argument slot is already filled" (MLS §12.4.1)
// =============================================================================

#[test]
fn func_019_named_slot_after_positional_accepted() {
    expect_success(
        r#"
        function F
            input Real a;
            input Real b = 1;
            output Real y;
        algorithm
            y := a + b;
        end F;
        model Test
            Real x(start = 0, fixed = true);
        equation
            der(x) = F(1, b = 2);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn func_019_named_arg_for_filled_slot_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        function F
            input Real a;
            input Real b = 1;
            output Real y;
        algorithm
            y := a + b;
        end F;
        model Test
            Real x(start = 0);
        equation
            der(x) = F(1, a = 2);
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF016",
    );
}

// =============================================================================
// FUNC-020: Unfilled slots error
// "Error if any unfilled slots remain after argument processing" (MLS §12.4.1)
// =============================================================================

#[test]
fn func_020_unfilled_mandatory_input_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        function G
            input Real a;
            input Real b;
            output Real y;
        algorithm
            y := a + b;
        end G;
        model Test
            Real x(start = 0);
        equation
            der(x) = G(1);
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF016",
    );
}

// =============================================================================
// FUNC-003: Output variables must be assigned inside the function
// =============================================================================

#[test]
fn func_003_unassigned_output_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            function F
                input Real u;
                output Real y;
            protected
                Real tmp;
            algorithm
                tmp := u;
            end F;
            Real z = F(1.0);
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF013",
    );
}

// =============================================================================
// FUNC-013: For simulation, function shall not be partial
// =============================================================================

#[test]
fn func_013_partial_function_call_rejected() {
    // Partial functions are excluded from executable-function collection, so
    // the call is reported as unresolved when lowering to the DAE. A precise
    // flatten-time diagnostic is not possible because flatten legitimately
    // converts partial base functions that redeclares later make concrete
    // (the MSL Media pattern).
    expect_failure_in_phase_with_code(
        r#"
        model M
            partial function F
                input Real u;
                output Real y;
            end F;
            Real z = F(1.0);
        end M;
    "#,
        "M",
        FailedPhase::ToDae,
        "ED005",
    );
}

// =============================================================================
// FUNC-016: Functions shall not be used in connections
// =============================================================================

#[test]
fn func_016_function_call_in_connect_rejected() {
    expect_parse_err_with_code(
        r#"
        model M
            connector C
                Real e;
                flow Real f;
            end C;
            function F
                output Real y;
            algorithm
                y := 1;
            end F;
            C c1;
        equation
            connect(F(), c1);
        end M;
    "#,
        "EP001",
    );
}

// =============================================================================
// FUNC-021: If function declared impure, any extending function shall be
// declared impure
// =============================================================================

#[test]
fn func_021_pure_function_extends_impure_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            impure function FBase
                input Real u;
                output Real y;
            algorithm
                y := u;
            end FBase;
            function FExt
                extends FBase;
            end FExt;
            Real z(start = 0);
        equation
            when time > 1 then
                z = FExt(1.0);
            end when;
        end M;
    "#,
        "M",
        "ER089",
    );
}

// =============================================================================
// FUNC-022: Impure only allowed from: impure function, when-equation/statement,
// pure(), initial equations/algorithms
// =============================================================================

#[test]
fn func_022_impure_call_in_continuous_equation_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            impure function F
                input Real u;
                output Real y;
            algorithm
                y := u;
            end F;
            Real z;
        equation
            z = F(time);
        end M;
    "#,
        "M",
        "ER088",
    );
}

// =============================================================================
// FUNC-034: Default values for inputs shall not depend on non-input variables
// in the function
// =============================================================================

#[test]
fn func_034_input_default_depends_on_output_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            function F
                input Real u = y;
                output Real y;
            algorithm
                y := u;
            end F;
            Real z = F();
        end M;
    "#,
        "M",
        "ER090",
    );
}

// =============================================================================
// FUNC-004: Error if impure function call is part of a system of equations
// =============================================================================

#[test]
fn func_004_impure_call_in_equation_system_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            impure function F
                input Real u;
                output Real y;
            algorithm
                y := u;
            end F;
            Real a;
            Real b;
        equation
            a + b = 1;
            b = F(a);
        end M;
    "#,
        "M",
        "ER088",
    );
}

// =============================================================================
// FUNC-008: Higher variability cannot assign to lower variability
// =============================================================================

#[test]
fn func_008_assign_to_parameter_in_algorithm_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            parameter Real p = 1;
            Real x = time;
        algorithm
            p := x;
        end M;
    "#,
        "M",
        "ER102",
    );
}

// =============================================================================
// FUNC-018: Relative ordering between input formal parameter declarations is
// significant
// =============================================================================

#[test]
fn func_018_named_arguments_reordered_accepted() {
    expect_success(
        r#"
        model M
            function F
                input Real a;
                input Real b;
                output Real y;
            algorithm
                y := a - b;
            end F;
            Real z = F(b = 1.0, a = 2.0);
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// FUNC-023: Binding execution order must not have cycles
// =============================================================================

#[test]
fn func_023_cyclic_function_bindings_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            function F
                input Real u;
                output Real y;
            protected
                Real a = b + 1;
                Real b = a + 1;
            algorithm
                y := a + u;
            end F;
            Real z = F(1.0);
        end M;
    "#,
        "M",
        "ER007",
    );
}

// =============================================================================
// FUNC-029: Conditional components in target record: it is an error
// =============================================================================

#[test]
fn func_029_constructor_for_conditional_record_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            parameter Boolean has = true;
            record R
                Real x;
                Real y if has;
            end R;
            R r = R(1.0, 2.0);
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF018",
    );
}

// =============================================================================
// FUNC-033: Function type parameter cannot be type-specifier of record or
// enumeration
// =============================================================================

#[test]
fn func_033_function_alias_of_record_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            record R
                Real x;
            end R;
            function Apply
                input F f;
                input Real u;
                output Real y;
                replaceable function F = R;
            algorithm
                y := u;
            end Apply;
            Real z = 1;
        end M;
    "#,
        "M",
        "ER091",
    );
}

// =============================================================================
// FUNC-030: Derivative output list shall not be empty
// =============================================================================

#[test]
fn func_030_derivative_function_without_outputs_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            function F
                input Real u;
                output Real y;
            algorithm
                y := u * u;
                annotation(derivative = dF);
            end F;
            function dF
                input Real u;
                input Real du;
            algorithm
                assert(true, "no outputs");
            end dF;
            Real z = F(time);
        end M;
    "#,
        "M",
        "ER120",
    );
}

// =============================================================================
// FUNC-031: zeroDerivative applies only if inputVar is independent of
// differentiation variables
// =============================================================================

#[test]
fn func_031_zero_derivative_names_non_input_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            function F
                input Real u;
                output Real y;
            algorithm
                y := u * u;
                annotation(derivative(zeroDerivative = w) = dF);
            end F;
            function dF
                input Real u;
                input Real du;
                output Real dy;
            algorithm
                dy := 2 * u * du;
            end dF;
            Real z = F(time);
        end M;
    "#,
        "M",
        "ER120",
    );
}

// =============================================================================
// FUNC-032: External function without explicit pure/impure declaration is
// deprecated
// =============================================================================

#[test]
fn func_032_external_function_without_purity_warns() {
    rumoca_contracts::test_support::expect_compile_warning(
        r#"
        model M
            function F
                input Real u;
                output Real y;
            external "C" y = my_func(u);
            end F;
            Real z(start = 0);
        equation
            when time > 1 then
                z = F(1.0);
            end when;
        end M;
    "#,
        "M",
        "WR001",
    );
}

// =============================================================================
// FUNC-027: Array arguments have to be the same size
// =============================================================================

#[test]
fn func_027_vectorized_args_size_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            function F
                input Real u;
                input Real v;
                output Real y;
            algorithm
                y := u + v;
            end F;
            Real a[2] = {1, 2};
            Real b[3] = {1, 2, 3};
            Real z[2];
        equation
            z = F(a, b);
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF016",
    );
}
