//! ALG (Algorithm) contract tests - MLS §11
//!
//! Tests for the 17 algorithm contracts defined in SPEC_0022.

use rumoca_compile::compile::FailedPhase;
use rumoca_contracts::test_support::{
    expect_compile_failure, expect_failure_in_phase_with_code, expect_parse_err_with_code,
    expect_parse_err_with_only_code, expect_parse_ok, expect_resolve_failure_with_code,
    expect_success,
};

// =============================================================================
// ALG-001: No equation syntax
// "Equation equality = shall not be used in an algorithm section"
// =============================================================================

#[test]
fn alg_001_assignment_in_algorithm() {
    // := is correct in algorithm sections
    expect_success(
        r#"
        model Test
            Real x(start = 0);
            Real y;
        algorithm
            y := x + 1;
        equation
            der(x) = -y;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn alg_001_no_equality_in_algorithm() {
    expect_parse_err_with_code(
        r#"
        model Test
            Real x;
            Real y;
        algorithm
            y = x + 1;
        equation
            x = 1;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// ALG-005: While scalar Boolean
// "Expression of while-statement shall be a scalar Boolean expression"
// =============================================================================

#[test]
fn alg_005_while_boolean() {
    expect_success(
        r#"
        function F
            input Integer n;
            output Integer result;
        protected
            Integer i;
        algorithm
            result := 0;
            i := 0;
            while i < n loop
                result := result + i;
                i := i + 1;
            end while;
        end F;
        model Test
            Integer y;
        equation
            y = F(5);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn alg_005_while_boolean_rejected_for_non_boolean_expr() {
    expect_compile_failure(
        r#"
        model Test
            Real x;
            Real y;
        algorithm
            while 1 loop
                y := y + 1;
            end while;
        equation
            y = x;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ALG-007: When not in function
// "When-statement shall not be used inside a function class"
// =============================================================================

#[test]
fn alg_007_no_when_in_function() {
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
// ALG-008: When not in initial
// "When-statement shall not occur inside an initial algorithm"
// =============================================================================

#[test]
fn alg_008_when_in_initial_algorithm_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real y;
        initial algorithm
            when y > 0 then
                y := 1;
            end when;
        end Test;
    "#,
        "Test",
        "ER018",
    );
}

#[test]
fn alg_007_when_in_model_algorithm() {
    expect_success(
        r#"
        model Test
            input Real x;
            Real y;
            algorithm
                y := 0;
                when x > 1 then
                    y := 1;
                end when;
            end Test;
        "#,
        "Test",
    );
}

// =============================================================================
// ALG-009: When no nesting
// "When-statement cannot be nested inside another when-statement"
// =============================================================================

#[test]
fn alg_009_no_nested_when_statements() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            discrete Real x(start = 0);
            discrete Real y(start = 0);
        algorithm
            when time > 1 then
                when time > 2 then
                    x := 1;
                end when;
                y := 1;
            end when;
        end Test;
    "#,
        "Test",
        "ER016",
    );
}

// =============================================================================
// ALG-010: When not in control
// "When-statements shall not occur inside while/for/if in algorithms"
// =============================================================================

#[test]
fn alg_010_when_in_for_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real y;
            Integer i;
        algorithm
            for i in 1:3 loop
                when y > 0 then
                    y := 1;
                end when;
            end for;
        equation
            y = 1;
        end Test;
    "#,
        "Test",
        "ER042",
    );
}

#[test]
fn alg_009_when_not_nested() {
    expect_success(
        r#"
        model Test
            discrete Real x(start = 0);
            discrete Real y(start = 0);
            algorithm
                when time > 1 then
                    x := 1;
                end when;
                when time > 2 then
                    y := 1;
                end when;
            end Test;
        "#,
        "Test",
    );
}

// =============================================================================
// ALG-012: break scope
// "break can only be used in while or for loop"
// =============================================================================

#[test]
fn alg_012_break_in_for() {
    expect_success(
        r#"
        function F
            input Integer n;
            output Integer result;
        algorithm
            result := 0;
            for i in 1:n loop
                if i > 5 then
                    break;
                end if;
                result := result + i;
            end for;
        end F;
        model Test
            Integer y;
        equation
            y = F(10);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn alg_012_break_outside_loop_fails() {
    expect_compile_failure(
        r#"
        function F
            input Integer n;
            output Integer result;
        algorithm
            break;
            result := 0;
        end F;
        "#,
        "F",
    );
}

// =============================================================================
// ALG-013: return scope
// "return can only be used inside functions"
// =============================================================================

#[test]
fn alg_013_return_in_function() {
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
            Real y;
        equation
            y = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn alg_013_return_outside_function_fails() {
    expect_compile_failure(
        r#"
        model Test
            Real x;
        algorithm
            return;
            x := 1;
        end Test;
        "#,
        "Test",
    );
}

// =============================================================================
// ALG-016: For range fixed
// "For-statement range expressions are evaluated once before entering loop"
// =============================================================================

#[test]
fn alg_016_for_range() {
    expect_success(
        r#"
        model Test
            Real x[5];
        algorithm
            for i in 1:5 loop
                x[i] := i * 1.0;
            end for;
        equation
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// Algorithm integration tests
// =============================================================================

#[test]
fn alg_basic_algorithm_section() {
    expect_success(
        r#"
        model Test
            Real x(start = 0);
            Real y;
        algorithm
            if x > 0 then
                y := 1;
            else
                y := -1;
            end if;
        equation
            der(x) = -y;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn alg_for_loop_algorithm() {
    expect_success(
        r#"
        model Test
            Real x[3];
            Real total;
        algorithm
            total := 0;
            for i in 1:3 loop
                x[i] := i * 2.0;
                total := total + x[i];
            end for;
        equation
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ALG-014: terminate not in function
// "terminate-statement shall not be used in functions" (MLS §8.3.8)
// =============================================================================

#[test]
fn alg_014_terminate_in_function_rejected() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x;
            terminate("not allowed here");
        end F;
        model Test
            Real x(start = 0);
        equation
            der(x) = F(1);
        end Test;
    "#,
        "Test",
        "ER056",
    );
}

// =============================================================================
// ALG-011: When discrete Boolean
// "Expression of when-statement shall be discrete-time Boolean" (MLS §8.3.5)
// =============================================================================

#[test]
fn alg_011_non_boolean_when_condition_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x(start = 0);
            discrete Real d;
        equation
            der(x) = 1;
            when x then
                d = 1;
            end when;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

// =============================================================================
// ALG-002: Only type, record, operator record, connector may appear as
// left-hand-side
// =============================================================================

#[test]
fn alg_002_assign_to_model_instance_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            model Sub
                Real x = 1;
            end Sub;
            Sub a;
            Sub b;
        algorithm
            a := b;
        end M;
    "#,
        "M",
        "ER111",
    );
}

// =============================================================================
// ALG-003: If for-statement contains event-generating expressions, index shall
// be evaluable
// =============================================================================

#[test]
fn alg_003_event_generating_for_with_noneval_range_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Real x = time;
            Integer n(start = 1);
            Real acc;
        algorithm
            acc := 0;
            for i in 1:integer(x) loop
                acc := acc + (if x > 0.5 then 1 else 0);
            end for;
        end M;
    "#,
        "M",
        "ER115",
    );
}

// =============================================================================
// ALG-006: Event-generating expressions not allowed in while condition or body
// =============================================================================

#[test]
fn alg_006_event_generating_while_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Real x = time;
            Real acc;
        algorithm
            acc := 0;
            while acc < x loop
                acc := acc + 1;
            end while;
        end M;
    "#,
        "M",
        "ER114",
    );
}

// =============================================================================
// ALG-004: Omitted iterator ranges require checked inference from qualifying
// subscript occurrences (MLS 3.7 §11.2.2.1). That inference does not exist, so
// the parser refuses the omission with `EP004` in every syntax that can carry
// an iterator. Only the omission is refused: the whole-array restriction the
// rule states is scoped to omitted ranges, so explicit-range whole-array
// assignment stays legal and must keep compiling.
//
// The registry row stays `Partial`. What these cases bound is the parse-time
// refusal of the omission. Range inference, the identical-range agreement rule,
// and the whole-array restriction that depends on it are all unimplemented and
// have no owner yet.
// =============================================================================

/// Acceptance contract for `EP004` (SPEC_0008 "Acceptance Contract Before
/// Rejection"): an explicit `in <range>` remains accepted in every syntax that
/// admits an iterator, so the refusal cannot have widened past the omission.
///
/// This is asserted at the parse boundary because the parse boundary is where
/// the single `ForIndex` issuer lives and where the refusal is decided. It
/// fails against a converter that rejects, or mis-detects, a present range in
/// any of the four forms.
#[test]
fn alg_004_explicit_ranges_are_accepted_in_every_iterator_syntax() {
    // for-statement
    expect_parse_ok(
        r#"
        function F
            input Real u;
            output Real y;
        protected
            Real a[3];
        algorithm
            for i in 1:3 loop
                a[i] := u;
            end for;
            y := a[1];
        end F;
    "#,
    );

    // for-equation
    expect_parse_ok(
        r#"
        model M
            Real a[3];
        equation
            for i in 1:3 loop
                a[i] = i;
            end for;
        end M;
    "#,
    );

    // array comprehension
    expect_parse_ok(
        r#"
        model M
            parameter Real a[3] = {1.0*i for i in 1:3};
        end M;
    "#,
    );

    // reduction argument
    expect_parse_ok(
        r#"
        model M
            parameter Real a[3] = {1.0, 2.0, 3.0};
            Real s = sum(a[i] for i in 1:3);
        end M;
    "#,
    );

    // several iterators in one clause, all explicit
    expect_parse_ok(
        r#"
        model M
            Real a[2,2];
        equation
            for i in 1:2, j in 1:2 loop
                a[i,j] = i + j;
            end for;
        end M;
    "#,
    );
}

#[test]
fn alg_004_implicit_statement_range_fails_closed_until_supported() {
    expect_parse_err_with_only_code(
        r#"
        model M
            function F
                input Real u;
                output Real y;
            protected
                Real a[3];
            algorithm
                for i loop
                    a := fill(u, 3);
                    a[i] := u;
                end for;
                y := a[1];
            end F;
            Real z = F(1.0);
        end M;
    "#,
        "EP004",
    );
}

#[test]
fn alg_004_implicit_equation_range_fails_closed_until_supported() {
    expect_parse_err_with_only_code(
        r#"
        model M
            Real a[3];
        equation
            for i loop
                a[i] = 1.0;
            end for;
        end M;
    "#,
        "EP004",
    );
}

#[test]
fn alg_004_implicit_comprehension_range_fails_closed_until_supported() {
    expect_parse_err_with_only_code(
        r#"
        model M
            parameter Real a[3] = {1.0*i for i};
        end M;
    "#,
        "EP004",
    );
}

#[test]
fn alg_004_implicit_reduction_range_fails_closed_until_supported() {
    expect_parse_err_with_only_code(
        r#"
        model M
            parameter Real a[3] = {1.0, 2.0, 3.0};
            Real s = sum(a[i] for i);
        end M;
    "#,
        "EP004",
    );
}

/// The omission is refused wherever it sits in the clause, not only in the
/// first iterator.
///
/// This is the case that observes iterator *arity*. The acceptance fixture
/// above parses a two-iterator clause but only asserts that parsing succeeded,
/// so it cannot see how many indices came out. Deleting the
/// `.chain(indices.for_indices_list…)` line from the parser's single
/// `convert_for_indices` silently drops every iterator after the first: this
/// source would then compile as a one-iterator loop, the omitted `j` would
/// never be visited, and no diagnostic would be issued at all. That mutant
/// passes every other ALG-004 case and fails only here.
#[test]
fn alg_004_implicit_later_index_range_fails_closed_until_supported() {
    expect_parse_err_with_only_code(
        r#"
        model M
            Real a[2,2];
        equation
            for i in 1:2, j loop
                a[i,j] = i + j;
            end for;
        end M;
    "#,
        "EP004",
    );
}

/// Positive control against the defect this campaign found: the previous
/// witness asserted `EP001`, which every syntax error in the file also
/// produces, so it passed on any typo and proved nothing about the omission.
///
/// Both inputs here are rejected for reasons unrelated to iterator ranges: one
/// through Parol's syntax path and one through the parser's semantic-conversion
/// path, which is the same channel the omission refusal travels. Neither may
/// report `EP004`. This fails against reusing `EP001` for the new variant and
/// against mapping every conversion failure to `EP004`. It does *not* fail
/// against reordering the two `convert_parol_error` downcast blocks: those test
/// disjoint concrete types, so their order is unobservable. The mutation that
/// makes the omission indistinguishable from these inputs is deleting the
/// `OmittedIterationRange` arm so the refusal drops through to the generic
/// `user_err` bucket; that one is caught by the `EP004` negatives above.
#[test]
fn alg_004_unrelated_rejections_cannot_satisfy_the_implicit_range_witness() {
    // Ordinary syntax corruption: a missing statement terminator.
    expect_parse_err_with_only_code(
        r#"
        model M
            function F
                input Real u;
                output Real y;
            protected
                Real a[3];
            algorithm
                for i in 1:3 loop
                    a[i] := u
                end for;
                y := a[1];
            end F;
            Real z = F(1.0);
        end M;
    "#,
        "EP001",
    );

    // A parse-phase semantic refusal: redeclaring a predefined type. This is
    // grammatically well formed and travels the same conversion-failure channel
    // as the omitted-range refusal, so it is the input that distinguishes
    // "EP004 is a distinct disposition" from "EP004 is what conversion failures
    // are now called".
    expect_parse_err_with_only_code(
        r#"
        model Real
            Real x;
        equation
            der(x) = 1;
        end Real;
    "#,
        "EP001",
    );
}

#[test]
fn alg_004_explicit_range_equation_loop_is_allowed() {
    expect_success(
        r#"
        model M
            Real a[3];
        equation
            for i in 1:3 loop
                a[i] = i;
            end for;
        end M;
    "#,
        "M",
    );
}

/// The comprehension form of the `EP004` acceptance contract, carried past the
/// parser to a full compile. The expression shape is the one already proven to
/// compile by `arr_026_nested_member_array_under_an_element_expanded_owner_is_subscriptable`,
/// so a failure here means the iterator converter regressed, not that
/// comprehension support moved.
#[test]
fn alg_004_explicit_range_comprehension_is_allowed() {
    expect_success(
        r#"
        model M
            parameter Integer n = 3;
            parameter Real p = 2.0;
            Real x[n];
        equation
            x = {p*time for k in 1:n};
        end M;
    "#,
        "M",
    );
}

#[test]
fn alg_004_whole_array_assignment_with_explicit_range_is_allowed() {
    expect_success(
        r#"
        model M
            function F
                input Real u;
                output Real y;
            protected
                Real a[3];
            algorithm
                for i in 1:3 loop
                    a := fill(u, 3);
                    a[1:2] := fill(u, 2);
                    a[i] := u;
                end for;
                y := a[1];
            end F;
            Real z = F(1.0);
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// ALG-017: Variables on the left-hand side of := must be initialized when
// algorithm is invoked
// =============================================================================

#[test]
fn alg_017_function_locals_initialized_per_invocation() {
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            function F
                input Real u;
                output Real y;
            protected
                Real acc = 10;
            algorithm
                acc := acc + u;
                y := acc;
            end F;
            Real t(start = 0, fixed = true);
            Real z;
        equation
            der(t) = 1;
            z = F(3.0);
        end M;
    "#,
        "M",
        1.0,
    );
    let z = trace.channel("z");
    assert!(
        z.iter().all(|&v| v == 13.0),
        "function locals must start from their defaults on every invocation, got {z:?}"
    );
}
