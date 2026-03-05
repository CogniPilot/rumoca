//! SIM (Simulation) contract tests - MLS §8.6, App B
//!
//! Tests for the 9 simulation contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_resolve_failure_with_code,
    expect_success, is_standalone_simulatable, unbound_fixed_parameter_names,
};
use rumoca_session::FailedPhase;

// =============================================================================
// SIM-001: Event iteration
// "Iterate solving equations until z == pre(z) and m == pre(m)"
// =============================================================================

#[test]
fn sim_001_when_guard_uses_pre_edge_activation() {
    let result = expect_success(
        r#"
        model Test
            discrete Boolean b(start = false);
            discrete Integer k(start = 0);
        equation
            when sample(0, 0.1) then
                b = not pre(b);
            end when;

            when b then
                k = pre(k) + 1;
            end when;
        end Test;
    "#,
        "Test",
    );

    let k_eq = result
        .dae
        .f_m
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "k"))
        .unwrap_or_else(|| panic!("expected lowered discrete assignment for k in f_m"));

    let rhs_debug = format!("{:?}", k_eq.rhs);
    assert!(
        rhs_debug.contains("Pre") && rhs_debug.contains("\"b\""),
        "when-guarded assignment should encode edge/pre semantics for event iteration; rhs={rhs_debug}"
    );
}

#[test]
fn sim_001_event_iteration_rejects_non_convergent_cycle() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            discrete Boolean a(start = false);
            discrete Boolean b(start = false);
        equation
            when sample(0, 0.1) then
                a = b;
                b = a;
            end when;
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED010",
    );
}

// =============================================================================
// SIM-002: Initialization fixed
// "Continuous Real with fixed=true adds equation vc = startExpression"
// =============================================================================

#[test]
fn sim_002_initialization_fixed() {
    let result = expect_balanced(
        r#"
        model Test
            Real x(start = 1.0);
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
    );
    // Check that start value is present in DAE
    assert!(!result.dae.states.is_empty(), "Should have state variables");
    let state = result.dae.states.values().next().unwrap();
    assert!(state.start.is_some(), "State should have start value");
}

#[test]
fn sim_002_fixed_start_type_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x(start = true, fixed = true);
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

// =============================================================================
// SIM-003: Parameter fixed default
// "For parameters: fixed defaults to true"
// =============================================================================

#[test]
fn sim_003_parameter_fixed_default() {
    let result = expect_success(
        r#"
        model Test
            parameter Real p = 1.0;
            Real x;
        equation
            x = p;
        end Test;
    "#,
        "Test",
    );
    assert!(
        !result.dae.parameters.is_empty(),
        "Should have parameters in DAE"
    );
}

#[test]
fn sim_003_parameter_missing_binding_not_standalone_simulatable() {
    let result = expect_success(
        r#"
        model Test
            parameter Real p;
            Real x(start = 1);
        equation
            der(x) = -p * x;
        end Test;
    "#,
        "Test",
    );

    assert!(
        !is_standalone_simulatable(&result),
        "Model with unbound fixed parameter should not be standalone-simulatable"
    );
    assert_eq!(
        unbound_fixed_parameter_names(&result),
        vec!["p".to_string()],
        "Expected `p` to be detected as unbound fixed parameter"
    );
}

#[test]
fn sim_003_parameter_with_binding_is_standalone_simulatable() {
    let result = expect_success(
        r#"
        model Test
            parameter Real p = 2.0;
            Real x(start = 1);
        equation
            der(x) = -p * x;
        end Test;
    "#,
        "Test",
    );

    assert!(
        is_standalone_simulatable(&result),
        "Parameter with binding should be standalone-simulatable"
    );
    assert!(
        unbound_fixed_parameter_names(&result).is_empty(),
        "No unbound fixed parameters expected"
    );
}

// =============================================================================
// SIM-004: Variable fixed default
// "For other variables: fixed defaults to false"
// =============================================================================

#[test]
fn sim_004_variable_default_fixed_false_does_not_force_fixed_true() {
    let result = expect_success(
        r#"
        model Test
            Real x;
        initial equation
            x = 1;
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
    );

    let state = result
        .dae
        .states
        .iter()
        .find(|(name, _)| name.as_str() == "x")
        .map(|(_, var)| var)
        .unwrap_or_else(|| panic!("expected state x"));
    assert_ne!(
        state.fixed,
        Some(true),
        "non-parameter variables must not default to fixed=true"
    );
    assert_eq!(
        result.dae.initial_equations.len(),
        1,
        "default fixed=false must not inject an extra initial equation"
    );
}

#[test]
fn sim_004_explicit_fixed_true_overrides_default_false() {
    let result = expect_success(
        r#"
        model Test
            Real x(fixed = true);
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
    );

    let state = result
        .dae
        .states
        .iter()
        .find(|(name, _)| name.as_str() == "x")
        .map(|(_, var)| var)
        .unwrap_or_else(|| panic!("expected state x"));
    assert_eq!(
        state.fixed,
        Some(true),
        "explicit fixed=true should be preserved for continuous variables"
    );
}

// =============================================================================
// SIM-009: DAE structure
// "System shall consist of differential equations, discrete equations, etc."
// =============================================================================

#[test]
fn sim_009_dae_has_ode_equations() {
    let result = expect_balanced(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = 1;
        end Test;
    "#,
        "Test",
    );
    assert!(
        !result.dae.f_x.is_empty(),
        "DAE should have continuous equations (f_x)"
    );
    assert!(
        !result.dae.states.is_empty(),
        "DAE should have state variables"
    );
}

#[test]
fn sim_009_dae_has_algebraic_equations() {
    let result = expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = 1;
        end Test;
    "#,
        "Test",
    );
    // The model has equations (no der)
    assert!(
        !result.dae.f_x.is_empty(),
        "DAE should have equations (f_x)"
    );
}

#[test]
fn sim_009_dae_structure_ode_and_algebraic() {
    let result = expect_balanced(
        r#"
        model Test
            Real x(start = 0);
            Real y;
        equation
            der(x) = -y;
            y = x * 2;
        end Test;
    "#,
        "Test",
    );
    assert!(
        !result.dae.states.is_empty(),
        "Should have state variables for ODE"
    );
    assert!(
        !result.dae.f_x.is_empty(),
        "Should have continuous equations (f_x)"
    );
}

// =============================================================================
// SIM integration tests
// =============================================================================

#[test]
fn sim_basic_integrator() {
    let result = expect_balanced(
        r#"
        model Integrator
            Real x(start = 0);
        equation
            der(x) = 1;
        end Integrator;
    "#,
        "Integrator",
    );
    assert_eq!(result.dae.states.len(), 1);
    assert_eq!(result.dae.f_x.len(), 1);
}

#[test]
fn sim_spring_mass() {
    let result = expect_balanced(
        r#"
        model SpringMass
            parameter Real k = 1;
            parameter Real m = 1;
            Real x(start = 1);
            Real v(start = 0);
        equation
            der(x) = v;
            m * der(v) = -k * x;
        end SpringMass;
    "#,
        "SpringMass",
    );
    assert_eq!(result.dae.states.len(), 2);
}

#[test]
fn sim_with_parameters() {
    let result = expect_balanced(
        r#"
        model Test
            parameter Real tau = 1;
            Real x(start = 1);
        equation
            tau * der(x) = -x;
        end Test;
    "#,
        "Test",
    );
    assert!(!result.dae.parameters.is_empty());
    assert!(!result.dae.states.is_empty());
}

#[test]
fn sim_with_when_clause() {
    let result = expect_success(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = 1;
            when x > 5 then
                reinit(x, 0);
            end when;
        end Test;
    "#,
        "Test",
    );
    assert!(
        !result.dae.relation.is_empty() && !result.dae.f_c.is_empty(),
        "DAE should expose canonical condition equations"
    );
}

#[test]
fn sim_009_strict_solver_dae_rejects_unlowered_sample_in_fx() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = if sample(0, 0.1) then 1 else 0;
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED014",
    );
}

#[test]
fn sim_009_dynamic_clock_constructor_without_static_schedule_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real u(start = 0);
            discrete Real s(start = 0);
        equation
            der(u) = 1;
            s = sample(u, Clock(u));
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED009",
    );
}

#[test]
fn sim_009_sample_allowed_in_discrete_when_condition() {
    let result = expect_success(
        r#"
        model Test
            discrete Integer k(start = 0);
        equation
            when sample(0, 0.1) then
                k = pre(k) + 1;
            end when;
        end Test;
    "#,
        "Test",
    );

    assert!(
        !result.dae.f_m.is_empty(),
        "sample() in when-condition should lower to discrete partition equations"
    );
}

#[test]
fn sim_009_runtime_metadata_consistent_for_hybrid_model() {
    let result = expect_success(
        r#"
        model Test
            Real x(start = 1);
            discrete Boolean sw(start = false);
        equation
            der(x) = if time > 0.5 then -x else x;
            when sample(0.1, 0.2) then
                sw = not pre(sw);
            end when;
        end Test;
    "#,
        "Test",
    );

    assert_eq!(
        result.dae.f_c.len(),
        result.dae.relation.len(),
        "f_c and relation must stay aligned for hybrid models"
    );
    assert!(
        result
            .dae
            .scheduled_time_events
            .iter()
            .any(|event| (*event - 0.5).abs() <= 1.0e-12),
        "time-driven discontinuity should be reflected in scheduled_time_events"
    );
    assert!(
        result
            .dae
            .scheduled_time_events
            .iter()
            .all(|event| event.is_finite()),
        "scheduled_time_events must contain finite values"
    );
}

#[test]
fn sim_009_fc_relation_covers_if_and_when_conditions() {
    let result = expect_success(
        r#"
        model Test
            Real x(start = 0);
            discrete Boolean b(start = false);
        equation
            der(x) = if x > 0.3 then -1 else 1;
            when x > 0.6 then
                b = not pre(b);
            end when;
        end Test;
    "#,
        "Test",
    );

    assert_eq!(
        result.dae.f_c.len(),
        result.dae.relation.len(),
        "f_c and relation must remain 1:1"
    );
    assert_eq!(
        result.dae.relation.len(),
        2,
        "expected both if-condition and when-condition in relation"
    );

    let relation_text: Vec<String> = result
        .dae
        .relation
        .iter()
        .map(|expr| format!("{expr:?}"))
        .collect();
    assert!(
        relation_text.iter().any(|expr| expr.contains("0.3")),
        "if-condition should be present in relation: {relation_text:?}"
    );
    assert!(
        relation_text.iter().any(|expr| expr.contains("0.6")),
        "when-condition should be present in relation: {relation_text:?}"
    );
}

#[test]
fn sim_009_fc_relation_ignores_noevent_conditions() {
    let result = expect_success(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = if noEvent(x > 0.2) then -1 else 1;
        end Test;
    "#,
        "Test",
    );

    assert!(
        result.dae.relation.is_empty(),
        "noEvent condition must not generate relation entries"
    );
    assert!(
        result.dae.f_c.is_empty(),
        "noEvent condition must not generate f_c entries"
    );
}

#[test]
fn sim_005_discrete_solved_form_acyclic_dependency() {
    let result = expect_success(
        r#"
        model Test
            discrete Boolean a(start = false);
            discrete Boolean b(start = false);
        equation
            when time > 0 then
                a = not pre(a);
                b = a;
            end when;
        end Test;
    "#,
        "Test",
    );

    assert!(
        result.dae.f_m.iter().all(|eq| eq.lhs.is_some()),
        "f_m equations must be explicit assignments"
    );
}

#[test]
fn sim_005_conditional_when_missing_branch_uses_pre_fallback() {
    let result = expect_success(
        r#"
        model Test
            Real x(start = 0);
            discrete Integer k(start = 0);
        equation
            der(x) = 1;
            when x > 0 then
                if x > 0.5 then
                    k = pre(k) + 1;
                end if;
            end when;
        end Test;
    "#,
        "Test",
    );

    let k_eq = result
        .dae
        .f_m
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "k"))
        .unwrap_or_else(|| {
            panic!(
                "expected explicit f_m assignment for k; f_m={:?}",
                result.dae.f_m
            )
        });

    let rhs_debug = format!("{:?}", k_eq.rhs);
    assert!(
        rhs_debug.contains("BuiltinCall") && rhs_debug.contains("Pre"),
        "conditional when lowering must preserve pre(k) fallback in missing branches; rhs={rhs_debug}"
    );
}

#[test]
fn sim_005_discrete_solved_form_rejects_cycle() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            discrete Boolean a(start = false);
            discrete Boolean b(start = false);
        equation
            when time > 0 then
                a = b;
                b = a;
            end when;
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED010",
    );
}

// =============================================================================
// SIM-006: Integer solved form
// "Solved variable must appear uniquely as term (no multiplicative factor)"
// =============================================================================

#[test]
fn sim_006_integer_assignment_form_in_when_is_accepted() {
    let result = expect_success(
        r#"
        model Test
            discrete Integer a(start = 0);
            discrete Integer b(start = 1);
        equation
            when sample(0, 0.1) then
                a = b;
                b = pre(a) + 1;
            end when;
        end Test;
    "#,
        "Test",
    );

    assert!(
        result.dae.f_m.iter().all(|eq| eq.lhs.is_some()),
        "integer discrete solved form must lower to explicit assignment equations in f_m"
    );
}

#[test]
fn sim_006_integer_lhs_with_multiplicative_factor_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            discrete Integer k(start = 0);
        equation
            when sample(0, 0.1) then
                2 * k = 4;
            end when;
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF004",
    );
}

// =============================================================================
// SIM-007: Non-Integer flip form
// "Non-Integer equations require at most flipping sides to obtain assignment form"
// =============================================================================

#[test]
fn sim_007_non_integer_assignment_form_in_when_is_accepted() {
    let result = expect_success(
        r#"
        model Test
            discrete Real zr(start = 0.0);
        equation
            when sample(0, 0.1) then
                zr = 1.0;
            end when;
        end Test;
    "#,
        "Test",
    );

    assert!(
        result
            .dae
            .f_z
            .iter()
            .any(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "zr")),
        "discrete Real assignment should lower into f_z"
    );
}

#[test]
fn sim_007_non_integer_needing_more_than_flip_is_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            discrete Real zr(start = 0.0);
        equation
            when sample(0, 0.1) then
                2.0 * zr = 1.0;
            end when;
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF004",
    );
}

// =============================================================================
// SIM-008: Discrete variable stability
// "Values of conditions c, z, and m only changed at event instant"
// =============================================================================

#[test]
fn sim_008_discrete_updates_use_pre_fallback_between_events() {
    let result = expect_success(
        r#"
        model Test
            discrete Integer d(start = 0);
        equation
            when sample(0, 0.1) then
                d = d + 1;
            end when;
        end Test;
    "#,
        "Test",
    );

    let d_eq = result
        .dae
        .f_m
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "d"))
        .unwrap_or_else(|| panic!("expected lowered discrete assignment for d in f_m"));
    let rhs_debug = format!("{:?}", d_eq.rhs);
    assert!(
        rhs_debug.contains("Pre"),
        "discrete updates should carry pre() fallback outside event instants; rhs={rhs_debug}"
    );
}

#[test]
fn sim_008_discrete_dependency_cycle_is_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            discrete Integer a(start = 0);
            discrete Integer b(start = 0);
        equation
            when sample(0, 0.1) then
                a = b;
                b = a;
            end when;
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED010",
    );
}

#[test]
fn sim_009_unresolved_function_rejected_in_resolve() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x(start = 1);
        equation
            der(x) = missingFn(x);
        end Test;
    "#,
        "Test",
        "ER002",
    );
}

#[test]
fn sim_009_unresolved_reference_rejected_in_resolve() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x(start = 1);
        equation
            der(x) = missingRef;
        end Test;
    "#,
        "Test",
        "ER002",
    );
}
