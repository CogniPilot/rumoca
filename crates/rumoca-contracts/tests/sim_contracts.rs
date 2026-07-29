//! SIM (Simulation) contract tests - MLS §8.6, App B
//!
//! Tests for the 9 simulation contracts defined in SPEC_0022.

use rumoca_compile::compile::{Dae, FailedPhase, VariableRole};
use rumoca_compile::{Session, SessionConfig};
use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_resolve_failure_with_code,
    expect_success, is_standalone_simulatable, unbound_fixed_parameter_names,
};

fn variable_count(dae: &Dae, role: VariableRole) -> usize {
    dae.inspect(|view| {
        view.variables()
            .filter(|(_, variable)| variable.role() == role)
            .count()
    })
}

fn variable_attributes(dae: &Dae, role: VariableRole, name: &str) -> Option<(bool, Option<bool>)> {
    dae.inspect(|view| {
        view.variables()
            .find(|(_, variable)| variable.role() == role && variable.name().as_str() == name)
            .map(|(_, variable)| (variable.start().is_some(), variable.fixed()))
    })
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
    assert_eq!(variable_count(&result.dae, VariableRole::State), 1);
    assert_eq!(
        variable_attributes(&result.dae, VariableRole::State, "x"),
        Some((true, None)),
        "state x should retain its start value without inventing fixed=true"
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
        variable_count(&result.dae, VariableRole::Parameter) > 0,
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
fn sim_004_non_parameter_variable_defaults_fixed_false() {
    let result = expect_success(
        r#"
        model Test
            Real x(start = 1.0);
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
    );

    assert_eq!(
        variable_attributes(&result.dae, VariableRole::State, "x"),
        Some((true, None)),
        "non-parameter variables should not default to fixed=true"
    );
    assert!(
        result
            .dae
            .inspect(|view| view.initialization_equation_count() == 0),
        "start value without fixed=true must not add an initialization equation"
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
        result
            .dae
            .inspect(|view| view.continuous_equation_count() > 0),
        "DAE should have continuous equations (f_x)"
    );
    assert!(
        variable_count(&result.dae, VariableRole::State) > 0,
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
        result
            .dae
            .inspect(|view| view.continuous_equation_count() > 0),
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
        variable_count(&result.dae, VariableRole::State) > 0,
        "Should have state variables for ODE"
    );
    assert!(
        result
            .dae
            .inspect(|view| view.continuous_equation_count() > 0),
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
    assert_eq!(variable_count(&result.dae, VariableRole::State), 1);
    assert_eq!(
        result.dae.inspect(|view| view.continuous_equation_count()),
        1
    );
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
    assert_eq!(variable_count(&result.dae, VariableRole::State), 2);
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
    assert!(variable_count(&result.dae, VariableRole::Parameter) > 0);
    assert!(variable_count(&result.dae, VariableRole::State) > 0);
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
        result
            .dae
            .inspect(|view| view.relation_count() > 0 && view.condition_count() > 0),
        "DAE should expose canonical condition equations"
    );
}

#[test]
fn sim_009_sample_in_fx_lowers_to_ordinary_dae_and_schedule_metadata() {
    let result = expect_success(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = if sample(0, 0.1) then 1 else 0;
        end Test;
    "#,
        "Test",
    );

    assert!(
        result
            .dae
            .inspect(|view| (0..view.clock_count()).any(|index| {
                let clock = view
                    .clock(view.clock_id(index).expect("dense checked clock"))
                    .expect("checked clock resolves");
                matches!(
                    clock.operation(),
                    rumoca_compile::compile::ClockOperation::Periodic(lattice)
                        if lattice.period().numerator() == 1
                            && lattice.period().denominator() == 10
                            && lattice.phase().is_zero()
                )
            })),
        "the periodic sample must remain represented by canonical DAE schedule metadata"
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
        result
            .dae
            .inspect(|view| view.discrete_assignment_count() > 0),
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

    assert!(
        result
            .dae
            .inspect(|view| (0..view.time_event_count()).any(|index| {
                let event = view
                    .time_event(view.time_event_id(index).expect("dense time event"))
                    .expect("checked time event resolves");
                event.instant().numerator() == 1 && event.instant().denominator() == 2
            })),
        "time-driven discontinuity should be reflected in scheduled_time_events"
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
        result.dae.inspect(|view| view.relation_count()),
        2,
        "expected both if-condition and when-condition in relation"
    );

    let relation_text = result.dae.inspect(|view| {
        (0..view.relation_count())
            .map(|index| {
                let relation = view
                    .relation(view.relation_id(index).expect("dense relation"))
                    .expect("checked relation resolves");
                result
                    .dae
                    .source_text(relation.provenance())
                    .expect("source relation has exact provenance")
                    .to_string()
            })
            .collect::<Vec<_>>()
    });
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
        result.dae.inspect(|view| view.relation_count() == 0),
        "noEvent condition must not generate relation entries"
    );
    assert!(
        result.dae.inspect(|view| view.condition_count() == 0),
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
        result
            .dae
            .inspect(|view| view.discrete_assignment_count() == 2),
        "the checked discrete partition must contain two typed assignments"
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

    assert!(
        result.dae.inspect(|view| {
            (0..view.discrete_assignment_count()).any(|index| {
                let assignment = view
                    .discrete_assignment(
                        view.discrete_assignment_id(index)
                            .expect("dense discrete assignment"),
                    )
                    .expect("checked discrete assignment resolves");
                let target = view
                    .variable(assignment.target().into())
                    .expect("assignment target resolves");
                if target.name().as_str() != "k" {
                    return false;
                }
                let mut has_pre_fallback = false;
                rumoca_compile::compile::for_each_expression(
                    view,
                    assignment.value(),
                    |_, expression| {
                        has_pre_fallback |= matches!(
                            expression.operation(),
                            rumoca_compile::compile::ExpressionOperation::Coordinate(
                                rumoca_compile::compile::CoordinateView::PreDiscreteValue(candidate)
                            ) if candidate == assignment.target()
                        );
                    },
                );
                has_pre_fallback
            })
        }),
        "conditional when lowering must preserve the typed pre(k) fallback"
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

#[test]
fn sim_009_unresolved_reference_rejected_in_multi_document_resolve() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "library.mo",
            r#"
            record Inner
                Real x;
            end Inner;
        "#,
        )
        .unwrap_or_else(|e| panic!("library parse failed: {e}"));
    session
        .add_document(
            "target.mo",
            r#"
            model Test
                Inner pid;
                Real x2;
            equation
                pid.x = 0;
                der(x2) = x;
            end Test;
        "#,
        )
        .unwrap_or_else(|e| panic!("target parse failed: {e}"));

    if let Ok(phase_result) = session.compile_model_phases("Test") {
        panic!("expected resolve failure, got {phase_result:?}");
    }

    let diagnostics = session.compile_model_diagnostics("Test");
    assert!(
        diagnostics.diagnostics.iter().any(|diag| {
            diag.code.as_deref() == Some("ER002")
                && diag.message.contains("unresolved component reference: 'x'")
        }),
        "expected ER002 unresolved `x`, got {:?}",
        diagnostics.diagnostics
    );
}

// =============================================================================
// SIM-006: Solved variable must appear uniquely as term (no multiplicative
// factor) on either side
// =============================================================================

#[test]
fn sim_006_nonlinear_discrete_when_equation_rejected() {
    rumoca_contracts::test_support::expect_failure_in_phase_with_code(
        r#"
        model M
            Integer n(start = 0);
            Boolean c = time > 1;
        equation
            when c then
                n * n = 4;
            end when;
        end M;
    "#,
        "M",
        rumoca_compile::compile::FailedPhase::Flatten,
        "EF004",
    );
}

// =============================================================================
// SIM-007: Non-Integer equations require at most flipping sides to obtain
// assignment form
// =============================================================================

#[test]
fn sim_007_when_equation_not_in_assignment_form_rejected() {
    rumoca_contracts::test_support::expect_failure_in_phase_with_code(
        r#"
        model M
            discrete Real x(start = 0);
            Boolean c = time > 1;
        equation
            when c then
                x + 1 = time;
            end when;
        end M;
    "#,
        "M",
        rumoca_compile::compile::FailedPhase::Flatten,
        "EF004",
    );
}

// =============================================================================
// SIM-001: Iterate solving equations until z == pre(z) and m == pre(m)
// =============================================================================

#[test]
fn sim_001_event_iteration_reaches_fixpoint() {
    // The first when-clause fires at t=1 and sets i=1; event iteration must
    // re-evaluate the second when-clause in the same event instant so j
    // settles at i+1 before integration resumes.
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            Real x(start = 0, fixed = true);
            discrete Integer i(start = 0, fixed = true);
            discrete Integer j(start = 0, fixed = true);
        equation
            der(x) = 1;
            when time > 1 then
                i = 1;
            end when;
            when i > 0 then
                j = i + 1;
            end when;
        end M;
    "#,
        "M",
        2.0,
    );
    assert_eq!(trace.final_value("i"), 1.0, "first when must fire");
    assert_eq!(
        trace.final_value("j"),
        2.0,
        "event iteration must settle j in the same event instant"
    );
}

// =============================================================================
// SIM-008: Values of conditions c, z, and m only changed at event instant,
// constant during continuous integration
// =============================================================================

#[test]
fn sim_008_discrete_values_constant_between_events() {
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            Real t(start = 0, fixed = true);
            discrete Integer n(start = 0, fixed = true);
        equation
            der(t) = 1;
            when time > 1 then
                n = 1;
            end when;
        end M;
    "#,
        "M",
        2.0,
    );
    let n = trace.channel("n");
    // n is piecewise constant: only the values 0 and 1 appear, with a single
    // monotone step at the event instant.
    assert!(
        n.iter().all(|&v| v == 0.0 || v == 1.0),
        "discrete variable must hold piecewise-constant values, got {n:?}"
    );
    assert!(
        n.windows(2).all(|w| w[0] <= w[1]),
        "discrete variable changed outside the single event instant: {n:?}"
    );
}

// =============================================================================
// EQN-035: Before start of integration, for all variables v, v = pre(v) must
// be guaranteed
// =============================================================================

#[test]
fn eqn_035_initialization_pre_consistency() {
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            Real t(start = 0, fixed = true);
            discrete Real x(start = 5, fixed = true);
        equation
            der(t) = 1;
            when time > 1 then
                x = pre(x) + 1;
            end when;
        end M;
    "#,
        "M",
        2.0,
    );
    let x = trace.channel("x");
    assert_eq!(
        x.first().copied(),
        Some(5.0),
        "x = pre(x) = start must hold before integration starts"
    );
    assert_eq!(trace.final_value("x"), 6.0);
}
