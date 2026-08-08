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

fn owner_target_index(dae: &Dae, owner_index: usize, target_name: &str) -> Option<usize> {
    dae.inspect(|view| {
        let owner_id = view.discrete_value_owner_id(owner_index)?;
        let owner = view.discrete_value_owner(owner_id)?;
        owner
            .targets()
            .iter()
            .enumerate()
            .find_map(|(target_index, target)| {
                let variable = view.variable(target.into())?;
                (variable.name().as_str() == target_name).then_some(target_index)
            })
    })
}

fn owner_target_reads_pre(dae: &Dae, owner_index: usize, target_index: usize) -> bool {
    dae.inspect(|view| {
        let Some(owner) = view
            .discrete_value_owner_id(owner_index)
            .and_then(|id| view.discrete_value_owner(id))
        else {
            return false;
        };
        let Some(target) = owner.targets().get(target_index) else {
            return false;
        };
        owner.branches().iter().any(|branch| {
            let Some((value, _)) = branch.values().get(target_index) else {
                return false;
            };
            let mut reads_pre = false;
            rumoca_compile::compile::for_each_expression(view, value, |_, expression| {
                reads_pre |= matches!(
                    expression.operation(),
                    rumoca_compile::compile::ExpressionOperation::Coordinate(
                        rumoca_compile::compile::CoordinateView::PreDiscreteValue(candidate)
                    ) if candidate == target
                );
            });
            reads_pre
        })
    })
}

fn owner_reads_its_pre_fallback(dae: &Dae, owner_index: usize, target_name: &str) -> bool {
    owner_target_index(dae, owner_index, target_name)
        .is_some_and(|target_index| owner_target_reads_pre(dae, owner_index, target_index))
}

fn dae_reads_pre_fallback(dae: &Dae, target_name: &str) -> bool {
    let owner_count = dae.inspect(|view| view.discrete_value_owner_count());
    (0..owner_count).any(|index| owner_reads_its_pre_fallback(dae, index, target_name))
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
            .inspect(|view| view.discrete_value_owner_count() > 0),
        "sample() in when-condition should lower to checked B.1c owners"
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
                event
                    .instant()
                    .is_some_and(|instant| instant.numerator() == 1 && instant.denominator() == 2)
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
            .inspect(|view| view.discrete_value_definition_count() == 2),
        "the checked discrete partition must contain two typed B.1c definitions"
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
        dae_reads_pre_fallback(&result.dae, "k"),
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

// =============================================================================
// MLS §3.7.5: `pre(y)` is the left limit `y(t^pre)`, and it is defined for a
// continuous-time `y` where the read is itself a discrete-time expression.
//
// The canonical MSL shape is `Modelica.Blocks.Math.Mean`
// (`Modelica 4.1.0/Blocks/Math.mo:2269-2274`):
//
//     der(x) = u;
//     when sample(t0 + 1/f, 1/f) then
//       y_last = if not yGreaterOrEqualZero then f*pre(x) else max(0.0, f*pre(x));
//       reinit(x, 0);
//     end when;
//
// `x` is a continuous state, `pre(x)` is read in the when-body, and the same
// body reinitializes `x`. The tick must observe the integral accumulated up to
// the event, not the reinitialized value.
// =============================================================================

#[test]
fn sim_009_pre_of_continuous_state_in_when_body_is_left_limit_before_reinit() {
    // `Modelica.Blocks.Math.Mean` reduced to its semantic core: with `u = 1`
    // and `f = 1`, one period accumulates exactly 1.0. Reading the post-reinit
    // value would give 0.0 instead.
    //
    // `x` starts at 5, not 0, so the MLS §8.6 seed is observable: the tick at
    // t_start reads pre(x) = x(t_start) = 5. With start = 0 a seeded lane and
    // an unseeded one are indistinguishable.
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            Real x(start = 5, fixed = true);
            discrete Real y_last(start = -1, fixed = true);
        equation
            der(x) = 1;
            when sample(0, 1) then
                y_last = pre(x);
                reinit(x, 0);
            end when;
        end M;
    "#,
        "M",
        3.5,
    );
    let y_last = trace.channel("y_last");
    assert!(
        y_last.iter().any(|value| (value - 5.0).abs() < 1.0e-6),
        "MLS §8.6 seeds pre(v) = v at t_start, so the tick at t_start must read \
         x(t_start) = 5; got {y_last:?}"
    );
    for (index, value) in y_last.iter().enumerate() {
        assert!(
            (value + 1.0).abs() < 1.0e-6
                || (value - 5.0).abs() < 1.0e-6
                || (value - 1.0).abs() < 1.0e-6,
            "after the first tick every tick must read the period integral 1.0 \
             (the left limit), not the reinitialized 0.0; sample {index} was {value}"
        );
    }
    assert!(
        (trace.final_value("y_last") - 1.0).abs() < 1.0e-6,
        "the last tick must still read the left limit, got {}",
        trace.final_value("y_last")
    );
}

#[test]
fn sim_009_pre_of_continuous_state_in_a_clocked_when_is_rejected() {
    // MLS §16.5/§16.8.1: a clock partition has no continuous-time left limit.
    // It reads its own coordinates with `previous()`, and a continuous value
    // enters only through `sample()`. OMC rejects this shape outright
    // ("Argument 1 of pre must be a discrete expression, but x is continuous").
    expect_failure_in_phase_with_code(
        r#"
        model M
            Real x(start = 0, fixed = true);
            Clock c = Clock(0.1);
            discrete Real y(start = -1, fixed = true);
        equation
            der(x) = 1;
            when c then
                y = pre(x);
            end when;
        end M;
    "#,
        "M",
        FailedPhase::ToDae,
        "ED019",
    );
}

#[test]
fn sim_009_pre_of_a_continuous_state_inside_a_reinit_value_is_the_left_limit() {
    // `reinit(x, pre(x) + 1)` evaluates its value at the event instant, so
    // `pre(x)` is the ordinary left limit. Rewriting it to a plain `x` (as this
    // wave's predecessor did) makes the equation `reinit(x, x + 1)`, which has
    // no solution and diverges.
    //
    // OMC integrates x from 0, so the pre-event values at the ticks are
    // 1, 3, 5 and x jumps to 2, 4, 6 respectively.
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            Real x(start = 0, fixed = true);
            discrete Real n(start = 0, fixed = true);
        equation
            der(x) = 1;
            when sample(1, 1) then
                n = pre(n) + 1;
                reinit(x, pre(x) + 1);
            end when;
        end M;
    "#,
        "M",
        3.5,
    );
    // Post-tick values follow OMC exactly: 1 -> 2 at t=1, 3 -> 4 at t=2,
    // 5 -> 6 at t=3, then free integration to 6.5 at t=3.5.
    assert!(
        (trace.final_value("x") - 6.5).abs() < 1.0e-4,
        "x must follow the OMC sequence and reach 6.5 at t = 3.5, got {}",
        trace.final_value("x")
    );
    assert!(
        (trace.final_value("n") - 3.0).abs() < 1.0e-9,
        "three ticks must have fired, got n = {}",
        trace.final_value("n")
    );
}

#[test]
fn sim_009_continuous_signal_extrema_body_tracks_min_and_max() {
    // `Modelica.Blocks.Math.ContinuousSignalExtrema` (Blocks/Math.mo:2589-2599)
    // is the second MSL site that reads `pre()` of continuous coordinates —
    // `pre(u)` on a continuous algebraic plus `pre(y_min)`/`pre(y_max)`/
    // `pre(t_min)`/`pre(t_max)`. This is its body with a scalar `sample()`
    // trigger; the block's own vector-when form is a separate construct that
    // only becomes reachable after the in-flight vector-when fix.
    //
    // Values are OMC's, from a dassl run at tolerance 1e-8.
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            Real u;
            Real y_min;
            Real y_max;
            Real t_min;
            Real t_max;
            // The block itself is stateless; this carries one continuous state
            // so the model is an ODE the solver can advance.
            Real ramp(start = 0, fixed = true);
        initial equation
            y_min = u;
            y_max = u;
            t_min = time;
            t_max = time;
        equation
            der(ramp) = 1;
            u = sin(6.2831853 * time);
            when sample(0.05, 0.05) then
                y_min = min({pre(y_min), u, pre(u)});
                y_max = max({pre(y_max), u, pre(u)});
                t_min = if y_min < pre(y_min) then time else pre(t_min);
                t_max = if y_max > pre(y_max) then time else pre(t_max);
            end when;
        end M;
    "#,
        "M",
        1.0,
    );
    // Over a full period of a unit sine the extrema are +/-1, found at the
    // quarter points; OMC reports t_max = 0.25 and t_min = 0.75.
    assert!(
        (trace.final_value("y_max") - 1.0).abs() < 1.0e-3,
        "y_max must reach +1, got {}",
        trace.final_value("y_max")
    );
    assert!(
        (trace.final_value("y_min") + 1.0).abs() < 1.0e-3,
        "y_min must reach -1, got {}",
        trace.final_value("y_min")
    );
    assert!(
        (trace.final_value("t_max") - 0.25).abs() < 0.05,
        "t_max must be the first quarter point, got {}",
        trace.final_value("t_max")
    );
    assert!(
        (trace.final_value("t_min") - 0.75).abs() < 0.05,
        "t_min must be the third quarter point, got {}",
        trace.final_value("t_min")
    );
}

#[test]
fn sim_009_msl_mean_block_body_averages_over_its_period() {
    // The body of `Modelica.Blocks.Math.Mean` verbatim, with the single change
    // that `t0` is a plain parameter instead of `parameter SI.Time t0(fixed =
    // false)` fixed by `initial equation t0 = time` — that spelling is a
    // separate unsupported construct (a non-parameter-evaluable `sample` start)
    // and would mask this one. The input and frequency match the OMC reference
    // run for this block (`Mean(f = 1)` fed a constant 2), which reports y = 2.0
    // exactly at every tick from t = 1 on. That value is only reachable if
    // `pre(x)` reads the state accumulated up to the tick rather than the value
    // `reinit(x, 0)` installs, which would give 0.0.
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            parameter Real f = 1 "Base frequency";
            parameter Real x0 = 0 "Start value of integrator state";
            parameter Real y0 = 0 "Start value of output";
            parameter Boolean yGreaterOrEqualZero = false;
            parameter Real t0 = 0 "Start time of simulation";
            Real u;
            Real y;
            Real x "Integrator state";
            discrete Real y_last "Last sampled mean value";
        initial equation
            x = x0;
            y_last = y0;
        equation
            u = 2;
            der(x) = u;
            when sample(t0 + 1/f, 1/f) then
                y_last = if not yGreaterOrEqualZero then f*pre(x) else max(0.0, f*pre(x));
                reinit(x, 0);
            end when;
            y = y_last;
        end M;
    "#,
        "M",
        2.5,
    );
    assert!(
        (trace.final_value("y") - 2.0).abs() < 1.0e-6,
        "the mean of u = 2 over a 1 s period is 2.0 (the OMC reference value), got {}",
        trace.final_value("y")
    );
}

#[test]
fn sim_009_pre_of_continuous_algebraic_in_when_body_snapshots_event_entry() {
    // The discriminating case: `a` depends on a discrete the same event
    // updates, so `pre(a)` and `a` differ at the tick. An implementation that
    // aliased `pre(a)` to `a` would read 11 at t = 1 instead of 1.
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            Real ramp(start = 0, fixed = true);
            Real a;
            discrete Real d(start = 0, fixed = true);
            discrete Real a_pre(start = 0, fixed = true);
        equation
            der(ramp) = 1;
            a = 10 * d + time;
            when sample(1, 1) then
                d = pre(d) + 1;
                a_pre = pre(a);
            end when;
        end M;
    "#,
        "M",
        1.5,
    );
    // The tolerance only has to separate the left limit from the live value,
    // which differ by 10; it absorbs the solver's event-localization error.
    assert!(
        (trace.final_value("a_pre") - 1.0).abs() < 1.0e-3,
        "pre(a) must be the left limit a(t^pre) = 10*0 + 1, got {}",
        trace.final_value("a_pre")
    );
    // The live `a` has already moved to 10*1 + t by the same event, so an
    // implementation that aliased `pre(a)` to `a` could not have passed the
    // assertion above.
    assert!(
        (trace.final_value("a") - 11.5).abs() < 1.0e-3,
        "the live `a` after the tick is 10*1 + 1.5, got {}",
        trace.final_value("a")
    );
}

#[test]
fn sim_009_pre_of_continuous_state_outside_when_clause_is_rejected() {
    // Ablation for the accept cases above: the same `pre(x)` on the same
    // continuous state is a typed rejection when no when-clause owns the read.
    // OMC rejects it too ("Argument 1 of pre must be a discrete expression,
    // but x is continuous").
    expect_failure_in_phase_with_code(
        r#"
        model M
            Real x(start = 0, fixed = true);
            discrete Real y(start = 0, fixed = true);
        equation
            der(x) = 1;
            y = pre(x);
        end M;
    "#,
        "M",
        FailedPhase::ToDae,
        "ED019",
    );
}

#[test]
fn sim_009_pre_of_continuous_state_in_when_condition_is_rejected() {
    // A when-clause's activation condition decides whether the event happens,
    // so it is not itself inside the event: there is no left limit to read.
    // OMC rejects this shape with the same discrete-expression diagnostic.
    expect_failure_in_phase_with_code(
        r#"
        model M
            Real x(start = 0, fixed = true);
            discrete Real y(start = 0, fixed = true);
        equation
            der(x) = 1;
            when pre(x) > 0.5 then
                y = 1;
            end when;
        end M;
    "#,
        "M",
        FailedPhase::ToDae,
        "ED019",
    );
}
