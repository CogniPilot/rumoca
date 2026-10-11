//! MLS §8.5: an arm-owned deadline keeps its exact guarded relation owner.
use super::*;

fn source(operator: &str, reset: &str) -> String {
    format!(
        "model GuardedDeadline Real x(start=0,fixed=true); output Real y; \
         discrete Real deadline(start=0.45,fixed=true); equation \
         y=if time>=0.2 then (if time {operator} deadline then 1 else 0) else 0; \
         der(x)=y; {reset} end GuardedDeadline;"
    )
}

fn check_curve(source: &str, expected: impl Fn(f64) -> f64) {
    let compiled = compile(source, "GuardedDeadline");
    compiled.dae.inspect(|view| {
        assert_eq!(view.root_count(), 1, "one guarded scalar time relation");
        assert!(
            view.time_events()
                .all(|(_, event)| event.deadline().is_none()),
            "the branch owns its relation rather than an unconditional dynamic deadline"
        );
        let (_, root) = view.roots().next().unwrap();
        let relation = view.relation(root.relation()).unwrap();
        assert!(view.model_activation_guard(relation.expression()).is_some());
    });
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let opts = options(policy);
        let mut session = SimulationSession::new(&compiled.dae, opts.clone())
            .expect("guarded deadline initializes");
        for ordinal in 0..=10 {
            let time = ordinal as f64 / 10.0;
            if ordinal != 0 {
                session.advance_to(time).expect("guarded deadline advances");
            }
            let state = session.state().unwrap();
            assert_eq!(state.time, time);
            let actual = state.values["x"];
            let expected = expected(time);
            assert!(
                actual.is_finite()
                    && (actual - expected).abs() <= 40.0 * (opts.atol + opts.rtol * expected.abs()),
                "{policy:?}: x({time})={actual}, expected {expected}"
            );
            if policy == SimExecutionPolicy::Auto {
                let receipt = session.execution_receipt();
                assert_eq!(receipt.engine, SimExecutionEngine::Cranelift, "{receipt:?}");
                assert_eq!(receipt.refusal, None, "{receipt:?}");
                assert!(receipt.declined.is_empty(), "{receipt:?}");
            }
        }
    }
}

#[test]
fn guarded_dynamic_deadline_rising_strict_and_nonstrict_crossings() {
    for operator in [">", ">="] {
        check_curve(
            &source(
                operator,
                "when time>=2 then deadline=pre(deadline); end when;",
            ),
            |time| (time - 0.45).max(0.0),
        );
    }
}

#[test]
fn guarded_dynamic_deadline_falling_strict_and_nonstrict_crossings() {
    for operator in ["<", "<="] {
        check_curve(
            &source(
                operator,
                "when time>=2 then deadline=pre(deadline); end when;",
            ),
            |time| (time - 0.2).clamp(0.0, 0.25),
        );
    }
}

#[test]
fn guarded_dynamic_deadline_reschedules_after_a_discrete_reset() {
    check_curve(
        &source(">", "when time>=0.65 then deadline=0.85; end when;"),
        |time| (time - 0.45).clamp(0.0, 0.2) + (time - 0.85).max(0.0),
    );
}

#[test]
fn guarded_dynamic_deadline_does_not_evaluate_inactive_arithmetic() {
    let source = "model GuardedDeadline Real x(start=0,fixed=true); \
        output Real y; discrete Real deadline(start=0.45,fixed=true); \
        equation y=if time>=2 then (if time>sqrt(deadline-1) then 1 else 0) else 0; \
        der(x)=y; when time>=2 then deadline=pre(deadline); end when; end GuardedDeadline;";
    check_curve(source, |_| 0.0);
    let selected = source.replacen("if time>=2", "if time>=0", 1);
    let compiled = compile(&selected, "GuardedDeadline");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let error = simulate_dae(&compiled.dae, &options(policy))
            .expect_err("selected undefined deadline arithmetic must still fault");
        let diagnostic = format!("{error:?}").to_lowercase();
        assert!(
            diagnostic.contains("finite")
                || diagnostic.contains("domain")
                || diagnostic.contains("nan"),
            "{policy:?}: {diagnostic}"
        );
    }
}

/// At initialization MLS §8.5 seeds held relations from the actual start point.
/// Starting at the threshold isolates equality from event-location brackets:
/// a later settled crossing may legitimately publish its right-limit truth.
#[test]
fn guarded_dynamic_deadline_preserves_equality_truth_at_initialization() {
    use rumoca_ir_solve::RootZeroDomain;

    for (operator, equality, after, zero_domain) in [
        (">", 0.0, 1.0, RootZeroDomain::Positive),
        (">=", 1.0, 1.0, RootZeroDomain::NonPositive),
        ("<", 0.0, 0.0, RootZeroDomain::Positive),
        ("<=", 1.0, 0.0, RootZeroDomain::NonPositive),
    ] {
        let compiled = compile(
            &source(
                operator,
                "when time>=2 then deadline=pre(deadline); end when;",
            ),
            "GuardedDeadline",
        );
        let problem = rumoca_sim::lower_solve_problem(&compiled.dae)
            .expect("the guarded relation has its checked root owner");
        assert_eq!(
            problem.events.root_zero_domains,
            [zero_domain],
            "{operator}"
        );
        for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
            let mut opts = options(policy);
            opts.t_start = 0.45;
            let mut session = SimulationSession::new(&compiled.dae, opts.clone())
                .expect("initialize exactly at the deadline");
            let state = session.state().unwrap();
            assert_eq!(state.time, 0.45, "{operator}, {policy:?}");
            assert_eq!(state.values["x"], 0.0, "{operator}, {policy:?}");
            assert_eq!(
                state.values["y"], equality,
                "{operator}, {policy:?}: exact initial equality"
            );
            session
                .advance_to(0.55)
                .expect("advance to the strict right side");
            let state = session.state().unwrap();
            assert_eq!(state.time, 0.55, "{operator}, {policy:?}");
            assert_eq!(
                state.values["y"], after,
                "{operator}, {policy:?}: right of threshold"
            );
            let expected = after * (0.55 - 0.45);
            let actual = state.values["x"];
            assert!(
                actual.is_finite()
                    && (actual - expected).abs() <= 40.0 * (opts.atol + opts.rtol * expected.abs()),
                "{operator}, {policy:?}: x={actual}, expected {expected}"
            );
            if policy == SimExecutionPolicy::Auto {
                let receipt = session.execution_receipt();
                assert_eq!(receipt.engine, SimExecutionEngine::Cranelift, "{receipt:?}");
                assert_eq!(receipt.refusal, None, "{receipt:?}");
                assert!(receipt.declined.is_empty(), "{receipt:?}");
            }
        }
    }
}
