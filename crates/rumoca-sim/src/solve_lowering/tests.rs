use rumoca_compile::compile::{Session, SessionConfig};

use super::entry::lower_dae_for_simulation;
use crate::{SimOptions, simulate_dae};

fn compile(source: &str, model: &str) -> rumoca_ir_dae::Dae {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("simulation_checked_dae.mo", source)
        .expect("fixture parses");
    session
        .compile_model(model)
        .expect("fixture compiles through checked ToDAE")
        .dae
}

#[test]
fn simulation_lowering_consumes_checked_todae_output_end_to_end() {
    let dae = compile(
        "model Decay Real x(start=1); equation der(x) = -x; end Decay;",
        "Decay",
    );

    let solve = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("checked scalar DAE lowers to a computable Solve model");

    assert_eq!(solve.problem.layout.y_scalars(), 1);
    assert_eq!(solve.initial_y, vec![1.0]);
}

#[test]
fn checked_algebraic_projection_executes_end_to_end() {
    let dae = compile(
        concat!(
            "model Coupled\n",
            "  Real x(start=1);\n",
            "  output Real y;\n",
            "equation\n",
            "  der(x) = -x;\n",
            "  y = 2*x;\n",
            "end Coupled;\n",
        ),
        "Coupled",
    );
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.05),
        ..SimOptions::default()
    };

    let result =
        simulate_dae(&dae, &options).expect("checked algebraic projection must be executable");
    let x = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state result column");
    let y = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("algebraic result column");

    assert_eq!(result.data[x].len(), result.data[y].len());
    for (&x_value, &y_value) in result.data[x].iter().zip(&result.data[y]) {
        assert!(
            (y_value - 2.0 * x_value).abs() <= 1.0e-8,
            "algebraic output must be refreshed from its matched residual: x={x_value}, y={y_value}"
        );
    }
}

#[test]
fn checked_transcendental_builtins_execute_end_to_end() {
    let dae = compile(
        concat!(
            "model TranscendentalBuiltins\n",
            "  Real x(start=0.5);\n",
            "  output Real y;\n",
            "equation\n",
            "  der(x) = 0;\n",
            "  y = asin(x) + atan2(x, 2);\n",
            "end TranscendentalBuiltins;\n",
        ),
        "TranscendentalBuiltins",
    );

    let result = simulate_dae(&dae, &SimOptions::default())
        .expect("checked transcendental builtin programs must execute");
    let y = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("builtin result column");
    let expected = 0.5_f64.asin() + 0.5_f64.atan2(2.0);

    assert!(
        result.data[y]
            .iter()
            .all(|value| (value - expected).abs() <= 1.0e-10)
    );
}

#[test]
fn smooth_and_no_event_remain_typed_and_execute_end_to_end() {
    let dae = compile(
        concat!(
            "model EventSuppressionBuiltins\n",
            "  Real x(start=-0.5);\n",
            "  output Real y;\n",
            "equation\n",
            "  der(x) = 0;\n",
            "  y = smooth(1, noEvent(if x > 0 then x else -x));\n",
            "end EventSuppressionBuiltins;\n",
        ),
        "EventSuppressionBuiltins",
    );

    dae.inspect(|view| {
        let builtins = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter_map(|expression| match expression.operation() {
                rumoca_ir_dae::ExpressionOperation::Builtin { builtin, .. } => {
                    Some((builtin, expression.provenance()))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        for expected in [
            rumoca_ir_dae::PureBuiltin::Smooth,
            rumoca_ir_dae::PureBuiltin::NoEvent,
        ] {
            let (_, provenance) = builtins
                .iter()
                .find(|(builtin, _)| *builtin == expected)
                .expect("checked DAE retains the typed builtin");
            assert_eq!(
                provenance.origin(),
                rumoca_ir_dae::DaeProvenanceOrigin::Source
            );
            assert!(
                dae.source_text(*provenance)
                    .is_some_and(|source| source.contains(match expected {
                        rumoca_ir_dae::PureBuiltin::Smooth => "smooth",
                        rumoca_ir_dae::PureBuiltin::NoEvent => "noEvent",
                        _ => unreachable!(),
                    }))
            );
        }
    });

    let result = simulate_dae(&dae, &SimOptions::default())
        .expect("typed smooth/noEvent programs must execute");
    let y = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("builtin result column");
    assert!(
        result.data[y]
            .iter()
            .all(|value| (value - 0.5).abs() <= 1.0e-10)
    );
}

#[test]
fn checked_relation_root_and_reinitialization_execute_end_to_end() {
    let dae = compile(
        concat!(
            "model ClocklessReinit\n",
            "  Real x(start=0);\n",
            "equation\n",
            "  der(x) = 1;\n",
            "  when x >= 1 then\n",
            "    reinit(x, 0);\n",
            "  end when;\n",
            "end ClocklessReinit;\n",
        ),
        "ClocklessReinit",
    );
    let options = SimOptions {
        t_end: 2.2,
        dt: Some(0.1),
        ..SimOptions::default()
    };

    let result =
        simulate_dae(&dae, &options).expect("checked relation event and reinit must execute");
    let x = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state result column");
    let final_x = result.data[x].last().copied().expect("final state value");

    assert!(
        (final_x - 0.2).abs() <= 2.0e-3,
        "two checked root-triggered reinitializations should leave x≈0.2, found {final_x}"
    );
}

#[test]
fn checked_termination_action_preserves_message_and_event_time() {
    let dae = compile(
        concat!(
            "model StopAtThreshold\n",
            "  Real x(start=0);\n",
            "equation\n",
            "  der(x) = 1;\n",
            "  when x >= 0.25 then\n",
            "    terminate(\"threshold reached\");\n",
            "  end when;\n",
            "end StopAtThreshold;\n",
        ),
        "StopAtThreshold",
    );
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.05),
        ..SimOptions::default()
    };

    let result = simulate_dae(&dae, &options).expect("checked termination action must execute");
    let termination = result
        .termination
        .expect("termination action must stop the simulation");

    assert!((termination.time - 0.25).abs() <= 2.0e-5);
    assert_eq!(termination.message, "threshold reached");
}

#[test]
fn checked_assertion_fails_with_its_source_message() {
    let dae = compile(
        concat!(
            "model CheckedAssertion\n",
            "  Real x(start=0);\n",
            "equation\n",
            "  der(x) = 1;\n",
            "  assert(x < 0.25, \"bound violated\");\n",
            "end CheckedAssertion;\n",
        ),
        "CheckedAssertion",
    );
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.05),
        ..SimOptions::default()
    };

    let error = simulate_dae(&dae, &options)
        .expect_err("a checked failing assertion must stop instead of returning a plausible trace");

    assert!(error.to_string().contains("bound violated"), "{error}");
}

#[test]
fn checked_constant_false_assertion_fails_at_initial_event() {
    let dae = compile(
        concat!(
            "model InitialAssertion\n",
            "  Real x(start=0);\n",
            "equation\n",
            "  der(x) = 0;\n",
            "  assert(false, \"initial invariant violated\");\n",
            "end InitialAssertion;\n",
        ),
        "InitialAssertion",
    );

    let error = simulate_dae(&dae, &SimOptions::default())
        .expect_err("a constant false assertion must not be consumed while seeding discrete rows");

    assert!(
        error.to_string().contains("initial invariant violated"),
        "{error}"
    );
}

#[test]
fn checked_pre_value_drives_self_rescheduling_discrete_updates() {
    let dae = compile(
        concat!(
            "model EventCounter\n",
            "  discrete Integer n(start=0);\n",
            "  Real x(start=0);\n",
            "  output Real y;\n",
            "equation\n",
            "  der(x) = 0;\n",
            "  y = n;\n",
            "  when time >= pre(n) + 1 then\n",
            "    n = pre(n) + 1;\n",
            "  end when;\n",
            "end EventCounter;\n",
        ),
        "EventCounter",
    );
    let options = SimOptions {
        t_end: 2.2,
        dt: Some(0.1),
        ..SimOptions::default()
    };

    let result =
        simulate_dae(&dae, &options).expect("checked pre-value update must execute at each root");
    let y = result
        .names
        .iter()
        .position(|name| name == "y")
        .unwrap_or_else(|| panic!("output result column; available={:?}", result.names));

    assert_eq!(result.data[y].last().copied(), Some(2.0));
}

#[test]
fn checked_event_trigger_does_not_reapply_at_a_branch_guard_root() {
    let dae = compile(
        concat!(
            "model TriggerDistinctFromBranch\n",
            "  discrete Integer n(start=0);\n",
            "  Real x(start=0);\n",
            "  output Real y;\n",
            "equation\n",
            "  der(x) = 1;\n",
            "  y = n;\n",
            "  when x >= 0.25 then\n",
            "    if x < 0.75 then\n",
            "      n = pre(n) + 1;\n",
            "    else\n",
            "      n = pre(n) + 10;\n",
            "    end if;\n",
            "  end when;\n",
            "end TriggerDistinctFromBranch;\n",
        ),
        "TriggerDistinctFromBranch",
    );
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.05),
        ..SimOptions::default()
    };

    let result = simulate_dae(&dae, &options)
        .expect("a branch-selection root must not retrigger its enclosing when");
    let y = result
        .names
        .iter()
        .position(|name| name == "y")
        .unwrap_or_else(|| panic!("output result column; available={:?}", result.names));

    assert_eq!(
        result.data[y].last().copied(),
        Some(1.0),
        "the persistent outer condition must not fire again when only its branch guard changes"
    );
}

#[test]
fn checked_when_elsewhen_priority_blocks_later_rise_while_first_is_true() {
    let dae = compile(
        concat!(
            "model PersistentFirstPriority\n",
            "  discrete Integer selected(start=0);\n",
            "  discrete Integer secondSeen(start=0);\n",
            "  Real x(start=0);\n",
            "  output Real selectedOut;\n",
            "  output Real secondSeenOut;\n",
            "equation\n",
            "  der(x) = 1;\n",
            "  selectedOut = selected;\n",
            "  secondSeenOut = secondSeen;\n",
            "  when x >= 0.25 then\n",
            "    selected = 1;\n",
            "  elsewhen x >= 0.75 then\n",
            "    selected = 2;\n",
            "  end when;\n",
            "  when x >= 0.75 then\n",
            "    secondSeen = 1;\n",
            "  end when;\n",
            "end PersistentFirstPriority;\n",
        ),
        "PersistentFirstPriority",
    );
    dae.inspect(|view| {
        assert_eq!(
            view.event_action_count(),
            3,
            "the checked DAE must retain both chain branches and the independent witness"
        );
    });
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.05),
        solver_mode: crate::SimSolverMode::RkLike,
        ..SimOptions::default()
    };

    let result = crate::rk45::simulate_dae(&dae, &options)
        .expect("checked when/elsewhen priority must execute");
    let selected = result
        .names
        .iter()
        .position(|name| name == "selectedOut")
        .unwrap_or_else(|| panic!("selected output column; available={:?}", result.names));
    let second_seen = result
        .names
        .iter()
        .position(|name| name == "secondSeenOut")
        .unwrap_or_else(|| panic!("second-event witness column; available={:?}", result.names));

    assert_eq!(
        result.data[second_seen].last().copied(),
        Some(1.0),
        "the independent witness must prove that the later condition rose"
    );
    assert_eq!(
        result.data[selected].last().copied(),
        Some(1.0),
        "a later rising condition must not outrank an earlier condition that remains true"
    );
}

#[test]
fn checked_when_elsewhen_priority_selects_first_on_simultaneous_rise() {
    let dae = compile(
        concat!(
            "model SimultaneousPriority\n",
            "  discrete Integer selected(start=0);\n",
            "  Real x(start=0);\n",
            "  output Real y;\n",
            "equation\n",
            "  der(x) = 1;\n",
            "  y = selected;\n",
            "  when x >= 0.5 then\n",
            "    selected = 1;\n",
            "  elsewhen x >= 0.5 then\n",
            "    selected = 2;\n",
            "  end when;\n",
            "end SimultaneousPriority;\n",
        ),
        "SimultaneousPriority",
    );
    dae.inspect(|view| {
        assert_eq!(
            view.event_action_count(),
            2,
            "the checked DAE must retain both simultaneous source branches"
        );
    });
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.05),
        solver_mode: crate::SimSolverMode::RkLike,
        ..SimOptions::default()
    };

    let result = crate::rk45::simulate_dae(&dae, &options)
        .expect("simultaneous checked when/elsewhen roots must execute");
    let y = result
        .names
        .iter()
        .position(|name| name == "y")
        .unwrap_or_else(|| panic!("priority output column; available={:?}", result.names));

    assert_eq!(
        result.data[y].last().copied(),
        Some(1.0),
        "the first source branch must win when multiple conditions rise simultaneously"
    );
}

#[test]
fn checked_when_elsewhen_later_branch_executes_after_first_becomes_false() {
    let dae = compile(
        concat!(
            "model SequentialPriority\n",
            "  discrete Integer selected(start=0);\n",
            "  Real x(start=0);\n",
            "  output Real y;\n",
            "equation\n",
            "  der(x) = 1;\n",
            "  y = selected;\n",
            "  when x >= 0.2 and x < 0.4 then\n",
            "    selected = 1;\n",
            "  elsewhen x >= 0.6 then\n",
            "    selected = 2;\n",
            "  end when;\n",
            "end SequentialPriority;\n",
        ),
        "SequentialPriority",
    );
    dae.inspect(|view| {
        assert_eq!(
            view.event_action_count(),
            2,
            "the checked DAE must retain both sequential source branches"
        );
    });
    let options = SimOptions {
        t_end: 0.8,
        dt: Some(0.05),
        solver_mode: crate::SimSolverMode::RkLike,
        ..SimOptions::default()
    };

    let result = crate::rk45::simulate_dae(&dae, &options)
        .expect("both checked when/elsewhen branches must execute in source order");
    let y = result
        .names
        .iter()
        .position(|name| name == "y")
        .unwrap_or_else(|| panic!("priority output column; available={:?}", result.names));

    assert!(
        result.data[y].contains(&1.0),
        "the trace must show the first branch executing before its condition becomes false"
    );
    assert_eq!(
        result.data[y].last().copied(),
        Some(2.0),
        "the same chain's later branch must execute after the first condition becomes false"
    );
}

#[test]
fn unprovided_input_is_rejected_instead_of_receiving_a_default_value() {
    let dae = compile(
        "model NeedsInput input Real u; output Real y; equation y = u; end NeedsInput;",
        "NeedsInput",
    );

    let error = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect_err("an input without a provider must fail before simulation");

    assert!(error.to_string().contains("input"));
    assert!(error.to_string().contains("provider"));
}
