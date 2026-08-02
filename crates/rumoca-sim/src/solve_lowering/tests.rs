use rumoca_compile::compile::{Session, SessionConfig};

use super::entry::lower_dae_for_simulation;
use crate::{SimOptions, simulate_dae};

fn compile(source: &str, model: &str) -> std::sync::Arc<rumoca_ir_dae::Dae> {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("simulation_checked_dae.mo", source)
        .expect("fixture parses");
    session
        .compile_model(model)
        .expect("fixture compiles through checked ToDAE")
        .dae
}

/// Source-ordered plan of the checked B.1c discrete-value owners: each entry pairs an
/// owner's target names with the number of `when` branches it retains.
///
/// Discrete assignments used to be flattened into the event-action arena, so the
/// when/elsewhen retention tests below counted `event_action_count()`. The checked DAE now
/// models them as discrete-value owners carrying an ordered branch list, and the event
/// action arena holds only `assert`/`terminate`/`reinit`. Counting retained `when` branches
/// per owner asserts the same property more precisely: it proves both that no chain branch
/// was dropped and that the branches stayed attached to their own target.
fn when_branch_plan(dae: &rumoca_ir_dae::Dae) -> Vec<(Vec<String>, usize)> {
    dae.inspect(|view| {
        (0..view.discrete_value_owner_count())
            .map(|index| {
                let id = view
                    .discrete_value_owner_id(index)
                    .expect("dense discrete-value owner identity resolves");
                let owner = view
                    .discrete_value_owner(id)
                    .expect("checked discrete-value owner resolves");
                let targets = owner
                    .targets()
                    .iter()
                    .map(|target| {
                        view.variable(rumoca_ir_dae::VariableId::from(target))
                            .expect("checked discrete-value target resolves")
                            .name()
                            .as_str()
                            .to_string()
                    })
                    .collect::<Vec<_>>();
                let when_branches = owner
                    .branches()
                    .iter()
                    .filter(|branch| {
                        matches!(
                            branch.activation(),
                            rumoca_ir_dae::DiscreteBranchActivation::When { .. }
                        )
                    })
                    .count();
                (targets, when_branches)
            })
            .collect()
    })
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

/// An `elsewhen` branch runs at its own rising edge even while the earlier
/// branch's condition is still true.
///
/// MLS §8.3.5 activates the equations of a when-equation *"only at the instant
/// when the scalar expression or any of the elements of the vector expression
/// becomes true"*, and §8.3.5.1 writes the chain as one if-expression per
/// assigned variable over `edge(b1)`, `edge(b2)`, …. `x >= 0.25` stays true past
/// `t = 0.75`, but it stopped *becoming* true at `t = 0.25`, so it holds no
/// claim on the later instant.
///
/// `omc` (dassl, `stopTime = 1.0`, `numberOfIntervals = 20`) runs the second
/// branch: `selected` is `0` through `t = 0.25`, `1` from the `0.25` right-limit
/// row, and `2` from the `0.75` right-limit row, alongside the independent
/// `secondSeen` witness which also steps to `1` there. rumoca used to hold
/// `selected = 1` for the whole run, because the branch guard subtracted the
/// earlier branch's *level* rather than its edge.
///
/// The `secondSeen` witness is kept because it is what separates the two ways
/// the second branch can fail to run: a condition that never rose at all, and a
/// condition that rose but was outranked. Only the second is the branch guard.
#[test]
fn checked_when_elsewhen_runs_its_later_branch_while_the_first_is_still_true() {
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
    assert_eq!(
        when_branch_plan(&dae),
        vec![
            (vec!["selected".to_string()], 2),
            (vec!["secondSeen".to_string()], 1),
        ],
        "the checked DAE must retain both chain branches and the independent witness"
    );
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
        Some(2.0),
        "the elsewhen branch runs at its own rising edge, as omc does: an earlier \
         condition that merely remains true has no edge left to outrank it with"
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
    assert_eq!(
        when_branch_plan(&dae),
        vec![(vec!["selected".to_string()], 2)],
        "the checked DAE must retain both simultaneous source branches"
    );
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
    assert_eq!(
        when_branch_plan(&dae),
        vec![(vec!["selected".to_string()], 2)],
        "the checked DAE must retain both sequential source branches"
    );
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

    // Checked inputs may now carry a declaration default, so the rejection names the two
    // sources it looked for instead of only the missing runtime provider.
    assert!(
        error
            .to_string()
            .contains("input `u` has neither a checked default nor a runtime value"),
        "{error}"
    );
}

/// GPU preparation hands the browser each input's `P` slot and the browser writes it
/// before every dispatch, so the prepared vectors only have to state the value that slot
/// holds until the first write: the declared `start` (MLS §4.4.2.1). Reading only the
/// binding rejected every shipped interactive model — `input Real throttle(start = 0)` in
/// `examples/interactive/rover` — and aborted `prepare_gpu_simulation` before any shader
/// was rendered.
#[test]
fn gpu_preparation_seeds_host_driven_inputs_from_their_declared_start() {
    let dae = compile(
        concat!(
            "model HostDrivenInput\n",
            "  parameter Real u0 = 2.0;\n",
            "  input Real u_cmd(start = u0);\n",
            "  Real x(start = u0, fixed = true);\n",
            "equation\n",
            "  der(x) = u_cmd - x;\n",
            "end HostDrivenInput;\n",
        ),
        "HostDrivenInput",
    );

    let prepared = super::entry::lower_dae_for_gpu_preparation(&dae, &SimOptions::default())
        .expect("a host-driven input carries its declared start into the prepared vectors");
    let slot = prepared
        .problem
        .layout
        .binding("u_cmd")
        .expect("the input keeps a storage slot the host can write");
    let rumoca_ir_solve::ScalarSlot::P { index, .. } = slot else {
        panic!("a host-driven input belongs in parameter storage, got {slot:?}");
    };
    assert_eq!(
        prepared.parameters.get(index).copied(),
        Some(2.0),
        "the seeded slot must hold the declared start, not a stand-in"
    );

    // The strict rule for headless simulation is untouched: the same model still has no
    // provider when nothing drives it.
    let error = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect_err("plain simulation still refuses an undriven input");
    assert!(
        error
            .to_string()
            .contains("input `u_cmd` has neither a checked default nor a runtime value"),
        "{error}"
    );
}

/// A `String` declaration carries no numeric value (MLS §3.8.4), so it must not be asked
/// for one while the runtime vectors are built. Every clocked partition in the MSL
/// declares `Modelica.Clocked.Types.SolverMethod solverMethod`, which made this the
/// failure mode of `Modelica.Clocked.Examples.Elementary.IntegerSignals.TimeBasedStep`.
#[test]
fn string_declaration_does_not_block_numeric_runtime_vectors() {
    let dae = compile(
        concat!(
            "model StringParameter\n",
            "  parameter String method = \"ExplicitEuler\";\n",
            "  Real x(start=1);\n",
            "equation\n",
            "  der(x) = -x;\n",
            "end StringParameter;\n",
        ),
        "StringParameter",
    );

    let solve = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("a String declaration must not be evaluated as a numeric runtime value");

    assert!(
        !solve.visible_names.iter().any(|name| name == "method"),
        "a String declaration has no numeric trace column: {:?}",
        solve.visible_names
    );

    let result = simulate_dae(&dae, &SimOptions::default())
        .expect("a model carrying a String parameter must still simulate");
    let x = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state result column");
    let final_x = result.data[x].last().copied();
    assert!(
        final_x.is_some_and(|value| (value - (-1.0_f64).exp()).abs() <= 1.0e-6),
        "expected x(1)≈exp(-1), found {final_x:?} at {:?}",
        result.times.last()
    );
}

/// MLS §8.6 leaves a `parameter` declared `fixed = false` without a value of its own: the
/// initialization equations determine it and `start` is only the iteration guess. This is
/// `Modelica.Electrical.Analog.Basic.SaturatingInductor.Ipar`, and the whole reason
/// `ShowSaturatingInductor` could not initialize.
#[test]
fn fixed_false_parameter_is_solved_from_its_initial_equation() {
    let dae = compile(
        concat!(
            "model UnsolvedParameter\n",
            "  parameter Real q(start=3, fixed=false);\n",
            "  Real x(start=1);\n",
            "initial equation\n",
            "  q*q = 4;\n",
            "equation\n",
            "  der(x) = -q*x;\n",
            "end UnsolvedParameter;\n",
        ),
        "UnsolvedParameter",
    );

    let solve = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("a `fixed = false` parameter is an initialization unknown, not a constant");
    let [block] = solve
        .problem
        .initialization
        .projection_plan
        .blocks
        .as_slice()
    else {
        panic!(
            "one initialization projection block expected, got {:?}",
            solve.problem.initialization.projection_plan.blocks
        );
    };
    assert_eq!(block.rows.len(), 1);
    assert_eq!(block.unknowns.len(), 1);
    assert!(matches!(
        block.unknowns[0],
        rumoca_ir_solve::ScalarSlot::P { .. }
    ));

    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.5),
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options)
        .expect("the initialization system must solve the `fixed = false` parameter");
    let x = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state result column");
    // q solves to 2 (the positive root nearest the start guess), so x(t) = exp(-2 t).
    assert!(
        result.data[x]
            .last()
            .is_some_and(|value| (value - (-2.0_f64).exp()).abs() <= 1.0e-5),
        "x must decay at the solved parameter rate, got {:?}",
        result.data[x].last()
    );
}

/// The predefined Real `start = 0.0` is still the initialization guess when a
/// `fixed = false` parameter does not spell a `start` modifier. It is not the
/// parameter's value: the initialization row remains its sole value owner.
#[test]
fn fixed_false_parameter_without_explicit_start_uses_the_checked_default_guess() {
    let dae = compile(
        concat!(
            "model DefaultParameterGuess\n",
            "  parameter Real q(fixed=false);\n",
            "  Real x(start=1);\n",
            "initial equation\n",
            "  q = 2;\n",
            "equation\n",
            "  der(x) = -q*x;\n",
            "end DefaultParameterGuess;\n",
        ),
        "DefaultParameterGuess",
    );

    let solve = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("the default start is a guess for the initialization unknown");
    assert!(matches!(
        solve
            .problem
            .initialization
            .projection_plan
            .blocks
            .as_slice(),
        [block]
            if matches!(
                block.unknowns.as_slice(),
                [rumoca_ir_solve::ScalarSlot::P { .. }]
            )
    ));

    let options = SimOptions {
        t_end: 0.5,
        dt: Some(0.5),
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options)
        .expect("the initialization equation, not the default guess, determines q");
    assert!(
        column(&result, "x")
            .last()
            .is_some_and(|value| (value - (-1.0_f64).exp()).abs() <= 1.0e-5),
        "q = 2 must determine the trajectory"
    );
}

/// The column of a simulation result, by variable name.
fn column<'result>(result: &'result crate::SimResult, name: &str) -> &'result [f64] {
    let index = result
        .names
        .iter()
        .position(|column| column == name)
        .unwrap_or_else(|| panic!("`{name}` has a result column, got {:?}", result.names));
    &result.data[index]
}

/// The initialization projection blocks a model lowers to, as
/// `(row count, unknown slots)` pairs.
fn projection_blocks(dae: &rumoca_ir_dae::Dae) -> Vec<(usize, Vec<rumoca_ir_solve::ScalarSlot>)> {
    let solve = lower_dae_for_simulation(dae, &SimOptions::default())
        .expect("the fixture lowers to a Solve problem");
    solve
        .problem
        .initialization
        .projection_plan
        .blocks
        .iter()
        .map(|block| (block.rows.len(), block.unknowns.clone()))
        .collect()
}

/// MLS 3.6 §8.6 makes the *states* unknowns of the initialization system too, not
/// only the `fixed = false` parameters: §4.8.1 gives `fixed` the default `false`
/// for everything that is not a parameter, and §8.6 says of such a start only that
/// it "is used as a guess value". So `initial equation x = 5` determines `x(0)`,
/// and the declared `start = 0` is the guess the projection begins from.
///
/// OpenModelica (`omc`) simulates the same source to `x(0) = 5`.
#[test]
fn state_is_solved_from_its_initial_equation() {
    let dae = compile(
        concat!(
            "model InitState\n",
            "  Real x(start=0, fixed=false);\n",
            "equation\n",
            "  der(x) = -x;\n",
            "initial equation\n",
            "  x = 5;\n",
            "end InitState;\n",
        ),
        "InitState",
    );

    assert!(
        matches!(
            projection_blocks(&dae).as_slice(),
            [(1, unknowns)] if matches!(unknowns.as_slice(), [rumoca_ir_solve::ScalarSlot::Y { .. }])
        ),
        "the initial equation owns one block over the state's solver slot, got {:?}",
        projection_blocks(&dae)
    );

    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.5),
        ..SimOptions::default()
    };
    let result =
        simulate_dae(&dae, &options).expect("the initialization system must solve the state");
    let x = column(&result, "x");
    assert!(
        (x[0] - 5.0).abs() <= 1.0e-12,
        "`initial equation x = 5` fixes x(0) exactly, got {}",
        x[0]
    );
    assert!(
        x.last()
            .is_some_and(|value| (value - 5.0 * (-1.0_f64).exp()).abs() <= 1.0e-5),
        "x decays from the solved initial value, got {:?}",
        x.last()
    );
}

/// MLS 3.6 §8.6: "For every Real variable `vc` with `fixed = true`, the equation
/// `vc = startExpression` is added to the initialization equations." That start is
/// therefore an *equation*, and the coordinate it determines must never also be a
/// projection unknown — the runtime seeds it and the projection would be a second
/// owner of the same storage slot. The stated value stands, and the initial
/// equation restating it stays a consistency check the residual test still has to
/// satisfy.
#[test]
fn fixed_true_state_is_not_an_initialization_projection_unknown() {
    let dae = compile(
        concat!(
            "model PinnedState\n",
            "  Real x(start=2, fixed=true);\n",
            "equation\n",
            "  der(x) = 0;\n",
            "initial equation\n",
            "  x = 2;\n",
            "end PinnedState;\n",
        ),
        "PinnedState",
    );

    assert_eq!(
        projection_blocks(&dae),
        Vec::new(),
        "a `fixed = true` state is determined by its own declaration"
    );

    let options = SimOptions {
        t_end: 1.0,
        dt: Some(1.0),
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options).expect("the restated value is consistent");
    assert!((column(&result, "x")[0] - 2.0).abs() <= 1.0e-12);
}

/// One §8.6 system solving a state and a `fixed = false` parameter together.
///
/// The two coordinates live in different runtime storage — the state in the solver
/// vector, the parameter in the parameter vector — so this is also what proves the
/// projection plans one block across both.
#[test]
fn a_state_and_a_fixed_false_parameter_are_solved_by_one_initialization_system() {
    let dae = compile(
        concat!(
            "model MixedInit\n",
            "  parameter Real q(start=1, fixed=false);\n",
            "  Real x(start=0, fixed=false);\n",
            "equation\n",
            "  der(x) = 0;\n",
            "initial equation\n",
            "  x + q = 5;\n",
            "  x - q = 1;\n",
            "end MixedInit;\n",
        ),
        "MixedInit",
    );

    let blocks = projection_blocks(&dae);
    let [(rows, unknowns)] = blocks.as_slice() else {
        panic!("one coupled initialization block expected, got {blocks:?}");
    };
    assert_eq!(*rows, 2);
    assert_eq!(unknowns.len(), 2);
    assert!(
        unknowns
            .iter()
            .any(|slot| matches!(slot, rumoca_ir_solve::ScalarSlot::P { .. }))
            && unknowns
                .iter()
                .any(|slot| matches!(slot, rumoca_ir_solve::ScalarSlot::Y { .. })),
        "the block owns both the parameter and the state slot, got {unknowns:?}"
    );

    let options = SimOptions {
        t_end: 1.0,
        dt: Some(1.0),
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options).expect("the 2x2 initialization system is solvable");
    assert!(
        (column(&result, "x")[0] - 3.0).abs() <= 1.0e-9,
        "x + q = 5 and x - q = 1 give x = 3, got {}",
        column(&result, "x")[0]
    );
}

/// MLS 3.6 §8.6 makes `der(x)` an unknown of the initialization system, and the
/// der-equations active at the initial instant. The Solve lowering discharges that
/// by substitution rather than by extra rows: the structural matching already named
/// the continuous row that determines `der(x)`, so `initial equation der(x) = 0` is
/// a residual over `x` alone and solves the steady state.
#[test]
fn a_steady_state_initial_equation_solves_the_state_it_constrains() {
    let dae = compile(
        concat!(
            "model SteadyInit\n",
            "  Real x(start=0, fixed=false);\n",
            "equation\n",
            "  der(x) = 3 - 2*x;\n",
            "initial equation\n",
            "  der(x) = 0;\n",
            "end SteadyInit;\n",
        ),
        "SteadyInit",
    );

    let options = SimOptions {
        t_end: 1.0,
        dt: Some(1.0),
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options).expect("the steady-state condition is solvable");
    let x = column(&result, "x");
    assert!(
        (x[0] - 1.5).abs() <= 1.0e-9,
        "der(x) = 0 with der(x) = 3 - 2x gives x(0) = 1.5, got {}",
        x[0]
    );
    assert!(
        x.last().is_some_and(|value| (value - 1.5).abs() <= 1.0e-6),
        "a steady state must not drift, got {:?}",
        x.last()
    );
}

/// A component whose rows cannot cover every state falls back, for the states left
/// over, to the guess their `start` carries — MLS 3.6 §4.8.1's default-`fixed`
/// reading, and the value the runtime already seeds. The rest of the component is
/// still planned around them, so one row still determines one state instead of the
/// whole system reverting to a residual nothing can satisfy.
///
/// *Which* state keeps its guess is a choice no part of MLS §8.6 makes, and it
/// diverges from OpenModelica; `rumoca_phase_solve`'s `initial_projection` module
/// header records the divergence and why neither answer is more correct. The
/// assertions below pin *this* choice so it stays a recorded fact rather than
/// silent drift.
#[test]
fn an_under_determined_state_component_keeps_the_remaining_start_guesses() {
    let dae = compile(
        concat!(
            "model ShortRows\n",
            "  Real x(start=0, fixed=false);\n",
            "  Real y(start=3, fixed=false);\n",
            "equation\n",
            "  der(x) = 0;\n",
            "  der(y) = 0;\n",
            "initial equation\n",
            "  x + y = 5;\n",
            "end ShortRows;\n",
        ),
        "ShortRows",
    );

    let blocks = projection_blocks(&dae);
    let [(rows, unknowns)] = blocks.as_slice() else {
        panic!("one square block expected, got {blocks:?}");
    };
    assert_eq!(
        (*rows, unknowns.len()),
        (1, 1),
        "a block is square: one row determines one state, never two in least squares"
    );

    let options = SimOptions {
        t_end: 1.0,
        dt: Some(1.0),
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options).expect("the reduced system is solvable");
    assert!(
        (column(&result, "y")[0] - 3.0).abs() <= 1.0e-12,
        "the unmatched state keeps its start guess, got {}",
        column(&result, "y")[0]
    );
    assert!(
        (column(&result, "x")[0] - 2.0).abs() <= 1.0e-9,
        "the matched state satisfies the row, got {}",
        column(&result, "x")[0]
    );
}

/// When one row must choose, the `fixed = false` parameter takes it.
///
/// MLS 3.6 §8.6 gives such a parameter no value at all without an initialization
/// equation ("there must be additional equations for them"), while §4.8.1 leaves a
/// state's unstated `fixed` at `false`, whose `start` §8.6 still calls a guess the
/// runtime seeds. So spending the single row on the parameter determines both
/// coordinates, and spending it on the state would leave the parameter's guess
/// masquerading as its value.
#[test]
fn one_row_between_a_parameter_and_a_state_is_spent_on_the_parameter() {
    let dae = compile(
        concat!(
            "model ShortParameterRows\n",
            "  parameter Real q(start=1, fixed=false);\n",
            "  Real x(start=0, fixed=false);\n",
            "equation\n",
            "  der(x) = 0;\n",
            "initial equation\n",
            "  x + q = 5;\n",
            "end ShortParameterRows;\n",
        ),
        "ShortParameterRows",
    );

    let blocks = projection_blocks(&dae);
    let [(rows, unknowns)] = blocks.as_slice() else {
        panic!("one square block expected, got {blocks:?}");
    };
    assert_eq!(*rows, 1);
    assert!(
        matches!(unknowns.as_slice(), [rumoca_ir_solve::ScalarSlot::P { .. }]),
        "the row determines the parameter, not the state, got {unknowns:?}"
    );

    let options = SimOptions {
        t_end: 1.0,
        dt: Some(1.0),
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options).expect("the reduced system is solvable");
    assert!(
        (column(&result, "x")[0]).abs() <= 1.0e-12,
        "the state keeps its start guess, got {}",
        column(&result, "x")[0]
    );
}

/// A `fixed = false` parameter has no fallback: MLS 3.6 §8.6 says "there must be
/// additional equations for them". A component whose rows cannot determine one is
/// therefore left entirely unplanned, keeping the typed residual failure rather
/// than shipping the parameter's guess as if it were its value.
#[test]
fn a_component_that_cannot_determine_a_fixed_false_parameter_is_left_unplanned() {
    let dae = compile(
        concat!(
            "model TwoUnsolvedParameters\n",
            "  parameter Real q(start=1, fixed=false);\n",
            "  parameter Real r(start=1, fixed=false);\n",
            "  Real x(start=0, fixed=true);\n",
            "equation\n",
            "  der(x) = q + r;\n",
            "initial equation\n",
            "  q + r = 5;\n",
            "end TwoUnsolvedParameters;\n",
        ),
        "TwoUnsolvedParameters",
    );

    assert_eq!(
        projection_blocks(&dae),
        Vec::new(),
        "one row cannot determine two `fixed = false` parameters"
    );
    let error = simulate_dae(&dae, &SimOptions::default())
        .expect_err("the unowned parameter leaves a residual the initialization cannot satisfy")
        .to_string();
    assert!(
        error.contains("outside the planned initialization unknown space")
            && error.contains("could not give a row of its own"),
        "an under-determined initialization names the unknown nothing solved, got: {error}"
    );
}

/// A failed initialization must name a coordinate, never only a residual row
/// index. The two answers the planner can give are both diagnostics: a row a
/// block solves reports the coordinate that block owns, and a row no block solves
/// reports that it is a §8.6 consistency check the rest of the system contradicts.
#[test]
fn a_failed_initialization_names_the_coordinate_its_row_was_planned_to_determine() {
    let unsolvable = compile(
        concat!(
            "model NoRoot\n",
            "  Real x(start=1, fixed=false);\n",
            "equation\n",
            "  der(x) = 0;\n",
            "initial equation\n",
            "  x*x + 1 = 0;\n",
            "end NoRoot;\n",
        ),
        "NoRoot",
    );
    let error = simulate_dae(&unsolvable, &SimOptions::default())
        .expect_err("x*x + 1 = 0 has no real root")
        .to_string();
    assert!(
        error.contains("target=x"),
        "the failure names the state the block was planned to determine, got: {error}"
    );

    let contradicted = compile(
        concat!(
            "model Contradicted\n",
            "  Real x(start=0, fixed=true);\n",
            "equation\n",
            "  der(x) = 0;\n",
            "initial equation\n",
            "  x = 5;\n",
            "end Contradicted;\n",
        ),
        "Contradicted",
    );
    let error = simulate_dae(&contradicted, &SimOptions::default())
        .expect_err("a `fixed = true` start of 0 contradicts `initial equation x = 5`")
        .to_string();
    assert!(
        error.contains("owner=surplus-check"),
        "a row over coordinates the rest of the system determined is a §8.6 consistency \
         check, got: {error}"
    );
}

/// A row no block solves is not automatically a surplus check, and calling it one
/// names the wrong defect. MLS 3.6 §8.6 makes a surplus row legal — a coordinate a
/// declaration determines may still be read by another initialization equation —
/// so failing one means two declarations contradict each other. A row over a
/// coordinate the projection never owned is the opposite: nothing solved that
/// coordinate. The two must not share a message.
///
/// `Modelica.Electrical.Analog.Examples.IdealTriacCircuit` is the MSL model this
/// separates: its failing row reads a discrete coordinate, and the old message
/// called it a consistency check.
#[test]
fn an_unowned_initialization_row_is_not_reported_as_a_surplus_check() {
    let discrete_read = compile(
        concat!(
            "model UnownedDiscrete\n",
            "  Real x(start=0, fixed=false);\n",
            "  discrete Real d(start=0, fixed=true);\n",
            "equation\n",
            "  der(x) = 0;\n",
            "  when time > 0.5 then\n",
            "    d = 1;\n",
            "  end when;\n",
            "initial equation\n",
            "  x = d + 2;\n",
            "end UnownedDiscrete;\n",
        ),
        "UnownedDiscrete",
    );
    let error = simulate_dae(&discrete_read, &SimOptions::default())
        .expect_err("a discrete coordinate is outside the planned unknown space")
        .to_string();
    assert!(
        error.contains("outside the planned initialization unknown space")
            && error.contains("discrete-time coordinate"),
        "an unowned discrete read names its kind, got: {error}"
    );
    assert!(
        !error.contains("surplus-check"),
        "a row nothing solved must not be reported as a surplus check, got: {error}"
    );

    let algebraic_read = compile(
        concat!(
            "model UnownedAlgebraic\n",
            "  Real x(start=0, fixed=false);\n",
            "  Real a;\n",
            "equation\n",
            "  a = 2*time + 5;\n",
            "  der(x) = a - x;\n",
            "initial equation\n",
            "  x = a + 7;\n",
            "end UnownedAlgebraic;\n",
        ),
        "UnownedAlgebraic",
    );
    let error = simulate_dae(&algebraic_read, &SimOptions::default())
        .expect_err("the reduced initialization solve does not own the algebraic dependency")
        .to_string();
    assert!(
        error.contains("algebraic/output") && error.contains("total derivative"),
        "an unowned algebraic read names the missing reduced-solve capability, got: {error}"
    );
}

/// Initialization residual certification observes settled algebraic/output
/// values, never their declaration seeds. This does not pretend that an
/// algebraic-reading row is already part of the reduced projection unknown
/// space: the unsupported steady-state shape fails closed with its typed owner,
/// while a system the existing projection can solve is certified against the
/// freshly reconstructed algebraic value.
#[test]
fn an_algebraic_reading_initialization_row_cannot_certify_against_a_stale_seed() {
    const SOURCE: &str = concat!(
        "model AlgebraicSeed\n",
        "  Real x(start=0, fixed=false);\n",
        "  Real a;\n",
        "equation\n",
        "  a = 2*time + 5;\n",
        "  der(x) = a - x;\n",
        "initial equation\n",
    );

    let steady = compile(
        &format!("{SOURCE}  der(x) = 0;\nend AlgebraicSeed;\n"),
        "AlgebraicSeed",
    );
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(1.0),
        ..SimOptions::default()
    };
    let error = simulate_dae(&steady, &options)
        .expect_err("the stale zero seeds must not certify x(0) = 0")
        .to_string();
    assert!(
        error.contains("algebraic/output") && error.contains("planned initialization unknown"),
        "the unsupported coupled shape must fail with its typed capability owner, got: {error}"
    );

    let consistent = compile(
        &format!("{SOURCE}  x = 5;\n  x = a;\nend AlgebraicSeed;\n"),
        "AlgebraicSeed",
    );
    let result = simulate_dae(&consistent, &options)
        .expect("x = 5 and x = a agree after the algebraic is reconstructed at a(0) = 5");
    assert!((column(&result, "x")[0] - 5.0).abs() <= 1.0e-12);
}
