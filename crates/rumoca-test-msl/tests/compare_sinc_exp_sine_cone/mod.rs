//! Pinned cone for `Modelica.Blocks.Examples.CompareSincExpSine`.
//!
//! This model wires nothing (no `connect` equations) and declares no
//! continuous state, yet each of its three signal sources contains an
//! `if time < startTime` guard. With `startTime = 0` the crossing is admitted
//! as a state-relation zero-crossing root rather than a scheduled time event,
//! so the checked DAE carries three relations, three roots, and three
//! conditions while `time_event_count` stays zero. This test measures that
//! cone rather than assuming it, and pins the Solve storage layout that the
//! three outputs and the one protected algebraic reach.
//!
//! The model depends on the whole Modelica Standard Library
//! (`Blocks.Sources.Sinc`, `Blocks.Sources.ExpSine`, `Modelica.Constants.pi`,
//! `Modelica.Icons.Example`), so it cannot be expressed as an inline source
//! string. It loads the cached MSL corpus through the same source-root path the
//! per-model worker uses, and compiles through the strict-reachable closure so
//! that unrelated MSL packages do not fail the requested model.

use rumoca_compile::compile::{Session, SessionConfig, SourceRootKind};
use rumoca_ir_dae::{VariableCausality, VariableRole};
use rumoca_ir_solve::{SolveStorageCoordinate, SolveVariableCausality, SolveVariableStorageRole};
use rumoca_sim::{
    DiffsolMethod, SimOptions, SimPacingMode, SimSolverMode, lower_dae_for_simulation,
};
use rumoca_solver::SimExecutionPolicy;

const MODEL: &str = "Modelica.Blocks.Examples.CompareSincExpSine";

fn simulation_options() -> SimOptions {
    // The horizon mirrors the model's `experiment(StopTime=1.0)` annotation.
    // Only the Solve storage projection is read here, so tolerances and the
    // execution policy do not affect the pinned facts.
    SimOptions {
        t_start: 0.0,
        t_end: 1.0,
        rtol: 1.0e-9,
        atol: 1.0e-9,
        dt: Some(0.0001),
        scalarize: false,
        max_wall_seconds: None,
        solver_mode: SimSolverMode::RkLike,
        diffsol_method: DiffsolMethod::Bdf,
        pacing_mode: SimPacingMode::AsFastAsPossible,
        param_overrides: Vec::new(),
        start_overrides: Vec::new(),
        execution_policy: SimExecutionPolicy::Interpreter,
    }
}

/// The checked DAE half: balance, event structure, and per-variable roles and
/// causality. Split from the Solve half so each obligation is one function; the
/// single compilation is still performed once by the caller, because compiling
/// this MSL model twice would double the test's corpus cost.
fn assert_checked_dae_structure(compilation: &rumoca_compile::compile::DaeCompilationResult) {
    // Balance: no state, one algebraic (the protected `sinc.x`), three outputs,
    // and four continuous equations (three output definitions plus the `x`
    // binding). The nonzero algebraic and output counts are not defaults, so
    // this pins the state count at zero without leaning on a zero default.
    assert_eq!(
        compilation.balance_detail,
        rumoca_phase_dae::BalanceDetail {
            state_unknowns: 0,
            algebraic_unknowns: 1,
            output_unknowns: 3,
            discrete_real_unknowns: 0,
            discrete_value_unknowns: 0,
            continuous_equations: 4,
            discrete_real_equations: 0,
            discrete_value_definitions: 0,
        }
    );

    compilation.dae.inspect(|view| {
        // Four continuous owners (the three output equations and the `x`
        // binding), no initialization or discrete owners.
        assert_eq!(view.continuous_owner_count(), 4);
        assert_eq!(view.initialization_owner_count(), 0);
        assert_eq!(view.discrete_real_equation_count(), 0);
        assert_eq!(view.discrete_value_owner_count(), 0);
        assert_eq!(view.model_event_transaction_count(), 0);

        // The corrected cone. Each `if time < startTime` guard becomes a
        // state-relation zero-crossing at startTime = 0, so relations, roots,
        // and conditions are each three while no time event is scheduled.
        // Pinning root_count = 3 alongside time_event_count = 0 distinguishes a
        // measured event structure from an event-free reading: were the three
        // guards ever reclassified as time events, root_count would fall to
        // zero and time_event_count would rise, and this pair would fail.
        assert_eq!(view.relation_count(), 3);
        assert_eq!(view.condition_count(), 3);
        assert_eq!(view.root_count(), 3);
        assert_eq!(view.time_event_count(), 0);
        assert_eq!(view.event_action_count(), 0);

        // No clocked, sampled, previous, terminal, or delayed structure.
        assert_eq!(view.clock_count(), 0);
        assert_eq!(view.previous_value_count(), 0);
        assert_eq!(view.terminal_count(), 0);
        assert_eq!(view.delay_count(), 0);

        // Variable inventory. Twenty-one declarations survive to the checked
        // DAE: seventeen retained parameters, three outputs, one algebraic.
        // The exact 21 and the per-role split are non-default counts, so an
        // inventory that silently collapsed (folding parameters, dropping the
        // protected algebraic) would fail here rather than pass vacuously.
        let variables: Vec<_> = view
            .variables()
            .map(|(_, variable)| {
                (
                    variable.name().as_str().to_owned(),
                    variable.role(),
                    variable.causality(),
                    variable.scalar_count(),
                )
            })
            .collect();
        assert_eq!(variables.len(), 21);

        let role_count = |role: VariableRole| {
            variables
                .iter()
                .filter(|(_, variable_role, _, _)| *variable_role == role)
                .count()
        };
        assert_eq!(role_count(VariableRole::Parameter), 17);
        assert_eq!(role_count(VariableRole::Output), 3);
        assert_eq!(role_count(VariableRole::Algebraic), 1);
        assert_eq!(role_count(VariableRole::State), 0);
        assert_eq!(role_count(VariableRole::Constant), 0);
        assert_eq!(role_count(VariableRole::Input), 0);
        assert_eq!(role_count(VariableRole::DiscreteReal), 0);
        assert_eq!(role_count(VariableRole::DiscreteValue), 0);

        // The three outputs. Their role is `Output`; their DAE causality is
        // `Local`, not `Output`. This corrects the reading that named them
        // "Output-causality" variables: the Output disposition lives in the
        // role, and each y is causality-Local inside the flattened model.
        for name in ["sinc.y", "expSine1.y", "expSine2.y"] {
            let (_, role, causality, scalar_count) = variables
                .iter()
                .find(|(variable_name, _, _, _)| variable_name == name)
                .unwrap_or_else(|| panic!("{name} is a checked DAE variable"));
            assert_eq!(*role, VariableRole::Output, "{name} role");
            assert_eq!(*causality, VariableCausality::Local, "{name} causality");
            assert_eq!(*scalar_count, 1, "{name} scalar count");
        }

        // The one protected algebraic inside Sinc: `x = 2*pi*f*(time-startTime)`.
        let (_, sinc_x_role, sinc_x_causality, sinc_x_scalars) = variables
            .iter()
            .find(|(variable_name, _, _, _)| variable_name == "sinc.x")
            .expect("sinc.x is a checked DAE variable");
        assert_eq!(*sinc_x_role, VariableRole::Algebraic);
        assert_eq!(*sinc_x_causality, VariableCausality::Local);
        assert_eq!(*sinc_x_scalars, 1);
    });
}

/// The Solve projection half: the Y vector has an empty state prefix, the one
/// algebraic occupies Y index 0, and the three outputs occupy the contiguous
/// tail.
fn assert_solve_y_placement(compilation: &rumoca_compile::compile::DaeCompilationResult) {
    // Solve projection. The DAE lowers to a Solve model whose Y vector has an
    // empty state prefix; the single algebraic occupies the first Y slot and
    // the three outputs occupy the trailing Y slots.
    let solve_model =
        lower_dae_for_simulation(&compilation.dae, &simulation_options()).expect("Solve lowering");
    let layout = solve_model.problem().solve_layout();
    assert_eq!(layout.state_scalar_count(), 0);
    assert_eq!(layout.algebraic_scalar_count(), 1);
    assert_eq!(layout.output_scalar_count(), 3);
    assert_eq!(solve_model.problem().layout().y_scalars(), 4);

    // Per-variable Y placement. The empty state prefix, the algebraic at Y index
    // 0, and the three outputs at the contiguous Y tail 1..4 are pinned exactly.
    // Because the algebraic and outputs share the Y vector, the outputs are the
    // tail after the algebraic, not immediately after the empty state prefix.
    let y_index = |name: &str| -> usize {
        let entry = solve_model
            .variable_catalog()
            .entries()
            .iter()
            .find(|entry| entry.name() == name)
            .unwrap_or_else(|| panic!("{name} has a Solve catalog entry"));
        assert_eq!(entry.storage().scalar_count, 1, "{name} scalar count");
        match entry.storage().base {
            SolveStorageCoordinate::Y(index) => index,
            other => panic!("{name} must occupy a Y slot, found {other:?}"),
        }
    };

    let sinc_x = solve_model
        .variable_catalog()
        .entries()
        .iter()
        .find(|entry| entry.name() == "sinc.x")
        .expect("sinc.x has a Solve catalog entry");
    assert_eq!(sinc_x.role(), SolveVariableStorageRole::Algebraic);
    assert_eq!(sinc_x.causality(), SolveVariableCausality::Local);
    assert_eq!(y_index("sinc.x"), 0);

    let mut output_slots = [
        ("sinc.y", y_index("sinc.y")),
        ("expSine1.y", y_index("expSine1.y")),
        ("expSine2.y", y_index("expSine2.y")),
    ];
    for (name, _) in output_slots {
        let entry = solve_model
            .variable_catalog()
            .entries()
            .iter()
            .find(|entry| entry.name() == name)
            .unwrap_or_else(|| panic!("{name} has a Solve catalog entry"));
        assert_eq!(
            entry.role(),
            SolveVariableStorageRole::Output,
            "{name} role"
        );
        assert_eq!(
            entry.causality(),
            SolveVariableCausality::Local,
            "{name} causality"
        );
    }
    output_slots.sort_by_key(|(_, index)| *index);
    assert_eq!(
        output_slots.map(|(_, index)| index),
        [1, 2, 3],
        "the three outputs occupy the contiguous Y tail after the single algebraic"
    );
}

/// The connectionless three-source model reaches a checked DAE with zero
/// continuous states, three state-relation zero-crossing roots, no time
/// events, three `Output`-role outputs, and one protected algebraic; the
/// Solve projection places that algebraic and those three outputs in the Y
/// vector with an empty state prefix.
#[test]
fn compare_sinc_exp_sine_reaches_three_state_relation_roots_and_no_time_events() {
    let msl_dir = crate::ensure_msl_downloaded().expect("MSL corpus is available");
    let mut session = Session::new(SessionConfig::default());
    let report =
        session.load_source_root_tolerant("msl", SourceRootKind::DurableExternal, &msl_dir, None);
    assert!(
        report.diagnostics.is_empty(),
        "the MSL source root loads without diagnostics: {:?}",
        report.diagnostics
    );

    // Strict-reachable compilation, the same closure mode the per-model worker
    // uses. Whole-corpus `compile_model` would fail on unrelated MSL packages.
    let compilation = session
        .compile_model_dae_strict_reachable_uncached_with_recovery(MODEL)
        .unwrap_or_else(|error| panic!("{MODEL} reaches checked DAE: {error}"));

    assert_checked_dae_structure(&compilation);
    assert_solve_y_placement(&compilation);
}
