//! Bounded `SolveModel` fixtures the verification harnesses drive.
//!
//! Every model here is deliberately small: at most three activation conditions
//! or one continuous state. They are built from Solve IR directly rather than
//! compiled from Modelica so a test can vary one dimension without putting the
//! compiler itself inside a runtime-contract check.

use rumoca_ir_solve as solve;

/// The largest condition count the exhaustive condition-memory test explores.
#[cfg(test)]
pub(super) const MAX_CONDITIONS: usize = 3;

/// The constant every generated activation condition is compared against.
#[cfg(test)]
pub(super) const CONDITION_THRESHOLD: f64 = 2.0;

/// Parameter layout of [`condition_memory_model`] as a function of its
/// condition count `n`:
///
/// ```text
/// p[0 .. n)        the source parameter of each condition, `s_i`
/// p[n .. 2n)       the activation buffer of each condition, `b_i`
/// p[2n]            the `initial()` flag
/// p[2n + 1]        the `initial()` activation buffer
/// ```
#[derive(Clone, Copy)]
#[cfg(test)]
pub(super) struct ConditionLayout {
    count: usize,
}

#[cfg(test)]
impl ConditionLayout {
    pub(super) fn new(count: usize) -> Self {
        Self { count }
    }

    pub(super) fn count(self) -> usize {
        self.count
    }

    /// The `p` slot holding condition `index`'s source parameter.
    pub(super) fn source(self, index: usize) -> usize {
        index
    }

    /// The `p` slot holding condition `index`'s activation buffer.
    pub(super) fn buffer(self, index: usize) -> usize {
        self.count + index
    }

    /// The `p` slot holding the `initial()` flag.
    pub(super) fn initial_flag(self) -> usize {
        2 * self.count
    }

    /// The `p` slot holding the `initial()` activation buffer.
    pub(super) fn initial_buffer(self) -> usize {
        2 * self.count + 1
    }

    fn parameter_count(self) -> usize {
        2 * self.count + 2
    }
}

fn fixture_block(rows: Vec<Vec<solve::LinearOp>>, name: &'static str) -> solve::ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(name), 1, 2);
    solve::ScalarProgramBlock::with_source_span(
        rows,
        span.require_provenance("solve verification fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

/// A condition-memory row reading `p[source] > CONDITION_THRESHOLD`.
#[cfg(test)]
fn threshold_row(source: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP {
            dst: 0,
            index: source,
        },
        solve::LinearOp::Const {
            dst: 1,
            value: CONDITION_THRESHOLD,
        },
        solve::LinearOp::Compare {
            dst: 2,
            op: solve::CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

/// A condition-memory row reading the `initial()` flag.
#[cfg(test)]
fn initial_row(flag: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP {
            dst: 0,
            index: flag,
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

/// A stateless model with `starts.len()` relational activation conditions plus
/// one `initial()` activation, each owning its own MLS §8.3.5.1 buffer.
///
/// Every condition is `s_i > CONDITION_THRESHOLD` over its own start value, so
/// a caller picks which conditions are already true at the initialization
/// instant purely by choosing `starts`.
#[cfg(test)]
pub(super) fn condition_memory_model(starts: &[f64]) -> solve::SolveModel {
    let layout = ConditionLayout::new(starts.len());
    let mut rows = (0..layout.count())
        .map(|index| threshold_row(layout.source(index)))
        .collect::<Vec<_>>();
    rows.push(initial_row(layout.initial_flag()));
    let mut targets = (0..layout.count())
        .map(|index| solve::scalar_slot_p(layout.buffer(index)))
        .collect::<Vec<_>>();
    targets.push(solve::scalar_slot_p(layout.initial_buffer()));
    let row_count = rows.len();
    let mut parameters = starts.to_vec();
    parameters.resize(layout.parameter_count(), 0.0);
    // The backends raise `initial()` before initialization settles, which is
    // the state the seed has to cope with: it must clear the flag for its own
    // evaluation without clearing it for the event that follows.
    parameters[layout.initial_flag()] = 1.0;
    solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                parameter_count: layout.parameter_count(),
                compiled_parameter_len: layout.parameter_count(),
                initial_event_parameter_index: Some(layout.initial_flag()),
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                condition_memory_parameter_indices: (0..layout.count())
                    .map(|index| layout.buffer(index))
                    .chain(std::iter::once(layout.initial_buffer()))
                    .collect(),
                ..Default::default()
            },
            discrete: solve::DiscreteSolveSystem {
                rhs: fixture_block(rows, "cm.mo"),
                update_targets: targets,
                row_roles: vec![solve::DiscreteRowRole::ConditionMemory; row_count],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent; row_count],
                observation_refresh: vec![false; row_count],
                integrator_history_effects: vec![
                    solve::IntegratorHistoryEffect::Preserve;
                    row_count
                ],
                clock_owners: vec![None; row_count],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: parameters.into(),
        ..Default::default()
    }
}

/// `dx/dt = x`, one continuous state starting at 1.
///
/// The smallest model an FMI ME component can legally be instantiated from:
/// `validate_explicit_solve_model` rejects a model with no continuous states,
/// so the lifecycle harness needs exactly one.
pub(super) fn single_state_model() -> solve::SolveModel {
    let derivative = vec![vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]];
    crate::fixture_layout::with_visible_expressions(
        solve::SolveModel {
            problem: solve::SolveProblem {
                layout: crate::fixture_layout::named_y_layout(
                    rumoca_ir_solve::VarLayout::from_parts(Default::default(), 0, 0),
                    vec!["x".to_string()],
                ),
                solve_layout: solve::SolveLayout {
                    state_scalar_count: 1,
                    ..Default::default()
                },
                continuous: solve::ContinuousSolveSystem {
                    derivative_rhs: solve::ComputeBlock::from_scalar_program_block(fixture_block(
                        derivative, "ss.mo",
                    )),
                    ..Default::default()
                },
                ..Default::default()
            },
            initial_y: vec![1.0].into(),
            solver_nominals: vec![1.0],

            ..Default::default()
        },
        vec!["x".to_string()],
        fixture_block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "ss.mo",
        ),
        Vec::new(),
    )
}

/// [`single_state_model`] plus one writable scalar input parameter.
///
/// The ME buffer and instance-brand proofs need the smallest model that can
/// issue a real value reference and observe an accepted parameter value.
pub(super) fn single_state_input_model() -> solve::SolveModel {
    let mut model = single_state_model();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.input_scalar_names = vec!["u".to_string()].into();
    model.parameters = vec![1.0].into();
    model
}

/// [`single_state_model`] plus one static component-owned time event.
pub(super) fn single_state_time_event_model() -> solve::SolveModel {
    let mut model = single_state_model();
    model.problem.events.scheduled_time_events = vec![0.5];
    model
}

/// [`single_state_model`] plus one scalar event-indicator surface `x`.
pub(super) fn single_state_indicator_model() -> solve::SolveModel {
    let mut model = single_state_model();
    model.problem.events.root_conditions = fixture_block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "si.mo",
    );
    model.problem.events.root_relation_memory_targets = vec![None];
    model.problem.events.root_zero_domains = vec![solve::RootZeroDomain::Previous];
    model
}

/// [`single_state_model`] plus one initialization update row that can never
/// settle: `q := q + increment` rewrites its own source every pass.
///
/// MLS §8.6 initialization is a fixed point, so a runtime that iterates it must
/// bound the iteration. This model is what forces that bound to be observed.
pub(super) fn divergent_initialization_model(increment: f64) -> solve::SolveModel {
    let mut model = single_state_model();
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.parameters = vec![0.0].into();
    let mut initialization = model.problem.initialization.clone().into_input();
    initialization.update_rhs = fixture_block(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: increment,
            },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        "di.mo",
    );
    initialization.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.initialization = solve::InitializationSolveSystem::construct(initialization)
        .expect("fixture initialization ownership is disjoint");
    model
}

/// A one-state model whose discrete equation settles during `initial()` but
/// diverges at a later runtime event: `q := if initial() or time <= 0 then q
/// else q + increment`. The mandatory post-initial pass still settles at zero.
pub(super) fn divergent_runtime_event_model(increment: f64) -> solve::SolveModel {
    let mut model = single_state_model();
    model.problem.solve_layout.parameter_count = 2;
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.initial_event_parameter_index = Some(1);
    model.parameters = vec![0.0, 1.0].into();
    model.problem.discrete = solve::DiscreteSolveSystem {
        rhs: fixture_block(
            vec![vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::Const {
                    dst: 1,
                    value: increment,
                },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::LoadP { dst: 3, index: 1 },
                solve::LinearOp::LoadTime { dst: 4 },
                solve::LinearOp::Const { dst: 5, value: 0.0 },
                solve::LinearOp::Compare {
                    dst: 6,
                    op: solve::CompareOp::Le,
                    lhs: 4,
                    rhs: 5,
                },
                solve::LinearOp::Binary {
                    dst: 7,
                    op: solve::BinaryOp::Or,
                    lhs: 3,
                    rhs: 6,
                },
                solve::LinearOp::Select {
                    dst: 8,
                    cond: 7,
                    if_true: 0,
                    if_false: 2,
                },
                solve::LinearOp::StoreOutput { src: 8 },
            ]],
            "dre.mo",
        ),
        update_targets: vec![solve::scalar_slot_p(0)],
        row_roles: vec![solve::DiscreteRowRole::Equation],
        pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
        observation_refresh: vec![false],
        integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
        clock_owners: vec![None],
        ..Default::default()
    };
    model
}
