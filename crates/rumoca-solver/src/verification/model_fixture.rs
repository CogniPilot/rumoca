//! Bounded `SolveModel` fixtures the verification harnesses drive.
//!
//! Every model here is small enough for a model checker to unwind: at most
//! three activation conditions, at most one continuous state. They are built
//! from Solve IR directly rather than compiled from Modelica so a harness can
//! vary one dimension — the number of conditions, a start value, a divergence
//! step — without a compiler in the loop.

use indexmap::IndexMap;
use rumoca_ir_solve as solve;

/// The largest condition count the bounded condition-memory harnesses explore.
///
/// The bound is a model-checking budget rather than a minimum: it has to stay
/// inside `kani::unwind(8)` while still admitting a model whose conditions are
/// not all true at the initialization instant, and whose seed walks several
/// threshold rows ahead of the `initial()` row instead of one special case.
pub(super) const MAX_CONDITIONS: usize = 3;

/// The constant every generated activation condition is compared against.
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
pub(super) struct ConditionLayout {
    count: usize,
}

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
                rhs: fixture_block(rows, "condition_memory_verification.mo"),
                update_targets: targets,
                row_roles: vec![solve::DiscreteRowRole::ConditionMemory; row_count],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent; row_count],
                observation_refresh: vec![false; row_count],
                clock_owners: vec![None; row_count],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters,
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
    solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string()],
                    name_to_idx: IndexMap::from([("x".to_string(), 0)]),
                    base_to_indices: IndexMap::from([("x".to_string(), vec![0])]),
                },
                state_scalar_count: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                derivative_rhs: solve::ComputeBlock::from_scalar_program_block(fixture_block(
                    derivative,
                    "single_state_verification.mo",
                )),
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![1.0],
        solver_nominals: vec![1.0],
        visible_names: vec!["x".to_string()],
        visible_value_rows: fixture_block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "single_state_verification.mo",
        ),
        ..Default::default()
    }
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
    model.parameters = vec![0.0];
    model.problem.initialization.update_rhs = fixture_block(
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
        "divergent_initialization_verification.mo",
    );
    model.problem.initialization.update_targets = vec![solve::scalar_slot_p(0)];
    model
}
