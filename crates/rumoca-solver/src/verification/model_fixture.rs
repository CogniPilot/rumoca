//! Bounded `SolveModel` fixtures the verification harnesses drive.
//!
//! Every model here is deliberately small: at most three activation conditions
//! or one continuous state. They are built from Solve IR directly rather than
//! compiled from Modelica so a test can vary one dimension without putting the
//! compiler itself inside a runtime-contract check.

use indexmap::IndexMap;
use rumoca_ir_solve as solve;

#[cfg(test)]
use crate::test_support::empty_binary64_first_product_model;

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
    let solve_layout = solve::SolveLayout {
        parameter_count: layout.count(),
        static_parameter_names: (0..layout.count())
            .map(|index| format!("s_{index}"))
            .collect(),
        compiled_parameter_len: layout.parameter_count(),
        initial_event_parameter_index: Some(layout.initial_flag()),
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem {
        rhs: fixture_block(rows, "cm.mo"),
        update_targets: targets,
        row_roles: vec![solve::DiscreteRowRole::ConditionMemory; row_count],
        pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent; row_count],
        observation_refresh: vec![false; row_count],
        integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve; row_count],
        clock_owners: vec![None; row_count],
        ..Default::default()
    };
    let events = solve::SolveEventPartition {
        condition_memory_parameter_indices: (0..layout.count())
            .map(|index| layout.buffer(index))
            .chain(std::iter::once(layout.initial_buffer()))
            .collect(),
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture::empty();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(IndexMap::new(), 0, layout.parameter_count()),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("condition-memory fixture satisfies the checked root contract"),
        parameters: parameters,
        ..empty_binary64_first_product_model()
    }
}

/// `dx/dt = x`, one continuous state starting at 1.
///
/// The smallest state-bearing model used by the lifecycle harness.
pub(super) fn single_state_model() -> solve::SolveModel {
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("ss_variables.mo"),
        1,
        2,
    );
    let derivative = vec![vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]];
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string()],
            name_to_idx: IndexMap::from([("x".to_string(), 0)]),
            base_to_indices: IndexMap::from([("x".to_string(), vec![0])]),
        },
        state_scalar_count: 1,
        variable_storage_runs: vec![solve::SolveVariableStorageRun {
            base: solve::SolveStorageCoordinate::Y(0),
            scalar_count: 1,
            role: solve::SolveVariableStorageRole::State,
            value_kind: solve::SolveVariableValueKind::Real,
        }],
        variable_declarations: vec![solve::SolveVariableDeclaration::new(
            solve::SolveVariableStorageRole::State,
            solve::SolveVariableValueKind::Real,
        )],
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(fixture_block(
            derivative, "ss.mo",
        )),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(IndexMap::new(), 1, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("single-state fixture satisfies the checked root contract"),
        initial_y: vec![1.0],
        solver_nominals: vec![1.0],
        visible_value_rows: fixture_block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "ss.mo",
        ),
        variable_entries: crate::test_support::explicit_real_scalar_catalog_entries(vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "x", 0, 1.0, 1.0, true, provenance,
            ),
        ]),
        ..empty_binary64_first_product_model()
    }
}

/// [`single_state_model`] plus one writable scalar input parameter.
///
/// The ME buffer and instance-brand proofs need the smallest model that can
/// issue a real value reference and observe an accepted parameter value.
pub(super) fn single_state_input_model() -> solve::SolveModel {
    let base = single_state_model();
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("ssi_variables.mo"),
        1,
        2,
    );
    let mut solve_layout = solve::SolveLayout {
        compiled_parameter_len: 1,
        input_scalar_names: vec!["u".to_string()],
        ..base.problem().solve_layout().clone()
    };
    solve_layout
        .variable_storage_runs
        .push(solve::SolveVariableStorageRun {
            base: solve::SolveStorageCoordinate::P(0),
            scalar_count: 1,
            role: solve::SolveVariableStorageRole::ExternalInput,
            value_kind: solve::SolveVariableValueKind::Real,
        });
    solve_layout
        .variable_declarations
        .push(solve::SolveVariableDeclaration::new(
            solve::SolveVariableStorageRole::ExternalInput,
            solve::SolveVariableValueKind::Real,
        ));
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous());
    let discrete = base.problem().discrete().clone();
    let events = base.problem().events().clone();
    let clocks = base.problem().clocks().clone();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        solve::VarLayout::from_parts(IndexMap::new(), 1, 1),
        solve_layout,
        continuous,
        base.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("single-state input fixture satisfies the checked root contract");
    crate::test_support::checked_solve_model! {
        problem: problem,
        parameters: vec![1.0],
        visible_value_rows: fixture_block(
            vec![
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    solve::LinearOp::LoadP { dst: 0, index: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            ],
            "ssi_visible.mo",
        ),
        variable_entries: crate::test_support::explicit_real_scalar_catalog_entries(vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "x", 0, 1.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::external_input(
                2, "u", 0, 1.0, provenance,
            ),
        ]),
        ..base
    }
}

/// [`single_state_model`] plus one static component-owned time event.
pub(super) fn single_state_time_event_model() -> solve::SolveModel {
    let base = single_state_model();
    let solve_layout = base.problem().solve_layout().clone();
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous());
    let discrete = base.problem().discrete().clone();
    let events = solve::SolveEventPartition {
        scheduled_time_events: vec![0.5],
        ..base.problem().events().clone()
    };
    let clocks = base.problem().clocks().clone();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        base.problem().layout().clone(),
        solve_layout,
        continuous,
        base.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("single-state time-event fixture satisfies the checked root contract");
    crate::test_support::checked_solve_model! { problem: problem, ..base }
}

/// [`single_state_model`] plus one scalar event-indicator surface `x`.
pub(super) fn single_state_indicator_model() -> solve::SolveModel {
    let base = single_state_model();
    let solve_layout = base.problem().solve_layout().clone();
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous());
    let discrete = base.problem().discrete().clone();
    let events = solve::SolveEventPartition {
        root_conditions: fixture_block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "si.mo",
        ),
        root_relation_memory_targets: vec![None],
        root_zero_domains: vec![solve::RootZeroDomain::Previous],
        root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::Frozen],
        ..base.problem().events().clone()
    };
    let clocks = base.problem().clocks().clone();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        base.problem().layout().clone(),
        solve_layout,
        continuous,
        base.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("single-state indicator fixture satisfies the checked root contract");
    crate::test_support::checked_solve_model! { problem: problem, ..base }
}

/// [`single_state_model`] plus one initialization update row that can never
/// settle: `q := q + increment` rewrites its own source every pass.
///
/// MLS §8.6 initialization is a fixed point, so a runtime that iterates it must
/// bound the iteration. This model is what forces that bound to be observed.
pub(super) fn divergent_initialization_model(increment: f64) -> solve::SolveModel {
    let base = single_state_model();
    let solve_layout = solve::SolveLayout {
        compiled_parameter_len: 1,
        ..base.problem().solve_layout().clone()
    };
    let initialization = solve::InitializationSolveSystem::construct(
        solve::ComputeBlock::default(),
        Vec::new(),
        Vec::new(),
        0,
        Vec::new(),
        solve::InitializationProjectionPlan::default(),
        (
            fixture_block(
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
            ),
            vec![solve::scalar_slot_p(0)],
        ),
    )
    .expect("the divergent-update fixture initialization system is exactly correlated");
    let discrete = base.problem().discrete().clone();
    let events = base.problem().events().clone();
    let clocks = base.problem().clocks().clone();
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous());
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        solve::VarLayout::from_parts(IndexMap::new(), 1, 1),
        solve_layout,
        continuous,
        initialization,
        discrete,
        events,
        clocks,
    )
    .expect("divergent initialization fixture satisfies the checked root contract");
    crate::test_support::checked_solve_model! {
        problem: problem,
        parameters: vec![0.0],
        ..base
    }
}

/// A one-state model whose discrete equation settles during `initial()` but
/// diverges at an ordinary runtime event: `q := if initial() then q else
/// q + increment`.
fn divergent_runtime_solve_layout(base: &solve::SolveModel) -> solve::SolveLayout {
    solve::SolveLayout {
        variable_storage_runs: vec![
            solve::SolveVariableStorageRun {
                base: solve::SolveStorageCoordinate::Y(0),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::State,
                value_kind: solve::SolveVariableValueKind::Real,
            },
            solve::SolveVariableStorageRun {
                base: solve::SolveStorageCoordinate::P(0),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::DiscreteReal,
                value_kind: solve::SolveVariableValueKind::Real,
            },
        ],
        variable_declarations: vec![
            solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::State,
                solve::SolveVariableValueKind::Real,
            ),
            solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::DiscreteReal,
                solve::SolveVariableValueKind::Real,
            ),
        ],
        compiled_parameter_len: 3,
        discrete_real_scalar_names: vec!["q".to_string()],
        initial_event_parameter_index: Some(1),
        pre_param_bindings: vec![solve::PreParamBinding {
            dest_p_index: 2,
            source: solve::PreParamSource::P { index: 0 },
            clock_schedule: None,
        }],
        ..base.problem().solve_layout().clone()
    }
}

pub(super) fn divergent_runtime_event_model(increment: f64) -> solve::SolveModel {
    let base = single_state_model();
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("dre_variables.mo"),
        1,
        2,
    );
    let solve_layout = divergent_runtime_solve_layout(&base);
    let discrete = solve::DiscreteSolveSystem {
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
                solve::LinearOp::Select {
                    dst: 4,
                    cond: 3,
                    if_true: 0,
                    if_false: 2,
                },
                solve::LinearOp::StoreOutput { src: 4 },
            ]],
            "dre.mo",
        ),
        update_targets: vec![solve::scalar_slot_p(0)],
        row_roles: vec![solve::DiscreteRowRole::Equation],
        pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
        observation_refresh: vec![false],
        integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
        clock_owners: vec![None],
        event_iteration_plan: solve::EventIterationPlan {
            runs: vec![solve::EventIterationRun {
                variable: 1,
                pre_binding_start: 0,
                owner: solve::EventIterationOwner::ScalarRows { start_row: 0 },
            }],
        },
        ..Default::default()
    };
    let events = base.problem().events().clone();
    let clocks = base.problem().clocks().clone();
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous());
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        solve::VarLayout::from_parts(IndexMap::new(), 1, 3),
        solve_layout,
        continuous,
        base.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("divergent runtime-event fixture satisfies the checked root contract");
    crate::test_support::checked_solve_model! {
        problem: problem,
        parameters: vec![0.0, 1.0, 0.0],
        visible_value_rows: fixture_block(
            vec![
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    solve::LinearOp::LoadP { dst: 0, index: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            ],
            "dre_visible.mo",
        ),
        variable_entries: crate::test_support::explicit_real_scalar_catalog_entries(vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "x", 0, 1.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::discrete_real(
                2, "q", 0, 0.0, provenance,
            ),
        ]),
        ..base
    }
}
