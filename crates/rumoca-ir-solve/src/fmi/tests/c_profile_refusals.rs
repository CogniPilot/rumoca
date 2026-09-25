//! Every C-profile refusal of an initialization or event partition the shared
//! kernel cannot execute names its exact reason (SPEC_0044 ME-PARAM-001 and
//! ME-EVENT-003).

use super::*;
use crate::{
    ComputeBlock, DiscreteRowRole, InitializationRowRole, LinearOp, PeriodicEventSchedule,
    PreParamBinding, PreParamSource, ScalarProgramBlock, ScalarSlot, SolveDelayPartition,
};

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_c_profile_refusals.mo"),
        0,
        1,
    )
}

fn rows(programs: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        programs,
        span()
            .require_provenance("C profile refusal fixture")
            .unwrap(),
    )
    .unwrap()
}

fn constant_row(value: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value },
        LinearOp::StoreOutput { src: 0 },
    ]
}

/// The refusal of the one-parameter kernel after `mutate`.
fn refusal(mutate: impl FnOnce(&mut SolveModel)) -> String {
    let (mut model, input) = super::max_step_duration_local::delay_bearing_model_with_one_run();
    model.problem.events.delays = SolveDelayPartition::default();
    mutate(&mut model);
    FmiComponent::construct(model, vec![input])
        .expect("the fixture is a checked component")
        .into_codegen_view()
        .try_c()
        .expect_err("the C profile refuses the partition")
        .to_string()
}

fn assert_refused(message: &str, mutate: impl FnOnce(&mut SolveModel)) {
    let refused = refusal(mutate);
    assert!(
        refused.contains(message),
        "expected `{message}`, got `{refused}`"
    );
}

/// One initialization residual row with the given role.
fn one_initial_row(model: &mut SolveModel, role: InitializationRowRole) {
    let mut initialization = model.problem.initialization.clone().into_input();
    initialization.residual =
        ComputeBlock::from_scalar_program_block(rows(vec![constant_row(0.0)]));
    initialization.row_roles = vec![role];
    model.problem.initialization = crate::InitializationSolveSystem::construct(initialization)
        .expect("one surplus row is a checked initialization");
}

const SETTLED: InitializationRowRole = InitializationRowRole::SurplusAlgebraicCheck;

#[test]
fn initialization_rows_over_declaration_seeds_are_refused() {
    assert_refused(
        "C initialization evaluates residual rows only on the settled algebraic refresh",
        |model| one_initial_row(model, InitializationRowRole::SurplusCheck),
    );
}

#[test]
fn homotopy_initialization_is_refused() {
    assert_refused(
        "C initialization cannot run the homotopy continuation",
        |model| {
            one_initial_row(model, SETTLED);
            model.problem.solve_layout.initial_homotopy_parameter_index = Some(0);
        },
    );
}

#[test]
fn delay_histories_in_initialization_are_refused() {
    assert_refused(
        "C initialization cannot synchronize delay histories",
        |model| {
            one_initial_row(model, SETTLED);
            let (delayed, _) = super::max_step_duration_local::delay_bearing_model_with_one_run();
            model.problem.events.delays = delayed.problem.events.delays;
        },
    );
}

#[test]
fn solver_coordinates_that_do_not_fill_the_y_storage_are_refused() {
    assert_refused(
        "C initialization needs the solver coordinates to fill the Y storage",
        |model| {
            one_initial_row(model, SETTLED);
            model.problem.layout = crate::VarLayout::from_parts(Default::default(), 1, 2);
        },
    );
}

/// A static event partition: one condition memory in parameter storage, so
/// the component has a discrete class without continuous indicators.
fn static_partition(model: &mut SolveModel) {
    model.problem.events.condition_memory_parameter_indices = vec![0];
}

/// One discrete row writing `p[0]` with `role`.
fn one_discrete_row(model: &mut SolveModel, role: DiscreteRowRole) {
    let discrete = &mut model.problem.discrete;
    discrete.rhs = rows(vec![constant_row(1.0)]);
    discrete.update_targets = vec![ScalarSlot::P {
        index: 0,
        byte_offset: 0,
    }];
    discrete.row_roles = vec![role];
    discrete.pre_modes = vec![crate::DiscreteEventPreMode::default()];
    discrete.observation_refresh = vec![false];
    discrete.integrator_history_effects = vec![crate::IntegratorHistoryEffect::default()];
    discrete.clock_owners = vec![None];
}

#[test]
fn event_edge_actions_are_refused() {
    assert_refused(
        "the C profile cannot execute event-edge discrete actions",
        |model| {
            static_partition(model);
            one_discrete_row(model, DiscreteRowRole::EventAction);
        },
    );
}

#[test]
fn clocked_previous_history_is_refused() {
    assert_refused(
        "the C profile cannot execute clocked previous() history",
        |model| {
            static_partition(model);
            model.problem.solve_layout.pre_param_bindings = vec![PreParamBinding {
                dest_p_index: 0,
                source: PreParamSource::P { index: 1 },
                clock_schedule: Some(PeriodicEventSchedule::from_seconds(0.1, 0.0).unwrap()),
            }];
        },
    );
}

#[test]
fn a_pre_value_read_by_the_continuous_kernel_is_refused() {
    assert_refused(
        "the continuous kernel reads a condition memory or a pre() value",
        |model| {
            static_partition(model);
            model.problem.solve_layout.pre_param_bindings = vec![PreParamBinding {
                dest_p_index: 0,
                source: PreParamSource::P { index: 1 },
                clock_schedule: None,
            }];
            model.problem.continuous.residual =
                ComputeBlock::from_scalar_program_block(rows(vec![vec![
                    LinearOp::LoadP { dst: 0, index: 0 },
                    LinearOp::StoreOutput { src: 0 },
                ]]));
        },
    );
}

#[test]
fn a_projection_plan_without_residual_rows_is_refused() {
    assert_refused(
        "C initialization projects blocks without residual rows",
        |model| {
            let mut initialization = model.problem.initialization.clone().into_input();
            initialization.projection_plan = crate::InitializationProjectionPlan {
                blocks: vec![crate::InitializationProjectionBlock::default()],
            };
            model.problem.initialization =
                crate::InitializationSolveSystem::construct(initialization)
                    .expect("an empty block over an empty residual is a checked shape");
        },
    );
}

/// The published causality of the one parameter after `mutate`.
fn published_causality(mutate: impl FnOnce(&mut SolveModel)) -> serde_json::Value {
    let (mut model, input) = super::max_step_duration_local::delay_bearing_model_with_one_run();
    model.problem.events.delays = SolveDelayPartition::default();
    mutate(&mut model);
    let view = FmiComponent::construct(model, vec![input])
        .expect("the fixture is a checked component")
        .into_codegen_view()
        .try_c()
        .expect("a parameter-determined assertion is admitted");
    serde_json::to_value(&view).unwrap()["variables"][0]["causality"].clone()
}

/// An error assertion whose condition reads the parameter `p` (storage index
/// 1) and nothing else reads it.
fn assertion_on_parameter(model: &mut SolveModel) {
    let events = &mut model.problem.events;
    events.actions = vec![crate::SolveEventAction {
        kind: crate::SolveEventActionKind::Assert,
        message: crate::SolveEventMessage {
            parts: vec![crate::SolveEventMessagePart::Text(
                "p must be positive".into(),
            )],
        },
        span: span(),
        origin: "assertion fixture".into(),
        clock_owner: None,
    }];
    events.action_conditions = rows(vec![vec![
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Compare {
            dst: 2,
            op: crate::CompareOp::Le,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]]);
}

/// SPEC_0044 ME-PARAM-001: an assertion condition is a read that keeps a
/// parameter settable, even when no other program reads it; the same
/// assertion over a constant leaves the parameter folded.
#[test]
fn a_parameter_read_only_by_an_assertion_condition_stays_settable() {
    assert_eq!(published_causality(assertion_on_parameter), "parameter");
    assert_eq!(
        published_causality(|model| {
            assertion_on_parameter(model);
            model.problem.events.action_conditions = rows(vec![constant_row(0.0)]);
        }),
        "local"
    );
}
