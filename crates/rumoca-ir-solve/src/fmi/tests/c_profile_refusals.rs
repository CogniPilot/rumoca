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

#[test]
fn c_profile_refuses_nonfinite_initialization_runs_without_dense_recovery() {
    for value in [
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::from_bits(0x7ff8_0000_0000_1234),
    ] {
        assert_refused("non-finite initialization", |model| {
            model.parameters.set(0, value).unwrap();
            assert!(!model.parameters.has_dense_view());
        });
    }
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
            model.initial_y = crate::SolveInitialValues::repeat(0.0, 1).unwrap();
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
        "the C profile executes only discrete equations and condition memories",
        |model| {
            static_partition(model);
            one_discrete_row(model, DiscreteRowRole::EventAction);
        },
    );
}

#[test]
fn sample_tick_pulse_buffers_are_refused() {
    assert_refused(
        "the C profile executes only discrete equations and condition memories",
        |model| {
            static_partition(model);
            one_discrete_row(model, DiscreteRowRole::PulseConditionMemory);
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

/// A pre() value the continuous kernel reads follows events, which the
/// parameter-determined partition never runs: the event iteration takes it.
#[test]
fn a_pre_value_read_by_the_continuous_kernel_runs_through_the_event_iteration() {
    let (mut model, input) = super::max_step_duration_local::delay_bearing_model_with_one_run();
    model.problem.events.delays = SolveDelayPartition::default();
    static_partition(&mut model);
    model.problem.solve_layout.pre_param_bindings = vec![PreParamBinding {
        dest_p_index: 0,
        source: PreParamSource::P { index: 1 },
        clock_schedule: None,
    }];
    model.problem.continuous.residual = ComputeBlock::from_scalar_program_block(rows(vec![vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]]));
    let refusal = super::static_assertions::validate(&model)
        .map(|_| ())
        .expect_err("the static partition refuses a pre() read");
    assert!(matches!(
        refusal,
        super::static_assertions::StaticRefusal::EventIteration(message)
            if message.contains("the continuous kernel reads a condition memory or a pre() value")
    ));
    FmiComponent::construct(model, vec![input])
        .expect("the fixture is a checked component")
        .into_codegen_view()
        .try_c()
        .expect("the event iteration executes the pre() read");
}

#[test]
fn a_projection_plan_without_residual_rows_is_refused() {
    assert_refused(
        "C initialization projects blocks without residual rows",
        |model| {
            let mut initialization = model.problem.initialization.clone().into_input();
            initialization.projection_plan = crate::InitializationProjectionPlan {
                iterates_discretes: false,
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
        assertion_projection: None,
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
/// parameter settable; a parameter no program reads keeps its parameter
/// variability too (MLS §4.5), since whether a program reads it does not
/// change what it is.
#[test]
fn a_parameter_read_only_by_an_assertion_condition_stays_settable() {
    assert_eq!(published_causality(assertion_on_parameter), "parameter");
    assert_eq!(
        published_causality(|model| {
            assertion_on_parameter(model);
            model.problem.events.action_conditions = rows(vec![constant_row(0.0)]);
        }),
        "parameter"
    );
}

/// A continuous residual row reading the parameter `p` (storage index 1) only
/// inside a nested sub-program.
fn nested_read(model: &mut SolveModel, row: Vec<LinearOp>) {
    model.problem.continuous.residual = ComputeBlock::from_scalar_program_block(rows(vec![row]));
}

fn load_parameter_row() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ]
}

/// SPEC_0044 ME-PARAM-001: reads inside fold updates and conditional arms are
/// reads; the parameter stays settable.
#[test]
fn a_parameter_read_only_inside_a_nested_program_stays_settable() {
    let fold = crate::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        },
        1,
        0,
        load_parameter_row(),
    )
    .expect("a one-carried fold is checked");
    assert_eq!(
        published_causality(|model| nested_read(
            model,
            vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::FunctionFold {
                    dst_start: 1,
                    initial_start: 0,
                    capture_start: 0,
                    program: std::sync::Arc::new(fold),
                },
                LinearOp::StoreOutput { src: 1 },
            ],
        )),
        "parameter",
        "a fold body read keeps the parameter settable"
    );
    let conditional = crate::FunctionConditionalProgram::checked(
        0,
        vec![1],
        [(constant_row(1.0), load_parameter_row())],
        constant_row(0.0),
    )
    .expect("a one-arm conditional is checked");
    assert_eq!(
        published_causality(|model| nested_read(
            model,
            vec![
                LinearOp::FunctionConditional {
                    dst_start: 0,
                    capture_start: 0,
                    program: std::sync::Arc::new(conditional),
                },
                LinearOp::StoreOutput { src: 0 },
            ],
        )),
        "parameter",
        "a conditional arm read keeps the parameter settable"
    );
}

#[test]
fn a_projection_holding_discretes_is_refused() {
    assert_refused(
        "C initialization cannot alternate the projection with held discretes",
        |model| {
            let mut initialization = model.problem.initialization.clone().into_input();
            initialization.projection_plan.iterates_discretes = true;
            model.problem.initialization =
                crate::InitializationSolveSystem::construct(initialization)
                    .expect("the flag does not change the checked shape");
        },
    );
}

/// A model whose only event is the tick of one periodic clock.
fn ticking_model(schedule: PeriodicEventSchedule) -> (SolveModel, crate::fmi::FmiVariableInput) {
    let (mut model, input) = super::max_step_duration_local::delay_bearing_model_with_one_run();
    model.problem.events.delays = SolveDelayPartition::default();
    model.problem.clocks.periodic_event_schedules = vec![schedule];
    model.problem.clocks.activation_parameter_indices = vec![0];
    (model, input)
}

/// SPEC_0044 ME-EVENT-002: a periodic clock is a time event the component
/// announces, carried as the exact ratio of its ticks.
#[test]
fn a_periodic_clock_is_a_time_event_with_an_exact_tick_ratio() {
    let (model, input) = ticking_model(PeriodicEventSchedule::from_seconds(0.1, 0.0).unwrap());
    let view = FmiComponent::construct(model, vec![input])
        .expect("the fixture is a checked component")
        .into_codegen_view()
        .try_c()
        .expect("a clock tick is a time event");
    let events = &serde_json::to_value(&view).unwrap()["scalar_events"]["time_events"];
    let clock = &events["clocks"][0];
    assert_eq!(clock["denominator"], 10.0);
    assert_eq!(clock["period_numerator"], 1.0);
    assert_eq!(clock["phase_numerator"], 0.0);
    assert_eq!(clock["activation"], 0);
    assert_eq!(
        events["match_tolerance"],
        rumoca_core::SCHEDULE_TIME_RELATIVE_TOLERANCE
    );
}

/// A phase anchored at the simulation start needs the experiment's start time,
/// which the packaged component learns only when it is set up.
#[test]
fn a_clock_anchored_at_the_simulation_start_is_refused() {
    let lattice = rumoca_core::ClockLattice::from_seconds(0.1, 0.0).unwrap();
    let schedule = PeriodicEventSchedule::from_schedule(
        rumoca_core::PeriodicClockSchedule::simulation_start_relative(lattice).unwrap(),
    )
    .unwrap();
    let (model, input) = ticking_model(schedule);
    let refused = FmiComponent::construct(model, vec![input])
        .expect("the fixture is a checked component")
        .into_codegen_view()
        .try_c()
        .expect_err("the anchor is unresolved")
        .to_string();
    assert!(
        refused.contains("anchored at the simulation start"),
        "{refused}"
    );
}

fn mode_declaration() -> rumoca_core::EnumerationDeclaration {
    rumoca_core::EnumerationDeclaration {
        name: "Flight.Mode".to_string(),
        literals: vec!["Off".to_string(), "Climb".to_string()],
    }
}

/// SPEC_0044 ME-EVENT-002: only an enumeration variable carries an
/// enumeration declaration, so a Real that names one is not a checked entry.
#[test]
fn an_enumeration_declaration_belongs_to_enumeration_variables_only() {
    let (mut model, mut input) = super::max_step_duration_local::delay_bearing_model_with_one_run();
    model.problem.events.delays = SolveDelayPartition::default();
    input.enumeration = Some(mode_declaration());
    let error = FmiComponent::construct(model, vec![input])
        .expect_err("a Real variable has no enumeration type");
    assert!(
        matches!(
            error,
            crate::fmi::FmiComponentError::StorageTypeMismatch { .. }
        ),
        "{error:?}"
    );
}

/// An enumeration variable publishes its type definition once, however many
/// variables are declared with it.
#[test]
fn an_enumeration_variable_publishes_its_type_definition() {
    let (mut model, mut input) = super::max_step_duration_local::delay_bearing_model_with_one_run();
    model.problem.events.delays = SolveDelayPartition::default();
    let kind = crate::SolveVariableValueKind::Enumeration;
    model.problem.solve_layout.variable_storage_runs[0].value_kind = kind;
    model.problem.solve_layout.variable_declarations = vec![crate::SolveVariableDeclaration::new(
        crate::SolveVariableStorageRole::Parameter,
        kind,
    )];
    input.value_kind = kind;
    input.start = vec![2.0];
    input.enumeration = Some(mode_declaration());
    let view = FmiComponent::construct(model, vec![input])
        .expect("an enumeration variable with its declaration is checked")
        .into_codegen_view()
        .try_c()
        .expect("an enumeration parameter is exported");
    let json = serde_json::to_value(&view).unwrap();
    assert_eq!(
        json["enumerations"],
        serde_json::json!([{"name": "Flight.Mode", "literals": ["Off", "Climb"]}])
    );
    assert_eq!(json["variables"][0]["enumeration"]["name"], "Flight.Mode");
}
