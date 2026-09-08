use rumoca_ir_solve::fmi::{FmiLinkedRuntimeFacts, FmiRuntimeFloat64Backing};

use super::*;

impl MeKernelBody {
    pub(super) fn instantiate_body(
        source: MeModelSource,
        config: &MeInstanceConfig,
        execution: crate::fmi_me::MeExecutionSelection,
    ) -> Result<(MeLifecycle, Self), MeError> {
        let prepared = prepare_me_instantiation(source, config, execution)?;
        Ok(Self::from_prepared(config, prepared))
    }

    fn from_prepared(
        config: &MeInstanceConfig,
        prepared: PreparedMeInstantiation,
    ) -> (MeLifecycle, Self) {
        let PreparedMeInstantiation {
            runtime,
            value_references,
            input_names,
            directional_state_references,
            directional_derivative_references,
            lifecycle,
            states,
            params,
            state_domain,
            stop_schedule,
            output_meta,
            indicator_storage,
            derivative_output_scratch,
            directional_seed_scratch,
            directional_sensitivity_scratch,
            directional_serialized_scratch,
            accepted_derivative_scratch,
            max_step_duration_value_reference,
            initial_y,
        } = prepared;
        let (root_crossing_capacity, scheduled_root_capacity) = runtime_capacities(&runtime);
        let state_count = state_domain.len();
        let caches = construction_caches(config, state_count, params.len());
        let body = Self {
            solver_y_guess: RefCell::new(initial_y.clone()),
            indicator_storage,
            derivative_output_scratch: RefCell::new(derivative_output_scratch),
            directional_seed_scratch: RefCell::new(directional_seed_scratch),
            directional_sensitivity_scratch: RefCell::new(directional_sensitivity_scratch),
            directional_serialized_scratch: RefCell::new(directional_serialized_scratch),
            accepted_derivative_scratch: RefCell::new(accepted_derivative_scratch),
            delay_params_scratch: RefCell::new(params.clone()),
            delay_solver_y_scratch: RefCell::new(initial_y.clone()),
            runtime: Rc::clone(&runtime),
            instance_brand: Rc::new(()),
            value_references,
            input_names,
            directional_state_references,
            directional_derivative_references,
            instance_name: config.instance_name,
            tolerance: config.tolerance,
            stop_time: config.stop_time,
            time: config.start_time,
            set_time_bounds: MeSetTimeBounds::at_start(config.start_time),
            post_event_eval_time: None,
            event_anchor_time: config.start_time,
            states,
            state_domain,
            stop_schedule,
            pending_event_entry: None,
            pending_state_event_entry: false,
            pending_event_stop: None,
            advance_state_to_event_right_limit: false,
            state_time_coincidence: StateTimeCoincidence::None,
            initial_event_pending: false,
            pending_root_crossings: Vec::with_capacity(root_crossing_capacity),
            pending_event_pre_y: EventVectorLatch::reserved(initial_y.len()),
            pending_event_pre_p: EventVectorLatch::reserved(params.len()),
            boundary_event_pre_y: EventVectorLatch::reserved(initial_y.len()),
            boundary_event_pre_p: EventVectorLatch::reserved(params.len()),
            event_solver_y_work: initial_y.clone(),
            event_state_before: vec![0.0; state_count],
            scheduled_root_index_scratch: Vec::with_capacity(scheduled_root_capacity),
            root_override_scratch: Vec::with_capacity(root_crossing_capacity),
            derivative_cache: caches.derivative,
            continuous_linearization_cache: caches.continuous_linearization,
            params,
            max_step_duration: None,
            max_step_duration_value_reference,
            termination: None,
            output_meta,
            settled_initialization_y: EventVectorLatch::reserved(initial_y.len()),
            #[cfg(test)]
            verification_fail_next_enter_initialization: false,
            #[cfg(test)]
            verification_fail_next_exit_initialization: false,
            #[cfg(test)]
            verification_fail_next_update_discrete_states: false,
            #[cfg(test)]
            verification_fail_next_completed_integrator_step: false,
            #[cfg(test)]
            verification_fail_next_enter_continuous_time_mode: false,
        };
        (lifecycle, body)
    }
}

fn runtime_capacities(runtime: &SolveRuntime) -> (usize, usize) {
    let root_crossings = runtime.root_condition_count();
    let scheduled_roots = runtime
        .model()
        .problem()
        .events()
        .scheduled_root_conditions
        .len();
    (root_crossings, scheduled_roots)
}

fn construction_caches(
    config: &MeInstanceConfig,
    state_count: usize,
    parameter_count: usize,
) -> ConstructionCaches {
    let start_time = config.start_time;
    ConstructionCaches {
        derivative: RefCell::new(CachedDerivative {
            valid: false,
            time: start_time,
            state: vec![0.0; state_count],
            derivative: vec![0.0; state_count],
        }),
        continuous_linearization: RefCell::new(CachedContinuousLinearization {
            valid: false,
            time: start_time,
            state: vec![0.0; state_count],
            parameters: vec![0.0; parameter_count],
        }),
    }
}

struct ConstructionCaches {
    derivative: RefCell<CachedDerivative>,
    continuous_linearization: RefCell<CachedContinuousLinearization>,
}

pub(super) struct PreparedMeInstantiation {
    pub(super) runtime: Rc<SolveRuntime>,
    pub(super) value_references: Vec<MeNamedFloat64Reference>,
    pub(super) input_names: Vec<String>,
    pub(super) directional_state_references: Vec<MeDirectionalReferenceDescriptor>,
    pub(super) directional_derivative_references: Vec<MeDirectionalReferenceDescriptor>,
    pub(super) lifecycle: MeLifecycle,
    pub(super) states: Vec<f64>,
    pub(super) params: Vec<f64>,
    pub(super) state_domain: MeContinuousStateDomain,
    pub(super) stop_schedule: SolveStopSchedule,
    pub(super) output_meta: Vec<SimVariableMeta>,
    pub(super) indicator_storage: EventIndicatorStorage,
    pub(super) derivative_output_scratch: Vec<f64>,
    pub(super) directional_seed_scratch: Vec<f64>,
    pub(super) directional_sensitivity_scratch: Vec<f64>,
    pub(super) directional_serialized_scratch: Vec<f64>,
    pub(super) accepted_derivative_scratch: Vec<f64>,
    pub(super) max_step_duration_value_reference: Option<u32>,
    pub(super) initial_y: Vec<f64>,
}

pub(super) fn prepare_me_instantiation(
    source: MeModelSource,
    config: &MeInstanceConfig,
    execution: crate::fmi_me::MeExecutionSelection,
) -> Result<PreparedMeInstantiation, MeError> {
    let (runtime_view, configuration) = source
        .into_parts()
        .map_err(|error| contract(error.to_string()))?;
    let value_references = prepared_float64_value_references(runtime_view.linked_runtime_facts());
    let input_names = value_references
        .iter()
        .filter(|reference| reference.access.causality == rumoca_ir_solve::fmi::FmiCausality::Input)
        .map(|reference| reference.name.clone())
        .collect();
    let (directional_state_references, directional_derivative_references) =
        prepared_directional_references(runtime_view.linked_runtime_facts())?;
    let max_step_duration_value_reference = runtime_view
        .linked_runtime_facts()
        .delay()
        .value_reference();
    rumoca_eval_solve::reset_solve_row_eval_trace();
    let runtime = Rc::new(match execution {
        crate::fmi_me::MeExecutionSelection::Native(backend) => {
            let backend = backend.into_runtime_backend();
            SolveRuntime::new_fmi(runtime_view, Some(backend.as_ref()))?
        }
        crate::fmi_me::MeExecutionSelection::Interpreter => {
            SolveRuntime::new_fmi(runtime_view, None)?
        }
    });
    let model = runtime.model();
    let linked_runtime_facts = runtime.fmi_linked_runtime_facts();
    let state_domain =
        MeContinuousStateDomain::from_linked(linked_runtime_facts.continuous_state_width());
    let state_count = state_domain.len();
    let initial_y = model.initial_y().to_vec();
    let states = initial_y[..state_count].to_vec();
    let params = model.parameters().to_vec();
    let stop_schedule =
        SolveStopSchedule::new(model.problem(), config.start_time, config.stop_time);
    let output_meta = convert_variable_meta(&model.variable_meta());
    let indicator_plan = linked_runtime_facts.indicator_plan();
    let indicator_storage =
        EventIndicatorStorage::try_construct(indicator_plan, &states, config.start_time)?;
    // Construction reserves the caller-publication buffers used by
    // `fmi3GetContinuousStateDerivatives` and `fmi3GetDirectionalDerivative`.
    // Evaluation completes into these buffers before publication; evaluator,
    // JVP, and delay workspaces retain their separate runtime behavior. The
    // serialization buffer is reserved at the issued directional table's full
    // serialized width: every admitted batch uses each issued value reference
    // at most once, so no batch serializes wider.
    let derivative_output_scratch =
        reserved_indicator_values(state_count, "state-derivative getter output")?;
    let directional_seed_scratch =
        reserved_indicator_values(state_count, "directional-derivative state seed")?;
    let directional_sensitivity_scratch =
        reserved_indicator_values(state_count, "directional-derivative state sensitivities")?;
    let directional_serialized_scratch = reserved_indicator_values(
        directional_serialized_maximum(&directional_derivative_references)?,
        "serialized directional sensitivities",
    )?;
    let accepted_derivative_scratch =
        reserved_indicator_values(state_domain.len(), "accepted derivatives")?;
    Ok(PreparedMeInstantiation {
        runtime,
        value_references,
        input_names,
        directional_state_references,
        directional_derivative_references,
        lifecycle: MeLifecycle::instantiated(configuration),
        states,
        params,
        state_domain,
        stop_schedule,
        output_meta,
        indicator_storage,
        derivative_output_scratch,
        directional_seed_scratch,
        directional_sensitivity_scratch,
        directional_serialized_scratch,
        accepted_derivative_scratch,
        max_step_duration_value_reference,
        initial_y,
    })
}

fn prepared_float64_value_references(
    facts: &FmiLinkedRuntimeFacts,
) -> Vec<MeNamedFloat64Reference> {
    facts
        .float64_descriptors()
        .iter()
        .map(|variable| MeNamedFloat64Reference {
            name: variable.name().to_owned(),
            value_reference: variable.value_reference(),
            backing: match variable.backing() {
                FmiRuntimeFloat64Backing::SolverVariable { base, width } => {
                    MeFloat64Backing::SolverVariable { base, width }
                }
                FmiRuntimeFloat64Backing::Parameter { base, width } => {
                    MeFloat64Backing::Parameter { base, width }
                }
                FmiRuntimeFloat64Backing::MaximumStepDuration => MeFloat64Backing::MaxStepDuration,
            },
            access: MeFloat64AccessEvidence {
                causality: variable.causality(),
                write_modes: variable.write_modes(),
            },
        })
        .collect()
}

fn prepared_directional_references(
    linked_runtime_facts: &FmiLinkedRuntimeFacts,
) -> Result<
    (
        Vec<MeDirectionalReferenceDescriptor>,
        Vec<MeDirectionalReferenceDescriptor>,
    ),
    MeError,
> {
    let mut state = Vec::new();
    let mut derivatives = Vec::new();
    let descriptors = linked_runtime_facts.directional_references();
    state
        .try_reserve_exact(descriptors.len())
        .map_err(|error| contract(format!("failed to reserve state value references: {error}")))?;
    derivatives
        .try_reserve_exact(descriptors.len())
        .map_err(|error| contract(format!("failed to reserve derivative references: {error}")))?;
    for descriptor in descriptors.iter().copied() {
        state.push(MeDirectionalReferenceDescriptor {
            value_reference: descriptor.state_value_reference(),
            backing: MeDirectionalBacking::ContinuousState {
                base: descriptor.storage_base(),
                width: descriptor.serialized_width(),
            },
        });
        derivatives.push(MeDirectionalReferenceDescriptor {
            value_reference: descriptor.derivative_value_reference(),
            backing: MeDirectionalBacking::ContinuousStateDerivative {
                base: descriptor.storage_base(),
                width: descriptor.serialized_width(),
            },
        });
    }
    Ok((state, derivatives))
}

/// The serialized width of the widest admissible directional-derivative
/// batch: every issued descriptor consumed once.
///
/// `admit_directional_references` admits a batch only against this issued
/// table and rejects a repeated value reference, so no admitted batch
/// serializes wider than this sum. The bound is the table's own serialized
/// width, not `state_count`: disjointness of the descriptors' state segments
/// is a property of the linked facts and is deliberately not restated here.
fn directional_serialized_maximum(
    issued: &[MeDirectionalReferenceDescriptor],
) -> Result<usize, MeError> {
    issued.iter().try_fold(0usize, |total, descriptor| {
        let (MeDirectionalBacking::ContinuousState { width, .. }
        | MeDirectionalBacking::ContinuousStateDerivative { width, .. }) = descriptor.backing;
        total.checked_add(width).ok_or_else(|| {
            contract("the issued directional-derivative table's serialized width overflows")
        })
    })
}

fn reserved_indicator_values(entries: usize, context: &'static str) -> Result<Vec<f64>, MeError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(entries)
        .map_err(|_| MeError::Allocation { context, entries })?;
    values.resize(entries, 0.0);
    Ok(values)
}
