use super::*;

struct EventRuntimePreparation {
    guarded_assignment_programs: Vec<PreparedGuardedAssignmentProgram>,
    event_transaction_programs: Vec<PreparedEventTransactionProgram>,
    event_transaction_output_scratch: Vec<Vec<f64>>,
    event_transaction_coverage: PreparedEventTransactionCoverage,
    observation_refresh_scalar_rows: Box<[usize]>,
    event_transaction_execution:
        Vec<ExecutionArm<Rc<dyn CompiledSolveEventTransaction>, EventTransactionPermit>>,
}

fn prepare_event_runtime(
    model: &solve::SolveModel,
    execution_request: ExecutionRequest<'_>,
    interpreter: EventTransactionPermit,
) -> Result<EventRuntimePreparation, RuntimeSolveError> {
    let guarded_assignment_programs = model
        .problem()
        .discrete()
        .guarded_assignments
        .iter()
        .map(PreparedGuardedAssignmentProgram::new)
        .collect::<Result<Vec<_>, _>>()?;
    let event_transaction_programs = model
        .problem()
        .discrete()
        .event_transactions
        .iter()
        .map(|program| PreparedEventTransactionProgram::new(program, model.pure_calls()))
        .collect::<Result<Vec<_>, _>>()?;
    let event_transaction_output_scratch = event_transaction_programs
        .iter()
        .map(|program| vec![0.0; program.output_scalar_count()])
        .collect();
    let event_transaction_coverage = PreparedEventTransactionCoverage::new(model);
    let observation_refresh_scalar_rows = model
        .problem()
        .discrete()
        .observation_refresh
        .iter()
        .enumerate()
        .filter_map(|(row, selected)| selected.then_some(row))
        .collect::<Box<[_]>>();
    let event_transaction_execution = event_transaction_programs
        .iter()
        .enumerate()
        .map(|(index, prepared)| match execution_request {
            ExecutionRequest::Interpreter => Ok(ExecutionArm::Interpreter(interpreter)),
            ExecutionRequest::Native(backend) => backend
                .compile_event_transaction(prepared.program())
                .map(ExecutionArm::Native)
                .map_err(|reason| {
                    RuntimeSolveError::native_compile(
                        NativeExecutionOwner::EventTransaction { index },
                        reason,
                    )
                }),
        })
        .collect::<Result<Vec<_>, RuntimeSolveError>>()?;
    Ok(EventRuntimePreparation {
        guarded_assignment_programs,
        event_transaction_programs,
        event_transaction_output_scratch,
        event_transaction_coverage,
        observation_refresh_scalar_rows,
        event_transaction_execution,
    })
}

struct RefreshSourcePreparation {
    plans: RawRefreshPlans,
    runtime: RefreshRuntimePreparation,
}

struct RefreshRuntimePreparation {
    static_refresh_parameter_indices: Box<[usize]>,
    root_conditions_execution: RootConditionExecution,
    initial_scalar_residual: solve::ScalarProgramBlock,
    initial_continuation: Option<InitialContinuationCoverage>,
    initial_residual_execution:
        ExecutionArm<Rc<dyn CompiledSolveExpression>, InitialResidualPermit>,
    initial_residual_jacobian_execution:
        ExecutionArm<Rc<dyn CompiledSolveJacobianExpression>, InitialResidualJacobianPermit>,
}

struct RawRefreshPlans {
    algebraic: solve::IssuedRefreshPlan,
    derivative: solve::IssuedRefreshPlan,
    root: solve::IssuedRefreshPlan,
    event: solve::IssuedRefreshPlan,
    root_after_derivative: solve::IssuedRefreshPlan,
    clock_events_after_event: Vec<solve::IssuedRefreshPlan>,
}

fn prepare_refresh_sources(
    model: &solve::SolveModel,
    execution_request: ExecutionRequest<'_>,
    implicit_scalar_rhs: &PreparedScalarProgramBlock,
    permits: &InterpreterExecutionPlan,
) -> Result<RefreshSourcePreparation, RuntimeSolveError> {
    let refresh_owners = model.problem().continuous().refresh_owners();
    let algebraic_refresh = refresh_owners.algebraic().clone();
    let derivative_refresh = refresh_owners.derivative().clone();
    let root_refresh = refresh_owners.root().clone();
    let event_refresh = refresh_owners.event().clone();
    let clock_event_refresh = refresh_owners.clock_events().to_vec();
    trace_refresh_plan(model, "algebraic", &algebraic_refresh);
    trace_refresh_plan(model, "derivative", &derivative_refresh);
    trace_refresh_plan(model, "root", &root_refresh);
    trace_refresh_plan(model, "event", &event_refresh);
    for (clock, plan) in clock_event_refresh.iter().enumerate() {
        trace_refresh_plan(model, &format!("clock-event-{clock}"), plan);
    }
    let static_refresh_parameter_indices = refresh_owners
        .static_parameter_indices()
        .to_vec()
        .into_boxed_slice();
    let root_refresh_after_derivative = refresh_owners.root_after_derivative().remainder().clone();
    let clock_event_refresh_after_event = refresh_owners
        .clock_events_after_event()
        .iter()
        .map(solve::RefreshRemainderRelation::remainder)
        .cloned()
        .collect::<Vec<_>>();
    trace_reverse_projection_coverage(model, implicit_scalar_rhs);
    let root_conditions_execution =
        match root_condition_plan(model, &root_refresh, permits.preparation_constant_roots)? {
            Some(plan) => RootConditionExecution::Planned {
                plan,
                interpreter: permits.root_conditions,
            },
            None => RootConditionExecution::Direct(expression_arm(
                execution_request,
                NativeExecutionOwner::RootConditions,
                permits.root_conditions,
                &model.problem().events().root_conditions,
            )?),
        };
    let (initial_scalar_residual, initial_continuation) =
        InitialContinuationCoverage::certify_runtime_blocks(
            model,
            implicit_scalar_rhs,
            &algebraic_refresh,
        )?;
    let initial_residual_execution = expression_arm(
        execution_request,
        NativeExecutionOwner::InitialResidual,
        permits.initial_residual,
        &initial_scalar_residual,
    )?;
    let initial_scalar_jacobian =
        to_scalar_program_block(&model.artifacts().initialization().residual_jacobian_v)?;
    let initial_residual_jacobian_execution = jacobian_arm(
        execution_request,
        NativeExecutionOwner::InitialResidualJacobian,
        permits.initial_residual_jacobian,
        &initial_scalar_jacobian,
    )?;
    Ok(RefreshSourcePreparation {
        plans: RawRefreshPlans {
            algebraic: algebraic_refresh,
            derivative: derivative_refresh,
            root: root_refresh,
            event: event_refresh,
            root_after_derivative: root_refresh_after_derivative,
            clock_events_after_event: clock_event_refresh_after_event,
        },
        runtime: RefreshRuntimePreparation {
            static_refresh_parameter_indices,
            root_conditions_execution,
            initial_scalar_residual,
            initial_continuation,
            initial_residual_execution,
            initial_residual_jacobian_execution,
        },
    })
}

struct ExecutableRefreshPreparation {
    algebraic_refresh: PreparedRefreshPlan,
    derivative_refresh: PreparedRefreshPlan,
    root_refresh: PreparedRefreshPlan,
    event_refresh: PreparedRefreshPlan,
    root_refresh_after_derivative: PreparedRefreshPlan,
    clock_event_refresh_after_event: Vec<PreparedRefreshPlan>,
    exact_assignments: ExactAssignmentExecutionCatalog,
}

fn prepare_executable_refreshes(
    plans: RawRefreshPlans,
    continuous_structural: &solve::ContinuousStructuralArtifacts,
    refresh_program_catalog: &PreparedRefreshProgramCatalog<'_>,
    execution_request: ExecutionRequest<'_>,
    model: &solve::SolveModel,
    exact_assignment_permit: ExactAssignmentPermit,
) -> Result<ExecutableRefreshPreparation, RuntimeSolveError> {
    let algebraic_refresh = prepare_refresh_plan(
        plans.algebraic,
        continuous_structural,
        refresh_program_catalog,
    )?;
    let derivative_refresh = prepare_refresh_plan(
        plans.derivative,
        continuous_structural,
        refresh_program_catalog,
    )?;
    let root_refresh =
        prepare_refresh_plan(plans.root, continuous_structural, refresh_program_catalog)?;
    let event_refresh =
        prepare_refresh_plan(plans.event, continuous_structural, refresh_program_catalog)?;
    let root_refresh_after_derivative = prepare_refresh_plan(
        plans.root_after_derivative,
        continuous_structural,
        refresh_program_catalog,
    )?;
    let clock_event_refresh_after_event = plans
        .clock_events_after_event
        .into_iter()
        .map(|plan| prepare_refresh_plan(plan, continuous_structural, refresh_program_catalog))
        .collect::<Result<Vec<_>, RuntimeSolveError>>()?;
    let mut executable_refreshes = vec![
        &algebraic_refresh,
        &derivative_refresh,
        &root_refresh,
        &event_refresh,
    ];
    executable_refreshes.push(&root_refresh_after_derivative);
    executable_refreshes.extend(clock_event_refresh_after_event.iter());
    let exact_assignments = exact_assignment_arms(
        execution_request,
        model,
        &executable_refreshes,
        exact_assignment_permit,
    )?;
    Ok(ExecutableRefreshPreparation {
        algebraic_refresh,
        derivative_refresh,
        root_refresh,
        event_refresh,
        root_refresh_after_derivative,
        clock_event_refresh_after_event,
        exact_assignments,
    })
}

struct DiscreteRuntimePreparation {
    delay_runtime: DelayRuntime,
    root_condition_count: usize,
    structured_discrete_rows: PreparedStructuredDiscreteRows,
    clock_partition_structured_rows: Vec<Vec<usize>>,
    clock_partition_clocks: Vec<solve::PeriodicClockId>,
}

fn prepare_discrete_runtime(
    model: &solve::SolveModel,
    structured_discrete_scalar_rhs: solve::ScalarProgramBlock,
) -> Result<DiscreteRuntimePreparation, RuntimeSolveError> {
    let delay_runtime = DelayRuntime::new(&model.problem().events().delays)?;
    let root_condition_count = total_root_condition_count(model, delay_runtime.event_root_count())?;
    let structured_discrete_rows =
        PreparedStructuredDiscreteRows::new(model, structured_discrete_scalar_rhs)?;
    let clock_partition_structured_rows = discrete_rows::clock_partition_structured_rows(
        model.problem().discrete().structured_updates.len(),
        structured_discrete_rows.rows(),
    );
    let clock_partition_clocks = discrete_rows::clock_partition_clocks(model.problem().discrete());
    Ok(DiscreteRuntimePreparation {
        delay_runtime,
        root_condition_count,
        structured_discrete_rows,
        clock_partition_structured_rows,
        clock_partition_clocks,
    })
}

struct StructuralRuntimePreparation {
    continuous: solve::ContinuousStructuralArtifacts,
    initialization: solve::InitializationStructuralArtifacts,
    algebraic_newton_caches: Vec<RefCell<crate::runtime::projection::SparseNewtonCache>>,
}

fn prepare_runtime_structure(model: &solve::SolveModel) -> StructuralRuntimePreparation {
    let continuous = model.artifacts().continuous().structural.clone();
    let initialization = model.artifacts().initialization().structural.clone();
    let algebraic_newton_caches = (0..continuous.algebraic_projection().len())
        .map(|_| RefCell::new(crate::runtime::projection::SparseNewtonCache::default()))
        .collect();
    StructuralRuntimePreparation {
        continuous,
        initialization,
        algebraic_newton_caches,
    }
}

struct ProgramRuntimePreparation {
    runtime: PreparedRuntimePrograms,
    structured_discrete_scalar_rhs: solve::ScalarProgramBlock,
}

struct PreparedRuntimePrograms {
    implicit_rhs_execution: ExecutionArm<Rc<dyn CompiledSolveExpression>, ImplicitResidualPermit>,
    implicit_projection_jacobian_execution:
        ExecutionArm<Rc<dyn CompiledSolveJacobianExpression>, ImplicitProjectionJacobianPermit>,
    implicit_full_jacobian_execution:
        ExecutionArm<Rc<dyn CompiledSolveJacobianExpression>, ImplicitFullJacobianPermit>,
    derivative_rhs_execution: ExecutionArm<Rc<dyn CompiledSolveExpression>, DerivativeRhsPermit>,
    implicit_projection_scalar_jacobian: solve::ScalarProgramBlock,
    implicit_full_jacobian_v: solve::ScalarProgramBlock,
    implicit_scalar_rhs: PreparedScalarProgramBlock,
    manifold_residual: PreparedComputeBlock,
    manifold_jacobian_v: PreparedComputeBlock,
    derivative_scalar_rhs: solve::ScalarProgramBlock,
    events: EventRuntimePreparation,
}

fn prepare_runtime_programs(
    model: &solve::SolveModel,
    execution_request: ExecutionRequest<'_>,
    permits: &InterpreterExecutionPlan,
    implicit_scalar_programs: solve::ScalarProgramBlock,
) -> Result<ProgramRuntimePreparation, RuntimeSolveError> {
    let implicit_rhs_execution = expression_arm(
        execution_request,
        NativeExecutionOwner::ImplicitResidual,
        permits.implicit_residual,
        &implicit_scalar_programs,
    )?;
    let implicit_projection_scalar_jacobian =
        to_scalar_program_block(&model.artifacts().continuous().implicit_jacobian_v)?;
    let implicit_projection_jacobian_execution = jacobian_arm(
        execution_request,
        NativeExecutionOwner::ImplicitProjectionJacobian,
        permits.implicit_projection_jacobian,
        &implicit_projection_scalar_jacobian,
    )?;
    let implicit_full_jacobian_v = model
        .artifacts()
        .continuous()
        .implicit_jacobian_v_scalar
        .clone();
    let implicit_full_jacobian_execution = jacobian_arm(
        execution_request,
        NativeExecutionOwner::ImplicitFullJacobian,
        permits.implicit_full_jacobian,
        &implicit_full_jacobian_v,
    )?;
    let implicit_scalar_rhs = PreparedScalarProgramBlock::new(implicit_scalar_programs)?;
    let (manifold_residual, manifold_jacobian_v) = prepare_manifold_projection_programs(model)?;
    let derivative_scalar_rhs =
        to_scalar_program_block(model.problem().continuous().derivative_rhs())?;
    let structured_discrete_scalar_rhs =
        to_scalar_program_block(&model.problem().discrete().structured_rhs)?;
    let events = prepare_event_runtime(model, execution_request, permits.event_transactions)?;
    let derivative_rhs_execution = expression_arm(
        execution_request,
        NativeExecutionOwner::DerivativeRhs,
        permits.derivative_rhs,
        &derivative_scalar_rhs,
    )?;
    Ok(ProgramRuntimePreparation {
        runtime: PreparedRuntimePrograms {
            implicit_rhs_execution,
            implicit_projection_jacobian_execution,
            implicit_full_jacobian_execution,
            derivative_rhs_execution,
            implicit_projection_scalar_jacobian,
            implicit_full_jacobian_v,
            implicit_scalar_rhs,
            manifold_residual,
            manifold_jacobian_v,
            derivative_scalar_rhs,
            events,
        },
        structured_discrete_scalar_rhs,
    })
}

struct FinalRuntimeBlocks {
    implicit_rhs: PreparedComputeBlock,
    implicit_projection_jacobian_v: PreparedComputeBlock,
    implicit_projection_scalar_jacobian_v: PreparedScalarProgramBlock,
    initial_residual: PreparedComputeBlock,
    initial_residual_jacobian_v: PreparedComputeBlock,
    initial_scalar_residual: PreparedScalarProgramBlock,
    derivative_rhs: PreparedComputeBlock,
    derivative_jacobian_v: PreparedScalarProgramBlock,
    derivative_scalar: PreparedScalarProgramBlock,
    implicit_jacobian_v: PreparedScalarProgramBlock,
    root_condition_rows: PreparedScalarProgramBlock,
    event_action_conditions: PreparedScalarProgramBlock,
    discrete_rhs: PreparedScalarProgramBlock,
    clock_partition_intermediates: PreparedScalarProgramBlock,
    runtime_assignment_rhs: PreparedScalarProgramBlock,
    post_commit_assignment_rhs: PreparedScalarProgramBlock,
    visible_value_rows: PreparedScalarProgramBlock,
}

fn prepare_final_runtime_blocks(
    model: &solve::SolveModel,
    implicit_projection_scalar_jacobian: solve::ScalarProgramBlock,
    initial_scalar_residual: solve::ScalarProgramBlock,
    derivative_scalar_rhs: solve::ScalarProgramBlock,
    implicit_full_jacobian_v: solve::ScalarProgramBlock,
) -> Result<FinalRuntimeBlocks, RuntimeSolveError> {
    Ok(FinalRuntimeBlocks {
        implicit_rhs: PreparedComputeBlock::new_with_label(
            model.problem().continuous().implicit_rhs(),
            "runtime_implicit_rhs",
        )?,
        implicit_projection_jacobian_v: PreparedComputeBlock::new_with_label(
            &model.artifacts().continuous().implicit_jacobian_v,
            "runtime_implicit_projection_jacobian_v",
        )?,
        implicit_projection_scalar_jacobian_v: PreparedScalarProgramBlock::new(
            implicit_projection_scalar_jacobian,
        )?,
        initial_residual: PreparedComputeBlock::new_with_label(
            model.problem().initialization().residual(),
            "runtime_initial_residual",
        )?,
        initial_residual_jacobian_v: PreparedComputeBlock::new_with_label(
            &model.artifacts().initialization().residual_jacobian_v,
            "runtime_initial_residual_jacobian_v",
        )?,
        initial_scalar_residual: PreparedScalarProgramBlock::new(initial_scalar_residual)?,
        derivative_rhs: PreparedComputeBlock::new_with_label(
            model.problem().continuous().derivative_rhs(),
            "runtime_derivative_rhs",
        )?,
        derivative_jacobian_v: PreparedScalarProgramBlock::new(
            model.artifacts().continuous().full_jacobian_v.clone(),
        )?,
        derivative_scalar: PreparedScalarProgramBlock::new(derivative_scalar_rhs)?,
        implicit_jacobian_v: PreparedScalarProgramBlock::new(implicit_full_jacobian_v)?,
        root_condition_rows: PreparedScalarProgramBlock::new(
            model.problem().events().root_conditions.clone(),
        )?,
        event_action_conditions: PreparedScalarProgramBlock::new(
            model.problem().events().action_conditions.clone(),
        )?,
        discrete_rhs: PreparedScalarProgramBlock::new(model.problem().discrete().rhs.clone())?,
        clock_partition_intermediates: PreparedScalarProgramBlock::new(
            model
                .problem()
                .discrete()
                .clock_partition_intermediates
                .clone(),
        )?,
        runtime_assignment_rhs: PreparedScalarProgramBlock::new(
            model.problem().discrete().runtime_assignment_rhs.clone(),
        )?,
        post_commit_assignment_rhs: PreparedScalarProgramBlock::new(
            model
                .problem()
                .discrete()
                .post_commit_assignment_rhs
                .clone(),
        )?,
        visible_value_rows: PreparedScalarProgramBlock::new(model.visible_value_rows().clone())?,
    })
}

struct RuntimeAssembly {
    model: SolveRuntimeModelOwner,
    structure: StructuralRuntimePreparation,
    programs: PreparedRuntimePrograms,
    refresh: RefreshRuntimePreparation,
    discrete: DiscreteRuntimePreparation,
    executable_refreshes: ExecutableRefreshPreparation,
    visible_value_plan: Option<VisibleValuePlan>,
    interpreter_execution: InterpreterExecutionPlan,
}

fn assemble_runtime(input: RuntimeAssembly) -> Result<SolveRuntime, RuntimeSolveError> {
    let execution_plan = RuntimeExecutionPlan {
        implicit_rhs: input.programs.implicit_rhs_execution,
        implicit_projection_jacobian: input.programs.implicit_projection_jacobian_execution,
        implicit_full_jacobian: input.programs.implicit_full_jacobian_execution,
        initial_residual: input.refresh.initial_residual_execution,
        initial_residual_jacobian: input.refresh.initial_residual_jacobian_execution,
        derivative_rhs: input.programs.derivative_rhs_execution,
        root_conditions: input.refresh.root_conditions_execution,
        event_transactions: input.programs.events.event_transaction_execution,
        exact_assignments: input.executable_refreshes.exact_assignments,
        interpreter: input.interpreter_execution,
    };
    let blocks = prepare_final_runtime_blocks(
        input.model.model(),
        input.programs.implicit_projection_scalar_jacobian,
        input.refresh.initial_scalar_residual,
        input.programs.derivative_scalar_rhs,
        input.programs.implicit_full_jacobian_v,
    )?;
    let output_names = input
        .model
        .model()
        .visible_names()
        .map(str::to_string)
        .collect();
    let visible_name_index = build_visible_name_index(input.model.model());
    Ok(SolveRuntime {
        model: input.model,
        output_names,
        visible_name_index,
        implicit_rhs: blocks.implicit_rhs,
        implicit_projection_jacobian_v: blocks.implicit_projection_jacobian_v,
        implicit_projection_scalar_jacobian_v: blocks.implicit_projection_scalar_jacobian_v,
        implicit_scalar_rhs: input.programs.implicit_scalar_rhs,
        manifold_residual: input.programs.manifold_residual,
        manifold_jacobian_v: input.programs.manifold_jacobian_v,
        initial_residual: blocks.initial_residual,
        initial_residual_jacobian_v: blocks.initial_residual_jacobian_v,
        initial_scalar_residual: blocks.initial_scalar_residual,
        derivative_rhs: blocks.derivative_rhs,
        derivative_jacobian_v: blocks.derivative_jacobian_v,
        derivative_scalar: blocks.derivative_scalar,
        implicit_jacobian_v: blocks.implicit_jacobian_v,
        continuous_structural: input.structure.continuous,
        initialization_structural: input.structure.initialization,
        algebraic_newton_caches: input.structure.algebraic_newton_caches,
        algebraic_refresh: input.executable_refreshes.algebraic_refresh,
        derivative_refresh: input.executable_refreshes.derivative_refresh,
        root_refresh: input.executable_refreshes.root_refresh,
        event_refresh: input.executable_refreshes.event_refresh,
        root_refresh_after_derivative: input.executable_refreshes.root_refresh_after_derivative,
        clock_event_refresh_after_event: input.executable_refreshes.clock_event_refresh_after_event,
        initial_continuation: input.refresh.initial_continuation,
        root_condition_rows: blocks.root_condition_rows,
        event_action_conditions: blocks.event_action_conditions,
        event_action_active_row_indices: RefCell::new(Vec::new()),
        discrete_rhs: blocks.discrete_rhs,
        observation_refresh_scalar_rows: input.programs.events.observation_refresh_scalar_rows,
        observation_refresh_p_scratch: RefCell::new(Vec::new()),
        observation_refresh_values_scratch: RefCell::new(Vec::new()),
        clock_partition_intermediates: blocks.clock_partition_intermediates,
        clock_partition_clocks: input.discrete.clock_partition_clocks,
        clock_partition_structured_rows: input.discrete.clock_partition_structured_rows,
        clock_partition_work_y: RefCell::new(Vec::new()),
        clock_partition_work_p: RefCell::new(Vec::new()),
        guarded_assignment_programs: input.programs.events.guarded_assignment_programs,
        event_transaction_programs: input.programs.events.event_transaction_programs,
        event_transaction_coverage: input.programs.events.event_transaction_coverage,
        runtime_assignment_rhs: blocks.runtime_assignment_rhs,
        post_commit_assignment_rhs: blocks.post_commit_assignment_rhs,
        update_values_scratch: RefCell::new(Vec::new()),
        structured_discrete_rows: input.discrete.structured_discrete_rows,
        visible_value_rows: blocks.visible_value_rows,
        visible_value_plan: input.visible_value_plan,
        visible_scratch: RefCell::new(Vec::new()),
        refresh_snapshot_scratch: RefCell::new(Vec::new()),
        refresh_probe_scratch: RefCell::new(Vec::new()),
        refresh_tensor_scratch: RefCell::new(Vec::new()),
        static_refresh_cache: RefCell::new(StaticRefreshCache::default()),
        static_refresh_parameter_indices: input.refresh.static_refresh_parameter_indices,
        parameter_static_gradient_cache: RefCell::new(ParameterStaticGradientCache::default()),
        torn_sweep_cache: TornSweepCache::default(),
        runtime_state: solve_eval::SimulationRuntimeState::new(),
        delay_runtime: input.discrete.delay_runtime,
        root_condition_count: input.discrete.root_condition_count,
        derivative_scratch: RefCell::new(StateDerivativeScratch::default()),
        root_scratch: RefCell::new(Vec::new()),
        reverse_scratch: RefCell::new(solve_eval::reverse::ReverseScratch::default()),
        clock_partition_intermediate_scratch: RefCell::new(Vec::new()),
        event_transaction_input_scratch: RefCell::new(Vec::new()),
        event_transaction_output_scratch: RefCell::new(
            input.programs.events.event_transaction_output_scratch,
        ),
        execution_plan,
        native_assignment_scratch: RefCell::new(Vec::new()),
        clock_activation_cache: RefCell::new(ClockActivationCache::default()),
    })
}

impl SolveRuntime {
    pub fn new(model: Arc<solve::SolveModel>) -> Result<Self, RuntimeSolveError> {
        Self::prepare(
            SolveRuntimeModelOwner::Standalone(model),
            ExecutionRequest::Interpreter,
        )
    }

    pub fn new_native(
        model: Arc<solve::SolveModel>,
        execution_backend: &dyn SolveExecutionBackend,
    ) -> Result<Self, RuntimeSolveError> {
        Self::prepare(
            SolveRuntimeModelOwner::Standalone(model),
            ExecutionRequest::Native(execution_backend),
        )
    }

    pub(crate) fn new_fmi(
        runtime: solve::fmi::FmiRuntimeView,
        execution_backend: Option<&dyn SolveExecutionBackend>,
    ) -> Result<Self, RuntimeSolveError> {
        let request =
            execution_backend.map_or(ExecutionRequest::Interpreter, ExecutionRequest::Native);
        Self::prepare(SolveRuntimeModelOwner::Fmi(runtime), request)
    }

    /// Borrow facts from the sealed FMI capability retained by this runtime.
    /// This method is crate-private because only linked FMI construction can
    /// own a runtime created by [`Self::new_fmi`].
    pub(crate) fn fmi_linked_runtime_facts(&self) -> &solve::fmi::FmiLinkedRuntimeFacts {
        match &self.model {
            SolveRuntimeModelOwner::Fmi(runtime) => runtime.linked_runtime_facts(),
            SolveRuntimeModelOwner::Standalone(_) => {
                unreachable!("linked FMI facts requested from a standalone Solve runtime")
            }
        }
    }

    fn prepare(
        model: SolveRuntimeModelOwner,
        execution_request: ExecutionRequest<'_>,
    ) -> Result<Self, RuntimeSolveError> {
        let model_ref = model.model();
        let interpreter_execution = InterpreterExecutionPlan::selected();
        let structure = prepare_runtime_structure(model_ref);
        let implicit_scalar_projection = solve_eval::to_scalar_program_projection(
            model_ref.problem().continuous().implicit_rhs(),
        )?;
        let refresh_program_sources = implicit_scalar_projection.sources().to_vec();
        let refresh_program_catalog =
            PreparedRefreshProgramCatalog::construct(&refresh_program_sources)?;
        let programs = prepare_runtime_programs(
            model_ref,
            execution_request,
            &interpreter_execution,
            implicit_scalar_projection.into_block(),
        )?;
        let refresh = prepare_refresh_sources(
            model_ref,
            execution_request,
            &programs.runtime.implicit_scalar_rhs,
            &interpreter_execution,
        )?;
        let visible_value_plan = visible_value_plan(model_ref);
        let discrete =
            prepare_discrete_runtime(model_ref, programs.structured_discrete_scalar_rhs)?;
        let executable_refreshes = prepare_executable_refreshes(
            refresh.plans,
            &structure.continuous,
            &refresh_program_catalog,
            execution_request,
            model_ref,
            interpreter_execution.exact_assignments,
        )?;
        drop(refresh_program_catalog);
        drop(refresh_program_sources);
        assemble_runtime(RuntimeAssembly {
            model,
            structure,
            programs: programs.runtime,
            refresh: refresh.runtime,
            discrete,
            executable_refreshes,
            visible_value_plan,
            interpreter_execution,
        })
    }

    /// Borrow the exact sealed Solve root retained by this runtime.
    #[must_use]
    pub fn model(&self) -> &solve::SolveModel {
        self.model.model()
    }

    /// Number of continuous-state coordinates, derived from the sealed root.
    #[must_use]
    pub fn state_count(&self) -> usize {
        self.model().state_scalar_count()
    }

    /// Number of solver Y coordinates, derived from the sealed root.
    #[must_use]
    pub fn solver_count(&self) -> usize {
        self.model().solver_scalar_count()
    }
    /// Classify each periodic clock once per exact event instant.
    ///
    /// A grouped discrete program can have hundreds of scalar outputs sharing
    /// one clock. Repeating exact-lattice conversion and division for every
    /// output was measurable in the event hot path even though the answer is
    /// owned by the clock, not by the row.
    pub(super) fn discrete_row_active_at(
        &self,
        row_idx: usize,
        t: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let owner = self
            .model
            .problem()
            .discrete()
            .clock_owners
            .get(row_idx)
            .copied()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "discrete clock-owner row index {row_idx} is out of bounds"
                ))
            })?;
        let Some(owner) = owner else {
            return Ok(true);
        };
        self.periodic_clock_active(owner, t, "discrete row")
    }

    pub(super) fn periodic_clock_active(
        &self,
        owner: solve::PeriodicClockId,
        t: f64,
        context: &str,
    ) -> Result<bool, RuntimeSolveError> {
        let schedules = &self.model().problem().clocks().periodic_event_schedules;
        if owner.index() >= schedules.len() {
            return Err(RuntimeSolveError::solve_ir(format!(
                "{context} refers to periodic clock {} outside the clock partition",
                owner.index()
            )));
        }
        let mut cache = self.clock_activation_cache.borrow_mut();
        if cache.time_bits != Some(t.to_bits()) {
            cache.active.clear();
            cache.active.extend(
                schedules
                    .iter()
                    .map(|schedule| crate::timeline::periodic_schedule_matches_time(schedule, t)),
            );
            cache.time_bits = Some(t.to_bits());
        }
        Ok(cache.active[owner.index()])
    }

    pub(super) fn eval_discrete_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        self.discrete_rhs.eval_row_outputs_unchecked_with_context(
            program,
            y,
            p,
            t,
            self.execution_plan
                .interpreter
                .discrete_scalar_rows
                .row_eval_context(self),
            out,
        )?;
        self.report_nonfinite_discrete_program_outputs(program, t, out);
        Ok(())
    }

    fn report_nonfinite_discrete_program_outputs(&self, program: usize, t: f64, values: &[f64]) {
        if !solve_eval::nan_trace::nan_trace_enabled() {
            return;
        }
        for (offset, value) in values.iter().copied().enumerate() {
            if value.is_finite() {
                continue;
            }
            let row = (0..self.model().problem().discrete().update_targets.len())
                .find(|&row| self.discrete_rhs.row_output_position(row) == Some((program, offset)));
            let target = row.and_then(|row| {
                self.model()
                    .problem()
                    .discrete()
                    .update_targets
                    .get(row)
                    .copied()
            });
            let name = target.and_then(|target| {
                self.model()
                    .problem()
                    .layout()
                    .bindings()
                    .iter()
                    .rev()
                    .find_map(|(name, slot)| (*slot == target).then(|| name.to_string()))
            });
            eprintln!(
                "[nan-trace] discrete program {program} output {offset} (row {}, target {}, slot {target:?}) @ t={t} = {}",
                row.map_or_else(|| "?".to_string(), |row| row.to_string()),
                name.as_deref().unwrap_or("<unnamed>"),
                if value.is_nan() { "NaN" } else { "inf" },
            );
        }
    }

    pub(super) fn report_nonfinite_implicit_residual_inputs(
        &self,
        t: f64,
        y: &[f64],
        residual: &[f64],
    ) {
        if !solve_eval::nan_trace::nan_trace_enabled() {
            return;
        }
        for (row, value) in residual.iter().copied().enumerate() {
            self.report_nonfinite_implicit_residual_row_inputs(t, y, row, value);
        }
    }

    pub(super) fn report_nonfinite_implicit_residual_row_inputs(
        &self,
        t: f64,
        y: &[f64],
        row: usize,
        value: f64,
    ) {
        if value.is_finite() || !solve_eval::nan_trace::nan_trace_enabled() {
            return;
        }
        let Some((program, _)) = self.implicit_scalar_rhs.row_output_position(row) else {
            return;
        };
        let Some(ops) = self.implicit_scalar_rhs.block().programs().get(program) else {
            return;
        };
        let mut inputs = BTreeSet::new();
        for op in ops {
            match op {
                solve::LinearOp::LoadY { index, .. } => {
                    inputs.insert(*index);
                }
                solve::LinearOp::TensorLoad {
                    input: solve::TensorInputKind::Y,
                    input_start,
                    count,
                    ..
                } => {
                    inputs.extend(*input_start..input_start.saturating_add(*count));
                }
                _ => {}
            }
        }
        eprintln!(
            "[nan-trace] implicit residual row {row} ({}) @ t={t} = {}",
            self.solver_name(row),
            if value.is_nan() { "NaN" } else { "inf" },
        );
        for index in inputs {
            eprintln!(
                "[nan-trace]   input y[{index}] ({}) = {}",
                self.solver_name(index),
                y.get(index)
                    .map_or_else(|| "<missing>".to_string(), ToString::to_string),
            );
        }
    }

    #[cfg(test)]
    pub(super) fn root_condition_plan_for_test(&self) -> Option<&RootConditionPlan> {
        match &self.execution_plan.root_conditions {
            RootConditionExecution::Planned { plan, .. } => Some(plan),
            RootConditionExecution::Direct(_) => None,
        }
    }

    pub fn has_delay_channels(&self) -> bool {
        !self.delay_runtime.is_empty()
    }

    pub fn reset_delay_history(&self) {
        self.delay_runtime.reset();
    }

    pub(crate) fn snapshot(&self) -> SolveRuntimeSnapshot {
        SolveRuntimeSnapshot {
            evaluator: self.runtime_state.snapshot(),
            delay: self.delay_runtime.snapshot(),
        }
    }

    pub(crate) fn snapshot_into(&self, snapshot: &mut SolveRuntimeSnapshot) {
        self.runtime_state.snapshot_into(&mut snapshot.evaluator);
        self.delay_runtime.snapshot_into(&mut snapshot.delay);
    }

    pub(crate) fn restore(&self, snapshot: &SolveRuntimeSnapshot) {
        // Parameter-static refresh values are exact-keyed derived data, not
        // component continuation state. Keeping them across an FMU-state
        // restore is safe because every read validates the complete parameter
        // key, and avoids copying the model-width cache for every host probe.
        self.runtime_state.restore(&snapshot.evaluator);
        self.delay_runtime.restore(&snapshot.delay);
    }

    #[cfg(test)]
    pub(crate) fn matches_snapshot(&self, snapshot: &SolveRuntimeSnapshot) -> bool {
        self.runtime_state.matches_snapshot(&snapshot.evaluator)
            && self.delay_runtime.matches_snapshot(&snapshot.delay)
    }

    pub fn initialize_delay_history(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .initialize(
                time,
                solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
            )
            .map_err(Into::into)
    }

    pub fn refresh_delay_values(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.delay_runtime
            .refresh(
                time,
                solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
            )
            .map_err(Into::into)
    }

    pub fn commit_delay_history(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .commit(
                time,
                solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
            )
            .map_err(Into::into)
    }

    pub fn root_condition_count(&self) -> usize {
        self.root_condition_count
    }

    pub fn derivative_settled_coordinate_can_refresh_roots(&self) -> bool {
        true
    }

    pub fn full_solver_y(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut solver_y = Vec::new();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        self.refresh_algebraic_and_output_slots(t, &mut solver_y, params, tol, max_iters)?;
        Ok(solver_y)
    }

    pub fn full_solver_y_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        solver_y: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        self.populate_solver_y_from_state(solver_y, state)?;
        self.refresh_algebraic_and_output_slots(t, solver_y, params, tol, max_iters)
    }

    pub fn full_solver_y_with_guess(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.refresh_algebraic_and_output_slots(t, guess, params, tol, max_iters)
    }

    pub fn eval_state_derivatives(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut derivative = zero_runtime_values(self.state_count(), "state derivative output")?;
        self.eval_state_derivatives_into(t, state, params, tol, max_iters, &mut derivative)?;
        Ok(derivative)
    }

    pub fn eval_state_derivatives_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut scratch = self.derivative_scratch.borrow_mut();
        let solver_y = &mut scratch.solver_y;
        self.populate_solver_y_from_state(solver_y, state)?;
        self.eval_state_derivatives_at_solver_y(t, params, tol, max_iters, solver_y, out)
    }

    pub fn eval_state_derivatives_with_guess(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut derivative = zero_runtime_values(self.state_count(), "state derivative output")?;
        self.eval_state_derivatives_with_guess_into(
            t,
            state,
            params,
            guess,
            AlgebraicSettle { tol, max_iters },
            &mut derivative,
        )?;
        Ok(derivative)
    }

    /// The public runtime API mirrors the solver callback inputs: it never
    /// hides the caller's scratch or output buffers behind an allocation. The
    /// convergence controls travel as the same `AlgebraicSettle` the
    /// linearization entry points already carry.
    pub fn eval_state_derivatives_with_guess_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        settle: AlgebraicSettle,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.refresh_derivative_dependencies(t, guess, params, settle.tol, settle.max_iters)?;
        self.eval_derivative_rhs_from_solver_y(t, guess, params, out)
    }

    pub fn eval_root_conditions(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return Ok(Vec::new());
        }
        let mut values = zero_runtime_values(root_count, "root condition output")?;
        self.eval_root_conditions_into(t, state, params, tol, max_iters, &mut values)?;
        Ok(values)
    }

    pub fn eval_root_conditions_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root condition output", root_count, out.len())?;
        let model_root_count = self.model().problem().events().root_conditions.len();
        let mut solver_y = self.root_scratch.borrow_mut();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        self.refresh_slots_with_plan(
            &self.root_refresh,
            RefreshSlotArgs {
                t,
                solver_y: &mut solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: false,
            },
        )?;
        self.eval_root_conditions_from_refreshed_solver_y(
            t,
            &solver_y,
            params,
            &mut out[..model_root_count],
        )?;
        self.delay_runtime
            .evaluate_event_roots(
                t,
                &solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
                &mut out[model_root_count..],
            )
            .map_err(RuntimeSolveError::from)?;
        validate_finite_runtime_output("root condition output", out)
    }

    pub fn eval_root_search_conditions_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut solver_y = self.root_scratch.borrow_mut();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        self.eval_root_search_conditions_at_solver_y(t, params, tol, max_iters, out, &mut solver_y)
    }

    /// The public runtime API mirrors the solver callback inputs: it never
    /// hides the certified warm start or the output buffer behind an
    /// allocation. The convergence controls travel as the same
    /// `AlgebraicSettle` the linearization entry points already carry.
    pub fn eval_root_search_conditions_with_guess_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        settle: AlgebraicSettle,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.eval_root_search_conditions_at_solver_y(
            t,
            params,
            settle.tol,
            settle.max_iters,
            out,
            guess,
        )
    }

    fn eval_root_search_conditions_at_solver_y(
        &self,
        t: f64,
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
        solver_y: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root search output", root_count, out.len())?;
        let model_root_count = self.model().problem().events().root_conditions.len();
        let model_roots_need_refresh = model_root_count > 0
            && match &self.execution_plan.root_conditions {
                RootConditionExecution::Planned { plan, .. } => !plan.search_rows.is_empty(),
                RootConditionExecution::Direct(_) => true,
            };
        if model_roots_need_refresh || self.delay_runtime.event_root_count() > 0 {
            self.refresh_slots_with_plan(
                &self.root_refresh,
                RefreshSlotArgs {
                    t,
                    solver_y,
                    params,
                    tol,
                    max_iters,
                    certify_coordinates: false,
                },
            )?;
        }
        if model_root_count > 0 {
            self.eval_model_root_search_conditions(
                t,
                params,
                solver_y,
                &mut out[..model_root_count],
            )?;
        }
        self.delay_runtime
            .evaluate_event_roots(
                t,
                solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
                &mut out[model_root_count..],
            )
            .map_err(RuntimeSolveError::from)?;
        validate_finite_runtime_output("root search output", out)
    }

    fn eval_model_root_search_conditions(
        &self,
        t: f64,
        params: &[f64],
        solver_y: &[f64],
        model_out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let RootConditionExecution::Planned { plan, interpreter } =
            &self.execution_plan.root_conditions
        else {
            return self
                .eval_root_conditions_from_refreshed_solver_y(t, solver_y, params, model_out);
        };
        self.validate_root_plan_output_len(plan, model_out)?;
        if plan.search_rows.is_empty() {
            return self.write_planned_root_search_defaults(plan, params, t, model_out);
        }
        self.write_planned_root_search_conditions(plan, interpreter, solver_y, params, t, model_out)
    }

    pub fn eval_root_search_conditions_after_derivative_settle_into(
        &self,
        t: f64,
        params: &[f64],
        solver_y: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if solver_y.len() != self.solver_count() {
            return Err(RuntimeSolveError::solve_ir(format!(
                "derivative-settled solver-y length mismatch: expected {}, got {}",
                self.solver_count(),
                solver_y.len()
            )));
        }
        self.refresh_slots_with_plan(
            &self.root_refresh_after_derivative,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: false,
            },
        )?;
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root search output", root_count, out.len())?;
        let model_root_count = self.model().problem().events().root_conditions.len();
        if model_root_count > 0 {
            let model_out = &mut out[..model_root_count];
            match &self.execution_plan.root_conditions {
                RootConditionExecution::Planned { plan, .. } if plan.search_rows.is_empty() => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    self.write_planned_root_search_defaults(plan, params, t, model_out)?;
                }
                RootConditionExecution::Planned { plan, interpreter } => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    self.write_planned_root_search_conditions(
                        plan,
                        interpreter,
                        solver_y,
                        params,
                        t,
                        model_out,
                    )?;
                }
                RootConditionExecution::Direct(_) => self
                    .eval_root_conditions_from_refreshed_solver_y(t, solver_y, params, model_out)?,
            }
        }
        self.delay_runtime
            .evaluate_event_roots(
                t,
                solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
                &mut out[model_root_count..],
            )
            .map_err(RuntimeSolveError::from)?;
        validate_finite_runtime_output("root search output", out)
    }

    pub(super) fn eval_root_conditions_from_refreshed_solver_y(
        &self,
        t: f64,
        y: &[f64],
        p: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match &self.execution_plan.root_conditions {
            RootConditionExecution::Planned { plan, interpreter } => {
                self.write_planned_root_conditions(plan, interpreter, y, p, t, out)?
            }
            RootConditionExecution::Direct(ExecutionArm::Native(compiled)) => {
                compiled.call(y, p, t, out).map_err(|reason| {
                    RuntimeSolveError::native_call(NativeExecutionOwner::RootConditions, reason)
                })?
            }
            RootConditionExecution::Direct(ExecutionArm::Interpreter(selected_arm)) => self
                .root_condition_rows
                .eval_with_context(y, p, t, selected_arm.row_eval_context(self), out)?,
        }
        validate_finite_runtime_output("root condition output", out)
    }

    fn write_planned_root_conditions(
        &self,
        plan: &RootConditionPlan,
        selected_arm: &RootConditionsPermit,
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.validate_root_plan_output_len(plan, out)?;
        for (slot, entry) in out.iter_mut().zip(plan.entries.iter().copied()) {
            *slot = match entry {
                RootConditionPlanEntry::ConstantNonZero(value) => value,
                RootConditionPlanEntry::DirectTime(root) => {
                    direct_time_root_value(root, params, t)?
                }
                RootConditionPlanEntry::ContinuousStatic => 0.0,
                RootConditionPlanEntry::Dynamic => 0.0,
            };
        }
        self.eval_planned_root_rows(selected_arm, &plan.evaluated_rows, y, params, t, out)
    }

    fn write_planned_root_search_conditions(
        &self,
        plan: &RootConditionPlan,
        selected_arm: &RootConditionsPermit,
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.write_planned_root_search_defaults(plan, params, t, out)?;
        self.eval_planned_root_rows(selected_arm, &plan.search_rows, y, params, t, out)
    }

    fn write_planned_root_search_defaults(
        &self,
        plan: &RootConditionPlan,
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.validate_root_plan_output_len(plan, out)?;
        for (slot, entry) in out.iter_mut().zip(plan.entries.iter().copied()) {
            *slot = match entry {
                RootConditionPlanEntry::ConstantNonZero(_)
                | RootConditionPlanEntry::ContinuousStatic
                | RootConditionPlanEntry::Dynamic => 1.0,
                RootConditionPlanEntry::DirectTime(root) => {
                    direct_time_root_search_default(root, params, t)?
                }
            };
        }
        Ok(())
    }

    fn eval_planned_root_rows(
        &self,
        selected_arm: &RootConditionsPermit,
        output_indices: &[usize],
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if output_indices.is_empty() {
            return Ok(());
        }
        self.eval_selected_outputs(
            selected_arm,
            SelectedRows {
                block: &self.root_condition_rows,
            },
            output_indices,
            RowEvalPoint { y, p: params, t },
            out,
        )
    }

    fn validate_root_plan_output_len(
        &self,
        plan: &RootConditionPlan,
        out: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if out.len() >= plan.entries.len() {
            return Ok(());
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "root condition plan output index {} out of bounds for {} values",
            plan.entries.len().saturating_sub(1),
            out.len()
        )))
    }
}
