//! Prepare every required execution owner before exposing a runtime.

use super::*;

impl SolveRuntime {
    // SPEC_0021: Exception - construction-issued owner binding stays contiguous for auditability.
    #[allow(clippy::too_many_lines)]
    pub(super) fn construct(
        model_owner: SolveRuntimeModel,
        execution_backend: Option<Rc<dyn SolveExecutionBackend>>,
        primary: Option<&SolveRuntime>,
    ) -> Result<Self, EvalSolveError> {
        model_owner
            .artifacts
            .continuous
            .structural
            .require_scalar_derivatives()
            .map_err(|error| EvalSolveError::Scalarization {
                message: error.to_string(),
                span: None,
            })?;
        let primary = primary.map(Self::scalar_evaluation).transpose()?;
        let primary = primary.filter(|primary| {
            match (
                execution_backend.as_ref(),
                primary.scalar.execution_backend.as_ref(),
            ) {
                (Some(current), Some(base)) => Rc::ptr_eq(current, base),
                (None, None) => true,
                _ => false,
            }
        });
        let model = &*model_owner;
        if let Some(backend) = &execution_backend {
            backend.validate_model_context(model).map_err(|message| {
                EvalSolveError::CompiledExecution {
                    message: format!("compiled backend model context refused: {message}"),
                    span: None,
                }
            })?;
        }
        let prepare = |pick: fn(&ScalarExecutionInventory) -> &PreparedScalarProgramBlock,
                       block: solve::ScalarProgramBlock| {
            let base = primary.map(|primary| pick(primary.scalar));
            BlockReuse::of(base, &block).prepared(base, block)
        };
        // The state-derivative programs are evaluated, differentiated, and
        // reversed, never solved for a target, so they derive no assignment
        // certificates (one materialized program per output of a tensor row).
        if !model.problem.continuous.refresh_owners.is_issued() {
            return Err(EvalSolveError::InvalidRow {
                message: "Solve model has no construction-issued continuous refresh owners"
                    .to_string(),
                span: None,
            });
        }
        let continuous_structural = model.artifacts.continuous.structural.clone();
        let initialization_structural = model.artifacts.initialization.structural.clone();
        let algebraic_newton_caches = continuous_structural
            .algebraic_projection()
            .iter()
            .map(|structure| {
                RefCell::new(crate::runtime::projection::SparseNewtonCache::for_block(
                    structure,
                ))
            })
            .collect();
        let implicit_scalar_projection =
            solve_eval::to_scalar_program_projection(&model.problem.continuous.implicit_rhs)?;
        let mut refresh_program_rows = FxHashMap::default();
        for (row, source) in implicit_scalar_projection
            .sources()
            .iter()
            .copied()
            .enumerate()
        {
            let Some(source) = source else {
                continue;
            };
            if refresh_program_rows.insert(source, row).is_some() {
                return Err(EvalSolveError::InvalidRow {
                    message: "canonical refresh scalar-program identity is duplicated".to_string(),
                    span: implicit_scalar_projection.block().program_span(row),
                });
            }
        }
        let implicit_scalar_programs = implicit_scalar_projection.into_block();
        refresh_projection::validate_projection_primal_source(
            &implicit_scalar_programs,
            &continuous_structural,
        )?;
        let implicit_projection_scalar_jacobian = refresh_projection::projection_jacobian_source(
            &model.artifacts.continuous.implicit_jacobian_v,
            &continuous_structural,
        )?;
        let implicit_full_jacobian_v = model
            .artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .clone();
        let replaced_rows = PrimalProgram::replaced_outputs(
            primary.map(|primary| &primary.scalar.implicit_scalar_rhs),
            &implicit_scalar_programs,
        )?
        .into_iter()
        .chain(DirectionalProgram::replaced_outputs(
            primary.map(|primary| &primary.scalar.implicit_projection_scalar_jacobian_v),
            &implicit_projection_scalar_jacobian,
        )?)
        .collect::<BTreeSet<_>>();
        let implicit_scalar_rhs = PrimalProgram::prepare_with_compute(
            "implicit_rhs",
            implicit_scalar_programs.clone(),
            &model.problem.continuous.implicit_rhs,
            execution_backend.as_ref(),
            primary.map(|primary| &primary.scalar.implicit_scalar_rhs),
        )?;
        let implicit_projection_scalar_jacobian_v = DirectionalProgram::prepare(
            "implicit_projection_jacobian_v",
            implicit_projection_scalar_jacobian.clone(),
            execution_backend.as_ref(),
            primary.map(|primary| &primary.scalar.implicit_projection_scalar_jacobian_v),
        )?;
        let implicit_jacobian_v = DirectionalProgram::prepare(
            "implicit_full_jacobian_v",
            implicit_full_jacobian_v.clone(),
            execution_backend.as_ref(),
            primary.map(|primary| &primary.scalar.implicit_jacobian_v),
        )?;
        let manifold = manifold_execution::PreparedManifoldProjection::new(
            model,
            execution_backend.as_deref(),
        )?;
        let derivative_scalar_rhs =
            to_scalar_program_block(&model.problem.continuous.derivative_rhs)?;
        // Scalarization is an evaluator-boundary view of the compact
        // structured map.  Build it once and share it between dependency
        // planning and the prepared runtime adapter.
        let structured_discrete_scalar_rhs =
            to_scalar_program_block(&model.problem.discrete.structured_rhs)?;
        let guarded_assignment_programs = model
            .problem
            .discrete
            .guarded_assignments
            .iter()
            .map(PreparedGuardedAssignmentProgram::new)
            .collect::<Result<Vec<_>, _>>()?;
        let event_transaction_programs = model
            .problem
            .discrete
            .event_transactions
            .iter()
            .map(|program| PreparedEventTransactionProgram::new(program, &model.pure_calls))
            .collect::<Result<Vec<_>, _>>()?;
        let event_transaction_output_scratch = event_transaction_programs
            .iter()
            .map(|program| vec![0.0; program.output_scalar_count()])
            .collect();
        let event_transaction_coverage = PreparedEventTransactionCoverage::new(model);
        let observation_refresh_scalar_rows = model
            .problem
            .discrete
            .observation_refresh
            .iter()
            .enumerate()
            .filter_map(|(row, selected)| selected.then_some(row))
            .collect::<Box<[_]>>();
        let compiled_event_transactions = event_transaction_programs
            .iter()
            .map(|prepared| {
                execution_backend
                    .as_ref()
                    .map(|backend| {
                        required_compiled(
                            "event_transaction",
                            &[],
                            backend.compile_event_transaction(prepared.program()),
                        )
                    })
                    .transpose()
            })
            .collect::<Result<Vec<_>, _>>()?;
        let derivative_scalar = EvaluationPrimal::prepare(
            "derivative_rhs",
            derivative_scalar_rhs.clone(),
            Some(&model.problem.continuous.derivative_rhs),
            execution_backend.as_ref(),
            primary.map(|primary| &primary.scalar.derivative_scalar),
        )?;
        let refresh_owners = &model.problem.continuous.refresh_owners;
        let algebraic_refresh = refresh_owners.algebraic().clone();
        let derivative_refresh = refresh_owners.derivative().clone();
        let root_refresh = refresh_owners.root().clone();
        let event_refresh = refresh_owners.event().clone();
        let clock_event_refresh = refresh_owners.clock_events().to_vec();
        for row in [
            &algebraic_refresh,
            &derivative_refresh,
            &root_refresh,
            &event_refresh,
        ]
        .into_iter()
        .flat_map(|plan| &plan.rows)
        .chain(clock_event_refresh.iter().flat_map(|plan| &plan.rows))
        {
            if !refresh_program_rows.contains_key(&row.source()) {
                return Err(EvalSolveError::InvalidRow {
                    message: "refresh row source does not replay its final scalar projection"
                        .to_string(),
                    span: None,
                });
            }
        }
        let (initial_scalar_residual, initial_continuation) =
            InitialContinuationCoverage::certify_runtime_blocks(
                model,
                &implicit_scalar_programs,
                &algebraic_refresh,
            )?;
        let settled_read_cones = initial_projection::settled_read_cones(
            model,
            &initial_scalar_residual,
            &implicit_scalar_programs,
            &algebraic_refresh.simultaneous_plan,
        )?;
        let projection_demand =
            refresh_projection::projection_jacobian_demand(model, settled_read_cones.as_deref())?;
        let compiled_algebraic_jacobians = match primary {
            Some(primary) => chart_sharing::shared_projection_jacobians(
                primary,
                &continuous_structural,
                &replaced_rows,
            ),
            None => refresh_projection::prepare_projection_jacobians(
                &continuous_structural,
                implicit_scalar_rhs.source(),
                &implicit_projection_scalar_jacobian_v,
                &projection_demand,
            )?,
        };
        trace_refresh_plan(model, "algebraic", &algebraic_refresh);
        trace_refresh_plan(model, "derivative", &derivative_refresh);
        trace_refresh_plan(model, "root", &root_refresh);
        trace_refresh_plan(model, "event", &event_refresh);
        for (clock, plan) in clock_event_refresh.iter().enumerate() {
            trace_refresh_plan(model, &format!("clock-event-{clock}"), plan);
        }
        let mut refresh_plans = vec![
            &algebraic_refresh,
            &derivative_refresh,
            &root_refresh,
            &event_refresh,
        ];
        refresh_plans.extend(clock_event_refresh.iter());
        let static_refresh_parameter_indices = static_refresh_parameter_indices(
            implicit_scalar_rhs.source(),
            refresh_plans,
            &refresh_program_rows,
        );
        let root_refresh_after_derivative = refresh_owners.root_after_derivative().cloned();
        let root_refresh_after_algebraic = refresh_owners.root_after_algebraic().cloned();
        let algebraic_refresh_after_derivative =
            refresh_owners.algebraic_after_derivative().cloned();
        let clock_event_refresh_after_event = refresh_owners.clock_events_after_event().to_vec();
        if clock_event_refresh_after_event.len() != clock_event_refresh.len() {
            return Err(EvalSolveError::InvalidRow {
                message: "clock refresh remainder inventory does not match clock owners"
                    .to_string(),
                span: None,
            });
        }
        trace_reverse_projection_coverage(model, implicit_scalar_rhs.source());
        let visible_rows = model.visible_projection().scalar_rows().map_err(|error| {
            EvalSolveError::InvalidRow {
                message: error.to_string(),
                span: error.span,
            }
        })?;
        let visible_value_plan =
            visible_value_plan(&visible_rows, model.visible_projection.scalar_count());
        let root_condition_plan = root_condition_plan(model);
        let compiled_lifecycle = execution_backend
            .as_ref()
            .map(|backend| {
                CompiledLifecycle::new(
                    &model_owner.execution_source(),
                    &visible_rows,
                    &structured_discrete_scalar_rhs,
                    &guarded_assignment_programs,
                    backend.as_ref(),
                )
            })
            .transpose()?;
        let compiled_root_conditions = compiled_lifecycle
            .as_ref()
            .map(CompiledLifecycle::root_expression);
        let initial_scalar_residual_owner = PrimalProgram::prepare_compact(
            "initial_residual",
            initial_scalar_residual.clone(),
            model.problem.initialization.residual(),
            execution_backend.as_ref(),
            primary.map(|primary| &primary.scalar.initial_scalar_residual),
        )?;
        let initial_scalar_jacobian =
            to_scalar_program_block(&model.artifacts.initialization.residual_jacobian_v)?;
        let initial_residual_jacobian_v = EvaluationDirectional::prepare(
            "initial_residual_jacobian_v",
            initial_scalar_jacobian,
            Some(&model.artifacts.initialization.residual_jacobian_v),
            execution_backend.as_ref(),
            primary.map(|primary| &primary.scalar.initial_residual_jacobian_v),
        )?;
        let delay_runtime = DelayRuntime::new_with_backend(
            &model.problem.events.delays,
            execution_backend.as_deref(),
        )?;
        let root_condition_count =
            total_root_condition_count(model, delay_runtime.event_root_count())?;
        let structured_discrete_rows =
            PreparedStructuredDiscreteRows::new(model, structured_discrete_scalar_rhs)?;
        let clock_partition_structured_rows = discrete_rows::clock_partition_structured_rows(
            model.problem.discrete.structured_updates.len(),
            structured_discrete_rows.rows(),
        );
        let clock_partition_clocks = discrete_rows::clock_partition_clocks(&model.problem.discrete);
        let block_kernels: Rc<[Option<Rc<block_kernels::BlockKernels>>]> =
            block_kernels::block_kernels(
                &block_kernels::BlockKernelSources {
                    owners: &model.problem.continuous.refresh_owners,
                    plan: &model.problem.continuous.algebraic_projection_plan,
                    residual: (implicit_scalar_rhs.source(), &implicit_scalar_programs),
                    jacobian_v: implicit_jacobian_v
                        .is_compiled()
                        .then(|| (implicit_jacobian_v.source(), &implicit_full_jacobian_v)),
                },
                execution_backend
                    .as_deref()
                    .filter(|_| implicit_scalar_rhs.is_compiled()),
            )
            .into();
        let block_splits = block_residual_split::block_residual_splits(
            // A block with a fused residual entry evaluates its whole residual
            // programs in one call and never reads a split.
            |index| {
                model
                    .problem
                    .continuous
                    .refresh_owners
                    .algebraic_projection_block_is_affine(index)
                    && !block_kernels
                        .get(index)
                        .and_then(Option::as_ref)
                        .is_some_and(|kernels| kernels.has_residual())
            },
            &model.problem.continuous.algebraic_projection_plan,
            &continuous_structural,
            implicit_scalar_rhs.source(),
            &implicit_scalar_programs,
            execution_backend.as_deref(),
            primary.map(|primary| {
                block_residual_split::SharedSplits::new(
                    &primary
                        .runtime
                        .model
                        .problem
                        .continuous
                        .algebraic_projection_plan,
                    &primary.scalar.block_splits,
                    &replaced_rows,
                )
            }),
        )
        .into();
        let state_jacobian_memo = state_jacobian_memo::StateJacobianMemo::certify(
            &derivative_refresh.simultaneous_plan,
            implicit_scalar_rhs.source(),
            &derivative_scalar_rhs,
        )
        .map(Rc::new);
        let (compiled_assignment_schedules, interpreted_assignment_schedules) =
            interpreted_schedules::InterpretedSchedules::construct(
                model,
                execution_backend.as_deref(),
            )?;
        let derivative_jacobian_v = EvaluationDirectional::prepare(
            "derivative_jacobian_v",
            model.artifacts.continuous.full_jacobian_v.clone(),
            None,
            execution_backend.as_ref(),
            primary.map(|primary| &primary.scalar.derivative_jacobian_v),
        )?;
        let has_compiled_projection = implicit_projection_scalar_jacobian_v.is_compiled();
        let expressions = match &execution_backend {
            None => PreparedExpressions::Reference,
            Some(backend) => PreparedExpressions::Compiled(Box::new(CompiledExpressions {
                implicit_targets: compiled_targets::TargetQueries::prepare_owned_with_primary(
                    &implicit_scalar_programs,
                    implicit_scalar_rhs.source(),
                    model
                        .problem
                        .continuous
                        .algebraic_projection_plan
                        .blocks
                        .scalar_blocks()
                        .iter()
                        .flat_map(|block| {
                            block.rows.iter().flat_map(|&row| {
                                block.y_indices.iter().map(move |&target| (row, target))
                            })
                        }),
                    backend.as_ref(),
                    RowEvalContext {
                        pure_calls: Some(&model.pure_calls),
                        external_tables: Some(model.external_tables.as_slice()),
                        ..Default::default()
                    },
                    primary.and_then(|primary| primary.scalar.compiled_targets(false)),
                )?,
                initial_targets: compiled_targets::TargetQueries::prepare_owned_with_primary(
                    &initial_scalar_residual,
                    initial_scalar_residual_owner.source(),
                    model
                        .problem
                        .initialization
                        .projection_plan()
                        .blocks
                        .iter()
                        .flat_map(|block| {
                            block.rows.iter().flat_map(|&row| {
                                block.unknowns.iter().filter_map(move |slot| match slot {
                                    solve::ScalarSlot::Y { index, .. } => Some((row, *index)),
                                    _ => None,
                                })
                            })
                        }),
                    backend.as_ref(),
                    RowEvalContext {
                        pure_calls: Some(&model.pure_calls),
                        external_tables: Some(model.external_tables.as_slice()),
                        ..Default::default()
                    },
                    primary.and_then(|primary| primary.scalar.compiled_targets(true)),
                )?,
                initial_update_jacobian: compiled_inventory::InitialUpdateJacobian::new(
                    model.artifacts.initialization.update_jacobian_v.as_ref(),
                    backend.as_ref(),
                )?,
                event_transactions: compiled_event_transactions
                    .into_iter()
                    .map(|entry| require_prepared("event_transaction", entry))
                    .collect::<Result<Box<[_]>, _>>()?,
                lifecycle: require_prepared("lifecycle", compiled_lifecycle)?,
                messages: compiled_messages::CompiledMessagePrograms::new(
                    model_owner.clone(),
                    backend.as_ref(),
                )?,
                root_conditions: require_prepared("root_conditions", compiled_root_conditions)?,
            })),
        };
        let mut scalar = ScalarExecutionInventory {
            coupled_event_jvps: coupled_event::CoupledEventJvps::prepare(
                &model.artifacts.discrete,
                execution_backend.as_ref(),
            )?,
            expressions,
            observation_authority: solve_eval::EventObservationAuthority::default(),
            torn_tangents: torn_tangent_evaluators(
                &model.problem.continuous.algebraic_projection_plan,
                &implicit_projection_scalar_jacobian,
                has_compiled_projection,
            ),
            // A backend-compiled JVP evaluates the colors natively, so the
            // colored lanes are built only for the interpreted runtime.
            colored_tangents: if has_compiled_projection {
                Rc::from(Vec::new())
            } else {
                colored_tangent_evaluators(
                    &model.problem.continuous.algebraic_projection_plan,
                    &continuous_structural,
                )
            },
            block_splits,
            block_kernels,
            active_split: std::cell::Cell::new(None),
            split_row_scratch: std::cell::RefCell::new(Vec::new()),
            residual_row_batch: std::cell::RefCell::default(),
            implicit_projection_scalar_jacobian_v,
            implicit_scalar_rhs,
            refresh_program_rows,
            manifold,
            initial_residual_jacobian_v,
            initial_scalar_residual: initial_scalar_residual_owner,
            compiled_algebraic_jacobians,
            derivative_jacobian_v,
            derivative_scalar,
            implicit_jacobian_v,
            continuous_structural,
            initialization_structural,
            algebraic_newton_caches,
            solver_variable_scales: model.solver_variable_scales().into(),
            state_jacobian_memo,
            seed_projection_cache: RefCell::new(SeedProjectionCache::default()),
            algebraic_refresh,
            settled_read_cones,
            derivative_refresh,
            root_refresh,
            event_refresh,
            root_refresh_after_derivative,
            root_refresh_after_algebraic,
            algebraic_refresh_after_derivative,
            clock_event_refresh_after_event,
            initial_continuation,
            root_condition_rows: prepare(
                |primary| &primary.root_condition_rows,
                model.problem.events.root_conditions.clone(),
            )?,
            event_action_conditions: prepare(
                |primary| &primary.event_action_conditions,
                model.problem.events.action_conditions.clone(),
            )?,
            event_action_active_row_indices: RefCell::new(Vec::new()),
            root_condition_plan,
            discrete_rhs: prepare(
                |primary| &primary.discrete_rhs,
                model.problem.discrete.rhs.clone(),
            )?,
            observation_refresh_scalar_rows,
            observation_refresh_p_scratch: RefCell::new(Vec::new()),
            observation_refresh_values_scratch: RefCell::new(Vec::new()),
            clock_partition_intermediates: prepare(
                |primary| &primary.clock_partition_intermediates,
                model.problem.discrete.clock_partition_intermediates.clone(),
            )?,
            clock_partition_clocks,
            clock_partition_structured_rows,
            clock_partition_work_y: RefCell::new(Vec::new()),
            clock_partition_work_p: RefCell::new(Vec::new()),
            guarded_assignment_programs,
            event_transaction_programs,
            event_transaction_coverage,
            initialization_update_rhs: prepare(
                |primary| &primary.initialization_update_rhs,
                model.problem.initialization.update_rhs().clone(),
            )?,
            dynamic_time_event_rhs: prepare(
                |primary| &primary.dynamic_time_event_rhs,
                model.problem.events.dynamic_time_event_rhs.clone(),
            )?,
            runtime_assignment_rhs: prepare(
                |primary| &primary.runtime_assignment_rhs,
                model.problem.discrete.runtime_assignment_rhs.clone(),
            )?,
            post_commit_assignment_rhs: prepare(
                |primary| &primary.post_commit_assignment_rhs,
                model.problem.discrete.post_commit_assignment_rhs.clone(),
            )?,
            update_values_scratch: RefCell::new(Vec::new()),
            structured_discrete_rows,
            visible_name_index: build_visible_name_index(model),
            visible_value_rows: prepare(|primary| &primary.visible_value_rows, visible_rows)?,
            visible_value_plan,
            visible_scratch: RefCell::new(Vec::new()),
            refresh_snapshot_scratch: RefCell::new(Vec::new()),
            refresh_probe_scratch: RefCell::new(Vec::new()),
            refresh_tensor_scratch: RefCell::new(Vec::new()),
            static_refresh_cache: RefCell::new(StaticRefreshCache::default()),
            static_refresh_parameter_indices,
            parameter_static_gradient_cache: RefCell::new(ParameterStaticGradientCache::default()),
            torn_sweep_cache: TornSweepCache::default(),
            derivative_scratch: RefCell::new(StateDerivativeScratch::default()),
            root_scratch: RefCell::new(Vec::new()),
            reverse_scratch: RefCell::new(solve_eval::reverse::ReverseScratch::default()),
            execution_backend,
            compiled_discrete_rows: RefCell::new(FxHashMap::default()),
            failed_discrete_rows: RefCell::new(BTreeSet::new()),
            compiled_clock_partition_intermediates: RefCell::new(FxHashMap::default()),
            failed_clock_partition_intermediates: RefCell::new(BTreeSet::new()),
            clock_partition_intermediate_scratch: RefCell::new(Vec::new()),
            compiled_guarded_assignments: RefCell::new(FxHashMap::default()),
            failed_guarded_assignments: RefCell::new(BTreeSet::new()),
            event_transaction_input_scratch: RefCell::new(Vec::new()),
            event_transaction_output_scratch: RefCell::new(event_transaction_output_scratch),
            compiled_root_rows: RefCell::new(FxHashMap::default()),
            failed_root_rows: RefCell::new(BTreeSet::new()),
            compiled_visible_rows: RefCell::new(FxHashMap::default()),
            failed_visible_rows: RefCell::new(BTreeSet::new()),
            compiled_event_action_rows: RefCell::new(FxHashMap::default()),
            failed_event_action_rows: RefCell::new(BTreeSet::new()),
            compiled_assignment_schedules,
            interpreted_assignment_schedules,
            native_projection_assignments: Default::default(),
            compiled_output_scratch: RefCell::new(Vec::new()),
        };
        scalar.observation_authority = solve_eval::EventObservationAuthority::derive(
            &model.pure_calls,
            &model.problem.events.actions,
            [
                scalar.root_condition_rows.block(),
                scalar.event_action_conditions.block(),
            ],
        )?;
        let runtime = Self::with_inventory(
            model_owner,
            RuntimeExecutionInventory::Scalar(Box::new(scalar)),
            delay_runtime,
            root_condition_count,
        );
        runtime.scalar_evaluation()?.prepare_torn_sweeps();
        Ok(runtime)
    }
}
