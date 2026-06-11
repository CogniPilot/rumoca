//! Lower DAE data into solver-facing IR.
//!
//! Lowering passes (`layout`, `lower`, `ad`) take a `dae::Dae` and produce
//! `ir-solve` row IR: variable layout, residual rows, Jacobian-vector rows,
//! discrete RHS, and root conditions. Concrete execution adapters live in
//! `rumoca-exec-*` crates.
//!
//! The DAE tree-walk interpreter (`eval`, `dual`, `sim_float`, `statement`) lives
//! in `rumoca-eval-dae`.

use std::collections::{BTreeMap, BTreeSet, HashSet};

use indexmap::IndexMap;

use rumoca_core::ExpressionVisitor;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::{BltBlock, EquationRef, Incidence, UnknownId};

// DAE function-call validation (compile-time preflight).
pub mod function_validation;

// Lowering passes (DAE → ir-solve rows).
pub mod ad;
mod appendix_b_validation;
mod discrete_pre_modes;
mod dummy_derivative;
mod dynamic_events;
mod event_actions;
mod initial_values;
pub mod layout;
pub mod lower;
mod observation_refresh;
mod path_utils;
mod projection_suffix;
mod runtime_assignments;
pub mod solve_model;
#[cfg(test)]
#[path = "tests/test_support.rs"]
mod test_support;

pub use ad::{
    lower_compute_block_jvp, lower_initial_residual_ad, lower_initial_residual_full_ad,
    lower_residual_ad, lower_residual_full_ad, lower_scalar_program_block_ad,
    lower_scalar_program_block_full_ad,
};
use discrete_pre_modes::discrete_pre_mode_for_equation;
#[cfg(test)]
pub(crate) use discrete_pre_modes::expression_contains_event_entry_pre_operator;
use layout::INITIAL_EVENT_PARAMETER_NAME;
pub use layout::{build_var_layout, build_var_layout_with_solver_len};
pub use lower::LowerError;
use lower::{
    lower_discrete_rhs, lower_initial_residual, lower_initial_update_rhs,
    lower_residual_rows_and_targets_from_equations, lower_root_conditions,
};
use lower::{
    lower_dynamic_time_event_rhs, lower_runtime_assignment_rhs,
    normalized_discrete_update_equations,
};
use observation_refresh::lower_discrete_observation_refresh;
use runtime_assignments::{
    lower_runtime_assignment_targets, runtime_assignment_equation, runtime_assignment_equations,
    runtime_tail_update_names, static_runtime_tail_equation,
};
pub use solve_model::{
    SolveModelLowerError, VisibleExpression, lower_dae_to_solve_model,
    lower_dae_to_solve_model_owned, lower_dae_to_solve_model_owned_with_visible_expressions,
    lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata,
    visible_expressions_for_dae,
};
type LinearOpRows = Vec<Vec<solve::LinearOp>>;
type ImplicitRowsAndTargets = (LinearOpRows, Vec<Option<solve::ScalarSlot>>);

/// Reset DAE evaluator state used while lowering DAE into Solve IR.
///
/// Solve lowering now creates and threads an explicit `EvalRuntimeState` for
/// each lowering request, so there is no process-global state to clear here.
pub fn clear_solve_lowering_runtime_state() {}

pub fn lower_solve_layout(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Result<solve::SolveLayout, LowerError> {
    let layout = build_var_layout_with_solver_len(dae_model, solver_len)?;
    Ok(lower_solve_layout_with_var_layout(
        dae_model, solver_len, &layout,
    ))
}

fn lower_solve_layout_with_var_layout(
    dae_model: &dae::Dae,
    solver_len: usize,
    layout: &solve::VarLayout,
) -> solve::SolveLayout {
    let state_scalar_count = scalar_count(dae_model.variables.states.values()).min(solver_len);
    let algebraic_scalar_count = scalar_count(dae_model.variables.algebraics.values())
        .min(solver_len.saturating_sub(state_scalar_count));
    let output_scalar_count = scalar_count(dae_model.variables.outputs.values()).min(
        solver_len
            .saturating_sub(state_scalar_count)
            .saturating_sub(algebraic_scalar_count),
    );
    let parameter_count = scalar_count(dae_model.variables.parameters.values());
    let input_scalar_names = collect_scalar_names(dae_model.variables.inputs.iter());
    let discrete_real_scalar_names =
        collect_scalar_names(dae_model.variables.discrete_reals.iter());
    let discrete_valued_scalar_names =
        collect_scalar_names(dae_model.variables.discrete_valued.iter());
    let compiled_parameter_len = layout.p_scalars();
    let initial_event_parameter_index = match layout.binding(INITIAL_EVENT_PARAMETER_NAME) {
        Some(solve::ScalarSlot::P { index, .. }) => Some(index),
        _ => None,
    };

    solve::SolveLayout {
        solver_maps: build_solver_name_index_maps(dae_model, solver_len),
        state_scalar_count,
        algebraic_scalar_count,
        output_scalar_count,
        parameter_count,
        compiled_parameter_len,
        input_scalar_names,
        discrete_real_scalar_names,
        discrete_valued_scalar_names,
        // MLS Appendix B B.1d condition memory is lowered as ordinary
        // solve-IR discrete update rows from `f_c`. Root rows only detect
        // crossings; they are not the authoritative condition-memory update.
        relation_memory_parameter_indices: Vec::new(),
        // MLS §8.6: `initial()` is true during initialization and false for
        // ordinary event/sampling evaluation. Store the phase flag as a
        // backend-neutral solve-IR runtime parameter so all row renderers read
        // the same lowered representation.
        initial_event_parameter_index,
        pre_param_bindings: build_pre_param_bindings(layout),
    }
}

fn build_pre_param_bindings(layout: &solve::VarLayout) -> Vec<solve::PreParamBinding> {
    let mut bindings = Vec::new();
    for (name, &slot) in layout.bindings() {
        let Some(source_name) = name.strip_prefix("__pre__.") else {
            continue;
        };
        let solve::ScalarSlot::P {
            index: dest_p_index,
            ..
        } = slot
        else {
            continue;
        };
        let source = match layout.binding(source_name) {
            Some(solve::ScalarSlot::Y { index, .. }) => solve::PreParamSource::Y { index },
            Some(solve::ScalarSlot::P { index, .. }) => solve::PreParamSource::P { index },
            _ => continue,
        };
        bindings.push(solve::PreParamBinding {
            dest_p_index,
            source,
        });
    }
    bindings
}

pub fn lower_solve_problem(dae_model: &dae::Dae) -> Result<solve::SolveProblem, LowerError> {
    lower_solve_problem_with_solver_len(dae_model, usize::MAX)
}

// SPEC_0021: Exception - top-level Solve-IR lowering entry point assembles the
// whole SolveProblem so stage contracts are visible at the phase boundary.
#[allow(clippy::too_many_lines)]
pub fn lower_solve_problem_with_solver_len(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Result<solve::SolveProblem, LowerError> {
    if ir_boundary_validation_enabled() {
        dae_model
            .validate_shape_contract()
            .map_err(|err| LowerError::ContractViolation {
                span: err.span(),
                reason: format!("invalid DAE IR shape contract: {err:?}"),
            })?;
    }
    appendix_b_validation::validate_solve_input_appendix_b_invariants(dae_model)?;
    // Eliminate dummy derivatives (`di = der(x)`) by substituting `der(x) -> di`
    // in all non-defining equations, so `di` is determined as an algebraic
    // unknown and `der(x) = di` is the trivial state-derivative link (matching
    // OpenModelica). Without this the implicit Newton system is structurally
    // singular for index-reduced models (e.g. mutually-coupled inductors).
    let dummy_eliminated = dummy_derivative::eliminate_dummy_derivatives(dae_model);
    let dae_model = dummy_eliminated.as_ref().unwrap_or(dae_model);
    // TODO(solve-ir): add a backend-neutral `SolveProblem -> SolveProblem`
    // scalarization pass here for vector-only solver renderers. The existing
    // `phase_structural::scalarize` pass intentionally remains DAE-to-DAE so
    // DAE templates can request scalarized equation form before rendering.
    let layout = build_var_layout_with_solver_len(dae_model, solver_len)?;
    let solver_len = layout.y_scalars();
    let solve_layout = lower_solve_layout_with_var_layout(dae_model, solver_len, &layout);

    let runtime_tail_updates = runtime_tail_update_names(dae_model)?;
    let runtime_assignment_equations =
        runtime_assignment_equations(dae_model, &runtime_tail_updates)?;
    let derivative_analysis = lower::analyze_derivative_rhs(dae_model)
        .map_err(|err| lower_problem_context(err, "analyze derivative RHS rows"))?;
    let state_derivative_rows = derivative_analysis.equation_flags().to_vec();
    let residual_equations =
        solver_residual_equations(dae_model, &runtime_tail_updates, &state_derivative_rows);
    // `solver_residual_equations` has already removed state-derivative rows.
    // The remaining original DAE indices are not a state-row prefix, so residual
    // lowering must not infer derivative-row behavior from `row_idx < n_x`.
    let (residual, residual_targets) = lower_residual_rows_and_targets_from_equations(
        dae_model,
        &layout,
        residual_equations.iter().copied(),
        0,
        |eq, row_count| {
            lower_continuous_row_targets_for_equation(dae_model, eq, &layout, row_count)
        },
    )
    .map_err(|err| lower_problem_context(err, "lower continuous residual rows and targets"))?;
    let derivative_rhs =
        lower::lower_derivative_rhs_with_analysis(dae_model, &layout, &derivative_analysis)
            .map_err(|err| lower_problem_context(err, "lower derivative RHS rows"))?;
    let derivative_rhs_scalar =
        rumoca_eval_solve::to_scalar_program_block(&derivative_rhs).programs;
    let (implicit_rhs, implicit_row_targets) = build_implicit_rhs_rows(
        &derivative_rhs_scalar,
        &residual,
        &residual_targets,
        solve_layout.state_scalar_count(),
        solve_layout.solver_scalar_count(),
    )?;
    let algebraic_projection_plan = lower_algebraic_projection_plan(
        &implicit_rhs,
        &implicit_row_targets,
        solve_layout.state_scalar_count(),
        solve_layout.solver_scalar_count(),
    )?;
    let runtime_assignment_targets =
        lower_runtime_assignment_targets(dae_model, &runtime_assignment_equations, &layout)?;
    let discrete_observation_refresh =
        lower_discrete_observation_refresh(dae_model, &layout, &runtime_assignment_targets)?;
    let initialization = lower_initialization_system(dae_model, &layout, &solve_layout)?;
    let dynamic_time_event_exprs = dynamic_events::collect_dynamic_time_event_exprs(dae_model);
    let implicit_rhs = build_implicit_rhs_compute_block(
        &derivative_rhs,
        implicit_rhs,
        solve_layout.state_scalar_count(),
    );
    let problem = solve::SolveProblem {
        schema_version: solve::SOLVE_SCHEMA_VERSION,
        continuous: solve::ContinuousSolveSystem {
            implicit_row_targets,
            implicit_rhs,
            algebraic_projection_plan,
            residual: solve::ScalarProgramBlock::with_program_spans(
                residual.clone(),
                program_spans_for_equations(&residual_equations),
            ),
            derivative_rhs,
        },
        initialization,
        discrete: solve::DiscreteSolveSystem {
            runtime_assignment_rhs: solve::ScalarProgramBlock::new(
                lower_runtime_assignment_rhs(dae_model, &layout, &runtime_assignment_equations)
                    .map_err(|err| lower_problem_context(err, "lower runtime assignment rows"))?,
            ),
            runtime_assignment_targets,
            rhs: solve::ScalarProgramBlock::new(
                lower_discrete_rhs(dae_model, &layout)
                    .map_err(|err| lower_problem_context(err, "lower discrete update rows"))?,
            ),
            update_targets: lower_discrete_update_targets(dae_model, &layout)
                .map_err(|err| lower_problem_context(err, "lower discrete update targets"))?,
            pre_modes: lower_discrete_pre_modes(dae_model)
                .map_err(|err| lower_problem_context(err, "lower discrete pre modes"))?,
            observation_refresh: discrete_observation_refresh,
        },
        events: solve::SolveEventPartition {
            root_conditions: solve::ScalarProgramBlock::new(
                lower_root_conditions(dae_model, &layout)
                    .map_err(|err| lower_problem_context(err, "lower root-condition rows"))?,
            ),
            root_relation_memory_targets: lower::lower_root_relation_memory_targets(
                dae_model, &layout,
            )
            .map_err(|err| lower_problem_context(err, "lower root relation memory targets"))?,
            scheduled_time_events: dae_model.events.scheduled_time_events.clone(),
            dynamic_time_event_names: dynamic_events::collect_dynamic_time_event_names(dae_model),
            dynamic_time_event_rhs: solve::ScalarProgramBlock::new(
                lower_dynamic_time_event_rhs(dae_model, &layout, &dynamic_time_event_exprs)
                    .map_err(|err| lower_problem_context(err, "lower dynamic time event rows"))?,
            ),
            action_conditions: solve::ScalarProgramBlock::with_program_spans(
                event_actions::lower_event_action_conditions(dae_model, &layout)
                    .map_err(|err| lower_problem_context(err, "lower event action rows"))?,
                dae_model
                    .events
                    .event_actions
                    .iter()
                    .map(|action| action.span)
                    .collect(),
            ),
            actions: event_actions::lower_event_actions(dae_model, &layout)
                .map_err(|err| lower_problem_context(err, "lower event actions"))?,
        },
        clocks: solve::SolveClockPartition {
            periodic_event_schedules: lower_periodic_event_schedules(dae_model),
        },
        solve_layout,
        layout,
    };

    appendix_b_validation::validate_solve_problem_appendix_b_invariants(&problem)?;
    if ir_boundary_validation_enabled() {
        problem
            .validate_shape_contract()
            .map_err(|err| LowerError::ContractViolation {
                span: err.span(),
                reason: format!("invalid Solve IR shape contract: {err:?}"),
            })?;
    }
    Ok(problem)
}

fn ir_boundary_validation_enabled() -> bool {
    cfg!(any(
        debug_assertions,
        test,
        feature = "strict-ir-validation"
    ))
}

fn program_spans_for_equations(equations: &[(usize, &dae::Equation)]) -> Vec<rumoca_core::Span> {
    equations
        .iter()
        .flat_map(|(_, eq)| std::iter::repeat_n(eq.span, eq.scalar_count.max(1)))
        .collect()
}

fn lower_initialization_system(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    solve_layout: &solve::SolveLayout,
) -> Result<solve::InitializationSolveSystem, LowerError> {
    let residual_equations = lower::initial_residual_equations(dae_model, layout)
        .map_err(|err| lower_problem_context(err, "collect initial residual equations"))?;
    let row_targets =
        lower_continuous_row_targets(dae_model, residual_equations.iter().copied(), layout)
            .map_err(|err| lower_problem_context(err, "lower initial row targets"))?;
    let update_equations = lower::initial_condition_update_equations(dae_model);
    let update_targets = lower_update_targets_from_equations(dae_model, layout, &update_equations)
        .map_err(|err| lower_problem_context(err, "lower initial update targets"))?;

    let residual_rows = lower_initial_residual(dae_model, layout)
        .map_err(|err| lower_problem_context(err, "lower initial residual rows"))?;
    let projection_indices = initial_projection_indices_for_layout(dae_model, solve_layout);
    let projection_plan = lower_projection_plan(
        &residual_rows,
        &row_targets,
        &projection_indices,
        0..residual_rows.len(),
    )?;

    Ok(solve::InitializationSolveSystem {
        row_targets,
        projection_indices,
        projection_plan,
        residual: solve::ScalarProgramBlock::with_program_spans(
            residual_rows,
            program_spans_for_equations(&residual_equations),
        ),
        update_rhs: solve::ScalarProgramBlock::new(
            lower_initial_update_rhs(dae_model, layout)
                .map_err(|err| lower_problem_context(err, "lower initial update rows"))?,
        ),
        update_targets,
    })
}

fn initial_projection_indices_for_layout(
    dae_model: &dae::Dae,
    solve_layout: &solve::SolveLayout,
) -> Vec<usize> {
    let state_count = solve_layout.state_scalar_count();
    let algebraic_end = state_count + solve_layout.algebraic_scalar_count();
    let mut indices = dae_model
        .variables
        .states
        .iter()
        .filter(|(_, var)| var.fixed != Some(true))
        .flat_map(|(name, var)| collect_scalar_names(std::iter::once((name, var))))
        .filter_map(|name| solve_layout.solver_idx_for_target(&name))
        .collect::<Vec<_>>();
    indices.extend(state_count..algebraic_end);
    indices.sort_unstable();
    indices.dedup();
    indices
}

pub fn lower_solve_artifacts(
    problem: &solve::SolveProblem,
) -> Result<solve::SolveArtifacts, LowerError> {
    lower_solve_artifacts_with_mass_matrix(problem, solve_identity_mass_matrix(problem))
}

pub fn lower_solve_artifacts_with_mass_matrix(
    problem: &solve::SolveProblem,
    mass_matrix: Vec<Vec<f64>>,
) -> Result<solve::SolveArtifacts, LowerError> {
    let artifacts = solve::SolveArtifacts {
        continuous: lower_continuous_solve_artifacts(problem, mass_matrix)?,
    };
    appendix_b_validation::validate_solve_artifacts_appendix_b_invariants(&artifacts)?;
    Ok(artifacts)
}

fn lower_continuous_solve_artifacts(
    problem: &solve::SolveProblem,
    mass_matrix: Vec<Vec<f64>>,
) -> Result<solve::ContinuousSolveArtifacts, LowerError> {
    let implicit_jacobian_v = lower_compute_block_jvp(&problem.continuous.implicit_rhs)
        .map_err(|err| lower_problem_context(err, "lower implicit Jacobian rows"))?;
    // Row-aligned scalar JVP of `implicit_rhs`: the state-only path propagates the
    // state seed through the algebraic projection row by row, indexing by the same
    // `row_idx` as the scalarized value residual. The tensor `implicit_jacobian_v`
    // above is not row-aligned once linear (`LinSolve`/`MatMul`) blocks appear, so
    // we lower a dedicated scalarized variant here (mirroring `full_jacobian_v`).
    let implicit_rhs_rows =
        rumoca_eval_solve::to_scalar_program_block(&problem.continuous.implicit_rhs);
    let implicit_jacobian_v_scalar = solve::ScalarProgramBlock::new(
        lower_scalar_program_block_full_ad(&implicit_rhs_rows.programs, &problem.layout)
            .map_err(|err| lower_problem_context(err, "lower scalar implicit Jacobian rows"))?,
    );
    let derivative_rhs_rows =
        rumoca_eval_solve::to_scalar_program_block(&problem.continuous.derivative_rhs);
    let full_jacobian_v = solve::ScalarProgramBlock::new(
        lower_scalar_program_block_full_ad(&derivative_rhs_rows.programs, &problem.layout)
            .map_err(|err| lower_problem_context(err, "lower derivative Jacobian rows"))?,
    );

    Ok(solve::ContinuousSolveArtifacts {
        mass_matrix,
        implicit_jacobian_v,
        implicit_jacobian_v_scalar,
        full_jacobian_v,
    })
}

pub fn solve_identity_mass_matrix(problem: &solve::SolveProblem) -> Vec<Vec<f64>> {
    let state_count = problem.solve_layout.state_scalar_count();
    let mut matrix = vec![vec![0.0; state_count]; state_count];
    for (idx, row) in matrix.iter_mut().enumerate() {
        row[idx] = 1.0;
    }
    matrix
}

fn lower_periodic_event_schedules(dae_model: &dae::Dae) -> Vec<solve::PeriodicEventSchedule> {
    dae_model
        .clocks
        .schedules
        .iter()
        .map(|schedule| solve::PeriodicEventSchedule {
            period_seconds: schedule.period_seconds,
            phase_seconds: schedule.phase_seconds,
        })
        .collect()
}

fn lower_problem_context(err: LowerError, context: &str) -> LowerError {
    match err {
        // A contract violation's message is already precise; adding lowering
        // context buries the invariant that was broken.
        err @ LowerError::ContractViolation { .. } => err,
        // Keeps its identity so the outermost projection boundary can still
        // recover it as a decline.
        err @ LowerError::ProjectionBudgetExceeded { .. } => err,
        // `with_context` preserves every variant's typed identity, so no
        // error needs to be re-encoded as a reason string here.
        err => err.with_context(context),
    }
}

fn solver_residual_equations<'a>(
    dae_model: &'a dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    state_derivative_rows: &[bool],
) -> Vec<(usize, &'a dae::Equation)> {
    dae_model
        .continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(row_idx, eq)| {
            debug_assert!(
                *row_idx < state_derivative_rows.len(),
                "row_idx {row_idx} out of bounds (state_derivative_rows len={}): \
                 equation count mismatch",
                state_derivative_rows.len()
            );
            solver_residual_equation(
                dae_model,
                runtime_tail_updates,
                state_derivative_rows[*row_idx],
                eq,
            )
        })
        .collect()
}

fn solver_residual_equation(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    is_state_derivative_row: bool,
    eq: &dae::Equation,
) -> bool {
    // MLS Appendix B B.1a: continuous equations are an unordered implicit set.
    // Solve-IR separates state derivative rows from algebraic residual rows by
    // equation structure, not by their source order in DAE `f_x`.
    !is_state_derivative_row
        && !static_runtime_tail_equation(dae_model, runtime_tail_updates, eq)
        && runtime_assignment_equation(dae_model, runtime_tail_updates, eq).is_none()
}

fn build_implicit_rhs_rows(
    derivative_rhs: &[Vec<solve::LinearOp>],
    residual: &[Vec<solve::LinearOp>],
    residual_targets: &[Option<solve::ScalarSlot>],
    state_scalar_count: usize,
    solver_scalar_count: usize,
) -> Result<ImplicitRowsAndTargets, LowerError> {
    let mut rows = vec![zero_rhs_row(); solver_scalar_count];
    let mut row_targets = vec![None; solver_scalar_count];
    let mut occupied = vec![false; solver_scalar_count];
    for (idx, target_row) in rows.iter_mut().enumerate().take(state_scalar_count) {
        let Some(row) = derivative_rhs.get(idx) else {
            return Err(LowerError::Unsupported {
                reason: format!("missing state derivative RHS row {idx} for solve layout"),
            });
        };
        *target_row = row.clone();
        row_targets[idx] = Some(solve::scalar_slot_y(idx));
        occupied[idx] = true;
    }
    place_targeted_residual_rows(
        &mut rows,
        &mut row_targets,
        &mut occupied,
        residual,
        residual_targets,
        state_scalar_count,
        solver_scalar_count,
    );
    Ok((rows, row_targets))
}

fn build_implicit_rhs_compute_block(
    derivative_rhs: &solve::ComputeBlock,
    scalar_programs: Vec<Vec<solve::LinearOp>>,
    state_scalar_count: usize,
) -> solve::ComputeBlock {
    if derivative_rhs.len() != state_scalar_count || state_scalar_count > scalar_programs.len() {
        return solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(
            scalar_programs,
        ));
    }
    let mut nodes = derivative_rhs.nodes.clone();
    let tail_rows = scalar_programs
        .into_iter()
        .skip(state_scalar_count)
        .collect::<Vec<_>>();
    if !tail_rows.is_empty() {
        nodes.push(solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::new(tail_rows),
        ));
    }
    solve::ComputeBlock { nodes }
}

fn lower_algebraic_projection_plan(
    rows: &[Vec<solve::LinearOp>],
    row_targets: &[Option<solve::ScalarSlot>],
    state_scalar_count: usize,
    solver_scalar_count: usize,
) -> Result<solve::AlgebraicProjectionPlan, LowerError> {
    let projection_indices = (state_scalar_count..solver_scalar_count).collect::<Vec<_>>();
    lower_projection_plan(
        rows,
        row_targets,
        &projection_indices,
        state_scalar_count..solver_scalar_count,
    )
}

fn lower_projection_plan(
    rows: &[Vec<solve::LinearOp>],
    row_targets: &[Option<solve::ScalarSlot>],
    projection_indices: &[usize],
    row_indices: std::ops::Range<usize>,
) -> Result<solve::AlgebraicProjectionPlan, LowerError> {
    let mut row_to_vars = BTreeMap::<usize, BTreeSet<usize>>::new();
    let projection_set = projection_indices.iter().copied().collect::<BTreeSet<_>>();

    for row_idx in row_indices {
        let mut y_indices =
            collect_algebraic_y_indices_for_row(rows[row_idx].as_slice(), &projection_set);
        if y_indices.is_empty()
            && let Some(solve::ScalarSlot::Y { index, .. }) =
                row_targets.get(row_idx).copied().flatten()
            && projection_set.contains(&index)
        {
            y_indices.insert(index);
        }
        if y_indices.is_empty() {
            continue;
        }
        row_to_vars.insert(row_idx, y_indices);
    }

    let projection_incidence = algebraic_projection_incidence(&row_to_vars);
    let blocks = projection_blt_blocks(&projection_incidence)?;
    Ok(solve::AlgebraicProjectionPlan {
        blocks: lower_blt_projection_blocks(&blocks, row_targets, &projection_incidence),
    })
}

fn projection_blt_blocks(
    projection_incidence: &ProjectionIncidence,
) -> Result<Vec<BltBlock>, LowerError> {
    if projection_incidence.incidence.n_eq == 0 && projection_incidence.incidence.n_var == 0 {
        return Ok(Vec::new());
    }
    let regular =
        rumoca_phase_structural::maximum_regular_subsystem(&projection_incidence.incidence)
            .map_err(|err| LowerError::Unsupported {
                reason: format!("lower algebraic projection BLT: {err}"),
            })?;
    rumoca_phase_structural::build_blt_from_incidence(&regular.incidence).map_err(|err| {
        LowerError::Unsupported {
            reason: format!("lower algebraic projection BLT: {err}"),
        }
    })
}

fn collect_algebraic_y_indices_for_row(
    row: &[solve::LinearOp],
    projection_set: &BTreeSet<usize>,
) -> BTreeSet<usize> {
    let mut defs = BTreeMap::<solve::Reg, RowDefUse>::new();
    let mut outputs = Vec::new();
    for op in row {
        match row_def_use(op) {
            RowDefUseOp::Def { dst, def_use } => {
                defs.insert(dst, def_use);
            }
            RowDefUseOp::Store { src } => outputs.push(src),
        }
    }
    let mut y_indices = BTreeSet::new();
    let mut visited = BTreeSet::new();
    let mut stack = outputs;
    while let Some(reg) = stack.pop() {
        if !visited.insert(reg) {
            continue;
        }
        let Some(def_use) = defs.get(&reg) else {
            continue;
        };
        if let Some(index) = def_use.loaded_y
            && projection_set.contains(&index)
        {
            y_indices.insert(index);
        }
        stack.extend(def_use.inputs.iter().copied());
    }
    y_indices
}

#[derive(Debug)]
struct RowDefUse {
    loaded_y: Option<usize>,
    inputs: Vec<solve::Reg>,
}

enum RowDefUseOp {
    Def { dst: solve::Reg, def_use: RowDefUse },
    Store { src: solve::Reg },
}

fn row_def_use(op: &solve::LinearOp) -> RowDefUseOp {
    use solve::LinearOp as Op;
    match *op {
        Op::Const { dst, .. } | Op::LoadTime { dst } | Op::LoadP { dst, .. } => {
            def_use(dst, None, Vec::new())
        }
        Op::LoadY { dst, index } => def_use(dst, Some(index), Vec::new()),
        Op::LoadSeed { dst, .. } => def_use(dst, None, Vec::new()),
        Op::Move { dst, src } | Op::Unary { dst, arg: src, .. } => def_use(dst, None, vec![src]),
        Op::Binary { dst, lhs, rhs, .. } | Op::Compare { dst, lhs, rhs, .. } => {
            def_use(dst, None, vec![lhs, rhs])
        }
        Op::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => def_use(dst, None, vec![cond, if_true, if_false]),
        Op::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            ..
        } => def_use(
            dst,
            None,
            reg_range(matrix_start, n * n)
                .chain(reg_range(rhs_start, n))
                .collect(),
        ),
        Op::TableBounds { dst, table_id, .. } => def_use(dst, None, vec![table_id]),
        Op::TableLookup {
            dst,
            table_id,
            column,
            input,
        }
        | Op::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => def_use(dst, None, vec![table_id, column, input]),
        Op::TableNextEvent {
            dst,
            table_id,
            time,
        } => def_use(dst, None, vec![table_id, time]),
        Op::RandomInitialState {
            dst,
            local_seed,
            global_seed,
            ..
        } => def_use(dst, None, vec![local_seed, global_seed]),
        Op::RandomResult {
            dst,
            state_start,
            state_len,
            ..
        }
        | Op::RandomState {
            dst,
            state_start,
            state_len,
            ..
        } => def_use(dst, None, reg_range(state_start, state_len).collect()),
        Op::ImpureRandomInit { dst, seed } => def_use(dst, None, vec![seed]),
        Op::ImpureRandom { dst, id, .. } => def_use(dst, None, vec![id]),
        Op::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            ..
        } => def_use(dst, None, vec![id, imin, imax]),
        Op::StoreOutput { src } => RowDefUseOp::Store { src },
    }
}

fn def_use(dst: solve::Reg, loaded_y: Option<usize>, inputs: Vec<solve::Reg>) -> RowDefUseOp {
    RowDefUseOp::Def {
        dst,
        def_use: RowDefUse { loaded_y, inputs },
    }
}

fn reg_range(start: solve::Reg, len: usize) -> impl Iterator<Item = solve::Reg> {
    (0..len).filter_map(move |offset| start.checked_add(offset.try_into().ok()?))
}

struct ProjectionIncidence {
    incidence: Incidence,
    unknown_y_indices: Vec<usize>,
}

fn algebraic_projection_incidence(
    row_to_vars: &BTreeMap<usize, BTreeSet<usize>>,
) -> ProjectionIncidence {
    let unknown_y_indices = row_to_vars
        .values()
        .flat_map(|vars| vars.iter().copied())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    let unknown_names = unknown_y_indices
        .iter()
        .copied()
        .map(projection_unknown_id)
        .collect::<Vec<_>>();
    let unknown_positions = unknown_y_indices
        .iter()
        .copied()
        .enumerate()
        .map(|(local_idx, y_idx)| (y_idx, local_idx))
        .collect::<BTreeMap<_, _>>();
    let equation_refs = row_to_vars
        .keys()
        .copied()
        .map(EquationRef)
        .collect::<Vec<_>>();
    let eq_unknowns = row_to_vars
        .values()
        .map(|vars| {
            vars.iter()
                .filter_map(|y_idx| unknown_positions.get(y_idx).copied())
                .collect::<HashSet<_>>()
        })
        .collect::<Vec<_>>();

    ProjectionIncidence {
        incidence: Incidence::new(eq_unknowns, equation_refs, unknown_names),
        unknown_y_indices,
    }
}

fn projection_unknown_id(y_idx: usize) -> UnknownId {
    UnknownId::SolverY(y_idx)
}

fn projection_y_index(
    unknown: &UnknownId,
    projection_incidence: &ProjectionIncidence,
) -> Option<usize> {
    projection_incidence
        .incidence
        .unknown_names
        .iter()
        .position(|candidate| candidate == unknown)
        .and_then(|idx| projection_incidence.unknown_y_indices.get(idx).copied())
}

fn lower_blt_projection_blocks(
    blocks: &[BltBlock],
    row_targets: &[Option<solve::ScalarSlot>],
    projection_incidence: &ProjectionIncidence,
) -> Vec<solve::AlgebraicProjectionBlock> {
    let blocks = blocks
        .iter()
        .filter_map(|block| match block {
            BltBlock::Scalar { equation, unknown } => {
                let y_index = projection_y_index(unknown, projection_incidence)?;
                Some(solve::AlgebraicProjectionBlock {
                    rows: vec![equation.0],
                    y_indices: vec![y_index],
                    causal_steps: Vec::new(),
                })
            }
            BltBlock::AlgebraicLoop {
                equations,
                unknowns,
            } => lower_algebraic_loop_projection_block(
                equations,
                unknowns,
                row_targets,
                projection_incidence,
            ),
        })
        .collect::<Vec<_>>();
    merge_overlapping_projection_blocks(blocks)
}

fn merge_overlapping_projection_blocks(
    blocks: Vec<solve::AlgebraicProjectionBlock>,
) -> Vec<solve::AlgebraicProjectionBlock> {
    let mut merged = Vec::new();
    for block in blocks {
        merge_projection_block(&mut merged, block);
    }
    merged
}

fn merge_projection_block(
    merged: &mut Vec<solve::AlgebraicProjectionBlock>,
    mut block: solve::AlgebraicProjectionBlock,
) {
    let mut idx = 0;
    while idx < merged.len() {
        if projection_blocks_overlap(&merged[idx], &block) {
            let previous = merged.remove(idx);
            block = combine_projection_blocks(previous, block);
            idx = 0;
        } else {
            idx += 1;
        }
    }
    merged.push(block);
}

fn projection_blocks_overlap(
    lhs: &solve::AlgebraicProjectionBlock,
    rhs: &solve::AlgebraicProjectionBlock,
) -> bool {
    lhs.y_indices
        .iter()
        .any(|index| rhs.y_indices.binary_search(index).is_ok())
}

fn combine_projection_blocks(
    lhs: solve::AlgebraicProjectionBlock,
    rhs: solve::AlgebraicProjectionBlock,
) -> solve::AlgebraicProjectionBlock {
    solve::AlgebraicProjectionBlock {
        rows: merge_unique(lhs.rows, rhs.rows),
        y_indices: merge_unique(lhs.y_indices, rhs.y_indices),
        causal_steps: lhs
            .causal_steps
            .into_iter()
            .chain(rhs.causal_steps)
            .collect(),
    }
}

fn merge_unique(mut lhs: Vec<usize>, rhs: Vec<usize>) -> Vec<usize> {
    lhs.extend(rhs);
    lhs.sort_unstable();
    lhs.dedup();
    lhs
}

fn lower_algebraic_loop_projection_block(
    equations: &[EquationRef],
    unknowns: &[UnknownId],
    row_targets: &[Option<solve::ScalarSlot>],
    projection_incidence: &ProjectionIncidence,
) -> Option<solve::AlgebraicProjectionBlock> {
    let rows = equations.iter().map(|eq| eq.0).collect::<Vec<_>>();
    let mut y_indices = unknowns
        .iter()
        .filter_map(|unknown| projection_y_index(unknown, projection_incidence))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<BTreeSet<_>>();
    for row in &rows {
        if let Some(solve::ScalarSlot::Y { index, .. }) = row_targets.get(*row).copied().flatten()
            && projection_incidence.unknown_y_indices.contains(&index)
        {
            y_indices.insert(index);
        }
    }
    let y_indices = y_indices.into_iter().collect::<Vec<_>>();
    if rows.is_empty() || y_indices.is_empty() {
        return None;
    }
    Some(solve::AlgebraicProjectionBlock {
        rows,
        y_indices,
        causal_steps: Vec::new(),
    })
}

fn place_targeted_residual_rows(
    rows: &mut [Vec<solve::LinearOp>],
    row_targets: &mut [Option<solve::ScalarSlot>],
    occupied: &mut [bool],
    residual: &[Vec<solve::LinearOp>],
    residual_targets: &[Option<solve::ScalarSlot>],
    state_scalar_count: usize,
    solver_scalar_count: usize,
) {
    let mut placed = vec![false; residual.len()];
    for (residual_idx, row) in residual.iter().enumerate() {
        let Some(solve::ScalarSlot::Y { index, .. }) =
            residual_targets.get(residual_idx).copied().flatten()
        else {
            continue;
        };
        if index < state_scalar_count || index >= solver_scalar_count || occupied[index] {
            continue;
        }
        rows[index] = row.clone();
        row_targets[index] = residual_targets[residual_idx];
        occupied[index] = true;
        placed[residual_idx] = true;
    }

    let mut fallback_idx = state_scalar_count;
    for (residual_idx, row) in residual.iter().enumerate() {
        if placed[residual_idx] {
            continue;
        }
        let residual_target = residual_targets.get(residual_idx).copied().flatten();
        let fallback_row_target = fallback_residual_row_target(residual_target, state_scalar_count);
        let target_idx =
            next_fallback_implicit_row(occupied, &mut fallback_idx, solver_scalar_count);
        if let Some(target_idx) = target_idx {
            rows[target_idx] = row.clone();
            row_targets[target_idx] = fallback_row_target;
            occupied[target_idx] = true;
        }
    }
}

fn fallback_residual_row_target(
    residual_target: Option<solve::ScalarSlot>,
    state_scalar_count: usize,
) -> Option<solve::ScalarSlot> {
    let Some(solve::ScalarSlot::Y { index, .. }) = residual_target else {
        return None;
    };
    (index < state_scalar_count).then_some(solve::scalar_slot_y(index))
}

fn next_fallback_implicit_row(
    occupied: &[bool],
    fallback_idx: &mut usize,
    solver_scalar_count: usize,
) -> Option<usize> {
    while *fallback_idx < solver_scalar_count {
        let idx = *fallback_idx;
        *fallback_idx += 1;
        if !occupied[idx] {
            return Some(idx);
        }
    }
    None
}

fn zero_rhs_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 0.0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn lower_continuous_row_targets<'a>(
    dae_model: &dae::Dae,
    equations: impl IntoIterator<Item = (usize, &'a dae::Equation)>,
    layout: &solve::VarLayout,
) -> Result<Vec<Option<solve::ScalarSlot>>, LowerError> {
    let equations: Vec<(usize, &dae::Equation)> = equations.into_iter().collect();
    let mut targets = Vec::with_capacity(equations.len());
    for (_, eq) in equations {
        targets.extend(lower_continuous_row_targets_for_equation(
            dae_model,
            eq,
            layout,
            eq.scalar_count.max(1),
        )?);
    }
    Ok(targets)
}

fn lower_continuous_row_targets_for_equation(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    layout: &solve::VarLayout,
    row_count: usize,
) -> Result<Vec<Option<solve::ScalarSlot>>, LowerError> {
    let mut targets = Vec::with_capacity(row_count);
    if let Some(lhs) = eq.lhs.as_ref()
        && let Some(names) = scalarized_record_target_names(lhs.as_str(), layout)
    {
        push_bound_target_slots(layout, names, &mut targets)?;
        return Ok(targets);
    }
    if let Some(names) = scalarized_record_residual_target_names(&eq.rhs, layout) {
        push_bound_target_slots(layout, names, &mut targets)?;
        return Ok(targets);
    }
    for flat_index in 0..row_count {
        let Some(name) = continuous_row_target_name(dae_model, eq, layout, flat_index, row_count)?
        else {
            targets.push(None);
            continue;
        };
        let Some(slot) = layout.binding(name.as_str()) else {
            return Err(LowerError::MissingBinding { name });
        };
        targets.push(Some(slot));
    }
    Ok(targets)
}

fn push_bound_target_slots(
    layout: &solve::VarLayout,
    names: Vec<String>,
    targets: &mut Vec<Option<solve::ScalarSlot>>,
) -> Result<(), LowerError> {
    for name in names {
        let Some(slot) = layout.binding(name.as_str()) else {
            return Err(LowerError::MissingBinding { name });
        };
        targets.push(Some(slot));
    }
    Ok(())
}

fn scalarized_record_target_names(base: &str, layout: &solve::VarLayout) -> Option<Vec<String>> {
    lower::scalarized_record_field_binding_names(base, layout)
}

fn scalarized_record_residual_target_names(
    expr: &rumoca_core::Expression,
    layout: &solve::VarLayout,
) -> Option<Vec<String>> {
    let rumoca_core::Expression::Binary {
        op,
        lhs,
        rhs: _,
        span: _,
    } = expr
    else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    scalarized_record_target_names(name.as_str(), layout)
}

fn continuous_row_target_name(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    layout: &solve::VarLayout,
    flat_index: usize,
    scalar_count: usize,
) -> Result<Option<String>, LowerError> {
    if let Some(lhs) = eq.lhs.as_ref() {
        return continuous_equation_scalar_name(
            dae_model,
            lhs.var_name(),
            flat_index,
            scalar_count,
            eq.span,
        );
    }
    Ok(residual_expression_target_name(
        dae_model,
        layout,
        &eq.rhs,
        flat_index,
        scalar_count,
    ))
}

fn continuous_equation_scalar_name(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<Option<String>, LowerError> {
    if scalar_count <= 1 {
        return Ok(Some(lhs.as_str().to_string()));
    }
    let Some(dims) = continuous_equation_dims(dae_model, lhs) else {
        if continuous_equation_known_non_target_dims(dae_model, lhs).is_some() {
            return Ok(None);
        }
        return Err(LowerError::ContractViolation {
            reason: format!(
                "continuous equation array LHS `{}` must be a known DAE variable",
                lhs.as_str()
            ),
            span,
        });
    };
    Ok(Some(dae::scalar_name_text_for_flat_index(
        lhs.as_str(),
        dims,
        flat_index,
    )))
}

fn continuous_equation_known_non_target_dims<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a [i64]> {
    dae_model
        .variables
        .inputs
        .get(lhs)
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .or_else(|| dae_model.variables.discrete_valued.get(lhs))
        .or_else(|| dae_model.variables.parameters.get(lhs))
        .map(|var| var.dims.as_slice())
}

fn residual_expression_target_name(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Option<String> {
    // MLS §8.3: an equation `a = b` is represented in DAE as the residual
    // `a - b = 0`. Recover the solve-row target from that expression tree,
    // not from rendered source text, so projectors can use direct row targets.
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            ..
        } => target_expr_scalar_name(dae_model, lhs, flat_index, scalar_count),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        } => positive_additive_target_name(dae_model, layout, lhs, flat_index, scalar_count)
            .or_else(|| {
                positive_additive_target_name(dae_model, layout, rhs, flat_index, scalar_count)
            }),
        _ => None,
    }
}

fn positive_additive_target_name(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Option<String> {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        } => positive_additive_target_name(dae_model, layout, lhs, flat_index, scalar_count)
            .or_else(|| {
                positive_additive_target_name(dae_model, layout, rhs, flat_index, scalar_count)
            }),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs,
            rhs,
            ..
        } => multiplicative_target_name(dae_model, layout, lhs, rhs, flat_index, scalar_count),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs,
            rhs,
            ..
        } => divisive_target_name(dae_model, layout, lhs, rhs, flat_index, scalar_count),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus,
            ..
        } => None,
        rumoca_core::Expression::Unary {
            op:
                rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus | rumoca_core::OpUnary::Empty,
            rhs,
            ..
        } => positive_additive_target_name(dae_model, layout, rhs, flat_index, scalar_count),
        _ => target_expr_scalar_name(dae_model, expr, flat_index, scalar_count),
    }
}

fn multiplicative_target_name(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Option<String> {
    let lhs_target = target_expr_y_scalar_name(dae_model, layout, lhs, flat_index, scalar_count);
    let rhs_target = target_expr_y_scalar_name(dae_model, layout, rhs, flat_index, scalar_count);
    match (lhs_target, rhs_target) {
        (Some(name), None)
            if expression_is_solver_y_free(dae_model, layout, rhs, flat_index, scalar_count) =>
        {
            Some(name)
        }
        (None, Some(name))
            if expression_is_solver_y_free(dae_model, layout, lhs, flat_index, scalar_count) =>
        {
            Some(name)
        }
        _ => None,
    }
}

fn divisive_target_name(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Option<String> {
    target_expr_y_scalar_name(dae_model, layout, lhs, flat_index, scalar_count)
        .filter(|_| expression_is_solver_y_free(dae_model, layout, rhs, flat_index, scalar_count))
}

fn target_expr_y_scalar_name(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Option<String> {
    let name = target_expr_scalar_name(dae_model, expr, flat_index, scalar_count)?;
    matches!(
        layout.binding(name.as_str()),
        Some(solve::ScalarSlot::Y { .. })
    )
    .then_some(name)
}

fn expression_is_solver_y_free(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> bool {
    if let Some(name) = target_expr_scalar_name(dae_model, expr, flat_index, scalar_count) {
        return !matches!(
            layout.binding(name.as_str()),
            Some(solve::ScalarSlot::Y { .. }) | None
        );
    }
    match expr {
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expression_is_solver_y_free(dae_model, layout, lhs, flat_index, scalar_count)
                && expression_is_solver_y_free(dae_model, layout, rhs, flat_index, scalar_count)
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            expression_is_solver_y_free(dae_model, layout, rhs, flat_index, scalar_count)
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => args.iter().all(|arg| {
            expression_is_solver_y_free(dae_model, layout, arg, flat_index, scalar_count)
        }),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().all(|(condition, value)| {
                expression_is_solver_y_free(dae_model, layout, condition, flat_index, scalar_count)
                    && expression_is_solver_y_free(
                        dae_model,
                        layout,
                        value,
                        flat_index,
                        scalar_count,
                    )
            }) && expression_is_solver_y_free(
                dae_model,
                layout,
                else_branch,
                flat_index,
                scalar_count,
            )
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => elements.iter().all(|element| {
            expression_is_solver_y_free(dae_model, layout, element, flat_index, scalar_count)
        }),
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            expression_is_solver_y_free(dae_model, layout, start, flat_index, scalar_count)
                && step.as_ref().is_none_or(|step| {
                    expression_is_solver_y_free(dae_model, layout, step, flat_index, scalar_count)
                })
                && expression_is_solver_y_free(dae_model, layout, end, flat_index, scalar_count)
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            expression_is_solver_y_free(dae_model, layout, expr, flat_index, scalar_count)
                && indices.iter().all(|index| {
                    expression_is_solver_y_free(
                        dae_model,
                        layout,
                        &index.range,
                        flat_index,
                        scalar_count,
                    )
                })
                && filter.as_ref().is_none_or(|filter| {
                    expression_is_solver_y_free(dae_model, layout, filter, flat_index, scalar_count)
                })
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            expression_is_solver_y_free(dae_model, layout, base, flat_index, scalar_count)
                && subscripts.iter().all(|subscript| {
                    subscript_expr(subscript).is_none_or(|expr| {
                        expression_is_solver_y_free(
                            dae_model,
                            layout,
                            expr,
                            flat_index,
                            scalar_count,
                        )
                    })
                })
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            expression_is_solver_y_free(dae_model, layout, base, flat_index, scalar_count)
        }
        rumoca_core::Expression::VarRef { subscripts, .. } => subscripts.iter().all(|subscript| {
            subscript_expr(subscript).is_none_or(|expr| {
                expression_is_solver_y_free(dae_model, layout, expr, flat_index, scalar_count)
            })
        }),
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => true,
    }
}

fn subscript_expr(subscript: &rumoca_core::Subscript) -> Option<&rumoca_core::Expression> {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => Some(expr),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => None,
    }
}

fn target_expr_scalar_name(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => var_ref_target_name(dae_model, name, subscripts, flat_index, scalar_count),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                span: rumoca_core::Span::DUMMY,
            } = base.as_ref()
            else {
                return None;
            };
            if !base_subscripts.is_empty() {
                return None;
            }
            var_ref_target_name(dae_model, name, subscripts, flat_index, scalar_count)
        }
        _ => None,
    }
}

fn var_ref_target_name(
    dae_model: &dae::Dae,
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    flat_index: usize,
    scalar_count: usize,
) -> Option<String> {
    if !subscripts.is_empty() {
        if let Some(indices) =
            sliced_target_indices(dae_model, name.var_name(), subscripts, flat_index)
        {
            return Some(dae::format_subscript_key(name.as_str(), &indices));
        }
        let indices = literal_positive_indices(subscripts)?;
        return Some(dae::format_subscript_key(name.as_str(), &indices));
    }
    continuous_equation_scalar_name_if_known(dae_model, name.var_name(), flat_index, scalar_count)
}

fn continuous_equation_scalar_name_if_known(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
) -> Option<String> {
    if scalar_count <= 1 {
        return Some(lhs.as_str().to_string());
    }
    let dims = continuous_equation_dims(dae_model, lhs)?;
    Some(dae::scalar_name_text_for_flat_index(
        lhs.as_str(),
        dims,
        flat_index,
    ))
}

fn sliced_target_indices(
    dae_model: &dae::Dae,
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    flat_index: usize,
) -> Option<Vec<usize>> {
    let dims = continuous_equation_dims(dae_model, name)?;
    if subscripts.len() > dims.len() {
        return None;
    }
    let mut parts = Vec::with_capacity(dims.len());
    let mut selected_dims = Vec::new();
    for (dim_idx, dim) in dims.iter().copied().enumerate() {
        let dim = usize::try_from(dim).ok()?;
        if dim == 0 {
            return None;
        }
        let indices = match subscripts.get(dim_idx) {
            Some(rumoca_core::Subscript::Index { value, .. }) if *value > 0 => {
                vec![*value as usize]
            }
            Some(rumoca_core::Subscript::Expr { expr, .. }) => {
                vec![literal_positive_index_expr(expr)?]
            }
            Some(rumoca_core::Subscript::Colon { .. }) | None => (1..=dim).collect(),
            _ => return None,
        };
        if indices.iter().any(|index| *index == 0 || *index > dim) {
            return None;
        }
        if indices.len() > 1 {
            selected_dims.push(indices.len() as i64);
        }
        parts.push(indices);
    }
    let selected_subscripts = if selected_dims.is_empty() {
        if flat_index == 0 {
            Vec::new()
        } else {
            return None;
        }
    } else {
        dae::flat_index_to_subscripts(&selected_dims, flat_index)?
    };
    let mut selected_iter = selected_subscripts.into_iter();
    let mut indices = Vec::with_capacity(parts.len());
    for part in parts {
        if part.len() == 1 {
            indices.push(part[0]);
        } else {
            let selected = selected_iter.next()?;
            indices.push(part[selected - 1]);
        }
    }
    Some(indices)
}

fn literal_positive_index_expr(expr: &rumoca_core::Expression) -> Option<usize> {
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    else {
        return None;
    };
    (*value > 0).then_some(*value as usize)
}

fn literal_positive_indices(subscripts: &[rumoca_core::Subscript]) -> Option<Vec<usize>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value: index, .. } if *index > 0 => {
                Some(*index as usize)
            }
            _ => None,
        })
        .collect()
}

fn continuous_equation_dims<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a [i64]> {
    dae_model
        .variables
        .states
        .get(lhs)
        .or_else(|| dae_model.variables.algebraics.get(lhs))
        .or_else(|| dae_model.variables.outputs.get(lhs))
        .map(|var| var.dims.as_slice())
}

pub fn solver_vector_names(
    dae_model: &dae::Dae,
    n_total: usize,
) -> Result<Vec<String>, LowerError> {
    Ok(lower_solve_layout(dae_model, n_total)?.solver_maps.names)
}

pub fn build_solver_name_index_maps(
    dae_model: &dae::Dae,
    y_len: usize,
) -> solve::SolverNameIndexMaps {
    let solver_names = collect_solver_names(dae_model, y_len);
    let mut name_to_idx: IndexMap<String, usize> = solver_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();
    insert_solver_name_aliases(dae_model, y_len, &mut name_to_idx);
    let mut base_to_indices: IndexMap<String, Vec<usize>> = IndexMap::new();
    for (idx, name) in solver_names.iter().enumerate() {
        let base = dae::component_base_name(name).unwrap_or_else(|| name.to_string());
        base_to_indices.entry(base).or_default().push(idx);
    }

    solve::SolverNameIndexMaps {
        names: solver_names,
        name_to_idx,
        base_to_indices,
    }
}

fn scalar_count<'a>(vars: impl Iterator<Item = &'a dae::Variable>) -> usize {
    vars.map(dae::Variable::size).sum()
}

fn var_scalar_names(name: &str, var: &dae::Variable) -> Vec<String> {
    let size = var.size();
    if size <= 1 && var.dims.is_empty() {
        return vec![name.to_string()];
    }
    (0..size)
        .map(|idx| dae::scalar_name_text_for_flat_index(name, &var.dims, idx))
        .collect()
}

fn collect_scalar_names<'a>(
    vars: impl Iterator<Item = (&'a rumoca_core::VarName, &'a dae::Variable)>,
) -> Vec<String> {
    vars.flat_map(|(name, var)| var_scalar_names(name.as_str(), var))
        .collect()
}

fn collect_solver_names(dae_model: &dae::Dae, solver_len: usize) -> Vec<String> {
    let mut names = collect_scalar_names(
        dae_model
            .variables
            .states
            .iter()
            .chain(dae_model.variables.algebraics.iter())
            .chain(dae_model.variables.outputs.iter())
            .filter(|(name, _)| !layout::is_runtime_parameter_tail_variable(dae_model, name)),
    );
    names.truncate(solver_len);
    names
}

fn lower_discrete_update_targets(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<Vec<solve::ScalarSlot>, LowerError> {
    let equations = normalized_discrete_update_equations(dae_model)?;
    lower_update_targets_from_equations(dae_model, layout, &equations)
}

fn lower_update_targets_from_equations(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    equations: &[dae::Equation],
) -> Result<Vec<solve::ScalarSlot>, LowerError> {
    let mut targets = Vec::new();
    for eq in equations {
        let Some(lhs) = eq.lhs.as_ref() else {
            return Err(LowerError::Unsupported {
                reason: "discrete update equation is missing a target".to_string(),
            });
        };
        let scalar_count = eq.scalar_count.max(1);
        for flat_index in 0..scalar_count {
            let name = discrete_update_scalar_name(
                dae_model,
                lhs.var_name(),
                flat_index,
                scalar_count,
                eq.span,
            )?;
            let Some(slot) = layout.binding(name.as_str()) else {
                return Err(LowerError::MissingBinding { name });
            };
            targets.push(slot);
        }
    }
    Ok(targets)
}

fn lower_discrete_pre_modes(
    dae_model: &dae::Dae,
) -> Result<Vec<solve::DiscreteEventPreMode>, LowerError> {
    let mut modes = Vec::new();
    for eq in normalized_discrete_update_equations(dae_model)? {
        let scalar_count = eq.scalar_count.max(1);
        let mode = discrete_pre_mode_for_equation(dae_model, &eq);
        modes.extend(std::iter::repeat_n(mode, scalar_count));
    }
    Ok(modes)
}

fn collect_expression_read_slots(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    out: &mut Vec<solve::ScalarSlot>,
) {
    struct ReadSlotCollector<'a, 'out> {
        dae_model: &'a dae::Dae,
        layout: &'a solve::VarLayout,
        out: &'out mut Vec<solve::ScalarSlot>,
    }

    impl ExpressionVisitor for ReadSlotCollector<'_, '_> {
        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
        ) {
            collect_var_ref_read_slots(
                self.dae_model,
                self.layout,
                name.var_name(),
                subscripts,
                self.out,
            );
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }
    }

    let mut collector = ReadSlotCollector {
        dae_model,
        layout,
        out,
    };
    collector.visit_expression(expr);
}

fn collect_var_ref_read_slots(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    out: &mut Vec<solve::ScalarSlot>,
) {
    if let Some(indices) = literal_positive_indices(subscripts) {
        let key = if indices.is_empty() {
            name.as_str().to_string()
        } else {
            dae::format_subscript_key(name.as_str(), &indices)
        };
        if let Some(slot) = layout.binding(key.as_str()) {
            out.push(slot);
        }
        return;
    }
    collect_all_var_slots(dae_model, layout, name, out);
}

fn collect_all_var_slots(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    name: &rumoca_core::VarName,
    out: &mut Vec<solve::ScalarSlot>,
) {
    let Some(var) = variable_by_name(dae_model, name) else {
        if let Some(slot) = layout.binding(name.as_str()) {
            out.push(slot);
        }
        return;
    };
    for idx in 0..var.size().max(1) {
        let key = if var.size() <= 1 && var.dims.is_empty() {
            name.as_str().to_string()
        } else {
            dae::scalar_name_text_for_flat_index(name.as_str(), &var.dims, idx)
        };
        if let Some(slot) = layout.binding(key.as_str()) {
            out.push(slot);
        }
    }
}

fn variable_by_name<'a>(
    dae_model: &'a dae::Dae,
    name: &rumoca_core::VarName,
) -> Option<&'a dae::Variable> {
    dae_model
        .variables
        .states
        .get(name)
        .or_else(|| dae_model.variables.algebraics.get(name))
        .or_else(|| dae_model.variables.outputs.get(name))
        .or_else(|| dae_model.variables.inputs.get(name))
        .or_else(|| dae_model.variables.discrete_reals.get(name))
        .or_else(|| dae_model.variables.discrete_valued.get(name))
        .or_else(|| dae_model.variables.parameters.get(name))
}

fn condition_memory_base_name(dae_model: &dae::Dae) -> Option<String> {
    let lhs = dae_model.conditions.equations.first()?.lhs.as_ref()?;
    dae::component_base_name(lhs.as_str())
}

fn discrete_update_scalar_name(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<String, LowerError> {
    if scalar_count <= 1 {
        return Ok(lhs.as_str().to_string());
    }
    let dims =
        discrete_update_dims(dae_model, lhs).ok_or_else(|| LowerError::ContractViolation {
            reason: format!(
                "discrete update array LHS `{}` must be a known DAE variable",
                lhs.as_str()
            ),
            span,
        })?;
    Ok(dae::scalar_name_text_for_flat_index(
        lhs.as_str(),
        dims,
        flat_index,
    ))
}

fn discrete_update_dims<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a [i64]> {
    dae_model
        .variables
        .states
        .get(lhs)
        .or_else(|| dae_model.variables.algebraics.get(lhs))
        .or_else(|| dae_model.variables.outputs.get(lhs))
        .or_else(|| dae_model.variables.inputs.get(lhs))
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .or_else(|| dae_model.variables.discrete_valued.get(lhs))
        .map(|var| var.dims.as_slice())
}

fn insert_solver_name_aliases(
    dae_model: &dae::Dae,
    solver_len: usize,
    name_to_idx: &mut IndexMap<String, usize>,
) {
    let solver_name_set: HashSet<String> = name_to_idx.keys().cloned().collect();
    let mut offset = 0usize;
    for (name, var) in dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
    {
        if layout::is_runtime_parameter_tail_variable(dae_model, name) {
            continue;
        }
        let size = var.size();
        if size == 0 {
            continue;
        }
        if offset >= solver_len {
            break;
        }

        let visible_size = size.min(solver_len - offset);
        if size > 1
            && first_visible_scalar_name(name.as_str(), var)
                .as_deref()
                .is_some_and(|scalar| solver_name_set.contains(scalar))
        {
            name_to_idx
                .entry(name.as_str().to_string())
                .or_insert(offset);
        }
        for flat_idx in 0..visible_size {
            let canonical_name = if size <= 1 && var.dims.is_empty() {
                name.as_str().to_string()
            } else {
                dae::scalar_name_text_for_flat_index(name.as_str(), &var.dims, flat_idx)
            };
            if !solver_name_set.contains(canonical_name.as_str()) {
                continue;
            }
            name_to_idx
                .entry(canonical_name)
                .or_insert(offset + flat_idx);
        }
        offset += size;
    }
}

fn first_visible_scalar_name(name: &str, var: &dae::Variable) -> Option<String> {
    if var.size() == 0 {
        return None;
    }
    Some(if var.size() <= 1 && var.dims.is_empty() {
        name.to_string()
    } else {
        dae::scalar_name_text_for_flat_index(name, &var.dims, 0)
    })
}

#[cfg(test)]
mod tests;
