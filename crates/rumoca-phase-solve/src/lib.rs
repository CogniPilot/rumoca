//! Lower DAE data into solver-facing IR.
//!
//! Lowering passes (`layout`, `lower`, `ad`) take a `dae::Dae` and produce
//! `ir-solve` row IR: variable layout, residual rows, Jacobian-vector rows,
//! discrete RHS, and root conditions. Concrete execution adapters live in
//! `rumoca-exec-*` crates.
//!
//! The DAE tree-walk interpreter (`eval`, `dual`, `sim_float`, `statement`) lives
//! in `rumoca-eval-dae`.
//!
//! SPEC_0021 file-size exception: phase-solve facade still owns exports,
//! compatibility tests, and lowering integration. split plan: keep moving
//! tests and integration-only helpers into focused modules.

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
mod capacity;
mod continuous_row_targets;
mod discrete_pre_modes;
mod dummy_derivative;
mod dynamic_events;
mod event_actions;
mod implicit_rhs;
mod initial_values;
pub mod layout;
pub mod lower;
mod observation_refresh;
mod path_utils;
mod projection_suffix;
mod residual_compute_block;
mod runtime_assignments;
pub mod solve_model;
mod stencil;
mod subscript_indices;
#[cfg(test)]
#[path = "tests/test_support.rs"]
mod test_support;
#[cfg(test)]
mod tests;

pub use ad::{
    lower_compute_block_jvp, lower_initial_residual_ad, lower_initial_residual_full_ad,
    lower_residual_ad, lower_residual_full_ad, lower_scalar_program_block_ad,
    lower_scalar_program_block_full_ad, lower_scalar_program_block_full_ad_with_spans,
};
pub use capacity::lower_solve_layout;
pub(crate) use capacity::*;
#[cfg(test)]
use continuous_row_targets::{
    continuous_equation_scalar_name, scalarized_record_target_names, target_expr_scalar_name,
};
use continuous_row_targets::{
    lower_continuous_row_targets, lower_continuous_row_targets_for_equation,
};
use discrete_pre_modes::discrete_pre_mode_for_equation;
#[cfg(test)]
pub(crate) use discrete_pre_modes::expression_contains_event_entry_pre_operator;
#[cfg(test)]
use implicit_rhs::zero_rhs_row;
use implicit_rhs::{
    build_implicit_rhs_compute_block, build_implicit_rhs_rows, state_only_implicit_rows_and_targets,
};
use layout::INITIAL_EVENT_PARAMETER_NAME;
pub use layout::{build_var_layout, build_var_layout_with_solver_len};
pub use lower::LowerError;
use lower::{
    lower_discrete_rhs_from_equations, lower_initial_residual, lower_initial_update_rhs,
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
    ParameterOverrideError, SolveModelLowerError, VisibleExpression, lower_dae_to_solve_model,
    lower_dae_to_solve_model_owned,
    lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata,
    lower_dae_to_solve_model_owned_with_visible_expressions,
    lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata,
    propagate_parameter_overrides, visible_expressions_for_dae,
};
pub(crate) use subscript_indices::{checked_literal_positive_indices, subscript_source_span};
/// Reset DAE evaluator state used while lowering DAE into Solve IR.
///
/// Solve lowering now creates and threads an explicit `EvalRuntimeState` for
/// each lowering request, so there is no process-global state to clear here.
pub fn clear_solve_lowering_runtime_state() {}

fn lower_solve_layout_with_var_layout(
    dae_model: &dae::Dae,
    solver_len: usize,
    layout: &solve::VarLayout,
) -> Result<solve::SolveLayout, LowerError> {
    let span = dae_model_span(dae_model)?;
    let state_scalar_count = scalar_count(dae_model.variables.states.values())?.min(solver_len);
    let remaining_after_states = checked_layout_remainder(
        solver_len,
        state_scalar_count,
        "state scalar layout segment",
        span,
    )?;
    let algebraic_scalar_count =
        scalar_count(dae_model.variables.algebraics.values())?.min(remaining_after_states);
    let remaining_after_algebraics = checked_layout_remainder(
        remaining_after_states,
        algebraic_scalar_count,
        "algebraic scalar layout segment",
        span,
    )?;
    let output_scalar_count =
        scalar_count(dae_model.variables.outputs.values())?.min(remaining_after_algebraics);
    let parameter_count = scalar_count(dae_model.variables.parameters.values())?;
    let input_scalar_names = collect_scalar_names(dae_model.variables.inputs.iter())?;
    let discrete_real_scalar_names =
        collect_scalar_names(dae_model.variables.discrete_reals.iter())?;
    let discrete_valued_scalar_names =
        collect_scalar_names(dae_model.variables.discrete_valued.iter())?;
    let compiled_parameter_len = layout.p_scalars();
    let initial_event_parameter_index = match layout.binding(INITIAL_EVENT_PARAMETER_NAME) {
        Some(solve::ScalarSlot::P { index, .. }) => Some(index),
        _ => None,
    };

    Ok(solve::SolveLayout {
        solver_maps: build_solver_name_index_maps(dae_model, solver_len)?,
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
    })
}

fn checked_layout_remainder(
    total: usize,
    consumed: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    total.checked_sub(consumed).ok_or_else(|| {
        lower_contract_violation(
            format!("{context} consumes {consumed} entries from only {total} available"),
            span,
        )
    })
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
    lower_solve_problem_with_solver_len_and_model_span(dae_model, solver_len, None)
}

// SPEC_0021: Exception - implementation for the top-level Solve-IR lowering
// entry point remains one unit so stage contracts are visible at the phase
// boundary.
#[allow(clippy::too_many_lines)]
pub(crate) fn lower_solve_problem_with_solver_len_and_model_span(
    dae_model: &dae::Dae,
    solver_len: usize,
    fallback_model_span: Option<rumoca_core::Span>,
) -> Result<solve::SolveProblem, LowerError> {
    if ir_boundary_validation_enabled() {
        dae_model.validate_shape_contract().map_err(|err| {
            lower_contract_violation(format!("invalid DAE IR shape contract: {err}"), err.span())
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
    if dae_model_has_no_solve_lowering_inputs(dae_model) {
        return Ok(solve::SolveProblem::default());
    }
    let model_span = match fallback_model_span {
        Some(span) => span,
        None => dae_model_span(dae_model)?,
    };
    // TODO(solve-ir): add a backend-neutral `SolveProblem -> SolveProblem`
    // scalarization pass here for vector-only solver renderers. The existing
    // `phase_structural::scalarize` pass intentionally remains DAE-to-DAE so
    // DAE templates can request scalarized equation form before rendering.
    let layout = build_var_layout_with_solver_len(dae_model, solver_len)?;
    let solver_len = layout.y_scalars();
    let solve_layout = lower_solve_layout_with_var_layout(dae_model, solver_len, &layout)?;

    let runtime_tail_updates = runtime_tail_update_names(dae_model)?;
    let runtime_assignment_equations =
        runtime_assignment_equations(dae_model, &runtime_tail_updates)?;
    let discrete_update_equations = normalized_discrete_update_equations(dae_model)
        .map_err(|err| lower_problem_context(err, "collect discrete update equations"))?;
    let derivative_analysis = lower::analyze_derivative_rhs(dae_model)
        .map_err(|err| lower_problem_context(err, "analyze derivative RHS rows"))?;
    let state_derivative_rows = lower_bool_slice_copy(
        derivative_analysis.equation_flags(),
        "state derivative row flag count",
        model_span,
    )?;
    let residual_equations =
        solver_residual_equations(dae_model, &runtime_tail_updates, &state_derivative_rows)?;
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
    let state_scalar_count = solve_layout.state_scalar_count();
    let solver_scalar_count = solve_layout.solver_scalar_count();
    let derivative_rhs_len = derivative_rhs
        .len()
        .map_err(|err| lower_optional_contract_violation(err.to_string(), err.source_span()))?;
    let state_only_implicit_rhs = residual.is_empty()
        && solver_scalar_count == state_scalar_count
        && derivative_rhs_len == state_scalar_count;
    let implicit = if state_only_implicit_rhs {
        state_only_implicit_rows_and_targets(state_scalar_count, model_span)?
    } else {
        let derivative_rhs_scalar = rumoca_eval_solve::to_scalar_program_block(&derivative_rhs)
            .map_err(|err| lower_problem_context(err.into(), "scalarize derivative RHS rows"))?
            .programs;
        build_implicit_rhs_rows(
            &derivative_rhs_scalar,
            &residual,
            &residual_targets,
            state_scalar_count,
            solver_scalar_count,
            model_span,
        )?
    };
    debug_assert_eq!(implicit.residual_to_implicit_rows.len(), residual.len());
    let algebraic_projection_plan = lower_algebraic_projection_plan(
        &implicit.rows,
        &implicit.row_targets,
        state_scalar_count,
        solver_scalar_count,
        model_span,
    )?;
    let runtime_assignment_targets =
        lower_runtime_assignment_targets(dae_model, &runtime_assignment_equations, &layout)?;
    let discrete_observation_refresh =
        lower_discrete_observation_refresh(dae_model, &layout, &runtime_assignment_targets)?;
    let initialization = lower_initialization_system(dae_model, &layout, &solve_layout)?;
    let dynamic_time_event_exprs = dynamic_events::collect_dynamic_time_event_exprs(dae_model)
        .map_err(|err| lower_problem_context(err, "collect dynamic time event expressions"))?;
    let residual_block = residual_compute_block::build_residual_compute_block(
        dae_model,
        &layout,
        &residual,
        &residual_targets,
        &residual_equations,
    )?;
    let implicit_rhs = build_implicit_rhs_compute_block(
        &derivative_rhs,
        &residual_block,
        &implicit.residual_to_implicit_rows,
        implicit.rows,
        state_scalar_count,
        model_span,
    )
    .map_err(|err| lower_problem_context(err, "build implicit RHS compute block"))?;
    let problem = solve::SolveProblem {
        schema_version: solve::SOLVE_SCHEMA_VERSION,
        continuous: solve::ContinuousSolveSystem {
            implicit_row_targets: implicit.row_targets,
            implicit_rhs,
            algebraic_projection_plan,
            residual: residual_block,
            derivative_rhs,
        },
        initialization,
        discrete: solve::DiscreteSolveSystem {
            runtime_assignment_rhs: solve::ScalarProgramBlock::with_program_spans(
                lower_runtime_assignment_rhs(dae_model, &layout, &runtime_assignment_equations)
                    .map_err(|err| lower_problem_context(err, "lower runtime assignment rows"))?,
                program_spans_for_owned_equations(&runtime_assignment_equations)?,
            )?,
            runtime_assignment_targets,
            rhs: solve::ScalarProgramBlock::with_program_spans(
                lower_discrete_rhs_from_equations(dae_model, &layout, &discrete_update_equations)
                    .map_err(|err| lower_problem_context(err, "lower discrete update rows"))?,
                program_spans_for_owned_equations(&discrete_update_equations)?,
            )?,
            update_targets: lower_discrete_update_targets(dae_model, &layout)
                .map_err(|err| lower_problem_context(err, "lower discrete update targets"))?,
            pre_modes: lower_discrete_pre_modes(dae_model)
                .map_err(|err| lower_problem_context(err, "lower discrete pre modes"))?,
            observation_refresh: discrete_observation_refresh,
        },
        events: solve::SolveEventPartition {
            root_conditions: solve::ScalarProgramBlock::with_program_spans(
                lower_root_conditions(dae_model, &layout)
                    .map_err(|err| lower_problem_context(err, "lower root-condition rows"))?,
                root_condition_program_spans(dae_model)?,
            )?,
            root_relation_memory_targets: lower::lower_root_relation_memory_targets(
                dae_model, &layout,
            )
            .map_err(|err| lower_problem_context(err, "lower root relation memory targets"))?,
            scheduled_time_events: dae_model.events.scheduled_time_events.clone(),
            dynamic_time_event_names: dynamic_events::collect_dynamic_time_event_names(dae_model),
            dynamic_time_event_rhs: solve::ScalarProgramBlock::with_program_spans(
                lower_dynamic_time_event_rhs(dae_model, &layout, &dynamic_time_event_exprs)
                    .map_err(|err| lower_problem_context(err, "lower dynamic time event rows"))?,
                program_spans_for_expressions(
                    &dynamic_time_event_exprs,
                    "dynamic time event row span count",
                    model_span,
                )?,
            )?,
            action_conditions: solve::ScalarProgramBlock::with_program_spans(
                event_actions::lower_event_action_conditions(dae_model, &layout)
                    .map_err(|err| lower_problem_context(err, "lower event action rows"))?,
                dae_model
                    .events
                    .event_actions
                    .iter()
                    .map(|action| action.span)
                    .collect(),
            )?,
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
        problem.validate_shape_contract().map_err(|err| {
            lower_optional_contract_violation(
                format!("invalid Solve IR shape contract: {err}"),
                err.source_span(),
            )
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

fn program_spans_for_equations(
    equations: &[(usize, &dae::Equation)],
) -> Result<Vec<rumoca_core::Span>, LowerError> {
    let mut spans = Vec::new();
    for (_, eq) in equations {
        let row_count = eq.scalar_count.max(1);
        reserve_lower_capacity(
            &mut spans,
            row_count,
            "residual program span row count",
            eq.span,
        )?;
        for _ in 0..row_count {
            spans.push(eq.span);
        }
    }
    Ok(spans)
}

fn program_spans_for_owned_equations(
    equations: &[dae::Equation],
) -> Result<Vec<rumoca_core::Span>, LowerError> {
    let mut spans = Vec::new();
    for eq in equations {
        let row_count = eq.scalar_count.max(1);
        reserve_lower_capacity(
            &mut spans,
            row_count,
            "scalar program span row count",
            eq.span,
        )?;
        for _ in 0..row_count {
            spans.push(eq.span);
        }
    }
    Ok(spans)
}

fn program_spans_for_expressions(
    expressions: &[rumoca_core::Expression],
    context: &'static str,
    fallback_span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Span>, LowerError> {
    let context_span = expression_context_span(expressions, fallback_span);
    let mut spans = lower_vec_with_capacity(expressions.len(), context, context_span)?;
    for expression in expressions {
        spans.push(expression.span().unwrap_or(context_span));
    }
    Ok(spans)
}

fn expression_context_span(
    expressions: &[rumoca_core::Expression],
    fallback_span: rumoca_core::Span,
) -> rumoca_core::Span {
    expressions
        .iter()
        .find_map(|expression| expression.span().filter(|span| !span.is_dummy()))
        .unwrap_or(fallback_span)
}

fn root_condition_program_spans(
    dae_model: &dae::Dae,
) -> Result<Vec<rumoca_core::Span>, LowerError> {
    let fallback_span = root_condition_context_span(dae_model)?;
    let root_count = dae_model
        .conditions
        .relations
        .len()
        .checked_add(dae_model.events.synthetic_root_conditions.len())
        .and_then(|count| count.checked_add(dae_model.clocks.triggered_conditions.len()))
        .ok_or_else(|| {
            lower_contract_violation(
                "root condition span count overflows host index range".to_string(),
                fallback_span,
            )
        })?;
    let mut spans =
        lower_vec_with_capacity(root_count, "root condition row span count", fallback_span)?;
    for condition in &dae_model.conditions.relations {
        spans.push(condition.span().unwrap_or(fallback_span));
    }
    for condition in &dae_model.events.synthetic_root_conditions {
        spans.push(condition.span().unwrap_or(fallback_span));
    }
    for condition in &dae_model.clocks.triggered_conditions {
        spans.push(condition.span().unwrap_or(fallback_span));
    }
    Ok(spans)
}

fn root_condition_context_span(dae_model: &dae::Dae) -> Result<rumoca_core::Span, LowerError> {
    if let Some(span) = dae_model
        .conditions
        .relations
        .iter()
        .chain(dae_model.events.synthetic_root_conditions.iter())
        .chain(dae_model.clocks.triggered_conditions.iter())
        .find_map(|expression| expression.span().filter(|span| !span.is_dummy()))
    {
        return Ok(span);
    }
    dae_model_span(dae_model)
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
    let update_equations = lower::initial_condition_update_equations(dae_model)
        .map_err(|err| lower_problem_context(err, "collect initial condition updates"))?;
    let update_targets = lower_update_targets_from_equations(dae_model, layout, &update_equations)
        .map_err(|err| lower_problem_context(err, "lower initial update targets"))?;

    let residual_rows = lower_initial_residual(dae_model, layout)
        .map_err(|err| lower_problem_context(err, "lower initial residual rows"))?;
    let projection_indices = initial_projection_indices_for_layout(dae_model, solve_layout)?;
    let projection_plan = lower_projection_plan(
        &residual_rows,
        &row_targets,
        &projection_indices,
        0..residual_rows.len(),
        dae_model_span(dae_model)?,
    )?;

    Ok(solve::InitializationSolveSystem {
        row_targets,
        projection_indices,
        projection_plan,
        residual: solve::ScalarProgramBlock::with_program_spans(
            residual_rows,
            program_spans_for_equations(&residual_equations)?,
        )?,
        update_rhs: solve::ScalarProgramBlock::with_program_spans(
            lower_initial_update_rhs(dae_model, layout)
                .map_err(|err| lower_problem_context(err, "lower initial update rows"))?,
            program_spans_for_owned_equations(&update_equations)?,
        )?,
        update_targets,
    })
}

fn initial_projection_indices_for_layout(
    dae_model: &dae::Dae,
    solve_layout: &solve::SolveLayout,
) -> Result<Vec<usize>, LowerError> {
    let span = dae_model_span(dae_model)?;
    let state_count = solve_layout.state_scalar_count();
    let algebraic_count = solve_layout.algebraic_scalar_count();
    let algebraic_end = state_count.checked_add(algebraic_count).ok_or_else(|| {
        lower_contract_violation(
            "initial projection algebraic range overflows host index range".to_string(),
            span,
        )
    })?;
    let mut indices =
        lower_vec_with_capacity(algebraic_count, "initial projection index count", span)?;
    for (name, var) in dae_model
        .variables
        .states
        .iter()
        .filter(|(_, var)| var.fixed != Some(true))
    {
        let scalar_names = var_scalar_names(name.as_str(), var)?;
        reserve_lower_capacity(
            &mut indices,
            scalar_names.len(),
            "initial projection state index count",
            var.source_span,
        )?;
        for scalar_name in scalar_names {
            if let Some(index) = solve_layout.solver_idx_for_target(scalar_name.as_str()) {
                indices.push(index);
            }
        }
    }
    reserve_lower_capacity(
        &mut indices,
        algebraic_count,
        "initial projection algebraic index count",
        span,
    )?;
    indices.extend(state_count..algebraic_end);
    indices.sort_unstable();
    indices.dedup();
    Ok(indices)
}

pub fn lower_solve_artifacts(
    problem: &solve::SolveProblem,
) -> Result<solve::SolveArtifacts, LowerError> {
    lower_solve_artifacts_with_mass_matrix(problem, solve_identity_mass_matrix(problem)?)
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
        rumoca_eval_solve::to_scalar_program_block(&problem.continuous.implicit_rhs)
            .map_err(|err| lower_problem_context(err.into(), "scalarize implicit RHS rows"))?;
    let implicit_jacobian_v_scalar = solve::ScalarProgramBlock::with_program_spans(
        lower_scalar_program_block_full_ad_with_spans(
            &implicit_rhs_rows.programs,
            &implicit_rhs_rows.program_spans,
            &problem.layout,
        )
        .map_err(|err| lower_problem_context(err, "lower scalar implicit Jacobian rows"))?,
        implicit_rhs_rows.program_spans,
    )?;
    let derivative_rhs_rows =
        rumoca_eval_solve::to_scalar_program_block(&problem.continuous.derivative_rhs)
            .map_err(|err| lower_problem_context(err.into(), "scalarize derivative RHS rows"))?;
    let full_jacobian_v = solve::ScalarProgramBlock::with_program_spans(
        lower_scalar_program_block_full_ad_with_spans(
            &derivative_rhs_rows.programs,
            &derivative_rhs_rows.program_spans,
            &problem.layout,
        )
        .map_err(|err| lower_problem_context(err, "lower derivative Jacobian rows"))?,
        derivative_rhs_rows.program_spans,
    )?;

    Ok(solve::ContinuousSolveArtifacts {
        mass_matrix,
        implicit_jacobian_v,
        implicit_jacobian_v_scalar,
        full_jacobian_v,
    })
}

pub fn solve_identity_mass_matrix(
    problem: &solve::SolveProblem,
) -> Result<Vec<Vec<f64>>, LowerError> {
    let state_count = problem.solve_layout.state_scalar_count();
    let span = solve_problem_span(problem);
    let mut matrix =
        lower_vec_with_optional_capacity(state_count, "identity mass matrix rows", span)?;
    for idx in 0..state_count {
        let mut row =
            lower_vec_with_optional_capacity(state_count, "identity mass matrix row", span)?;
        row.resize(state_count, 0.0);
        row[idx] = 1.0;
        matrix.push(row);
    }
    Ok(matrix)
}

fn solve_problem_span(problem: &solve::SolveProblem) -> Option<rumoca_core::Span> {
    compute_block_span(&problem.continuous.derivative_rhs)
        .or_else(|| compute_block_span(&problem.continuous.implicit_rhs))
        .or_else(|| compute_block_span(&problem.continuous.residual))
}

fn compute_block_span(block: &solve::ComputeBlock) -> Option<rumoca_core::Span> {
    block.nodes.iter().find_map(compute_node_span)
}

fn compute_node_span(node: &solve::ComputeNode) -> Option<rumoca_core::Span> {
    let span = match node {
        solve::ComputeNode::ScalarPrograms(block) => block
            .program_spans
            .iter()
            .copied()
            .find(|span| !span.is_dummy())?,
        solve::ComputeNode::MatMul { span, .. }
        | solve::ComputeNode::LinSolve { span, .. }
        | solve::ComputeNode::Map { span, .. }
        | solve::ComputeNode::AffineStencil { span, .. } => *span,
    };
    (!span.is_dummy()).then_some(span)
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
        err @ (LowerError::ContractViolation { .. }
        | LowerError::UnspannedContractViolation { .. }) => err,
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
) -> Result<Vec<(usize, &'a dae::Equation)>, LowerError> {
    let mut equations = Vec::new();
    for (row_idx, eq) in dae_model.continuous.equations.iter().enumerate() {
        let Some(&is_state_derivative_row) = state_derivative_rows.get(row_idx) else {
            return Err(lower_contract_violation(
                format!("missing state-derivative flag for residual equation {row_idx}"),
                eq.span,
            ));
        };
        if solver_residual_equation(dae_model, runtime_tail_updates, is_state_derivative_row, eq)? {
            equations.push((row_idx, eq));
        }
    }
    Ok(equations)
}

fn solver_residual_equation(
    dae_model: &dae::Dae,
    runtime_tail_updates: &HashSet<String>,
    is_state_derivative_row: bool,
    eq: &dae::Equation,
) -> Result<bool, LowerError> {
    // MLS Appendix B B.1a: continuous equations are an unordered implicit set.
    // Solve-IR separates state derivative rows from algebraic residual rows by
    // equation structure, not by their source order in DAE `f_x`.
    Ok(!is_state_derivative_row
        && !static_runtime_tail_equation(dae_model, runtime_tail_updates, eq)?
        && runtime_assignment_equation(dae_model, runtime_tail_updates, eq)?.is_none())
}

fn lower_algebraic_projection_plan(
    rows: &[Vec<solve::LinearOp>],
    row_targets: &[Option<solve::ScalarSlot>],
    state_scalar_count: usize,
    solver_scalar_count: usize,
    context_span: rumoca_core::Span,
) -> Result<solve::AlgebraicProjectionPlan, LowerError> {
    let projection_count = solver_scalar_count
        .checked_sub(state_scalar_count)
        .ok_or_else(|| {
            lower_contract_violation(
                "algebraic projection range starts after solver scalar count".to_string(),
                context_span,
            )
        })?;
    let mut projection_indices = lower_vec_with_capacity(
        projection_count,
        "algebraic projection index count",
        context_span,
    )?;
    projection_indices.extend(state_scalar_count..solver_scalar_count);
    lower_projection_plan(
        rows,
        row_targets,
        &projection_indices,
        state_scalar_count..solver_scalar_count,
        context_span,
    )
}

fn lower_projection_plan(
    rows: &[Vec<solve::LinearOp>],
    row_targets: &[Option<solve::ScalarSlot>],
    projection_indices: &[usize],
    row_indices: std::ops::Range<usize>,
    context_span: rumoca_core::Span,
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

    let projection_incidence = algebraic_projection_incidence(&row_to_vars, context_span)?;
    let blocks = projection_blt_blocks(&projection_incidence)?;
    Ok(solve::AlgebraicProjectionPlan {
        blocks: lower_blt_projection_blocks(
            &blocks,
            row_targets,
            &projection_incidence,
            context_span,
        )?,
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
        Op::LoadIndexedP { dst, index, .. } | Op::LoadIndexedSeed { dst, index, .. } => {
            def_use(dst, None, vec![index])
        }
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
    context_span: rumoca_core::Span,
) -> Result<ProjectionIncidence, LowerError> {
    let unknown_y_set = row_to_vars
        .values()
        .flat_map(|vars| vars.iter().copied())
        .collect::<BTreeSet<_>>();
    let mut unknown_y_indices = lower_vec_with_capacity(
        unknown_y_set.len(),
        "projection unknown index count",
        context_span,
    )?;
    unknown_y_indices.extend(unknown_y_set);

    let mut unknown_names = lower_vec_with_capacity(
        unknown_y_indices.len(),
        "projection unknown name count",
        context_span,
    )?;
    for y_idx in &unknown_y_indices {
        unknown_names.push(projection_unknown_id(*y_idx));
    }

    let unknown_positions = unknown_y_indices
        .iter()
        .copied()
        .enumerate()
        .map(|(local_idx, y_idx)| (y_idx, local_idx))
        .collect::<BTreeMap<_, _>>();

    let mut equation_refs = lower_vec_with_capacity(
        row_to_vars.len(),
        "projection equation ref count",
        context_span,
    )?;
    let mut eq_unknowns = lower_vec_with_capacity(
        row_to_vars.len(),
        "projection equation unknown count",
        context_span,
    )?;
    for (row_idx, vars) in row_to_vars {
        equation_refs.push(EquationRef(*row_idx));
        let mut unknowns =
            lower_hash_set_with_capacity(vars.len(), "projection row unknown count", context_span)?;
        for y_idx in vars {
            if let Some(local_idx) = unknown_positions.get(y_idx).copied() {
                unknowns.insert(local_idx);
            }
        }
        eq_unknowns.push(unknowns);
    }

    Ok(ProjectionIncidence {
        incidence: Incidence::new(eq_unknowns, equation_refs, unknown_names),
        unknown_y_indices,
    })
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
    context_span: rumoca_core::Span,
) -> Result<Vec<solve::AlgebraicProjectionBlock>, LowerError> {
    let mut lowered = lower_vec_with_capacity(
        blocks.len(),
        "algebraic projection block count",
        context_span,
    )?;
    for block in blocks {
        let block = match block {
            BltBlock::Scalar { equation, unknown } => {
                projection_y_index(unknown, projection_incidence)
                    .map(|y_index| scalar_projection_block(equation.0, y_index, context_span))
                    .transpose()?
            }
            BltBlock::AlgebraicLoop {
                equations,
                unknowns,
            } => lower_algebraic_loop_projection_block(
                equations,
                unknowns,
                row_targets,
                projection_incidence,
                context_span,
            )?,
        };
        if let Some(block) = block {
            lowered.push(block);
        }
    }
    merge_overlapping_projection_blocks(lowered, context_span)
}

fn merge_overlapping_projection_blocks(
    blocks: Vec<solve::AlgebraicProjectionBlock>,
    context_span: rumoca_core::Span,
) -> Result<Vec<solve::AlgebraicProjectionBlock>, LowerError> {
    let mut merged = lower_vec_with_capacity(
        blocks.len(),
        "merged algebraic projection block count",
        context_span,
    )?;
    for block in blocks {
        merge_projection_block(&mut merged, block, context_span)?;
    }
    Ok(merged)
}

fn merge_projection_block(
    merged: &mut Vec<solve::AlgebraicProjectionBlock>,
    mut block: solve::AlgebraicProjectionBlock,
    context_span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let mut idx = 0;
    while idx < merged.len() {
        if projection_blocks_overlap(&merged[idx], &block) {
            let previous = merged.remove(idx);
            block = combine_projection_blocks(previous, block, context_span)?;
            idx = 0;
        } else {
            idx += 1;
        }
    }
    merged.push(block);
    Ok(())
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
    context_span: rumoca_core::Span,
) -> Result<solve::AlgebraicProjectionBlock, LowerError> {
    let causal_step_count = lhs
        .causal_steps
        .len()
        .checked_add(rhs.causal_steps.len())
        .ok_or_else(|| {
            lower_contract_violation(
                "merged algebraic projection causal-step count overflows host index range"
                    .to_string(),
                context_span,
            )
        })?;
    let mut causal_steps = lower_vec_with_capacity(
        causal_step_count,
        "merged algebraic projection causal-step count",
        context_span,
    )?;
    causal_steps.extend(lhs.causal_steps);
    causal_steps.extend(rhs.causal_steps);
    Ok(solve::AlgebraicProjectionBlock {
        rows: merge_unique(
            lhs.rows,
            rhs.rows,
            "merged algebraic projection row count",
            context_span,
        )?,
        y_indices: merge_unique(
            lhs.y_indices,
            rhs.y_indices,
            "merged algebraic projection target count",
            context_span,
        )?,
        causal_steps,
    })
}

fn merge_unique(
    lhs: Vec<usize>,
    rhs: Vec<usize>,
    context: &'static str,
    context_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let capacity = lhs.len().checked_add(rhs.len()).ok_or_else(|| {
        lower_contract_violation(
            format!("{context} overflows host index range"),
            context_span,
        )
    })?;
    let mut merged = lower_vec_with_capacity(capacity, context, context_span)?;
    merged.extend(lhs);
    merged.extend(rhs);
    merged.sort_unstable();
    merged.dedup();
    Ok(merged)
}

fn scalar_projection_block(
    row: usize,
    y_index: usize,
    context_span: rumoca_core::Span,
) -> Result<solve::AlgebraicProjectionBlock, LowerError> {
    let mut rows = lower_vec_with_capacity(
        1,
        "scalar algebraic projection block row count",
        context_span,
    )?;
    rows.push(row);
    let mut y_indices = lower_vec_with_capacity(
        1,
        "scalar algebraic projection block target count",
        context_span,
    )?;
    y_indices.push(y_index);
    Ok(solve::AlgebraicProjectionBlock {
        rows,
        y_indices,
        causal_steps: Vec::new(),
    })
}

fn sorted_set_values(
    values: BTreeSet<usize>,
    context: &'static str,
    context_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut out = lower_vec_with_capacity(values.len(), context, context_span)?;
    out.extend(values);
    Ok(out)
}

fn collect_equation_rows(
    equations: &[EquationRef],
    context_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut rows = lower_vec_with_capacity(
        equations.len(),
        "algebraic loop projection row count",
        context_span,
    )?;
    for equation in equations {
        rows.push(equation.0);
    }
    Ok(rows)
}

fn lower_algebraic_loop_projection_block(
    equations: &[EquationRef],
    unknowns: &[UnknownId],
    row_targets: &[Option<solve::ScalarSlot>],
    projection_incidence: &ProjectionIncidence,
    context_span: rumoca_core::Span,
) -> Result<Option<solve::AlgebraicProjectionBlock>, LowerError> {
    let rows = collect_equation_rows(equations, context_span)?;
    let y_indices = sorted_set_values(
        loop_projection_target_set(unknowns, row_targets, &rows, projection_incidence),
        "algebraic loop projection target count",
        context_span,
    )?;
    if rows.is_empty() || y_indices.is_empty() {
        return Ok(None);
    }
    Ok(Some(solve::AlgebraicProjectionBlock {
        rows,
        y_indices,
        causal_steps: Vec::new(),
    }))
}

fn loop_projection_target_set(
    unknowns: &[UnknownId],
    row_targets: &[Option<solve::ScalarSlot>],
    rows: &[usize],
    projection_incidence: &ProjectionIncidence,
) -> BTreeSet<usize> {
    let mut y_indices = BTreeSet::new();
    for unknown in unknowns {
        if let Some(index) = projection_y_index(unknown, projection_incidence) {
            y_indices.insert(index);
        }
    }
    for row in rows {
        if let Some(solve::ScalarSlot::Y { index, .. }) = row_targets.get(*row).copied().flatten()
            && projection_incidence.unknown_y_indices.contains(&index)
        {
            y_indices.insert(index);
        }
    }
    y_indices
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
) -> Result<solve::SolverNameIndexMaps, LowerError> {
    let solver_names = collect_solver_names(dae_model, y_len)?;
    let span = dae_model_span(dae_model)?;
    let mut name_to_idx = IndexMap::new();
    reserve_lower_index_map_capacity(
        &mut name_to_idx,
        solver_names.len(),
        "solver name index count",
        span,
    )?;
    for (idx, name) in solver_names.iter().enumerate() {
        name_to_idx.insert(name.clone(), idx);
    }
    insert_solver_name_aliases(dae_model, y_len, &mut name_to_idx)?;
    let mut base_to_indices: IndexMap<String, Vec<usize>> = IndexMap::new();
    for (idx, name) in solver_names.iter().enumerate() {
        let base = dae::component_base_name(name).unwrap_or_else(|| name.to_string());
        if let Some(indices) = base_to_indices.get_mut(&base) {
            reserve_lower_capacity(indices, 1, "solver base-name scalar index count", span)?;
            indices.push(idx);
            continue;
        }
        reserve_lower_index_map_capacity(
            &mut base_to_indices,
            1,
            "solver base-name index count",
            span,
        )?;
        let mut indices = lower_vec_with_capacity(1, "solver base-name scalar index count", span)?;
        indices.push(idx);
        base_to_indices.insert(base, indices);
    }

    Ok(solve::SolverNameIndexMaps {
        names: solver_names,
        name_to_idx,
        base_to_indices,
    })
}

fn variable_size(var: &dae::Variable) -> Result<usize, LowerError> {
    var.try_size()
        .map_err(|err| lower_contract_violation(err.to_string(), err.span()))
}

fn scalar_count<'a>(
    mut vars: impl Iterator<Item = &'a dae::Variable>,
) -> Result<usize, LowerError> {
    vars.try_fold(0usize, |acc, var| {
        variable_size(var).and_then(|size| {
            acc.checked_add(size).ok_or_else(|| {
                lower_contract_violation(
                    "DAE scalar count overflows usize".to_string(),
                    var.source_span,
                )
            })
        })
    })
}

fn var_scalar_names(name: &str, var: &dae::Variable) -> Result<Vec<String>, LowerError> {
    let size = variable_size(var)?;
    if size <= 1 && var.dims.is_empty() {
        let mut names = lower_vec_with_capacity(1, "variable scalar name count", var.source_span)?;
        names.push(name.to_string());
        return Ok(names);
    }
    let mut names = lower_vec_with_capacity(size, "variable scalar name count", var.source_span)?;
    for idx in 0..size {
        names.push(dae::scalar_name_text_for_flat_index(name, &var.dims, idx));
    }
    Ok(names)
}

fn collect_scalar_names<'a>(
    vars: impl Iterator<Item = (&'a rumoca_core::VarName, &'a dae::Variable)>,
) -> Result<Vec<String>, LowerError> {
    let mut names = Vec::new();
    for (name, var) in vars {
        let var_names = var_scalar_names(name.as_str(), var)?;
        reserve_lower_capacity(
            &mut names,
            var_names.len(),
            "collected scalar name count",
            var.source_span,
        )?;
        names.extend(var_names);
    }
    Ok(names)
}

fn collect_solver_names(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Result<Vec<String>, LowerError> {
    let mut names = collect_scalar_names(
        dae_model
            .variables
            .states
            .iter()
            .chain(dae_model.variables.algebraics.iter())
            .chain(dae_model.variables.outputs.iter())
            .filter(|(name, _)| !layout::is_runtime_parameter_tail_variable(dae_model, name)),
    )?;
    names.truncate(solver_len);
    Ok(names)
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
    let mut targets = lower_vec_with_capacity(
        equations.len(),
        "discrete update target count",
        dae_model_span(dae_model)?,
    )?;
    for eq in equations {
        let Some(lhs) = eq.lhs.as_ref() else {
            return Err(LowerError::Unsupported {
                reason: "discrete update equation is missing a target".to_string(),
            });
        };
        let scalar_count = eq.scalar_count.max(1);
        reserve_lower_capacity(
            &mut targets,
            scalar_count,
            "discrete update target count",
            eq.span,
        )?;
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
    let equations = normalized_discrete_update_equations(dae_model)?;
    let mut modes = lower_vec_with_capacity(
        equations.len(),
        "discrete pre-mode count",
        dae_model_span(dae_model)?,
    )?;
    for eq in equations {
        let scalar_count = eq.scalar_count.max(1);
        let mode = discrete_pre_mode_for_equation(dae_model, &eq);
        reserve_lower_capacity(&mut modes, scalar_count, "discrete pre-mode count", eq.span)?;
        modes.extend(std::iter::repeat_n(mode, scalar_count));
    }
    Ok(modes)
}

fn collect_expression_read_slots(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    out: &mut Vec<solve::ScalarSlot>,
) -> Result<(), LowerError> {
    struct ReadSlotCollector<'a, 'out> {
        dae_model: &'a dae::Dae,
        layout: &'a solve::VarLayout,
        out: &'out mut Vec<solve::ScalarSlot>,
        error: Option<LowerError>,
    }

    impl ExpressionVisitor for ReadSlotCollector<'_, '_> {
        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
        ) {
            if self.error.is_none()
                && let Err(err) = collect_var_ref_read_slots(
                    self.dae_model,
                    self.layout,
                    name.var_name(),
                    subscripts,
                    name.span(),
                    self.out,
                )
            {
                self.error = Some(err);
            }
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }
    }

    let mut collector = ReadSlotCollector {
        dae_model,
        layout,
        out,
        error: None,
    };
    collector.visit_expression(expr);
    match collector.error {
        Some(err) => Err(err),
        None => Ok(()),
    }
}

fn collect_var_ref_read_slots(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    owner_span: Option<rumoca_core::Span>,
    out: &mut Vec<solve::ScalarSlot>,
) -> Result<(), LowerError> {
    if let Some(indices) = checked_literal_positive_indices(subscripts, owner_span)? {
        let key = if indices.is_empty() {
            name.as_str().to_string()
        } else {
            dae::format_subscript_key(name.as_str(), &indices)
        };
        if let Some(slot) = layout.binding(key.as_str()) {
            reserve_lower_optional_capacity(out, 1, "expression read slot count", owner_span)?;
            out.push(slot);
        }
        return Ok(());
    }
    collect_all_var_slots(dae_model, layout, name, owner_span, out)
}

fn collect_all_var_slots(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    name: &rumoca_core::VarName,
    owner_span: Option<rumoca_core::Span>,
    out: &mut Vec<solve::ScalarSlot>,
) -> Result<(), LowerError> {
    let Some(var) = variable_by_name(dae_model, name) else {
        if let Some(slot) = layout.binding(name.as_str()) {
            reserve_lower_optional_capacity(out, 1, "expression read slot count", owner_span)?;
            out.push(slot);
        }
        return Ok(());
    };
    let size = variable_size(var)?;
    reserve_lower_capacity(
        out,
        size.max(1),
        "expression read slot count",
        var.source_span,
    )?;
    for idx in 0..size.max(1) {
        let key = if size <= 1 && var.dims.is_empty() {
            name.as_str().to_string()
        } else {
            dae::scalar_name_text_for_flat_index(name.as_str(), &var.dims, idx)
        };
        if let Some(slot) = layout.binding(key.as_str()) {
            out.push(slot);
        }
    }
    Ok(())
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
    let dims = discrete_update_dims(dae_model, lhs).ok_or_else(|| {
        lower_contract_violation(
            format!(
                "discrete update array LHS `{}` must be a known DAE variable",
                lhs.as_str()
            ),
            span,
        )
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
) -> Result<(), LowerError> {
    let span = dae_model_span(dae_model)?;
    let mut solver_name_set = HashSet::new();
    reserve_lower_hash_set_capacity(
        &mut solver_name_set,
        name_to_idx.len(),
        "solver name alias lookup count",
        span,
    )?;
    for name in name_to_idx.keys() {
        solver_name_set.insert(name.clone());
    }
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
        let size = variable_size(var)?;
        if size == 0 {
            continue;
        }
        if offset >= solver_len {
            break;
        }

        let visible_size = size.min(solver_len - offset);
        if size > 1
            && first_visible_scalar_name(name.as_str(), var)?
                .as_deref()
                .is_some_and(|scalar| solver_name_set.contains(scalar))
            && !name_to_idx.contains_key(name.as_str())
        {
            reserve_lower_index_map_capacity(
                name_to_idx,
                1,
                "solver name alias count",
                var.source_span,
            )?;
            name_to_idx.insert(name.as_str().to_string(), offset);
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
            let scalar_index =
                checked_solver_scalar_index(offset, flat_idx, canonical_name.as_str(), var)?;
            if !name_to_idx.contains_key(canonical_name.as_str()) {
                reserve_lower_index_map_capacity(
                    name_to_idx,
                    1,
                    "solver scalar name alias count",
                    var.source_span,
                )?;
                name_to_idx.insert(canonical_name, scalar_index);
            }
        }
        offset = checked_solver_scalar_offset(offset, size, name.as_str(), var)?;
    }
    Ok(())
}

fn checked_solver_scalar_index(
    offset: usize,
    flat_idx: usize,
    canonical_name: &str,
    var: &dae::Variable,
) -> Result<usize, LowerError> {
    offset.checked_add(flat_idx).ok_or_else(|| {
        lower_contract_violation(
            format!("solver scalar index for `{canonical_name}` overflows host index range"),
            var.source_span,
        )
    })
}

fn checked_solver_scalar_offset(
    offset: usize,
    size: usize,
    name: &str,
    var: &dae::Variable,
) -> Result<usize, LowerError> {
    offset.checked_add(size).ok_or_else(|| {
        lower_contract_violation(
            format!("solver scalar offset after `{name}` overflows host index range"),
            var.source_span,
        )
    })
}

fn first_visible_scalar_name(
    name: &str,
    var: &dae::Variable,
) -> Result<Option<String>, LowerError> {
    let size = variable_size(var)?;
    if size == 0 {
        return Ok(None);
    }
    Ok(Some(if size <= 1 && var.dims.is_empty() {
        name.to_string()
    } else {
        dae::scalar_name_text_for_flat_index(name, &var.dims, 0)
    }))
}
