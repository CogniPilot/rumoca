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
mod capacity;
mod continuous_row_targets;
pub mod diagnostic_codes;
mod discrete_pre_modes;
mod dynamic_events;
mod event_actions;
mod implicit_rhs;
mod init_plan_trace;
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
pub mod tensor_declines;
mod tensor_report;
#[cfg(test)]
#[path = "tests/test_support.rs"]
mod test_support;
#[cfg(test)]
mod tests;
mod timing;

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
pub use diagnostic_codes::SOLVE_LOWER_DIAGNOSTIC_CODES;
use discrete_pre_modes::discrete_pre_mode_for_equation;
#[cfg(test)]
pub(crate) use discrete_pre_modes::expression_contains_event_entry_pre_operator;
#[cfg(test)]
use implicit_rhs::zero_rhs_row;
use implicit_rhs::{
    build_implicit_rhs_compute_block, build_implicit_rhs_rows, state_only_implicit_rows_and_targets,
};
use layout::{HOMOTOPY_LAMBDA_PARAMETER_NAME, INITIAL_EVENT_PARAMETER_NAME};
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
    lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata_and_overrides,
    lower_dae_to_solve_model_owned_value_only_with_visible_expressions_and_metadata,
    lower_dae_to_solve_model_owned_value_only_with_visible_expressions_and_metadata_and_overrides,
    lower_dae_to_solve_model_owned_with_visible_expressions,
    lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata,
    lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata_and_overrides,
    propagate_parameter_overrides, visible_expressions_for_dae,
};
pub(crate) use subscript_indices::{checked_literal_positive_indices, subscript_source_span};
pub use tensor_declines::{
    LoweredContinuousFamilies, LoweredFamily, TensorDeclineJournal, TensorDeclineRecord,
    TensorFallbackCount, TensorFallbackReason, TensorHeadroom,
};
pub use tensor_report::{
    TensorFallback, TensorPreservationReport, TensorReportProvenance, tensor_preservation_report,
    tensor_preservation_report_from_lowering, tensor_preservation_report_with_declines,
};
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
    let terminal_event_parameter_index =
        match layout.binding(rumoca_core::TERMINAL_EVENT_PARAMETER_NAME) {
            Some(solve::ScalarSlot::P { index, .. }) => Some(index),
            _ => None,
        };
    let initial_homotopy_parameter_index = match layout.binding(HOMOTOPY_LAMBDA_PARAMETER_NAME) {
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
        terminal_event_parameter_index,
        initial_homotopy_parameter_index,
        pre_param_bindings: build_pre_param_bindings(dae_model, layout),
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

fn build_pre_param_bindings(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Vec<solve::PreParamBinding> {
    let mut bindings = Vec::new();
    for (name, &slot) in layout.bindings() {
        let Some(source_name) = rumoca_core::pre_slot_base(name.as_str()) else {
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
            clock_schedule: pre_source_clock_schedule(dae_model, source_name),
        });
    }
    bindings
}

fn pre_source_clock_schedule(
    dae_model: &dae::Dae,
    source_name: &str,
) -> Option<solve::PeriodicEventSchedule> {
    let timing = dae_model.clocks.timings.get(source_name).or_else(|| {
        let scalar = rumoca_core::parse_scalar_name(source_name)?;
        dae_model.clocks.timings.get(scalar.base)
    })?;
    Some(solve::PeriodicEventSchedule {
        period_seconds: timing.period_seconds,
        phase_seconds: timing.phase_seconds,
    })
}

pub fn lower_solve_problem(dae_model: &dae::Dae) -> Result<solve::SolveProblem, LowerError> {
    lower_solve_problem_with_solver_len(dae_model, usize::MAX)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SolveProblemLoweringProfile {
    Runtime,
    RuntimeValueOnly,
    GpuPreparation,
}

impl SolveProblemLoweringProfile {
    fn load_projection_algebraics_in_derivative_rhs(self) -> bool {
        matches!(self, Self::Runtime | Self::RuntimeValueOnly)
    }

    fn lower_residual_equations(self) -> bool {
        matches!(self, Self::Runtime | Self::RuntimeValueOnly)
    }

    fn lower_initialization_system(self) -> bool {
        self == Self::Runtime
    }

    fn lower_initialization_updates(self) -> bool {
        matches!(self, Self::Runtime | Self::RuntimeValueOnly)
    }

    fn lower_runtime_systems(self) -> bool {
        matches!(self, Self::Runtime | Self::RuntimeValueOnly)
    }
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

// SPEC_0021: Exception - top-level Solve-IR lowering entry point assembles the
// whole SolveProblem so stage contracts are visible at the phase boundary.
#[allow(clippy::too_many_lines)]
pub(crate) fn lower_solve_problem_with_solver_len_and_model_span(
    dae_model: &dae::Dae,
    solver_len: usize,
    fallback_model_span: Option<rumoca_core::Span>,
) -> Result<solve::SolveProblem, LowerError> {
    lower_solve_problem_with_solver_len_and_model_span_and_profile(
        dae_model,
        solver_len,
        fallback_model_span,
        SolveProblemLoweringProfile::Runtime,
    )
}

pub(crate) fn lower_solve_problem_with_solver_len_and_model_span_and_profile(
    dae_model: &dae::Dae,
    solver_len: usize,
    fallback_model_span: Option<rumoca_core::Span>,
    profile: SolveProblemLoweringProfile,
) -> Result<solve::SolveProblem, LowerError> {
    let mut declines = TensorDeclineJournal::new();
    lower_solve_problem_with_declines(
        dae_model,
        solver_len,
        fallback_model_span,
        profile,
        &mut declines,
    )
}

/// Lower the continuous system and keep the journal of per-family tensor
/// declines, so [`tensor_preservation_report_with_declines`] can name the
/// branch that scalarized each structured family instead of restating that one
/// did.
pub fn lower_solve_problem_with_tensor_declines(
    dae_model: &dae::Dae,
) -> Result<(solve::SolveProblem, TensorDeclineJournal), LowerError> {
    let mut declines = TensorDeclineJournal::new();
    let problem = lower_solve_problem_with_declines(
        dae_model,
        usize::MAX,
        None,
        SolveProblemLoweringProfile::Runtime,
        &mut declines,
    )?;
    Ok((problem, declines))
}

// SPEC_0021: Exception - implementation for the top-level Solve-IR lowering
// entry point remains one unit so stage contracts are visible at the phase
// boundary.
#[allow(clippy::too_many_lines)]
fn lower_solve_problem_with_declines(
    dae_model: &dae::Dae,
    solver_len: usize,
    fallback_model_span: Option<rumoca_core::Span>,
    profile: SolveProblemLoweringProfile,
    declines: &mut TensorDeclineJournal,
) -> Result<solve::SolveProblem, LowerError> {
    if ir_boundary_validation_enabled() {
        dae_model.validate_shape_contract().map_err(|err| {
            lower_contract_violation(format!("invalid DAE IR shape contract: {err}"), err.span())
        })?;
    }
    appendix_b_validation::validate_solve_input_appendix_b_invariants(dae_model)?;
    // Consume the finalized output of the structural DAE-to-DAE alias pass.
    // Keeping the pass in phase-structural enforces SPEC_0007's phase boundary:
    // Solve lowering never owns mathematical DAE rewrites.
    let dummy_eliminated =
        rumoca_phase_structural::dae_prepare::eliminate_dummy_derivative_aliases(dae_model);
    let dae_model = dummy_eliminated.as_ref().unwrap_or(dae_model);
    // Record the continuous families THIS lowering consumes, before any of it
    // runs. Alias elimination above can already have rewritten the caller's
    // DAE, and family attribution is positional, so the report must measure
    // against the list lowering saw rather than one the caller kept a handle to.
    declines.record_lowered_continuous(dae_model);
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
    let timer = timing::stage_start();
    let layout = build_var_layout_with_solver_len(dae_model, solver_len)?;
    timing::log_stage("problem.build_var_layout", timer);
    let solver_len = layout.y_scalars();
    let timer = timing::stage_start();
    let solve_layout = lower_solve_layout_with_var_layout(dae_model, solver_len, &layout)?;
    timing::log_stage("problem.lower_solve_layout", timer);

    let timer = timing::stage_start();
    let runtime_tail_updates = runtime_tail_update_names(dae_model)?;
    let runtime_assignment_equations =
        runtime_assignment_equations(dae_model, &runtime_tail_updates)?;
    let discrete_update_equations = normalized_discrete_update_equations(dae_model)
        .map_err(|err| lower_problem_context(err, "collect discrete update equations"))?;
    timing::log_stage("problem.collect_runtime_equations", timer);
    let timer = timing::stage_start();
    let mut derivative_analysis = lower::analyze_derivative_rhs(dae_model)
        .map_err(|err| lower_problem_context(err, "analyze derivative RHS rows"))?;
    let state_derivative_rows = lower_bool_slice_copy(
        derivative_analysis.equation_flags(),
        "state derivative row flag count",
        model_span,
    )?;
    timing::log_stage("problem.analyze_derivative_rhs", timer);
    let timer = timing::stage_start();
    let residual_equations = if profile.lower_residual_equations() {
        solver_residual_equations(dae_model, &runtime_tail_updates, &state_derivative_rows)?
    } else {
        Vec::new()
    };
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
    timing::log_stage("problem.lower_residual_rows", timer);
    // Derivative lowering must LOAD retained algebraic unknowns from their projected
    // slot rather than inline their definitions (roadmap 4b): inlining a boundary cell
    // whose flux folds to a constant makes a structured derivative family non-uniform
    // and blocks stencil preservation. The retained unknowns are exactly the residual
    // targets that land in the algebraic Y-segment — solved by the algebraic projection
    // and refreshed before derivative evaluation.
    let algebraic_y_end = solve_layout.state_scalar_count() + solve_layout.algebraic_scalar_count();
    let solved_algebraic_y: std::collections::HashSet<usize> = residual_targets
        .iter()
        .flatten()
        .filter_map(|slot| match slot {
            solve::ScalarSlot::Y { index, .. }
                if *index >= solve_layout.state_scalar_count() && *index < algebraic_y_end =>
            {
                Some(*index)
            }
            _ => None,
        })
        .collect();
    if profile.load_projection_algebraics_in_derivative_rhs() {
        derivative_analysis.load_retained_algebraics(&layout, &solved_algebraic_y);
    }
    let timer = timing::stage_start();
    let derivative_rhs = lower::lower_derivative_rhs_with_analysis(
        dae_model,
        &layout,
        &derivative_analysis,
        declines,
    )
    .map_err(|err| lower_problem_context(err, "lower derivative RHS rows"))?;
    timing::log_stage("problem.lower_derivative_rhs", timer);
    let state_scalar_count = solve_layout.state_scalar_count();
    let solver_scalar_count = solve_layout.solver_scalar_count();
    let derivative_rhs_len = derivative_rhs
        .len()
        .map_err(|err| lower_optional_contract_violation(err.to_string(), err.source_span()))?;
    let state_only_implicit_rhs = residual.is_empty()
        && solver_scalar_count == state_scalar_count
        && derivative_rhs_len == state_scalar_count;
    let timer = timing::stage_start();
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
    timing::log_stage("problem.build_implicit_rows", timer);
    debug_assert_eq!(implicit.residual_to_implicit_rows.len(), residual.len());
    let timer = timing::stage_start();
    let algebraic_projection_plan = lower_algebraic_projection_plan(
        &implicit.rows,
        &implicit.row_targets,
        state_scalar_count,
        solver_scalar_count,
        model_span,
    )?;
    timing::log_stage("problem.lower_projection_plan", timer);
    let timer = timing::stage_start();
    let runtime_assignment_targets = if profile.lower_runtime_systems() {
        lower_runtime_assignment_targets(dae_model, &runtime_assignment_equations, &layout)?
    } else {
        Vec::new()
    };
    let discrete_observation_refresh = if profile.lower_runtime_systems() {
        lower_discrete_observation_refresh(dae_model, &layout, &runtime_assignment_targets)?
    } else {
        Vec::new()
    };
    timing::log_stage("problem.lower_runtime_systems", timer);
    let timer = timing::stage_start();
    let initialization = if profile.lower_initialization_system() {
        lower_initialization_system(dae_model, &layout, &solve_layout)?
    } else if profile.lower_initialization_updates() {
        lower_initialization_updates_only(dae_model, &layout)?
    } else {
        solve::InitializationSolveSystem::default()
    };
    timing::log_stage("problem.lower_initialization", timer);
    let dynamic_time_event_exprs = if profile.lower_runtime_systems() {
        dynamic_events::collect_dynamic_time_event_exprs(dae_model)
            .map_err(|err| lower_problem_context(err, "collect dynamic time event expressions"))?
    } else {
        Vec::new()
    };
    let timer = timing::stage_start();
    let residual_block = if profile.lower_residual_equations() {
        residual_compute_block::build_residual_compute_block(
            dae_model,
            &layout,
            &residual,
            &residual_targets,
            &residual_equations,
            declines,
        )?
    } else {
        solve::ComputeBlock::default()
    };
    timing::log_stage("problem.build_residual_block", timer);
    let timer = timing::stage_start();
    let implicit_rhs = build_implicit_rhs_compute_block(
        &derivative_rhs,
        &residual_block,
        &implicit.residual_to_implicit_rows,
        implicit.rows,
        state_scalar_count,
        model_span,
    )
    .map_err(|err| lower_problem_context(err, "build implicit RHS compute block"))?;
    timing::log_stage("problem.build_implicit_rhs_block", timer);
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
        discrete: lower_discrete_system_for_profile(
            DiscreteSystemInputs {
                dae_model,
                layout: &layout,
                runtime_assignment_equations: &runtime_assignment_equations,
                runtime_assignment_targets,
                discrete_update_equations: &discrete_update_equations,
                discrete_observation_refresh,
            },
            profile,
        )?,
        events: lower_event_partition_for_profile(
            dae_model,
            &layout,
            &dynamic_time_event_exprs,
            model_span,
            profile,
        )?,
        clocks: if profile.lower_runtime_systems() {
            solve::SolveClockPartition {
                periodic_event_schedules: lower_periodic_event_schedules(dae_model),
            }
        } else {
            solve::SolveClockPartition::default()
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

struct DiscreteSystemInputs<'a> {
    dae_model: &'a dae::Dae,
    layout: &'a solve::VarLayout,
    runtime_assignment_equations: &'a [dae::Equation],
    runtime_assignment_targets: Vec<solve::ScalarSlot>,
    discrete_update_equations: &'a [dae::Equation],
    discrete_observation_refresh: Vec<bool>,
}

fn lower_discrete_system_for_profile(
    inputs: DiscreteSystemInputs<'_>,
    profile: SolveProblemLoweringProfile,
) -> Result<solve::DiscreteSolveSystem, LowerError> {
    if !profile.lower_runtime_systems() {
        return Ok(solve::DiscreteSolveSystem::default());
    }
    Ok(solve::DiscreteSolveSystem {
        runtime_assignment_rhs: solve::ScalarProgramBlock::with_program_spans(
            lower_runtime_assignment_rhs(
                inputs.dae_model,
                inputs.layout,
                inputs.runtime_assignment_equations,
            )
            .map_err(|err| lower_problem_context(err, "lower runtime assignment rows"))?,
            program_spans_for_owned_equations(inputs.runtime_assignment_equations)?,
        )?,
        runtime_assignment_targets: inputs.runtime_assignment_targets,
        rhs: solve::ScalarProgramBlock::with_program_spans(
            lower_discrete_rhs_from_equations(
                inputs.dae_model,
                inputs.layout,
                inputs.discrete_update_equations,
            )
            .map_err(|err| lower_problem_context(err, "lower discrete update rows"))?,
            program_spans_for_owned_equations(inputs.discrete_update_equations)?,
        )?,
        update_targets: lower_discrete_update_targets(inputs.dae_model, inputs.layout)
            .map_err(|err| lower_problem_context(err, "lower discrete update targets"))?,
        pre_modes: lower_discrete_pre_modes(inputs.dae_model)
            .map_err(|err| lower_problem_context(err, "lower discrete pre modes"))?,
        observation_refresh: inputs.discrete_observation_refresh,
    })
}

fn lower_event_partition_for_profile(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    dynamic_time_event_exprs: &[rumoca_core::Expression],
    model_span: rumoca_core::Span,
    profile: SolveProblemLoweringProfile,
) -> Result<solve::SolveEventPartition, LowerError> {
    if !profile.lower_runtime_systems() {
        return Ok(solve::SolveEventPartition::default());
    }
    Ok(solve::SolveEventPartition {
        root_conditions: solve::ScalarProgramBlock::with_program_spans(
            lower_root_conditions(dae_model, layout)
                .map_err(|err| lower_problem_context(err, "lower root-condition rows"))?,
            root_condition_program_spans(dae_model)?,
        )?,
        root_relation_memory_targets: lower::lower_root_relation_memory_targets(dae_model, layout)
            .map_err(|err| lower_problem_context(err, "lower root relation memory targets"))?,
        root_zero_domains: lower::lower_root_zero_domains(dae_model)
            .map_err(|err| lower_problem_context(err, "lower root zero domains"))?,
        scheduled_root_conditions: lower::lower_scheduled_root_conditions(dae_model)
            .map_err(|err| lower_problem_context(err, "lower scheduled root conditions"))?,
        scheduled_time_events: dae_model.events.scheduled_time_events.clone(),
        dynamic_time_event_names: dynamic_events::collect_dynamic_time_event_names(dae_model),
        dynamic_time_event_rhs: solve::ScalarProgramBlock::with_program_spans(
            lower_dynamic_time_event_rhs(dae_model, layout, dynamic_time_event_exprs)
                .map_err(|err| lower_problem_context(err, "lower dynamic time event rows"))?,
            program_spans_for_expressions(
                dynamic_time_event_exprs,
                "dynamic time event row span count",
                model_span,
            )?,
        )?,
        action_conditions: solve::ScalarProgramBlock::with_program_spans(
            event_actions::lower_event_action_conditions(dae_model, layout)
                .map_err(|err| lower_problem_context(err, "lower event action rows"))?,
            dae_model
                .events
                .event_actions
                .iter()
                .map(|action| action.span)
                .collect(),
        )?,
        actions: event_actions::lower_event_actions(dae_model, layout)
            .map_err(|err| lower_problem_context(err, "lower event actions"))?,
        has_terminal_event: dae_model.events.has_terminal_event,
        delays: lower_delay_partition(dae_model, layout)?,
    })
}

fn lower_delay_partition(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<solve::SolveDelayPartition, LowerError> {
    let channels = &dae_model.events.delay_channels;
    let sources: Vec<_> = channels
        .iter()
        .map(|channel| channel.source.clone())
        .collect();
    let delay_times: Vec<_> = channels
        .iter()
        .map(|channel| channel.delay_time.clone())
        .collect();
    let delay_maxima: Vec<_> = channels
        .iter()
        .map(|channel| {
            channel
                .delay_max
                .clone()
                .unwrap_or_else(|| channel.delay_time.clone())
        })
        .collect();
    let spans: Vec<_> = channels.iter().map(|channel| channel.span).collect();
    let lower_rows = |expressions: &[rumoca_core::Expression],
                      label: &'static str|
     -> Result<solve::ScalarProgramBlock, LowerError> {
        solve::ScalarProgramBlock::with_program_spans(
            lower::lower_expression_rows_from_expressions(
                expressions,
                layout,
                &dae_model.symbols.functions,
            )
            .map_err(|err| lower_problem_context(err, label))?,
            spans.clone(),
        )
        .map_err(LowerError::from)
    };
    let mut value_parameter_indices = Vec::with_capacity(channels.len());
    for channel in channels {
        let Some(solve::ScalarSlot::P { index, .. }) =
            layout.binding(channel.value_parameter.as_str())
        else {
            return Err(lower_contract_violation(
                format!(
                    "delay channel value parameter `{}` is not bound to a Solve P slot",
                    channel.value_parameter
                ),
                channel.span,
            ));
        };
        value_parameter_indices.push(index);
    }

    Ok(solve::SolveDelayPartition {
        source_rhs: lower_rows(&sources, "lower delay source rows")?,
        delay_time_rhs: lower_rows(&delay_times, "lower delay-time rows")?,
        delay_max_rhs: lower_rows(&delay_maxima, "lower delay-maximum rows")?,
        value_parameter_indices,
        source_is_discrete: channels
            .iter()
            .map(|channel| channel.source_is_discrete)
            .collect(),
    })
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
    let timer = timing::stage_start();
    let residual_equations = lower::initial_residual_equations(dae_model, layout)
        .map_err(|err| lower_problem_context(err, "collect initial residual equations"))?;
    let row_targets =
        lower_continuous_row_targets(dae_model, residual_equations.iter().copied(), layout)
            .map_err(|err| lower_problem_context(err, "lower initial row targets"))?;
    let update_equations = lower::initial_condition_update_equations(dae_model, layout)
        .map_err(|err| lower_problem_context(err, "collect initial condition updates"))?;
    let update_targets = lower_update_targets_from_equations(dae_model, layout, &update_equations)
        .map_err(|err| lower_problem_context(err, "lower initial update targets"))?;
    timing::log_stage("init.row_and_update_targets", timer);

    let timer = timing::stage_start();
    let residual_rows = lower_initial_residual(dae_model, layout)
        .map_err(|err| lower_problem_context(err, "lower initial residual rows"))?;
    timing::log_stage("init.lower_initial_residual", timer);
    let timer = timing::stage_start();
    let projection_unknowns =
        initial_projection_unknowns_for_layout(dae_model, layout, solve_layout)?;
    let initialization_span = dae_model_span(dae_model)?;
    let combined_projection_indices = initialization_combined_projection_indices(
        &projection_unknowns,
        solve_layout.solver_scalar_count(),
        initialization_span,
    )?;
    timing::log_stage("init.projection_unknowns", timer);
    let timer = timing::stage_start();
    let combined_projection_plan = lower_projection_plan(
        &residual_rows,
        &row_targets,
        &combined_projection_indices,
        0..residual_rows.len(),
        initialization_span,
        Some(solve_layout.solver_scalar_count()),
    )?;
    let projection_plan = initialization_projection_plan_from_combined(
        combined_projection_plan,
        solve_layout.solver_scalar_count(),
        initialization_span,
    )?;
    timing::log_stage("init.lower_projection_plan", timer);

    // Array-native residual: route through the same structured lowering the
    // continuous system uses, so grid `for`-loop equations (e.g. the immersed-mask
    // `sig[i,j]`) collapse into a few `Map`/`AffineStencil` tensor nodes instead of
    // one scalar program per cell. This is the dominant initialization cost on PDE
    // grids (it was ~80% of the whole Solve-IR before this change).
    // The initialization residual reuses the continuous structured lowering, but
    // the tensor-preservation KPI measures the CONTINUOUS system's nodes only.
    // Journaling initialization declines against continuous family indices would
    // attribute a reason to a family this report never scores, so the
    // initialization pass gets its own journal and it is dropped here.
    let mut initialization_declines = TensorDeclineJournal::new();
    let timer = timing::stage_start();
    let residual = residual_compute_block::build_residual_compute_block(
        dae_model,
        layout,
        &residual_rows,
        &row_targets,
        &residual_equations,
        &mut initialization_declines,
    )?;
    timing::log_stage("init.build_residual_block", timer);
    init_plan_trace::trace_initialization_plan(
        layout,
        &row_targets,
        &projection_plan,
        residual_rows.len(),
    );
    Ok(solve::InitializationSolveSystem {
        row_targets,
        projection_unknowns,
        projection_plan,
        residual,
        update_rhs: solve::ScalarProgramBlock::with_program_spans(
            lower_initial_update_rhs(dae_model, layout)
                .map_err(|err| lower_problem_context(err, "lower initial update rows"))?,
            program_spans_for_owned_equations(&update_equations)?,
        )?,
        update_targets,
    })
}

fn lower_initialization_updates_only(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<solve::InitializationSolveSystem, LowerError> {
    let update_equations = lower::initial_condition_update_equations(dae_model, layout)
        .map_err(|err| lower_problem_context(err, "collect initial condition updates"))?;
    Ok(solve::InitializationSolveSystem {
        update_rhs: solve::ScalarProgramBlock::with_program_spans(
            lower_initial_update_rhs(dae_model, layout)
                .map_err(|err| lower_problem_context(err, "lower initial update rows"))?,
            program_spans_for_owned_equations(&update_equations)?,
        )?,
        update_targets: lower_update_targets_from_equations(dae_model, layout, &update_equations)
            .map_err(|err| {
            lower_problem_context(err, "lower initial update targets")
        })?,
        ..Default::default()
    })
}

fn initial_projection_unknowns_for_layout(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    solve_layout: &solve::SolveLayout,
) -> Result<Vec<solve::ScalarSlot>, LowerError> {
    let span = dae_model_span(dae_model)?;
    let state_count = solve_layout.state_scalar_count();
    let solver_count = solve_layout.solver_scalar_count();
    let non_state_count = solver_count.checked_sub(state_count).ok_or_else(|| {
        lower_contract_violation(
            "initial projection non-state range starts after solver scalar count".to_string(),
            span,
        )
    })?;
    let mut unknowns =
        lower_vec_with_capacity(solver_count, "initial projection index count", span)?;
    reserve_lower_capacity(
        &mut unknowns,
        non_state_count,
        "initial projection non-state index count",
        span,
    )?;
    unknowns.extend((state_count..solver_count).map(solve::scalar_slot_y));
    for (name, var) in dae_model
        .variables
        .states
        .iter()
        .filter(|(_, var)| var.fixed != Some(true))
    {
        let scalar_names = var_scalar_names(name.as_str(), var)?;
        reserve_lower_capacity(
            &mut unknowns,
            scalar_names.len(),
            "initial projection state index count",
            var.source_span,
        )?;
        for scalar_name in scalar_names {
            if let Some(index) = solve_layout.solver_idx_for_target(scalar_name.as_str()) {
                unknowns.push(solve::scalar_slot_y(index));
            }
        }
    }
    for (name, var) in dae_model
        .variables
        .parameters
        .iter()
        .filter(|(_, var)| var.fixed == Some(false))
    {
        let scalar_names = var_scalar_names(name.as_str(), var)?;
        reserve_lower_capacity(
            &mut unknowns,
            scalar_names.len(),
            "initial projection parameter count",
            var.source_span,
        )?;
        for scalar_name in scalar_names {
            let Some(slot @ solve::ScalarSlot::P { .. }) = layout.binding(scalar_name.as_str())
            else {
                return Err(LowerError::MissingBinding { name: scalar_name });
            };
            unknowns.push(slot);
        }
    }
    Ok(unknowns)
}

fn initialization_combined_projection_indices(
    unknowns: &[solve::ScalarSlot],
    p_seed_offset: usize,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut indices = lower_vec_with_capacity(
        unknowns.len(),
        "initial combined projection index count",
        span,
    )?;
    for unknown in unknowns {
        let index = combined_projection_index(*unknown, Some(p_seed_offset)).ok_or_else(|| {
            lower_contract_violation(
                format!("initial projection unknown `{unknown:?}` is not a Y/P slot"),
                span,
            )
        })?;
        indices.push(index);
    }
    Ok(indices)
}

fn combined_projection_index(
    slot: solve::ScalarSlot,
    p_seed_offset: Option<usize>,
) -> Option<usize> {
    match slot {
        solve::ScalarSlot::Y { index, .. } => Some(index),
        solve::ScalarSlot::P { index, .. } => p_seed_offset?.checked_add(index),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => None,
    }
}

fn initialization_projection_plan_from_combined(
    plan: solve::AlgebraicProjectionPlan,
    p_seed_offset: usize,
    span: rumoca_core::Span,
) -> Result<solve::InitializationProjectionPlan, LowerError> {
    let mut blocks =
        lower_vec_with_capacity(plan.blocks.len(), "initial projection block count", span)?;
    for block in plan.blocks {
        let mut unknowns = lower_vec_with_capacity(
            block.y_indices.len(),
            "initial projection block unknown count",
            span,
        )?;
        for index in block.y_indices {
            let unknown = if index < p_seed_offset {
                solve::scalar_slot_y(index)
            } else {
                solve::scalar_slot_p(index - p_seed_offset)
            };
            unknowns.push(unknown);
        }
        blocks.push(solve::InitializationProjectionBlock {
            rows: block.rows,
            unknowns,
        });
    }
    Ok(solve::InitializationProjectionPlan { blocks })
}

pub fn lower_solve_artifacts(
    problem: &solve::SolveProblem,
) -> Result<solve::SolveArtifacts, LowerError> {
    lower_solve_artifacts_with_mass_matrix(problem, solve::MassMatrix::Identity)
}

pub fn lower_solve_artifacts_with_mass_matrix(
    problem: &solve::SolveProblem,
    mass_matrix: solve::MassMatrix,
) -> Result<solve::SolveArtifacts, LowerError> {
    let artifacts = solve::SolveArtifacts {
        continuous: lower_continuous_solve_artifacts(problem, mass_matrix)?,
        initialization: solve::InitializationSolveArtifacts {
            residual_jacobian_v: ad::lower_compute_block_full_jvp(
                &problem.initialization.residual,
                problem.solve_layout.solver_scalar_count(),
            )
            .map_err(|err| lower_problem_context(err, "lower initial residual Jacobian rows"))?,
        },
    };
    appendix_b_validation::validate_solve_artifacts_appendix_b_invariants(&artifacts)?;
    Ok(artifacts)
}

fn lower_continuous_solve_artifacts(
    problem: &solve::SolveProblem,
    mass_matrix: solve::MassMatrix,
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
    let implicit_jacobian_v_scalar = solve::ScalarProgramBlock::with_output_indices(
        lower_scalar_program_block_full_ad_with_spans(
            &implicit_rhs_rows.programs,
            &implicit_rhs_rows.program_spans,
            &problem.layout,
        )
        .map_err(|err| lower_problem_context(err, "lower scalar implicit Jacobian rows"))?,
        implicit_rhs_rows.program_spans,
        implicit_rhs_rows.output_indices,
    )?;
    let derivative_rhs_rows =
        rumoca_eval_solve::to_scalar_program_block(&problem.continuous.derivative_rhs)
            .map_err(|err| lower_problem_context(err.into(), "scalarize derivative RHS rows"))?;
    let full_jacobian_v = solve::ScalarProgramBlock::with_output_indices(
        lower_scalar_program_block_full_ad_with_spans(
            &derivative_rhs_rows.programs,
            &derivative_rhs_rows.program_spans,
            &problem.layout,
        )
        .map_err(|err| lower_problem_context(err, "lower derivative Jacobian rows"))?,
        derivative_rhs_rows.program_spans,
        derivative_rhs_rows.output_indices,
    )?;

    Ok(solve::ContinuousSolveArtifacts {
        mass_matrix,
        implicit_jacobian_v,
        implicit_jacobian_v_scalar,
        full_jacobian_v,
    })
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
        None,
    )
}

fn lower_projection_plan(
    rows: &[Vec<solve::LinearOp>],
    row_targets: &[Option<solve::ScalarSlot>],
    projection_indices: &[usize],
    row_indices: std::ops::Range<usize>,
    context_span: rumoca_core::Span,
    p_seed_offset: Option<usize>,
) -> Result<solve::AlgebraicProjectionPlan, LowerError> {
    let mut row_to_vars = BTreeMap::<usize, BTreeSet<usize>>::new();
    let projection_set = projection_indices.iter().copied().collect::<BTreeSet<_>>();

    for row_idx in row_indices {
        let mut y_indices = collect_projection_indices_for_row(
            rows[row_idx].as_slice(),
            &projection_set,
            p_seed_offset,
            context_span,
        )?;
        if y_indices.is_empty()
            && let Some(index) = row_targets
                .get(row_idx)
                .copied()
                .flatten()
                .and_then(|target| combined_projection_index(target, p_seed_offset))
            && projection_set.contains(&index)
        {
            y_indices.insert(index);
        }
        if y_indices.is_empty() {
            continue;
        }
        row_to_vars.insert(row_idx, y_indices);
    }

    let projection_incidence = algebraic_projection_incidence(
        &row_to_vars,
        row_targets,
        projection_indices,
        context_span,
        p_seed_offset,
    )?;
    let blocks = projection_blt_blocks(&projection_incidence, context_span)?;
    Ok(solve::AlgebraicProjectionPlan {
        blocks: lower_blt_projection_blocks(&blocks, &projection_incidence, context_span)?,
    })
}

fn projection_blt_blocks(
    projection_incidence: &ProjectionIncidence,
    context_span: rumoca_core::Span,
) -> Result<Vec<BltBlock>, LowerError> {
    if projection_incidence.incidence.n_eq == 0 && projection_incidence.incidence.n_var == 0 {
        return Ok(Vec::new());
    }
    let regular = rumoca_phase_structural::maximum_regular_subsystem(
        &projection_incidence.incidence,
        &projection_incidence.preferred_unknowns,
    )
    .map_err(|err| {
        lower_contract_violation(
            format!("failed to select algebraic projection subsystem: {err}"),
            context_span,
        )
    })?;
    Ok(regular.blocks)
}

fn collect_projection_indices_for_row(
    row: &[solve::LinearOp],
    projection_set: &BTreeSet<usize>,
    p_seed_offset: Option<usize>,
    context_span: rumoca_core::Span,
) -> Result<BTreeSet<usize>, LowerError> {
    let mut defs = BTreeMap::<solve::Reg, RowDefUse>::new();
    let mut outputs = Vec::new();
    for op in row {
        match row_def_use(op, context_span)? {
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
        if let (Some(offset), Some(index)) = (p_seed_offset, def_use.loaded_p)
            && let Some(combined) = offset.checked_add(index)
            && projection_set.contains(&combined)
        {
            y_indices.insert(combined);
        }
        if let (Some(offset), Some((base, count))) = (p_seed_offset, def_use.loaded_p_range) {
            // Solve-IR defines a zero-count indexed load as the singleton
            // base slot (the runtime clamps the index to offset zero).
            let effective_count = count.max(1);
            let end = base.checked_add(effective_count).ok_or_else(|| {
                lower_contract_violation(
                    "indexed P-load range exceeds host index range".to_string(),
                    context_span,
                )
            })?;
            let combined_base = offset.checked_add(base).ok_or_else(|| {
                lower_contract_violation(
                    "indexed P-load seed base exceeds host index range".to_string(),
                    context_span,
                )
            })?;
            let combined_end = offset.checked_add(end).ok_or_else(|| {
                lower_contract_violation(
                    "indexed P-load seed end exceeds host index range".to_string(),
                    context_span,
                )
            })?;
            for &combined in projection_set.range(combined_base..combined_end) {
                y_indices.insert(combined);
            }
        }
        stack.extend(def_use.inputs.iter().copied());
    }
    Ok(y_indices)
}

#[derive(Debug)]
struct RowDefUse {
    loaded_y: Option<usize>,
    loaded_p: Option<usize>,
    loaded_p_range: Option<(usize, usize)>,
    inputs: Vec<solve::Reg>,
}

enum RowDefUseOp {
    Def { dst: solve::Reg, def_use: RowDefUse },
    Store { src: solve::Reg },
}

fn row_def_use(
    op: &solve::LinearOp,
    context_span: rumoca_core::Span,
) -> Result<RowDefUseOp, LowerError> {
    use solve::LinearOp as Op;
    Ok(match *op {
        Op::Const { dst, .. } | Op::LoadTime { dst } => def_use(dst, None, Vec::new()),
        Op::LoadP { dst, index } => def_use_slots(dst, None, Some(index), None, Vec::new()),
        Op::LoadY { dst, index } => def_use(dst, Some(index), Vec::new()),
        Op::LoadSeed { dst, .. } => def_use(dst, None, Vec::new()),
        Op::LoadIndexedP {
            dst,
            base,
            count,
            index,
        } => def_use_slots(dst, None, None, Some((base, count)), vec![index]),
        Op::LoadIndexedSeed { dst, index, .. } => def_use(dst, None, vec![index]),
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
        } => {
            let matrix_len = n.checked_mul(n).ok_or_else(|| {
                lower_contract_violation(
                    "projection linear-solve matrix register count overflows".to_string(),
                    context_span,
                )
            })?;
            let mut inputs = projection_reg_range(matrix_start, matrix_len, context_span)?;
            let rhs = projection_reg_range(rhs_start, n, context_span)?;
            reserve_lower_capacity(
                &mut inputs,
                rhs.len(),
                "projection linear-solve input register count",
                context_span,
            )?;
            inputs.extend(rhs);
            def_use(dst, None, inputs)
        }
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
        } => def_use(
            dst,
            None,
            projection_reg_range(state_start, state_len, context_span)?,
        ),
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
    })
}

fn def_use(dst: solve::Reg, loaded_y: Option<usize>, inputs: Vec<solve::Reg>) -> RowDefUseOp {
    def_use_slots(dst, loaded_y, None, None, inputs)
}

fn def_use_slots(
    dst: solve::Reg,
    loaded_y: Option<usize>,
    loaded_p: Option<usize>,
    loaded_p_range: Option<(usize, usize)>,
    inputs: Vec<solve::Reg>,
) -> RowDefUseOp {
    RowDefUseOp::Def {
        dst,
        def_use: RowDefUse {
            loaded_y,
            loaded_p,
            loaded_p_range,
            inputs,
        },
    }
}

fn projection_reg_range(
    start: solve::Reg,
    len: usize,
    span: rumoca_core::Span,
) -> Result<Vec<solve::Reg>, LowerError> {
    let mut registers = lower_vec_with_capacity(len, "projection register dependency count", span)?;
    for offset in 0..len {
        let offset = solve::Reg::try_from(offset).map_err(|_| {
            lower_contract_violation(
                format!("projection register offset {offset} exceeds Solve-IR register range"),
                span,
            )
        })?;
        let register = start.checked_add(offset).ok_or_else(|| {
            lower_contract_violation(
                format!("projection register range starting at {start} overflows"),
                span,
            )
        })?;
        registers.push(register);
    }
    Ok(registers)
}

struct ProjectionIncidence {
    incidence: Incidence,
    unknown_y_indices: Vec<usize>,
    preferred_unknowns: Vec<Option<usize>>,
}

fn algebraic_projection_incidence(
    row_to_vars: &BTreeMap<usize, BTreeSet<usize>>,
    row_targets: &[Option<solve::ScalarSlot>],
    projection_indices: &[usize],
    context_span: rumoca_core::Span,
    p_seed_offset: Option<usize>,
) -> Result<ProjectionIncidence, LowerError> {
    let mut unknown_y_set = row_to_vars
        .values()
        .flat_map(|vars| vars.iter().copied())
        .collect::<BTreeSet<_>>();
    let mut unknown_y_indices = lower_vec_with_capacity(
        unknown_y_set.len(),
        "projection unknown index count",
        context_span,
    )?;
    for y_idx in projection_indices {
        if unknown_y_set.remove(y_idx) {
            unknown_y_indices.push(*y_idx);
        }
    }
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
    let mut preferred_unknowns = lower_vec_with_capacity(
        row_to_vars.len(),
        "projection preferred unknown count",
        context_span,
    )?;
    let mut ordered_rows = lower_vec_with_capacity(
        row_to_vars.len(),
        "projection equation ordering count",
        context_span,
    )?;
    ordered_rows.extend(row_to_vars.iter());
    // Non-evaluable parameters and runtime delay slots are initialization
    // unknowns with owning initial equations. Give those P-target equations
    // first causal-preference priority, so a rectangular initialization system
    // leaves genuinely free state starts unmatched instead of dropping a P
    // owner and pinning a transport-history slot to an arbitrary seed.
    ordered_rows.sort_unstable_by_key(|(row_idx, _)| {
        let parameter_target = matches!(
            row_targets.get(**row_idx).copied().flatten(),
            Some(solve::ScalarSlot::P { .. })
        );
        (!parameter_target, **row_idx)
    });
    for (row_idx, vars) in ordered_rows {
        equation_refs.push(EquationRef(*row_idx));
        let mut unknowns =
            lower_hash_set_with_capacity(vars.len(), "projection row unknown count", context_span)?;
        for y_idx in vars {
            if let Some(local_idx) = unknown_positions.get(y_idx).copied() {
                unknowns.insert(local_idx);
            }
        }
        eq_unknowns.push(unknowns);
        preferred_unknowns.push(
            row_targets
                .get(*row_idx)
                .copied()
                .flatten()
                .and_then(|target| {
                    combined_projection_index(target, p_seed_offset)
                        .and_then(|index| unknown_positions.get(&index).copied())
                })
                .filter(|local_idx| vars.contains(&unknown_y_indices[*local_idx])),
        );
    }

    Ok(ProjectionIncidence {
        incidence: Incidence::new(eq_unknowns, equation_refs, unknown_names),
        unknown_y_indices,
        preferred_unknowns,
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
                let y_index =
                    projection_y_index(unknown, projection_incidence).ok_or_else(|| {
                        lower_contract_violation(
                            format!("projection BLT unknown `{unknown}` has no solver-y index"),
                            context_span,
                        )
                    })?;
                scalar_projection_block(equation.0, y_index, context_span)?
            }
            BltBlock::AlgebraicLoop {
                equations,
                unknowns,
            } => lower_algebraic_loop_projection_block(
                equations,
                unknowns,
                projection_incidence,
                context_span,
            )?,
            // The projection incidence is built through `Incidence::new`, which
            // leaves the structured-matching descriptors empty, so no compact
            // family block can reach Solve lowering. Treat it as a hard
            // contract violation rather than dropping the family's rows.
            BltBlock::StructuredScalar(structured) => {
                return Err(lower_contract_violation(
                    structured.unsupported_by("projection BLT lowering"),
                    context_span,
                ));
            }
        };
        lowered.push(block);
    }
    Ok(lowered)
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
    Ok(solve::AlgebraicProjectionBlock { rows, y_indices })
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
    projection_incidence: &ProjectionIncidence,
    context_span: rumoca_core::Span,
) -> Result<solve::AlgebraicProjectionBlock, LowerError> {
    let rows = collect_equation_rows(equations, context_span)?;
    let mut loop_y_indices = lower_vec_with_capacity(
        unknowns.len(),
        "algebraic loop projection target count",
        context_span,
    )?;
    for unknown in unknowns {
        let y_index = projection_y_index(unknown, projection_incidence).ok_or_else(|| {
            lower_contract_violation(
                format!("projection BLT unknown `{unknown}` has no solver-y index"),
                context_span,
            )
        })?;
        loop_y_indices.push(y_index);
    }
    if rows.len() != loop_y_indices.len() {
        return Err(lower_contract_violation(
            format!(
                "projection BLT block has {} equations but {} solver-y unknowns",
                rows.len(),
                loop_y_indices.len()
            ),
            context_span,
        ));
    }
    let mut y_indices = loop_y_indices.clone();
    y_indices.sort_unstable();
    Ok(solve::AlgebraicProjectionBlock { rows, y_indices })
}

mod solver_names;
pub use solver_names::{build_solver_name_index_maps, solver_vector_names};
use solver_names::{collect_scalar_names, scalar_count, var_scalar_names, variable_size};

mod discrete_layout;
use discrete_layout::*;

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
