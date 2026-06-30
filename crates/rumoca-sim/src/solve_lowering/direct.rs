//! Guarded direct DAE -> Solve IR lowering for models that are already in an
//! explicit runtime shape. Anything outside this narrow proof falls back to the
//! structural Modelica path in [`super::entry`].

use std::collections::{BTreeSet, HashMap};

use rumoca_core::{Expression, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::{SimOptions, SimSolverMode};

use super::structural_lowering::metadata_attachment_lower_error;

pub(super) fn lower_direct_dae_for_simulation(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    param_overrides: &HashMap<String, f64>,
) -> Result<Option<solve::SolveModel>, rumoca_phase_solve::SolveModelLowerError> {
    if opts.solver_mode != SimSolverMode::RkLike {
        return Ok(None);
    }

    let metadata_dae = attach_reference_metadata(dae_model)?;
    if let Some(reason) = projected_slot_rejection(&metadata_dae) {
        trace_direct_rejection(reason);
        return Ok(None);
    }
    if let Some(reason) = direct_state_value_assignment_rejection(&metadata_dae) {
        trace_direct_rejection(reason);
        return Ok(None);
    }
    let visible_expressions = match rumoca_phase_solve::visible_expressions_for_dae(&metadata_dae) {
        Ok(expressions) => expressions,
        Err(err) => {
            trace_direct_rejection(format!("visible expression lowering failed: {err}"));
            return Ok(None);
        }
    };
    let lowered = metadata_dae.clone();
    let solve_model =
        match rumoca_phase_solve::lower_dae_to_solve_model_owned_value_only_with_visible_expressions_and_metadata_and_overrides(
            lowered,
            visible_expressions,
            &metadata_dae,
            param_overrides,
        ) {
            Ok(model) => model,
            Err(err) => {
                trace_direct_rejection(format!("direct solve lowering failed: {err}"));
                return Ok(None);
            }
        };

    match validate_direct_runtime_model(&solve_model) {
        Ok(()) => Ok(Some(solve_model)),
        Err(reason) => {
            trace_direct_rejection(reason);
            Ok(None)
        }
    }
}

pub(super) fn lower_direct_dae_for_gpu_preparation(
    dae_model: &dae::Dae,
) -> Result<Option<solve::SolveModel>, rumoca_phase_solve::SolveModelLowerError> {
    let metadata_dae = attach_reference_metadata(dae_model)?;
    let lowered = metadata_dae.clone();
    match rumoca_phase_solve::lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata(
        lowered,
        &metadata_dae,
    ) {
        Ok(solve_model) => Ok(Some(solve_model)),
        Err(err) => {
            trace_direct_rejection(format!("direct GPU-preparation lowering failed: {err}"));
            Ok(None)
        }
    }
}

fn attach_reference_metadata(
    dae_model: &dae::Dae,
) -> Result<dae::Dae, rumoca_phase_solve::SolveModelLowerError> {
    let mut metadata_dae = dae_model.clone();
    rumoca_phase_dae::attach_dae_reference_metadata(&mut metadata_dae)
        .map_err(metadata_attachment_lower_error)?;
    Ok(metadata_dae)
}

fn projected_slot_rejection(dae_model: &dae::Dae) -> Option<String> {
    if !dae_model.variables.algebraics.is_empty() {
        return Some(format!(
            "model has {} algebraic variables",
            dae_model.variables.algebraics.len()
        ));
    }
    if !dae_model.variables.outputs.is_empty() {
        return Some(format!(
            "model has {} output variables",
            dae_model.variables.outputs.len()
        ));
    }
    None
}

fn direct_state_value_assignment_rejection(dae_model: &dae::Dae) -> Option<String> {
    let state_names = dae_model
        .variables
        .states
        .keys()
        .cloned()
        .collect::<Vec<_>>();
    if state_names.is_empty() {
        return None;
    }
    for (eq_idx, eq) in dae_model.continuous.equations.iter().enumerate() {
        if let Some(lhs) = &eq.lhs
            && state_names
                .iter()
                .any(|state_name| lhs.var_name() == state_name)
        {
            return Some(format!(
                "continuous equation {eq_idx} directly assigns state `{}`",
                lhs.as_str()
            ));
        }
        if let Some(state_name) = residual_direct_state_assignment(&eq.rhs, &state_names) {
            return Some(format!(
                "continuous residual equation {eq_idx} directly assigns state `{}`",
                state_name.as_str()
            ));
        }
    }
    None
}

fn residual_direct_state_assignment<'a>(
    expr: &'a Expression,
    state_names: &'a [VarName],
) -> Option<&'a VarName> {
    match expr {
        Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            if let Some(state_name) = direct_state_ref(lhs, state_names)
                && !dae::expr_contains_der_of(rhs, state_name)
            {
                return Some(state_name);
            }
            if let Some(state_name) = direct_state_ref(rhs, state_names)
                && !dae::expr_contains_der_of(lhs, state_name)
            {
                return Some(state_name);
            }
            None
        }
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs,
            ..
        } => residual_direct_state_assignment(rhs, state_names),
        _ => None,
    }
}

fn direct_state_ref<'a>(expr: &'a Expression, state_names: &'a [VarName]) -> Option<&'a VarName> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    state_names
        .iter()
        .find(|state_name| dae::var_ref_matches_unknown(name, subscripts, state_name))
}

fn validate_direct_runtime_model(model: &solve::SolveModel) -> Result<(), String> {
    let state_count = model.state_scalar_count();
    if state_count == 0 {
        return Err("model has no states".to_string());
    }

    let derivative_rhs_len = model
        .problem
        .continuous
        .derivative_rhs
        .len()
        .map_err(|err| err.to_string())?;
    if derivative_rhs_len != state_count {
        return Err(format!(
            "derivative RHS has {derivative_rhs_len} rows for {state_count} state scalars"
        ));
    }

    validate_tail_residual_targets(model)?;
    validate_no_projected_derivative_dependencies(model)
}

fn validate_tail_residual_targets(model: &solve::SolveModel) -> Result<(), String> {
    let state_count = model.state_scalar_count();
    let implicit_rhs_len = model
        .problem
        .continuous
        .implicit_rhs
        .len()
        .map_err(|err| err.to_string())?;
    if model.problem.continuous.implicit_row_targets.len() != implicit_rhs_len {
        return Err(format!(
            "implicit row target count {} does not match implicit RHS row count {implicit_rhs_len}",
            model.problem.continuous.implicit_row_targets.len()
        ));
    }
    if implicit_rhs_len < state_count {
        return Err(format!(
            "implicit RHS has {implicit_rhs_len} rows for {state_count} state scalars"
        ));
    }

    for (row_idx, target) in model
        .problem
        .continuous
        .implicit_row_targets
        .iter()
        .enumerate()
        .skip(state_count)
    {
        match target {
            Some(solve::ScalarSlot::Y { index, .. }) if *index < state_count => {
                return Err(format!(
                    "implicit residual row {row_idx} targets state Y[{index}]"
                ));
            }
            Some(solve::ScalarSlot::Y { .. }) => {}
            Some(other) => {
                return Err(format!(
                    "implicit residual row {row_idx} targets non-solver slot {other:?}"
                ));
            }
            None => {
                return Err(format!("implicit residual row {row_idx} has no target"));
            }
        }
    }
    Ok(())
}

fn validate_no_projected_derivative_dependencies(model: &solve::SolveModel) -> Result<(), String> {
    let derivative_rows =
        rumoca_eval_solve::to_scalar_program_block(&model.problem.continuous.derivative_rhs)
            .map_err(|err| err.to_string())?;
    let direct_deps = derivative_non_state_loads(model, &derivative_rows);
    if !direct_deps.is_empty() {
        return Err(format!(
            "derivative RHS reads projected non-state Y slots {:?}",
            direct_deps.into_iter().collect::<Vec<_>>()
        ));
    }
    Ok(())
}

fn derivative_non_state_loads(
    model: &solve::SolveModel,
    derivative_rows: &solve::ScalarProgramBlock,
) -> BTreeSet<usize> {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    derivative_rows
        .programs
        .iter()
        .take(state_count)
        .flat_map(|row| non_state_y_loads(row, state_count, solver_count))
        .collect()
}

fn non_state_y_loads(
    row: &[solve::LinearOp],
    state_count: usize,
    solver_count: usize,
) -> Vec<usize> {
    let mut loads = row
        .iter()
        .filter_map(|op| match *op {
            solve::LinearOp::LoadY { index, .. }
                if index >= state_count && index < solver_count =>
            {
                Some(index)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    loads.sort_unstable();
    loads.dedup();
    loads
}

fn trace_direct_rejection(reason: impl AsRef<str>) {
    tracing::debug!(
        target: "rumoca_sim::solve_lowering",
        "direct simulation lowering rejected: {}",
        reason.as_ref()
    );
}
