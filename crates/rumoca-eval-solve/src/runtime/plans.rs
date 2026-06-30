use rumoca_ir_solve as solve;
use rumoca_solver::RuntimeSolveError;

use crate::{self as solve_eval, RowEvalContext};

#[derive(Clone, Copy)]
pub(super) enum DirectVisibleSource {
    Time,
    SolverY {
        index: usize,
        span: Option<rumoca_core::Span>,
    },
    Param {
        index: usize,
        span: Option<rumoca_core::Span>,
    },
}

#[derive(Clone, Copy)]
pub(super) enum VisibleValuePlanEntry {
    Direct(DirectVisibleSource),
    Expression,
}

#[derive(Clone)]
pub(super) struct VisibleExpressionGroup {
    pub(super) row_index: usize,
    pub(super) output_indices: Vec<usize>,
}

#[derive(Clone)]
pub(super) struct VisibleValuePlan {
    pub(super) entries: Vec<VisibleValuePlanEntry>,
    pub(super) expression_rows: Vec<usize>,
    pub(super) expression_groups: Vec<VisibleExpressionGroup>,
}

#[derive(Clone, Copy)]
enum DirectTimeRootKind {
    ParamMinusTime,
    TimeMinusParam,
}

#[derive(Clone, Copy)]
pub(super) struct DirectTimeRoot {
    param_index: usize,
    kind: DirectTimeRootKind,
    span: Option<rumoca_core::Span>,
}

#[derive(Clone, Copy)]
pub(super) enum RootConditionPlanEntry {
    ConstantNonZero(f64),
    DirectTime(DirectTimeRoot),
    StaticParameter,
    Dynamic,
}

#[derive(Clone)]
pub(super) struct RootConditionPlan {
    pub(super) entries: Vec<RootConditionPlanEntry>,
    pub(super) evaluated_rows: Vec<usize>,
    pub(super) search_rows: Vec<usize>,
}

pub(super) fn visible_value_plan(model: &solve::SolveModel) -> Option<VisibleValuePlan> {
    let rows = &model.visible_value_rows;
    if rows.row_count() != model.visible_names.len()
        || rows.output_count() != model.visible_names.len()
        || !rows.uses_local_contiguous_output_indices()
    {
        return None;
    }
    let mut entries = Vec::with_capacity(rows.row_count());
    let mut expression_rows = Vec::new();
    let mut expression_groups = Vec::new();
    for (row_idx, row) in rows.programs.iter().enumerate() {
        let output_count = solve::ScalarProgramBlock::program_output_count(row);
        if output_count != 1 {
            return None;
        }
        if let Some(source) = direct_visible_source(row, rows.program_span(row_idx)) {
            entries.push(VisibleValuePlanEntry::Direct(source));
            continue;
        }
        entries.push(VisibleValuePlanEntry::Expression);
        match visible_expression_group_index(rows, &expression_groups, row) {
            Some(group_idx) => expression_groups[group_idx].output_indices.push(row_idx),
            None => {
                expression_rows.push(row_idx);
                expression_groups.push(VisibleExpressionGroup {
                    row_index: row_idx,
                    output_indices: vec![row_idx],
                });
            }
        }
    }
    Some(VisibleValuePlan {
        entries,
        expression_rows,
        expression_groups,
    })
}

pub(super) fn root_condition_plan(model: &solve::SolveModel) -> Option<RootConditionPlan> {
    let roots = &model.problem.events.root_conditions;
    if roots.row_count() != roots.output_count() || !roots.uses_local_contiguous_output_indices() {
        return None;
    }
    let mut entries = Vec::with_capacity(roots.row_count());
    let mut evaluated_rows = Vec::new();
    let mut search_rows = Vec::new();
    for (row_idx, row) in roots.programs.iter().enumerate() {
        if solve::ScalarProgramBlock::program_output_count(row) != 1 {
            return None;
        }
        let span = roots.program_span(row_idx);
        if let Some(root) = direct_time_root(row, span) {
            entries.push(RootConditionPlanEntry::DirectTime(root));
            continue;
        }
        if let Some(value) = constant_nonzero_root_value(row) {
            entries.push(RootConditionPlanEntry::ConstantNonZero(value));
            continue;
        }
        if parameter_static_root(row) {
            entries.push(RootConditionPlanEntry::StaticParameter);
            evaluated_rows.push(row_idx);
            continue;
        }
        entries.push(RootConditionPlanEntry::Dynamic);
        evaluated_rows.push(row_idx);
        search_rows.push(row_idx);
    }
    Some(RootConditionPlan {
        entries,
        evaluated_rows,
        search_rows,
    })
}

fn direct_time_root(
    row: &[solve::LinearOp],
    span: Option<rumoca_core::Span>,
) -> Option<DirectTimeRoot> {
    let [
        first_load,
        second_load,
        solve::LinearOp::Binary {
            dst,
            op: solve::BinaryOp::Sub,
            lhs,
            rhs,
        },
        solve::LinearOp::StoreOutput { src },
    ] = row
    else {
        return None;
    };
    if dst != src {
        return None;
    }
    let (time_reg, param_reg, param_index) = time_and_param_loads(first_load, second_load)?;
    if *lhs == param_reg && *rhs == time_reg {
        return Some(DirectTimeRoot {
            param_index,
            kind: DirectTimeRootKind::ParamMinusTime,
            span,
        });
    }
    if *lhs == time_reg && *rhs == param_reg {
        return Some(DirectTimeRoot {
            param_index,
            kind: DirectTimeRootKind::TimeMinusParam,
            span,
        });
    }
    None
}

fn time_and_param_loads(
    first: &solve::LinearOp,
    second: &solve::LinearOp,
) -> Option<(solve::Reg, solve::Reg, usize)> {
    match (first, second) {
        (
            solve::LinearOp::LoadTime { dst: time_reg },
            solve::LinearOp::LoadP {
                dst: param_reg,
                index,
            },
        )
        | (
            solve::LinearOp::LoadP {
                dst: param_reg,
                index,
            },
            solve::LinearOp::LoadTime { dst: time_reg },
        ) => Some((*time_reg, *param_reg, *index)),
        _ => None,
    }
}

fn constant_nonzero_root_value(row: &[solve::LinearOp]) -> Option<f64> {
    if !row.iter().all(constant_root_op_allowed) {
        return None;
    }
    let value =
        solve_eval::eval_row_with_context(row, &[], &[], 0.0, RowEvalContext::default()).ok()?;
    (value.is_finite() && value != 0.0).then_some(value)
}

fn constant_root_op_allowed(op: &solve::LinearOp) -> bool {
    matches!(
        op,
        solve::LinearOp::Const { .. }
            | solve::LinearOp::Move { .. }
            | solve::LinearOp::Unary { .. }
            | solve::LinearOp::Binary { .. }
            | solve::LinearOp::Compare { .. }
            | solve::LinearOp::Select { .. }
            | solve::LinearOp::StoreOutput { .. }
    )
}

fn parameter_static_root(row: &[solve::LinearOp]) -> bool {
    row.iter().all(parameter_static_root_op_allowed)
}

fn parameter_static_root_op_allowed(op: &solve::LinearOp) -> bool {
    matches!(
        op,
        solve::LinearOp::Const { .. }
            | solve::LinearOp::LoadP { .. }
            | solve::LinearOp::Move { .. }
            | solve::LinearOp::Unary { .. }
            | solve::LinearOp::Binary { .. }
            | solve::LinearOp::Compare { .. }
            | solve::LinearOp::Select { .. }
            | solve::LinearOp::StoreOutput { .. }
    )
}

fn visible_expression_group_index(
    rows: &solve::ScalarProgramBlock,
    groups: &[VisibleExpressionGroup],
    row: &[solve::LinearOp],
) -> Option<usize> {
    groups
        .iter()
        .position(|group| rows.programs[group.row_index].as_slice() == row)
}

fn direct_visible_source(
    row: &[solve::LinearOp],
    span: Option<rumoca_core::Span>,
) -> Option<DirectVisibleSource> {
    match row {
        [
            solve::LinearOp::LoadTime { dst },
            solve::LinearOp::StoreOutput { src },
        ] if dst == src => Some(DirectVisibleSource::Time),
        [
            solve::LinearOp::LoadY { dst, index },
            solve::LinearOp::StoreOutput { src },
        ] if dst == src => Some(DirectVisibleSource::SolverY {
            index: *index,
            span,
        }),
        [
            solve::LinearOp::LoadP { dst, index },
            solve::LinearOp::StoreOutput { src },
        ] if dst == src => Some(DirectVisibleSource::Param {
            index: *index,
            span,
        }),
        _ => None,
    }
}

pub(super) fn direct_visible_value(
    source: DirectVisibleSource,
    y: &[f64],
    params: &[f64],
    t: f64,
) -> Result<f64, RuntimeSolveError> {
    match source {
        DirectVisibleSource::Time => Ok(t),
        DirectVisibleSource::SolverY { index, span } => y
            .get(index)
            .copied()
            .ok_or_else(|| missing_direct_visible_input("y", index, span)),
        DirectVisibleSource::Param { index, span } => params
            .get(index)
            .copied()
            .ok_or_else(|| missing_direct_visible_input("p", index, span)),
    }
}

fn missing_direct_visible_input(
    input: &'static str,
    index: usize,
    span: Option<rumoca_core::Span>,
) -> RuntimeSolveError {
    RuntimeSolveError::solve_ir_with_span(format!("missing {input}[{index}]"), span)
}

pub(super) fn direct_time_root_value(
    root: DirectTimeRoot,
    params: &[f64],
    t: f64,
) -> Result<f64, RuntimeSolveError> {
    let event_time = direct_time_root_time(root, params)?;
    Ok(match root.kind {
        DirectTimeRootKind::ParamMinusTime => event_time - t,
        DirectTimeRootKind::TimeMinusParam => t - event_time,
    })
}

pub(super) fn direct_time_root_search_default(
    root: DirectTimeRoot,
    params: &[f64],
    t: f64,
) -> Result<f64, RuntimeSolveError> {
    let value = direct_time_root_value(root, params, t)?;
    Ok(if value.is_finite() { 1.0 } else { value })
}

pub(super) fn direct_time_root_time(
    root: DirectTimeRoot,
    params: &[f64],
) -> Result<f64, RuntimeSolveError> {
    params
        .get(root.param_index)
        .copied()
        .ok_or_else(|| missing_direct_visible_input("p", root.param_index, root.span))
}

pub(super) fn copy_grouped_expression_values(
    plan: &VisibleValuePlan,
    values: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    for group in &plan.expression_groups {
        let value = values
            .get(group.row_index)
            .copied()
            .ok_or_else(|| visible_plan_output_index_error(group.row_index, values.len()))?;
        for &output_index in &group.output_indices {
            let len = values.len();
            let slot = values
                .get_mut(output_index)
                .ok_or_else(|| visible_plan_output_index_error(output_index, len))?;
            *slot = value;
        }
    }
    Ok(())
}

pub(super) fn visible_plan_output_index_error(index: usize, len: usize) -> RuntimeSolveError {
    RuntimeSolveError::solve_ir(format!(
        "visible value plan output index {index} out of bounds for {len} values"
    ))
}
