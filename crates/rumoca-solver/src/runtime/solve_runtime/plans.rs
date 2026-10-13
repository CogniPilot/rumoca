use rumoca_ir_solve as solve;
use rustc_hash::{FxHashMap, FxHasher};
use std::hash::Hasher;
use std::io::{self, Write};

use crate::RuntimeSolveError;
use rumoca_eval_solve::EvalSolveError;

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
pub(super) struct DirectTimeRoot {
    param_index: usize,
    sign: solve::TimeRootSign,
    span: Option<rumoca_core::Span>,
}

#[derive(Clone, Copy)]
pub(super) enum RootConditionPlanEntry {
    DirectTime(DirectTimeRoot),
    ContinuousStatic,
    Dynamic,
}

#[derive(Clone)]
pub(super) struct RootConditionPlan {
    pub(super) entries: Vec<RootConditionPlanEntry>,
    pub(super) evaluated_rows: Vec<usize>,
    pub(super) search_rows: Vec<usize>,
}

pub(super) fn visible_value_plan(
    rows: &solve::ScalarProgramBlock,
    visible_count: usize,
) -> Option<VisibleValuePlan> {
    if rows.row_count() != visible_count
        || rows.output_count() != visible_count
        || !rows.uses_local_contiguous_output_indices()
    {
        return None;
    }
    let mut entries = Vec::with_capacity(rows.row_count());
    let mut expression_rows = Vec::new();
    let mut expression_groups = Vec::new();
    let mut expression_groups_by_fingerprint = FxHashMap::<u64, Vec<usize>>::default();
    for (row_idx, row) in rows.programs().iter().enumerate() {
        let output_count = solve::ScalarProgramBlock::program_output_count(row);
        if output_count != 1 {
            return None;
        }
        if let Some(source) = direct_visible_source(row, rows.program_span(row_idx)) {
            entries.push(VisibleValuePlanEntry::Direct(source));
            continue;
        }
        entries.push(VisibleValuePlanEntry::Expression);
        let fingerprint = visible_program_fingerprint(row)?;
        match visible_expression_group_index(
            rows,
            &expression_groups,
            expression_groups_by_fingerprint
                .get(&fingerprint)
                .map_or(&[], Vec::as_slice),
            row,
        ) {
            Some(group_idx) => expression_groups[group_idx].output_indices.push(row_idx),
            None => {
                expression_rows.push(row_idx);
                let group_index = expression_groups.len();
                expression_groups.push(VisibleExpressionGroup {
                    row_index: row_idx,
                    output_indices: vec![row_idx],
                });
                expression_groups_by_fingerprint
                    .entry(fingerprint)
                    .or_default()
                    .push(group_index);
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
    if !roots.uses_local_contiguous_output_indices() {
        return None;
    }
    let search = solve::RootSearchPlan::derive(&model.problem);
    let mut entries = Vec::with_capacity(search.len());
    let mut evaluated_rows = Vec::new();
    let mut search_rows = Vec::new();
    let mut output = 0;
    for (program, row) in roots.programs().iter().enumerate() {
        let span = roots.program_span(program);
        let first = output;
        output += solve::ScalarProgramBlock::program_output_count(row);
        for index in first..output {
            let role = *search.roles().get(index)?;
            entries.push(match role {
                solve::RootSearchRole::AnnouncedTime { param_index, sign } => {
                    RootConditionPlanEntry::DirectTime(DirectTimeRoot {
                        param_index,
                        sign,
                        span,
                    })
                }
                solve::RootSearchRole::Static => {
                    evaluated_rows.push(index);
                    RootConditionPlanEntry::ContinuousStatic
                }
                solve::RootSearchRole::Search => {
                    evaluated_rows.push(index);
                    search_rows.push(index);
                    RootConditionPlanEntry::Dynamic
                }
            });
        }
    }
    if output != search.len() {
        return None;
    }
    tracing::debug!(
        target: "rumoca_solver::root_plan",
        roots = entries.len(),
        evaluated = evaluated_rows.len(),
        search = search_rows.len(),
        scheduled = model.problem.events.scheduled_root_conditions.len(),
        "root condition execution plan"
    );
    Some(RootConditionPlan {
        entries,
        evaluated_rows,
        search_rows,
    })
}

fn visible_expression_group_index(
    rows: &solve::ScalarProgramBlock,
    groups: &[VisibleExpressionGroup],
    candidates: &[usize],
    row: &[solve::LinearOp],
) -> Option<usize> {
    candidates
        .iter()
        .copied()
        .find(|&group| rows.programs()[groups[group].row_index].as_slice() == row)
}

/// Hash a checked program without allocating a second encoded buffer.
///
/// The fingerprint is only a lookup accelerator: a bucket hit is always
/// confirmed by exact `LinearOp` slice equality above, so neither a collision
/// nor a future wire-format change can become semantic identity.
fn visible_program_fingerprint(row: &[solve::LinearOp]) -> Option<u64> {
    let mut hasher = FxHasher::default();
    serde_json::to_writer(HasherWriter(&mut hasher), row).ok()?;
    Some(hasher.finish())
}

struct HasherWriter<'a>(&'a mut FxHasher);

impl Write for HasherWriter<'_> {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        self.0.write(bytes);
        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
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
    Ok(match root.sign {
        solve::TimeRootSign::ParamMinusTime => event_time - t,
        solve::TimeRootSign::TimeMinusParam => t - event_time,
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

/// Total root-condition count: the model's own conditions plus the roots the
/// delay runtime schedules.
pub(super) fn total_root_condition_count(
    model: &solve::SolveModel,
    delay_event_roots: usize,
) -> Result<usize, EvalSolveError> {
    model
        .problem
        .events
        .root_conditions
        .len()
        .checked_add(delay_event_roots)
        .ok_or_else(|| EvalSolveError::ShapeContract {
            message: "combined model and delay root count exceeds host index range".to_string(),
            span: None,
        })
}
