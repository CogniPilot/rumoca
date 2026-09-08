use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;
use rustc_hash::{FxHashMap, FxHasher};
use std::collections::BTreeSet;
use std::hash::Hasher;
use std::io::{self, Write};

use crate::RuntimeSolveError;
use rumoca_eval_solve::{EvalSolveError, PreparedComputeBlock};

use super::PreparationConstantRootsPermit;

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
    ContinuousStatic,
    Dynamic,
}

#[derive(Clone)]
pub(super) struct RootConditionPlan {
    pub(super) entries: Vec<RootConditionPlanEntry>,
    pub(super) evaluated_rows: Vec<usize>,
    pub(super) search_rows: Vec<usize>,
}

pub(super) fn visible_value_plan(model: &solve::SolveModel) -> Option<VisibleValuePlan> {
    let rows = model.visible_value_rows();
    if rows.row_count() != model.visible_name_count()
        || rows.output_count() != model.visible_name_count()
        || !rows.uses_local_contiguous_output_indices()
    {
        return None;
    }
    let mut entries = Vec::with_capacity(rows.row_count());
    let mut expression_rows = Vec::new();
    let mut expression_groups = Vec::new();
    let mut expression_groups_by_fingerprint = FxHashMap::<u64, Vec<usize>>::default();
    for (row_idx, row) in rows.programs().iter().enumerate() {
        let output_count = rows.stored_output_count_for_program(row_idx)?;
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

pub(super) fn root_condition_plan(
    model: &solve::SolveModel,
    root_refresh: &solve::IssuedRefreshPlan,
    preparation: PreparationConstantRootsPermit,
) -> Result<Option<RootConditionPlan>, RuntimeSolveError> {
    let roots = &model.problem().events().root_conditions;
    if !roots.uses_local_contiguous_output_indices() {
        return Ok(None);
    }
    let mut entries = Vec::with_capacity(roots.output_count());
    let mut evaluated_rows = Vec::new();
    let mut search_rows = Vec::new();
    let static_y = root_refresh
        .static_causal_rows()
        .iter()
        .map(|row| row.target_index())
        .collect::<BTreeSet<_>>();
    let mut output = 0;
    for (program, row) in roots.programs().iter().enumerate() {
        let Some(output_count) = roots.stored_output_count_for_program(program) else {
            return Ok(None);
        };
        if output_count == 0 {
            return Ok(None);
        }
        let span = roots.program_span(program);
        if output_count == 1
            && let Some(root) = direct_time_root(row, span)
        {
            entries.push(RootConditionPlanEntry::DirectTime(root));
            output += 1;
            continue;
        }
        if output_count == 1
            && let Some(value) = constant_nonzero_root_value(row, model, preparation)?
        {
            entries.push(RootConditionPlanEntry::ConstantNonZero(value));
            output += 1;
            continue;
        }
        if continuous_static_root(row, &static_y) {
            for index in output..output + output_count {
                entries.push(RootConditionPlanEntry::ContinuousStatic);
                evaluated_rows.push(index);
            }
            output += output_count;
            continue;
        }
        for index in output..output + output_count {
            entries.push(RootConditionPlanEntry::Dynamic);
            evaluated_rows.push(index);
            search_rows.push(index);
        }
        output += output_count;
    }
    if output != roots.output_count() {
        return Ok(None);
    }
    tracing::debug!(
        target: "rumoca_solver::root_plan",
        roots = entries.len(),
        evaluated = evaluated_rows.len(),
        search = search_rows.len(),
        scheduled = model.problem().events().scheduled_root_conditions.len(),
        "root condition execution plan"
    );
    Ok(Some(RootConditionPlan {
        entries,
        evaluated_rows,
        search_rows,
    }))
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

fn constant_nonzero_root_value(
    row: &[solve::LinearOp],
    model: &solve::SolveModel,
    preparation: PreparationConstantRootsPermit,
) -> Result<Option<f64>, RuntimeSolveError> {
    if !row.iter().all(constant_root_op_allowed) {
        return Ok(None);
    }
    let value = solve_eval::eval_row_with_context(
        row,
        &[],
        &[],
        0.0,
        preparation.row_eval_context_for_model(model),
    )?;
    Ok((value.is_finite() && value != 0.0).then_some(value))
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

fn continuous_static_root(row: &[solve::LinearOp], static_y: &BTreeSet<usize>) -> bool {
    let Ok(y_dependencies) = solve::StructuralPattern::derive_output_y_dependencies(row, None)
    else {
        return false;
    };
    if y_dependencies
        .iter()
        .any(|dependencies| !dependencies.is_subset(static_y))
    {
        return false;
    }
    // Every P slot is fixed during one accepted FMI Model Exchange interval.
    // Inputs and discrete values may mutate only between accepted intervals,
    // where the host refreshes retained indicators before continuous search
    // resumes. Keep the full Event Mode value, but do not expose a P-only
    // surface to the continuous root finder.
    solve::StructuralPattern::derive_output_p_dependencies(row, None).is_ok()
        && output_dependencies_are_empty(solve::StructuralPattern::derive_output_time_dependencies(
            row, None,
        ))
        && output_dependencies_are_empty(solve::StructuralPattern::derive_output_seed_dependencies(
            row, None,
        ))
        && output_dependencies_are_empty(
            solve::StructuralPattern::derive_output_effect_dependencies(row, None),
        )
}

fn output_dependencies_are_empty(
    dependencies: Result<Vec<bool>, solve::StructuralPatternError>,
) -> bool {
    dependencies.is_ok_and(|dependencies| dependencies.iter().all(|depends| !depends))
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

/// Prepare the manifold residual and its Jacobian-vector product.
pub(super) fn prepare_manifold_projection_programs(
    model: &solve::SolveModel,
) -> Result<(PreparedComputeBlock, PreparedComputeBlock), EvalSolveError> {
    Ok((
        PreparedComputeBlock::new_with_label(
            model.problem().continuous().manifold_residual(),
            "runtime_manifold_residual",
        )?,
        PreparedComputeBlock::new_with_label(
            &model.artifacts().continuous().manifold_jacobian_v,
            "runtime_manifold_jacobian_v",
        )?,
    ))
}

/// Total root-condition count: the model's own conditions plus the roots the
/// delay runtime schedules.
pub(super) fn total_root_condition_count(
    model: &solve::SolveModel,
    delay_event_roots: usize,
) -> Result<usize, EvalSolveError> {
    model
        .problem()
        .events()
        .root_conditions
        .len()
        .checked_add(delay_event_roots)
        .ok_or_else(|| EvalSolveError::ShapeContract {
            message: "combined model and delay root count exceeds host index range".to_string(),
            span: None,
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn continuous_static_root_uses_certified_tensor_dependency_flow() {
        let parameter_tensor_root = vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::P,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            solve::LinearOp::MatrixMultiply {
                dst_start: 2,
                lhs_start: 0,
                rhs_start: 0,
                rows: 1,
                inner: 2,
                columns: 1,
                lanes: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ];
        assert!(continuous_static_root(
            &parameter_tensor_root,
            &BTreeSet::new(),
        ));

        let state_tensor_root = vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::Y,
                input_start: 3,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            solve::LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ];
        assert!(!continuous_static_root(
            &state_tensor_root,
            &BTreeSet::from([3]),
        ));
        assert!(continuous_static_root(
            &state_tensor_root,
            &BTreeSet::from([3, 4]),
        ));
    }

    #[test]
    fn continuous_static_root_rejects_time_dependency_through_register_flow() {
        let time_root = vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ];
        assert!(!continuous_static_root(&time_root, &BTreeSet::new()));
    }

    #[test]
    fn continuous_static_root_rejects_seed_dependency() {
        let seed_root = vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 3 },
            solve::LinearOp::StoreOutput { src: 0 },
        ];
        assert!(!continuous_static_root(&seed_root, &BTreeSet::new()));
    }
}
