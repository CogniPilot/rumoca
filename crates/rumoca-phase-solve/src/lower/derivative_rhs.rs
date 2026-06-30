mod direct_matmul;
mod equation_collection;
#[path = "derivative_rhs/function_projection.rs"]
mod function_projection;
mod linear_parts;
mod projection;
#[cfg(test)]
mod tests;
use super::{
    DirectAssignmentValue, IndexedBindingMap, LowerBuilder, LowerBuilderMetadata, LowerError,
    Scope, compile_time,
    helpers::{build_indexed_binding_map, expr_tag, short_expr},
};
pub(super) use equation_collection::*;
use indexmap::IndexMap;
pub(super) use linear_parts::*;
pub(super) use projection::*;
use rumoca_core::{Literal, OpBinary, OpUnary};
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{BinaryOp, ComputeBlock, ComputeNode, LinearOp, Reg, ScalarSlot, VarLayout};
use std::collections::HashSet;
use std::sync::Arc;

use function_projection::{
    function_call_projected_scalars_with_owner, function_projected_residuals_with_owner,
};

#[derive(Debug, Clone)]
pub(in crate::lower) struct StateScalar {
    name: String,
    base: String,
    component: usize,
    base_size: usize,
}

#[derive(Debug, Clone)]
pub(in crate::lower) struct DerivativeEquation {
    coefficients: IndexMap<String, rumoca_core::Expression>,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
    dae_equation_index: Option<usize>,
}

pub(in crate::lower) struct DerivativeLinearCtx<'a> {
    state_names: &'a HashSet<String>,
    dae_model: &'a dae::Dae,
    structural_bindings: &'a IndexMap<String, f64>,
}

pub(crate) struct DerivativeRhsAnalysis {
    states: Vec<StateScalar>,
    equations: Vec<DerivativeEquation>,
    direct_equations: IndexMap<String, usize>,
    direct_assignments: Arc<IndexMap<String, DirectAssignmentValue>>,
    component_roots: Vec<usize>,
    components: IndexMap<usize, Vec<usize>>,
    structural_bindings: Arc<IndexMap<String, f64>>,
    equation_flags: Vec<bool>,
}

impl DerivativeRhsAnalysis {
    pub(crate) fn equation_flags(&self) -> &[bool] {
        &self.equation_flags
    }

    pub(crate) fn direct_dae_equation_index_for_state(&self, state_name: &str) -> Option<usize> {
        self.direct_equations
            .get(state_name)
            .and_then(|equation| self.equations[*equation].dae_equation_index)
    }

    /// Drop direct-assignment definitions for algebraics that are retained solver
    /// unknowns (solved by the algebraic projection and refreshed before derivative
    /// evaluation). Removing them from the inline map makes *every* lowering path —
    /// the pre-pass rewriter, the `LowerBuilder`'s own inlining, and the access-proof
    /// builder — uniformly LOAD the variable from its Y-slot instead of inlining a
    /// definition that folds to a constant at boundary cells. That uniformity is what
    /// lets a structured derivative family preserve as an `AffineStencil` (roadmap
    /// step 4b). `solved_algebraic_y` is the set of retained algebraic Y-indices,
    /// empty for the isolated `analyze_derivative_rhs` path (no projection runs there,
    /// so inlining stays the only correct standalone behavior).
    pub(crate) fn load_retained_algebraics(
        &mut self,
        layout: &VarLayout,
        solved_algebraic_y: &HashSet<usize>,
    ) {
        if solved_algebraic_y.is_empty() {
            return;
        }
        let assignments = Arc::make_mut(&mut self.direct_assignments);
        assignments
            .retain(|key, _| !is_slot_backed_projection_algebraic(key, layout, solved_algebraic_y));
    }
}

pub(crate) fn analyze_derivative_rhs(
    dae_model: &dae::Dae,
) -> Result<DerivativeRhsAnalysis, LowerError> {
    let states = collect_state_scalars(dae_model)?;
    // O(1) membership: the per-equation derivative probes test `state_names.contains`
    // once per candidate key, so a linear `&[String]` scan made the whole pass
    // O(equations x states) — quadratic in grid size for tensor PDE families.
    let state_names: HashSet<String> = states.iter().map(|state| state.name.clone()).collect();
    let structural_bindings = compile_time::structural_bindings(dae_model)?;
    let (equations, equation_flags) =
        collect_derivative_equations(dae_model, &state_names, &structural_bindings)?;
    let direct_equations = collect_direct_derivative_equations(&equations);
    let direct_assignments =
        collect_direct_assignments(dae_model, &equation_flags, &structural_bindings)?;
    let (component_roots, components) =
        derivative_state_components(dae_model, &states, &equations)?;
    Ok(DerivativeRhsAnalysis {
        states,
        equations,
        direct_equations,
        direct_assignments: Arc::new(direct_assignments),
        component_roots,
        components,
        structural_bindings: Arc::new(structural_bindings),
        equation_flags,
    })
}

fn derivative_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_derivative_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn derivative_index_map_with_capacity<K, V>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<IndexMap<K, V>, LowerError>
where
    K: std::hash::Hash + Eq,
{
    let mut values = IndexMap::new();
    values.try_reserve(capacity).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

fn reserve_derivative_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn active_assignment_stack(span: rumoca_core::Span) -> Result<Vec<String>, LowerError> {
    derivative_vec_with_capacity(0, "active direct-assignment stack", span)
}

fn push_active_assignment(
    active_assignments: &mut Vec<String>,
    key: String,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    reserve_derivative_capacity(
        active_assignments,
        1,
        "active direct-assignment stack",
        span,
    )?;
    active_assignments.push(key);
    Ok(())
}

fn derivative_rhs_expr_span(
    expr: &rumoca_core::Expression,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span().filter(|span| !span.is_dummy()).ok_or_else(|| {
        LowerError::UnspannedContractViolation {
            reason: "derivative RHS expression requires source span metadata".to_string(),
        }
    })
}

fn derivative_rhs_expr_or_owner_span(
    expr: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    if let Some(span) = expr.span().filter(|span| !span.is_dummy()) {
        return Ok(span);
    }
    if !owner_span.is_dummy() {
        return Ok(owner_span);
    }
    Err(LowerError::UnspannedContractViolation {
        reason: "derivative RHS expression requires source span metadata".to_string(),
    })
}

fn first_derivative_state_span(
    dae_model: &dae::Dae,
    states: &[StateScalar],
) -> Result<rumoca_core::Span, LowerError> {
    for state in states {
        if let Some(span) = derivative_state_span(dae_model, state)? {
            return Ok(span);
        }
    }
    dae_derivative_context_span(dae_model)
}

fn first_dae_state_span(dae_model: &dae::Dae) -> Result<rumoca_core::Span, LowerError> {
    dae_derivative_context_span(dae_model)
}

fn dae_derivative_context_span(dae_model: &dae::Dae) -> Result<rumoca_core::Span, LowerError> {
    if let Some(span) = dae_model
        .variables
        .states
        .values()
        .find_map(|var| (!var.source_span.is_dummy()).then_some(var.source_span))
        .or_else(|| {
            dae_model
                .continuous
                .equations
                .iter()
                .find_map(|equation| (!equation.span.is_dummy()).then_some(equation.span))
        })
    {
        return Ok(span);
    }
    Err(LowerError::UnspannedContractViolation {
        reason: "derivative RHS context requires state or equation source provenance".to_string(),
    })
}

fn derivative_row_span(
    row: &DerivativeEquation,
    fallback_span: rumoca_core::Span,
) -> rumoca_core::Span {
    if row.span.is_dummy() {
        fallback_span
    } else {
        row.span
    }
}

fn derivative_rows_span(
    rows: &[&DerivativeEquation],
    fallback_span: rumoca_core::Span,
) -> rumoca_core::Span {
    rows.iter()
        .find_map(|row| (!row.span.is_dummy()).then_some(row.span))
        .unwrap_or(fallback_span)
}

fn derivative_state_span(
    dae_model: &dae::Dae,
    state: &StateScalar,
) -> Result<Option<rumoca_core::Span>, LowerError> {
    Ok(dae_model
        .variables
        .states
        .get(&rumoca_core::VarName::new(state.base.as_str()))
        .and_then(|var| (!var.source_span.is_dummy()).then_some(var.source_span)))
}

fn derivative_state_or_context_span(
    dae_model: &dae::Dae,
    state: &StateScalar,
) -> Result<rumoca_core::Span, LowerError> {
    match derivative_state_span(dae_model, state)? {
        Some(span) => Ok(span),
        None => dae_derivative_context_span(dae_model),
    }
}

pub(super) fn lower_derivative_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<ComputeBlock, LowerError> {
    let analysis = analyze_derivative_rhs(dae_model)?;
    lower_derivative_rhs_with_analysis(dae_model, layout, &analysis)
}

// SPEC_0021: Exception - derivative RHS lowering owns block assembly across
// direct assignments, fallback rows, tensor families, and validation.
#[allow(clippy::too_many_lines)]
pub(crate) fn lower_derivative_rhs_with_analysis(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    analysis: &DerivativeRhsAnalysis,
) -> Result<ComputeBlock, LowerError> {
    if analysis.states.is_empty() {
        return Ok(ComputeBlock::default());
    }
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    let lowering_ctx = DerivativeRhsLoweringContext {
        equations: &analysis.equations,
        direct_assignments: &analysis.direct_assignments,
        dae_model,
        layout,
        structural_bindings: &analysis.structural_bindings,
        indexed_bindings: &indexed_bindings,
    };
    let mut block = ComputeBlock::default();
    let span = first_dae_state_span(dae_model)?;
    let mut pending_derivative_programs =
        derivative_vec_with_capacity(0, "pending derivative program count", span)?;
    let y_slot_ranges = crate::stencil::structured_y_slot_ranges(layout)?;
    let mut processed = derivative_vec_with_capacity(
        analysis.states.len(),
        "derivative processed state count",
        span,
    )?;
    processed.resize(analysis.states.len(), false);
    let mut i = 0;

    while i < analysis.states.len() {
        if processed[i] {
            i += 1;
            continue;
        }
        let state = &analysis.states[i];

        let component_root = analysis
            .component_roots
            .get(i)
            .copied()
            .ok_or_else(|| missing_derivative_component_root_error(dae_model, state))?;
        let component = analysis
            .components
            .get(&component_root)
            .ok_or_else(|| missing_derivative_component_error(dae_model, state, component_root))?;
        if component.len() > 1 {
            flush_pending_derivative_programs(
                &mut block.nodes,
                &mut pending_derivative_programs,
                dae_model,
            )?;

            let mut group = derivative_vec_with_capacity(
                component.len(),
                "coupled derivative group state count",
                derivative_state_or_context_span(dae_model, state)?,
            )?;
            for idx in component {
                group.push(analysis.states[*idx].clone());
            }
            let node = lower_linsolve_group(&group, &lowering_ctx)?;
            reserve_derivative_capacity(
                &mut block.nodes,
                1,
                "derivative compute node count",
                derivative_state_or_context_span(dae_model, state)?,
            )?;
            block.nodes.push(node);
            for idx in component.iter().copied() {
                processed[idx] = true;
            }
            i += 1;
            continue;
        }

        if let Some(group_len) = direct_vector_group_len(analysis, &processed, i) {
            match lower_direct_row_group(analysis, i, group_len, &lowering_ctx) {
                Ok(DirectRowGroupLowering::Scalar(row)) => {
                    flush_pending_derivative_programs(
                        &mut block.nodes,
                        &mut pending_derivative_programs,
                        dae_model,
                    )?;
                    let span = derivative_state_or_context_span(dae_model, state)?;
                    let output_indices = (i..i + group_len).collect::<Vec<_>>();
                    let program_spans = vec![span];
                    let scalar_block = rumoca_ir_solve::ScalarProgramBlock::with_output_indices(
                        vec![row],
                        program_spans,
                        output_indices,
                    )?;
                    reserve_derivative_capacity(
                        &mut block.nodes,
                        1,
                        "derivative direct vector compute node count",
                        span,
                    )?;
                    block.nodes.push(ComputeNode::ScalarPrograms(scalar_block));
                    processed[i..i + group_len].fill(true);
                    i += group_len;
                    continue;
                }
                Ok(DirectRowGroupLowering::Tensor(node)) => {
                    flush_pending_derivative_programs(
                        &mut block.nodes,
                        &mut pending_derivative_programs,
                        dae_model,
                    )?;
                    reserve_derivative_capacity(
                        &mut block.nodes,
                        1,
                        "derivative direct vector tensor node count",
                        derivative_state_or_context_span(dae_model, state)?,
                    )?;
                    block.nodes.push(node);
                    processed[i..i + group_len].fill(true);
                    i += group_len;
                    continue;
                }
                Err(LowerError::Unsupported { .. }) => {}
                Err(err) => return Err(err),
            }
        }

        let dae_equation_index = analysis
            .direct_equations
            .get(&state.name)
            .and_then(|equation| analysis.equations[*equation].dae_equation_index);
        let span = dae_equation_index
            .and_then(|index| dae_model.continuous.equations.get(index))
            .map(|equation| equation.span)
            .filter(|span| !span.is_dummy())
            .map(Ok)
            .unwrap_or_else(|| derivative_state_or_context_span(dae_model, state))?;
        let row = lower_state_derivative_row(state, &analysis.direct_equations, &lowering_ctx)?;
        reserve_derivative_capacity(
            &mut pending_derivative_programs,
            1,
            "pending derivative program count",
            span,
        )?;
        pending_derivative_programs.push(crate::stencil::StructuredProgram {
            load_y_ranges: crate::stencil::structured_load_y_ranges(&row, &y_slot_ranges, span)?,
            ops: row,
            output_index: i,
            pointwise_output_y_index: Some(i),
            span,
            output_y_range: state_output_y_range(dae_model, state, i)?,
            dae_equation_index,
            access_proof: derivative_row_access_proof(
                state,
                &analysis.direct_equations,
                &lowering_ctx,
            )?,
        });
        processed[i] = true;
        i += 1;
    }

    flush_pending_derivative_programs(
        &mut block.nodes,
        &mut pending_derivative_programs,
        dae_model,
    )?;

    Ok(block)
}

fn flush_pending_derivative_programs(
    nodes: &mut Vec<ComputeNode>,
    pending: &mut Vec<crate::stencil::StructuredProgram>,
    dae_model: &dae::Dae,
) -> Result<(), LowerError> {
    if !pending.is_empty() {
        crate::stencil::push_structured_programs(
            nodes,
            pending,
            &dae_model.continuous.structured_equations,
            &dae_model.continuous.equations,
        )?;
    }
    Ok(())
}

fn state_output_y_range(
    dae_model: &dae::Dae,
    state: &StateScalar,
    output_index: usize,
) -> Result<std::ops::Range<usize>, LowerError> {
    let start = output_index.checked_sub(state.component).ok_or_else(|| {
        derivative_component_contract_error(
            dae_model,
            state,
            format!(
                "derivative output index {output_index} is before component {} for state `{}`",
                state.component, state.name
            ),
        )
    })?;
    let end = start.checked_add(state.base_size).ok_or_else(|| {
        derivative_component_contract_error(
            dae_model,
            state,
            format!(
                "derivative output range overflows for state `{}` with base size {}",
                state.name, state.base_size
            ),
        )
    })?;
    Ok(start..end)
}

pub(super) fn lower_derivative_rhs_scalar_programs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let analysis = analyze_derivative_rhs(dae_model)?;
    if analysis.states.is_empty() {
        return Ok(Vec::new());
    }
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    let lowering_ctx = DerivativeRhsLoweringContext {
        equations: &analysis.equations,
        direct_assignments: &analysis.direct_assignments,
        dae_model,
        layout,
        structural_bindings: &analysis.structural_bindings,
        indexed_bindings: &indexed_bindings,
    };

    let span = first_derivative_state_span(dae_model, &analysis.states)?;
    let mut rows =
        derivative_vec_with_capacity(analysis.states.len(), "derivative scalar row count", span)?;
    for (idx, state) in analysis.states.iter().enumerate() {
        let component_root = analysis
            .component_roots
            .get(idx)
            .copied()
            .ok_or_else(|| missing_derivative_component_root_error(dae_model, state))?;
        let component = analysis
            .components
            .get(&component_root)
            .ok_or_else(|| missing_derivative_component_error(dae_model, state, component_root))?;
        if component.len() > 1 {
            let mut group = derivative_vec_with_capacity(
                component.len(),
                "coupled derivative scalar group state count",
                derivative_state_or_context_span(dae_model, state)?,
            )?;
            for state_idx in component {
                group.push(analysis.states[*state_idx].clone());
            }
            rows.push(lower_linsolve_group_component(
                state,
                &group,
                &lowering_ctx,
            )?);
        } else {
            rows.push(lower_state_derivative_row(
                state,
                &analysis.direct_equations,
                &lowering_ctx,
            )?);
        }
    }
    Ok(rows)
}

fn missing_derivative_component_root_error(
    dae_model: &dae::Dae,
    state: &StateScalar,
) -> LowerError {
    derivative_component_contract_error(
        dae_model,
        state,
        format!(
            "derivative RHS component roots are missing state `{}`",
            state.name
        ),
    )
}

fn missing_derivative_component_error(
    dae_model: &dae::Dae,
    state: &StateScalar,
    component_root: usize,
) -> LowerError {
    derivative_component_contract_error(
        dae_model,
        state,
        format!(
            "derivative RHS component map is missing state `{}` with root `{component_root}`",
            state.name
        ),
    )
}

fn derivative_component_contract_error(
    dae_model: &dae::Dae,
    state: &StateScalar,
    reason: String,
) -> LowerError {
    match derivative_state_or_context_span(dae_model, state) {
        Ok(span) => LowerError::contract_violation(reason, span),
        Err(_) => LowerError::UnspannedContractViolation { reason },
    }
}

struct DerivativeRhsLoweringContext<'a> {
    equations: &'a [DerivativeEquation],
    direct_assignments: &'a Arc<IndexMap<String, DirectAssignmentValue>>,
    dae_model: &'a dae::Dae,
    layout: &'a VarLayout,
    structural_bindings: &'a Arc<IndexMap<String, f64>>,
    indexed_bindings: &'a IndexedBindingMap,
}

/// True when `key` names an algebraic that is a retained solver unknown — solved by
/// the algebraic projection and refreshed into its Y-slot before `derivative_rhs`
/// evaluation. Such a variable must be LOADED from its slot, never inlined: inlining
/// a boundary cell whose definition folds to a constant (e.g. `Q_cond[1] = 0`) makes
/// the derivative family non-uniform and blocks stencil preservation (roadmap 4b).
fn is_slot_backed_projection_algebraic(
    key: &str,
    layout: &VarLayout,
    solved_algebraic_y: &HashSet<usize>,
) -> bool {
    matches!(
        layout.binding(key),
        Some(ScalarSlot::Y { index, .. }) if solved_algebraic_y.contains(&index)
    )
}

pub(super) fn state_derivative_equation_flags(
    dae_model: &dae::Dae,
) -> Result<Vec<bool>, LowerError> {
    Ok(analyze_derivative_rhs(dae_model)?.equation_flags)
}

fn lower_state_derivative_row(
    state: &StateScalar,
    direct_equations: &IndexMap<String, usize>,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    if let Some(row) = direct_equations
        .get(&state.name)
        .and_then(|row_idx| ctx.equations.get(*row_idx))
    {
        return lower_direct_row(row, state, ctx);
    }
    lower_coupled_row(state, ctx)
}

fn derivative_row_access_proof(
    state: &StateScalar,
    direct_equations: &IndexMap<String, usize>,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Option<crate::stencil::StructuredAccessProof>, LowerError> {
    let Some(row) = direct_equations
        .get(&state.name)
        .and_then(|row_idx| ctx.equations.get(*row_idx))
    else {
        return Ok(None);
    };
    let Some(coefficient) = row.coefficients.get(&state.name) else {
        return Ok(None);
    };
    let mut active_assignments = active_assignment_stack(row.span)?;
    let mut builder = crate::stencil::StructuredAccessProofBuilder::new();
    let Some(()) = collect_access_operands(&mut builder, &row.rhs, ctx, &mut active_assignments)?
    else {
        return Ok(None);
    };
    let Some(()) =
        collect_access_operands(&mut builder, coefficient, ctx, &mut active_assignments)?
    else {
        return Ok(None);
    };
    Ok(Some(builder.finish()))
}

fn collect_access_operands(
    builder: &mut crate::stencil::StructuredAccessProofBuilder,
    expr: &rumoca_core::Expression,
    ctx: &DerivativeRhsLoweringContext<'_>,
    active_assignments: &mut Vec<String>,
) -> Result<Option<()>, LowerError> {
    builder.collect_expression_result(expr, |base, subscripts, span, operands| {
        collect_var_ref_access_operands(base, subscripts, span, ctx, active_assignments, operands)
    })
}

fn collect_var_ref_access_operands(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &DerivativeRhsLoweringContext<'_>,
    active_assignments: &mut Vec<String>,
    operands: &mut Vec<crate::stencil::StructuredAccessOperand>,
) -> Result<Option<()>, LowerError> {
    let Some(indices) =
        optional_direct_assignment_indices(subscripts, ctx.structural_bindings, span)?
    else {
        return Ok(None);
    };
    let key = if indices.is_empty() {
        base.to_string()
    } else {
        dae::format_subscript_key(base, &indices)
    };
    if let Some(assignment) = ctx.direct_assignments.get(key.as_str()) {
        return collect_direct_assignment_access_operands(
            key.as_str(),
            assignment,
            ctx,
            active_assignments,
            operands,
        );
    }
    let Some(slot) = ctx.layout.binding(&key) else {
        return Ok(None);
    };
    let Some(operand) = crate::stencil::structured_access_operand_for_slot(slot) else {
        return Ok(None);
    };
    operands.push(operand);
    Ok(Some(()))
}

fn collect_direct_assignment_access_operands(
    key: &str,
    assignment: &DirectAssignmentValue,
    ctx: &DerivativeRhsLoweringContext<'_>,
    active_assignments: &mut Vec<String>,
    operands: &mut Vec<crate::stencil::StructuredAccessOperand>,
) -> Result<Option<()>, LowerError> {
    if active_assignments.iter().any(|active| active == key) {
        return Ok(None);
    }
    let Some(expr) = direct_assignment_access_expr(assignment, ctx)? else {
        return Ok(None);
    };
    push_active_assignment(
        active_assignments,
        key.to_string(),
        derivative_rhs_expr_or_owner_span(&expr, assignment.span)?,
    )?;
    let mut builder = crate::stencil::StructuredAccessProofBuilder::new();
    let result = collect_access_operands(&mut builder, &expr, ctx, active_assignments);
    active_assignments.pop();
    match result? {
        Some(()) => {
            builder.append_to(
                operands,
                derivative_rhs_expr_or_owner_span(&expr, assignment.span)?,
            )?;
            Ok(Some(()))
        }
        None => Ok(None),
    }
}

fn direct_assignment_access_expr(
    assignment: &DirectAssignmentValue,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let span = derivative_rhs_expr_or_owner_span(&assignment.rhs, assignment.span)?;
    let dims = expression_result_dims(
        &assignment.rhs,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    )?;
    let Some(flat_index) = assignment.flat_index else {
        if dims.is_empty() {
            return Ok(Some(assignment.rhs.clone()));
        }
        if checked_direct_assignment_scalar_count(&dims, span)? == 1 {
            return project_expression_scalar(
                &assignment.rhs,
                &dims,
                0,
                ctx.dae_model,
                ctx.structural_bindings,
                span,
            );
        }
        return Ok(None);
    };
    if dims.is_empty() {
        return Ok(None);
    }
    let projected_index = assignment
        .repeat_period
        .filter(|period| *period > 0)
        .map_or(flat_index, |period| flat_index % period);
    project_expression_scalar(
        &assignment.rhs,
        &dims,
        projected_index,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    )
}

fn checked_direct_assignment_scalar_count(
    dims: &[usize],
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    dims.iter().try_fold(1usize, |count, dim| {
        count.checked_mul(*dim).ok_or_else(|| {
            LowerError::contract_violation(
                "direct assignment scalar count overflows host index range",
                span,
            )
        })
    })
}

fn collect_direct_derivative_equations(
    equations: &[DerivativeEquation],
) -> IndexMap<String, usize> {
    let mut direct = IndexMap::new();
    for (idx, equation) in equations.iter().enumerate() {
        if equation.coefficients.len() == 1
            && let Some(state_name) = equation.coefficients.keys().next()
        {
            direct.insert(state_name.clone(), idx);
        }
    }
    direct
}

type DerivativeStateComponents = (Vec<usize>, IndexMap<usize, Vec<usize>>);

fn derivative_state_components(
    dae_model: &dae::Dae,
    states: &[StateScalar],
    equations: &[DerivativeEquation],
) -> Result<DerivativeStateComponents, LowerError> {
    if states.is_empty() {
        return Ok((Vec::new(), IndexMap::new()));
    }
    let span = first_derivative_state_span(dae_model, states)?;
    let mut state_indices = IndexMap::new();
    state_indices.try_reserve(states.len()).map_err(|_| {
        LowerError::contract_violation(
            "derivative component state index capacity exceeds host memory limits",
            span,
        )
    })?;
    for (idx, state) in states.iter().enumerate() {
        state_indices.insert(state.name.as_str(), idx);
    }
    let mut parent =
        derivative_vec_with_capacity(states.len(), "derivative component parent count", span)?;
    for idx in 0..states.len() {
        parent.push(idx);
    }
    for equation in equations {
        let mut row_indices = derivative_vec_with_capacity(
            equation.coefficients.len(),
            "derivative component row index count",
            equation.span,
        )?;
        for name in equation.coefficients.keys() {
            if let Some(idx) = state_indices.get(name.as_str()).copied() {
                row_indices.push(idx);
            }
        }
        if let Some((&first, rest)) = row_indices.split_first() {
            for &idx in rest {
                union_components(&mut parent, first, idx);
            }
        }
    }

    let mut roots =
        derivative_vec_with_capacity(states.len(), "derivative component root count", span)?;
    let mut components = IndexMap::<usize, Vec<usize>>::new();
    components.try_reserve(states.len()).map_err(|_| {
        LowerError::contract_violation(
            "derivative component map capacity exceeds host memory limits",
            span,
        )
    })?;
    for idx in 0..states.len() {
        let root = find_component_root(&mut parent, idx);
        if let Some(component) = components.get_mut(&root) {
            reserve_derivative_capacity(component, 1, "derivative component member count", span)?;
            component.push(idx);
        } else {
            let mut component =
                derivative_vec_with_capacity(1, "derivative component member count", span)?;
            component.push(idx);
            components.insert(root, component);
        }
        roots.push(root);
    }
    Ok((roots, components))
}

fn union_components(parent: &mut [usize], lhs: usize, rhs: usize) {
    let lhs_root = find_component_root(parent, lhs);
    let rhs_root = find_component_root(parent, rhs);
    if lhs_root != rhs_root {
        parent[rhs_root] = lhs_root;
    }
}

fn find_component_root(parent: &mut [usize], idx: usize) -> usize {
    if parent[idx] != idx {
        parent[idx] = find_component_root(parent, parent[idx]);
    }
    parent[idx]
}

fn lower_direct_row(
    equation: &DerivativeEquation,
    state: &StateScalar,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = row_builder(
        ctx.dae_model,
        ctx.layout,
        ctx.direct_assignments,
        ctx.structural_bindings,
        ctx.indexed_bindings,
    );
    let scope = Scope::new();
    let mut active_assignments = active_assignment_stack(equation.span)?;
    let rhs_expr = inline_direct_assignment_expr(&equation.rhs, ctx, &mut active_assignments)?;
    let rhs = lower_state_component_expr(&mut builder, &rhs_expr, state, equation.span, &scope)?;
    let mut coeff_active_assignments = active_assignment_stack(equation.span)?;
    let coeff_expr = inline_direct_assignment_expr(
        &equation.coefficients[&state.name],
        ctx,
        &mut coeff_active_assignments,
    )?;
    let coeff = builder.lower_expr(&coeff_expr, &scope, 0)?;
    let value = builder.emit_binary_at(BinaryOp::Div, rhs, coeff, equation.span)?;
    builder.ops.push(LinearOp::StoreOutput { src: value });
    Ok(builder.ops)
}

// SPEC_0021: Exception - direct assignment inlining is a recursive expression
// rewriter; keeping cases together prevents divergent cycle checks.
#[allow(clippy::too_many_lines)]
fn inline_direct_assignment_expr(
    expr: &rumoca_core::Expression,
    ctx: &DerivativeRhsLoweringContext<'_>,
    active_assignments: &mut Vec<String>,
) -> Result<rumoca_core::Expression, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } => Ok(inline_direct_assignment_var_ref(
            name.as_str(),
            subscripts,
            *span,
            ctx,
            active_assignments,
        )?
        .unwrap_or_else(|| expr.clone())),
        rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
            Ok(rumoca_core::Expression::Binary {
                op: op.clone(),
                lhs: Box::new(inline_direct_assignment_expr(lhs, ctx, active_assignments)?),
                rhs: Box::new(inline_direct_assignment_expr(rhs, ctx, active_assignments)?),
                span: *span,
            })
        }
        rumoca_core::Expression::Unary { op, rhs, span } => Ok(rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(inline_direct_assignment_expr(rhs, ctx, active_assignments)?),
            span: *span,
        }),
        rumoca_core::Expression::BuiltinCall {
            function,
            args,
            span,
        } => {
            let mut inlined_args =
                derivative_vec_with_capacity(args.len(), "inlined builtin argument count", *span)?;
            for arg in args {
                inlined_args.push(inline_direct_assignment_expr(arg, ctx, active_assignments)?);
            }
            Ok(rumoca_core::Expression::BuiltinCall {
                function: *function,
                args: inlined_args,
                span: *span,
            })
        }
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            span,
        } => {
            let mut inlined_elements =
                derivative_vec_with_capacity(elements.len(), "inlined array element count", *span)?;
            for element in elements {
                inlined_elements.push(inline_direct_assignment_expr(
                    element,
                    ctx,
                    active_assignments,
                )?);
            }
            Ok(rumoca_core::Expression::Array {
                elements: inlined_elements,
                is_matrix: *is_matrix,
                span: *span,
            })
        }
        rumoca_core::Expression::Tuple { elements, span } => {
            let mut inlined_elements =
                derivative_vec_with_capacity(elements.len(), "inlined tuple element count", *span)?;
            for element in elements {
                inlined_elements.push(inline_direct_assignment_expr(
                    element,
                    ctx,
                    active_assignments,
                )?);
            }
            Ok(rumoca_core::Expression::Tuple {
                elements: inlined_elements,
                span: *span,
            })
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            span,
        } => {
            let mut inlined_branches =
                derivative_vec_with_capacity(branches.len(), "inlined if branch count", *span)?;
            for (condition, branch) in branches {
                inlined_branches.push((
                    inline_direct_assignment_expr(condition, ctx, active_assignments)?,
                    inline_direct_assignment_expr(branch, ctx, active_assignments)?,
                ));
            }
            Ok(rumoca_core::Expression::If {
                branches: inlined_branches,
                else_branch: Box::new(inline_direct_assignment_expr(
                    else_branch,
                    ctx,
                    active_assignments,
                )?),
                span: *span,
            })
        }
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => Ok(rumoca_core::Expression::Index {
            base: Box::new(inline_direct_assignment_expr(
                base,
                ctx,
                active_assignments,
            )?),
            subscripts: subscripts.clone(),
            span: *span,
        }),
        rumoca_core::Expression::FieldAccess { base, field, span } => {
            Ok(rumoca_core::Expression::FieldAccess {
                base: Box::new(inline_direct_assignment_expr(
                    base,
                    ctx,
                    active_assignments,
                )?),
                field: field.clone(),
                span: *span,
            })
        }
        _ => Ok(expr.clone()),
    }
}

fn inline_direct_assignment_var_ref(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &DerivativeRhsLoweringContext<'_>,
    active_assignments: &mut Vec<String>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    if let Some(inlined) =
        inline_direct_assignment_slice(base, subscripts, ctx, active_assignments)?
    {
        return Ok(Some(inlined));
    }
    let Some(indices) =
        optional_direct_assignment_indices(subscripts, ctx.structural_bindings, span)?
    else {
        return Ok(None);
    };
    let key = if indices.is_empty() {
        base.to_string()
    } else {
        dae::format_subscript_key(base, &indices)
    };
    let Some(assignment) = ctx.direct_assignments.get(key.as_str()) else {
        return Ok(None);
    };
    if active_assignments
        .iter()
        .any(|active| active == key.as_str())
    {
        return Ok(None);
    }
    let Some(expr) = direct_assignment_access_expr(assignment, ctx)? else {
        return Ok(None);
    };
    push_active_assignment(
        active_assignments,
        key,
        derivative_rhs_expr_or_owner_span(&assignment.rhs, assignment.span)?,
    )?;
    let inlined = inline_direct_assignment_expr(&expr, ctx, active_assignments);
    active_assignments.pop();
    Ok(Some(inlined?))
}

fn inline_direct_assignment_slice(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    ctx: &DerivativeRhsLoweringContext<'_>,
    active_assignments: &mut Vec<String>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    if subscripts.is_empty() || scalar_direct_assignment_subscripts(subscripts) {
        return Ok(None);
    }
    let Some(assignment) = ctx.direct_assignments.get(base) else {
        return Ok(None);
    };
    if active_assignments.iter().any(|active| active == base) {
        return Ok(None);
    }
    push_active_assignment(
        active_assignments,
        base.to_string(),
        derivative_rhs_expr_or_owner_span(&assignment.rhs, assignment.span)?,
    )?;
    let alias_result =
        inline_direct_assignment_slice_alias(&assignment.rhs, subscripts, ctx, active_assignments);
    match alias_result {
        Ok(Some(inlined)) => {
            active_assignments.pop();
            return Ok(Some(inlined));
        }
        Ok(None) => {}
        Err(err) => {
            active_assignments.pop();
            return Err(err);
        }
    }
    let base_expr = inline_direct_assignment_expr(&assignment.rhs, ctx, active_assignments);
    active_assignments.pop();
    let base_expr = base_expr?;
    let span = subscripts
        .iter()
        .map(rumoca_core::Subscript::span)
        .find(|span| !span.is_dummy())
        .map_or_else(|| derivative_rhs_expr_span(&base_expr), Ok)?;
    if let Some(projected) = project_direct_assignment_slice(&base_expr, subscripts, span, ctx)? {
        return Ok(Some(projected));
    }
    Ok(Some(rumoca_core::Expression::Index {
        base: Box::new(base_expr),
        subscripts: subscripts.to_vec(),
        span,
    }))
}

fn inline_direct_assignment_slice_alias(
    rhs: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    ctx: &DerivativeRhsLoweringContext<'_>,
    active_assignments: &mut Vec<String>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let rumoca_core::Expression::VarRef {
        name,
        subscripts: rhs_subscripts,
        ..
    } = rhs
    else {
        return Ok(None);
    };
    if !rhs_subscripts.is_empty() {
        return Ok(None);
    }
    inline_direct_assignment_slice(name.as_str(), subscripts, ctx, active_assignments)
}

fn project_direct_assignment_slice(
    base_expr: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let base_dims =
        match expression_result_dims(base_expr, ctx.dae_model, ctx.structural_bindings, span) {
            Ok(dims) if !dims.is_empty() => dims,
            Ok(_) => return Ok(None),
            Err(err) => return Err(err),
        };
    let result_dims =
        result_dims_for_subscripts(&base_dims, subscripts, ctx.structural_bindings, span)?;
    let indexed = rumoca_core::Expression::Index {
        base: Box::new(base_expr.clone()),
        subscripts: subscripts.to_vec(),
        span,
    };
    let Some(elements) = project_expression_scalars(
        &indexed,
        &result_dims,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    )?
    else {
        return Ok(None);
    };
    Ok(Some(rumoca_core::Expression::Array {
        elements,
        is_matrix: false,
        span,
    }))
}

fn scalar_direct_assignment_subscripts(subscripts: &[rumoca_core::Subscript]) -> bool {
    subscripts.iter().all(|subscript| {
        matches!(
            subscript,
            rumoca_core::Subscript::Index { value, .. } if *value > 0
        )
    })
}

fn optional_direct_assignment_indices(
    subscripts: &[rumoca_core::Subscript],
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    match compile_time_subscript_indices_with_owner(subscripts, structural_bindings, owner_span) {
        Ok(indices) => Ok(Some(indices)),
        Err(
            LowerError::Unsupported { .. }
            | LowerError::UnsupportedAt { .. }
            | LowerError::DynamicSubscript,
        ) => Ok(None),
        Err(err) => Err(err),
    }
}

/// When several consecutive state scalars share one vector-valued direct
/// equation (e.g. `der(X) = quad_deriv13(...)` projected over X[1..n]), lower
/// the shared RHS ONCE into a single multi-output program with one StoreOutput
/// per component, instead of one self-contained program per output that each
/// re-derive the whole RHS. Returns the size of the group starting at `start`,
/// or `None` if no valid complete vector group is present.
/// The shared computation behind a per-component derivative RHS. For
/// `der(X) = f(...)` each component's RHS is `f(...)[k]` (an `Index` over the
/// same base call); the base `f(...)` is what we want to compute once.
fn shared_vector_rhs_base(expr: &rumoca_core::Expression) -> &rumoca_core::Expression {
    match expr {
        rumoca_core::Expression::Index { base, .. } => base,
        other => other,
    }
}

fn direct_vector_group_len(
    analysis: &DerivativeRhsAnalysis,
    processed: &[bool],
    start: usize,
) -> Option<usize> {
    let head = &analysis.states[start];
    let base_size = head.base_size;
    if base_size <= 1 || processed[start] || head.component != 0 {
        return None;
    }
    // The whole group must fit, be single-SCC each (the linsolve grouping owns
    // coupled states), all direct equations whose RHS projects the SAME base
    // computation, with components 0..base_size laid out consecutively.
    if start + base_size > analysis.states.len() {
        return None;
    }
    let head_eq_idx = *analysis.direct_equations.get(&head.name)?;
    let head_base = shared_vector_rhs_base(&analysis.equations[head_eq_idx].rhs);
    for offset in 0..base_size {
        let idx = start + offset;
        let state = &analysis.states[idx];
        if processed[idx]
            || state.base != head.base
            || state.base_size != base_size
            || state.component != offset
        {
            return None;
        }
        let component = analysis.components.get(&analysis.component_roots[idx])?;
        if component.len() > 1 {
            return None;
        }
        let eq_idx = *analysis.direct_equations.get(&state.name)?;
        if shared_vector_rhs_base(&analysis.equations[eq_idx].rhs) != head_base {
            return None;
        }
    }
    Some(base_size)
}

enum DirectRowGroupLowering {
    Scalar(Vec<LinearOp>),
    Tensor(ComputeNode),
}

/// Lower a group of consecutive vector-state components that share one direct
/// RHS. Direct tensor products stay as tensor nodes when the group can use the
/// product output stream directly; everything else falls back to one
/// multi-output scalar program.
fn lower_direct_row_group(
    analysis: &DerivativeRhsAnalysis,
    start: usize,
    group_len: usize,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<DirectRowGroupLowering, LowerError> {
    if let Some(node) =
        direct_matmul::lower_direct_row_group_matmul(analysis, start, group_len, ctx)?
    {
        return Ok(DirectRowGroupLowering::Tensor(node));
    }

    lower_direct_row_group_scalar(analysis, start, group_len, ctx)
        .map(DirectRowGroupLowering::Scalar)
}

fn lower_direct_row_group_scalar(
    analysis: &DerivativeRhsAnalysis,
    start: usize,
    group_len: usize,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = row_builder(
        ctx.dae_model,
        ctx.layout,
        ctx.direct_assignments,
        ctx.structural_bindings,
        ctx.indexed_bindings,
    );
    let scope = Scope::new();

    let head = &analysis.states[start];
    let head_eq = &analysis.equations[analysis.direct_equations[&head.name]];
    // Compute every component of the shared base RHS once (shared by RowCse
    // within this single builder), then project each component below.
    let head_base = shared_vector_rhs_base(&head_eq.rhs);
    let values =
        builder.lower_array_like_values_with_source_context(head_base, head_eq.span, &scope, 0)?;
    if values.len() != group_len {
        return Err(LowerError::Unsupported {
            reason: format!(
                "vector derivative RHS for `{}` produced {} values for a group of {group_len}",
                head.base,
                values.len()
            ),
        });
    }

    for offset in 0..group_len {
        let state = &analysis.states[start + offset];
        let equation = &analysis.equations[analysis.direct_equations[&state.name]];
        let rhs = values[state.component];
        let coeff = builder.lower_expr(&equation.coefficients[&state.name], &scope, 0)?;
        let value = builder.emit_binary_at(BinaryOp::Div, rhs, coeff, equation.span)?;
        builder.ops.push(LinearOp::StoreOutput { src: value });
    }
    Ok(builder.ops)
}

fn lower_state_component_expr(
    builder: &mut LowerBuilder,
    expr: &rumoca_core::Expression,
    state: &StateScalar,
    source_context_span: rumoca_core::Span,
    scope: &Scope,
) -> Result<Reg, LowerError> {
    if state.base_size > 1 {
        return lower_row_rhs_expr(
            builder,
            expr,
            state.component,
            state.base_size,
            source_context_span,
            scope,
        );
    }
    builder.lower_expr_with_source_context(expr, source_context_span, scope, 0)
}

fn lower_row_rhs_expr(
    builder: &mut LowerBuilder,
    expr: &rumoca_core::Expression,
    row_index: usize,
    row_count: usize,
    source_context_span: rumoca_core::Span,
    scope: &Scope,
) -> Result<Reg, LowerError> {
    if row_count <= 1 {
        return builder.lower_expr_with_source_context(expr, source_context_span, scope, 0);
    }

    let values =
        builder.lower_array_like_values_with_source_context(expr, source_context_span, scope, 0)?;
    if values.len() == row_count {
        return values
            .get(row_index)
            .copied()
            .ok_or_else(|| LowerError::Unsupported {
                reason: format!(
                    "derivative RHS row {row_index} is out of bounds for RHS width {}",
                    values.len()
                ),
            });
    }
    if let [value] = values.as_slice() {
        return Ok(*value);
    }
    Err(super::unsupported_at(
        format!(
            "derivative RHS width {} does not match row count {row_count} for {} {}",
            values.len(),
            expr_tag(expr),
            short_expr(expr, 800)
        ),
        derivative_rhs_expr_span(expr)?,
    ))
}

fn lower_coupled_row(
    state: &StateScalar,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let base_rows = coupled_rows_for_base(ctx.equations, state, ctx.dae_model)?;
    if base_rows.len() < state.base_size {
        return Err(LowerError::Unsupported {
            reason: format!("missing explicit derivative equation for `{}`", state.name),
        });
    }
    lower_dense_solve_component(state, &base_rows[..state.base_size], ctx)
}

/// Build a `ComputeNode::LinSolve` for a connected group of n state scalars
/// that are coupled by a dense linear system.
///
/// The setup ops compute the n×n coefficient matrix A and the n-vector RHS b
/// into contiguous register ranges, exactly as `lower_dense_solve_component`
/// does for one component. Unlike that function, we do it once and emit a
/// single tensor node that backends can execute without repeating the solve.
fn lower_linsolve_group(
    states: &[StateScalar],
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<ComputeNode, LowerError> {
    let setup = build_dense_group_solve_setup(states, ctx)?;

    Ok(ComputeNode::LinSolve {
        setup_ops: setup.ops,
        matrix_start: setup.matrix_start,
        rhs_start: setup.rhs_start,
        n: setup.n,
        next_reg: setup.next_reg,
        metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
        span: setup.span,
    })
}

fn lower_linsolve_group_component(
    state: &StateScalar,
    states: &[StateScalar],
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut setup = build_dense_group_solve_setup(states, ctx)?;
    let component = states
        .iter()
        .position(|group_state| group_state.name == state.name)
        .ok_or_else(|| LowerError::Unsupported {
            reason: format!(
                "state `{}` is not present in derivative solve group",
                state.name
            ),
        })?;
    let dst = setup
        .next_reg
        .checked_add(1)
        .map(|next| {
            let dst = setup.next_reg;
            setup.next_reg = next;
            dst
        })
        .ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "Solve register allocation overflow after r{}",
                    setup.next_reg
                ),
                setup.span,
            )
        })?;
    setup.ops.push(LinearOp::LinearSolveComponent {
        dst,
        matrix_start: setup.matrix_start,
        rhs_start: setup.rhs_start,
        n: setup.n,
        component,
    });
    setup.ops.push(LinearOp::StoreOutput { src: dst });
    Ok(setup.ops)
}

struct DenseGroupSolveSetup {
    ops: Vec<LinearOp>,
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    next_reg: Reg,
    span: rumoca_core::Span,
}

fn build_dense_group_solve_setup(
    states: &[StateScalar],
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<DenseGroupSolveSetup, LowerError> {
    let n = states.len();
    let span = first_derivative_state_span(ctx.dae_model, states)?;
    let state_names: HashSet<&str> = states.iter().map(|state| state.name.as_str()).collect();
    let rows = coupled_rows_for_states(ctx.equations, &state_names, span)?;
    let span = derivative_rows_span(&rows, span);
    if rows.len() < n {
        let state_summary = derivative_state_name_summary(states, span)?;
        let key_summary = derivative_row_key_summary(&rows, span)?;
        return Err(LowerError::Unsupported {
            reason: format!(
                "missing explicit derivative equations for coupled group `{}`: found {}/{} rows with coefficient keys {}",
                state_summary,
                rows.len(),
                n,
                key_summary
            ),
        });
    }

    let mut builder = row_builder(
        ctx.dae_model,
        ctx.layout,
        ctx.direct_assignments,
        ctx.structural_bindings,
        ctx.indexed_bindings,
    );
    let scope = Scope::new();
    let matrix_reg_count = n.checked_mul(n).ok_or_else(|| {
        LowerError::contract_violation(
            "dense derivative matrix register count overflows host index range",
            span,
        )
    })?;
    let mut matrix_regs = derivative_vec_with_capacity(
        matrix_reg_count,
        "dense derivative matrix register count",
        span,
    )?;
    let mut rhs_regs =
        derivative_vec_with_capacity(n, "dense derivative RHS register count", span)?;

    for (row_idx, row) in rows[..n].iter().enumerate() {
        let row_span = derivative_row_span(row, span);
        for state in states {
            matrix_regs.push(
                lower_inlined_or_zero(
                    &mut builder,
                    row.coefficients.get(&state.name),
                    row_span,
                    ctx,
                    &scope,
                )
                .map_err(|err| {
                    err.with_context(format!(
                        "lower derivative coefficient row {row_idx} for `{}`",
                        state.name
                    ))
                })?,
            );
        }
        rhs_regs.push(
            lower_inlined_row_rhs_expr(&mut builder, &row.rhs, row_idx, n, row_span, ctx, &scope)
                .map_err(|err| err.with_context(format!("lower derivative RHS row {row_idx}")))?,
        );
    }

    builder.ensure_reg_capacity(
        checked_dense_solve_reg_count(matrix_regs.len(), rhs_regs.len(), 0, span)?,
        span,
    )?;
    let matrix_start = builder.try_pack_registers(&matrix_regs, span)?;
    let rhs_start = builder.try_pack_registers(&rhs_regs, span)?;
    Ok(DenseGroupSolveSetup {
        ops: builder.ops,
        matrix_start,
        rhs_start,
        n,
        next_reg: builder.next_reg,
        span,
    })
}

fn derivative_state_name_summary(
    states: &[StateScalar],
    span: rumoca_core::Span,
) -> Result<String, LowerError> {
    let mut names =
        derivative_vec_with_capacity(states.len(), "derivative state summary count", span)?;
    for state in states {
        names.push(state.name.as_str());
    }
    Ok(names.join(", "))
}

fn derivative_row_key_summary(
    rows: &[&DerivativeEquation],
    span: rumoca_core::Span,
) -> Result<String, LowerError> {
    if rows.is_empty() {
        return Ok("[]".to_string());
    }
    let mut row_summaries =
        derivative_vec_with_capacity(rows.len(), "derivative row summary count", span)?;
    for row in rows {
        let mut keys = derivative_vec_with_capacity(
            row.coefficients.len(),
            "derivative row key summary count",
            row.span,
        )?;
        for key in row.coefficients.keys() {
            keys.push(key.as_str());
        }
        row_summaries.push(format!("[{}]", keys.join(", ")));
    }
    Ok(format!("[{}]", row_summaries.join(", ")))
}

fn lower_dense_solve_component(
    state: &StateScalar,
    rows: &[&DerivativeEquation],
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = row_builder(
        ctx.dae_model,
        ctx.layout,
        ctx.direct_assignments,
        ctx.structural_bindings,
        ctx.indexed_bindings,
    );
    let scope = Scope::new();
    let span = derivative_rows_span(
        rows,
        derivative_state_or_context_span(ctx.dae_model, state)?,
    );
    let matrix_reg_count = rows.len().checked_mul(state.base_size).ok_or_else(|| {
        LowerError::contract_violation(
            format!(
                "dense derivative matrix register count overflows for state `{}`",
                state.name
            ),
            span,
        )
    })?;
    let mut matrix_regs = derivative_vec_with_capacity(
        matrix_reg_count,
        "dense derivative component matrix register count",
        span,
    )?;
    let mut rhs_regs = derivative_vec_with_capacity(
        rows.len(),
        "dense derivative component RHS register count",
        span,
    )?;
    for (row_idx, row) in rows.iter().enumerate() {
        let row_span = derivative_row_span(row, span);
        for component in 0..state.base_size {
            let name = format!("{}[{}]", state.base, component + 1);
            matrix_regs.push(
                lower_inlined_or_zero(
                    &mut builder,
                    row.coefficients.get(&name),
                    row_span,
                    ctx,
                    &scope,
                )
                .map_err(|err| {
                    err.with_context(format!(
                        "lower derivative coefficient row {row_idx} for `{name}`"
                    ))
                })?,
            );
        }
        rhs_regs.push(
            lower_inlined_row_rhs_expr(
                &mut builder,
                &row.rhs,
                row_idx,
                state.base_size,
                row_span,
                ctx,
                &scope,
            )
            .map_err(|err| err.with_context(format!("lower derivative RHS row {row_idx}")))?,
        );
    }
    builder.ensure_reg_capacity(
        checked_dense_solve_reg_count(matrix_regs.len(), rhs_regs.len(), 1, span)?,
        span,
    )?;
    let matrix_start = builder.try_pack_registers(&matrix_regs, span)?;
    let rhs_start = builder.try_pack_registers(&rhs_regs, span)?;
    let dst = builder.try_alloc_reg(span)?;
    builder.ops.push(LinearOp::LinearSolveComponent {
        dst,
        matrix_start,
        rhs_start,
        n: state.base_size,
        component: state.component,
    });
    builder.ops.push(LinearOp::StoreOutput { src: dst });
    Ok(builder.ops)
}

fn lower_inlined_or_zero(
    builder: &mut LowerBuilder<'_>,
    expr: Option<&rumoca_core::Expression>,
    fallback_span: rumoca_core::Span,
    ctx: &DerivativeRhsLoweringContext<'_>,
    scope: &Scope,
) -> Result<Reg, LowerError> {
    match expr {
        Some(expr) => {
            let span = derivative_rhs_expr_or_owner_span(expr, fallback_span)?;
            let mut active_assignments = active_assignment_stack(span)?;
            let inlined = inline_direct_assignment_expr(expr, ctx, &mut active_assignments)?;
            builder.lower_expr_with_source_context(&inlined, span, scope, 0)
        }
        None => builder.emit_const_at(0.0, fallback_span),
    }
}

fn lower_inlined_row_rhs_expr(
    builder: &mut LowerBuilder,
    expr: &rumoca_core::Expression,
    row_index: usize,
    row_count: usize,
    fallback_span: rumoca_core::Span,
    ctx: &DerivativeRhsLoweringContext<'_>,
    scope: &Scope,
) -> Result<Reg, LowerError> {
    let span = derivative_rhs_expr_or_owner_span(expr, fallback_span)?;
    let mut active_assignments = active_assignment_stack(span)?;
    let inlined = inline_direct_assignment_expr(expr, ctx, &mut active_assignments)?;
    lower_row_rhs_expr(builder, &inlined, row_index, row_count, span, scope)
}

fn checked_dense_solve_reg_count(
    matrix_len: usize,
    rhs_len: usize,
    extra: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    matrix_len
        .checked_add(rhs_len)
        .and_then(|count| count.checked_add(extra))
        .ok_or_else(|| {
            LowerError::contract_violation(
                "dense derivative solve register allocation count overflow",
                span,
            )
        })
}

fn row_builder<'a>(
    dae_model: &'a dae::Dae,
    layout: &'a VarLayout,
    direct_assignments: &Arc<IndexMap<String, DirectAssignmentValue>>,
    structural_bindings: &Arc<IndexMap<String, f64>>,
    indexed_bindings: &'a IndexedBindingMap,
) -> LowerBuilder<'a> {
    LowerBuilder::new_with_metadata(
        layout,
        &dae_model.symbols.functions,
        LowerBuilderMetadata {
            clock_intervals: Some(&dae_model.clocks.intervals),
            clock_timings: Some(&dae_model.clocks.timings),
            triggered_clock_conditions: Some(&dae_model.clocks.triggered_conditions),
            discrete_valued_names: Some(&dae_model.variables.discrete_valued),
            variable_starts: Some(&dae_model.metadata.variable_starts),
            dae_variables: Some(&dae_model.variables),
            indexed_bindings: Some(indexed_bindings),
            is_initial_mode: false,
        },
    )
    .with_structural_bindings(structural_bindings.clone())
    .with_direct_assignments(direct_assignments.clone())
    .with_dedup_access_ops(false)
}

fn coupled_rows_for_base<'a>(
    equations: &'a [DerivativeEquation],
    state: &StateScalar,
    dae_model: &dae::Dae,
) -> Result<Vec<&'a DerivativeEquation>, LowerError> {
    let mut rows = derivative_vec_with_capacity(
        equations.len(),
        "coupled derivative base row count",
        derivative_state_or_context_span(dae_model, state)?,
    )?;
    for equation in equations {
        if equation
            .coefficients
            .keys()
            .all(|name| dae::component_base_name(name).as_deref() == Some(state.base.as_str()))
        {
            rows.push(equation);
        }
    }
    Ok(rows)
}

fn coupled_rows_for_states<'a>(
    equations: &'a [DerivativeEquation],
    state_names: &HashSet<&str>,
    span: rumoca_core::Span,
) -> Result<Vec<&'a DerivativeEquation>, LowerError> {
    let mut rows =
        derivative_vec_with_capacity(equations.len(), "coupled derivative row count", span)?;
    for equation in equations {
        if !equation.coefficients.is_empty()
            && equation
                .coefficients
                .keys()
                .all(|name| state_names.contains(&name.as_str()))
        {
            rows.push(equation);
        }
    }
    Ok(rows)
}

fn expanded_direct_derivative_equations(
    target: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    span: rumoca_core::Span,
) -> Result<Option<Vec<DerivativeEquation>>, LowerError> {
    let target_keys = derivative_arg_binding_keys(target, dae_model, structural_bindings, span)?;
    if target_keys.is_empty() || !target_keys.iter().all(|key| state_names.contains(key)) {
        return Ok(None);
    }
    let rhs_values = scalarized_rhs_expressions_with_owner(
        rhs,
        target,
        target_keys.len(),
        dae_model,
        structural_bindings,
        span,
    )?;
    if rhs_values.len() != target_keys.len() {
        return Ok(None);
    }

    let mut equations =
        derivative_vec_with_capacity(target_keys.len(), "direct derivative equation rows", span)?;
    for (key, rhs) in target_keys.into_iter().zip(rhs_values) {
        let mut coefficients =
            derivative_index_map_with_capacity(1, "direct derivative coefficient row", span)?;
        coefficients.insert(key, one_expr_with_span(span));
        equations.push(DerivativeEquation {
            coefficients,
            rhs,
            span,
            dae_equation_index: None,
        });
    }
    Ok(Some(equations))
}

fn derivative_equation_from_if_residual(
    residual: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
    owner_span: rumoca_core::Span,
) -> Result<Option<DerivativeEquation>, LowerError> {
    let span = match residual {
        rumoca_core::Expression::If { span, .. } if !span.is_dummy() => *span,
        rumoca_core::Expression::If { .. } => owner_span,
        _ => {
            return Ok(None);
        }
    };

    let Some((coefficients, remainder)) = derivative_linear_parts(residual, ctx, span)? else {
        return Ok(None);
    };
    Ok(Some(DerivativeEquation {
        coefficients,
        rhs: rhs_without_remainder(zero_expr_with_span(span), remainder, span),
        span,
        dae_equation_index: None,
    }))
}
