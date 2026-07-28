mod direct_family;
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

use direct_family::*;
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
    projection_index: Option<usize>,
}

impl DerivativeEquation {
    fn structured_dae_equation_index(&self) -> Option<usize> {
        self.projection_index
            .is_none()
            .then_some(self.dae_equation_index)
            .flatten()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ContinuousEquationRowProjection {
    Source,
    FunctionOutput { index: usize },
}

#[derive(Debug, Clone)]
pub(crate) struct ContinuousEquationRow {
    pub(crate) source_equation_index: usize,
    pub(crate) projection: ContinuousEquationRowProjection,
    pub(crate) equation: dae::Equation,
    pub(crate) is_derivative: bool,
}

#[derive(Debug, Clone)]
struct DerivativeComponent {
    state_indices: Vec<usize>,
    equation_indices: Vec<usize>,
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
    components: IndexMap<usize, DerivativeComponent>,
    structural_bindings: Arc<IndexMap<String, f64>>,
    continuous_rows: Vec<ContinuousEquationRow>,
}

impl DerivativeRhsAnalysis {
    pub(crate) fn take_continuous_rows(&mut self) -> Vec<ContinuousEquationRow> {
        std::mem::take(&mut self.continuous_rows)
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
    let (continuous_rows, equations, _) =
        collect_continuous_equation_rows(dae_model, &state_names, &structural_bindings)?;
    let direct_assignments =
        collect_direct_assignments(dae_model, &continuous_rows, &structural_bindings)?;
    let (component_roots, components) =
        derivative_state_components(dae_model, &states, &equations, &structural_bindings)?;
    let direct_equations =
        collect_direct_derivative_equations(dae_model, &states, &equations, &components)?;
    Ok(DerivativeRhsAnalysis {
        states,
        equations,
        direct_equations,
        direct_assignments: Arc::new(direct_assignments),
        component_roots,
        components,
        structural_bindings: Arc::new(structural_bindings),
        continuous_rows,
    })
}

pub(in crate::lower) fn collect_runtime_direct_assignments(
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<IndexMap<String, DirectAssignmentValue>, LowerError> {
    let state_names = dae_model
        .variables
        .states
        .keys()
        .map(|name| name.as_str().to_string())
        .collect::<HashSet<_>>();
    let (continuous_rows, _, _) =
        collect_continuous_equation_rows(dae_model, &state_names, structural_bindings)?;
    let mut assignments =
        collect_direct_assignments(dae_model, &continuous_rows, structural_bindings)?;
    for (name, parameter) in &dae_model.variables.parameters {
        if parameter.causality != dae::VariableCausality::CalculatedParameter {
            continue;
        }
        let Some(binding) = parameter.start.as_ref() else {
            continue;
        };
        assignments
            .entry(name.as_str().to_string())
            .or_insert_with(|| {
                DirectAssignmentValue::full(
                    binding.clone(),
                    parameter.start_span.unwrap_or(parameter.source_span),
                )
            });
    }
    Ok(assignments)
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
    let mut declines = crate::tensor_declines::TensorDeclineJournal::new();
    lower_derivative_rhs_with_analysis(dae_model, layout, &analysis, &mut declines)
}

// SPEC_0021: Exception - derivative RHS lowering owns block assembly across
// direct assignments, fallback rows, tensor families, and validation.
#[allow(clippy::too_many_lines)]
pub(crate) fn lower_derivative_rhs_with_analysis(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    analysis: &DerivativeRhsAnalysis,
    declines: &mut crate::tensor_declines::TensorDeclineJournal,
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
    let mut direct_family_nodes =
        lower_direct_derivative_families(&mut processed, analysis, &lowering_ctx, &y_slot_ranges)?;
    let mut i = 0;

    while i < analysis.states.len() {
        if let Some(mut family_nodes) = direct_family_nodes.shift_remove(&i) {
            flush_pending_derivative_programs(
                &mut block.nodes,
                &mut pending_derivative_programs,
                dae_model,
                declines,
            )?;
            reserve_derivative_capacity(
                &mut block.nodes,
                family_nodes.len(),
                "ordered direct derivative family tensor node count",
                derivative_state_or_context_span(dae_model, &analysis.states[i])?,
            )?;
            block.nodes.append(&mut family_nodes);
        }
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
        if component.state_indices.len() > 1 {
            flush_pending_derivative_programs(
                &mut block.nodes,
                &mut pending_derivative_programs,
                dae_model,
                declines,
            )?;

            let mut group = derivative_vec_with_capacity(
                component.state_indices.len(),
                "coupled derivative group state count",
                derivative_state_or_context_span(dae_model, state)?,
            )?;
            for idx in &component.state_indices {
                group.push(analysis.states[*idx].clone());
            }
            let span = derivative_state_or_context_span(dae_model, state)?;
            let node = if component_indices_are_contiguous_from(&component.state_indices, i) {
                lower_linsolve_group(&group, component, &lowering_ctx)?
            } else {
                let program = lower_linsolve_group_program(&group, component, &lowering_ctx)?;
                ComputeNode::ScalarPrograms(
                    rumoca_ir_solve::ScalarProgramBlock::with_output_indices(
                        vec![program],
                        vec![span],
                        component.state_indices.clone(),
                    )?,
                )
            };
            reserve_derivative_capacity(
                &mut block.nodes,
                1,
                "derivative compute node count",
                span,
            )?;
            block.nodes.push(node);
            for idx in component.state_indices.iter().copied() {
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
                        declines,
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
                        declines,
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
            .and_then(|equation| analysis.equations[*equation].structured_dae_equation_index());
        let span = dae_equation_index
            .and_then(|index| dae_model.continuous.equations.get(index))
            .map(|equation| equation.span)
            .filter(|span| !span.is_dummy())
            .map(Ok)
            .unwrap_or_else(|| derivative_state_or_context_span(dae_model, state))?;
        let row = lower_state_derivative_row(state, &analysis.direct_equations, &lowering_ctx)?;
        let producer_load_strides = match dae_equation_index {
            Some(equation_index) => crate::stencil::producer_load_strides_for_dae_equation(
                layout,
                &dae_model.continuous.structured_equations,
                equation_index,
                &row,
                span,
            )?,
            None => None,
        };
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
            producer_load_strides,
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
        declines,
    )?;
    if !direct_family_nodes.is_empty() {
        return Err(LowerError::contract_violation(
            "direct derivative family nodes were not placed in state-output order",
            span,
        ));
    }

    Ok(block)
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
        if component.state_indices.len() > 1 {
            let mut group = derivative_vec_with_capacity(
                component.state_indices.len(),
                "coupled derivative scalar group state count",
                derivative_state_or_context_span(dae_model, state)?,
            )?;
            for state_idx in &component.state_indices {
                group.push(analysis.states[*state_idx].clone());
            }
            rows.push(lower_linsolve_group_component(
                state,
                &group,
                component,
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

/// Initialization-only source ownership summary: one omission flag per
/// continuous DAE equation. A flag is true only when every normalized
/// projection is a state derivative row, so a mixed source remains intact in
/// the initialization residual.
///
/// Runtime assignment, residual, and direct-assignment lowering must classify
/// `ContinuousEquationRow` records instead of consulting this source summary.
pub(super) fn state_derivative_equation_flags(
    dae_model: &dae::Dae,
) -> Result<Vec<bool>, LowerError> {
    let states = collect_state_scalars(dae_model)?;
    let state_names = states
        .iter()
        .map(|state| state.name.clone())
        .collect::<HashSet<_>>();
    let structural_bindings = compile_time::structural_bindings(dae_model)?;
    let (_, _, source_equations_all_derivative) =
        collect_continuous_equation_rows(dae_model, &state_names, &structural_bindings)?;
    Ok(source_equations_all_derivative)
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
    Err(LowerError::contract_violation(
        format!(
            "singleton derivative component for `{}` has no checked direct equation",
            state.name
        ),
        derivative_state_or_context_span(ctx.dae_model, state)?,
    ))
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
    dae_model: &dae::Dae,
    states: &[StateScalar],
    equations: &[DerivativeEquation],
    components: &IndexMap<usize, DerivativeComponent>,
) -> Result<IndexMap<String, usize>, LowerError> {
    let mut direct = IndexMap::new();
    if states.is_empty() {
        return Ok(direct);
    }
    let state_span = first_derivative_state_span(dae_model, states)?;
    direct.try_reserve(states.len()).map_err(|_| {
        LowerError::contract_violation(
            "direct derivative equation map capacity exceeds host memory limits",
            state_span,
        )
    })?;
    for component in components.values() {
        if component.state_indices.len() != 1 {
            continue;
        }
        let state_index = component.state_indices[0];
        let equation_index = component.equation_indices[0];
        let state = states.get(state_index).ok_or_else(|| {
            LowerError::contract_violation(
                format!("derivative component references missing state index {state_index}"),
                state_span,
            )
        })?;
        let state_context_span = derivative_state_or_context_span(dae_model, state)?;
        let equation = equations.get(equation_index).ok_or_else(|| {
            LowerError::contract_violation(
                format!("derivative component references missing equation index {equation_index}"),
                state_context_span,
            )
        })?;
        if equation.coefficients.len() != 1 || !equation.coefficients.contains_key(&state.name) {
            return Err(LowerError::contract_violation(
                format!(
                    "singleton derivative component for `{}` does not own one matching coefficient",
                    state.name
                ),
                derivative_row_span(equation, state_context_span),
            ));
        }
        if direct.insert(state.name.clone(), equation_index).is_some() {
            return Err(LowerError::contract_violation(
                format!(
                    "singleton derivative component for `{}` was constructed more than once",
                    state.name
                ),
                derivative_row_span(equation, state_context_span),
            ));
        }
    }
    Ok(direct)
}

type DerivativeStateComponents = (Vec<usize>, IndexMap<usize, DerivativeComponent>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct DerivativeEquationOrderKey {
    owner_span: (u64, usize, usize),
    coefficients: Vec<(usize, u64, (u64, usize, usize))>,
    rhs: (u64, (u64, usize, usize)),
}

fn derivative_state_components(
    dae_model: &dae::Dae,
    states: &[StateScalar],
    equations: &[DerivativeEquation],
    structural_bindings: &IndexMap<String, f64>,
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
    let mut components = IndexMap::<usize, DerivativeComponent>::new();
    components.try_reserve(states.len()).map_err(|_| {
        LowerError::contract_violation(
            "derivative component map capacity exceeds host memory limits",
            span,
        )
    })?;
    for idx in 0..states.len() {
        let root = find_component_root(&mut parent, idx);
        if let Some(component) = components.get_mut(&root) {
            reserve_derivative_capacity(
                &mut component.state_indices,
                1,
                "derivative component member count",
                span,
            )?;
            component.state_indices.push(idx);
        } else {
            let mut state_indices =
                derivative_vec_with_capacity(1, "derivative component member count", span)?;
            state_indices.push(idx);
            components.insert(
                root,
                DerivativeComponent {
                    state_indices,
                    equation_indices: Vec::new(),
                },
            );
        }
        roots.push(root);
    }
    assign_and_validate_derivative_component_rows(
        dae_model,
        states,
        equations,
        &state_indices,
        &roots,
        &mut components,
        structural_bindings,
    )?;
    Ok((roots, components))
}

fn assign_and_validate_derivative_component_rows(
    dae_model: &dae::Dae,
    states: &[StateScalar],
    equations: &[DerivativeEquation],
    state_indices: &IndexMap<&str, usize>,
    roots: &[usize],
    components: &mut IndexMap<usize, DerivativeComponent>,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<(), LowerError> {
    for (equation_index, equation) in equations.iter().enumerate() {
        let mut row_root = None;
        for name in equation.coefficients.keys() {
            let state_index = state_indices.get(name.as_str()).copied().ok_or_else(|| {
                LowerError::contract_violation(
                    format!("derivative equation references non-state derivative `{name}`"),
                    equation.span,
                )
            })?;
            let root = roots.get(state_index).copied().ok_or_else(|| {
                LowerError::contract_violation(
                    format!("derivative equation state `{name}` is missing a component root"),
                    equation.span,
                )
            })?;
            match row_root {
                Some(expected) if expected != root => {
                    return Err(LowerError::contract_violation(
                        "derivative equation spans disconnected derivative components",
                        equation.span,
                    ));
                }
                Some(_) => {}
                None => row_root = Some(root),
            }
        }
        let root = row_root.ok_or_else(|| {
            LowerError::contract_violation(
                "collected derivative equation has no derivative coefficients",
                equation.span,
            )
        })?;
        let component = components.get_mut(&root).ok_or_else(|| {
            LowerError::contract_violation(
                format!("derivative equation references missing component root {root}"),
                equation.span,
            )
        })?;
        reserve_derivative_capacity(
            &mut component.equation_indices,
            1,
            "derivative component equation count",
            equation.span,
        )?;
        component.equation_indices.push(equation_index);
    }

    for component in components.values_mut() {
        let state_count = component.state_indices.len();
        let equation_count = component.equation_indices.len();
        if equation_count != state_count {
            let span =
                derivative_component_diagnostic_span(dae_model, states, equations, component)?;
            let names = derivative_component_state_summary(states, component, span)?;
            return Err(super::unsupported_at(
                format!(
                    "derivative component [{names}] has {equation_count} derivative-containing equations for {state_count} state derivatives; lowering requires one complete square derivative system and does not discard extra equations or select rows by encounter order"
                ),
                span,
            ));
        }
        if equation_count > 1 {
            canonicalize_derivative_component_rows(
                equations,
                state_indices,
                &mut component.equation_indices,
            )?;
        }
        reject_singular_constant_derivative_component(
            dae_model,
            states,
            equations,
            component,
            structural_bindings,
        )?;
    }
    Ok(())
}

fn canonicalize_derivative_component_rows(
    equations: &[DerivativeEquation],
    state_indices: &IndexMap<&str, usize>,
    equation_indices: &mut Vec<usize>,
) -> Result<(), LowerError> {
    let span = equation_indices
        .iter()
        .filter_map(|index| equations.get(*index))
        .find_map(|equation| (!equation.span.is_dummy()).then_some(equation.span))
        .unwrap_or(rumoca_core::Span::DUMMY);
    reject_duplicate_derivative_coefficient_rows(equations, equation_indices, span)?;
    let mut ordered = derivative_vec_with_capacity(
        equation_indices.len(),
        "canonical derivative component row count",
        span,
    )?;
    for equation_index in equation_indices.iter().copied() {
        let equation = equations.get(equation_index).ok_or_else(|| {
            LowerError::contract_violation(
                format!("derivative component references missing equation index {equation_index}"),
                span,
            )
        })?;
        let mut coefficients = derivative_vec_with_capacity(
            equation.coefficients.len(),
            "canonical derivative coefficient count",
            derivative_row_span(equation, span),
        )?;
        for (name, coefficient) in &equation.coefficients {
            let state_index = state_indices.get(name.as_str()).copied().ok_or_else(|| {
                LowerError::contract_violation(
                    format!("derivative equation references non-state derivative `{name}`"),
                    equation.span,
                )
            })?;
            coefficients.push((
                state_index,
                rumoca_core::expression_semantic_fingerprint(coefficient),
                expression_span_order_key(coefficient),
            ));
        }
        coefficients.sort_unstable();
        ordered.push((
            DerivativeEquationOrderKey {
                owner_span: span_order_key(equation.span),
                coefficients,
                rhs: (
                    rumoca_core::expression_semantic_fingerprint(&equation.rhs),
                    expression_span_order_key(&equation.rhs),
                ),
            },
            equation_index,
        ));
    }
    ordered.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
    reject_derivative_order_key_ties(&ordered, equations, span)?;
    equation_indices.clear();
    reserve_derivative_capacity(
        equation_indices,
        ordered.len(),
        "canonical derivative component row count",
        span,
    )?;
    equation_indices.extend(ordered.into_iter().map(|(_, index)| index));
    Ok(())
}

fn reject_duplicate_derivative_coefficient_rows(
    equations: &[DerivativeEquation],
    equation_indices: &[usize],
    fallback_span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let mut duplicate_span: Option<rumoca_core::Span> = None;
    for (position, lhs_index) in equation_indices.iter().copied().enumerate() {
        let lhs = equations.get(lhs_index).ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "duplicate derivative row check references missing equation index {lhs_index}"
                ),
                fallback_span,
            )
        })?;
        for rhs_index in equation_indices[position + 1..].iter().copied() {
            let rhs = equations.get(rhs_index).ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "duplicate derivative row check references missing equation index {rhs_index}"
                    ),
                    fallback_span,
                )
            })?;
            if !derivative_coefficients_semantically_equal(lhs, rhs) {
                continue;
            }
            let span = [lhs.span, rhs.span]
                .into_iter()
                .filter(|span| !span.is_dummy())
                .min_by_key(|span| span_order_key(*span))
                .unwrap_or(fallback_span);
            duplicate_span = Some(
                duplicate_span
                    .map(|current| {
                        if span_order_key(span) < span_order_key(current) {
                            span
                        } else {
                            current
                        }
                    })
                    .unwrap_or(span),
            );
        }
    }
    let Some(span) = duplicate_span else {
        return Ok(());
    };
    Err(super::unsupported_at(
        "duplicate equivalent derivative coefficient rows make the derivative system singular"
            .to_string(),
        span,
    ))
}

fn derivative_coefficients_semantically_equal(
    lhs: &DerivativeEquation,
    rhs: &DerivativeEquation,
) -> bool {
    lhs.coefficients.len() == rhs.coefficients.len()
        && lhs.coefficients.iter().all(|(name, lhs_coefficient)| {
            rhs.coefficients.get(name).is_some_and(|rhs_coefficient| {
                rumoca_core::expressions_semantically_equal(lhs_coefficient, rhs_coefficient)
            })
        })
}

fn reject_derivative_order_key_ties(
    ordered: &[(DerivativeEquationOrderKey, usize)],
    equations: &[DerivativeEquation],
    fallback_span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let mut group_start = 0usize;
    while group_start < ordered.len() {
        let mut group_end = group_start + 1;
        while group_end < ordered.len() && ordered[group_end].0 == ordered[group_start].0 {
            group_end += 1;
        }
        if group_end - group_start > 1 {
            reject_derivative_order_key_tie_group(
                &ordered[group_start..group_end],
                equations,
                fallback_span,
            )?;
        }
        group_start = group_end;
    }
    Ok(())
}

fn reject_derivative_order_key_tie_group(
    group: &[(DerivativeEquationOrderKey, usize)],
    equations: &[DerivativeEquation],
    fallback_span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let mut rows = derivative_vec_with_capacity(
        group.len(),
        "canonical derivative tie row count",
        fallback_span,
    )?;
    for (_, equation_index) in group {
        rows.push(equations.get(*equation_index).ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "canonical derivative tie references missing equation index {equation_index}"
                ),
                fallback_span,
            )
        })?);
    }
    let span = rows
        .iter()
        .map(|row| row.span)
        .filter(|span| !span.is_dummy())
        .min_by_key(|span| span_order_key(*span))
        .unwrap_or(fallback_span);
    let first = rows[0];
    let all_equivalent = rows[1..]
        .iter()
        .all(|row| derivative_rows_semantically_equal(first, row));
    let reason = if all_equivalent {
        "duplicate equivalent derivative rows make the derivative system singular"
    } else {
        "derivative row semantic fingerprints and source spans collide without exact equality; canonical ordering is unresolved"
    };
    Err(super::unsupported_at(reason.to_string(), span))
}

fn derivative_rows_semantically_equal(lhs: &DerivativeEquation, rhs: &DerivativeEquation) -> bool {
    derivative_coefficients_semantically_equal(lhs, rhs)
        && rumoca_core::expressions_semantically_equal(&lhs.rhs, &rhs.rhs)
}

fn reject_singular_constant_derivative_component(
    dae_model: &dae::Dae,
    states: &[StateScalar],
    equations: &[DerivativeEquation],
    component: &DerivativeComponent,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<(), LowerError> {
    let span = derivative_component_diagnostic_span(dae_model, states, equations, component)?;
    let Some(matrix) =
        constant_derivative_component_matrix(states, equations, component, structural_bindings)?
    else {
        return Ok(());
    };
    if constant_matrix_has_proven_singular_rows(&matrix) {
        let names = derivative_component_state_summary(states, component, span)?;
        return Err(super::unsupported_at(
            format!(
                "compile-time derivative coefficient matrix for component [{names}] is singular"
            ),
            span,
        ));
    }
    Ok(())
}

fn constant_derivative_component_matrix(
    states: &[StateScalar],
    equations: &[DerivativeEquation],
    component: &DerivativeComponent,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Option<Vec<Vec<f64>>>, LowerError> {
    let span = component
        .equation_indices
        .iter()
        .filter_map(|index| equations.get(*index))
        .find_map(|equation| (!equation.span.is_dummy()).then_some(equation.span))
        .unwrap_or(rumoca_core::Span::DUMMY);
    let mut matrix = derivative_vec_with_capacity(
        component.equation_indices.len(),
        "constant derivative matrix row count",
        span,
    )?;
    for equation_index in &component.equation_indices {
        let equation = equations.get(*equation_index).ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "constant derivative matrix references missing equation index {equation_index}"
                ),
                span,
            )
        })?;
        let mut row = derivative_vec_with_capacity(
            component.state_indices.len(),
            "constant derivative matrix column count",
            derivative_row_span(equation, span),
        )?;
        for state_index in &component.state_indices {
            let state = states.get(*state_index).ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "constant derivative matrix references missing state index {state_index}"
                    ),
                    span,
                )
            })?;
            let value = match equation.coefficients.get(&state.name) {
                Some(coefficient) => {
                    let Some(value) = eval_derivative_constant(coefficient, structural_bindings)
                    else {
                        return Ok(None);
                    };
                    value
                }
                None => 0.0,
            };
            if !value.is_finite() {
                return Err(super::unsupported_at(
                    "compile-time derivative coefficient matrix contains a non-finite value"
                        .to_string(),
                    derivative_row_span(equation, span),
                ));
            }
            row.push(value);
        }
        matrix.push(row);
    }
    Ok(Some(matrix))
}

fn eval_derivative_constant(
    expr: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
) -> Option<f64> {
    rumoca_eval_dae::constant::eval_scalar_const_expr_with(expr, &|name, subscripts| {
        let key = derivative_constant_binding_key(name, subscripts)?;
        structural_bindings
            .get(&key)
            .copied()
            .map(rumoca_eval_dae::constant::ConstValue::Real)
    })
}

fn derivative_constant_binding_key(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }
    let indices = subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } if *value > 0 => {
                usize::try_from(*value).ok()
            }
            rumoca_core::Subscript::Colon { .. } | rumoca_core::Subscript::Expr { .. } => None,
            rumoca_core::Subscript::Index { .. } => None,
        })
        .collect::<Option<Vec<_>>>()?;
    Some(dae::format_subscript_key(name.as_str(), &indices))
}

/// Conservatively proves singularity without performing floating arithmetic.
///
/// Every coefficient is already a finite represented `f64`. An all-zero row
/// or two exactly equal rows is therefore a proof of linear dependence over
/// those represented constants. Other constant matrices stay on the runtime
/// `LinSolve` path: floating elimination could underflow or round a nonzero
/// pivot to zero and is not a proof of mathematical singularity.
fn constant_matrix_has_proven_singular_rows(matrix: &[Vec<f64>]) -> bool {
    matrix.iter().enumerate().any(|(row_index, row)| {
        row.iter().all(|coefficient| *coefficient == 0.0)
            || matrix[..row_index].iter().any(|previous| previous == row)
    })
}

fn expression_span_order_key(expr: &rumoca_core::Expression) -> (u64, usize, usize) {
    expr.span()
        .map_or(span_order_key(rumoca_core::Span::DUMMY), span_order_key)
}

fn span_order_key(span: rumoca_core::Span) -> (u64, usize, usize) {
    (span.source.0, span.start.0, span.end.0)
}

fn derivative_component_diagnostic_span(
    dae_model: &dae::Dae,
    states: &[StateScalar],
    equations: &[DerivativeEquation],
    component: &DerivativeComponent,
) -> Result<rumoca_core::Span, LowerError> {
    if let Some(span) = component
        .equation_indices
        .iter()
        .filter_map(|index| equations.get(*index))
        .map(|equation| equation.span)
        .filter(|span| !span.is_dummy())
        .min_by_key(|span| span_order_key(*span))
    {
        return Ok(span);
    }
    let Some(state) = component
        .state_indices
        .first()
        .and_then(|index| states.get(*index))
    else {
        return Err(LowerError::UnspannedContractViolation {
            reason: "derivative component has neither source equations nor states".to_string(),
        });
    };
    derivative_state_or_context_span(dae_model, state)
}

fn derivative_component_state_summary(
    states: &[StateScalar],
    component: &DerivativeComponent,
    span: rumoca_core::Span,
) -> Result<String, LowerError> {
    let mut names = derivative_vec_with_capacity(
        component.state_indices.len(),
        "derivative component state summary count",
        span,
    )?;
    for state_index in &component.state_indices {
        let state = states.get(*state_index).ok_or_else(|| {
            LowerError::contract_violation(
                format!("derivative component references missing state index {state_index}"),
                span,
            )
        })?;
        names.push(state.name.as_str());
    }
    Ok(names.join(", "))
}

fn union_components(parent: &mut [usize], lhs: usize, rhs: usize) {
    let lhs_root = find_component_root(parent, lhs);
    let rhs_root = find_component_root(parent, rhs);
    if lhs_root != rhs_root {
        let (root, child) = if lhs_root < rhs_root {
            (lhs_root, rhs_root)
        } else {
            (rhs_root, lhs_root)
        };
        parent[child] = root;
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
        if component.state_indices.len() > 1 {
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

/// Build a `ComputeNode::LinSolve` for a connected group of n state scalars
/// that are coupled by a dense linear system.
///
/// The setup ops compute the n×n coefficient matrix A and the n-vector RHS b
/// into contiguous register ranges. The component was checked during analysis,
/// so lowering consumes its complete row set without encounter-order selection.
fn lower_linsolve_group(
    states: &[StateScalar],
    component: &DerivativeComponent,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<ComputeNode, LowerError> {
    let setup = build_dense_group_solve_setup(states, component, ctx)?;

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

fn lower_linsolve_group_program(
    states: &[StateScalar],
    component: &DerivativeComponent,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut setup = build_dense_group_solve_setup(states, component, ctx)?;
    for component in 0..setup.n {
        let dst = setup.next_reg;
        setup.next_reg = setup.next_reg.checked_add(1).ok_or_else(|| {
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
    }
    Ok(setup.ops)
}

fn lower_linsolve_group_component(
    state: &StateScalar,
    states: &[StateScalar],
    derivative_component: &DerivativeComponent,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut setup = build_dense_group_solve_setup(states, derivative_component, ctx)?;
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
    component: &DerivativeComponent,
    ctx: &DerivativeRhsLoweringContext<'_>,
) -> Result<DenseGroupSolveSetup, LowerError> {
    let n = states.len();
    let fallback_span = first_derivative_state_span(ctx.dae_model, states)?;
    let span = component
        .equation_indices
        .iter()
        .filter_map(|index| ctx.equations.get(*index))
        .find_map(|equation| (!equation.span.is_dummy()).then_some(equation.span))
        .unwrap_or(fallback_span);
    if component.state_indices.len() != n || component.equation_indices.len() != n {
        return Err(LowerError::contract_violation(
            format!(
                "checked derivative component changed shape before lowering: {} states, {} component states, {} equations",
                n,
                component.state_indices.len(),
                component.equation_indices.len()
            ),
            span,
        ));
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

    for (row_idx, equation_index) in component.equation_indices.iter().copied().enumerate() {
        let row = ctx.equations.get(equation_index).ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "checked derivative component references missing equation index {equation_index}"
                ),
                span,
            )
        })?;
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
            projection_index: None,
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
        projection_index: None,
    }))
}
