mod equation_collection;
mod linear_parts;
mod projection;
use super::{
    DirectAssignmentValue, IndexedBindingMap, LowerBuilder, LowerBuilderMetadata, LowerError,
    Scope, compile_time, helpers::build_indexed_binding_map,
};
pub(super) use equation_collection::*;
use indexmap::IndexMap;
pub(super) use linear_parts::*;
pub(super) use projection::*;
use rumoca_core::{Literal, OpBinary, OpUnary};
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{
    BinaryOp, ComputeBlock, ComputeNode, LinearOp, Reg, ScalarProgramBlock, VarLayout,
};
use std::sync::Arc;

#[path = "derivative_rhs/function_projection.rs"]
mod function_projection;
use function_projection::{function_call_projected_scalars, function_projected_residuals};

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
}

pub(in crate::lower) struct DerivativeLinearCtx<'a> {
    state_names: &'a [String],
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
}

pub(crate) fn analyze_derivative_rhs(
    dae_model: &dae::Dae,
) -> Result<DerivativeRhsAnalysis, LowerError> {
    let states = collect_state_scalars(dae_model);
    let state_names = states
        .iter()
        .map(|state| state.name.clone())
        .collect::<Vec<_>>();
    let structural_bindings = compile_time::structural_bindings(dae_model);
    let (equations, equation_flags) =
        collect_derivative_equations(dae_model, &state_names, &structural_bindings)?;
    let direct_equations = collect_direct_derivative_equations(&equations);
    let direct_assignments = collect_direct_assignments(dae_model, &equation_flags);
    let (component_roots, components) = derivative_state_components(&states, &equations);
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

pub(super) fn lower_derivative_rhs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<ComputeBlock, LowerError> {
    let analysis = analyze_derivative_rhs(dae_model)?;
    lower_derivative_rhs_with_analysis(dae_model, layout, &analysis)
}

pub(crate) fn lower_derivative_rhs_with_analysis(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    analysis: &DerivativeRhsAnalysis,
) -> Result<ComputeBlock, LowerError> {
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
    let mut pending_scalar_programs: Vec<Vec<LinearOp>> = Vec::new();
    let mut processed = vec![false; analysis.states.len()];
    let mut i = 0;

    while i < analysis.states.len() {
        if processed[i] {
            i += 1;
            continue;
        }
        let state = &analysis.states[i];

        let component = analysis
            .components
            .get(&analysis.component_roots[i])
            .cloned()
            .expect("every root is inserted into components during derivative_state_components");
        if component.len() > 1 {
            // Flush preceding scalar rows into a ScalarPrograms node.
            if !pending_scalar_programs.is_empty() {
                block
                    .nodes
                    .push(ComputeNode::ScalarPrograms(ScalarProgramBlock::new(
                        std::mem::take(&mut pending_scalar_programs),
                    )));
            }

            let group = component
                .iter()
                .map(|idx| analysis.states[*idx].clone())
                .collect::<Vec<_>>();
            let node = lower_linsolve_group(
                &group,
                &analysis.equations,
                &analysis.direct_assignments,
                dae_model,
                layout,
                &analysis.structural_bindings,
                &indexed_bindings,
            )?;
            block.nodes.push(node);
            for idx in component {
                processed[idx] = true;
            }
            i += 1;
            continue;
        }

        // Consecutive vector components sharing one direct RHS (e.g.
        // `der(X) = f(...)`) lower to a SINGLE multi-output program so the RHS
        // is computed once instead of re-inlined per component. This is a pure
        // optimization: if the shared base cannot be lowered as a whole vector,
        // fall back to per-state lowering below.
        if let Some(group_len) = direct_vector_group_len(analysis, &processed, i)
            && let Ok(row) = lower_direct_row_group(analysis, i, group_len, &lowering_ctx)
        {
            pending_scalar_programs.push(row);
            processed[i..i + group_len].fill(true);
            i += group_len;
            continue;
        }

        // Scalar / direct-equation state — build one row.
        let row = lower_state_derivative_row(state, &analysis.direct_equations, &lowering_ctx)?;
        pending_scalar_programs.push(row);
        processed[i] = true;
        i += 1;
    }

    if !pending_scalar_programs.is_empty() {
        block
            .nodes
            .push(ComputeNode::ScalarPrograms(ScalarProgramBlock::new(
                pending_scalar_programs,
            )));
    }

    Ok(block)
}

pub(super) fn lower_derivative_rhs_scalar_programs(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let analysis = analyze_derivative_rhs(dae_model)?;
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    let lowering_ctx = DerivativeRhsLoweringContext {
        equations: &analysis.equations,
        direct_assignments: &analysis.direct_assignments,
        dae_model,
        layout,
        structural_bindings: &analysis.structural_bindings,
        indexed_bindings: &indexed_bindings,
    };

    let mut rows = Vec::with_capacity(analysis.states.len());
    for (idx, state) in analysis.states.iter().enumerate() {
        let component = analysis
            .components
            .get(&analysis.component_roots[idx])
            .cloned()
            .expect("every root is inserted into components during derivative_state_components");
        if component.len() > 1 {
            let group = component
                .iter()
                .map(|state_idx| analysis.states[*state_idx].clone())
                .collect::<Vec<_>>();
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

struct DerivativeRhsLoweringContext<'a> {
    equations: &'a [DerivativeEquation],
    direct_assignments: &'a Arc<IndexMap<String, DirectAssignmentValue>>,
    dae_model: &'a dae::Dae,
    layout: &'a VarLayout,
    structural_bindings: &'a Arc<IndexMap<String, f64>>,
    indexed_bindings: &'a IndexedBindingMap,
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
        return lower_direct_row(
            row,
            state,
            ctx.direct_assignments,
            ctx.dae_model,
            ctx.layout,
            ctx.structural_bindings,
            ctx.indexed_bindings,
        );
    }
    lower_coupled_row(
        state,
        ctx.equations,
        ctx.direct_assignments,
        ctx.dae_model,
        ctx.layout,
        ctx.structural_bindings,
        ctx.indexed_bindings,
    )
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

fn derivative_state_components(
    states: &[StateScalar],
    equations: &[DerivativeEquation],
) -> (Vec<usize>, IndexMap<usize, Vec<usize>>) {
    let state_indices = states
        .iter()
        .enumerate()
        .map(|(idx, state)| (state.name.as_str(), idx))
        .collect::<IndexMap<_, _>>();
    let mut parent = (0..states.len()).collect::<Vec<_>>();
    for equation in equations {
        let row_indices = equation
            .coefficients
            .keys()
            .filter_map(|name| state_indices.get(name.as_str()).copied())
            .collect::<Vec<_>>();
        if let Some((&first, rest)) = row_indices.split_first() {
            for &idx in rest {
                union_components(&mut parent, first, idx);
            }
        }
    }

    let mut roots = Vec::with_capacity(states.len());
    let mut components = IndexMap::<usize, Vec<usize>>::new();
    for idx in 0..states.len() {
        let root = find_component_root(&mut parent, idx);
        roots.push(root);
        components.entry(root).or_default().push(idx);
    }
    (roots, components)
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
    direct_assignments: &Arc<IndexMap<String, DirectAssignmentValue>>,
    dae_model: &dae::Dae,
    layout: &VarLayout,
    structural_bindings: &Arc<IndexMap<String, f64>>,
    indexed_bindings: &IndexedBindingMap,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = row_builder(
        dae_model,
        layout,
        direct_assignments,
        structural_bindings,
        indexed_bindings,
    );
    let scope = Scope::new();
    let rhs = lower_state_component_expr(&mut builder, &equation.rhs, state, &scope)?;
    let coeff = builder.lower_expr(&equation.coefficients[&state.name], &scope, 0)?;
    let value = builder.emit_binary(BinaryOp::Div, rhs, coeff);
    builder.ops.push(LinearOp::StoreOutput { src: value });
    Ok(builder.ops)
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

/// Lower a group of consecutive vector-state components that share one direct
/// RHS into a single multi-output program: compute the RHS vector once, then
/// emit `value[component] / coeff` + StoreOutput for each component in order.
fn lower_direct_row_group(
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
    let values = builder.lower_array_like_values(head_base, &scope, 0)?;
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
        let value = builder.emit_binary(BinaryOp::Div, rhs, coeff);
        builder.ops.push(LinearOp::StoreOutput { src: value });
    }
    Ok(builder.ops)
}

fn lower_state_component_expr(
    builder: &mut LowerBuilder,
    expr: &rumoca_core::Expression,
    state: &StateScalar,
    scope: &Scope,
) -> Result<Reg, LowerError> {
    if state.base_size > 1 {
        return lower_row_rhs_expr(builder, expr, state.component, state.base_size, scope);
    }
    builder.lower_expr(expr, scope, 0)
}

fn lower_row_rhs_expr(
    builder: &mut LowerBuilder,
    expr: &rumoca_core::Expression,
    row_index: usize,
    row_count: usize,
    scope: &Scope,
) -> Result<Reg, LowerError> {
    if row_count <= 1 {
        return builder.lower_expr(expr, scope, 0);
    }

    let values = builder.lower_array_like_values(expr, scope, 0)?;
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
    Err(LowerError::Unsupported {
        reason: format!(
            "derivative RHS width {} does not match row count {row_count}",
            values.len()
        ),
    })
}

fn lower_coupled_row(
    state: &StateScalar,
    equations: &[DerivativeEquation],
    direct_assignments: &Arc<IndexMap<String, DirectAssignmentValue>>,
    dae_model: &dae::Dae,
    layout: &VarLayout,
    structural_bindings: &Arc<IndexMap<String, f64>>,
    indexed_bindings: &IndexedBindingMap,
) -> Result<Vec<LinearOp>, LowerError> {
    let base_rows = coupled_rows_for_base(equations, state);
    if base_rows.len() < state.base_size {
        return Err(LowerError::Unsupported {
            reason: format!("missing explicit derivative equation for `{}`", state.name),
        });
    }
    lower_dense_solve_component(
        state,
        &base_rows[..state.base_size],
        direct_assignments,
        dae_model,
        layout,
        structural_bindings,
        indexed_bindings,
    )
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
    equations: &[DerivativeEquation],
    direct_assignments: &Arc<IndexMap<String, DirectAssignmentValue>>,
    dae_model: &dae::Dae,
    layout: &VarLayout,
    structural_bindings: &Arc<IndexMap<String, f64>>,
    indexed_bindings: &IndexedBindingMap,
) -> Result<ComputeNode, LowerError> {
    let setup = build_dense_group_solve_setup(
        states,
        equations,
        direct_assignments,
        dae_model,
        layout,
        structural_bindings,
        indexed_bindings,
    )?;

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
    let mut setup = build_dense_group_solve_setup(
        states,
        ctx.equations,
        ctx.direct_assignments,
        ctx.dae_model,
        ctx.layout,
        ctx.structural_bindings,
        ctx.indexed_bindings,
    )?;
    let component = states
        .iter()
        .position(|group_state| group_state.name == state.name)
        .ok_or_else(|| LowerError::Unsupported {
            reason: format!(
                "state `{}` is not present in derivative solve group",
                state.name
            ),
        })?;
    let dst = setup.next_reg;
    setup.next_reg += 1;
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
    equations: &[DerivativeEquation],
    direct_assignments: &Arc<IndexMap<String, DirectAssignmentValue>>,
    dae_model: &dae::Dae,
    layout: &VarLayout,
    structural_bindings: &Arc<IndexMap<String, f64>>,
    indexed_bindings: &IndexedBindingMap,
) -> Result<DenseGroupSolveSetup, LowerError> {
    let n = states.len();
    let state_names = states
        .iter()
        .map(|state| state.name.as_str())
        .collect::<Vec<_>>();
    let rows = coupled_rows_for_states(equations, &state_names);
    if rows.len() < n {
        return Err(LowerError::Unsupported {
            reason: format!(
                "missing explicit derivative equations for coupled group `{}`: found {}/{} rows with coefficient keys {}",
                states
                    .iter()
                    .map(|state| state.name.as_str())
                    .collect::<Vec<_>>()
                    .join(", "),
                rows.len(),
                n,
                derivative_row_key_summary(&rows)
            ),
        });
    }

    let mut builder = row_builder(
        dae_model,
        layout,
        direct_assignments,
        structural_bindings,
        indexed_bindings,
    );
    let scope = Scope::new();
    let mut matrix_regs = Vec::new();
    let mut rhs_regs = Vec::new();

    for (row_idx, row) in rows[..n].iter().enumerate() {
        for state in states {
            matrix_regs.push(
                lower_or_zero(&mut builder, row.coefficients.get(&state.name), &scope).map_err(
                    |err| {
                        err.with_context(format!(
                            "lower derivative coefficient row {row_idx} for `{}`",
                            state.name
                        ))
                    },
                )?,
            );
        }
        rhs_regs.push(
            lower_row_rhs_expr(&mut builder, &row.rhs, row_idx, n, &scope)
                .map_err(|err| err.with_context(format!("lower derivative RHS row {row_idx}")))?,
        );
    }

    let matrix_start = pack_registers(&mut builder, &matrix_regs);
    let rhs_start = pack_registers(&mut builder, &rhs_regs);
    Ok(DenseGroupSolveSetup {
        ops: builder.ops,
        matrix_start,
        rhs_start,
        n,
        next_reg: builder.next_reg,
        span: rows
            .first()
            .map(|row| row.span)
            .unwrap_or(rumoca_core::Span::DUMMY),
    })
}

fn derivative_row_key_summary(rows: &[&DerivativeEquation]) -> String {
    if rows.is_empty() {
        return "[]".to_string();
    }
    let row_summaries = rows
        .iter()
        .map(|row| {
            format!(
                "[{}]",
                row.coefficients
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        })
        .collect::<Vec<_>>();
    format!("[{}]", row_summaries.join(", "))
}

fn lower_dense_solve_component(
    state: &StateScalar,
    rows: &[&DerivativeEquation],
    direct_assignments: &Arc<IndexMap<String, DirectAssignmentValue>>,
    dae_model: &dae::Dae,
    layout: &VarLayout,
    structural_bindings: &Arc<IndexMap<String, f64>>,
    indexed_bindings: &IndexedBindingMap,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = row_builder(
        dae_model,
        layout,
        direct_assignments,
        structural_bindings,
        indexed_bindings,
    );
    let scope = Scope::new();
    let mut matrix_regs = Vec::new();
    let mut rhs_regs = Vec::new();
    for (row_idx, row) in rows.iter().enumerate() {
        for component in 0..state.base_size {
            let name = format!("{}[{}]", state.base, component + 1);
            matrix_regs.push(
                lower_or_zero(&mut builder, row.coefficients.get(&name), &scope).map_err(
                    |err| {
                        err.with_context(format!(
                            "lower derivative coefficient row {row_idx} for `{name}`"
                        ))
                    },
                )?,
            );
        }
        rhs_regs.push(
            lower_row_rhs_expr(&mut builder, &row.rhs, row_idx, state.base_size, &scope)
                .map_err(|err| err.with_context(format!("lower derivative RHS row {row_idx}")))?,
        );
    }
    let matrix_start = pack_registers(&mut builder, &matrix_regs);
    let rhs_start = pack_registers(&mut builder, &rhs_regs);
    let dst = builder.alloc_reg();
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

fn lower_or_zero(
    builder: &mut LowerBuilder<'_>,
    expr: Option<&rumoca_core::Expression>,
    scope: &Scope,
) -> Result<Reg, LowerError> {
    match expr {
        Some(expr) => builder.lower_expr(expr, scope, 0),
        None => Ok(builder.emit_const(0.0)),
    }
}

fn pack_registers(builder: &mut LowerBuilder<'_>, regs: &[Reg]) -> Reg {
    let start = builder.next_reg;
    for &src in regs {
        let dst = builder.alloc_reg();
        builder.ops.push(LinearOp::Move { dst, src });
    }
    start
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
}

fn coupled_rows_for_base<'a>(
    equations: &'a [DerivativeEquation],
    state: &StateScalar,
) -> Vec<&'a DerivativeEquation> {
    equations
        .iter()
        .filter(|equation| {
            equation
                .coefficients
                .keys()
                .all(|name| dae::component_base_name(name).as_deref() == Some(state.base.as_str()))
        })
        .collect()
}

fn coupled_rows_for_states<'a>(
    equations: &'a [DerivativeEquation],
    state_names: &[&str],
) -> Vec<&'a DerivativeEquation> {
    equations
        .iter()
        .filter(|equation| {
            !equation.coefficients.is_empty()
                && equation
                    .coefficients
                    .keys()
                    .all(|name| state_names.contains(&name.as_str()))
        })
        .collect()
}

fn expanded_direct_derivative_equations(
    target: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    state_names: &[String],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    span: rumoca_core::Span,
) -> Option<Vec<DerivativeEquation>> {
    let target_keys = derivative_arg_binding_keys(target, dae_model, structural_bindings).ok()?;
    if target_keys.is_empty() || !target_keys.iter().all(|key| state_names.contains(key)) {
        return None;
    }
    let rhs_values = scalarized_rhs_expressions(
        rhs,
        target,
        target_keys.len(),
        dae_model,
        structural_bindings,
    )
    .ok()?;
    if rhs_values.len() != target_keys.len() {
        return None;
    }

    Some(
        target_keys
            .into_iter()
            .zip(rhs_values)
            .map(|(key, rhs)| DerivativeEquation {
                coefficients: IndexMap::from([(key, one_expr())]),
                rhs,
                span,
            })
            .collect(),
    )
}

fn derivative_equation_from_if_residual(
    residual: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
) -> Option<DerivativeEquation> {
    let span = match residual {
        rumoca_core::Expression::If { span, .. } => *span,
        _ => {
            return None;
        }
    };

    let (coefficients, remainder) = derivative_linear_parts(residual, ctx)?;
    Some(DerivativeEquation {
        coefficients,
        rhs: rhs_without_remainder(zero_expr(), remainder),
        span,
    })
}
#[cfg(test)]
mod tests {
    use super::*;

    fn real(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: Literal::Real(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn scalar_var(name: &str) -> dae::Variable {
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            ..Default::default()
        }
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn expression_result_dims_rejects_missing_scalar_binding() {
        let dae_model = dae::Dae::default();
        let err = expression_result_dims(&var_ref("missing"), &dae_model, &IndexMap::new())
            .expect_err("missing binding must not default to scalar shape");
        assert!(matches!(err, LowerError::MissingBinding { name } if name == "missing"));
    }

    #[test]
    fn expression_result_dims_accepts_existing_scalar_binding() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
        let dims = expression_result_dims(&var_ref("x"), &dae_model, &IndexMap::new())
            .expect("existing scalar binding has scalar shape");
        assert!(dims.is_empty());
    }

    #[test]
    fn binding_keys_reject_missing_scalarized_binding() {
        let dae_model = dae::Dae::default();
        let subscripts = vec![rumoca_core::Subscript::generated_index(
            1,
            rumoca_core::Span::DUMMY,
        )];
        let err = binding_keys_for_subscripted_name("x", &subscripts, &dae_model, &IndexMap::new())
            .expect_err("missing scalarized binding must not be fabricated");
        assert!(matches!(err, LowerError::MissingBinding { name } if name == "x[1]"));
    }

    #[test]
    fn binding_keys_accept_existing_scalarized_binding() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new("x[1]"), scalar_var("x[1]"));
        let subscripts = vec![rumoca_core::Subscript::generated_index(
            1,
            rumoca_core::Span::DUMMY,
        )];
        let keys =
            binding_keys_for_subscripted_name("x", &subscripts, &dae_model, &IndexMap::new())
                .expect("existing scalarized binding should lower");
        assert_eq!(keys, vec!["x[1]"]);
    }

    #[test]
    fn literal_array_elements_flat_flattens_matrix_rows_once() {
        let rows = vec![
            rumoca_core::Expression::Array {
                elements: vec![real(1.0), real(2.0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Array {
                elements: vec![real(3.0), real(4.0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
        ];

        let values = literal_array_elements_flat(&rows);
        let literals = values
            .iter()
            .map(|expr| match expr {
                rumoca_core::Expression::Literal {
                    value: Literal::Real(value),
                    ..
                } => *value,
                other => panic!("expected real literal, got {other:?}"),
            })
            .collect::<Vec<_>>();

        assert_eq!(literals, vec![1.0, 2.0, 3.0, 4.0]);
    }
}
