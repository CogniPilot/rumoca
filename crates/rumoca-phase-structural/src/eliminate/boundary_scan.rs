use super::*;

pub(super) struct BoundaryScanCtx<'a> {
    pub(super) dae: &'a Dae,
    pub(super) state_names: &'a [VarName],
    pub(super) unknown_index: &'a BoundaryUnknownIndex<'a>,
    pub(super) state_derivative_matcher: &'a DerivativeNameMatcher,
    pub(super) runtime_protected_unknowns: &'a IndexSet<String>,
    pub(super) runtime_defined_discrete_targets: &'a HashSet<String>,
    pub(super) direct_definitions: &'a DirectDefinitionIndex,
}

pub(super) struct BoundaryScanState {
    pub(super) resolved: HashSet<VarName>,
    pub(super) substitutions: Vec<Substitution>,
    pub(super) eliminated_eq_indices: Vec<usize>,
    pub(super) eliminated_eq_flags: Vec<bool>,
}

impl BoundaryScanState {
    pub(super) fn new(equation_count: usize) -> Self {
        Self {
            resolved: HashSet::new(),
            substitutions: Vec::new(),
            eliminated_eq_indices: Vec::new(),
            eliminated_eq_flags: vec![false; equation_count],
        }
    }

    fn push_solution(
        &mut self,
        dae: &Dae,
        eq_idx: usize,
        var_name: VarName,
        solution: Expression,
    ) -> Result<(), StructuralError> {
        self.substitutions
            .push(substitution_for_var(dae, var_name.clone(), solution)?);
        self.eliminated_eq_indices.push(eq_idx);
        self.eliminated_eq_flags[eq_idx] = true;
        self.resolved.insert(var_name);
        Ok(())
    }
}

pub(super) fn scan_boundary_equations(
    eq_order: Vec<(usize, usize)>,
    ctx: &BoundaryScanCtx<'_>,
    state: &mut BoundaryScanState,
) -> Result<(), StructuralError> {
    for (eq_idx, _) in eq_order {
        scan_boundary_equation(eq_idx, ctx, state)?;
    }
    Ok(())
}

fn scan_boundary_equation(
    eq_idx: usize,
    ctx: &BoundaryScanCtx<'_>,
    state: &mut BoundaryScanState,
) -> Result<(), StructuralError> {
    if state.eliminated_eq_flags[eq_idx] {
        return Ok(());
    }
    let equation = &ctx.dae.continuous.equations[eq_idx];
    let expr = equation_analysis_expr(equation);
    let eq_rhs = apply_substitutions_in_order(&expr, &state.substitutions)?;
    let is_connection_eq = equation.origin.starts_with("connection equation:");
    let live = find_live_scalar_unknowns(&eq_rhs, ctx.unknown_index, &state.resolved)?;
    if is_connection_eq
        && let Some((var_name, solution)) = scalar_connection_alias_for_elimination(
            ctx.dae,
            &eq_rhs,
            ctx.runtime_protected_unknowns,
            ctx.runtime_defined_discrete_targets,
        )?
    {
        return state.push_solution(ctx.dae, eq_idx, var_name, solution);
    }
    if should_skip_connection_equation(
        ctx.dae,
        &eq_rhs,
        is_connection_eq,
        &live,
        ctx.runtime_defined_discrete_targets,
    ) {
        return Ok(());
    }
    let has_state_derivative = expr_contains_der_of_any(&eq_rhs, ctx.state_derivative_matcher);
    let indexed_multiscalar_slice_equation =
        expr_contains_indexed_multiscalar_slice_ref(&eq_rhs, ctx.dae)?;
    if indexed_multiscalar_slice_equation {
        return Ok(());
    }
    if let Some((var_name, solution)) = aggregate_alias_for_elimination(
        ctx.dae,
        &eq_rhs,
        ctx.runtime_protected_unknowns,
        ctx.runtime_defined_discrete_targets,
    )? {
        return state.push_solution(ctx.dae, eq_idx, var_name, solution);
    }
    if live.is_empty() {
        let mut zero_unknown_ctx = ZeroUnknownEliminationCtx {
            dae: ctx.dae,
            state_names: ctx.state_names,
            unknown_index: ctx.unknown_index,
            resolved: &state.resolved,
            runtime_protected_unknowns: ctx.runtime_protected_unknowns,
            runtime_defined_discrete_targets: ctx.runtime_defined_discrete_targets,
            substitutions: &mut state.substitutions,
            eliminated_eq_indices: &mut state.eliminated_eq_indices,
            eliminated_eq_flags: &mut state.eliminated_eq_flags,
        };
        return try_eliminate_zero_unknown_equation(
            eq_idx,
            &eq_rhs,
            has_state_derivative,
            &mut zero_unknown_ctx,
        );
    }
    let indexed_flow_equation = is_flow_equation_origin(&equation.origin)
        && expr_contains_indexed_multiscalar_ref(&eq_rhs, ctx.dae)?;
    let can_eliminate_pairwise_flow_alias = equation.origin.starts_with("flow sum equation:")
        && can_eliminate_pairwise_flow_alias(ctx.dae, &eq_rhs, &live);
    if (indexed_flow_equation || indexed_multiscalar_slice_equation)
        && !can_eliminate_pairwise_flow_alias
    {
        return Ok(());
    }
    let choice_ctx = EliminationChoiceContext {
        dae: ctx.dae,
        eq_idx,
        has_state_derivative,
        runtime_protected_unknowns: ctx.runtime_protected_unknowns,
        direct_definitions: ctx.direct_definitions,
        allow_multi_live_trivial_alias: can_eliminate_pairwise_flow_alias,
    };
    if let Some((var_name, solution)) =
        choose_solvable_unknown_for_elimination(&choice_ctx, &eq_rhs, &live)?
    {
        state.push_solution(ctx.dae, eq_idx, var_name, solution)?;
    }
    Ok(())
}

fn can_eliminate_pairwise_flow_alias(dae: &Dae, eq_rhs: &Expression, live: &[VarName]) -> bool {
    live.len() <= 2
        && live.iter().any(|candidate| {
            try_solve_for_unknown_in_dae(dae, eq_rhs, candidate).is_some_and(|solution| {
                !expr_contains_unknown_in_dae(dae, &solution, candidate)
                    && is_trivial_alias_in_dae(dae, &solution)
            })
        })
}
