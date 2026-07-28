use super::*;

fn log_direct_demotion_scan_summary(
    trace: bool,
    state_count: usize,
    substitutions: &HashMap<String, DirectStateDemotionPlan>,
    counters: &DirectDemotionCounters,
) {
    if !trace {
        return;
    }
    crate::structural_trace!(
        "[sim-trace] direct-assignment-demotion scan: states={} candidates={} accepted={} skip_flow_sum_origin={} skip_unsafe_non_state_alias={} skip_when={} skip_always={} skip_self_der={} skip_derivative_alias_feedback={} skip_der_in_defining_expr={} skip_nonsmooth_defining_expr={} skip_unsliced_vector_ref={} skip_no_der={} skip_non_state_der={}",
        state_count,
        counters.n_candidates,
        substitutions.len(),
        counters.n_skip_flow_sum_origin,
        counters.n_skip_unsafe_non_state_alias,
        counters.n_skip_when_assigned,
        counters.n_skip_always_state,
        counters.n_skip_self_der,
        counters.n_skip_derivative_alias_feedback,
        counters.n_skip_der_in_defining_expr,
        counters.n_skip_nonsmooth_defining_expr,
        counters.n_skip_unsliced_vector_ref,
        counters.n_skip_no_der_expr,
        counters.n_skip_non_state_der
    );
}

pub(super) fn collect_non_state_continuous_unknown_names(dae: &Dae) -> HashSet<String> {
    let mut names = HashSet::new();
    for (name, variable) in dae
        .variables
        .algebraics
        .iter()
        .chain(dae.variables.outputs.iter())
    {
        names.insert(name.as_str().to_string());
        if variable.dims.is_empty() {
            continue;
        }
        for flat_index in 0..variable.size() {
            names.insert(dae::scalar_name_text_for_flat_index(
                name.as_str(),
                &variable.dims,
                flat_index,
            ));
        }
    }
    names
}

pub(super) fn is_connection_equation_origin(origin: &str) -> bool {
    origin.starts_with("connection equation:")
}

pub(super) fn expr_refs_only_parameters_constants_or_time(dae: &Dae, expr: &Expression) -> bool {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().all(|name| {
        name.as_str() == "time"
            || dae.variables.parameters.contains_key(&name)
            || dae.variables.constants.contains_key(&name)
    })
}

pub(super) fn expression_contains_any_der_call(expr: &Expression) -> bool {
    dae::ContainsDerChecker::check(expr)
}

pub(super) fn equation_defining_expr_for_unknown(
    dae: &Dae,
    eq: &Equation,
    unknown_name: &VarName,
) -> Option<Expression> {
    if let Some(lhs) = eq.lhs.as_ref()
        && lhs.var_name() == unknown_name
    {
        if expression_contains_any_der_call(&eq.rhs) {
            return None;
        }
        return Some(eq.rhs.clone());
    }
    if let Expression::Binary { op, lhs, rhs, .. } = &eq.rhs
        && *op == OpBinary::Sub
    {
        if full_width_aggregate_target(dae, eq, lhs, unknown_name)
            && !expr_contains_var(rhs, unknown_name)
            && !expression_contains_any_der_call(rhs)
        {
            return Some(*rhs.clone());
        }
        if full_width_aggregate_target(dae, eq, rhs, unknown_name)
            && !expr_contains_var(lhs, unknown_name)
            && !expression_contains_any_der_call(lhs)
        {
            return Some(*lhs.clone());
        }
    }
    let defining_expr = residual_defining_expr(eq, unknown_name)?;
    if expression_contains_any_der_call(&defining_expr) {
        return None;
    }
    Some(defining_expr)
}

/// Whether one indexed reference accounts for the target's complete scalar
/// width in this equation.
///
/// The subscript coverage and scalar-count equality form the shape
/// certificate. Every explicit subscript must retain the complete declared
/// dimension, except that selecting index one of a singleton dimension is
/// equivalent to selecting that complete dimension. This rejects a component
/// row such as `x[1] = ...` for an aggregate `x[3]`, while accepting both
/// ToDAE's compact `x[:]` rows and singleton aggregate rows.
fn full_width_aggregate_target(
    dae: &Dae,
    equation: &Equation,
    expr: &Expression,
    target: &VarName,
) -> bool {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return false;
    };
    if subscripts.is_empty() || name.var_name() != target {
        return false;
    }
    let variable = dae
        .variables
        .algebraics
        .get(target)
        .or_else(|| dae.variables.outputs.get(target))
        .or_else(|| dae.variables.states.get(target));
    variable.is_some_and(|variable| {
        subscripts_cover_complete_shape(subscripts, &variable.dims)
            && equation.scalar_count == variable.size()
    })
}

fn subscripts_cover_complete_shape(subscripts: &[Subscript], dims: &[i64]) -> bool {
    if subscripts.len() > dims.len() {
        return false;
    }
    subscripts
        .iter()
        .zip(dims)
        .all(|(subscript, dim)| match subscript {
            Subscript::Colon { .. } => true,
            Subscript::Index { value: 1, .. } => *dim == 1,
            Subscript::Index { .. } | Subscript::Expr { .. } => false,
        })
}

/// Value-closure well-foundedness over defining expressions.
///
/// MLS Appendix B / SPEC_0022: variables that appear differentiated remain
/// states, so a direct-assignment candidate is only a dummy trajectory when its
/// defining expression resolves to a value that is *determined* and reaches no
/// state.
///
/// "Determined" is a least fixpoint: a non-state unknown is settled once one of
/// its invertible rows reads only parameters, constants, `time`, and already
/// settled unknowns. Both halves of that matter.
///
/// * Taking *some* row rather than the only row is what lets the scan cross a
///   connector node. A two-pin component states its current twice (`i =
///   pin_p.i` and `i = -pin_n.i`) and a node states it again through the flow
///   sum, so requiring a unique defining row refuses every current in an
///   electrical circuit — including the constant excitation current a DC
///   machine's flux is pinned to.
/// * Requiring the fixpoint to close, rather than treating a cycle as
///   harmless, is what keeps a free coordinate free. A translational flange
///   position is defined only in terms of its neighbours' positions; that
///   cycle never settles, and reading it as "no state found" would demote a
///   mass position that nothing determines.
struct AliasClosureScan<'a> {
    dae: &'a Dae,
    definitions: &'a DefiningExprIndex,
    state_name_set: &'a HashSet<String>,
    non_state_unknown_names: &'a HashSet<String>,
    structural_bindings: &'a HashMap<String, f64>,
    excluded_eq_index: usize,
}

impl AliasClosureScan<'_> {
    fn is_non_state_unknown(&self, name: &VarName) -> bool {
        self.non_state_unknown_names.contains(name.as_str())
    }

    fn value_is_undetermined_or_state_dependent(&self, defining_expr: &Expression) -> bool {
        let Some(roots) = self.non_state_refs(defining_expr) else {
            return true;
        };
        let reachable = self.reachable_closure(&roots);
        let settled = self.settled_names(&reachable);
        !roots.iter().all(|name| settled.contains(name.as_str()))
    }

    /// Non-state unknowns read by `expr`, or `None` if `expr` reads a state.
    fn non_state_refs(&self, expr: &Expression) -> Option<IndexSet<VarName>> {
        let refs = derivative_coordinate_dependencies_with_bindings(
            self.dae,
            self.structural_bindings,
            expr,
        );
        let mut out = IndexSet::new();
        for ref_name in refs {
            if self.state_name_set.contains(ref_name.as_str()) {
                return None;
            }
            if self.non_state_unknown_names.contains(ref_name.as_str()) {
                out.insert(ref_name);
            }
        }
        Some(out)
    }

    fn usable_candidates(&self, name: &VarName) -> impl Iterator<Item = &Expression> {
        self.definitions
            .get(name.as_str())
            .into_iter()
            .flatten()
            .filter(|candidate| candidate.equation_index != self.excluded_eq_index)
            .map(|candidate| &candidate.expr)
    }

    /// Every non-state unknown any defining row in the closure can read.
    fn reachable_closure(&self, roots: &IndexSet<VarName>) -> IndexSet<VarName> {
        let mut reachable = roots.clone();
        let mut next = 0;
        while next < reachable.len() {
            let name = reachable
                .get_index(next)
                .expect("closure index is bounded by the set length")
                .clone();
            for candidate in self.usable_candidates(&name) {
                let refs = derivative_coordinate_dependencies_with_bindings(
                    self.dae,
                    self.structural_bindings,
                    candidate,
                );
                reachable.extend(
                    refs.into_iter()
                        .filter(|name| self.is_non_state_unknown(name)),
                );
            }
            next += 1;
        }
        reachable
    }

    fn settled_names(&self, reachable: &IndexSet<VarName>) -> HashSet<String> {
        let mut settled: HashSet<String> = HashSet::new();
        loop {
            let newly_settled = reachable
                .iter()
                .filter(|name| !settled.contains(name.as_str()))
                .filter(|name| self.has_settled_candidate(name, &settled))
                .map(|name| name.as_str().to_string())
                .collect::<Vec<_>>();
            if newly_settled.is_empty() {
                return settled;
            }
            settled.extend(newly_settled);
        }
    }

    /// Names in `roots` whose value closure is independent of time, states,
    /// inputs, and discrete storage.
    ///
    /// This is a constructive zero-derivative certificate. Each accepted name
    /// has a defining candidate whose references are parameters, constants, or
    /// names accepted by an earlier least-fixpoint round.
    fn time_invariant_names(&self, roots: &IndexSet<VarName>) -> Option<HashSet<String>> {
        let reachable = self.reachable_closure(roots);
        let mut settled = HashSet::new();
        loop {
            let newly_settled = reachable
                .iter()
                .filter(|name| !settled.contains(name.as_str()))
                .filter(|name| self.has_time_invariant_candidate(name, &settled))
                .map(|name| name.as_str().to_string())
                .collect::<Vec<_>>();
            if newly_settled.is_empty() {
                break;
            }
            settled.extend(newly_settled);
        }
        roots
            .iter()
            .all(|name| settled.contains(name.as_str()))
            .then_some(settled)
    }

    fn has_time_invariant_candidate(&self, name: &VarName, settled: &HashSet<String>) -> bool {
        self.usable_candidates(name)
            .any(|candidate| self.candidate_is_time_invariant(candidate, settled))
    }

    fn candidate_is_time_invariant(
        &self,
        candidate: &Expression,
        settled: &HashSet<String>,
    ) -> bool {
        derivative_coordinate_dependencies_with_bindings(
            self.dae,
            self.structural_bindings,
            candidate,
        )
        .iter()
        .all(|name| self.reference_is_time_invariant(name, settled))
    }

    fn reference_is_time_invariant(&self, name: &VarName, settled: &HashSet<String>) -> bool {
        if self.state_name_set.contains(name.as_str()) || name.as_str() == "time" {
            return false;
        }
        if self.non_state_unknown_names.contains(name.as_str()) {
            return settled.contains(name.as_str());
        }
        self.definitions_are_compile_time_known(name)
    }

    fn definitions_are_compile_time_known(&self, name: &VarName) -> bool {
        // Names outside the continuous-unknown set are accepted only when
        // their declaration proves compile-time ownership. Inputs, discrete
        // storage, and undeclared references are deliberately rejected.
        self.dae.variables.parameters.contains_key(name)
            || self.dae.variables.constants.contains_key(name)
    }

    fn has_settled_candidate(&self, name: &VarName, settled: &HashSet<String>) -> bool {
        self.usable_candidates(name).any(|candidate| {
            let refs = derivative_coordinate_dependencies_with_bindings(
                self.dae,
                self.structural_bindings,
                candidate,
            );
            refs.iter().all(|ref_name| {
                !self.state_name_set.contains(ref_name.as_str())
                    && (!self.non_state_unknown_names.contains(ref_name.as_str())
                        || settled.contains(ref_name.as_str()))
            })
        })
    }
}

fn defining_expr_references_unsafe_non_state_alias_closure(
    dae: &Dae,
    definitions: &DefiningExprIndex,
    defining_expr: &Expression,
    state_name_set: &HashSet<String>,
    non_state_unknown_names: &HashSet<String>,
    structural_bindings: &HashMap<String, f64>,
    excluded_eq_index: usize,
) -> bool {
    AliasClosureScan {
        dae,
        definitions,
        state_name_set,
        non_state_unknown_names,
        structural_bindings,
        excluded_eq_index,
    }
    .value_is_undetermined_or_state_dependent(defining_expr)
}

fn apply_direct_demotion_plans(
    dae: &mut Dae,
    substitutions: &HashMap<String, DirectStateDemotionPlan>,
) -> usize {
    let mut demoted_this_round = 0usize;
    let mut plans: Vec<&DirectStateDemotionPlan> = substitutions.values().collect();
    plans.sort_by(|a, b| a.state_name.as_str().cmp(b.state_name.as_str()));
    for plan in plans {
        demoted_this_round += apply_direct_demotion_plan(dae, plan);
    }
    demoted_this_round
}

pub(super) fn apply_direct_demotion_plan(dae: &mut Dae, plan: &DirectStateDemotionPlan) -> usize {
    rewrite_state_derivative_everywhere(dae, &plan.state_name, &plan.der_expr);
    if let Some(var) = dae.variables.states.shift_remove(&plan.state_name) {
        dae.variables
            .algebraics
            .insert(plan.state_name.clone(), var);
        return 1;
    }
    0
}

/// Why a direct-assignment row was not turned into a demotion plan.
///
/// The scan summary only carries totals, which cannot say which state a given
/// refusal belongs to. Naming the state next to the reason is what makes a
/// single blocked index-reduction chain findable in a whole-library trace.
#[derive(Clone, Copy)]
enum DirectDemotionReject {
    FlowSumOrigin,
    WhenAssigned,
    AlwaysState,
    SelfDerDefiningExpr,
    SelfDerReplacement,
    DerivativeAliasFeedback,
    DerInDefiningExpr,
    NonsmoothDefiningExpr,
    UnsafeNonStateAlias,
    UnslicedVectorRef,
    NonStateDer,
}

fn reject_direct_demotion(
    round: &DirectDemotionRound<'_>,
    counters: &mut DirectDemotionCounters,
    state_name: &VarName,
    reason: DirectDemotionReject,
) -> Option<DirectStateDemotionPlan> {
    let label = match reason {
        DirectDemotionReject::FlowSumOrigin => {
            counters.n_skip_flow_sum_origin += 1;
            "flow_sum_origin"
        }
        DirectDemotionReject::WhenAssigned => {
            counters.n_skip_when_assigned += 1;
            "when_assigned"
        }
        DirectDemotionReject::AlwaysState => {
            counters.n_skip_always_state += 1;
            "state_select_always"
        }
        DirectDemotionReject::SelfDerDefiningExpr => {
            counters.n_skip_self_der += 1;
            "self_der_defining_expr"
        }
        DirectDemotionReject::SelfDerReplacement => {
            counters.n_skip_self_der += 1;
            "self_der_replacement"
        }
        DirectDemotionReject::DerivativeAliasFeedback => {
            counters.n_skip_derivative_alias_feedback += 1;
            "derivative_alias_feedback"
        }
        DirectDemotionReject::DerInDefiningExpr => {
            counters.n_skip_der_in_defining_expr += 1;
            "der_in_defining_expr"
        }
        DirectDemotionReject::NonsmoothDefiningExpr => {
            counters.n_skip_nonsmooth_defining_expr += 1;
            "nonsmooth_defining_expr"
        }
        DirectDemotionReject::UnsafeNonStateAlias => {
            counters.n_skip_unsafe_non_state_alias += 1;
            "unsafe_non_state_alias"
        }
        DirectDemotionReject::UnslicedVectorRef => {
            counters.n_skip_unsliced_vector_ref += 1;
            "unsliced_vector_ref"
        }
        DirectDemotionReject::NonStateDer => {
            counters.n_skip_non_state_der += 1;
            "non_state_der"
        }
    };
    if round.trace {
        crate::structural_trace!(
            "[sim-trace] direct-assignment rejected state={} reason={}",
            state_name.as_str(),
            label
        );
    }
    None
}

fn direct_demotion_plan_for_equation(
    round: &DirectDemotionRound<'_>,
    eq_index: usize,
    eq: &Equation,
    counters: &mut DirectDemotionCounters,
) -> Result<Option<DirectStateDemotionPlan>, StructuralError> {
    let Some((state_name, defining_expr)) =
        extract_state_direct_assignment_equation(eq, &round.state_names, &round.state_name_set)
    else {
        return Ok(None);
    };
    let defining_expr = if is_connection_equation_origin(&eq.origin) {
        match connection_component_fixed_defining_expr(
            round.dae,
            &state_name,
            &round.state_name_set,
        ) {
            Some(expr) => expr,
            None => defining_expr,
        }
    } else {
        defining_expr
    };
    counters.n_candidates += 1;
    if eq.origin.starts_with("flow sum equation:") {
        return Ok(reject_direct_demotion(
            round,
            counters,
            &state_name,
            DirectDemotionReject::FlowSumOrigin,
        ));
    }
    log_direct_assignment_candidate(round.trace, counters, round.dae, eq, &state_name);
    direct_demotion_plan_for_state(round, eq_index, &state_name, defining_expr, counters)
}

fn direct_demotion_plan_for_state(
    round: &DirectDemotionRound<'_>,
    eq_index: usize,
    state_name: &VarName,
    defining_expr: Expression,
    counters: &mut DirectDemotionCounters,
) -> Result<Option<DirectStateDemotionPlan>, StructuralError> {
    if round.when_assigned_states.contains(state_name.as_str()) {
        return Ok(reject_direct_demotion(
            round,
            counters,
            state_name,
            DirectDemotionReject::WhenAssigned,
        ));
    }
    let Some(state) = round.dae.variables.states.get(state_name) else {
        return Ok(None);
    };
    if state.state_select == rumoca_core::StateSelect::Always {
        return Ok(reject_direct_demotion(
            round,
            counters,
            state_name,
            DirectDemotionReject::AlwaysState,
        ));
    }
    if expr_contains_der_of(&defining_expr, state_name) {
        return Ok(reject_direct_demotion(
            round,
            counters,
            state_name,
            DirectDemotionReject::SelfDerDefiningExpr,
        ));
    }
    if !state_ders_in_expr_independently_defined(&defining_expr, state_name, round)? {
        return Ok(reject_direct_demotion(
            round,
            counters,
            state_name,
            DirectDemotionReject::DerInDefiningExpr,
        ));
    }
    if !super::state_row_reduction::expression_has_piecewise_smooth_values(
        &defining_expr,
        round.dae,
        &round.structural_bindings,
    ) {
        return Ok(reject_direct_demotion(
            round,
            counters,
            state_name,
            DirectDemotionReject::NonsmoothDefiningExpr,
        ));
    }
    // `der(state)` links are substituted symbolically on demotion (gated by
    // `state_ders_in_expr_independently_defined` above and validated again in
    // `choose_derivative_replacement`), so mask them before scanning for value
    // dependencies on states or unsafe alias closures.
    let alias_scan_expr = mask_state_der_calls(&defining_expr, &round.state_name_set);
    if defining_expr_references_unsafe_non_state_alias_closure(
        round.dae,
        &round.non_state_defining_exprs,
        &alias_scan_expr,
        &round.state_name_set,
        &round.non_state_unknown_names,
        &round.structural_bindings,
        eq_index,
    ) {
        return Ok(reject_direct_demotion(
            round,
            counters,
            state_name,
            DirectDemotionReject::UnsafeNonStateAlias,
        ));
    }
    if state.size() > 1 || expr_contains_unsliced_vector_ref(&defining_expr, round.dae) {
        // MLS §10.1: array state shape is semantic IR. This path substitutes
        // whole `der(state)` calls, so unsliced compound states stay intact.
        return Ok(reject_direct_demotion(
            round,
            counters,
            state_name,
            DirectDemotionReject::UnslicedVectorRef,
        ));
    }
    direct_demotion_replacement_plan(round, eq_index, state_name, &defining_expr, counters)
}

fn trace_independent_derivative_map(
    round: &DirectDemotionRound<'_>,
    state_name: &VarName,
    independent_map: &HashMap<String, Expression>,
) {
    if !round.trace {
        return;
    }
    for state in round.state_names.iter().take(8) {
        crate::structural_trace!(
            "[sim-trace] independent derivative replacement blocked candidate={} state={} value={}",
            state_name.as_str(),
            state.as_str(),
            independent_map
                .get(state.as_str())
                .map(|value| truncate_debug(&format!("{value:?}"), 420))
                .unwrap_or_else(|| "<missing>".to_string())
        );
    }
}

/// Differentiate the accepted defining expression and check the result is a
/// usable replacement for `der(state_name)`.
fn direct_demotion_replacement_plan(
    round: &DirectDemotionRound<'_>,
    defining_equation: usize,
    state_name: &VarName,
    defining_expr: &Expression,
    counters: &mut DirectDemotionCounters,
) -> Result<Option<DirectStateDemotionPlan>, StructuralError> {
    let shared_derivative = choose_derivative_replacement(
        defining_expr,
        &round.state_name_set,
        round.dae,
        &round.der_map,
        counters,
    );
    let mut der_expr = match shared_derivative {
        Some(derivative) => derivative,
        None => {
            // The DAE-wide closure can choose a valid but circular definition
            // for one member of a trajectory chain. Rebuild against this
            // candidate alone, explicitly rejecting its own derivative
            // relation. Acceptance then carries a stronger witness: the
            // replacement is derivable without the relation it will rewrite.
            let independent_map = build_independent_derivative_map_for_direct_state_definition(
                round.dae,
                defining_expr,
                state_name,
            )?;
            let mut independent_map = independent_map;
            seed_time_invariant_derivatives(
                round,
                defining_equation,
                defining_expr,
                &mut independent_map,
            );
            let Some(derivative) = choose_derivative_replacement(
                defining_expr,
                &round.state_name_set,
                round.dae,
                &independent_map,
                counters,
            ) else {
                trace_independent_derivative_map(round, state_name, &independent_map);
                return Ok(None);
            };
            derivative
        }
    };
    if expr_contains_der_of(&der_expr, state_name) {
        return Ok(reject_direct_demotion(
            round,
            counters,
            state_name,
            DirectDemotionReject::SelfDerReplacement,
        ));
    }
    if derivative_replacement_reads_own_alias(round.dae, state_name, &der_expr) {
        let independent_map = build_independent_derivative_map_for_direct_state_definition(
            round.dae,
            defining_expr,
            state_name,
        )?;
        let Some(independent_der_expr) = choose_derivative_replacement(
            defining_expr,
            &round.state_name_set,
            round.dae,
            &independent_map,
            counters,
        ) else {
            return Ok(reject_direct_demotion(
                round,
                counters,
                state_name,
                DirectDemotionReject::DerivativeAliasFeedback,
            ));
        };
        der_expr = independent_der_expr;
        if derivative_replacement_reads_own_alias(round.dae, state_name, &der_expr) {
            return Ok(reject_direct_demotion(
                round,
                counters,
                state_name,
                DirectDemotionReject::DerivativeAliasFeedback,
            ));
        }
    }
    if expr_contains_der_of_non_state(&der_expr, &round.state_name_set) {
        return Ok(reject_direct_demotion(
            round,
            counters,
            state_name,
            DirectDemotionReject::NonStateDer,
        ));
    }
    if round.trace && counters.n_trace_logged_candidates < 16 {
        crate::structural_trace!(
            "[sim-trace] direct-assignment accepted state={} der_expr={}",
            state_name.as_str(),
            truncate_debug(&format!("{:?}", der_expr), 1200)
        );
        counters.n_trace_logged_candidates += 1;
    }
    Ok(Some(DirectStateDemotionPlan {
        state_name: state_name.clone(),
        der_expr,
    }))
}

fn seed_time_invariant_derivatives(
    round: &DirectDemotionRound<'_>,
    defining_equation: usize,
    defining_expr: &Expression,
    derivative_map: &mut HashMap<String, Expression>,
) {
    let scan = AliasClosureScan {
        dae: round.dae,
        definitions: &round.non_state_defining_exprs,
        state_name_set: &round.state_name_set,
        non_state_unknown_names: &round.non_state_unknown_names,
        structural_bindings: &round.structural_bindings,
        excluded_eq_index: defining_equation,
    };
    let Some(roots) = scan.non_state_refs(defining_expr) else {
        return;
    };
    let Some(time_invariant) = scan.time_invariant_names(&roots) else {
        return;
    };
    for name in time_invariant {
        let name = VarName::new(name);
        let variable = round
            .dae
            .variables
            .algebraics
            .get(&name)
            .or_else(|| round.dae.variables.outputs.get(&name));
        if let Some(variable) = variable {
            derivative_map.insert(name.as_str().to_string(), zero_expr(variable.source_span));
        }
    }
}

/// Whether the proposed replacement reads a variable whose defining row is
/// `alias = der(state_name)`.
///
/// Using that alias to justify demotion is circular: rewriting the defining row
/// turns it into `alias = alias`, so the transformation removes the only
/// evidence relating the successor trajectory to the candidate state. This
/// exact name-membership check is the local certificate that every accepted
/// derivative replacement is independent of the derivative relation it will
/// rewrite.
fn derivative_replacement_reads_own_alias(
    dae: &Dae,
    state_name: &VarName,
    replacement: &Expression,
) -> bool {
    let aliases = dae
        .continuous
        .equations
        .iter()
        .filter_map(|equation| try_extract_derivative_alias(equation, state_name))
        .collect::<HashSet<_>>();
    if aliases.is_empty() {
        return false;
    }
    let mut refs = IndexSet::new();
    replacement.collect_var_refs(&mut refs);
    refs.iter().any(|name| aliases.contains(name))
}

/// `der(z)` links inside a defining expression are demotable only when `z`'s
/// own derivative definition is closed-form (not the symbolic `der(z)`
/// fallback) and does not feed back through the candidate state. This admits
/// differentiator chains (`y = der(x)` reading a state with its own ODE row)
/// while keeping kinematic aliases as states: in `v = der(s)` the alias row
/// itself defines `der(s) = v`, so `der(s)`'s value contains the candidate.
fn state_ders_in_expr_independently_defined(
    defining_expr: &Expression,
    candidate: &VarName,
    round: &DirectDemotionRound<'_>,
) -> Result<bool, StructuralError> {
    let inner_states = derivative_states_in_eq(defining_expr, &round.state_names);
    let shared_is_independent = |inner_state: &VarName| {
        round
            .der_map
            .get(inner_state.as_str())
            .is_some_and(|value| {
                !expr_contains_der_of(value, inner_state) && !expr_contains_var(value, candidate)
            })
    };
    if inner_states.iter().all(shared_is_independent) {
        return Ok(true);
    }

    // The DAE-wide map may choose the connection row that feeds the candidate
    // (`der1.y = der2.u`) even when an independently prolonged row also proves
    // `der(der1.u)`. Rebuild with the candidate derivative forbidden. The
    // resulting map is a local acyclicity certificate: every derivative read
    // by this defining expression has a value that neither refers to itself
    // nor reaches the state being considered for demotion.
    let independent_map = build_independent_derivative_map_for_direct_state_definition(
        round.dae,
        defining_expr,
        candidate,
    )?;
    let independent = inner_states.iter().all(|inner_state| {
        independent_map
            .get(inner_state.as_str())
            .is_some_and(|value| {
                !expr_contains_der_of(value, inner_state) && !expr_contains_var(value, candidate)
            })
    });
    if !independent && round.trace {
        for inner_state in &inner_states {
            crate::structural_trace!(
                "[sim-trace] derivative-reader certificate blocked candidate={} inner_state={} shared={} independent={}",
                candidate.as_str(),
                inner_state.as_str(),
                round
                    .der_map
                    .get(inner_state.as_str())
                    .map(|value| truncate_debug(&format!("{value:?}"), 320))
                    .unwrap_or_else(|| "<missing>".to_string()),
                independent_map
                    .get(inner_state.as_str())
                    .map(|value| truncate_debug(&format!("{value:?}"), 320))
                    .unwrap_or_else(|| "<missing>".to_string())
            );
        }
    }
    Ok(independent)
}

fn collect_direct_demotion_plans(
    dae: &Dae,
    trace: bool,
) -> Result<HashMap<String, DirectStateDemotionPlan>, StructuralError> {
    let timer = structural_timing_start("direct_demotion.collect_round");
    let Some(round) = DirectDemotionRound::new(dae, trace)? else {
        return Ok(HashMap::new());
    };
    structural_timing_done("direct_demotion.collect_round", timer);
    let mut substitutions = HashMap::new();
    let mut counters = DirectDemotionCounters::default();

    let timer = structural_timing_start("direct_demotion.scan_equations");
    for (eq_index, eq) in round.dae.continuous.equations.iter().enumerate() {
        let Some(plan) = direct_demotion_plan_for_equation(&round, eq_index, eq, &mut counters)?
        else {
            continue;
        };
        substitutions
            .entry(plan.state_name.as_str().to_string())
            .or_insert(plan);
    }
    structural_timing_done("direct_demotion.scan_equations", timer);

    log_direct_demotion_scan_summary(trace, round.state_count(), &substitutions, &counters);
    Ok(substitutions)
}

/// Demote states that are explicitly defined by direct assignment equations
/// (`state = expr`) and substitute `der(state)` with `d/dt(expr)` throughout
/// the system.
///
/// This removes structurally over-constrained "dummy/trajectory" states from
/// the differential set and keeps derivative chains algebraically consistent.
/// The defining expression need not reference `time` directly; if `d/dt(expr)`
/// can be resolved without introducing derivatives of non-state variables, the
/// state is demoted. States assigned in `when` clauses are preserved, since
/// they participate in event/reinit updates and must remain in the state vector.
pub fn demote_direct_assigned_states(dae: &mut Dae) -> Result<usize, StructuralError> {
    let mut total_demoted = 0usize;
    let mut round_index = 0usize;

    loop {
        let trace = sim_trace_enabled();
        let label = format!("direct_demotion.round[{round_index}].collect_plans");
        let timer = structural_timing_start(&label);
        let substitutions = collect_direct_demotion_plans(dae, trace)?;
        structural_timing_done(&label, timer);

        if substitutions.is_empty() {
            break;
        }

        let label = format!("direct_demotion.round[{round_index}].apply_plans");
        let timer = structural_timing_start(&label);
        let demoted_this_round = apply_direct_demotion_plans(dae, &substitutions);
        structural_timing_done(&label, timer);

        if demoted_this_round == 0 {
            break;
        }
        total_demoted += demoted_this_round;
        round_index += 1;
    }

    Ok(total_demoted)
}
