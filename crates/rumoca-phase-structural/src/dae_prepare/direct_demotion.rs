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
    dae.variables
        .algebraics
        .keys()
        .chain(dae.variables.outputs.keys())
        .map(|name| name.as_str().to_string())
        .collect()
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
    let defining_expr = residual_defining_expr(eq, unknown_name)?;
    if expression_contains_any_der_call(&defining_expr) {
        return None;
    }
    Some(defining_expr)
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
    definitions: &'a DefiningExprIndex,
    state_name_set: &'a HashSet<String>,
    non_state_unknown_names: &'a HashSet<String>,
    excluded_eq_index: usize,
}

impl AliasClosureScan<'_> {
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
        let mut refs = IndexSet::new();
        expr.collect_var_refs(&mut refs);
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
            let mut refs = IndexSet::new();
            for candidate in self.usable_candidates(&name) {
                candidate.collect_var_refs(&mut refs);
            }
            reachable.extend(
                refs.into_iter()
                    .filter(|ref_name| self.non_state_unknown_names.contains(ref_name.as_str())),
            );
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

    fn has_settled_candidate(&self, name: &VarName, settled: &HashSet<String>) -> bool {
        self.usable_candidates(name).any(|candidate| {
            let mut refs = IndexSet::new();
            candidate.collect_var_refs(&mut refs);
            refs.iter().all(|ref_name| {
                !self.state_name_set.contains(ref_name.as_str())
                    && (!self.non_state_unknown_names.contains(ref_name.as_str())
                        || settled.contains(ref_name.as_str()))
            })
        })
    }
}

fn defining_expr_references_unsafe_non_state_alias_closure(
    definitions: &DefiningExprIndex,
    defining_expr: &Expression,
    state_name_set: &HashSet<String>,
    non_state_unknown_names: &HashSet<String>,
    excluded_eq_index: usize,
) -> bool {
    AliasClosureScan {
        definitions,
        state_name_set,
        non_state_unknown_names,
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
    if !state_ders_in_expr_independently_defined(&defining_expr, state_name, round) {
        return Ok(reject_direct_demotion(
            round,
            counters,
            state_name,
            DirectDemotionReject::DerInDefiningExpr,
        ));
    }
    if !super::state_row_reduction::expression_is_smooth_for_index_reduction(
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
        &round.non_state_defining_exprs,
        &alias_scan_expr,
        &round.state_name_set,
        &round.non_state_unknown_names,
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
    direct_demotion_replacement_plan(round, state_name, &defining_expr, counters)
}

/// Differentiate the accepted defining expression and check the result is a
/// usable replacement for `der(state_name)`.
fn direct_demotion_replacement_plan(
    round: &DirectDemotionRound<'_>,
    state_name: &VarName,
    defining_expr: &Expression,
    counters: &mut DirectDemotionCounters,
) -> Result<Option<DirectStateDemotionPlan>, StructuralError> {
    let Some(mut der_expr) = choose_derivative_replacement(
        defining_expr,
        &round.state_name_set,
        round.dae,
        &round.der_map,
        counters,
    ) else {
        return Ok(None);
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
) -> bool {
    derivative_states_in_eq(defining_expr, &round.state_names)
        .iter()
        .all(|inner_state| {
            round
                .der_map
                .get(inner_state.as_str())
                .is_some_and(|value| {
                    !expr_contains_der_of(value, inner_state)
                        && !expr_contains_var(value, candidate)
                })
        })
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
