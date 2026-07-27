use super::*;

/// Whole-model analysis tables shared by one constrained-dummy scan.
///
/// [`crate::scalarize::build_expression_scalarization_context`] and
/// [`crate::static_eval::structural_scalar_bindings`] each walk every variable
/// (and, for structural values, every binding) in the DAE. The constrained-dummy
/// scan evaluates one candidate per state and previously rebuilt both tables
/// inside every candidate, which makes the scan quadratic in model size for no
/// benefit: the tables are pure functions of an immutable `&Dae`.
///
/// The struct deliberately owns its tables instead of borrowing the `Dae` it was
/// built from, so a caller holding `&mut Dae` can build it, use it, and then
/// mutate the DAE without fighting the borrow checker. That also makes the
/// staleness contract explicit at every call site: an analysis is valid exactly
/// as long as the DAE it was built from is unchanged.
pub(super) struct DummyStateAnalysis {
    scalarization: crate::scalarize::ExpressionScalarizationContext,
    structural_bindings: HashMap<String, f64>,
}

impl DummyStateAnalysis {
    pub(super) fn build(dae: &Dae) -> Result<Self, StructuralError> {
        Ok(Self {
            scalarization: crate::scalarize::build_expression_scalarization_context(dae)?,
            structural_bindings: crate::static_eval::structural_scalar_bindings(dae),
        })
    }

    pub(super) fn scalarization(&self) -> &crate::scalarize::ExpressionScalarizationContext {
        &self.scalarization
    }

    pub(super) fn structural_bindings(&self) -> &HashMap<String, f64> {
        &self.structural_bindings
    }
}

/// Apply structural dummy-derivative reduction for constrained states.
///
/// The source DAE initially marks every variable that appears under `der()` as
/// a state. Models with position constraints can therefore contain dependent
/// states. This pass selects states already identified by constrained-dummy
/// analysis and applies the Mattsson-Söderlind dummy-derivative construction to
/// each of them: it declares a generated algebraic unknown for the state's
/// derivative, rewrites every `der(state)` to a plain reference to that unknown,
/// appends the differentiated defining constraint as the unknown's own defining
/// row, and moves the state itself to the algebraic partition (its original
/// constraint still determines it) before BLT.
///
/// The construction *adds an equation*; it does not inline the differentiated
/// tree into every `der(state)` reader. Inlining is what makes this pass
/// quadratic in a constraint chain: round `k`'s differentiated expression reads
/// derivatives that round `k-1` already replaced, so each substitution embeds
/// the previous — already grown — expression, and the growth compounds along
/// the chain. Naming the derivative keeps every reader a single leaf, so the
/// pass grows the system by one row and one unknown per demoted state instead.
/// Both the original constraint and its derivative are retained, so the reduced
/// system stays on the original solution manifold (MLS 3.7).
pub fn reduce_constrained_dummy_derivatives(dae: &mut Dae) -> Result<usize, StructuralError> {
    let deficiency_before = super::demotion_rank_check::scalar_rank_deficiency(dae);
    let total_demoted = reduce_constrained_dummy_derivatives_in_place(dae)?;
    super::demotion_rank_check::trace_pass_rank_transition(
        "reduce_constrained_dummy_derivatives",
        deficiency_before,
        super::demotion_rank_check::scalar_rank_deficiency(dae),
    );
    Ok(total_demoted)
}

fn reduce_constrained_dummy_derivatives_in_place(dae: &mut Dae) -> Result<usize, StructuralError> {
    let mut total_demoted = 0usize;
    let protected_group_states =
        dummy_derivative_group::planned_complete_dummy_derivative_group_states(dae)?;

    // Each round commits one plan. Exchanges strictly raise StateSelect rank;
    // ordinary demotions reduce the finite state set, so neither can cycle.
    loop {
        // A state-free system has no constrained-dummy candidates at all, so
        // it must not pay for the whole-model analysis tables.
        if dae.variables.states.is_empty() {
            if !reduce_one_dummy_derivative_group(dae, &mut total_demoted)? {
                break;
            }
            continue;
        }
        // One scan per round shares one analysis. The DAE cannot change under
        // it: `apply_constrained_dummy_derivative_plan` stages its edits on a
        // copy and commits only when it returns non-zero, and the only two
        // mutations in the round (that commit, plus `pin_structural_params`)
        // are immediately followed by `break`, which starts a fresh round with
        // a freshly built analysis.
        let analysis = DummyStateAnalysis::build(dae)?;
        let definitions = constrained_dummy_state_defining_exprs_with_analysis(dae, &analysis)?;
        crate::structural_trace!(
            "[sim-trace] constrained-dummy scan: candidates={:?}",
            definitions.keys().collect::<Vec<_>>()
        );
        if definitions.is_empty() {
            if !reduce_one_dummy_derivative_group(dae, &mut total_demoted)? {
                break;
            }
            continue;
        }

        // One reading per round, for the same reason there is one analysis per
        // round: every candidate below is judged against this DAE, and the first
        // candidate that commits ends the round.
        let round_rank = super::demotion_rank_check::round_rank(dae);
        let mut demoted_this_round = false;
        for (state_name, definition) in definitions {
            if !dae.variables.states.contains_key(&state_name)
                || protected_group_states.contains(&state_name)
                || state_has_overlapping_event_update(dae, &state_name)
            {
                continue;
            }
            let Some(plan) = constrained_dummy_derivative_plan_for_definition_with_analysis(
                dae,
                &state_name,
                &definition,
                &analysis,
            )?
            else {
                crate::structural_trace!(
                    "[sim-trace] constrained-dummy plan rejected state={}",
                    state_name.as_str()
                );
                continue;
            };
            crate::structural_trace!(
                "[sim-trace] constrained-dummy demoting state={} structural_params={:?}",
                state_name.as_str(),
                definition.structural_params
            );
            let applied = apply_constrained_dummy_derivative_plan(dae, &plan, &round_rank)?;
            if applied == 0 {
                continue;
            }
            total_demoted += applied;
            pin_structural_params(dae, &definition.structural_params);
            demoted_this_round = true;
            break;
        }
        if !demoted_this_round && !reduce_one_dummy_derivative_group(dae, &mut total_demoted)? {
            break;
        }
    }

    Ok(total_demoted)
}

/// Commit one complete dummy-derivative group, adding what it demoted to
/// `total_demoted`. Returns `false` when the group reducer found nothing left
/// to do, which is the reduction loop's termination condition.
fn reduce_one_dummy_derivative_group(
    dae: &mut Dae,
    total_demoted: &mut usize,
) -> Result<bool, StructuralError> {
    let demoted = dummy_derivative_group::reduce_one_complete_dummy_derivative_group(dae)?;
    if demoted == 0 {
        return Ok(false);
    }
    *total_demoted += demoted;
    Ok(true)
}

pub(super) fn state_has_overlapping_event_update(dae: &Dae, state_name: &VarName) -> bool {
    dae.discrete
        .real_updates
        .iter()
        .chain(&dae.discrete.valued_updates)
        .filter_map(|equation| equation.lhs.as_ref())
        .any(|target| {
            target.var_name() == state_name
                || rumoca_core::parse_scalar_name(target.as_str())
                    .is_some_and(|scalar| scalar.base == state_name.as_str())
        })
}

pub(super) fn constrained_dummy_derivative_plan_for_definition_with_analysis(
    dae: &Dae,
    state_name: &VarName,
    definition: &ConstrainedDummyDefinition,
    analysis: &DummyStateAnalysis,
) -> Result<Option<ConstrainedDummyDerivativePlan>, StructuralError> {
    let Some(state) = dae.variables.states.get(state_name) else {
        return Ok(None);
    };
    if state.state_select == rumoca_core::StateSelect::Always
        || state_has_overlapping_event_update(dae, state_name)
    {
        return Ok(None);
    }
    let seed_exprs = definition
        .aggregate_defining_expr
        .iter()
        .chain(definition.component_defining_exprs.values())
        .cloned()
        .collect::<Vec<_>>();
    if seed_exprs.iter().any(|expr| {
        !state_row_reduction::expression_is_smooth_for_index_reduction(
            expr,
            dae,
            analysis.structural_bindings(),
        )
    }) {
        return Ok(None);
    }
    let der_map = build_relaxed_derivative_map_for_state_definition(dae, &seed_exprs, state_name)?;
    constrained_dummy_derivative_plan(dae, state_name, definition, &der_map, analysis)
}

fn constrained_dummy_derivative_plan(
    dae: &Dae,
    state_name: &VarName,
    definition: &ConstrainedDummyDefinition,
    der_map: &HashMap<String, Expression>,
    analysis: &DummyStateAnalysis,
) -> Result<Option<ConstrainedDummyDerivativePlan>, StructuralError> {
    if let Some(defining_expr) = &definition.aggregate_defining_expr {
        let Some(der_expr) = symbolic_time_derivative(defining_expr, dae, der_map) else {
            return Ok(None);
        };
        if derivative_depends_on_state_derivative(dae, &der_expr, state_name) {
            return Ok(None);
        }
        let Some(state) = dae.variables.states.get(state_name) else {
            return Ok(None);
        };
        let Some(promoted_state_names) =
            preferred_derivative_state_exchange(dae, state_name, std::slice::from_ref(&der_expr))
        else {
            return Ok(None);
        };
        if !state.dims.is_empty() {
            let derivative_dims = row_shape::expression_dims_for_row_count(dae, &der_expr)?;
            if derivative_dims != Some(state.dims.clone()) {
                return Ok(None);
            }
        }
        let component_der_exprs = if state.dims.is_empty() {
            IndexMap::from_iter([(state_name.clone(), der_expr.clone())])
        } else {
            let rows = crate::scalarize::scalarize_expression_rows(
                &der_expr,
                state.size(),
                analysis.scalarization(),
            )?;
            if rows.len() != state.size() {
                return Ok(None);
            }
            rows.into_iter()
                .enumerate()
                .map(|(flat_index, expr)| {
                    (
                        dae::scalar_name_for_flat_index(state_name, &state.dims, flat_index),
                        expr,
                    )
                })
                .collect()
        };
        return Ok(Some(ConstrainedDummyDerivativePlan {
            state_name: state_name.clone(),
            component_der_exprs,
            aggregate_der_expr: Some(der_expr),
            promoted_state_names,
        }));
    }

    let mut component_der_exprs = IndexMap::new();
    for (component_name, defining_expr) in &definition.component_defining_exprs {
        let Some(der_expr) = symbolic_time_derivative(defining_expr, dae, der_map) else {
            return Ok(None);
        };
        if derivative_depends_on_state_derivative(dae, &der_expr, state_name) {
            return Ok(None);
        }
        component_der_exprs.insert(component_name.clone(), der_expr);
    }
    let Some(promoted_state_names) = preferred_derivative_state_exchange(
        dae,
        state_name,
        &component_der_exprs.values().cloned().collect::<Vec<_>>(),
    ) else {
        return Ok(None);
    };
    let aggregate_der_expr =
        compact_uniform_static_derivative(dae, state_name, &component_der_exprs);
    if dae
        .continuous
        .equations
        .iter()
        .any(|equation| contains_exact_unsliced_der_of_state(&equation.rhs, state_name))
        && aggregate_der_expr.is_none()
    {
        return Ok(None);
    }
    Ok(Some(ConstrainedDummyDerivativePlan {
        state_name: state_name.clone(),
        component_der_exprs,
        aggregate_der_expr,
        promoted_state_names,
    }))
}

fn preferred_derivative_state_exchange(
    dae: &Dae,
    state_name: &VarName,
    derivative_exprs: &[Expression],
) -> Option<Vec<VarName>> {
    let source = dae.variables.states.get(state_name)?;
    let mut promoted = Vec::new();
    for expression in derivative_exprs {
        collect_der_of_algebraics(expression, dae, &mut promoted);
    }
    promoted.sort();
    promoted.dedup();
    if promoted.len() > 1 {
        crate::structural_trace!(
            "[sim-trace] constrained-dummy exchange rejected state={} reason=multiple_promoted_states promoted={:?}",
            state_name.as_str(),
            promoted.iter().map(VarName::as_str).collect::<Vec<_>>()
        );
        return None;
    }
    if let Some(target_name) = promoted.first() {
        let target = dae.variables.algebraics.get(target_name)?;
        let has_discrete_updates =
            !dae.discrete.real_updates.is_empty() || !dae.discrete.valued_updates.is_empty();
        let shape_mismatch = target.dims != source.dims;
        let target_forbidden = target.state_select == rumoca_core::StateSelect::Never;
        let rank_not_improved =
            state_select_rank(target.state_select) <= state_select_rank(source.state_select);
        let target_has_event_update = state_has_overlapping_event_update(dae, target_name);
        let derivative_missing = !derivative_exprs
            .iter()
            .any(|expression| expr_contains_der_of(expression, target_name));
        if has_discrete_updates
            || shape_mismatch
            || target_forbidden
            || rank_not_improved
            || target_has_event_update
            || derivative_missing
        {
            crate::structural_trace!(
                "[sim-trace] constrained-dummy exchange rejected state={} target={} reason=target_ineligible discrete_updates={} shape_mismatch={} target_forbidden={} rank_not_improved={} target_event_update={} derivative_missing={} source_select={:?} target_select={:?}",
                state_name.as_str(),
                target_name.as_str(),
                has_discrete_updates,
                shape_mismatch,
                target_forbidden,
                rank_not_improved,
                target_has_event_update,
                derivative_missing,
                source.state_select,
                target.state_select
            );
            return None;
        }
    }

    let future_states = dae
        .variables
        .states
        .keys()
        .chain(promoted.iter())
        .map(|name| name.as_str().to_string())
        .collect::<HashSet<_>>();
    let derivative_closed = derivative_exprs
        .iter()
        .all(|expression| !expr_contains_der_of_non_state(expression, &future_states));
    if !derivative_closed {
        crate::structural_trace!(
            "[sim-trace] constrained-dummy exchange rejected state={} reason=non_state_derivative",
            state_name.as_str()
        );
        return None;
    }
    Some(promoted)
}

fn compact_uniform_static_derivative(
    dae: &Dae,
    state_name: &VarName,
    component_der_exprs: &IndexMap<VarName, Expression>,
) -> Option<Expression> {
    let state = dae.variables.states.get(state_name)?;
    if state.dims.is_empty() {
        return component_der_exprs.get(state_name).cloned();
    }
    let bindings = crate::static_eval::structural_scalar_bindings(dae);
    let mut values = (0..state.size()).map(|flat_index| {
        let component = dae::scalar_name_for_flat_index(state_name, &state.dims, flat_index);
        crate::static_eval::eval_static_number(component_der_exprs.get(&component)?, &bindings)
    });
    let first = values.next()??;
    if !first.is_finite() || values.any(|value| value != Some(first)) {
        return None;
    }
    let span = state.source_span;
    let mut args = state
        .dims
        .iter()
        .map(|dimension| Expression::Literal {
            value: Literal::Integer(*dimension),
            span,
        })
        .collect::<Vec<_>>();
    let function = if first == 0.0 {
        BuiltinFunction::Zeros
    } else {
        args.insert(
            0,
            Expression::Literal {
                value: Literal::Real(first),
                span,
            },
        );
        BuiltinFunction::Fill
    };
    Some(Expression::BuiltinCall {
        function,
        args,
        span,
    })
}

fn contains_exact_unsliced_der_of_state(expr: &Expression, state_name: &VarName) -> bool {
    struct Checker<'a> {
        state_name: &'a VarName,
        found: bool,
    }
    impl ExpressionVisitor for Checker<'_> {
        fn visit_expression(&mut self, expr: &Expression) {
            if der_call_targets_exact_unsliced_state(expr, self.state_name) {
                self.found = true;
            } else if !self.found {
                self.walk_expression(expr);
            }
        }
    }
    let mut checker = Checker {
        state_name,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

fn der_call_targets_exact_unsliced_state(expr: &Expression, state_name: &VarName) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } if matches!(
            args.as_slice(),
            [Expression::VarRef { name, subscripts, .. }]
                if subscripts.is_empty() && name.var_name() == state_name
        )
    )
}

/// Rename the root of a reference that is rooted at `state_name` onto the
/// dummy-derivative namespace.
///
/// The generated name is `__dummyder__.<state>`, so every reference rooted at
/// the state — the bare name, or a component such as `v_0[2]` written with its
/// subscripts embedded in the name — maps onto the dummy by prefixing the
/// namespace. Returns `None` for a reference rooted anywhere else, including a
/// name that merely shares a textual prefix with the state.
fn dummy_rooted_name(name: &VarName, state_name: &VarName, dummy_name: &VarName) -> Option<String> {
    let text = name.as_str();
    if text == state_name.as_str() {
        return Some(dummy_name.as_str().to_string());
    }
    let suffix = text.strip_prefix(state_name.as_str())?;
    suffix
        .starts_with('[')
        .then(|| format!("{}{suffix}", dummy_name.as_str()))
}

/// The dummy-derivative reference that stands for `der(<arg>)`.
///
/// `arg` is whatever the model wrote under `der()`: the whole state, one
/// element of it, or a slice of it. Because the state and its dummy derivative
/// have the same shape, the reference is reproduced verbatim with its root
/// renamed, so `der(v_0[2])` becomes `__dummyder__.v_0[2]` — a leaf, not a tree.
/// Returns `None` when the argument is not a reference rooted at `state_name`,
/// which leaves the `der()` call in place for the caller's post-check to reject.
fn dummy_derivative_reference(
    arg: &Expression,
    state_name: &VarName,
    dummy_name: &VarName,
) -> Option<Expression> {
    match arg {
        Expression::VarRef {
            name,
            subscripts,
            span,
        } => Some(Expression::VarRef {
            name: Reference::new(dummy_rooted_name(name.var_name(), state_name, dummy_name)?),
            subscripts: subscripts.clone(),
            span: *span,
        }),
        Expression::Index {
            base,
            subscripts,
            span,
        } => Some(Expression::Index {
            base: Box::new(dummy_derivative_reference(base, state_name, dummy_name)?),
            subscripts: subscripts.clone(),
            span: *span,
        }),
        _ => None,
    }
}

fn substitute_der_of_state_with_dummy(
    expr: &Expression,
    state_name: &VarName,
    dummy_name: &VarName,
) -> Expression {
    struct Rewriter<'a> {
        state_name: &'a VarName,
        dummy_name: &'a VarName,
    }
    impl ExpressionRewriter for Rewriter<'_> {
        fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
            if let Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args,
                ..
            } = expr
                && let [arg] = args.as_slice()
                && let Some(reference) =
                    dummy_derivative_reference(arg, self.state_name, self.dummy_name)
            {
                return reference;
            }
            self.walk_expression(expr)
        }
    }
    Rewriter {
        state_name,
        dummy_name,
    }
    .rewrite_expression(expr)
}

/// Rewrite every `der(<state>)` in the DAE to a reference to the state's
/// generated dummy derivative.
pub(super) fn rewrite_state_derivative_as_dummy_everywhere(
    dae: &mut Dae,
    state_name: &VarName,
    dummy_name: &VarName,
) {
    for equation in dae
        .continuous
        .equations
        .iter_mut()
        .chain(&mut dae.initialization.equations)
        .chain(&mut dae.discrete.real_updates)
        .chain(&mut dae.discrete.valued_updates)
        .chain(&mut dae.conditions.equations)
    {
        equation.rhs = substitute_der_of_state_with_dummy(&equation.rhs, state_name, dummy_name);
    }
    for expr in dae
        .conditions
        .relations
        .iter_mut()
        .chain(&mut dae.events.synthetic_root_conditions)
        .chain(&mut dae.clocks.triggered_conditions)
        .chain(&mut dae.clocks.constructor_exprs)
    {
        *expr = substitute_der_of_state_with_dummy(expr, state_name, dummy_name);
    }
    for action in &mut dae.events.event_actions {
        action.condition =
            substitute_der_of_state_with_dummy(&action.condition, state_name, dummy_name);
        let message = match &mut action.kind {
            rumoca_ir_dae::DaeEventActionKind::Assert { message }
            | rumoca_ir_dae::DaeEventActionKind::Terminate { message } => message,
        };
        *message = substitute_der_of_state_with_dummy(message, state_name, dummy_name);
    }
}

/// Origin stamped on the row that defines a generated dummy derivative.
pub(super) const CONSTRAINED_DUMMY_ROW_ORIGIN: &str =
    "index_reduction:d_dt_constrained_dummy_derivative";

/// Differentiated constraint behind each dummy derivative this pass generated.
///
/// Every such row is `<dummy> - <derivative> = 0`, so the map sends the dummy's
/// name to the expression it stands for.
fn constrained_dummy_definitions(dae: &Dae) -> HashMap<VarName, &Expression> {
    dae.continuous
        .equations
        .iter()
        .filter(|equation| equation.origin == CONSTRAINED_DUMMY_ROW_ORIGIN)
        .filter_map(|equation| match &equation.rhs {
            Expression::Binary {
                op: OpBinary::Sub,
                lhs,
                rhs,
                ..
            } => match lhs.as_ref() {
                Expression::VarRef { name, .. } => Some((name.var_name().clone(), rhs.as_ref())),
                _ => None,
            },
            _ => None,
        })
        .collect()
}

/// Every name the current DAE already writes for `der(<state_name>)`.
///
/// That is the generated dummy plus every `v` carried by a derivative-alias row
/// `v = der(state_name)`. The set stays deliberately local: only names a single
/// row equates to this state's derivative qualify, never names reachable
/// through a chain of unrelated alias rows, because such a chain merges
/// quantities that are equal for reasons of their own and would reject genuine
/// constraints.
fn state_derivative_synonyms(dae: &Dae, state_name: &VarName) -> HashSet<VarName> {
    let mut synonyms = HashSet::from([dummy_derivative_group::dummy_derivative_name(state_name)]);
    for equation in &dae.continuous.equations {
        if let Some(alias) = try_extract_derivative_alias(equation, state_name) {
            synonyms.insert(alias);
        }
    }
    synonyms
}

/// True when `expr` is `der(<state_name>)`, whole or one scalar component.
fn is_der_of_state(expr: &Expression, state_name: &VarName) -> bool {
    let Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args,
        ..
    } = expr
    else {
        return false;
    };
    let [argument] = args.as_slice() else {
        return false;
    };
    state_row_reduction::expression_exact_name(argument).is_some_and(|exact| {
        exact == state_name.as_str()
            || dae::component_base_name(&exact).is_some_and(|base| base == state_name.as_str())
    })
}

/// The expressions rows of the current DAE already write for `der(<state>)`.
///
/// A residual row `0 = v - der(x)` (the derivative alias a `Revolute` writes as
/// `w = der(phi)`) or `0 = der(x) - e` (a plain ODE row) *is* a definition of
/// `der(x)`. Both orientations are collected, since the row is a residual and
/// carries no preferred side.
fn existing_state_derivative_values<'a>(dae: &'a Dae, state_name: &VarName) -> Vec<&'a Expression> {
    dae.continuous
        .equations
        .iter()
        .filter_map(|equation| {
            let Expression::Binary {
                op: OpBinary::Sub | OpBinary::SubElem,
                lhs,
                rhs,
                ..
            } = &equation.rhs
            else {
                return None;
            };
            if is_der_of_state(lhs, state_name) {
                return Some(rhs.as_ref());
            }
            is_der_of_state(rhs, state_name).then(|| lhs.as_ref())
        })
        .collect()
}

/// The row defining the dummy derivative `name` stands for, if this pass wrote
/// one.
///
/// A dummy whose differentiated constraint kept the state's array shape is
/// defined by one aggregate row under its own name; otherwise the definition is
/// written component-wise. MLS 10.1 makes a one-element array a distinct type
/// from a scalar, so a singleton state is a legal instance of the second case
/// while every reader still writes its aggregate name — hence the second lookup.
fn dummy_definition<'a>(
    dae: &Dae,
    definitions: &HashMap<VarName, &'a Expression>,
    expr: &Expression,
) -> Option<&'a Expression> {
    let name = VarName::new(state_row_reduction::expression_exact_name(expr)?);
    if let Some(found) = definitions.get(&name) {
        return Some(found);
    }
    let variable = dae.variables.algebraics.get(&name)?;
    if variable.dims.is_empty() || variable.size() != 1 {
        return None;
    }
    let component = dae::scalar_name_for_flat_index(&name, &variable.dims, 0);
    definitions.get(&component).copied()
}

/// Follow a chain of bare generated-dummy references to what it stands for.
///
/// Only a *reference* is followed, never a reference buried in a larger tree,
/// so this cannot rebuild the inlined expression the naming formulation exists
/// to avoid: the result is always an expression that already lives in the DAE.
fn resolve_dummy_reference<'a>(
    dae: &Dae,
    definitions: &HashMap<VarName, &'a Expression>,
    expr: &'a Expression,
) -> &'a Expression {
    let mut current = expr;
    for _ in 0..=definitions.len() {
        let Some(next) = dummy_definition(dae, definitions, current) else {
            break;
        };
        current = next;
    }
    current
}

/// True when the differentiated constraint only restates a row the DAE has.
///
/// The check is deliberately confined to the one shape the naming formulation
/// creates and inlining could not: a differentiated constraint that *is*,
/// whole, a reference to a dummy derivative an earlier round generated. Such a
/// constraint contributes nothing of its own — its entire content is the row
/// that defines that dummy — so if that row is already the row defining
/// `der(state_name)`, the appended copy adds no rank. Substitution collapses the
/// two into the same equation, duplicate removal drops one, and the demotion has
/// contributed an unknown without contributing an equation. That is an
/// under-determined system, which is exactly what a holonomic constraint
/// reduction must never produce.
///
/// A differentiated constraint that carries any structure of its own is left
/// alone: it may well share a value with an existing row at a point without
/// being the same equation, and rejecting on that would refuse genuine
/// index reduction.
///
/// The two sides are compared as *values*, not as trees. Chain-rule
/// differentiation writes a product rule out in full, so the row a previous
/// round appended for `der(x)` reads `-(0.0*y + (-1.0)*1)` where the row the DAE
/// already carries for the same derivative is the literal `1`. Those are the
/// same equation. A comparison that cannot see it lets a redundant demotion
/// through — for a two-state chain `x[1] = y`, `der(x[1]) = 1`, `der(y) = 1` it
/// demotes both states and leaves a model with one degree of freedom carrying no
/// state at all — which is exactly what this gate exists to stop.
///
/// Constant folding uses no variable bindings, so it only ever collapses an
/// expression whose value is fixed by the model text. Anything reading a
/// variable falls back to the structural comparison, where two rows are equal
/// only if they are the same tree.
fn derivative_restates_an_existing_row(
    dae: &Dae,
    definitions: &HashMap<VarName, &Expression>,
    expr: &Expression,
    state_name: &VarName,
) -> bool {
    if dummy_definition(dae, definitions, expr).is_none() {
        return false;
    }
    let resolved = resolve_dummy_reference(dae, definitions, expr);
    existing_state_derivative_values(dae, state_name)
        .into_iter()
        .map(|value| resolve_dummy_reference(dae, definitions, value))
        .any(|value| rows_carry_the_same_value(resolved, value))
}

/// True when two residual values are the same equation.
///
/// The exact arithmetic identities are folded first. That is what removes the
/// `0.0 * y` term a product rule leaves behind, and with it the only reference
/// to a variable in an otherwise constant derivative — without it the constant
/// evaluation below refuses the whole expression because `y` has no binding.
fn rows_carry_the_same_value(lhs: &Expression, rhs: &Expression) -> bool {
    let lhs = crate::eliminate::simplify_arithmetic_identities(lhs.clone());
    let rhs = crate::eliminate::simplify_arithmetic_identities(rhs.clone());
    if rumoca_core::expressions_semantically_equal(&lhs, &rhs) {
        return true;
    }
    let bindings = HashMap::new();
    let (Some(lhs), Some(rhs)) = (
        crate::static_eval::eval_static_number(&lhs, &bindings),
        crate::static_eval::eval_static_number(&rhs, &bindings),
    ) else {
        return false;
    };
    lhs.is_finite() && lhs == rhs
}

/// True when `expr` still depends on `der(<state_name>)` once every generated
/// dummy derivative it reads is resolved through the row that defines it, or
/// when it merely restates a row the DAE already carries for that derivative.
///
/// Naming a derivative instead of inlining it keeps expressions small, but it
/// also hides self-dependence: a differentiated constraint that reads
/// `__dummyder__.y` where `y`'s own differentiated constraint reads
/// `der(state_name)` is `der(state_name)` written indirectly, and demoting on
/// it would replace an independent coordinate by a pair of rows that say the
/// same thing twice. Inlining used to expose that to a plain syntactic check;
/// walking the dummy rows restores it without rebuilding the trees, and
/// [`derivative_restates_an_existing_row`] closes the remaining hole — a
/// differentiated constraint that lands on a row already defining the
/// derivative rather than on the `der()` call itself.
fn derivative_depends_on_state_derivative(
    dae: &Dae,
    expr: &Expression,
    state_name: &VarName,
) -> bool {
    let definitions = constrained_dummy_definitions(dae);
    if derivative_restates_an_existing_row(dae, &definitions, expr, state_name) {
        return true;
    }
    let synonyms = state_derivative_synonyms(dae, state_name);
    let mut visited = HashSet::new();
    let mut pending = vec![expr];
    while let Some(current) = pending.pop() {
        if expr_contains_der_of(current, state_name) {
            return true;
        }
        for name in collect_rhs_var_refs(current) {
            let base =
                dae::component_base_name(name.as_str()).map_or_else(|| name.clone(), VarName::new);
            if synonyms.contains(&base) {
                return true;
            }
            // A dummy whose differentiated constraint kept the state's array
            // shape is defined under its aggregate name; one written
            // component-wise is defined under the component name its readers
            // write. MLS 10.1 makes `x[1]` a distinct type from a scalar, so a
            // singleton array state is always the second case while every reader
            // still writes the component. Looking the definition up only under
            // the base name therefore stops the walk at the first component
            // reference and reports independence where the constraint in fact
            // resolves back to `der(state_name)`.
            pending.extend(unvisited_dummy_definitions(
                &definitions,
                &mut visited,
                [name, base],
            ));
        }
    }
    false
}

/// The definitions of `keys` not walked yet, marking each key as walked.
fn unvisited_dummy_definitions<'a>(
    definitions: &HashMap<VarName, &'a Expression>,
    visited: &mut HashSet<VarName>,
    keys: [VarName; 2],
) -> Vec<&'a Expression> {
    keys.into_iter()
        .filter(|key| visited.insert(key.clone()))
        .filter_map(|key| definitions.get(&key).copied())
        .collect()
}

/// Residual row `<dummy> - <differentiated constraint> = 0`.
fn dummy_derivative_defining_row(
    dummy_ref: Expression,
    derivative: &Expression,
    span: Span,
    scalar_count: usize,
) -> Equation {
    Equation {
        lhs: None,
        rhs: sub_expr(dummy_ref, derivative.clone(), span),
        span,
        origin: CONSTRAINED_DUMMY_ROW_ORIGIN.to_string(),
        scalar_count,
    }
}

/// Append the defining row(s) of the generated dummy derivative.
///
/// One aggregate row when the differentiated constraint keeps the state's array
/// shape, otherwise one row per scalar component — the same split the plan
/// already made when it differentiated the constraint.
fn append_dummy_derivative_definitions(
    dae: &mut Dae,
    plan: &ConstrainedDummyDerivativePlan,
    dummy_name: &VarName,
) -> Result<(), StructuralError> {
    let state = dae.variables.states.get(&plan.state_name).ok_or_else(|| {
        StructuralError::UnspannedContractViolation {
            reason: format!(
                "constrained dummy derivative requested for `{}`, which is no longer a state",
                plan.state_name.as_str()
            ),
        }
    })?;
    let span = state.source_span;
    let dims = state.dims.clone();
    let size = state.size();
    if let Some(derivative) = &plan.aggregate_der_expr {
        let dummy_ref = Expression::VarRef {
            name: Reference::new(dummy_name.as_str()),
            subscripts: Vec::new(),
            span,
        };
        dae.continuous.equations.push(dummy_derivative_defining_row(
            dummy_ref, derivative, span, size,
        ));
        return Ok(());
    }
    for flat_index in 0..size {
        let component = dae::scalar_name_for_flat_index(&plan.state_name, &dims, flat_index);
        let derivative = plan.component_der_exprs.get(&component).ok_or_else(|| {
            StructuralError::UnspannedContractViolation {
                reason: format!(
                    "constrained dummy derivative for `{}` has no differentiated constraint for component `{}`",
                    plan.state_name.as_str(),
                    component.as_str()
                ),
            }
        })?;
        let dummy_ref = Expression::VarRef {
            name: Reference::new(
                dae::scalar_name_for_flat_index(dummy_name, &dims, flat_index).as_str(),
            ),
            subscripts: Vec::new(),
            span,
        };
        dae.continuous.equations.push(dummy_derivative_defining_row(
            dummy_ref, derivative, span, 1,
        ));
    }
    Ok(())
}

/// Scalar rows of the continuous partition — what the structural matcher counts.
pub(super) fn scalar_row_total(dae: &Dae) -> usize {
    dae.continuous
        .equations
        .iter()
        .map(|equation| equation.scalar_count)
        .sum()
}

/// Every distinct name the continuous partition still differentiates.
fn der_column_names(dae: &Dae) -> HashSet<String> {
    struct Collector {
        names: HashSet<String>,
    }
    impl ExpressionVisitor for Collector {
        fn visit_expression(&mut self, expr: &Expression) {
            if let Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args,
                ..
            } = expr
                && let [argument] = args.as_slice()
                && let Some(name) = state_row_reduction::expression_exact_name(argument)
            {
                self.names.insert(name);
            }
            self.walk_expression(expr);
        }
    }
    let mut collector = Collector {
        names: HashSet::new(),
    };
    for equation in &dae.continuous.equations {
        collector.visit_expression(&equation.rhs);
    }
    collector.names
}

/// Scalar unknowns the structural matcher will see: one per algebraic component
/// plus one per scalar `der()` column. A state is not an unknown — the
/// integrator supplies it — so only its derivative column counts.
fn scalar_unknown_total(dae: &Dae) -> usize {
    let algebraics: usize = dae.variables.algebraics.values().map(Variable::size).sum();
    let derivatives: usize = der_column_names(dae)
        .iter()
        .map(|name| {
            let name = VarName::new(name.as_str());
            dae.variables
                .states
                .get(&name)
                .or_else(|| dae.variables.algebraics.get(&name))
                .map_or(1, Variable::size)
        })
        .sum();
    algebraics + derivatives
}

/// Hard-fail when committing a demotion did not preserve the structural balance.
///
/// The Mattsson-Söderlind construction is an exchange with a fixed arithmetic:
/// the demoted state contributes one unknown per scalar component, its
/// derivative keeps the column it already had under the generated name, and the
/// differentiated constraint supplies exactly one new row per component. Rows
/// and unknowns therefore both grow by the state's scalar size and by nothing
/// else, whatever the surrounding model looks like.
///
/// SPEC_0008 forbids recovering silently from a violation of that arithmetic. A
/// demotion that adds an unknown without adding a row leaves a system that can
/// still admit a perfect matching elsewhere and then integrates a different
/// model than the one that was written, so the pass names the state and stops.
///
/// # Which balance
///
/// This measure counts the *residual system*: algebraics plus `der()` columns.
/// It is the right measure here because this pass only ever demotes a state
/// whose derivative already occupies a column, so the column is exchanged for
/// the dummy's and nothing else moves. The row-group naming form entered from
/// [`super::state_row_reduction`] is defined on the opposite case — a state with
/// no `der(state)` row at all — where no column is exchanged, so it answers to
/// the same SPEC_0008 arithmetic read in the model's own variable counts. See
/// [`super::dummy_row_group::verify_row_group_demotion_preserves_balance`].
fn verify_demotion_preserves_balance(
    before: &Dae,
    after: &Dae,
    state_name: &VarName,
    size: usize,
    span: Span,
) -> Result<(), StructuralError> {
    let row_growth = scalar_row_total(after).wrapping_sub(scalar_row_total(before));
    let unknown_growth = scalar_unknown_total(after).wrapping_sub(scalar_unknown_total(before));
    if row_growth == size && unknown_growth == size {
        return Ok(());
    }
    Err(StructuralError::ContractViolation {
        reason: format!(
            "index reduction unbalanced the system while demoting state `{}`: \
             demoting {size} scalar state(s) added {row_growth} scalar row(s) and \
             {unknown_growth} scalar unknown(s), but the dummy-derivative construction \
             must add exactly one row and one unknown per demoted scalar state",
            state_name.as_str()
        ),
        span,
    })
}

/// Commit one Mattsson-Söderlind dummy-derivative construction.
///
/// Returns `0` when the construction cannot be completed — the generated name
/// is taken, or a `der(state)` reference the rewriter cannot express as a
/// reference to the dummy survives — or when the staged system fails the rank
/// witness in [`super::demotion_rank_check::demotion_is_rank_justified`],
/// so the caller moves on to the next candidate with the DAE untouched.
fn apply_constrained_dummy_derivative_plan(
    dae: &mut Dae,
    plan: &ConstrainedDummyDerivativePlan,
    round_rank: &super::demotion_rank_check::RoundRank,
) -> Result<usize, StructuralError> {
    let dummy_name = dummy_derivative_group::dummy_derivative_name(&plan.state_name);
    if dummy_derivative_group::variable_name_is_taken(dae, &dummy_name) {
        return Ok(0);
    }
    let Some(demoted) = dae.variables.states.get(&plan.state_name) else {
        return Ok(0);
    };
    let (size, span) = (demoted.size(), demoted.source_span);
    let mut staged = super::copy_accounting::clone_dae(dae);
    dummy_derivative_group::declare_dummy_derivative(&mut staged, &plan.state_name, &dummy_name)?;
    rewrite_state_derivative_as_dummy_everywhere(&mut staged, &plan.state_name, &dummy_name);
    if staged
        .continuous
        .equations
        .iter()
        .any(|equation| expr_contains_der_of(&equation.rhs, &plan.state_name))
    {
        return Ok(0);
    }
    append_dummy_derivative_definitions(&mut staged, plan, &dummy_name)?;
    let Some(var) = staged.variables.states.shift_remove(&plan.state_name) else {
        return Ok(0);
    };
    staged
        .variables
        .algebraics
        .insert(plan.state_name.clone(), var);
    for promoted_name in &plan.promoted_state_names {
        let Some(var) = staged.variables.algebraics.shift_remove(promoted_name) else {
            return Ok(0);
        };
        staged.variables.states.insert(promoted_name.clone(), var);
    }
    verify_demotion_preserves_balance(dae, &staged, &plan.state_name, size, span)?;
    if !super::demotion_rank_check::demotion_is_rank_justified(
        round_rank,
        &staged,
        &plan.state_name,
        !plan.promoted_state_names.is_empty(),
    ) {
        return Ok(0);
    }
    *dae = staged;
    Ok(1)
}
