//! Checked B.1c and clocked discrete-Real projection into GALEC `DoStep`.

#[cfg(test)]
mod tests;

use std::collections::HashSet;

use super::*;

#[derive(Clone)]
pub(super) struct ClockedAssignment {
    pub(super) targets: HashSet<u32>,
    pub(super) reads: HashSet<u32>,
    pub(super) statements: Vec<gast::Spanned<gast::Statement>>,
    pub(super) span: Span,
    pub(super) is_preamble: bool,
    pub(super) requires_preamble: bool,
}

pub(super) struct ClockedAssignments {
    #[cfg(test)]
    pub(super) statements: Vec<gast::Spanned<gast::Statement>>,
    pub(super) locals: Vec<gast::VariableDeclaration>,
    pub(super) called_user_functions: HashSet<u32>,
    pub(super) assignments: Vec<ClockedAssignment>,
}

#[cfg(test)]
pub(super) fn lower_clocked_assignments<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
) -> Result<ClockedAssignments, GalecTargetError> {
    lower_clocked_assignments_for_domain(view, clock, by_id, pre_names, true)
}

pub(super) fn lower_clocked_assignments_for_domain<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    include_unclocked_actions: bool,
) -> Result<ClockedAssignments, GalecTargetError> {
    let mut pending = Vec::new();
    let mut locals = Vec::new();
    let mut called_user_functions = HashSet::new();
    let mut lowerer = ExpressionLowerer::with_do_step_effects(view, by_id, pre_names)
        .with_causal_inlining()
        .with_temporary_namespace(format!("clocked{}", clock.index()));
    let causal = CausalReadExpansion::derive(view);
    let shared_calls = lower_clock_domain_preamble(
        view,
        clock,
        include_unclocked_actions,
        &causal,
        &mut lowerer,
        &mut pending,
    )?;
    lower_discrete_value_owners(
        &mut DiscreteValueLowering {
            view,
            clock,
            by_id,
            lowerer: &mut lowerer,
            shared_calls: &shared_calls,
        },
        include_unclocked_actions,
        &mut pending,
    )?;
    lower_discrete_real_equations(
        view,
        clock,
        by_id,
        &mut pending,
        &mut lowerer,
        &shared_calls,
    )?;
    locals.extend(lowerer.take_temporary_locals());
    called_user_functions.extend(lowerer.take_called_user_functions());
    lower_event_actions(
        view,
        clock,
        by_id,
        pre_names,
        include_unclocked_actions,
        &mut pending,
    )?;
    for assignment in &mut pending {
        assignment.reads = causal.expand(std::mem::take(&mut assignment.reads));
    }
    #[cfg(test)]
    let statements = order_assignments(&pending)?;
    Ok(ClockedAssignments {
        #[cfg(test)]
        statements,
        locals,
        called_user_functions,
        assignments: pending,
    })
}

/// Hoist repeated eager calls that are stable across the whole domain entry.
///
/// The preamble runs before every clocked assignment of its domain, so a call
/// may only be hoisted when nothing it reads is written by a clocked assignment
/// in this `DoStep`. That read set must be taken through the same
/// [`CausalReadExpansion`] the scheduler uses: a call that names only algebraic
/// coordinates can still reach discrete state through causal algebraic
/// inlining, and hoisting it would evaluate it against the previous tick's
/// value. Admitting only domain-entry-stable calls also keeps the preamble free
/// of incoming schedule edges, so the barrier every assignment in the domain
/// places on it can never close a cycle.
fn lower_clock_domain_preamble<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    include_unclocked: bool,
    causal: &CausalReadExpansion<'dae>,
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    pending: &mut Vec<ClockedAssignment>,
) -> Result<SharedMaterializedFunctionCalls, GalecTargetError> {
    let mutable_targets = view
        .variables()
        .filter_map(|(id, variable)| {
            matches!(
                variable.identity(),
                dae::VariableIdentity::DiscreteReal(_) | dae::VariableIdentity::DiscreteValue(_)
            )
            .then_some(id.index())
        })
        .collect::<HashSet<_>>();
    let mut occurrences = HashMap::<u32, (dae::ExprId<'dae>, usize)>::new();
    for root in clock_domain_value_roots(view, clock, include_unclocked)? {
        let mut seen = HashSet::new();
        collect_eager_calls(view, root, &mut seen, &mut occurrences);
    }
    let mut calls = occurrences
        .into_values()
        .filter_map(|(call, count)| {
            if count < 2 {
                return None;
            }
            let mut reads = HashSet::new();
            collect_current_reads(view, call, &mut reads);
            let reads = causal.expand(reads);
            reads.is_disjoint(&mutable_targets).then_some((call, reads))
        })
        .collect::<Vec<_>>();
    calls.sort_by_key(|(call, _)| call.index());
    if calls.is_empty() {
        return Ok(HashMap::new());
    }
    let span = view
        .expression(calls[0].0)
        .expect("checked eager call resolves")
        .provenance()
        .span();
    let mut reads = HashSet::new();
    for (call, call_reads) in calls {
        lowerer.materialize_eager_call(call)?;
        reads.extend(call_reads);
    }
    let statements = lowerer.drain_prefix_statements();
    let shared = lowerer.shared_materialized_function_calls();
    if !statements.is_empty() {
        pending.push(ClockedAssignment {
            targets: HashSet::new(),
            reads,
            statements,
            span,
            is_preamble: true,
            requires_preamble: false,
        });
    }
    Ok(shared)
}

fn clock_domain_value_roots<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    include_unclocked: bool,
) -> Result<Vec<dae::ExprId<'dae>>, GalecTargetError> {
    let mut roots = Vec::new();
    let real_clocks = discrete_real_clock_owners(view);
    let causal_plan = causal_discrete_plan(view)?;
    for index in 0..view.discrete_real_equation_count() {
        let Some(definition) = causal_plan.discrete_real_definition(index) else {
            continue;
        };
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        let target = dae::VariableId::from(definition.target());
        let value = definition.value();
        if require_discrete_real_clock_owner(&real_clocks, target, equation.provenance().span())?
            == clock.index()
            && activation_allows_domain_preamble(view, equation.activation(), clock)
        {
            roots.push(value);
        }
    }
    let value_clocks = discrete_value_clock_owners(view);
    for index in 0..view.discrete_value_owner_count() {
        let owner = view
            .discrete_value_owner(
                view.discrete_value_owner_id(index)
                    .expect("dense checked B.1c owner identity"),
            )
            .expect("checked B.1c owner resolves");
        if !discrete_value_owner_runs_in_domain(
            view,
            owner,
            clock,
            include_unclocked,
            &value_clocks,
        )? {
            continue;
        }
        for branch in owner.branches().iter() {
            let activation_allows_preamble = match branch.activation() {
                dae::DiscreteBranchActivation::Always => true,
                dae::DiscreteBranchActivation::When { guard, .. } => {
                    condition_is_domain_clock_only(view, guard, clock)
                }
            };
            if activation_allows_preamble {
                roots.extend(branch.values().iter().map(|(value, _)| value));
            }
        }
    }
    Ok(roots)
}

fn activation_allows_domain_preamble<'dae>(
    view: dae::DaeView<'dae>,
    activation: dae::DiscreteRealActivation<'dae>,
    clock: dae::ClockId<'dae>,
) -> bool {
    match activation {
        dae::DiscreteRealActivation::Always => true,
        dae::DiscreteRealActivation::When { guard, .. } => {
            condition_is_domain_clock_only(view, guard, clock)
        }
    }
}

/// Prove that evaluating a value at domain entry cannot cross a runtime guard.
///
/// `Clock` and `Always` add no condition inside the already-selected DoStep
/// domain. Relations, discrete predicates, negation, and disjunction remain
/// lazy and therefore forbid preamble hoisting.
fn condition_is_domain_clock_only<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
    clock: dae::ClockId<'dae>,
) -> bool {
    match view
        .condition(condition)
        .expect("checked preamble condition resolves")
        .operation()
    {
        dae::ConditionOperation::Clock(found) => found == clock,
        dae::ConditionOperation::Always => true,
        dae::ConditionOperation::And(lhs, rhs) => {
            condition_is_domain_clock_only(view, lhs, clock)
                && condition_is_domain_clock_only(view, rhs, clock)
        }
        dae::ConditionOperation::Initial
        | dae::ConditionOperation::Relation(_)
        | dae::ConditionOperation::Discrete(_)
        | dae::ConditionOperation::Not(_)
        | dae::ConditionOperation::Or(_, _)
        | dae::ConditionOperation::AnyRise(_, _) => false,
    }
}

fn collect_eager_calls<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    seen: &mut HashSet<u32>,
    occurrences: &mut HashMap<u32, (dae::ExprId<'dae>, usize)>,
) {
    expression_functions::for_each_eager_call(view, expression, seen, &mut |call, owner| {
        // Count construction-issued invocations, not scalar projections.
        occurrences
            .entry(owner)
            .and_modify(|(_, count)| *count += 1)
            .or_insert((call, 1));
    });
}

/// One shared causal-definition proof plus the variable index it is queried by.
///
/// Deriving the proof is whole-model work; every clock domain and every
/// assignment in it asks the same questions, so it is derived once per domain
/// and reused.
struct CausalReadExpansion<'dae> {
    view: dae::DaeView<'dae>,
    definitions: rumoca_phase_structural::CausalDefinitions<'dae>,
    variables: HashMap<u32, dae::VariableId<'dae>>,
}

impl<'dae> CausalReadExpansion<'dae> {
    fn derive(view: dae::DaeView<'dae>) -> Self {
        Self {
            definitions: rumoca_phase_structural::CausalDefinitions::derive(view),
            variables: view.variables().map(|(id, _)| (id.index(), id)).collect(),
            view,
        }
    }

    /// Expand current-tick reads through every exact acyclic algebraic
    /// definition.
    ///
    /// Clock-domain ordering cannot stop at an intermediate algebraic
    /// coordinate: if `command = filtered` and `filtered = slowState`, the fast
    /// domain reads `slowState` even though its lowered expression initially
    /// names `filtered`. An array assembled one element at a time
    /// (`alias[1] = slowState; alias[2] = 0;`) hides the same edge behind a
    /// complete scalar definition set rather than one whole-variable
    /// definition, so both forms are followed.
    /// [`rumoca_phase_structural::CausalDefinitions`] is the construction proof
    /// that this traversal is finite and semantics-preserving.
    fn expand(&self, mut reads: HashSet<u32>) -> HashSet<u32> {
        let mut pending = reads.iter().copied().collect::<Vec<_>>();
        let mut expanded = HashSet::new();
        while let Some(index) = pending.pop() {
            if !expanded.insert(index) {
                continue;
            }
            let Some(&variable) = self.variables.get(&index) else {
                continue;
            };
            let mut definition_reads = HashSet::new();
            self.collect_definition_reads(variable, &mut definition_reads);
            pending.extend(
                definition_reads
                    .into_iter()
                    .filter(|dependency| reads.insert(*dependency)),
            );
        }
        reads
    }

    fn collect_definition_reads(&self, variable: dae::VariableId<'dae>, reads: &mut HashSet<u32>) {
        if let Some(definition) = self.definitions.definition_for_variable(variable) {
            collect_current_reads(self.view, definition, reads);
            return;
        }
        if !self.definitions.fully_defines_variable(variable) {
            return;
        }
        let Some(scalar_count) = self
            .view
            .variable(variable)
            .and_then(|declaration| u32::try_from(declaration.scalar_count()).ok())
        else {
            return;
        };
        for scalar in 0..scalar_count {
            if let Some(definition) = self
                .definitions
                .scalar_definition_for_variable(variable, scalar)
            {
                collect_current_reads(self.view, definition, reads);
            }
        }
    }
}

/// One planned clocked discrete-`Real` definition, before emission.
struct PlannedDiscreteReal<'refs, 'dae> {
    target: dae::VariableId<'dae>,
    value: dae::ExprId<'dae>,
    span: Span,
    activation: dae::DiscreteRealActivation<'dae>,
    classified: &'refs ClassifiedVariable<'dae>,
}

/// The construction-issued call occurrence a definition value projects.
///
/// DAE-C21: the N result projections of one multi-output call all carry the
/// same issued `Call.owner`. Reading that issued identity is the only
/// admissible way to recognise them as one invocation — never the callee body,
/// name, span, argument shape, or an expression comparison.
fn projected_call_owner<'dae>(view: dae::DaeView<'dae>, value: dae::ExprId<'dae>) -> Option<u32> {
    let mut current = value;
    loop {
        match view.expression(current)?.operation() {
            dae::ExpressionOperation::Call { owner, .. } => return Some(owner.index()),
            dae::ExpressionOperation::Field { base, .. }
            | dae::ExpressionOperation::Index { base, .. } => current = base,
            _ => return None,
        }
    }
}

fn activation_key(activation: dae::DiscreteRealActivation<'_>) -> (u32, u32) {
    match activation {
        dae::DiscreteRealActivation::Always => (u32::MAX, u32::MAX),
        dae::DiscreteRealActivation::When { trigger, guard } => (trigger.index(), guard.index()),
    }
}

fn plan_clocked_discrete_reals<'refs, 'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &'refs HashMap<u32, ClassifiedVariable<'dae>>,
) -> Result<Vec<PlannedDiscreteReal<'refs, 'dae>>, GalecTargetError> {
    let clock_owners = discrete_real_clock_owners(view);
    let causal_plan = causal_discrete_plan(view)?;
    let mut planned = Vec::new();
    for index in 0..view.discrete_real_equation_count() {
        let Some(definition) = causal_plan.discrete_real_definition(index) else {
            continue;
        };
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        let span = equation.provenance().span();
        let target = dae::VariableId::from(definition.target());
        if require_discrete_real_clock_owner(&clock_owners, target, span)? != clock.index() {
            continue;
        }
        let classified = by_id.get(&target.index()).ok_or_else(|| {
            GalecTargetError::UnknownVariableReference {
                name: format!("#{}", target.index()),
                span: Some(span),
            }
        })?;
        planned.push(PlannedDiscreteReal {
            target,
            value: definition.value(),
            span,
            activation: equation.activation(),
            classified,
        });
    }
    Ok(planned)
}

/// Partition planned definitions into one emission group per issued call
/// occurrence, preserving first-definition order.
///
/// Grouping is what makes one source invocation emit one call: the intra-group
/// materialization memo is only severed at a group boundary
/// (`finish_statement_group`), so N projections spread over N groups become N
/// calls. A target with more than one definition keeps its own group, because
/// the conditional-owner merge below rewrites that group in place.
fn group_planned_discrete_reals<'dae>(
    view: dae::DaeView<'dae>,
    planned: &[PlannedDiscreteReal<'_, 'dae>],
) -> Vec<Vec<usize>> {
    let mut definition_counts: HashMap<u32, usize> = HashMap::new();
    for plan in planned {
        *definition_counts.entry(plan.target.index()).or_default() += 1;
    }
    let mut groups: Vec<Vec<usize>> = Vec::new();
    let mut open: HashMap<(u32, (u32, u32)), usize> = HashMap::new();
    for (slot, plan) in planned.iter().enumerate() {
        let key = (definition_counts.get(&plan.target.index()) == Some(&1))
            .then(|| projected_call_owner(view, plan.value))
            .flatten()
            .map(|owner| (owner, activation_key(plan.activation)));
        match key.and_then(|key| open.get(&key).copied()) {
            Some(existing) => groups[existing].push(slot),
            None => {
                if let Some(key) = key {
                    open.insert(key, groups.len());
                }
                groups.push(vec![slot]);
            }
        }
    }
    groups
}

fn lower_discrete_real_equations<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pending: &mut Vec<ClockedAssignment>,
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    shared_calls: &SharedMaterializedFunctionCalls,
) -> Result<(), GalecTargetError> {
    let planned = plan_clocked_discrete_reals(view, clock, by_id)?;
    let groups = group_planned_discrete_reals(view, &planned);
    let mut owners: HashMap<u32, (usize, bool)> = HashMap::new();
    for group in &groups {
        let assignment = lower_discrete_real_group(
            view,
            clock,
            lowerer,
            shared_calls,
            &planned,
            group.as_slice(),
        )?;
        merge_discrete_real_assignment(
            pending,
            &mut owners,
            &planned,
            group.as_slice(),
            assignment,
        )?;
    }
    Ok(())
}

/// Emit one group of planned definitions as a single schedulable assignment.
///
/// Every definition in the group is lowered before the prefix is drained, so
/// the shared call materializes once and the later projections read its result
/// temporaries.
fn lower_discrete_real_group<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    shared_calls: &SharedMaterializedFunctionCalls,
    planned: &[PlannedDiscreteReal<'_, 'dae>],
    group: &[usize],
) -> Result<ClockedAssignment, GalecTargetError> {
    let head = &planned[group[0]];
    let span = head.span;
    let (guard, guard_prefix) = match head.activation {
        dae::DiscreteRealActivation::Always => (None, Vec::new()),
        dae::DiscreteRealActivation::When { trigger, guard } => {
            require_periodic_trigger(view, trigger, clock, span)?;
            let guard = lower_action_guard(view, guard, clock, lowerer, span)?;
            let prefix = lowerer.take_prefix_statements_with_shared_calls(shared_calls);
            (guard, prefix)
        }
    };
    let mut assignments = Vec::new();
    let mut reads = HashSet::new();
    let mut targets = HashSet::new();
    for &slot in group {
        let plan = &planned[slot];
        targets.insert(plan.target.index());
        collect_current_reads(view, plan.value, &mut reads);
        if let dae::DiscreteRealActivation::When { trigger, guard } = plan.activation {
            collect_condition_current_reads(view, trigger, &mut reads);
            collect_condition_current_reads(view, guard, &mut reads);
        }
        append_definition_assignments(
            lowerer,
            plan.value,
            plan.classified,
            plan.span,
            &mut assignments,
        )?;
    }
    // A merged group writes its own targets in causal order, so a read of one
    // of them by a later member is satisfied inside the group exactly as the
    // scheduler would have satisfied it between the separate groups. Retaining
    // such a read would be a self-dependency and would reject a valid model.
    // An unmerged group keeps its read set byte-for-byte as before, so a single
    // definition's self-read still reaches the scheduler unchanged.
    if group.len() > 1 {
        for target in &targets {
            reads.remove(target);
        }
    }
    let mut body = lowerer.take_prefix_statements_with_shared_calls(shared_calls);
    body.extend(assignments);
    let statements = match guard {
        Some(condition) => {
            let mut statements = guard_prefix;
            statements.push(gast::Spanned::new(
                gast::Statement::If(gast::IfStatement {
                    branches: vec![gast::IfBranch {
                        condition: gast::Condition::Expression(condition),
                        body,
                        span,
                    }],
                    else_body: None,
                }),
                span,
            ));
            statements
        }
        None => body,
    };
    Ok(ClockedAssignment {
        targets,
        reads,
        statements,
        span,
        is_preamble: false,
        requires_preamble: !shared_calls.is_empty(),
    })
}

/// Emit one checked clocked definition into `assignments`.
///
/// A rank-`n` target is normally projected coordinate by coordinate, which is
/// correct but turns one source definition into `product(dimensions)` Algorithm
/// Code assignments — 225 of them for a 15x15 covariance. When the definition's
/// value already denotes ONE array-shaped GALEC storage object of exactly the
/// target's shape and element type, that whole coordinate set is by
/// construction the identity map of the source's coordinates onto the target's,
/// under the one row-major order both sides are subscripted with. One
/// whole-array assignment is then constructed in place of the set.
///
/// This is emission and not recognition (TRP-020/021/035): the decision is
/// taken from the checked DAE value BEFORE any coordinate statement exists, and
/// no already-emitted statement is inspected, matched or rewritten. When the
/// whole-array form is not proven the coordinate projection runs exactly as
/// before.
fn append_definition_assignments<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    value: dae::ExprId<'dae>,
    classified: &ClassifiedVariable<'dae>,
    span: Span,
    assignments: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let dimensions = classified.variable.value_type().dimensions();
    if let Some(source) = whole_array_definition_source(lowerer, value, classified)? {
        assignments.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: state_reference_indexed(classified.name.clone(), &[], span),
                value: source,
            },
            span,
        ));
        return Ok(());
    }
    for indices in row_major_indices(dimensions) {
        let lowered = lowerer.lower_element(value, &indices)?;
        let value = coerce(lowered, classified.scalar_type, span)?;
        assignments.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: state_reference_indexed(classified.name.clone(), &indices, span),
                value,
            },
            span,
        ));
    }
    Ok(())
}

/// The single array-shaped GALEC storage object `value` already denotes, when
/// copying it whole is provably the coordinate projection this definition would
/// otherwise emit.
///
/// The proof obligations, each of which fails closed:
///
/// * **Rank.** A rank-0 target has no coordinate set to collapse.
/// * **Correspondence.** The value's checked shape must equal the target's,
///   extent by extent. Both sides are then subscripted by the same
///   [`row_major_indices`] sequence, so coordinate `k` of the source is
///   coordinate `k` of the target — a permuted, transposed, offset or
///   differently-extented source never reaches this form because it is not
///   spelled as a bare reference to one whole object.
/// * **Completeness.** [`row_major_indices`] enumerates every coordinate of the
///   target exactly once, and the whole-array assignment replaces exactly that
///   enumeration, so no coordinate is dropped, added or written twice.
/// * **Element type.** `coerce` may insert a per-coordinate `real(...)`
///   conversion, which a whole-array copy cannot express, so a differing scalar
///   type keeps the coordinate form.
/// * **Single source, no interleaving.** Only the two operations below can
///   answer: a directly-lowerable call, whose materialized result temporary is
///   one object written by one prefix statement that already dominates every
///   coordinate of this group; and an expression that names whole checked
///   storage. Both replace one contiguous coordinate run of one definition, so
///   nothing is reordered across a neighbouring definition or a guard.
/// * **Aliasing.** GALEC whole-array assignment and the coordinate projection
///   it replaces both copy forward in row-major order, so an overlapping source
///   and target produce the same values either way.
fn whole_array_definition_source<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    value: dae::ExprId<'dae>,
    classified: &ClassifiedVariable<'dae>,
) -> Result<Option<gast::Expression>, GalecTargetError> {
    let dimensions = classified.variable.value_type().dimensions();
    if dimensions.is_empty() {
        return Ok(None);
    }
    let node = lowerer
        .view
        .expression(value)
        .expect("checked clocked definition value resolves");
    let value_type = node.value_type();
    if value_type.is_record() || value_type.dimensions() != dimensions {
        return Ok(None);
    }
    let span = node.provenance().span();
    let source_scalar = scalar_type(
        value_type.scalar_type(),
        classified.variable.name().as_str(),
        span,
    )?;
    if source_scalar != classified.scalar_type {
        return Ok(None);
    }
    // A directly lowerable call's selected result IS a materialized temporary of
    // the result's own shape. Asking for it at rank `n` rather than per
    // coordinate reuses the same memoized `MultiAssignment` prefix the
    // coordinate projection would have emitted, so the call still happens
    // exactly once and still dominates this assignment.
    if let dae::ExpressionOperation::Call {
        function,
        output,
        arguments,
        ..
    } = node.operation()
        && lowerer.materialize_function_values
        && user_functions::is_directly_lowerable(lowerer.view, function)
    {
        let lowered = lowerer.lower_call_at(value, function, output, arguments, &[], span)?;
        if lowered.scalar_type != classified.scalar_type {
            return Ok(None);
        }
        return Ok(Some(lowered.expression));
    }
    // Otherwise only an expression that already names whole checked storage
    // qualifies. Everything computed — a negation, a sum, a conditional, an
    // array constructor, an inlined causal local — answers `None` here and
    // keeps its coordinate projection.
    lowerer.direct_whole_aggregate_reference(value)
}

fn merge_discrete_real_assignment<'dae>(
    pending: &mut Vec<ClockedAssignment>,
    owners: &mut HashMap<u32, (usize, bool)>,
    planned: &[PlannedDiscreteReal<'_, 'dae>],
    group: &[usize],
    assignment: ClockedAssignment,
) -> Result<(), GalecTargetError> {
    let head = &planned[group[0]];
    let unconditional = matches!(head.activation, dae::DiscreteRealActivation::Always);
    // Only single-definition targets are grouped, so a repeated definition is
    // always a one-member group and keeps the established conditional-owner
    // merge.
    if let Some(&(owner, owner_unconditional)) = owners.get(&head.target.index()) {
        if unconditional || owner_unconditional {
            return Err(unsupported(
                "multiple-discrete-real-definitions",
                format!(
                    "discrete Real `{}` has multiple definitions without one conditional owner",
                    head.classified.variable.name()
                ),
                head.span,
            ));
        }
        pending[owner].reads.extend(assignment.reads);
        pending[owner].statements.extend(assignment.statements);
        return Ok(());
    }
    let index = pending.len();
    for &slot in group {
        owners.insert(planned[slot].target.index(), (index, unconditional));
    }
    pending.push(assignment);
    Ok(())
}

fn causal_discrete_plan<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<rumoca_phase_structural::CausalDiscretePlan<'dae>, GalecTargetError> {
    rumoca_phase_structural::CausalDiscretePlan::derive(view).map_err(|error| {
        let rumoca_phase_structural::CausalDiscreteError::NonComputable { span } = error;
        coupled_discrete_real_equation(span)
    })
}

fn coupled_discrete_real_equation(span: Span) -> GalecTargetError {
    unsupported(
        "coupled-discrete-real-equation",
        "a coupled B.1b residual cannot be represented as one GALEC state assignment".to_owned(),
        span,
    )
}

fn require_discrete_real_clock_owner<'dae>(
    owners: &HashMap<u32, u32>,
    target: dae::VariableId<'dae>,
    span: Span,
) -> Result<u32, GalecTargetError> {
    match owners.get(&target.index()).copied() {
        Some(clock) => Ok(clock),
        None => Err(unsupported(
            "clock-domain",
            "discrete Real definition has no explicit clock owner".to_owned(),
            span,
        )),
    }
}

fn discrete_real_clock_owners(view: dae::DaeView<'_>) -> HashMap<u32, u32> {
    (0..view.clock_ownership_count())
        .filter_map(|index| {
            let id = view
                .clock_ownership_id(index)
                .expect("dense checked clock ownership identity");
            let ownership = view
                .clock_ownership(id)
                .expect("checked clock ownership resolves");
            (ownership.kind() == dae::ClockedVariableKind::DiscreteReal)
                .then_some((ownership.variable().index(), ownership.clock().index()))
        })
        .collect()
}

fn lower_event_actions<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    include_unclocked: bool,
    pending: &mut Vec<ClockedAssignment>,
) -> Result<(), GalecTargetError> {
    let mut lowerer =
        ExpressionLowerer::with_assertions(view, by_id, pre_names).with_causal_inlining();
    for index in 0..view.event_action_count() {
        let action = view
            .event_action(
                view.event_action_id(index)
                    .expect("dense checked event action identity"),
            )
            .expect("checked event action resolves");
        let span = action.provenance().span();
        let dae::EventActionOperation::Assert { level: None, .. } = action.operation() else {
            return Err(unsupported(
                "event-action",
                format!(
                    "event action `{}` cannot be represented in GALEC DoStep",
                    event_name(action.operation())
                ),
                span,
            ));
        };
        let trigger_is_always = matches!(
            view.condition(action.trigger())
                .expect("checked event trigger resolves")
                .operation(),
            dae::ConditionOperation::Always
        );
        let trigger_clocks = condition_clocks(view, action.trigger());
        if trigger_clocks.is_empty() && !include_unclocked {
            continue;
        }
        if !trigger_clocks.is_empty() && !trigger_clocks.contains(&clock.index()) {
            continue;
        }
        if !trigger_is_always {
            require_periodic_trigger(view, action.trigger(), clock, span)?;
        }
        let guard = lower_action_guard(view, action.guard(), clock, &mut lowerer, span)?;
        let signal = gast::Spanned::new(
            gast::Statement::Signal(vec![gast::Identifier::new(
                gast::PredefinedSignal::InvalidArgument.name(),
            )]),
            span,
        );
        let mut statements = lowerer.take_prefix_statements();
        statements.extend(match guard {
            Some(condition) => vec![gast::Spanned::new(
                gast::Statement::If(gast::IfStatement {
                    branches: vec![gast::IfBranch {
                        condition: gast::Condition::Expression(condition),
                        body: vec![signal],
                        span,
                    }],
                    else_body: None,
                }),
                span,
            )],
            None => vec![signal],
        });
        let mut reads = HashSet::new();
        collect_condition_current_reads(view, action.trigger(), &mut reads);
        collect_condition_current_reads(view, action.guard(), &mut reads);
        pending.push(ClockedAssignment {
            targets: HashSet::new(),
            reads,
            statements,
            span,
            is_preamble: false,
            requires_preamble: false,
        });
    }
    Ok(())
}

fn condition_clocks<'dae>(view: dae::DaeView<'dae>, root: dae::ConditionId<'dae>) -> HashSet<u32> {
    let mut pending = vec![root];
    let mut seen = HashSet::new();
    let mut clocks = HashSet::new();
    while let Some(condition) = pending.pop() {
        if !seen.insert(condition.index()) {
            continue;
        }
        match view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation()
        {
            dae::ConditionOperation::Clock(clock) => {
                clocks.insert(clock.index());
            }
            dae::ConditionOperation::Not(inner) => pending.push(inner),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => pending.extend([lhs, rhs]),
            dae::ConditionOperation::Initial
            | dae::ConditionOperation::Always
            | dae::ConditionOperation::Relation(_)
            | dae::ConditionOperation::Discrete(_) => {}
        }
    }
    clocks
}

struct DiscreteValueLowering<'context, 'refs, 'dae> {
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &'refs HashMap<u32, ClassifiedVariable<'dae>>,
    lowerer: &'context mut ExpressionLowerer<'refs, 'dae>,
    shared_calls: &'context SharedMaterializedFunctionCalls,
}

fn lower_discrete_value_owners<'dae>(
    context: &mut DiscreteValueLowering<'_, '_, 'dae>,
    include_unclocked: bool,
    pending: &mut Vec<ClockedAssignment>,
) -> Result<(), GalecTargetError> {
    let clock_owners = discrete_value_clock_owners(context.view);
    for index in 0..context.view.discrete_value_owner_count() {
        let owner = context
            .view
            .discrete_value_owner(
                context
                    .view
                    .discrete_value_owner_id(index)
                    .expect("dense checked B.1c owner identity"),
            )
            .expect("checked B.1c owner resolves");
        if !discrete_value_owner_runs_in_domain(
            context.view,
            owner,
            context.clock,
            include_unclocked,
            &clock_owners,
        )? {
            continue;
        }
        pending.push(lower_discrete_value_owner(context, owner)?);
    }
    Ok(())
}

fn discrete_value_owner_runs_in_domain<'dae>(
    view: dae::DaeView<'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
    clock: dae::ClockId<'dae>,
    include_unclocked: bool,
    clock_owners: &HashMap<u32, u32>,
) -> Result<bool, GalecTargetError> {
    let explicit_clocks = owner
        .targets()
        .iter()
        .map(|target| clock_owners.get(&target.index()).copied())
        .collect::<Option<HashSet<_>>>();
    if explicit_clocks
        .as_ref()
        .is_some_and(|clocks| clocks.len() != 1)
    {
        return Err(unsupported(
            "clock-domain",
            "one atomic discrete value owner spans multiple clock domains".to_owned(),
            owner.provenance().span(),
        ));
    }
    let trigger_clocks = owner
        .branches()
        .iter()
        .flat_map(|branch| match branch.activation() {
            dae::DiscreteBranchActivation::When { trigger, .. } => condition_clocks(view, trigger),
            dae::DiscreteBranchActivation::Always => HashSet::new(),
        })
        .collect::<HashSet<_>>();
    let explicit_clocks = explicit_clocks.filter(|clocks| !clocks.is_empty());
    Ok(
        !((explicit_clocks.is_none() && trigger_clocks.is_empty() && !include_unclocked)
            || explicit_clocks
                .as_ref()
                .is_some_and(|clocks| !clocks.contains(&clock.index()))
            || (explicit_clocks.is_none()
                && !trigger_clocks.is_empty()
                && !trigger_clocks.contains(&clock.index()))),
    )
}

fn discrete_value_clock_owners(view: dae::DaeView<'_>) -> HashMap<u32, u32> {
    (0..view.clock_ownership_count())
        .filter_map(|index| {
            let id = view
                .clock_ownership_id(index)
                .expect("dense checked clock ownership identity");
            let ownership = view
                .clock_ownership(id)
                .expect("checked clock ownership resolves");
            (ownership.kind() == dae::ClockedVariableKind::DiscreteValue)
                .then_some((ownership.variable().index(), ownership.clock().index()))
        })
        .collect()
}

fn lower_discrete_value_owner<'dae>(
    context: &mut DiscreteValueLowering<'_, '_, 'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<ClockedAssignment, GalecTargetError> {
    let span = owner.provenance().span();
    let target_variables = owner
        .targets()
        .iter()
        .map(dae::VariableId::from)
        .collect::<Vec<_>>();
    let classified = target_variables
        .iter()
        .map(|target| {
            context.by_id.get(&target.index()).ok_or_else(|| {
                GalecTargetError::UnknownVariableReference {
                    name: format!("#{}", target.index()),
                    span: Some(span),
                }
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut reads = HashSet::new();
    let mut conditional = Vec::new();
    let mut unconditional = None;
    for branch in owner.branches().iter() {
        let branch_span = branch.provenance().span();
        match branch.activation() {
            dae::DiscreteBranchActivation::Always => {
                unconditional = Some(lower_discrete_value_branch(
                    context.lowerer,
                    context.shared_calls,
                    &classified,
                    branch,
                )?);
            }
            dae::DiscreteBranchActivation::When { trigger, guard } => {
                require_periodic_trigger(context.view, trigger, context.clock, branch_span)?;
                collect_condition_current_reads(context.view, trigger, &mut reads);
                collect_condition_current_reads(context.view, guard, &mut reads);
                let condition = lower_action_guard(
                    context.view,
                    guard,
                    context.clock,
                    context.lowerer,
                    branch_span,
                )?
                .unwrap_or(gast::Expression::Bool(true));
                let condition_prefix = context
                    .lowerer
                    .take_prefix_statements_with_shared_calls(context.shared_calls);
                let body = lower_discrete_value_branch(
                    context.lowerer,
                    context.shared_calls,
                    &classified,
                    branch,
                )?;
                conditional.push(GuardedDiscreteValueBranch {
                    condition_prefix,
                    branch: gast::IfBranch {
                        condition: gast::Condition::Expression(condition),
                        body,
                        span: branch_span,
                    },
                });
            }
        }
        for (value, _) in branch.values().iter() {
            collect_current_reads(context.view, value, &mut reads);
        }
    }
    let statements = compose_discrete_value_branches(conditional, unconditional, span);
    Ok(ClockedAssignment {
        targets: target_variables
            .into_iter()
            .map(|target| target.index())
            .collect(),
        reads,
        statements,
        span,
        is_preamble: false,
        requires_preamble: !context.shared_calls.is_empty(),
    })
}

struct GuardedDiscreteValueBranch {
    condition_prefix: Vec<gast::Spanned<gast::Statement>>,
    branch: gast::IfBranch,
}

/// Preserve lazy `elsewhen` guard evaluation when a guard needs statements.
///
/// A materialized conditional or function call in an `elsewhen` guard must be
/// initialized before that guard is read, but only after every preceding guard
/// has evaluated false. A flat GALEC `elseif` chain cannot place statements
/// between guards, so such branches become nested `else if` statements.
fn compose_discrete_value_branches(
    conditional: Vec<GuardedDiscreteValueBranch>,
    unconditional: Option<Vec<gast::Spanned<gast::Statement>>>,
    span: Span,
) -> Vec<gast::Spanned<gast::Statement>> {
    if conditional.is_empty() {
        return unconditional.unwrap_or_default();
    }
    if conditional
        .iter()
        .all(|branch| branch.condition_prefix.is_empty())
    {
        return vec![gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: conditional
                    .into_iter()
                    .map(|branch| branch.branch)
                    .collect(),
                else_body: unconditional,
            }),
            span,
        )];
    }
    let mut tail = unconditional.unwrap_or_default();
    for guarded in conditional.into_iter().rev() {
        let mut statements = guarded.condition_prefix;
        statements.push(gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: vec![guarded.branch],
                else_body: (!tail.is_empty()).then_some(tail),
            }),
            span,
        ));
        tail = statements;
    }
    tail
}

fn lower_discrete_value_branch<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    shared_calls: &SharedMaterializedFunctionCalls,
    targets: &[&ClassifiedVariable<'dae>],
    branch: dae::DiscreteValueBranchView<'dae>,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let mut assignments = Vec::new();
    for (classified, (value, provenance)) in targets.iter().zip(branch.values().iter()) {
        append_definition_assignments(
            lowerer,
            value,
            classified,
            provenance.span(),
            &mut assignments,
        )?;
    }
    let mut statements = lowerer.take_prefix_statements_with_shared_calls(shared_calls);
    statements.extend(assignments);
    Ok(statements)
}

fn collect_current_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    reads: &mut HashSet<u32>,
) {
    dae::for_each_expression(view, expression, |_, node| {
        let id = match node.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Input(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteValue(id)) => {
                Some(dae::VariableId::from(id))
            }
            _ => None,
        };
        if let Some(id) = id {
            reads.insert(id.index());
        }
    });
}

fn collect_condition_current_reads<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    reads: &mut HashSet<u32>,
) {
    let mut pending = vec![root];
    let mut seen = HashSet::new();
    while let Some(condition) = pending.pop() {
        if !seen.insert(condition.index()) {
            continue;
        }
        match view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation()
        {
            dae::ConditionOperation::Initial
            | dae::ConditionOperation::Always
            | dae::ConditionOperation::Clock(_) => {}
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked relation identity resolves")
                    .expression();
                collect_current_reads(view, expression, reads);
            }
            dae::ConditionOperation::Discrete(expression) => {
                collect_current_reads(view, expression, reads);
            }
            dae::ConditionOperation::Not(inner) => pending.push(inner),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                pending.extend([lhs, rhs]);
            }
        }
    }
}

fn require_periodic_trigger<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
    span: Span,
) -> Result<(), GalecTargetError> {
    let mut seen = HashSet::new();
    if condition_requires_clock(view, root, expected, &mut seen) {
        return Ok(());
    }
    Err(unsupported(
        "runtime-event-trigger",
        "a conditional assignment is not owned by the admitted periodic DoStep clock".to_owned(),
        span,
    ))
}

fn condition_requires_clock<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
    seen: &mut HashSet<u32>,
) -> bool {
    if !seen.insert(condition.index()) {
        return false;
    }
    match view
        .condition(condition)
        .expect("checked condition identity resolves")
        .operation()
    {
        dae::ConditionOperation::Clock(found) => found == expected,
        dae::ConditionOperation::And(lhs, rhs) => {
            let mut lhs_seen = seen.clone();
            let mut rhs_seen = seen.clone();
            condition_requires_clock(view, lhs, expected, &mut lhs_seen)
                || condition_requires_clock(view, rhs, expected, &mut rhs_seen)
        }
        // Every arm of a disjunction — and every element of a vector activation
        // — must be owned by the clock, or the activation can reach the
        // assignment off-tick.
        dae::ConditionOperation::Or(lhs, rhs) | dae::ConditionOperation::AnyRise(lhs, rhs) => {
            let mut lhs_seen = seen.clone();
            let mut rhs_seen = seen.clone();
            condition_requires_clock(view, lhs, expected, &mut lhs_seen)
                && condition_requires_clock(view, rhs, expected, &mut rhs_seen)
        }
        dae::ConditionOperation::Initial
        | dae::ConditionOperation::Always
        | dae::ConditionOperation::Relation(_)
        | dae::ConditionOperation::Discrete(_)
        | dae::ConditionOperation::Not(_) => false,
    }
}

#[cfg(test)]
fn order_assignments(
    pending: &[ClockedAssignment],
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let preamble = pending.iter().position(|assignment| assignment.is_preamble);
    let targets = pending
        .iter()
        .enumerate()
        .flat_map(|(index, assignment)| {
            assignment
                .targets
                .iter()
                .map(move |target| (*target, index))
        })
        .collect::<HashMap<_, _>>();
    let mut emitted = vec![false; pending.len()];
    let mut emitted_owners = 0usize;
    let mut ordered = Vec::with_capacity(pending.len());
    while emitted_owners < pending.len() {
        let Some(index) = pending.iter().enumerate().position(|(index, assignment)| {
            !emitted[index]
                && (!assignment.requires_preamble
                    || preamble.is_none_or(|preamble| emitted[preamble]))
                && assignment.reads.iter().all(|read| {
                    targets
                        .get(read)
                        .is_none_or(|dependency| *dependency == index || emitted[*dependency])
                })
        }) else {
            let span = pending
                .iter()
                .enumerate()
                .find(|(index, _)| !emitted[*index])
                .expect("unfinished ordering has one unemitted owner")
                .1
                .span;
            return Err(unsupported(
                "discrete-algebraic-loop",
                "clocked assignments contain a current-tick dependency cycle".to_owned(),
                span,
            ));
        };
        emitted[index] = true;
        emitted_owners += 1;
        ordered.extend(pending[index].statements.iter().cloned());
    }
    Ok(ordered)
}
