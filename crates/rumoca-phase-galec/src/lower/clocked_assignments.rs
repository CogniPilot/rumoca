//! Checked B.1c and clocked discrete-Real projection into GALEC `DoStep`.

mod call_plan;
#[cfg(test)]
mod tests;

use std::collections::HashSet;

use super::*;
pub(super) use call_plan::ClockedCallPlan;

#[derive(Clone)]
pub(super) struct ClockedAssignment {
    pub(super) targets: HashSet<u32>,
    pub(super) reads: HashSet<u32>,
    pub(super) regions: HashSet<EmissionRegion>,
    pub(super) statements: Vec<gast::Spanned<gast::Statement>>,
    pub(super) span: Span,
}

pub(super) struct ClockedAssignments {
    #[cfg(test)]
    pub(super) statements: Vec<gast::Spanned<gast::Statement>>,
    pub(super) locals: Vec<gast::VariableDeclaration>,
    pub(super) called_user_functions: HashSet<u32>,
    pub(super) assignments: Vec<ClockedAssignment>,
    pub(super) call_actions: Vec<PreparedCallActions>,
}

#[cfg(test)]
pub(super) fn lower_clocked_assignments<'dae>(
    lowering: BlockLowering<'_, 'dae>,
    clock: dae::ClockId<'dae>,
) -> Result<ClockedAssignments, GalecTargetError> {
    let plan = ClockedCallPlan::construct(lowering, &HashSet::from([clock.index()]), clock)?;
    let mut retained_calls = RetainedCallResults::default();
    lower_clocked_assignments_for_domain(lowering, clock, &plan, &mut retained_calls, true)
}

pub(super) fn lower_clocked_assignments_for_domain<'refs, 'dae>(
    lowering: BlockLowering<'refs, 'dae>,
    clock: dae::ClockId<'dae>,
    call_plan: &ClockedCallPlan<'refs, 'dae>,
    retained_calls: &mut RetainedCallResults,
    retain_unguarded: bool,
) -> Result<ClockedAssignments, GalecTargetError> {
    let BlockLowering {
        view,
        definitions,
        by_id,
        pre_names,
        arithmetic,
    } = lowering;
    let mut pending = Vec::new();
    let mut locals = Vec::new();
    let mut called_user_functions = HashSet::new();
    let mut call_actions = Vec::new();
    let real_plan = call_plan.real_domain(clock);
    let discrete_value_owners = call_plan.discrete_value_domain(clock);
    let event_actions = call_plan.event_action_domain(clock);
    let mut lowerer =
        ExpressionLowerer::with_do_step_effects(view, definitions, by_id, pre_names, arithmetic)
            .with_causal_inlining()
            .with_temporary_namespace(TemporaryNamespace::Clocked(clock));
    let causal = CausalReadExpansion::new(view, definitions);
    lower_discrete_value_owners(
        &mut DiscreteValueLowering {
            view,
            clock,
            by_id,
            lowerer: &mut lowerer,
            call_actions: &mut call_actions,
        },
        discrete_value_owners,
        &mut pending,
    )?;
    lower_discrete_real_equations(
        &mut DiscreteRealLowering {
            view,
            clock,
            lowerer: &mut lowerer,
            call_actions: &mut call_actions,
            retained_calls,
            retain_unguarded,
        },
        &mut pending,
        real_plan,
    )?;
    lower_event_actions(
        view,
        clock,
        event_actions,
        &mut lowerer,
        &mut pending,
        &mut call_actions,
    )?;
    locals.extend(lowerer.take_temporary_locals());
    called_user_functions.extend(lowerer.take_called_user_functions());
    for assignment in &mut pending {
        assignment.reads = causal.expand(std::mem::take(&mut assignment.reads), assignment.span)?;
    }
    #[cfg(test)]
    let statements = order_assignments(&pending, &call_actions)?;
    Ok(ClockedAssignments {
        #[cfg(test)]
        statements,
        locals,
        called_user_functions,
        assignments: pending,
        call_actions,
    })
}

/// One shared causal-definition proof plus the variable index it is queried by.
///
/// Deriving the proof is whole-model work; every clock domain and every
/// assignment in it asks the same questions, so it is derived once per domain
/// and reused.
struct CausalReadExpansion<'a, 'dae> {
    view: dae::DaeView<'dae>,
    definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
    variables: HashMap<u32, dae::VariableId<'dae>>,
}

impl<'a, 'dae> CausalReadExpansion<'a, 'dae> {
    fn new(
        view: dae::DaeView<'dae>,
        definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
    ) -> Self {
        Self {
            definitions,
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
    fn expand(
        &self,
        mut reads: HashSet<u32>,
        consumer_span: Span,
    ) -> Result<HashSet<u32>, GalecTargetError> {
        let mut pending = reads.iter().copied().collect::<Vec<_>>();
        let mut expanded = HashSet::new();
        while let Some(index) = pending.pop() {
            if !expanded.insert(index) {
                continue;
            }
            let variable = require_causal_variable(&self.variables, index, consumer_span)?;
            let mut definition_reads = HashSet::new();
            self.collect_definition_reads(variable, &mut definition_reads, consumer_span)?;
            pending.extend(
                definition_reads
                    .into_iter()
                    .filter(|dependency| reads.insert(*dependency)),
            );
        }
        Ok(reads)
    }

    fn collect_definition_reads(
        &self,
        variable: dae::VariableId<'dae>,
        reads: &mut HashSet<u32>,
        consumer_span: Span,
    ) -> Result<(), GalecTargetError> {
        if let Some(definition) = self.definitions.definition_for_variable(variable) {
            collect_current_reads(self.view, definition, reads);
            return Ok(());
        }
        if !self.definitions.fully_defines_variable(variable) {
            return Ok(());
        }
        let declaration =
            self.view
                .variable(variable)
                .ok_or(GalecTargetError::ForeignCausalRead {
                    variable_index: variable.index(),
                    span: consumer_span,
                })?;
        visit_complete_scalar_definition_family(
            declaration.name().as_str(),
            declaration.scalar_count(),
            declaration.declaration().span(),
            |scalar| {
                self.definitions
                    .scalar_definition_for_variable(variable, scalar)
            },
            |definition| {
                collect_current_reads(self.view, definition, reads);
            },
        )
    }
}

fn require_causal_variable<T: Copy>(
    variables: &HashMap<u32, T>,
    index: u32,
    span: Span,
) -> Result<T, GalecTargetError> {
    variables
        .get(&index)
        .copied()
        .ok_or(GalecTargetError::ForeignCausalRead {
            variable_index: index,
            span,
        })
}

/// Visit one construction-claimed complete scalar family exactly once.
///
/// Acceptance contract (SPEC_0008): every family whose scalar count fits the
/// DAE `u32` identity domain and supplies exactly one definition at every
/// ordinal is accepted and visited in canonical ordinal order. Overflow or a
/// missing member rejects before a partial family can authorize scheduling.
fn visit_complete_scalar_definition_family<T: Copy>(
    variable: &str,
    scalar_count: usize,
    span: Span,
    mut definition: impl FnMut(u32) -> Option<T>,
    mut visit: impl FnMut(T),
) -> Result<(), GalecTargetError> {
    let scalar_capacity = scalar_count;
    let scalar_count = u32::try_from(scalar_count).map_err(|_| {
        GalecTargetError::CausalScalarDefinitionOverflow {
            variable: variable.to_owned(),
            scalar_count,
            span,
        }
    })?;
    let mut complete = Vec::with_capacity(scalar_capacity);
    for scalar in 0..scalar_count {
        let value = definition(scalar).ok_or_else(|| {
            GalecTargetError::IncompleteCausalScalarDefinitions {
                variable: variable.to_owned(),
                missing_scalar: scalar,
                scalar_count,
                span,
            }
        })?;
        complete.push(value);
    }
    for value in complete {
        visit(value);
    }
    Ok(())
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
    for (index, equation) in view.discrete_real_equations().enumerate() {
        let Some(definition) = causal_plan.discrete_real_definition(index) else {
            continue;
        };
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

struct DiscreteRealLowering<'context, 'refs, 'dae> {
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    lowerer: &'context mut ExpressionLowerer<'refs, 'dae>,
    call_actions: &'context mut Vec<PreparedCallActions>,
    retained_calls: &'context mut RetainedCallResults,
    retain_unguarded: bool,
}

#[derive(Clone, Copy)]
struct DiscreteRealGroup<'group, 'refs, 'dae> {
    index: usize,
    planned: &'group [PlannedDiscreteReal<'refs, 'dae>],
    slots: &'group [usize],
}

fn lower_discrete_real_equations<'refs, 'dae>(
    context: &mut DiscreteRealLowering<'_, 'refs, 'dae>,
    pending: &mut Vec<ClockedAssignment>,
    plan: &call_plan::ClockedRealDomainPlan<'refs, 'dae>,
) -> Result<(), GalecTargetError> {
    let planned = plan.planned();
    let mut owners: HashMap<u32, (usize, bool)> = HashMap::new();
    for (group_index, group) in plan.groups().iter().enumerate() {
        let group = DiscreteRealGroup {
            index: group_index,
            planned,
            slots: group.as_slice(),
        };
        let assignment = lower_discrete_real_group(context, group)?;
        merge_discrete_real_assignment(pending, &mut owners, planned, group.slots, assignment)?;
    }
    Ok(())
}

/// Emit one group of planned definitions as a single schedulable assignment.
///
/// Every definition in the group is lowered before the prefix is drained, so
/// the shared call materializes once and the later projections read its result
/// temporaries.
fn lower_discrete_real_group<'dae>(
    context: &mut DiscreteRealLowering<'_, '_, 'dae>,
    group: DiscreteRealGroup<'_, '_, 'dae>,
) -> Result<ClockedAssignment, GalecTargetError> {
    let DiscreteRealLowering {
        view,
        clock,
        lowerer,
        call_actions,
        retained_calls,
        retain_unguarded,
    } = context;
    let head = &group.planned[group.slots[0]];
    let span = head.span;
    let targets = group
        .slots
        .iter()
        .map(|slot| group.planned[*slot].target.index())
        .collect::<HashSet<_>>();
    let mut regions = HashSet::new();
    let (guard, guard_prefix) = match head.activation {
        dae::DiscreteRealActivation::Always => (None, Vec::new()),
        dae::DiscreteRealActivation::When { trigger, guard } => {
            require_periodic_trigger(*view, trigger, *clock, span)?;
            let region = EmissionRegion::ClockedRealGuard {
                clock: clock.index(),
                group: group.index,
            };
            regions.insert(region);
            let prepared = lowerer.prepare_emission_group(
                region,
                &HashSet::new(),
                CrossGroupCallRetention::Refuse,
                |lowerer, _| {
                    lower_action_guard(
                        &mut ActionGuardContext {
                            view: *view,
                            expected: *clock,
                            lowerer,
                            span,
                        },
                        guard,
                    )
                },
            )?;
            let mut prefix = Vec::new();
            let guard = prepared.commit_into(&mut prefix, call_actions);
            (guard, prefix)
        }
    };
    let mut reads = collect_discrete_real_group_reads(*view, group);
    let value_region = EmissionRegion::ClockedRealValue {
        clock: clock.index(),
        group: group.index,
    };
    regions.insert(value_region);
    let retention = match (*retain_unguarded, head.activation, guard.is_some()) {
        (true, dae::DiscreteRealActivation::Always, false)
        | (true, dae::DiscreteRealActivation::When { .. }, false) => {
            CrossGroupCallRetention::Unguarded(retained_calls)
        }
        (true, dae::DiscreteRealActivation::When { guard, .. }, true) => {
            CrossGroupCallRetention::ExactGuard {
                retained: retained_calls,
                activation: RetainedCallActivation::exact(*clock, guard),
            }
        }
        _ => CrossGroupCallRetention::Refuse,
    };
    let prepared = lowerer.prepare_emission_group(
        value_region,
        &targets,
        retention,
        |lowerer, assignments| {
            for &slot in group.slots {
                let plan = &group.planned[slot];
                append_definition_assignments(
                    lowerer,
                    plan.value,
                    plan.classified,
                    plan.span,
                    assignments,
                )?;
            }
            Ok(())
        },
    )?;
    let mut body = Vec::new();
    prepared.commit_into(&mut body, call_actions);
    // A merged group writes its own targets in causal order, so a read of one
    // of them by a later member is satisfied inside the group exactly as the
    // scheduler would have satisfied it between the separate groups. Retaining
    // such a read would be a self-dependency and would reject a valid model.
    // An unmerged group keeps its read set byte-for-byte as before, so a single
    // definition's self-read still reaches the scheduler unchanged.
    if group.slots.len() > 1 {
        for target in &targets {
            reads.remove(target);
        }
    }
    let statements = compose_discrete_real_statements(guard, guard_prefix, body, span);
    Ok(ClockedAssignment {
        targets,
        reads,
        regions,
        statements,
        span,
    })
}

fn collect_discrete_real_group_reads<'dae>(
    view: dae::DaeView<'dae>,
    group: DiscreteRealGroup<'_, '_, 'dae>,
) -> HashSet<u32> {
    let mut reads = HashSet::new();
    for &slot in group.slots {
        let plan = &group.planned[slot];
        collect_current_reads(view, plan.value, &mut reads);
        if let dae::DiscreteRealActivation::When { trigger, guard } = plan.activation {
            collect_condition_current_reads(view, trigger, &mut reads);
            collect_condition_current_reads(view, guard, &mut reads);
        }
    }
    reads
}

fn compose_discrete_real_statements(
    guard: Option<gast::Expression>,
    mut guard_prefix: Vec<gast::Spanned<gast::Statement>>,
    body: Vec<gast::Spanned<gast::Statement>>,
    span: Span,
) -> Vec<gast::Spanned<gast::Statement>> {
    let Some(condition) = guard else {
        return body;
    };
    guard_prefix.push(gast::Spanned::new(
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
    guard_prefix
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
    if let Some(statements) = tensor_definition_assignments(lowerer, value, classified, span) {
        assignments.extend(statements);
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

/// Lower one rank-`n` clocked definition through the function-body tensor
/// path, when its value carries a whole-array contraction that the coordinate
/// projection would dissolve (reconstruction ledger R-9).
///
/// The value is lowered ONCE at symbolic axis iterators, exactly as a
/// function-body whole-array assignment is, so a contraction keeps its
/// free-index loops and its hoisted invariant half instead of re-deriving the
/// contraction per target coordinate. The decision is taken from the checked
/// DAE value before any coordinate statement exists
/// ([`expression_projection::contains_whole_array_contraction`]), so this is
/// emission and not recognition (TRP-020/021/035).
///
/// Every precondition fails closed to the coordinate projection:
///
/// * A value without a matrix-involving contraction keeps the coordinate
///   form byte for byte, so equations that never dissolve are untouched.
/// * A value that reads its own target's current tick keeps the coordinate
///   form. (Such a self-read is a scheduling cycle and is rejected later,
///   but this path must not change WHICH refusal the model receives.)
/// * A value the symbolic-index projection cannot lower (for example an
///   array constructor selected per coordinate) keeps the coordinate form:
///   the trial runs on a clone of the lowerer, so a failed attempt leaves no
///   temporary, cache entry or emitted statement behind.
fn tensor_definition_assignments<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    value: dae::ExprId<'dae>,
    classified: &ClassifiedVariable<'dae>,
    span: Span,
) -> Option<Vec<gast::Spanned<gast::Statement>>> {
    if classified.variable.value_type().dimensions().is_empty()
        || !expression_projection::contains_whole_array_contraction(lowerer.view, value)
        || lowerer.tensor_call_projection(value).contains_call
    {
        return None;
    }
    let mut current_reads = HashSet::new();
    collect_current_reads(lowerer.view, value, &mut current_reads);
    if current_reads.contains(&classified.id.index()) {
        return None;
    }
    let mut trial = lowerer.clone();
    match lower_tensor_definition(&mut trial, value, classified, span) {
        Ok(statements) => {
            *lowerer = trial;
            Some(statements)
        }
        Err(_) => None,
    }
}

/// The tensor-loop statement sequence for one clocked whole-array definition:
/// the mirror of `user_functions::lower_tensor_function_assignment`, storing
/// to checked state instead of a function local.
///
/// No snapshot prologue is needed here: the caller has already proven the
/// value does not read the target's current tick, so the store can never
/// observe its own writes.
fn lower_tensor_definition<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    value: dae::ExprId<'dae>,
    classified: &ClassifiedVariable<'dae>,
    span: Span,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let dimensions = classified.variable.value_type().dimensions();
    let names = dimensions
        .iter()
        .enumerate()
        .map(|(axis, _)| {
            gast::Name::ident(format!(
                "rumoca_tensor_{}_{}_{}",
                lowerer.temporary_namespace, lowerer.temporary_counter, axis
            ))
        })
        .collect::<Vec<_>>();
    lowerer.temporary_counter += 1;
    let bounds_depth = lowerer.loop_index_bounds.len();
    for (name, &extent) in names.iter().zip(dimensions) {
        lowerer.loop_index_bounds.push(LoopIndexBound {
            name: name.clone(),
            minimum: 1,
            maximum: i64::from(extent),
        });
    }
    let indices = names
        .iter()
        .cloned()
        .map(|name| gast::Expression::Ref(gast::Reference::local(name)))
        .collect::<Vec<_>>();
    let prefix_start = lowerer.pending_prefix_statements.len();
    let lowered = lowerer.lower_at(value, &indices);
    lowerer.loop_index_bounds.truncate(bounds_depth);
    let lowered = coerce(lowered?, classified.scalar_type, span)?;
    let prefixes = lowerer.pending_prefix_statements.split_off(prefix_start);
    let (before, mut body) = user_functions::partition_tensor_prefixes(prefixes, &names);
    lowerer.pending_prefix_statements.extend(before);
    body.push(gast::Spanned::new(
        gast::Statement::Assignment {
            target: state_reference_with_subscripts(classified.name.clone(), indices, span),
            value: lowered,
        },
        span,
    ));
    Ok(user_functions::nest_tensor_loops(
        body,
        &names,
        &expression_projection::AxisBounds {
            extents: dimensions,
            proven: &|index, extent| lowerer.prove_dynamic_index(index, extent, span).is_ok(),
        },
        span,
    ))
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
        pending[owner].regions.extend(assignment.regions);
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
    clock_owners_of_kind(view, dae::ClockedVariableKind::DiscreteReal)
}

fn lower_event_actions<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    action_ids: &[dae::EventActionId<'dae>],
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    pending: &mut Vec<ClockedAssignment>,
    call_actions: &mut Vec<PreparedCallActions>,
) -> Result<(), GalecTargetError> {
    for action_id in action_ids {
        let action = view
            .event_action(*action_id)
            .expect("planned event action resolves");
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
        if !trigger_is_always {
            require_periodic_trigger(view, action.trigger(), clock, span)?;
        }
        let prepared = lowerer.prepare_emission_group(
            EmissionRegion::EventAction {
                clock: clock.index(),
                action: action_id.index(),
            },
            &HashSet::new(),
            CrossGroupCallRetention::Refuse,
            |lowerer, statements| {
                let guard = lower_action_guard(
                    &mut ActionGuardContext {
                        view,
                        expected: clock,
                        lowerer,
                        span,
                    },
                    action.guard(),
                )?;
                let signal = gast::Spanned::new(
                    gast::Statement::Signal(vec![gast::Identifier::new(
                        gast::PredefinedSignal::InvalidArgument.name(),
                    )]),
                    span,
                );
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
                Ok(())
            },
        )?;
        let mut statements = Vec::new();
        prepared.commit_into(&mut statements, call_actions);
        let mut reads = HashSet::new();
        collect_condition_current_reads(view, action.trigger(), &mut reads);
        collect_condition_current_reads(view, action.guard(), &mut reads);
        pending.push(ClockedAssignment {
            targets: HashSet::new(),
            reads,
            regions: HashSet::from([EmissionRegion::EventAction {
                clock: clock.index(),
                action: action_id.index(),
            }]),
            statements,
            span,
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
    call_actions: &'context mut Vec<PreparedCallActions>,
}

fn lower_discrete_value_owners<'dae>(
    context: &mut DiscreteValueLowering<'_, '_, 'dae>,
    owner_ids: &[dae::DiscreteValueOwnerId<'dae>],
    pending: &mut Vec<ClockedAssignment>,
) -> Result<(), GalecTargetError> {
    for owner_id in owner_ids {
        let owner = context
            .view
            .discrete_value_owner(*owner_id)
            .expect("checked B.1c owner resolves");
        pending.push(lower_discrete_value_owner(context, *owner_id, owner)?);
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
    clock_owners_of_kind(view, dae::ClockedVariableKind::DiscreteValue)
}

/// Owning clock of every checked coordinate of one clocked variable kind.
fn clock_owners_of_kind(
    view: dae::DaeView<'_>,
    kind: dae::ClockedVariableKind,
) -> HashMap<u32, u32> {
    view.clock_ownerships()
        .filter_map(|(_, ownership)| {
            (ownership.kind() == kind)
                .then_some((ownership.variable().index(), ownership.clock().index()))
        })
        .collect()
}

fn lower_discrete_value_owner<'dae>(
    context: &mut DiscreteValueLowering<'_, '_, 'dae>,
    owner_id: dae::DiscreteValueOwnerId<'dae>,
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
    let targets = target_variables
        .iter()
        .map(|target| target.index())
        .collect::<HashSet<_>>();
    let request = DiscreteValueOwnerRequest {
        owner_id,
        classified: &classified,
        targets: &targets,
    };
    let mut emission = DiscreteValueOwnerEmission {
        regions: HashSet::new(),
        reads: HashSet::new(),
        conditional: Vec::new(),
        unconditional: None,
    };
    for (branch_index, branch) in owner.branches().iter().enumerate() {
        lower_discrete_value_owner_branch(context, request, branch_index, branch, &mut emission)?;
    }
    let statements =
        compose_discrete_value_branches(emission.conditional, emission.unconditional, span);
    Ok(ClockedAssignment {
        targets,
        reads: emission.reads,
        regions: emission.regions,
        statements,
        span,
    })
}

#[derive(Clone, Copy)]
struct DiscreteValueOwnerRequest<'owner, 'dae> {
    owner_id: dae::DiscreteValueOwnerId<'dae>,
    classified: &'owner [&'owner ClassifiedVariable<'dae>],
    targets: &'owner HashSet<u32>,
}

struct DiscreteValueOwnerEmission {
    regions: HashSet<EmissionRegion>,
    reads: HashSet<u32>,
    conditional: Vec<GuardedDiscreteValueBranch>,
    unconditional: Option<Vec<gast::Spanned<gast::Statement>>>,
}

fn lower_discrete_value_owner_branch<'dae>(
    context: &mut DiscreteValueLowering<'_, '_, 'dae>,
    request: DiscreteValueOwnerRequest<'_, 'dae>,
    branch_index: usize,
    branch: dae::DiscreteValueBranchView<'dae>,
    emission: &mut DiscreteValueOwnerEmission,
) -> Result<(), GalecTargetError> {
    let branch_span = branch.provenance().span();
    match branch.activation() {
        dae::DiscreteBranchActivation::Always => {
            let region = discrete_value_region(context.clock, request.owner_id, branch_index);
            emission.regions.insert(region);
            let prepared = context.lowerer.prepare_emission_group(
                region,
                request.targets,
                CrossGroupCallRetention::Refuse,
                |lowerer, statements| {
                    lower_discrete_value_branch(lowerer, request.classified, branch, statements)
                },
            )?;
            let mut statements = Vec::new();
            prepared.commit_into(&mut statements, context.call_actions);
            emission.unconditional = Some(statements);
        }
        dae::DiscreteBranchActivation::When { trigger, guard } => {
            require_periodic_trigger(context.view, trigger, context.clock, branch_span)?;
            collect_condition_current_reads(context.view, trigger, &mut emission.reads);
            collect_condition_current_reads(context.view, guard, &mut emission.reads);
            let guard_region = EmissionRegion::DiscreteValueGuard {
                clock: context.clock.index(),
                owner: request.owner_id.index(),
                branch: branch_index,
            };
            emission.regions.insert(guard_region);
            let prepared_guard = context.lowerer.prepare_emission_group(
                guard_region,
                &HashSet::new(),
                CrossGroupCallRetention::Refuse,
                |lowerer, _| {
                    lower_action_guard(
                        &mut ActionGuardContext {
                            view: context.view,
                            expected: context.clock,
                            lowerer,
                            span: branch_span,
                        },
                        guard,
                    )
                },
            )?;
            let mut condition_prefix = Vec::new();
            let condition = prepared_guard
                .commit_into(&mut condition_prefix, context.call_actions)
                .unwrap_or(gast::Expression::Bool(true));
            let value_region = discrete_value_region(context.clock, request.owner_id, branch_index);
            emission.regions.insert(value_region);
            let prepared_value = context.lowerer.prepare_emission_group(
                value_region,
                request.targets,
                CrossGroupCallRetention::Refuse,
                |lowerer, statements| {
                    lower_discrete_value_branch(lowerer, request.classified, branch, statements)
                },
            )?;
            let mut body = Vec::new();
            prepared_value.commit_into(&mut body, context.call_actions);
            emission.conditional.push(GuardedDiscreteValueBranch {
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
        collect_current_reads(context.view, value, &mut emission.reads);
    }
    Ok(())
}

fn discrete_value_region(
    clock: dae::ClockId<'_>,
    owner: dae::DiscreteValueOwnerId<'_>,
    branch: usize,
) -> EmissionRegion {
    EmissionRegion::DiscreteValueValue {
        clock: clock.index(),
        owner: owner.index(),
        branch,
    }
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
    targets: &[&ClassifiedVariable<'dae>],
    branch: dae::DiscreteValueBranchView<'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    for (classified, (value, provenance)) in targets.iter().zip(branch.values().iter()) {
        append_definition_assignments(lowerer, value, classified, provenance.span(), statements)?;
    }
    Ok(())
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
    call_actions: &[PreparedCallActions],
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let dependencies = call_actions
        .iter()
        .map(|action| (action.region(), action.dependencies().clone()))
        .collect::<HashMap<_, _>>();
    let argument_reads = call_actions
        .iter()
        .map(|action| (action.region(), action.argument_reads()))
        .collect::<HashMap<_, _>>();
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
    let region_owners = pending
        .iter()
        .enumerate()
        .flat_map(|(index, assignment)| {
            assignment
                .regions
                .iter()
                .map(move |region| (*region, index))
        })
        .collect::<HashMap<_, _>>();
    let mut emitted = vec![false; pending.len()];
    let mut emitted_owners = 0usize;
    let mut ordered = Vec::with_capacity(pending.len());
    while emitted_owners < pending.len() {
        let Some(index) = pending.iter().enumerate().position(|(index, assignment)| {
            !emitted[index]
                && assignment_reads_are_ready(
                    assignment,
                    index,
                    &emitted,
                    &argument_reads,
                    &targets,
                )
                && assignment_regions_are_ready(
                    assignment,
                    index,
                    &emitted,
                    &dependencies,
                    &region_owners,
                )
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

#[cfg(test)]
fn assignment_reads_are_ready(
    assignment: &ClockedAssignment,
    owner: usize,
    emitted: &[bool],
    argument_reads: &HashMap<EmissionRegion, HashSet<u32>>,
    targets: &HashMap<u32, usize>,
) -> bool {
    assignment
        .reads
        .iter()
        .chain(
            assignment
                .regions
                .iter()
                .filter_map(|region| argument_reads.get(region))
                .flatten(),
        )
        .all(|read| target_owner_is_ready(*read, owner, emitted, targets))
}

#[cfg(test)]
fn target_owner_is_ready(
    read: u32,
    owner: usize,
    emitted: &[bool],
    targets: &HashMap<u32, usize>,
) -> bool {
    let Some(dependency) = targets.get(&read) else {
        return true;
    };
    *dependency == owner || emitted[*dependency]
}

#[cfg(test)]
fn assignment_regions_are_ready(
    assignment: &ClockedAssignment,
    owner: usize,
    emitted: &[bool],
    dependencies: &HashMap<EmissionRegion, HashSet<EmissionRegion>>,
    region_owners: &HashMap<EmissionRegion, usize>,
) -> bool {
    assignment.regions.iter().all(|region| {
        region_dependencies_are_ready(*region, owner, emitted, dependencies, region_owners)
    })
}

#[cfg(test)]
fn region_dependencies_are_ready(
    region: EmissionRegion,
    owner: usize,
    emitted: &[bool],
    dependencies: &HashMap<EmissionRegion, HashSet<EmissionRegion>>,
    region_owners: &HashMap<EmissionRegion, usize>,
) -> bool {
    let Some(dependencies) = dependencies.get(&region) else {
        return true;
    };
    dependencies
        .iter()
        .all(|dependency| region_owner_is_ready(*dependency, owner, emitted, region_owners))
}

#[cfg(test)]
fn region_owner_is_ready(
    dependency: EmissionRegion,
    owner: usize,
    emitted: &[bool],
    region_owners: &HashMap<EmissionRegion, usize>,
) -> bool {
    let Some(dependency_owner) = region_owners.get(&dependency) else {
        return true;
    };
    *dependency_owner == owner || emitted[*dependency_owner]
}
