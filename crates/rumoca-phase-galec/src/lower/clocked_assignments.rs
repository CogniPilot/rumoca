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
    let shared_calls = lower_clock_domain_preamble(
        view,
        clock,
        include_unclocked_actions,
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
        assignment.reads =
            expand_causal_definition_reads(view, std::mem::take(&mut assignment.reads));
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

fn lower_clock_domain_preamble<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    include_unclocked: bool,
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
    let causal_definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
    for index in 0..view.discrete_real_equation_count() {
        if causal_definitions.consumes_discrete_real_equation(index) {
            continue;
        }
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        let (target, value) = explicit_discrete_real_definition(view, equation)?;
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
    if !seen.insert(expression.index()) {
        return;
    }
    let operation = view
        .expression(expression)
        .expect("checked eager expression resolves")
        .operation();
    let mut children = Vec::new();
    match operation {
        dae::ExpressionOperation::Literal(_)
        | dae::ExpressionOperation::Coordinate(_)
        | dae::ExpressionOperation::Conditional(_)
        | dae::ExpressionOperation::Comprehension { .. }
        | dae::ExpressionOperation::FunctionValue { .. }
        | dae::ExpressionOperation::FunctionFoldParameter { .. }
        | dae::ExpressionOperation::FunctionFoldOutput { .. } => {}
        dae::ExpressionOperation::Call { arguments, .. } => {
            occurrences
                .entry(expression.index())
                .and_modify(|(_, count)| *count += 1)
                .or_insert((expression, 1));
            children.extend(arguments.iter());
        }
        dae::ExpressionOperation::Unary { operand, .. } => children.push(operand),
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
            children.extend([lhs, rhs]);
        }
        dae::ExpressionOperation::Array(operands)
        | dae::ExpressionOperation::Record(operands)
        | dae::ExpressionOperation::Builtin {
            arguments: operands,
            ..
        } => {
            children.extend(operands.iter());
        }
        dae::ExpressionOperation::Field { base, .. } => children.push(base),
        dae::ExpressionOperation::Range(range) => {
            children.push(range.start().expression());
            if let Some(step) = range.explicit_step() {
                children.push(step.expression());
            }
            children.push(range.stop().expression());
        }
        dae::ExpressionOperation::Index { base, subscripts } => {
            children.push(base);
            for subscript in subscripts.iter() {
                match subscript {
                    dae::SubscriptView::Index { expression, .. }
                    | dae::SubscriptView::Slice { expression, .. } => children.push(expression),
                    dae::SubscriptView::Whole { .. } => {}
                }
            }
        }
        dae::ExpressionOperation::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            children.extend([base, value]);
            for subscript in subscripts.iter() {
                match subscript {
                    dae::SubscriptView::Index { expression, .. }
                    | dae::SubscriptView::Slice { expression, .. } => children.push(expression),
                    dae::SubscriptView::Whole { .. } => {}
                }
            }
        }
        dae::ExpressionOperation::StringConversion { value, format, .. } => {
            children.push(value);
            match format {
                dae::StringConversionFormatView::Options {
                    minimum_length,
                    left_justified,
                    significant_digits,
                } => {
                    for option in [minimum_length, left_justified, significant_digits]
                        .into_iter()
                        .flatten()
                    {
                        children.push(option);
                    }
                }
                dae::StringConversionFormatView::Format { value } => children.push(value),
            }
        }
        dae::ExpressionOperation::ClockTransfer { source, .. } => children.push(source),
    }
    for child in children {
        collect_eager_calls(view, child, seen, occurrences);
    }
}

/// Expand current-tick reads through every exact acyclic algebraic definition.
///
/// Clock-domain ordering cannot stop at an intermediate algebraic coordinate:
/// if `command = filtered` and `filtered = slowState`, the fast domain reads
/// `slowState` even though its lowered expression initially names `filtered`.
/// [`CausalDefinitions`] is the construction proof that this traversal is
/// finite and semantics-preserving.
fn expand_causal_definition_reads(view: dae::DaeView<'_>, mut reads: HashSet<u32>) -> HashSet<u32> {
    let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
    let mut pending = reads.iter().copied().collect::<Vec<_>>();
    let mut expanded = HashSet::new();
    while let Some(index) = pending.pop() {
        if !expanded.insert(index) {
            continue;
        }
        let Some(variable) = view
            .variables()
            .map(|(id, _)| id)
            .find(|id| id.index() == index)
        else {
            continue;
        };
        let Some(definition) = definitions.definition_for_variable(variable) else {
            continue;
        };
        let mut definition_reads = HashSet::new();
        collect_current_reads(view, definition, &mut definition_reads);
        for dependency in definition_reads {
            if reads.insert(dependency) {
                pending.push(dependency);
            }
        }
    }
    reads
}

fn lower_discrete_real_equations<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pending: &mut Vec<ClockedAssignment>,
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    shared_calls: &SharedMaterializedFunctionCalls,
) -> Result<(), GalecTargetError> {
    let mut owners: HashMap<u32, (usize, bool)> = HashMap::new();
    let clock_owners = discrete_real_clock_owners(view);
    let causal_definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
    for index in 0..view.discrete_real_equation_count() {
        if causal_definitions.consumes_discrete_real_equation(index) {
            continue;
        }
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        let span = equation.provenance().span();
        let (target, value) = explicit_discrete_real_definition(view, equation)?;
        let owner_clock = require_discrete_real_clock_owner(&clock_owners, target, span)?;
        if owner_clock != clock.index() {
            continue;
        }
        let classified = by_id.get(&target.index()).ok_or_else(|| {
            GalecTargetError::UnknownVariableReference {
                name: format!("#{}", target.index()),
                span: Some(span),
            }
        })?;
        let (guard, guard_prefix, unconditional) = match equation.activation() {
            dae::DiscreteRealActivation::Always => (None, Vec::new(), true),
            dae::DiscreteRealActivation::When { trigger, guard } => {
                require_periodic_trigger(view, trigger, clock, span)?;
                let guard = lower_action_guard(view, guard, clock, lowerer, span)?;
                let prefix = lowerer.take_prefix_statements_with_shared_calls(shared_calls);
                (guard, prefix, false)
            }
        };
        let mut assignments = Vec::with_capacity(classified.variable.scalar_count());
        for indices in row_major_indices(classified.variable.value_type().dimensions()) {
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
        let mut reads = HashSet::new();
        collect_current_reads(view, value, &mut reads);
        if let dae::DiscreteRealActivation::When { trigger, guard } = equation.activation() {
            collect_condition_current_reads(view, trigger, &mut reads);
            collect_condition_current_reads(view, guard, &mut reads);
        }
        let assignment = ClockedAssignment {
            targets: [target.index()].into_iter().collect(),
            reads,
            statements,
            span,
            is_preamble: false,
            requires_preamble: !shared_calls.is_empty(),
        };
        if let Some(&(owner, owner_unconditional)) = owners.get(&target.index()) {
            if unconditional || owner_unconditional {
                return Err(unsupported(
                    "multiple-discrete-real-definitions",
                    format!(
                        "discrete Real `{}` has multiple definitions without one conditional owner",
                        classified.variable.name()
                    ),
                    span,
                ));
            }
            pending[owner].reads.extend(assignment.reads);
            pending[owner].statements.extend(assignment.statements);
        } else {
            owners.insert(target.index(), (pending.len(), unconditional));
            pending.push(assignment);
        }
    }
    Ok(())
}

fn explicit_discrete_real_definition<'dae>(
    view: dae::DaeView<'dae>,
    equation: dae::DiscreteRealEquationView<'dae>,
) -> Result<(dae::VariableId<'dae>, dae::ExprId<'dae>), GalecTargetError> {
    let span = equation.provenance().span();
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = view
        .expression(equation.residual())
        .expect("checked discrete Real residual resolves")
        .operation()
    else {
        return Err(coupled_discrete_real_equation(span));
    };
    match (
        direct_discrete_real_coordinate(view, lhs),
        direct_discrete_real_coordinate(view, rhs),
    ) {
        (Some(target), None) if !reads_current_target(view, rhs, target) => {
            Ok((dae::VariableId::from(target), rhs))
        }
        (None, Some(target)) if !reads_current_target(view, lhs, target) => {
            Ok((dae::VariableId::from(target), lhs))
        }
        (Some(lhs_target), Some(rhs_target)) => {
            explicit_direct_alias(view, lhs_target, rhs_target, lhs, rhs, span)
        }
        _ => Err(coupled_discrete_real_equation(span)),
    }
}

/// Solve an exact coordinate alias without pretending an arbitrary B.1b row is solved.
///
/// A top-level output causality is an exact semantic direction retained by DAE.
/// When exactly one side has that causality, the residual defines that output
/// from the other clocked coordinate. Otherwise `lhs - rhs = 0` is exactly
/// solved for `lhs`; unique-target and acyclic current-tick checks below must
/// still prove that all selected aliases form one executable assignment graph.
fn explicit_direct_alias<'dae>(
    view: dae::DaeView<'dae>,
    lhs_target: dae::DiscreteRealId<'dae>,
    rhs_target: dae::DiscreteRealId<'dae>,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
    span: Span,
) -> Result<(dae::VariableId<'dae>, dae::ExprId<'dae>), GalecTargetError> {
    let lhs_output = view
        .variable(dae::VariableId::from(lhs_target))
        .expect("checked B.1b lhs variable resolves")
        .causality()
        == dae::VariableCausality::Output;
    let rhs_output = view
        .variable(dae::VariableId::from(rhs_target))
        .expect("checked B.1b rhs variable resolves")
        .causality()
        == dae::VariableCausality::Output;
    match (lhs_output, rhs_output) {
        (true, false) => Ok((dae::VariableId::from(lhs_target), rhs)),
        (false, true) => Ok((dae::VariableId::from(rhs_target), lhs)),
        _ if lhs_target != rhs_target => Ok((dae::VariableId::from(lhs_target), rhs)),
        _ => Err(coupled_discrete_real_equation(span)),
    }
}

fn direct_discrete_real_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<dae::DiscreteRealId<'dae>> {
    match view
        .expression(expression)
        .expect("checked residual operand resolves")
        .operation()
    {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(id)) => Some(id),
        _ => None,
    }
}

fn reads_current_target<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    target: dae::DiscreteRealId<'dae>,
) -> bool {
    let mut reads = HashSet::new();
    collect_current_reads(view, expression, &mut reads);
    reads.contains(&dae::VariableId::from(target).index())
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
        let span = provenance.span();
        for indices in row_major_indices(classified.variable.value_type().dimensions()) {
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
