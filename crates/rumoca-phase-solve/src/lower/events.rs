use std::collections::BTreeSet;

use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::clocks::LoweredClocks;
use super::{
    LoweredLayout, ScalarCompiler, ScalarRows, delay_value_scalar_slot, variable_scalar_slot,
};
use crate::LowerError;

pub(super) fn lower_discrete_and_events<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
) -> Result<(solve::DiscreteSolveSystem, solve::SolveEventPartition), LowerError> {
    let mut discrete = DiscreteRows::new(view);
    lower_discrete_real_equations(view, layout, clocks, &mut discrete)?;
    lower_discrete_value_owners(view, layout, clocks, &mut discrete)?;
    let mut event_actions = Vec::new();
    let mut action_conditions = ScalarRows::default();
    lower_event_actions(
        view,
        layout,
        clocks,
        &mut discrete,
        &mut event_actions,
        &mut action_conditions,
    )?;
    lower_condition_memory(view, layout, clocks, &mut discrete)?;
    let roots = lower_roots(view, layout, clocks, &discrete.relation_memory_owners)?;
    let (scheduled_time_events, dynamic_time_event_rhs) = lower_time_events(view, layout)?;
    let delays = lower_delays(view, layout)?;
    let discrete = discrete.finish()?;
    let events = solve::SolveEventPartition {
        root_conditions: roots.programs,
        root_relation_memory_targets: roots.relation_memory_targets,
        root_zero_domains: roots.zero_domains,
        condition_memory_parameter_indices: layout.condition_memory.clone(),
        scheduled_time_events,
        dynamic_time_event_rhs,
        action_conditions: action_conditions.into_scalar_block()?,
        actions: event_actions,
        has_terminal_event: view.terminal_count() != 0,
        delays,
        ..solve::SolveEventPartition::default()
    };
    Ok((discrete, events))
}

fn lower_delays<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
) -> Result<solve::SolveDelayPartition, LowerError> {
    let mut source_rhs = ScalarRows::default();
    let mut delay_time_rhs = ScalarRows::default();
    let mut delay_max_rhs = ScalarRows::default();
    let mut value_parameter_indices = Vec::new();
    let mut source_is_discrete = Vec::new();
    for index in 0..view.delay_count() {
        let id = view.delay_id(index).expect("dense delay identity resolves");
        let delay = view.delay(id).expect("checked delay identity resolves");
        let span = delay.provenance().span();
        let (delay_time, delay_max) = match delay.operation() {
            dae::DelayOperation::ParameterDelay { delay_time } => {
                (delay_time.expression(), delay_time.expression())
            }
            dae::DelayOperation::BoundedDelay {
                delay_time,
                delay_max,
            } => (delay_time, delay_max.expression()),
        };
        let scalar_count = delay
            .value_type()
            .scalar_count()
            .expect("checked delay value type has scalar capacity");
        for scalar in 0..scalar_count {
            let channel = value_parameter_indices.len();
            source_rhs.push(
                ScalarCompiler::new(view, layout, None).program(delay.source(), scalar)?,
                span,
                channel,
            );
            delay_time_rhs.push(
                ScalarCompiler::new(view, layout, None).program(delay_time, 0)?,
                span,
                channel,
            );
            delay_max_rhs.push(
                ScalarCompiler::new(view, layout, None).program(delay_max, 0)?,
                span,
                channel,
            );
            let slot = delay_value_scalar_slot(layout, id.index(), scalar, span)?;
            let solve::ScalarSlot::P { index, .. } = slot else {
                unreachable!("delay values always occupy runtime-managed P slots")
            };
            value_parameter_indices.push(index);
            source_is_discrete.push(delay.variability() != dae::ExpressionVariability::Continuous);
        }
    }
    Ok(solve::SolveDelayPartition {
        source_rhs: source_rhs.into_scalar_block()?,
        delay_time_rhs: delay_time_rhs.into_scalar_block()?,
        delay_max_rhs: delay_max_rhs.into_scalar_block()?,
        value_parameter_indices,
        source_is_discrete,
    })
}

#[derive(Default)]
struct DiscreteRows<'dae> {
    rows: ScalarRows,
    targets: Vec<solve::ScalarSlot>,
    roles: Vec<solve::DiscreteRowRole>,
    pre_modes: Vec<solve::DiscreteEventPreMode>,
    clock_owners: Vec<Option<solve::PeriodicClockId>>,
    structured_rhs: solve::ComputeBlock,
    structured_updates: Vec<solve::StructuredDiscreteUpdate>,
    structured_output_cursor: usize,
    relation_memory_owners: RelationMemoryOwners<'dae>,
}

impl<'dae> DiscreteRows<'dae> {
    fn new(view: dae::DaeView<'dae>) -> Self {
        Self {
            relation_memory_owners: RelationMemoryOwners::new(view),
            ..Self::default()
        }
    }

    fn push(
        &mut self,
        program: Vec<solve::LinearOp>,
        span: Span,
        target: solve::ScalarSlot,
        role: solve::DiscreteRowRole,
        pre_mode: solve::DiscreteEventPreMode,
        clock_owner: Option<solve::PeriodicClockId>,
    ) {
        let output = self.targets.len();
        self.rows.push(program, span, output);
        self.targets.push(target);
        self.roles.push(role);
        self.pre_modes.push(pre_mode);
        self.clock_owners.push(clock_owner);
    }

    fn finish(self) -> Result<solve::DiscreteSolveSystem, LowerError> {
        let rhs = self.rows.into_scalar_block()?;
        Ok(solve::DiscreteSolveSystem {
            update_targets: self.targets,
            row_roles: self.roles,
            pre_modes: self.pre_modes,
            observation_refresh: vec![false; rhs.programs().len()],
            clock_owners: self.clock_owners,
            structured_rhs: self.structured_rhs,
            structured_updates: self.structured_updates,
            rhs,
            ..solve::DiscreteSolveSystem::default()
        })
    }
}

#[derive(Clone, Copy, Default)]
enum RelationMemoryOwner {
    #[default]
    Unclaimed,
    Unique(solve::ScalarSlot),
    Ambiguous,
}

#[derive(Default)]
struct RelationMemoryOwners<'dae> {
    relation_by_expression: Vec<Option<dae::RelationId<'dae>>>,
    owners: Vec<RelationMemoryOwner>,
}

impl<'dae> RelationMemoryOwners<'dae> {
    fn new(view: dae::DaeView<'dae>) -> Self {
        let mut relation_by_expression = vec![None; view.expression_count()];
        for index in 0..view.relation_count() {
            let relation = view
                .relation_id(index)
                .expect("dense checked relation identity resolves");
            let expression = view
                .relation(relation)
                .expect("checked relation resolves")
                .expression();
            relation_by_expression[expression.index() as usize] = Some(relation);
        }
        Self {
            relation_by_expression,
            owners: vec![RelationMemoryOwner::Unclaimed; view.relation_count()],
        }
    }

    fn claim_exact_expression(&mut self, expression: dae::ExprId<'dae>, target: solve::ScalarSlot) {
        let solve::ScalarSlot::P { .. } = target else {
            return;
        };
        let Some(relation) = self
            .relation_by_expression
            .get(expression.index() as usize)
            .copied()
            .flatten()
        else {
            return;
        };
        let owner = &mut self.owners[relation.index() as usize];
        *owner = match *owner {
            RelationMemoryOwner::Unclaimed => RelationMemoryOwner::Unique(target),
            RelationMemoryOwner::Unique(existing) if existing == target => *owner,
            RelationMemoryOwner::Unique(_) | RelationMemoryOwner::Ambiguous => {
                RelationMemoryOwner::Ambiguous
            }
        };
    }

    fn target(&self, relation: dae::RelationId<'dae>) -> Option<solve::ScalarSlot> {
        match self.owners.get(relation.index() as usize).copied() {
            Some(RelationMemoryOwner::Unique(target)) => Some(target),
            Some(RelationMemoryOwner::Unclaimed | RelationMemoryOwner::Ambiguous) | None => None,
        }
    }
}

fn lower_discrete_real_equations<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
) -> Result<(), LowerError> {
    let definitions = resolve_discrete_real_definitions(view)?;
    let mut conditional = Vec::new();
    for (index, (target, value)) in definitions.into_iter().enumerate() {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        let span = equation.provenance().span();
        let variable = dae::VariableId::from(target);
        match equation.activation() {
            dae::DiscreteRealActivation::Always => {
                lower_unconditional_discrete_real(
                    view, layout, clocks, rows, variable, value, span,
                )?;
            }
            dae::DiscreteRealActivation::When { trigger, guard } => {
                conditional.push(EventUpdate {
                    trigger,
                    guard,
                    variable: variable.index(),
                    value,
                    span,
                    clock: checked_discrete_real_activation_clock(
                        view, clocks, variable, guard, span,
                    )?,
                });
            }
        }
    }
    lower_guarded_updates(
        view,
        layout,
        clocks,
        rows,
        &conditional,
        solve::DiscreteRowRole::Equation,
    )
}

fn checked_discrete_real_activation_clock<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    variable: dae::VariableId<'dae>,
    guard: dae::ConditionId<'dae>,
    span: Span,
) -> Result<Option<dae::ClockId<'dae>>, LowerError> {
    let activation = condition_clock_owner(view, guard);
    let owner = clocks.variable_owner(variable).map(|(clock, _)| clock);
    match (activation, owner) {
        (None, None) => Ok(None),
        (Some(activation), Some(owner)) if activation == owner => Ok(Some(owner)),
        (Some(_), None) => Err(LowerError::non_computable(
            "clock-activated discrete Real definition has no matching target clock owner",
            span,
        )),
        (None, Some(_)) => Err(LowerError::non_computable(
            "clock-owned discrete Real target has a non-clock event activation",
            span,
        )),
        (Some(_), Some(_)) => Err(LowerError::non_computable(
            "discrete Real definition activation does not match its target clock owner",
            span,
        )),
    }
}

fn lower_unconditional_discrete_real<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
    variable: dae::VariableId<'dae>,
    value: dae::ExprId<'dae>,
    span: Span,
) -> Result<(), LowerError> {
    let value_type = view
        .expression(value)
        .expect("checked discrete definition value resolves")
        .value_type();
    let clock = clocks.variable_owner(variable);
    let sampled = clocks.variable_is_sampled(variable);
    for scalar in 0..value_type
        .scalar_count()
        .expect("checked expression scalar capacity")
    {
        let program = match clock {
            Some((clock, _)) if sampled => {
                ScalarCompiler::new(view, layout, None).sampled_program(clock, value, scalar)?
            }
            Some((clock, _)) => {
                ScalarCompiler::new(view, layout, None).clocked_program(clock, value, scalar)?
            }
            None => ScalarCompiler::new(view, layout, None).program(value, scalar)?,
        };
        let target = variable_scalar_slot(layout, variable.index(), scalar, span)?;
        rows.push(
            program,
            span,
            target,
            solve::DiscreteRowRole::Equation,
            expression_pre_mode(view, value, sampled),
            clock.map(|(_, solve)| solve),
        );
    }
    Ok(())
}

/// Orients every discrete `Real` row toward the coordinate that row defines.
///
/// A row that names a whole discrete `Real` coordinate on exactly one side
/// states its own causality. A row that names one on *each* side states only
/// that the two are equal: MLS §9.1 connection equations between clocked
/// signals — `connect(sample.y, assignClock.u)` in the `Modelica.Clocked`
/// examples — have that shape, and their target is whichever coordinate the
/// rest of the partition leaves undefined.
///
/// A two-sided row is therefore oriented by forced elimination: it takes the one
/// candidate that no other row already defines. The one-sided rows state the
/// initial set of defined coordinates — several `when` branches may define the
/// same coordinate, so that set is a union and never an exclusive claim — and
/// each forced two-sided orientation extends it until no further row is forced.
/// A row that never becomes forced admits more than one causality and is
/// reported at its own span, never guessed.
fn resolve_discrete_real_definitions<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<Vec<(dae::DiscreteRealId<'dae>, dae::ExprId<'dae>)>, LowerError> {
    let count = view.discrete_real_equation_count();
    let mut candidates = Vec::with_capacity(count);
    let mut spans = Vec::with_capacity(count);
    for index in 0..count {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        candidates.push(discrete_real_definition_candidates(
            view,
            equation.residual(),
        ));
        spans.push(equation.provenance().span());
    }
    let mut resolved = vec![None; count];
    let mut defined = BTreeSet::new();
    let mut pending = count;
    for (index, row) in candidates.iter().enumerate() {
        if let [definition] = row.as_slice() {
            defined.insert(definition.0.index());
            resolved[index] = Some(*definition);
            pending -= 1;
        }
    }
    while pending != 0 {
        let forced = force_discrete_real_rows(&candidates, &mut resolved, &mut defined);
        if forced == 0 {
            let unresolved = resolved
                .iter()
                .position(Option::is_none)
                .expect("a pending row has no resolved definition");
            return Err(LowerError::non_computable(
                "coupled discrete Real residual is not an explicit computable definition",
                spans[unresolved],
            ));
        }
        pending -= forced;
    }
    Ok(resolved
        .into_iter()
        .map(|definition| definition.expect("every discrete Real row was oriented"))
        .collect())
}

/// One elimination sweep: orients every still-open row whose candidates have
/// been narrowed to a single coordinate no other row defines, and reports how
/// many rows the sweep oriented.
fn force_discrete_real_rows<'dae>(
    candidates: &[Vec<(dae::DiscreteRealId<'dae>, dae::ExprId<'dae>)>],
    resolved: &mut [Option<(dae::DiscreteRealId<'dae>, dae::ExprId<'dae>)>],
    defined: &mut BTreeSet<u32>,
) -> usize {
    let mut forced = 0;
    for (index, row) in candidates.iter().enumerate() {
        let mut open = row
            .iter()
            .filter(|(target, _)| !defined.contains(&target.index()));
        let (None, Some(definition), None) = (resolved[index], open.next(), open.next()) else {
            continue;
        };
        defined.insert(definition.0.index());
        resolved[index] = Some(*definition);
        forced += 1;
    }
    forced
}

/// The discrete `Real` coordinates one residual could define, in residual order.
fn discrete_real_definition_candidates<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Vec<(dae::DiscreteRealId<'dae>, dae::ExprId<'dae>)> {
    let Some(residual) = view.expression(residual) else {
        return Vec::new();
    };
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = residual.operation()
    else {
        return Vec::new();
    };
    [(lhs, rhs), (rhs, lhs)]
        .into_iter()
        .filter_map(|(side, value)| {
            compatible_discrete_definition(view, whole_discrete_real(view, side)?, value)
        })
        .collect()
}

fn compatible_discrete_definition<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::DiscreteRealId<'dae>,
    value: dae::ExprId<'dae>,
) -> Option<(dae::DiscreteRealId<'dae>, dae::ExprId<'dae>)> {
    let variable = view.variable(dae::VariableId::from(target))?;
    let expression = view.expression(value)?;
    (defines_discrete_real(variable.value_type(), expression.value_type())
        && !reads_current_discrete_real(view, value, target))
    .then_some((target, value))
}

/// True when a value of type `value` can define a discrete `Real` target of type `target`.
///
/// The checked DAE preserves each expression's own source type, so the Integer literal in
/// `x = 1` keeps `ScalarType::Integer` even when `x` is `Real`. MLS §6.7 admits that
/// implicit Integer-to-Real conversion, so requiring identical value types would reject
/// a legal explicit definition. Shape must still agree exactly.
fn defines_discrete_real(target: &dae::ValueType, value: &dae::ValueType) -> bool {
    target.dimensions() == value.dimensions()
        && target.scalar_type() == dae::ScalarType::Real
        && matches!(
            value.scalar_type(),
            dae::ScalarType::Real | dae::ScalarType::Integer
        )
}

/// True when `value` reads the *current* coordinate of `target`.
///
/// Only a current-value occurrence couples a discrete Real definition to itself. MLS
/// §3.7.5 defines `pre(x)` as the left limit of `x`, which is already settled when the
/// event fires, so `x = a * pre(x) + b * u` is an explicit computable definition of `x`.
/// The generic [`dae::expr_contains_var`] query deliberately treats `pre` and current
/// coordinates as the same declaration, so it cannot be used to decide computability.
fn reads_current_discrete_real<'dae>(
    view: dae::DaeView<'dae>,
    value: dae::ExprId<'dae>,
    target: dae::DiscreteRealId<'dae>,
) -> bool {
    let mut found = false;
    dae::for_each_expression(view, value, |_, expression| {
        found |= matches!(
            expression.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(candidate))
                if candidate == target
        );
    });
    found
}

fn whole_discrete_real<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<dae::DiscreteRealId<'dae>> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(variable)) => {
            Some(variable)
        }
        _ => None,
    }
}

fn lower_discrete_value_owners<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
) -> Result<(), LowerError> {
    for index in 0..view.discrete_value_owner_count() {
        let id = view
            .discrete_value_owner_id(index)
            .expect("dense B.1c owner identity resolves");
        let owner = view
            .discrete_value_owner(id)
            .expect("checked B.1c owner resolves");
        let first = owner
            .branches()
            .get(0)
            .expect("checked B.1c owner has a nonempty branch set");
        if owner.structure().is_some() {
            lower_structured_discrete_value_owner(view, layout, clocks, rows, owner)?;
            continue;
        }
        match first.activation() {
            dae::DiscreteBranchActivation::Always => {
                lower_unconditional_discrete_value_owner(view, layout, clocks, rows, owner)?;
            }
            dae::DiscreteBranchActivation::When { .. } => {
                lower_conditional_discrete_value_owner(view, layout, clocks, rows, owner)?;
            }
        }
    }
    Ok(())
}

/// Lowers an always-active B.1c owner (`Integer`/`Boolean`/enumeration discretes).
///
/// A clocked partition can own such a target without any `when`: MLS §16.5 makes every
/// equation of a clocked partition active exactly on its partition's clock ticks, so
/// `counter = previous(counter) + 1` is an unconditional equation whose target carries a
/// clock ownership. The row therefore has to be compiled *under* that clock — the same
/// treatment [`lower_unconditional_discrete_real`] gives a clocked discrete `Real` — so
/// that `previous(...)` resolves against its owning schedule and the row is scheduled on
/// its clock's ticks instead of on every event.
fn lower_unconditional_discrete_value_owner<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<(), LowerError> {
    let branch = owner
        .branches()
        .get(0)
        .expect("checked unconditional B.1c owner has one branch");
    debug_assert_eq!(owner.branches().len(), 1);
    for (target, (value, provenance)) in owner.targets().iter().zip(branch.values().iter()) {
        let expression = view
            .expression(value)
            .expect("checked B.1c value expression resolves");
        let span = provenance.span();
        let variable = dae::VariableId::from(target);
        let clock = clocks.variable_owner(variable);
        let sampled = clocks.variable_is_sampled(variable);
        for scalar in 0..expression
            .value_type()
            .scalar_count()
            .expect("checked B.1c value scalar capacity")
        {
            let program =
                match clock {
                    Some((clock, _)) if sampled => ScalarCompiler::new(view, layout, None)
                        .sampled_program(clock, value, scalar)?,
                    Some((clock, _)) => ScalarCompiler::new(view, layout, None)
                        .clocked_program(clock, value, scalar)?,
                    None => ScalarCompiler::new(view, layout, None).program(value, scalar)?,
                };
            let target = variable_scalar_slot(layout, target.index(), scalar, span)?;
            rows.relation_memory_owners
                .claim_exact_expression(value, target);
            rows.push(
                program,
                span,
                target,
                solve::DiscreteRowRole::Equation,
                expression_pre_mode(view, value, sampled),
                clock.map(|(_, solve)| solve),
            );
        }
    }
    Ok(())
}

fn lower_structured_discrete_value_owner<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<(), LowerError> {
    let structure = owner
        .structure()
        .expect("caller selects one checked structured B.1c owner");
    let branch = owner
        .branches()
        .get(0)
        .expect("checked structured B.1c owner has a branch");
    if owner.branches().len() != 1
        || !matches!(branch.activation(), dae::DiscreteBranchActivation::Always)
    {
        return Err(LowerError::non_computable(
            "conditional structured B.1c maps are not yet representable",
            owner.provenance().span(),
        ));
    }
    let domain = view
        .domain(structure.domain())
        .expect("checked structured B.1c domain resolves")
        .structured()
        .clone();
    let point_count = domain.scalar_count().map_err(|error| {
        LowerError::contract(
            format!("structured B.1c domain is invalid: {error}"),
            owner.provenance().span(),
        )
    })?;
    for (target, (value, provenance)) in owner.targets().iter().zip(branch.values().iter()) {
        let span = provenance.span();
        let variable = dae::VariableId::from(target);
        let clock = clocks.variable_owner(variable);
        let sampled = clocks.variable_is_sampled(variable);
        let (base_ops, load_strides, const_strides) =
            structured_map_program(StructuredMapProgramInput {
                view,
                layout,
                domain_id: structure.domain(),
                domain: &domain,
                scalar_view: structure.scalar_view(),
                value,
                clock,
                sampled,
                span,
            })?;
        let output_map =
            solve::TensorOutputMap::dense_contiguous(rows.structured_output_cursor, &domain)
                .map_err(|_| LowerError::contract("structured B.1c output map overflow", span))?;
        let node_index = rows.structured_rhs.nodes.len();
        rows.structured_rhs.nodes.push(solve::ComputeNode::Map {
            domain: domain.clone(),
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata: solve::TensorNodeMetadata::default(),
            span,
        });
        let base = variable_scalar_slot(layout, target.index(), 0, span)?;
        prove_contiguous_structured_target(layout, target.index(), base, point_count, span)?;
        let target_map = solve::TensorOutputMap::dense_contiguous(0, &domain)
            .map_err(|_| LowerError::contract("structured B.1c target map overflow", span))?;
        rows.structured_updates
            .push(solve::StructuredDiscreteUpdate {
                node_index,
                target: solve::StructuredDiscreteTargetMap {
                    base,
                    map: target_map,
                },
                role: solve::DiscreteRowRole::Equation,
                pre_mode: expression_pre_mode(view, value, sampled),
                observation_refresh: false,
                clock_owner: clock.map(|(_, solve)| solve),
            });
        rows.structured_output_cursor = rows
            .structured_output_cursor
            .checked_add(point_count)
            .ok_or_else(|| LowerError::contract("structured B.1c row count overflow", span))?;
    }
    Ok(())
}

struct StructuredMapProgramInput<'scope, 'dae> {
    view: dae::DaeView<'dae>,
    layout: &'scope LoweredLayout<'dae>,
    domain_id: dae::DomainId<'dae>,
    domain: &'scope rumoca_core::StructuredIndexDomain,
    scalar_view: rumoca_core::ComprehensionScalarView,
    value: dae::ExprId<'dae>,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
    sampled: bool,
    span: Span,
}

type AffineProgramCertificate = (
    Vec<solve::LinearOp>,
    Vec<solve::AffineStencilLoadStride>,
    Vec<solve::AffineStencilConstStride>,
);

fn structured_map_program(
    input: StructuredMapProgramInput<'_, '_>,
) -> Result<AffineProgramCertificate, LowerError> {
    let points = input.domain.index_tuples().map_err(|error| {
        LowerError::contract(
            format!("structured B.1c domain is invalid: {error}"),
            input.span,
        )
    })?;
    let Some(base_point) = points.first() else {
        return Err(LowerError::non_computable(
            "empty structured B.1c domain has no compact base program",
            input.span,
        ));
    };
    let extents = input
        .domain
        .extents()
        .map_err(|error| {
            LowerError::contract(
                format!("structured B.1c domain is invalid: {error}"),
                input.span,
            )
        })?
        .into_iter()
        .map(|extent| {
            u32::try_from(extent)
                .map_err(|_| LowerError::contract("structured B.1c extent overflow", input.span))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut programs = Vec::with_capacity(points.len());
    for (point, values) in points.iter().enumerate() {
        let scalar = input
            .scalar_view
            .body_scalar(point, &extents)
            .ok_or_else(|| {
                LowerError::contract("structured B.1c scalar view overflow", input.span)
            })?;
        let compiler =
            ScalarCompiler::new(input.view, input.layout, Some((input.domain_id, values)));
        programs.push(match input.clock {
            Some((clock, _)) if input.sampled => {
                compiler.sampled_program(clock, input.value, scalar)?
            }
            Some((clock, _)) => compiler.clocked_program(clock, input.value, scalar)?,
            None => compiler.program(input.value, scalar)?,
        });
    }
    derive_affine_program_certificate(input.domain, base_point, &points, &programs, input.span)
}

fn derive_affine_program_certificate(
    domain: &rumoca_core::StructuredIndexDomain,
    base_point: &[i64],
    points: &[Vec<i64>],
    programs: &[Vec<solve::LinearOp>],
    span: Span,
) -> Result<AffineProgramCertificate, LowerError> {
    let base = &programs[0];
    if programs.iter().any(|program| program.len() != base.len()) {
        return Err(non_affine_structured_program(span));
    }
    let offsets = points
        .iter()
        .map(|point| domain_point_offsets(domain, base_point, point, span))
        .collect::<Result<Vec<_>, _>>()?;
    let mut load_strides = Vec::new();
    let mut const_strides = Vec::new();
    let evidence = AffineProgramEvidence {
        programs,
        offsets: &offsets,
        rank: domain.binders.len(),
        span,
    };
    for (op_position, base_op) in base.iter().enumerate() {
        if let Some((kind, dst, base_index)) = affine_load(base_op) {
            let terms = infer_load_terms(kind, dst, base_index, op_position, evidence)?;
            if !terms.is_empty() {
                load_strides.push(solve::AffineStencilLoadStride { op_position, terms });
            }
        } else if let solve::LinearOp::Const { dst, value } = base_op {
            let terms = infer_const_terms(*dst, *value, op_position, evidence)?;
            if !terms.is_empty() {
                const_strides.push(solve::AffineStencilConstStride { op_position, terms });
            }
        } else if programs
            .iter()
            .any(|program| program[op_position] != *base_op)
        {
            return Err(non_affine_structured_program(span));
        }
    }
    Ok((base.clone(), load_strides, const_strides))
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AffineLoadKind {
    Y,
    P,
    Seed,
}

#[derive(Clone, Copy)]
struct AffineProgramEvidence<'scope> {
    programs: &'scope [Vec<solve::LinearOp>],
    offsets: &'scope [Vec<isize>],
    rank: usize,
    span: Span,
}

fn affine_load(op: &solve::LinearOp) -> Option<(AffineLoadKind, solve::Reg, usize)> {
    match *op {
        solve::LinearOp::LoadY { dst, index } => Some((AffineLoadKind::Y, dst, index)),
        solve::LinearOp::LoadP { dst, index } => Some((AffineLoadKind::P, dst, index)),
        solve::LinearOp::LoadSeed { dst, index } => Some((AffineLoadKind::Seed, dst, index)),
        _ => None,
    }
}

fn domain_point_offsets(
    domain: &rumoca_core::StructuredIndexDomain,
    base: &[i64],
    point: &[i64],
    span: Span,
) -> Result<Vec<isize>, LowerError> {
    domain
        .binders
        .iter()
        .enumerate()
        .map(|(dimension, binder)| {
            isize::try_from((point[dimension] - base[dimension]) / binder.step)
                .map_err(|_| LowerError::contract("structured B.1c domain offset overflow", span))
        })
        .collect()
}

fn prove_contiguous_structured_target(
    layout: &LoweredLayout<'_>,
    variable: u32,
    base: solve::ScalarSlot,
    scalar_count: usize,
    span: Span,
) -> Result<(), LowerError> {
    for scalar in 0..scalar_count {
        let actual = variable_scalar_slot(layout, variable, scalar, span)?;
        let expected = match base {
            solve::ScalarSlot::Y { index, .. } => {
                index.checked_add(scalar).map(solve::scalar_slot_y)
            }
            solve::ScalarSlot::P { index, .. } => {
                index.checked_add(scalar).map(solve::scalar_slot_p)
            }
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => None,
        }
        .ok_or_else(|| LowerError::contract("structured B.1c target map overflow", span))?;
        if actual != expected {
            return Err(LowerError::contract(
                "structured B.1c target does not own contiguous Solve storage",
                span,
            ));
        }
    }
    Ok(())
}

fn dimension_probe(offsets: &[Vec<isize>], dimension: usize) -> Option<usize> {
    offsets.iter().position(|offset| {
        offset[dimension] == 1
            && offset
                .iter()
                .enumerate()
                .all(|(other, value)| other == dimension || *value == 0)
    })
}

fn infer_load_terms(
    kind: AffineLoadKind,
    dst: solve::Reg,
    base_index: usize,
    op_position: usize,
    evidence: AffineProgramEvidence<'_>,
) -> Result<Vec<solve::AffineStencilIndexStrideTerm>, LowerError> {
    let mut coefficients = vec![0_isize; evidence.rank];
    for (dimension, coefficient) in coefficients.iter_mut().enumerate() {
        let Some(probe) = dimension_probe(evidence.offsets, dimension) else {
            continue;
        };
        let Some((probe_kind, probe_dst, probe_index)) =
            affine_load(&evidence.programs[probe][op_position])
        else {
            return Err(non_affine_structured_program(evidence.span));
        };
        if probe_kind != kind || probe_dst != dst {
            return Err(non_affine_structured_program(evidence.span));
        }
        *coefficient = isize::try_from(probe_index)
            .ok()
            .and_then(|value| value.checked_sub(isize::try_from(base_index).ok()?))
            .ok_or_else(|| non_affine_structured_program(evidence.span))?;
    }
    for (program, offset) in evidence.programs.iter().zip(evidence.offsets) {
        let Some((actual_kind, actual_dst, actual_index)) = affine_load(&program[op_position])
        else {
            return Err(non_affine_structured_program(evidence.span));
        };
        let expected = affine_index(base_index, &coefficients, offset)
            .ok_or_else(|| non_affine_structured_program(evidence.span))?;
        if actual_kind != kind || actual_dst != dst || actual_index != expected {
            return Err(non_affine_structured_program(evidence.span));
        }
    }
    Ok(coefficients
        .into_iter()
        .enumerate()
        .filter_map(|(dimension, stride)| {
            (stride != 0).then_some(solve::AffineStencilIndexStrideTerm { dimension, stride })
        })
        .collect())
}

fn affine_index(base: usize, coefficients: &[isize], offsets: &[isize]) -> Option<usize> {
    let mut value = isize::try_from(base).ok()?;
    for (&coefficient, &offset) in coefficients.iter().zip(offsets) {
        value = value.checked_add(coefficient.checked_mul(offset)?)?;
    }
    usize::try_from(value).ok()
}

fn infer_const_terms(
    dst: solve::Reg,
    base_value: f64,
    op_position: usize,
    evidence: AffineProgramEvidence<'_>,
) -> Result<Vec<solve::AffineStencilConstStrideTerm>, LowerError> {
    let mut coefficients = vec![0.0; evidence.rank];
    for (dimension, coefficient) in coefficients.iter_mut().enumerate() {
        let Some(probe) = dimension_probe(evidence.offsets, dimension) else {
            continue;
        };
        let solve::LinearOp::Const {
            dst: probe_dst,
            value,
        } = evidence.programs[probe][op_position]
        else {
            return Err(non_affine_structured_program(evidence.span));
        };
        if probe_dst != dst {
            return Err(non_affine_structured_program(evidence.span));
        }
        *coefficient = value - base_value;
    }
    for (program, offset) in evidence.programs.iter().zip(evidence.offsets) {
        let solve::LinearOp::Const {
            dst: actual_dst,
            value,
        } = program[op_position]
        else {
            return Err(non_affine_structured_program(evidence.span));
        };
        let expected = coefficients
            .iter()
            .zip(offset)
            .fold(base_value, |value, (coefficient, offset)| {
                value + coefficient * (*offset as f64)
            });
        if actual_dst != dst || value != expected {
            return Err(non_affine_structured_program(evidence.span));
        }
    }
    Ok(coefficients
        .into_iter()
        .enumerate()
        .filter_map(|(dimension, stride)| {
            (stride != 0.0).then_some(solve::AffineStencilConstStrideTerm { dimension, stride })
        })
        .collect())
}

fn non_affine_structured_program(span: Span) -> LowerError {
    LowerError::non_computable(
        "structured B.1c body does not have one proven affine scalar program",
        span,
    )
}

fn lower_conditional_discrete_value_owner<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<(), LowerError> {
    for (target_ordinal, target) in owner.targets().iter().enumerate() {
        let variable = view
            .variable(dae::VariableId::from(target))
            .expect("checked B.1c target resolves");
        for scalar in 0..variable.scalar_count() {
            let mut lowered = Vec::new();
            for branch in owner.branches().iter() {
                let branch = lower_checked_discrete_value_branch(
                    view,
                    layout,
                    clocks,
                    target,
                    target_ordinal,
                    scalar,
                    branch,
                )?;
                record_guarded_target(
                    &mut lowered,
                    branch.target,
                    branch.assignment,
                    branch.clock,
                    branch.has_pre,
                    branch.span,
                )?;
            }
            let [target] = lowered.as_slice() else {
                unreachable!("one B.1c target and scalar creates one guarded target")
            };
            let program = match target.clock {
                Some((clock, _)) => ScalarCompiler::new(view, layout, None)
                    .clocked_guarded_assignments_program(
                        clock,
                        &target.branches,
                        target.target,
                        target.span,
                    )?,
                None => ScalarCompiler::new(view, layout, None).guarded_assignments_program(
                    &target.branches,
                    target.target,
                    target.span,
                )?,
            };
            rows.push(
                program,
                target.span,
                target.target,
                solve::DiscreteRowRole::EventAction,
                target.pre_mode,
                target.clock.map(|(_, clock)| clock),
            );
        }
    }
    Ok(())
}

struct LoweredDiscreteValueBranch<'dae> {
    target: solve::ScalarSlot,
    assignment: GuardedAssignment<'dae>,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
    has_pre: bool,
    span: Span,
}

fn lower_checked_discrete_value_branch<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    target: dae::DiscreteValueId<'dae>,
    target_ordinal: usize,
    scalar: usize,
    branch: dae::DiscreteValueBranchView<'dae>,
) -> Result<LoweredDiscreteValueBranch<'dae>, LowerError> {
    let dae::DiscreteBranchActivation::When { trigger, guard } = branch.activation() else {
        unreachable!("checked B.1c owner cannot mix always and when branches")
    };
    let (value, provenance) = branch
        .values()
        .get(target_ordinal)
        .expect("checked B.1c branch arity matches its target set");
    let span = provenance.span();
    let target = variable_scalar_slot(layout, target.index(), scalar, span)?;
    let clock = condition_clock_owner(view, guard)
        .map(|clock| clocks.clock(clock).map(|solve| (clock, solve)))
        .transpose()?;
    let trigger_memory = condition_memory(layout, trigger, span)?;
    let has_pre = expression_contains_pre(view, value)
        || condition_contains_pre(view, trigger)
        || condition_contains_pre(view, guard);
    Ok(LoweredDiscreteValueBranch {
        target,
        assignment: (trigger, guard, value, scalar, trigger_memory),
        clock,
        has_pre,
        span,
    })
}

fn lower_event_actions<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    discrete: &mut DiscreteRows<'dae>,
    actions: &mut Vec<solve::SolveEventAction>,
    action_conditions: &mut ScalarRows,
) -> Result<(), LowerError> {
    let mut updates = Vec::new();
    for index in 0..view.event_action_count() {
        let id = view
            .event_action_id(index)
            .expect("dense event action identity resolves");
        let action = view
            .event_action(id)
            .expect("checked event action identity resolves");
        match action.operation() {
            dae::EventActionOperation::Assert { message, level } => {
                if level.is_some() {
                    return Err(LowerError::unsupported(
                        "assertion levels do not yet have checked Solve lowering",
                        action.provenance().span(),
                    ));
                }
                if condition_clock_owner(view, action.guard()).is_some() {
                    return Err(LowerError::unsupported(
                        "clocked assertions do not yet have checked Solve action scheduling",
                        action.provenance().span(),
                    ));
                }
                push_message_action(
                    view,
                    layout,
                    action,
                    message,
                    solve::SolveEventActionKind::Assert,
                    actions,
                    action_conditions,
                )?;
            }
            dae::EventActionOperation::Terminate { message } => {
                if condition_clock_owner(view, action.guard()).is_some() {
                    return Err(LowerError::unsupported(
                        "clocked termination does not yet have checked Solve action scheduling",
                        action.provenance().span(),
                    ));
                }
                push_message_action(
                    view,
                    layout,
                    action,
                    message,
                    solve::SolveEventActionKind::Terminate,
                    actions,
                    action_conditions,
                )?;
            }
            dae::EventActionOperation::Reinitialize { state, value } => {
                updates.push(EventUpdate {
                    trigger: action.trigger(),
                    guard: action.guard(),
                    variable: state.index(),
                    value,
                    span: action.provenance().span(),
                    clock: condition_clock_owner(view, action.guard()),
                });
            }
        }
    }
    lower_guarded_updates(
        view,
        layout,
        clocks,
        discrete,
        &updates,
        solve::DiscreteRowRole::EventAction,
    )
}

fn push_message_action<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    action: dae::EventActionView<'dae>,
    message: dae::ExprId<'dae>,
    kind: solve::SolveEventActionKind,
    actions: &mut Vec<solve::SolveEventAction>,
    conditions: &mut ScalarRows,
) -> Result<(), LowerError> {
    let span = action.provenance().span();
    let message = lower_message(view, layout, message)?;
    let trigger_memory = condition_memory(layout, action.trigger(), span)?;
    let program = ScalarCompiler::new(view, layout, None).edge_condition_program(
        action.trigger(),
        action.guard(),
        trigger_memory,
        span,
    )?;
    conditions.push(program, span, actions.len());
    actions.push(solve::SolveEventAction {
        kind,
        message,
        span,
        origin: action.provenance().origin().to_string(),
    });
    Ok(())
}

fn lower_message<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    message: dae::ExprId<'dae>,
) -> Result<solve::SolveEventMessage, LowerError> {
    let mut parts = Vec::new();
    lower_message_parts(view, layout, message, &mut parts)?;
    Ok(solve::SolveEventMessage { parts })
}

fn lower_message_parts<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    message: dae::ExprId<'dae>,
    parts: &mut Vec<solve::SolveEventMessagePart>,
) -> Result<(), LowerError> {
    let expression = view
        .expression(message)
        .expect("checked event message expression resolves");
    match expression.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::String(message)) => {
            parts.push(solve::SolveEventMessagePart::Text(message.clone()));
            Ok(())
        }
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Add,
            lhs,
            rhs,
        } if expression.value_type().scalar_type() == dae::ScalarType::String => {
            lower_message_parts(view, layout, lhs, parts)?;
            lower_message_parts(view, layout, rhs, parts)
        }
        dae::ExpressionOperation::StringConversion { value, format, .. } => {
            let source = match view
                .expression(value)
                .expect("checked String conversion value resolves")
                .value_type()
                .scalar_type()
            {
                dae::ScalarType::Real => solve::SolveStringConversionSource::Real,
                dae::ScalarType::Integer => solve::SolveStringConversionSource::Integer,
                dae::ScalarType::Boolean => solve::SolveStringConversionSource::Boolean,
                dae::ScalarType::Enumeration
                | dae::ScalarType::String
                | dae::ScalarType::Record => {
                    unreachable!("checked String conversion has a supported scalar source")
                }
            };
            let value = ScalarCompiler::new(view, layout, None).program(value, 0)?;
            let format = lower_message_format(view, layout, format)?;
            parts.push(solve::SolveEventMessagePart::Conversion {
                value,
                source,
                format,
            });
            Ok(())
        }
        _ => Err(LowerError::unsupported(
            "Solve event messages require String literals, concatenation, or checked String conversions",
            expression.provenance().span(),
        )),
    }
}

fn lower_message_format<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    format: dae::StringConversionFormatView<'dae>,
) -> Result<solve::SolveStringConversionFormat, LowerError> {
    Ok(match format {
        dae::StringConversionFormatView::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } => solve::SolveStringConversionFormat::Options {
            minimum_length: lower_message_option(view, layout, minimum_length)?,
            left_justified: lower_message_option(view, layout, left_justified)?,
            significant_digits: lower_message_option(view, layout, significant_digits)?,
        },
        dae::StringConversionFormatView::Format { value } => {
            let expression = view
                .expression(value)
                .expect("checked String format expression resolves");
            return Err(LowerError::unsupported(
                "explicit String format is not representable in checked Solve event messages",
                expression.provenance().span(),
            ));
        }
    })
}

fn lower_message_option<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    value: Option<dae::ExprId<'dae>>,
) -> Result<Option<Vec<solve::LinearOp>>, LowerError> {
    value
        .map(|value| ScalarCompiler::new(view, layout, None).program(value, 0))
        .transpose()
}

#[derive(Clone, Copy)]
struct EventUpdate<'dae> {
    trigger: dae::ConditionId<'dae>,
    guard: dae::ConditionId<'dae>,
    variable: u32,
    value: dae::ExprId<'dae>,
    span: Span,
    clock: Option<dae::ClockId<'dae>>,
}

type GuardedAssignment<'dae> = (
    dae::ConditionId<'dae>,
    dae::ConditionId<'dae>,
    dae::ExprId<'dae>,
    usize,
    usize,
);

struct GuardedTarget<'dae> {
    target: solve::ScalarSlot,
    span: Span,
    branches: Vec<GuardedAssignment<'dae>>,
    pre_mode: solve::DiscreteEventPreMode,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
}

fn lower_guarded_updates<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
    updates: &[EventUpdate<'dae>],
    role: solve::DiscreteRowRole,
) -> Result<(), LowerError> {
    let mut targets = Vec::<GuardedTarget<'dae>>::new();
    for update in updates {
        let expression = view
            .expression(update.value)
            .expect("checked event update expression resolves");
        let has_pre = expression_contains_pre(view, update.value)
            || condition_contains_pre(view, update.trigger)
            || condition_contains_pre(view, update.guard);
        for scalar in 0..expression
            .value_type()
            .scalar_count()
            .expect("checked event update scalar capacity")
        {
            let target = variable_scalar_slot(layout, update.variable, scalar, update.span)?;
            let clock = update
                .clock
                .map(|clock| clocks.clock(clock).map(|solve| (clock, solve)))
                .transpose()?;
            let trigger_memory = condition_memory(layout, update.trigger, update.span)?;
            let branch = (
                update.trigger,
                update.guard,
                update.value,
                scalar,
                trigger_memory,
            );
            record_guarded_target(&mut targets, target, branch, clock, has_pre, update.span)?;
        }
    }
    for target in targets {
        let program = match target.clock {
            Some((clock, _)) => ScalarCompiler::new(view, layout, None)
                .clocked_guarded_assignments_program(
                    clock,
                    &target.branches,
                    target.target,
                    target.span,
                )?,
            None => ScalarCompiler::new(view, layout, None).guarded_assignments_program(
                &target.branches,
                target.target,
                target.span,
            )?,
        };
        rows.push(
            program,
            target.span,
            target.target,
            role,
            target.pre_mode,
            target.clock.map(|(_, clock)| clock),
        );
    }
    Ok(())
}

fn record_guarded_target<'dae>(
    targets: &mut Vec<GuardedTarget<'dae>>,
    target: solve::ScalarSlot,
    branch: GuardedAssignment<'dae>,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
    has_pre: bool,
    span: Span,
) -> Result<(), LowerError> {
    let Some(group) = targets
        .iter_mut()
        .find(|group| same_target(group.target, target))
    else {
        targets.push(GuardedTarget {
            target,
            span,
            branches: vec![branch],
            pre_mode: event_update_pre_mode(has_pre),
            clock,
        });
        return Ok(());
    };
    if group.clock != clock {
        return Err(LowerError::non_computable(
            "one event target has incompatible clock activation owners",
            span,
        ));
    }
    group.branches.push(branch);
    group.pre_mode =
        event_update_pre_mode(has_pre || group.pre_mode == solve::DiscreteEventPreMode::EventEntry);
    Ok(())
}

fn event_update_pre_mode(has_pre: bool) -> solve::DiscreteEventPreMode {
    if has_pre {
        solve::DiscreteEventPreMode::EventEntry
    } else {
        solve::DiscreteEventPreMode::FollowCurrent
    }
}

fn lower_condition_memory<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
) -> Result<(), LowerError> {
    for index in 0..view.condition_count() {
        let condition = view
            .condition_id(index)
            .expect("dense condition identity resolves");
        let condition_view = view
            .condition(condition)
            .expect("checked condition identity resolves");
        if condition_clock_owner(view, condition).is_some() {
            continue;
        }
        // An `Always` activation is not a `when`: MLS §8.5 gives an internal
        // buffer to an *event generating expression*, and a section that runs
        // because the section runs generates no event. With no buffer its slot
        // stays zero and `edge(c) = c and not pre(c)` reads the level, which is
        // what an unguarded algorithm section and a section-level `assert`
        // mean. The test is the node, not the shape of an expression: a source
        // `when true then` is a real `when` and keeps its buffer, so §8.3.5.1
        // starts that buffer true and it never has an edge to run on.
        if matches!(condition_view.operation(), dae::ConditionOperation::Always) {
            continue;
        }
        let span = condition_view.provenance().span();
        let memory = condition_memory(layout, condition, span)?;
        let target = solve::scalar_slot_p(memory);
        // A condition built from clocked relations is only meaningful on its clock's
        // ticks, and its operands only resolve while that schedule is active.
        let clock = condition_operand_clock(view, clocks, condition, span)?;
        let program = match clock {
            Some((clock, _)) => ScalarCompiler::new(view, layout, None)
                .clocked_condition_program(clock, condition)?,
            None => ScalarCompiler::new(view, layout, None).condition_program(condition)?,
        };
        rows.push(
            program,
            span,
            target,
            solve::DiscreteRowRole::ConditionMemory,
            solve::DiscreteEventPreMode::FollowCurrent,
            clock.map(|(_, solve)| solve),
        );
    }
    Ok(())
}

/// The clock whose partition owns every relation reachable from `condition`.
///
/// Returns `None` for a continuous-time condition. Two different owning clocks would
/// make the condition unschedulable, so that is rejected rather than resolved by
/// picking one.
fn condition_operand_clock<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    condition: dae::ConditionId<'dae>,
    span: Span,
) -> Result<Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>, LowerError> {
    let mut owner: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)> = None;
    let mut conflict = false;
    let mut visit = |found: (dae::ClockId<'dae>, solve::PeriodicClockId)| match owner {
        Some((clock, _)) if clock != found.0 => conflict = true,
        Some(_) => {}
        None => owner = Some(found),
    };
    let mut pending = vec![condition];
    while let Some(current) = pending.pop() {
        let node = view
            .condition(current)
            .expect("checked condition identity resolves");
        match node.operation() {
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked condition relation resolves")
                    .expression();
                if let Some(found) = expression_clock_owner(view, clocks, expression) {
                    visit(found);
                }
            }
            dae::ConditionOperation::Discrete(expression) => {
                if let Some(found) = expression_clock_owner(view, clocks, expression) {
                    visit(found);
                }
            }
            dae::ConditionOperation::Not(operand) => pending.push(operand),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                pending.push(lhs);
                pending.push(rhs);
            }
            dae::ConditionOperation::Initial
            | dae::ConditionOperation::Always
            | dae::ConditionOperation::Clock(_) => {}
        }
    }
    if conflict {
        return Err(LowerError::non_computable(
            "condition mixes relations from different clock partitions",
            span,
        ));
    }
    Ok(owner)
}

fn condition_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
) -> Option<dae::ClockId<'dae>> {
    let condition = view
        .condition(condition)
        .expect("checked condition identity resolves");
    match condition.operation() {
        dae::ConditionOperation::Initial => None,
        dae::ConditionOperation::Clock(clock) => Some(clock),
        dae::ConditionOperation::And(lhs, rhs) => merge_condition_clocks(
            condition_clock_owner(view, lhs),
            condition_clock_owner(view, rhs),
            false,
        ),
        dae::ConditionOperation::Or(lhs, rhs) | dae::ConditionOperation::AnyRise(lhs, rhs) => {
            merge_condition_clocks(
                condition_clock_owner(view, lhs),
                condition_clock_owner(view, rhs),
                true,
            )
        }
        dae::ConditionOperation::Always
        | dae::ConditionOperation::Relation(_)
        | dae::ConditionOperation::Discrete(_)
        | dae::ConditionOperation::Not(_) => None,
    }
}

fn merge_condition_clocks<'dae>(
    lhs: Option<dae::ClockId<'dae>>,
    rhs: Option<dae::ClockId<'dae>>,
    disjunction: bool,
) -> Option<dae::ClockId<'dae>> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
        (Some(clock), None) | (None, Some(clock)) if !disjunction => Some(clock),
        _ => None,
    }
}

pub(in crate::lower) fn condition_memory(
    layout: &LoweredLayout<'_>,
    condition: dae::ConditionId<'_>,
    span: Span,
) -> Result<usize, LowerError> {
    layout
        .condition_memory
        .get(condition.index() as usize)
        .copied()
        .ok_or_else(|| LowerError::contract("condition has no Solve memory slot", span))
}

fn same_target(lhs: solve::ScalarSlot, rhs: solve::ScalarSlot) -> bool {
    matches!(
        (lhs, rhs),
        (
            solve::ScalarSlot::Y {
                index: lhs_index,
                ..
            },
            solve::ScalarSlot::Y {
                index: rhs_index,
                ..
            }
        ) | (
            solve::ScalarSlot::P {
                index: lhs_index,
                ..
            },
            solve::ScalarSlot::P {
                index: rhs_index,
                ..
            }
        ) if lhs_index == rhs_index
    )
}

struct LoweredRoots {
    programs: solve::ScalarProgramBlock,
    zero_domains: Vec<solve::RootZeroDomain>,
    relation_memory_targets: Vec<Option<solve::ScalarSlot>>,
}

fn lower_roots<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    relation_memory_owners: &RelationMemoryOwners<'dae>,
) -> Result<LoweredRoots, LowerError> {
    let mut rows = ScalarRows::default();
    let mut zero_domains = Vec::with_capacity(view.root_count());
    let mut relation_memory_targets = Vec::with_capacity(view.root_count());
    for index in 0..view.root_count() {
        let id = view.root_id(index).expect("dense root identity resolves");
        let root = view.root(id).expect("checked root identity resolves");
        let relation = view
            .relation(root.relation())
            .expect("checked root relation resolves");
        if expression_clock_owner(view, clocks, relation.expression()).is_some() {
            continue;
        }
        rows.push(
            ScalarCompiler::new(view, layout, None).root_program(root.relation())?,
            root.provenance().span(),
            zero_domains.len(),
        );
        zero_domains.push(root_zero_domain(view, relation.expression()));
        relation_memory_targets.push(relation_memory_owners.target(root.relation()));
    }
    Ok(LoweredRoots {
        programs: rows.into_scalar_block()?,
        zero_domains,
        relation_memory_targets,
    })
}

/// The clock whose partition an expression's operands belong to, if any.
///
/// MLS §16.5 confines `previous(x)` and every clock-owned declaration to their owning
/// partition, so an operand that names one is decisive: the expression is evaluated on
/// that clock's ticks and nowhere else. That is what keeps a clocked relation out of the
/// continuous root set (MLS §16.8.1 raises no state event for it — the tick already is
/// the event) and what tells the condition-memory row which schedule to compile under.
///
/// The first owner found wins; a clocked partition cannot mix clocks, and the callers
/// that must reject a mix check for it explicitly.
fn expression_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<(dae::ClockId<'dae>, solve::PeriodicClockId)> {
    let mut owner = None;
    dae::for_each_expression(view, expression, |_, node| {
        if owner.is_some() {
            return;
        }
        let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
            return;
        };
        if let dae::CoordinateView::Previous(previous) = coordinate {
            let clock = view
                .previous(previous)
                .expect("checked previous identity resolves")
                .clock();
            let solve = clocks
                .clock(clock)
                .expect("checked previous history names a lowered clock");
            owner = Some((clock, solve));
            return;
        }
        owner = super::coordinate_variable(coordinate)
            .or_else(|| super::pre_coordinate_variable(coordinate))
            .and_then(|index| view.variable_id(index as usize))
            .and_then(|id| clocks.variable_owner(id));
    });
    owner
}

fn root_zero_domain<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> solve::RootZeroDomain {
    let operation = view
        .expression(expression)
        .expect("checked relation expression resolves")
        .operation();
    match operation {
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::LessEqual | dae::BinaryOperator::GreaterEqual,
            ..
        } => solve::RootZeroDomain::NonPositive,
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Less | dae::BinaryOperator::Greater,
            ..
        } => solve::RootZeroDomain::Positive,
        _ => solve::RootZeroDomain::Previous,
    }
}

fn lower_time_events<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
) -> Result<(Vec<f64>, solve::ScalarProgramBlock), LowerError> {
    let mut scheduled = Vec::new();
    let mut dynamic = ScalarRows::default();
    for index in 0..view.time_event_count() {
        let id = view
            .time_event_id(index)
            .expect("dense time event identity resolves");
        let event = view
            .time_event(id)
            .expect("checked time event identity resolves");
        match event.operation() {
            dae::TimeEventOperation::Static(instant) => scheduled.push(instant.to_f64()),
            dae::TimeEventOperation::Dynamic(deadline) => dynamic.push(
                ScalarCompiler::new(view, layout, None).program(deadline, 0)?,
                event.provenance().span(),
                dynamic.len(),
            ),
        }
    }
    Ok((scheduled, dynamic.into_scalar_block()?))
}

fn expression_pre_mode<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    sampled: bool,
) -> solve::DiscreteEventPreMode {
    if sampled || expression_contains_pre(view, expression) {
        solve::DiscreteEventPreMode::EventEntry
    } else {
        solve::DiscreteEventPreMode::FollowCurrent
    }
}

fn condition_contains_pre<'dae>(view: dae::DaeView<'dae>, root: dae::ConditionId<'dae>) -> bool {
    let mut pending = vec![root];
    let mut visited = vec![false; view.condition_count()];
    while let Some(condition) = pending.pop() {
        let index = condition.index() as usize;
        if visited[index] {
            continue;
        }
        visited[index] = true;
        let condition = view
            .condition(condition)
            .expect("checked condition identity resolves");
        match condition.operation() {
            dae::ConditionOperation::Initial => {}
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked relation identity resolves")
                    .expression();
                if expression_contains_pre(view, expression) {
                    return true;
                }
            }
            dae::ConditionOperation::Discrete(expression) => {
                if expression_contains_pre(view, expression) {
                    return true;
                }
            }
            dae::ConditionOperation::Clock(_) | dae::ConditionOperation::Always => {}
            dae::ConditionOperation::Not(operand) => pending.push(operand),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                pending.push(rhs);
                pending.push(lhs);
            }
        }
    }
    false
}

fn expression_contains_pre<'dae>(view: dae::DaeView<'dae>, root: dae::ExprId<'dae>) -> bool {
    let mut found = false;
    dae::for_each_expression(view, root, |_, expression| {
        found |= matches!(
            expression.operation(),
            dae::ExpressionOperation::Coordinate(
                dae::CoordinateView::PreDiscreteReal(_)
                    | dae::CoordinateView::PreDiscreteValue(_)
                    | dae::CoordinateView::PreState(_)
                    | dae::CoordinateView::PreAlgebraic(_)
                    | dae::CoordinateView::Previous(_)
            )
        );
    });
    found
}
