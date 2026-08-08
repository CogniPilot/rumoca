use std::collections::{BTreeMap, BTreeSet};

use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::clocks::LoweredClocks;
use super::{
    LoweredLayout, ScalarCompiler, ScalarRows, delay_value_scalar_slot, variable_scalar_slot,
};
use crate::LowerError;

mod integrator_history;
pub(super) mod structured;

use integrator_history::{
    HistoryDependencySlot, collect_linear_op_dependencies, derive_integrator_history_effects,
    history_dependency_slot,
};
use structured::lower_discrete_value_owners;

pub(super) fn lower_discrete_and_events<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    continuous: &solve::ContinuousSolveSystem,
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
    let event_iteration_plan = build_event_iteration_plan(view, layout, &discrete)?;
    let mut discrete = discrete.finish(
        &roots.relation_memory_targets,
        &layout.solve_layout.relation_memory_parameter_indices,
    )?;
    discrete.event_iteration_plan = event_iteration_plan;
    derive_integrator_history_effects(
        &mut discrete,
        continuous,
        layout.solve_layout.state_scalar_count,
    );
    let root_relation_refresh_roles = solve::derive_root_relation_refresh_roles(
        &roots.programs,
        &discrete.runtime_assignment_rhs,
        &discrete.runtime_assignment_targets,
        layout.solve_layout.state_scalar_count,
        layout.solve_layout.solver_scalar_count(),
    )?;
    let events = solve::SolveEventPartition {
        root_conditions: roots.programs,
        root_relation_memory_targets: roots.relation_memory_targets,
        root_zero_domains: roots.zero_domains,
        root_relation_refresh_roles,
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

fn build_event_iteration_plan<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    discrete: &DiscreteRows<'dae>,
) -> Result<solve::EventIterationPlan, LowerError> {
    let mut runs = Vec::new();
    for (id, variable) in view.variables().filter(|(_, variable)| {
        variable.causality() != dae::VariableCausality::Input
            && matches!(
                variable.role(),
                dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue
            )
    }) {
        let scalar_count = variable.scalar_count();
        if scalar_count == 0 {
            continue;
        }
        let span = variable.declaration().span();
        let current_base = variable_scalar_slot(layout, id.index(), 0, span)?;
        let solve::ScalarSlot::P {
            index: current_p_base,
            ..
        } = current_base
        else {
            return Err(LowerError::contract(
                "event-iteration current lane is not P-backed",
                span,
            ));
        };
        let pre_p_base = layout
            .pre_variables
            .get(id.index() as usize)
            .copied()
            .flatten()
            .ok_or_else(|| {
                LowerError::contract("event-iteration run has no ordinary pre lane", span)
            })?;
        if layout.solve_layout.variable_storage_runs[id.index() as usize]
            .event_iteration_kind()
            .is_none()
        {
            return Err(LowerError::non_computable(
                "discrete event-iteration value is not runtime-representable",
                span,
            ));
        }
        let pre_binding_start = layout
            .pre_binding_starts
            .get(id.index() as usize)
            .copied()
            .flatten()
            .ok_or_else(|| {
                LowerError::contract(
                    "event-iteration run has no exact ordinary pre binding",
                    span,
                )
            })?;
        for scalar in 0..scalar_count {
            let binding = layout
                .solve_layout
                .pre_param_bindings
                .get(pre_binding_start + scalar)
                .ok_or_else(|| {
                    LowerError::contract("event-iteration binding run overflow", span)
                })?;
            if binding.dest_p_index != pre_p_base + scalar
                || binding.clock_schedule.is_some()
                || !matches!(binding.source, solve::PreParamSource::P { index } if index == current_p_base + scalar)
            {
                return Err(LowerError::contract(
                    "event-iteration run does not own one contiguous ordinary pre-binding slice",
                    span,
                ));
            }
        }
        let owner = discrete.event_iteration_owner(id, scalar_count, span)?;
        runs.push(solve::EventIterationRun {
            variable: id.index() as usize,
            pre_binding_start,
            owner,
        });
    }
    runs.sort_by_key(|run| run.pre_binding_start);
    Ok(solve::EventIterationPlan { runs })
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
    runtime_rows: ScalarRows,
    runtime_targets: Vec<solve::ScalarSlot>,
    post_commit_rows: ScalarRows,
    post_commit_targets: Vec<solve::ScalarSlot>,
    root_refresh_candidates: Vec<RootRefreshCandidate>,
    rows: ScalarRows,
    targets: Vec<solve::ScalarSlot>,
    roles: Vec<solve::DiscreteRowRole>,
    pre_modes: Vec<solve::DiscreteEventPreMode>,
    clock_owners: Vec<Option<solve::PeriodicClockId>>,
    structured_rhs: solve::ComputeBlock,
    structured_updates: Vec<solve::StructuredDiscreteUpdate>,
    structured_output_cursor: usize,
    relation_memory_owners: RelationMemoryOwners<'dae>,
    event_iteration_owners: Vec<Option<EventIterationOwnerClaim>>,
}

#[derive(Clone, Copy)]
enum EventIterationOwnerClaim {
    ScalarRows {
        start_row: usize,
        base: solve::ScalarSlot,
        claimed: usize,
    },
    StructuredUpdate {
        update_index: usize,
    },
}

impl<'dae> DiscreteRows<'dae> {
    fn new(view: dae::DaeView<'dae>) -> Self {
        Self {
            relation_memory_owners: RelationMemoryOwners::new(view),
            event_iteration_owners: vec![None; view.variable_count()],
            ..Self::default()
        }
    }

    fn claim_scalar_event_owner(
        &mut self,
        variable: dae::VariableId<'dae>,
        target: solve::ScalarSlot,
        span: Span,
    ) -> Result<(), LowerError> {
        let variable_index = variable.index() as usize;
        let row = self.targets.len();
        let claim = self
            .event_iteration_owners
            .get_mut(variable_index)
            .ok_or_else(|| LowerError::contract("event owner variable is out of bounds", span))?;
        match claim {
            None => {
                *claim = Some(EventIterationOwnerClaim::ScalarRows {
                    start_row: row,
                    base: target,
                    claimed: 1,
                });
            }
            Some(EventIterationOwnerClaim::ScalarRows {
                start_row,
                base,
                claimed,
            }) => {
                let expected_row = start_row.checked_add(*claimed).ok_or_else(|| {
                    LowerError::contract("event owner scalar-row range overflow", span)
                })?;
                let expected_target = offset_runtime_slot(*base, *claimed, span)?;
                if row != expected_row || target != expected_target {
                    return Err(LowerError::contract(
                        "event owner scalar rows are not one contiguous target run",
                        span,
                    ));
                }
                *claimed += 1;
            }
            Some(EventIterationOwnerClaim::StructuredUpdate { .. }) => {
                return Err(LowerError::contract(
                    "event coordinate has both scalar and structured owners",
                    span,
                ));
            }
        }
        Ok(())
    }

    fn claim_structured_event_owner(
        &mut self,
        variable: dae::VariableId<'dae>,
        update_index: usize,
        span: Span,
    ) -> Result<(), LowerError> {
        let claim = self
            .event_iteration_owners
            .get_mut(variable.index() as usize)
            .ok_or_else(|| LowerError::contract("event owner variable is out of bounds", span))?;
        if claim.is_some() {
            return Err(LowerError::contract(
                "event coordinate has more than one producer owner",
                span,
            ));
        }
        *claim = Some(EventIterationOwnerClaim::StructuredUpdate { update_index });
        Ok(())
    }

    fn event_iteration_owner(
        &self,
        variable: dae::VariableId<'dae>,
        scalar_count: usize,
        span: Span,
    ) -> Result<solve::EventIterationOwner, LowerError> {
        match self
            .event_iteration_owners
            .get(variable.index() as usize)
            .copied()
            .flatten()
        {
            None => Ok(solve::EventIterationOwner::Hold),
            Some(EventIterationOwnerClaim::ScalarRows {
                start_row, claimed, ..
            }) if claimed == scalar_count => {
                Ok(solve::EventIterationOwner::ScalarRows { start_row })
            }
            Some(EventIterationOwnerClaim::ScalarRows { .. }) => Err(LowerError::contract(
                "event owner does not cover the complete variable",
                span,
            )),
            Some(EventIterationOwnerClaim::StructuredUpdate { update_index }) => {
                Ok(solve::EventIterationOwner::StructuredUpdate { update_index })
            }
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

    fn push_root_refresh_candidate(
        &mut self,
        program: Vec<solve::LinearOp>,
        span: Span,
        target: solve::ScalarSlot,
    ) {
        self.root_refresh_candidates.push(RootRefreshCandidate {
            program,
            span,
            target,
        });
    }

    fn finish(
        mut self,
        root_relation_targets: &[Option<solve::ScalarSlot>],
        relation_memory_parameter_indices: &[usize],
    ) -> Result<solve::DiscreteSolveSystem, LowerError> {
        self.partition_root_relation_refresh(root_relation_targets);
        let runtime_assignment_rhs = self.runtime_rows.into_scalar_block()?;
        let runtime_assignment_roles = solve::derive_runtime_assignment_roles(
            &runtime_assignment_rhs,
            &self.runtime_targets,
            relation_memory_parameter_indices,
        )?;
        let root_reachable = solve::derive_root_reachable_runtime_rows(
            &runtime_assignment_rhs,
            &self.runtime_targets,
            root_relation_targets,
            &runtime_assignment_roles,
        )?;
        let mut post_commit_assignment_runtime_rows = Vec::new();
        for (runtime_row, (role, reachable)) in runtime_assignment_roles
            .iter()
            .zip(root_reachable)
            .enumerate()
        {
            if *role != solve::RuntimeAssignmentRole::RelationFree || !reachable {
                continue;
            }
            let output = self.post_commit_targets.len();
            self.post_commit_rows.push(
                runtime_assignment_rhs
                    .program(runtime_row)
                    .expect("runtime assignment certificate is row-aligned")
                    .to_vec(),
                runtime_assignment_rhs
                    .program_span(runtime_row)
                    .expect("runtime assignment provenance is checked"),
                output,
            );
            self.post_commit_targets
                .push(self.runtime_targets[runtime_row]);
            post_commit_assignment_runtime_rows.push(runtime_row);
        }
        let post_commit_assignment_rhs = self.post_commit_rows.into_scalar_block()?;
        let rhs = self.rows.into_scalar_block()?;
        Ok(solve::DiscreteSolveSystem {
            event_iteration_plan: solve::EventIterationPlan::default(),
            runtime_assignment_rhs,
            runtime_assignment_targets: self.runtime_targets,
            runtime_assignment_roles,
            post_commit_assignment_rhs,
            post_commit_assignment_targets: self.post_commit_targets,
            post_commit_assignment_runtime_rows,
            update_targets: self.targets,
            row_roles: self.roles,
            pre_modes: self.pre_modes,
            observation_refresh: vec![false; rhs.programs().len()],
            integrator_history_effects: vec![
                solve::IntegratorHistoryEffect::Restart;
                rhs.programs().len()
            ],
            clock_owners: self.clock_owners,
            structured_rhs: self.structured_rhs,
            structured_updates: self.structured_updates,
            rhs,
        })
    }

    /// Derive the runtime refresh plan for combinational owners fed by root
    /// relation memory.
    ///
    /// The source event rows remain authoritative and retain their original
    /// order. The event-iteration plan admits every unconditional, unclocked
    /// `FollowCurrent` owner in the transitive scalar dependency closure of an
    /// aligned root-memory target. The post-commit plan is constructed from the
    /// same typed candidates. The checked post-commit projection is derived
    /// later from the finalized runtime plan and its certified relation roles.
    /// Dependency forms that the compact proof cannot read remain event-only.
    fn partition_root_relation_refresh(
        &mut self,
        root_relation_targets: &[Option<solve::ScalarSlot>],
    ) {
        let candidates = std::mem::take(&mut self.root_refresh_candidates);
        let root_reachable = root_relation_targets
            .iter()
            .flatten()
            .copied()
            .filter_map(history_dependency_slot)
            .collect::<BTreeSet<_>>();
        let selected = select_root_refresh_candidates(&candidates, root_reachable);
        for (candidate, selected) in candidates.into_iter().zip(selected) {
            if selected {
                let output = self.runtime_targets.len();
                self.runtime_rows
                    .push(candidate.program, candidate.span, output);
                self.runtime_targets.push(candidate.target);
            }
        }
    }
}

fn offset_runtime_slot(
    base: solve::ScalarSlot,
    offset: usize,
    span: Span,
) -> Result<solve::ScalarSlot, LowerError> {
    match base {
        solve::ScalarSlot::P { index, .. } => index
            .checked_add(offset)
            .map(solve::scalar_slot_p)
            .ok_or_else(|| LowerError::contract("event owner P range overflow", span)),
        solve::ScalarSlot::Y { index, .. } => index
            .checked_add(offset)
            .map(solve::scalar_slot_y)
            .ok_or_else(|| LowerError::contract("event owner Y range overflow", span)),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => Err(LowerError::contract(
            "event owner is not runtime-backed",
            span,
        )),
    }
}

fn select_root_refresh_candidates(
    candidates: &[RootRefreshCandidate],
    mut reachable: BTreeSet<HistoryDependencySlot>,
) -> Vec<bool> {
    let mut selected = vec![false; candidates.len()];
    loop {
        let mut progress = false;
        for (index, candidate) in candidates.iter().enumerate() {
            if selected[index] {
                continue;
            }
            let mut dependencies = BTreeSet::new();
            if collect_linear_op_dependencies(&candidate.program, &mut dependencies).is_none()
                || dependencies.is_disjoint(&reachable)
            {
                continue;
            }
            let Some(target) = history_dependency_slot(candidate.target) else {
                continue;
            };
            selected[index] = true;
            progress |= reachable.insert(target);
        }
        if !progress {
            break;
        }
    }
    selected
}

struct RootRefreshCandidate {
    program: Vec<solve::LinearOp>,
    span: Span,
    target: solve::ScalarSlot,
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
                    variable,
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
        rows.claim_scalar_event_owner(variable, target, span)?;
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
pub(super) fn resolve_discrete_real_definitions<'dae>(
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
                    variable: dae::VariableId::from(state),
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
    let compiler = ScalarCompiler::new(view, layout, None);
    let program = match condition_clock_owner(view, action.guard()) {
        Some(clock) => compiler.clocked_action_condition_program(clock, action.guard(), span)?,
        None => {
            let trigger_memory = condition_memory(layout, action.trigger(), span)?;
            compiler.edge_condition_program(
                action.trigger(),
                action.guard(),
                trigger_memory,
                span,
            )?
        }
    };
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
    variable: dae::VariableId<'dae>,
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
    variable: dae::VariableId<'dae>,
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
        let pre_mode = merge_pre_mode(
            expression_pre_mode(view, update.value, false),
            merge_pre_mode(
                condition_pre_mode(view, update.trigger),
                condition_pre_mode(view, update.guard),
            ),
        );
        for scalar in 0..expression
            .value_type()
            .scalar_count()
            .expect("checked event update scalar capacity")
        {
            let target =
                variable_scalar_slot(layout, update.variable.index(), scalar, update.span)?;
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
            record_guarded_target(
                &mut targets,
                update.variable,
                target,
                branch,
                clock,
                pre_mode,
                update.span,
            )?;
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
        rows.claim_scalar_event_owner(target.variable, target.target, target.span)?;
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
    variable: dae::VariableId<'dae>,
    target: solve::ScalarSlot,
    branch: GuardedAssignment<'dae>,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
    pre_mode: solve::DiscreteEventPreMode,
    span: Span,
) -> Result<(), LowerError> {
    let Some(group) = targets
        .iter_mut()
        .find(|group| same_target(group.target, target))
    else {
        targets.push(GuardedTarget {
            variable,
            target,
            span,
            branches: vec![branch],
            pre_mode,
            clock,
        });
        return Ok(());
    };
    if group.variable != variable {
        return Err(LowerError::contract(
            "one guarded storage target has multiple variable identities",
            span,
        ));
    }
    if group.clock != clock {
        return Err(LowerError::non_computable(
            "one event target has incompatible clock activation owners",
            span,
        ));
    }
    group.branches.push(branch);
    group.pre_mode = merge_pre_mode(group.pre_mode, pre_mode);
    Ok(())
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
    lower_structured_roots(
        view,
        layout,
        clocks,
        &mut rows,
        &mut zero_domains,
        &mut relation_memory_targets,
    )?;
    Ok(LoweredRoots {
        programs: rows.into_scalar_block()?,
        zero_domains,
        relation_memory_targets,
    })
}

fn lower_structured_roots<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut ScalarRows,
    zero_domains: &mut Vec<solve::RootZeroDomain>,
    relation_memory_targets: &mut Vec<Option<solve::ScalarSlot>>,
) -> Result<(), LowerError> {
    for index in 0..view.structured_root_count() {
        let id = view
            .structured_root_id(index)
            .expect("dense structured-root identity resolves");
        let root = view
            .structured_root(id)
            .expect("checked structured-root identity resolves");
        if expression_clock_owner(view, clocks, root.expression()).is_some() {
            return Err(LowerError::non_computable(
                "clocked structured root families are not continuously monitored",
                root.provenance().span(),
            ));
        }
        let domain = view
            .domain(root.domain())
            .expect("checked structured-root domain resolves")
            .structured();
        let points = domain.index_tuples().map_err(|error| {
            LowerError::contract(
                format!("structured root domain is invalid: {error}"),
                root.provenance().span(),
            )
        })?;
        for point in points {
            let row = zero_domains.len();
            rows.push(
                ScalarCompiler::new(view, layout, Some((root.domain(), &point)))
                    .root_expression_program(root.expression(), root.provenance().span())?,
                root.provenance().span(),
                row,
            );
            zero_domains.push(root_zero_domain(view, root.expression()));
            relation_memory_targets.push(None);
        }
    }
    Ok(())
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
    if sampled {
        return solve::DiscreteEventPreMode::EventEntry;
    }
    let mut mode = solve::DiscreteEventPreMode::FollowCurrent;
    dae::for_each_expression(view, expression, |_, expression| {
        let found = match expression.operation() {
            dae::ExpressionOperation::Coordinate(
                dae::CoordinateView::PreDiscreteReal(_) | dae::CoordinateView::PreDiscreteValue(_),
            ) => solve::DiscreteEventPreMode::Fixed,
            dae::ExpressionOperation::Coordinate(
                dae::CoordinateView::PreState(_)
                | dae::CoordinateView::PreAlgebraic(_)
                | dae::CoordinateView::Previous(_),
            ) => solve::DiscreteEventPreMode::EventEntry,
            _ => solve::DiscreteEventPreMode::FollowCurrent,
        };
        mode = merge_pre_mode(mode, found);
    });
    mode
}

fn condition_pre_mode<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
) -> solve::DiscreteEventPreMode {
    let mut pending = vec![root];
    let mut visited = vec![false; view.condition_count()];
    let mut mode = solve::DiscreteEventPreMode::FollowCurrent;
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
                mode = merge_pre_mode(mode, expression_pre_mode(view, expression, false));
            }
            dae::ConditionOperation::Discrete(expression) => {
                mode = merge_pre_mode(mode, expression_pre_mode(view, expression, false));
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
    mode
}

fn merge_pre_mode(
    lhs: solve::DiscreteEventPreMode,
    rhs: solve::DiscreteEventPreMode,
) -> solve::DiscreteEventPreMode {
    match (lhs, rhs) {
        (solve::DiscreteEventPreMode::EventEntry, _)
        | (_, solve::DiscreteEventPreMode::EventEntry) => solve::DiscreteEventPreMode::EventEntry,
        (solve::DiscreteEventPreMode::Fixed, _) | (_, solve::DiscreteEventPreMode::Fixed) => {
            solve::DiscreteEventPreMode::Fixed
        }
        (
            solve::DiscreteEventPreMode::FollowCurrent,
            solve::DiscreteEventPreMode::FollowCurrent,
        ) => solve::DiscreteEventPreMode::FollowCurrent,
    }
}

#[cfg(test)]
mod integrator_history_effect_tests {
    use super::*;

    fn provenance() -> rumoca_core::ProvenanceSpan {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("integrator_history_effect.mo"),
            0,
            1,
        )
        .require_provenance("integrator history effect fixture")
        .unwrap()
    }

    fn load_program(slot: solve::ScalarSlot) -> Vec<solve::LinearOp> {
        let load = match slot {
            solve::ScalarSlot::Y { index, .. } => solve::LinearOp::LoadY { dst: 0, index },
            solve::ScalarSlot::P { index, .. } => solve::LinearOp::LoadP { dst: 0, index },
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => unreachable!(),
        };
        vec![load, solve::LinearOp::StoreOutput { src: 0 }]
    }

    fn scalar_block(programs: Vec<Vec<solve::LinearOp>>) -> solve::ScalarProgramBlock {
        solve::ScalarProgramBlock::with_source_span(programs, provenance()).unwrap()
    }

    fn compute_block(programs: Vec<Vec<solve::LinearOp>>) -> solve::ComputeBlock {
        solve::ComputeBlock::from_scalar_program_block(scalar_block(programs))
    }

    fn discrete_with_targets(targets: Vec<solve::ScalarSlot>) -> solve::DiscreteSolveSystem {
        let row_count = targets.len();
        solve::DiscreteSolveSystem {
            rhs: scalar_block(
                targets
                    .iter()
                    .map(|_| {
                        vec![
                            solve::LinearOp::Const { dst: 0, value: 0.0 },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ]
                    })
                    .collect(),
            ),
            update_targets: targets,
            row_roles: vec![solve::DiscreteRowRole::Equation; row_count],
            pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent; row_count],
            observation_refresh: vec![false; row_count],
            integrator_history_effects: vec![solve::IntegratorHistoryEffect::Restart; row_count],
            clock_owners: vec![None; row_count],
            ..solve::DiscreteSolveSystem::default()
        }
    }

    #[test]
    fn direct_continuous_dependencies_and_state_targets_restart() {
        let continuous = solve::ContinuousSolveSystem {
            derivative_rhs: compute_block(vec![load_program(solve::scalar_slot_p(0))]),
            ..Default::default()
        };
        let mut discrete = discrete_with_targets(vec![
            solve::scalar_slot_p(0),
            solve::scalar_slot_p(1),
            solve::scalar_slot_y(0),
        ]);

        derive_integrator_history_effects(&mut discrete, &continuous, 1);

        assert_eq!(
            discrete.integrator_history_effects,
            [
                solve::IntegratorHistoryEffect::Restart,
                solve::IntegratorHistoryEffect::Preserve,
                solve::IntegratorHistoryEffect::Restart,
            ]
        );
    }

    #[test]
    fn runtime_assignment_dependencies_propagate_transitively() {
        let continuous = solve::ContinuousSolveSystem {
            derivative_rhs: compute_block(vec![load_program(solve::scalar_slot_p(0))]),
            ..Default::default()
        };
        let mut discrete = discrete_with_targets(vec![solve::scalar_slot_p(1)]);
        discrete.runtime_assignment_rhs = scalar_block(vec![load_program(solve::scalar_slot_p(1))]);
        discrete.runtime_assignment_targets = vec![solve::scalar_slot_p(0)];

        derive_integrator_history_effects(&mut discrete, &continuous, 0);

        assert_eq!(
            discrete.integrator_history_effects,
            [solve::IntegratorHistoryEffect::Restart]
        );
    }

    #[test]
    fn disconnected_runtime_assignment_cycles_fail_closed() {
        let continuous = solve::ContinuousSolveSystem::default();
        let mut discrete =
            discrete_with_targets(vec![solve::scalar_slot_p(2), solve::scalar_slot_p(4)]);
        discrete.runtime_assignment_rhs = scalar_block(vec![
            load_program(solve::scalar_slot_p(3)),
            load_program(solve::scalar_slot_p(2)),
        ]);
        discrete.runtime_assignment_targets =
            vec![solve::scalar_slot_p(2), solve::scalar_slot_p(3)];

        derive_integrator_history_effects(&mut discrete, &continuous, 0);

        assert_eq!(
            discrete.integrator_history_effects,
            [
                solve::IntegratorHistoryEffect::Restart,
                solve::IntegratorHistoryEffect::Preserve,
            ]
        );
    }
}
