use super::*;

/// The activation of a section that is not a `when`.
///
/// An unguarded algorithm section and an `assert` in an equation,
/// initial-equation, or initial-algorithm section both run because the section
/// runs — MLS §8.3.7 violates an assertion because its condition *is* false, not
/// because it became false — so their activation is a level.
///
/// This is [`dae::ConditionInput::Always`] and deliberately *not* a literal
/// `true`. A model author's `when true then` is a real `when`: §8.3.5.1 starts
/// its activation buffer at the condition's own value and §8.6 holds
/// `pre(b) = b` before integration, so it has no rising edge and never runs.
/// Lowering the two to the same node made "is this a `when`?" a question about
/// expression shape, and answered it wrong for every source `when <literal>`.
pub(super) fn always_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    owner_span: Span,
) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, owner_span)?;
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(condition, dae::ConditionInput::Always, provenance)
    })?;
    Ok(condition)
}

#[derive(Clone, Copy)]
pub(super) struct WhenChainsRequest<'input, 'shape, 'dae> {
    coordinates: &'input HashMap<VarName, Coordinate<'dae>>,
    functions: &'input FunctionRegistry<'shape, 'dae>,
    sample_lattices: &'input [(Span, ClockLattice)],
    clocks: &'input LoweredClocks<'dae>,
    chains: &'input [flat::WhenChain],
    topology: &'input DiscreteValueTopologyPlan,
    when_owners: &'input HashMap<Span, ClockPlan>,
}

impl<'input, 'shape, 'dae> WhenChainsRequest<'input, 'shape, 'dae> {
    pub(super) const fn new(
        coordinates: &'input HashMap<VarName, Coordinate<'dae>>,
        functions: &'input FunctionRegistry<'shape, 'dae>,
        sample_lattices: &'input [(Span, ClockLattice)],
        clocks: &'input LoweredClocks<'dae>,
        chains: &'input [flat::WhenChain],
        topology: &'input DiscreteValueTopologyPlan,
        when_owners: &'input HashMap<Span, ClockPlan>,
    ) -> Self {
        Self {
            coordinates,
            functions,
            sample_lattices,
            clocks,
            chains,
            topology,
            when_owners,
        }
    }
}

struct WhenLowering<'work, 'input, 'shape, 'dae> {
    construction: &'work mut dae::DaeConstruction<'dae>,
    discrete_values: &'work mut DiscreteValueStaging<'dae>,
    request: WhenChainsRequest<'input, 'shape, 'dae>,
    target_owners: WhenTargetOwners,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct WhenSourceOwner(u32);

#[derive(Clone, Copy)]
struct WhenSemanticOwners {
    source: WhenSourceOwner,
    discrete_value: Option<DiscreteValueOwnerHandle>,
}

#[derive(Default)]
struct WhenTargetOwners {
    states: Vec<Option<WhenSourceOwner>>,
    discrete_reals: Vec<Option<WhenSourceOwner>>,
}

impl WhenTargetOwners {
    fn claim_state(
        &mut self,
        target: dae::StateId<'_>,
        owner: WhenSourceOwner,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        claim_when_target(
            &mut self.states,
            target.index(),
            owner,
            "reinitialization owner",
            span,
        )
    }

    fn claim_discrete_real(
        &mut self,
        target: dae::DiscreteRealId<'_>,
        owner: WhenSourceOwner,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        claim_when_target(
            &mut self.discrete_reals,
            target.index(),
            owner,
            "discrete Real when owner",
            span,
        )
    }
}

fn claim_when_target(
    targets: &mut Vec<Option<WhenSourceOwner>>,
    target: u32,
    owner: WhenSourceOwner,
    kind: &'static str,
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let index = target as usize;
    if targets.len() <= index {
        targets.resize(index + 1, None);
    }
    match targets[index] {
        Some(existing) if existing != owner => {
            Err(dae::DaeConstructionError::DuplicateDefinition {
                kind,
                index: target,
                span,
            })
        }
        Some(_) => Ok(()),
        None => {
            targets[index] = Some(owner);
            Ok(())
        }
    }
}

pub(super) fn lower_when_chains<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    request: WhenChainsRequest<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let chains = request.chains;
    let mut lowering = WhenLowering {
        construction,
        discrete_values,
        request,
        target_owners: WhenTargetOwners::default(),
    };
    for (index, chain) in chains.iter().enumerate() {
        let source_owner =
            WhenSourceOwner(u32::try_from(index).expect("Flat when-owner count fits in u32"));
        lowering.lower_chain(source_owner, chain)?;
    }
    Ok(())
}

fn when_chain_discrete_value_targets<'dae>(
    chain: &flat::WhenChain,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
) -> Vec<VarName> {
    let mut targets = HashSet::new();
    for branch in chain.branches() {
        collect_when_targets(&branch.equations, &mut targets);
    }
    let mut targets = targets
        .into_iter()
        .filter(|target| matches!(coordinates[target], Coordinate::DiscreteValue(_)))
        .collect::<Vec<_>>();
    targets.sort_by_key(|target| match coordinates[target] {
        Coordinate::DiscreteValue(target) => target.index(),
        _ => unreachable!("targets are filtered to discrete-value coordinates"),
    });
    targets
}

fn collect_when_targets(equations: &[flat::WhenEquation], targets: &mut HashSet<VarName>) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { target, .. } => {
                targets.insert(target.clone());
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => collect_conditional_targets(branches, else_branch.as_deref(), targets),
            flat::WhenEquation::Reinit { .. }
            | flat::WhenEquation::Assert { .. }
            | flat::WhenEquation::Terminate { .. }
            | flat::WhenEquation::FunctionCallOutputs { .. } => {}
        }
    }
}

fn collect_conditional_targets(
    branches: &[(Expression, Vec<flat::WhenEquation>)],
    else_branch: Option<&[flat::WhenEquation]>,
    targets: &mut HashSet<VarName>,
) {
    for (_, equations) in branches {
        collect_when_targets(equations, targets);
    }
    if let Some(equations) = else_branch {
        collect_when_targets(equations, targets);
    }
}

impl<'shape, 'dae> WhenLowering<'_, '_, 'shape, 'dae> {
    fn lower_chain(
        &mut self,
        source_owner: WhenSourceOwner,
        chain: &flat::WhenChain,
    ) -> Result<(), dae::DaeConstructionError> {
        let owner_provenance = dae::DaeProvenance::source(chain.span())?;
        let discrete_value = self.discrete_values.owner(
            owner_provenance,
            when_chain_discrete_value_targets(chain, self.request.coordinates),
            self.request.coordinates,
            self.request.topology,
        )?;
        let guards = self.lower_chain_guards(chain)?;
        self.own_chain_clocks(chain, &guards)?;
        let owners = WhenSemanticOwners {
            source: source_owner,
            discrete_value,
        };
        for (branch, guard) in chain.branches().zip(guards) {
            self.lower_equations(owners, guard, &branch.equations)?;
        }
        Ok(())
    }

    /// The activation of each branch of a `when`/`elsewhen` chain (MLS §8.3.5).
    ///
    /// Each branch is activated by *its own* rising edge and by nothing else:
    /// §8.3.5 activates the equations of a when-equation *"only at the instant
    /// when the scalar expression or any of the elements of the vector expression
    /// becomes true"*, and §8.3.5.1 spells the chain out as one if-expression per
    /// assigned variable whose arms are `edge(b1)`, `edge(b2)`, … over one
    /// `Boolean bi` per branch condition. An `elsewhen` therefore carries no
    /// condition of its own beyond its own edge.
    ///
    /// What the earlier branches do own is *priority*, and §8.3.5.4 (the Single
    /// Assignment Rule applied to when-equations) scopes that precisely: the
    /// chain form *"can be used to resolve assignment conflicts since the first
    /// of the when/elsewhen parts are given higher priority than
    /// later ones"*. A conflict exists only where two arms of the same
    /// if-expression are selected at one instant, i.e. where two branch edges
    /// coincide — and the arms are ordered, so the earlier one wins. §8.3.5.1
    /// also requires every branch of a chain to assign the same component
    /// references, so ordering the arms is the whole of the resolution.
    ///
    /// Subtracting the earlier branches' *level* here instead — guarding branch
    /// `i` with `cond_i and not (cond_1 or …)` — outlaws far more than a
    /// conflict: a `cond_1` that stays true suppresses every later branch for the
    /// rest of the run, so `when time > 0.3 then y = 1; elsewhen time > 0.7 then
    /// y = 2;` held `y = 1` where OpenModelica reaches `y = 2` at `t = 0.7`.
    fn lower_chain_guards(
        &mut self,
        chain: &flat::WhenChain,
    ) -> Result<Vec<EventGuard<'dae>>, dae::DaeConstructionError> {
        let mut guards = Vec::with_capacity(chain.branch_count());
        for branch in chain.branches() {
            let (condition, owner_clock) = self.lower_condition(branch)?;
            guards.push(EventGuard {
                trigger: condition,
                condition,
                owner_clock,
                branch_provenance: dae::DaeProvenance::source(branch.span)?,
                always: false,
                parent_activation: None,
            });
        }
        Ok(guards)
    }

    fn own_chain_clocks(
        &mut self,
        chain: &flat::WhenChain,
        guards: &[EventGuard<'dae>],
    ) -> Result<(), dae::DaeConstructionError> {
        for (branch, guard) in chain.branches().zip(guards) {
            if let Some(clock) = guard.owner_clock {
                own_clocked_targets(
                    self.construction,
                    self.request.coordinates,
                    clock.into(),
                    &branch.equations,
                )?;
            }
        }
        Ok(())
    }
}

fn own_clocked_targets<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    clock: dae::ClockId<'dae>,
    equations: &[flat::WhenEquation],
) -> Result<(), dae::DaeConstructionError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { target, .. } => {
                let provenance = dae::DaeProvenance::source(equation.span())?;
                construction.clocks(|clocks| match coordinates[target] {
                    Coordinate::DiscreteReal(variable) => {
                        clocks.own_discrete_real(clock, variable, provenance)?;
                        Ok(())
                    }
                    Coordinate::DiscreteValue(variable) => {
                        clocks.own_discrete_value(clock, variable, provenance)?;
                        Ok(())
                    }
                    _ => unreachable!("clock analysis accepts only discrete clocked targets"),
                })?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, branch) in branches {
                    own_clocked_targets(construction, coordinates, clock, branch)?;
                }
                if let Some(else_branch) = else_branch {
                    own_clocked_targets(construction, coordinates, clock, else_branch)?;
                }
            }
            flat::WhenEquation::Reinit { .. }
            | flat::WhenEquation::Assert { .. }
            | flat::WhenEquation::Terminate { .. } => {}
            flat::WhenEquation::FunctionCallOutputs { .. } => {
                unreachable!("analysis rejects unchecked event function calls")
            }
        }
    }
    Ok(())
}

impl<'shape, 'dae> WhenLowering<'_, '_, 'shape, 'dae> {
    fn lower_condition(
        &mut self,
        branch: &flat::WhenBranch,
    ) -> Result<
        (dae::ConditionId<'dae>, Option<dae::PeriodicClockId<'dae>>),
        dae::DaeConstructionError,
    > {
        let expression = &branch.condition;
        let Some((clock, span)) = self.branch_clock(branch)? else {
            return lower_condition(
                self.construction,
                self.request.coordinates,
                self.request.functions,
                self.request.sample_lattices,
                expression,
            );
        };
        let provenance = dae::DaeProvenance::source(span)?;
        let condition = self
            .construction
            .conditions(|conditions| conditions.reserve(provenance))?;
        self.construction.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Clock(clock.into()),
                provenance,
            )
        })?;
        Ok((condition, Some(clock)))
    }

    /// The periodic clock this `when` branch is triggered by, if it has one.
    ///
    /// A branch either names its clock coordinate directly or uses the MLS
    /// §16.5.1 inferred-clock form `when Clock()`, whose owner the clock-domain
    /// analysis already proved for the branch occurrence.
    fn branch_clock(
        &self,
        branch: &flat::WhenBranch,
    ) -> Result<Option<(dae::PeriodicClockId<'dae>, Span)>, dae::DaeConstructionError> {
        if let Expression::VarRef {
            name,
            subscripts,
            span,
        } = &branch.condition
        {
            let clock = subscripts
                .is_empty()
                .then(|| {
                    self.request
                        .clocks
                        .by_coordinate
                        .get(name.var_name())
                        .copied()
                })
                .flatten();
            return Ok(clock.map(|clock| (clock, *span)));
        }
        if !is_inferred_clock_condition(&branch.condition) {
            return Ok(None);
        }
        let plan = self
            .request
            .when_owners
            .get(&branch.span)
            .ok_or(dae::DaeConstructionError::MissingClockDomainOwner { span: branch.span })?;
        let clock = self.request.clocks.id(plan, branch.span)?;
        Ok(Some((clock, branch.span)))
    }
}

impl<'shape, 'dae> WhenLowering<'_, '_, 'shape, 'dae> {
    fn lower_equations(
        &mut self,
        owners: WhenSemanticOwners,
        guard: EventGuard<'dae>,
        equations: &[flat::WhenEquation],
    ) -> Result<(), dae::DaeConstructionError> {
        for equation in equations {
            self.lower_equation(owners, guard, equation)?;
        }
        Ok(())
    }

    fn lower_equation(
        &mut self,
        owners: WhenSemanticOwners,
        guard: EventGuard<'dae>,
        equation: &flat::WhenEquation,
    ) -> Result<(), dae::DaeConstructionError> {
        match equation {
            flat::WhenEquation::Assign { target, value, .. } => {
                self.lower_assignment(owners, guard, target, value, equation.span())
            }
            flat::WhenEquation::Reinit { state, value, .. } => {
                self.lower_reinit(owners.source, guard, state, value, equation.span())
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => self.lower_assert(guard, condition, message, level.as_deref(), equation.span()),
            flat::WhenEquation::Terminate { message, .. } => {
                self.lower_terminate(guard, message, equation.span())
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => self.lower_conditional(
                owners,
                guard,
                branches,
                else_branch.as_deref(),
                equation.span(),
            ),
            flat::WhenEquation::FunctionCallOutputs { span, .. } => {
                unreachable!("analysis explicitly rejects event function-call outputs at {span:?}")
            }
        }
    }

    fn lower_assignment(
        &mut self,
        owners: WhenSemanticOwners,
        guard: EventGuard<'dae>,
        target: &VarName,
        value: &Expression,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        let provenance = dae::DaeProvenance::source(span)?;
        let coordinate = self.request.coordinates[target];
        if let Coordinate::DiscreteReal(target) = coordinate {
            self.target_owners
                .claim_discrete_real(target, owners.source, span)?;
        }
        let value = match guard.owner_clock {
            Some(clock) => lower_clocked_expression(
                self.construction,
                self.request.coordinates,
                self.request.functions,
                clock,
                value,
                None,
            )?,
            None => lower_expression(
                self.construction,
                self.request.coordinates,
                self.request.functions,
                value,
                None,
            )?,
        };
        lower_when_assignment(
            self.construction,
            self.discrete_values,
            owners.discrete_value,
            coordinate,
            guard,
            value,
            provenance,
        )
    }

    fn lower_reinit(
        &mut self,
        source_owner: WhenSourceOwner,
        guard: EventGuard<'dae>,
        state: &VarName,
        value: &Expression,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        let Coordinate::State(state) = self.request.coordinates[state] else {
            unreachable!("analysis accepts only state reinitialization")
        };
        self.target_owners.claim_state(state, source_owner, span)?;
        let value = lower_expression(
            self.construction,
            self.request.coordinates,
            self.request.functions,
            value,
            None,
        )?;
        let provenance = dae::DaeProvenance::source(span)?;
        self.construction.events(|events| {
            events.reinitialize(guard.trigger, guard.condition, state, value, provenance)
        })?;
        Ok(())
    }

    fn lower_assert(
        &mut self,
        guard: EventGuard<'dae>,
        condition: &Expression,
        message: &Expression,
        level: Option<&Expression>,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        let (condition, _) = lower_condition(
            self.construction,
            self.request.coordinates,
            self.request.functions,
            self.request.sample_lattices,
            condition,
        )?;
        let failed = negate_condition(self.construction, condition, span)?;
        let action_guard =
            combine_conditions(self.construction, guard.condition, failed, false, span)?;
        let message = lower_expression(
            self.construction,
            self.request.coordinates,
            self.request.functions,
            message,
            None,
        )?;
        let level = lower_optional_expression(
            self.construction,
            self.request.coordinates,
            self.request.functions,
            level,
        )?;
        let provenance = dae::DaeProvenance::source(span)?;
        self.construction.events(|events| {
            events.assert_with_level(guard.trigger, action_guard, message, level, provenance)
        })?;
        Ok(())
    }

    fn lower_terminate(
        &mut self,
        guard: EventGuard<'dae>,
        message: &Expression,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        let message = lower_expression(
            self.construction,
            self.request.coordinates,
            self.request.functions,
            message,
            None,
        )?;
        let provenance = dae::DaeProvenance::source(span)?;
        self.construction.events(|events| {
            events.terminate(guard.trigger, guard.condition, message, provenance)
        })?;
        Ok(())
    }
}

pub(super) fn lower_when_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    target: Coordinate<'dae>,
    guard: EventGuard<'dae>,
    value: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    match target {
        Coordinate::DiscreteReal(target) => {
            let lhs = construction.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::DiscreteReal(target))
            })?;
            let residual = generated_residual(construction, provenance, lhs, value)?;
            construction.discrete(|discrete| {
                if guard.always {
                    discrete.real_equation(provenance, |equation| equation.residual(residual))
                } else {
                    discrete.when_real_equation(
                        guard.trigger,
                        guard.condition,
                        provenance,
                        |equation| equation.residual(residual),
                    )
                }
            })?;
            Ok(())
        }
        Coordinate::DiscreteValue(target) => {
            let owner = discrete_owner
                .expect("a discrete-value event assignment has one semantic B.1c owner");
            if guard.always {
                discrete_values.always(owner, target, value, guard.branch_provenance, provenance)
            } else {
                discrete_values.when(super::discrete_values::DiscreteWhenAssignment {
                    owner,
                    trigger: guard.trigger,
                    guard: guard.condition,
                    parent: guard.parent_activation,
                    target,
                    value,
                    branch_provenance: guard.branch_provenance,
                    action_provenance: provenance,
                })
            }
        }
        _ => unreachable!("analysis accepts only discrete when targets"),
    }
}

impl<'shape, 'dae> WhenLowering<'_, '_, 'shape, 'dae> {
    fn lower_conditional(
        &mut self,
        owners: WhenSemanticOwners,
        parent: EventGuard<'dae>,
        branches: &[(Expression, Vec<flat::WhenEquation>)],
        else_branch: Option<&[flat::WhenEquation]>,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        let mut previous = None;
        for (condition, equations) in branches {
            previous = Some(
                self.lower_conditional_branch(owners, parent, previous, condition, equations)?,
            );
        }
        if let Some(equations) = else_branch.filter(|branch| !branch.is_empty()) {
            self.lower_conditional_else(owners, parent, previous, equations, span)?;
        }
        Ok(())
    }

    fn lower_conditional_branch(
        &mut self,
        owners: WhenSemanticOwners,
        parent: EventGuard<'dae>,
        previous: Option<dae::ConditionId<'dae>>,
        condition: &Expression,
        equations: &[flat::WhenEquation],
    ) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
        let branch_span = condition
            .span()
            .expect("analysis proves conditional when provenance");
        let (condition, _) = lower_condition(
            self.construction,
            self.request.coordinates,
            self.request.functions,
            self.request.sample_lattices,
            condition,
        )?;
        let available = match previous {
            Some(previous) => {
                let not_previous = negate_condition(self.construction, previous, branch_span)?;
                combine_conditions(
                    self.construction,
                    condition,
                    not_previous,
                    false,
                    branch_span,
                )?
            }
            None => condition,
        };
        let guard_condition = combine_conditions(
            self.construction,
            parent.condition,
            available,
            false,
            branch_span,
        )?;
        let guard = EventGuard {
            trigger: parent.trigger,
            condition: guard_condition,
            owner_clock: parent.owner_clock,
            branch_provenance: dae::DaeProvenance::source(branch_span)?,
            always: false,
            parent_activation: Some((parent.trigger, parent.condition)),
        };
        self.lower_equations(owners, guard, equations)?;
        match previous {
            Some(previous) => {
                combine_conditions(self.construction, previous, condition, true, branch_span)
            }
            None => Ok(condition),
        }
    }

    fn lower_conditional_else(
        &mut self,
        owners: WhenSemanticOwners,
        parent: EventGuard<'dae>,
        previous: Option<dae::ConditionId<'dae>>,
        equations: &[flat::WhenEquation],
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        let condition = match previous {
            Some(previous) => {
                let available = negate_condition(self.construction, previous, span)?;
                combine_conditions(self.construction, parent.condition, available, false, span)?
            }
            None => parent.condition,
        };
        let guard = EventGuard {
            trigger: parent.trigger,
            condition,
            owner_clock: parent.owner_clock,
            branch_provenance: dae::DaeProvenance::source(span)?,
            always: false,
            parent_activation: Some((parent.trigger, parent.condition)),
        };
        self.lower_equations(owners, guard, equations)
    }
}
