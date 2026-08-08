//! Replay the equation and topology owners onto the rebuilt expressions.
//!
//! Owners are rebuilt last because every one of them names expressions and
//! identities the earlier stages produced. The ordering inside is a real
//! dependency chain, not a convention: relations precede the conditions
//! defined against them, conditions precede the roots and discrete owners that
//! activate on them. A holonomic reduction supplies its one residual
//! replacement here, which is the only point where the rebuilt system stops
//! being an exact structural copy of its source.

use rumoca_ir_dae as dae;

use super::declarations::RebuiltDomain;
use super::event_owners::{define_conditions, rebuild_events, rebuild_relations, rebuild_roots};
use super::temporal::RebuiltClock;
use super::variables::{ReservedVariable, TargetVariable};

pub(super) struct RebuiltOwnerIdentities<'borrow, 'target> {
    pub(super) variables: &'borrow [ReservedVariable<'target>],
    pub(super) domains: &'borrow [RebuiltDomain<'target>],
    pub(super) conditions: &'borrow [dae::ConditionId<'target>],
    pub(super) clocks: &'borrow [RebuiltClock<'target>],
}

pub(super) fn rebuild_semantic_owners<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    identities: RebuiltOwnerIdentities<'_, 'target>,
    replacement: Option<(u32, dae::ExprId<'target>)>,
) -> Result<(), dae::DaeConstructionError> {
    rebuild_equations(source, target, expressions, identities.domains, replacement)?;
    rebuild_initial_discrete_values(source, target, expressions, identities.variables)?;
    let relations = rebuild_relations(source, target, expressions)?;
    define_conditions(
        source,
        target,
        expressions,
        identities.conditions,
        &relations,
        identities.clocks,
    )?;
    rebuild_discrete_equations(source, target, expressions, identities.conditions)?;
    rebuild_roots(
        source,
        target,
        expressions,
        identities.domains,
        identities.conditions,
        &relations,
    )?;
    rebuild_events(
        source,
        target,
        expressions,
        identities.variables,
        identities.conditions,
    )?;
    rebuild_discrete_value_owners(
        source,
        target,
        expressions,
        identities.variables,
        identities.conditions,
    )
}

fn rebuild_equations<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    domains: &[RebuiltDomain<'target>],
    replacement: Option<(u32, dae::ExprId<'target>)>,
) -> Result<(), dae::DaeConstructionError> {
    target.continuous(|target| {
        for owner in source.continuous_owners() {
            match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => {
                    let residual = replacement
                        .filter(|(source, _)| *source == equation.residual().index())
                        .map_or(
                            expressions[equation.residual().index() as usize],
                            |(_, target)| target,
                        );
                    target.value_equation(equation.provenance(), residual)?;
                }
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    rebuild_continuous_family(target, family, expressions, domains)?;
                }
            }
        }
        Ok(())
    })?;
    target.initialization(|target| {
        for owner in source.initialization_owners() {
            match owner {
                dae::InitializationOwnerView::Residual { equation, .. } => {
                    target.value_equation(
                        equation.provenance(),
                        expressions[equation.residual().index() as usize],
                    )?;
                }
                dae::InitializationOwnerView::Structured { family, .. } => {
                    rebuild_initialization_family(target, family, expressions, domains)?;
                }
            }
        }
        Ok(())
    })
}

/// Replay every MLS §8.6 discrete initial-value definition.
///
/// A structural transform is an exact copy of its source apart from the one
/// residual a reduction replaces, so a definition that survived construction
/// must survive the rebuild. Dropping one here would leave the coordinate at
/// its declared `start` instead of the value the initial algorithm determined —
/// a silently different trajectory rather than a typed failure.
fn rebuild_initial_discrete_values<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    variables: &[ReservedVariable<'target>],
) -> Result<(), dae::DaeConstructionError> {
    target.initialization(|target| {
        for definition in source.initial_discrete_values() {
            let value = expressions[definition.value().index() as usize];
            let identity = variables[definition.target().index() as usize].identity;
            match identity {
                TargetVariable::DiscreteReal(coordinate) => {
                    target.discrete_real_initial_value(
                        coordinate,
                        value,
                        definition.provenance(),
                    )?;
                }
                TargetVariable::DiscreteValue(coordinate) => {
                    target.discrete_value_initial_value(
                        coordinate,
                        value,
                        definition.provenance(),
                    )?;
                }
                _ => unreachable!(
                    "a checked discrete initial-value target retains its discrete role"
                ),
            }
        }
        Ok(())
    })
}

fn rebuild_discrete_equations<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    conditions: &[dae::ConditionId<'target>],
) -> Result<(), dae::DaeConstructionError> {
    target.discrete(|target| {
        for index in 0..source.discrete_real_equation_count() {
            let equation = source
                .discrete_real_equation(index)
                .expect("finalized discrete-real equation resolves");
            let build = |target: &mut dae::ResidualEquation<'_, 'target>| {
                target.residual(expressions[equation.residual().index() as usize])
            };
            match equation.activation() {
                dae::DiscreteRealActivation::Always => {
                    target.real_equation(equation.provenance(), build)?;
                }
                dae::DiscreteRealActivation::When { trigger, guard } => {
                    target.when_real_equation(
                        conditions[trigger.index() as usize],
                        conditions[guard.index() as usize],
                        equation.provenance(),
                        build,
                    )?;
                }
            }
        }
        Ok(())
    })
}

fn rebuild_discrete_value_owners<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    variables: &[ReservedVariable<'target>],
    conditions: &[dae::ConditionId<'target>],
) -> Result<(), dae::DaeConstructionError> {
    let source_owners = (0..source.discrete_value_owner_count())
        .map(|index| {
            source
                .discrete_value_owner(
                    source
                        .discrete_value_owner_id(index)
                        .expect("finalized B.1c owner ordinal resolves"),
                )
                .expect("finalized B.1c owner identity resolves")
        })
        .collect::<Vec<_>>();
    let plan = source_owners
        .iter()
        .copied()
        .flat_map(|owner| owner.targets().iter())
        .map(|source_target| rebuilt_discrete_value(variables, source_target))
        .collect::<Vec<_>>();
    target.b1c(plan, |topology| {
        for source_owner in source_owners {
            let targets = source_owner
                .targets()
                .iter()
                .map(|source_target| rebuilt_discrete_value(variables, source_target))
                .collect::<Vec<_>>();
            topology.owner(source_owner.provenance(), targets, |target_owner| {
                rebuild_discrete_value_branches(target_owner, source_owner, expressions, conditions)
            })?;
        }
        Ok(())
    })
}

fn rebuild_discrete_value_branches<'target>(
    target: &mut dae::DiscreteValueOwner<'_, 'target>,
    source: dae::DiscreteValueOwnerView<'_>,
    expressions: &[dae::ExprId<'target>],
    conditions: &[dae::ConditionId<'target>],
) -> Result<(), dae::DaeConstructionError> {
    for branch in source.branches().iter() {
        let values = branch
            .values()
            .iter()
            .map(|(value, provenance)| (expressions[value.index() as usize], provenance));
        match branch.activation() {
            dae::DiscreteBranchActivation::Always => {
                target.always(branch.provenance(), values)?;
            }
            dae::DiscreteBranchActivation::When { trigger, guard } => {
                target.when(
                    conditions[trigger.index() as usize],
                    conditions[guard.index() as usize],
                    branch.provenance(),
                    values,
                )?;
            }
        }
    }
    Ok(())
}

fn rebuilt_discrete_value<'target>(
    variables: &[ReservedVariable<'target>],
    source: dae::DiscreteValueId<'_>,
) -> dae::DiscreteValueId<'target> {
    let TargetVariable::DiscreteValue(target) = variables[source.index() as usize].identity else {
        unreachable!("checked B.1c owner target retains its discrete-value role")
    };
    target
}

fn rebuild_continuous_family<'target>(
    target: &mut dae::ContinuousEquations<'_, 'target>,
    family: dae::StructuredFamilyView<'_>,
    expressions: &[dae::ExprId<'target>],
    domains: &[RebuiltDomain<'target>],
) -> Result<(), dae::DaeConstructionError> {
    let domain = domains[family.domain().index() as usize].id;
    target.structured_family(
        family.provenance(),
        domain,
        family.scalar_view(),
        |target| rebuild_family_bodies(target, family, expressions),
    )?;
    Ok(())
}

fn rebuild_initialization_family<'target>(
    target: &mut dae::InitializationEquations<'_, 'target>,
    family: dae::StructuredFamilyView<'_>,
    expressions: &[dae::ExprId<'target>],
    domains: &[RebuiltDomain<'target>],
) -> Result<(), dae::DaeConstructionError> {
    let domain = domains[family.domain().index() as usize].id;
    target.structured_family(
        family.provenance(),
        domain,
        family.scalar_view(),
        |target| rebuild_family_bodies(target, family, expressions),
    )?;
    Ok(())
}

fn rebuild_family_bodies<'target>(
    target: &mut dae::StructuredResiduals<'_, 'target>,
    family: dae::StructuredFamilyView<'_>,
    expressions: &[dae::ExprId<'target>],
) -> Result<(), dae::DaeConstructionError> {
    for body in family.bodies().iter() {
        target.body(expressions[body.index() as usize])?;
    }
    Ok(())
}
