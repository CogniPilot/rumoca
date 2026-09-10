//! Drive a whole-DAE reconstruction for one accepted constraint.
//!
//! Both entry points replay the source system into a fresh
//! [`dae::DaeConstruction`] in dependency order — types, domains, variable
//! reservations, conditions, clocks, temporal and delay coordinates,
//! functions, expressions, then semantic owners — so the replacement DAE is a
//! finalized peer of the original rather than a patched copy. The two differ
//! only in what they substitute: a demotion reserves the chosen state as
//! algebraic, while a holonomic reduction replaces one residual with its
//! second derivative and reports the manifold expressions it displaced.

use rumoca_ir_dae as dae;

use super::constraints::DifferentiationFacts;
use super::declarations::{rebuild_domains, rebuild_types, reserve_conditions};
use super::expressions::{ExpressionRebuilder, RebuiltBaseIdentities, RebuiltIdentities};
use super::functions::rebuild_functions;
use super::runtime_quotients::{QuotientExpressionContext, RuntimeQuotientReplayPlan};
use super::semantic_owners::{RebuiltOwnerIdentities, rebuild_semantic_owners};
use super::temporal::{rebuild_clocks, rebuild_delay_coordinates, rebuild_temporal_coordinates};
use super::variables::{
    ReservedVariable, define_static_variables_at, define_variables, reserve_variables,
};
use super::{DirectStateConstraint, HolonomicConstraint, LiftedManifoldOwner, ManifoldConstraint};
use crate::StructuralError;

pub(super) fn rebuild_holonomic_constraint(
    model: &dae::Dae,
    constraint: &HolonomicConstraint,
    prior_manifold: &[ManifoldConstraint],
) -> Result<(dae::Dae, Vec<ManifoldConstraint>), StructuralError> {
    let mut manifold = Vec::with_capacity(prior_manifold.len() + 2);
    let rebuilt = model.inspect(|source| {
        dae::Dae::construct(model.source_map().clone(), |target| {
            prepare_rebuild(
                source,
                target,
                None,
                constraint.lifted_algebraic,
                |prepared| {
                    let PreparedRebuild {
                        context,
                        target,
                        variables,
                        rebuilt_state,
                        quotients,
                        expressions,
                    } = prepared;
                    manifold.extend(prior_manifold.iter().map(|entry| ManifoldConstraint {
                        expression: expressions[entry.expression as usize].index(),
                        lifted: entry.lifted.map(|lifted| LiftedManifoldOwner {
                            residual: expressions[lifted.residual as usize].index(),
                            ..lifted
                        }),
                    }));
                    let replacement = rebuild_holonomic_replacement(
                        context,
                        target,
                        variables,
                        rebuilt_state,
                        constraint,
                        &mut manifold,
                    )?;
                    define_variables(source, target, &expressions, variables)?;
                    rebuild_semantic_owners(
                        source,
                        target,
                        &expressions,
                        RebuiltOwnerIdentities {
                            variables,
                            domains: context.domains,
                            conditions: context.conditions,
                            clocks: context.clocks,
                        },
                        Some(super::semantic_owners::EquationReplacement {
                            owner_ordinal: constraint.owner_ordinal,
                            body_ordinal: constraint.body_ordinal,
                            residual: constraint.residual,
                            replacement,
                        }),
                        quotients,
                    )
                },
            )
        })
    });
    rebuilt
        .map(|dae| (dae, manifold))
        .map_err(construction_failure)
}

#[cfg(test)]
pub(super) fn rebuild_with_state_demotion(
    model: &dae::Dae,
    candidate: DirectStateConstraint,
) -> Result<dae::Dae, StructuralError> {
    rebuild_with_state_demotion_and_manifold(model, candidate, &[]).map(|(dae, _)| dae)
}

pub(super) fn rebuild_with_state_demotion_and_manifold(
    model: &dae::Dae,
    candidate: DirectStateConstraint,
    prior_manifold: &[ManifoldConstraint],
) -> Result<(dae::Dae, Vec<ManifoldConstraint>), StructuralError> {
    let mut manifold = Vec::with_capacity(prior_manifold.len());
    let rebuilt = model.inspect(|source| {
        dae::Dae::construct(model.source_map().clone(), |target| {
            prepare_rebuild(source, target, Some(candidate), None, |prepared| {
                let PreparedRebuild {
                    context,
                    target,
                    variables,
                    expressions,
                    quotients,
                    ..
                } = prepared;
                restore_prior_manifold(
                    context,
                    target,
                    variables,
                    &expressions,
                    candidate,
                    prior_manifold,
                    &mut manifold,
                )?;
                let replacement =
                    restored_equation_replacement(prior_manifold, candidate, &expressions);
                define_variables(source, target, &expressions, variables)?;
                rebuild_semantic_owners(
                    source,
                    target,
                    &expressions,
                    RebuiltOwnerIdentities {
                        variables,
                        domains: context.domains,
                        conditions: context.conditions,
                        clocks: context.clocks,
                    },
                    replacement,
                    quotients,
                )
            })
        })
    });
    rebuilt
        .map(|dae| (dae, manifold))
        .map_err(construction_failure)
}

#[derive(Clone, Copy)]
struct RebuildContext<'source, 'borrow, 'target> {
    source: dae::DaeView<'source>,
    types: &'borrow [dae::ValueTypeId<'target>],
    functions: &'borrow [super::functions::RebuiltFunction<'target>],
    domains: &'borrow [super::declarations::RebuiltDomain<'target>],
    conditions: &'borrow [dae::ConditionId<'target>],
    clocks: &'borrow [super::temporal::RebuiltClock<'target>],
    previous: &'borrow [dae::PreviousId<'target>],
    terminals: &'borrow [dae::TerminalId<'target>],
    facts: &'borrow DifferentiationFacts,
    candidate: Option<DirectStateConstraint>,
}

impl<'target> RebuildContext<'_, '_, 'target> {
    fn identities<'borrow>(
        &'borrow self,
        variables: &'borrow [ReservedVariable<'target>],
    ) -> RebuiltIdentities<'borrow, 'target> {
        RebuiltIdentities {
            base: RebuiltBaseIdentities {
                types: self.types,
                variables,
                domains: self.domains,
                conditions: self.conditions,
                clocks: self.clocks,
                previous: self.previous,
                terminals: self.terminals,
            },
            functions: self.functions,
        }
    }
}

struct PreparedRebuild<'source, 'borrow, 'target> {
    context: RebuildContext<'source, 'borrow, 'target>,
    target: &'borrow mut dae::DaeConstruction<'target>,
    variables: &'borrow mut [ReservedVariable<'target>],
    rebuilt_state: &'borrow mut [Option<dae::ExprId<'target>>],
    quotients: &'borrow mut RuntimeQuotientReplayPlan<'target>,
    expressions: Vec<dae::ExprId<'target>>,
}

fn prepare_rebuild<'source, 'target>(
    source: dae::DaeView<'source>,
    target: &mut dae::DaeConstruction<'target>,
    candidate: Option<DirectStateConstraint>,
    lifted_algebraic: Option<u32>,
    finish: impl FnOnce(PreparedRebuild<'source, '_, 'target>) -> Result<(), dae::DaeConstructionError>,
) -> Result<(), dae::DaeConstructionError> {
    let mut quotients = begin_reconstruction(source, target)?;
    let types = rebuild_types(source, target)?;
    let domains = rebuild_domains(source, target)?;
    let mut variables = reserve_variables(
        source,
        target,
        &types,
        candidate.map(|candidate| candidate.state),
        lifted_algebraic,
    )?;
    let conditions = reserve_conditions(source, target)?;
    let clocks = rebuild_clocks(source, target, &variables, &conditions)?;
    let temporal = rebuild_temporal_coordinates(source, target, &variables, &clocks)?;
    let facts = DifferentiationFacts::collect(source);
    let mut rebuilt_state = vec![None; source.expression_count()];
    let base = RebuiltBaseIdentities {
        types: &types,
        variables: &variables,
        domains: &domains,
        conditions: &conditions,
        clocks: &clocks,
        previous: &temporal.previous,
        terminals: &temporal.terminals,
    };
    let functions = rebuild_functions(
        source,
        target,
        base,
        &facts,
        candidate,
        &mut rebuilt_state,
        &mut quotients,
    )?;
    rebuild_delay_coordinates(
        source,
        target,
        RebuiltIdentities {
            base,
            functions: &functions,
        },
        &facts,
        candidate,
        &mut rebuilt_state,
        &mut quotients,
    )?;
    let context = RebuildContext {
        source,
        types: &types,
        functions: &functions,
        domains: &domains,
        conditions: &conditions,
        clocks: &clocks,
        previous: &temporal.previous,
        terminals: &temporal.terminals,
        facts: &facts,
        candidate,
    };
    let expressions = rebuild_expressions_and_static_variables(
        context,
        target,
        &mut variables,
        &mut rebuilt_state,
        &mut quotients,
    )?;
    finish(PreparedRebuild {
        context,
        target,
        variables: &mut variables,
        rebuilt_state: &mut rebuilt_state,
        quotients: &mut quotients,
        expressions,
    })
}

fn restore_prior_manifold<'source, 'target>(
    context: RebuildContext<'source, '_, 'target>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    expressions: &[dae::ExprId<'target>],
    candidate: DirectStateConstraint,
    prior_manifold: &[ManifoldConstraint],
    manifold: &mut Vec<ManifoldConstraint>,
) -> Result<(), dae::DaeConstructionError> {
    let mut manifold_state = vec![None; context.source.expression_count()];
    target.expressions(|expression_target| {
        let mut rebuilder = ExpressionRebuilder::new(
            context.source,
            expression_target,
            context.identities(variables),
            context.facts,
            Some(candidate),
            &mut manifold_state,
        )
        .substituting_demoted_value();
        for entry in prior_manifold {
            if entry
                .lifted
                .is_some_and(|lifted| lifted.state == candidate.state)
            {
                continue;
            }
            let source_expression = context
                .source
                .expression_id(entry.expression as usize)
                .expect("retained manifold expression resolves");
            manifold.push(ManifoldConstraint {
                expression: rebuilder.rebuild(source_expression)?.index(),
                lifted: entry.lifted.map(|lifted| LiftedManifoldOwner {
                    residual: expressions[lifted.residual as usize].index(),
                    ..lifted
                }),
            });
        }
        Ok(())
    })
}

fn restored_equation_replacement<'target>(
    prior_manifold: &[ManifoldConstraint],
    candidate: DirectStateConstraint,
    expressions: &[dae::ExprId<'target>],
) -> Option<super::semantic_owners::EquationReplacement<'target>> {
    let restored = prior_manifold.iter().find(|entry| {
        entry
            .lifted
            .is_some_and(|lifted| lifted.state == candidate.state)
    })?;
    restored
        .lifted
        .map(|lifted| super::semantic_owners::EquationReplacement {
            owner_ordinal: lifted.owner_ordinal,
            body_ordinal: lifted.body_ordinal,
            residual: lifted.residual,
            replacement: expressions[restored.expression as usize],
        })
}

fn rebuild_holonomic_replacement<'target>(
    context: RebuildContext<'_, '_, 'target>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    rebuilt: &mut [Option<dae::ExprId<'target>>],
    constraint: &HolonomicConstraint,
    manifold: &mut Vec<ManifoldConstraint>,
) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
    target.expressions(|expression_target| {
        let mut rebuilder = ExpressionRebuilder::new(
            context.source,
            expression_target,
            context.identities(variables),
            context.facts,
            None,
            rebuilt,
        );
        let source_residual = context
            .source
            .expression_id(constraint.residual as usize)
            .expect("holonomic residual resolves");
        let provenance = dae::DaeProvenance::generated(
            dae::DaeGeneration::IndexReduction,
            constraint.owner.span(),
        )?;
        if let Some(algebraic) = constraint.lifted_algebraic {
            let value = rebuilder.materialize_lifted_algebraic_value(
                algebraic,
                &constraint.proof,
                provenance,
            )?;
            let replacement = rebuilder.differentiate_lifted_algebraic(
                algebraic,
                &constraint.proof,
                provenance,
            )?;
            manifold.push(ManifoldConstraint {
                expression: value.index(),
                lifted: Some(LiftedManifoldOwner {
                    state: algebraic,
                    owner_ordinal: constraint.owner_ordinal,
                    body_ordinal: constraint.body_ordinal,
                    residual: replacement.index(),
                }),
            });
            return Ok(replacement);
        }
        let value = rebuilder.materialize_holonomic_value(
            source_residual,
            &constraint.proof,
            provenance,
        )?;
        manifold.push(ManifoldConstraint {
            expression: value.index(),
            lifted: None,
        });
        let first =
            rebuilder.differentiate_holonomic(source_residual, 1, &constraint.proof, provenance)?;
        if constraint.proof.maximum_order == 1 {
            return rebuilder.materialize_derivative(first, source_residual, provenance);
        }
        if !matches!(first, super::differentiation::Derivative::Zero) {
            let first = rebuilder.materialize_derivative(first, source_residual, provenance)?;
            manifold.push(ManifoldConstraint {
                expression: first.index(),
                lifted: None,
            });
        }
        let second =
            rebuilder.differentiate_holonomic(source_residual, 2, &constraint.proof, provenance)?;
        rebuilder.materialize_derivative(second, source_residual, provenance)
    })
}

fn rebuild_expressions_and_static_variables<'source, 'target>(
    context: RebuildContext<'source, '_, 'target>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &mut [ReservedVariable<'target>],
    rebuilt: &mut [Option<dae::ExprId<'target>>],
    quotients: &mut RuntimeQuotientReplayPlan<'target>,
) -> Result<Vec<dae::ExprId<'target>>, dae::DaeConstructionError> {
    let (before_expressions, milestones) = static_definition_milestones(context.source);
    define_static_variables_at(
        context.source,
        target,
        rebuilt,
        variables,
        &before_expressions,
    )?;

    let mut next = 0_usize;
    let mut milestone = 0_usize;
    while milestone < milestones.len() {
        let expression = milestones[milestone].0;
        rebuild_expression_segment(
            context,
            target,
            variables,
            rebuilt,
            quotients,
            next,
            expression + 1,
        )?;
        next = expression + 1;

        let first = milestone;
        while milestone < milestones.len() && milestones[milestone].0 == expression {
            milestone += 1;
        }
        let ready = milestones[first..milestone]
            .iter()
            .map(|(_, variable)| *variable)
            .collect::<Vec<_>>();
        define_static_variables_at(context.source, target, rebuilt, variables, &ready)?;
    }
    rebuild_expression_segment(
        context,
        target,
        variables,
        rebuilt,
        quotients,
        next,
        context.source.expression_count(),
    )?;
    Ok(rebuilt
        .iter()
        .map(|expression| expression.expect("every source expression was rebuilt"))
        .collect())
}

fn rebuild_expression_segment<'source, 'target>(
    context: RebuildContext<'source, '_, 'target>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    rebuilt: &mut [Option<dae::ExprId<'target>>],
    quotients: &mut RuntimeQuotientReplayPlan<'target>,
    start: usize,
    end: usize,
) -> Result<(), dae::DaeConstructionError> {
    if start == end {
        return Ok(());
    }
    for index in start..end {
        quotients.replay_model_owners_through(
            index,
            target,
            QuotientExpressionContext {
                source: context.source,
                identities: context.identities(variables),
                facts: context.facts,
                candidate: context.candidate,
            },
            rebuilt,
        )?;
        target.expressions(|expressions| {
            let mut rebuilder = ExpressionRebuilder::new(
                context.source,
                expressions,
                context.identities(variables),
                context.facts,
                context.candidate,
                rebuilt,
            );
            let source_id = context
                .source
                .expression_id(index)
                .expect("finalized expression ordinal resolves");
            rebuilder.rebuild(source_id)?;
            Ok(())
        })?;
    }
    Ok(())
}

fn begin_reconstruction<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
) -> Result<RuntimeQuotientReplayPlan<'target>, dae::DaeConstructionError> {
    let plan = RuntimeQuotientReplayPlan::collect(source)?;
    if let Some(declaration) = source.predefined_string_declaration() {
        target.register_predefined_string(declaration)?;
    }
    Ok(plan)
}

fn static_definition_milestones(source: dae::DaeView<'_>) -> (Vec<usize>, Vec<(usize, usize)>) {
    let mut before_expressions = Vec::new();
    let mut milestones = Vec::new();
    for (id, variable) in source.variables() {
        if !matches!(
            variable.role(),
            dae::VariableRole::Parameter | dae::VariableRole::Constant
        ) {
            continue;
        }
        let ready = [
            variable.binding(),
            variable.start(),
            variable.minimum(),
            variable.maximum(),
            variable.nominal(),
        ]
        .into_iter()
        .flatten()
        .map(|expression| expression.index() as usize)
        .max();
        match ready {
            Some(expression) => milestones.push((expression, id.index() as usize)),
            None => before_expressions.push(id.index() as usize),
        }
    }
    milestones.sort_unstable();
    (before_expressions, milestones)
}

pub(super) fn construction_failure(error: dae::DaeConstructionError) -> StructuralError {
    match error.source_span() {
        Some(span) => StructuralError::ContractViolation {
            reason: format!("checked index-reduction reconstruction failed: {error}"),
            span,
        },
        None => StructuralError::UnspannedContractViolation {
            reason: format!("checked index-reduction reconstruction failed: {error}"),
        },
    }
}
