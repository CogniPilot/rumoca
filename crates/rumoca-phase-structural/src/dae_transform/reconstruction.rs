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
use super::semantic_owners::{RebuiltOwnerIdentities, rebuild_semantic_owners};
use super::temporal::{rebuild_clocks, rebuild_delay_coordinates, rebuild_temporal_coordinates};
use super::variables::{
    ReservedVariable, define_static_variables_at, define_variables, reserve_variables,
};
use super::{DirectStateConstraint, HolonomicConstraint};
use crate::StructuralError;

pub(super) fn rebuild_holonomic_constraint(
    model: &dae::Dae,
    constraint: &HolonomicConstraint,
) -> Result<(dae::Dae, Vec<u32>), StructuralError> {
    let mut manifold = Vec::with_capacity(2);
    let rebuilt = model.inspect(|source| {
        dae::Dae::construct(model.source_map().clone(), |target| {
            if let Some(declaration) = source.predefined_string_declaration() {
                target.register_predefined_string(declaration)?;
            }
            let types = rebuild_types(source, target)?;
            let domains = rebuild_domains(source, target)?;
            let mut variables = reserve_variables(source, target, &types, None)?;
            let conditions = reserve_conditions(source, target)?;
            let clocks = rebuild_clocks(source, target, &variables, &conditions)?;
            let temporal = rebuild_temporal_coordinates(source, target, &variables, &clocks)?;
            let facts = DifferentiationFacts::collect(source);
            let mut rebuilt_state = vec![None; source.expression_count()];
            let functions = rebuild_functions(
                source,
                target,
                RebuiltBaseIdentities {
                    types: &types,
                    variables: &variables,
                    domains: &domains,
                    conditions: &conditions,
                    clocks: &clocks,
                    previous: &temporal.previous,
                    terminals: &temporal.terminals,
                },
                &facts,
                None,
                &mut rebuilt_state,
            )?;
            rebuild_delay_coordinates(
                source,
                target,
                RebuiltIdentities {
                    base: RebuiltBaseIdentities {
                        types: &types,
                        variables: &variables,
                        domains: &domains,
                        conditions: &conditions,
                        clocks: &clocks,
                        previous: &temporal.previous,
                        terminals: &temporal.terminals,
                    },
                    functions: &functions,
                },
                &facts,
                None,
                &mut rebuilt_state,
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
                candidate: None,
            };
            let expressions = rebuild_expressions_and_static_variables(
                context,
                target,
                &mut variables,
                &mut rebuilt_state,
            )?;
            let replacement = rebuild_holonomic_replacement(
                context,
                target,
                &variables,
                &mut rebuilt_state,
                constraint,
                &expressions,
                &mut manifold,
            )?;
            define_variables(source, target, &expressions, &mut variables)?;
            rebuild_semantic_owners(
                source,
                target,
                &expressions,
                RebuiltOwnerIdentities {
                    variables: &variables,
                    domains: &domains,
                    conditions: &conditions,
                    clocks: &clocks,
                },
                Some((constraint.residual, replacement)),
            )
        })
    });
    rebuilt
        .map(|dae| (dae, manifold))
        .map_err(construction_failure)
}

pub(super) fn rebuild_with_state_demotion(
    model: &dae::Dae,
    candidate: DirectStateConstraint,
) -> Result<dae::Dae, StructuralError> {
    let rebuilt = model.inspect(|source| {
        dae::Dae::construct(model.source_map().clone(), |target| {
            if let Some(declaration) = source.predefined_string_declaration() {
                target.register_predefined_string(declaration)?;
            }
            let types = rebuild_types(source, target)?;
            let domains = rebuild_domains(source, target)?;
            let mut variables = reserve_variables(source, target, &types, Some(candidate.state))?;
            let conditions = reserve_conditions(source, target)?;
            let clocks = rebuild_clocks(source, target, &variables, &conditions)?;
            let temporal = rebuild_temporal_coordinates(source, target, &variables, &clocks)?;
            let facts = DifferentiationFacts::collect(source);
            let mut rebuilt_state = vec![None; source.expression_count()];
            let functions = rebuild_functions(
                source,
                target,
                RebuiltBaseIdentities {
                    types: &types,
                    variables: &variables,
                    domains: &domains,
                    conditions: &conditions,
                    clocks: &clocks,
                    previous: &temporal.previous,
                    terminals: &temporal.terminals,
                },
                &facts,
                Some(candidate),
                &mut rebuilt_state,
            )?;
            rebuild_delay_coordinates(
                source,
                target,
                RebuiltIdentities {
                    base: RebuiltBaseIdentities {
                        types: &types,
                        variables: &variables,
                        domains: &domains,
                        conditions: &conditions,
                        clocks: &clocks,
                        previous: &temporal.previous,
                        terminals: &temporal.terminals,
                    },
                    functions: &functions,
                },
                &facts,
                Some(candidate),
                &mut rebuilt_state,
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
                candidate: Some(candidate),
            };
            let expressions = rebuild_expressions_and_static_variables(
                context,
                target,
                &mut variables,
                &mut rebuilt_state,
            )?;
            define_variables(source, target, &expressions, &mut variables)?;
            rebuild_semantic_owners(
                source,
                target,
                &expressions,
                RebuiltOwnerIdentities {
                    variables: &variables,
                    domains: &domains,
                    conditions: &conditions,
                    clocks: &clocks,
                },
                None,
            )
        })
    });
    rebuilt.map_err(construction_failure)
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

fn rebuild_holonomic_replacement<'target>(
    context: RebuildContext<'_, '_, 'target>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    rebuilt: &mut [Option<dae::ExprId<'target>>],
    constraint: &HolonomicConstraint,
    expressions: &[dae::ExprId<'target>],
    manifold: &mut Vec<u32>,
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
        let first =
            rebuilder.differentiate_holonomic(source_residual, 1, &constraint.proof, provenance)?;
        let first = rebuilder.materialize_derivative(first, provenance)?;
        let second =
            rebuilder.differentiate_holonomic(source_residual, 2, &constraint.proof, provenance)?;
        manifold.extend([
            expressions[constraint.residual as usize].index(),
            first.index(),
        ]);
        rebuilder.materialize_derivative(second, provenance)
    })
}

fn rebuild_expressions_and_static_variables<'source, 'target>(
    context: RebuildContext<'source, '_, 'target>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &mut [ReservedVariable<'target>],
    rebuilt: &mut [Option<dae::ExprId<'target>>],
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
        rebuild_expression_segment(context, target, variables, rebuilt, next, expression + 1)?;
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
    start: usize,
    end: usize,
) -> Result<(), dae::DaeConstructionError> {
    if start == end {
        return Ok(());
    }
    target.expressions(|expressions| {
        let mut rebuilder = ExpressionRebuilder::new(
            context.source,
            expressions,
            context.identities(variables),
            context.facts,
            context.candidate,
            rebuilt,
        );
        for index in start..end {
            let source_id = context
                .source
                .expression_id(index)
                .expect("finalized expression ordinal resolves");
            rebuilder.rebuild(source_id)?;
        }
        Ok(())
    })
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
