//! One checked reconstruction applying an alias quotient (SPEC_0040 STRUCT-T02).

use rumoca_ir_dae as dae;

use super::super::alias_quotient::{AliasDefinition, AliasPlan, AliasSubstitution};
use super::super::semantic_owners::EquationReplacement;
use super::super::variables::{
    ReservedVariable, TargetVariable, ValueAlias, value_alias_coordinate,
};
use super::{
    DifferentiationFacts, PreparedRebuild, RebuildRequest, RebuiltOwnerIdentities,
    construction_failure, define_variables, prepare_rebuild, rebuild_semantic_owners,
};
use crate::StructuralError;

pub(in super::super) fn rebuild_alias_quotient(
    model: &dae::Dae,
    plan: &AliasPlan,
) -> Result<dae::Dae, StructuralError> {
    model
        .inspect(|source| {
            dae::Dae::construct(model.source_map().clone(), |target| {
                let facts = DifferentiationFacts::collect(source);
                prepare_rebuild(
                    source,
                    target,
                    &facts,
                    RebuildRequest {
                        value_aliases: &plan.substitutions,
                        source_functions_only: true,
                        ..Default::default()
                    },
                    |prepared| {
                        let PreparedRebuild {
                            context,
                            target,
                            variables,
                            expressions,
                            quotients,
                            ..
                        } = prepared;
                        define_variables(source, target, &expressions, variables)?;
                        let definitions =
                            alias_definitions(source, target, variables, &expressions, plan)?;
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
                            &definitions,
                            quotients,
                        )
                    },
                )
            })
        })
        .map_err(construction_failure)
}

/// Attach each eliminated member's representative to its reservation.
pub(super) fn reserve_value_aliases(
    variables: &mut [ReservedVariable<'_>],
    substitutions: &[Option<AliasSubstitution>],
) {
    for (member, substitution) in substitutions.iter().enumerate() {
        let Some(substitution) = substitution else {
            continue;
        };
        let representative = variables[substitution.representative as usize].identity;
        variables[member].value_alias = Some(ValueAlias {
            representative,
            negated: substitution.negated,
        });
    }
}

/// Every spanning-tree edge replacement of the plan, in owner order.
fn alias_definitions<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    expressions: &[dae::ExprId<'target>],
    plan: &AliasPlan,
) -> Result<Vec<EquationReplacement<'target>>, dae::DaeConstructionError> {
    plan.definitions
        .iter()
        .map(|(&owner, definition)| {
            alias_definition(source, target, variables, expressions, owner, *definition)
        })
        .collect()
}

/// The body `member - (±representative)` that replaces one spanning-tree edge
/// at its source owner position, reading both through the member's own access
/// (`m` or `m[binders]`) in that body.
fn alias_definition<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    expressions: &[dae::ExprId<'target>],
    owner_ordinal: usize,
    definition: AliasDefinition,
) -> Result<EquationReplacement<'target>, dae::DaeConstructionError> {
    let reserved = &variables[definition.member as usize];
    let alias = reserved
        .value_alias
        .expect("every defined alias member carries its representative");
    let TargetVariable::Algebraic(member) = reserved.identity else {
        unreachable!("an eliminated alias member is algebraic");
    };
    let access = source
        .expression_id(definition.access as usize)
        .and_then(|id| source.expression(id))
        .expect("alias access resolves in its source");
    let provenance = access.provenance();
    let subscripts = match access.operation() {
        dae::ExpressionOperation::Index { subscripts, .. } => subscripts
            .iter()
            .map(|subscript| match subscript {
                dae::SubscriptView::Index {
                    expression,
                    provenance,
                } => dae::Subscript::Index {
                    expression: expressions[expression.index() as usize],
                    provenance,
                },
                _ => unreachable!("alias accesses index by family binders"),
            })
            .collect::<Vec<_>>(),
        _ => Vec::new(),
    };
    let replacement = target.expressions(|target| {
        let mut read = |coordinate| {
            let value = target.at(provenance).coordinate(coordinate)?;
            if subscripts.is_empty() {
                Ok(value)
            } else {
                target
                    .at(provenance)
                    .index(value, subscripts.iter().cloned())
            }
        };
        let member = read(dae::CoordinateInput::Algebraic(member))?;
        let representative = read(value_alias_coordinate(alias))?;
        let operator = if alias.negated {
            dae::BinaryOperator::Add
        } else {
            dae::BinaryOperator::Subtract
        };
        target
            .at(provenance)
            .binary(operator, member, representative)
    })?;
    Ok(EquationReplacement {
        owner_ordinal,
        body_ordinal: definition.body,
        residual: definition.residual,
        replacement,
    })
}
