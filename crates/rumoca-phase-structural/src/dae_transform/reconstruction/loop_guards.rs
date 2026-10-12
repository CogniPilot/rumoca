//! Replay a DAE with loop-guarded smooth relations promoted to event owners.

use rumoca_ir_dae as dae;

use super::{
    DifferentiationFacts, PreparedRebuild, RebuildRequest, RebuiltOwnerIdentities,
    construct_replay, construction_failure, define_variables, prepare_rebuild,
    rebuild_semantic_owners,
};
use crate::StructuralError;

/// Rebuild `model` unchanged, then give each relation in `guards` (source
/// expression ordinals) the relation, condition, and root an ordinary state
/// relation owns. Returns the rebuilt DAE and the replayed manifold ordinals.
pub(in crate::dae_transform) fn rebuild_loop_guards(
    model: &dae::Dae,
    guards: &[u32],
    prior_manifold: &[u32],
) -> Result<(dae::Dae, Vec<u32>), StructuralError> {
    let mut manifold = Vec::with_capacity(prior_manifold.len());
    let rebuilt = model.inspect(|source| {
        construct_replay(model, |target| {
            let facts = DifferentiationFacts::collect(source);
            prepare_rebuild(
                source,
                target,
                &facts,
                RebuildRequest::default(),
                |prepared| {
                    let PreparedRebuild {
                        context,
                        target,
                        variables,
                        expressions,
                        quotients,
                        ..
                    } = prepared;
                    manifold.extend(
                        prior_manifold
                            .iter()
                            .map(|&id| expressions[id as usize].index()),
                    );
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
                        &[],
                        quotients,
                    )?;
                    own_state_relations(source, target, &expressions, guards)
                },
            )
        })
    });
    rebuilt
        .map(|model| (model, manifold))
        .map_err(construction_failure)
}

fn own_state_relations<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    guards: &[u32],
) -> Result<(), dae::DaeConstructionError> {
    for &guard in guards {
        own_state_relation(source, target, expressions[guard as usize], guard)?;
    }
    Ok(())
}

/// The owner phase-dae construction gives a state relation
/// (`lower_state_relation_root`): the relation, a condition defined by it, and
/// the root that condition activates.
fn own_state_relation<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    relation: dae::ExprId<'target>,
    source_ordinal: u32,
) -> Result<(), dae::DaeConstructionError> {
    let span = source
        .expression_id(source_ordinal as usize)
        .and_then(|id| source.expression(id))
        .map(|expression| expression.provenance().span())
        .expect("a loop-guard ordinal names a finalized expression");
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let relation = target.conditions(|conditions| conditions.relation(relation, provenance))?;
    let condition = target.conditions(|conditions| conditions.reserve(provenance))?;
    target.conditions(|conditions| {
        conditions.define(
            condition,
            dae::ConditionInput::Relation(relation),
            provenance,
        )
    })?;
    target
        .conditions(|conditions| conditions.root(relation, condition, provenance))
        .map(|_| ())
}
