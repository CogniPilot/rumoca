//! One checked reconstruction inlining admitted annotated calls (SPEC_0040
//! STRUCT-T10(b)).

use rumoca_ir_dae as dae;

use super::{
    DifferentiationFacts, PreparedRebuild, RebuildRequest, RebuiltOwnerIdentities,
    construction_failure, define_variables, prepare_rebuild, rebuild_semantic_owners,
};
use crate::StructuralError;

/// Rebuild `model` with every flagged call replaced by its substituted body,
/// replaying the retained manifold expressions onto the rebuilt arena.
pub(in super::super) fn rebuild_inlined_calls(
    model: &dae::Dae,
    plan: &[bool],
    prior_manifold: &[u32],
) -> Result<(dae::Dae, Vec<u32>), StructuralError> {
    let mut manifold = Vec::with_capacity(prior_manifold.len());
    model
        .inspect(|source| {
            dae::Dae::construct(model.source_map().clone(), |target| {
                let facts = DifferentiationFacts::collect(source);
                prepare_rebuild(
                    source,
                    target,
                    &facts,
                    RebuildRequest {
                        inline_calls: plan,
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
                        )
                    },
                )
            })
        })
        .map(|model| (model, manifold))
        .map_err(construction_failure)
}
