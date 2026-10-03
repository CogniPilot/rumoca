//! One checked reconstruction declaring inert states algebraic (SPEC_0040
//! STRUCT-T04).

use rumoca_ir_dae as dae;

use super::{
    DifferentiationFacts, PreparedRebuild, RebuildRequest, RebuiltOwnerIdentities,
    construction_failure, define_variables, prepare_rebuild, rebuild_semantic_owners,
};
use crate::StructuralError;

pub(in super::super) fn rebuild_inert_states(
    model: &dae::Dae,
    inert: &[u32],
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
                        inert_states: inert,
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
        .map_err(construction_failure)
}
