//! One checked reconstruction folding evaluable parameters (SPEC_0040
//! STRUCT-T10(a)).

use std::sync::Arc;

use rumoca_ir_dae as dae;

use super::super::evaluable_parameters::FoldedValue;
use super::{
    DifferentiationFacts, PreparedRebuild, RebuildRequest, RebuiltOwnerIdentities,
    construction_failure, define_variables, prepare_rebuild, rebuild_semantic_owners,
};
use crate::StructuralError;

pub(in super::super) fn rebuild_folded_parameters(
    model: &dae::Dae,
    plan: &[Option<Arc<FoldedValue>>],
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
                        folded_parameters: plan,
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
