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

use super::constraints::explicit_derivative_definitions;
use super::declarations::{rebuild_domains, rebuild_types, reserve_conditions};
use super::expressions::{ExpressionRebuilder, RebuiltBaseIdentities, RebuiltIdentities};
use super::functions::rebuild_functions;
use super::semantic_owners::{RebuiltOwnerIdentities, rebuild_semantic_owners};
use super::temporal::{rebuild_clocks, rebuild_delay_coordinates, rebuild_temporal_coordinates};
use super::variables::{define_variables, reserve_variables};
use super::{DirectStateConstraint, HolonomicConstraint};
use crate::StructuralError;

pub(super) fn rebuild_holonomic_constraint(
    model: &dae::Dae,
    constraint: HolonomicConstraint,
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
            let derivative_definitions = explicit_derivative_definitions(source);
            let base_identities = RebuiltBaseIdentities {
                types: &types,
                variables: &variables,
                domains: &domains,
                conditions: &conditions,
                clocks: &clocks,
                previous: &temporal.previous,
                terminals: &temporal.terminals,
            };
            let mut rebuilt_state = vec![None; source.expression_count()];
            let functions = rebuild_functions(
                source,
                target,
                base_identities,
                &derivative_definitions,
                None,
                &mut rebuilt_state,
            )?;
            let identities = RebuiltIdentities {
                base: base_identities,
                functions: &functions,
            };
            rebuild_delay_coordinates(
                source,
                target,
                identities,
                &derivative_definitions,
                None,
                &mut rebuilt_state,
            )?;
            let (expressions, replacement) = target.expressions(|expressions| {
                let mut rebuilder = ExpressionRebuilder::new(
                    source,
                    expressions,
                    identities,
                    &derivative_definitions,
                    None,
                    &mut rebuilt_state,
                );
                let rebuilt = rebuilder.rebuild_all()?;
                let source_residual = source
                    .expression_id(constraint.residual as usize)
                    .expect("holonomic residual resolves");
                let provenance = dae::DaeProvenance::generated(
                    dae::DaeGeneration::IndexReduction,
                    constraint.owner.span(),
                )?;
                let first = rebuilder.differentiate_order(source_residual, 1, provenance)?;
                let first = rebuilder.materialize_derivative(first, provenance)?;
                let second = rebuilder.differentiate_order(source_residual, 2, provenance)?;
                let second = rebuilder.materialize_derivative(second, provenance)?;
                manifold.extend([rebuilt[constraint.residual as usize].index(), first.index()]);
                Ok((rebuilt, second))
            })?;
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
            let derivative_definitions = explicit_derivative_definitions(source);
            let base_identities = RebuiltBaseIdentities {
                types: &types,
                variables: &variables,
                domains: &domains,
                conditions: &conditions,
                clocks: &clocks,
                previous: &temporal.previous,
                terminals: &temporal.terminals,
            };
            let mut rebuilt_state = vec![None; source.expression_count()];
            let functions = rebuild_functions(
                source,
                target,
                base_identities,
                &derivative_definitions,
                Some(candidate),
                &mut rebuilt_state,
            )?;
            let identities = RebuiltIdentities {
                base: base_identities,
                functions: &functions,
            };
            rebuild_delay_coordinates(
                source,
                target,
                identities,
                &derivative_definitions,
                Some(candidate),
                &mut rebuilt_state,
            )?;
            let expressions = target.expressions(|expressions| {
                let mut rebuilder = ExpressionRebuilder::new(
                    source,
                    expressions,
                    identities,
                    &derivative_definitions,
                    Some(candidate),
                    &mut rebuilt_state,
                );
                rebuilder.rebuild_all()
            })?;
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
