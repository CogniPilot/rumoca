//! Reuse the root replay for coupled formal derivatives of original equations.

use super::super::formal_derivatives::EquationProlongation;
use super::*;

type RebuiltFormal = (dae::Dae, Vec<Vec<u32>>, Vec<EquationProlongation>);

/// A refused formal rebuild. `owner` names the continuous equation owner whose
/// prolongation was refused, by its first canonical scalar row, with the first
/// derivative level it could not construct; it is `None` for any other refusal.
pub(in crate::dae_transform) struct FormalRebuildRefusal {
    pub owner: Option<(usize, u32)>,
    /// The source expression ordinal of the operation the formal derivative
    /// has no rule for, when that is the refusal.
    pub operation: Option<u32>,
    pub error: StructuralError,
}

pub(in crate::dae_transform) fn rebuild_formal(
    model: &dae::Dae,
    source: dae::DaeView<'_>,
    orders: &[u32],
    equation_orders: &[u32],
) -> Result<RebuiltFormal, Box<FormalRebuildRefusal>> {
    let facts = DifferentiationFacts::collect(source);
    let mut coordinates = Vec::new();
    let mut equations = Vec::new();
    let mut refused = None;
    let rebuilt = construct_replay(model, |target| {
        prepare_rebuild(
            source,
            target,
            &facts,
            RebuildRequest {
                formal_orders: Some(orders),
                ..Default::default()
            },
            |prepared| {
                let PreparedRebuild {
                    context,
                    target,
                    variables,
                    rebuilt_state,
                    quotients,
                    expressions,
                } = prepared;
                coordinates = variables
                    .iter()
                    .map(|variable| {
                        std::iter::once(variable.identity.variable().index())
                            .chain(variable.formal_derivatives.iter().map(|id| id.index()))
                            .collect()
                    })
                    .collect();
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
                equations = append_derivatives(
                    context,
                    target,
                    variables,
                    rebuilt_state,
                    (equation_orders, &mut refused),
                )?;
                Ok(())
            },
        )
    })
    .map_err(|error| {
        let operation = match error {
            dae::DaeConstructionError::IncompleteDefinition { kind, index, .. }
                if kind == crate::dae_transform::differentiation::FORMAL_OPERATION_REFUSAL =>
            {
                Some(index)
            }
            _ => None,
        };
        Box::new(FormalRebuildRefusal {
            owner: refused,
            operation,
            error: construction_failure(error),
        })
    })?;
    Ok((rebuilt, coordinates, equations))
}

fn append_derivatives<'source, 'target>(
    context: RebuildContext<'source, '_, 'target>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    rebuilt_state: &mut [Option<dae::ExprId<'target>>],
    (equation_orders, refused): (&[u32], &mut Option<(usize, u32)>),
) -> Result<Vec<EquationProlongation>, dae::DaeConstructionError> {
    let mut row = 0;
    let mut owned = std::collections::BTreeSet::new();
    let mut next = context.source.continuous_owner_count();
    let mut equations = Vec::with_capacity(next);
    for (original, owner) in context.source.continuous_owners().enumerate() {
        let start = row;
        crate::incidence::projection::visit_owner_rows(context.source, owner, |_| {
            row += 1;
            Ok(())
        })
        .map_err(|_| dae::DaeConstructionError::IncompleteDefinition {
            kind: "formal derivative source rows",
            index: start as u32,
            span: owner_provenance(owner).span(),
        })?;
        let order = if row == start {
            0
        } else {
            equation_orders[start]
        };
        if order > crate::differential_structure::FORMAL_ORDER_PROFILE {
            *refused = Some((
                start,
                crate::differential_structure::FORMAL_ORDER_PROFILE + 1,
            ));
            return Err(dae::DaeConstructionError::IncompleteDefinition {
                kind: "formal derivative order beyond shared differentiation profile",
                index: order,
                span: owner_provenance(owner).span(),
            });
        }
        for level in 1..=order {
            append_owner(
                context,
                target,
                variables,
                rebuilt_state,
                owner,
                (level as u8, &mut owned),
            )
            .inspect_err(|_| *refused = Some((start, level)))?;
        }
        let end = next.checked_add(order as usize).ok_or(
            dae::DaeConstructionError::IncompleteDefinition {
                kind: "formal equation owner count overflow",
                index: order,
                span: owner_provenance(owner).span(),
            },
        )?;
        equations.push(EquationProlongation {
            original,
            derivatives: next..end,
        });
        next = end;
    }
    Ok(equations)
}

fn owner_provenance(owner: dae::ContinuousOwnerView<'_>) -> dae::DaeProvenance {
    match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => equation.provenance(),
        dae::ContinuousOwnerView::Structured { family, .. } => family.provenance(),
    }
}

fn append_owner<'source, 'target>(
    context: RebuildContext<'source, '_, 'target>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    rebuilt_state: &mut [Option<dae::ExprId<'target>>],
    owner: dae::ContinuousOwnerView<'source>,
    (order, owned): (u8, &mut std::collections::BTreeSet<u32>),
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(
        dae::DaeGeneration::IndexReduction,
        owner_provenance(owner).span(),
    )?;
    let bodies = match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => vec![equation.residual()],
        dae::ContinuousOwnerView::Structured { family, .. } => family.bodies().iter().collect(),
    };
    let (derivatives, event_relations) = target.expressions(|expressions| {
        let mut rebuilder = ExpressionRebuilder::new(
            context.source,
            expressions,
            context.identities(variables),
            context.facts,
            None,
            rebuilt_state,
        )
        .with_formal_derivatives();
        let derivatives = bodies
            .iter()
            .map(|&body| {
                let derivative = rebuilder.differentiate_order(body, order, provenance)?;
                rebuilder.materialize_derivative(derivative, body, provenance)
            })
            .collect::<Result<Vec<_>, dae::DaeConstructionError>>()?;
        Ok((derivatives, std::mem::take(&mut rebuilder.event_relations)))
    })?;
    // A derivative that jumps where a relation changes is fixed between that
    // relation's events only when the relation owns one (MLS §8.5).
    for (relation, span) in event_relations {
        if owned.insert(relation.index()) {
            super::loop_guards::own_relation(target, relation, span)?;
        }
    }
    target.continuous(|target| {
        match owner {
            dae::ContinuousOwnerView::Residual { .. } => {
                target.value_equation(provenance, derivatives[0])?;
            }
            dae::ContinuousOwnerView::Structured { family, .. } => {
                target.structured_family(
                    provenance,
                    context.domains[family.domain().index() as usize].id,
                    family.scalar_view(),
                    |target| {
                        derivatives
                            .into_iter()
                            .try_for_each(|body| target.body(body))
                    },
                )?;
            }
        }
        Ok(())
    })
}
