//! Exact extension of implicit derivative systems (SPEC_0040 STRUCT-T09).
//!
//! The alias and its defining equality have the complete state tensor shape.
//! Substitution gives a bijection of solutions without selecting a numeric
//! derivative pivot; the ordinary algebraic BLT retains the joint system.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_dae as dae;

use super::variables::{ReservedVariable, TargetVariable};
use super::{ManifoldEntry, PreparedDae, PreparedSystem, structural_analysis, transformed};
use crate::{BltBlock, EquationRef, StructuralError, UnknownId};

pub(super) fn normalize(prepared: PreparedDae<'_>) -> Result<PreparedDae<'_>, StructuralError> {
    let selected = prepared.inspect(implicit_states);
    if selected.is_empty() {
        return Ok(prepared);
    }
    // Rebuilding the derivative aliases reorders nothing in the manifold: the
    // rebuilt expression ordinals come back in the input order, so the redundancy
    // classification is zipped back onto them unchanged.
    let (ids, redundant): (Vec<u32>, Vec<bool>) = match &prepared {
        PreparedDae::Borrowed { .. } => (Vec::new(), Vec::new()),
        PreparedDae::Transformed {
            manifold,
            manifold_redundant,
            ..
        } => (manifold.to_vec(), manifold_redundant.to_vec()),
    };
    let (model, ids) =
        super::reconstruction::rebuild_derivative_aliases(prepared.as_dae(), &selected, &ids)?;
    let manifold = ids
        .into_iter()
        .zip(redundant)
        .map(|(expression, redundant)| ManifoldEntry {
            expression,
            redundant,
        })
        .collect::<Vec<_>>();
    let structural = structural_analysis(&model)?;
    transformed(model, manifold, structural)
}

fn implicit_states(system: PreparedSystem<'_, '_>) -> Vec<u32> {
    let Some(sorted) = system.structural else {
        return Vec::new();
    };
    let definitions =
        existing_derivative_owners(system.view, &sorted.blocks, sorted.matching.iter().copied());
    let tensor_derivatives = tensor_derivative_states(system.view);
    let mut selected = uncovered_tensor_states(system.view, &definitions, &tensor_derivatives);
    for block in &sorted.blocks {
        let BltBlock::AlgebraicLoop { unknowns, .. } = block else {
            continue;
        };
        if !unknowns
            .iter()
            .any(|unknown| matches!(unknown, UnknownId::Algebraic { .. }))
        {
            continue;
        }
        selected.extend(unknowns.iter().filter_map(|unknown| match unknown {
            UnknownId::Derivative { state, .. } => Some(state.index()),
            _ => None,
        }));
    }
    selected.into_iter().collect()
}

/// Extend a singular system before higher differentiation consumes its rates.
/// The partial matching proves only explicit derivative-owner coverage;
/// coupled native blocks require the completed analysis consumed by `normalize`.
pub(super) fn normalize_tensors(model: &dae::Dae) -> Result<Option<dae::Dae>, StructuralError> {
    let selected = model.inspect(|view| {
        let tensor_derivatives = tensor_derivative_states(view);
        if tensor_derivatives.is_empty() {
            return Ok(Vec::new());
        }
        let incidence = crate::incidence::build_incidence(view)?;
        let preferences = crate::explicit_derivative_preferences(view, &incidence);
        let (match_eq, _) = crate::maximum_matching(&incidence, &preferences);
        let matching = match_eq.iter().enumerate().filter_map(|(row, unknown)| {
            unknown.map(|id| (incidence.equation_refs[row], incidence.unknowns[id]))
        });
        let definitions = existing_derivative_owners(view, &[], matching);
        Ok::<_, StructuralError>(
            uncovered_tensor_states(view, &definitions, &tensor_derivatives)
                .into_iter()
                .collect::<Vec<_>>(),
        )
    })?;
    if selected.is_empty() {
        return Ok(None);
    }
    super::reconstruction::rebuild_derivative_aliases(model, &selected, &[])
        .map(|(model, _)| Some(model))
}

fn uncovered_tensor_states(
    view: dae::DaeView<'_>,
    definitions: &BTreeMap<u32, usize>,
    tensor_derivatives: &BTreeSet<u32>,
) -> BTreeSet<u32> {
    view.variables()
        .filter_map(|(id, variable)| {
            (variable.role() == dae::VariableRole::State
                && tensor_derivatives.contains(&id.index())
                && definitions.get(&id.index()).copied().unwrap_or(0) != variable.scalar_count())
            .then_some(id.index())
        })
        .collect()
}

fn tensor_derivative_states(view: dae::DaeView<'_>) -> BTreeSet<u32> {
    let roots = view.continuous_owners().flat_map(|owner| match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => vec![equation.residual()],
        dae::ContinuousOwnerView::Structured { family, .. } => family.bodies().iter().collect(),
    });
    let mut traversal = dae::ExpressionTraversal::new();
    let mut tensors = Vec::new();
    traversal.visit_pruned(view, roots, |expression, node| {
        if node.value_type().is_scalar() {
            return true;
        }
        tensors.push(expression);
        false
    });
    let mut states = BTreeSet::new();
    traversal.visit_pruned(view, tensors, |_, node| {
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) =
            node.operation()
        {
            states.insert(state.index());
        }
        true
    });
    states
}

fn derivative_block_rows(blocks: &[BltBlock<'_>]) -> BTreeSet<usize> {
    let mut covered = BTreeSet::new();
    for block in blocks {
        let BltBlock::AlgebraicLoop {
            equations,
            unknowns,
            ..
        } = block
        else {
            continue;
        };
        if !unknowns
            .iter()
            .all(|unknown| matches!(unknown, UnknownId::Derivative { .. }))
        {
            continue;
        }
        covered.extend(equations.iter().map(|equation| equation.0));
    }
    covered
}

struct ExplicitDerivativeOwner {
    start: usize,
    end: usize,
    targets: Vec<Option<u32>>,
}

fn existing_derivative_owners<'dae>(
    view: dae::DaeView<'dae>,
    blocks: &[BltBlock<'dae>],
    matching: impl Iterator<Item = (EquationRef, UnknownId<'dae>)>,
) -> BTreeMap<u32, usize> {
    let block_rows = derivative_block_rows(blocks);
    let owners = explicit_derivative_owners(view);
    let mut covered = BTreeMap::new();
    for (row, unknown) in matching {
        let UnknownId::Derivative { state, .. } = unknown else {
            continue;
        };
        let owner = &owners[owners.partition_point(|owner| owner.end <= row.0)];
        let body = (row.0 - owner.start) % owner.targets.len();
        if block_rows.contains(&row.0) || owner.targets[body] == Some(state.index()) {
            *covered.entry(state.index()).or_default() += 1;
        }
    }
    covered
}

fn explicit_derivative_owners(view: dae::DaeView<'_>) -> Vec<ExplicitDerivativeOwner> {
    let mut start = 0;
    view.continuous_owners()
        .map(|owner| {
            let (rows, targets) = match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => (
                    1,
                    vec![explicit_derivative_target(view, equation.residual())],
                ),
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    let domain = view
                        .domain(family.domain())
                        .expect("checked family domain resolves");
                    let targets = family
                        .bodies()
                        .iter()
                        .map(|body| explicit_derivative_target(view, body))
                        .collect::<Vec<_>>();
                    (domain.scalar_count() as usize * targets.len(), targets)
                }
            };
            let owner = ExplicitDerivativeOwner {
                start,
                end: start + rows,
                targets,
            };
            start = owner.end;
            owner
        })
        .collect()
}

fn explicit_derivative_target<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<u32> {
    let (lhs, rhs) = crate::residual_normalization::equation_sides(view, residual)?;
    match (
        derivative_projection(view, lhs),
        derivative_projection(view, rhs),
    ) {
        (Some(state), None) | (None, Some(state)) => Some(state),
        _ => None,
    }
}

fn derivative_projection<'dae>(
    view: dae::DaeView<'dae>,
    mut value: dae::ExprId<'dae>,
) -> Option<u32> {
    loop {
        match view.expression(value)?.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) => {
                return Some(state.index());
            }
            dae::ExpressionOperation::Index { base, .. } => value = base,
            _ => return None,
        }
    }
}

pub(super) fn reserve_aliases<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    types: &[dae::ValueTypeId<'target>],
    variables: &mut [ReservedVariable<'target>],
    selected: &[u32],
) -> Result<(), dae::DaeConstructionError> {
    if selected.is_empty() {
        return Ok(());
    }
    let mut names = source
        .variables()
        .map(|(_, variable)| variable.name().clone())
        .collect::<BTreeSet<_>>();
    for &ordinal in selected {
        let variable = source
            .variable_id(ordinal as usize)
            .and_then(|id| source.variable(id))
            .expect("selected derivative belongs to this source DAE");
        assert!(matches!(
            variable.identity(),
            dae::VariableIdentity::State(_)
        ));
        let provenance = alias_provenance(variable)?;
        let name = available_name(variable.name(), &mut names);
        let start = target.expressions(|expressions| {
            super::expressions::shaped_zero(
                expressions,
                variable.value_type().dimensions(),
                provenance,
            )
        })?;
        let alias = target.variables(|target| {
            let (alias, reservation) = target.reserve_algebraic(
                name,
                types[variable.value_type_id().index() as usize],
                provenance,
            )?;
            target.define(
                reservation,
                dae::VariableAttributes {
                    start: Some(start),
                    fixed: Some(vec![false]),
                    origin: dae::VariableOrigin::Generated,
                    ..Default::default()
                },
                provenance,
            )?;
            Ok(alias)
        })?;
        variables[ordinal as usize].derivative_alias = Some(alias);
    }
    Ok(())
}

fn alias_provenance(
    variable: dae::VariableView<'_>,
) -> Result<dae::DaeProvenance, dae::DaeConstructionError> {
    dae::DaeProvenance::generated(
        dae::DaeGeneration::IndexReduction,
        variable.declaration().span(),
    )
}

fn available_name(
    name: &rumoca_core::VarName,
    names: &mut BTreeSet<rumoca_core::VarName>,
) -> rumoca_core::VarName {
    let base = format!("$derivative.{name}");
    let mut candidate = rumoca_core::VarName::new(&base);
    let mut suffix = 0;
    while !names.insert(candidate.clone()) {
        suffix += 1;
        candidate = rumoca_core::VarName::new(format!("{base}.{suffix}"));
    }
    candidate
}

pub(super) fn append_definitions<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
) -> Result<(), dae::DaeConstructionError> {
    for ((_, variable), reserved) in source.variables().zip(variables) {
        let Some(alias) = reserved.derivative_alias else {
            continue;
        };
        let TargetVariable::State(state) = reserved.identity else {
            unreachable!("derivative aliases preserve the selected state role");
        };
        let provenance = alias_provenance(variable)?;
        let residual = target.expressions(|expressions| {
            let derivative = expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let alias = expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::Algebraic(alias))?;
            expressions
                .at(provenance)
                .binary(dae::BinaryOperator::Subtract, derivative, alias)
        })?;
        target.continuous(|equations| equations.value_equation(provenance, residual))?;
    }
    Ok(())
}
