//! MLS 3.7 §4.9.7.1 `StateSelect` priorities on a system the structural reducer
//! accepted without a constrained state manifold.
//!
//! The reducer integrates the differentiated coordinates its demotions leave.
//! That basis is the preferred one unless a `StateSelect.prefer` value outside
//! it holds a formal successor (STRUCT-T07 offset refinement admits one exactly
//! when the monotone closure preserves the formal dimension), a
//! `StateSelect.never` value is integrated, or a demoted source state ranks
//! above an integrated one. Then the formal selection ranks the complete
//! candidate set by the source preferences: a primary basis equal to the
//! reducer's keeps the reducer's system, and any other basis replaces it, with
//! its alternate charts, through the same checked candidate construction a
//! reduced constraint group uses. A `never` value no admissible basis avoids is
//! a typed refusal.

use std::collections::{BTreeSet, HashMap};

use rumoca_core::StateSelect;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::{
    FormalDerivativeSystem, PreparedDae, StructuralError, construct_formal_derivatives,
};

use super::{
    AlternateSelections, PreparedSelection, basis_names, prepare_alternate_charts,
    quotient_formal_candidate, select,
};

/// Prepare the basis the source preferences select, or retain `prepared`.
pub(super) fn prefer_or_retain<'source>(
    model: &'source dae::Dae,
    prepared: PreparedDae<'source>,
    overrides: &HashMap<String, f64>,
) -> Result<PreparedSelection<'source>, StructuralError> {
    let Some(formal) = preference_candidates(model, &prepared)? else {
        return Ok(PreparedSelection::retained(prepared));
    };
    let mut alternate_selections = AlternateSelections::default();
    let mut basis = Vec::new();
    let candidate = formal.construct_state_candidate_with_charts(|formal| {
        let (selection, alternates, primary) = select(formal, overrides)?;
        alternate_selections = alternates;
        basis = basis_names(formal.source, &primary);
        Ok(selection)
    })?;
    if prepared
        .as_dae()
        .inspect(|view| same_integrated_scalars(&basis, view))
    {
        return Ok(PreparedSelection::retained(prepared));
    }
    let alternates = prepare_alternate_charts(&formal, &alternate_selections)?;
    let (primary, formal_aliases) = quotient_formal_candidate(candidate.into_prepared()?)?;
    Ok(PreparedSelection {
        primary,
        alternates,
        exchanges: alternate_selections.exchanges,
        formal_aliases,
        basis: Some(basis),
    })
}

/// Whether Solve lowering replaces the reducer's manifold-free basis of
/// `prepared` with the basis the source preferences select; the decision
/// [`prefer_or_retain`] takes.
pub(super) fn executes_preferred_basis(
    model: &dae::Dae,
    prepared: &PreparedDae<'_>,
) -> Result<bool, StructuralError> {
    let Some(formal) = preference_candidates(model, prepared)? else {
        return Ok(false);
    };
    let basis = formal.inspect(|formal| {
        select(formal, &HashMap::new()).map(|(_, _, primary)| basis_names(formal.source, &primary))
    })?;
    Ok(!prepared
        .as_dae()
        .inspect(|view| same_integrated_scalars(&basis, view)))
}

/// The formal derivatives the preferred selection ranks, or `None` when the
/// reducer's manifold-free basis is already the preferred one.
fn preference_candidates<'model>(
    model: &'model dae::Dae,
    prepared: &PreparedDae<'_>,
) -> Result<Option<FormalDerivativeSystem<'model>>, StructuralError> {
    if !prepared.inspect(|system| system.manifold.is_empty()) {
        return Ok(None);
    }
    let ranked = model.inspect(|source| {
        prepared
            .as_dae()
            .inspect(|integrated| basis_violates_ranks(source, integrated))
    });
    if !ranked && !model.inspect(|source| source.variables().any(unintegrated_prefer)) {
        return Ok(None);
    }
    let formal = construct_formal_derivatives(model)?;
    let admitted = formal.inspect(|formal| {
        formal.source.variables().any(|(id, variable)| {
            unintegrated_prefer((id, variable)) && formal.coordinate(id, 1).is_some()
        })
    });
    Ok((ranked || admitted).then_some(formal))
}

/// A continuous Real `prefer` value the source does not differentiate.
fn unintegrated_prefer((_, variable): (dae::VariableId<'_>, dae::VariableView<'_>)) -> bool {
    continuous_real(variable)
        && variable.state_select() == StateSelect::Prefer
        && variable.role() != dae::VariableRole::State
}

/// Whether the reducer's basis integrates a `never` value, or demoted a source
/// state that ranks above one it integrates (MLS 3.7 §4.9.7.1 order `never` <
/// `avoid` < `default` < `prefer` < `always`).
fn basis_violates_ranks(source: dae::DaeView<'_>, integrated: dae::DaeView<'_>) -> bool {
    let kept = integrated
        .variables()
        .filter(|(_, variable)| variable.role() == dae::VariableRole::State)
        .map(|(_, variable)| variable.name().to_string())
        .collect::<BTreeSet<_>>();
    let mut lowest_kept = None::<u8>;
    let mut highest_demoted = None::<u8>;
    for (_, variable) in source.variables() {
        if variable.role() != dae::VariableRole::State || !continuous_real(variable) {
            continue;
        }
        let rank = rank(variable.state_select());
        if kept.contains(variable.name().as_str()) {
            lowest_kept = Some(lowest_kept.map_or(rank, |lowest| lowest.min(rank)));
        } else {
            highest_demoted = Some(highest_demoted.map_or(rank, |highest| highest.max(rank)));
        }
    }
    lowest_kept == Some(0)
        || matches!((lowest_kept, highest_demoted), (Some(kept), Some(demoted)) if demoted > kept)
}

fn rank(selection: StateSelect) -> u8 {
    match selection {
        StateSelect::Never => 0,
        StateSelect::Avoid => 1,
        StateSelect::Default => 2,
        StateSelect::Prefer => 3,
        StateSelect::Always => 4,
    }
}

fn continuous_real(variable: dae::VariableView<'_>) -> bool {
    variable.variability() == dae::ExpressionVariability::Continuous
        && variable.value_type().scalar_type() == dae::ScalarType::Real
}

/// Whether a named formal basis integrates exactly the scalars `view` holds as
/// states; a formal derivative coordinate is never such a scalar.
fn same_integrated_scalars(basis: &[String], view: dae::DaeView<'_>) -> bool {
    let states = view
        .variables()
        .filter(|(_, variable)| variable.role() == dae::VariableRole::State)
        .flat_map(|(_, variable)| {
            (0..variable.scalar_count()).filter_map(move |scalar| variable.scalar_name(scalar))
        })
        .collect::<BTreeSet<_>>();
    basis.len() == states.len() && basis.iter().all(|name| states.contains(name))
}
