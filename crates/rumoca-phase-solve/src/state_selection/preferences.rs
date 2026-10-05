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
//! reduced constraint group uses. An integrated `never` value is a requirement:
//! a basis that cannot avoid it is a typed refusal. `prefer` candidates and
//! state ranks are requests: when their formal construction or checked
//! selection is refused, the reducer's basis is kept and the refusal is
//! recorded on the selection.

use std::collections::{BTreeSet, HashMap};

use rumoca_core::StateSelect;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::{
    FormalDerivativeSystem, PreparedDae, StructuralError, construct_formal_derivatives,
};

use super::{
    AlternateSelections, PreparedSelection, basis_names, candidate_basis, prepare_alternate_charts,
    quotient_formal_candidate, select,
};

/// Prepare the basis the source preferences select, or retain `prepared`.
pub(super) fn prefer_or_retain<'source>(
    model: &'source dae::Dae,
    prepared: PreparedDae<'source>,
    overrides: &HashMap<String, f64>,
) -> Result<PreparedSelection<'source>, StructuralError> {
    let Some(request) = PreferenceRequest::of(model, &prepared) else {
        return Ok(PreparedSelection::retained(prepared));
    };
    let formal = match construct_formal_derivatives(model) {
        Ok(formal) => formal,
        Err(error) => return request.refused(prepared, error),
    };
    if !request.admits(&formal) {
        return Ok(PreparedSelection::retained(prepared));
    }
    let mut alternate_selections = AlternateSelections::default();
    let candidate = match formal.construct_state_candidate_with_charts(|formal| {
        let (selection, alternates, _) = select(formal, overrides)?;
        alternate_selections = alternates;
        Ok(selection)
    }) {
        Ok(candidate) => candidate,
        Err(error) => return request.refused(prepared, error),
    };
    let basis = candidate_basis(&candidate)?;
    if !prepared
        .as_dae()
        .inspect(|view| ranks_above(model, &basis, view))
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
        withheld_preferences: None,
    })
}

/// Whether Solve lowering replaces the reducer's manifold-free basis of
/// `prepared` with the basis the source preferences select; the decision
/// [`prefer_or_retain`] takes.
pub(super) fn executes_preferred_basis(
    model: &dae::Dae,
    prepared: &PreparedDae<'_>,
) -> Result<bool, StructuralError> {
    let Some(request) = PreferenceRequest::of(model, prepared) else {
        return Ok(false);
    };
    let selected = construct_formal_derivatives(model).and_then(|formal| {
        if !request.admits(&formal) {
            return Ok(None);
        }
        formal.inspect(|formal| {
            select(formal, &HashMap::new())
                .map(|(_, _, primary)| Some(basis_names(formal.source, &primary)))
        })
    });
    match selected {
        Ok(Some(basis)) => Ok(prepared
            .as_dae()
            .inspect(|view| ranks_above(model, &basis, view))),
        Ok(None) => Ok(false),
        Err(error) if request.required => Err(error),
        Err(_) => Ok(false),
    }
}

/// What the source preferences ask of the reducer's manifold-free basis. An
/// integrated `never` value is a requirement; `prefer` candidates and state
/// ranks are requests (MLS 3.7 §4.9.7.1), so a request whose formal
/// construction or checked selection is refused keeps the reducer's basis and
/// records the refusal, while a refused requirement fails.
struct PreferenceRequest {
    required: bool,
    ranked: bool,
}

impl PreferenceRequest {
    fn of(model: &dae::Dae, prepared: &PreparedDae<'_>) -> Option<Self> {
        if !prepared.inspect(|system| system.manifold.is_empty()) {
            return None;
        }
        let (required, ranked) = model.inspect(|source| {
            prepared
                .as_dae()
                .inspect(|integrated| basis_violations(source, integrated))
        });
        let requested =
            ranked || model.inspect(|source| source.variables().any(unintegrated_prefer));
        (required || requested).then_some(Self { required, ranked })
    }

    /// Whether the formal construction holds a candidate the request ranks:
    /// always for a requirement or a rank violation, and otherwise when an
    /// undifferentiated `prefer` value received a formal successor.
    fn admits(&self, formal: &FormalDerivativeSystem<'_>) -> bool {
        self.required
            || self.ranked
            || formal.inspect(|formal| {
                formal.source.variables().any(|(id, variable)| {
                    unintegrated_prefer((id, variable)) && formal.coordinate(id, 1).is_some()
                })
            })
    }

    fn refused<'source>(
        &self,
        prepared: PreparedDae<'source>,
        error: StructuralError,
    ) -> Result<PreparedSelection<'source>, StructuralError> {
        if self.required {
            return Err(error);
        }
        let mut retained = PreparedSelection::retained(prepared);
        retained.withheld_preferences = Some(error.to_string());
        Ok(retained)
    }
}

/// A continuous Real `prefer` value the source does not differentiate.
fn unintegrated_prefer((_, variable): (dae::VariableId<'_>, dae::VariableView<'_>)) -> bool {
    variable.continuous_state_select() == Some(StateSelect::Prefer)
        && variable.role() != dae::VariableRole::State
}

/// Whether the reducer's basis violates a requirement, integrating a `never`
/// value or omitting an `always` value, and whether it demoted a source state
/// that ranks above one it integrates (MLS 3.7
/// §4.9.7.1 order `never` < `avoid` < `default` < `prefer` < `always`).
fn basis_violations(source: dae::DaeView<'_>, integrated: dae::DaeView<'_>) -> (bool, bool) {
    let kept = integrated
        .variables()
        .filter(|(_, variable)| variable.role() == dae::VariableRole::State)
        .map(|(_, variable)| variable.name().to_string())
        .collect::<BTreeSet<_>>();
    let mut lowest_kept = None::<u8>;
    let mut highest_demoted = None::<u8>;
    let mut always_omitted = false;
    for (_, variable) in source.variables() {
        let Some(selection) = variable.continuous_state_select() else {
            continue;
        };
        always_omitted |=
            selection == StateSelect::Always && !kept.contains(variable.name().as_str());
        if variable.role() != dae::VariableRole::State {
            continue;
        }
        let rank = selection.rank();
        if kept.contains(variable.name().as_str()) {
            lowest_kept = Some(lowest_kept.map_or(rank, |lowest| lowest.min(rank)));
        } else {
            highest_demoted = Some(highest_demoted.map_or(rank, |highest| highest.max(rank)));
        }
    }
    let ranked = matches!(
        (lowest_kept, highest_demoted),
        (Some(kept), Some(demoted)) if demoted > kept
    );
    (lowest_kept == Some(0) || always_omitted, ranked)
}

/// Whether a named basis honors the source `StateSelect` preferences (MLS 3.7
/// §4.9.7.1) strictly better than the scalars `incumbent` integrates: the
/// basis's scalar ranks, sorted from highest, exceed the incumbent's
/// lexicographically. A formal derivative coordinate ranks as its variable. A
/// basis that ranks no higher than the incumbent is no reason to replace it.
pub(super) fn ranks_above(model: &dae::Dae, basis: &[String], incumbent: dae::DaeView<'_>) -> bool {
    let incumbent = incumbent
        .variables()
        .filter(|(_, variable)| variable.role() == dae::VariableRole::State)
        .flat_map(|(_, variable)| variable.scalar_names())
        .collect::<Vec<_>>();
    model.inspect(|source| {
        let ranks = source
            .variables()
            .flat_map(|(_, variable)| {
                let rank = variable
                    .continuous_state_select()
                    .unwrap_or_default()
                    .rank();
                variable.scalar_names().map(move |name| (name, rank))
            })
            .collect::<std::collections::HashMap<_, _>>();
        let profile = |names: &[String]| {
            let mut profile = names
                .iter()
                .map(|name| {
                    ranks
                        .get(variable_scalar_name(name))
                        .copied()
                        .unwrap_or(StateSelect::Default.rank())
                })
                .collect::<Vec<_>>();
            profile.sort_unstable_by(|left, right| right.cmp(left));
            profile
        };
        profile(basis) > profile(&incumbent)
    })
}

/// The variable scalar a basis name integrates: a formal derivative coordinate
/// `der(der(x))` names the scalar `x`.
fn variable_scalar_name(mut name: &str) -> &str {
    while let Some(inner) = name
        .strip_prefix("der(")
        .and_then(|inner| inner.strip_suffix(')'))
    {
        name = inner;
    }
    name
}

/// Whether a named basis integrates an undifferentiated `StateSelect.prefer`
/// value of `model` and ranks above `incumbent`. A retained manifold of
/// conserved first integrals keeps its source coordinates against a selection
/// that only drops lower-ranked differentiated coordinates: reducing it folds
/// where a selected coordinate passes through zero (SPEC_0053 §1).
pub(super) fn integrates_preferred_value(
    model: &dae::Dae,
    basis: &[String],
    incumbent: dae::DaeView<'_>,
) -> bool {
    let preferred = model.inspect(|source| {
        source
            .variables()
            .filter(|&(id, variable)| unintegrated_prefer((id, variable)))
            .flat_map(|(_, variable)| variable.scalar_names())
            .collect::<BTreeSet<_>>()
    });
    basis
        .iter()
        .any(|name| preferred.contains(variable_scalar_name(name)))
        && ranks_above(model, basis, incumbent)
}
