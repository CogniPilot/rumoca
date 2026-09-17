//! Checked coordinate transformations; numerical regularity remains an execution obligation.

use super::*;
use rumoca_core::StateSelect;

/// A selected scalar view and its successor from one formal derivative root.
#[derive(Clone, Copy)]
pub struct FormalStateCoordinate<'formal> {
    value: dae::VariableId<'formal>,
    successor: dae::VariableId<'formal>,
    scalar: u32,
}

#[derive(Clone, Copy)]
pub(in crate::dae_transform) struct SelectedCoordinate {
    pub value: u32,
    pub successor: u32,
    pub scalar: u32,
}

/// The product of a reduced state selection: the primary independent basis and
/// the bounded set of admissible alternate charts for a folding definitional
/// first-integral coordinate group. The charts name their coordinates by the
/// formal value variable index and scalar; [`FormalDerivativeSystem::construct_state_candidate`]
/// rebinds them onto the finalized transformed DAE.
pub struct StateSelection<'formal> {
    pub coordinates: Vec<FormalStateCoordinate<'formal>>,
    pub charts: Vec<ReducedSelectionChart>,
}

/// One admissible reduced chart in formal value-variable index space. The
/// `dependent` coordinates are reconstructed and the `independent` coordinates
/// are integrated; each entry is a `(formal value variable index, scalar)` pair.
pub struct ReducedSelectionChart {
    pub dependent: Vec<(u32, u32)>,
    pub independent: Vec<(u32, u32)>,
    pub trial_rcond: f64,
    pub trial_singular_threshold: f64,
}

/// A checked coordinate transformation retaining the complete source equations.
pub struct FormalStateCandidate<'system, 'source> {
    formal: &'system FormalDerivativeSystem<'source>,
    model: dae::Dae,
    variables: Vec<u32>,
    state: Option<u32>,
    selection: Vec<SelectedCoordinate>,
    charts: Vec<super::super::PreparedReducedChart>,
    structural: super::super::PreparedStructuralAnalysis,
}

pub struct FormalStateCandidateView<'map, 'source, 'formal, 'target> {
    pub formal: FormalDerivativeView<'map, 'source, 'formal>,
    pub view: dae::DaeView<'target>,
    variables: &'map [u32],
    state: Option<u32>,
    selection: &'map [SelectedCoordinate],
}

impl<'source, 'formal> FormalDerivativeView<'_, 'source, 'formal> {
    /// Select a source scalar at a formal order with an existing successor.
    pub fn state_coordinate(
        &self,
        variable: dae::VariableId<'source>,
        order: usize,
        scalar: u32,
    ) -> Result<FormalStateCoordinate<'formal>, StructuralError> {
        let source = self
            .source
            .variable(variable)
            .expect("branded source coordinate");
        if source.value_type().is_record()
            || source.value_type().scalar_type() != dae::ScalarType::Real
            || scalar as usize >= source.scalar_count()
        {
            return Err(candidate_error(
                "state coordinate requires an in-bounds Real tensor scalar",
            ));
        }
        if order == 0 && source.state_select() == StateSelect::Never {
            return Err(candidate_error(
                "state coordinate conflicts with StateSelect.never",
            ));
        }
        let next = order
            .checked_add(1)
            .ok_or_else(|| candidate_error("state derivative order overflows"))?;
        let (Some(value), Some(successor)) = (
            self.coordinate(variable, order),
            self.coordinate(variable, next),
        ) else {
            return Err(candidate_error(
                "state coordinate has no formal derivative successor",
            ));
        };
        Ok(FormalStateCoordinate {
            value,
            successor,
            scalar,
        })
    }
}

impl<'source> FormalDerivativeSystem<'source> {
    pub fn construct_state_candidate(
        &self,
        select: impl for<'s, 'f> FnOnce(
            FormalDerivativeView<'_, 's, 'f>,
        )
            -> Result<Vec<FormalStateCoordinate<'f>>, StructuralError>,
    ) -> Result<FormalStateCandidate<'_, 'source>, StructuralError> {
        self.construct_state_candidate_with_charts(|view| {
            Ok(StateSelection {
                coordinates: select(view)?,
                charts: Vec::new(),
            })
        })
    }

    /// Like [`Self::construct_state_candidate`], but the selection also issues a
    /// bounded set of admissible reduced charts for a folding definitional
    /// first-integral coordinate group. The charts are carried onto the finalized
    /// candidate and, through [`FormalStateCandidate::into_prepared`], onto the
    /// prepared DAE; they change no primary basis.
    pub fn construct_state_candidate_with_charts(
        &self,
        select: impl for<'s, 'f> FnOnce(
            FormalDerivativeView<'_, 's, 'f>,
        ) -> Result<StateSelection<'f>, StructuralError>,
    ) -> Result<FormalStateCandidate<'_, 'source>, StructuralError> {
        let (selection, charts) = self.inspect(|view| {
            let chosen = select(view)?;
            check_selection(&view, &chosen.coordinates)?;
            let selection = chosen
                .coordinates
                .into_iter()
                .map(|coordinate| SelectedCoordinate {
                    value: coordinate.value.index(),
                    successor: coordinate.successor.index(),
                    scalar: coordinate.scalar,
                })
                .collect::<Vec<_>>();
            Ok((selection, chosen.charts))
        })?;
        let (model, variables, state) =
            super::super::reconstruction::rebuild_state_candidate(&self.model, &selection)?;
        let charts = charts
            .into_iter()
            .map(|chart| rebind_chart(&variables, chart))
            .collect::<Result<Vec<_>, _>>()?;
        let structural = super::super::structural_analysis(&model)?;
        Ok(FormalStateCandidate {
            formal: self,
            model,
            variables,
            state,
            selection,
            charts,
            structural,
        })
    }
}

/// Rebind a reduced chart's coordinates from formal value-variable indices onto
/// the finalized transformed DAE's variable ordinals.
fn rebind_chart(
    variables: &[u32],
    chart: ReducedSelectionChart,
) -> Result<super::super::PreparedReducedChart, StructuralError> {
    let rebind = |coordinates: Vec<(u32, u32)>| {
        coordinates
            .into_iter()
            .map(|(formal, scalar)| {
                variables
                    .get(formal as usize)
                    .copied()
                    .map(|ordinal| (ordinal, scalar))
                    .ok_or_else(|| {
                        candidate_error("reduced chart coordinate has no transformed owner")
                    })
            })
            .collect::<Result<Vec<_>, _>>()
    };
    Ok(super::super::PreparedReducedChart {
        dependent: rebind(chart.dependent)?.into_boxed_slice(),
        independent: rebind(chart.independent)?.into_boxed_slice(),
        trial_rcond: chart.trial_rcond,
        trial_singular_threshold: chart.trial_singular_threshold,
    })
}

fn check_selection<'formal>(
    view: &FormalDerivativeView<'_, '_, 'formal>,
    chosen: &[FormalStateCoordinate<'formal>],
) -> Result<(), StructuralError> {
    if chosen.len() != view.formal_dimension() {
        return Err(candidate_error(
            "state coordinate count differs from the formal dimension",
        ));
    }
    let mut selected = BTreeSet::new();
    for coordinate in chosen {
        if !selected.insert((coordinate.value.index(), coordinate.scalar)) {
            return Err(candidate_error(
                "state coordinate selection repeats a scalar",
            ));
        }
    }
    // Only a genuine state variable is a forced `StateSelect.always` request. An
    // algebraic or output coordinate has no independent integration slot, so it
    // is structurally determined and MLS 3.6 §4.8.8 permits demoting it; the
    // selection releases such a coordinate to a dependent slot when it cannot be
    // an independent state, and this obligation must not require it back.
    for (id, source) in view.source.variables() {
        if source.state_select() != StateSelect::Always
            || source.variability() != dae::ExpressionVariability::Continuous
            || source.role() != dae::VariableRole::State
        {
            continue;
        }
        let value = view.coordinate(id, 0).expect("source value is retained");
        if (0..source.scalar_count())
            .any(|scalar| !selected.contains(&(value.index(), scalar as u32)))
        {
            return Err(candidate_error(
                "state coordinate selection omits StateSelect.always",
            ));
        }
    }
    Ok(())
}

impl FormalStateCandidate<'_, '_> {
    /// Prepare the exact coordinate transformation. Like every `PreparedDae`,
    /// this establishes structural ownership; runtime initialization and
    /// numerical reconstruction must still establish regularity before use.
    pub fn into_prepared(self) -> Result<super::super::PreparedDae<'static>, StructuralError> {
        super::super::transformed(
            self.model,
            Vec::new(),
            self.structural,
            self.charts.into_boxed_slice(),
        )
    }

    pub fn inspect<R>(
        &self,
        inspect: impl for<'s, 'f, 't> FnOnce(FormalStateCandidateView<'_, 's, 'f, 't>) -> R,
    ) -> R {
        self.formal.inspect(|formal| {
            self.model.inspect(|view| {
                inspect(FormalStateCandidateView {
                    formal,
                    view,
                    variables: &self.variables,
                    state: self.state,
                    selection: &self.selection,
                })
            })
        })
    }
}

impl<'source, 'target> FormalStateCandidateView<'_, 'source, '_, 'target> {
    pub fn coordinate(
        &self,
        source: dae::VariableId<'source>,
        order: usize,
    ) -> Option<dae::VariableId<'target>> {
        let formal = self.formal.coordinate(source, order)?;
        self.view
            .variable_id(self.variables[formal.index() as usize] as usize)
    }

    pub fn state(&self) -> Option<dae::VariableId<'target>> {
        self.state.and_then(|id| self.view.variable_id(id as usize))
    }

    /// Value, successor, and row-major scalar offset for one integration slot.
    pub fn projection(
        &self,
        slot: usize,
    ) -> Option<(dae::VariableId<'target>, dae::VariableId<'target>, u32)> {
        let selected = self.selection.get(slot)?;
        Some((
            self.view
                .variable_id(self.variables[selected.value as usize] as usize)?,
            self.view
                .variable_id(self.variables[selected.successor as usize] as usize)?,
            selected.scalar,
        ))
    }
}

fn candidate_error(reason: &str) -> StructuralError {
    StructuralError::UnspannedContractViolation {
        reason: reason.into(),
    }
}
