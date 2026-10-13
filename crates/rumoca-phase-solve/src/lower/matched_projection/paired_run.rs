//! Exact source-order paired projection of one full declaration.

use super::{LoweredLayout, MatchedFamily, checked_storage_run, dae, solve};
use crate::LowerError;
use std::ops::Range;

pub(in crate::lower) struct MatchedDeclarationRowRun<'view, 'dae> {
    family: &'view MatchedFamily<'dae>,
    declaration_index: usize,
    storage: solve::SolveVariableStorageRun,
    y_range: Range<usize>,
}

impl<'dae> MatchedFamily<'dae> {
    pub(in crate::lower) fn full_declaration_row_run(
        &self,
        view: dae::DaeView<'dae>,
        layout: &LoweredLayout<'dae>,
    ) -> Result<Option<MatchedDeclarationRowRun<'_, 'dae>>, LowerError> {
        let Some(context) = self.matching.context() else {
            return Ok(None);
        };
        let Some([rumoca_phase_structural::UnknownId::Algebraic { variable, .. }]) =
            context.declaration_bases()
        else {
            return Ok(None);
        };
        self.declaration_row_run(
            view,
            layout,
            context.source(),
            *variable,
            &self.matching.row_range(),
        )
    }
    pub(in crate::lower) fn declaration_row_run(
        &self,
        view: dae::DaeView<'dae>,
        layout: &LoweredLayout<'dae>,
        source: dae::ContinuousFamilyId<'dae>,
        target: dae::AlgebraicId<'dae>,
        rows: &Range<usize>,
    ) -> Result<Option<MatchedDeclarationRowRun<'_, 'dae>>, LowerError> {
        let Some(context) = self.matching.context() else {
            return Ok(None);
        };
        if context.source() != source
            || self.matching.equations_per_point() != 1
            || self.matching.row_range() != *rows
        {
            return Ok(None);
        }
        let Some(
            [
                rumoca_phase_structural::UnknownId::Algebraic {
                    variable,
                    scalar: 0,
                },
            ],
        ) = context.declaration_bases()
        else {
            return Ok(None);
        };
        let [Some(storage)] = self.storage_runs.as_ref() else {
            return Ok(None);
        };
        if *variable != target || storage.scalar_count != self.matching.point_count() {
            return Ok(None);
        }
        let domain = view.domain(context.domain()).ok_or_else(|| {
            LowerError::contract(
                "paired source domain does not resolve",
                self.matching.span(),
            )
        })?;
        let (current_storage, y_range) =
            checked_storage_run(layout, &self.matching, 0, target, 0, domain.structured())?;
        if current_storage != *storage {
            return Err(LowerError::contract(
                "paired family storage belongs to another layout",
                self.matching.span(),
            ));
        }
        let dense =
            solve::TensorOutputMap::dense_contiguous(0, domain.structured()).map_err(|error| {
                LowerError::contract(
                    format!("paired declaration image: {error:?}"),
                    self.matching.span(),
                )
            })?;
        let steps = self.matching.unknown_steps().first().ok_or_else(|| {
            LowerError::contract(
                "paired source declaration has no stride vector",
                self.matching.span(),
            )
        })?;
        for term in &dense.strides {
            if i64::try_from(term.stride).ok() != steps.get(term.dimension).copied() {
                return Ok(None);
            }
        }
        Ok(Some(MatchedDeclarationRowRun {
            family: self,
            declaration_index: target.index() as usize,
            storage: current_storage,
            y_range,
        }))
    }
}

impl MatchedDeclarationRowRun<'_, '_> {
    pub(in crate::lower) fn declaration_storage(&self) -> (usize, solve::SolveVariableStorageRun) {
        (self.declaration_index, self.storage)
    }
    pub(in crate::lower) fn rows(&self) -> Range<usize> {
        self.family.matching.row_range()
    }

    pub(in crate::lower) fn y_range(&self) -> Range<usize> {
        self.y_range.clone()
    }
}
