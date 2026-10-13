//! Source-owned projection stages and their explicit legacy scalar views.

mod paired_run;
#[cfg(test)]
mod tests;

use super::{AlgebraicBlockMatch, LoweredLayout, variable_scalar_slot};
use crate::LowerError;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::{StructuredScalarBlock, TearingResult, UnknownId};

pub(super) enum MatchedProjection<'dae> {
    Explicit(AlgebraicBlockMatch<'dae>),
    Family(MatchedFamily<'dae>),
}

pub(super) struct MatchedFamily<'dae> {
    matching: StructuredScalarBlock<'dae>,
    storage_runs: Box<[Option<solve::SolveVariableStorageRun>]>,
}

#[derive(Clone, Copy)]
pub(super) enum MatchedBlockView<'view, 'dae> {
    Explicit(&'view AlgebraicBlockMatch<'dae>),
    Singleton {
        row: usize,
        unknown: UnknownId<'dae>,
    },
}

impl<'dae> MatchedProjection<'dae> {
    pub(super) fn visit_scalar_blocks(
        &self,
        mut visit: impl FnMut(MatchedBlockView<'_, 'dae>) -> Result<(), LowerError>,
    ) -> Result<(), LowerError> {
        match self {
            Self::Explicit(block) => visit(MatchedBlockView::Explicit(block)),
            Self::Family(family) => family.visit_scalar_blocks(visit),
        }
    }
}

impl<'dae> MatchedBlockView<'_, 'dae> {
    pub(super) fn match_count(&self) -> usize {
        match self {
            Self::Explicit(block) => block.matches.len(),
            Self::Singleton { .. } => 1,
        }
    }

    pub(super) fn visit_matches(
        self,
        mut visit: impl FnMut(usize, UnknownId<'dae>) -> Result<(), LowerError>,
    ) -> Result<(), LowerError> {
        match self {
            Self::Explicit(block) => {
                for (row, unknown) in &block.matches {
                    visit(*row, *unknown)?;
                }
                Ok(())
            }
            Self::Singleton { row, unknown } => visit(row, unknown),
        }
    }

    pub(super) fn tearing(&self) -> Option<&TearingResult> {
        match self {
            Self::Explicit(block) => block.tearing.as_ref(),
            Self::Singleton { .. } => None,
        }
    }
}

impl<'dae> MatchedFamily<'dae> {
    pub(super) fn all_matches_are_algebraic(&self) -> bool {
        self.storage_runs.iter().all(Option::is_some)
    }

    pub(super) fn matched_unknown(&self, row: usize) -> Option<UnknownId<'dae>> {
        let offset = row.checked_sub(self.matching.first_equation_index())?;
        let width = self.matching.equations_per_point();
        if width == 0 {
            return None;
        }
        let (actual, unknown) = self
            .matching
            .matched_declaration(offset / width, offset % width)?;
        (actual.0 == row).then_some(unknown)
    }

    pub(super) fn matching(&self) -> &StructuredScalarBlock<'dae> {
        &self.matching
    }

    pub(super) fn admit(
        view: dae::DaeView<'dae>,
        layout: &LoweredLayout<'dae>,
        family: &StructuredScalarBlock<'dae>,
    ) -> Result<Option<Self>, LowerError> {
        let Some(context) = family.context() else {
            return Ok(None);
        };
        let Some(bases) = context.declaration_bases() else {
            return Ok(None);
        };
        let domain = view.domain(context.domain()).ok_or_else(|| {
            LowerError::contract("matching source domain does not resolve", family.span())
        })?;
        let storage_runs = bases
            .iter()
            .enumerate()
            .map(|(position, base)| match *base {
                UnknownId::Algebraic { variable, scalar } => checked_storage_run(
                    layout,
                    family,
                    position,
                    variable,
                    scalar,
                    domain.structured(),
                )
                .map(|(storage, _)| Some(storage)),
                UnknownId::Derivative { .. } => Ok(None),
                UnknownId::Solver(_) | UnknownId::Unmatched { .. } => Err(LowerError::contract(
                    "matching declaration certificate contains a non-declaration unknown",
                    family.span(),
                )),
            })
            .collect::<Result<Box<[_]>, LowerError>>()?;
        Ok(Some(Self {
            matching: family.clone(),
            storage_runs,
        }))
    }

    fn visit_scalar_blocks(
        &self,
        mut visit: impl FnMut(MatchedBlockView<'_, 'dae>) -> Result<(), LowerError>,
    ) -> Result<(), LowerError> {
        for point in 0..self.matching.point_count() {
            self.visit_point_blocks(point, &mut visit)?;
        }
        Ok(())
    }

    fn visit_point_blocks(
        &self,
        point: usize,
        visit: &mut impl FnMut(MatchedBlockView<'_, 'dae>) -> Result<(), LowerError>,
    ) -> Result<(), LowerError> {
        for (position, storage) in self.storage_runs.iter().enumerate() {
            let Some(storage) = storage else { continue };
            visit(self.scalar_block(point, position, storage)?)?;
        }
        Ok(())
    }

    fn scalar_block(
        &self,
        point: usize,
        position: usize,
        storage: &solve::SolveVariableStorageRun,
    ) -> Result<MatchedBlockView<'_, 'dae>, LowerError> {
        let (row, unknown) = self
            .matching
            .matched_declaration(point, position)
            .ok_or_else(|| {
                LowerError::contract(
                    "matching family declaration projection is missing",
                    self.matching.span(),
                )
            })?;
        let UnknownId::Algebraic { scalar, .. } = unknown else {
            return Err(LowerError::contract(
                "matching family storage is not bound to an algebraic declaration",
                self.matching.span(),
            ));
        };
        if usize::try_from(scalar)
            .ok()
            .is_none_or(|scalar| scalar >= storage.scalar_count)
        {
            return Err(LowerError::contract(
                "issued family scalar exceeds its storage run",
                self.matching.span(),
            ));
        }
        Ok(MatchedBlockView::Singleton {
            row: row.0,
            unknown,
        })
    }
}

fn checked_storage_run(
    layout: &LoweredLayout<'_>,
    family: &StructuredScalarBlock<'_>,
    position: usize,
    variable: dae::AlgebraicId<'_>,
    scalar: u32,
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<(solve::SolveVariableStorageRun, std::ops::Range<usize>), LowerError> {
    let span = family.span();
    let run = layout
        .solve_layout
        .variable_storage_runs
        .get(variable.index() as usize)
        .copied()
        .ok_or_else(|| LowerError::contract("family declaration has no Solve storage run", span))?;
    let base = variable_scalar_slot(layout, variable.index(), 0, span)?;
    let declaration = layout
        .variables
        .get(variable.index() as usize)
        .ok_or_else(|| LowerError::contract("family declaration has no variable layout", span))?;
    if run.base != base || run.scalar_count != declaration.count {
        return Err(LowerError::contract(
            "family storage run disagrees with its canonical declaration layout",
            span,
        ));
    }
    let typed_declaration = layout
        .solve_layout
        .variable_declarations
        .get(variable.index() as usize)
        .ok_or_else(|| LowerError::contract("family has no canonical Solve declaration", span))?;
    if typed_declaration.role() != run.role || typed_declaration.value_kind() != run.value_kind {
        return Err(LowerError::contract(
            "family storage disagrees with its canonical typed declaration",
            span,
        ));
    }
    let solve::ScalarSlot::Y { index, .. } = base else {
        return Err(LowerError::contract(
            "family declaration storage is not in Y",
            span,
        ));
    };
    let y_end = index.checked_add(run.scalar_count).ok_or_else(|| {
        LowerError::contract("family storage endpoint exceeds target index range", span)
    })?;
    if y_end > layout.solve_layout.solver_scalar_count() {
        return Err(LowerError::contract(
            "family storage run exceeds the checked Solve Y capacity",
            span,
        ));
    }
    let steps = family
        .unknown_steps()
        .get(position)
        .ok_or_else(|| LowerError::contract("matching family body has no stride vector", span))?;
    let strides = steps
        .iter()
        .copied()
        .enumerate()
        .map(|(dimension, step)| {
            isize::try_from(step)
                .map(|stride| solve::AffineStencilIndexStrideTerm { dimension, stride })
                .map_err(|_| {
                    LowerError::contract("family scalar stride exceeds target index range", span)
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let map = solve::TensorOutputMap {
        start: usize::try_from(scalar).map_err(|_| {
            LowerError::contract("family scalar base exceeds target index range", span)
        })?,
        strides,
    };
    let end = map
        .output_count(domain)
        .map_err(|error| LowerError::contract(format!("family scalar image: {error:?}"), span))?;
    if end > run.scalar_count {
        return Err(LowerError::contract(
            "family scalar image exceeds its declaration storage run",
            span,
        ));
    }
    Ok((run, index..y_end))
}
