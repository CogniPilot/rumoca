//! Declaration context retained by the sole checked BLT family issuer.

use super::StructuredScalarBlock;
use crate::dae_transform::{ErasedUnknown, PreparedBinding};
use crate::incidence::{Incidence, StructuredMatchingFamily};
use crate::types::UnknownId;
use rumoca_ir_dae as dae;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct StructuredFamilyContext<'dae> {
    source: dae::ContinuousFamilyId<'dae>,
    domain: dae::DomainId<'dae>,
    declaration_bases: Option<Box<[UnknownId<'dae>]>>,
}

impl<'dae> StructuredFamilyContext<'dae> {
    pub fn source(&self) -> dae::ContinuousFamilyId<'dae> {
        self.source
    }

    pub fn domain(&self) -> dae::DomainId<'dae> {
        self.domain
    }

    pub fn declaration_bases(&self) -> Option<&[UnknownId<'dae>]> {
        self.declaration_bases.as_deref()
    }
}

pub(super) fn checked_source<'dae>(
    view: dae::DaeView<'dae>,
    matching: &StructuredMatchingFamily,
    incidence: &Incidence<'dae>,
    declarations: Option<Box<[UnknownId<'dae>]>>,
) -> Option<StructuredFamilyContext<'dae>> {
    let owner = matching.source_owner_index;
    let range = matching.row_range()?;
    if incidence.owner_first_row.get(owner).copied() != Some(range.start)
        || incidence
            .owner_first_row
            .get(owner.checked_add(1)?)
            .copied()
            != Some(range.end)
    {
        return None;
    }
    let dae::ContinuousOwnerView::Structured { family, .. } = view.continuous_owner(owner)? else {
        return None;
    };
    let domain = view.domain(family.domain())?;
    if family.bodies().len() != matching.equations_per_point
        || usize::try_from(family.scalar_rows()).ok()? != matching.row_count()?
        || !domain
            .extents()
            .iter()
            .copied()
            .map(usize::try_from)
            .eq(matching.extents.iter().copied().map(Ok))
    {
        return None;
    }
    Some(bind_source(view, matching, declarations))
}

pub(super) fn bind_source<'dae>(
    view: dae::DaeView<'dae>,
    matching: &StructuredMatchingFamily,
    declaration_bases: Option<Box<[UnknownId<'dae>]>>,
) -> StructuredFamilyContext<'dae> {
    let Some(dae::ContinuousOwnerView::Structured { id, family }) =
        view.continuous_owner(matching.source_owner_index)
    else {
        unreachable!("checked family retains its actual structured source owner")
    };
    StructuredFamilyContext {
        source: id,
        domain: family.domain(),
        declaration_bases,
    }
}

pub(super) fn base_declarations<'dae>(
    family: &StructuredMatchingFamily,
    incidence: &Incidence<'dae>,
) -> Option<Box<[UnknownId<'dae>]>> {
    family
        .base_unknowns
        .iter()
        .map(|index| {
            let declaration = *incidence.unknowns.get(*index)?;
            matches!(
                declaration,
                UnknownId::Derivative { .. } | UnknownId::Algebraic { .. }
            )
            .then_some(declaration)
        })
        .collect()
}

pub(super) fn matches_declaration<'dae>(
    family: &StructuredMatchingFamily,
    incidence: &Incidence<'dae>,
    bases: &[UnknownId<'dae>],
    index: usize,
    position: usize,
) -> bool {
    project_declaration(family, bases, index, position)
        .is_some_and(|projected| incidence.unknowns.get(index).copied() == Some(projected))
}

fn project_declaration<'dae>(
    family: &StructuredMatchingFamily,
    bases: &[UnknownId<'dae>],
    index: usize,
    position: usize,
) -> Option<UnknownId<'dae>> {
    let base = *bases.get(position)?;
    let base_index = *family.base_unknowns.get(position)?;
    let displacement = i64::try_from(index)
        .ok()?
        .checked_sub(i64::try_from(base_index).ok()?)?;
    let scalar = |base: u32| {
        i64::from(base)
            .checked_add(displacement)
            .and_then(|scalar| u32::try_from(scalar).ok())
    };
    match base {
        UnknownId::Derivative {
            state,
            scalar: base,
        } => Some(UnknownId::Derivative {
            state,
            scalar: scalar(base)?,
        }),
        UnknownId::Algebraic {
            variable,
            scalar: base,
        } => Some(UnknownId::Algebraic {
            variable,
            scalar: scalar(base)?,
        }),
        UnknownId::Solver(_) | UnknownId::Unmatched { .. } => None,
    }
}

impl<'dae> StructuredScalarBlock<'dae> {
    pub fn matched_declaration(
        &self,
        point: usize,
        position: usize,
    ) -> Option<(crate::EquationRef, UnknownId<'dae>)> {
        let bases = self.context.as_ref()?.declaration_bases.as_deref()?;
        let (row, index) = self.matching.candidate(point, position)?;
        Some((
            crate::EquationRef(row),
            project_declaration(&self.matching, bases, index, position)?,
        ))
    }
}

#[derive(Clone)]
pub(crate) struct ErasedStructuredScalarBlock {
    matching: Arc<StructuredMatchingFamily>,
    declaration_bases: Option<Box<[ErasedUnknown]>>,
    has_context: bool,
}

impl StructuredScalarBlock<'_> {
    pub(crate) fn erase(self) -> ErasedStructuredScalarBlock {
        ErasedStructuredScalarBlock {
            matching: self.matching,
            has_context: self.context.is_some(),
            declaration_bases: self
                .context
                .and_then(|context| context.declaration_bases)
                .map(|bases| bases.iter().copied().map(ErasedUnknown::erase).collect()),
        }
    }
}

impl ErasedStructuredScalarBlock {
    pub(crate) fn bind<'dae>(&self, binding: PreparedBinding<'dae>) -> StructuredScalarBlock<'dae> {
        let view = binding.view();
        let declarations = self
            .declaration_bases
            .as_ref()
            .map(|bases| bases.iter().copied().map(|base| base.bind(view)).collect());
        StructuredScalarBlock {
            matching: self.matching.clone(),
            context: self
                .has_context
                .then(|| bind_source(view, &self.matching, declarations)),
        }
    }
}
