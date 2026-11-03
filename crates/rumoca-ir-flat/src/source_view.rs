//! Canonical, target-neutral source equation views (SPEC_0041 §7).

mod residual;
#[cfg(test)]
mod tests;

pub use residual::{SourceEquationPresentation, SourceEquationSides};

use crate::{Equation, EquationOrigin, Model, StructuredEquationFamily};
use rumoca_core::{ComprehensionScalarView, Expression, Span, StructuredIndexBinder};
use std::ops::Range;

/// A source equation or one complete source loop, in partition order.
#[derive(Debug)]
pub enum SourceEquationRow<'model> {
    Equation(&'model Equation),
    Loop(SourceEquationLoop<'model>),
}

/// An exact source family: no scalar placeholders or rendered headers.
#[derive(Debug)]
pub struct SourceEquationLoop<'model> {
    binders: &'model [StructuredIndexBinder],
    body: &'model [Expression],
    span: Span,
    origin: &'model EquationOrigin,
}

impl<'model> SourceEquationLoop<'model> {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn origin(&self) -> &'model EquationOrigin {
        self.origin
    }

    pub fn binders(&self) -> &'model [StructuredIndexBinder] {
        self.binders
    }

    pub fn body(&self) -> &'model [Expression] {
        self.body
    }
}

/// An immutable source partition bound by borrow to its canonical Model.
#[derive(Debug)]
pub struct SourceEquationPartition<'model> {
    rows: Vec<SourceEquationRow<'model>>,
}

impl<'model> SourceEquationPartition<'model> {
    pub fn rows(&self) -> &[SourceEquationRow<'model>] {
        &self.rows
    }
}

/// Source presentation refuses incomplete/ambiguous family metadata.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceEquationViewError {
    UnsupportedFamily { family: usize, span: Span },
    InvalidRange { family: usize, span: Span },
    Overlap { family: usize, span: Span },
}

impl std::fmt::Display for SourceEquationViewError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid source-equation presentation: {self:?}")
    }
}

impl std::error::Error for SourceEquationViewError {}

impl Model {
    pub fn source_equations(&self) -> Result<SourceEquationPartition<'_>, SourceEquationViewError> {
        partition(&self.equations, &self.structured_equations)
    }

    pub fn source_initial_equations(
        &self,
    ) -> Result<SourceEquationPartition<'_>, SourceEquationViewError> {
        partition(&self.initial_equations, &self.initial_structured_equations)
    }
}

fn family_loop<'model>(
    family: &'model StructuredEquationFamily,
    index: usize,
    row_count: usize,
) -> Result<(Range<usize>, SourceEquationLoop<'model>), SourceEquationViewError> {
    let template = family
        .template
        .as_ref()
        .filter(|template| {
            template.scalar_view == ComprehensionScalarView::BinderSubstitution
                && template.body.len() == family.equations_per_point
                && !template.body.is_empty()
                && !family.domain.binders.is_empty()
        })
        .ok_or(SourceEquationViewError::UnsupportedFamily {
            family: index,
            span: family.span,
        })?;
    let range = family
        .materialized_rows()
        .filter(|range| range.start <= range.end && range.end <= row_count)
        .ok_or(SourceEquationViewError::InvalidRange {
            family: index,
            span: family.span,
        })?;
    Ok((
        range,
        SourceEquationLoop {
            binders: &family.domain.binders,
            body: &template.body,
            span: family.span,
            origin: &family.origin,
        },
    ))
}

fn partition<'model>(
    equations: &'model [Equation],
    families: &'model [StructuredEquationFamily],
) -> Result<SourceEquationPartition<'model>, SourceEquationViewError> {
    let mut loops = Vec::new();
    for (index, family) in families.iter().enumerate() {
        if family.interiors_materialized() {
            continue;
        }
        let (range, source_loop) = family_loop(family, index, equations.len())?;
        if !range.is_empty() {
            loops.push((range, index, source_loop));
        }
    }
    loops.sort_by_key(|(range, _, _)| range.start);
    let mut end = 0;
    for (range, index, _) in &loops {
        if range.start < end {
            return Err(SourceEquationViewError::Overlap {
                family: *index,
                span: families[*index].span,
            });
        }
        end = range.end;
    }
    let mut rows = Vec::new();
    let mut cursor = 0;
    for (range, _, source_loop) in loops {
        rows.extend(
            equations[cursor..range.start]
                .iter()
                .map(SourceEquationRow::Equation),
        );
        rows.push(SourceEquationRow::Loop(source_loop));
        cursor = range.end;
    }
    rows.extend(equations[cursor..].iter().map(SourceEquationRow::Equation));
    Ok(SourceEquationPartition { rows })
}
