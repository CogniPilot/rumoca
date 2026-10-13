//! Declaration-level coordinate queries a projection walk answers.

use super::dae;

/// A declaration-level coordinate query a cache can recognize.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CoordinateQuery {
    /// The continuous unknowns: algebraic coordinates and state derivatives.
    Unknowns,
}

impl CoordinateQuery {
    pub(super) fn selects(self, coordinate: dae::CoordinateView<'_>) -> bool {
        match self {
            Self::Unknowns => matches!(
                coordinate,
                dae::CoordinateView::Algebraic(_) | dae::CoordinateView::Derivative(_)
            ),
        }
    }
}

/// The declaration-level query one projection walk answers.
#[derive(Clone, Copy)]
pub(super) enum ProjectionQuery<'visit, 'dae> {
    /// A caller predicate, opaque to the cache.
    Predicate(&'visit dyn Fn(dae::CoordinateView<'dae>) -> bool),
    /// A named query, whose answers belong to the expression alone.
    Named(CoordinateQuery),
}

impl<'dae> ProjectionQuery<'_, 'dae> {
    pub(super) fn selects(self, coordinate: dae::CoordinateView<'dae>) -> bool {
        match self {
            Self::Predicate(relevant) => relevant(coordinate),
            Self::Named(query) => query.selects(coordinate),
        }
    }

    pub(super) const fn named(self) -> Option<CoordinateQuery> {
        match self {
            Self::Named(query) => Some(query),
            Self::Predicate(_) => None,
        }
    }
}
