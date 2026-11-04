//! Canonical source selection inside `der`, without numerical differentiation.

use crate::{
    CoordinateView, DaeProvenance, DaeView, ExprId, ExpressionOperation, ExpressionVariability,
    StateId, SubscriptView, SubscriptsView,
};

/// One source selection, with its original subscript expressions.
#[derive(Clone, Copy)]
pub struct SourceDerivativeSelection<'dae> {
    subscripts: SubscriptsView<'dae>,
    parenthesize_base: bool,
}

impl<'dae> SourceDerivativeSelection<'dae> {
    pub fn subscripts(self) -> SubscriptsView<'dae> {
        self.subscripts
    }

    pub fn parenthesize_base(self) -> bool {
        self.parenthesize_base
    }
}

/// A checked source-only path into one derivative-state coordinate.
pub struct SourceDerivativeProjection<'dae> {
    source: ExprId<'dae>,
    state: StateId<'dae>,
    selections: Box<[SourceDerivativeSelection<'dae>]>,
    provenance: DaeProvenance,
}

impl<'dae> SourceDerivativeProjection<'dae> {
    pub fn source(&self) -> ExprId<'dae> {
        self.source
    }

    pub fn state(&self) -> StateId<'dae> {
        self.state
    }

    pub fn selections(&self) -> &[SourceDerivativeSelection<'dae>] {
        &self.selections
    }

    pub fn provenance(&self) -> DaeProvenance {
        self.provenance
    }
}

impl<'dae> DaeView<'dae> {
    /// Issue `der(state[...])` presentation only for time-invariant selections.
    /// Nonconstant/nonparameter subscripts stay outside der in ordinary source
    /// expression presentation. No DAE node or numerical derivative is changed.
    pub fn source_derivative_projection(
        self,
        source: ExprId<'dae>,
    ) -> Option<SourceDerivativeProjection<'dae>> {
        let provenance = self.expression(source)?.provenance();
        let mut current = source;
        let mut selections = Vec::new();
        loop {
            match self.expression(current)?.operation() {
                ExpressionOperation::Coordinate(CoordinateView::Derivative(state)) => {
                    selections.reverse();
                    return Some(SourceDerivativeProjection {
                        source,
                        state,
                        selections: selections.into_boxed_slice(),
                        provenance,
                    });
                }
                ExpressionOperation::Index { base, subscripts }
                    if invariant_subscripts(self, subscripts) =>
                {
                    let parenthesize_base = !matches!(
                        self.expression(base)?.operation(),
                        ExpressionOperation::Coordinate(_)
                    );
                    selections.push(SourceDerivativeSelection {
                        subscripts,
                        parenthesize_base,
                    });
                    current = base;
                }
                _ => return None,
            }
        }
    }
}

fn invariant_subscripts<'dae>(view: DaeView<'dae>, subscripts: SubscriptsView<'dae>) -> bool {
    subscripts.iter().all(|subscript| match subscript {
        SubscriptView::Whole { .. } => true,
        SubscriptView::Index { expression, .. } | SubscriptView::Slice { expression, .. } => {
            view.expression(expression).is_some_and(|expression| {
                matches!(
                    expression.variability(),
                    ExpressionVariability::Constant | ExpressionVariability::Parameter
                )
            })
        }
    })
}
