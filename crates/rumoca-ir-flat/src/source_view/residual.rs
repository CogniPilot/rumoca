//! Source equation presentation; no numerical expression transformation.

#[cfg(test)]
mod tests;

use crate::{Equation, EquationOrigin};
use rumoca_core::{Expression, OpBinary, Span};

/// The sides selected by the canonical source owner, in original order.
#[derive(Clone, Copy, Debug)]
pub enum SourceEquationSides<'equation> {
    Equality {
        left: &'equation Expression,
        right: &'equation Expression,
    },
    ZeroResidual(&'equation Expression),
}

/// A borrowed presentation tied to one source equation and its provenance.
#[derive(Debug)]
pub struct SourceEquationPresentation<'equation> {
    sides: SourceEquationSides<'equation>,
    span: Span,
    origin: &'equation EquationOrigin,
}

impl<'equation> SourceEquationPresentation<'equation> {
    pub fn sides(&self) -> SourceEquationSides<'equation> {
        self.sides
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn origin(&self) -> &'equation EquationOrigin {
        self.origin
    }
}

impl Equation {
    /// Issue source equality presentation without changing the residual AST.
    pub fn source_presentation(&self) -> SourceEquationPresentation<'_> {
        let sides = match &self.residual {
            Expression::Binary {
                op: OpBinary::Sub | OpBinary::SubElem,
                lhs,
                rhs,
                ..
            } => SourceEquationSides::Equality {
                left: lhs,
                right: rhs,
            },
            residual => SourceEquationSides::ZeroResidual(residual),
        };
        SourceEquationPresentation {
            sides,
            span: self.span,
            origin: &self.origin,
        }
    }
}
