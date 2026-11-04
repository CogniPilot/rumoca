//! Borrowed canonical DAE source presentation (SPEC_0041 §7).

mod derivative_projection;

pub use derivative_projection::{SourceDerivativeProjection, SourceDerivativeSelection};

use crate::{
    BinaryOperator, DaeProvenance, DaeView, ExprId, ExpressionOperation, ResidualEquationView,
};

/// Original source operands, or the original complete residual.
#[derive(Clone, Copy, Debug)]
pub enum SourceEquationSides<'dae> {
    Equality {
        left: ExprId<'dae>,
        right: ExprId<'dae>,
    },
    ZeroResidual(ExprId<'dae>),
}

/// Source presentation preserves its branded operands and equation provenance.
#[derive(Clone, Copy, Debug)]
pub struct SourceEquationPresentation<'dae> {
    sides: SourceEquationSides<'dae>,
    provenance: DaeProvenance,
}

impl<'dae> SourceEquationPresentation<'dae> {
    pub fn sides(self) -> SourceEquationSides<'dae> {
        self.sides
    }

    pub fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

impl<'dae> DaeView<'dae> {
    /// Inspect the issued equation without rewriting or evaluating its DAG.
    pub fn source_equation_presentation(
        self,
        equation: ResidualEquationView<'dae>,
    ) -> Option<SourceEquationPresentation<'dae>> {
        let residual = equation.residual();
        let sides = match self.expression(residual)?.operation() {
            ExpressionOperation::Binary {
                operator: BinaryOperator::Subtract,
                lhs,
                rhs,
            } => SourceEquationSides::Equality {
                left: lhs,
                right: rhs,
            },
            _ => SourceEquationSides::ZeroResidual(residual),
        };
        Some(SourceEquationPresentation {
            sides,
            provenance: equation.provenance(),
        })
    }
}
