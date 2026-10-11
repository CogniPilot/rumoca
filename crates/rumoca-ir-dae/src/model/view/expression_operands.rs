//! Read-only operand shapes; consumers carry the selected shape into iteration.

use super::{ExprId, ExpressionOperands};

#[derive(Clone, Copy)]
pub struct ConditionalExpressionOperands<'dae> {
    branches: &'dae [[u32; 2]],
    fallback: ExprId<'dae>,
}

impl<'dae> ConditionalExpressionOperands<'dae> {
    pub fn branch_count(self) -> usize {
        self.branches.len()
    }

    pub fn branches(self) -> impl ExactSizeIterator<Item = (ExprId<'dae>, ExprId<'dae>)> {
        self.branches
            .iter()
            .map(|[condition, value]| (ExprId::from_raw(*condition), ExprId::from_raw(*value)))
    }

    pub fn fallback(self) -> ExprId<'dae> {
        self.fallback
    }
}

impl<'dae> ExpressionOperands<'dae> {
    /// Project exactly two operands without retaining optional ordinal reads.
    pub fn binary(self) -> Option<[ExprId<'dae>; 2]> {
        let [lhs, rhs] = self.raw else {
            return None;
        };
        Some([ExprId::from_raw(*lhs), ExprId::from_raw(*rhs)])
    }

    /// Project condition/value pairs and the mandatory fallback once.
    pub fn conditional(self) -> Option<ConditionalExpressionOperands<'dae>> {
        let (&fallback, branches) = self.raw.split_last()?;
        let (branches, remainder) = branches.as_chunks::<2>();
        if branches.is_empty() || !remainder.is_empty() {
            return None;
        }
        Some(ConditionalExpressionOperands {
            branches,
            fallback: ExprId::from_raw(fallback),
        })
    }
}
