use super::*;
use rumoca_core::Reference;

pub(super) struct Affine {
    pub(super) constant: i128,
    pub(super) terms: Vec<(Reference, i128)>,
    pub(super) bounds: (i64, i64),
}

impl Affine {
    pub(super) fn checked(expression: &Expression, shapes: &ShapeEnvironment) -> Option<Self> {
        match expression {
            Expression::Literal {
                value: Literal::Integer(value),
                ..
            } => Some(Self::constant(*value)),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                if name.resolved_function().is_some()
                    || name.component_ref().is_some_and(|reference| {
                        reference.parts().len() != 1
                            || reference.parts().iter().any(|part| !part.subs.is_empty())
                    })
                {
                    return None;
                }
                if let Some(value) = shapes.slice_constant(name) {
                    return Some(Self::constant(value));
                }
                if name
                    .instance_id()
                    .is_some_and(|instance| !shapes.slice_reference_scope(instance))
                {
                    return None;
                }
                let bounds = shapes.slice_binder_bounds(name.var_name())?;
                Some(Self {
                    constant: 0,
                    terms: vec![(name.clone(), 1)],
                    bounds,
                })
            }
            Expression::Unary {
                op: OpUnary::Plus,
                rhs,
                ..
            } => Self::checked(rhs, shapes),
            Expression::Unary {
                op: OpUnary::Minus,
                rhs,
                ..
            } => {
                let mut value = Self::checked(rhs, shapes)?;
                value.scale(-1)?;
                Some(value)
            }
            Expression::Binary { op, lhs, rhs, .. } => {
                let left = Self::checked(lhs, shapes)?;
                let right = Self::checked(rhs, shapes)?;
                Self::binary(op, left, right)
            }
            _ => None,
        }
    }

    fn constant(value: i64) -> Self {
        Self {
            constant: i128::from(value),
            terms: Vec::new(),
            bounds: (value, value),
        }
    }

    fn binary(op: &OpBinary, mut left: Self, mut right: Self) -> Option<Self> {
        match op {
            OpBinary::Add | OpBinary::AddElem => {
                left.bounds = (
                    left.bounds.0.checked_add(right.bounds.0)?,
                    left.bounds.1.checked_add(right.bounds.1)?,
                );
                left.add_terms(right, 1)?;
            }
            OpBinary::Sub | OpBinary::SubElem => {
                left.bounds = (
                    left.bounds.0.checked_sub(right.bounds.1)?,
                    left.bounds.1.checked_sub(right.bounds.0)?,
                );
                left.add_terms(right, -1)?;
            }
            OpBinary::Mul | OpBinary::MulElem if right.terms.is_empty() => {
                left.scale(i64::try_from(right.constant).ok()?)?;
            }
            OpBinary::Mul | OpBinary::MulElem if left.terms.is_empty() => {
                right.scale(i64::try_from(left.constant).ok()?)?;
                return Some(right);
            }
            _ => return None,
        }
        Some(left)
    }

    fn scale(&mut self, scale: i64) -> Option<()> {
        let low = self.bounds.0.checked_mul(scale)?;
        let high = self.bounds.1.checked_mul(scale)?;
        self.bounds = (low.min(high), low.max(high));
        self.constant = self.constant.checked_mul(i128::from(scale))?;
        for (_, coefficient) in &mut self.terms {
            *coefficient = coefficient.checked_mul(i128::from(scale))?;
        }
        Some(())
    }

    fn add_terms(&mut self, right: Self, scale: i128) -> Option<()> {
        self.constant = self
            .constant
            .checked_add(right.constant.checked_mul(scale)?)?;
        for (reference, coefficient) in right.terms {
            let coefficient = coefficient.checked_mul(scale)?;
            if let Some((_, current)) = self
                .terms
                .iter_mut()
                .find(|(owned, _)| same_reference(owned, &reference))
            {
                *current = current.checked_add(coefficient)?;
            } else {
                self.terms.push((reference, coefficient));
            }
        }
        Some(())
    }

    pub(super) fn same_terms(&self, other: &Self) -> bool {
        self.terms.len() == other.terms.len()
            && self.terms.iter().all(|(reference, coefficient)| {
                other.terms.iter().any(|(candidate, value)| {
                    value == coefficient && same_reference(reference, candidate)
                })
            })
    }
}

/// Source spans differ between endpoint occurrences; all semantic reference
/// fields and every path-part declaration remain part of this local identity.
pub(in crate::construction) fn same_reference(left: &Reference, right: &Reference) -> bool {
    let same_path = match (left.component_ref(), right.component_ref()) {
        (None, None) => true,
        (Some(left), Some(right)) => {
            left.local() == right.local()
                && left.parts().len() == right.parts().len()
                && left.parts().iter().zip(right.parts()).all(|(left, right)| {
                    left.ident == right.ident
                        && left.def_id == right.def_id
                        && left.subs.is_empty()
                        && right.subs.is_empty()
                })
        }
        _ => false,
    };
    same_path
        && left.var_name() == right.var_name()
        && left.instance_id() == right.instance_id()
        && left.resolved_function() == right.resolved_function()
        && left.is_generated() == right.is_generated()
}
