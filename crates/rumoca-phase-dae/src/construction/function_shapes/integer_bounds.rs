//! Conservative Integer interval proofs for compact function domains, and
//! exact range extents whose bounds share an unproven Integer term.
//!
//! MLS §10.4.1 defines the elements of `a:b` and `a:s:b` from the bounds, so
//! the number of elements depends only on the step and on `b - a`. A slice
//! such as `state[i - 2:i - 1]` inside `for i in 3:2:n loop` has a loop
//! variable in both bounds: neither bound is a translation-time value, yet
//! their difference is the exact Integer `1` for every value of `i`, which is
//! what MLS §12.2 needs from a function-local extent. [`exact_range_distance`]
//! proves that difference by cancelling the shared terms of two affine forms.

#[cfg(test)]
mod affine_tests;
mod finite_for;
mod flow;
mod operations;
#[cfg(test)]
mod tests;

pub(in crate::construction) use finite_for::infer_finite_for_counter_bounds;

use super::*;

impl ShapeEnvironment {
    /// A conservative finite Integer interval for `expression`, if this scope
    /// can prove one using exact Integer arithmetic.
    pub(in crate::construction) fn proven_integer_bounds(
        &self,
        expression: &Expression,
    ) -> Option<(i64, i64)> {
        if let Some(ProvenValue::Integer(value)) = eval_expr(expression, &self.shape_aware_values())
            .ok()
            .as_ref()
            .and_then(ProvenValue::from_settled)
        {
            return Some((value, value));
        }
        match expression {
            Expression::Literal {
                value: Literal::Integer(value),
                ..
            } => Some((*value, *value)),
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.integer_bounds.get(name.var_name()).copied(),
            Expression::Unary { op, rhs, .. } => {
                let (lower, upper) = self.proven_integer_bounds(rhs)?;
                match op {
                    OpUnary::Plus => Some((lower, upper)),
                    OpUnary::Minus => Some((upper.checked_neg()?, lower.checked_neg()?)),
                    _ => None,
                }
            }
            Expression::Binary { op, lhs, rhs, .. } => {
                let (lhs_lower, lhs_upper) = self.proven_integer_bounds(lhs)?;
                let (rhs_lower, rhs_upper) = self.proven_integer_bounds(rhs)?;
                match op {
                    OpBinary::Add | OpBinary::AddElem => Some((
                        lhs_lower.checked_add(rhs_lower)?,
                        lhs_upper.checked_add(rhs_upper)?,
                    )),
                    OpBinary::Sub | OpBinary::SubElem => Some((
                        lhs_lower.checked_sub(rhs_upper)?,
                        lhs_upper.checked_sub(rhs_lower)?,
                    )),
                    OpBinary::Mul | OpBinary::MulElem => {
                        operations::multiply((lhs_lower, lhs_upper), (rhs_lower, rhs_upper))
                    }
                    _ => None,
                }
            }
            Expression::BuiltinCall { function, args, .. } => {
                operations::builtin(self, *function, args)
            }
            _ => None,
        }
    }

    /// Bounds of the values produced by one ascending or descending Integer
    /// range. Empty ranges have no binder value and therefore return `None`.
    pub(in crate::construction) fn proven_range_bounds(
        &self,
        expression: &Expression,
    ) -> Option<(i64, i64)> {
        let Expression::Range {
            start, step, end, ..
        } = expression
        else {
            return None;
        };
        let (start_lower, start_upper) = self.proven_integer_bounds(start)?;
        let (step_lower, step_upper) = step
            .as_deref()
            .map(|step| self.proven_integer_bounds(step))
            .unwrap_or(Some((1, 1)))?;
        let (end_lower, end_upper) = self.proven_integer_bounds(end)?;
        if start_lower != start_upper || step_lower != step_upper || step_lower == 0 {
            return None;
        }
        let start = start_lower;
        let step = step_lower;
        if step > 0 {
            if end_upper < start {
                return None;
            }
            let distance = end_upper.checked_sub(start)?;
            let upper = start.checked_add(distance.checked_div(step)?.checked_mul(step)?)?;
            Some((start, upper))
        } else {
            if end_lower > start {
                return None;
            }
            let magnitude = step.checked_neg()?;
            let distance = start.checked_sub(end_lower)?;
            let lower =
                start.checked_sub(distance.checked_div(magnitude)?.checked_mul(magnitude)?)?;
            Some((lower, start))
        }
    }
}

/// Propagate conservative finite Integer intervals through function flow.
///
/// These intervals specialize compact runtime domains; they are never exact
/// translation-time values and therefore cannot select a branch.
pub(in crate::construction) fn infer_function_integer_bounds(
    statements: &[rumoca_core::Statement],
    shapes: &mut ShapeEnvironment,
) {
    let mut invalidated = flow::invalidated_integer_targets(statements);
    for target in &invalidated {
        shapes.integer_bounds.remove(target);
        shapes.values.remove_parameter(target.as_str());
    }
    infer_acyclic_integer_bounds(statements, shapes, &mut invalidated);
    for target in invalidated {
        shapes.integer_bounds.remove(&target);
        shapes.values.remove_parameter(target.as_str());
    }
}

fn infer_acyclic_integer_bounds(
    statements: &[rumoca_core::Statement],
    shapes: &mut ShapeEnvironment,
    invalidated: &mut HashSet<VarName>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                infer_integer_assignment(comp, value, shapes, invalidated);
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                infer_loop_integer_bounds(indices, equations, shapes, invalidated);
            }
            rumoca_core::Statement::While { block, .. } => {
                infer_acyclic_integer_bounds(&block.stmts, shapes, invalidated);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    infer_acyclic_integer_bounds(&block.stmts, shapes, invalidated);
                }
                if let Some(fallback) = else_block {
                    infer_acyclic_integer_bounds(fallback, shapes, invalidated);
                }
            }
            _ => {}
        }
    }
}

fn infer_integer_assignment(
    component: &rumoca_core::ComponentReference,
    value: &Expression,
    shapes: &mut ShapeEnvironment,
    invalidated: &mut HashSet<VarName>,
) {
    let Some(target) = integer_assignment_target(component) else {
        return;
    };
    if invalidated.contains(&target) {
        return;
    }
    if let Some((lower, upper)) = shapes.proven_integer_bounds(value) {
        shapes.merge_integer_bounds(target, lower, upper);
    } else {
        shapes.integer_bounds.remove(&target);
        shapes.values.remove_parameter(target.as_str());
        invalidated.insert(target);
    }
}

fn infer_loop_integer_bounds(
    indices: &[rumoca_core::ForIndex],
    statements: &[rumoca_core::Statement],
    shapes: &mut ShapeEnvironment,
    invalidated: &mut HashSet<VarName>,
) {
    let mut scoped_shapes = shapes.clone();
    bind_loop_integer_bounds(indices, &mut scoped_shapes);
    infer_acyclic_integer_bounds(statements, &mut scoped_shapes, invalidated);
    for target in flow::written_integer_targets(statements) {
        if let Some((lower, upper)) = scoped_shapes.integer_bounds.get(&target) {
            shapes.merge_integer_bounds(target, *lower, *upper);
        }
    }
}

fn bind_loop_integer_bounds(indices: &[rumoca_core::ForIndex], shapes: &mut ShapeEnvironment) {
    for index in indices {
        let Some((lower, upper)) = shapes.proven_range_bounds(&index.range) else {
            continue;
        };
        shapes.bind_integer_bounds(VarName::new(&index.ident), lower, upper);
    }
}

fn integer_assignment_target(component: &rumoca_core::ComponentReference) -> Option<VarName> {
    let [part] = component.parts() else {
        return None;
    };
    part.subs.is_empty().then(|| component.to_var_name())
}

/// `constant + sum(coefficient * term)` over Integer scalars whose values
/// this scope does not prove. Terms are kept in first-seen order and compared
/// by their exact Flat name, never hashed.
struct AffineInteger {
    constant: i64,
    terms: Vec<(VarName, i64)>,
}

impl AffineInteger {
    fn constant(value: i64) -> Self {
        Self {
            constant: value,
            terms: Vec::new(),
        }
    }

    fn term(name: VarName) -> Self {
        Self {
            constant: 0,
            terms: vec![(name, 1)],
        }
    }

    fn scaled(mut self, factor: i64) -> Option<Self> {
        self.constant = self.constant.checked_mul(factor)?;
        for (_, coefficient) in &mut self.terms {
            *coefficient = coefficient.checked_mul(factor)?;
        }
        Some(self)
    }

    fn plus(mut self, other: Self) -> Option<Self> {
        self.constant = self.constant.checked_add(other.constant)?;
        for (name, coefficient) in other.terms {
            match self
                .terms
                .iter_mut()
                .find(|(existing, _)| *existing == name)
            {
                Some((_, existing)) => *existing = existing.checked_add(coefficient)?,
                None => self.terms.push((name, coefficient)),
            }
        }
        Some(self)
    }

    /// The value when every term cancels.
    fn exact(&self) -> Option<i64> {
        self.terms
            .iter()
            .all(|(_, coefficient)| *coefficient == 0)
            .then_some(self.constant)
    }
}

/// The affine form of an Integer expression: proven values fold to constants
/// and an unproven unsubscripted Integer reference becomes a term.
fn affine_integer(expression: &Expression, values: &ShapeEnvironment) -> Option<AffineInteger> {
    if let Ok(value) = evaluate_shape_integer(expression, values) {
        return Some(AffineInteger::constant(value));
    }
    match expression {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(AffineInteger::constant(*value)),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(AffineInteger::term(name.var_name().clone())),
        Expression::Unary {
            op: OpUnary::Plus,
            rhs,
            ..
        } => affine_integer(rhs, values),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => affine_integer(rhs, values)?.scaled(-1),
        Expression::Binary { op, lhs, rhs, .. } => match op {
            OpBinary::Add | OpBinary::AddElem => {
                affine_integer(lhs, values)?.plus(affine_integer(rhs, values)?)
            }
            OpBinary::Sub | OpBinary::SubElem => {
                affine_integer(lhs, values)?.plus(affine_integer(rhs, values)?.scaled(-1)?)
            }
            OpBinary::Mul | OpBinary::MulElem => {
                let lhs = affine_integer(lhs, values)?;
                let rhs = affine_integer(rhs, values)?;
                match (lhs.exact(), rhs.exact()) {
                    (Some(factor), _) => rhs.scaled(factor),
                    (None, Some(factor)) => lhs.scaled(factor),
                    (None, None) => None,
                }
            }
            _ => None,
        },
        _ => None,
    }
}

/// The exact `end - start` of a range whose bounds differ by a proven
/// Integer, or `None` when the unproven terms do not cancel.
pub(super) fn exact_range_distance(
    start: &Expression,
    end: &Expression,
    values: &ShapeEnvironment,
) -> Option<i64> {
    let start = affine_integer(start, values)?;
    let end = affine_integer(end, values)?;
    end.plus(start.scaled(-1)?)?.exact()
}
