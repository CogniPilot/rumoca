//! Conservative Integer interval proofs for compact function domains.

use super::*;

impl ShapeEnvironment {
    /// A conservative finite Integer interval for `expression`, if this scope
    /// can prove one using exact Integer arithmetic.
    pub(in crate::construction) fn proven_integer_bounds(
        &self,
        expression: &Expression,
    ) -> Option<(i64, i64)> {
        if let Some(ProvenValue::Integer(value)) = eval_expr(expression, &self.values)
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
                    _ => None,
                }
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
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                if let Some(target) = integer_assignment_target(comp)
                    && let Some((lower, upper)) = shapes.proven_integer_bounds(value)
                {
                    shapes.merge_integer_bounds(target, lower, upper);
                }
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                bind_loop_integer_bounds(indices, shapes);
                infer_function_integer_bounds(equations, shapes);
            }
            rumoca_core::Statement::While { block, .. } => {
                infer_function_integer_bounds(&block.stmts, shapes);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    infer_function_integer_bounds(&block.stmts, shapes);
                }
                if let Some(fallback) = else_block {
                    infer_function_integer_bounds(fallback, shapes);
                }
            }
            _ => {}
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
