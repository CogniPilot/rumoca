//! The intervals of Integer and Real expressions under the facts of one path.
//!
//! Each endpoint is proven separately with checked arithmetic, so a
//! half-bounded operand still bounds a result where the operation allows
//! (`max(border, y - radius)` is bounded below when `border` is). What
//! `ShapeEnvironment` proves (settled values, binder bounds) is met with what
//! the path facts prove, so neither source is lost. A conditional
//! expression's interval is the hull of its arms, each read under the facts
//! of the condition that selects it (MLS §3.6.5).

use super::selections::exact_real;
use super::*;
use rumoca_core::BuiltinFunction;

impl GuardFacts {
    /// The Integer interval of `expression` on this path.
    pub(in crate::construction::analysis) fn integer_interval(
        &self,
        expression: &Expression,
        scope: FactScope<'_>,
    ) -> IntegerInterval {
        let settled = scope.shapes.proven_integer_interval(expression);
        let structural = match expression {
            Expression::VarRef { .. } | Expression::Index { .. } => FactSubject::of(expression)
                .and_then(|subject| self.fact(&subject))
                .and_then(|fact| match fact {
                    ValueFact::Integer(interval) => Some(interval),
                    ValueFact::Real(_) => None,
                })
                .unwrap_or(IntegerInterval::UNBOUNDED),
            Expression::Unary {
                op: OpUnary::Minus,
                rhs,
                ..
            } => self.integer_interval(rhs, scope).negate(),
            Expression::Unary {
                op: OpUnary::Plus,
                rhs,
                ..
            } => self.integer_interval(rhs, scope),
            Expression::Binary { op, lhs, rhs, .. } => {
                let lhs = self.integer_interval(lhs, scope);
                let rhs = self.integer_interval(rhs, scope);
                match op {
                    OpBinary::Add | OpBinary::AddElem => lhs.plus(rhs),
                    OpBinary::Sub | OpBinary::SubElem => lhs.minus(rhs),
                    OpBinary::Mul | OpBinary::MulElem => lhs.times(rhs),
                    _ => IntegerInterval::UNBOUNDED,
                }
            }
            Expression::BuiltinCall { function, args, .. } => {
                self.integer_builtin(*function, args, scope)
            }
            Expression::If {
                branches,
                else_branch,
                ..
            } => self.conditional_hull(branches, else_branch, scope, |facts, arm| {
                facts.integer_interval(arm, scope)
            }),
            _ => IntegerInterval::UNBOUNDED,
        };
        settled.meet(structural)
    }

    fn integer_builtin(
        &self,
        function: BuiltinFunction,
        args: &[Expression],
        scope: FactScope<'_>,
    ) -> IntegerInterval {
        match (function, args) {
            // MLS §3.7.1.1: `integer(x)` is the largest Integer not above `x`.
            (BuiltinFunction::Integer, [value]) => floor_interval(self.real_interval(value, scope)),
            (BuiltinFunction::Min | BuiltinFunction::Max, [lhs, rhs]) => {
                let lhs = self.integer_interval(lhs, scope);
                let rhs = self.integer_interval(rhs, scope);
                if function == BuiltinFunction::Min {
                    lhs.minimum(rhs)
                } else {
                    lhs.maximum(rhs)
                }
            }
            (BuiltinFunction::Div, [lhs, rhs]) => self
                .integer_interval(rhs, scope)
                .bounds()
                .filter(|(lower, upper)| lower == upper && *lower > 0)
                .map_or(IntegerInterval::UNBOUNDED, |(divisor, _)| {
                    self.integer_interval(lhs, scope).divided_by(divisor)
                }),
            _ => IntegerInterval::UNBOUNDED,
        }
    }

    /// The closed hull of the Real values of `expression` on this path. A
    /// Real reference is bounded only by its facts, which no NaN holds; an
    /// Integer-valued expression by its exact Integer interval.
    pub(super) fn real_interval(
        &self,
        expression: &Expression,
        scope: FactScope<'_>,
    ) -> RealInterval {
        if let Some(value) = numeric_literal(expression) {
            return RealInterval::exact(value).unwrap_or(RealInterval::UNBOUNDED);
        }
        if is_integer_valued(expression, scope) {
            let interval = self.integer_interval(expression, scope);
            return RealInterval {
                lower: interval.lower.and_then(exact_real),
                upper: interval.upper.and_then(exact_real),
            };
        }
        match expression {
            Expression::VarRef { .. } | Expression::Index { .. } => FactSubject::of(expression)
                .and_then(|subject| self.fact(&subject))
                .and_then(|fact| match fact {
                    ValueFact::Real(interval) => Some(interval),
                    ValueFact::Integer(_) => None,
                })
                .unwrap_or(RealInterval::UNBOUNDED),
            Expression::Unary {
                op: OpUnary::Minus,
                rhs,
                ..
            } => {
                let interval = self.real_interval(rhs, scope);
                RealInterval {
                    lower: interval.upper.map(|value| -value),
                    upper: interval.lower.map(|value| -value),
                }
            }
            Expression::BuiltinCall {
                function: BuiltinFunction::Floor,
                args,
                ..
            } => match args.as_slice() {
                [value] => {
                    let interval = self.real_interval(value, scope);
                    RealInterval {
                        lower: interval.lower.map(f64::floor),
                        upper: interval.upper.map(f64::floor),
                    }
                }
                _ => RealInterval::UNBOUNDED,
            },
            _ => RealInterval::UNBOUNDED,
        }
    }

    /// The hull of `arm_interval` over the arms of a conditional expression,
    /// each read under the facts that select it; an arm no path reaches adds
    /// nothing.
    fn conditional_hull<I: Copy + Hull>(
        &self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        scope: FactScope<'_>,
        arm_interval: impl Fn(&Self, &Expression) -> I,
    ) -> I {
        let conditions = branches
            .iter()
            .map(|(condition, _)| condition)
            .collect::<Vec<_>>();
        let arms = branches
            .iter()
            .map(|(_, arm)| arm)
            .chain(std::iter::once(else_branch));
        self.branch_entries(&conditions, scope)
            .iter()
            .zip(arms)
            .filter(|(facts, _)| !facts.is_unreachable())
            .map(|(facts, arm)| arm_interval(facts, arm))
            .reduce(Hull::hull)
            .unwrap_or(I::UNBOUNDED)
    }

    /// The fact this path proves for `subject`, if any.
    fn fact(&self, subject: &FactSubject) -> Option<ValueFact> {
        self.path.as_ref()?.get(subject).copied()
    }
}

trait Hull {
    const UNBOUNDED: Self;
    fn hull(self, other: Self) -> Self;
}

impl Hull for IntegerInterval {
    const UNBOUNDED: Self = IntegerInterval::UNBOUNDED;
    fn hull(self, other: Self) -> Self {
        IntegerInterval::hull(self, other)
    }
}

/// The Integers `integer(x)` (equivalently `floor`) yields for `x` in
/// `interval`; an endpoint outside the exact Integer range of a Real is not
/// proven.
fn floor_interval(interval: RealInterval) -> IntegerInterval {
    const EXACT: f64 = (1_i64 << 53) as f64;
    let endpoint = |value: Option<f64>| {
        value
            .map(f64::floor)
            .filter(|value| (-EXACT..=EXACT).contains(value))
            .map(|value| value as i64)
    };
    IntegerInterval {
        lower: endpoint(interval.lower),
        upper: endpoint(interval.upper),
    }
}

/// Whether `expression` has an Integer value: an Integer literal or an
/// Integer reference combined by Integer arithmetic.
fn is_integer_valued(expression: &Expression, scope: FactScope<'_>) -> bool {
    match expression {
        Expression::Literal {
            value: Literal::Integer(_),
            ..
        } => true,
        Expression::VarRef { .. } | Expression::Index { .. } => {
            FactSubject::of(expression).is_some_and(|subject| scope.is_integer(&subject))
        }
        Expression::Unary {
            op: OpUnary::Minus | OpUnary::Plus,
            rhs,
            ..
        } => is_integer_valued(rhs, scope),
        Expression::Binary {
            op:
                OpBinary::Add
                | OpBinary::AddElem
                | OpBinary::Sub
                | OpBinary::SubElem
                | OpBinary::Mul
                | OpBinary::MulElem,
            lhs,
            rhs,
            ..
        } => is_integer_valued(lhs, scope) && is_integer_valued(rhs, scope),
        Expression::BuiltinCall { function, args, .. } => match function {
            BuiltinFunction::Integer | BuiltinFunction::Size => true,
            BuiltinFunction::Div | BuiltinFunction::Min | BuiltinFunction::Max => {
                args.iter().all(|arg| is_integer_valued(arg, scope))
            }
            _ => false,
        },
        _ => false,
    }
}
